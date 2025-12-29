
;;; orgmetajson.el --- Export Org heading metadata and content to JSON -*- lexical-binding: t; -*-
;; Author: Ben Sanchez
;; Maintainer: Ben Sanchez
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; Keywords: outlines, files, convenience
;; URL: https://example.com/orgmetajson
;; License: MIT

;;; Commentary:
;;
;; orgmetajson exports Org-mode heading metadata and content as JSON using
;; Emacs’s built-in json.el and Org’s org-element APIs. It supports:
;; - Title, TODO keyword, priority, tags (optionally inherited), file-level tags
;; - SCHEDULED and DEADLINE timestamps
;; - Properties drawer key-value pairs
;; - Outline path (ancestors)
;; - Either the element’s raw content or the whole subtree content
;; - Pretty printing
;;
;; Interactive entry points:
;; - `orgmetajson-export-at-point`         : export the heading at point
;; - `orgmetajson-export-buffer`           : export all headings in buffer
;; - `orgmetajson-export-matches`          : export matched headings via a tags/TODO matcher
;;
;; Customization:
;; - `orgmetajson-include-inherited-tags`
;; - `orgmetajson-export-subtree-content`
;; - `orgmetajson-pretty-print-json`
;; - `orgmetajson-filename-format`
;;
;; Installation:
;; 1. Save this file as orgmetajson.el.
;; 2. Add its directory to your `load-path`.
;; 3. (require 'orgmetajson)
;;
;; Usage examples:
;;   M-x orgmetajson-export-at-point
;;   M-x orgmetajson-export-buffer
;;   M-x orgmetajson-export-matches
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'json)
(require 'seq)

(defgroup orgmetajson nil
  "Export Org heading metadata and content to JSON."
  :group 'org
  :prefix "orgmetajson-")

(defcustom orgmetajson-include-inherited-tags t
  "When non-nil, include inherited tags using `org-get-tags` semantics."
  :type 'boolean
  :group 'orgmetajson)

(defcustom orgmetajson-export-subtree-content nil
  "When non-nil, export the entire subtree text instead of the element’s raw content."
  :type 'boolean
  :group 'orgmetajson)

(defcustom orgmetajson-pretty-print-json t
  "When non-nil, pretty-print JSON output via `json-encoding-pretty-print`."
  :type 'boolean
  :group 'orgmetajson)

(defcustom orgmetajson-filename-format "%04d_%s.json"
  "Format string for output filenames.
It receives (counter slug) and must produce a string ending in .json."
  :type 'string
  :group 'orgmetajson)


;;; Internal utilities

(defun orgmetajson--filetags ()
  "Return file-level `#+FILETAGS:` as a list of strings."
  (let* ((kw (org-collect-keywords '("FILETAGS")))
         (raw (car (alist-get "FILETAGS" kw))))
    (and raw (split-string raw "[ :]+" t))))

(defun orgmetajson--headline-tags (hl include-inherited)
  "Return the tags for headline HL.
If INCLUDE-INHERITED is non-nil, compute tags via Org’s tag APIs at the headline."
  (if (and include-inherited hl)
      (save-excursion
        (goto-char (org-element-property :begin hl))
        (org-back-to-heading t)
        (org-get-tags))
    (and hl (org-element-property :tags hl))))

(defun orgmetajson--timestamp-string (obj)
  "Return a string for timestamp object OBJ from org-element."
  (when (and obj (eq (org-element-type obj) 'timestamp))
    (org-element-interpret-data obj)))

(defun orgmetajson--properties-drawer (hl)
  "Return an alist of properties from headline HL."
  (let (props)
    (when hl
      (org-element-map (org-element-contents hl) 'property-drawer
        (lambda (pd)
          (org-element-map (org-element-contents pd) 'node-property
            (lambda (np)
              (let ((key (org-element-property :key np))
                    (val (org-element-property :value np)))
                (push (cons key val) props)))))))
    (nreverse props)))

(defun orgmetajson--outline-path (hl)
  "Return a list of titles forming the outline path for headline HL."
  (when hl
    (mapcar (lambda (h)
              (org-element-interpret-data (org-element-property :title h)))
            (org-element-lineage hl '(headline) nil))))

(defun orgmetajson--subtree-text (hl)
  "Return the entire subtree text for headline HL, without text properties."
  (when hl
    (save-excursion
      (goto-char (org-element-property :begin hl))
      (org-back-to-heading t)
      (let ((beg (point))
            (end (progn
                   (org-end-of-subtree t t)
                   (point))))
        (buffer-substring-no-properties beg end)))))

(defun orgmetajson--sanitize-slug (s)
  "Return a filesystem-safe slug from string S."
  (when s
    (let ((slug (replace-regexp-in-string "[^A-Za-z0-9_]+" "_"
                                          (string-trim s))))
      (replace-regexp-in-string "_+" "_" slug))))


;;; Record construction

(defun orgmetajson--element-heading-record (element include-inherited subtree-content)
  "Return an alist describing ELEMENT’s headline and content.

INCLUDE-INHERITED: include inherited tags.
SUBTREE-CONTENT: include entire subtree text instead of element content."
  (let* ((hl (org-element-lineage element '(headline) t))
         (title-obj (and hl (org-element-property :title hl)))
         (title (and title-obj (org-element-interpret-data title-obj)))
         (todo (and hl (org-element-property :todo-keyword hl)))
         (priority (and hl (org-element-property :priority hl))) ;; character code, e.g., ?A
         (scheduled (orgmetajson--timestamp-string (and hl (org-element-property :scheduled hl))))
         (deadline  (orgmetajson--timestamp-string (and hl (org-element-property :deadline hl))))
         (tags      (orgmetajson--headline-tags hl include-inherited))
         (filetags  (orgmetajson--filetags))
         (archivedp (and hl (org-element-property :archivedp hl)))
         (commentedp (and hl (org-element-property :commentedp hl)))
         (level     (and hl (org-element-property :level hl)))
         (outline-path (orgmetajson--outline-path hl))
         (properties (orgmetajson--properties-drawer hl))
         (content
          (if subtree-content
              (orgmetajson--subtree-text hl)
            (let ((b (org-element-property :contents-begin element))
                  (e (org-element-property :contents-end element)))
              (and b e (buffer-substring-no-properties b e))))))
    `((title        . ,title)
      (level        . ,level)
      (todo         . ,todo)
      (priority     . ,(and priority (char-to-string priority)))
      (tags         . ,tags)
      (filetags     . ,filetags)
      (scheduled    . ,scheduled)
      (deadline     . ,deadline)
      (archived     . ,archivedp)
      (commented    . ,commentedp)
      (outline_path . ,outline-path)
      (properties   . ,properties)
      (content      . ,content))))


;;; Public API

;;;###autoload
(defun orgmetajson-export-at-point (file &optional include-inherited subtree-content pretty)
  "Export the headline containing the element at point to JSON FILE.

INCLUDE-INHERITED: when non-nil, include inherited tags.
SUBTREE-CONTENT: when non-nil, include the whole subtree content.
PRETTY: when non-nil, pretty-print JSON.

When called interactively, prompt for FILE and use user defaults from
`orgmetajson-include-inherited-tags`, `orgmetajson-export-subtree-content`,
and `orgmetajson-pretty-print-json`."
  (interactive
   (list (read-file-name "Write JSON to file: ")
         orgmetajson-include-inherited-tags
         orgmetajson-export-subtree-content
         orgmetajson-pretty-print-json))
  (let* ((json-encoding-pretty-print (or pretty json-encoding-pretty-print))
         (element (org-element-at-point))
         (record (orgmetajson--element-heading-record element include-inherited subtree-content)))
    (with-temp-file file
      (insert (json-encode record)))
    (message "orgmetajson: wrote %s" file)))

;;;###autoload
(defun orgmetajson-export-buffer (directory &optional matcher include-inherited subtree-content pretty)
  "Export all (optionally MATCHER-filtered) headlines in the current buffer to DIRECTORY.

DIRECTORY: output directory for JSON files (created if missing).
MATCHER: tags/TODO matcher string (nil for all entries), e.g., \"TODO+backend-blocked\".
INCLUDE-INHERITED, SUBTREE-CONTENT, PRETTY: same semantics as `orgmetajson-export-at-point`."
  (interactive
   (list (read-directory-name "Export directory: " nil nil t)
         (let ((m (read-string "Matcher (empty for all): ")))
           (unless (string-empty-p m) m))
         orgmetajson-include-inherited-tags
         orgmetajson-export-subtree-content
         orgmetajson-pretty-print-json))
  (unless (file-directory-p directory)
    (make-directory directory t))
  (let ((json-encoding-pretty-print (or pretty json-encoding-pretty-print))
        (counter 0))
    (org-map-entries
     (lambda ()
       (let* ((element (org-element-at-point))
              (record (orgmetajson--element-heading-record element include-inherited subtree-content))
              (title (alist-get 'title record))
              (path  (alist-get 'outline_path record))
              (slug  (orgmetajson--sanitize-slug
                      (mapconcat #'identity (append path (and title (list title))) "_")))
              (file  (expand-file-name (format orgmetajson-filename-format counter (or slug "entry"))
                                       directory)))
         (setq counter (1+ counter))
         (with-temp-file file
           (insert (json-encode record)))))
     matcher 'file))
  (message "orgmetajson: exported buffer to %s" directory))

;;;###autoload
(defun orgmetajson-export-matches (directory matcher &optional include-inherited subtree-content pretty)
  "Export headlines matching MATCHER in the current buffer to DIRECTORY.

MATCHER: tags/TODO matcher string, e.g., \"TODO+backend-blocked\".
Other arguments are as in `orgmetajson-export-at-point`."
  (interactive
   (list (read-directory-name "Export directory: " nil nil t)
         (read-string "Matcher: ")
         orgmetajson-include-inherited-tags
         orgmetajson-export-subtree-content
         orgmetajson-pretty-print-json))
  (orgmetajson-export-buffer directory matcher include-inherited subtree-content pretty))


;;; Minimal tests

(require 'ert)

(ert-deftest orgmetajson--sanitize-slug-basic ()
  (should (equal (orgmetajson--sanitize-slug "Hello World!") "Hello_World_")))

(ert-deftest orgmetajson--sanitize-slug-collapse ()
  (should (equal (orgmetajson--sanitize-slug "A  B---C") "A_B_C")))


(provide 'orgmetajson)

