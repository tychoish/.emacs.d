;;; legacy/blog.el --- generic note-file creator (not loaded) -*- lexical-binding: t; -*-

;;; Commentary:
;; `bootstrap-create-note-file' and `bootstrap-define-project-notes' used to
;; live in `bootstrap.el'. Neither is Denote-integrated (no frontmatter, no
;; identifier, no signature) and neither is blog-specific -- it was a generic
;; "write a date-prefixed file somewhere" utility used for three unrelated
;; project note directories. Parked here for reference; not required or
;; loaded from anywhere.

;;; Code:

(defun bootstrap-create-note-file (title &optional &key path)
  "Create a new file for a post of with the specified TITLE."
  (interactive "sName: ")
  (let* ((slug (f-make-slug title))
         (datetime (format-time-string "%Y-%02m-%02d"))
         (draft-fn (file-name-concat (or path
			                 (annotated-completing-read-directory))
			             (concat datetime "." slug "." bootstrap-blog-extension))))
    (if (file-exists-p draft-fn)
        (find-file draft-fn)
      (find-file draft-fn)
      (insert (concat "# " title))
      (goto-char (point-max))
      (whitespace-cleanup)
      (insert "\n"))
    (message "new note: %s" draft-fn)))

(cl-defmacro bootstrap-define-project-notes (&key project path)
  (let ((symbol (intern (format "bootstrap-create-%s-note" project)))
	(path (expand-file-name path)))
    `(defun ,symbol (name)
       ,(format "Create a date prefixed note file in the %s project in %s." project path)
       (interactive "sName: ")
       (bootstrap-create-note-file name :path ,path))))

;;; blog.el ends here
