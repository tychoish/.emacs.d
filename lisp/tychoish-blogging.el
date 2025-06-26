;; tychoish-blogging -- static blog posting automation

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0-pre
;; Package-Requires: ((f "0.20.0"))
;; Homepage: https://github.com/tychoish/.eamcs.d
;; Keywords: emacs blog

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides some convenience functions for creating posts and
;; automating the publication and editing workflows of drafting blog
;; posts.

;; Some dependencies on other libraries, notably "f" for file path
;; handling and some functions in my local-functions library.

;;; Code:

(require 'f)

(defvar tychoish-blog-path (expand-file-name "~/blog")
  "Path to the blog's project directory.")

(defvar tychoish-blog-extension ".md"
  "File extension for the blog files.")

(defun tychoish-blog-insert-date ()
  "Insert date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun make-filename-slug (s)
  "Turn a string, S, into a slug for a blog post filename."
  (replace-regexp-in-string
   " " "-" (downcase
            (string-clean-whitespace
             (replace-regexp-in-string
              "[^A-Za-z0-9 ]" "" s)))))

(defun tychoish-blog-push ()
  "Run 'make push' in a compile buffer for the project."
  (interactive)
  (let ((compile-buffer-name "*tychoish-blog-push*")
        (push-command "time PATH=/usr/local/bin:$PATH make push")
        ;; set's the context for the command
        (default-directory tychoish-blog-path))

    (when (tychoish-uniq-compile-buffer compile-buffer-name push-command)
      (recompile))
    (revbufs)))

(defun tychoish-blog-create-post (title)
  "Create a new file for a post of with the specified TITLE."
  (interactive "sPost Title: ")
  (let* ((slug (make-filename-slug title))
         (draft-fn (f-join tychoish-blog-path (concat slug tychoish-blog-extension))))
    (if (file-exists-p draft-fn)
        (find-file draft-fn)
      (progn
	(kill-new title)
        (find-file draft-fn)
        (yas-expand-snippet
         (yas-lookup-snippet "hugo"))))
    (message "working on post: %s" draft-fn)))

(defun tychoish-create-note-file (title)
  "Create a new file for a post of with the specified TITLE."
  (interactive "sName: ")
  (let* ((slug (make-filename-slug title))
         (datetime (format-time-string "%Y-%02m-%02d"))
         (draft-fn (f-join tychoish-project-note-file (concat datetime "." slug ".md"))))
    (if (file-exists-p draft-fn)
        (find-file draft-fn)
      (progn
        (find-file draft-fn)
        (insert (concat "# " title))
        (end-of-buffer)
        (whitespace-cleanup)
        (insert "\n")))
    (message "new note: %s" draft-fn)))

(defun tychoish-blog-publish-post ()
  "Move the blog post in the current buffer to the publication location.
Does nothing if the current post is not in the drafts folder."
  (interactive)
    (let* ((publish-directory (f-join tychoish-blog-path "content" "post"))
           (original-file-name (buffer-file-name (current-buffer)))
           (published-file-name (f-join publish-directory (file-name-nondirectory original-file-name)))
           (current-point (point)))
      (cond
       ((not (equal (file-name-extension original-file-name t) ".rst"))
        (message "post %s has incorrect extension" original-file-name))
       ((buffer-modified-p)
        (message "file %s is modified. please save before publishing" original-file-name))
       ((file-exists-p published-file-name)
        (message "published file exists with same name. not publishing"))
       (t
        (message "publishing: %s" published-file-name)
        (rename-file original-file-name published-file-name)
        (kill-buffer nil)
        (find-file published-file-name)
        (set-window-point (selected-window) current-point)
        (message "published %s to %s" original-file-name publish-directory)))))

(defun tychoish-blog-open-drafts-dired ()
  "Open a dired buffer for the drafts folder."
  (interactive)
  (find-file (expand-file-name tychoish-blog-path)))

(provide 'tychoish-blogging)
;;; tychoish-blogging.el ends here
