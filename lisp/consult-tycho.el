;;; consult-tycho.el --- tycho(ish) consult helpers -*- lexical-binding: t -*-

;; Author: sam kleinman
;; Maintainer: tychoish

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These are mostly helpers to for writing my own set of consult
;; helpers largely for "better" incremental (rip)grep tools.

;;; Code:

(require 'f)

(require 'builder)

(eval-when-compile
  (require 'yasnippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; blogging

(defvar tychoish-blog-path (expand-file-name "~/blog")
  "Path to the blog's project directory.")

(defvar tychoish-blog-extension ".md"
  "File extension for the blog files.")

(defun tychoish-blog-create-post (title)
  "Create a new file for a post of with the specified TITLE."
  (interactive "sPost Title: ")
  (let* ((slug (f-make-slug title))
         (draft-fn (f-join tychoish-blog-path (concat slug "-" tychoish-blog-extension))))
    (if (file-exists-p draft-fn)
        (find-file draft-fn)
      (kill-new title)
      (find-file draft-fn)
      (yas-expand-snippet
       (yas-lookup-snippet "hugo")))
    (message "working on post: %s" draft-fn)))

(defun tychoish-create-note-file (title &optional &key path)
  "Create a new file for a post of with the specified TITLE."
  (interactive "sName: ")
  (let* ((slug (f-make-slug title))
         (datetime (format-time-string "%Y-%02m-%02d"))
         (draft-fn (f-join (or path
			       (builder--select-directory))
			   (concat datetime "." slug "." tychoish-blog-extension))))
    (if (file-exists-p draft-fn)
        (find-file draft-fn)
      (find-file draft-fn)
      (insert (concat "# " title))
      (goto-char (point-max))
      (whitespace-cleanup)
      (insert "\n"))
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
       ((not (equal (file-name-extension original-file-name t) tychoish-blog-extension))
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

(provide 'consult-tycho)
;;; consult-tycho.el ends here
