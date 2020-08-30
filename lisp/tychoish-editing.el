;;; tychoish-editing.el --- tools for editing 

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/tychoish/.emacs.d
;; Keywords: editing utils

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

;; these are a collection of text editing functions 

;;; Code:

(defvar *tychoish-save-hook-off* t)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun tycho-toggle-hooks ()
  "Reset the before-save hook to preven cleaning up."
  (interactive)
  (cond
   ((equal *tychoish-save-hook-off* nil)
    (add-hook 'before-save-hook 'whitespace-cleanup)
    (setq show-trailing-whitespace t)
    (message "whitespace-cleanup on")
    (setq *tychoish-save-hook-off* t))
   ((equal *tychoish-save-hook-off* t)
    (remove-hook 'before-save-hook 'whitespace-cleanup)
    (setq show-trailing-whitespace nil)
    (message "whitespace-cleanup off")
    (setq *tychoish-save-hook-off* nil))))

(defun font-lock-show-tabs ()
  "Return a font-lock style keyword for tab characters."
  '(("\t" 0 'trailing-whitespace prepend)))

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for strings beyond WIDTH that use 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(defun tychoish-electric-pair ()
  "Insert character pair without sournding spaces."
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defvar electrify-return-match "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
	(save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun word-count (&optional start end)
  "Prints number of lines, words and characters in region or whole buffer."
  (interactive)
  (let ((n 0)
	(start (if mark-active (region-beginning) (point-min)))
	(end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (message "Lines: %3d; Words: %3d; Characters: %3d" (count-lines start end) n (- end start))))

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines between BEG and END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
	(line-beginning-position 2)))))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
	(forward-line)
	(when (or (< arg 0) (not (eobp)))
	  (transpose-lines arg))
	(forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; macros
(fset 'markdown-indent-code "     \C-a\C-n")
(fset 'rst-indent-code "   \C-a\C-n")

(provide 'tychoish-editing)
;;; tychoish-editing.el ends here
