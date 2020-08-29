;;; local-functions.el --- a collection of adhoc functions.

;;; Commentary:

;; ad hoc functions, used in other parts of the config.

;;; Code:

(defun font-lock-show-tabs ()
  "Return a font-lock style keyword for tab characters."
  '(("\t" 0 'trailing-whitespace prepend)))

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for strings beyond WIDTH that use 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

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

(defun ssh-reagent ()
  (interactive)
  (let* ((sshdir (car (directory-files "/tmp" nil "ssh-*")))
	 (agent (car (directory-files (concat "/tmp/" sshdir) nil "agent.*"))))
    (setenv "SSH_AUTH_SOCK" (concat "/tmp/" sshdir "/" agent)))
  (message "Attached to SSH Session"))

(defalias 'sshra 'ssh-reagent)

(defvar *tychoish-save-hook-off* t)

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

(defvar git-grep-command "git --no-pager grep --no-color --line-number <C> <R>"
  "The command to run with M-x git-grep.")

(defun git-grep (regexp)
  "Search for the given regexp using `git grep' in the current directory."
  (interactive "sRegexp: ")
  (unless (boundp 'grep-find-template) (grep-compute-defaults))
  (let ((old-command grep-find-template))
    (grep-apply-setting 'grep-find-template git-grep-command)
    (rgrep regexp "*" "")
    (grep-apply-setting 'grep-find-template old-command)))

(defun git-grep-repo ()
  "Search for the given regexp using `git grep' in the current directory."
  (interactive)
  (unless (boundp 'grep-find-template) (grep-compute-defaults))
  (let ((old-command grep-find-template)
	(search-text (buffer-substring-no-properties (region-beginning) (region-end))))
    (message search-text)
    (grep-apply-setting 'grep-find-template "git --no-pager grep --no-color --line-number <C> <R>")
    (rgrep search-text "*" (magit-toplevel))
    (grep-apply-setting 'grep-find-template old-command)))

(defadvice grep-expand-template (around grep-expand-template-with-git-color)
  (when (and (string-match "^git grep " (ad-get-arg 0))3
	     (not (string-match " --color=" (ad-get-arg 0))))
    (ad-set-arg 0 (replace-regexp-in-string
		   "^git grep " "git grep --color=auto " (ad-get-arg 0))))
  ad-do-it)

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

(defun default-string (default input)
  "return the default value if the string is empty or nil"
  (cond
   ((string-equal default input)
    default)
   ((eq input nil)
    default)
   ((string-equal input "")
    default)
   (t
    input)))

(defmacro with-timer (name &rest body)
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06fs" ,name (float-time (time-since time)))))

(defun tychoish-legacy-mode-line ()
  (interactive)
  (setq-default mode-line-format
		(list
		 mode-line-mule-info
		 mode-line-client
		 mode-line-modified
		 mode-line-remote
		 mode-line-frame-identification
		 "<"
		 tychoish-emacs-identifier
		 ">:"
		 mode-line-buffer-identification
		 " "
		 mode-line-position
		 '(vc-mode vc-mode)
		 "%M"
		 global-mode-string
		 ""
		 mode-line-modes)))

(defun tychoish-minimize-mode-line ()
  (delight 'emacs-lisp-mode "elisp")
  (delight 'fundamental-mode "fund")
  (delight 'lisp-mode "lisp")
  (delight 'lisp-interaction-mode "li")
  (delight 'auto-fill-mode "afm")
  (diminish 'overwrite-mode "om")
  (diminish 'org-indent-mode)
  (diminish 'org-capture-mode)
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)
  (diminish 'refill-mode "rf")
  (diminish 'auto-fill-mode "afm")
  (diminish 'visual-line-mode "wrap")
  (diminish 'ctags-auto-update-mode))

(defun tychoish-setup-modeline ()
  (interactive)
  (tychoish-legacy-mode-line)
  (doom-modeline-mode 1)
  (tychoish-doom-modeline-setup)
  (tychoish-minimize-mode-line))

(defun tychoish-setup-global-modes ()
  (windmove-default-keybindings)
  (fringe-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (global-page-break-lines-mode 1)
  (delete-selection-mode 1)
  (winner-mode 1)
  (show-paren-mode t)
  (winum-mode 1)
  (transient-mark-mode 1)
  (column-number-mode t)
  (electric-pair-mode 1)

  (when (daemonp)
    (let ((gc-cons-threshold 800000))
      (session-initialize)
      (recentf-mode 1)
      (desktop-save-mode 1))))

(defun gui-p ()
  (or (daemonp) (window-system)))

(defun tychoish-get-config-file-prefix (name)
  (format "%s-%s-%s" (system-name) (default-string "generic" (daemonp)) name))

(defun tychoish-get-config-file-path (name)
  (concat (expand-file-name user-emacs-directory) (tychoish-get-config-file-prefix name)))

(defun tychoish-setup-user-local-config ()
  "Ensure that all config files in the '~/.emacs.d/user' are loaded."
  (let ((dirname (concat (expand-file-name user-emacs-directory) "user")))
    (when (file-accessible-directory-p dirname)
      (add-to-list 'load-path dirname)

      (mapc (lambda (fn)
	      (when (string-match-p "\\.el$" fn)
		(with-timer
		 (format "loading userf config [%s]" fn)
		 (require (intern (string-remove-suffix ".el" fn))))))
	    (directory-files dirname))) t))

;; macros
(fset 'markdown-indent-code "     \C-a\C-n")
(fset 'rst-indent-code "   \C-a\C-n")

(provide 'local-functions)
;;; local-functions.el ends here
