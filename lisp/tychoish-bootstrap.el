;;; tychoish-bootstrap.el --- Utilities used during emacssetup

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0-pre
;; Package-Requires: ((emacs "24.4"))
;; Keywords: internal maint emacs startup dotemacs config
;; Homepage: https://github.com/tychoish/.eamcs.d

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

;; This package holds the functions I want to call directly during
;; start up, as well as functions useful for building configuration
;; that don't make sense to define elsewhere.  As this file needs to
;; load during startup my goal is to keep it as short as possible, and
;; acknowledging that the collection of functionality might need to be
;; slightly more eclectic than many other packages.

;;; Code:

(defvar tychoish-backup-directory (expand-file-name (concat user-emacs-directory "backups/"))
  "Defines a location for auto-save backup files.
This value is used to set both 'auto-save-filename-transforms'
and 'backup-directory-alist' by the `tychoish-set-backup-directory'
function which is called during startup.  Use `tychoish-set-backup-directory'
to change the value of this variable.")

(defun tychoish-set-backup-directory (path)
  "Set the backup directory to PATH and configure appropriately."
  (setq tychoish-backup-directory path)
  (setq auto-save-file-name-transforms `((".*" ,tychoish-backup-directory t)))
  (add-to-list 'backup-directory-alist (cons "." tychoish-backup-directory))

  (unless (file-exists-p tychoish-backup-directory)
    (make-directory tychoish-backup-directory))
  (chmod tychoish-backup-directory #o700))

(defmacro with-timer (name &rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06fs" ,name (float-time (time-since time)))))

(defmacro with-slow-op-timer (name threshold &rest body)
  "Send a message the BODY operation of NAME takes longer to execute than the THRESHOLD."
  `(let ((time (current-time)))
     ,@body
     (tychoish--threshold-logger ,threshold (time-to-seconds (time-since time)) ,name)))

(defun tychoish--threshold-logger (threshold duration name)
  "Send a message with the DURATION and NAME when the duration is over the THRESHOLD."
  (when (>  duration threshold)
    (message "%s: %.06fs" name duration)))

(defun gui-p ()
  "Return t when the current session is or may be a GUI session."
  (when (or (daemonp) (window-system))
    t))

(defun default-string (default input)
  "Return the DEFAULT value if the INPUT is empty or nil."
  (cond
   ((string-equal default input)
    default)
   ((eq input nil)
    default)
   ((string-equal input "")
    default)
   (t
    input)))

(defun tychoish-setup-global-modes ()
  "Set up useful global modes, for use at the end of the setup process."

  (unless (daemonp)
    (menu-bar-mode -1))

  ;; setting the list before calling the function reduce the total time
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (scroll-bar-mode -1)

  (tool-bar-mode -1)
  (delete-selection-mode 1)
  (show-paren-mode t)
  (transient-mark-mode 1)
  (column-number-mode t)
  (electric-pair-mode 1)

  (when (gui-p)
    (which-key-mode 1)
    (diminish 'which-key-mode))

  (unless (gui-p)
    (xterm-mouse-mode 1))

  (when (daemonp)
    (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
    (let ((gc-cons-threshold 800000))
      (session-initialize)
      (desktop-save-mode 1)
      (desktop-read))))

(defvar after-theme-change-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-theme-change-hook))

(defadvice enable-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-theme-change-hook))

(defadvice disable-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-theme-change-hook))

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun on-frame-open (frame)
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (unless (display-graphic-p frame)
    (set-face-foreground 'default "unspecified-fg" frame)
    (set-face-background 'default "unspecified-bg" frame)))

(defun on-after-init ()
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (on-frame-open (selected-frame)))

;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
(add-hook 'after-make-frame-functions #'on-frame-open)
(add-hook 'window-setup-hook #'on-after-init)

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defun djcb-opacity-modify (&optional dec)
  "Modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	  (oldalpha (if alpha-or-nil alpha-or-nil 100))
	  (newalpha (if dec (- oldalpha 2) (+ oldalpha 2))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(defun opacity-increase ()
  (interactive)
  (djcb-opacity-modify))

(defun opacity-decrease ()
  (interactive)
  (djcb-opacity-modify t))

(defun opacity-reset ()
  (interactive)
  (modify-frame-parameters nil `((alpha . 100))))

(defun tychoish-load-light-theme ()
  (interactive)
  (disable-all-themes)
  (when (load-theme 'modus-operandi t t)
    (enable-theme 'modus-operandi))
  (add-to-list 'default-frame-alist '(alpha . 100)))

(defun tychoish-load-dark-theme ()
  (interactive)
  (disable-all-themes)
  (when (load-theme 'modus-vivendi t t)
    (enable-theme 'modus-vivendi))
  (add-to-list 'default-frame-alist '(alpha . 95)))

(defun tychoish-setup-font (name number)
  (interactive "sName: \nNSize: ")
  (let ((new-font-name (concat name "-" (number-to-string number))))
    (face-remap-add-relative new-font-name)
    (add-to-list 'default-frame-alist (cons 'font new-font-name))))
(setq default-frame-alist nil)

(defun tychoish-get-config-file-prefix (name)
  "Build a config file basename, for NAME.
This combines the host name and the dameon name."
  (format "%s-%s-%s" (system-name) (default-string "generic" (daemonp)) name))

(defun tychoish-get-config-file-path (name)
  "Return an absolute path for NAME in the configuration directory.
The is unique to the system and daemon instance."
  (concat (expand-file-name user-emacs-directory) (tychoish-get-config-file-prefix name)))

(defun tychoish-setup-user-local-config ()
  "Ensure that all config files in the `user-emacs-directory' + '/user' path are loaded."
  (let ((dirname (concat (expand-file-name user-emacs-directory) "user")))
    (when (file-accessible-directory-p dirname)
      (add-to-list 'load-path dirname)
      (mapc (lambda (fn)
	      (when (and (string-match-p "\\.el$" fn)
			 (not (string-match-p "^flycheck_.*\\.el$" fn)))
		(with-slow-op-timer (format "loading user config [%s]" fn) 0.10
		 (require (intern (string-remove-suffix ".el" fn))))))
	    (directory-files dirname))) t))

(defalias 'kill-buffers-matching-name 'kill-matching-buffers)

(defun kill-buffers-matching-path (regexp &optional internal-too no-ask)
  "Kill buffers whose name matches the specified REGEXP.
Ignores buffers whose name starts with a space, unless optional
prefix argument INTERNAL-TOO is non-nil.  Asks before killing
each buffer, unless NO-ASK is non-nil."
  (interactive "sKill buffers visiting a path matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-file-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (funcall (if no-ask 'kill-buffer 'kill-buffer-ask) buffer)))))

(defun display-startup-echo-area-message ()  "Called during setup, intentially a noop, which omit the message."  nil)
(defun emacs-repository-version-git (dir)  "Noop definition of function to speed up startup" "")
(defun emacs-repository-get-version (&optional dir ext)  "Noop definition of function to speed up startup" "")

(defvar my-suppress-message-p t)
(defun ad:suppress-message (f &rest arg)
  (if my-suppress-message-p
      (let ((inhibit-message t)
            (message-log-max nil))
        (apply f arg))
    (apply f arg)))

(defvar tychoish-xterm-mouse-state nil)
(defun xterm-mouse-mode-toggle ()
  (interactive)
  (if tychoish-xterm-mouse-state
      (xterm-mouse-mode -1)
    (xterm-mouse-mode 1))
  (setq tychoish-xterm-mouse-state (not tychoish-xterm-mouse-state)))

(advice-add 'emacs-repository-branch-git :around #'ad:suppress-message)
(advice-add 'emacs-repository-version-git :around #'ad:suppress-message)

(provide 'tychoish-bootstrap)
;;; tychoish-bootstrap.el ends here

