;;; setup-defaults.el --- Standard library and built-in defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Global settings and configuration for built-in Emacs subsystems with no
;; use-package declarations.

;;; Code:

(defvar sprite-instance-id)

(setq jit-lock-defer-time 0.2)
(setq jit-lock-stealth-nice 0.2)
(setq jit-lock-stealth-load 100)
(setq-default indicate-empty-lines t)
(setq tooltip-resize-echo-area t)

(setq backup-by-copying t)
(setq delete-old-versions t)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq find-file-visit-truename t)
(setq auto-revert-verbose nil)
(setq auto-revert-avoid-polling t)
(setq auto-revert-interval 60)

(setq vc-follow-symlinks t)
(setq version-control t)

(setq recentf-auto-cleanup 'never)
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-menu-items 100)

(setq frame-title-format '(:eval (format "%s:%s" (or (bound-and-true-p sprite-instance-id) "emacs") (buffer-name))))

(setq create-lockfiles nil)

(setq-default cursor-in-non-selected-windows nil)
(setq scroll-conservatively 25)
(setq scroll-preserve-screen-position t)

(setq ring-bell-function #'ignore)
(setq-default truncate-lines t)
(setq use-dialog-box nil)
(setq use-short-answers t)
(setq-default indent-tabs-mode nil) ; (setq tab-width 4)
(setq shell-command-dont-erase-buffer 'end-last-out)
(setq comment-auto-fill-only-comments t)
(setq select-enable-clipboard nil) ;; select-enable-primary already defaults to nil.
(setq makefile-electric-keys t)

(setq switch-to-prev-buffer-skip 'visible)
(setq split-height-threshold 100)
(setq window-sides-vertical t)

(setq show-paren-delay 0.25)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(advice-add 'display-startup-echo-area-message :override #'ignore)
(advice-add 'ask-user-about-supersession-threat :override #'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; system -- darwin or linux specific settings

(when (eq system-type 'darwin)
  (setq read-process-output-max (* 64 1024))
  (setq ns-function-modifier 'hyper)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(when (eq system-type 'gnu/linux)
  (setq read-process-output-max (* 1024 1024)))

(provide 'setup-defaults)
;;; setup-defaults.el ends here
