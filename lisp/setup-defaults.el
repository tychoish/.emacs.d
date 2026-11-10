;;; setup-defaults.el --- Standard library and built-in defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Global settings and configuration for built-in Emacs subsystems with no
;; use-package declarations.

;;; Code:

(defvar sprite-instance-id)

;; TODO consider: if these settings are configured in a global mode,
;; and reverted when the mode is enabled/disabled, the purpose of the
;; mode is to change the default. I don't want to make the system
;; unusable to users who have different preferences, as an all or
;; nothing setting. Would be nice to have a function that would allow
;; overriding the default by name/symbol (quoted so as to avoid
;; import/compile errors.)

(setq jit-lock-defer-time 0.2)
(setq jit-lock-stealth-nice 0.2)
(setq jit-lock-stealth-load 100)
(setq-default indicate-empty-lines t)
(setq tooltip-resize-echo-area t) ;; TODO does this logically belong with minaid?

(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq use-dialog-box nil)
(setq use-short-answers t)
(setq select-enable-clipboard nil) ;; select-enable-primary already defaults to nil.

(setq auto-revert-verbose nil)
(setq auto-revert-avoid-polling t)
(setq auto-revert-interval 60)
(setq backup-by-copying t)
(setq delete-old-versions t)

(setq find-file-visit-truename t) ;; TODO does this logically belong with minaid
(setq vc-follow-symlinks t)
(setq version-control t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(setq recentf-auto-cleanup 'never) ;; TODO move to minaid (?)
(setq recentf-keep '(file-remote-p file-readable-p)) ;; TODO move to minaid (?)
(setq recentf-max-menu-items 100) ;; TODO move to minaid (?)
(setq show-paren-delay 0.25) ;; TODO move to eglox (?)

(setq frame-title-format '(:eval (format "%s:%s" (or (bound-and-true-p sprite-instance-id) "emacs") (buffer-name)))) ;; should this go in sprite-mode?

(setq create-lockfiles nil)

(setq-default cursor-in-non-selected-windows nil)
(setq scroll-conservatively 25)
(setq scroll-preserve-screen-position t)

(setq comment-auto-fill-only-comments t) ;; TODO move to eglox?

(setq ring-bell-function #'ignore)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil) ; (setq tab-width 4)
(setq shell-command-dont-erase-buffer 'end-last-out)
(setq makefile-electric-keys t) ;; TODO should this move to eglox with other electric key?

(setq switch-to-prev-buffer-skip 'visible)
(setq split-height-threshold 100)
(setq window-sides-vertical t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package management

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpaish" . "https://tychoish.github.io/elpaish/snapshot/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities '(("elpaish" . 100)
                                   ("melpa" . 99)
                                   ("nongnu" . 50)
                                   ("gnu" . 25)))

;; TODO I presume this _must_ be set before
;; elpaish-install/sprite-state-mapping is realized? If not this
;; should move to one of those locations.
(setq package-quickstart-file (file-name-concat user-emacs-directory "state/package-quickstart.el"))

(provide 'setup-defaults)
;;; setup-defaults.el ends here
