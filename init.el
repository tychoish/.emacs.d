;; init.el -- tycho's emacs configuration

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

;; init-without-gc
(let ((file-name-handler-alist nil)
      (gc-cons-threshold 80000000000000))
    ;; reset gc after init is complete
  (add-to-list 'after-init-hook
               (lambda ()
                 (setq max-specpdl-size 13000)
                 (setq gc-cons-threshold 800000)
                 (let ((garbage-collection-messages t))
                   (garbage-collect))
                 (let ((msg (format "started (%d) in %s" (emacs-pid) (emacs-init-time))))
                   (message (concat "emacs: " msg))
                   (alert msg :title (format "emacs-%s" tychoish-emacs-identifier))))
               (when (string-match "NATIVE_COMP" system-configuration-features)
                 (setq native-comp-deferred-compilation t)
                 (setq native-compile-prune-cache t)))
;; start: init-without-gc
  (eval-when-compile
    (require 'use-package))

  (use-package package
    :init
    (setq package-user-dir (concat user-emacs-directory "elpa"))
    (setq package-enable-at-startup nil)
    :config
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
    (setq package-archive-priorities '(("melpa"    . 3)
				       ("gnu"    . 2)
				       ("nongnu"    . 1)
                                       ("jcs-elpa" . 0))))

  ;; (toggle-debug-on-error)
  (setq use-package-expand-minimally t)
  (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  (setq use-package-minimum-reported-time 0.5)

  (add-to-list 'load-path (concat user-emacs-directory "lisp"))


  (defvar local-notes-directory (expand-file-name "~/notes")
    "Defines where notes (e.g. org, roam, deft, etc.) stores are located.")

  (defvar user-org-directories nil
    "Defines additional directories where org files might exist.")

  (defvar tychoish-disable-external-notifications nil
    "disable external notification support.")

  (defvar tychoish-emacs-identifier (or (daemonp) "solo"))

  (defvar tychoish/eglot-default-server-configuration '())

  ;; all use-package declarations and configuration
  (use-package tychoish-core
    :config
    (setq initial-major-mode 'fundamental-mode)
    (setq initial-scratch-message nil)
    (setq inhibit-startup-message t)
    (setq inhibit-startup-echo-area-message (user-login-name))
    :demand))

(provide 'init)
;;; init.el ends here
