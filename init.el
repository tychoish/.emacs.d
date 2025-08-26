;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

;; init-without-gc
(let ((file-name-handler-alist nil)
      (gc-cons-threshold 80000000000000))

    ;; reset gc after init is complete
  (add-to-list 'emacs-startup-hook
               (lambda ()
                 (setq tychoish/emacs-instance-id (tychoish/resolve-instance-id))
                 (setq max-specpdl-size 13000)
                 (setq gc-cons-threshold 800000)
                 (let ((garbage-collection-messages t)) (garbage-collect))

                 (let ((msg (format "started (%d) in %s" (emacs-pid) (emacs-init-time))))
                   (message "emacs: %s" msg)
                   (alert msg :title (format "emacs-%s" tychoish/emacs-instance-id)))))

  (when (string-match "NATIVE_COMP" system-configuration-features)
    (setq native-comp-jit-compilation t)
    (setq native-comp-deferred-compilation t)
    (setq native-compile-prune-cache t))

  ;; start: init-without-gc
  (defvar local-notes-directory (expand-file-name "~/notes")
    "Defines where notes (e.g. org, roam, deft, etc.) stores are located.")

  (defvar user-org-directories nil
    "Defines additional directories where org files might exist.")

  (defvar tychoish-disable-external-notifications nil
    "disable external notification support.")

  (defvar tychoish/eglot-default-server-configuration '())

  (defvar tychoish/emacs-instance-id nil)

  (defvar cli/instance-id  nil)

  (defun cli/resolve-id ()
    (or (when (string-equal "--id" argi)
	  (setq cli/instance-id (pop argv)))
	(when (and (> (length argi) 5)
		   (or (string-prefix-p "--id=" argi)
                       (string-prefix-p "--id " argi)))
	  (setq cli/instance-id (substring argi 5)))))

  (add-to-list 'command-line-functions #'cli/resolve-id)

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
  ;; (setq use-package-expand-minimally t)
  ;; (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  (setq use-package-minimum-reported-time 0.5)

  (add-to-list 'load-path (concat user-emacs-directory "lisp"))

  ;; all use-package declarations and configuration
  (use-package tychoish-core
    :init
    (setq initial-major-mode 'fundamental-mode)
    (setq initial-scratch-message nil)
    (setq inhibit-startup-message t)
    :config
    (setq inhibit-startup-echo-area-message (user-login-name))
    (setq frame-title-format '(:eval (format "%s:%s" tychoish/emacs-instance-id (buffer-name))))
    (add-to-list 'mode-line-misc-info '(:eval (format "[%s]" tychoish/emacs-instance-id)))
    :demand))

(provide 'init)
;;; init.el ends here
