;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

(defmacro with-gc-suppressed (&rest body)
  `(progn
     (let ((gc-cons-threshold 800000000000000))
       ,@body)
     (let ((garbage-collection-messages t))
       (garbage-collect))))

(defmacro with-file-name-handler-disabled (&rest body)
  `(let ((file-name-handler-alist nil))
     ,@body))

(cl-defmacro with-slow-op-timer (name &rest body)
  "Send a message the BODY operation of NAME takes longer to execute than a hardcoded threshold."
  (let ((threshold 0.01))
    `(let* ((inhibit-message t)
	    (time (current-time))
	    (return-value (progn ,@body))
	    (duration (time-to-seconds (time-since time))))
       (when (and (or debug-on-error init-file-debug) (> duration ,threshold))
	 (message "[slow-op]: %s: %.06fs" ,name duration))
       return-value)))

(with-gc-suppressed
 (with-file-name-handler-disabled
  (defvar tychoish/emacs-instance-id nil)
  (defvar tychoish/startup-complete-time nil)
  (defvar cli/instance-id  nil)

  (defun tychoish/startup-notify (msg)
    (message "emacs: %s" msg)
    (alert msg :title (format "emacs-%s" tychoish/emacs-instance-id)))

  (defun tychoish/startup-mark-complete ()
    (unless tychoish/startup-complete-time
      (setq tychoish/startup-complete-time (current-time))))

  (defun tychoish/startup-report-timing ()
    (tychoish/startup-notify
     (format "started (pid=%d) in %s [wall=%s]"
	     (emacs-pid)
	     (emacs-init-time)
	     (float-time (time-since before-init-time)))))

  (add-hook 'emacs-startup-hook 'tychoish/startup-mark-complete 80)
  (add-hook 'emacs-startup-hook 'tychoish/startup-report-timing 90)

  (when (string-match "NATIVE_COMP" system-configuration-features)
    (setq native-comp-jit-compilation t)
    (setq native-comp-deferred-compilation t)
    (setq native-compile-prune-cache t))

  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)
  (setq inhibit-startup-message t)

  (setq server-use-tcp t)
  ;; (setq server-host "127.0.0.1")
  ;; (setq server-port 2286)

  (defvar local-notes-directory (expand-file-name "~/notes")
    "Defines where notes (e.g. org, roam, deft, etc.) stores are located.")

  (defvar user-org-directories nil
    "Defines additional directories where org files might exist.")

  (defvar tychoish-disable-external-notifications nil
    "disable external notification support.")

  (defvar tychoish/eglot-default-server-configuration '())

  (defvar tychoish/bootstrap-packages '(f s dash ht)
    "A collection of packages installed outside of use-package for performance reasons.
The `--bootstrap CLI' flag will ensure that emacs is properly configured.")

  (defun cli/resolve-id ()
    (or (when (string-equal "--id" argi)
	  (setq cli/instance-id (pop argv)))
	(when (and (> (length argi) 5)
		   (or (string-prefix-p "--id=" argi)
		       (string-prefix-p "--id " argi)))
	  (setq cli/instance-id (substring argi 5)))))

  (defun cli/bootstrap ()
    (when (string-prefix-p "--bootstrap" argi)
      (let ((packages '(f s dash ht))
	    installed)
	(dolist (pkg packages)
	  (if (package-installed-p pkg)
	      (push pkg installed)
	    (package-install pkg)))
	(when (string-suffix-p "upgrade" argi)
	  (dolist (pkg installed)
	    (package-upgrade pkg)))
	(message "bootstrap complete: installed %S; restart emacs without `--bootstrap'" installed))))

  (add-to-list 'command-line-functions #'cli/resolve-id)
  (add-to-list 'command-line-functions #'cli/bootstrap)

  (eval-when-compile
    (require 'use-package))

  (setq package-user-dir (concat user-emacs-directory "elpa"))
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (setq package-archive-priorities '(("melpa"    . 3)
				     ("nongnu"    . 2)
				     ("gnu"    . 1)
				     ("jcs-elpa" . 0)))

  ;; (toggle-debug-on-error)
  ;; (setq use-package-expand-minimally t)
  ;; (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  (setq use-package-minimum-reported-time 0.5)
  (setq user-emacs-directory (expand-file-name user-emacs-directory))

  (add-to-list 'load-path (concat user-emacs-directory "lisp"))
  (add-to-list 'load-path (concat user-emacs-directory "user"))

  ;; (only) functions and macros used in the rest of the configuration
  (with-slow-op-timer
   "<init.el> load tychoish-common"
   (require 'tychoish-common)
   (tychoish/set-up-instance-name))
  ;; customized setup and configuration of core emacs and included packages
  (with-slow-op-timer
   "<init.el> load tychoish-bootstrap"
   (require 'tychoish-bootstrap))
  ;; all remaining use-package declarations.
  (with-slow-op-timer
   "<init.el> load tychoish-core"
   (require 'tychoish-core))
  ;; load the user/*.el files
  (with-slow-op-timer
   "<init.el> load user directory"
   (tychoish-set-up-user-local-config))))

(provide 'init)
