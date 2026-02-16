;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

(with-gc-suppressed
 (defvar tychoish/startup-complete-time nil "Timestamp reflecting when the instance' startup process actually completed.")
 (defvar tychoish/bootstrap-packages '(f s dash ht anaphora fn) "Packages installed with the `--botstrap' CLI flag outside of use-package; for performance.")
 (defvar tychoish/eglot-default-server-configuration nil "Define eglot Server configuration variable early for use later.")

 (defvar tychoish/emacs-instance-id nil "Name of emacs instance. `work', `personal', and `hud' are common long lived instances; other ephemeral ones may be useful.")
 (defvar cli/instance-id  nil "cli specified daemon/instance name")

 (defvar local-notes-directory (expand-file-name "~/notes") "Defines where notes (e.g. org, roam, deft, etc.) stores are located.")
 (defvar user-org-directories nil "Defines additional directories where org files might exist.")
 (defvar user-home-directory (expand-file-name "~") "path to the current home directory. cached during init.")

 (defvar tychoish-disable-external-notifications nil "disable external notification support.")

 (setq initial-major-mode 'fundamental-mode)
 (setq initial-scratch-message nil)
 (setq inhibit-startup-message t)
 (setq package-enable-at-startup nil)

 (setq user-emacs-directory (expand-file-name user-emacs-directory))

 (setq server-use-tcp t)

 ;; (setq server-host "127.0.0.1")
 ;; (setq server-port 2286)

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

 (add-to-list 'command-line-functions 'cli/resolve-id)
 (add-to-list 'command-line-functions 'cli/bootstrap)

 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
 (add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
 (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
 (setq package-archive-priorities '(("melpa"    . 3)
				    ("nongnu"    . 2)
				    ("gnu"    . 1)
				    ("jcs-elpa" . 0)))

 (declare-function alert "alert")

 (defun tychoish/startup-report-timing ()
   (let ((msg (format "started (pid=%d) in %s [wall=%s]"
		      (emacs-pid)
		      (emacs-init-time)
		      (float-time (time-since before-init-time)))))
     (message "emacs: %s" msg)
     (alert msg :title (format "emacs-%s" tychoish/emacs-instance-id))))

 (defun tychoish/startup-mark-complete ()
   (unless tychoish/startup-complete-time
     (setq tychoish/startup-complete-time (current-time))))

 (add-hook 'emacs-startup-hook 'tychoish/startup-mark-complete 80)
 (add-hook 'emacs-startup-hook 'tychoish/startup-report-timing 90)

 (with-file-name-handler-disabled
  (add-to-list 'load-path (concat user-emacs-directory "lisp"))
  (add-to-list 'load-path (concat user-emacs-directory "user"))
  ;; (only) functions and macros used in the rest of the configuration
  (with-slow-op-timer "<init.el> tychoish-common"
   (require 'tychoish-common)
   (declare-function tychoish/set-up-instance-name "tychoish-common")
   (tychoish/set-up-instance-name))

  ;; customized setup and configuration of core emacs and included packages

  (with-slow-op-timer "<init.el> tychoish-bootstrap"
   (require 'tychoish-bootstrap)
   (setq custom-file (tychoish/conf-state-path "custom.el"))
   'tychoish-bootstrap)

  ;; remaining use-package declarations.

  (with-slow-op-timer "<init.el> load tychoish-core"
   (require 'tychoish-core))

  (with-slow-op-timer "<init.el> tychoish-mail"
   (require 'tychoish-mail))

  (with-slow-op-timer "<core.el> load tychoish-org"
   (require 'tychoish-org))

  ;; load the user/*.el files

  (with-slow-op-timer "<init.el> user-files"
   (declare-function tychoish-set-up-user-local-config 'tychoish-bootstrap)
   (tychoish-set-up-user-local-config))))

(provide 'init)
