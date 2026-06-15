;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

;; Load-path setup and package bootstrap.  lisp/ and user/ are added so the
;; configuration modules (bootstrap, tychoish-core, …) can be found.
;; xtdlib is bootstrapped here because it must be on load-path before the
;; modules are compiled — their eval-when-compile forms need it at byte-compile
;; time, and package-initialize alone will not install a missing package.
;; When the elpa/ submodule is present the checkout is added to load-path
;; directly; on a fresh install without the submodule it is fetched via
;; package-vc from GitHub.
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "user" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "external/sprite" user-emacs-directory))
  (package-initialize)
  (unless (package-installed-p 'xtdlib)
    (let ((checkout (expand-file-name "external/xtdlib" user-emacs-directory)))
      (if (file-directory-p checkout)
          (add-to-list 'load-path checkout)
        (package-vc-install '(xtdlib :url "https://github.com/tychoish/xtdlib.el"))))))

(with-gc-suppressed
 (defvar tychoish/startup-complete-time nil
   "Timestamp reflecting when the instance' startup process actually completed.")
 (defvar tychoish/bootstrap-packages '(f s dash ht anaphora fn)
   "Packages installed with the `--botstrap' CLI flag outside of use-package.")
 (defvar tychoish/eglot-default-server-configuration nil
   "Define eglot Server configuration variable early for use later.")

 (defvar sprite-instance-id nil
   "Name of emacs instance. `work', `personal', and `hud' are common long
lived instances. Other ephemeral instance names ones may be useful.")

 (defvar local-notes-directory (expand-file-name "~/notes")
   "Defines where notes (e.g. org, roam, deft, etc.) stores are located.")
 (defvar user-org-directories nil
   "Defines additional directories where org files might exist.")
 (defvar user-home-directory (expand-file-name "~")
   "path to the current home directory. cached during init.")

 (defvar tychoish-disable-external-notifications nil
   "disable external notification support.")

 (setq initial-major-mode 'fundamental-mode)
 (setq initial-scratch-message nil)
 (setq inhibit-startup-message t)

 (setq user-emacs-directory (expand-file-name user-emacs-directory))

 (setq server-use-tcp t)

 ;; (setq server-host "127.0.0.1")
 ;; (setq server-port 2286)

 (defun cli/bootstrap ()
   (when (string-prefix-p "--bootstrap" argi)
     (let ((packages '(f s dash ht cond-let))
	   installed)
       (dolist (pkg packages)
	 (if (package-installed-p pkg)
	     (push pkg installed)
	   (package-install pkg)))
       (when (string-suffix-p "upgrade" argi)
	 (dolist (pkg installed)
	   (package-upgrade pkg)))
       (message "bootstrap complete: installed %S; restart emacs without `--bootstrap'" installed))))

 (defun cli/time-reporting ()
   (when (string-prefix-p "--with-slow-op-timing" argi)
     (message "[op]: enabling time reporting")
     (setq slow-op-reporting t)))

 (add-to-list 'command-line-functions 'cli/time-reporting)
 (add-to-list 'command-line-functions 'cli/bootstrap)

 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
 (add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
 (setq package-archive-priorities '(("melpa"    . 100)
				    ("nongnu"    . 50)
				    ("gnu"    . 25)
				    ("jcs-elpa" . 10)))

 (declare-function alert "alert")

 (defun tychoish/startup-report-timing ()
   (let* ((startup-time (float-time (time-subtract tychoish/startup-complete-time before-init-time)))
	  (init-time (float-time (time-subtract after-init-time before-init-time)))
	  (wall-time (float-time (time-since before-init-time)))
	  (msg (format "started (pid=%d) [user=%s sys=%s wall=%s]"
		      (emacs-pid)
		      startup-time
		      init-time
		      wall-time)))
     (message "[emacs]: <%s> wall time %s" sprite-instance-id wall-time)
     (message "[emacs]: <%s> user time %s" sprite-instance-id startup-time)
     (message "[emacs]: <%s> init time %s" sprite-instance-id init-time)
     (alert msg :title (format "emacs-%s" sprite-instance-id))))

 (defun tychoish/startup-mark-complete ()
   (unless tychoish/startup-complete-time
     (setq tychoish/startup-complete-time (current-time))))

 (add-hook 'emacs-startup-hook 'tychoish/startup-mark-complete 99)
 (add-hook (if (daemonp) 'emacs-startup-hook 'window-setup-hook) 'tychoish/startup-report-timing 100)

 (with-file-name-handler-disabled
  (with-slow-op-timer "<init> sprite"
   (require 'sprite))

  (setq custom-file (sprite-state-path "custom.el"))

  (with-slow-op-timer "<init> bootstrap"
   (require 'bootstrap))

  ;; remaining use-package declarations.
  (with-slow-op-timer "<init> tychoish-core"
   (require 'tychoish-core))

  (with-slow-op-timer "<init> tychoish-mail"
   (require 'tychoish-mail))

  (with-slow-op-timer "<init> tychoish-org"
   (require 'tychoish-org))

  ;; load the user/*.el files
  (with-slow-op-timer "<init> user-files"
   (declare-function tychoish-set-up-user-local-config 'bootstrap)
   (tychoish-set-up-user-local-config))))

(provide 'init)
