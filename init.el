;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:
(declare-function alert "alert")
(declare-function bootstrap-set-up-user-local-config "bootstrap")
(declare-function builder-org-babel-execute-file "builder")
(declare-function builder-org-babel-execute-directory "builder")

(with-gc-suppressed
 (defvar tychoish/startup-complete-time nil
   "Timestamp reflecting when the instance' startup process actually completed.")
 (defvar tychoish/eglot-default-server-configuration nil
   "Define eglot Server configuration variable early for use later.")

 (defvar sprite-instance-id nil
   "Name of emacs instance. `work', `personal', and `hud' are common long
lived instances. Other ephemeral instance names ones may be useful.")

 (defvar local-notes-directory nil
   "Defines where notes (e.g. org, roam, deft, etc.) stores are located.")

 (defvar user-org-directories nil
   "Defines additional directories where org files might exist.")

 (defvar tychoish-disable-external-notifications nil
   "disable external notification support.")

 (setq initial-major-mode 'fundamental-mode)
 (setq initial-scratch-message nil)
 (setq inhibit-startup-message t)
 (setq user-emacs-directory (expand-file-name user-emacs-directory))
 (setq server-use-tcp t)
 ;; (setq server-host "127.0.0.1")
 ;; (setq server-port 2286)

 (defun cli/time-reporting ()
   (when (string-prefix-p "--with-slow-op-timing" argi)
     (message "[op]: enabling time reporting")
     (setq slow-op-reporting t)))

 (defvar cli/org-exec-file nil
   "Org file to execute via `builder-org-babel-execute-file'; set by --org-exec.")

 (defun cli/org-exec ()
   "Handle --org-exec FILE command-line flag."
   (when (string-prefix-p "--org-exec" argi)
     (setq cli/org-exec-file (pop argv))
     t))

 (defun cli/org-exec-run ()
   "Execute the org file named by `cli/org-exec-file' then exit Emacs.
Called from `after-init-hook' so the full config is loaded first."
   (when cli/org-exec-file
     (builder-org-babel-execute-file cli/org-exec-file)
     (kill-emacs 0)))

 (defvar cli/org-exec-dir nil
   "Directory to execute all org babel blocks in; set by --org-exec-dir.")

 (defun cli/org-exec-dir ()
   "Handle --org-exec-dir DIR command-line flag."
   (when (string-prefix-p "--org-exec-dir" argi)
     (setq cli/org-exec-dir (pop argv))
     t))

 (defun cli/org-exec-dir-run ()
   "Execute all org files in `cli/org-exec-dir' then exit Emacs.
Called from `after-init-hook' so the full config is loaded first."
   (when cli/org-exec-dir
     (builder-org-babel-execute-directory cli/org-exec-dir)
     (kill-emacs 0)))

 (add-to-list 'command-line-functions 'cli/time-reporting)
 (add-to-list 'command-line-functions 'cli/org-exec)
 (add-to-list 'command-line-functions 'cli/org-exec-dir)
 (add-hook 'after-init-hook #'cli/org-exec-run)
 (add-hook 'after-init-hook #'cli/org-exec-dir-run)

 (defun tychoish/startup-report-timing ()
   (let ((startup-time (float-time (time-subtract tychoish/startup-complete-time before-init-time)))
	 (init-time (float-time (time-subtract after-init-time before-init-time)))
	 (wall-time (float-time (time-since before-init-time))))
     (message "[emacs]: <%s> wall time %s" sprite-instance-id wall-time)
     (message "[emacs]: <%s> user time %s" sprite-instance-id startup-time)
     (message "[emacs]: <%s> init time %s" sprite-instance-id init-time)
     (alert (format "started (pid=%d) [user=%s sys=%s wall=%s]" (emacs-pid) startup-time init-time wall-time)
	    :title (format "emacs-%s" sprite-instance-id))))

 (defun tychoish/startup-mark-complete ()
   (unless tychoish/startup-complete-time
     (setq tychoish/startup-complete-time (current-time))))

 (add-hook 'emacs-startup-hook 'tychoish/startup-mark-complete 99)
 (add-hook (if (daemonp) 'emacs-startup-hook 'window-setup-hook) 'tychoish/startup-report-timing 100)

 (defun bootstrap-package (package path url)
   "Ensure PACKAGE at PATH (relative to `user-emacs-directory') is on `load-path', using a local checkout or `package-vc-install'."
   (unless (package-installed-p package)
     (let ((checkout (expand-file-name path user-emacs-directory)))
       (if (file-directory-p checkout)
           (add-to-list 'load-path checkout)
         (package-vc-install `(,package :url ,url))))))

 (with-file-name-handler-disabled
  (with-slow-op-timer "<init> package"
    (package-initialize)
    (setq package-archives
	  '(("melpa" . "https://melpa.org/packages/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("gnu" . "https://elpa.gnu.org/packages/")
            ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))))

  (with-slow-op-timer "<init> external"
    (bootstrap-package 'xtdlib "external/xtdlib" "https://github.com/tychoish/xtdlib.el")
    (bootstrap-package 'sprite "external/sprite" "https://github.com/tychoish/sprite"))

  (with-slow-op-timer "<init> sprite"
    (require 'sprite))

  (setq custom-file (sprite-state-path "custom.el"))

  (with-slow-op-timer "<init> local-lisp"
    (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
    (with-slow-op-timer "<init> bootstrap"
      (require 'bootstrap))

    ;; remaining use-package declarations.
    (with-slow-op-timer "<init> tychoish-core"
      (require 'tychoish-core))

    (with-slow-op-timer "<init> tychoish-mail"
      (require 'tychoish-mail))

    ;; load the user/*.el files
    (with-slow-op-timer "<init> user-files"
      (add-to-list 'load-path (expand-file-name "user" user-emacs-directory))
      (bootstrap-set-up-user-local-config)))))

(provide 'init)
