;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

(with-gc-suppressed
 (with-file-name-handler-disabled
  (eval-when-compile
    (require 'subr-x))

  (defvar tychoish/startup-complete-time nil
    "Timestamp when startup finished.")
  (defvar tychoish/eglot-default-server-configuration nil
    "Eglot server configuration, populated later.")

  (defvar sprite-instance-id nil
    "Name of this Emacs instance, e.g. `work', `personal', `hud'.")

  (defvar local-notes-directory nil
    "Directory where notes (org, roam, deft, etc.) are stored.")

  (defvar user-org-directories nil
    "Additional directories that may contain org files.")

  (defvar tychoish-disable-external-notifications nil
    "Disable external notifications when non-nil.")

  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)
  (setq inhibit-startup-message t)
  (setq user-emacs-directory (expand-file-name user-emacs-directory))
  (setq server-use-tcp t)
  ;; (setq server-host "127.0.0.1")
  ;; (setq server-port 2286)

  (defun cli/time-reporting ()
    ;; `early-init.el' already set `slow-op-reporting' and
    ;; `use-package-compute-statistics'; this just acknowledges the flag.
    (when (string-prefix-p "--with-slow-op-timing" argi)
      (message "[op]: enabling time reporting")))

  (defvar cli/org-exec-file nil
    "Org file to execute via `builder-org-babel-execute-file'; set by --org-exec.")

  (defun cli/org-exec ()
    "Handle --org-exec FILE command-line flag."
    (when (string-prefix-p "--org-exec" argi)
      (setq cli/org-exec-file (pop argv))
      t))

  (defun cli/org-exec-run ()
    "Execute `cli/org-exec-file', then exit Emacs.
Runs from `after-init-hook', after the full config has loaded."
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
    "Execute all org files in `cli/org-exec-dir', then exit Emacs.
Runs from `after-init-hook', after the full config has loaded."
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
      (message "[emacs]: <%s> init time %s" sprite-instance-id init-time)
      (message "[emacs]: <%s> user time %s" sprite-instance-id startup-time)
      (message "[emacs]: <%s> wall time %s" sprite-instance-id wall-time)
      (alert (format "started (pid=%d) [init=%s user=%s wall=%s]" (emacs-pid) init-time startup-time wall-time)
	     :title (format "emacs-%s" sprite-instance-id))))

  (defun tychoish/startup-mark-complete ()
    (unless tychoish/startup-complete-time
      (setq tychoish/startup-complete-time (current-time))))

  (add-hook 'emacs-startup-hook 'tychoish/startup-mark-complete 99)
  (add-hook (if (daemonp) 'emacs-startup-hook 'window-setup-hook) 'tychoish/startup-report-timing 100)

  (defun bootstrap-package-quickstart-stale-p ()
    "Return non-nil when `package-quickstart-file' is missing or older than an installed package."
    (or (not (file-exists-p package-quickstart-file))
        (and (file-directory-p package-user-dir)
             (let ((quickstart-mtime (file-attribute-modification-time
                                       (file-attributes package-quickstart-file))))
               (seq-some (lambda (pkg-dir)
                           (and (file-directory-p pkg-dir)
                                (file-exists-p (expand-file-name (package--description-file pkg-dir) pkg-dir))
                                (time-less-p quickstart-mtime
                                             (file-attribute-modification-time (file-attributes pkg-dir)))))
                         (directory-files package-user-dir t "\\`[^.]" t))))))

  (with-slow-op-timer "<init> package all"
    (with-slow-op-timer "<init> package require"
      (require 'package))

    (with-slow-op-timer "<init> package quickstart"
      (setq package-quickstart-file (file-name-concat user-emacs-directory "state/package-quickstart.el"))
      (when (bootstrap-package-quickstart-stale-p)
	(message "[bootstrap] package-quickstart-file is stale relative to package-user-dir; refreshing")
	(package-quickstart-refresh)))

    (with-slow-op-timer "<init> package activation"
      (load package-quickstart-file t t t t))

    (setq package-archives
	  '(("melpa" . "https://melpa.org/packages/")
	    ("elpaish" . "https://tychoish.github.io/elpaish/snapshot/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("gnu" . "https://elpa.gnu.org/packages/")
            ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))

    (with-slow-op-timer "<init> [local] require all"
      (with-slow-op-timer "<init> [local] core dependencies"
	(use-package sprite :ensure t :demand t)
	(use-package xtdlib :ensure t :demand t))

      (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
      (with-slow-op-timer "<init> [local] require bootstrap.el"
	(require 'bootstrap))

      ;; remaining use-package declarations.
      (with-slow-op-timer "<init> [local] require tychoish-core.el"
	(require 'tychoish-core)))

    ;; load the user/*.el files
    (with-slow-op-timer "<init> [user] load all"
      (let ((user-libs-dir (expand-file-name "user" user-emacs-directory)))
	(add-to-list 'load-path user-libs-dir)
	(bootstrap-set-up-user-local-config user-libs-dir))))))

(provide 'init)
