;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

(with-gc-suppressed
 (with-file-name-handler-disabled
  (eval-when-compile
    (require 'subr-x))

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
  ;; (setq use-package-compute-statistics t)
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


  (with-slow-op-timer "<init> package all"
    (with-slow-op-timer "<init> require cl-macs"
      (require 'cl-macs))

    (setq package-quickstart-file (file-name-concat user-emacs-directory "state/package-quickstart.el"))

    (if (file-exists-p package-quickstart-file)
	(with-slow-op-timer "<init> package quickstart"
          (load package-quickstart-file t t t t))
      (with-slow-op-timer "<init> package activation"
        (require 'package)
        (package-activate-all)))

    (with-slow-op-timer "<init> [local] require all"
      (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

      (with-slow-op-timer "<init> [local] require setup-package-defaults.el"
	(require 'setup-package-defaults))

      (with-slow-op-timer "<init> [local] install elpaish"
	(unless (package-installed-p 'elpaish)
	  (package-install 'elpaish)))

      (with-slow-op-timer "<init> [local] install sprite xtdlib"
	(unless (and (package-installed-p 'sprite)
	             (package-installed-p 'xtdlib))
	  (elpaish-install-packages '(sprite xtdlib))))

      (unless (file-exists-p package-quickstart-file)
        (require 'setup-packages)
        (with-slow-op-timer "<init> [cold] install all configured packages"
          (elpaish-install-packages setup-packages-list :sync t))
        (with-slow-op-timer "<init> [cold] generate package quickstart"
          (bootstrap-refresh-package-quickstart t)))

      (with-slow-op-timer "<init> [local] require sprite"
	(require 'sprite))

      (with-slow-op-timer "<init> [local] require xtd-macro"
	(require 'xtd-macro))

      (with-slow-op-timer "<init> [local] require hud-mode.el"
	(require 'hud-mode))

      (with-slow-op-timer "<init> [local] require bootstrap.el"
	(require 'bootstrap))

      (with-slow-op-timer "<init> [local] require setup-defaults.el"
	(require 'setup-defaults))

      (with-slow-op-timer "<init> [local] require setup-hooks.el"
	(require 'setup-hooks))

      ;; remaining use-package declarations.
      (with-slow-op-timer "<init> [local] require setup-core.el"
	(require 'setup-core)))

    ;; load the user/*.el files
    (with-slow-op-timer "<init> [user] load all"
      (let ((user-libs-dir (expand-file-name "user" user-emacs-directory)))
	(add-to-list 'load-path user-libs-dir)
	(bootstrap-set-up-user-local-config user-libs-dir))))))

(provide 'init)
;;; init.el ends here
