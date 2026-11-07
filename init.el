;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

(with-gc-suppressed
 (with-file-name-handler-disabled
  (eval-when-compile
    (require 'subr-x))

  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)
  (setq inhibit-startup-message t)
  ;; (setq use-package-compute-statistics t)
  (setq user-emacs-directory (expand-file-name user-emacs-directory))
  (setq server-use-tcp t)
  ;; (setq server-host "127.0.0.1")
  ;; (setq server-port 2286)

  (with-slow-op-timer "<init> package all"
    (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

    (with-slow-op-timer "<init> [local] require clix"
      (require 'clix))

    (with-slow-op-timer "<init> [local] require setup-package-defaults.el"
      (require 'setup-package-defaults))

    (setq package-quickstart-file (file-name-concat user-emacs-directory "state/package-quickstart.el"))

    (let ((quickstart-target (let ((elc (concat (file-name-sans-extension package-quickstart-file) ".elc")))
                               (if (file-exists-p elc) elc package-quickstart-file))))
      (if (file-exists-p quickstart-target)
          (with-slow-op-timer "<init> [warm] package quickstart"
            (load quickstart-target t t t t))
        (with-slow-op-timer "<init> [cold] bootstrap packages"
          (with-slow-op-timer "<init> package activation"
            (require 'package)
            (package-activate-all))
          (with-slow-op-timer "<init> [cold] install elpaish"
            (unless (package-installed-p 'elpaish)
              (package-install 'elpaish)))
          (with-slow-op-timer "<init> [cold] setup packages"
            (require 'setup-packages))
          (with-slow-op-timer "<init> [cold] install all configured packages"
            (elpaish-install-packages :sync t))
          (with-slow-op-timer "<init> [cold] generate package quickstart"
            (bootstrap-refresh-package-quickstart t)))))

    (with-slow-op-timer "<init> [local] require all"
      (with-slow-op-timer "<init> [local] [feature] require setup-cmdr"
	(require 'setup-cmdr)
	(setup-cmdr-install-bindings)
	(setup-cmdr-register-commands)
	(cmdr-mode 1))

      (with-slow-op-timer "<init> [local] [feature] require bootstrap.el"
        (require 'bootstrap))

      (with-slow-op-timer "<init> [local] [config] require setup-defaults.el"
        (require 'setup-defaults))

      (with-slow-op-timer "<init> [local] [config] require setup-hooks.el"
        (require 'setup-hooks))

      ;; remaining use-package declarations.
      (with-slow-op-timer "<init> [local] [config] require setup-core.el"
        (require 'setup-core))))

  ;; load the user/*.el files
  (with-slow-op-timer "<init> [user] load all"
    (let ((user-libs-dir (expand-file-name "user" user-emacs-directory)))
      (add-to-list 'load-path user-libs-dir)
      (bootstrap-set-up-user-local-config user-libs-dir)))))

(provide 'init)
;;; init.el ends here
