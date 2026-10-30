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
    (with-slow-op-timer "<init> require cl-macs"
      (require 'cl-macs))

    (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

    (with-slow-op-timer "<init> [local] require clix"
      (require 'clix))

    (with-slow-op-timer "<init> [local] require setup-package-defaults.el"
      (require 'setup-package-defaults))

    (setq package-quickstart-file (file-name-concat user-emacs-directory "state/package-quickstart.el"))

    (if (file-exists-p package-quickstart-file)
        (with-slow-op-timer "<init> [warm] package quickstart"
          (load package-quickstart-file t t t t))
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
          (elpaish-install-packages setup-packages-list :sync t))
        (with-slow-op-timer "<init> [cold] generate package quickstart"
          (bootstrap-refresh-package-quickstart t))))

    (with-slow-op-timer "<init> [local] require all"
      (with-slow-op-timer "<init> [local] [pkg] require sprite"
        (require 'sprite))

      (with-slow-op-timer "<init> [local] [pkg] require xtd-macro"
        (require 'xtd-macro))

      (with-slow-op-timer "<init> [local] [feature] require hud-mode.el"
        (require 'hud-mode))

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
