;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

(with-gc-suppressed
 (with-file-name-handler-disabled
  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)
  (setq inhibit-startup-message t)
  (setq user-emacs-directory (expand-file-name user-emacs-directory))
  (setq server-use-tcp t)
  ;; (setq server-host "127.0.0.1")
  ;; (setq server-port 2286)

  (with-slow-op-timer "<init> [local] all"
    (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

    (with-slow-op-timer "<init> [local] [config] require setup-defaults.el"
      (require 'setup-defaults))

    (with-slow-op-timer "<init> [local] [config] require bootstrap.el"
      (require 'bootstrap))

    (with-slow-op-timer "<init> [local] [packaging] initialize"
      (bootstrap-initialize-packages))

    (with-slow-op-timer "<init> [local] [feature] require clix"
      (require 'clix))

    (with-slow-op-timer "<init> [local] [config] all"
      (with-slow-op-timer "<init> [local] [config] require setup-modes.el"
        (require 'setup-modes))

      (with-slow-op-timer "<init> [local] [config] require setup-core.el"
        (require 'setup-core))))

  ;; load the user/*.el files
  (with-slow-op-timer "<init> [user] all"
    (bootstrap-load-user-local-config))))

(provide 'init)
;;; init.el ends here
