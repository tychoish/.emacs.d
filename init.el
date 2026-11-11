;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

(with-gc-suppressed
 (with-file-name-handler-disabled
  (with-slow-op-timer "<init> [site] all"
    (setq user-emacs-directory (expand-file-name user-emacs-directory))
    (setq server-use-tcp t)
    ;; (setq server-host "127.0.0.1")
    ;; (setq server-port 2286)

    (setq package-archives
          '(("melpa" . "https://melpa.org/packages/")
            ("elpaish" . "https://tychoish.github.io/elpaish/snapshot/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("gnu" . "https://elpa.gnu.org/packages/")))

    (setq package-quickstart-file (expand-file-name "state/package-quickstart.el" user-emacs-directory ))
    (setq package-archive-priorities '(("elpaish" . 100) ("melpa" . 99) ("nongnu" . 50) ("gnu" . 25)))

    (with-slow-op-timer "<init> [local] config initialization"
      (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

      (with-slow-op-timer "<init> [local] [config] require bootstrap.el"
        (require 'bootstrap))

      (with-slow-op-timer "<init> [local] [packaging] initialization"
        (bootstrap-initialize-packages))

      (with-slow-op-timer "<init> [local] [config] require setup-core.el"
        (require 'setup-core)))

    ;; load the user/*.el files
    (with-slow-op-timer "<init> [user] loaded user files"
      (bootstrap-load-user-local-config)))))

(provide 'init)
;;; init.el ends here
