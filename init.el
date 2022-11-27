;; init.el -- tycho's emacs configuration

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

(setq gc-cons-threshold 80000000000000)

(let ((file-name-handler-alist nil))
  (add-to-list 'after-init-hook
  	       (lambda ()
  		 (setq max-specpdl-size 13000)
  		 (setq gc-cons-threshold 800000)
  		 (let ((garbage-collection-messages t)) (garbage-collect))
  		 (let ((msg (format "started (%d) in %s" (emacs-pid) (emacs-init-time))))
  		   (message (concat "emacs: " msg))
  		   (when (daemonp) (alert msg :title (format "emacs-%s" tychoish-emacs-identifier))))))

  (require 'package)
  (setq package-user-dir (concat user-emacs-directory "elpa"))
  (setq package-enable-at-startup nil)

  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package))

  (defvar local-notes-directory (expand-file-name "~/notes")
    "Defines where notes (e.g. org, roam, deft, etc.) stores are located.")

  (defvar user-org-directories nil
    "Defines additional directories where org files might exist.")

  (add-to-list 'load-path (concat user-emacs-directory "lisp"))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")

  ;; all use-package declarations and configuration
  (use-package tychoish-core
    :config
    (tychoish-setup-global-modes)
    (tychoish-setup-modeline)
    (tychoish-setup-user-local-config)
    ))

(provide 'init)
;;; init.el ends here
