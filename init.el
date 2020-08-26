;; init.el -- tycho's emacs configuration

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

(setq gc-cons-threshold 80000000000000)
(add-to-list 'after-init-hook
	     (lambda ()
	       (setq gc-cons-threshold 800000)
	       (let ((garbage-collection-messages t))
		 (garbage-collect))
	       (alert (format "started (%d) in %s" (emacs-pid) (emacs-init-time))
		      :title (format "emacs-%s" tychoish-emacs-identifier))))

(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(setq package-enable-at-startup nil)

(package-initialize)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(defvar local-notes-directory (expand-file-name "~/notes")
  "Defines where notes (e.g. org, roam, deft, etc.) stores are located.")

(add-to-list 'load-path (concat user-emacs-directory "conf"))
(add-to-list 'load-path (concat user-emacs-directory "ext"))

(use-package local-functions)     ;; function library I've written/etc.
(use-package settings)            ;; collection of local settings
(use-package programming)         ;; all use-package declarations and configuration

(tychoish-setup-global-modes)
(tychoish-setup-modeline)
(tychoish-setup-user-local-config)

(provide 'init)
;;; init.el ends here
