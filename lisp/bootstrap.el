;;; bootstrap.el --- Utilities used during emacs setup -*- lexical-binding: t; -*-

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0-pre
;; Package-Requires: ((emacs "24.4"))
;; Keywords: internal maint emacs startup dotemacs config
;; Homepage: https://github.com/bootstrap-.eamcs.d

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package holds the functions I want to call directly during
;; start up, as well as functions useful for building configuration
;; that don't make sense to define elsewhere.  As this file needs to
;; load during startup my goal is to keep it as short as possible, and
;; acknowledging that the collection of functionality might need to be
;; slightly more eclectic than many other packages.

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forward Declarations

(declare-function package-activate-all "package")
(declare-function sprite-batch-startup "sprite-batch")
(declare-function sprite-run-batch "sprite-batch")
(declare-function sprite-state-path "sprite")
(declare-function sprite-instance-name "sprite")
(declare-function elpaish-install-packages "elpaish-install")
(declare-function elpaish-install-refresh-package-quickstart "elpaish-install")
(declare-function elpaish-install-package-quickstart-stale-p "elpaish-install")

;; Dynamic variables configured here or populated by other packages
(defvar package-quickstart-file)
(defvar sprite-state-locks-enabled)
(defvar sprite-state-auto-save-enabled)
(defvar sprite-state-desktop-enabled)
(defvar sprite-state-mappings)
(defvar sprite-startup-operations)
(defvar sprite-first-frame-operations)
(defvar sprite-instance-id)
(defvar mcpkit-instance-name-function)
(defvar cmdr-opt-in-specs)
(defvar bookmark-save-flag)
(defvar treesit-auto-langs)
(defvar org-directory)
(defvar org-agenda-files)
(defvar org-annotate-file-storage-file)
(defvar org-default-notes-file)
(defvar org-archive-location)
(defvar deft-directory)
(defvar denote-directory)

;; Org and note paths populated by `bootstrap-set-local-notes-directory'
(defvar local-notes-directory nil
  "Directory where notes are stored.")
(defvar user-org-directories nil
  "Extra directories containing org files.")

(defvar bootstrap-startup-complete-time nil
  "Timestamp when startup finished.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deferred Module Autoloads

(autoload 'tychoish-mail-select-account "setup-mail" nil t)
(autoload 'tychoish-define-mail-account "setup-mail")
(autoload 'tychoish-mail-add-refile-rule "setup-mail")
(autoload 'mu4e "setup-mail" nil t)
(autoload 'mu4e-update-index "setup-mail" nil t)
(autoload 'mu4e-update-mail-and-index "setup-mail" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Bootstrap

(defun bootstrap-initialize-packages ()
  "Initialize packages from quickstart file, or perform cold bootstrap."
  (let ((elc (concat (file-name-sans-extension package-quickstart-file) ".elc")))
    (cond
     ((file-exists-p elc)
      (load elc t t t t))
     ((file-exists-p package-quickstart-file)
      (load package-quickstart-file t t t t))
     (t
      (package-activate-all)
      (when (elpaish-install-package-quickstart-stale-p)
        (elpaish-install-packages 'core))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state -- setup desktop/bookmarks/savehist
;; hooks -- these run incrementally and lazily by sprite in `after-init-hook'

(setq sprite-state-locks-enabled t)
(setq sprite-state-auto-save-enabled t)
(setq sprite-state-desktop-enabled t)
(setq bookmark-save-flag 1)
(setq mcpkit-instance-name-function #'sprite-instance-name)
(setq cmdr-opt-in-specs '(defaults))

(setq sprite-state-mappings
      '((project-list-file . "projects.el")
        (bookmark-default-file . "bookmarks.el")
        (tramp-persistency-file-name . "tramp.el")
        (transient-history-file . "transient-history.el")
        (transient-values-file . "transient-values.el")
        (eshell-history-file-name . "eshell")
        (shell-maker-root-path . "shell-maker")
        (auto-save-list-file-prefix . "auto-save-list/")
        (request-storage-directory . "request/")
        (url-configuration-directory . "url/")
        (denote-sync-cache-directory . "denote-sync/")
        (recentf-save-file . "recentf.el")
        (savehist-file . "history.el")
        (dape-default-breakpoints-file . "dape-breakpoints.el")
        (projectile-known-projects-file . "projectile-bookmarks.eld")
        (projectile-frecency-file . "projectile-frecency.eld")))

(setq sprite-startup-operations
  '(bootstrap-set-up-fringe-mode
    overmark-global-mode
    column-number-mode
    delete-selection-mode
    winner-mode
    electric-pair-mode
    pixel-scroll-precision-mode
    xterm-mouse-mode
    repeat-mode
    cmdr-mode
    cmdr-frame-mode
    sprite-mode
    which-key-mode
    eglox-global-mode
    minaid-mode
    telegax-global-mode
    sprite-state-configure
    bootstrap-silent-savehist
    bootstrap-silent-recentf
    popper-mode
    grepx-mode
    bootstrap-require-treesit-auto
    bootstrap-treesit-auto
    bootstrap-require-setup-project
    global-treesit-auto-mode
    projectile-mode
    denote-dash-global-mode
    denote-tree-mode
    magit-dash-global-mode
    builder-mode
    chex-global-mode
    nerd-icons-completion-mode
    orgx-global-mode
    global-auto-revert-mode
    orgx-capture-setup-standard-templates
    annotated-completing-read-setup-history
    mcpkit-init-services
    gptel-mcp-mode
    sprite-state-global-mode
    bootstrap-schedule-maintenance
    bootstrap-startup-timing))

(setq sprite-first-frame-operations
  '(bootstrap-ensure-default-font
    ensure-frame-opacity
    disable-all-themes
    bootstrap-default-theme
    hud-modeline-mode
    bootstrap-frame-startup-timing))

(defvar bootstrap-package-maintenance
  '(bootstrap-require-setup-packages
    bootstrap-ensure-packages-installed
    bootstrap-package-quickstart
    builder-emacs-conf-native-compile-all)
  "Operations to run after startup to maintain")

(defun bootstrap-run-startup () (sprite-batch-startup :delay 0.05))
(defun bootstrap-schedule-maintenance () (sprite-run-batch :run-after 300 :operations bootstrap-package-maintenance))
(defun bootstrap-require-setup-project () (require 'setup-project))
(defun bootstrap-require-setup-packages () (require 'setup-packages))
(defun bootstrap-require-treesit-auto () (require 'treesit-auto))
(defun bootstrap-silent-savehist () (let ((inhibit-message t) (message-log-max nil)) (savehist-mode 1)))
(defun bootstrap-silent-recentf () (let ((inhibit-message t) (message-log-max nil)) (recentf-mode 1)))
(defun bootstrap-package-quickstart () (elpaish-install-refresh-package-quickstart nil))
(defun bootstrap-setup-custom-file () (setq custom-file (sprite-state-path "custom.el")))
(defun bootstrap-set-up-fringe-mode () (set-fringe-mode '(4 . 4)))
(defun bootstrap-ensure-packages-installed () (elpaish-install-packages :async t))
(defun bootstrap-after-first-frame () (sprite-batch-startup sprite-first-frame-operations :frame (selected-frame)))
(defun bootstrap-startup-mark-complete () (setq bootstrap-startup-complete-time (or bootstrap-startup-complete-time (current-time))))
(defun bootstrap-ensure-default-font () (ensure-frame-font "Source Code Pro" 13))
(defun bootstrap-default-theme () (when (load-theme 'modus-operandi t t) (enable-theme 'modus-operandi)))
(defun bootstrap-startup-timing () (message "[emacs]: <%s> lazy init time %s" sprite-instance-id (float-time (time-since before-init-time))))
(defun bootstrap-frame-startup-timing () (message "[emacs]: <%s> frame init time %s" sprite-instance-id (float-time (time-since before-init-time))))
(defun bootstrap-treesit-auto () (setq treesit-auto-langs (delete 'yaml treesit-auto-langs)))

(add-hook 'after-init-hook #'cmdr-spec-discover)
(add-hook 'after-init-hook #'bootstrap-setup-custom-file -90)
(add-hook 'emacs-startup-hook #'bootstrap-run-startup)
(add-hook 'emacs-startup-hook #'bootstrap-startup-mark-complete 99)
(add-hook 'window-setup-hook #'bootstrap-after-first-frame)
(add-hook 'window-setup-hook #'bootstrap-report-startup-timing 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions -- initialization

(defun bootstrap-load-user-local-config ()
  "Load all .el files in PATH."
  (let ((path (expand-file-name "user" user-emacs-directory)))
    (add-to-list 'load-path path)

    (dolist (file (directory-files path :full "\\.el$") nil)
      (let ((name (file-name-sans-extension (file-name-nondirectory file))))
	(require (intern name))))))

(defun bootstrap-report-startup-timing ()
  (let ((startup-time (float-time (time-subtract bootstrap-startup-complete-time before-init-time)))
	(init-time (float-time (time-subtract after-init-time before-init-time)))
	(wall-time (float-time (time-since before-init-time))))
    (message "[emacs]: <%s> init time %s" sprite-instance-id init-time)
    (message "[emacs]: <%s> user time %s" sprite-instance-id startup-time)
    (message "[emacs]: <%s> wall time %s" sprite-instance-id wall-time)
    (when (fboundp 'alert)
      (alert (format "started (pid=%d) [init=%s user=%s wall=%s]" (emacs-pid) init-time startup-time wall-time)
	     :title (format "emacs-%s" sprite-instance-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; higher order functions -- probably belong somewhere else, but here for now

(defun bootstrap-set-notes-directory (&optional path)
  "Set notes directory to PATH and update note paths."
  (interactive
   (list (read-directory-name "Notes directory: " (or local-notes-directory "~/notes"))))
  (when path
    (setq local-notes-directory (expand-file-name path)))
  (unless local-notes-directory
    (error "Must define `local-notes-directory'"))
  (setq org-directory (file-name-concat local-notes-directory "org"))
  (setq org-agenda-files (thread-last (list org-directory user-org-directories)
                                      (flatten-tree)
                                      (seq-map #'expand-file-name)
                                      (seq-filter #'identity)
                                      (seq-map #'string-trim)
                                      (seq-remove #'string-empty-p)
                                      (seq-uniq)))
  (setq org-annotate-file-storage-file (file-name-concat org-directory "records.org"))
  (setq org-default-notes-file (file-name-concat org-directory "records.org"))
  (setq org-archive-location (file-name-concat org-directory "archive/%s::datetree/"))
  (setq deft-directory (file-name-concat local-notes-directory "deft"))
  (setq denote-directory (file-name-concat local-notes-directory "denote"))
  local-notes-directory)

;; ssh-agent -- tools to make sure emacs session can connect to ssh-agent
(defun bootstrap-ensure-ssh-agent ()
  "Ensure SSH agent is configured and remove hooks."
  (bootstrap-set-up-ssh-agent)
  (remove-hook 'eat-mode-hook #'bootstrap-ensure-ssh-agent)
  (remove-hook 'magit-mode-hook #'bootstrap-ensure-ssh-agent)
  (remove-hook 'telega-root-mode-hook #'bootstrap-ensure-ssh-agent))

(add-hook 'eat-mode-hook #'bootstrap-ensure-ssh-agent)
(add-hook 'magit-mode-hook #'bootstrap-ensure-ssh-agent)
(add-hook 'telega-root-mode-hook #'bootstrap-ensure-ssh-agent)

(defun find-ssh-agent-socket-candidates ()
  (thread-last
    (directory-files temporary-file-directory t ".*ssh-.*/agent.*" :nosort)
    (append (list (format "/run/user/%d/ssh-agent.socket" (user-uid))))
    (seq-remove #'null)
    (seq-uniq)
    (seq-filter #'file-writable-p)
    (seq-sort #'string-lessp)
    (nreverse)))

(defun bootstrap-set-up-ssh-agent ()
  (interactive)
  (let (env-value sockets)
    (unless (setq env-value (getenv "SSH_AUTH_SOCK"))
      (setq sockets (find-ssh-agent-socket-candidates))
      (when (and sockets
		 (<= 1 (length sockets)))
	(setq env-value (setenv "SSH_AUTH_SOCK" (car sockets)))))
    env-value))

(provide 'bootstrap)
;;; bootstrap.el ends here
