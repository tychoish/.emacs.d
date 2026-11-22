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
(declare-function package-installed-p "package")
(declare-function package-install "package")
(declare-function sprite-batch-startup "sprite-batch")
(declare-function sprite-run-batch "sprite-batch")
(declare-function magit-insert-worktrees "magit-worktree")
(declare-function magit-insert-modules "magit-submodule")
(declare-function sprite-state-path "sprite")
(declare-function sprite-state-configure "sprite-state")
(declare-function sprite-instance-name "sprite")
(declare-function elpaish-install-packages "elpaish-install")
(declare-function elpaish-install-refresh-package-quickstart "elpaish-install")
(declare-function cmdr-spec-discover "cmdr-spec")
(declare-function cmdr-spec-install "cmdr-spec")
(declare-function builder-elisp-enable-ert-advice "builder-elisp")
(declare-function agent-shell-chat--relabel-all "agent-shell-chat-mode")

;; Dynamic variables configured here or populated by other packages
(defvar package-quickstart-file)
(defvar sprite-state-locks-enabled)
(defvar sprite-state-auto-save-enabled)
(defvar sprite-state-desktop-enabled)
(defvar sprite-state-desktop-deferred)
(defvar sprite-state-desktop-restore-style)
(defvar sprite-state-recentf-enabled)
(defvar sprite-state-mappings)
(defvar sprite-startup-operations)
(defvar sprite-first-frame-operations)
(defvar sprite-instance-id)
(defvar mcpkit-instance-name-function)
(defvar cmdr-opt-in-specs)
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

(defvar bootstrap-package-maintenance nil
  "Operations run after startup for package maintenance.")

(defvar bootstrap-enabled-commands
  '(downcase-region
    narrow-to-region
    upcase-region
    list-timers
    list-threads
    dired-find-alternate-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deferred Module Autoloads

(defvar mailx-refile-rules nil)

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
      (unless (package-installed-p 'elpaish)
        (package-install 'elpaish))
      (require 'setup-packages)
      (elpaish-install-packages :sync t)))))

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
    (message "[emacs]: <%s> wall time %s" sprite-instance-id wall-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init configuration

(setq sprite-mode-map-prefix '("C-c t z" . cmdr-mode-map))
(setq sprite-state-locks-enabled t)
(setq sprite-state-auto-save-enabled t)
(setq sprite-state-desktop-enabled t)
(setq sprite-state-desktop-deferred t)
(setq sprite-state-desktop-restore-style 'staged)
(setq sprite-state-recentf-enabled t)
(setq mcpkit-instance-name-function #'sprite-instance-name)
(setq cmdr-opt-in-specs '(defaults windows display browser docs buffers))

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq select-enable-clipboard nil)
(setq shell-command-dont-erase-buffer 'end-last-out)
(setq jit-lock-defer-time 0.2)
(setq read-process-output-max 1048576) ;; 1MB

(setq-default indicate-empty-lines t)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default cursor-in-non-selected-windows nil)

(setq scroll-conservatively 25)
(setq scroll-preserve-screen-position t)
(setq window-sides-vertical t)
(setq ring-bell-function #'ignore)
(setq switch-to-prev-buffer-skip 'visible)
(setq split-height-threshold 100)

(setq alert-log-messages nil)
(setq alert-fade-time 15)

(setq ns-function-modifier 'hyper) ;; darwin
(setq ns-command-modifier 'meta) ;; darwin
(setq ns-option-modifier 'super) ;; darwin

(add-to-list 'term-file-aliases '("alacritty" . "xterm"))
(add-to-list 'term-file-aliases '("ghostty" . "xterm-ghostty"))

(advice-add 'display-startup-echo-area-message :override #'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; third-party package configuration

(setq emojify-display-style 'unicode)
(setq afp-fill-comments-only-mode-list '(prog-mode))

(setq denote-dash-default-filter '(not "journal"))
(setq denote-known-keywords '("project" "reference" "journal" "idea" "writing" "agent" "plan"))

;; thinking of dropping journal/explore/review
(setq denote-journal-title-format nil)
(setq denote-explore-network-format "graphviz")
(setq denote-review-insert-after "date")

(setq agent-shell-header-style 'text)
(setq agent-shell-anthropic-default-session-mode-id "auto")
(setq agent-shell-prefer-session-resume nil) ;; prefer session/load (replay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state -- setup desktop/bookmarks/savehist
;; hooks -- these run incrementally and lazily by sprite in `after-init-hook'

(setq sprite-state-mappings
      '((project-list-file . "projects.el")
        (bookmark-default-file . "bookmarks.el")
        (tramp-persistency-file-name . "tramp.el")
        (transient-history-file . "transient-history.el")
        (transient-values-file . "transient-values.el")
        (recentf-save-file . "recentf.el")
        (savehist-file . "history.el")
        (dape-default-breakpoints-file . "dape-breakpoints.el")
        (code-review-db-database-file . "code-review/db.sqlite")
        (shell-maker-root-path . "shell-maker/")
        (pcache-directory . "pcache/")
        (agent-shell-queue-state-directory . "agent-shell/")
        (agent-shell-queue-safe-save-directory . "agent-shell/backups/")
        (auto-save-list-file-prefix . "auto-save-list/")
        (hitl-plan-directory . "hitl/")
        (magit-dash-prune-cache-dir . "magit-dash-gh-prune/")
        (mcpkit-stats-file . "mcpkit-stats.eld")
        (request-storage-directory . "request/")
        (url-configuration-directory . "url/")
        (denote-sync-cache-directory . "denote-sync/")
        (code-review-log-file . "code-review/error.log")
        (eshell-history-file-name . "eshell")
        (projectile-known-projects-file . "projectile-bookmarks.eld")
        (projectile-frecency-file . "projectile-frecency.eld")))

(setq sprite-state-mode-hooks
      '((dired-mode-hook . nerd-icons-dired-mode)
        (sqlite-mode-hook . sqlite-extras-minor-mode)
        (LaTeX-mode-hook . (turn-on-reftex visual-line-mode turn-off-auto-fill))
        (popper-mode-hook . popper-echo-mode)
        (org-mode-hook . aggressive-fill-paragraph-mode)
        (markdown-mode-hook . aggressive-fill-paragraph-mode)
        (magit-status-mode-hook . (magit-dash-prune-lazy-prefetch
                                   bootstrap-magit-status-add-modules
                                   bootstrap-magit-status-add-worktrees))
        (agent-shell-mode-hook . (agent-shell-bold-input-setup
                                  ansi-color-for-comint-mode-on
                                  agent-shell-notifications-mode
                                  agent-shell-queue-overload-mode))
        (agent-shell-viewport-edit-mode-hook . agent-shell-notifications-viewport-edit-mode)
        (agent-shell-viewport-view-mode-hook . agent-shell-notifications-viewport-view-mode)
        (elpaish-install-bootstrap-finished-functions . bootstrap-cmdr-spec-discover)
        (sprite-state-reload-hook . cmdr-spec-discover)
        (erc-mode-hook . bootstrap-require-tracking)
        (eat-mode-hook . bootstrap-ensure-ssh-agent)
        (magit-mode-hook . (bootstrap-ensure-ssh-agent
                            bootstrap-enable-magit-diff-edit-hunk))
        (cmdr-spec-agent-shell-activate-hook . agent-shell-queue-global-mode)
        (cmdr-spec-gptel-activate-hook . setup-core-gptel-activate)))

(setq sprite-state-auto-mode-alist
      '(("\\.xml\\'" . nxml-mode)
        ("\\.rst\\'" . rst-mode)
        ("\\.tex\\'" . LaTeX-mode)
        ("\\.service\\'" . conf-unix-mode)
        ("\\.timer\\'" . conf-unix-mode)
        ("\\.target\\'" . conf-unix-mode)
        ("\\.mount\\'" . conf-unix-mode)
        ("\\.automount\\'" . conf-unix-mode)
        ("\\.slice\\'" . conf-unix-mode)
        ("\\.socket\\'" . conf-unix-mode)
        ("\\.path\\'" . conf-unix-mode)
        ("\\.conf\\'" . conf-unix-mode)
        ("\\.mdwn\\'" . markdown-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode)
        ("\\.txt\\'" . markdown-mode)
        (".*mutt.*" . message-mode)))

(setq sprite-first-frame-operations
  '(bootstrap-ensure-default-font
    ensure-frame-opacity
    disable-all-themes
    bootstrap-default-theme
    hud-modeline-mode
    bootstrap-report-startup-timing))

(setq sprite-startup-operations
  '(;; State paths, primary keymaps, and completion history come first.
    sprite-state-configure
    bootstrap-set-up-fringe-mode
    bootstrap-enable-commands
    (:mode cmdr-mode)
    (:mode cmdr-frame-mode)
    (:mode sprite-mode)
    (:mode sprite-state-global-mode)
    (:mode minaid-mode)
    annotated-completing-read-setup-history
    (:mode nerd-icons-completion-mode)
    (:mode column-number-mode)
    (:mode delete-selection-mode)
    (:mode xterm-mouse-mode)
    (:mode popper-mode)
    (:mode winner-mode)
    (:mode overmark-global-mode)

    ;; Programming and project integrations before restoring buffers.
    bootstrap-treesit-auto
    (:mode global-treesit-auto-mode)
    (:mode eglox-global-mode)
    (:mode chex-global-mode)
    (:mode builder-mode)
    (:mode grepx-mode)
    (:mode magit-dash-global-mode)

    ;; Note-taking and agent integrations precede desktop restoration.
    (:mode orgx-global-mode)
    (:mode mailx-global-mode)
    (:mode denote-dash-global-mode)
    (:mode denote-rename-buffer-mode)
    (:mode denote-tree-mode)
    (:mode consult-notes-denote-mode)
    (:mode mcpkit-agent-shell-mode)
    (:mode mcpkit-gptel-mode)
    mcpkit-init-services
    (:mode telegax-global-mode)
    (:mode global-auto-revert-mode)
    sprite-state-start-desktop
    bootstrap-setup-builder
    ;; Refresh discovery after integrations load; defer package maintenance.
    cmdr-spec-discover
    bootstrap-alert
    bootstrap-schedule-maintenance
    bootstrap-startup-timing))

(setq bootstrap-package-maintenance
      '(bootstrap-require-setup-packages
        bootstrap-ensure-packages-installed
        bootstrap-package-quickstart
        sprite-state-reload
        builder-emacs-conf-native-compile-all))

(defun bootstrap-require-setup-packages () (require 'setup-packages))
(defun bootstrap-require-tracking () (require 'tracking))
(defun bootstrap-treesit-auto () (require 'treesit-auto) (setq treesit-auto-langs (delete 'yaml treesit-auto-langs)))
(defun bootstrap-setup-custom-file () (setq custom-file (sprite-state-path "custom.el")))
(defun bootstrap-package-quickstart () (elpaish-install-refresh-package-quickstart nil))
(defun bootstrap-ensure-packages-installed () (elpaish-install-packages :async t))
(defun bootstrap-cmdr-spec-discover (&rest _) (cmdr-spec-discover))
(defun bootstrap-ensure-default-font () (ensure-frame-font "Source Code Pro" 13))
(defun agent-shell-bold-input-setup () (face-remap-add-relative 'comint-highlight-input :weight 'bold))
(defun bootstrap-set-up-fringe-mode () (set-fringe-mode '(4 . 4)))
(defun bootstrap-startup-mark-complete () (setq bootstrap-startup-complete-time (or bootstrap-startup-complete-time (current-time))))
(defun bootstrap-startup-timing () (message "[emacs]: <%s> lazy init time %s" sprite-instance-id (float-time (time-since before-init-time))))
(defun bootstrap-magit-status-add-worktrees () (add-hook 'magit-status-sections-hook #'magit-insert-worktrees t))
(defun bootstrap-magit-status-add-modules () (add-hook 'magit-status-sections-hook #'magit-insert-modules t))
(defun bootstrap-enable-commands () (dolist (sym bootstrap-enabled-commands) (put sym 'disabled nil)))
(defun bootstrap-enable-magit-diff-edit-hunk () (put 'magit-diff-edit-hunk-commit 'disabled nil))
(defun bootstrap-after-first-frame () (sprite-batch-startup sprite-first-frame-operations :delay 0.05 :frame (selected-frame)))
(defun bootstrap-run-startup () (sprite-batch-startup :delay 0.05))
(defun bootstrap-schedule-maintenance () (sprite-run-batch :run-after 300 :operations bootstrap-package-maintenance))
(defun bootstrap-setup-builder () (advice-add 'builder--get-candidates :before #'bootstrap-load-builder-tasks))
(defun bootstrap-load-builder-tasks (&rest _) (require 'builder-tasks) (advice-remove 'builder--get-candidates #'bootstrap-load-builder-tasks))

(add-hook 'after-init-hook #'cmdr-spec-discover)
(add-hook 'after-init-hook #'bootstrap-setup-custom-file -90)
(add-hook 'emacs-startup-hook #'bootstrap-run-startup)
(add-hook 'emacs-startup-hook #'bootstrap-startup-mark-complete 99)
(add-hook 'window-setup-hook #'bootstrap-after-first-frame)

(add-to-list 'major-mode-remap-alist '(yaml-ts-mode . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; higher order functions -- probably belong somewhere else, but here for now

(defun bootstrap-default-theme ()
  (setq modus-themes-deuteranopia t)
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (message-separator bg-main)))

  (when (load-theme 'modus-operandi t t)
    (enable-theme 'modus-operandi)))

(defun bootstrap-alert ()
  (setq alert-default-style
        (cond
         ((eq system-type 'darwin)
          (if (display-graphic-p) 'osx-notifier 'notifier))
         ((executable-find "notify-send") 'libnotify)
         ((and (eq system-type 'gnu/linux) (getenv "DBUS_SESSION_BUS_ADDRESS"))
          'notifications)
         (t
          'message))))

(defun setup-core-gptel-activate ()
  "Configure site-specific backends and model defaults for gptel."
  (setq gptel-include-reasoning 'ignore)
  (setq-default gptel-backend (gptel-make-openai-oauth "codex"))
  (setq-default gptel-model 'gpt-6.1-terra)
  (gptel-make-gh-copilot "gh-copilot"))

(declare-function orgx-set-notes-directory "orgx" (&optional path))

(defun bootstrap-set-notes-directory (&optional path)
  "Set note directories using PATH, including Denote extensions."
  (interactive
   (list (read-directory-name "Notes directory: " (or local-notes-directory "~/notes"))))
  (orgx-set-notes-directory path)
  (setq deft-directory (file-name-concat local-notes-directory "deft")
        denote-directory (file-name-concat local-notes-directory "denote"))
  (setq denote-journal-directory (file-name-concat denote-directory "journal"))
  (setq denote-explore-network-directory (file-name-concat denote-directory "explore"))
  local-notes-directory)

;; ssh-agent -- tools to make sure emacs session can connect to ssh-agent
(defun bootstrap-ensure-ssh-agent ()
  "Ensure SSH agent is configured and remove hooks."
  (bootstrap-set-up-ssh-agent)
  (remove-hook 'eat-mode-hook #'bootstrap-ensure-ssh-agent)
  (remove-hook 'magit-mode-hook #'bootstrap-ensure-ssh-agent))

(defun bootstrap-set-up-ssh-agent ()
  (interactive)
  (let (env-value sockets)
    (unless (setq env-value (getenv "SSH_AUTH_SOCK"))
      (setq sockets (thread-last
                      (directory-files temporary-file-directory t ".*ssh-.*/agent.*" :nosort)
                      (append (list (format "/run/user/%d/ssh-agent.socket" (user-uid))))
                      (seq-remove #'null)
                      (seq-uniq)
                      (seq-filter #'file-writable-p)
                      (seq-sort #'string-greaterp))))
    (when (length> sockets 0)
      (setq env-value (setenv "SSH_AUTH_SOCK" (car sockets))))
      env-value))

(provide 'bootstrap)
;;; bootstrap.el ends here
