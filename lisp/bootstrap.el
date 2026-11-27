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

(autoload 'treesit-auto-mode "treesit-auto")

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
(declare-function elpaish-install-register-packages "elpaish-install" (&rest pkgs))
(declare-function elpaish-install-register-groups "elpaish-install" (groups))
(declare-function nerd-icons-completion-mode "nerd-icons-completion")

;; Dynamic variables configured here or populated by other packages
(defvar package-quickstart-file)
(defvar package-quickstart)
(defvar package-user-dir)
(defvar sprite-state-enabled-commands)
(defvar sprite-state-locks-enabled)
(defvar sprite-state-auto-save-enabled)
(defvar sprite-state-desktop-enabled)
(defvar sprite-state-desktop-deferred)
(defvar sprite-state-desktop-restore-style)
(defvar sprite-state-recentf-enabled)
(defvar sprite-state-mappings)
(defvar sprite-startup-operations)
(defvar sprite-instance-id)
(defvar mcpkit-instance-name-function)
(defvar cmdr-opt-in-specs)
(defvar treesit-auto-langs)

(defvar bootstrap-startup-complete-time nil
  "Timestamp when startup finished.")

(defvar bootstrap-end-to-end-time nil
  "Timestamp when all startup completed.")

(defvar bootstrap-package-maintenance nil
  "Operations run after startup for package maintenance.")

(defvar bootstrap-packages nil
  "Alist mapping package group names to lists of package symbols.
Groups other than `install' (or `install-only') are registered for continuous
upgrades via `elpaish-install-register-upgrade-package'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Bootstrap

(defvar bootstrap--quickstart-rebuild-needed nil
  "Non-nil when a failed quickstart load requires a forced rebuild.")

(defun bootstrap-initialize-packages ()
  "Initialize packages from quickstart file, or perform cold bootstrap."
  (unless (condition-case nil
              (let ((load-prefer-newer nil))
                (load (file-name-sans-extension package-quickstart-file) t t))
            (error
             (setq bootstrap--quickstart-rebuild-needed t)
             (let ((package-quickstart nil)
                   (package-quickstart-file
                    (make-temp-name (expand-file-name "emacs-pkg-" temporary-file-directory))))
               (package-activate-all))
             t))
    (package-activate-all)
    (unless (package-installed-p 'elpaish)
      (package-install 'elpaish))
    (bootstrap-register-packages)
    (elpaish-install-packages :sync t)))

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
	(wall-time (float-time (setq bootstrap-end-to-end-time (or bootstrap-end-to-end-time
                                                                   (time-since before-init-time)))))
        (inhibit-message t))
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
(setq sprite-state-enabled-commands
      '(downcase-region
        narrow-to-region
        upcase-region
        list-timers
        list-threads
        dired-find-alternate-file))

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

(setopt find-file-visit-truename t)

(setq window-sides-vertical t)
(setq ring-bell-function #'ignore)

(setq alert-log-messages nil)
(setq alert-fade-time 15)

(setq ns-function-modifier 'hyper) ;; darwin
(setq ns-command-modifier 'meta) ;; darwin
(setq ns-option-modifier 'super) ;; darwin

(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

(add-to-list 'term-file-aliases '("alacritty" . "xterm"))
(add-to-list 'term-file-aliases '("ghostty" . "xterm-ghostty"))

(advice-add 'display-startup-echo-area-message :override #'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; third-party package configuration

(setq emojify-display-style 'unicode)
(setq denote-dash-default-filter '(not "journal"))
(setq denote-known-keywords '("project" "reference" "journal" "idea" "writing" "agent" "plan"))
(setq denote-explore-network-format "graphviz")

(setq diff-review-command-prefix "C-c -")
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
        (org-id-locations-file . "org-id-locations.eld")
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
        (orgx-bulk-directory . "orgx-bulk")
        (url-configuration-directory . "url/")
        (denote-sync-cache-directory . "denote-sync/")
        (code-review-log-file . "code-review/error.log")
        (eshell-history-file-name . "eshell")
        (projectile-known-projects-file . "projectile-bookmarks.eld")
        (projectile-frecency-file . "projectile-frecency.eld")))

(setq sprite-state-mode-hooks
      '((dired-mode-hook . nerd-icons-dired-mode)
        (minibuffer-setup-hook . bootstrap-enable-nerd-icons-completion)
        (sqlite-mode-hook . sqlite-extras-minor-mode)
        (LaTeX-mode-hook . (turn-on-reftex visual-line-mode turn-off-auto-fill))
        (prog-mode-hook . (overmark-global-mode treesit-auto-mode))
        (text-mode-hook . (overmark-global-mode))
        (special-mode-hook . popper-mode)
        (compilation-mode . popper-mode)
        (popper-mode-hook . popper-echo-mode)
        (org-mode-hook . (orgx-fill-guard-mode aggressive-fill-paragraph-mode))
        (markdown-mode-hook . (orgx-fill-guard-mode aggressive-fill-paragraph-mode))
        (magit-status-mode-hook . (magit-dash-prune-lazy-prefetch
                                   bootstrap-magit-status-add-modules
                                   bootstrap-magit-status-add-worktrees))
        (agent-shell-mode-hook . (agent-shell-bold-input-setup
                                  ansi-color-for-comint-mode-on
                                  agent-shell-queue-ask-steering-mode
                                  agent-shell-notifications-mode
                                  agent-shell-queue-overload-mode))
        (agent-shell-viewport-edit-mode-hook . agent-shell-notifications-viewport-edit-mode)
        (agent-shell-viewport-view-mode-hook . agent-shell-notifications-viewport-view-mode)
        (elpaish-install-after-package-install-hook . cmdr-spec-discover)
        (sprite-state-global-mode-hook . annotated-completing-read-setup-history)
        (sprite-state-reload-hook . cmdr-spec-discover)
        (cmdr-frame-mode-hook . (bootstrap-set-up-fringe-mode
                                 xterm-mouse-mode))
        (hud-modeline-mode-hook . (column-number-mode))
        (erc-mode-hook . bootstrap-require-tracking)
        (eat-mode-hook . bootstrap-ensure-ssh-agent)
        (magit-mode-hook . (bootstrap-ensure-ssh-agent
                            bootstrap-enable-magit-diff-edit-hunk))
        (cmdr-spec-agent-shell-activate-hook . (agent-shell-queue-global-mode
                                                mcpkit-agent-shell-mode
                                                mcpkit-init-services))
        (cmdr-spec-gptel-activate-hook . mcpkit-gptel-mode)
        (cmdr-spec-denote-activate-hook . (denote-tree-mode
                                           consult-notes-denote-mode
                                           denote-rename-buffer-mode))
        (cmdr-spec-mailx-activate-hook . mailx-global-mode)
        (compilation-mode-hook . builder-mode)))

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

(setq sprite-startup-operations
  '(;; Appearance is configured before the rest of the startup batch.
    bootstrap-default-theme

    ;; State paths, primary keymaps, and completion history come next.
    sprite-state-configure
    (:mode cmdr-mode)
    (:mode cmdr-frame-mode)
    (:op cmdr-spec-discover)
    (:mode sprite-state-global-mode)

    (:mode delete-selection-mode)
    (:mode winner-mode)

    ;; Programming and project integrations before restoring buffers.
    (:mode eglox-global-mode)
    (:mode chex-global-mode)

    ;; Note-taking and core integrations precede desktop restoration.
    (:mode orgx-global-mode)
    (:mode global-auto-revert-mode)
    sprite-state-start-desktop
    (:mode sprite-mode)
    (:mode minaid-mode)
    bootstrap-setup-builder
    bootstrap-alert
    bootstrap-schedule-maintenance
    (:mode hud-modeline-mode)))

(setq bootstrap-package-maintenance
      '(bootstrap-register-packages
        bootstrap-ensure-packages-installed
        bootstrap-package-quickstart
        sprite-state-reload
        builder-emacs-conf-native-compile-all))

(setq bootstrap-packages
      '((agent-shell-libs . (acp shell-maker))
        (agent-shell . (agent-shell agent-shell-notifications agent-shell-queue agent-shell-workflow))
        (emax-core-libs . (sprite annotated-completing-read clix logx gen  xtdlib mcpkit))
        (text-rendering . (overmark modus-themes))
        (emax-applications . (arch builder magit-dash elpaish elpaish-keyring hitl telega-bot hud-modeline))
        (deltas . (magit code-review diff-review))
        (dev-tools . (eglot-test-at-point org-docsgen))
        (emax-customization . (orgx grepx cmdr mailx minaid eglox chex telegax))
        (robot-tools . (gptel-pup tailscale ollama-tailnet sprite-mcp gptel gptel-agent gptel-aibo))
        (completion . (cape orderless marginalia consult corfu tempel vertico))
        (checking . (flycheck flycheck-aspell flycheck-vale flyspell-correct consult-flyspell consult-flycheck))
        (denote . (denote denote-convert denote-dash denote-mcp denote-sync denote-tree))
        ;; the following are not upgraded in the usual upgrade batch
        (install-libs . (async alert wgrep treesit-auto))
        (install-apps . (eat telega dape docker ollama journalctl-mode slime popper anzu))
        (install-org . (ob-go ob-mermaid ob-rust ox-gist ox-hugo ox-leanpub ox-rst org-contrib toc-org))
        (install-icons . (emojify nerd-icons nerd-icons-corfu nerd-icons-completion nerd-icons-xref nerd-icons-dired))
        (install-completion . (consult-denote consult-eglot embark-consult eglot-tempel tempel-collection))
        (install-denote . (denote-explore denote-journal-capture denote-markdown denote-sequence denote-silo))
        (install-git . (gist git-link magit-gh revbufs))
        (install-handlers . (yaml-pro pkgbuild-mode protobuf-mode cov undercover ninja-mode))
        (install . (aggressive-fill-paragraph))))

(defun bootstrap-require-tracking () (require 'tracking))
(defun bootstrap-treesit-auto () (require 'treesit-auto) (setq treesit-auto-langs (delete 'yaml treesit-auto-langs)))
(defun bootstrap-setup-custom-file () (setq custom-file (sprite-state-path "custom.el")))
(defun bootstrap-ensure-packages-installed () (elpaish-install-packages :async t))
(defun bootstrap-register-packages () (elpaish-install-register-groups bootstrap-packages))
(defun agent-shell-bold-input-setup () (face-remap-add-relative 'comint-highlight-input :weight 'bold))
(defun bootstrap-set-up-fringe-mode () (set-fringe-mode (if (equal sprite-instance-id "hud") 0 '(4 . 4))))
(defun bootstrap-startup-mark-complete () (setq bootstrap-startup-complete-time (or bootstrap-startup-complete-time (current-time))))
(defun bootstrap-magit-status-add-worktrees () (add-hook 'magit-status-sections-hook #'magit-insert-worktrees t))
(defun bootstrap-magit-status-add-modules () (add-hook 'magit-status-sections-hook #'magit-insert-modules t))
(defun bootstrap-enable-magit-diff-edit-hunk () (put 'magit-diff-edit-hunk-commit 'disabled nil))
(defun bootstrap-enable-nerd-icons-completion () (remove-hook 'minibuffer-setup-hook #'bootstrap-enable-nerd-icons-completion) (nerd-icons-completion-mode 1))
(defun bootstrap-schedule-maintenance () (unless noninteractive (sprite-run-batch :run-after 300 :operations bootstrap-package-maintenance)))
(defun bootstrap-setup-builder () (advice-add 'builder--get-candidates :before #'bootstrap-load-builder-tasks))
(defun bootstrap-load-builder-tasks (&rest _) (require 'builder-tasks) (advice-remove 'builder--get-candidates #'bootstrap-load-builder-tasks))
(defun bootstrap-run-startup () (add-to-list 'sprite-startup-operations 'bootstrap-report-startup-timing t) (sprite-batch-startup :delay 0))

(defun bootstrap-package-quickstart ()
  (when (elpaish-install-refresh-package-quickstart (bootstrap-quickstart-is-stale-p))
    (setq bootstrap--quickstart-rebuild-needed nil)))

(defun bootstrap-quickstart-is-stale-p ()
  (or bootstrap--quickstart-rebuild-needed
      (and (file-directory-p package-user-dir)
            (file-exists-p package-quickstart-file)
            (file-newer-than-file-p package-user-dir package-quickstart-file))))

(add-hook 'after-init-hook #'bootstrap-setup-custom-file -90)
(add-hook 'emacs-startup-hook #'bootstrap-startup-mark-complete 97)
(add-hook 'emacs-startup-hook #'bootstrap-run-startup 98)
(add-to-list 'major-mode-remap-alist '(yaml-ts-mode . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; higher order functions -- probably belong somewhere else, but here for now

(defun bootstrap-default-theme ()
  (setq modus-themes-deuteranopia t)
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (message-separator bg-main)))

  (modus-themes-select 'modus-operandi))

(defun bootstrap-alert ()
  (setq alert-default-style
        (cond
         ((and (eq system-type 'darwin)
               (display-graphic-p))
          'osx-notifier)
         ((eq system-type 'darwin)
          'notifier)
         ((executable-find "notify-send")
          'libnotify)
         ((and (eq system-type 'gnu/linux) (getenv "DBUS_SESSION_BUS_ADDRESS"))
          'notifications)
         (t
          'message))))

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
