;;; setup-core.el -- contains all major use-package forms -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Package hooks, with-eval-after-load forms, and mode integration glue
;; for packages configured outside use-package.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

(require 'cl-lib)
(require 'cmdr-macros)
(require 'cmdr-keys)
(require 'cmdr-spec)
(require 'setup-modes)

(declare-function sprite-state-path "sprite-state")
(declare-function electric-pair-default-inhibit "elec-pair")
(declare-function electric-pair-conservative-inhibit "elec-pair")
(declare-function arch-enable-desktop-notifications "arch")
(declare-function popper-group-by-project "popper")

(defvar denote-directory)
(defvar magit-mode-map)
(defvar magit-status-mode-map)
(defvar magit-module-sections-nested)
(defvar popper-reference-buffers)
(defvar popper-echo-dispatch-keys)
(defvar popper-display-control)
(defvar sprite-mode-map-prefix)
(defvar alert-default-style)
(defvar alert-fade-time)
(defvar alert-libnotify-command)
(defvar alert-log-messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLI flags

(require 'clix)

(register-clix-flag slow-op-timing
  :flag "--with-slow-op-timing" :doc "Enable slow operation timing." :consume nil
  :handler (message "[op]: enabling time reporting"))

(register-clix-flag sprite-id
  :flag "--id" :doc "Specify Emacs instance ID for sprite."
  :var sprite-cli-instance-id)

(register-clix-flag org-exec
  :flag "--org-exec" :doc "Execute an Org file's Babel blocks."
  :action (progn (require 'org) (require 'ob) (require 'builder-tasks)
                 (builder-org-babel-execute-file clix-arg-org-exec))
  :final t)

(register-clix-flag org-exec-dir
  :flag "--org-exec-dir" :doc "Execute Org Babel blocks in a directory."
  :action (progn (require 'org) (require 'ob) (require 'builder-tasks)
                 (builder-org-babel-execute-directory clix-arg-org-exec-dir))
  :final t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cmdr keybindings & specifications

(autoload 'projectile-save-project-buffers-picker "eglox-project" nil t)

(defvar tychoish-core-map (cmdr-keys-prefix cmdr-mode-map "C-c t" "core")
  "The cmdr core prefix map under C-c t.")

(setq sprite-mode-map-prefix (cons "z" 'tychoish-core-map))

(advice-add 'set-frame-font :around #'cmdr-set-frame-font-interactive)

(cmdr-keys-prefix cmdr-mode-map "C-c t t" "theme")
(cmdr-keys-set cmdr-mode-map "C-c t t r" #'disable-all-themes)
(cmdr-keys-set cmdr-mode-map "C-c t t i" #'toggle-hud-modeline-icons)
(cmdr-keys-set cmdr-mode-map "C-c t t s" #'toggle-hud-modeline-show-buffer-size)
(cmdr-keys-set cmdr-mode-map "C-c t p" #'toggle-electric-pair-inhibition)
(cmdr-keys-set cmdr-mode-map "C-c t s" #'daemons-dash)
(cmdr-keys-set cmdr-mode-map "C-c h s" #'eglox-describe-symbol-dwim)
(cmdr-keys-set cmdr-mode-map "C-c m a" #'tychoish-mail-select-account)
(cmdr-keys-set cmdr-mode-map "C-x C-b s" #'projectile-save-project-buffers-picker)

(cmdr-register-spec
  :name mail-accounts
  :probe tychoish-mail-select-account
  :commands ((cmdr-spec-command :category mu4e
                                :command tychoish-mail-select-account
                                :display-name "mail-select-account"
                                :description "select mail account"
                                :transient-key "ma")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core & UI systems

(with-eval-after-load 'elec-pair
  (add-to-list 'electric-pair-pairs '(?< . ?>))
  (add-to-list 'electric-pair-pairs '(?{ . ?}))
  (add-to-list 'electric-pair-pairs '(?\[ . ?\])))

(with-eval-after-load 'savehist
  (setq savehist-coding-system 'utf-8-emacs)
  (add-to-list 'savehist-additional-variables 'denote--title-history)
  (add-to-list 'savehist-additional-variables 'denote--keywords-history)
  (add-to-list 'savehist-additional-variables 'compile-history))

(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'register-alist)
  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'org-mode)
  (add-to-list 'desktop-modes-not-to-save 'eww-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|"
                "\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$"))

  (setq desktop-files-not-to-save
        (concat "\\(\\`/[^/:]*:\\|(ftp)\'\\)"
                "^/usr/lib/go/.*\\|"
                "^/usr/lib/rustlib/.*\\|"
                "^/home.+go/pkg/mod\\|"
                "^/home.+\\.cargo")))

(with-eval-after-load 'popper
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*Compile-Log\\*"
          "\\*compilation\\*"
          "\\*grep\\*"
          "\\*xref\\*"
          "\\*Flymake diagnostics.*\\*"
          help-mode
          compilation-mode))
  (setq popper-echo-dispatch-keys '("C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
  (setq popper-display-control nil))

(with-eval-after-load 'whitespace
  (setq whitespace-style '(face trailing tabs spaces lines newline missing-newline-at-eof empty space-mark tab-mark newline-mark)))

(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-log-types '(frameset))
  (add-to-list 'warning-suppress-types '(files missing-lexbind-cookie)))

(with-eval-after-load 'which-key
  (setq which-key-idle-delay 0.25)
  (setq which-key-idle-secondary-delay 0.125)
  (setq which-key-lighter ""))

(with-eval-after-load 'modus-themes
  (setq modus-themes-deuteranopia t)
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (message-separator bg-main))))

(defun tychoish-alert-default-style ()
  "Determine the preferred `alert' style for the current environment."
  (cond
   ((eq system-type 'darwin)
    (if (display-graphic-p) 'osx-notifier 'notifier))
   (alert-libnotify-command
    'libnotify)
   ((and (eq system-type 'gnu/linux) (getenv "DBUS_SESSION_BUS_ADDRESS"))
    'notifications)
   (t
    'message)))

(defun tychoish-alert-darwin-server-init ()
  "Switch `alert-default-style' to `osx-notifier' on first GUI client frame."
  (when (display-graphic-p)
    (setq alert-default-style 'osx-notifier)
    (remove-hook 'server-after-make-frame-hook #'tychoish-alert-darwin-server-init)))

(with-eval-after-load 'alert
  (setq alert-log-messages nil)
  (setq alert-fade-time 15)
  (setq alert-default-style (tychoish-alert-default-style))

  (when (and (eq system-type 'darwin) (daemonp))
    (add-hook 'server-after-make-frame-hook #'tychoish-alert-darwin-server-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Icons & display

(with-eval-after-load 'emojify
  (setq emojify-emoji-styles '(ascii unicode github))
  (setq emojify-display-style 'unicode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shells & communication

(autoload 'eshell-cmpl-initialize "em-cmpl")

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook #'eshell-cmpl-initialize))

(with-eval-after-load 'comint
  (keymap-set comint-mode-map "M-n" #'comint-next-input)
  (keymap-set comint-mode-map "M-p" #'comint-previous-input)
  (keymap-set comint-mode-map "<down>" #'comint-next-matching-input-from-input)
  (keymap-set comint-mode-map "<up>" #'comint-previous-matching-input-from-input))

(with-eval-after-load 'agent-shell
  (require 'setup-agent-shell))

(with-eval-after-load 'erc
  (require 'tracking))

(with-eval-after-load 'tracking
  (setq tracking-max-mode-line-entries 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search & navigation

(keymap-set isearch-mode-map "C-o" #'isearch-occur)

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "w" #'wdired-change-to-wdired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Build & system tools

(with-eval-after-load 'builder-core
  (require 'builder-tasks))

(with-eval-after-load 'arch
  (arch-enable-desktop-notifications)
  (run-with-idle-timer 2 nil #'arch--populate-cache))

(with-eval-after-load 'elpaish
  (cmdr-transient-insert-suffix-once 'elpaish-menu '(-1 0) '("x" "extended elpaish commands" execute-extended-elpaish-command)))

(with-eval-after-load 'docker
  (setq docker-terminal-backend 'eat)
  (cmdr-transient-insert-suffix-once 'docker '(-1 0) '("m" "emacs docker commands" execute-extended-docker-command)))

(with-eval-after-load 'slime
  (setq inferior-lisp-program "sbcl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Git & VCS

(with-eval-after-load 'magit
  (setq magit-module-sections-nested t)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (add-hook 'magit-status-sections-hook 'magit-insert-worktrees t)
  (add-hook 'magit-status-sections-hook 'magit-insert-modules t)

  (defvar-keymap magit-command-mode-map) ;; "/"
  (keymap-set magit-mode-map "/" (cons "magit-command" magit-command-mode-map))
  (keymap-set magit-status-mode-map "C-n" #'next-line)
  (keymap-set magit-status-mode-map "C-p" #'previous-line)
  (keymap-set magit-command-mode-map "x" (cons "magit-commands" #'execute-extended-magit-command))
  (keymap-set magit-command-mode-map "m" (cons "(s)merge-commands" #'execute-extended-magit-command))

  (when (fboundp 'hud-modeline-set-segment-action)
    (hud-modeline-set-segment-action 'vc #'magit-dispatch)))

(with-eval-after-load 'magit-dash
  (setq magit-dash-gh-prune-cache-dir (sprite-state-path "magit-dash-gh-prune")))

(with-eval-after-load 'magit-gh
  (cmdr-transient-insert-suffix-once 'magit-gh '(-1)
    ["magit-dash"
     ("g" "magit-dash: prune/CI logs/PR comments/gh auth" magit-dash-gh-menu)]))

(with-eval-after-load 'git-link
  (defun ad:git-link--new-copy-to-clipboard (link)
    "Also copy LINK to the system clipboard."
    (with-temp-buffer
      (insert link)
      (clipboard-kill-ring-save (point-min) (point-max))))

  (advice-add 'git-link--new :after #'ad:git-link--new-copy-to-clipboard))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Denote & notes

(with-eval-after-load 'denote
  (when (default-boundp 'denote-directory)
    (setq denote-directory '()))

  (add-to-list 'display-buffer-alist '("\\`\\*denote-sequence-hierarchy" (display-buffer-reuse-window display-buffer-pop-up-window)))
  (add-to-list 'denote-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote"))
  (setq denote-journal-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote" "journal"))
  (setq denote-explore-network-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "explore"))
  (setq denote-dash-default-filter '(not "journal"))
  (setq denote-review-insert-after "date")
  (setq denote-known-keywords '("project" "reference" "journal" "idea" "writing" "agent" "plan"))
  (setq denote-save-buffers t)
  (setq denote-prompts '(title keywords file-type))
  (setq denote-journal-title-format nil)
  (setq denote-sequence-scheme 'alphanumeric)
  (setq denote-explore-network-format "graphviz")

  (setq denote-rename-buffer-function #'denote-dash-rename-buffer)
  (denote-rename-buffer-mode 1)

  (defun ad:denote-link-ol-get-heading--org-only (fn &rest args)
    "Only call FN in `org-mode' buffers."
    (when (derived-mode-p 'org-mode)
      (apply fn args)))
  (advice-add 'denote-link-ol-get-heading :around
	      #'ad:denote-link-ol-get-heading--org-only)

  (setq denote-link-description-format #'denote-dash-link-description-format))

(with-eval-after-load 'denote-dash
  (keymap-set denote-dash-org-map "r" #'orgx-denote-migrate-subtree)
  (make-read-extended-command-for-prefix "denote"
    :bind-map denote-dash-global-map
    :bind-key "x"))

(with-eval-after-load 'consult-notes
  (consult-notes-denote-mode 1)

  (defun ad:consult-notes--single-directory-string (fn &rest args)
    "Let-bind `denote-directory' to a single string for the duration of FN."
    (let ((denote-directory (denote-directories-get-common-root)))
      (apply fn args)))
  (advice-add 'consult-notes :around #'ad:consult-notes--single-directory-string)
  (advice-add 'consult-notes-search-in-all-notes :around #'ad:consult-notes--single-directory-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AI & LLM integration

(with-eval-after-load 'ollama-tailnet
  (setq ollama-tailnet-fallback-strategy :auto-local)
  (cmdr-transient-insert-suffix-once 'ollama-transient-menu '(-1 0) '("x" "emacs ollama commands" execute-extended-ollama-command)))

(with-eval-after-load 'gptel
  (keymap-set gptel-mode-map "C-c m" #'gptel-menu)
  (setq gptel-include-reasoning 'ignore)
  (setq-default gptel-backend (gptel-make-openai-oauth "codex"))
  (setq-default gptel-model 'gpt-6.1-terra)
  (gptel-make-gh-copilot "gh-copilot"))

(provide 'setup-core)
;;; setup-core.el ends here
