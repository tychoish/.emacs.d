;;; setup-core.el -- contains all major use-package forms -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Provides my collection of use-package forms and related
;; configuration for loading and configuring all Emacs packages.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERNAL

(eval-when-compile
  (require 'xtd-macro))

(defvar local-notes-directory nil
  "Directory where notes (org, roam, deft, etc.) are stored.")

(defvar user-org-directories nil
  "Additional directories that may contain org files.")

(autoload 'hud-dispatch "hud" nil t)
(autoload 'hud-select "hud" nil t)
(autoload 'eglox-mode "eglox")

(autoload 'tychoish-mail-select-account "setup-mail" nil t)
(autoload 'tychoish-define-mail-account "setup-mail")
(autoload 'tychoish-mail-add-refile-rule "setup-mail")

(autoload 'daemons-dash "daemons-dash" nil t)
(autoload 'daemons-dash-dispatch "daemons-dash" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CORE PACKAGES

(use-package delight
  :ensure t
  :defer (not (gui-p))
  :demand (gui-p)
  :commands (delight)
  :config
  (delight 'fundamental-mode "fun" 'simple)
  (delight 'abbrev-mode "abb")
  (delight 'emacs-lisp-mode '("el" (lexical-binding ":l" ":d")) 'elisp-mode)
  (delight 'lisp-interaction-mode "lisp" 'elisp-mode)
  (delight 'sh-mode "sh" 'sh-script)
  (delight 'org-mode "org" 'org-mode)
  (delight 'org-agenda-mode "agenda" 'org-agenda)
  (delight 'rst-mode "rst" 'rst-mode)
  (delight 'markdown-mode "mdwn")
  (delight 'emojify-mode)
  (delight 'nerd-icons-dired-mode nil 'nerd-icons-dired)
  (delight 'nerd-icons-xref-mode nil 'nerd-icons-xref)
  (delight 'nerd-icons-completion-mode nil 'nerd-icons-completion)
  (delight 'nerd-icons-dired-mode "")
  (delight 'builder-mode "")
  (delight 'projectile-mode nil 'projectile)
  (delight 'eglot--managed-mode nil 'eglot)
  (delight 'eldoc-mode nil 'eldoc)
  (delight 'telega-chat-auto-fill-mode nil)
  (delight 'anzu-mode " az" 'anzu)
  (delight 'flyspell-mode " fs" 'flyspell)
  (delight 'flycheck-mode " fc" 'flycheck)
  (delight 'visual-line-mode " wr" 'simple)
  (delight 'auto-fill-function " afm" 'simple)
  (delight 'overwrite-mode " om" 'simple)
  (delight 'refill-mode " rf" 'refill)
  (delight 'auto-revert-mode nil 'autorevert)
  (delight 'async-bytecomp-package-mode "" 'async-bytecomp)
  (delight 'dired-async-mode "" 'dired-async)
  (delight 'agent-shell-completion-mode nil 'agent-shell-completion)
  (delight 'agent-shell-ui-mode nil 'agent-shell-ui)
  (delight 'go-ts-mode "go.ts")
  (delight 'go-mod-ts-mode "go.mod.ts")
  (delight 'go-mode "go")
  (delight 'rust-mode "rs")
  (delight 'rustic-mode "rs(x)")
  (delight 'python-ts-mode "py.ts")
  (delight 'python-mode "py")
  (delight 'lisp-mode "lisp")
  (delight 'slime-mode "sl")
  (delight 'slime-autodoc-mode nil)
  (delight 'minaid-mode nil 'minaid)
  (delight 'denote-sequence-hierarchy-mode "Hierarchy" 'denote-sequence)
  (delight 'outline-minor-mode nil 'outline)
  (delight 'cursor-sensor-mode nil 'cursor-sensor)
  (delight 'agent-shell-notifications-mode nil 'agent-shell-notifications)
  (delight 'terraform-mode "tf" 'terraform-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; chat and notifications

(use-package telega
  :ensure t
  :defines (telega-chat-mode-hook)
  :commands (telega-chat-mode
	     telega-extras-switch-to-root
	     telega-extras-force-kill
	     telega-extras-disconnect)
  :init
  (hud-bind-deferred-keymap hud-mode-map "C-c v" 'telega 'telega-prefix-map "telega")
  (hud-bind-deferred-keymap hud-mode-map "C-c n" 'telega 'telega-prefix-map "telega")
  :config
  (add-hook 'telega-chat-mode-hook #'minaid-corfu-text-mode-setup)

  (make-read-extended-command-for-prefix "telega"
    :bind-map telega-prefix-map
    :bind-key "x"
    :key-alias "telega-commands")
  (add-hook 'telega-chat-mode-hook #'telega-chat-auto-fill-mode)

  (when (eq system-type 'darwin)
    (setq telega-server-libs-prefix "/opt/homebrew")
    (setq-default alert-default-style 'osx-notifier))

  (when (eq system-type 'gnu/linux)
    (setq telega-server-libs-prefix "/usr"))

  (setq telega-emoji-use-images t)

  (setq telega-root-view-grouping-folders t)
  (setq telega-folder-icons-alist nil)

  (setq telega-root-view-grouping-other-chats nil)
  (setq telega-root-view-grouping-alist '(("open" . has-chatbuf)
					  ("personal" chat-list "personal")
					  ("groups" type basicgroup supergroup)))

  (setq telega-filter-custom-show-folders nil)
  (setq telega-filter-button-width '(.2 12 16))
  (setq telega-filters-custom '(("main" main)
				("groups" type basicgroup supergroup)
				("broadcast" type channel)
				("archive" archive)))
  (setq telega-root-view-top-categories
	'(("Users" . 10) ("Groups" . 10) ("Channels" . 10) ("Bots" . 10)
	  ("InlineBots" . 10) ("Calls" . 10)))
  (setq telega-chat-folders-exclude '("unknowns" "InlineBots" "Calls" "Personal"))

  (setq telega-chat-folders-insexp #'telega-folders-insert-default)

  (setq telega-use-images t)
  (setq telega-chat-input-markups '("markdown2"))
  (setq telega-use-tracking-for '(or unmuted mention unread))
  (setq telega-markdown2-backquotes-as-precode t)
  (setq telega-debug nil)

  (setq telega-chat--display-buffer-action
	'((display-buffer-reuse-window display-buffer-use-some-window)))

  (require 'tracking)
  (require 'telega-alert)
  (require 'telega-extras)
  (telega-extras-setup)
  (setq telega-extras-enable-chatbuf-idle-timers t
        telega-extras-chatbuf-bury-idle-time 1200
        telega-extras-chatbuf-kill-idle-time 14400)
  (setq telega-chat-auto-fill-mode-lighter "")
  (telega-mode-line-mode 1)
  (telega-alert-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; writing (english) configuration

(use-package denote
  :ensure t
  :defer t
  :commands (denote-dired denote-directories)
  :config
  (setq denote-dash-default-filter '(not "journal"))
  (setq denote-review-insert-after "date")
  (when (default-boundp 'denote-directory)
    (setq denote-directory '()))
  (add-to-list 'denote-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote"))
  (setq denote-file-type 'org)
  (setq denote-known-keywords '("project" "reference" "journal" "idea" "writing" "migration" "agent" "plan" "singing"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-save-buffers t)
  (setq denote-prompts '(title keywords file-type))
  (setq denote-journal-title-format nil)
  (setq denote-journal-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote" "journal"))
  (setq denote-sequence-scheme 'alphanumeric)
  (setq denote-explore-network-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "explore"))
  (setq denote-explore-network-format "graphviz")
  (setq denote-explore-network-graphviz-filetype "svg")
  (add-to-list 'display-buffer-alist
               '("\\`\\*denote-sequence-hierarchy "
                 (display-buffer-reuse-window display-buffer-pop-up-window)))
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'denote--title-history)
    (add-to-list 'savehist-additional-variables 'denote--keywords-history))

  (setq denote-rename-buffer-function #'denote-dash-rename-buffer)
  (denote-rename-buffer-mode 1)

  (defun ad:denote-link-ol-get-heading--org-only (fn &rest args)
    "Only call FN in `org-mode' buffers.
`denote-link-ol-store' calls this unconditionally, even from
`markdown-mode' buffers, which makes `org-get-heading' warn via
`org-element-at-point' about running outside an Org buffer."
    (when (derived-mode-p 'org-mode)
      (apply fn args)))
  (advice-add 'denote-link-ol-get-heading :around
              #'ad:denote-link-ol-get-heading--org-only)

  (setq denote-link-description-format #'denote-dash-link-description-format))

(use-package chex
  :ensure nil
  :demand t
  :config
  (chex-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ROBOTS (AI) Integration

(use-package gptel
  :ensure t
  :defer t
  :functions (gptel-make-anthropic gptel-make-gh-copilot gptel-make-gemini)
  :config
  (defvar gemini-api-key nil)
  (defvar anthropic-api-key nil)
  (defvar openai-api-key nil)

  (require 'cl-lib)

  (cl-defun make-gptel-set-up-backend-functions (&key name model backend key api-key)
    "Register buffer-local and session-default backend switchers for MODEL as NAME."
    (declare (indent defun))
    (let ((local-function-symbol (intern (format "gptel-set-backend-%s" name)))
          (default-function-symbol (intern (format "gptel-set-backend-default-%s" name))))
      (defalias local-function-symbol
        (lambda ()
          (interactive)
          (setq-local gptel-model model)
          (when api-key
            (setq-local gptel-api-key (if (functionp api-key) api-key (lambda () api-key))))
          (setq-local gptel-backend backend)
          (message "[gptel] set backend to %s for the local buffer" name))
        (format "Set LLM backend for the current buffer to `%s'" model))

      (defalias default-function-symbol
        (lambda ()
          (interactive)
          (setq-default gptel-model model)
          (when api-key
            (setq-default gptel-api-key (if (functionp api-key) api-key (lambda () api-key))))
          (setq-default gptel-backend backend)
          (message "[gptel] set default backend to %s" name))
        (format "Set the default LLM backend for the current session to `%s'" model))

      (keymap-set gptel-mode-map (format "C-c r a m %s" (upcase key)) default-function-symbol)
      (keymap-set gptel-mode-map (format "C-c r a m %s" (downcase key)) local-function-symbol)
      (keymap-set hud-robot-gptel-set-default-model-map (downcase key) default-function-symbol)))

  (keymap-set gptel-mode-map "C-c m" #'gptel-menu)

  (setq gptel-include-reasoning 'ignore)

  (make-gptel-set-up-backend-functions
   :name "sonnet-5"
   :key "s"
   :model 'claude-sonnet-5
   :backend (gptel-make-anthropic "claude-sonnet" :key anthropic-api-key :stream t)
   :api-key anthropic-api-key)

  (make-gptel-set-up-backend-functions
   :name "gemini-flash"
   :key "f"
   :model 'gemini-flash-latest
   :backend (gptel-make-gemini "gemini" :key gemini-api-key :stream t))

  (make-gptel-set-up-backend-functions
   :name "copilot"
   :key "c"
   :model 'gpt-5-mini
   :backend (gptel-make-gh-copilot "copilot"))

  (make-gptel-set-up-backend-functions
   :name "gpt-5.6-terra"
   :key "m"
   :backend (gptel-make-openai "openai" :key openai-api-key)
   :model 'gpt-5.6-terra
   :api-key openai-api-key)

  (gptel-set-backend-default-gemini-flash)
  (require 'gptel-integrations)
  (gptel-mcp-mode 1))

(provide 'setup-core)
;;; setup-core.el ends here
