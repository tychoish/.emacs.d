(use-package google-gemini
  :defer t
  :ensure nil
  :commands (google-gemini-chat-prompt
	     google-gemini-content-prompt
	     google-gemini-count-tokens-prompt
	     google-gemini-list-models
	     google-gemini-model-info))


(use-package copilot-chat
  :ensure t
  :bind (:map tychoish/robot-copilot-map ;; "C-c r c"
	      ("m" . copilot-chat-transient)
	      ("t" . copilot-chat))
  :config
  (setq copilot-chat-frontend 'markdown)
  (setq copilot-chat-follow t)
  (setq copilot-chat-markdown-prompt "##"))

(use-package copilot
  :ensure t
  :bind (:map tychoish/robot-map ;; "C-c r"
	      :prefix "c"
	      :prefix-map tychoish/robot-copilot-map ;; "C-c r c"
	      ("s" . copilot-complete)
	      ("g" . copilot-clear-overlay)
	      :map tychoish/completion-map
	      ("r" . copilot-complete)
	      :map copilot-completion-map
	      ("C-c a" . copilot-accept-completion)
	      ("C-c l" . copilot-accept-completion-by-line)
	      ("C-c w" . copilot-accept-completion-by-word)
	      ("C-c n" . copilot-next-completion)
	      ("C-c p" . copilot-previous-completion)
	      ("C-c C-i" . copilot-insert-completion))
  :commands (copilot-login copilot-mode)
  :config
  (add-to-list 'copilot-major-mode-alist '("rustic-mode" . "rust"))
  (add-to-list 'copilot-major-mode-alist '("go-ts-mode" . "go"))
  (add-to-list 'copilot-major-mode-alist '("c-ts-mode" . "c"))
  (add-to-list 'copilot-major-mode-alist '("c++-ts-mode" . "cpp"))
  (add-to-list 'copilot-major-mode-alist '("python-ts-mode" . "python"))
  (add-to-list 'copilot-major-mode-alist '("yaml-mode" . "yaml"))
  (add-to-list 'copilot-major-mode-alist '("bash-ts-mode" . "shellscript")))

(use-package claude-code-ide
  :load-path "external/claude-code-ide"
  ;; :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind (:map tychoish/robot-claude-code-ide-map
	      ("l" . claude-code-ide-list-sessions)
	      ("t" . claude-code-ide-toggle)
	      ("b" . claude-code-ide-switch-to-buffer)
	      ("k" . claude-code-ide-stop)
	      ("s" . claude-code-ide-continue)
	      ("e" . claude-code-ide-send-escape))
  :commands (claude-code-ide claude-code-ide-menu)
  :init
  (bind-keys
   :map tychoish/robot-map
   :prefix "i"
   :prefix-map tychoish/robot-claude-code-ide-map
   ("m" . claude-code-ide-menu)
   ("c" . claude-code-ide))
  (make-read-extended-command-for-prefix "claude-code-ide"
    :bind-map tychoish/robot-claude-code-ide-map
    :bind-key "x")
  :config
  (setq claude-code-ide-diagnostics-backend 'flycheck)
  (setq claude-code-ide-terminal-backend 'eat)
  (setq claude-code-ide-prevent-reflow-glitch t)
  (setq claude-code-ide-terminal-initialization-delay 0.2)
  (setq claude-code-ide-eat-preserve-position t)
  (setq claude-code-ide-vterm-anti-flicker t)
  (claude-code-ide-emacs-tools-setup))

(use-package aidermacs
  :ensure t
  :commands (aidermacs-transient-menu)
  :init
  (bind-keys
   :map tychoish/robot-map
   :prefix "a"
   :prefix-map tychoish/robot-aider-map
   ("m" . aidermacs-transient-menu))
  (make-read-extended-command-for-prefix "aidermacs"
    :bind-map tychoish/robot-aider-map
    :bind-key "x")
  (make-read-extended-command-for-prefix "aidermacs-model"
    :bind-map tychoish/robot-aider-map
    :bind-key "l")
  :init 
  (cl-defmacro make-aidermacs-model-selection-function (&optional &key name default-model weak-model editor-model architect-model copilot)
    "Define a command to switch the aider model settings, including changing the live session."
    (unless (and name default-model)
      (user-error "must specify both name (%s) and default-model (%s)" name default-model))
    (let ((symbol-name (format "aidermacs-model-use-%s" name)))
      `(defun ,(intern symbol-name) ()
	 ,(format "Switch to using `%s' (%s) as the default model for aidermacs." default-model name)
	 (interactive)
	 (if ,copilot
             (progn
               (message "setup environment for copilot")
               (setenv "OPENAI_API_KEY" github-oauth-token)
               (setenv "OPENAI_API_BASE" "https://api.githubcopilot.com"))
           (setenv "OPENAI_API_BASE" nil)
           (setenv "OPENAI_API_KEY" openai-api-key))
	 (setq aidermacs-default-model ,default-model)
	 (setq aidermacs-architect-model ,(or architect-model default-model))
	 (setq aidermacs-editor-model ,(or editor-model default-model))
	 (setq aidermacs-weak-model ,weak-model)
	 (when-let* ((buf (aidermacs-select-buffer-name)))
	   (with-current-buffer buf
	     (aidermacs-send-command-with-prefix "/model " (or aidermacs-architect-model aidermacs-default-model))
	     (aidermacs-send-command-with-prefix "/weak-model " aidermacs-weak-model)
	     (aidermacs-send-command-with-prefix "/editor-model " aidermacs-editor-model))))))
  :config
  (setq aidermacs-default-chat-mode 'architect)
  (setq aidermacs-program "aider")

  (with-toggle-once tychoish/aider-setup-state
    (when (boundp 'anthropic-api-key)
      (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
      (message "[robots]: set environment variable for anthropic"))

    (when (boundp 'gemini-api-key)
      (setenv "GEMINI_API_KEY" gemini-api-key)
      (message "[robots]: set environment variable for gemini"))

    (when (boundp 'openai-api-key)
      (setenv "OPENAI_API_KEY" openai-api-key)
      (message "[robots]: set environment variable for OpenAI"))

    (when-let* ((uv-bin-path (expand-file-name "~/.local/bin"))
		(_ (file-exists-p uv-bin-path))
		(aider-bin-path (f-join uv-bin-path "aider"))
		(_ (file-exists-p aider-bin-path))
		(search-path (getenv "PATH")))

      (unless (s-contains-p uv-bin-path search-path)
	(setenv "PATH" (format "%s:%s" search-path uv-bin-path)))

      (add-to-list 'exec-path uv-bin-path)))
  (tychoish/aider-setup-state)
  (defvar aidermacs-model-settings-path (f-join user-emacs-directory "aider.model.settings.yml"))

  (mapc (make-add-to-list-fn aidermacs-extra-args :append t)
	(list (concat "--input-history-file=" (sprite-state-path "aider.input-history.md"))
	      (concat "--chat-history-file=" (sprite-state-path "aider.chat-history.md"))
	      (concat "--model-settings-file=" aidermacs-model-settings-path)
	      "--editor-edit-format=udiff" "--edit-format=udiff"
	      "--notifications" "--cache-prompts"))

  (make-aidermacs-model-selection-function
   :name "claude-max"
   :default-model "anthropic/claude-sonnet-4-5"
   :architect-model "anthropic/claude-opus-4-5")

  (make-aidermacs-model-selection-function
   :name "claude-sonnet"
   :default-model "anthropic/claude-sonnet-4-5"
   :weak-model "anthropic/claude-haiku-4-5")

  (make-aidermacs-model-selection-function
   :name "gpt-4o"
   :default-model "opeani/gpt-4o-mini"
   :architect-model "openai/gpt-4o")

  (make-aidermacs-model-selection-function
   :name "gpt-4"
   :default-model "opeani/gpt-4o"
   :weak-model "opeani/gpt-4o-mini"
   :architect-model "opeani/gpt-4.5-preview")

  (make-aidermacs-model-selection-function
   :name "gpt-5"
   :default-model "opeani/gpt-5"
   :architect-model "opeani/gpt-5.2"
   :weak-model "opeani/gpt-5-nano")

  (make-aidermacs-model-selection-function
   :name "gpt-5-mini"
   :default-model "opeani/gpt-5-mini"
   :weak-model "opeani/gpt-5-nano")

  (make-aidermacs-model-selection-function
   :name "gemini-2"
   :default-model "gemini/gemini-2.5-flash"
   :architect-model "gemini/gemini-2.5-pro"
   :weak-model "gemini/gemini-2.5-flash-lite")

  (make-aidermacs-model-selection-function
   :name "gemini"
   :default-model "gemini/gemini-flash-latest"
   :architect-model "gemini/gemini-pro")

  (make-aidermacs-model-selection-function
   :name "gemini-3-flash"
   :default-model "gemini/gemini-3-flash-preview")

  (make-aidermacs-model-selection-function
   :name "copilot-gemini" :copilot 'use-copilot
   :default-model "github_copilot/gemini-2.5-pro")

  (make-aidermacs-model-selection-function
   :name "copilot-gpt-4o" :copilot 'use-copilot
   :default-model "github_copilot/gpt-4o-mini"
   :architect-model "github_copilot/gpt-4o")

  (make-aidermacs-model-selection-function
   :name "copilot-gpt-5" :copilot 'use-copilot
   :default-model "github_copilot/gpt-5-mini"
   :architect-model "github_copilot/gpt-5.2"
   :weak-model "github_copilot/gpt-5-nano")

  (make-aidermacs-model-selection-function
   :name "copilot-claude" :copilot 'use-copilot
   :default-model "github_copilot/claude-sonnet-4.5"
   :architect-model "github_copilot/claude-opus-4.5"
   :weak-model "github_copilot/claude-haiku-4.5")

  (make-aidermacs-model-selection-function
   :name "copilot-claude-sonnet" :copilot 'use-copilot
   :default-model "github_copilot/claude-sonnet-4.5"
   :weak-model "github_copilot/claude-haiku-4.5"))

