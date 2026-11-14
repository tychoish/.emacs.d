;;; setup-agent-shell.el --- Agent-shell configuration and workflows -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for agent-shell, agent-shell-queue, and agent-shell-notifications.

;;; Code:

(require 'cl-lib)
(require 'sprite)
(require 'cmdr-keys)

(eval-when-compile
  (require 'agent-shell))

(defvar which-key-replacement-alist)
(defvar shell-maker--busy)
(declare-function agent-shell-queue-setup-capture-completion "agent-shell-queue-ui")
(declare-function agent-shell-interrupt-confirmed-p "agent-shell")
(declare-function agent-shell-queue-format-file-extension "agent-shell-queue-persistence")
(declare-function mcpkit-agent-shell-mode "mcpkit-agent-shell")

(defconst tychoish/agent-shell-terse-persona
  "Be EXTREMELY concise. No preambles. No conversational filler. Provide direct answers, code, or commands immediately."
  "Prompt requesting terse agent output.")

(defvar tychoish/agent-shell-terse-output t
  "Non-nil to pass terse persona to agent sessions.")

(with-eval-after-load 'which-key
  (push '((nil . "^agent-shell-") . (nil . "")) which-key-replacement-alist))

(with-eval-after-load 'agent-shell-queue
  (keymap-set agent-shell-queue-mode-map "C-c j" (cons "robot-agent-shell" (cmdr-keys-prefix cmdr-mode-map "C-c r s" "agent-shell"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings

(setq agent-shell-github-acp-command '("gh" "copilot" "--acp"))
(setq agent-shell-pi-acp-command '("npx" "-y" "pi-acp"))
(setq agent-shell-cursor-acp-command '("cursor-agent" "acp"))
(setq agent-shell-antigravity-acp-command '("agy" "acp" "--dangerously-skip-permissions"))
(setq agent-shell-dot-subdir-function #'agent-shell-dot-subdir)
(setq agent-shell-header-style 'text)
(setq agent-shell-activity-group-expand-by-default t)
(setq agent-shell-thought-process-expand-by-default t)
(setq agent-shell-anthropic-authentication (agent-shell-anthropic-make-authentication :login t))
(setq agent-shell-anthropic-default-session-mode-id "auto")
(setq agent-shell-agent-configs #'tychoish--agent-shell-installed-configs)

(setq agent-shell-queue-state-file-function #'tychoish--agent-shell-queue-state-file)
(setq agent-shell-queue-archive-file-function #'tychoish--agent-shell-queue-archive-file)
(setq agent-shell-queue-archive-enabled t)
(setq agent-shell-queue-safe-save t)
(setq agent-shell-queue-safe-save-max-files 50)
(setq agent-shell-queue-show-ordinal-column nil)
(setq agent-shell-queue-write-log-enabled t)
(setq agent-shell-queue-serialization-format 'org)
(setq agent-shell-queue-stall-timeout 600)

(advice-add 'agent-shell-experimental--on-session-push-end :around #'ad:agent-shell-recover-stuck-push-end)
(advice-add 'agent-shell-interrupt :around #'ad:agent-shell-interrupt-recover-if-unanswered '((depth . -100)))

(cmdr-keys-set cmdr-mode-map "C-c r s v" #'tychoish--agent-shell-toggle-terse-output)

(keymap-set agent-shell-mode-map "C-c C-c" #'agent-shell-submit)
(keymap-set agent-shell-mode-map "C-c C-k" #'agent-shell-interrupt)
(keymap-set agent-shell-mode-map "C-c C-p" #'agent-shell-resolve-permissions)
(keymap-set agent-shell-mode-map "C-c b" #'agent-shell-switch-buffer)
(keymap-set agent-shell-mode-map "C-c j" (cons "robot-agent-shell" (cmdr-keys-prefix cmdr-mode-map "C-c r s" "agent-shell")))
(keymap-set agent-shell-mode-map "C-c m" #'agent-shell-menu-select-action)
(keymap-set agent-shell-mode-map "C-c x" #'agent-shell-menu-select-command)
(keymap-set agent-shell-mode-map "C-c TAB" #'agent-shell-menu-select-collapse)
(keymap-set agent-shell-mode-map "M-SPC" #'agent-shell-menu-select-session-mode)

(add-hook 'agent-shell-mode-hook #'agent-shell-corfu-setup)
(add-hook 'agent-shell-mode-hook #'agent-shell-bold-input-setup)
(add-hook 'agent-shell-mode-hook #'ansi-color-for-comint-mode-on)
(add-hook 'agent-shell-mode-hook #'tychoish--agent-shell-shorten-antigravity-modeline)

(add-hook 'agent-shell-queue-mode #'bootstrap-require-agent-shell-queue-org)
(add-hook 'agent-shell-queue-capture-mode-hook #'agent-shell-queue-setup-capture-completion)
(add-hook 'agent-shell-queue-edit-mode-hook #'agent-shell-queue-setup-capture-completion)

(add-hook 'agent-shell-mode-hook #'agent-shell-notifications-mode)
(add-hook 'agent-shell-mode-hook #'agent-shell-queue-overload-mode)
(add-hook 'agent-shell-viewport-edit-mode-hook #'agent-shell-notifications-mode)
(add-hook 'agent-shell-viewport-edit-view-hook #'agent-shell-notifications-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terse Persona & Environment

(defun tychoish--agent-shell--apply-environment ()
  "Update agent environment variables from current toggle state."
  (setq agent-shell-anthropic-claude-environment
        (let ((process-environment
               (seq-remove (lambda (entry) (string-prefix-p "CLAUDE_PERSONA=" entry))
                           process-environment)))
          (apply #'agent-shell-make-environment-variables
                 (append (when tychoish/agent-shell-terse-output
                           (list "CLAUDE_PERSONA" tychoish/agent-shell-terse-persona))
                         (list "CLAUDE_CODE_DISABLE_EXPLORE_PLAN_AGENTS" "1"
                               "CLAUDE_AGENT_SDK_DISABLE_BUILTIN_AGENTS" "1"
                               "ENABLE_CLAUDEAI_MCP_SERVERS" "false"
                               :inherit-env t)))))
  (setq agent-shell-omp-environment
        (agent-shell-make-environment-variables
         "ENABLE_CLAUDEAI_MCP_SERVERS" "false"
         "CLAUDE_CODE_DISABLE_EXPLORE_PLAN_AGENTS" "1"
         "CLAUDE_AGENT_SDK_DISABLE_BUILTIN_AGENTS" "1"
         "OMP_DOCS_DIR" (file-name-concat (or (bound-and-true-p local-notes-directory)
                                              (expand-file-name "~/notes"))
                                          "omp" "projects"))))

;;;###autoload
(defun tychoish--agent-shell-toggle-terse-output ()
  "Toggle terse agent output."
  (interactive)
  (setq tychoish/agent-shell-terse-output (not tychoish/agent-shell-terse-output))
  (tychoish--agent-shell--apply-environment)
  (message "Agent terse output: %s"
           (if tychoish/agent-shell-terse-output "on" "off")))

(tychoish--agent-shell--apply-environment) ;; TODO is this needed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent-shell Buffer Helpers & Hooks

(defun agent-shell-dot-subdir (subdir)
  "Resolve SUBDIR under agent-shell state path."
  (file-name-concat (sprite-state-path "agent-shell") subdir))

(defun agent-shell-corfu-setup ()
  "Configure Corfu auto-completion for agent-shell buffers."
  (corfu-mode +1)
  (setq-local corfu-auto-prefix 2)
  (setq-local completion-at-point-functions
              (cons #'cape-dabbrev (remq t completion-at-point-functions))))

(defun agent-shell-bold-input-setup ()
  "Render submitted prompt text in bold."
  (face-remap-add-relative 'comint-highlight-input :weight 'bold))

(defun tychoish--agent-shell-shorten-antigravity-modeline ()
  "Display \"agy\" in modeline for Antigravity sessions."
  (when (or (eq (map-nested-elt agent-shell--state '(:agent-config :identifier)) 'antigravity)
            (equal mode-name "Antigravity"))
    (setq mode-name "agy")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice & Workarounds

(defun ad:agent-shell-recover-stuck-push-end (orig-fn &rest args)
  "Recover busy state if push end arrives without an active request."
  (apply orig-fn args)
  (when shell-maker--busy
    (message "agent-shell: recovering stuck busy state after session_push_end")
    (setq shell-maker--busy nil)
    (when-let* ((on-finished (plist-get args :on-finished)))
      (funcall on-finished))))

(defun tychoish--agent-shell--force-clear-busy (shell-buffer request)
  "Force SHELL-BUFFER ready if REQUEST never received a response."
  (when (buffer-live-p shell-buffer)
    (with-current-buffer shell-buffer
      (when (and shell-maker--busy
                 (member request (map-elt agent-shell--state :active-requests)))
        (message "agent-shell: forcing recovery after interrupt got no response")
        (map-put! agent-shell--state :active-requests
                  (seq-remove (lambda (r) (equal r request))
                              (map-elt agent-shell--state :active-requests)))
        (setq shell-maker--busy nil)
        (shell-maker-finish-output :config shell-maker--config :success nil)))))

(defun ad:agent-shell-interrupt-recover-if-unanswered (orig-fn &rest args)
  "Arm a 5s fallback to recover stuck busy state after interrupt."
  (if (or (not (derived-mode-p 'agent-shell-mode))
          (not (map-nested-elt agent-shell--state '(:session :id))))
      (apply orig-fn args)
    (when (or (car args) (agent-shell-interrupt-confirmed-p))
      (let ((shell-buffer (current-buffer))
            (request (car (map-elt agent-shell--state :active-requests))))
        (prog1 (funcall orig-fn t)
          (when request
            (run-with-timer 5 nil #'tychoish--agent-shell--force-clear-busy
                            shell-buffer request)))))))

(defun tychoish--agent-shell--config-installed-p (config)
  "Return non-nil if CONFIG is Antigravity or its executable is available."
  (or (eq (map-elt config :identifier) 'antigravity)
      (when-let* ((cm (map-elt config :client-maker))
              (client (ignore-errors (funcall cm (current-buffer))))
              (cmd-spec (map-elt client :command))
              (cmd (if (consp cmd-spec) (car cmd-spec) cmd-spec))
              ((stringp cmd))
              ((not (string-empty-p cmd))))
        (executable-find cmd t))))

(defun tychoish--agent-shell-installed-configs ()
  "Return available default agent configs, including bootstrappable Antigravity."
  (seq-filter #'tychoish--agent-shell--config-installed-p
              (seq-map (lambda (e) (if (functionp e) (funcall e) e))
                       (agent-shell-default-agent-config-makers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary Agent-Shell Configuration

(defun tychoish--agent-shell-queue-state-file ()
  "Queue state file under agent-shell state directory."
  (expand-file-name (concat "queue" (agent-shell-queue-format-file-extension
                                     agent-shell-queue-serialization-format))
                    (sprite-state-path "agent-shell")))

(defun tychoish--agent-shell-queue-archive-file ()
  "Archive file under agent-shell state directory."
  (expand-file-name "queue-archive.jsonl"
                    (sprite-state-path "agent-shell")))

(defun bootstrap-require-agent-shell-queue-org () (require 'agent-shell-queue-org))

(provide 'setup-agent-shell)
;;; setup-agent-shell.el ends here
