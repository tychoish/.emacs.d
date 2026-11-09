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
(defvar nerd-icons-mode-icon-alist)

(with-eval-after-load 'nerd-icons
  (unless (assq 'agent-shell-queue-item-view-mode nerd-icons-mode-icon-alist)
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(agent-shell-queue-item-view-mode nerd-icons-codicon "nf-cod-checklist" :face nerd-icons-green))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terse Persona & Environment

(defconst tychoish/agent-shell-terse-persona
  "Be EXTREMELY concise. No preambles. No conversational filler. Provide direct answers, code, or commands immediately."
  "Prompt requesting terse agent output.")

(defvar tychoish/agent-shell-terse-output t
  "Non-nil to pass terse persona to agent sessions.")

(defun tychoish/agent-shell--apply-environment ()
  "Update agent environment variables from current toggle state."
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "CLAUDE_PERSONA" tychoish/agent-shell-terse-persona
         "CLAUDE_CODE_DISABLE_EXPLORE_PLAN_AGENTS" "1"
         "CLAUDE_AGENT_SDK_DISABLE_BUILTIN_AGENTS" "1"
         "ENABLE_CLAUDEAI_MCP_SERVERS" "false"
         :inherit-env t))
  (setq agent-shell-omp-environment
        (agent-shell-make-environment-variables
         "ENABLE_CLAUDEAI_MCP_SERVERS" "false"
         "CLAUDE_CODE_DISABLE_EXPLORE_PLAN_AGENTS" "1"
         "CLAUDE_AGENT_SDK_DISABLE_BUILTIN_AGENTS" "1"
         "OMP_DOCS_DIR" (file-name-concat (or (bound-and-true-p local-notes-directory)
                                              (expand-file-name "~/notes"))
                                          "omp" "projects"))))

;;;###autoload
(defun tychoish/agent-shell-toggle-terse-output ()
  "Toggle terse agent output."
  (interactive)
  (setq tychoish/agent-shell-terse-output (not tychoish/agent-shell-terse-output))
  (tychoish/agent-shell--apply-environment)
  (message "Agent terse output: %s"
           (if tychoish/agent-shell-terse-output "on" "off")))

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

(defun tychoish/agent-shell-shorten-antigravity-modeline ()
  "Display \"agy\" in modeline for Antigravity sessions."
  (when (or (eq (map-nested-elt agent-shell--state '(:agent-config :identifier)) 'antigravity)
            (equal mode-name "Antigravity"))
    (setq mode-name "agy")))

(defun tychoish--agent-shell-buffer-p (buffer _action)
  "Return non-nil if BUFFER derives from `agent-shell-mode'."
  (with-current-buffer buffer
    (derived-mode-p 'agent-shell-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice & Workarounds

(defvar shell-maker--busy)

(defun ad:agent-shell-recover-stuck-push-end (orig-fn &rest args)
  "Recover busy state if push end arrives without an active request."
  (apply orig-fn args)
  (when shell-maker--busy
    (message "agent-shell: recovering stuck busy state after session_push_end")
    (setq shell-maker--busy nil)
    (when-let* ((on-finished (plist-get args :on-finished)))
      (funcall on-finished))))

(defun tychoish/agent-shell--force-clear-busy (shell-buffer request)
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
  (let ((shell-buffer (current-buffer))
        (request (car (map-elt agent-shell--state :active-requests))))
    (apply orig-fn args)
    (when (and request (map-nested-elt agent-shell--state '(:session :id)))
      (run-with-timer 5 nil #'tychoish/agent-shell--force-clear-busy
                       shell-buffer request))))

(defun ad:acp-route-incoming-message-isolate-errors (orig-fn &rest args)
  "Isolate errors from ACP response handling to avoid wedging drain loop."
  (condition-case err
      (apply orig-fn args)
    (error
     (message "acp: error routing incoming message, isolated: %S" err)
     nil)))

(defun tychoish/agent-shell--config-installed-p (config)
  "Return non-nil if CONFIG is Antigravity or its executable is available."
  (or (eq (map-elt config :identifier) 'antigravity)
      (when-let* ((cm (map-elt config :client-maker))
              (client (ignore-errors (funcall cm (current-buffer))))
              (cmd-spec (map-elt client :command))
              (cmd (if (consp cmd-spec) (car cmd-spec) cmd-spec))
              ((stringp cmd))
              ((not (string-empty-p cmd))))
        (executable-find cmd t))))

(defun tychoish/agent-shell-installed-configs ()
  "Return available default agent configs, including bootstrappable Antigravity."
  (seq-filter #'tychoish/agent-shell--config-installed-p
              (seq-map (lambda (e) (if (functionp e) (funcall e) e))
                       (agent-shell-default-agent-config-makers))))

(defun ad:agent-shell-insert-to-shell-buffer-advance-point (orig-fn &rest args)
  "Advance point and window-point to `point-max' after insertion."
  (prog1 (apply orig-fn args)
    (when-let* ((shell-buffer (plist-get args :shell-buffer))
                ((buffer-live-p shell-buffer)))
      (with-current-buffer shell-buffer
        (goto-char (point-max))
        (when-let* ((win (get-buffer-window shell-buffer t)))
          (set-window-point win (point-max)))))))

(defun ad:agent-shell--refresh-session-title (orig-fn &optional event)
  "Preserve user session title for Claude sessions."
  (unless (and-let* ((agent-name (map-nested-elt agent-shell--state '(:agent-config :mode-line-name)))
                     ((equal agent-name "Claude"))
                     (title (map-nested-elt agent-shell--state '(:session :title)))
                     ((stringp title))
                     ((not (string-empty-p title)))))
    (funcall orig-fn event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MCPKit Session Integration

(defun agent-shell-mcpkit-active-url ()
  "Return HTTP URL for active MCP server, or nil."
  (when-let* ((port (or (mcpkit-active-port)
                        (progn (mcpkit-init-services-sync) (mcpkit-active-port)))))
    (if (and (fboundp 'mcpkit-proxy-active-port)
             (mcpkit-proxy-active-port)
             (bound-and-true-p sprite-instance-id))
        (format "http://127.0.0.1:%d/sprite/%s/mcp"
                (mcpkit-proxy-active-port)
                sprite-instance-id)
      (format "http://127.0.0.1:%d/mcp" port))))

(defun agent-shell-mcpkit-server-spec ()
  "Return ACP McpServer specification for current session's MCPKit."
  (when-let* ((url (agent-shell-mcpkit-active-url)))
    `((name . "emacs")
      (type . "http")
      (url . ,url)
      (headers . ()))))

(defun ad:agent-shell--mcp-servers-filter-nil (orig-fn &rest args)
  "Filter out nil server entries from dynamic resolvers."
  (when-let* ((servers (apply orig-fn args)))
    (let ((filtered (seq-filter (lambda (s) (map-elt s 'name)) servers)))
      (when (> (length filtered) 0)
        (apply #'vector (append filtered nil))))))

(defun agent-shell-mcpkit-environment-variables ()
  "Return environment variables exposing MCPKit connection info."
  (when-let* ((port (or (mcpkit-active-port)
                        (progn (mcpkit-init-services-sync) (mcpkit-active-port)))))
    (list (format "MCPKIT_PORT=%d" port)
          (format "EMACS_MCP_PORT=%d" port)
          (format "EMACS_MCP_URL=http://127.0.0.1:%d/mcp" port)
          (format "EMACS_SPRITE_ID=%s" (or (bound-and-true-p sprite-instance-id) (system-name)))
          (format "EMACS_SERVER_NAME=%s" (or (bound-and-true-p server-name) "server")))))

(defun ad:agent-shell--make-acp-client-inject-mcpkit-env (orig-fn &rest args)
  "Inject MCPKit environment variables into ACP client."
  (let* ((env (plist-get args :environment-variables))
         (extra (agent-shell-mcpkit-environment-variables)))
    (if extra
        (apply orig-fn (plist-put (copy-sequence args) :environment-variables (append extra env)))
      (apply orig-fn args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary Agent-Shell Configuration

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
(setq agent-shell-agent-configs #'tychoish/agent-shell-installed-configs)
(setq agent-shell-mcp-servers (list (lambda () (agent-shell-mcpkit-server-spec))))

(advice-add 'agent-shell-experimental--on-session-push-end :around #'ad:agent-shell-recover-stuck-push-end)
(advice-add 'agent-shell-interrupt :around #'ad:agent-shell-interrupt-recover-if-unanswered)
(advice-add 'acp--route-incoming-message :around #'ad:acp-route-incoming-message-isolate-errors)
(advice-add 'agent-shell--insert-to-shell-buffer :around #'ad:agent-shell-insert-to-shell-buffer-advance-point)
(advice-add 'agent-shell--refresh-session-title :around #'ad:agent-shell--refresh-session-title)
(advice-add 'agent-shell--mcp-servers :around #'ad:agent-shell--mcp-servers-filter-nil)
(advice-add 'agent-shell--make-acp-client :around #'ad:agent-shell--make-acp-client-inject-mcpkit-env)

(keymap-set agent-shell-mode-map "C-c C-c" #'agent-shell-submit)
(keymap-set agent-shell-mode-map "C-c C-k" #'agent-shell-interrupt)
(keymap-set agent-shell-mode-map "C-c C-p" #'agent-shell-resolve-permissions)
(keymap-set agent-shell-mode-map "C-c C-a" #'agent-shell-menu-select-action)
(keymap-set agent-shell-mode-map "C-c b" #'agent-shell-switch-buffer)
(keymap-set agent-shell-mode-map "C-c j" (cons "robot-agent-shell" (cmdr-keys-prefix cmdr-mode-map "C-c r s" "agent-shell")))
(keymap-set agent-shell-mode-map "C-c m" #'agent-shell-menu-select-action)
(keymap-set agent-shell-mode-map "C-c x" #'agent-shell-menu-select-command)
(keymap-set agent-shell-mode-map "C-c TAB" #'agent-shell-menu-select-collapse)
(keymap-set agent-shell-mode-map "C-TAB" #'agent-shell-next-item)
(keymap-set agent-shell-mode-map "M-SPC" #'agent-shell-menu-select-session-mode)

(cmdr-keys-set cmdr-mode-map "C-c r s v" #'tychoish/agent-shell-toggle-terse-output)

(add-hook 'agent-shell-mode-hook #'agent-shell-corfu-setup)
(add-hook 'agent-shell-mode-hook #'agent-shell-bold-input-setup)
(add-hook 'agent-shell-mode-hook #'ansi-color-for-comint-mode-on)
(add-hook 'agent-shell-mode-hook #'tychoish/agent-shell-shorten-antigravity-modeline)

(add-to-list 'display-buffer-alist
             '(tychoish--agent-shell-buffer-p
               (display-buffer-reuse-window
                display-buffer-use-some-window)
               (reusable-frames . t)))

(with-eval-after-load 'which-key
  (push '((nil . "^agent-shell-") . (nil . "")) which-key-replacement-alist))

(tychoish/agent-shell--apply-environment)

(defun tychoish--agent-shell-queue-state-file ()
  "Queue state file under agent-shell state directory."
  (let ((ext (cond
              ((eq (bound-and-true-p agent-shell-queue-serialization-format) 'json) "json")
              ((eq (bound-and-true-p agent-shell-queue-serialization-format) 'yaml) "yaml")
              ((eq (bound-and-true-p agent-shell-queue-serialization-format) 'org) "org")
              (t "el"))))
    (expand-file-name (concat "queue." ext)
                      (sprite-state-path "agent-shell"))))

(defun tychoish--agent-shell-queue-archive-file ()
  "Archive file under agent-shell state directory."
  (expand-file-name "queue-archive.jsonl"
                    (sprite-state-path "agent-shell")))

(defvar alert-fade-time)
(declare-function agent-shell-queue-setup-capture-completion "agent-shell-queue-ui")

(defun tychoish/agent-shell-notifications-alert-send (plist)
  "Send agent-shell notification PLIST through `alert'."
  (let* ((title (plist-get plist :title))
         (body (plist-get plist :body))
         (icon (plist-get plist :app-icon))
         (timeout (plist-get plist :timeout))
         (buf-name (plist-get plist :shell-buffer-name))
         (alert-fade-time (if (and (numberp timeout) (> timeout 0))
                              timeout
                            alert-fade-time)))
    (alert (or body title "")
           :title (if (and buf-name (not (string-empty-p buf-name)))
                      (format "%s <%s>" (or title "agent-shell") buf-name)
                    (or title "agent-shell"))
           :icon icon
           :category 'agent-shell
           :severity 'normal))
  nil)

(defun tychoish/agent-shell-notifications-alert-close (_id)
  "No-op close function for alert notifications." nil)

(defun tychoish/agent-shell-notifications--add-buffer-name (orig type shell-buffer event)
  "Add `:shell-buffer-name' to alert payload plist."
  (append (funcall orig type shell-buffer event)
          (list :shell-buffer-name (buffer-name shell-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; agent-shell-queue Integration

(with-eval-after-load 'agent-shell-queue
  (setq agent-shell-queue-write-log-enabled t)
  (require 'agent-shell-queue-org)
  (setq agent-shell-queue-serialization-format 'org)
  (setq agent-shell-queue-stall-timeout 600)
  (agent-shell-queue-overload-mode 1)

  (keymap-set agent-shell-queue-mode-map "C-c j" (cons "robot-agent-shell" (cmdr-keys-prefix cmdr-mode-map "C-c r s" "agent-shell")))

  (add-hook 'agent-shell-queue-capture-mode-hook #'agent-shell-queue-setup-capture-completion)
  (add-hook 'agent-shell-queue-edit-mode-hook #'agent-shell-queue-setup-capture-completion)

  (setq agent-shell-queue-state-file-function #'tychoish--agent-shell-queue-state-file)
  (setq agent-shell-queue-archive-file-function #'tychoish--agent-shell-queue-archive-file)
  (setq agent-shell-queue-archive-enabled t)
  (setq agent-shell-queue-safe-save t)
  (setq agent-shell-queue-safe-save-max-files 50)
  (setq agent-shell-queue-show-ordinal-column nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; agent-shell-notifications Integration

(with-eval-after-load 'agent-shell-notifications
  (advice-add 'agent-shell-notifications--make-notification-plist
              :around #'tychoish/agent-shell-notifications--add-buffer-name)

  (setq agent-shell-notifications-provider nil)
  (setq agent-shell-notifications-send-function #'tychoish/agent-shell-notifications-alert-send)
  (setq agent-shell-notifications-close-function #'tychoish/agent-shell-notifications-alert-close)
  (setq agent-shell-notifications-timeout 30))

(add-hook 'agent-shell-mode-hook #'agent-shell-queue-global-mode)
(add-hook 'agent-shell-mode-hook #'agent-shell-notifications-mode)
(add-hook 'agent-shell-viewport-edit-mode-hook #'agent-shell-notifications-mode)
(add-hook 'agent-shell-viewport-edit-view-hook #'agent-shell-notifications-mode)

(provide 'setup-agent-shell)
;;; setup-agent-shell.el ends here
