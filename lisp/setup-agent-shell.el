;;; setup-agent-shell.el --- Agent-shell configuration and workflows -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for agent-shell, agent-shell-queue, and agent-shell-notifications.

;;; Code:

(require 'cl-lib)
(require 'sprite)
(require 'cmdr-keys)
(require 'agent-shell-queue)

(eval-when-compile
  (require 'agent-shell))

(defvar which-key-replacement-alist)
(declare-function shell-maker-busy "shell-maker")
(declare-function shell-maker-finish-output "shell-maker")
(declare-function agent-shell-interrupt-confirmed-p "agent-shell")
(declare-function agent-shell-menu-resolve-permission "agent-shell-menu")
(autoload 'agent-shell-menu-next-buffer "agent-shell-menu" nil t)
(autoload 'agent-shell-menu-previous-buffer "agent-shell-menu" nil t)

(with-eval-after-load 'which-key
  (push '((nil . "^agent-shell-") . (nil . "")) which-key-replacement-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings

(setq agent-shell-pi-acp-command '("npx" "-y" "pi-acp"))
(setq agent-shell-cursor-acp-command '("cursor-agent" "acp"))
(setq agent-shell-antigravity-acp-command '("agy" "acp" "--dangerously-skip-permissions"))
(setq agent-shell-dot-subdir-function (lambda (subdir) (file-name-concat (sprite-state-path "agent-shell") subdir)))
(setq agent-shell-header-style 'text)
(setq agent-shell-anthropic-default-session-mode-id "auto")
(setq agent-shell-agent-configs #'tychoish--agent-shell-installed-configs)
(setq agent-shell-display-action
      '((display-buffer-reuse-window display-buffer-use-some-window)
        (reusable-frames . t)))

(setq agent-shell-anthropic-claude-environment
      '("CLAUDE_CODE_DISABLE_EXPLORE_PLAN_AGENTS=1"
        "CLAUDE_AGENT_SDK_DISABLE_BUILTIN_AGENTS=1"
        "ENABLE_CLAUDEAI_MCP_SERVERS=false"))

(setq agent-shell-omp-environment
      (list "ENABLE_CLAUDEAI_MCP_SERVERS=false"
            "CLAUDE_CODE_DISABLE_EXPLORE_PLAN_AGENTS=1"
            "CLAUDE_AGENT_SDK_DISABLE_BUILTIN_AGENTS=1"
            (concat "OMP_DOCS_DIR="
                    (file-name-concat (or (bound-and-true-p local-notes-directory)
                                          (expand-file-name "~/notes"))
                                      "omp" "projects"))))

(setq agent-shell-queue-archive-enabled t)
(setq agent-shell-queue-safe-save t)
(setq agent-shell-queue-safe-save-max-files 50)
(setq agent-shell-queue-show-ordinal-column nil)
(setq agent-shell-queue-write-log-enabled t)
(setq agent-shell-queue-serialization-format 'org)
(setq agent-shell-queue-stall-timeout 600)

(advice-add 'agent-shell-interrupt :around #'ad:agent-shell-interrupt-recover-if-unanswered '((depth . -100)))

(keymap-set agent-shell-queue-mode-map "C-c j" (cons "robot-agent-shell" (cmdr-keys-prefix cmdr-mode-map "C-c r s" "agent-shell")))

(keymap-set agent-shell-mode-map "C-c C-c" #'agent-shell-submit)
(keymap-set agent-shell-mode-map "C-c C-k" #'agent-shell-interrupt)
(keymap-set agent-shell-mode-map "C-c C-p" #'agent-shell-menu-resolve-permission)
(keymap-set agent-shell-mode-map "C-c b" #'agent-shell-switch-buffer)
(keymap-set agent-shell-mode-map "C-c ]" #'agent-shell-menu-next-buffer)
(keymap-set agent-shell-mode-map "C-c [" #'agent-shell-menu-previous-buffer)
(keymap-set agent-shell-mode-map "C-c j" (cons "robot-agent-shell" (cmdr-keys-prefix cmdr-mode-map "C-c r s" "agent-shell")))
(keymap-set agent-shell-mode-map "C-c m" #'agent-shell-menu-select-action)
(keymap-set agent-shell-mode-map "C-c x" #'agent-shell-menu-select-command)
(keymap-set agent-shell-mode-map "C-c TAB" #'agent-shell-menu-select-collapse)
(keymap-set agent-shell-mode-map "M-SPC" #'agent-shell-menu-select-session-mode)

(add-hook 'agent-shell-mode-hook #'agent-shell-corfu-setup)
(add-hook 'agent-shell-mode-hook #'agent-shell-bold-input-setup)
(add-hook 'agent-shell-mode-hook #'ansi-color-for-comint-mode-on)
(add-hook 'agent-shell-mode-hook #'tychoish--agent-shell-shorten-antigravity-modeline)

(add-hook 'agent-shell-mode-hook #'agent-shell-notifications-mode)
(add-hook 'agent-shell-mode-hook #'agent-shell-queue-overload-mode)
(add-hook 'agent-shell-viewport-edit-mode-hook #'agent-shell-notifications-viewport-edit-mode)
(add-hook 'agent-shell-viewport-view-mode-hook #'agent-shell-notifications-viewport-view-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent-shell Buffer Helpers & Hooks

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

(defun tychoish--agent-shell--force-clear-busy (shell-buffer request)
  "Force SHELL-BUFFER ready if REQUEST never received a response."
  (when (buffer-live-p shell-buffer)
    (with-current-buffer shell-buffer
      (when (and (shell-maker-busy)
                 (member request (map-elt agent-shell--state :active-requests)))
        (message "agent-shell: forcing recovery after interrupt got no response")
        (map-put! agent-shell--state :active-requests
                  (seq-remove (lambda (r) (equal r request))
                              (map-elt agent-shell--state :active-requests)))
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
  "Return non-nil if CONFIG executable is available on PATH."
  (if-let* ((cm (map-elt config :client-maker))
            (client (ignore-errors (funcall cm (current-buffer))))
            (cmd-spec (map-elt client :command))
            (cmd (if (consp cmd-spec) (car cmd-spec) cmd-spec))
            ((stringp cmd))
            ((not (string-empty-p cmd))))
      (executable-find cmd t)
    (when (eq (map-elt config :identifier) 'antigravity)
      (executable-find "agy" t))))

(defun tychoish--agent-shell-installed-configs ()
  "Return available default agent configs whose executable is installed."
  (seq-filter (lambda (maker)
                (tychoish--agent-shell--config-installed-p
                 (if (functionp maker) (funcall maker) maker)))
              (agent-shell-default-agent-config-makers)))

(provide 'setup-agent-shell)
;;; setup-agent-shell.el ends here
