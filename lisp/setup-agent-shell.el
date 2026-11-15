;;; setup-agent-shell.el --- Agent-shell configuration and workflows -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for agent-shell, agent-shell-queue, and agent-shell-notifications.

;;; Code:

(require 'cl-lib)
(require 'sprite)
(require 'cmdr-keys)
(require 'agent-shell-queue)

(defvar which-key-replacement-alist)
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

(add-hook 'agent-shell-mode-hook #'agent-shell-bold-input-setup)
(add-hook 'agent-shell-mode-hook #'ansi-color-for-comint-mode-on)
(add-hook 'agent-shell-mode-hook #'tychoish--agent-shell-shorten-antigravity-modeline)

(add-hook 'agent-shell-mode-hook #'agent-shell-notifications-mode)
(add-hook 'agent-shell-mode-hook #'agent-shell-queue-overload-mode)
(add-hook 'agent-shell-viewport-edit-mode-hook #'agent-shell-notifications-viewport-edit-mode)
(add-hook 'agent-shell-viewport-view-mode-hook #'agent-shell-notifications-viewport-view-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent-shell Buffer Helpers & Hooks

(defun agent-shell-bold-input-setup ()
  "Render submitted prompt text in bold."
  (face-remap-add-relative 'comint-highlight-input :weight 'bold))

(defun tychoish--agent-shell-shorten-antigravity-modeline ()
  "Display \"agy\" in modeline for Antigravity sessions."
  (when (or (eq (map-nested-elt agent-shell--state '(:agent-config :identifier)) 'antigravity)
            (equal mode-name "Antigravity"))
    (setq mode-name "agy")))

(provide 'setup-agent-shell)
;;; setup-agent-shell.el ends here
