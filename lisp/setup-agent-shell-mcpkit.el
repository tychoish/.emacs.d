;;; setup-agent-shell-mcpkit.el --- MCPKit integration for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; Supply the local MCPKit server and environment to agent-shell clients.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MCPKit Session Integration

(defun agent-shell-mcpkit--active-port ()
  "Return the active MCPKit port, initializing services when necessary."
  (or (mcpkit-active-port)
      (progn (mcpkit-init-services-sync) (mcpkit-active-port))))

(defun agent-shell-mcpkit-active-url ()
  "Return HTTP URL for active MCP server, or nil."
  (when-let* ((port (agent-shell-mcpkit--active-port)))
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
      (unless (seq-empty-p filtered)
        (vconcat filtered)))))

(defun agent-shell-mcpkit-environment-variables ()
  "Return environment variables exposing MCPKit connection info."
  (when-let* ((port (agent-shell-mcpkit--active-port)))
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

;; Agent-shell evaluates lambda forms here, but treats symbols as server data.
(setq agent-shell-mcp-servers (list (lambda () (agent-shell-mcpkit-server-spec))))
(advice-add 'agent-shell--mcp-servers :around #'ad:agent-shell--mcp-servers-filter-nil)
(advice-add 'agent-shell--make-acp-client :around #'ad:agent-shell--make-acp-client-inject-mcpkit-env)

(provide 'setup-agent-shell-mcpkit)
;;; setup-agent-shell-mcpkit.el ends here
