
(tychoish/mcp-servers-init)

(use-package mcp
  :ensure t
  :commands (mcp-hub-start-all-servers)
  :config
  (require 'mcp-hub)
  (tychoish/mcp-servers-init)
  (make-read-extended-command-for-prefix "mcp"
    :key-alias "mcp-commands"
    :bind-key "/"
    :bind-map mcp-hub-mode-map)

  (flex-defun tychoish/mcp-hub-refresh ()
    "Recompute `mcp-hub-servers' from `tychoish/mcp-build-servers'."
    (setq mcp-hub-servers
	  (mapcar #'tychoish/mcp-spec->hub (tychoish/mcp-build-servers))))

  (defun tychoish/mcp-hub-skip-disabled (orig-fn name &rest args)
    "Around advice: refuse to start a server marked :disabled in `tychoish/mcp-servers'."
    (let ((spec (cl-find name tychoish/mcp-servers
                         :key (lambda (s) (plist-get s :name))
                         :test #'equal)))
      (if (and spec (plist-get spec :disabled))
          (message "mcp-hub: skipping disabled server %S" name)
        (apply orig-fn name args))))
  (tychoish/mcp-hub-refresh)
  (advice-add 'mcp-hub-start-all-servers :before #'tychoish/mcp-hub-refresh)
  (advice-add 'mcp-hub-start-server :around #'tychoish/mcp-hub-skip-disabled))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; agent shell specific 

  (advice-add 'agent-shell :before #'tychoish/agent-shell-mcp-refresh)

  (defun tychoish/agent-shell-mcp-refresh (&rest _)
    "Recompute `agent-shell-mcp-servers' from `tychoish/mcp-build-servers'."
    (setq agent-shell-mcp-servers
  	  (and (mapcar #'tychoish/mcp-spec->acp (tychoish/mcp-build-servers)) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mcp configuration / setup

(defvar tychoish/gopls-mcp-port 38713
  "TCP port for the gopls MCP HTTP endpoint (shared or standalone).")

(defvar tychoish/gopls-mcp-backends '(shared standalone stdio)
  "Ordered fallback list of gopls MCP backends to try.
Resolution stops at the first viable backend.  Recognized symbols:

  `shared': connect to a running `gopls serve -mcp.listen=...'
     daemon (typically managed by systemd).  Viable when
     something is listening on `tychoish/gopls-mcp-port'.
  `standalone': spawn (and reuse) a dedicated `gopls mcp -listen=...'
     process from Emacs.  Viable when `gopls' is on PATH.
  `stdio': declare a per-client `gopls mcp' stdio command; each
     MCP client launches its own gopls.  Always viable when
     `gopls' is on PATH.
  `auto-remote': declare a stdio entry that runs `gopls -remote=auto
     mcp', letting gopls's daemon-discovery logic find or
     spawn the lsp daemon.  Note: as of current gopls, the
     `mcp' subcommand does not actually consume `-remote',
     so this behaves the same as `stdio' but is kept as a
     documented option.
  `none': disable the gopls entry entirely.")

(defvar tychoish/gopls-mcp--standalone-process nil
  "Process handle for an Emacs-spawned `gopls mcp -listen' instance.")

(defvar tychoish/mcp-servers nil
  "Normalized MCP server specs shared by mcp.el and agent-shell.
Populated lazily by `tychoish/mcp-servers-init' the first time either
`mcp-hub' or `agent-shell' loads, so PATH lookups don't run at startup.
The gopls entry is resolved dynamically via `tychoish/gopls-mcp-resolve'
and prepended by `tychoish/mcp-build-servers'.
Each entry is a plist with one of:
  (:name NAME :command CMD :args (ARGS...) [:env ((K . V) ...)] [:disabled t])
  (:name NAME :url URL [:transport http|sse] [:headers ((K . V) ...)] [:disabled t])
Entries with `:disabled t' are excluded by `tychoish/mcp-build-servers'.")

(defun tychoish/mcp-servers-init (&optional force)
  "Populate `tychoish/mcp-servers' if not already set.
With prefix arg FORCE, reset and repopulate unconditionally."
  (interactive "P")
  (when (or force (null tychoish/mcp-servers))
    (setq tychoish/mcp-servers nil)
    ;; (push '(:name "git" :command "uvx" :args ("mcp-server-git") :disabled t) tychoish/mcp-servers)
    ;; (push '(:name "gh" :command "gh" :args ("mcp") :disabled t) tychoish/mcp-servers)
    (push '(:name "rg" :command "npx" :args ("-y" "mcp-ripgrep@latest") :disabled t) tychoish/mcp-servers)

    ;; (push '(:name "godoc" :command "godoc-mcpr") tychoish/mcp-servers)
    ;; (push '(:name "time" :command "uvx" :args ("mcp-server-time")) tychoish/mcp-servers)
    ;; (push '(:name "fetch" :command "uvx" :args ("mcp-server-fetch")) tychoish/mcp-servers)
    ;; (push '(:name "awsdoc" :command "awslabs.aws-documentation-mcp-server") tychoish/mcp-servers)
    ;; (push (list :name "lsp-mcp-rust" :command "npx" :args (list "tritlo/lsp-mcp" "rust" (executable-find "rust-analyzer"))) tychoish/mcp-servers)
    ;; (push (list :name "lsp-mcp-bash" :command "npx" :args (list "tritlo/lsp-mcp" "bash" (executable-find "bash-language-server") "start")) tychoish/mcp-servers)
    ;; (push (list :name "lsp-mcp-yaml" :command "npx" :args (list "tritlo/lsp-mcp" "yaml" (executable-find "yaml-language-server") "--stdio")) tychoish/mcp-servers)
    ;; (push '(:name "github" :command "npx" :args ("-y" "mcp-remote" "https://api.githubcopilot.com/mcp")) tychoish/mcp-servers)
    ;; (push '(:name "linear" :command "npx" :args ("-y" "mcp-remote" "https://mcp.linear.app/mcp")) tychoish/mcp-servers)
    ;; (push '(:name "notion" :command "npx" :args ("-y" "mcp-remote" "https://mcp.notion.com/mcp")) tychoish/mcp-servers)
    ;; (push '(:name "google-workspace" :command "uvx" :args ("workspace-mcp")) tychoish/mcp-servers)

    (message "mcp-servers-init: %d servers registered (%d active)"
             (length tychoish/mcp-servers)
             (cl-count-if-not (lambda (s) (plist-get s :disabled)) tychoish/mcp-servers))))

(defun tychoish/gopls-mcp--port-alive-p ()
  "Return non-nil if something is accepting TCP on `tychoish/gopls-mcp-port'."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "gopls-mcp-probe"
                   :host "127.0.0.1"
                   :service tychoish/gopls-mcp-port
                   :nowait nil
                   :noquery t
                   :buffer nil)))
        (delete-process proc)
        t)
    (error nil)))

(defun tychoish/gopls-mcp--ensure-standalone ()
  "Start a standalone `gopls mcp -listen' on `tychoish/gopls-mcp-port'.
Reuses any existing live process.  Returns the process or nil on failure."
  (unless (and tychoish/gopls-mcp--standalone-process
               (process-live-p tychoish/gopls-mcp--standalone-process))
    (when (executable-find "gopls")
      (setq tychoish/gopls-mcp--standalone-process
            (make-process
             :name "gopls-mcp"
             :buffer (get-buffer-create " *gopls-mcp*")
             :command (list "gopls" "mcp"
                            (format "-listen=127.0.0.1:%d" tychoish/gopls-mcp-port))
             :noquery t))))
  tychoish/gopls-mcp--standalone-process)

(defun tychoish/gopls-mcp--resolve-one (backend)
  "Return a normalized MCP server spec for BACKEND, or nil if not viable."
  (pcase backend
    ('none nil)
    ('shared (when (tychoish/gopls-mcp--port-alive-p)
	       `(:name "gopls" :url ,(format "http://127.0.0.1:%d" tychoish/gopls-mcp-port) :transport http :disabled t)))
    ('standalone (when (tychoish/gopls-mcp--ensure-standalone)
		   `(:name "gopls" :url ,(format "http://127.0.0.1:%d" tychoish/gopls-mcp-port) :transport http :disabled t)))
    ('stdio (when (executable-find "gopls")
	      `(:name "gopls" :command "gopls" :args ("mcp")  :disabled t)))
    ('auto-remote (when (executable-find "gopls")
		    `(:name "gopls" :command "gopls" :args ("-remote=auto" "mcp") :disabled t)))))

(defun tychoish/gopls-mcp-resolve ()
  "Resolve a gopls MCP spec by walking `tychoish/gopls-mcp-backends'."
  (cl-some #'tychoish/gopls-mcp--resolve-one tychoish/gopls-mcp-backends))

(defun tychoish/mcp-build-servers ()
  "Build the active MCP server list with a freshly-resolved gopls entry.
Servers with `:disabled t' in `tychoish/mcp-servers' are excluded."
  (tychoish/mcp-servers-init)
  (let* ((active (cl-remove-if (lambda (s) (plist-get s :disabled)) tychoish/mcp-servers))
         (gopls (tychoish/gopls-mcp-resolve)))
    (if gopls
        (cons gopls active)
      active)))

(defun tychoish/mcp-spec->hub (spec)
  "Translate SPEC to a `mcp-hub-servers' alist entry: (NAME . PLIST)."
  (let* ((name (plist-get spec :name))
         (url  (plist-get spec :url)))
    (cons name
          (if url
              (list :url url)
            (let ((plist (list :command (plist-get spec :command))))
              (when-let* ((args (plist-get spec :args)))
                (setq plist (plist-put plist :args args)))
              plist)))))

(defun tychoish/mcp-spec->acp (spec)
  "Translate SPEC to an `agent-shell-mcp-servers' ACP McpServer alist."
  (let ((name (plist-get spec :name))
        (url  (plist-get spec :url)))
    (if url
        `((name . ,name)
          (type . ,(symbol-name (or (plist-get spec :transport) 'http)))
          (url . ,url)
          (headers . ,(or (plist-get spec :headers) '())))
      `((name . ,name)
        (command . ,(plist-get spec :command))
        (args . ,(or (plist-get spec :args) '()))
        (env . ,(or (plist-get spec :env) '()))))))

 
