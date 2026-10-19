;;; eglox.el --- Eglot extensions and stability enhancements -*- lexical-binding: t; -*-

;; Author: Tycho Garen
;; Keywords: languages, tools, lsp, eglot
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Extensions, throttled activation, server lifecycle management, keymaps,
;; and stability enhancements for Eglot (Emacs LSP client).
;;
;; Features:
;; - `eglox-mode': Buffer-local minor mode for Eglot-managed buffers, bundling
;;   save hooks, import organization, and buffer-local LSP keybindings.
;; - `eglox-global-mode': Global minor mode managing connection throttling,
;;   server deduplication, dead server pruning, and projectile cache warming.
;; - `eglox-global-map' and `eglox-mode-map': Dedicated keymaps for global and
;;   buffer-local LSP commands.
;; - Rate-limited buffer activation to avoid connection storms.
;; - Dead and duplicate server pruning to prevent resource leaks and reconnect loops.
;; - Projectile cache warming integration on project switch.
;; - Guarded completion-at-point to prevent repeating jsonrpc errors.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'hud-mode)

(declare-function projectile-project-root "projectile")
(declare-function projectile-index-project-async "projectile")
(declare-function projectile-load-project-cache "projectile")
(declare-function eglot-tempel-mode "eglot-tempel")
(declare-function flycheck-eglot-mode "flycheck-eglot")
(declare-function execute-extended-eglot-command "hud-mode")
(declare-function consult-eglot-symbols "consult-eglot")
(declare-function eglot-test-at-point "eglot-test-at-point")

(declare-function eglot "eglot")
(declare-function eglot-reconnect "eglot")
(declare-function eglot-shutdown "eglot")
(declare-function eglot-list-connections "eglot")
(declare-function eglot-forget-pending-continuations "eglot")
(declare-function eglot-rename "eglot")
(declare-function eglot-code-actions "eglot")
(declare-function eglot-code-action-quickfix "eglot")
(declare-function eglot-code-action-rewrite "eglot")
(declare-function eglot-code-action-organize-imports "eglot")
(declare-function eglot-format-buffer "eglot")
(declare-function eglot-signal-didChangeConfiguration "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot--current-server-or-lose "eglot")
(declare-function eglot-ensure "eglot")
(declare-function eglox-activate-enqueue "eglox")
(declare-function eglot-managed-p "eglot")
(declare-function eglot--lookup-mode "eglot")
(declare-function eglot--maybe-activate-editing-mode "eglot")
(declare-function eglot-alternatives "eglot")
(declare-function eglot--project "eglot")
(declare-function eglot--shutdown-requested "eglot")
(declare-function eglot--managed-mode-off "eglot")
(declare-function jsonrpc-shutdown "jsonrpc")
(declare-function jsonrpc--process "jsonrpc")
(declare-function eieio-oset "eieio-core")

(defvar projectile-projects-cache)
(defvar projectile-enable-caching)
(defvar projectile-project-changed-functions)
(defvar hud-ide-map)
(defvar eglot-tempel-mode nil)
(defvar eglot--cached-server)
(defvar eglot-menu-string)
(defvar eglot-autoshutdown)
(defvar eglot-extend-to-xref)
(defvar eglot-report-progress)
(defvar eglot-stay-out-of)
(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)
(defvar eglot-mode-map)
(defvar eglot--managed-mode)
(defvar eglot--servers-by-project (make-hash-table :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymaps

;;;###autoload
(defvar-keymap eglox-global-map
  :name "eglot"
  :doc "Eglot global commands under C-c l l."
  "s" #'eglot
  "r" #'eglot-reconnect
  "k" #'eglot-shutdown
  "l" #'eglot-list-connections
  "c" #'execute-extended-eglot-command
  "g" #'eglot-forget-pending-continuations)

;;;###autoload
(defvar-keymap eglox-mode-map
  :name "eglot-mode"
  :doc "Eglot buffer commands."
  "e" #'execute-extended-eglot-command
  "s" #'consult-eglot-symbols
  "r" #'eglot-rename
  "a" #'eglot-code-actions
  "i" #'eglox-code-action-inline
  "f" #'eglot-format-buffer
  "o" #'eglox-organize-imports-for-hook
  "q" #'eglot-code-action-quickfix
  "w" #'eglot-code-action-rewrite
  "t" #'eglot-test-at-point
  "u" #'eglox-update-workspace)

(defvar-keymap eglox-minor-mode-map
  :doc "Keymap active when `eglox-mode' is enabled."
  "C-c l" (cons "eglot" eglox-mode-map))

(defconst eglox-mode-hooks
  '(js-mode-hook
    js-ts-mode-hook
    typescript-ts-mode-hook
    go-ts-mode-hook
    go-mod-ts-mode-hook
    yaml-mode-hook
    rustic-mode-hook
    c++-ts-mode-hook
    c-ts-mode-hook
    c-or-c++-mode-hook
    python-mode-hook
    python-ts-mode-hook
    go-mode-hook
    go-mod-mode-hook
    bash-ts-mode-hook
    bash-mode-hook
    sh-mode-hook)
  "Major mode hooks where `eglox-ensure' is installed.")

(defvar eglox-default-server-configuration
  `((:yaml :format
           (:enable t
            :singleQuote :json-false
            :bracketSpacing t
            :proseWrap "preserve"
            :printWidth 80)
           :validate t
           :hover t
           :completion t)
    (:gopls :gofumpt t
            :usePlaceholders :json-false
            :hoverKind "FullDocumentation"
            :linkTarget "pkg.go.dev"
            :analyses (:unreachable t
                       :unusedvariable t)
            :codelenses (:test t
                         :tidy t
                         :upgradeDependency t
                         :generate t)
            :hints (:parameterNames :json-false
                    :ignoredError t
                    :constantValues t
                    :compositeLiteralTypes :json-false
                    :compositeLiteralFields :json-false
                    :rangeVariableTypes :json-false
                    :functionTypeParameters :json-false))
    (:rust-analyzer :initializationOptions
                    (:server (:extraEnv (:RUSTUP_TOOLCHAIN ,(if (executable-find "rustup") "nightly" "stable")))
                     :rust (:analyzerTargetDir t)
                     :cargo (:buildScripts (:enable t :features "all"))
                     :procMacro (:enable :json-false :attributes (:enable t))
                     :check (:workspace t)))
    (:pylsp :plugins
            (:black (:enabled t)
             :jedi_completion (:enabled t
                               :fuzzy t
                               :include_params t)
             :ruff (:enabled t
                    :formatEnabled t
                    :lineLength 100
                    :format ["I"]
                    :extendSelect ["I"])
             :rope (:enabled t)
             :flake8 (:enabled :json-false)
             :pycodestyle (:enabled :json-false)
             :mccabe (:enabled :json-false)
             :autopep8 (:enabled :json-false)
             :pyflakes (:enabled :json-false))))
  "Default Eglot workspace configuration.")

(defvar eglox--eglot-initialized nil
  "Non-nil when Eglot settings and integrations have been applied.")

(defun eglox--init-eglot (&rest _args)
  "Initialize Eglot settings, server programs, and capabilities.
Rest argument _ARGS is ignored so this can be used on hooks or advices."
  (unless eglox--eglot-initialized
    (setq eglox--eglot-initialized t)
    (let ((gc-cons-threshold most-positive-fixnum))
      (require 'eglot))
    (setq eglot-menu-string "eg")
    (setq eglot-autoshutdown t)
    (setq eglot-extend-to-xref t)
    (setq eglot-report-progress nil)
    (add-to-list 'eglot-stay-out-of 'flymake)
    (add-to-list 'eglot-stay-out-of 'company)
    (set-face-attribute 'eglot-highlight-symbol-face nil :underline nil :weight 'bold)
    (add-to-list 'eglot-server-programs
                 `((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
                   . ("gopls")))
    (add-to-list 'eglot-server-programs
                 `((c-mode c++-mode c-ts-mode c++-ts-mode c-or-c++-ts-mode c-or-c++-mode)
                   . ,(eglot-alternatives
                       `(("clangd" "-j=8" "--background-index" "--clang-tidy" "--cross-file-rename" "--completion-style=detailed")
                         ("clangd")
                         ("ccls")))))
    (setq-default eglot-workspace-configuration eglox-default-server-configuration)
    (keymap-set eglot-mode-map "C-c l" (cons "eglot" eglox-mode-map))
    (cl-defmethod eglot-client-capabilities :around (_server)
      (let* ((caps (cl-call-next-method))
             (workspace (plist-get caps :workspace)))
        (plist-put workspace :didChangeWatchedFiles
                   (list :dynamicRegistration :json-false))
        caps))
    (eglox--install-eglot-integrations)))

(defun eglox--after-load-hook (file)
  "Hook for `after-load-functions' to initialize Eglot when its FILE loads."
  (when (string-match-p "/eglot\\(\\.\\|\\'\\)" file)
    (eglox--init-eglot)))

;;;###autoload
(defun eglox-ensure ()
  "Initialize Eglot and ensure it manages the current buffer."
  (interactive)
  (eglox--init-eglot)
  (eglot-ensure))

(add-hook 'after-load-functions #'eglox--after-load-hook)
(advice-add 'eglot :before #'eglox--init-eglot)
(advice-add 'eglot-reconnect :before #'eglox--init-eglot)
(advice-add 'eglot-ensure :before #'eglox--init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Managed Mode Hook & Completion Guard

(defun eglox-ensure-hook ()
  "Enable `eglot-tempel-mode' if inactive, and cycle `flycheck-eglot-mode'.
Avoids reconnect loops by only activating `eglot-tempel-mode' when disabled."
  (unless eglot-tempel-mode
    (eglot-tempel-mode 1))
  (flycheck-eglot-mode -1)
  (flycheck-eglot-mode 1))

(defun ad:eglot-completion-at-point-guard-no-server (orig-fn)
  "Return nil if no Eglot server is connected instead of signaling error.
Prevents repeating jsonrpc errors when completion timers run after mode
teardown."
  (when (eglot-current-server)
    (funcall orig-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projectile Cache Warming

(defconst eglox-projectile-warm-cache-idle-delay 3
  "Seconds of idle time between warming queued Projectile caches.")

(defconst eglox-projectile-warm-cache-chunk-size 3
  "Maximum cold project roots to index per idle timer tick.")

(defvar eglox-projectile-warm-cache-queue nil
  "Project roots queued for background Projectile indexing.")

(defvar eglox-projectile-warm-cache-idle-timer nil
  "Idle timer that drains `eglox-projectile-warm-cache-queue'.")

(defun eglox-projectile-cache-warm-p (root)
  "Return non-nil if ROOT has an in-memory or on-disk Projectile cache."
  (and (bound-and-true-p projectile-enable-caching)
       (or (map-elt projectile-projects-cache root)
           (and (eq projectile-enable-caching 'persistent)
                (projectile-load-project-cache root)))))

(defun eglox-projectile-warm-cache-process-queue ()
  "Index cold projects from `eglox-projectile-warm-cache-queue' in batches.
Cancels `eglox-projectile-warm-cache-idle-timer' when the queue is empty."
  (if (null eglox-projectile-warm-cache-queue)
      (when eglox-projectile-warm-cache-idle-timer
        (cancel-timer eglox-projectile-warm-cache-idle-timer)
        (setq eglox-projectile-warm-cache-idle-timer nil))
    (dotimes (_ eglox-projectile-warm-cache-chunk-size)
      (when-let* ((root (pop eglox-projectile-warm-cache-queue)))
        (unless (eglox-projectile-cache-warm-p root)
          (projectile-index-project-async root))))))

(defun eglox-projectile-warm-cache-enqueue (root)
  "Queue ROOT for background Projectile indexing if not already warm."
  (unless (or (member root eglox-projectile-warm-cache-queue)
              (eglox-projectile-cache-warm-p root))
    (setq eglox-projectile-warm-cache-queue
          (append eglox-projectile-warm-cache-queue (list root))))
  (unless eglox-projectile-warm-cache-idle-timer
    (setq eglox-projectile-warm-cache-idle-timer
          (run-with-idle-timer eglox-projectile-warm-cache-idle-delay t
                               #'eglox-projectile-warm-cache-process-queue))))

(defun eglox-projectile-warm-cache-for-buffer ()
  "Queue background Projectile indexing for the current buffer's project root."
  (when-let* ((root (and (bound-and-true-p projectile-mode)
                         (projectile-project-root))))
    (eglox-projectile-warm-cache-enqueue root)))

(defun eglox-projectile-warm-cache-on-project-change (_new-root _previous-root)
  "Warm Projectile cache when switching projects."
  (eglox-projectile-warm-cache-for-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save Hooks & Actions

(defun eglox-server-live-p ()
  "Return non-nil if the current buffer's Eglot server process is running."
  (when-let* ((server (eglot-current-server)))
    (process-live-p (jsonrpc--process server))))

;;;###autoload
(defun eglox-format-for-hook ()
  "Format the current buffer using Eglot if managed.
When the buffer's server has died, reconnect orphaned buffers instead."
  (interactive)
  (when (eglot-managed-p)
    (if (eglox-server-live-p)
        (eglot-format-buffer)
      (eglox-cleanup-stale-connections))))

;;;###autoload
(defun eglox-organize-imports-for-hook ()
  "Organize imports for the current buffer using Eglot if managed.
When the buffer's server has died, reconnect orphaned buffers instead."
  (interactive)
  (when (eglot-managed-p)
    (if (eglox-server-live-p)
        (with-demoted-errors "WARN (`eglox-organize-imports-for-hook'): %S"
          (eglot-code-action-organize-imports (point-min) (point-max)))
      (eglox-cleanup-stale-connections))))

;;;###autoload
(defun eglox-code-action-inline ()
  "Execute \"source.inline\" code action if managed."
  (interactive)
  (when (eglot-managed-p)
    (with-demoted-errors "WARN (`eglox-code-action-inline'): %S"
      (eglot-code-actions nil nil "source.inline" t))))

;;;###autoload
(defun eglox-update-workspace ()
  "Push updated workspace configuration to the active Eglot server."
  (interactive)
  (eglot-signal-didChangeConfiguration (eglot--current-server-or-lose)))

(defun eglox-before-save-hook ()
  "Add buffer-local format and import organization to `before-save-hook'."
  (add-hook 'before-save-hook #'eglox-format-for-hook nil t)
  (add-hook 'before-save-hook #'eglox-organize-imports-for-hook nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server Pruning & Connection Lifecycle

(defun eglox-prune-dead-servers ()
  "Remove non-live server processes from `eglot--servers-by-project'."
  (map-do
   (lambda (project servers)
     (let ((live (seq-filter (lambda (s) (process-live-p (jsonrpc--process s))) servers)))
       (unless (eq (length live) (length servers))
         (setf (map-elt eglot--servers-by-project project) live))))
   eglot--servers-by-project))

(defun eglox-prune-duplicate-servers (&optional _server)
  "Keep at most one server per project in `eglot--servers-by-project'.
Shuts down duplicate servers. Optional _SERVER argument is ignored for
compatibility with `eglot-connect-hook'."
  (map-do
   (lambda (project servers)
     (when (cdr servers)
       (setf (map-elt eglot--servers-by-project project) (list (car servers)))
       (seq-do (lambda (s)
                 (eieio-oset s 'shutdown-requested t)
                 (ignore-errors (jsonrpc-shutdown s t)))
               (cdr servers))))
   eglot--servers-by-project))

(defun eglox-reconnect-orphaned-buffers ()
  "Re-run `eglot-ensure' on live file buffers whose cached server has died."
  (seq-do
   (lambda (buffer)
     (with-current-buffer buffer
       (when (and (bound-and-true-p eglot--managed-mode)
                  (bound-and-true-p eglot--cached-server)
                  (not (process-live-p (jsonrpc--process eglot--cached-server))))
         (setq eglot--cached-server nil)
         (ignore-errors (eglot--managed-mode-off))
         (when buffer-file-name
           (ignore-errors (eglot-ensure))))))
   (buffer-list)))

;;;###autoload
(defun eglox-ensure-unmanaged-buffers ()
  "Run `eglot-ensure' in file buffers of `eglox-mode-hooks' modes lacking Eglot.
Buffers are queued for throttled activation against an existing server, and
`eglot-ensure' covers projects with no server yet, so this does not start a
connection storm.  Return the number of buffers scheduled."
  (interactive)
  (let* ((modes (seq-map (lambda (hook)
                           (intern (string-remove-suffix "-hook" (symbol-name hook))))
                         eglox-mode-hooks))
         (buffers (seq-filter
                   (lambda (buffer)
                     (with-current-buffer buffer
                       (and buffer-file-name
                            (apply #'derived-mode-p modes)
                            (not (bound-and-true-p eglot--managed-mode)))))
                   (buffer-list))))
    (seq-do (lambda (buffer)
              (with-current-buffer buffer
                (ignore-errors (eglot-ensure)))
              (eglox-activate-enqueue buffer))
            buffers)
    (when (called-interactively-p 'interactive)
      (message "eglox: scheduled eglot in %d buffer(s)" (length buffers)))
    (length buffers)))

;;;###autoload
(defun eglox-cleanup-stale-connections ()
  "Prune dead and duplicate servers, then reconnect orphaned buffers."
  (interactive)
  (eglox-prune-dead-servers)
  (eglox-prune-duplicate-servers)
  (eglox-reconnect-orphaned-buffers))

(defvar eglox-connecting-projects nil
  "List of project roots currently establishing an initial Eglot connection.")

(defun eglox-mark-connecting (server)
  "Add SERVER's project to `eglox-connecting-projects'."
  (let ((project (eglot--project server)))
    (unless (member project eglox-connecting-projects)
      (push project eglox-connecting-projects))))

(defun eglox-unmark-connecting (server)
  "Remove SERVER's project from `eglox-connecting-projects'."
  (setq eglox-connecting-projects
        (delete (eglot--project server) eglox-connecting-projects)))

(defun ad:eglot--connect-dedupe (orig-fn managed-modes project class contact language-ids)
  "Skip redundant `eglot--connect' calls for PROJECT.
Returns an existing live server if present, or nil if a connection handshake
is currently in flight for PROJECT."
  (cond
   ((let ((existing (car (gethash project eglot--servers-by-project))))
      (and existing (process-live-p (jsonrpc--process existing)) existing)))
   ((member project eglox-connecting-projects) nil)
   (t (funcall orig-fn managed-modes project class contact language-ids))))

(defun ad:eglot--on-shutdown-cleanup-stale (orig-fn server)
  "Clean up stale connections when SERVER exits unexpectedly."
  (let ((crashed (not (eglot--shutdown-requested server))))
    (eglox-unmark-connecting server)
    (ignore-errors (funcall orig-fn server))
    (when crashed
      (eglox-cleanup-stale-connections))))

(defvar eglox-cleanup-stale-connections-timer nil
  "Idle timer running `eglox-cleanup-stale-connections'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Throttled Buffer Activation

(defconst eglox-activate-burst-limit 3
  "Maximum buffers activated within `eglox-activate-burst-window' before queuing.")

(defconst eglox-activate-burst-window 2
  "Burst window in seconds for `eglox-activate-burst-limit'.")

(defconst eglox-activate-batch-delay 3
  "Delay in seconds between drains of `eglox-activate-queue'.")

(defvar eglox-activate-queue nil
  "Buffers queued for throttled `eglot--maybe-activate-editing-mode'.")

(defvar eglox-activate-recent nil
  "Timestamps of recent unthrottled activations (newest first).")

(defvar eglox-activate-timer nil
  "Timer draining `eglox-activate-queue', or nil when idle.")

(defconst eglox-activate-log-interval 60
  "Minimum seconds between queue size log messages from the drain timer.")

(defvar eglox-activate-last-log 0.0
  "Float time of the last `eglox-activate-queue' size log message.")

(defun eglox-activate-buffer-pending-p (buffer)
  "Return non-nil if BUFFER is live, non-empty, and not yet managed by Eglot."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (not (zerop (buffer-size)))
              (not (bound-and-true-p eglot--managed-mode))))))

(defun eglox-activate-prune-queue ()
  "Drop dead, empty, and already managed buffers from `eglox-activate-queue'.
Return the number of buffers removed."
  (let ((before (length eglox-activate-queue)))
    (setq eglox-activate-queue
          (seq-filter #'eglox-activate-buffer-pending-p eglox-activate-queue))
    (- before (length eglox-activate-queue))))

;;;###autoload
(defun eglox-activate-queue-size ()
  "Return the number of pending buffers in `eglox-activate-queue'.
Dead, empty, and managed buffers are pruned first."
  (eglox-activate-prune-queue)
  (length eglox-activate-queue))

;;;###autoload
(defun eglox-activate-queue-report ()
  "Report the activation queue size and drain timer state in the echo area."
  (interactive)
  (let ((pruned (eglox-activate-prune-queue)))
    (message "eglox: %d buffer(s) queued for activation (%d pruned), drain timer %s"
             (length eglox-activate-queue) pruned
             (if (timerp eglox-activate-timer) "active" "idle"))))

(defun eglox-activate-log-queue (pruned)
  "Log the size of `eglox-activate-queue' and PRUNED count at most once per interval."
  (when (>= (- (float-time) eglox-activate-last-log) eglox-activate-log-interval)
    (setq eglox-activate-last-log (float-time))
    (message "eglox: %d buffer(s) queued for activation (%d pruned)"
             (length eglox-activate-queue) pruned)))

(defun eglox-activate-process-queue ()
  "Prune, then drain up to `eglox-activate-burst-limit' queued buffers per tick.
Cancels `eglox-activate-timer' when the queue is empty."
  (let ((pruned (eglox-activate-prune-queue)))
    (if (null eglox-activate-queue)
        (when eglox-activate-timer
          (cancel-timer eglox-activate-timer)
          (setq eglox-activate-timer nil))
      (eglox-activate-log-queue pruned)
      (dotimes (_ eglox-activate-burst-limit)
        (when-let* ((buf (pop eglox-activate-queue)))
          (ignore-errors
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (eglot--maybe-activate-editing-mode)))))))))

;;;###autoload
(defun eglox-activate-flush-queue (&optional count)
  "Activate up to COUNT queued buffers now, bypassing the burst throttle.
COUNT defaults to 25; interactively, use a numeric prefix argument to change it.
Return the number of buffers left in the queue."
  (interactive "P")
  (eglox-activate-prune-queue)
  (dotimes (_ (if count (prefix-numeric-value count) 25))
    (when-let* ((buf (pop eglox-activate-queue)))
      (ignore-errors
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((eglox-activate-recent nil))
              (eglot--maybe-activate-editing-mode)))))))
  (when (called-interactively-p 'interactive)
    (message "eglox: %d buffer(s) still queued" (length eglox-activate-queue)))
  (length eglox-activate-queue))

(defun eglox-activate-enqueue (buffer)
  "Add BUFFER to `eglox-activate-queue' if pending, and start drain timer if needed."
  (eglox-activate-prune-queue)
  (when (and (eglox-activate-buffer-pending-p buffer)
             (not (memq buffer eglox-activate-queue)))
    (setq eglox-activate-queue
          (append eglox-activate-queue (list buffer))))
  (when (and eglox-activate-queue (not eglox-activate-timer))
    (setq eglox-activate-timer
          (run-with-timer eglox-activate-batch-delay
                          eglox-activate-batch-delay
                          #'eglox-activate-process-queue))))

(defun ad:eglot--maybe-activate-editing-mode-throttle (orig-fn)
  "Rate-limit buffer activation to prevent connection storms.
Buffers without a file or configured server bypass throttling.
Surplus buffers are queued in `eglox-activate-queue'."
  (setq eglox-activate-recent
        (seq-filter (lambda (ts) (< (float-time (time-since ts))
                                    eglox-activate-burst-window))
                    eglox-activate-recent))
  (cond
   ((not (and buffer-file-name (cdr (eglot--lookup-mode major-mode))))
    (funcall orig-fn))
   ((< (length eglox-activate-recent) eglox-activate-burst-limit)
    (push (current-time) eglox-activate-recent)
    (funcall orig-fn))
   (t
    (eglox-activate-enqueue (current-buffer)))))

(defun eglox-activate-prioritize-visible (&optional window-or-frame)
  "Immediately activate WINDOW-OR-FRAME's buffer if pending in the throttle queue."
  (let* ((win (cond
               ((windowp window-or-frame) window-or-frame)
               ((framep window-or-frame) (frame-selected-window window-or-frame))
               (t (selected-window))))
         (buf (and (window-live-p win) (window-buffer win))))
    (when (and buf (memq buf eglox-activate-queue))
      (setq eglox-activate-queue (delq buf eglox-activate-queue))
      (with-current-buffer buf
        (eglot--maybe-activate-editing-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor Modes (Local & Global)

;;;###autoload
(define-minor-mode eglox-mode
  "Buffer-local minor mode for Eglot enhancements in managed buffers."
  :lighter " eglox"
  :keymap eglox-minor-mode-map
  (if eglox-mode
      (progn
        (eglox-ensure-hook)
        (eglox-before-save-hook))
    (remove-hook 'before-save-hook #'eglox-format-for-hook t)
    (remove-hook 'before-save-hook #'eglox-organize-imports-for-hook t)))

(defun eglox--managed-mode-sync ()
  "Synchronize `eglox-mode' with `eglot--managed-mode'."
  (eglox-mode (if (bound-and-true-p eglot--managed-mode) 1 -1)))

(defvar eglox-global-mode)

(defun eglox--install-eglot-integrations ()
  "Install advices, hooks, and background timers for Eglot.
Only called when `eglot' is loaded and `eglox-global-mode' is enabled."
  (when eglox-global-mode
    (advice-add 'eglot-completion-at-point :around #'ad:eglot-completion-at-point-guard-no-server)
    (advice-add 'eglot--connect :around #'ad:eglot--connect-dedupe)
    (advice-add 'eglot--on-shutdown :around #'ad:eglot--on-shutdown-cleanup-stale)
    (advice-add 'eglot--maybe-activate-editing-mode :around #'ad:eglot--maybe-activate-editing-mode-throttle)
    (add-hook 'eglot-managed-mode-hook #'eglox--managed-mode-sync)
    (add-hook 'eglot-connect-hook #'eglox-prune-duplicate-servers)
    (add-hook 'eglot-server-initialized-hook #'eglox-mark-connecting)
    (add-hook 'eglot-connect-hook #'eglox-unmark-connecting)
    (unless (timerp eglox-cleanup-stale-connections-timer)
      (setq eglox-cleanup-stale-connections-timer
            (run-with-idle-timer 300 t #'eglox-cleanup-stale-connections)))))

(defun eglox--uninstall-eglot-integrations ()
  "Remove advices, hooks, and background timers for Eglot."
  (advice-remove 'eglot-completion-at-point #'ad:eglot-completion-at-point-guard-no-server)
  (advice-remove 'eglot--connect #'ad:eglot--connect-dedupe)
  (advice-remove 'eglot--on-shutdown #'ad:eglot--on-shutdown-cleanup-stale)
  (advice-remove 'eglot--maybe-activate-editing-mode #'ad:eglot--maybe-activate-editing-mode-throttle)
  (remove-hook 'eglot-managed-mode-hook #'eglox--managed-mode-sync)
  (remove-hook 'eglot-connect-hook #'eglox-prune-duplicate-servers)
  (remove-hook 'eglot-server-initialized-hook #'eglox-mark-connecting)
  (remove-hook 'eglot-connect-hook #'eglox-unmark-connecting)
  (when (timerp eglox-cleanup-stale-connections-timer)
    (cancel-timer eglox-cleanup-stale-connections-timer)
    (setq eglox-cleanup-stale-connections-timer nil)))

(defun eglox--enable-global ()
  "Enable global Eglot extensions, mode hooks, and projectile integration."
  (add-hook 'projectile-project-changed-functions #'eglox-projectile-warm-cache-on-project-change)
  (add-hook 'window-selection-change-functions #'eglox-activate-prioritize-visible)
  (dolist (hook eglox-mode-hooks)
    (add-hook hook #'eglox-ensure))
  (keymap-set hud-ide-map "l" (cons "eglot" eglox-global-map))
  (when eglox--eglot-initialized
    (eglox--install-eglot-integrations)))

(defun eglox--disable-global ()
  "Disable global Eglot extensions, mode hooks, timers, and advices."
  (remove-hook 'projectile-project-changed-functions #'eglox-projectile-warm-cache-on-project-change)
  (remove-hook 'window-selection-change-functions #'eglox-activate-prioritize-visible)
  (dolist (hook eglox-mode-hooks)
    (remove-hook hook #'eglox-ensure))
  (eglox--uninstall-eglot-integrations)
  (when (timerp eglox-activate-timer)
    (cancel-timer eglox-activate-timer)
    (setq eglox-activate-timer nil))
  (when (timerp eglox-projectile-warm-cache-idle-timer)
    (cancel-timer eglox-projectile-warm-cache-idle-timer)
    (setq eglox-projectile-warm-cache-idle-timer nil)))

;;;###autoload
(define-minor-mode eglox-global-mode
  "Global minor mode bundling Eglot enhancements and stability protections."
  :global t
  :group 'eglot
  (if eglox-global-mode
      (eglox--enable-global)
    (eglox--disable-global)))

(provide 'eglox)
;;; eglox.el ends here
