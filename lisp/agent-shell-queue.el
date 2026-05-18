;;; agent-shell-queue.el --- Persistent prompt queue for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell

;; This file is not part of GNU Emacs

;;; Code:

(require 'cl-lib)
(require 'agent-shell)
(require 'annotated-completing-read)
(require 'ht)
(require 'transient)
(require 'alert)

(defface agent-shell-queue-blocked-face
  '((t :foreground "darkorange3"))
  "Face for queue items that are paused, deferred, or blocked.")

(defface agent-shell-queue-unassigned-face
  '((t :foreground "cornflowerblue"))
  "Face for queue items not yet assigned to any shell.")

(defface agent-shell-queue-compact-face
  '((t :foreground "steelblue3"))
  "Face for compact (non-LLM manual) work items.")

(defface agent-shell-queue-draft-face
  '((t :foreground "gray50" :slant italic))
  "Face for queue items saved as drafts (not yet queued for dispatch).")

(defconst agent-shell-queue--unassigned-key "(unassigned)"
  "Alist bucket key for items not yet assigned to any shell.")

(declare-function shell-maker-busy "shell-maker")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function markdown-mode "markdown-mode")

(defun agent-shell-queue--default-instance-name ()
  "Return a string identifying the current Emacs instance."
  (let ((d (daemonp)))
    (cond ((eq d t) "primary")
          (d d)
          (t (system-name)))))

(defvar agent-shell-queue-instance-name #'agent-shell-queue--default-instance-name
  "Instance identifier written into archive records.
May be a string or a zero-argument function that returns a string.
Defaults to the daemon name or system hostname.  Override in config:
  (setq agent-shell-queue-instance-name \"<name>\")
  (setq agent-shell-queue-instance-name \\='get-instance-name")

;;; Configuration

(defvar agent-shell-queue-serialization-format 'plist
  "Format used to persist queue state to disk.
One of:
  `plist' — s-expression with keyword-keyed plists (default; no extra deps)
  `json'  — JSON via built-in `json-serialize'/`json-parse-string' (Emacs 27+)
  `yaml'  — YAML via `yaml-encode'/`yaml-parse-string' from the `yaml' package")

(defvar agent-shell-queue-idle-delay 60.0
  "Idle delay in seconds for the backup auto-send timer.
Primary draining happens via `shell-maker-finish-output' advice; this timer
is only a safety net for buffers that become idle outside that path.")

(defvar agent-shell-queue-background-prefix "/background "
  "String prepended to prompts flagged for background sub-agent execution.")

(defvar agent-shell-queue-clear-command "/clear"
  "Command string sent when a clear item is dequeued.")

(defvar agent-shell-queue-done-log-file nil
  "File path for appending completed queue items as JSON lines.
When nil (the default), completed items are not logged to disk.")

(defvar agent-shell-queue-state-file-function #'agent-shell-queue--default-state-file
  "Function returning the path to the queue state file.")

(defvar agent-shell-queue-pick-buffer-function #'agent-shell-queue--default-pick-buffer
  "Function called with a PROMPT string to pick an agent-shell buffer.")

(defvar agent-shell-queue-archive-file nil
  "File path for the JSONL archive written by `agent-shell-queue-buffer-archive'.
Each line is a JSON object containing the item's prompt, target path,
instance name, run flag, dispatch/completion timestamps, and runtime.
When nil, archiving is not available.")

(defvar agent-shell-queue-paused nil
  "When non-nil, the queue will not submit new work to any session.")

(defvar agent-shell-queue--session-paused nil
  "List of buffer names of individually paused sessions.
Populated automatically when `agent-shell-interrupt' is called on a session
with queued items.  Cleared by `agent-shell-queue-session-resume' or
`agent-shell-queue-unpause-all-sessions'.")

(defvar agent-shell-queue--editing-ids nil
  "List of item IDs currently open in an edit buffer.
Items in this list are skipped by dispatch until the edit is saved or cancelled.")

(defvar agent-shell-queue--last-flush-time nil
  "Float-time of the most recent queue state write to disk.")

(defvar agent-shell-queue--next-flush-time nil
  "Time of the next scheduled auto-flush, or nil if none is pending.")

(defvar agent-shell-queue-auto-flush-interval 300
  "Seconds between automatic queue flushes.  Set to nil to disable.")

(defvar agent-shell-queue--stale-item-ids nil
  "List of item IDs that failed dispatch due to struct mismatch after code reload.
These are automatically deferred and their buffers paused.")

(defvar agent-shell-queue-before-reload-hook nil
  "Hook run just before code and state are reloaded.
Queue is paused and flushed to disk before this hook fires.")

(defvar agent-shell-queue-after-reload-hook nil
  "Hook run after code and state have been reloaded from disk.")

(defvar agent-shell-queue--wait-timers nil
  "Alist of (ITEM-ID . TIMER) for active wait-until items.
Timers are cancelled automatically when items are removed or the queue reloads.")

(defvar agent-shell-queue--compact-running nil
  "List of (BUF-NAME . ITEM-ID) pairs for compact items currently dispatched.
Used by `agent-shell-queue-mark-done' to clean up session-pause state.")

(defvar agent-shell-queue--remove-all-confirmed nil
  "When non-nil, skip per-item confirmation in remove commands.
Set to t when user answers \\='a\\=' (all) at a removal prompt.")

(defvar agent-shell-queue--response-start-positions nil
  "Alist of (ITEM-ID . BUFFER-POSITION) recording where in the shell buffer
each dispatched LLM prompt begins.  Used to capture the response text on
turn-complete and store it in the item's response field.")

(defvar agent-shell-queue-save-function nil
  "When non-nil, called instead of the default file-based save logic.
The function is called with no arguments and must persist the current
value of `agent-shell-queue--items' to a durable store.
Used by backends such as `agent-shell-queue-db' to bypass file I/O.")

(defvar agent-shell-queue-load-function nil
  "When non-nil, called instead of the default file-based load logic.
The function is called with no arguments and must populate
`agent-shell-queue--items' from a durable store.
Used by backends such as `agent-shell-queue-db' to bypass file I/O.")

(defvar agent-shell-queue-safe-save nil
  "When non-nil, write a versioned backup before each queue state save.
Backups are written to `agent-shell-queue-safe-save-directory' using the
format selected by `agent-shell-queue-safe-save-format'.
Has no effect when `agent-shell-queue-save-function' is set.")

(defvar agent-shell-queue-safe-save-directory nil
  "Directory for versioned queue backups written when `agent-shell-queue-safe-save' is non-nil.
Nil means use a subdirectory of `temporary-file-directory' named
\"emacs-<instance>\" where <instance> comes from `agent-shell-queue-instance-name'.")

(defvar agent-shell-queue-safe-save-format nil
  "Serialization format for safe-save backups, or nil to use `agent-shell-queue-serialization-format'.")

(defun agent-shell-queue-pause ()
  "Pause the global queue; no new items will be dispatched."
  (interactive)
  (setq agent-shell-queue-paused t)
  (message "agent-shell-queue: PAUSED — no new items will be dispatched")
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-resume ()
  "Resume the global queue after being paused."
  (interactive)
  (setq agent-shell-queue-paused nil)
  (message "agent-shell-queue: running")
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-unpause-all-sessions ()
  "Clear the per-session pause list, resuming all individually paused sessions."
  (interactive)
  (setq agent-shell-queue--session-paused nil)
  (message "agent-shell-queue: all session pauses cleared")
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-session-pause (&optional buf)
  "Pause dispatch for BUF (default: current agent-shell session)."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Pause dispatch for: "))))
  (when buf
    (let ((name (buffer-name buf)))
      (cl-pushnew name agent-shell-queue--session-paused :test #'equal)
      (message "agent-shell-queue: %s PAUSED" name)
      (agent-shell-queue--refresh-buffer))))

(defun agent-shell-queue-session-resume (&optional buf)
  "Resume dispatch for BUF (default: current agent-shell session).
Any running `pause' or `compact' item for BUF is marked done automatically."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Resume dispatch for: "))))
  (when buf
    (let ((name (buffer-name buf)))
      (setq agent-shell-queue--session-paused
            (delete name agent-shell-queue--session-paused))
      (setq agent-shell-queue--compact-running
            (cl-remove name agent-shell-queue--compact-running :key #'car :test #'equal))
      (dolist (item (cdr (assoc name agent-shell-queue--items)))
        (when (and (eq (agent-shell-queue-item-status item) 'running)
                   (memq (agent-shell-queue-item-kind item) '(pause compact)))
          (setf (agent-shell-queue-item-status item) 'done)
          (setf (agent-shell-queue-item-completed item) (float-time))
          (agent-shell-queue--append-done-log name item)))
      (agent-shell-queue--save)
      (message "agent-shell-queue: %s resumed" name)
      (agent-shell-queue--refresh-buffer)
      (agent-shell-queue--send-next-for-buffer buf))))

(defun agent-shell-queue--on-interrupt (&optional _force)
  "Pause the per-buffer queue when `agent-shell-interrupt' is called.
Installed as :before advice on `agent-shell-interrupt'."
  (when (and (derived-mode-p 'agent-shell-mode)
             (assoc (buffer-name) agent-shell-queue--items))
    (cl-pushnew (buffer-name) agent-shell-queue--session-paused :test #'equal)
    (agent-shell-queue--refresh-buffer)))

(advice-add 'agent-shell-interrupt :before #'agent-shell-queue--on-interrupt)

(defvar-local agent-shell-queue-intercept-mode nil
  "When non-nil in an agent-shell buffer, capture user-typed turns as queue items.")

(defun agent-shell-queue--on-submit-intercept (&rest _)
  "Capture user-typed shell turn as a queue item when intercept mode is active.
Installed as :before advice on `shell-maker-submit'."
  (when (and agent-shell-queue-intercept-mode
             (called-interactively-p 'interactive)
             (derived-mode-p 'agent-shell-mode))
    (let* ((buf (current-buffer))
           (buf-name (buffer-name buf))
           (input (save-excursion
                    (goto-char (point-max))
                    (when (re-search-backward comint-prompt-regexp nil t)
                      (string-trim
                       (buffer-substring-no-properties (match-end 0) (point-max)))))))
      (when (and input (not (string-empty-p input)))
        (agent-shell-queue--ensure-loaded)
        (let ((item (agent-shell-queue--make-item input nil 'prompt)))
          (setf (agent-shell-queue-item-status item) 'running)
          (setf (agent-shell-queue-item-dispatched item) (float-time))
          (agent-shell-queue--add-item-to-bucket buf-name item)
          (push (cons (agent-shell-queue-item-id item) (point-max))
                agent-shell-queue--response-start-positions)
          (agent-shell-queue--save)
          (agent-shell-queue--refresh-buffer))))))

(advice-add 'shell-maker-submit :before #'agent-shell-queue--on-submit-intercept)

(defun agent-shell-queue-toggle-intercept-mode (&optional buf)
  "Toggle shell-intercept mode for BUF; when active, user-typed turns are queued."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Toggle intercept for: "))))
  (when buf
    (with-current-buffer buf
      (setq agent-shell-queue-intercept-mode (not agent-shell-queue-intercept-mode))
      (message "agent-shell-queue intercept: %s in %s"
               (if agent-shell-queue-intercept-mode "ENABLED" "disabled")
               (buffer-name buf))
      (agent-shell-queue--refresh-buffer))))

(defun agent-shell-queue--default-state-file ()
  "Default queue state file path under `user-emacs-directory'."
  (let ((ext (pcase agent-shell-queue-serialization-format
               ('json "json")
               ('yaml "yaml")
               (_ "el"))))
    (expand-file-name (concat "agent-shell-queue." ext) user-emacs-directory)))

(defun agent-shell-queue--default-pick-buffer (prompt)
  "Pick a live agent-shell buffer using PROMPT via `completing-read'."
  (when-let ((bufs (agent-shell-buffers)))
    (get-buffer (completing-read prompt (mapcar #'buffer-name bufs) nil t))))

;;; Struct macro

(defmacro agent-shell-queue--defstruct (type-name &rest fields)
  "Define a cl-defstruct TYPE-NAME with FIELDS and generate plist serializers.
FIELDS are plain symbols.  Generates constructor TYPE-NAME--make plus:
  TYPE-NAME-to-plist   — struct → keyword-keyed plist
  TYPE-NAME-from-plist — keyword-keyed plist → struct"
  (declare (indent 1))
  (let* ((sname   (symbol-name type-name))
         (ctor    (intern (concat sname "--make")))
         (to-fn   (intern (concat sname "-to-plist")))
         (from-fn (intern (concat sname "-from-plist"))))
    `(progn
       (cl-defstruct (,type-name (:constructor ,ctor) (:copier nil))
         ,@fields)
       (defun ,to-fn (item)
         ,(format "Convert %s ITEM to a keyword-keyed plist." sname)
         (list ,@(cl-mapcan
                  (lambda (f)
                    (list (intern (concat ":" (symbol-name f)))
                          `(,(intern (concat sname "-" (symbol-name f))) item)))
                  fields)))
       (defun ,from-fn (plist)
         ,(format "Reconstruct a %s from keyword-keyed PLIST." sname)
         (,ctor ,@(cl-mapcan
                   (lambda (f)
                     (let ((kw (intern (concat ":" (symbol-name f)))))
                       (list kw `(plist-get plist ,kw))))
                   fields))))))

;;; Data model

(agent-shell-queue--defstruct agent-shell-queue-item
  id prompt status kind background created dispatched completed response)

(defvar agent-shell-queue--items nil
  "Alist of (BUFFER-NAME . ITEM-LIST) for all queued prompts.")

(defvar agent-shell-queue--loaded nil
  "Non-nil after the on-disk state has been read into memory.")

(defvar agent-shell-queue--idle-timer nil
  "Idle timer for auto-sending active queue items.")

(defvar agent-shell-queue--subscriptions nil
  "Alist of (BUF-NAME . TOKEN) for active `turn-complete' subscriptions.
Each entry is registered when the first item is queued for that buffer and
removed when the queue for that buffer empties or the buffer is killed.")

(defun agent-shell-queue--gen-id ()
  "Generate a short unique item ID: q + one digit + four alphanumeric chars."
  (let ((chars "abcdefghijklmnopqrstuvwxyz0123456789"))
    (concat "q"
            (number-to-string (random 10))
            (apply #'string
                   (mapcar (lambda (_) (aref chars (random 36)))
                           (make-list 4 nil))))))

(defun agent-shell-queue--make-item (prompt &optional background kind)
  "Return a new active queue item for PROMPT."
  (agent-shell-queue-item--make
   :id (agent-shell-queue--gen-id)
   :prompt prompt
   :status 'active
   :kind (or kind 'prompt)
   :background background
   :created (float-time)))


;;; Local utilities

(defun agent-shell-queue--state-file ()
  "Return the path to the on-disk queue state file."
  (funcall agent-shell-queue-state-file-function))

(defun agent-shell-queue--pick-buffer (prompt)
  "Pick a live agent-shell buffer using PROMPT."
  (funcall agent-shell-queue-pick-buffer-function prompt))

(defun agent-shell-queue--format-age (delta)
  "Format DELTA time-value as a short relative age string."
  (let ((s (float-time delta)))
    (cond ((< s 60)    (format "%ds" (truncate s)))
          ((< s 3600)  (format "%dm" (truncate (/ s 60))))
          ((< s 86400) (format "%dh" (truncate (/ s 3600))))
          (t           (format "%dd" (truncate (/ s 86400)))))))

;;; Persistence

;; -- plist format --

(defun agent-shell-queue--serialize-plist ()
  "Serialize queue items to an s-expression string (plist item format)."
  (with-temp-buffer
    (prin1 (mapcar (lambda (pair)
                     (cons (car pair)
                           (mapcar #'agent-shell-queue-item-to-plist (cdr pair))))
                   agent-shell-queue--items)
           (current-buffer))
    (buffer-string)))

(defun agent-shell-queue--deserialize-plist (str)
  "Deserialize STR (plist format) into an items alist."
  (let ((data (read str)))
    (unless (listp data)
      (error "Expected list, got %S" data))
    (mapcar (lambda (pair)
              (cons (car pair)
                    (mapcar #'agent-shell-queue-item-from-plist (cdr pair))))
            data)))

;; -- JSON format --

(defun agent-shell-queue--item-to-json (item)
  "Convert ITEM to a JSON-serializable plist.
Status is stored as a string; background as a JSON boolean."
  (list :id (agent-shell-queue-item-id item)
        :prompt (agent-shell-queue-item-prompt item)
        :status (symbol-name (agent-shell-queue-item-status item))
        :kind (symbol-name (or (agent-shell-queue-item-kind item) 'prompt))
        :background (if (agent-shell-queue-item-background item) t :false)
        :created (agent-shell-queue-item-created item)))

(defun agent-shell-queue--item-from-json (obj)
  "Reconstruct a queue item from JSON-parsed plist OBJ.
Status is interned; background truthy only when exactly `t'."
  (agent-shell-queue-item--make
   :id (plist-get obj :id)
   :prompt (plist-get obj :prompt)
   :status (intern (plist-get obj :status))
   :kind (intern (or (plist-get obj :kind) "prompt"))
   :background (eq t (plist-get obj :background))
   :created (plist-get obj :created)))

(defun agent-shell-queue--serialize-json ()
  "Serialize queue items to a JSON string."
  (unless (fboundp 'json-serialize)
    (error "json-serialize not available (requires Emacs 27+)"))
  (json-serialize
   (vconcat
    (mapcar (lambda (pair)
              (list :buffer (car pair)
                    :items  (vconcat (mapcar #'agent-shell-queue--item-to-json
                                            (cdr pair)))))
            agent-shell-queue--items))))

(defun agent-shell-queue--deserialize-json (str)
  "Deserialize STR (JSON format) into an items alist."
  (unless (fboundp 'json-parse-string)
    (error "json-parse-string not available (requires Emacs 27+)"))
  (let ((buckets (json-parse-string str
                                    :object-type 'plist
                                    :array-type  'list
                                    :null-object  nil
                                    :false-object nil)))
    (mapcar (lambda (bucket)
              (cons (plist-get bucket :buffer)
                    (mapcar #'agent-shell-queue--item-from-json
                            (plist-get bucket :items))))
            buckets)))

;; -- YAML format --

(defun agent-shell-queue--item-to-yaml (item)
  "Convert ITEM to a hash-table suitable for `yaml-encode'.
Status is stored as a string; background as t or nil."
  (let ((h (make-hash-table :test 'equal)))
    (puthash "id" (agent-shell-queue-item-id item) h)
    (puthash "prompt" (agent-shell-queue-item-prompt item) h)
    (puthash "status" (symbol-name (agent-shell-queue-item-status item)) h)
    (puthash "kind" (symbol-name (or (agent-shell-queue-item-kind item) 'prompt)) h)
    (puthash "background" (if (agent-shell-queue-item-background item) t nil) h)
    (puthash "created" (agent-shell-queue-item-created item) h)
    h))

(defun agent-shell-queue--item-from-yaml (obj)
  "Reconstruct a queue item from a hash-table OBJ produced by `yaml-parse-string'."
  (agent-shell-queue-item--make
   :id (gethash "id" obj)
   :prompt (gethash "prompt" obj)
   :status (intern (gethash "status" obj))
   :kind (intern (or (gethash "kind" obj) "prompt"))
   :background (eq t (gethash "background" obj))
   :created (gethash "created" obj)))

(defun agent-shell-queue--serialize-yaml ()
  "Serialize queue items to a YAML string via `yaml-encode'."
  (unless (fboundp 'yaml-encode)
    (error "yaml-encode not available; install the `yaml' package"))
  (yaml-encode
   (vconcat
    (mapcar (lambda (pair)
              (let ((h (make-hash-table :test 'equal)))
                (puthash "buffer" (car pair) h)
                (puthash "items"  (vconcat (mapcar #'agent-shell-queue--item-to-yaml
                                                   (cdr pair)))
                         h)
                h))
            agent-shell-queue--items))))

(defun agent-shell-queue--deserialize-yaml (str)
  "Deserialize STR (YAML format) into an items alist via `yaml-parse-string'."
  (unless (fboundp 'yaml-parse-string)
    (error "yaml-parse-string not available; install the `yaml' package"))
  (let ((buckets (yaml-parse-string str
                                    :object-type  'hash-table
                                    :sequence-type 'list
                                    :null-object   nil
                                    :false-object  nil)))
    (mapcar (lambda (bucket)
              (cons (gethash "buffer" bucket)
                    (mapcar #'agent-shell-queue--item-from-yaml
                            (gethash "items" bucket))))
            buckets)))

;; -- Dispatch --

(defvar agent-shell-queue--format-dispatch
  '((plist agent-shell-queue--serialize-plist agent-shell-queue--deserialize-plist)
    (json agent-shell-queue--serialize-json agent-shell-queue--deserialize-json)
    (yaml agent-shell-queue--serialize-yaml agent-shell-queue--deserialize-yaml))
  "Alist of (FORMAT SERIALIZE-FN DESERIALIZE-FN) for queue persistence.")

(defun agent-shell-queue-register-format (fmt serialize-fn deserialize-fn)
  "Register a serialization FORMAT with SERIALIZE-FN and DESERIALIZE-FN.
FMT is a symbol; SERIALIZE-FN takes no args and returns a string;
DESERIALIZE-FN takes a string and returns an items alist."
  (let ((existing (assq fmt agent-shell-queue--format-dispatch)))
    (if existing
        (setcdr existing (list serialize-fn deserialize-fn))
      (push (list fmt serialize-fn deserialize-fn) agent-shell-queue--format-dispatch))))

(defun agent-shell-queue--serialize ()
  "Serialize queue items using `agent-shell-queue-serialization-format'."
  (let ((entry (assq agent-shell-queue-serialization-format agent-shell-queue--format-dispatch)))
    (unless entry
      (error "Unknown agent-shell-queue-serialization-format: %S"
             agent-shell-queue-serialization-format))
    (funcall (cadr entry))))

(defun agent-shell-queue--deserialize (str)
  "Deserialize STR using `agent-shell-queue-serialization-format'."
  (let ((entry (assq agent-shell-queue-serialization-format agent-shell-queue--format-dispatch)))
    (unless entry
      (error "Unknown agent-shell-queue-serialization-format: %S"
             agent-shell-queue-serialization-format))
    (funcall (caddr entry) str)))

(defun agent-shell-queue--safe-save-directory ()
  "Return the directory for safe-save backups.
Uses `agent-shell-queue-safe-save-directory' when set, otherwise
a subdirectory of `temporary-file-directory' named emacs-<instance>."
  (or agent-shell-queue-safe-save-directory
      (expand-file-name
       (format "emacs-%s"
               (let ((v agent-shell-queue-instance-name))
                 (if (functionp v) (funcall v) v)))
       (temporary-file-directory))))

(defun agent-shell-queue--safe-save-extension (fmt)
  "Return the file extension string for serialization format FMT."
  (pcase fmt
    ('json ".json")
    ('yaml ".yaml")
    (_ ".el")))


(defun agent-shell-queue--save ()
  "Persist `agent-shell-queue--items', excluding done and running items.
Delegates to `agent-shell-queue-save-function' when set; otherwise writes
to the file returned by `agent-shell-queue--state-file'.
When `agent-shell-queue-safe-save' is non-nil and no custom save function
is set, writes a versioned backup before overwriting the state file."
  (if agent-shell-queue-save-function
      (funcall agent-shell-queue-save-function)
    (let* ((file (agent-shell-queue--state-file))
           (agent-shell-queue--items
            (cl-remove-if
             (lambda (pair) (null (cdr pair)))
             (mapcar (lambda (pair)
                       (cons (car pair)
                             (cl-remove-if
                              (lambda (item)
                                (memq (agent-shell-queue-item-status item) '(done running)))
                              (cdr pair))))
                     agent-shell-queue--items)))
           (serialized (agent-shell-queue--serialize)))
      (when (and agent-shell-queue-safe-save (file-exists-p file))
        (let* ((fmt (or agent-shell-queue-safe-save-format
                        agent-shell-queue-serialization-format))
               (ext (agent-shell-queue--safe-save-extension fmt))
               (dir (agent-shell-queue--safe-save-directory))
               (backup (expand-file-name
                        (format "agent-shell-queue-archive-%d%s"
                                (truncate (float-time))
                                ext)
                        dir)))
          (make-directory dir t)
          (with-temp-file backup
            (insert serialized))))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert serialized))))
  (setq agent-shell-queue--last-flush-time (float-time))
  (when agent-shell-queue-auto-flush-interval
    (setq agent-shell-queue--next-flush-time
          (time-add (current-time)
                    (seconds-to-time agent-shell-queue-auto-flush-interval)))))

(defun agent-shell-queue--append-done-log (buf-name item)
  "Append a JSON line for the completed ITEM in BUF-NAME to the done log.
No-op when `agent-shell-queue-done-log-file' is nil."
  (when agent-shell-queue-done-log-file
    (condition-case err
        (let ((entry (json-serialize
                      (list :id (agent-shell-queue-item-id item)
                            :target buf-name
                            :prompt (agent-shell-queue-item-prompt item)
                            :background (if (agent-shell-queue-item-background item) t :false)
                            :created (agent-shell-queue-item-created item)
                            :completed (float-time)))))
          (make-directory (file-name-directory agent-shell-queue-done-log-file) t)
          (write-region (concat entry "\n") nil agent-shell-queue-done-log-file t 'silent))
      (error (message "agent-shell-queue: done-log write failed: %s" err)))))

(defun agent-shell-queue--write-archive (buf-name item)
  "Append a JSONL record for ITEM in BUF-NAME to `agent-shell-queue-archive-file'.
The record includes the target buffer's directory, instance name, whether the
item was dispatched, ISO-8601 archive timestamp, and runtime (dispatched→completed)."
  (when agent-shell-queue-archive-file
    (condition-case err
        (let* ((file agent-shell-queue-archive-file)
               (buf (get-buffer buf-name))
               (path (when (buffer-live-p buf)
                       (buffer-local-value 'default-directory buf)))
               (dispatched (agent-shell-queue-item-dispatched item))
               (completed (agent-shell-queue-item-completed item))
               (ran (not (null dispatched)))
               (runtime (when (and dispatched completed) (- completed dispatched)))
               (instance (let ((n agent-shell-queue-instance-name))
                           (if (functionp n) (funcall n) n)))
               (entry (json-serialize
                       (list :id (agent-shell-queue-item-id item)
                             :prompt (agent-shell-queue-item-prompt item)
                             :status (symbol-name (agent-shell-queue-item-status item))
                             :background (if (agent-shell-queue-item-background item) t :false)
                             :target buf-name
                             :path (or path :null)
                             :instance (or instance :null)
                             :ran (if ran t :false)
                             :archived (format-time-string "%Y-%m-%dT%H:%M:%S")
                             :created (or (agent-shell-queue-item-created item) :null)
                             :dispatched (or dispatched :null)
                             :completed (or completed :null)
                             :runtime (or runtime :null)))))
          (make-directory (file-name-directory file) t)
          ;; Advisory lock via Emacs's own .#file mechanism — no subprocess,
          ;; no persistent fd.  write-region with append opens, writes, closes.
          (unwind-protect
              (progn
                (lock-file file)
                (write-region (concat entry "\n") nil file t 'silent))
            (unlock-file file)))
      (error (message "agent-shell-queue: archive write failed: %s" err)))))

;;;###autoload
(defun agent-shell-queue-buffer-archive ()
  "Archive the item at point to `agent-shell-queue-archive-file' and remove it.
Errors if `agent-shell-queue-archive-file' is not set."
  (interactive)
  (unless agent-shell-queue-archive-file
    (user-error "Set `agent-shell-queue-archive-file' before archiving"))
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (agent-shell-queue--write-archive (car pair) item)
    (agent-shell-queue-remove id)
    (agent-shell-queue-buffer-refresh)
    (message "agent-shell-queue: archived %s" id)))

(defun agent-shell-queue--load ()
  "Populate `agent-shell-queue--items' from the durable store.
Delegates to `agent-shell-queue-load-function' when set; otherwise reads
from the file returned by `agent-shell-queue--state-file'."
  (if agent-shell-queue-load-function
      (condition-case err
          (funcall agent-shell-queue-load-function)
        (error (message "agent-shell-queue: load failed: %s" err)))
    (let ((file (agent-shell-queue--state-file)))
      (when (file-exists-p file)
        (condition-case err
            (let* ((str   (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string)))
                   (items (agent-shell-queue--deserialize str)))
              (setq agent-shell-queue--items items))
          (error (message "agent-shell-queue: ignoring unreadable state: %s" err)))))))

(defun agent-shell-queue--ensure-loaded ()
  "Load queue state from disk on first call."
  (unless agent-shell-queue--loaded
    (agent-shell-queue--load)
    (setq agent-shell-queue--loaded t)))

(add-hook 'kill-emacs-hook #'agent-shell-queue--save)

;;; Queue operations

(defun agent-shell-queue--item-by-id (id)
  "Return (BUF-NAME . ITEM) for the item with ID, or nil."
  (catch 'found
    (dolist (pair agent-shell-queue--items)
      (dolist (item (cdr pair))
        (when (equal (agent-shell-queue-item-id item) id)
          (throw 'found (cons (car pair) item)))))))

(defun agent-shell-queue--add-item-to-bucket (bucket-name item)
  "Append ITEM to the BUCKET-NAME bucket in `agent-shell-queue--items'."
  (let ((pair (assoc bucket-name agent-shell-queue--items)))
    (if pair
        (setcdr pair (append (cdr pair) (list item)))
      (setq agent-shell-queue--items
            (append agent-shell-queue--items (list (list bucket-name item)))))))

(defun agent-shell-queue-add (prompt buf &optional background)
  "Add a new active item for PROMPT destined for BUF.  Save and refresh.
When BACKGROUND is non-nil the item is flagged for sub-agent execution.
Registers a `turn-complete' subscription on BUF if one is not already active."
  (agent-shell-queue--ensure-loaded)
  (let* ((buf-name (buffer-name buf))
         (item (agent-shell-queue--make-item prompt background)))
    (agent-shell-queue--add-item-to-bucket buf-name item)
    (agent-shell-queue--ensure-subscription buf)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    item))

;;;###autoload
(defun agent-shell-queue-enqueue-emacs (form buf)
  "Enqueue a Lisp FORM to run in Emacs before the next prompt dispatched to BUF.
FORM is read from the minibuffer using `read-expression-map' (same as
`eval-expression').  When dispatched, the form is evaluated via `eval';
errors are reported as messages and the item is still marked done so the
queue advances.  The item blocks the session queue like any other running item."
  (interactive
   (list (read-from-minibuffer "Emacs call: " nil read-expression-map nil
                               'read-expression-history)
         (agent-shell-queue--pick-buffer "Target session: ")))
  (agent-shell-queue--ensure-loaded)
  (let* ((buf-name (buffer-name buf))
         (item (agent-shell-queue--make-item form nil 'emacs)))
    (agent-shell-queue--add-item-to-bucket buf-name item)
    (agent-shell-queue--ensure-subscription buf)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    item))

(defun agent-shell-queue-add-unassigned (prompt &optional background)
  "Add a new item for PROMPT to the unassigned bucket.  Save and refresh.
Unassigned items display in blue and sort after all shell-assigned items."
  (agent-shell-queue--ensure-loaded)
  (let* ((item (agent-shell-queue--make-item prompt background)))
    (agent-shell-queue--add-item-to-bucket agent-shell-queue--unassigned-key item)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    item))

(defun agent-shell-queue-remove (id)
  "Remove the item with ID from the queue.  Save.
Drops the `turn-complete' subscription for any bucket that becomes empty.
Cancels any pending wait timer for the item.
Always logs the removed item's prompt to *Messages*."
  (when-let ((pair (assoc id agent-shell-queue--wait-timers)))
    (cancel-timer (cdr pair))
    (setq agent-shell-queue--wait-timers
          (cl-remove id agent-shell-queue--wait-timers :key #'car :test #'equal)))
  (let ((before-names (mapcar #'car agent-shell-queue--items)))
    (dolist (pair agent-shell-queue--items)
      (when-let ((item (cl-find id (cdr pair)
                                :key #'agent-shell-queue-item-id :test #'equal)))
        (message "agent-shell-queue: removed %s [%s]: %s"
                 id (car pair)
                 (truncate-string-to-width
                  (agent-shell-queue-item-prompt item) 120 nil nil "…")))
      (setcdr pair (cl-remove-if (lambda (item)
                                   (equal (agent-shell-queue-item-id item) id))
                                 (cdr pair))))
    (setq agent-shell-queue--items
          (cl-remove-if (lambda (pair) (null (cdr pair)))
                        agent-shell-queue--items))
    (let ((after-names (mapcar #'car agent-shell-queue--items)))
      (dolist (name before-names)
        (unless (member name after-names)
          (agent-shell-queue--drop-subscription name))))
    (agent-shell-queue--save)))

(defun agent-shell-queue--confirm-remove (item)
  "Prompt the user to confirm removing ITEM.
Returns t to proceed, nil to skip.  When user answers \\='a\\=', sets
`agent-shell-queue--remove-all-confirmed' so future calls return t immediately."
  (or agent-shell-queue--remove-all-confirmed
      (let* ((display (truncate-string-to-width
                       (agent-shell-queue-item-prompt item) 60 nil nil "…"))
             (answer (read-char-choice
                      (format "Remove [%s]? (y)es (n)o (a)ll: " display)
                      '(?y ?n ?a ?Y ?N ?A))))
        (pcase answer
          ((or ?y ?Y) t)
          ((or ?a ?A) (setq agent-shell-queue--remove-all-confirmed t) t)
          (_ nil)))))

(defun agent-shell-queue-defer (id)
  "Toggle status of item ID between `active' and `deferred'.  Save."
  (when-let ((pair (agent-shell-queue--item-by-id id)))
    (let ((item (cdr pair)))
      (setf (agent-shell-queue-item-status item)
            (if (eq (agent-shell-queue-item-status item) 'active) 'deferred 'active))
      (agent-shell-queue--save))))

(defun agent-shell-queue-edit (id new-prompt)
  "Replace the prompt of item ID with NEW-PROMPT.  Save."
  (when-let ((pair (agent-shell-queue--item-by-id id)))
    (setf (agent-shell-queue-item-prompt (cdr pair)) new-prompt)
    (agent-shell-queue--save)))

(defun agent-shell-queue-set-background-task (id flag)
  "Set the background flag of item ID to FLAG.  Save."
  (when-let ((pair (agent-shell-queue--item-by-id id)))
    (setf (agent-shell-queue-item-background (cdr pair)) flag)
    (agent-shell-queue--save)))

(defun agent-shell-queue--move (id delta)
  "Shift item ID by DELTA positions within its buffer's list."
  (when-let ((pair (agent-shell-queue--item-by-id id)))
    (let* ((buf-name (car pair))
           (cell (assoc buf-name agent-shell-queue--items))
           (items (cdr cell))
           (idx (cl-position id items :key #'agent-shell-queue-item-id :test #'equal))
           (new-idx (and idx (+ idx delta))))
      (when (and new-idx (>= new-idx 0) (< new-idx (length items)))
        (let ((new-items (copy-sequence items)))
          (cl-rotatef (nth idx new-items) (nth new-idx new-items))
          (setcdr cell new-items)
          (agent-shell-queue--save))))))

(defun agent-shell-queue-move-up (id)
  "Move item ID one position earlier in its buffer's queue."
  (agent-shell-queue--move id -1))

(defun agent-shell-queue-move-down (id)
  "Move item ID one position later in its buffer's queue."
  (agent-shell-queue--move id 1))

(defun agent-shell-queue--assign-item (id new-buf-name)
  "Move the item with ID to the NEW-BUF-NAME bucket.
NEW-BUF-NAME may be a live buffer name or `agent-shell-queue--unassigned-key'.
Drops the subscription on the old bucket if it empties; ensures one on the new."
  (when-let ((pair (agent-shell-queue--item-by-id id)))
    (let* ((old-name (car pair))
           (item (cdr pair)))
      (unless (equal old-name new-buf-name)
        (let ((old-cell (assoc old-name agent-shell-queue--items)))
          (setcdr old-cell
                  (cl-remove-if (lambda (i)
                                  (equal (agent-shell-queue-item-id i) id))
                                (cdr old-cell))))
        (setq agent-shell-queue--items
              (cl-remove-if (lambda (p) (null (cdr p)))
                            agent-shell-queue--items))
        (unless (assoc old-name agent-shell-queue--items)
          (agent-shell-queue--drop-subscription old-name))
        (let ((new-cell (assoc new-buf-name agent-shell-queue--items)))
          (if new-cell
              (setcdr new-cell (append (cdr new-cell) (list item)))
            (setq agent-shell-queue--items
                  (append agent-shell-queue--items (list (list new-buf-name item))))))
        (unless (equal new-buf-name agent-shell-queue--unassigned-key)
          (when-let ((new-buf (get-buffer new-buf-name)))
            (agent-shell-queue--ensure-subscription new-buf)))
        (agent-shell-queue--save)
        (agent-shell-queue--refresh-buffer)))))

(defun agent-shell-queue--redirect-dead-target (id buf-name)
  "When BUF-NAME is no longer live, offer to assign item ID to another shell.
Returns non-nil if the item was successfully assigned and sent."
  (let ((available (agent-shell-buffers)))
    (if (null available)
        (user-error "Buffer %s is gone and no agent-shell buffers are live" buf-name)
      (when (yes-or-no-p (format "Buffer %s is gone. Assign to another shell? " buf-name))
        (let* ((names (mapcar #'buffer-name available))
               (new-name (completing-read (format "Assign '%s' to: " buf-name) names nil t)))
          (when (and new-name (not (string-empty-p new-name)))
            (agent-shell-queue--assign-item id new-name)
            (agent-shell-queue-send-item id)
            t))))))

(defun agent-shell-queue--handle-stale-item (id buf-name err)
  "Pause BUF-NAME and defer item ID after a struct access error ERR.
Called when dispatching item ID raises an error, which indicates the item
was built against an older struct definition before a code reload."
  (cl-pushnew id agent-shell-queue--stale-item-ids :test #'equal)
  (when-let ((pair (agent-shell-queue--item-by-id id)))
    (condition-case nil
        (setf (agent-shell-queue-item-status (cdr pair)) 'deferred)
      (error nil)))
  (cl-pushnew buf-name agent-shell-queue--session-paused :test #'equal)
  (message "agent-shell-queue: item %s in %s appears stale after code reload; deferred and queue paused (%s)"
           id buf-name err)
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-send-item (id)
  "Send the item with ID to its target buffer, marking it as running.
Items flagged as background are wrapped with `agent-shell-queue-background-prefix'.
The item transitions to done when the buffer's turn-complete event fires.
Running and done items are not persisted across sessions."
  (cl-block agent-shell-queue-send-item
    (when-let ((pair (agent-shell-queue--item-by-id id)))
      (let* ((buf-name (car pair))
             (item (cdr pair))
             (buf (get-buffer buf-name)))
        (unless (buffer-live-p buf)
          (agent-shell-queue--redirect-dead-target id buf-name)
          (cl-return-from agent-shell-queue-send-item nil))
        (condition-case err
            (progn
              (setf (agent-shell-queue-item-status item) 'running)
              (setf (agent-shell-queue-item-dispatched item) (float-time))
              (agent-shell-queue--save)
              (agent-shell-queue--refresh-buffer)
              (pcase (agent-shell-queue-item-kind item)
                ('emacs
                 (condition-case eval-err
                     (eval (read (agent-shell-queue-item-prompt item)) t)
                   (error (message "agent-shell-queue: emacs item %s error: %s" id eval-err)))
                 (setf (agent-shell-queue-item-completed item) (float-time))
                 (setf (agent-shell-queue-item-status item) 'done)
                 (agent-shell-queue--append-done-log buf-name item)
                 (agent-shell-queue--save)
                 (agent-shell-queue--refresh-buffer)
                 (agent-shell-queue--alert-if-empty)
                 (agent-shell-queue--send-next-for-buffer buf))
                ((or 'pause 'compact)
                 (cl-pushnew (cons buf-name id) agent-shell-queue--compact-running :test #'equal)
                 (cl-pushnew buf-name agent-shell-queue--session-paused :test #'equal)
                 (agent-shell-queue--save)
                 (agent-shell-queue--refresh-buffer)
                 (alert (if (eq (agent-shell-queue-item-kind item) 'pause)
                            (format "Queue for %s paused — human action required" buf-name)
                          (format "Manual work required: %s"
                                  (agent-shell-queue-item-prompt item)))
                        :title (format "Queue → %s" buf-name)
                        :category 'agent-shell-queue
                        :severity 'high
                        :persistent t))
                ('wait
                 (let* ((target (date-to-time (agent-shell-queue-item-prompt item)))
                        (delay (max 0 (float-time (time-subtract target (current-time)))))
                        (wait-timer
                         (run-with-timer
                          delay nil
                          (lambda ()
                            (setq agent-shell-queue--wait-timers
                                  (cl-remove id agent-shell-queue--wait-timers
                                             :key #'car :test #'equal))
                            (when-let* ((wpair (agent-shell-queue--item-by-id id))
                                        (witem (cdr wpair))
                                        (wbuf-name (car wpair)))
                              (setf (agent-shell-queue-item-completed witem) (float-time))
                              (setf (agent-shell-queue-item-status witem) 'done)
                              (agent-shell-queue--append-done-log wbuf-name witem)
                              (agent-shell-queue--save)
                              (agent-shell-queue--refresh-buffer)
                              (agent-shell-queue--alert-if-empty)
                              (when-let ((wbuf (get-buffer wbuf-name)))
                                (agent-shell-queue--send-next-for-buffer wbuf)))))))
                   (push (cons id wait-timer) agent-shell-queue--wait-timers)
                   (agent-shell-queue--save)
                   (agent-shell-queue--refresh-buffer)))
                (_
                 (progn
                  (alert (truncate-string-to-width (agent-shell-queue-item-prompt item) 80 nil nil "…")
                         :title (format "Queue → %s" buf-name)
                         :category 'agent-shell-queue
                         :severity 'low)
                  (push (cons id (with-current-buffer buf (point-max)))
                        agent-shell-queue--response-start-positions)
                  (let ((prompt (if (agent-shell-queue-item-background item)
                                    (concat agent-shell-queue-background-prefix
                                            (agent-shell-queue-item-prompt item))
                                  (agent-shell-queue-item-prompt item))))
                    (agent-shell-insert :text prompt :submit t :no-focus t :shell-buffer buf))))))
          (error
           (agent-shell-queue--handle-stale-item id buf-name err)))))))

(defun agent-shell-queue--capture-response (id buf-name)
  "Capture the final summary text for item ID from BUF-NAME and clean up tracking.
Captures only the text after the last tool-execution block (identified by the
`agent-shell-ui-state' text property).  Falls back to text after the recorded
dispatch position when no tool blocks are present."
  (let ((pos-pair (assoc id agent-shell-queue--response-start-positions)))
    (setq agent-shell-queue--response-start-positions
          (cl-remove id agent-shell-queue--response-start-positions
                     :key #'car :test #'equal))
    (when-let* (pos-pair
                (start-pos (cdr pos-pair))
                (sbuf (get-buffer buf-name))
                (pair (agent-shell-queue--item-by-id id)))
      (let ((text (with-current-buffer sbuf
                    (save-excursion
                      (goto-char (point-max))
                      (let* ((last-block (text-property-search-backward
                                          'agent-shell-ui-state))
                             (capture-from (if last-block
                                               (prop-match-end last-block)
                                             start-pos)))
                        (when (< capture-from (point-max))
                          (string-trim
                           (buffer-substring-no-properties
                            capture-from (point-max)))))))))
        (when (and text (not (string-empty-p text)))
          (setf (agent-shell-queue-item-response (cdr pair))
                (if (> (length text) 8192)
                    (concat (substring text 0 8192) "\n\n…[truncated]")
                  text)))))))

(defun agent-shell-queue--mark-running-done (buf-name)
  "Mark any running items for BUF-NAME as done, recording completion time.
Only fires the empty-queue alert when at least one item was actually marked done."
  (let ((marked nil))
    (dolist (item (cdr (assoc buf-name agent-shell-queue--items)))
      (when (eq (agent-shell-queue-item-status item) 'running)
        (unless (memq (agent-shell-queue-item-kind item) '(pause compact context))
          (agent-shell-queue--capture-response
           (agent-shell-queue-item-id item) buf-name))
        (setf (agent-shell-queue-item-completed item) (float-time))
        (setf (agent-shell-queue-item-status item) 'done)
        (agent-shell-queue--append-done-log buf-name item)
        (setq marked t)))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (when marked
      (agent-shell-queue--alert-if-empty))))

;;; Auto-send — per-buffer turn-complete subscriptions (primary) + idle timer (backup)

(defun agent-shell-queue--alert-if-empty ()
  "Send a persistent alert when no active or running items remain in any queue."
  (let ((has-work (cl-some (lambda (pair)
                             (cl-some (lambda (item)
                                        (memq (agent-shell-queue-item-status item)
                                              '(active running)))
                                      (cdr pair)))
                           agent-shell-queue--items)))
    (unless has-work
      (alert "All queued tasks complete"
             :title "Agent Queue"
             :category 'agent-shell-queue
             :severity 'normal
             :persistent t))))

(defun agent-shell-queue--next-dispatchable-item (items)
  "Return the first item in ITEMS eligible for dispatch, or nil."
  (cl-find-if (lambda (i)
                (and (eq (agent-shell-queue-item-status i) 'active)
                     (not (member (agent-shell-queue-item-id i)
                                  agent-shell-queue--editing-ids))))
              items))

(defun agent-shell-queue--send-next-for-buffer (buf)
  "Attempt to send the first active queue item for BUF.
Deferred via a zero-delay timer to let the current event complete before
submitting the next prompt.  Deferred items are skipped.
No-op when `agent-shell-queue-paused' is non-nil."
  (run-with-timer
   0 nil
   (lambda ()
     (when (and (buffer-live-p buf)
                (not agent-shell-queue-paused)
                (not (member (buffer-name buf) agent-shell-queue--session-paused)))
       (with-current-buffer buf
         (unless (shell-maker-busy)
           (let* ((buf-name (buffer-name))
                  (items (cdr (assoc buf-name agent-shell-queue--items)))
                  (item (agent-shell-queue--next-dispatchable-item items)))
             (when item
               (agent-shell-queue-send-item
                (agent-shell-queue-item-id item))))))))))

(defun agent-shell-queue--drop-subscription (buf-name)
  "Unsubscribe from `turn-complete' events for BUF-NAME and remove from registry.
Safe to call with a dead buffer — the subscription token is merely discarded."
  (when-let ((pair (assoc buf-name agent-shell-queue--subscriptions)))
    (let ((token (cdr pair))
          (buf (get-buffer buf-name)))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf (derived-mode-p 'agent-shell-mode)))
        (ignore-errors
          (with-current-buffer buf
            (agent-shell-unsubscribe :subscription token)))))
    (setq agent-shell-queue--subscriptions
          (cl-remove buf-name agent-shell-queue--subscriptions
                     :key #'car :test #'equal))))

(defun agent-shell-queue--ensure-subscription (buf)
  "Subscribe to `turn-complete' events on BUF if no subscription exists yet.
Also subscribes to `clean-up' so the registry is updated when BUF is killed."
  (let ((buf-name (buffer-name buf)))
    (unless (assoc buf-name agent-shell-queue--subscriptions)
      (let ((token (agent-shell-subscribe-to
                    :shell-buffer buf
                    :event 'turn-complete
                    :on-event (lambda (_event)
                                (agent-shell-queue--mark-running-done buf-name)
                                (agent-shell-queue--send-next-for-buffer buf)))))
        (push (cons buf-name token) agent-shell-queue--subscriptions))
      (agent-shell-subscribe-to
       :shell-buffer buf
       :event 'clean-up
       :on-event (lambda (_event)
                   (setq agent-shell-queue--subscriptions
                         (cl-remove buf-name agent-shell-queue--subscriptions
                                    :key #'car :test #'equal)))))))

(defun agent-shell-queue--auto-send ()
  "Backup scan: send the first active item for each idle agent-shell bucket.
Runs infrequently; deferred items are always skipped.
Primary draining is handled by per-buffer `turn-complete' subscriptions.
No-op when `agent-shell-queue-paused' is non-nil."
  (when (and agent-shell-queue--loaded agent-shell-queue--items
             (not agent-shell-queue-paused))
    (dolist (pair (copy-sequence agent-shell-queue--items))
      (let* ((buf-name (car pair))
             (buf (get-buffer buf-name)))
        (when (and (buffer-live-p buf)
                   (not (member buf-name agent-shell-queue--session-paused))
                   (not (with-current-buffer buf (shell-maker-busy))))
          (let ((items (cdr (assoc buf-name agent-shell-queue--items))))
            (when-let ((item (agent-shell-queue--next-dispatchable-item items)))
              (agent-shell-queue-send-item
               (agent-shell-queue-item-id item)))))))))

(defun agent-shell-queue--setup-hooks ()
  "Start the backup idle-scan timer.
Per-buffer draining is registered lazily via `agent-shell-queue--ensure-subscription'
when items are first added for a given buffer."
       (unless agent-shell-queue--idle-timer
	 (setq agent-shell-queue--idle-timer
               (run-with-idle-timer agent-shell-queue-idle-delay t
				    #'agent-shell-queue--auto-send))))

;;; Queue buffer

;;;###autoload
(defun agent-shell-queue-flush ()
  "Force-save queue state to disk immediately."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (agent-shell-queue--save)
  (message "agent-shell-queue: state saved to disk"))

;;;###autoload
(defun agent-shell-queue-reload ()
  "Pause, flush, reload source code, and reload state from disk.
Stops the idle timer, drops all turn-complete subscriptions, reloads
`agent-shell-queue.el' from source, re-reads queue state from disk, and
reinstates subscriptions for buffers with active/running items.
Queue remains paused after reload; call `agent-shell-queue-resume' when ready."
  (interactive)
  (setq agent-shell-queue-paused t)
  (agent-shell-queue--save)
  (when agent-shell-queue--idle-timer
    (cancel-timer agent-shell-queue--idle-timer)
    (setq agent-shell-queue--idle-timer nil))
  (dolist (pair agent-shell-queue--wait-timers)
    (cancel-timer (cdr pair)))
  (setq agent-shell-queue--wait-timers nil)
  (dolist (pair (copy-sequence agent-shell-queue--subscriptions))
    (agent-shell-queue--drop-subscription (car pair)))
  (run-hooks 'agent-shell-queue-before-reload-hook)
  (setq agent-shell-queue--items nil
        agent-shell-queue--loaded nil
        agent-shell-queue--subscriptions nil)
  (let* ((lib (locate-library "agent-shell-queue"))
         (src (if (and lib (string-suffix-p ".elc" lib))
                  (concat (file-name-sans-extension lib) ".el")
                lib)))
    (unless (and src (file-exists-p src))
      (error "agent-shell-queue-reload: cannot locate source file"))
    (load-file src))
  (agent-shell-queue--load)
  (setq agent-shell-queue--loaded t)
  (dolist (pair agent-shell-queue--items)
    (let* ((buf-name (car pair))
           (buf (get-buffer buf-name)))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf (derived-mode-p 'agent-shell-mode))
                 (cl-some (lambda (item)
                            (memq (agent-shell-queue-item-status item) '(active running)))
                          (cdr pair)))
        (agent-shell-queue--ensure-subscription buf))))
  (run-hooks 'agent-shell-queue-after-reload-hook)
  (agent-shell-queue--refresh-buffer)
  (when-let ((buf (get-buffer "*agent-shell-queue*")))
    (with-current-buffer buf
      (force-mode-line-update)))
  (message "agent-shell-queue: reloaded from disk — still PAUSED (M-x agent-shell-queue-resume to run)"))

;;;###autoload
(defun agent-shell-queue-clear-unparsable ()
  "Remove items whose struct fields cannot be read; print each to *Messages*.
Useful after a code reload that left in-memory structs with mismatched layouts.
When called interactively, prompts y/n/a for each candidate before removing it.
Affected buffer queues are paused and the queue state is saved."
  (interactive "P")
  (agent-shell-queue--ensure-loaded)
  (let (candidates)
    (dolist (pair agent-shell-queue--items)
      (dolist (item (cdr pair))
        (condition-case _
            (ignore (agent-shell-queue-item-id item)
                    (agent-shell-queue-item-prompt item)
                    (agent-shell-queue-item-status item))
          (error
           (push (cons (car pair) item) candidates)))))
    (if (null candidates)
        (message "agent-shell-queue: no unparsable items found")
      (let (removed (accept-all current-prefix-arg))
        (dolist (entry candidates)
          (let ((buf-name (car entry))
                (item (cdr entry)))
            (message "agent-shell-queue: unparsable item in %s: %S" buf-name item)
            (when (or accept-all
                      (not (called-interactively-p 'any))
                      (let ((ch (read-char-choice
                                 (format "Remove from %s? (y)es (n)o (a)ll: " buf-name)
                                 '(?y ?n ?a))))
                        (cond ((eq ch ?a) (setq accept-all t))
                              ((eq ch ?n) nil)
                              (t t))))
              (let ((cell (assoc buf-name agent-shell-queue--items)))
                (when cell
                  (setcdr cell (cl-remove item (cdr cell) :test #'eq))))
              (cl-pushnew buf-name agent-shell-queue--session-paused :test #'equal)
              (push entry removed))))
        (if (null removed)
            (message "agent-shell-queue: no items removed")
          (setq agent-shell-queue--items
                (cl-remove-if (lambda (pair) (null (cdr pair)))
                              agent-shell-queue--items))
          (agent-shell-queue--save)
          (agent-shell-queue--refresh-buffer)
          (message "agent-shell-queue: removed %d unparsable item(s); affected queues paused"
                   (length removed)))))))

;;;###autoload
(defun agent-shell-queue-show-disk-state ()
  "Display the on-disk queue state file in a read-only popup buffer."
  (interactive)
  (let ((file (agent-shell-queue--state-file)))
    (unless (file-exists-p file)
      (user-error "Queue state file does not exist: %s" file))
    (let ((buf (get-buffer-create "*agent-shell-queue-disk*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents file)
          (goto-char (point-min)))
        (setq buffer-read-only t)
        (setq-local revert-buffer-function
                    (lambda (_ignore-auto _noconfirm)
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert-file-contents file)
                        (goto-char (point-min)))))
        (set-visited-file-name nil t)
        (rename-buffer "*agent-shell-queue-disk*" t)
        (pcase (file-name-extension file)
          ("el" (when (fboundp 'emacs-lisp-mode) (emacs-lisp-mode)))
          ("json" (when (fboundp 'json-mode) (json-mode)))
          ("yaml" (when (fboundp 'yaml-mode) (yaml-mode))))
        (read-only-mode 1))
      (display-buffer buf '(display-buffer-below-selected
                            (window-height . 0.4))))))

(defun agent-shell-queue--status-string (item &optional buf-name next-p)
  "Return a status string for ITEM in BUF-NAME.
NEXT-P, when non-nil, marks the item as the next to be dispatched."
  (car (agent-shell-queue--item-display item buf-name next-p)))


(defun agent-shell-queue--item-display (item buf-name &optional _next-p)
  "Return (STATUS-STRING . FACE) for ITEM in BUF-NAME.
NEXT-P, when non-nil, marks the item as the next to be dispatched."
  (let* ((status (agent-shell-queue-item-status item))
         (kind (agent-shell-queue-item-kind item))
         (bg (agent-shell-queue-item-background item))
         (editing (member (agent-shell-queue-item-id item) agent-shell-queue--editing-ids))
         (blocked (and buf-name (member buf-name agent-shell-queue--session-paused)))
         (unassigned (equal buf-name agent-shell-queue--unassigned-key))
         (done (eq status 'done))
         (running (eq status 'running))
         (status-str
          (cond ((eq status 'invalid) "invalid")
                ((eq kind 'context) "context")
                ((eq kind 'emacs) (if done "emacs.done" (if running "emacs.running" "emacs")))
                ((eq kind 'wait) (if done "wait.done" (if running "wait.running" "wait")))
                ((memq kind '(pause compact))
                 (if done "done"
                   (if running "running.blocked"
                     (symbol-name kind))))
                (done "done")
                (running (if bg "running.active.bg" "running.active"))
                (editing "editing")
                ((and (eq status 'active) blocked) "paused<shell>")
                ((and (eq status 'active) agent-shell-queue-paused) "paused<all>")
                ((and (eq status 'deferred) bg) "held.bg")
                ((eq status 'deferred) "held")
                ((eq status 'draft) "draft")
                (bg "scheduled.bg")
                (t "scheduled")))
         (face
          (cond ((eq status 'invalid) 'font-lock-warning-face)
                (done 'shadow)
                (running (if (memq kind '(pause compact))
                             'agent-shell-queue-blocked-face
                           'italic))
                ((eq status 'draft) 'agent-shell-queue-draft-face)
                (unassigned 'agent-shell-queue-unassigned-face)
                ((eq kind 'compact) 'agent-shell-queue-compact-face)
                ((eq kind 'emacs) 'font-lock-function-name-face)
                ((eq kind 'wait) 'font-lock-string-face)
                ((or blocked (memq kind '(pause context))) 'agent-shell-queue-blocked-face)
                (t nil))))
    (cons status-str face)))

(defun agent-shell-queue--refresh-buffer ()
  "Refresh the *agent-shell-queue* buffer if it is visible."
  (when-let ((buf (get-buffer "*agent-shell-queue*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'agent-shell-queue-mode)
          (agent-shell-queue-buffer-refresh))))))

;;; Scope / narrowing

(defvar-local agent-shell-queue--display-scope nil
  "Current display scope for the queue buffer.
nil means global (show all items).
`(directory . DIR)' means items for shell buffers under DIR.
`(buffer . BUF-NAME)' means items for exactly that buffer.")

(defun agent-shell-queue--scope-label (scope)
  "Return a short human-readable string for SCOPE."
  (pcase scope
    ('nil "global")
    (`(buffer . ,name) (format "buffer:%s" name))
    (`(directory . ,dir) (abbreviate-file-name dir))))

(defun agent-shell-queue--scope-matches-p (buf-name scope)
  "Return non-nil if BUF-NAME belongs to SCOPE.
The unassigned bucket only matches the global scope."
  (pcase scope
    ('nil t)
    (`(buffer . ,name) (equal buf-name name))
    (`(directory . ,dir)
     (and (not (equal buf-name agent-shell-queue--unassigned-key))
          (when-let ((buf (get-buffer buf-name)))
            (string-prefix-p (expand-file-name dir)
                             (expand-file-name
                              (buffer-local-value 'default-directory buf))))))))

(defun agent-shell-queue--scope-candidates ()
  "Return an alist of (LABEL . SCOPE) covering global, directories, and buffers.
Directories are derived from live shell buffers' default-directory.
Temporary buffers (no live session) are excluded from directory scopes."
  (let ((dirs (make-hash-table :test 'equal))
        (buf-entries nil)
        result)
    (dolist (pair agent-shell-queue--items)
      (let* ((buf-name (car pair))
             (buf (get-buffer buf-name)))
        (unless (equal buf-name agent-shell-queue--unassigned-key)
          (push (cons buf-name (cons 'buffer buf-name)) buf-entries)
          (when (buffer-live-p buf)
            (puthash (expand-file-name
                      (buffer-local-value 'default-directory buf))
                     t dirs)))))
    (push (cons "global (all)" nil) result)
    (dolist (dir (sort (hash-table-keys dirs) #'string<))
      (push (cons (abbreviate-file-name dir) (cons 'directory dir)) result))
    (dolist (entry (nreverse buf-entries))
      (push entry result))
    (nreverse result)))

;;;###autoload
(defun agent-shell-queue-set-scope ()
  "Narrow the queue buffer view: choose global, a directory, or a specific buffer."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((candidates (agent-shell-queue--scope-candidates))
         (table (ht-create)))
    (dolist (c candidates)
      (let* ((scope (cdr c))
             (count (cl-reduce
                     (lambda (acc pair)
                       (+ acc (if (agent-shell-queue--scope-matches-p (car pair) scope)
                                  (length (cdr pair)) 0)))
                     agent-shell-queue--items :initial-value 0)))
        (ht-set table (car c) (format "%d item(s)" count))))
    (let* ((label (annotated-completing-read table
                                             :prompt "queue scope => "
                                             :category 'agent-shell-queue-scope
                                             :require-match t
                                             :history 'agent-shell-queue-set-scope))
           (scope (cdr (assoc label candidates))))
      (setq-local agent-shell-queue--display-scope scope)
      (agent-shell-queue-buffer-refresh)
      (force-mode-line-update))))

;;;###autoload
(defun agent-shell-queue-scope-global ()
  "Reset the queue buffer to the global scope (show all items)."
  (interactive)
  (setq-local agent-shell-queue--display-scope nil)
  (agent-shell-queue-buffer-refresh)
  (force-mode-line-update))

;;;###autoload
(defun agent-shell-queue-export ()
  "Export items in the current scope to a read-only YAML buffer.
Includes all statuses (active, deferred, running, done)."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (unless (fboundp 'yaml-encode)
    (error "yaml-encode not available; install the `yaml' package"))
  (let* ((scope agent-shell-queue--display-scope)
         (label (agent-shell-queue--scope-label scope))
         (out-name (format "*agent-shell-queue-export: %s*" label))
         (buckets nil)
         (total-items (cl-reduce (lambda (acc pair)
                                   (+ acc (length (cdr pair))))
                                 agent-shell-queue--items
                                 :initial-value 0))
         (multi-p (> total-items 1)))
    (dolist (pair agent-shell-queue--items)
      (when (agent-shell-queue--scope-matches-p (car pair) scope)
        (let* ((items (if multi-p
                          (cl-remove-if
                           (lambda (item)
                             (and (eq (agent-shell-queue-item-status item) 'done)
                                  (memq (agent-shell-queue-item-kind item)
                                        '(pause compact context))))
                           (cdr pair))
                        (cdr pair)))
               (h (make-hash-table :test 'equal)))
          (puthash "buffer" (car pair) h)
          (puthash "items"
                   (vconcat (mapcar #'agent-shell-queue--item-to-yaml-edit items))
                   h)
          (push h buckets))))
    (with-current-buffer (get-buffer-create out-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if buckets (yaml-encode (vconcat (nreverse buckets))) ""))
        (goto-char (point-min))
        (when (fboundp 'yaml-mode) (yaml-mode)))
      (display-buffer (current-buffer)))))

(defvar agent-shell-queue-mode-map
  (let ((m (make-sparse-keymap)))
    ;; View / send / remove
    (define-key m (kbd "RET")      #'agent-shell-queue-buffer-view-item)
    (define-key m (kbd "s")        #'agent-shell-queue-buffer-send)
    (define-key m (kbd "k")        #'agent-shell-queue-buffer-remove)
    (define-key m (kbd "DEL")      #'agent-shell-queue-buffer-remove)
    (define-key m (kbd "R")        #'agent-shell-queue-buffer-reenqueue)
    (define-key m (kbd "A")        #'agent-shell-queue-buffer-archive)
    (define-key m (kbd "z")        #'agent-shell-queue-buffer-mark-done)
    ;; Edit
    (define-key m (kbd "e")        #'agent-shell-queue-buffer-edit)
    (define-key m (kbd "E")        #'agent-shell-queue-edit-task)
    ;; Hold / release (suspend item from auto-dispatch without removing)
    (define-key m (kbd "d")        #'agent-shell-queue-buffer-hold)
    (define-key m (kbd "u")        #'agent-shell-queue-buffer-release)
    ;; Background flag
    (define-key m (kbd "b")        #'agent-shell-queue-buffer-enable-background-task)
    (define-key m (kbd "B")        #'agent-shell-queue-buffer-disable-background-task)
    ;; Move / assign
    (define-key m (kbd "t")        #'agent-shell-queue-buffer-assign)
    (define-key m (kbd "M-<up>")   #'agent-shell-queue-buffer-move-up)
    (define-key m (kbd "M-<down>") #'agent-shell-queue-buffer-move-down)
    ;; Pause / resume session queue dispatch
    (define-key m (kbd "p")        #'agent-shell-queue-session-pause)
    (define-key m (kbd "P")        #'agent-shell-queue-session-resume)
    ;; Insert items
    (define-key m (kbd "i")        #'agent-shell-queue-insert-pause)
    (define-key m (kbd "I")        #'agent-shell-queue-insert-clear-context)
    (define-key m (kbd "!")        #'agent-shell-queue-enqueue-emacs)
    (define-key m (kbd "T")        #'agent-shell-queue-insert-wait)
    (define-key m (kbd "C")        #'agent-shell-queue-insert-compact)
    ;; Capture entry points
    (define-key m (kbd "w")        #'agent-shell-queue-capture)
    (define-key m (kbd "a")        #'agent-shell-queue-buffer-capture-after)
    (define-key m (kbd "n")        #'agent-shell-queue-capture-unassigned)
    (define-key m (kbd "y")        #'agent-shell-queue-capture-from-clipboard)
    ;; Navigation / display
    (define-key m (kbd "TAB")      #'agent-shell-queue-buffer-jump-to-next)
    (define-key m (kbd "g")        #'agent-shell-queue-buffer-refresh)
    (define-key m (kbd "r")        #'agent-shell-queue-buffer-refresh)
    (define-key m (kbd "C-r")      #'agent-shell-queue-reload)
    (define-key m (kbd "D")        #'agent-shell-queue-show-disk-state)
    ;; Scope / narrowing
    (define-key m (kbd "N")        #'agent-shell-queue-set-scope)
    (define-key m (kbd "W")        #'agent-shell-queue-scope-global)
    (define-key m (kbd "c")        #'agent-shell-queue-buffer-context-menu)
    (define-key m (kbd "x")        #'agent-shell-queue-raw-edit)
    (define-key m (kbd "X")        #'agent-shell-queue-import)
    (define-key m (kbd "O")        #'agent-shell-queue-buffer-open-shell)
    (define-key m (kbd "m")        #'agent-shell-queue-menu)
    (define-key m (kbd "?")        #'describe-bindings)
    (define-key m (kbd "q")        #'quit-window)
    m)
  "Keymap for `agent-shell-queue-mode'.")

(define-derived-mode agent-shell-queue-mode tabulated-list-mode "Queue"
  "Major mode for reviewing and managing the agent-shell prompt queue."
  (setq tabulated-list-format
        [("Status" 12 t) ("Age" 6 t) ("#" 4 nil) ("Buffer" 17 t) ("Prompt" 20 nil)])
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tab-line-mode 1)
  (setq tab-line-format
        '(:eval (let* ((state (agent-shell-queue--activity-state))
                       (sessions (length (agent-shell-buffers)))
                       (scope agent-shell-queue--display-scope)
                       (depth (cl-reduce
                               (lambda (acc pair)
                                 (+ acc (if (agent-shell-queue--scope-matches-p
                                             (car pair) scope)
                                            (length (cdr pair)) 0)))
                               agent-shell-queue--items
                               :initial-value 0))
                       (scope-display
                        (unless (null scope)
                          (format "  |  Scope: %s"
                                  (agent-shell-queue--scope-label scope))))
                       (flush-display
                        (if agent-shell-queue--last-flush-time
                            (format "%s ago" (agent-shell-queue--format-age
                                             (time-since agent-shell-queue--last-flush-time)))
                          "never"))
                       (next-display
                        (when agent-shell-queue--next-flush-time
                          (let ((remaining (float-time (time-subtract
                                                        agent-shell-queue--next-flush-time
                                                        (current-time)))))
                            (when (> remaining 0)
                              (format "  |  Next sync in %s"
                                      (agent-shell-queue--format-age
                                       (seconds-to-time remaining)))))))
                       (intercept-display
                        (let ((intercepting
                               (cl-remove-if-not
                                (lambda (b)
                                  (buffer-local-value
                                   'agent-shell-queue-intercept-mode b))
                                (agent-shell-buffers))))
                          (when intercepting
                            (format "  |  INTERCEPT: %s"
                                    (mapconcat #'buffer-name intercepting ", "))))))
                  (format " Queue: %s  |  Sessions: %d  |  Depth: %d%s%s  |  Flushed: %s%s"
                          state sessions depth (or scope-display "")
                          (or intercept-display "")
                          flush-display (or next-display ""))))))

(defun agent-shell-queue--activity-state ()
  "Return a propertized string describing the queue's current activity level."
  (cond
   (agent-shell-queue-paused
    (propertize "PAUSED" 'face 'warning))
   ((cl-some (lambda (pair)
               (cl-some (lambda (item)
                          (eq (agent-shell-queue-item-status item) 'running))
                        (cdr pair)))
             agent-shell-queue--items)
    (propertize "running" 'face 'success))
   ((cl-some (lambda (pair)
               (cl-some (lambda (item)
                          (eq (agent-shell-queue-item-status item) 'active))
                        (cdr pair)))
             agent-shell-queue--items)
    (propertize "waiting" 'face 'font-lock-comment-face))
   (t
    (propertize "idle" 'face 'shadow))))


(defun agent-shell-queue-buffer-refresh ()
  "Rebuild the tabulated list from current queue state."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((scope agent-shell-queue--display-scope)
         (visible-items (cl-remove-if-not
                         (lambda (pair)
                           (agent-shell-queue--scope-matches-p (car pair) scope))
                         agent-shell-queue--items))
         (unassigned-pair (assoc agent-shell-queue--unassigned-key visible-items))
         (assigned-pairs (cl-remove agent-shell-queue--unassigned-key
                                    visible-items :key #'car :test #'equal))
         (ordered (if unassigned-pair
                      (append assigned-pairs (list unassigned-pair))
                    assigned-pairs))
         (single-bucket-p (= (length ordered) 1))
         (next-id-map (mapcar (lambda (pair)
                                (cons (car pair)
                                      (when-let ((next (agent-shell-queue--next-dispatchable-item
                                                        (cdr pair))))
                                        (agent-shell-queue-item-id next))))
                              agent-shell-queue--items))
         (pw (max 20 (- (window-width) (if single-bucket-p 38 43)))))
    (setq tabulated-list-format
          (if single-bucket-p
              (vector '("Status" 12 t) '("Age" 6 t) '("Buffer" 17 t) (list "Prompt" pw nil))
            (vector '("Status" 12 t) '("Age" 6 t) '("#" 4 nil) '("Buffer" 17 t)
                    (list "Prompt" pw nil))))
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (let (entries)
            (dolist (pair ordered)
              (dolist (item (cdr pair))
                (let* ((id (agent-shell-queue-item-id item))
                       (next-p (equal id (cdr (assoc (car pair) next-id-map))))
                       (display (agent-shell-queue--item-display item (car pair) next-p))
                       (status-str (car display))
                       (face (cdr display))
                       (cell (lambda (str) (if face (propertize str 'face face) str)))
                       (items-for-buf (cdr (assoc (car pair) agent-shell-queue--items)))
                       (idx (cl-position id items-for-buf
                                         :key #'agent-shell-queue-item-id :test #'equal))
                       (ordinal (if idx (1+ idx) 0))
                       (status (agent-shell-queue-item-status item))
                       (dispatched (agent-shell-queue-item-dispatched item))
                       (completed (agent-shell-queue-item-completed item))
                       (age-str (cond
                                 ((and (eq status 'done) dispatched completed)
                                  (agent-shell-queue--format-age
                                   (time-subtract completed dispatched)))
                                 ((and (eq status 'running) dispatched)
                                  (agent-shell-queue--format-age (time-since dispatched)))
                                 (t "")))
                       (first-line (car (split-string
                                         (agent-shell-queue-item-prompt item) "\n")))
                       (buf-cell (funcall cell
                                          (if (equal (car pair) agent-shell-queue--unassigned-key)
                                              "(unassigned)" (car pair)))))
                  (push (list id
                              (if single-bucket-p
                                  (vector (funcall cell status-str)
                                          (funcall cell age-str)
                                          buf-cell
                                          (funcall cell (truncate-string-to-width
                                                         first-line pw nil nil "…")))
                                (vector (funcall cell status-str)
                                        (funcall cell age-str)
                                        (funcall cell (if (> ordinal 0)
                                                         (number-to-string ordinal) ""))
                                        buf-cell
                                        (funcall cell (truncate-string-to-width
                                                       first-line pw nil nil "…")))))
                        entries))))
            (nreverse entries)))
    (tabulated-list-print t)))

(defun agent-shell-queue-buffer-jump-to-next ()
  "Move point to the next item that will be dispatched."
  (interactive)
  (let ((next-ids (cl-loop for pair in agent-shell-queue--items
                           for next = (agent-shell-queue--next-dispatchable-item (cdr pair))
                           when next collect (agent-shell-queue-item-id next))))
    (if (null next-ids)
        (message "No pending items in queue")
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (member (tabulated-list-get-id) next-ids)
              (setq found t)
            (forward-line 1)))
        (unless found
          (message "No pending items visible in current scope"))))))

(defun agent-shell-queue-buffer-hold ()
  "Hold the item at point — suspend it from auto-dispatch without removing it."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'active)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'deferred)
    (agent-shell-queue--save)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-release ()
  "Release the held item at point — resume it for auto-dispatch."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'deferred)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'active)
    (agent-shell-queue--save)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-remove ()
  "Remove the item at point from the queue, with confirmation."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (_ (agent-shell-queue--confirm-remove item)))
    (agent-shell-queue-remove id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-send ()
  "Send the item at point to its target buffer now."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (agent-shell-queue-send-item id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-reenqueue (id)
  "Create a new active queue item from the done item with ID."
  (let ((pair (agent-shell-queue--item-by-id id)))
    (unless pair (user-error "No queue item with id %s" id))
    (let* ((old-item (cdr pair))
           (buf (or (get-buffer (car pair))
                    (user-error "Target buffer %s is no longer live" (car pair)))))
      (unless (eq (agent-shell-queue-item-status old-item) 'done)
        (user-error "Item %s is not done; cannot re-enqueue" id))
      (agent-shell-queue-add
       (agent-shell-queue-item-prompt old-item)
       buf
       (agent-shell-queue-item-background old-item)))))

(defun agent-shell-queue-buffer-reenqueue ()
  "Re-enqueue the done item at point as a new active item."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (_ (eq (agent-shell-queue-item-status (cdr pair)) 'done)))
    (agent-shell-queue-reenqueue id)
    (agent-shell-queue-buffer-refresh)))

;;;###autoload
(defun agent-shell-queue-insert-pause (&optional buf position)
  "Insert a pause item into BUF's queue, optionally at 1-based POSITION.
When called interactively, prompts for the target buffer."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Insert pause for: "))
         nil))
  (when buf
    (agent-shell-queue--ensure-loaded)
    (let* ((item (agent-shell-queue-item--make
                  :id (agent-shell-queue--gen-id)
                  :prompt "[PAUSE — waiting for human]"
                  :status 'active
                  :kind 'pause
                  :created (float-time)))
           (id (agent-shell-queue-item-id item))
           (buf-name (buffer-name buf)))
      (agent-shell-queue--add-item-to-bucket buf-name item)
      (when (and position (> position 0))
        (let ((len (length (cdr (assoc buf-name agent-shell-queue--items)))))
          (dotimes (_ (max 0 (- len position)))
            (agent-shell-queue--move id -1))))
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (message "Pause inserted into %s queue" buf-name))))

;;;###autoload
(defun agent-shell-queue-insert-clear-context (prompt &optional buf)
  "Insert a context-drop item with PROMPT into BUF's queue.
When called interactively, prompts for target buffer and context text."
  (interactive
   (let* ((buf (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                   (agent-shell-queue--pick-buffer "Context drop for: ")))
          (prompt (read-string "Context: ")))
     (list prompt buf)))
  (when (and prompt buf (not (string-empty-p prompt)))
    (agent-shell-queue--ensure-loaded)
    (let* ((item (agent-shell-queue--make-item prompt nil 'context))
           (buf-name (buffer-name buf)))
      (agent-shell-queue--add-item-to-bucket buf-name item)
      (agent-shell-queue--ensure-subscription buf)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (message "Context drop inserted into %s queue" buf-name))))

;;;###autoload
(defun agent-shell-queue-insert-wait (buf)
  "Insert a wait-until item into BUF's queue.
Prompts for a target date/time; uses `org-read-date' when available,
otherwise reads a string parseable by `date-to-time' (e.g. \"2026-05-16 14:30\").
When dispatched the item blocks the queue until the target time is reached,
then marks itself done and advances to the next item automatically."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Wait in queue for: "))))
  (when buf
    (agent-shell-queue--ensure-loaded)
    (let* ((target (if (fboundp 'org-read-date)
                       (org-read-date t t nil "Wait until: ")
                     (date-to-time
                      (read-from-minibuffer "Wait until (YYYY-MM-DD HH:MM): "))))
           (display (format-time-string "%Y-%m-%d %H:%M:%S" target))
           (item (agent-shell-queue--make-item display nil 'wait))
           (buf-name (buffer-name buf)))
      (agent-shell-queue--add-item-to-bucket buf-name item)
      (agent-shell-queue--ensure-subscription buf)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (message "Wait until %s inserted into %s queue" display buf-name))))

(defun agent-shell-queue-insert-compact (prompt &optional buf)
  "Insert a compact (non-LLM manual) item with PROMPT into BUF's queue.
When dispatched the item pauses the queue and alerts; use
`agent-shell-queue-mark-done' to complete it and advance the queue."
  (interactive
   (let* ((buf (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                   (agent-shell-queue--pick-buffer "Compact item for: ")))
          (prompt (read-string "Manual task: ")))
     (list prompt buf)))
  (when (and prompt buf (not (string-empty-p prompt)))
    (agent-shell-queue--ensure-loaded)
    (let* ((item (agent-shell-queue--make-item prompt nil 'compact))
           (buf-name (buffer-name buf)))
      (agent-shell-queue--add-item-to-bucket buf-name item)
      (agent-shell-queue--ensure-subscription buf)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (message "Compact item inserted into %s queue" buf-name))))

(defun agent-shell-queue-mark-done (id)
  "Mark item ID as done without dispatching it through the LLM.
If the item is a compact item that paused a session, the session is resumed
and the queue advances to the next item."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (buf-name (car pair)))
    (when (eq (agent-shell-queue-item-status item) 'done)
      (user-error "Item %s is already done" id))
    (setf (agent-shell-queue-item-status item) 'done)
    (setf (agent-shell-queue-item-completed item) (float-time))
    (agent-shell-queue--append-done-log buf-name item)
    (when (member (cons buf-name id) agent-shell-queue--compact-running)
      (setq agent-shell-queue--compact-running
            (cl-remove (cons buf-name id) agent-shell-queue--compact-running :test #'equal))
      (setq agent-shell-queue--session-paused
            (cl-remove buf-name agent-shell-queue--session-paused :test #'equal)))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (when-let ((buf (get-buffer buf-name)))
      (agent-shell-queue--send-next-for-buffer buf))))

(defun agent-shell-queue-buffer-mark-done ()
  "Mark the item at point as done."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (agent-shell-queue-mark-done id)))

(defun agent-shell-queue-item-view-mark-done ()
  "Mark the displayed item as done."
  (interactive)
  (when-let ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-mark-done id)
    (agent-shell-queue-item-view-refresh)))

;;; Item view

(defvar agent-shell-queue-item-view-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Core lifecycle
    (define-key m (kbd "s")        #'agent-shell-queue-item-view-send)
    (define-key m (kbd "k")        #'agent-shell-queue-item-view-remove)
    (define-key m (kbd "DEL")      #'agent-shell-queue-item-view-remove)
    (define-key m (kbd "R")        #'agent-shell-queue-item-view-reenqueue)
    (define-key m (kbd "A")        #'agent-shell-queue-item-view-archive)
    (define-key m (kbd "z")        #'agent-shell-queue-item-view-mark-done)
    ;; Edit
    (define-key m (kbd "e")        #'agent-shell-queue-item-view-edit)
    ;; Hold / release
    (define-key m (kbd "d")        #'agent-shell-queue-item-view-hold)
    (define-key m (kbd "u")        #'agent-shell-queue-item-view-release)
    ;; Background flag
    (define-key m (kbd "b")        #'agent-shell-queue-item-view-enable-background-task)
    (define-key m (kbd "B")        #'agent-shell-queue-item-view-disable-background-task)
    ;; Move / assign
    (define-key m (kbd "M-<up>")   #'agent-shell-queue-item-view-move-up)
    (define-key m (kbd "M-<down>") #'agent-shell-queue-item-view-move-down)
    (define-key m (kbd "t")        #'agent-shell-queue-item-view-assign)
    ;; Refresh / menu / close
    (define-key m (kbd "g")        #'agent-shell-queue-item-view-refresh)
    (define-key m (kbd "m")        #'agent-shell-queue-item-menu)
    (define-key m (kbd "q")        #'quit-window)
    m)
  "Keymap for `agent-shell-queue-item-view-mode'.")

(define-derived-mode agent-shell-queue-item-view-mode markdown-mode "Queue-Item"
  "Read-only view of a single agent-shell queue item."
  (setq buffer-read-only t))

(defvar-local agent-shell-queue--item-view-id nil
  "ID of the queue item displayed in this item-view buffer.")

(defvar-local agent-shell-queue--item-view-queue-buf nil
  "The queue buffer that spawned this item view.")

(defun agent-shell-queue--render-item-view (id item target)
  "Render ITEM with ID and TARGET into the current buffer."
  (let* ((created (agent-shell-queue-item-created item))
         (dispatched (agent-shell-queue-item-dispatched item))
         (completed (agent-shell-queue-item-completed item))
         (bg (agent-shell-queue-item-background item))
         (kind (agent-shell-queue-item-kind item))
         (next-p (when-let ((first (agent-shell-queue--next-dispatchable-item
                                    (cdr (assoc target agent-shell-queue--items)))))
                   (equal (agent-shell-queue-item-id first) id)))
         (sep (make-string 40 ?─)))
    (insert (propertize "Queue Item\n" 'face 'bold))
    (insert sep "\n")
    (insert (format "%-12s %s\n" "ID:" id))
    (insert (format "%-12s %s\n" "Target:"
                    (if (equal target agent-shell-queue--unassigned-key)
                        "(unassigned)" target)))
    (insert (format "%-12s %s\n" "Status:" (agent-shell-queue--status-string item target next-p)))
    (insert (format "%-12s %s\n" "Kind:" (symbol-name (or kind 'prompt))))
    (insert (format "%-12s %s\n" "Background:" (if bg "yes" "no")))
    (insert sep "\n")
    (insert (format "%-12s %s (%s ago)\n" "Created:"
                    (format-time-string "%F %T" created)
                    (agent-shell-queue--format-age (time-since created))))
    (when dispatched
      (insert (format "%-12s %s (%s ago)\n" "Dispatched:"
                      (format-time-string "%F %T" dispatched)
                      (agent-shell-queue--format-age (time-since dispatched)))))
    (when completed
      (insert (format "%-12s %s (%s ago)\n" "Completed:"
                      (format-time-string "%F %T" completed)
                      (agent-shell-queue--format-age (time-since completed)))))
    (when (and dispatched completed)
      (insert (format "%-12s %s\n" "Latency:"
                      (agent-shell-queue--format-age
                       (time-subtract completed dispatched)))))
    (insert sep "\n")
    (insert (propertize "Prompt:\n" 'face 'bold))
    (insert (agent-shell-queue-item-prompt item) "\n")
    (when-let ((response (agent-shell-queue-item-response item)))
      (insert sep "\n")
      (insert (propertize "Response:\n" 'face 'bold))
      (insert response "\n"))
    (insert sep "\n")
    (insert (propertize
             "[e] edit  [s] send  [k] remove  [d/u] hold/release  [m] menu  [q] close"
             'face 'shadow))))

(defun agent-shell-queue-buffer-view-item ()
  "Open an item-view window below showing the item at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (target (car pair))
              (queue-buf (current-buffer))
              (view-name (format "*agent-shell-queue-item: %s*" id))
              (view-buf (get-buffer-create view-name)))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-shell-queue-item-view-mode)
        (setq agent-shell-queue--item-view-id id
              agent-shell-queue--item-view-queue-buf queue-buf)
        (agent-shell-queue--render-item-view id item target)))
    (display-buffer view-buf '(display-buffer-below-selected
                               (window-height . 0.35)))))

(defun agent-shell-queue-item-view-refresh ()
  "Refresh the content of the current item-view buffer."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (target (car pair)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (agent-shell-queue--render-item-view id item target))))

(defun agent-shell-queue-item-view-send ()
  "Send the displayed item to its target buffer now."
  (interactive)
  (when-let ((id agent-shell-queue--item-view-id))
    (quit-window)
    (agent-shell-queue-send-item id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-item-view-remove ()
  "Remove the displayed item from the queue, with confirmation."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (_ (agent-shell-queue--confirm-remove item)))
    (quit-window)
    (agent-shell-queue-remove id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-item-view-hold ()
  "Suspend the displayed item from auto-dispatch."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'active)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'deferred)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-release ()
  "Resume the displayed item for auto-dispatch."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'deferred)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'active)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-reenqueue ()
  "Re-enqueue the displayed done item as a new active item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (_ (eq (agent-shell-queue-item-status (cdr pair)) 'done)))
    (quit-window)
    (agent-shell-queue-reenqueue id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-item-view-archive ()
  "Archive the displayed item to `agent-shell-queue-archive-file'."
  (interactive)
  (unless agent-shell-queue-archive-file
    (user-error "Set `agent-shell-queue-archive-file' before archiving"))
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (agent-shell-queue--write-archive (car pair) item)
    (quit-window)
    (agent-shell-queue-remove id)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: archived %s" id)))

(defun agent-shell-queue-item-view-enable-background-task ()
  "Flag the displayed item for background sub-agent execution."
  (interactive)
  (when-let ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-set-background-task id t)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-disable-background-task ()
  "Clear the background sub-agent flag from the displayed item."
  (interactive)
  (when-let ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-set-background-task id nil)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-move-up ()
  "Move the displayed item one position earlier in its queue."
  (interactive)
  (when-let ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-move-up id)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-move-down ()
  "Move the displayed item one position later in its queue."
  (interactive)
  (when-let ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-move-down id)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-assign ()
  "Assign the displayed item to a different agent-shell buffer."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (bufs (or (agent-shell-buffers)
                        (user-error "No live agent-shell buffers"))))
    (let* ((current-dir (when-let ((b (get-buffer (car pair))))
                          (buffer-local-value 'default-directory b)))
           (table (ht-create)))
      (dolist (buf bufs)
        (let* ((dir (buffer-local-value 'default-directory buf))
               (ann (if (and current-dir (equal dir current-dir))
                        (concat "(same dir) " (abbreviate-file-name dir))
                      (abbreviate-file-name (or dir "")))))
          (ht-set table (buffer-name buf) ann)))
      (when-let* ((new-name (annotated-completing-read table
                                                       :prompt "assign to: "
                                                       :category 'agent-shell-buffer
                                                       :require-match t
                                                       :history 'agent-shell-queue-buffer-assign))
                  ((not (equal new-name (car pair)))))
        (agent-shell-queue--assign-item id new-name)
        (agent-shell-queue--refresh-buffer)
        (agent-shell-queue-item-view-refresh)))))

(defun agent-shell-queue-item-view-edit ()
  "Open the edit buffer for the displayed item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (qbuf agent-shell-queue--item-view-queue-buf)
              ((buffer-live-p qbuf)))
    (quit-window)
    (with-current-buffer qbuf
      (goto-char (point-min))
      (while (and (not (equal (tabulated-list-get-id) id))
                  (not (eobp)))
        (forward-line 1))
      (agent-shell-queue-buffer-edit))))

;;; Transient predicates

(defun agent-shell-queue--iv-item ()
  "Return the item being viewed in the current item-view buffer, or nil."
  (when (and (boundp 'agent-shell-queue--item-view-id) agent-shell-queue--item-view-id)
    (cdr (agent-shell-queue--item-by-id agent-shell-queue--item-view-id))))

(defun agent-shell-queue--iv-status ()
  "Return the status of the item being viewed, or nil."
  (when-let ((item (agent-shell-queue--iv-item)))
    (agent-shell-queue-item-status item)))

(defun agent-shell-queue--iv-bg-p ()
  "Return non-nil if the viewed item has background mode enabled."
  (when-let ((item (agent-shell-queue--iv-item)))
    (agent-shell-queue-item-background item)))

(defun agent-shell-queue--point-item ()
  "Return the queue item at point in the queue buffer, or nil."
  (when-let ((id (and (derived-mode-p 'agent-shell-queue-mode)
                      (tabulated-list-get-id))))
    (cdr (agent-shell-queue--item-by-id id))))

(defun agent-shell-queue--point-status ()
  "Return the status of the queue item at point, or nil."
  (when-let ((item (agent-shell-queue--point-item)))
    (agent-shell-queue-item-status item)))

(defun agent-shell-queue--point-bg-p ()
  "Return non-nil if the item at point has background mode enabled."
  (when-let ((item (agent-shell-queue--point-item)))
    (agent-shell-queue-item-background item)))

(transient-define-prefix agent-shell-queue-item-menu ()
  "Actions for the item shown in the current item-view buffer."
  [["Send / Lifecycle"
    ("s" "Send now" agent-shell-queue-item-view-send
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running draft)))))
    ("z" "Mark done" agent-shell-queue-item-view-mark-done
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running)))))
    ("R" "Re-enqueue" agent-shell-queue-item-view-reenqueue
     :if (lambda () (eq (agent-shell-queue--iv-status) 'done)))
    ("A" "Archive" agent-shell-queue-item-view-archive)
    ("k" "Remove" agent-shell-queue-item-view-remove)]
   ["Edit / Hold"
    ("e" "Edit" agent-shell-queue-item-view-edit
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running)))))
    ("d" "Hold (suspend from dispatch)" agent-shell-queue-item-view-hold
     :if (lambda () (eq (agent-shell-queue--iv-status) 'active)))
    ("u" "Release (resume dispatch)" agent-shell-queue-item-view-release
     :if (lambda () (memq (agent-shell-queue--iv-status) '(deferred draft))))
    ("b" "Enable background task" agent-shell-queue-item-view-enable-background-task
     :if (lambda () (and (not (memq (agent-shell-queue--iv-status) '(done running)))
                         (not (agent-shell-queue--iv-bg-p)))))
    ("B" "Disable background task" agent-shell-queue-item-view-disable-background-task
     :if (lambda () (and (not (memq (agent-shell-queue--iv-status) '(done running)))
                         (agent-shell-queue--iv-bg-p))))]
   ["Move / Assign"
    ("M-<up>" "Move up" agent-shell-queue-item-view-move-up
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running)))))
    ("M-<down>" "Move down" agent-shell-queue-item-view-move-down
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running)))))
    ("t" "Assign to shell…" agent-shell-queue-item-view-assign
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running)))))]])

(defun agent-shell-queue-buffer-move-up ()
  "Move the item at point one position earlier."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (agent-shell-queue-move-up id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-move-down ()
  "Move the item at point one position later."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (agent-shell-queue-move-down id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-enable-background-task ()
  "Flag the item at point for background sub-agent execution."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (agent-shell-queue-set-background-task id t)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-disable-background-task ()
  "Clear the background sub-agent flag from the item at point."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (agent-shell-queue-set-background-task id nil)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-assign ()
  "Assign the item at point to an agent-shell buffer.
Buffers sharing the same `default-directory' as the current target are annotated.
Items in the unassigned bucket are moved to the selected shell's queue."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (bufs (or (agent-shell-buffers)
                        (user-error "No live agent-shell buffers"))))
    (let* ((current-dir (when-let ((b (get-buffer (car pair))))
                          (buffer-local-value 'default-directory b)))
           (table (ht-create)))
      (dolist (buf bufs)
        (let* ((dir (buffer-local-value 'default-directory buf))
               (ann (if (and current-dir (equal dir current-dir))
                        (concat "(same dir) " (abbreviate-file-name dir))
                      (abbreviate-file-name (or dir "")))))
          (ht-set table (buffer-name buf) ann)))
      (when-let* ((new-name (annotated-completing-read table
                                                       :prompt "assign to: "
                                                       :category 'agent-shell-buffer
                                                       :require-match t
                                                       :history 'agent-shell-queue-buffer-assign))
                  ((not (equal new-name (car pair)))))
        (agent-shell-queue--assign-item id new-name)))))

(defun agent-shell-queue-buffer-context-menu ()
  "Offer context-sensitive actions for the item at point via completing-read."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (let* ((status (agent-shell-queue-item-status item))
           (done (eq status 'done))
           (bg (agent-shell-queue-item-background item))
           (cmds (append
                  (unless done
                    (cl-remove nil
                      (list
                       (cons "send now" #'agent-shell-queue-buffer-send)
                       (when (eq status 'active)
                         (cons "hold (suspend from dispatch)" #'agent-shell-queue-buffer-hold))
                       (when (eq status 'deferred)
                         (cons "release (resume dispatch)" #'agent-shell-queue-buffer-release))
                       (if bg
                           (cons "disable background sub-agent" #'agent-shell-queue-buffer-disable-background-task)
                         (cons "enable background sub-agent" #'agent-shell-queue-buffer-enable-background-task))
                       (cons "edit prompt" #'agent-shell-queue-buffer-edit)
                       (cons "assign to shell" #'agent-shell-queue-buffer-assign)
                       (cons "move up" #'agent-shell-queue-buffer-move-up)
                       (cons "move down" #'agent-shell-queue-buffer-move-down)
                       (cons "insert pause checkpoint" #'agent-shell-queue-insert-pause)
                       (cons "insert context drop" #'agent-shell-queue-insert-clear-context))))
                  (when done
                    (list (cons "re-enqueue (new active copy)" #'agent-shell-queue-buffer-reenqueue)))
                  (list (cons "remove" #'agent-shell-queue-buffer-remove))))
           (table (ht-create)))
      (dolist (c cmds)
        (ht-set table (car c)
                (or (car (split-string (or (documentation (cdr c)) "") "\n")) "")))
      (when-let* ((choice (annotated-completing-read table
                                                     :prompt "action => "
                                                     :category 'agent-shell-queue-action
                                                     :require-match t
                                                     :history 'agent-shell-queue-buffer-context-menu))
                  (cmd (cdr (assoc choice cmds))))
        (call-interactively cmd)))))

;;;###autoload
(defun agent-shell-queue-buffer-open ()
  "Open (or refresh) the *agent-shell-queue* buffer."
  (interactive)
  (let ((buf (get-buffer-create "*agent-shell-queue*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-shell-queue-mode)
        (agent-shell-queue-mode))
      (agent-shell-queue-buffer-refresh))
    (pop-to-buffer buf)))

(defun agent-shell-queue-buffer-open-shell ()
  "Switch to the shell buffer for the item at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (buf (or (get-buffer (car pair))
                       (user-error "Shell buffer %s is not live" (car pair)))))
    (pop-to-buffer buf)))

;;;###autoload
(defun agent-shell-queue-capture (&optional buf)
  "Open a capture buffer targeting BUF (nil adds to the unassigned queue).
When called interactively from an agent-shell buffer, targets that buffer.
With a prefix argument, opens an unassigned capture instead."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (agent-shell-queue--open-capture buf (current-buffer)))

;;; Transient menu

(transient-define-prefix agent-shell-queue-menu ()
  "Actions for the item at point in the queue buffer."
  [["Queue control"
    ("p" "Pause (global)" agent-shell-queue-pause)
    ("R" "Resume (global)" agent-shell-queue-resume)
    ("lp" "Pause (session)" agent-shell-queue-session-pause)
    ("lR" "Resume (session)" agent-shell-queue-session-resume)
    ("U" "Resume all sessions" agent-shell-queue-unpause-all-sessions)
    ("F" "Flush to disk" agent-shell-queue-flush)
    ("D" "Show disk state" agent-shell-queue-show-disk-state)
    ("o" "Open queue buffer" agent-shell-queue-buffer-open)
    ("O" "Open shell for item at point" agent-shell-queue-buffer-open-shell)]
   ["Send / Remove"
    ("s" "Send now" agent-shell-queue-buffer-send
     :if (lambda () (not (memq (agent-shell-queue--point-status) '(done running nil draft)))))
    ("z" "Mark done" agent-shell-queue-buffer-mark-done
     :if (lambda () (not (memq (agent-shell-queue--point-status) '(done running nil)))))
    ("S" "Resend item" agent-shell-queue-buffer-reenqueue
     :if (lambda () (eq (agent-shell-queue--point-status) 'done)))
    ("A" "Archive" agent-shell-queue-buffer-archive
     :if (lambda () (agent-shell-queue--point-item)))
    ("k" "Remove" agent-shell-queue-buffer-remove
     :if (lambda () (agent-shell-queue--point-item)))]
   ["Edit / Hold Task"
    ("E" "Edit task (select)" agent-shell-queue-edit-task)
    ("e" "Edit at point" agent-shell-queue-buffer-edit
     :if (lambda () (not (memq (agent-shell-queue--point-status) '(done running nil)))))
    ("d" "Hold (suspend from dispatch)" agent-shell-queue-buffer-hold
     :if (lambda () (eq (agent-shell-queue--point-status) 'active)))
    ("u" "Release (resume dispatch)" agent-shell-queue-buffer-release
     :if (lambda () (memq (agent-shell-queue--point-status) '(deferred draft))))
    ("b" "Enable background task" agent-shell-queue-buffer-enable-background-task
     :if (lambda () (and (not (memq (agent-shell-queue--point-status) '(done running nil)))
                         (not (agent-shell-queue--point-bg-p)))))
    ("B" "Disable background task" agent-shell-queue-buffer-disable-background-task
     :if (lambda () (and (not (memq (agent-shell-queue--point-status) '(done running nil)))
                         (agent-shell-queue--point-bg-p))))]
   ["Move / Assign"
    ("M-<up>" "Move up" agent-shell-queue-buffer-move-up
     :if (lambda () (not (memq (agent-shell-queue--point-status) '(done running nil)))))
    ("M-<down>" "Move down" agent-shell-queue-buffer-move-down
     :if (lambda () (not (memq (agent-shell-queue--point-status) '(done running nil)))))
    ("t" "Assign to shell…" agent-shell-queue-buffer-assign
     :if (lambda () (not (memq (agent-shell-queue--point-status) '(done running nil)))))]]
  [["Capture"
    ("w" "Compose (write)" agent-shell-queue-capture)
    ("a" "Insert after point" agent-shell-queue-buffer-capture-after)
    ("n" "Unassigned capture" agent-shell-queue-capture-unassigned)
    ("r" "From region" agent-shell-queue-capture-from-region)
    ("y" "From clipboard" agent-shell-queue-capture-from-clipboard)
    ("c" "From context" agent-shell-queue-capture-from-context)]
   ["Insert"
    ("i" "Insert pause checkpoint" agent-shell-queue-insert-pause)
    ("I" "Insert context drop" agent-shell-queue-insert-clear-context)
    ("C" "Insert compact (manual)" agent-shell-queue-insert-compact)
    ("!" "Insert Emacs call" agent-shell-queue-enqueue-emacs)
    ("T" "Insert wait-until (timer)" agent-shell-queue-insert-wait)]
   ["Scope / Export"
    ("N" "Set scope (narrow)" agent-shell-queue-set-scope)
    ("W" "Widen to global scope" agent-shell-queue-scope-global)
    ("v" "Export scope to YAML" agent-shell-queue-export)
    ("li" "Toggle intercept mode" agent-shell-queue-toggle-intercept-mode)]
   ["Raw Edit / Import"
    ("C-e" "Raw edit queue (YAML)" agent-shell-queue-raw-edit)
    ("C-E" "Import items (YAML)" agent-shell-queue-import)
    ("C-r" "Reload from disk" agent-shell-queue-reload)]])

;;; Edit popup

(defvar agent-shell-queue-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-edit-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-edit-cancel)
    (define-key m (kbd "C-x C-s") #'agent-shell-queue-edit-save-and-flush)
    m)
  "Keymap for `agent-shell-queue-edit-mode'.")

(define-derived-mode agent-shell-queue-edit-mode markdown-mode "Queue-Edit"
  "Mode for editing a queued prompt in a popup buffer.")

(defvar-local agent-shell-queue--editing-id nil
  "Item ID being edited in this `agent-shell-queue-edit-mode' buffer.")

(defun agent-shell-queue--open-edit-for-id (id)
  "Open an edit popup for the item with ID.
Enforces the one-edit-at-a-time constraint: if a different item is already
being edited, switches to that buffer and signals an error."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (let ((existing (get-buffer "*agent-shell-queue-edit*")))
      (if (and existing
               (buffer-live-p existing)
               (not (equal (buffer-local-value 'agent-shell-queue--editing-id existing) id)))
          (progn
            (pop-to-buffer existing '(display-buffer-below-selected))
            (user-error "agent-shell-queue: already editing item %s — save or cancel first"
                        (buffer-local-value 'agent-shell-queue--editing-id existing)))
        (let ((edit-buf (get-buffer-create "*agent-shell-queue-edit*")))
          (with-current-buffer edit-buf
            (let ((prev-id agent-shell-queue--editing-id))
              (when prev-id
                (setq agent-shell-queue--editing-ids
                      (delete prev-id agent-shell-queue--editing-ids))))
            (erase-buffer)
            (insert (agent-shell-queue-item-prompt item))
            (agent-shell-queue-edit-mode)
            (setq-local agent-shell-queue--editing-id id))
          (cl-pushnew id agent-shell-queue--editing-ids :test #'equal)
          (agent-shell-queue--refresh-buffer)
          (pop-to-buffer edit-buf '(display-buffer-below-selected)))))))

(defun agent-shell-queue-buffer-edit ()
  "Open a popup buffer to edit the prompt of the item at point."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (agent-shell-queue--open-edit-for-id id)))

;;;###autoload
(defun agent-shell-queue-edit-task ()
  "Select a queued item via completing-read and open its edit buffer.
Candidates are all non-done, non-running items across all buffers.
Annotations show queue position, target buffer, buffer state, item status,
and age."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let ((table (ht-create))
        (id-by-key (ht-create)))
    (dolist (pair agent-shell-queue--items)
      (let* ((buf-name (car pair))
             (buf (get-buffer buf-name))
             (buf-state (cond
                         ((member buf-name agent-shell-queue--session-paused) "paused")
                         ((and buf (with-current-buffer buf (shell-maker-busy))) "busy")
                         (t "idle"))))
        (cl-loop for item in (cdr pair)
                 for pos from 1
                 unless (memq (agent-shell-queue-item-status item) '(done running))
                 do (let* ((id (agent-shell-queue-item-id item))
                           (prompt (agent-shell-queue-item-prompt item))
                           (status (agent-shell-queue--status-string item))
                           (age (agent-shell-queue--format-age
                                 (time-since (agent-shell-queue-item-created item))))
                           (key (format "%s: %s" id
                                        (truncate-string-to-width prompt 60 nil nil "…")))
                           (ann (format "#%d · %s [%s] · %s · %s"
                                        pos buf-name buf-state status age)))
                      (ht-set table key ann)
                      (ht-set id-by-key key id)))))
    (when (ht-empty-p table)
      (user-error "No editable queued items"))
    (when-let* ((choice (annotated-completing-read table
                                                   :prompt "edit task: "
                                                   :category 'agent-shell-queue-item
                                                   :require-match t
                                                   :history 'agent-shell-queue-edit-task))
                (id (ht-get id-by-key choice)))
      (agent-shell-queue--open-edit-for-id id))))

(defun agent-shell-queue-edit-save-and-flush ()
  "Save the edited prompt, close the popup, and flush the queue to disk."
  (interactive)
  (agent-shell-queue-edit-confirm)
  (agent-shell-queue--save)
  (message "agent-shell-queue: edit saved and flushed to disk"))

(defun agent-shell-queue-edit-confirm ()
  "Save the edited prompt and close the popup."
  (interactive)
  (let ((new-prompt (string-trim (buffer-string)))
        (id agent-shell-queue--editing-id))
    (setq agent-shell-queue--editing-ids
          (delete id agent-shell-queue--editing-ids))
    (quit-window t)
    (unless (string-empty-p new-prompt)
      (agent-shell-queue-edit id new-prompt))
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-edit-cancel ()
  "Discard edits and close the popup."
  (interactive)
  (let ((id agent-shell-queue--editing-id))
    (setq agent-shell-queue--editing-ids
          (delete id agent-shell-queue--editing-ids)))
  (quit-window t)
  (agent-shell-queue--refresh-buffer))

;;; Capture buffer

(defvar agent-shell-queue-capture-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-capture-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-capture-cancel)
    (define-key m (kbd "C-c C-s") #'agent-shell-queue-capture-save-draft)
    (define-key m (kbd "C-c C-b") #'agent-shell-queue-capture-enable-background-task)
    (define-key m (kbd "C-c M-b") #'agent-shell-queue-capture-disable-background-task)
    (define-key m (kbd "C-c C-y") #'agent-shell-queue-capture-yank-kill)
    (define-key m (kbd "C-c M-w") #'agent-shell-queue-capture-yank-clipboard)
    (define-key m (kbd "C-c C-p") #'agent-shell-queue-capture-insert-thing-at-point)
    (define-key m (kbd "C-c C-x") #'agent-shell-queue-capture-select-context)
    m)
  "Keymap for `agent-shell-queue-capture-mode'.")

(define-derived-mode agent-shell-queue-capture-mode markdown-mode "Queue-Capture"
  "Mode for composing a queued agent-shell prompt.
\\{agent-shell-queue-capture-mode-map}")

(defvar-local agent-shell-queue--capture-target nil
  "Target agent-shell buffer for this capture session.")

(defvar-local agent-shell-queue--capture-origin nil
  "Buffer from which capture was launched, used for context insertion.")

(defvar-local agent-shell-queue--capture-background-task nil
  "When non-nil, the captured prompt will be flagged for background execution.")

(defvar-local agent-shell-queue--capture-after-id nil
  "When non-nil, the confirmed item will be inserted after this item ID.")

(defvar-local agent-shell-queue--capture-draft-id nil
  "ID of the draft item saved from this capture buffer, or nil.
Set by `agent-shell-queue-capture-save-draft' and used to update rather
than duplicate the draft on subsequent saves.")

(defun agent-shell-queue--open-capture (target-buf &optional origin-buf initial-content)
  "Open a capture buffer targeting TARGET-BUF (nil for unassigned queue).
Multiple capture buffers can be open simultaneously; each is named after its target.
ORIGIN-BUF is used for context-insertion commands; defaults to current buffer.
INITIAL-CONTENT, when non-nil, is inserted into the buffer before display."
  (let ((capture-buf (get-buffer-create
                      (if target-buf
                          (format "*agent-shell-queue-capture: %s*" (buffer-name target-buf))
                        "*agent-shell-queue-capture: unassigned*"))))
    (with-current-buffer capture-buf
      (erase-buffer)
      (agent-shell-queue-capture-mode)
      (setq agent-shell-queue--capture-target target-buf
            agent-shell-queue--capture-origin (or origin-buf (current-buffer))
            agent-shell-queue--capture-background-task nil
            agent-shell-queue--capture-after-id nil)
      (when (and initial-content (not (string-empty-p initial-content)))
        (insert initial-content)))
    (pop-to-buffer capture-buf '(display-buffer-below-selected))
    capture-buf))

(defun agent-shell-queue-capture-confirm ()
  "Confirm capture: queue the buffer contents and close."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (buf agent-shell-queue--capture-target)
        (bg agent-shell-queue--capture-background-task)
        (after-id agent-shell-queue--capture-after-id))
    (quit-window t)
    (unless (string-empty-p prompt)
      (message "agent-shell: %s" (truncate-string-to-width prompt 200 nil nil "…"))
      (cond
       (after-id
        (when-let ((pair (agent-shell-queue--item-by-id after-id)))
          (let* ((bucket-name (car pair))
                 (item (agent-shell-queue--make-item prompt bg))
                 (items (cdr (assoc bucket-name agent-shell-queue--items)))
                 (idx (cl-position after-id items
                                   :key #'agent-shell-queue-item-id :test #'equal)))
            (if idx
                (let ((cell (assoc bucket-name agent-shell-queue--items)))
                  (setcdr cell (append (cl-subseq items 0 (1+ idx))
                                       (list item)
                                       (cl-subseq items (1+ idx)))))
              (agent-shell-queue--add-item-to-bucket bucket-name item))
            (when buf (agent-shell-queue--ensure-subscription buf))
            (agent-shell-queue--save)
            (agent-shell-queue--refresh-buffer))))
       (buf
        (agent-shell-queue-add prompt buf bg)
        (agent-shell-queue--send-next-for-buffer buf))
       (t
        (agent-shell-queue-add-unassigned prompt bg))))))

(defun agent-shell-queue-capture-cancel ()
  "Discard the capture buffer without queuing."
  (interactive)
  (quit-window t))

(defun agent-shell-queue-capture-save-draft ()
  "Save capture buffer contents as a draft queue item without closing.
The item is stored with `draft' status and skipped by dispatch.
If a draft was previously saved from this buffer it is updated in place."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (buf agent-shell-queue--capture-target)
        (bg agent-shell-queue--capture-background-task))
    (when (string-empty-p prompt)
      (user-error "Buffer is empty — nothing to save as draft"))
    (if (and agent-shell-queue--capture-draft-id
             (agent-shell-queue--item-by-id agent-shell-queue--capture-draft-id))
        (progn
          (agent-shell-queue-edit agent-shell-queue--capture-draft-id prompt)
          (message "agent-shell-queue: draft updated (%s)" agent-shell-queue--capture-draft-id))
      (let* ((item (agent-shell-queue--make-item prompt bg))
             (bucket-name (if buf (buffer-name buf) agent-shell-queue--unassigned-key)))
        (setf (agent-shell-queue-item-status item) 'draft)
        (agent-shell-queue--ensure-loaded)
        (agent-shell-queue--add-item-to-bucket bucket-name item)
        (when buf (agent-shell-queue--ensure-subscription buf))
        (agent-shell-queue--save)
        (agent-shell-queue--refresh-buffer)
        (setq agent-shell-queue--capture-draft-id (agent-shell-queue-item-id item))
        (message "agent-shell-queue: draft saved (%s)" agent-shell-queue--capture-draft-id)))))

(defun agent-shell-queue-capture-enable-background-task ()
  "Flag this capture for background sub-agent execution."
  (interactive)
  (setq agent-shell-queue--capture-background-task t)
  (message "Background: on"))

(defun agent-shell-queue-capture-disable-background-task ()
  "Clear the background sub-agent flag from this capture."
  (interactive)
  (setq agent-shell-queue--capture-background-task nil)
  (message "Background: off"))

(defun agent-shell-queue-capture-yank-kill ()
  "Insert the most recent kill-ring entry at point."
  (interactive)
  (when kill-ring (insert (car kill-ring))))

(defun agent-shell-queue-capture-yank-clipboard ()
  "Insert the current clipboard contents at point."
  (interactive)
  (when-let ((sel (ignore-errors (gui-get-selection 'CLIPBOARD))))
    (insert sel)))

(defun agent-shell-queue-capture-insert-thing-at-point ()
  "Insert the thing at point from the buffer that opened this capture."
  (interactive)
  (when-let ((origin agent-shell-queue--capture-origin))
    (when (buffer-live-p origin)
      (when-let ((thing (with-current-buffer origin
                          (or (thing-at-point 'url t)
                              (thing-at-point 'filename t)
                              (thing-at-point 'symbol t)
                              (thing-at-point 'word t)))))
        (insert thing)))))

(declare-function annotated-completing-read-context-from-point "annotated-completing-read")

(defun agent-shell-queue-capture-select-context ()
  "Select a string from the origin buffer's context via completing-read and insert it."
  (interactive)
  (when-let ((origin agent-shell-queue--capture-origin))
    (when (buffer-live-p origin)
      (let ((text (with-current-buffer origin
                    (annotated-completing-read-context-from-point
                     :prompt "insert context: "
                     :history 'agent-shell-queue-capture-select-context))))
        (unless (string-empty-p text)
          (insert text))))))

;;; Entry points

;;;###autoload
(defun agent-shell-queue-enqueue (prompt &optional buf background)
  "Queue PROMPT for BUF, optionally flagged for BACKGROUND sub-agent execution.
Send immediately if BUF is idle, otherwise store in the queue.
When called interactively, opens a capture buffer for composing the prompt."
  (interactive
   (let ((target (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                     (agent-shell-queue--pick-buffer "Enqueue to: "))))
     (agent-shell-queue--open-capture target (current-buffer))
     (list nil nil nil)))
  (when prompt
    (let ((buf (or buf
                   (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                   (agent-shell-queue--pick-buffer "Enqueue to: "))))
      (with-current-buffer buf
        (if (shell-maker-busy)
            (agent-shell-queue-add prompt buf background)
          (agent-shell-insert :text (if background
                                        (concat agent-shell-queue-background-prefix prompt)
                                      prompt)
                              :submit t :no-focus t))))))

;;;###autoload
(defun agent-shell-queue-enqueue-clear (&optional buf)
  "Enqueue a clear command for BUF.
Uses `agent-shell-queue-clear-command' as the prompt."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Clear queue for: "))))
  (agent-shell-queue-enqueue agent-shell-queue-clear-command buf))

;;;###autoload
(defun agent-shell-queue-capture-unassigned ()
  "Open a capture buffer to compose a prompt for the unassigned queue.
Unassigned items display in blue and can later be assigned to a shell via `t'."
  (interactive)
  (agent-shell-queue--open-capture nil (current-buffer)))

(defun agent-shell-queue-buffer-capture-after ()
  "Open a capture buffer for an item to be inserted after the item at point.
The confirmed item is spliced into the queue immediately after the current row,
rather than appended to the end."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id)))
    (let* ((bucket-name (car pair))
           (target-buf (unless (equal bucket-name agent-shell-queue--unassigned-key)
                         (get-buffer bucket-name)))
           (capture-buf (agent-shell-queue--open-capture target-buf (current-buffer))))
      (with-current-buffer capture-buf
        (setq agent-shell-queue--capture-after-id id)
        (setq-local header-line-format
                    (propertize (format " Inserting after item %s" id) 'face 'shadow))))))

;;;###autoload
(defun agent-shell-queue-capture-from-region (&optional buf)
  "Open a capture buffer pre-seeded with the active region text.
When no region is active, opens an empty capture.  BUF is the target
agent-shell buffer; nil adds to the unassigned queue."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (let ((text (when (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end)))))
    (agent-shell-queue--open-capture buf (current-buffer) text)))

;;;###autoload
(defun agent-shell-queue-capture-from-context (&optional buf)
  "Open a capture buffer pre-seeded with a string selected from context.
Candidates include thing-at-point, the active region, the current line, and
the kill ring.  BUF is the target agent-shell buffer; nil for the unassigned queue."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (let* ((origin (current-buffer))
         (text (annotated-completing-read-context-from-point
                :prompt "seed capture: "
                :history 'agent-shell-queue-capture-from-context)))
    (agent-shell-queue--open-capture buf origin (unless (string-empty-p text) text))))

;;;###autoload
(defun agent-shell-queue-capture-from-clipboard (&optional buf)
  "Open a capture buffer pre-seeded with the current clipboard contents.
BUF is the target agent-shell buffer; nil adds to the unassigned queue."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (let ((text (ignore-errors (gui-get-selection 'CLIPBOARD))))
    (agent-shell-queue--open-capture buf (current-buffer) text)))

;;; Raw edit mode

(defvar agent-shell-queue-raw-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-raw-edit-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-raw-edit-cancel)
    m)
  "Keymap for `agent-shell-queue-raw-edit-mode'.")

(define-derived-mode agent-shell-queue-raw-edit-mode text-mode "Queue-RawEdit"
  "Mode for directly editing the queue in YAML format.
The queue is paused while this buffer is live.
Confirm with \\[agent-shell-queue-raw-edit-confirm], cancel with \\[agent-shell-queue-raw-edit-cancel].
\\{agent-shell-queue-raw-edit-mode-map}")

(defvar-local agent-shell-queue--raw-edit-snapshot nil
  "Hash-table of id→item for the queue state when raw edit was started.")

(defvar-local agent-shell-queue--raw-edit-was-paused nil
  "Whether the queue was already paused before entering raw edit mode.")

(defun agent-shell-queue--item-to-yaml-edit (item)
  "Convert ITEM to a hash-table for raw editing; omits nil timestamp fields."
  (let ((h (make-hash-table :test 'equal)))
    (puthash "id" (agent-shell-queue-item-id item) h)
    (puthash "prompt" (agent-shell-queue-item-prompt item) h)
    (puthash "status" (symbol-name (agent-shell-queue-item-status item)) h)
    (puthash "kind" (symbol-name (or (agent-shell-queue-item-kind item) 'prompt)) h)
    (puthash "background" (if (agent-shell-queue-item-background item) t nil) h)
    (puthash "created" (agent-shell-queue-item-created item) h)
    (when (agent-shell-queue-item-dispatched item)
      (puthash "dispatched" (agent-shell-queue-item-dispatched item) h))
    (when (agent-shell-queue-item-completed item)
      (puthash "completed" (agent-shell-queue-item-completed item) h))
    h))

(defun agent-shell-queue--render-to-yaml ()
  "Render active/deferred queue items to a YAML string for raw editing."
  (unless (fboundp 'yaml-encode)
    (error "yaml-encode not available; install the `yaml' package"))
  (let ((buckets (cl-remove nil
                   (mapcar (lambda (pair)
                             (let ((items (cl-remove-if
                                           (lambda (item)
                                             (memq (agent-shell-queue-item-status item)
                                                   '(done running)))
                                           (cdr pair))))
                               (when items
                                 (let ((h (make-hash-table :test 'equal)))
                                   (puthash "buffer" (car pair) h)
                                   (puthash "items"
                                            (vconcat (mapcar #'agent-shell-queue--item-to-yaml-edit items))
                                            h)
                                   h))))
                           agent-shell-queue--items))))
    (if buckets (yaml-encode (vconcat buckets)) "")))

(defun agent-shell-queue--make-edit-snapshot ()
  "Return a hash-table mapping item ID to item struct for all current items."
  (let ((table (ht-create)))
    (dolist (pair agent-shell-queue--items)
      (dolist (item (cdr pair))
        (ht-set table (agent-shell-queue-item-id item) item)))
    table))

;;;###autoload
(defun agent-shell-queue-raw-edit ()
  "Open the queue for direct YAML editing.
The queue is paused while the edit buffer is live.
Confirm changes with \\[agent-shell-queue-raw-edit-confirm];
cancel with \\[agent-shell-queue-raw-edit-cancel]."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (unless (fboundp 'yaml-encode)
    (user-error "Raw edit requires the `yaml' package"))
  (let* ((was-paused agent-shell-queue-paused)
         (buf (get-buffer-create "*agent-shell-queue-raw-edit*"))
         (yaml (agent-shell-queue--render-to-yaml)))
    (setq agent-shell-queue-paused t)
    (agent-shell-queue--refresh-buffer)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-shell-queue-raw-edit-mode)
        (setq agent-shell-queue--raw-edit-snapshot (agent-shell-queue--make-edit-snapshot)
              agent-shell-queue--raw-edit-was-paused was-paused)
        (insert yaml)))
    (pop-to-buffer buf '(display-buffer-below-selected (window-height . 0.5)))))

(defun agent-shell-queue--raw-edit-fail (text errors)
  "Save TEXT to a timestamped fail file, report ERRORS, leave queue paused."
  (let* ((dir (file-name-directory (agent-shell-queue--state-file)))
         (file (expand-file-name
                (format "asq-edit-failed-%s.yaml" (format-time-string "%Y%m%dT%H%M%S"))
                dir)))
    (with-temp-file file (insert text))
    (dolist (err (nreverse errors))
      (message "agent-shell-queue raw edit: %s" err))
    (message "agent-shell-queue: %d error(s) — buffer saved to %s (queue remains paused)"
             (length errors) file)))

(defun agent-shell-queue--parse-yaml-item (item-h snapshot)
  "Validate hash-table ITEM-H against SNAPSHOT; return (item . errors) or (nil . errors)."
  (let ((id (gethash "id" item-h))
        (prompt (gethash "prompt" item-h))
        (status-str (gethash "status" item-h "active"))
        (kind-str (gethash "kind" item-h "prompt"))
        (bg (gethash "background" item-h))
        (created (gethash "created" item-h))
        (dispatched (gethash "dispatched" item-h))
        (completed (gethash "completed" item-h))
        errors)
    (let* ((status (condition-case nil (intern status-str) (error nil)))
           (kind (condition-case nil (intern kind-str) (error nil)))
           (orig (and id snapshot (ht-get snapshot id))))
      (when (or (null prompt)
                (and (stringp prompt) (string-empty-p (string-trim prompt))))
        (push (format "item '%s': missing or empty prompt" (or id "new")) errors))
      (unless (memq status '(active deferred draft invalid))
        (push (format "item '%s': invalid status '%s'" (or id "?") status-str) errors))
      (unless (memq kind '(prompt pause context emacs wait compact))
        (push (format "item '%s': invalid kind '%s'" (or id "?") kind-str) errors))
      (when orig
        (when (and created
                   (not (equal (float created)
                               (float (agent-shell-queue-item-created orig)))))
          (push (format "item '%s': 'created' is immutable" id) errors))
        (let ((od (agent-shell-queue-item-dispatched orig)))
          (when (and od dispatched (not (equal (float dispatched) (float od))))
            (push (format "item '%s': 'dispatched' is immutable" id) errors)))
        (let ((oc (agent-shell-queue-item-completed orig)))
          (when (and oc completed (not (equal (float completed) (float oc))))
            (push (format "item '%s': 'completed' is immutable" id) errors)))
        (when (eq (agent-shell-queue-item-status orig) 'done)
          (push (format "item '%s': completed item status cannot change" id) errors)))
      (if errors
          (cons nil errors)
        (let* ((final-id (or id (agent-shell-queue--gen-id)))
               (final-created (or created
                                  (and orig (agent-shell-queue-item-created orig))
                                  (float-time)))
               (final-dispatched (or dispatched (and orig (agent-shell-queue-item-dispatched orig))))
               (final-completed (or completed (and orig (agent-shell-queue-item-completed orig)))))
          (cons (agent-shell-queue-item--make
                 :id final-id
                 :prompt (string-trim prompt)
                 :status (or status 'active)
                 :kind (or kind 'prompt)
                 :background (eq t bg)
                 :created final-created
                 :dispatched final-dispatched
                 :completed final-completed)
                nil))))))

(defun agent-shell-queue--yaml-buckets (parsed)
  "Normalize PARSED (vector or list) to a list of bucket hash-tables."
  (cond
   ((vectorp parsed) (append parsed nil))
   ((listp parsed) parsed)
   (t (list parsed))))

(defun agent-shell-queue-raw-edit-confirm ()
  "Validate and apply the YAML in this raw-edit buffer."
  (interactive)
  (unless (fboundp 'yaml-parse-string)
    (user-error "Raw edit requires the `yaml' package"))
  (let* ((text (buffer-string))
         (snapshot agent-shell-queue--raw-edit-snapshot)
         (was-paused agent-shell-queue--raw-edit-was-paused)
         errors
         parsed)
    (condition-case err
        (setq parsed (yaml-parse-string text
                                        :object-type 'hash-table
                                        :sequence-type 'list
                                        :null-object nil
                                        :false-object nil))
      (error (push (format "YAML parse error: %s" (cadr err)) errors)))
    (unless errors
      (let ((all-ids nil)
            (new-buckets nil))
        (dolist (bucket (agent-shell-queue--yaml-buckets parsed))
          (when (hash-table-p bucket)
            (let* ((buf-name (gethash "buffer" bucket))
                   (items-raw (gethash "items" bucket))
                   (items-list (cond
                                ((vectorp items-raw) (append items-raw nil))
                                ((listp items-raw) items-raw)
                                (t nil)))
                   (bucket-items nil))
              (unless buf-name
                (push "a bucket is missing the 'buffer' field" errors))
              (dolist (item-h items-list)
                (when (hash-table-p item-h)
                  (let ((id (gethash "id" item-h)))
                    (when (and id (member id all-ids))
                      (push (format "duplicate ID '%s'" id) errors))
                    (when id (push id all-ids))
                    (let ((result (agent-shell-queue--parse-yaml-item item-h snapshot)))
                      (if (cdr result)
                          (setq errors (append errors (cdr result)))
                        (push (car result) bucket-items))))))
              (when (and buf-name bucket-items)
                (push (cons buf-name (nreverse bucket-items)) new-buckets)))))
        (unless errors
          ;; Preserve running/done items from current queue
          (let ((preserved nil))
            (dolist (pair agent-shell-queue--items)
              (let ((kept (cl-remove-if-not
                           (lambda (item)
                             (memq (agent-shell-queue-item-status item) '(running done)))
                           (cdr pair))))
                (when kept
                  (push (cons (car pair) kept) preserved))))
            ;; Merge: start from edited buckets, append preserved running/done
            (let ((result (nreverse new-buckets)))
              (dolist (pair (nreverse preserved))
                (let ((cell (assoc (car pair) result)))
                  (if cell
                      (setcdr cell (append (cdr cell) (cdr pair)))
                    (push pair result))))
              (setq agent-shell-queue--items
                    (cl-remove-if (lambda (p) (null (cdr p))) result))))
          (setq agent-shell-queue-paused was-paused)
          (agent-shell-queue--save)
          (quit-window t)
          (agent-shell-queue--refresh-buffer)
          (message "agent-shell-queue: raw edit applied%s"
                   (if was-paused " (queue remains paused)" "")))))
    (when errors
      (agent-shell-queue--raw-edit-fail text errors))))

(defun agent-shell-queue-raw-edit-cancel ()
  "Cancel raw edit; restore the queue pause state from before editing."
  (interactive)
  (let ((was-paused agent-shell-queue--raw-edit-was-paused))
    (quit-window t)
    (setq agent-shell-queue-paused was-paused)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: raw edit cancelled%s"
             (if was-paused "" " (queue resumed)"))))

;;; Import

;;;###autoload
(defun agent-shell-queue-import (&optional source)
  "Import queue items from YAML.
With no prefix arg reads from clipboard; with prefix arg prompts for a file.
For each item whose ID already exists, prompts to keep, replace, or assign new ID."
  (interactive (list (if current-prefix-arg 'file 'clipboard)))
  (unless (fboundp 'yaml-parse-string)
    (user-error "Import requires the `yaml' package"))
  (agent-shell-queue--ensure-loaded)
  (let* ((text
          (if (eq source 'file)
              (let ((f (read-file-name "Import YAML from file: ")))
                (with-temp-buffer (insert-file-contents f) (buffer-string)))
            (or (ignore-errors (gui-get-selection 'CLIPBOARD))
                (user-error "Clipboard is empty"))))
         (parsed
          (condition-case err
              (yaml-parse-string text
                                 :object-type 'hash-table
                                 :sequence-type 'list
                                 :null-object nil
                                 :false-object nil)
            (error (user-error "YAML parse error: %s" (cadr err)))))
         (added 0)
         (skipped 0))
    (dolist (bucket (agent-shell-queue--yaml-buckets parsed))
      (when (hash-table-p bucket)
        (let* ((buf-name (gethash "buffer" bucket))
               (items-raw (gethash "items" bucket))
               (items-list (cond
                            ((vectorp items-raw) (append items-raw nil))
                            ((listp items-raw) items-raw)
                            (t nil))))
          (dolist (item-h items-list)
            (when (hash-table-p item-h)
              (let* ((raw-id (gethash "id" item-h))
                     (existing (and raw-id (agent-shell-queue--item-by-id raw-id)))
                     (final-id
                      (cond
                       ((null raw-id) (agent-shell-queue--gen-id))
                       ((null existing) raw-id)
                       (t (let ((choice (completing-read
                                         (format "ID '%s' exists — " raw-id)
                                         '("keep existing (skip)"
                                           "replace existing"
                                           "assign new ID")
                                         nil t)))
                            (cond
                             ((string-prefix-p "keep" choice) 'skip)
                             ((string-prefix-p "replace" choice) raw-id)
                             (t (agent-shell-queue--gen-id))))))))
                (if (eq final-id 'skip)
                    (cl-incf skipped)
                  (when (and (stringp final-id) (equal final-id raw-id) existing)
                    (agent-shell-queue-remove raw-id))
                  (let* ((prompt (gethash "prompt" item-h ""))
                         (status-str (gethash "status" item-h "active"))
                         (status (condition-case nil (intern status-str) (error 'active)))
                         (kind-str (gethash "kind" item-h "prompt"))
                         (kind (condition-case nil (intern kind-str) (error 'prompt)))
                         (bg (eq t (gethash "background" item-h)))
                         (target-buf (and buf-name (not (equal buf-name agent-shell-queue--unassigned-key))
                                          (get-buffer buf-name)))
                         (item (agent-shell-queue-item--make
                                :id final-id
                                :prompt (if (string-empty-p (string-trim (or prompt "")))
                                            "(imported)" (string-trim prompt))
                                :status (if (memq status '(active deferred invalid)) status 'active)
                                :kind (if (memq kind '(prompt pause context emacs wait compact)) kind 'prompt)
                                :background bg
                                :created (or (gethash "created" item-h) (float-time)))))
                    (when target-buf
                      (agent-shell-queue--ensure-subscription target-buf))
                    (agent-shell-queue--add-item-to-bucket
                     (if (and buf-name (not (string-empty-p buf-name)))
                         buf-name
                       agent-shell-queue--unassigned-key)
                     item)
                    (cl-incf added)))))))))
    (when (> added 0)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer))
    (message "agent-shell-queue: imported %d item(s)%s"
             added (if (> skipped 0) (format " (%d skipped)" skipped) ""))))

;;; Initialize on load

(agent-shell-queue--setup-hooks)

(provide 'agent-shell-queue)

;;; agent-shell-queue.el ends here
