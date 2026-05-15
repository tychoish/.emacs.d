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

(defconst agent-shell-queue--unassigned-key "(unassigned)"
  "Alist bucket key for items not yet assigned to any shell.")

(declare-function shell-maker-busy "shell-maker")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")

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
  (setq agent-shell-queue-instance-name #'get-instance-name")

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

(defvar agent-shell-queue--buffer-paused nil
  "List of buffer names whose queues are individually paused.
Populated automatically when `agent-shell-interrupt' is called on a buffer
with queued items.  Cleared by `agent-shell-queue-buffer-toggle-buffer-pause'.")

(defvar agent-shell-queue--editing-ids nil
  "List of item IDs currently open in an edit buffer.
Items in this list are skipped by dispatch until the edit is saved or cancelled.")

(defvar agent-shell-queue--last-flush-time nil
  "Float-time of the most recent queue state write to disk.")

(defun agent-shell-queue-toggle-pause ()
  "Toggle the global queue pause state."
  (interactive)
  (setq agent-shell-queue-paused (not agent-shell-queue-paused))
  (message "agent-shell-queue: %s"
           (if agent-shell-queue-paused "PAUSED — no new items will be sent" "running"))
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-unpause-all-buffers ()
  "Clear the per-buffer pause list, resuming all individually paused sessions."
  (interactive)
  (setq agent-shell-queue--buffer-paused nil)
  (message "agent-shell-queue: all buffer pauses cleared")
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-buffer-toggle-buffer-pause (&optional buf)
  "Toggle the per-buffer pause for BUF (default: current agent-shell buffer)."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Toggle pause for: "))))
  (when buf
    (let ((name (buffer-name buf)))
      (if (member name agent-shell-queue--buffer-paused)
          (progn
            (setq agent-shell-queue--buffer-paused
                  (delete name agent-shell-queue--buffer-paused))
            (message "agent-shell-queue: %s resumed" name))
        (cl-pushnew name agent-shell-queue--buffer-paused :test #'equal)
        (message "agent-shell-queue: %s PAUSED" name))
      (agent-shell-queue--refresh-buffer))))

(defun agent-shell-queue--on-interrupt (&optional _force)
  "Pause the per-buffer queue when `agent-shell-interrupt' is called.
Installed as :before advice on `agent-shell-interrupt'."
  (when (and (derived-mode-p 'agent-shell-mode)
             (assoc (buffer-name) agent-shell-queue--items))
    (cl-pushnew (buffer-name) agent-shell-queue--buffer-paused :test #'equal)
    (agent-shell-queue--refresh-buffer)))

(advice-add 'agent-shell-interrupt :before #'agent-shell-queue--on-interrupt)

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
  id prompt status kind background created dispatched completed)

(defvar agent-shell-queue--items nil
  "Alist of (BUFFER-NAME . ITEM-LIST) for all queued prompts.")

(defvar agent-shell-queue--counter 0
  "Monotone counter for generating unique item IDs.")

(defvar agent-shell-queue--loaded nil
  "Non-nil after the on-disk state has been read into memory.")

(defvar agent-shell-queue--idle-timer nil
  "Idle timer for auto-sending active queue items.")

(defvar agent-shell-queue--subscriptions nil
  "Alist of (BUF-NAME . TOKEN) for active `turn-complete' subscriptions.
Each entry is registered when the first item is queued for that buffer and
removed when the queue for that buffer empties or the buffer is killed.")

(defun agent-shell-queue--make-item (prompt &optional background kind)
  "Return a new active queue item for PROMPT."
  (agent-shell-queue-item--make
   :id (format "q-%d" (cl-incf agent-shell-queue--counter))
   :prompt prompt
   :status 'active
   :kind (or kind 'prompt)
   :background background
   :created (float-time)))

(defun agent-shell-queue--make-pause-item ()
  "Return a new pause item that will halt queue dispatch when reached."
  (agent-shell-queue-item--make
   :id (format "q-%d" (cl-incf agent-shell-queue--counter))
   :prompt "[PAUSE — waiting for human]"
   :status 'active
   :kind 'pause
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

(defun agent-shell-queue--active-items ()
  "Return `agent-shell-queue--items' with done and running items stripped out."
  (let ((transient '(done running)))
    (cl-remove-if
     (lambda (pair) (null (cdr pair)))
     (mapcar (lambda (pair)
               (cons (car pair)
                     (cl-remove-if (lambda (item)
                                     (memq (agent-shell-queue-item-status item) transient))
                                   (cdr pair))))
             agent-shell-queue--items))))

(defun agent-shell-queue--save ()
  "Write `agent-shell-queue--items' to disk, excluding done items."
  (let ((file (agent-shell-queue--state-file))
        (agent-shell-queue--items (agent-shell-queue--active-items)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (agent-shell-queue--serialize))))
  (setq agent-shell-queue--last-flush-time (float-time)))

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
  "Read queue state from disk; silently ignore missing or unreadable files."
  (let ((file (agent-shell-queue--state-file)))
    (when (file-exists-p file)
      (condition-case err
          (let* ((str   (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string)))
                 (items (agent-shell-queue--deserialize str)))
            (setq agent-shell-queue--items items)
            (dolist (pair agent-shell-queue--items)
              (dolist (item (cdr pair))
                (when-let ((num (string-to-number
                                 (substring (agent-shell-queue-item-id item) 2))))
                  (when (> num agent-shell-queue--counter)
                    (setq agent-shell-queue--counter num))))))
        (error (message "agent-shell-queue: ignoring unreadable state: %s" err))))))

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
Drops the `turn-complete' subscription for any bucket that becomes empty."
  (let ((before-names (mapcar #'car agent-shell-queue--items)))
    (dolist (pair agent-shell-queue--items)
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

(defun agent-shell-queue-toggle-background (id)
  "Toggle the background flag of item ID.  Save."
  (when-let ((pair (agent-shell-queue--item-by-id id)))
    (let ((item (cdr pair)))
      (setf (agent-shell-queue-item-background item)
            (not (agent-shell-queue-item-background item)))
      (agent-shell-queue--save))))

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
        (setf (agent-shell-queue-item-status item) 'running)
        (setf (agent-shell-queue-item-dispatched item) (float-time))
        (agent-shell-queue--save)
        (agent-shell-queue--refresh-buffer)
        (alert (truncate-string-to-width (agent-shell-queue-item-prompt item) 80 nil nil "…")
               :title (format "Queue → %s" buf-name)
               :category 'agent-shell-queue
               :severity 'low)
        (let ((prompt (if (agent-shell-queue-item-background item)
                          (concat agent-shell-queue-background-prefix
                                  (agent-shell-queue-item-prompt item))
                        (agent-shell-queue-item-prompt item))))
          (agent-shell-insert :text prompt :submit t :shell-buffer buf))))))

(defun agent-shell-queue--mark-running-done (buf-name)
  "Mark any running items for BUF-NAME as done, recording completion time.
Only fires the empty-queue alert when at least one item was actually marked done."
  (let ((marked nil))
    (dolist (item (cdr (assoc buf-name agent-shell-queue--items)))
      (when (eq (agent-shell-queue-item-status item) 'running)
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
                (not (member (buffer-name buf) agent-shell-queue--buffer-paused)))
       (with-current-buffer buf
         (unless (shell-maker-busy)
           (let* ((buf-name (buffer-name))
                  (items (cdr (assoc buf-name agent-shell-queue--items)))
                  (item (agent-shell-queue--next-dispatchable-item items)))
             (when item
               (if (eq (agent-shell-queue-item-kind item) 'pause)
                   (let ((id (agent-shell-queue-item-id item)))
                     (agent-shell-queue-remove id)
                     (cl-pushnew buf-name agent-shell-queue--buffer-paused :test #'equal)
                     (agent-shell-queue--refresh-buffer)
                     (alert (format "Queue for %s paused — human action required" buf-name)
                            :title "Agent Queue: Paused"
                            :category 'agent-shell-queue
                            :severity 'high
                            :persistent t))
                 (agent-shell-queue-send-item
                  (agent-shell-queue-item-id item)))))))))))

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
                   (not (member buf-name agent-shell-queue--buffer-paused))
                   (not (with-current-buffer buf (shell-maker-busy))))
          (let ((items (cdr (assoc buf-name agent-shell-queue--items))))
            (when-let ((item (agent-shell-queue--next-dispatchable-item items)))
              (agent-shell-queue-send-item
               (agent-shell-queue-item-id item)))))))))

(defun agent-shell-queue--start-timer ()
  "Start the backup idle-scan timer if it is not already running."
  (unless agent-shell-queue--idle-timer
    (setq agent-shell-queue--idle-timer
          (run-with-idle-timer agent-shell-queue-idle-delay t
                               #'agent-shell-queue--auto-send))))

(defun agent-shell-queue--setup-hooks ()
  "Start the backup idle-scan timer.
Per-buffer draining is registered lazily via `agent-shell-queue--ensure-subscription'
when items are first added for a given buffer."
  (agent-shell-queue--start-timer))

;;; Queue buffer

;;;###autoload
(defun agent-shell-queue-flush ()
  "Force-save queue state to disk immediately."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (agent-shell-queue--save)
  (message "agent-shell-queue: state saved to disk"))

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

(defun agent-shell-queue--status-string (item &optional buf-name)
  "Return a status string for ITEM in BUF-NAME."
  (car (agent-shell-queue--item-display item buf-name)))

(defun agent-shell-queue--item-age-string (item)
  "Return a relative age string for ITEM.
Done items show time since completion; all others show time since creation."
  (let ((ref (if (and (eq (agent-shell-queue-item-status item) 'done)
                      (agent-shell-queue-item-completed item))
                 (agent-shell-queue-item-completed item)
               (agent-shell-queue-item-created item))))
    (agent-shell-queue--format-age (time-since ref))))

(defun agent-shell-queue--item-display (item buf-name)
  "Return (STATUS-STRING . FACE) for ITEM in BUF-NAME."
  (let* ((status (agent-shell-queue-item-status item))
         (kind (agent-shell-queue-item-kind item))
         (bg (agent-shell-queue-item-background item))
         (editing (member (agent-shell-queue-item-id item) agent-shell-queue--editing-ids))
         (blocked (and buf-name (member buf-name agent-shell-queue--buffer-paused)))
         (unassigned (equal buf-name agent-shell-queue--unassigned-key))
         (done (eq status 'done))
         (running (eq status 'running))
         (status-str
          (cond ((eq kind 'pause) "pause")
                ((eq kind 'context) "context")
                (done "done")
                (running (if bg "running/bg" "running"))
                (editing "editing")
                ((and (eq status 'active) blocked) "buf-blocked")
                ((and (eq status 'deferred) bg) "deferred/bg")
                ((eq status 'deferred) "deferred")
                (bg "active/bg")
                (t "active")))
         (face
          (cond (done 'shadow)
                (running 'italic)
                (unassigned 'agent-shell-queue-unassigned-face)
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

(defun agent-shell-queue--prompt-column-width ()
  "Compute elastic width for the Prompt column based on the current window."
  ;; Fixed columns: Status(12) Age(6) #(4) Buffer(20) = 42, plus 4 separators
  (max 20 (- (window-width) 42 4)))

(defun agent-shell-queue--ordinal (buf-name id)
  "Return 1-based position of item ID in BUF-NAME's queue, or 0 if not found."
  (let* ((items (cdr (assoc buf-name agent-shell-queue--items)))
         (idx (cl-position id items :key #'agent-shell-queue-item-id :test #'equal)))
    (if idx (1+ idx) 0)))

(defun agent-shell-queue--make-entry (buf-name item prompt-width)
  "Build a `tabulated-list-mode' entry for ITEM in BUF-NAME using PROMPT-WIDTH."
  (let* ((id (agent-shell-queue-item-id item))
         (display (agent-shell-queue--item-display item buf-name))
         (status-str (car display))
         (face (cdr display))
         (cell (lambda (str) (if face (propertize str 'face face) str)))
         (ordinal (agent-shell-queue--ordinal buf-name id)))
    (list id
          (vector (funcall cell status-str)
                  (funcall cell (agent-shell-queue--item-age-string item))
                  (funcall cell (if (> ordinal 0) (number-to-string ordinal) ""))
                  (funcall cell (if (equal buf-name agent-shell-queue--unassigned-key)
                                    "(unassigned)" buf-name))
                  (funcall cell (truncate-string-to-width
                                 (agent-shell-queue-item-prompt item)
                                 prompt-width nil nil "…"))))))

(defvar agent-shell-queue-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "d")        #'agent-shell-queue-buffer-defer)
    (define-key m (kbd "k")        #'agent-shell-queue-buffer-remove)
    (define-key m (kbd "DEL")      #'agent-shell-queue-buffer-remove)
    (define-key m (kbd "e")        #'agent-shell-queue-buffer-edit)
    (define-key m (kbd "E")        #'agent-shell-queue-edit-task)
    (define-key m (kbd "b")        #'agent-shell-queue-buffer-toggle-background)
    (define-key m (kbd "t")        #'agent-shell-queue-buffer-assign)
    (define-key m (kbd "RET")      #'agent-shell-queue-buffer-view-item)
    (define-key m (kbd "s")        #'agent-shell-queue-buffer-send)
    (define-key m (kbd "R")        #'agent-shell-queue-buffer-reenqueue)
    (define-key m (kbd "A")        #'agent-shell-queue-buffer-archive)
    (define-key m (kbd "M-<up>")   #'agent-shell-queue-buffer-move-up)
    (define-key m (kbd "M-<down>") #'agent-shell-queue-buffer-move-down)
    (define-key m (kbd "g")        #'agent-shell-queue-buffer-refresh)
    (define-key m (kbd "r")        #'agent-shell-queue-buffer-refresh)
    (define-key m (kbd "P")        #'agent-shell-queue-toggle-pause)
    (define-key m (kbd "D")        #'agent-shell-queue-show-disk-state)
    (define-key m (kbd "c")        #'agent-shell-queue-buffer-context-menu)
    (define-key m (kbd "x")        #'agent-shell-queue-buffer-context-menu)
    (define-key m (kbd "i")        #'agent-shell-queue-insert-pause)
    (define-key m (kbd "I")        #'agent-shell-queue-insert-clear-context)
    (define-key m (kbd "m")        #'agent-shell-queue-menu)
    (define-key m (kbd "q")        #'quit-window)
    m)
  "Keymap for `agent-shell-queue-mode'.")

(define-derived-mode agent-shell-queue-mode tabulated-list-mode "Queue"
  "Major mode for reviewing and managing the agent-shell prompt queue."
  (setq tabulated-list-format
        [("Status" 12 t) ("Age" 6 t) ("#" 4 nil) ("Buffer" 20 t) ("Prompt" 20 nil)])
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tab-line-mode 1)
  (setq tab-line-format '(:eval (agent-shell-queue--header-line))))

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

(defun agent-shell-queue--header-line ()
  "Return a tab-line string: activity state, session count, depth, flush time."
  (let* ((state (agent-shell-queue--activity-state))
         (sessions (length (agent-shell-buffers)))
         (depth (cl-reduce (lambda (acc pair) (+ acc (length (cdr pair))))
                           agent-shell-queue--items
                           :initial-value 0))
         (flush-str (if agent-shell-queue--last-flush-time
                        (agent-shell-queue--format-age
                         (time-since agent-shell-queue--last-flush-time))
                      "never")))
    (format " Queue: %s  |  Sessions: %d  |  Depth: %d  |  Flushed: %s ago"
            state sessions depth flush-str)))

(defun agent-shell-queue-buffer-refresh ()
  "Rebuild the tabulated list from current queue state."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let ((pw (agent-shell-queue--prompt-column-width)))
    (setq tabulated-list-format
          (vector '("Status" 12 t) '("Age" 6 t) '("#" 4 nil) '("Buffer" 20 t)
                  (list "Prompt" pw nil)))
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (let* (entries
                 (unassigned-pair (assoc agent-shell-queue--unassigned-key agent-shell-queue--items))
                 (assigned-pairs (cl-remove agent-shell-queue--unassigned-key
                                            agent-shell-queue--items
                                            :key #'car :test #'equal))
                 (ordered (if unassigned-pair
                              (append assigned-pairs (list unassigned-pair))
                            assigned-pairs)))
            (dolist (pair ordered)
              (dolist (item (cdr pair))
                (push (agent-shell-queue--make-entry (car pair) item pw) entries)))
            (nreverse entries)))
    (tabulated-list-print t)))

(defun agent-shell-queue-buffer-defer ()
  "Toggle deferred status on the item at point."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (agent-shell-queue-defer id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-remove ()
  "Remove the item at point from the queue."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
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
    (let* ((item (agent-shell-queue--make-pause-item))
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

;;; Item detail view

(defvar agent-shell-queue-detail-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") #'agent-shell-queue-detail-edit)
    (define-key m (kbd "s") #'agent-shell-queue-detail-send)
    (define-key m (kbd "q") #'quit-window)
    m)
  "Keymap for `agent-shell-queue-detail-mode'.")

(define-derived-mode agent-shell-queue-detail-mode special-mode "Queue-Detail"
  "Read-only view of a single agent-shell queue item.")

(defvar-local agent-shell-queue--detail-id nil
  "ID of the queue item displayed in this detail buffer.")

(defvar-local agent-shell-queue--detail-queue-buf nil
  "The queue buffer that spawned this detail view.")

(defun agent-shell-queue-buffer-view-item ()
  "Open a detail window below showing the item at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (queue-buf (current-buffer))
              (detail-buf (get-buffer-create "*agent-shell-queue-detail*")))
    (with-current-buffer detail-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-shell-queue-detail-mode)
        (setq agent-shell-queue--detail-id id
              agent-shell-queue--detail-queue-buf queue-buf)
        (insert (propertize "Queue Item\n" 'face 'bold))
        (insert (make-string 40 ?─) "\n")
        (insert (format "%-12s %s\n" "ID:" id))
        (insert (format "%-12s %s\n" "Target:" (car pair)))
        (insert (format "%-12s %s\n" "Status:" (agent-shell-queue--status-string item)))
        (insert (format "%-12s %s\n" "Age:"
                        (agent-shell-queue--format-age
                         (time-since (agent-shell-queue-item-created item)))))
        (insert (make-string 40 ?─) "\n")
        (insert (propertize "Prompt:\n" 'face 'bold))
        (insert (agent-shell-queue-item-prompt item) "\n")
        (insert (make-string 40 ?─) "\n")
        (insert (propertize "[e] edit  [s] send  [q] close" 'face 'shadow))))
    (display-buffer detail-buf '(display-buffer-below-selected
                                 (window-height . 0.35)))))

(defun agent-shell-queue-detail-edit ()
  "Open the edit buffer for the item shown in this detail view."
  (interactive)
  (when-let* ((id agent-shell-queue--detail-id)
              (qbuf agent-shell-queue--detail-queue-buf)
              ((buffer-live-p qbuf)))
    (quit-window)
    (with-current-buffer qbuf
      (goto-char (point-min))
      (while (and (not (equal (tabulated-list-get-id) id))
                  (not (eobp)))
        (forward-line 1))
      (agent-shell-queue-buffer-edit))))

(defun agent-shell-queue-detail-send ()
  "Send the item shown in this detail view immediately."
  (interactive)
  (when-let ((id agent-shell-queue--detail-id))
    (quit-window)
    (agent-shell-queue-send-item id)
    (agent-shell-queue--refresh-buffer)))

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

(defun agent-shell-queue-buffer-toggle-background ()
  "Toggle the background sub-agent flag on the item at point."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (agent-shell-queue-toggle-background id)
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
           (deferred (eq status 'deferred))
           (done (eq status 'done))
           (bg (agent-shell-queue-item-background item))
           (cmds (append
                  (unless done
                    (list
                     (cons "send now" #'agent-shell-queue-buffer-send)
                     (cons (if deferred "undefer (resume auto-send)" "defer (freeze)")
                           #'agent-shell-queue-buffer-defer)
                     (cons (if bg "unflag background" "flag as background sub-agent")
                           #'agent-shell-queue-buffer-toggle-background)
                     (cons "edit prompt" #'agent-shell-queue-buffer-edit)
                     (cons "assign to shell" #'agent-shell-queue-buffer-assign)
                     (cons "move up" #'agent-shell-queue-buffer-move-up)
                     (cons "move down" #'agent-shell-queue-buffer-move-down)
                     (cons "insert pause before" #'agent-shell-queue-insert-pause)
                     (cons "insert context drop" #'agent-shell-queue-insert-clear-context)))
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
(defun agent-shell-queue-open-buffer ()
  "Open (or refresh) the *agent-shell-queue* buffer."
  (interactive)
  (let ((buf (get-buffer-create "*agent-shell-queue*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-shell-queue-mode)
        (agent-shell-queue-mode))
      (agent-shell-queue-buffer-refresh))
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
  [["Queue"
    ("P"   "Pause/resume (global)"   agent-shell-queue-toggle-pause)
    ("B"   "Pause/resume (buffer)"   agent-shell-queue-buffer-toggle-buffer-pause)
    ("U"   "Unpause all sessions"    agent-shell-queue-unpause-all-buffers)
    ("F"   "Flush to disk"           agent-shell-queue-flush)
    ("D"   "Show disk state"         agent-shell-queue-show-disk-state)
    ("o"   "Open queue buffer"       agent-shell-queue-open-buffer)
    ("i"   "Insert pause"            agent-shell-queue-insert-pause)
    ("I"   "Insert context drop"     agent-shell-queue-insert-clear-context)]
   ["Send / Remove"
    ("RET" "View item"    agent-shell-queue-buffer-view-item)
    ("s"   "Send now"     agent-shell-queue-buffer-send)
    ("R"   "Re-enqueue"   agent-shell-queue-buffer-reenqueue)
    ("A"   "Archive"      agent-shell-queue-buffer-archive)
    ("k"   "Remove"       agent-shell-queue-buffer-remove)]
   ["Edit"
    ("E"  "Edit task (select)" agent-shell-queue-edit-task)
    ("e"  "Edit at point"      agent-shell-queue-buffer-edit)
    ("d"  "Toggle defer"       agent-shell-queue-buffer-defer)
    ("b"  "Toggle background"  agent-shell-queue-buffer-toggle-background)]
   ["Move / Assign"
    ("M-<up>"   "Move up" agent-shell-queue-buffer-move-up)
    ("M-<down>" "Move down" agent-shell-queue-buffer-move-down)
    ("t"        "Assign to shell…" agent-shell-queue-buffer-assign)
    ("u"        "Enqueue unassigned" agent-shell-queue-enqueue-unassigned)]
   ["Capture"
    ("c" "New capture" agent-shell-queue-capture)]])

;;; Edit popup

(defvar agent-shell-queue-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-edit-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-edit-cancel)
    (define-key m (kbd "C-x C-s") #'agent-shell-queue-edit-save-and-flush)
    m)
  "Keymap for `agent-shell-queue-edit-mode'.")

(define-derived-mode agent-shell-queue-edit-mode text-mode "Queue-Edit"
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
                         ((member buf-name agent-shell-queue--buffer-paused) "paused")
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
    (define-key m (kbd "C-c C-b") #'agent-shell-queue-capture-toggle-background)
    (define-key m (kbd "C-c C-y") #'agent-shell-queue-capture-yank-kill)
    (define-key m (kbd "C-c M-w") #'agent-shell-queue-capture-yank-clipboard)
    (define-key m (kbd "C-c C-p") #'agent-shell-queue-capture-insert-thing-at-point)
    m)
  "Keymap for `agent-shell-queue-capture-mode'.")

(define-derived-mode agent-shell-queue-capture-mode text-mode "Queue-Capture"
  "Mode for composing a queued agent-shell prompt.
\\{agent-shell-queue-capture-mode-map}")

(defvar-local agent-shell-queue--capture-target nil
  "Target agent-shell buffer for this capture session.")

(defvar-local agent-shell-queue--capture-origin nil
  "Buffer from which capture was launched, used for context insertion.")

(defvar-local agent-shell-queue--capture-background nil
  "When non-nil, the captured prompt will be flagged for background execution.")

(defun agent-shell-queue--capture-buffer-name (target-buf)
  "Return a unique capture buffer name for TARGET-BUF (nil = unassigned)."
  (if target-buf
      (format "*agent-shell-queue-capture: %s*" (buffer-name target-buf))
    "*agent-shell-queue-capture: unassigned*"))

(defun agent-shell-queue--open-capture (target-buf &optional origin-buf)
  "Open a capture buffer targeting TARGET-BUF (nil for unassigned queue).
Multiple capture buffers can be open simultaneously; each is named after its target.
ORIGIN-BUF is used for context-insertion commands; defaults to current buffer."
  (let ((capture-buf (get-buffer-create (agent-shell-queue--capture-buffer-name target-buf))))
    (with-current-buffer capture-buf
      (erase-buffer)
      (agent-shell-queue-capture-mode)
      (setq agent-shell-queue--capture-target target-buf
            agent-shell-queue--capture-origin (or origin-buf (current-buffer))
            agent-shell-queue--capture-background nil))
    (pop-to-buffer capture-buf '(display-buffer-below-selected))
    capture-buf))

(defun agent-shell-queue-capture-confirm ()
  "Confirm capture: queue the buffer contents and close."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (buf agent-shell-queue--capture-target)
        (bg agent-shell-queue--capture-background))
    (quit-window t)
    (unless (string-empty-p prompt)
      (if buf
          (agent-shell-queue-enqueue prompt buf bg)
        (agent-shell-queue-add-unassigned prompt bg)))))

(defun agent-shell-queue-capture-cancel ()
  "Discard the capture buffer without queuing."
  (interactive)
  (quit-window t))

(defun agent-shell-queue-capture-toggle-background ()
  "Toggle whether the captured prompt will run in a background sub-agent."
  (interactive)
  (setq agent-shell-queue--capture-background
        (not agent-shell-queue--capture-background))
  (message "Background: %s"
           (if agent-shell-queue--capture-background "on" "off")))

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
(defun agent-shell-queue-enqueue-unassigned ()
  "Open a capture buffer to compose a prompt for the unassigned queue.
Unassigned items display in blue and can later be assigned to a shell via `t'."
  (interactive)
  (agent-shell-queue--open-capture nil (current-buffer)))

;;; Initialize on load

(agent-shell-queue--setup-hooks)

(provide 'agent-shell-queue)

;;; agent-shell-queue.el ends here
