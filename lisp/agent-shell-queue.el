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

(declare-function shell-maker-busy "shell-maker")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")

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

(defvar agent-shell-queue-state-file-function #'agent-shell-queue--default-state-file
  "Function returning the path to the queue state file.")

(defvar agent-shell-queue-pick-buffer-function #'agent-shell-queue--default-pick-buffer
  "Function called with a PROMPT string to pick an agent-shell buffer.")

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
  id prompt status background created completed)

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

(defun agent-shell-queue--make-item (prompt &optional background)
  "Return a new active queue item for PROMPT, optionally flagged for BACKGROUND."
  (agent-shell-queue-item--make
   :id (format "q-%d" (cl-incf agent-shell-queue--counter))
   :prompt prompt
   :status 'active
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
  (list :id         (agent-shell-queue-item-id item)
        :prompt     (agent-shell-queue-item-prompt item)
        :status     (symbol-name (agent-shell-queue-item-status item))
        :background (if (agent-shell-queue-item-background item) t :false)
        :created    (agent-shell-queue-item-created item)))

(defun agent-shell-queue--item-from-json (obj)
  "Reconstruct a queue item from JSON-parsed plist OBJ.
Status is interned; background truthy only when exactly `t'."
  (agent-shell-queue-item--make
   :id         (plist-get obj :id)
   :prompt     (plist-get obj :prompt)
   :status     (intern (plist-get obj :status))
   :background (eq t (plist-get obj :background))
   :created    (plist-get obj :created)))

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
    (puthash "id"         (agent-shell-queue-item-id item)                          h)
    (puthash "prompt"     (agent-shell-queue-item-prompt item)                      h)
    (puthash "status"     (symbol-name (agent-shell-queue-item-status item))        h)
    (puthash "background" (if (agent-shell-queue-item-background item) t nil)       h)
    (puthash "created"    (agent-shell-queue-item-created item)                     h)
    h))

(defun agent-shell-queue--item-from-yaml (obj)
  "Reconstruct a queue item from a hash-table OBJ produced by `yaml-parse-string'."
  (agent-shell-queue-item--make
   :id         (gethash "id"         obj)
   :prompt     (gethash "prompt"     obj)
   :status     (intern  (gethash "status" obj))
   :background (eq t    (gethash "background" obj))
   :created    (gethash "created"    obj)))

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

(defun agent-shell-queue--serialize ()
  "Serialize queue items using `agent-shell-queue-serialization-format'."
  (pcase agent-shell-queue-serialization-format
    ('plist (agent-shell-queue--serialize-plist))
    ('json  (agent-shell-queue--serialize-json))
    ('yaml  (agent-shell-queue--serialize-yaml))
    (fmt    (error "Unknown agent-shell-queue-serialization-format: %S" fmt))))

(defun agent-shell-queue--deserialize (str)
  "Deserialize STR using `agent-shell-queue-serialization-format'."
  (pcase agent-shell-queue-serialization-format
    ('plist (agent-shell-queue--deserialize-plist str))
    ('json  (agent-shell-queue--deserialize-json str))
    ('yaml  (agent-shell-queue--deserialize-yaml str))
    (fmt    (error "Unknown agent-shell-queue-serialization-format: %S" fmt))))

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
      (insert (agent-shell-queue--serialize)))))

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

(defun agent-shell-queue-add (prompt buf &optional background)
  "Add a new active item for PROMPT destined for BUF.  Save and refresh.
When BACKGROUND is non-nil the item is flagged for sub-agent execution.
Registers a `turn-complete' subscription on BUF if one is not already active."
  (agent-shell-queue--ensure-loaded)
  (let* ((buf-name (buffer-name buf))
         (item (agent-shell-queue--make-item prompt background))
         (pair (assoc buf-name agent-shell-queue--items)))
    (if pair
        (setcdr pair (append (cdr pair) (list item)))
      (setq agent-shell-queue--items
            (append agent-shell-queue--items (list (list buf-name item)))))
    (agent-shell-queue--ensure-subscription buf)
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

(defun agent-shell-queue--retarget-item (id new-buf-name)
  "Move the item with ID to the NEW-BUF-NAME bucket.
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
        (when-let ((new-buf (get-buffer new-buf-name)))
          (agent-shell-queue--ensure-subscription new-buf))
        (agent-shell-queue--save)
        (agent-shell-queue--refresh-buffer)))))

(defun agent-shell-queue-send-item (id)
  "Send the item with ID to its target buffer, marking it as running.
Items flagged as background are wrapped with `agent-shell-queue-background-prefix'.
The item transitions to done when the buffer's turn-complete event fires.
Running and done items are not persisted across sessions."
  (when-let ((pair (agent-shell-queue--item-by-id id)))
    (let* ((buf-name (car pair))
           (item (cdr pair))
           (buf (get-buffer buf-name)))
      (unless (buffer-live-p buf)
        (user-error "Target buffer %s is no longer live" buf-name))
      (setf (agent-shell-queue-item-status item) 'running)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (let ((prompt (if (agent-shell-queue-item-background item)
                        (concat agent-shell-queue-background-prefix
                                (agent-shell-queue-item-prompt item))
                      (agent-shell-queue-item-prompt item))))
        (agent-shell-insert :text prompt :submit t :shell-buffer buf)))))

(defun agent-shell-queue--mark-running-done (buf-name)
  "Mark any running items for BUF-NAME as done, recording completion time."
  (dolist (item (cdr (assoc buf-name agent-shell-queue--items)))
    (when (eq (agent-shell-queue-item-status item) 'running)
      (setf (agent-shell-queue-item-completed item) (float-time))
      (setf (agent-shell-queue-item-status item) 'done)
      (agent-shell-queue--append-done-log buf-name item)))
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer))

;;; Auto-send — per-buffer turn-complete subscriptions (primary) + idle timer (backup)

(defun agent-shell-queue--send-next-for-buffer (buf)
  "Attempt to send the first active queue item for BUF.
Deferred via a zero-delay timer to let the current event complete before
submitting the next prompt.  Deferred items are skipped."
  (run-with-timer
   0 nil
   (lambda ()
     (when (buffer-live-p buf)
       (with-current-buffer buf
         (unless (shell-maker-busy)
           (let* ((buf-name (buffer-name))
                  (items (cdr (assoc buf-name agent-shell-queue--items)))
                  (item (cl-find 'active items
                                 :key #'agent-shell-queue-item-status)))
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
Primary draining is handled by per-buffer `turn-complete' subscriptions."
  (when (and agent-shell-queue--loaded agent-shell-queue--items)
    (dolist (pair (copy-sequence agent-shell-queue--items))
      (let* ((buf-name (car pair))
             (buf (get-buffer buf-name)))
        (when (and (buffer-live-p buf)
                   (not (with-current-buffer buf (shell-maker-busy))))
          (let ((items (cdr (assoc buf-name agent-shell-queue--items))))
            (when-let ((item (cl-find 'active items
                                      :key #'agent-shell-queue-item-status)))
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

(defun agent-shell-queue--status-string (item)
  "Return a status string for ITEM reflecting status and background flag."
  (let ((status (agent-shell-queue-item-status item))
        (bg (agent-shell-queue-item-background item)))
    (cond ((eq status 'done)              "done")
          ((eq status 'running)           (if bg "running/bg" "running"))
          ((and (eq status 'deferred) bg) "deferred/bg")
          ((eq status 'deferred)          "deferred")
          (bg                             "active/bg")
          (t                              "active"))))

(defun agent-shell-queue--item-age-string (item)
  "Return a relative age string for ITEM.
Done items show time since completion; all others show time since creation."
  (let ((ref (if (and (eq (agent-shell-queue-item-status item) 'done)
                      (agent-shell-queue-item-completed item))
                 (agent-shell-queue-item-completed item)
               (agent-shell-queue-item-created item))))
    (agent-shell-queue--format-age (time-since ref))))

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
         (status (agent-shell-queue-item-status item))
         (done    (eq status 'done))
         (running (eq status 'running))
         (face (cond (done    '(shadow (:strike-through t)))
                     (running 'bold)
                     (t       nil)))
         (cell (lambda (str)
                 (if face (propertize str 'face face) str)))
         (ordinal (agent-shell-queue--ordinal buf-name id)))
    (list id
          (vector (funcall cell (agent-shell-queue--status-string item))
                  (funcall cell (agent-shell-queue--item-age-string item))
                  (funcall cell (if (> ordinal 0) (number-to-string ordinal) ""))
                  (funcall cell buf-name)
                  (funcall cell (truncate-string-to-width
                                 (agent-shell-queue-item-prompt item)
                                 prompt-width nil nil "…"))))))

(defvar agent-shell-queue-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "d")        #'agent-shell-queue-buffer-defer)
    (define-key m (kbd "k")        #'agent-shell-queue-buffer-remove)
    (define-key m (kbd "DEL")      #'agent-shell-queue-buffer-remove)
    (define-key m (kbd "e")        #'agent-shell-queue-buffer-edit)
    (define-key m (kbd "b")        #'agent-shell-queue-buffer-toggle-background)
    (define-key m (kbd "t")        #'agent-shell-queue-buffer-retarget)
    (define-key m (kbd "RET")      #'agent-shell-queue-buffer-view-item)
    (define-key m (kbd "s")        #'agent-shell-queue-buffer-send)
    (define-key m (kbd "M-<up>")   #'agent-shell-queue-buffer-move-up)
    (define-key m (kbd "M-<down>") #'agent-shell-queue-buffer-move-down)
    (define-key m (kbd "g")        #'agent-shell-queue-buffer-refresh)
    (define-key m (kbd "r")        #'agent-shell-queue-buffer-refresh)
    (define-key m (kbd "c")        #'agent-shell-queue-buffer-context-menu)
    (define-key m (kbd "x")        #'agent-shell-queue-buffer-context-menu)
    (define-key m (kbd "m")        #'agent-shell-queue-menu)
    (define-key m (kbd "q")        #'quit-window)
    m)
  "Keymap for `agent-shell-queue-mode'.")

(define-derived-mode agent-shell-queue-mode tabulated-list-mode "Queue"
  "Major mode for reviewing and managing the agent-shell prompt queue."
  (setq tabulated-list-format
        [("Status" 12 t) ("Age" 6 t) ("#" 4 nil) ("Buffer" 20 t) ("Prompt" 20 nil)])
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

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
          (let (entries)
            (dolist (pair agent-shell-queue--items)
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

(defun agent-shell-queue-buffer-retarget ()
  "Move the item at point to a different target agent-shell buffer.
Buffers sharing the same `default-directory' as the current target are annotated."
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
                                                       :prompt "retarget to: "
                                                       :category 'agent-shell-buffer
                                                       :require-match t
                                                       :history 'agent-shell-queue-buffer-retarget))
                  ((not (equal new-name (car pair)))))
        (agent-shell-queue--retarget-item id new-name)))))

(defun agent-shell-queue-buffer-context-menu ()
  "Offer context-sensitive actions for the item at point via completing-read."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (let* ((deferred (eq (agent-shell-queue-item-status item) 'deferred))
           (bg (agent-shell-queue-item-background item))
           (cmds (list
                  (cons "send now"
                        #'agent-shell-queue-buffer-send)
                  (cons (if deferred "undefer (resume auto-send)" "defer (freeze)")
                        #'agent-shell-queue-buffer-defer)
                  (cons (if bg "unflag background" "flag as background sub-agent")
                        #'agent-shell-queue-buffer-toggle-background)
                  (cons "edit prompt"
                        #'agent-shell-queue-buffer-edit)
                  (cons "retarget to another buffer"
                        #'agent-shell-queue-buffer-retarget)
                  (cons "move up"
                        #'agent-shell-queue-buffer-move-up)
                  (cons "move down"
                        #'agent-shell-queue-buffer-move-down)
                  (cons "remove"
                        #'agent-shell-queue-buffer-remove)))
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
(defun agent-shell-queue-open ()
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
  "Open the queue capture buffer targeting BUF.
When called interactively, prompts for a target agent-shell buffer if not
already in one."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Capture for: "))))
  (when buf
    (agent-shell-queue--open-capture buf (current-buffer))))

;;; Transient menu

(transient-define-prefix agent-shell-queue-menu ()
  "Actions for the item at point in the queue buffer."
  [["Send / Remove"
    ("RET" "View item"  agent-shell-queue-buffer-view-item)
    ("s"   "Send now"   agent-shell-queue-buffer-send)
    ("k"   "Remove"     agent-shell-queue-buffer-remove)]
   ["Edit"
    ("e" "Edit prompt"        agent-shell-queue-buffer-edit)
    ("d" "Toggle defer"       agent-shell-queue-buffer-defer)
    ("b" "Toggle background"  agent-shell-queue-buffer-toggle-background)]
   ["Move / Retarget"
    ("M-<up>"   "Move up"   agent-shell-queue-buffer-move-up)
    ("M-<down>" "Move down" agent-shell-queue-buffer-move-down)
    ("t"        "Retarget…" agent-shell-queue-buffer-retarget)]
   ["Capture"
    ("c" "New capture" agent-shell-queue-capture)]])

;;; Edit popup

(defvar agent-shell-queue-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-edit-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-edit-cancel)
    m)
  "Keymap for `agent-shell-queue-edit-mode'.")

(define-derived-mode agent-shell-queue-edit-mode text-mode "Queue-Edit"
  "Mode for editing a queued prompt in a popup buffer.")

(defvar-local agent-shell-queue--editing-id nil
  "Item ID being edited in this `agent-shell-queue-edit-mode' buffer.")

(defun agent-shell-queue-buffer-edit ()
  "Open a popup buffer to edit the prompt of the item at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (edit-buf (get-buffer-create "*agent-shell-queue-edit*")))
    (with-current-buffer edit-buf
      (erase-buffer)
      (insert (agent-shell-queue-item-prompt item))
      (agent-shell-queue-edit-mode)
      (setq-local agent-shell-queue--editing-id id))
    (pop-to-buffer edit-buf '(display-buffer-below-selected))))

(defun agent-shell-queue-edit-confirm ()
  "Save the edited prompt and close the popup."
  (interactive)
  (let ((new-prompt (string-trim (buffer-string)))
        (id agent-shell-queue--editing-id))
    (quit-window t)
    (unless (string-empty-p new-prompt)
      (agent-shell-queue-edit id new-prompt))
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-edit-cancel ()
  "Discard edits and close the popup."
  (interactive)
  (quit-window t))

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

(defun agent-shell-queue--open-capture (target-buf &optional origin-buf)
  "Open the capture buffer targeting TARGET-BUF.
ORIGIN-BUF is used for context-insertion commands; defaults to current buffer."
  (let ((capture-buf (get-buffer-create "*agent-shell-queue-capture*")))
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
      (agent-shell-queue-enqueue prompt buf bg))))

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

;;; Initialize on load

(agent-shell-queue--setup-hooks)

(provide 'agent-shell-queue)

;;; agent-shell-queue.el ends here
