;;; agent-shell-queue.el --- Persistent prompt queue for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/dot-emacs
;; Package-Requires: ((emacs "29.1") (transient "0.4") (annotated-completing-read "0.1") (agent-shell "0.1"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Implements a persistent prompt queue for agent-shell sessions, supporting
;; multi-session dispatch with pause, resume, and archive lifecycle management.
;; Queue state is serialized to plist, JSON, or YAML for session persistence
;; across Emacs restarts.  Interactive capture, edit, and item-view buffers
;; allow queue manipulation without leaving Emacs.  Fork operations split a
;; queue across multiple sessions for parallel workloads.

;;; Code:

(require 'cl-lib)
(require 'agent-shell)
(require 'annotated-completing-read)
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
(declare-function yaml-encode "yaml")
(declare-function yaml-parse-string "yaml")
(declare-function yaml-mode "yaml-mode")

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

(defvar agent-shell-queue-archive-enabled nil
  "When non-nil, completed items can be archived and `agent-shell-queue-buffer-archive'
is active.  The destination path is controlled separately by
`agent-shell-queue-archive-file-function'.  Set to t to enable archiving.")

(defvar agent-shell-queue-archive-file-function #'agent-shell-queue--default-archive-file
  "Function returning the JSONL archive file path.  Called with no arguments.
Override to store the archive at a custom location.  Only consulted when
`agent-shell-queue-archive-enabled' is non-nil.")

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
queue items to a durable store.
Used by backends such as `agent-shell-queue-db' to bypass file I/O.")

(defvar agent-shell-queue-load-function nil
  "When non-nil, called instead of the default file-based load logic.
The function is called with no arguments and must populate
the queue items from a durable store.
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

;;; Struct macro

(defmacro agent-shell-queue--defstruct (type-name &rest fields)
  "Define a cl-defstruct TYPE-NAME with FIELDS and generate plist serializers.
FIELDS are plain symbols.  Generates constructor TYPE-NAME--make plus:
  TYPE-NAME-to-plist   — struct → keyword-keyed plist
  TYPE-NAME-from-plist — keyword-keyed plist → struct"
  (declare (indent 1))
  (let* ((sname (symbol-name type-name))
         (ctor (intern (concat sname "--make")))
         (to-fn (intern (concat sname "-to-plist")))
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

(cl-defstruct (agent-shell-queue-store
               (:constructor agent-shell-queue--make-store)
               (:copier nil))
  "Queue state bundle: items, serialization format, and file path."
  items    ; (BUFFER-NAME . ITEM-LIST) alist
  format   ; symbol: plist | json | yaml
  file)    ; string: absolute path to state file

(defvar agent-shell-queue--items nil
  "Items alist used by format-specific serializers (e.g. org).
Bound dynamically by `agent-shell-queue--serialize-items' methods
before calling the format's serialize helper.")

(defvar agent-shell-queue--store
  (agent-shell-queue--make-store :items nil :format 'plist :file nil)
  "Live queue store.  Items are loaded from disk by --load, written by --save.")

(cl-defstruct (agent-shell-queue-queue
               (:constructor agent-shell-queue-queue--make)
               (:copier nil))
  "Queue runtime state and reference to the active store."
  (store 'agent-shell-queue--store) ; symbol naming the live store variable
  (paused nil) ; boolean: global pause flag
  (session-paused nil) ; list of buffer names paused from dispatch
  (editing-ids nil)) ; list of item IDs currently open in an edit buffer

(defvar agent-shell-queue--queue
  (agent-shell-queue-queue--make)
  "The active queue object.  Persisted via `savehist-additional-variables'.")

(defun agent-shell-queue-pause ()
  "Pause the global queue; no new items will be dispatched."
  (interactive)
  (setf (agent-shell-queue-queue-paused agent-shell-queue--queue) t)
  (message "agent-shell-queue: PAUSED — no new items will be dispatched")
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-resume ()
  "Resume the global queue after being paused."
  (interactive)
  (setf (agent-shell-queue-queue-paused agent-shell-queue--queue) nil)
  (message "agent-shell-queue: running")
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-unpause-all-sessions ()
  "Clear the per-session pause list, resuming all individually paused sessions."
  (interactive)
  (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) nil)
  (message "agent-shell-queue: all session pauses cleared")
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-session-pause (&optional buf)
  "Pause dispatch for BUF (default: current agent-shell session)."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Pause dispatch for: "))))
  (when-let* ((name (buffer-name buf)))
    (cl-pushnew name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
    (message "agent-shell-queue: %s PAUSED" name)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-session-resume (&optional buf)
  "Resume dispatch for BUF (default: current agent-shell session).
Any running `pause' or `compact' item for BUF is marked done automatically."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Resume dispatch for: "))))

  (when-let* ((name (buffer-name buf)))
    (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
          (delete name (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
    (setq agent-shell-queue--compact-running
          (seq-remove (lambda (it) (equal (car it) name)) agent-shell-queue--compact-running))
    (dolist (it (cdr (assoc name (agent-shell-queue-store-items agent-shell-queue--store))))
      (when (and (eq (agent-shell-queue-item-status it) 'running)
                 (memq (agent-shell-queue-item-kind it) '(pause compact)))
        (setf (agent-shell-queue-item-status it) 'done)
        (setf (agent-shell-queue-item-completed it) (float-time))
        (agent-shell-queue--append-done-log name it)))
    (agent-shell-queue--save)
    (message "agent-shell-queue: %s resumed" name)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue--send-next-for-buffer buf)))

(defun agent-shell-queue--on-interrupt (&optional _force)
  "Pause the per-buffer queue when `agent-shell-interrupt' is called.
Installed as :before advice on `agent-shell-interrupt'."
  (when (and (derived-mode-p 'agent-shell-mode)
             (assoc (buffer-name) (agent-shell-queue-store-items agent-shell-queue--store)))
    (cl-pushnew (buffer-name) (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
    (agent-shell-queue--refresh-buffer)))

(advice-add 'agent-shell-interrupt :before #'agent-shell-queue--on-interrupt)

(defvar-local agent-shell-queue-intercept-mode nil
  "When non-nil in an agent-shell buffer, capture user-typed turns as queue items.")

(defun agent-shell-queue--on-submit-intercept (&rest _)
  "Capture user-typed shell turn as a queue item when intercept mode is active.
Installed as :before advice on `shell-maker-submit'."
  (when-let* ((_ agent-shell-queue-intercept-mode)
              (_ (called-interactively-p 'interactive))
              (_ (derived-mode-p 'agent-shell-mode))
              (buf-name (buffer-name (current-buffer)))
              (input (save-excursion
                       (goto-char (point-max))
                       (when (re-search-backward comint-prompt-regexp nil t)
                         (string-trim
                          (buffer-substring-no-properties (match-end 0) (point-max))))))
              (_ (not (string-empty-p input)))
              (_ (or (agent-shell-queue--ensure-loaded) t))
              (item (agent-shell-queue--make-item input nil 'prompt)))
  (setf (agent-shell-queue-item-status item) 'running)
  (setf (agent-shell-queue-item-dispatched item) (float-time))
  (agent-shell-queue--add-item-to-bucket buf-name item)
  (push (cons (agent-shell-queue-item-id item) (point-max))
        agent-shell-queue--response-start-positions)
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer)))

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
  (expand-file-name (concat "agent-shell-queue."
                            (substring (agent-shell-queue-format-file-extension
                                        agent-shell-queue-serialization-format)
                                       1))
                    user-emacs-directory))

(defun agent-shell-queue--default-archive-file ()
  "Default JSONL archive file path under `user-emacs-directory'."
  (expand-file-name "agent-shell-queue-archive.jsonl" user-emacs-directory))

(defun agent-shell-queue--archive-file ()
  "Return the resolved archive file path, or nil if archiving is disabled."
  (when agent-shell-queue-archive-enabled
    (funcall agent-shell-queue-archive-file-function)))

(defun agent-shell-queue--default-pick-buffer (prompt)
  "Pick a live agent-shell buffer using PROMPT via `completing-read'."
  (when-let* ((bufs (agent-shell-buffers)))
    (get-buffer (completing-read prompt (seq-map #'buffer-name bufs) nil t))))

(defvar agent-shell-queue--loaded nil
  "Non-nil after the on-disk state has been read into memory.")

(defvar agent-shell-queue--idle-timer nil
  "Idle timer for auto-sending active queue items.")

(defvar agent-shell-queue--subscriptions nil
  "Alist of (BUF-NAME . TOKEN) for active `turn-complete' subscriptions.
Each entry is registered when the first item is queued for that buffer and
removed when the queue for that buffer empties or the buffer is killed.")

(defvar agent-shell-queue-blocked-session-modes '("dontAsk" "plan")
  "Session mode IDs that block queue dispatch.
When a target shell is in one of these modes the item is not sent and
the session queue is paused until the mode changes.")

(defun agent-shell-queue--gen-id ()
  "Generate a short unique item ID: q + one digit + four alphanumeric chars."
  (let ((chars "abcdefghijklmnopqrstuvwxyz0123456789"))
    (concat "q"
            (number-to-string (random 10))
            (apply #'string
                   (seq-map (lambda (_it) (aref chars (random 36))) (make-list 4 nil))))))

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

(defun agent-shell-queue--current-store ()
  "Return the live store, ensuring format and file reflect current config."
  (setf (agent-shell-queue-store-format agent-shell-queue--store)
        agent-shell-queue-serialization-format)
  (setf (agent-shell-queue-store-file agent-shell-queue--store)
        (agent-shell-queue--state-file))
  agent-shell-queue--store)

(defun agent-shell-queue--pick-buffer (prompt)
  "Pick a live agent-shell buffer using PROMPT."
  (funcall agent-shell-queue-pick-buffer-function prompt))

(defun agent-shell-queue--format-age (delta)
  "Format DELTA time-value as a short relative age string.
Alternative using stdlib: (car (split-string (format-seconds \"%dd %hh %mm %ss%z\" s)))
but that yields \"0d\" for a zero delta instead of \"0s\"."
  (let ((s (float-time delta)))
    (cond ((< s 60) (format "%ds" (truncate s)))
          ((< s 3600) (format "%dm" (truncate (/ s 60))))
          ((< s 86400) (format "%dh" (truncate (/ s 3600))))
          (t (format "%dd" (truncate (/ s 86400)))))))

;;; Persistence

;; -- plist format --

(defun agent-shell-queue--serialize-plist (items)
  "Serialize ITEMS to an s-expression string (plist item format)."
  (with-temp-buffer
    (prin1 (seq-map (lambda (it) (cons (car it) (seq-map #'agent-shell-queue-item-to-plist (cdr it)))) items)
           (current-buffer))
    (buffer-string)))

(defun agent-shell-queue--deserialize-plist (str)
  "Deserialize STR (plist format) into an items alist."
  (let ((data (read str)))
    (unless (listp data)
      (error "Expected list, got %S" data))
    (seq-map (lambda (it) (cons (car it) (seq-map #'agent-shell-queue-item-from-plist (cdr it)))) data)))

;; -- JSON format --

(defun agent-shell-queue--item-to-json (item)
  "Convert ITEM to a JSON-serializable plist.
Status is stored as a string; background as a JSON boolean."
  (list
   :id (agent-shell-queue-item-id item)
   :prompt (agent-shell-queue-item-prompt item)
   :status (symbol-name (agent-shell-queue-item-status item))
   :kind (symbol-name (or (agent-shell-queue-item-kind item) 'prompt))
   :background (if (agent-shell-queue-item-background item) t :false)
   :created (agent-shell-queue-item-created item)
   :dispatched (or (agent-shell-queue-item-dispatched item) :null)
   :completed (or (agent-shell-queue-item-completed item) :null)
   :response (or (agent-shell-queue-item-response item) :null)))

(defun agent-shell-queue--item-from-json (obj)
  "Reconstruct a queue item from JSON-parsed plist OBJ.
Status is interned; background truthy only when exactly `t'."
  (agent-shell-queue-item--make
   :id (plist-get obj :id)
   :prompt (plist-get obj :prompt)
   :status (intern (plist-get obj :status))
   :kind (intern (or (plist-get obj :kind) "prompt"))
   :background (eq t (plist-get obj :background))
   :created (plist-get obj :created)
   :dispatched (plist-get obj :dispatched)
   :completed (plist-get obj :completed)
   :response (let ((r (plist-get obj :response))) (unless (eq r :null) r))))

(defun agent-shell-queue--serialize-json (items)
  "Serialize ITEMS to a JSON string."
  (unless (fboundp 'json-serialize)
    (error "json-serialize not available (requires Emacs 27+)"))
  (json-serialize
   (vconcat
    (seq-map (lambda (it)
               (list :buffer (car it)
                     :items (vconcat (seq-map #'agent-shell-queue--item-to-json (cdr it)))))
             items))))

(defun agent-shell-queue--deserialize-json (str)
  "Deserialize STR (JSON format) into an items alist."
  (unless (fboundp 'json-parse-string)
    (error "json-parse-string not available (requires Emacs 27+)"))
  (thread-last (json-parse-string str
                                  :object-type 'plist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil)
               (seq-map (lambda (bucket)
                          (cons (plist-get bucket :buffer)
                                (seq-map #'agent-shell-queue--item-from-json
                                         (plist-get bucket :items)))))))

;; -- YAML format --

(defun agent-shell-queue--item-to-yaml (item)
  "Convert ITEM to a hash-table suitable for `yaml-encode'.
Status is stored as a string; background as t or nil."
  (let ((h (make-hash-table :test 'equal)))
    (map-put! h "id" (agent-shell-queue-item-id item))
    (map-put! h "prompt" (agent-shell-queue-item-prompt item))
    (map-put! h "status" (symbol-name (agent-shell-queue-item-status item)))
    (map-put! h "kind" (symbol-name (or (agent-shell-queue-item-kind item) 'prompt)))
    (map-put! h "background" (if (agent-shell-queue-item-background item) t nil))
    (map-put! h "created" (agent-shell-queue-item-created item))
    (map-put! h "dispatched" (agent-shell-queue-item-dispatched item))
    (map-put! h "completed" (agent-shell-queue-item-completed item))
    (map-put! h "response" (or (agent-shell-queue-item-response item) :null))
    h))

(defun agent-shell-queue--item-from-yaml (obj)
  "Reconstruct a queue item from a hash-table OBJ produced by `yaml-parse-string'."
  (agent-shell-queue-item--make
   :id (map-elt obj "id")
   :prompt (map-elt obj "prompt")
   :status (intern (map-elt obj "status"))
   :kind (intern (or (map-elt obj "kind") "prompt"))
   :background (eq t (map-elt obj "background"))
   :created (map-elt obj "created")
   :dispatched (map-elt obj "dispatched")
   :completed (map-elt obj "completed")
   :response (let ((r (map-elt obj "response"))) (unless (eq r :null) r))))

(defun agent-shell-queue--serialize-yaml (items)
  "Serialize ITEMS to a YAML string via `yaml-encode'."
  (unless (fboundp 'yaml-encode)
    (error "yaml-encode not available; install the `yaml' package"))
  (yaml-encode
   (vconcat
    (seq-map (lambda (pair)
               (let ((h (make-hash-table :test 'equal)))
                 (map-put! h "buffer" (car pair))
                 (map-put! h "items" (vconcat (seq-map #'agent-shell-queue--item-to-yaml (cdr pair))))
                 h))
             items))))

(defun agent-shell-queue--deserialize-yaml (str)
  "Deserialize STR (YAML format) into an items alist via `yaml-parse-string'."
  (unless (fboundp 'yaml-parse-string)
    (error "yaml-parse-string not available; install the `yaml' package"))
  (thread-last (yaml-parse-string str
                                  :object-type 'hash-table
                                  :sequence-type 'list
                                  :null-object nil
                                  :false-object nil)
               (seq-map (lambda (bucket)
                          (cons (map-elt bucket "buffer")
                                (seq-map #'agent-shell-queue--item-from-yaml
                                         (map-elt bucket "items")))))))

;; -- Serialization generics --

(cl-defgeneric agent-shell-queue--serialize-items (format items)
  "Serialize ITEMS for FORMAT, returning a string.
Signals an error for unknown formats.")

(cl-defgeneric agent-shell-queue--deserialize-items (format string)
  "Deserialize STRING for FORMAT, returning an items alist.
Signals an error for unknown formats.")

(cl-defgeneric agent-shell-queue-format-file-extension (_format)
  "Return the file extension string (with leading dot) for FORMAT."
  ".el")

(cl-defmethod agent-shell-queue--serialize-items ((_format (eql plist)) items)
  (agent-shell-queue--serialize-plist items))

(cl-defmethod agent-shell-queue--deserialize-items ((_format (eql plist)) string)
  (agent-shell-queue--deserialize-plist string))

(cl-defmethod agent-shell-queue-format-file-extension ((_format (eql plist)))
  ".el")

(cl-defmethod agent-shell-queue--serialize-items ((_format (eql json)) items)
  (agent-shell-queue--serialize-json items))

(cl-defmethod agent-shell-queue--deserialize-items ((_format (eql json)) string)
  (agent-shell-queue--deserialize-json string))

(cl-defmethod agent-shell-queue-format-file-extension ((_format (eql json)))
  ".json")

(cl-defmethod agent-shell-queue--serialize-items ((_format (eql yaml)) items)
  (agent-shell-queue--serialize-yaml items))

(cl-defmethod agent-shell-queue--deserialize-items ((_format (eql yaml)) string)
  (agent-shell-queue--deserialize-yaml string))

(cl-defmethod agent-shell-queue-format-file-extension ((_format (eql yaml)))
  ".yaml")

(defun agent-shell-queue-register-format (fmt serialize-fn deserialize-fn)
  "Register serialization FORMAT with SERIALIZE-FN and DESERIALIZE-FN.
FMT is a symbol; SERIALIZE-FN takes one argument (an items alist) and returns
a string; DESERIALIZE-FN takes a string and returns an items alist.
Installs cl-generic methods for `agent-shell-queue--serialize-items' and
`agent-shell-queue--deserialize-items' specialised on (eql FMT)."
  (eval `(cl-defmethod agent-shell-queue--serialize-items ((_format (eql ,fmt)) items)
           (funcall ,serialize-fn items))
        t)
  (eval `(cl-defmethod agent-shell-queue--deserialize-items ((_format (eql ,fmt)) string)
           (funcall ,deserialize-fn string))
        t))

(defun agent-shell-queue-serialize (store)
  "Serialize STORE to a string using the format recorded in STORE."
  (agent-shell-queue--serialize-items
   (agent-shell-queue-store-format store)
   (agent-shell-queue-store-items store)))

(defun agent-shell-queue-deserialize (store string)
  "Parse STRING using the format in STORE, returning an items alist."
  (agent-shell-queue--deserialize-items
   (agent-shell-queue-store-format store)
   string))

(defun agent-shell-queue--safe-save-directory ()
  "Return the directory for safe-save backups.
Uses `agent-shell-queue-safe-save-directory' when set, otherwise
a subdirectory of `temporary-file-directory' named emacs-<instance>."
  (or agent-shell-queue-safe-save-directory
      (expand-file-name
       (format "emacs-%s"
               (if (functionp agent-shell-queue-instance-name)
                   (funcall agent-shell-queue-instance-name)
                 agent-shell-queue-instance-name))
       (temporary-file-directory))))

(defun agent-shell-queue--save ()
  "Persist all queue items; all items are persisted regardless of status.
Delegates to `agent-shell-queue-save-function' when set; otherwise writes
to the file in the current store.
When `agent-shell-queue-safe-save' is non-nil and no custom save function
is set, writes a versioned backup before overwriting the state file."
  (if agent-shell-queue-save-function
      (funcall agent-shell-queue-save-function)
    (let* ((base-store (agent-shell-queue--current-store))
           (store (agent-shell-queue--make-store
                   :items (agent-shell-queue-store-items base-store)
                   :format (agent-shell-queue-store-format base-store)
                   :file (agent-shell-queue-store-file base-store)))
           (serialized (agent-shell-queue-serialize store))
           (file (agent-shell-queue-store-file store)))
      (when-let* ((_ agent-shell-queue-safe-save)
                  (_ (file-exists-p file))
                  (fmt (or agent-shell-queue-safe-save-format
                           (agent-shell-queue-store-format store)))
                  (ext (agent-shell-queue-format-file-extension fmt))
                  (dir (agent-shell-queue--safe-save-directory))
                  (backup (expand-file-name
                           (format "agent-shell-queue-archive-%d%s"
                                   (truncate (float-time)) ext)
                           dir)))
        (make-directory dir t)
        (with-temp-file backup
          (insert (agent-shell-queue-serialize
                   (agent-shell-queue--make-store
                    :items (agent-shell-queue-store-items base-store)
                    :format fmt
                    :file nil)))))
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
  (when-let* ((file (agent-shell-queue--archive-file)))
    (condition-case err
        (let* ((path (when-let* ((buf (get-buffer buf-name))
                                 ((buffer-live-p buf)))
                       (buffer-local-value 'default-directory buf)))
               (dispatched (agent-shell-queue-item-dispatched item))
               (completed (agent-shell-queue-item-completed item))
               (runtime (when (and dispatched completed) (- completed dispatched)))
               (instance (if (functionp agent-shell-queue-instance-name)
                            (funcall agent-shell-queue-instance-name)
                          agent-shell-queue-instance-name))
               (entry (json-serialize
                       (list :id (agent-shell-queue-item-id item)
                             :prompt (agent-shell-queue-item-prompt item)
                             :status (symbol-name (agent-shell-queue-item-status item))
                             :background (if (agent-shell-queue-item-background item) t :false)
                             :target buf-name
                             :path (or path :null)
                             :instance (or instance :null)
                             :ran (if dispatched t :false)
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
  "Archive the item at point to the archive file and remove it from the queue.
Archiving must be enabled via `agent-shell-queue-archive-enabled'.
The destination path is provided by `agent-shell-queue-archive-file-function'."
  (interactive)
  (unless agent-shell-queue-archive-enabled
    (user-error "Enable archiving by setting `agent-shell-queue-archive-enabled' to t"))
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue--write-archive (car pair) item)
    (agent-shell-queue-remove id)
    (agent-shell-queue-buffer-refresh)
    (message "agent-shell-queue: archived %s" id)))

(defun agent-shell-queue--load ()
  "Populate the live store items from the durable store.
Delegates to `agent-shell-queue-load-function' when set; otherwise reads
from the file in the current store.  After loading, items with status
`running' are normalized to `active' since a running item from a previous
session cannot be resumed and must be re-dispatched."
  (if agent-shell-queue-load-function
      (condition-case err
          (funcall agent-shell-queue-load-function)
        (error (message "agent-shell-queue: load failed: %s" err)))
    (let* ((store (agent-shell-queue--current-store))
           (file (agent-shell-queue-store-file store)))
      (when (file-exists-p file)
        (condition-case err
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (agent-shell-queue-deserialize
                   store
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))
          (error (message "agent-shell-queue: ignoring unreadable state: %s" err)))))
    ;; Items that were running when the session ended cannot be resumed.
    ;; Normalize them to active so they will be re-dispatched.
    (dolist (pair (agent-shell-queue-store-items agent-shell-queue--store))
      (dolist (item (cdr pair))
        (when (eq (agent-shell-queue-item-status item) 'running)
          (setf (agent-shell-queue-item-status item) 'active)
          (setf (agent-shell-queue-item-dispatched item) nil))))))

(defun agent-shell-queue--ensure-loaded ()
  "Load queue state from disk on first call."
  (unless agent-shell-queue--loaded
    (agent-shell-queue--load)
    (setq agent-shell-queue--loaded t)))

(add-hook 'kill-emacs-hook #'agent-shell-queue--save)

;;; Store predicates

(defun agent-shell-queue--item-id-matches-p (id item)
  "Return non-nil when ITEM's id equals ID."
  (equal (agent-shell-queue-item-id item) id))

(defun agent-shell-queue--bucket-empty-p (pair)
  "Return non-nil when PAIR is a bucket cell whose item list is empty."
  (null (cdr pair)))

(defun agent-shell-queue--wait-timer-id-matches-p (id pair)
  "Return non-nil when PAIR is a wait-timer cell keyed by ID."
  (equal (car pair) id))

;;; Queue operations

(defun agent-shell-queue--item-by-id (id)
  "Return (BUF-NAME . ITEM) for the item with ID, or nil."
  (thread-last
    ;; intput
    (agent-shell-queue-store-items agent-shell-queue--store)
    ;; pipeline handlers
    (seq-mapcat (lambda (pair)
                  (seq-map (lambda (item) (cons (car pair) item)) (cdr pair))))
    (seq-find (lambda (it) (equal (agent-shell-queue-item-id (cdr it)) id)))))

(defun agent-shell-queue--add-item-to-bucket (bucket-name item)
  "Append ITEM to the BUCKET-NAME bucket in the live store items."
  (if-let* ((pair (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
      ;; then
      (setcdr pair (append (cdr pair) (list item)))
    ;; else
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (append (agent-shell-queue-store-items agent-shell-queue--store) (list (list bucket-name item))))))

(defun agent-shell-queue-add (prompt buf &optional background)
  "Add a new active item for PROMPT destined for BUF.  Save and refresh.
When BACKGROUND is non-nil the item is flagged for sub-agent execution.
Registers a `turn-complete' subscription on BUF if one is not already active."
  (agent-shell-queue--ensure-loaded)
  (let ((item (agent-shell-queue--make-item prompt background)))
    (agent-shell-queue--add-item-to-bucket (buffer-name buf) item)
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
  (let ((item (agent-shell-queue--make-item form nil 'emacs)))
    (agent-shell-queue--add-item-to-bucket (buffer-name buf) item)
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
  (when-let* ((pair (assoc id agent-shell-queue--wait-timers)))
    (cancel-timer (cdr pair))
    (setq agent-shell-queue--wait-timers
          (seq-remove (lambda (pair) (agent-shell-queue--wait-timer-id-matches-p id pair))
                      agent-shell-queue--wait-timers)))
  (when-let* ((found (agent-shell-queue--item-by-id id)))
    (message "agent-shell-queue: removed %s [%s]: %s"
             id (car found)
             (truncate-string-to-width
              (agent-shell-queue-item-prompt (cdr found)) 120 nil nil "…")))
  (let ((before-names (seq-map #'car (agent-shell-queue-store-items agent-shell-queue--store))))
    (dolist (it (agent-shell-queue-store-items agent-shell-queue--store))
      (setcdr it (seq-remove (lambda (item) (agent-shell-queue--item-id-matches-p id item)) (cdr it))))
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (seq-remove #'agent-shell-queue--bucket-empty-p (agent-shell-queue-store-items agent-shell-queue--store)))
    (seq-do #'agent-shell-queue--drop-subscription
            (seq-remove (lambda (it) (assoc it (agent-shell-queue-store-items agent-shell-queue--store)))
                        before-names)))
  (agent-shell-queue--save))

(defun agent-shell-queue--confirm-remove (item)
  "Prompt the user to confirm removing ITEM.
Returns t to proceed, nil to skip.  When user answers \\='a\\=', sets
`agent-shell-queue--remove-all-confirmed' so future calls return t immediately."
  (or agent-shell-queue--remove-all-confirmed
      (let ((answer (read-char-choice
                     (format "Remove [%s]? (y)es (n)o (a)ll: "
                             (truncate-string-to-width
                              (agent-shell-queue-item-prompt item) 60 nil nil "…"))
                     '(?y ?n ?a ?Y ?N ?A))))
        (pcase answer
          ((or ?y ?Y) t)
          ((or ?a ?A) (setq agent-shell-queue--remove-all-confirmed t) t)
          (_ nil)))))

(defun agent-shell-queue-defer (id)
  "Toggle status of item ID between `active' and `deferred'.  Save."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (setf (agent-shell-queue-item-status item)
          (if (eq (agent-shell-queue-item-status item) 'active) 'deferred 'active))
    (agent-shell-queue--save)))

(defun agent-shell-queue-edit (id new-prompt)
  "Replace the prompt of item ID with NEW-PROMPT.  Save."
  ;; does it make sense to make a "agent-shell-queue--with-save" macro and then inline all of this?
  (when-let* ((pair (agent-shell-queue--item-by-id id)))
    (setf (agent-shell-queue-item-prompt (cdr pair)) new-prompt)
    (agent-shell-queue--save)))

(defun agent-shell-queue-set-background-task (id flag)
  "Set the background flag of item ID to FLAG.  Save."
  (when-let* ((pair (agent-shell-queue--item-by-id id)))
    (setf (agent-shell-queue-item-background (cdr pair)) flag)
    (agent-shell-queue--save)))

(defun agent-shell-queue--move (id delta)
  "Shift item ID by DELTA positions within its buffer's list."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (cell (assoc (car pair) (agent-shell-queue-store-items agent-shell-queue--store)))
              (items (cdr cell))
              (idx (cl-position id items :key #'agent-shell-queue-item-id :test #'equal))
              (new-idx (+ idx delta))
              (_ (>= new-idx 0))
              (_ (< new-idx (length items))))
    (let ((new-items (copy-sequence items)))
      (cl-rotatef (nth idx new-items) (nth new-idx new-items))
      (setcdr cell new-items)
      (agent-shell-queue--save))))

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
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (old-name (car pair))
              (item (cdr pair))
              (_ (not (equal old-name new-buf-name))))
    (let ((old-cell (assoc old-name (agent-shell-queue-store-items agent-shell-queue--store))))
      (setcdr old-cell
              (seq-remove (lambda (item) (agent-shell-queue--item-id-matches-p id item)) (cdr old-cell))))

    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (seq-remove #'agent-shell-queue--bucket-empty-p (agent-shell-queue-store-items agent-shell-queue--store)))

    (unless (assoc old-name (agent-shell-queue-store-items agent-shell-queue--store))
      (agent-shell-queue--drop-subscription old-name))

    (if-let* ((new-cell (assoc new-buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
        (setcdr new-cell (append (cdr new-cell) (list item)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (append (agent-shell-queue-store-items agent-shell-queue--store) (list (list new-buf-name item)))))

    (when-let* ((_ (not (equal new-buf-name agent-shell-queue--unassigned-key)))
                (new-buf (get-buffer new-buf-name)))
      (agent-shell-queue--ensure-subscription new-buf))

    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue--redirect-dead-target (id buf-name)
  "When BUF-NAME is no longer live, offer to assign item ID to another shell.
Returns non-nil if the item was successfully assigned and sent."
  (if-let* ((available (agent-shell-buffers)))
      (when-let* ((_ (yes-or-no-p (format "Buffer %s is gone. Assign to another shell? " buf-name)))
                  (new-name (completing-read (format "Assign '%s' to: " buf-name)
                                             (seq-map #'buffer-name available) nil t))
                  (_ (not (string-empty-p new-name))))
        (agent-shell-queue--assign-item id new-name)
        (agent-shell-queue-send-item id)
        t)
    (user-error "Buffer %s is gone and no agent-shell buffers are live" buf-name)))

(defun agent-shell-queue--handle-stale-item (id buf-name err)
  "Pause BUF-NAME and defer item ID after a struct access error ERR.
Called when dispatching item ID raises an error, which indicates the item
was built against an older struct definition before a code reload."
  (cl-pushnew id agent-shell-queue--stale-item-ids :test #'equal)
  (when-let* ((pair (agent-shell-queue--item-by-id id)))
    (condition-case nil
        (setf (agent-shell-queue-item-status (cdr pair)) 'deferred)
      (error nil)))

  (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)

  (message "agent-shell-queue: item %s in %s appears stale after code reload; deferred and queue paused (%s)"
           id buf-name err)

  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue--complete-item (item buf-name)
  "Mark ITEM in BUF-NAME as done and trigger the next dispatch cycle.
Records completion time, appends to the done log, persists state,
refreshes the queue buffer, fires the empty-queue alert if warranted,
and dispatches the next item for BUF-NAME if the buffer is still live."
  (setf (agent-shell-queue-item-completed item) (float-time))
  (setf (agent-shell-queue-item-status item) 'done)
  (agent-shell-queue--append-done-log buf-name item)
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer)
  (agent-shell-queue--alert-if-empty)
  (when-let* ((buf (get-buffer buf-name)))
    (agent-shell-queue--send-next-for-buffer buf)))

(defun agent-shell-queue--wait-timer-fire (id)
  "Handle expiry of the wait timer for item ID."
  (setq agent-shell-queue--wait-timers
        (seq-remove (lambda (pair) (agent-shell-queue--wait-timer-id-matches-p id pair))
                    agent-shell-queue--wait-timers))
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (buf-name (car pair)))
    (agent-shell-queue--complete-item item buf-name)))

(defun agent-shell-queue-send-item (id)
  "Send the item with ID to its target buffer, marking it as running.
Items flagged as background are wrapped with `agent-shell-queue-background-prefix'.
The item transitions to done when the buffer's turn-complete event fires.
Running and done items are not persisted across sessions."
  (when-let* ((pair (agent-shell-queue--item-by-id id)))
    (let* ((buf-name (car pair))
           (item (cdr pair))
           (buf (get-buffer buf-name)))
      (cond
       ((not (buffer-live-p buf))
        (agent-shell-queue--redirect-dead-target id buf-name))
       ((agent-shell-queue--session-mode-blocked-p buf)
        (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
        (message "agent-shell-queue: dispatch blocked — session %s is in mode %s"
                 buf-name
                 (map-nested-elt (buffer-local-value 'agent-shell--state buf)
                                 '(:session :mode-id))))
       (t
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
                 (agent-shell-queue--complete-item item buf-name))
                ((or 'pause 'compact)
                 (cl-pushnew (cons buf-name id) agent-shell-queue--compact-running :test #'equal)
                 (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
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
                        (wait-timer (run-with-timer delay nil
                                                    #'agent-shell-queue--wait-timer-fire
                                                    id)))
                   (push (cons id wait-timer) agent-shell-queue--wait-timers)
                   (agent-shell-queue--save)
                   (agent-shell-queue--refresh-buffer)))
                (_
                 (alert (truncate-string-to-width (agent-shell-queue-item-prompt item) 80 nil nil "…")
                        :title (format "Queue → %s" buf-name)
                        :category 'agent-shell-queue
                        :severity 'low)
                 (agent-shell-insert
                  :text (if (agent-shell-queue-item-background item)
                            (concat agent-shell-queue-background-prefix
                                    (agent-shell-queue-item-prompt item))
                          (agent-shell-queue-item-prompt item))
                  :submit t :no-focus t :shell-buffer buf)
                 ;; Record after insert so start-pos is past the submitted "Claude> [prompt]" line.
                 (push (cons id (with-current-buffer buf (point-max)))
                       agent-shell-queue--response-start-positions))))
          (error
           (agent-shell-queue--handle-stale-item id buf-name err))))))))

(defun agent-shell-queue--capture-response (id buf-name)
  "Capture the visible response text for item ID from BUF-NAME and clean up tracking.
Collects every plain-text segment (no `agent-shell-ui-state') between the end
of the echoed prompt (first field transition after START-POS) and the shell-maker
end-of-output boundary (field=boundary).  This mirrors what the user sees when
all collapsed blocks are folded: only the prose between them."
  (let ((pos-pair (assoc id agent-shell-queue--response-start-positions)))
    (setq agent-shell-queue--response-start-positions
          (seq-remove (lambda (it) (equal (car it) id)) agent-shell-queue--response-start-positions))
    (when-let* (pos-pair
                (start-pos (cdr pos-pair))
                (sbuf (get-buffer buf-name))
                (pair (agent-shell-queue--item-by-id id)))
      (let ((text (with-current-buffer sbuf
                    (save-excursion
                      (let* (;; Turn boundary: shell-maker end-of-output marker.
                             (end-marker (progn
                                           (goto-char (point-max))
                                           (text-property-search-backward
                                            'field 'boundary t)))
                             (end-pos (if (and end-marker
                                               (> (prop-match-beginning end-marker) start-pos))
                                          (prop-match-beginning end-marker)
                                        (point-max)))
                             ;; The echoed prompt has field=input; model response
                             ;; starts where field transitions away from input.
                             (response-start
                              (or (next-single-property-change start-pos 'field nil end-pos)
                                  start-pos))
                             ;; Walk forward collecting plain-text segments (no
                             ;; agent-shell-ui-state), skipping collapsed blocks.
                             (pos response-start)
                             (segments nil))
                        (while (< pos end-pos)
                          (let ((state (get-text-property pos 'agent-shell-ui-state)))
                            (if (assq :collapsed state)
                                ;; Collapsible block (thinking, tool call, etc.) — skip.
                                (setq pos (or (next-single-property-change
                                               pos 'agent-shell-ui-state nil end-pos)
                                              end-pos))
                              ;; Plain text or no state — collect up to the next boundary.
                              (let* ((seg-end (or (next-single-property-change
                                                   pos 'agent-shell-ui-state nil end-pos)
                                                  end-pos))
                                     (seg (string-trim
                                           (buffer-substring-no-properties pos seg-end))))
                                (when (not (string-empty-p seg))
                                  (push seg segments))
                                (setq pos seg-end)))))
                        (when segments
                          (string-join (nreverse segments) "\n\n")))))))
        (when (and text (not (string-empty-p text)))
          (setf (agent-shell-queue-item-response (cdr pair))
                (if (> (length text) 8192)
                    (concat (substring text 0 8192) "\n\n…[truncated]")
                  text)))))))

(defun agent-shell-queue--mark-running-done (buf-name)
  "Mark any running items for BUF-NAME as done, recording completion time.
If any item in the bucket is already aborted or incomplete, pauses the session
queue instead of continuing — the queue must be manually resumed.
Only fires the empty-queue alert when at least one item was actually marked done."
  (let (marked halted)
    (dolist (item (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
      (cond
       ((eq (agent-shell-queue-item-status item) 'running)
        (unless (memq (agent-shell-queue-item-kind item) '(pause compact context))
          (agent-shell-queue--capture-response
           (agent-shell-queue-item-id item) buf-name))
        (setf (agent-shell-queue-item-completed item) (float-time))
        (setf (agent-shell-queue-item-status item) 'done)
        (agent-shell-queue--append-done-log buf-name item)
        (setq marked t))
       ((memq (agent-shell-queue-item-status item) '(aborted incomplete))
        (setq halted t))))
    (when halted
      (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (when marked
      (agent-shell-queue--alert-if-empty))))

(defun agent-shell-queue--mark-running-incomplete (buf-name)
  "Mark any running items for BUF-NAME as incomplete and pause the session queue.
Called when the shell buffer exits or is killed while a task was in flight.
The queue must be manually resumed via `agent-shell-queue-session-resume'."
  (when (thread-last (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store)))
                     (seq-map (lambda (it)
                                (when (eq (agent-shell-queue-item-status it) 'running)
                                  (setf (agent-shell-queue-item-completed it) (float-time))
                                  (setf (agent-shell-queue-item-status it) 'incomplete)
                                  t)))
                     (seq-filter #'identity))
    (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal))
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue--session-mode-blocked-p (buf)
  "Return non-nil if BUF's session mode is in `agent-shell-queue-blocked-session-modes'."
  (when-let* ((_ (buffer-live-p buf))
              (mode-id (map-nested-elt (buffer-local-value 'agent-shell--state buf)
                                       '(:session :mode-id))))
    (member mode-id agent-shell-queue-blocked-session-modes)))

;;; Auto-send — per-buffer turn-complete subscriptions (primary) + idle timer (backup)

(defun agent-shell-queue--alert-if-empty ()
  "Send a persistent alert when no active or running items remain in any queue."
  (unless (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
             (seq-mapcat #'cdr)
             (seq-some (lambda (item)
                         (memq (agent-shell-queue-item-status item) '(active running)))))
    (alert "All queued tasks complete"
           :title "Agent Queue"
           :category 'agent-shell-queue
           :severity 'normal
           :persistent t)))

(defun agent-shell-queue--next-dispatchable-item (items)
  "Return the first item in ITEMS eligible for dispatch, or nil."
  (seq-find (lambda (it)
              (and (eq (agent-shell-queue-item-status it) 'active)
                   (not (member (agent-shell-queue-item-id it)
                                (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)))))
            items))

(defun agent-shell-queue--dispatch-if-ready (buf)
  "Send the next dispatchable item for BUF if all conditions are met."
  (when (and (buffer-live-p buf)
             (not (agent-shell-queue-queue-paused agent-shell-queue--queue))
             (not (member (buffer-name buf) (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
    (with-current-buffer buf
      (when-let* ((_ (not (shell-maker-busy)))
                  (buf-name (buffer-name))
                  (item (agent-shell-queue--next-dispatchable-item
                         (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))))
        (agent-shell-queue-send-item (agent-shell-queue-item-id item))))))

(defun agent-shell-queue--send-next-for-buffer (buf)
  "Attempt to send the first active queue item for BUF.
Deferred via a zero-delay timer to let the current event complete before
submitting the next prompt.  Deferred items are skipped.
No-op when the queue is globally paused."
  (run-with-timer 0 nil #'agent-shell-queue--dispatch-if-ready buf))

(defun agent-shell-queue--drop-subscription (buf-name)
  "Unsubscribe from `turn-complete' events for BUF-NAME and remove from registry.
Safe to call with a dead buffer — the subscription token is merely discarded."
  (when-let* ((pair (assoc buf-name agent-shell-queue--subscriptions))
              (buf (get-buffer buf-name))
              (_ (buffer-live-p buf))
              (_ (with-current-buffer buf (derived-mode-p 'agent-shell-mode))))
    (ignore-errors
      (with-current-buffer buf
        (agent-shell-unsubscribe :subscription (cdr pair)))))

  (setq agent-shell-queue--subscriptions
        (seq-remove (lambda (it) (equal (car it) buf-name)) agent-shell-queue--subscriptions)))

(defun agent-shell-queue--on-turn-complete (buf buf-name _event)
  "Handle a turn-complete event for BUF (named BUF-NAME)."
  (agent-shell-queue--mark-running-done buf-name)
  (agent-shell-queue--send-next-for-buffer buf))

(defun agent-shell-queue--on-clean-up (buf-name _event)
  "Handle a clean-up event for BUF-NAME."
  (agent-shell-queue--mark-running-incomplete buf-name)
  (setq agent-shell-queue--subscriptions
        (seq-remove (lambda (it) (equal (car it) buf-name))
                    agent-shell-queue--subscriptions)))

(defun agent-shell-queue--ensure-subscription (buf)
  "Subscribe to `turn-complete' events on BUF if no subscription exists yet.
Also subscribes to `clean-up' so the registry is updated when BUF is killed."
  (when-let* ((buf-name (buffer-name buf))
              (_ (not (assoc buf-name agent-shell-queue--subscriptions))))
    (push (cons buf-name
                (agent-shell-subscribe-to
                 :shell-buffer buf
                 :event 'turn-complete
                 :on-event (lambda (event)
                             (agent-shell-queue--on-turn-complete buf buf-name event))))
          agent-shell-queue--subscriptions)
    (agent-shell-subscribe-to
     :shell-buffer buf
     :event 'clean-up
     :on-event (lambda (event)
                 (agent-shell-queue--on-clean-up buf-name event)))))

(defun agent-shell-queue--auto-send ()
  "Backup scan: send the first active item for each idle agent-shell bucket.
Runs infrequently; deferred items are always skipped.
Primary draining is handled by per-buffer `turn-complete' subscriptions.
No-op when the queue is globally paused."
  (when (and agent-shell-queue--loaded (agent-shell-queue-store-items agent-shell-queue--store)
             (not (agent-shell-queue-queue-paused agent-shell-queue--queue)))
    (dolist (it (copy-sequence (agent-shell-queue-store-items agent-shell-queue--store)))
      (when-let* ((buf-name (car it))
                  (buf (get-buffer buf-name))
                  (_ (buffer-live-p buf))
                  (_ (not (member buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
                  (_ (not (with-current-buffer buf (shell-maker-busy))))
                  (item (agent-shell-queue--next-dispatchable-item
                         (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))))
        (agent-shell-queue-send-item (agent-shell-queue-item-id item))))))

(defun agent-shell-queue--setup-hooks ()
  "Start the backup idle-scan timer.
Per-buffer draining is registered lazily via `agent-shell-queue--ensure-subscription'
when items are first added for a given buffer."
  (setq agent-shell-queue--idle-timer
        (or agent-shell-queue--idle-timer
            (run-with-idle-timer agent-shell-queue-idle-delay t #'agent-shell-queue--auto-send))))

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
  (setf (agent-shell-queue-queue-paused agent-shell-queue--queue) t)
  (agent-shell-queue--save)
  (when agent-shell-queue--idle-timer
    (cancel-timer agent-shell-queue--idle-timer)
    (setq agent-shell-queue--idle-timer nil))

  (seq-do (lambda (pair) (cancel-timer (cdr pair))) agent-shell-queue--wait-timers)
  (setq agent-shell-queue--wait-timers nil)
  (dolist (it (copy-sequence agent-shell-queue--subscriptions))
    (agent-shell-queue--drop-subscription (car it)))
  (run-hooks 'agent-shell-queue-before-reload-hook)
  (setf (agent-shell-queue-store-items agent-shell-queue--store) nil)
  (setq agent-shell-queue--loaded nil
        agent-shell-queue--subscriptions nil)
  (if-let* ((lib (locate-library "agent-shell-queue"))
             (src (if (string-suffix-p ".elc" lib)
                      (concat (file-name-sans-extension lib) ".el")
                    lib))
             (_ (file-exists-p src)))
      (load-file src)
    (error "agent-shell-queue-reload: cannot locate source file"))
  (agent-shell-queue--load)
  (setq agent-shell-queue--loaded t)
  (dolist (it (agent-shell-queue-store-items agent-shell-queue--store))
    (when-let* ((buf (get-buffer (car it)))
                (_ (buffer-live-p buf))
                (_ (with-current-buffer buf (derived-mode-p 'agent-shell-mode)))
                (_ (seq-some (lambda (item)
                               (memq (agent-shell-queue-item-status item) '(active running)))
                             (cdr it))))
      (agent-shell-queue--ensure-subscription buf)))

  (run-hooks 'agent-shell-queue-after-reload-hook)
  (agent-shell-queue--refresh-buffer)

  (when-let* ((buf (get-buffer "*agent-shell-queue*")))
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
  (let ((candidates (thread-last
		      (agent-shell-queue-store-items agent-shell-queue--store)
                      (seq-mapcat
                       (lambda (pair)
                         (thread-last (cdr pair)
                                      (seq-map (lambda (it)
                                                 (condition-case _
                                                     (ignore (agent-shell-queue-item-id it)
                                                             (agent-shell-queue-item-prompt it)
                                                             (agent-shell-queue-item-status it))
                                                   (error (cons (car pair) it)))))
                                      (seq-filter #'identity))))))
        removed
        (accept-all current-prefix-arg))
    (cond
     ((null candidates)
      (message "agent-shell-queue: no unparsable items found"))
     (t
      (dolist (it candidates)
        (let ((buf-name (car it))
              (item (cdr it)))
          (message "agent-shell-queue: unparsable item in %s: %S" buf-name item)
          (when (or accept-all (not (called-interactively-p 'any))
                    (let ((ch (read-char-choice
                               (format "Remove from %s? (y)es (n)o (a)ll: " buf-name)
                               '(?y ?n ?a))))
                      (cond ((eq ch ?a) (setq accept-all t))
                            ((eq ch ?n) nil)
                            (t t))))
            (when-let* ((cell (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
              (setcdr cell (seq-remove (lambda (it) (eq it item)) (cdr cell))))
            (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
            (push it removed))))
      (cond
       ((null removed)
        (message "agent-shell-queue: no items removed"))
       (t
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (seq-remove #'agent-shell-queue--bucket-empty-p (agent-shell-queue-store-items agent-shell-queue--store)))
        (agent-shell-queue--save)
        (agent-shell-queue--refresh-buffer)
        (message "agent-shell-queue: removed %d unparsable item(s); affected queues paused"
                 (length removed))))))))

(defun agent-shell-queue--revert-disk-view (file _ignore-auto _noconfirm)
  "Re-read FILE into the disk-state view buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents file)
    (goto-char (point-min))))

;;;###autoload
(defun agent-shell-queue-show-disk-state ()
  "Display the on-disk queue state file in a read-only popup buffer."
  (interactive)
  (if-let* ((file (agent-shell-queue--state-file))
             (_ (file-exists-p file))
             (buf (get-buffer-create "*agent-shell-queue-disk*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents file)
          (goto-char (point-min)))
        (setq buffer-read-only t)
        (setq-local revert-buffer-function
                    (lambda (ignore-auto noconfirm)
                      (agent-shell-queue--revert-disk-view file ignore-auto noconfirm)))
        (set-visited-file-name nil t)
        (rename-buffer "*agent-shell-queue-disk*" t)
        (pcase (file-name-extension file)
          ("el" (when (fboundp 'emacs-lisp-mode) (emacs-lisp-mode)))
          ("json" (when (fboundp 'json-mode) (json-mode)))
          ((or "yaml" "yml") (when (fboundp 'yaml-mode) (yaml-mode))))
        (read-only-mode 1)
        (display-buffer buf '(display-buffer-below-selected (window-height . 0.4))))
    (user-error "Queue state file does not exist: %s" file)))

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
         (editing (member (agent-shell-queue-item-id item) (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)))
         (blocked (and buf-name (member buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
         (unassigned (equal buf-name agent-shell-queue--unassigned-key))
         (done (eq status 'done))
         (running (eq status 'running))
         (aborted (eq status 'aborted))
         (status-str
          (cond ((eq status 'invalid) "invalid")
                ((eq status 'pending-fork) "pending-fork")
                ((eq kind 'context) "context")
                ((eq kind 'emacs) (if done "emacs.done" (if running "emacs.running" "emacs")))
                ((eq kind 'wait) (if done "wait.done" (if running "wait.running" "wait")))
                ((memq kind '(pause compact))
                 (cond (done "done")
                       (running "running.blocked")
                       (t (symbol-name kind))))
                ((eq status 'incomplete) "incomplete")
                (done "done")
                (aborted "aborted")
                (running (if bg "running.active.bg" "running.active"))
                (editing "editing")
                ((and (eq status 'active) blocked) "paused<shell>")
                ((and (eq status 'active) (agent-shell-queue-queue-paused agent-shell-queue--queue)) "paused<all>")
                ((and (eq status 'deferred) bg) "held.bg")
                ((eq status 'deferred) "held")
                ((eq status 'draft) "draft")
                (bg "scheduled.bg")
                (t "scheduled")))
         (face
          (cond ((eq status 'invalid) 'font-lock-warning-face)
                ((eq status 'pending-fork) 'agent-shell-queue-pending-fork-face)
                ((eq status 'incomplete) 'font-lock-warning-face)
                (done 'shadow)
                (aborted 'font-lock-warning-face)
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
  (when-let* ((buf (get-buffer "*agent-shell-queue*"))
              (_ (buffer-live-p buf))
              (_ (with-current-buffer buf (derived-mode-p 'agent-shell-queue-mode))))
    (with-current-buffer buf
      (agent-shell-queue-buffer-refresh))))

;;; Scope / narrowing

(defvar-local agent-shell-queue--display-scope nil
  "Current display scope for the queue buffer.
nil means global (show all items).
`(directory . DIR)' means items for shell buffers under DIR.
`(buffer . BUF-NAME)' means items for exactly that buffer.")

;;; Display configuration

(defvar agent-shell-queue-show-buffer-column t
  "Show the Buffer column in the queue buffer.
Auto-suppressed when only one bucket is visible; that bucket's name
appears in the tab-line instead.")

(defvar agent-shell-queue-show-ordinal-column t
  "Show the ordinal (#) column in the queue buffer.")

(defvar agent-shell-queue-show-age-column t
  "Show the Age column in the queue buffer.")

(defvar agent-shell-queue-multiline-format nil
  "Display prompt on a second line with a separator between items.
When non-nil, `<down>' and `<up>' move by item rather than by line.")

;; Persist display preferences and queue state across sessions.
(with-eval-after-load 'savehist
  (dolist (it '(agent-shell-queue--queue
                agent-shell-queue-show-buffer-column
                agent-shell-queue-show-ordinal-column
                agent-shell-queue-show-age-column
                agent-shell-queue-multiline-format))
    (cl-pushnew it savehist-additional-variables)))

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
          (when-let* ((buf (get-buffer buf-name)))
            (string-prefix-p (expand-file-name dir)
                             (expand-file-name
                              (buffer-local-value 'default-directory buf))))))))

(defun agent-shell-queue--scope-candidates ()
  "Return an alist of (LABEL . SCOPE) covering global, directories, and buffers.
Directories are derived from live shell buffers' default-directory.
Temporary buffers (no live session) are excluded from directory scopes."
  (let* ((assigned (seq-remove (lambda (it)
                                 (equal (car it) agent-shell-queue--unassigned-key))
                               (agent-shell-queue-store-items agent-shell-queue--store)))
         (buf-entries (seq-map (lambda (it) (cons (car it) (cons 'buffer (car it)))) assigned))
         (dirs (thread-last assigned
                 (seq-filter (lambda (it) (buffer-live-p (get-buffer (car it)))))
                 (seq-map (lambda (it)
                            (expand-file-name
                             (buffer-local-value 'default-directory (get-buffer (car it))))))
                 (seq-uniq))))
    (append
     (list (cons "global (all)" nil))
     (seq-map (lambda (it) (cons (abbreviate-file-name it) (cons 'directory it)))
              (sort dirs #'string<))
     buf-entries)))

(defun agent-shell-queue--active-item-count (items)
  "Return the count of ITEMS whose status is not `done'."
  (seq-count (lambda (item)
               (not (eq (agent-shell-queue-item-status item) 'done)))
             items))

;;;###autoload
(defun agent-shell-queue-set-scope ()
  "Narrow the queue buffer view: choose global, a directory, or a specific buffer."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((candidates (agent-shell-queue--scope-candidates))
         (table (seq-map
                 (lambda (cand)
                   (let* ((scope (cdr cand))
                          (count (apply #'+
                                        (seq-map (lambda (it)
                                                   (if (agent-shell-queue--scope-matches-p (car it) scope)
                                                       (agent-shell-queue--active-item-count (cdr it))
                                                     0))
                                                 (agent-shell-queue-store-items agent-shell-queue--store)))))
                     (cons (car cand) (format "%d item(s)" count))))
                 candidates)))
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
         (out-name (format "*agent-shell-queue-export: %s*"
                           (agent-shell-queue--scope-label scope)))
         (buckets nil)
         (multi-p (> (apply #'+
                            (seq-map (lambda (it) (length (cdr it)))
                                     (agent-shell-queue-store-items agent-shell-queue--store)))
                     1)))
    (dolist (it (agent-shell-queue-store-items agent-shell-queue--store))
      (when-let* ((_ (agent-shell-queue--scope-matches-p (car it) scope)))
        (let* ((items (if multi-p
                          (seq-remove
                           (lambda (item)
                             (and (eq (agent-shell-queue-item-status item) 'done)
                                  (memq (agent-shell-queue-item-kind item)
                                        '(pause compact context))))
                           (cdr it))
                        (cdr it)))
               (h (make-hash-table :test #'equal)))
          (map-put! h "buffer" (car it))
          (map-put! h "items" (vconcat (seq-map #'agent-shell-queue--item-to-yaml-edit items)))
          (push h buckets))))
    (with-current-buffer (get-buffer-create out-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if buckets (yaml-encode (vconcat (nreverse buckets))) ""))
        (goto-char (point-min))

        (when (fboundp 'yaml-mode)
	  (yaml-mode)))
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
    ;; Pause / schedule (suspend item from auto-dispatch without removing)
    (define-key m (kbd "d")        #'agent-shell-queue-buffer-pause)
    (define-key m (kbd "u")        #'agent-shell-queue-buffer-schedule)
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
    (define-key m (kbd "<down>")   #'agent-shell-queue-next-item)
    (define-key m (kbd "<up>")     #'agent-shell-queue-prev-item)
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
    (define-key m (kbd "o")        #'agent-shell-queue-buffer-open-shell)
    (define-key m (kbd "X")        #'agent-shell-queue-buffer-abort)
    (define-key m (kbd "V")        #'agent-shell-queue-select-columns)
    (define-key m (kbd "m")        #'agent-shell-queue-menu)
    (define-key m (kbd "?")        #'describe-bindings)
    (define-key m (kbd "q")        #'quit-window)
    m)
  "Keymap for `agent-shell-queue-mode'.")

(defvar-local agent-shell-queue--last-column-structure nil
  "Column structure key from the last `tabulated-list-init-header' call.
A list of (show-buffer-p show-ordinal-p show-age-p) used to avoid
reinitializing headers on pure content refreshes.")

(define-derived-mode agent-shell-queue-mode tabulated-list-mode "Queue"
  "Major mode for reviewing and managing the agent-shell prompt queue."
  (setq tabulated-list-format
        (agent-shell-queue--column-format t (agent-shell-queue--prompt-width t)))
  (setq agent-shell-queue--last-column-structure
        (list t agent-shell-queue-show-ordinal-column agent-shell-queue-show-age-column))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tab-line-mode 1)
  (setq tab-line-format
        '(:eval (let* ((state (agent-shell-queue--activity-state))
                       (sessions (length (agent-shell-buffers)))
                       (scope agent-shell-queue--display-scope)
                       (visible-items
                        (seq-filter
                         (lambda (pair)
                           (agent-shell-queue--scope-matches-p (car pair) scope))
                         (agent-shell-queue-store-items agent-shell-queue--store)))
                       (depth (apply #'+
                                     (seq-map (lambda (it)
                                                (agent-shell-queue--active-item-count (cdr it)))
                                              visible-items)))
                       (single-bucket-name
                        (when (= (length visible-items) 1)
                          (caar visible-items)))
                       (bucket-display
                        (when single-bucket-name
                          (format "  |  Buffer: %s" single-bucket-name)))
                       (scope-display
                        (when (and (null single-bucket-name) scope)
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
                        (when-let* ((intercepting
                                     (seq-filter
                                      (lambda (b)
                                        (buffer-local-value
                                         'agent-shell-queue-intercept-mode b))
                                      (agent-shell-buffers))))
                          (format "  |  INTERCEPT: %s"
                                  (mapconcat #'buffer-name intercepting ", ")))))
                  (format " Queue: %s  |  Sessions: %d  |  Depth: %d%s%s%s  |  Flushed: %s%s"
                          state sessions depth
                          (or bucket-display "")
                          (or scope-display "")
                          (or intercept-display "")
                          flush-display (or next-display ""))))))

(defun agent-shell-queue--activity-state ()
  "Return a propertized string describing the queue's current activity level."
  (cond
   ((agent-shell-queue-queue-paused agent-shell-queue--queue) (propertize "PAUSED" 'face 'warning))
   ((thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                 (seq-mapcat #'cdr)
                 (seq-some (lambda (it) (eq (agent-shell-queue-item-status it) 'running))))
    (propertize "running" 'face 'success))
   ((thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                 (seq-mapcat #'cdr)
                 (seq-some (lambda (it) (eq (agent-shell-queue-item-status it) 'active))))
    (propertize "waiting" 'face 'font-lock-comment-face))
   (t
    (propertize "idle" 'face 'shadow))))


(defconst agent-shell-queue--status-column-width
  (max 6 (apply #'max
                (seq-map #'length
                         '("invalid" "context" "emacs.done" "emacs.running" "emacs"
                        "wait.done" "wait.running" "wait" "done" "running.blocked"
                        "pause" "compact" "aborted" "running.active.bg" "running.active"
                        "editing" "paused<shell>" "paused<all>" "held.bg" "held"
                          "draft" "scheduled.bg" "scheduled" "incomplete"))))
  "Width of the Status column: max(6, length of longest status display string).")

(defun agent-shell-queue--column-format (show-buffer-p pw)
  "Build the `tabulated-list-format' vector for current display settings.
SHOW-BUFFER-P controls whether the Buffer column is included.
PW is the width allocated to the Prompt column."
  (let (cols)
    (push (list "Status" agent-shell-queue--status-column-width t) cols)
    (when show-buffer-p
      (push (list "Buffer" 17 t) cols))
    (when agent-shell-queue-show-ordinal-column
      (push (list "#" 4 nil) cols))
    (when agent-shell-queue-show-age-column
      (push (list "Age" 6 t) cols))
    (push (list "Prompt" pw nil) cols)
    (apply #'vector (nreverse cols))))

(defun agent-shell-queue--prompt-width (show-buffer-p)
  "Compute available width for the Prompt column.
SHOW-BUFFER-P indicates whether the Buffer column is included."
  (max 20 (- (window-width)
             (+ agent-shell-queue--status-column-width
                (if show-buffer-p 17 0)
                (if agent-shell-queue-show-ordinal-column 4 0)
                (if agent-shell-queue-show-age-column 6 0)
                ;; tabulated-list adds one space between columns
                (+ 1
                   (if show-buffer-p 1 0)
                   (if agent-shell-queue-show-ordinal-column 1 0)
                   (if agent-shell-queue-show-age-column 1 0))))))

(defun agent-shell-queue-buffer-refresh ()
  "Rebuild the tabulated list from current queue state."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((scope agent-shell-queue--display-scope)
         (visible-items (seq-filter (lambda (it)
                                      (agent-shell-queue--scope-matches-p (car it) scope))
                                    (agent-shell-queue-store-items agent-shell-queue--store)))
         (unassigned-pair (assoc agent-shell-queue--unassigned-key visible-items))
         (assigned-pairs (seq-remove (lambda (it) (equal (car it) agent-shell-queue--unassigned-key))
                                     visible-items))
         (ordered (if unassigned-pair
                      (append assigned-pairs (list unassigned-pair))
                    assigned-pairs))
         (single-bucket-p (= (length ordered) 1))
         ;; When only one bucket is visible, its name goes in the tab-line
         ;; rather than a redundant column on every row.
         (show-buffer-p (and agent-shell-queue-show-buffer-column (not single-bucket-p)))
         (column-structure (list show-buffer-p
                                 agent-shell-queue-show-ordinal-column
                                 agent-shell-queue-show-age-column))
         (next-id-map (seq-map (lambda (it)
                                 (cons (car it)
                                       (when-let* ((next (agent-shell-queue--next-dispatchable-item
                                                          (cdr it))))
                                         (agent-shell-queue-item-id next))))
                               (agent-shell-queue-store-items agent-shell-queue--store)))
         (pw (agent-shell-queue--prompt-width show-buffer-p)))
    (unless (equal column-structure agent-shell-queue--last-column-structure)
      (setq agent-shell-queue--last-column-structure column-structure)
      (setq tabulated-list-format (agent-shell-queue--column-format show-buffer-p pw))
      (tabulated-list-init-header))
    (setq tabulated-list-entries
          (thread-last ordered
            (seq-mapcat
             (lambda (pair)
               (seq-map
                (lambda (item)
                  (let* ((id (agent-shell-queue-item-id item))
                         (next-p (equal id (cdr (assoc (car pair) next-id-map))))
                         (display (agent-shell-queue--item-display item (car pair) next-p))
                         (status-str (car display))
                         (face (cdr display))
                         (cell (lambda (str) (if face (propertize str 'face face) str)))
                         (idx (cl-position id
                                           (cdr (assoc (car pair) (agent-shell-queue-store-items agent-shell-queue--store)))
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
                                                "(unassigned)" (car pair))))
                         (row (let (cols)
                                (push (funcall cell status-str) cols)
                                (when show-buffer-p (push buf-cell cols))
                                (when agent-shell-queue-show-ordinal-column
                                  (push (funcall cell (if (> ordinal 0)
                                                          (number-to-string ordinal) ""))
                                        cols))
                                (when agent-shell-queue-show-age-column
                                  (push (funcall cell age-str) cols))
                                (push (funcall cell (truncate-string-to-width
                                                     first-line pw nil nil "…"))
                                      cols)
                                (apply #'vector (nreverse cols)))))
                    (list id row)))
                (cdr pair))))))
    (tabulated-list-print t)
    (when agent-shell-queue-multiline-format
      (agent-shell-queue--expand-multiline))))

(defun agent-shell-queue--expand-multiline ()
  "Expand each tabulated entry with a second prompt line and a separator.
Must be called immediately after `tabulated-list-print'."
  (let* ((inhibit-read-only t)
         (sep-face 'shadow)
         (sep-char ?─)
         ;; Collect (id . line-start-pos) in reverse buffer order so that
         ;; inserting extra lines below each entry does not shift earlier positions.
         (positions nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((id (tabulated-list-get-id)))
          (push (cons id (line-beginning-position)) positions))
        (forward-line 1)))
    ;; positions is already in reverse order due to push; process top-to-bottom
    ;; would corrupt offsets, so keep reverse (last entry first).
    (dolist (it positions)
      (when-let* ((id (car it))
                  (line-start (cdr it))
                  (item (cdr (agent-shell-queue--item-by-id id)))
                  (prompt (agent-shell-queue-item-prompt item)))
        (let ((face (cdr (agent-shell-queue--item-display item nil nil))))
          (save-excursion
            (goto-char line-start)
            (end-of-line)
            (let ((insert-start (point))
                  (sep (propertize
                        (make-string (max 4 (1- (window-width))) sep-char)
                        'face sep-face)))
              (insert "\n")
              (insert (propertize (concat "  " prompt) 'face face))
              (put-text-property insert-start (point) 'tabulated-list-id id)
              (insert "\n" sep)
              (put-text-property (1- (point)) (point)
                                 'agent-shell-queue-separator t))))))))

(defun agent-shell-queue-next-item ()
  "Move point to the first line of the next queue item."
  (interactive)
  (if (not agent-shell-queue-multiline-format)
      (forward-line 1)
    (let ((current-id (tabulated-list-get-id)))
      (forward-line 1)
      (while (and (not (eobp))
                  (equal (tabulated-list-get-id) current-id))
        (forward-line 1)))))

(defun agent-shell-queue-prev-item ()
  "Move point to the first line of the previous queue item."
  (interactive)
  (if (not agent-shell-queue-multiline-format)
      (forward-line -1)
    (let ((current-id (tabulated-list-get-id))
          (target-id nil))
      ;; Step backward until a different id appears
      (forward-line -1)
      (while (and (not (bobp))
                  (or (null (tabulated-list-get-id))
                      (equal (tabulated-list-get-id) current-id)))
        (forward-line -1))
      (setq target-id (tabulated-list-get-id))
      ;; Now find the first (topmost) line of that item
      (when target-id
        (while (and (not (bobp))
                    (equal (get-text-property (line-beginning-position 0)
                                              'tabulated-list-id)
                           target-id))
          (forward-line -1))
        ;; If we overshot past the item, go forward one line
        (unless (equal (tabulated-list-get-id) target-id)
          (forward-line 1))))))

(defun agent-shell-queue-buffer-jump-to-next ()
  "Move point to the next item that will be dispatched."
  (interactive)
  (let ((next-ids (cl-loop for pair in (agent-shell-queue-store-items agent-shell-queue--store)
                           for next = (agent-shell-queue--next-dispatchable-item (cdr pair))
                           when next collect (agent-shell-queue-item-id next))))
    (if (null next-ids)
        (message "No pending items in queue")
      (goto-char (point-min))
      (let (found)
        (while (and (not found) (not (eobp)))
          (if (member (tabulated-list-get-id) next-ids)
              (setq found t)
            (forward-line 1)))
        (unless found
          (message "No pending items visible in current scope"))))))

(defun agent-shell-queue-buffer-pause ()
  "Pause the item at point — suspend it from auto-dispatch without removing it."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'active)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'deferred)
    (agent-shell-queue--save)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-schedule ()
  "Schedule the paused item at point — resume it for auto-dispatch."
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
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-remove id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-send ()
  "Send the item at point to its target buffer now."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-send-item id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-reenqueue (id)
  "Create a new active queue item from the done item with ID."
  (when-let* ((pair (or (agent-shell-queue--item-by-id id)
                        (user-error "No queue item with id %s" id)))
               (old-item (cdr pair))
               (buf (or (get-buffer (car pair))
                        (user-error "Target buffer %s is no longer live" (car pair)))))
    (unless (memq (agent-shell-queue-item-status old-item) '(done aborted))
      (user-error "Item %s is not done or aborted; cannot re-enqueue" id))
    (agent-shell-queue-add
     (agent-shell-queue-item-prompt old-item)
     buf
     (agent-shell-queue-item-background old-item))))

(defun agent-shell-queue-buffer-reenqueue ()
  "Re-enqueue the done or aborted item at point as a new active item."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (_ (memq (agent-shell-queue-item-status (cdr pair)) '(done aborted))))
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
  (when-let* ((_ buf)
               (item (progn
                       (agent-shell-queue--ensure-loaded)
                       (agent-shell-queue-item--make
                        :id (agent-shell-queue--gen-id)
                        :prompt "[PAUSE — waiting for human]"
                        :status 'active
                        :kind 'pause
                        :created (float-time))))
               (id (agent-shell-queue-item-id item))
               (buf-name (buffer-name buf)))
      (agent-shell-queue--add-item-to-bucket buf-name item)
      (when (and position (> position 0))
        (dotimes (_ (max 0 (- (length (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
                              position)))
          (agent-shell-queue--move id -1)))
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (message "Pause inserted into %s queue" buf-name)))

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
    (let ((buf-name (buffer-name buf)))
      (agent-shell-queue--add-item-to-bucket buf-name (agent-shell-queue--make-item prompt nil 'context))
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
   (let ((buf (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                  (agent-shell-queue--pick-buffer "Compact item for: "))))
     (list (read-string "Manual task: ") buf)))
  (when (and prompt buf (not (string-empty-p prompt)))
    (agent-shell-queue--ensure-loaded)
    (let ((buf-name (buffer-name buf)))
      (agent-shell-queue--add-item-to-bucket buf-name (agent-shell-queue--make-item prompt nil 'compact))
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

    (agent-shell-queue--assert-not-running item)
    (setf (agent-shell-queue-item-status item) 'done)
    (setf (agent-shell-queue-item-completed item) (float-time))
    (agent-shell-queue--append-done-log buf-name item)

    (when (member (cons buf-name id) agent-shell-queue--compact-running)
      (setq agent-shell-queue--compact-running
            (seq-remove (lambda (it) (equal it (cons buf-name id))) agent-shell-queue--compact-running))
      (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
            (seq-remove (lambda (it) (equal it buf-name))
                        (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))

    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)

    (when-let* ((buf (get-buffer buf-name)))
      (agent-shell-queue--send-next-for-buffer buf))))

(defun agent-shell-queue-buffer-mark-done ()
  "Mark the item at point as done."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue-mark-done id)))

(defun agent-shell-queue-item-view-mark-done ()
  "Mark the displayed item as done."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id))
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
    ;; Pause / schedule
    (define-key m (kbd "d")        #'agent-shell-queue-item-view-pause)
    (define-key m (kbd "u")        #'agent-shell-queue-item-view-schedule)
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
  (setq-local fill-column 80)
  (let* ((created (agent-shell-queue-item-created item))
         (dispatched (agent-shell-queue-item-dispatched item))
         (completed (agent-shell-queue-item-completed item))
         (bg (agent-shell-queue-item-background item))
         (kind (agent-shell-queue-item-kind item))
         (next-p (when-let* ((first (agent-shell-queue--next-dispatchable-item
                                    (cdr (assoc target (agent-shell-queue-store-items agent-shell-queue--store))))))
                   (equal (agent-shell-queue-item-id first) id)))
         (sep (make-string 80 ?─))
         (field (lambda (label value)
                  (insert (propertize (format "%-12s" label) 'face 'bold))
                  (insert (format " %s\n" value)))))
    (insert (propertize "Queue Item\n" 'face 'bold))
    (insert sep "\n")
    (funcall field "ID:" id)
    (funcall field "Target:"
             (if (equal target agent-shell-queue--unassigned-key)
                 "(unassigned)" target))
    (funcall field "Status:" (agent-shell-queue--status-string item target next-p))
    (funcall field "Kind:" (symbol-name (or kind 'prompt)))
    (funcall field "Background:" (if bg "yes" "no"))
    (insert sep "\n")
    (funcall field "Created:"
             (format "%s (%s ago)"
                     (format-time-string "%F %T" created)
                     (agent-shell-queue--format-age (time-since created))))
    (when dispatched
      (funcall field "Dispatched:"
               (format "%s (%s ago)"
                       (format-time-string "%F %T" dispatched)
                       (agent-shell-queue--format-age (time-since dispatched)))))
    (when completed
      (funcall field "Completed:"
               (format "%s (%s ago)"
                       (format-time-string "%F %T" completed)
                       (agent-shell-queue--format-age (time-since completed)))))
    (when (and dispatched completed)
      (funcall field "Latency:"
               (agent-shell-queue--format-age (time-subtract completed dispatched))))
    (insert sep "\n")
    (insert (propertize "Prompt:\n" 'face 'bold))
    (insert (agent-shell-queue-item-prompt item) "\n")
    (when-let* ((response (agent-shell-queue-item-response item)))
      (insert sep "\n")
      (insert (propertize "Response:\n" 'face 'bold))
      (insert response "\n"))
    (insert sep "\n")
    (insert (propertize
             (let ((status (agent-shell-queue-item-status item)))
               (cond
                ((eq status 'running)
                 "[X] abort  [m] menu  [q] close")
                ((eq status 'active)
                 "[s] send now  [e] edit  [d] pause  [b] background  [m] menu  [q] close")
                ((eq status 'deferred)
                 "[u] schedule  [e] edit  [m] menu  [q] close")
                ((memq status '(done aborted))
                 "[R] re-enqueue  [A] archive  [k] remove  [m] menu  [q] close")
                ((eq status 'incomplete)
                 "[s] retry  [e] edit  [k] remove  [m] menu  [q] close")
                (t
                 "[e] edit  [k] remove  [m] menu  [q] close")))
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
              (target (car pair))
              (inhibit-read-only t))
    (erase-buffer)
    (agent-shell-queue--render-item-view id item target)))

(defun agent-shell-queue-item-view-send ()
  "Send the displayed item to its target buffer now."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (agent-shell-queue--assert-not-running item)
    (quit-window)
    (agent-shell-queue-send-item id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-item-view-remove ()
  "Remove the displayed item from the queue, with confirmation."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (_ (or (agent-shell-queue--assert-not-running item) t)))
    (when (agent-shell-queue--confirm-remove item)
      (quit-window)
      (agent-shell-queue-remove id)
      (agent-shell-queue--refresh-buffer))))

(defun agent-shell-queue-item-view-pause ()
  "Pause the displayed item — suspend it from auto-dispatch."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'active)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'deferred)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-schedule ()
  "Schedule the displayed item — resume it for auto-dispatch."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'deferred)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'active)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-reenqueue ()
  "Re-enqueue the displayed done or aborted item as a new active item."
  (interactive)https://github.com/casaphq/go-monorepo/pull/6420
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (_ (memq (agent-shell-queue-item-status (cdr pair)) '(done aborted))))
    (quit-window)
    (agent-shell-queue-reenqueue id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-item-view-archive ()
  "Archive the displayed item and close the view.
Archiving must be enabled via `agent-shell-queue-archive-enabled'."
  (interactive)
  (unless agent-shell-queue-archive-enabled
    (user-error "Enable archiving by setting `agent-shell-queue-archive-enabled' to t"))
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue--write-archive (car pair) item)
    (quit-window)
    (agent-shell-queue-remove id)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: archived %s" id)))

;;;###autoload
(defun agent-shell-queue-archive-done-n (n)
  "Archive the N oldest done items across all queues.
Errors if archiving is not enabled."
  (interactive (list (let ((n (read-number "Archive how many done items? " 10)))
                       (if (> n 0)
                           n
                         (user-error "N must be a positive number")))))
  (unless agent-shell-queue-archive-enabled
    (user-error "Enable archiving by setting `agent-shell-queue-archive-enabled' to t"))
  (let* ((all-pairs
          (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                       (seq-mapcat (lambda (bucket)
                                     (seq-map (lambda (item) (cons (car bucket) item))
                                              (cdr bucket))))
                       (seq-filter (lambda (pair)
                                     (eq (agent-shell-queue-item-status (cdr pair)) 'done)))
                       (seq-sort (lambda (a b)
                                   (< (agent-shell-queue-item-created (cdr a))
                                      (agent-shell-queue-item-created (cdr b)))))))
         (to-archive (seq-take all-pairs n))
         (count (length to-archive)))
    (when (= count 0)
      (user-error "No done items to archive"))
    (dolist (pair to-archive)
      (agent-shell-queue--write-archive (car pair) (cdr pair))
      (agent-shell-queue-remove (agent-shell-queue-item-id (cdr pair))))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: archived %d done item(s)" count)))

;;;###autoload
(defun agent-shell-queue-archive-done-all ()
  "Archive all done items across all queues.
Errors if archiving is not enabled or no done items exist."
  (interactive)
  (unless agent-shell-queue-archive-enabled
    (user-error "Enable archiving by setting `agent-shell-queue-archive-enabled' to t"))
  (let* ((all-pairs
          (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                       (seq-mapcat (lambda (bucket)
                                     (seq-map (lambda (item) (cons (car bucket) item))
                                              (cdr bucket))))
                       (seq-filter (lambda (pair)
                                     (eq (agent-shell-queue-item-status (cdr pair)) 'done)))
                       (seq-sort (lambda (a b)
                                   (< (agent-shell-queue-item-created (cdr a))
                                      (agent-shell-queue-item-created (cdr b)))))))
         (count (length all-pairs)))
    (when (= count 0)
      (user-error "No done items to archive"))
    (dolist (pair all-pairs)
      (agent-shell-queue--write-archive (car pair) (cdr pair))
      (agent-shell-queue-remove (agent-shell-queue-item-id (cdr pair))))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: archived %d done item(s)" count)))

;;;###autoload
(defun agent-shell-queue-load-archive (&optional file)
  "Import items from the JSONL archive file into the queue as active items.
FILE defaults to the path returned by `agent-shell-queue-archive-file-function';
when called interactively with a prefix argument, prompts for a file path."
  (interactive
   (list (if current-prefix-arg
             (read-file-name "Archive file: " nil (agent-shell-queue--archive-file) t)
           (or (agent-shell-queue--archive-file)
               (user-error "Set `agent-shell-queue-archive-enabled' or pass a file")))))
  (unless (file-exists-p file)
    (user-error "Archive file not found: %s" file))
  (let ((count 0)
        (content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
    (dolist (line (split-string content "\n"))
      (unless (string-blank-p line)
        (condition-case err
            (let* ((obj (json-parse-string line :object-type 'plist))
                   (item (agent-shell-queue-item--make
                          :id (agent-shell-queue--gen-id)
                          :prompt (plist-get obj :prompt)
                          :status 'active
                          :kind (intern (or (plist-get obj :kind) "prompt"))
                          :background (eq t (plist-get obj :background))
                          :created (float-time)))
                   (raw-target (plist-get obj :target))
                   (target (if (and raw-target (get-buffer raw-target))
                               raw-target
                             agent-shell-queue--unassigned-key)))
              (agent-shell-queue--add-item-to-bucket target item)
              (setq count (1+ count)))
          (error (message "agent-shell-queue: skipping malformed archive line: %s" err)))))
    (when (= count 0)
      (user-error "No items found in archive"))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: imported %d item(s) from archive" count)))

(defun agent-shell-queue-item-view-enable-background-task ()
  "Flag the displayed item for background sub-agent execution."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id t)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-disable-background-task ()
  "Clear the background sub-agent flag from the displayed item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id nil)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-move-up ()
  "Move the displayed item one position earlier in its queue."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-up id)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-move-down ()
  "Move the displayed item one position later in its queue."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-down id)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-assign ()
  "Assign the displayed item to a different agent-shell buffer."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (bufs (or (agent-shell-buffers)
                        (user-error "No live agent-shell buffers")))
              (_ (or (agent-shell-queue--assert-not-running (cdr pair)) t)))
    (let* ((current-dir (when-let* ((b (get-buffer (car pair))))
                          (buffer-local-value 'default-directory b)))
           (table (seq-map
                   (lambda (buf)
                     (let ((dir (buffer-local-value 'default-directory buf)))
                       (cons (buffer-name buf)
                             (if (and current-dir (equal dir current-dir))
                                 (concat "(same dir) " (abbreviate-file-name dir))
                               (abbreviate-file-name (or dir ""))))))
                   bufs)))
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
              (item (cdr (agent-shell-queue--item-by-id id)))
              (qbuf agent-shell-queue--item-view-queue-buf)
              ((buffer-live-p qbuf)))
    (agent-shell-queue--assert-not-running item)
    (quit-window)
    (with-current-buffer qbuf
      (goto-char (point-min))
      (while (and (not (equal (tabulated-list-get-id) id))
                  (not (eobp)))
        (forward-line 1))
      (agent-shell-queue-buffer-edit))))

;;; Running guard

(defun agent-shell-queue--assert-not-running (item)
  "Signal `user-error' if ITEM status is `running'.
Running items may only be interrupted via the abort command."
  (when (eq (agent-shell-queue-item-status item) 'running)
    (user-error "Cannot modify a running item; abort it first")))

(defun agent-shell-queue-buffer-abort ()
  "Interrupt the running item at point and mark it as aborted.
Pauses the session queue — call `agent-shell-queue-session-resume' to restart."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              ((eq (agent-shell-queue-item-status item) 'running)))
    (let ((buf-name (car pair)))
      (when-let* ((buf (get-buffer buf-name))
                  (_ (buffer-live-p buf)))
        (with-current-buffer buf
          (agent-shell-interrupt)))
      (setf (agent-shell-queue-item-status item) 'aborted)
      (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
      (agent-shell-queue--save)
      (agent-shell-queue-buffer-refresh))))

(defun agent-shell-queue-item-view-abort ()
  "Interrupt the running displayed item and mark it as aborted.
Pauses the session queue — call `agent-shell-queue-session-resume' to restart."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              ((eq (agent-shell-queue-item-status item) 'running)))

    (let ((buf-name (car pair)))
      (when-let* ((buf (get-buffer buf-name))
                  (_ (buffer-live-p buf)))
        (with-current-buffer buf
          (agent-shell-interrupt)))
      (setf (agent-shell-queue-item-status item) 'aborted)
      (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (agent-shell-queue-item-view-refresh))))

;;; Transient predicates

(defun agent-shell-queue--iv-item ()
  "Return the item being viewed in the current item-view buffer, or nil."
  (when (and (boundp 'agent-shell-queue--item-view-id) agent-shell-queue--item-view-id)
    (cdr (agent-shell-queue--item-by-id agent-shell-queue--item-view-id))))

(defun agent-shell-queue--iv-status ()
  "Return the status of the item being viewed, or nil."
  (when-let* ((item (agent-shell-queue--iv-item)))
    (agent-shell-queue-item-status item)))

(defun agent-shell-queue--iv-bg-p ()
  "Return non-nil if the viewed item has background mode enabled."
  (when-let* ((item (agent-shell-queue--iv-item)))
    (agent-shell-queue-item-background item)))

(defun agent-shell-queue--point-item ()
  "Return the queue item at point in the queue buffer, or nil."
  (when-let* ((id (and (derived-mode-p 'agent-shell-queue-mode)
                      (tabulated-list-get-id))))
    (cdr (agent-shell-queue--item-by-id id))))

(defun agent-shell-queue--point-status ()
  "Return the status of the queue item at point, or nil."
  (when-let* ((item (agent-shell-queue--point-item)))
    (agent-shell-queue-item-status item)))

(defun agent-shell-queue--point-bg-p ()
  "Return non-nil if the item at point has background mode enabled."
  (when-let* ((item (agent-shell-queue--point-item)))
    (agent-shell-queue-item-background item)))

(defun agent-shell-queue--point-running-p ()
  "Return non-nil when the item at point is running."
  (eq (agent-shell-queue--point-status) 'running))

(defun agent-shell-queue--point-not-running-p ()
  "Return non-nil when the item at point is not running."
  (not (agent-shell-queue--point-running-p)))

(defun agent-shell-queue--point-active-p ()
  "Return non-nil when the item at point is active."
  (eq (agent-shell-queue--point-status) 'active))

(defun agent-shell-queue--point-deferred-p ()
  "Return non-nil when the item at point is deferred or draft."
  (memq (agent-shell-queue--point-status) '(deferred draft)))

(defun agent-shell-queue--point-done-p ()
  "Return non-nil when the item at point is done or aborted."
  (memq (agent-shell-queue--point-status) '(done aborted)))

(defun agent-shell-queue--point-dispatchable-p ()
  "Return non-nil when the item at point can be dispatched."
  (not (memq (agent-shell-queue--point-status)
             '(done running aborted nil draft))))

(defun agent-shell-queue--point-not-done-p ()
  "Return non-nil when the item at point is in a not-done, non-running state."
  (not (memq (agent-shell-queue--point-status)
             '(done running aborted nil))))

(defun agent-shell-queue--point-editable-p ()
  "Return non-nil when the item at point can be edited or moved.
Items in aborted state remain editable; only running, done, or absent items are excluded."
  (not (memq (agent-shell-queue--point-status) '(done running nil))))

(transient-define-prefix agent-shell-queue-item-menu ()
  "Actions for the item shown in the current item-view buffer."
  [["Manage Task"
    ("s" "Dispatch now" agent-shell-queue-item-view-send
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted draft)))))
    ("X" "Abort (interrupt)" agent-shell-queue-item-view-abort
     :if (lambda () (eq (agent-shell-queue--iv-status) 'running)))
    ("R" "Re-enqueue" agent-shell-queue-item-view-reenqueue
     :if (lambda () (memq (agent-shell-queue--iv-status) '(done aborted))))
    ("z" "Mark done" agent-shell-queue-item-view-mark-done
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
    ("e" "Edit" agent-shell-queue-item-view-edit
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
    ("d" "Pause (suspend from dispatch)" agent-shell-queue-item-view-pause
     :if (lambda () (eq (agent-shell-queue--iv-status) 'active)))
    ("u" "Schedule (resume dispatch)" agent-shell-queue-item-view-schedule
     :if (lambda () (memq (agent-shell-queue--iv-status) '(deferred draft))))
    ("b" "Enable background task" agent-shell-queue-item-view-enable-background-task
     :if (lambda () (and (not (memq (agent-shell-queue--iv-status) '(done running aborted)))
                         (not (agent-shell-queue--iv-bg-p)))))
    ("B" "Disable background task" agent-shell-queue-item-view-disable-background-task
     :if (lambda () (and (not (memq (agent-shell-queue--iv-status) '(done running aborted)))
                         (agent-shell-queue--iv-bg-p))))
    ("A" "Archive" agent-shell-queue-item-view-archive
     :if (lambda () (not (eq (agent-shell-queue--iv-status) 'running))))
    ("k" "Remove" agent-shell-queue-item-view-remove
     :if (lambda () (not (eq (agent-shell-queue--iv-status) 'running))))]
   ["Move / Assign" :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted nil))))
    ("M-<up>" "Move up" agent-shell-queue-item-view-move-up
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
    ("M-<down>" "Move down" agent-shell-queue-item-view-move-down
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
    ("t" "Assign to shell…" agent-shell-queue-item-view-assign
     :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))]])

(defun agent-shell-queue-buffer-move-up ()
  "Move the item at point one position earlier."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-up id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-move-down ()
  "Move the item at point one position later."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-down id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-enable-background-task ()
  "Flag the item at point for background sub-agent execution."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id t)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-disable-background-task ()
  "Clear the background sub-agent flag from the item at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
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
    (agent-shell-queue--assert-not-running (cdr pair))
    (let* ((current-dir (when-let* ((b (get-buffer (car pair))))
                          (buffer-local-value 'default-directory b)))
           (table (seq-map (lambda (it)
                             (let ((dir (buffer-local-value 'default-directory it)))
                               (cons (buffer-name it)
                                     (if (and current-dir (equal dir current-dir))
                                         (concat "(same dir) " (abbreviate-file-name dir))
                                       (abbreviate-file-name (or dir ""))))))
                           bufs)))
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
                    (seq-remove #'null
                                (list
                       (cons "send now" #'agent-shell-queue-buffer-send)
                       (when (eq status 'active)
                         (cons "pause (suspend from dispatch)" #'agent-shell-queue-buffer-pause))
                       (when (eq status 'deferred)
                         (cons "schedule (resume dispatch)" #'agent-shell-queue-buffer-schedule))
                       (if bg
                           (cons "disable background sub-agent" #'agent-shell-queue-buffer-disable-background-task)
                         (cons "enable background sub-agent" #'agent-shell-queue-buffer-enable-background-task))
;; Assisted-by: AGENT_NAME:MODEL_VERSION                       (cons "edit prompt" #'agent-shell-queue-buffer-edit)
                       (cons "assign to shell" #'agent-shell-queue-buffer-assign)
                       (cons "move up" #'agent-shell-queue-buffer-move-up)
                       (cons "move down" #'agent-shell-queue-buffer-move-down)
                       (cons "insert pause checkpoint" #'agent-shell-queue-insert-pause)
                       (cons "insert context drop" #'agent-shell-queue-insert-clear-context))))
                  (when done
                    (list (cons "re-enqueue (new active copy)" #'agent-shell-queue-buffer-reenqueue)))
                  (list (cons "remove" #'agent-shell-queue-buffer-remove))))
           (table (seq-map (lambda (it)
                             (cons (car it)
                                   (or (car (split-string (or (documentation (cdr it)) "") "\n")) "")))
                           cmds)))
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

;;; Display toggle commands

(defun agent-shell-queue-toggle-buffer-column ()
  "Toggle visibility of the Buffer column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-buffer-column
        (not agent-shell-queue-show-buffer-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue buffer column: %s"
           (if agent-shell-queue-show-buffer-column "on" "off")))

(defun agent-shell-queue-toggle-ordinal-column ()
  "Toggle visibility of the ordinal (#) column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-ordinal-column
        (not agent-shell-queue-show-ordinal-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue ordinal column: %s"
           (if agent-shell-queue-show-ordinal-column "on" "off")))

(defun agent-shell-queue-toggle-age-column ()
  "Toggle visibility of the Age column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-age-column
        (not agent-shell-queue-show-age-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue age column: %s"
           (if agent-shell-queue-show-age-column "on" "off")))

(defun agent-shell-queue-toggle-multiline-format ()
  "Toggle multi-line display format for the queue buffer."
  (interactive)
  (setq agent-shell-queue-multiline-format
        (not agent-shell-queue-multiline-format))
  (agent-shell-queue-buffer-refresh)
  (message "Queue multi-line format: %s"
           (if agent-shell-queue-multiline-format "on" "off")))

;;;###autoload
(defun agent-shell-queue-select-columns ()
  "Pick column display options via `annotated-completing-read'.
Offers bulk presets, per-column visibility toggles, and the multi-line
format switch.  Changes take effect immediately via `agent-shell-queue-buffer-refresh'."
  (interactive)
  (unless (derived-mode-p 'agent-shell-queue-mode)
    (user-error "Not in an agent-shell queue buffer"))
  (let* ((columns `(("Buffer column" . agent-shell-queue-show-buffer-column)
                    ("Ordinal # column" . agent-shell-queue-show-ordinal-column)
                    ("Age column" . agent-shell-queue-show-age-column)))
         (table (make-hash-table :test #'equal)))
    (map-put! table "+ show all columns"
             (if (and agent-shell-queue-show-buffer-column
                      agent-shell-queue-show-ordinal-column
                      agent-shell-queue-show-age-column)
                 "already showing all columns"
               "enable Buffer, Ordinal, and Age columns"))
    (map-put! table "+ minimal: status and prompt only"
             (if (and (not agent-shell-queue-show-buffer-column)
                      (not agent-shell-queue-show-ordinal-column)
                      (not agent-shell-queue-show-age-column))
                 "already minimal"
               "hide Buffer, Ordinal, and Age columns"))
    (dolist (it columns)
      (let ((on (symbol-value (cdr it))))
        (map-put! table (car it)
                 (if on "visible · click to hide" "hidden · click to show"))))
    (map-put! table "Multi-line format"
             (if agent-shell-queue-multiline-format
                 "on · prompt on second line · click to disable"
               "off · single-line · click to enable"))
    (when-let* ((choice (annotated-completing-read
                        table
                        :prompt "queue columns: "
                        :category 'agent-shell-queue-column
                        :require-match t
                        :history 'agent-shell-queue-select-columns)))
      (cond
       ((equal choice "+ show all columns")
        (setq agent-shell-queue-show-buffer-column t
              agent-shell-queue-show-ordinal-column t
              agent-shell-queue-show-age-column t))
       ((equal choice "+ minimal: status and prompt only")
        (setq agent-shell-queue-show-buffer-column nil
              agent-shell-queue-show-ordinal-column nil
              agent-shell-queue-show-age-column nil))
       ((equal choice "Multi-line format")
        (setq agent-shell-queue-multiline-format (not agent-shell-queue-multiline-format)))
       (t
        (when-let* ((var (cdr (assoc choice columns))))
          (set var (not (symbol-value var))))))
      (agent-shell-queue-buffer-refresh))))

;;; Transient menu

(transient-define-prefix agent-shell-queue-menu ()
  "Actions for the item at point in the queue buffer."
  [["Queue control"
    ("p" "Pause (global)" agent-shell-queue-pause)
    ("R" "Resume (global)" agent-shell-queue-resume)
    ("lp" "Pause (session)" agent-shell-queue-session-pause)
    ("lR" "Resume (session)" agent-shell-queue-session-resume)
    ("U" "Resume all sessions" agent-shell-queue-unpause-all-sessions)]
   ["Manage Task" :if agent-shell-queue--point-item
    ("s" "Dispatch now" agent-shell-queue-buffer-send
     :if agent-shell-queue--point-dispatchable-p)
    ("X" "Abort (interrupt)" agent-shell-queue-buffer-abort
     :if agent-shell-queue--point-running-p)
    ("S" "Re-enqueue" agent-shell-queue-buffer-reenqueue
     :if agent-shell-queue--point-done-p)
    ("z" "Mark done" agent-shell-queue-buffer-mark-done
     :if agent-shell-queue--point-not-done-p)
    ("E" "Edit task (select)" agent-shell-queue-edit-task)
    ("e" "Edit at point" agent-shell-queue-buffer-edit
     :if agent-shell-queue--point-editable-p)
    ("P" "Pause (suspend from dispatch)" agent-shell-queue-buffer-pause
     :if agent-shell-queue--point-active-p)
    ("u" "Schedule (resume dispatch)" agent-shell-queue-buffer-schedule
     :if agent-shell-queue--point-deferred-p)
    ("b" "Enable background task" agent-shell-queue-buffer-enable-background-task
     :if (lambda () (and (agent-shell-queue--point-editable-p)
                         (not (agent-shell-queue--point-bg-p)))))
    ("B" "Disable background task" agent-shell-queue-buffer-disable-background-task
     :if (lambda () (and (agent-shell-queue--point-editable-p)
                         (agent-shell-queue--point-bg-p))))
    ("A" "Archive" agent-shell-queue-buffer-archive
     :if agent-shell-queue--point-not-running-p)
    ("k" "Remove" agent-shell-queue-buffer-remove
     :if agent-shell-queue--point-not-running-p)]
   ["Move / Assign" :if agent-shell-queue--point-editable-p
    ("M-<up>" "Move up" agent-shell-queue-buffer-move-up
     :if agent-shell-queue--point-editable-p)
    ("M-<down>" "Move down" agent-shell-queue-buffer-move-down
     :if agent-shell-queue--point-editable-p)
    ("t" "Assign to shell…" agent-shell-queue-buffer-assign
     :if agent-shell-queue--point-editable-p)]]
  [["Capture"
    ("w" "Compose (write)" agent-shell-queue-capture)
    ("a" "Insert after point" agent-shell-queue-buffer-capture-after)
    ("n" "Unassigned capture" agent-shell-queue-capture-unassigned)
    ("r" "From region" agent-shell-queue-capture-from-region)
    ("y" "From clipboard" agent-shell-queue-capture-from-clipboard)
    ("c" "From context" agent-shell-queue-capture-from-context)
    ("q" "Enqueue prompt" agent-shell-queue-enqueue)
    ("Q" "Enqueue clear" agent-shell-queue-enqueue-clear)]
   ["Insert"
    ("i" "Insert pause checkpoint" agent-shell-queue-insert-pause)
    ("I" "Insert context drop" agent-shell-queue-insert-clear-context)
    ("C" "Insert compact (manual)" agent-shell-queue-insert-compact)
    ("!" "Insert Emacs call" agent-shell-queue-enqueue-emacs)
    ("T" "Insert wait-until (timer)" agent-shell-queue-insert-wait)]
   ["Fork"
    ("xf" "Fork queue at point" agent-shell-queue-buffer-fork
     :if agent-shell-queue--point-item)
    ("xb" "Insert fork task before point" agent-shell-queue-buffer-insert-fork-before
     :if agent-shell-queue--point-item)
    ("xa" "Insert fork task after point" agent-shell-queue-buffer-insert-fork-after
     :if agent-shell-queue--point-item)
    ("xr" "Release pending-fork items" agent-shell-queue-release-pending-fork)]
   ["Scope / Export"
    ("N" "Set scope (narrow)" agent-shell-queue-set-scope)
    ("W" "Widen to global scope" agent-shell-queue-scope-global)
    ("v" "Export scope to YAML" agent-shell-queue-export)
    ("li" "Toggle intercept mode" agent-shell-queue-toggle-intercept-mode)
    ("F" "Flush to disk" agent-shell-queue-flush)
    ("D" "Show disk state" agent-shell-queue-show-disk-state)
    ("O" "Open shell for item at point" agent-shell-queue-buffer-open-shell
     :if agent-shell-queue--point-item)]
   ["Display"
    ("dv" "Column display options" agent-shell-queue-select-columns)
    ("db" "Toggle buffer column" agent-shell-queue-toggle-buffer-column
     :description (lambda ()
                    (if agent-shell-queue-show-buffer-column
                        "Hide buffer column"
		      "Show buffer column")))
    ("dn" "Toggle ordinal (#) column" agent-shell-queue-toggle-ordinal-column
     :description (lambda ()
                    (if agent-shell-queue-show-ordinal-column
                        "Hide ordinal (#) column"
		      "Show ordinal (#) column")))
    ("da" "Toggle age column" agent-shell-queue-toggle-age-column
     :description (lambda ()
                    (if agent-shell-queue-show-age-column
                        "Hide age column"
		      "Show age column")))
    ("dm" "Toggle multi-line format" agent-shell-queue-toggle-multiline-format
     :description (lambda ()
                    (if agent-shell-queue-multiline-format
                        "Single-line format"
		      "Multi-line format")))]
   ["Raw Edit / Import"
    ("C-e" "Raw edit queue (YAML)" agent-shell-queue-raw-edit)
    ("C-E" "Import items (YAML)" agent-shell-queue-import)
    ("C-r" "Reload from disk" agent-shell-queue-reload)]])

(define-advice agent-shell-queue-menu (:before () guard-queue-buffer)
  "Signal an error when not invoked from an agent-shell queue overview buffer."
  (unless (derived-mode-p 'agent-shell-queue-mode)
    (user-error "Queue menu is only available from the agent-shell queue buffer")))

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
    (if-let* ((existing (get-buffer "*agent-shell-queue-edit*"))
              (_ (buffer-live-p existing))
              (_ (not (equal (buffer-local-value 'agent-shell-queue--editing-id existing) id))))
        (progn
          (pop-to-buffer existing '(display-buffer-below-selected))
          (user-error "agent-shell-queue: already editing item %s — save or cancel first"
                      (buffer-local-value 'agent-shell-queue--editing-id existing)))
      (let ((edit-buf (get-buffer-create "*agent-shell-queue-edit*")))
          (with-current-buffer edit-buf
            (when-let* ((prev-id agent-shell-queue--editing-id))
              (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)
                    (delete prev-id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue))))
            (erase-buffer)
            (insert (agent-shell-queue-item-prompt item))
            (agent-shell-queue-edit-mode)
            (setq-local agent-shell-queue--editing-id id))
          (cl-pushnew id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) :test #'equal)
          (agent-shell-queue--refresh-buffer)
          (pop-to-buffer edit-buf '(display-buffer-below-selected))))))

(defun agent-shell-queue-buffer-edit ()
  "Open a popup buffer to edit the prompt of the item at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue--open-edit-for-id id)))

;;;###autoload
(defun agent-shell-queue-edit-task ()
  "Select a queued item via completing-read and open its edit buffer.
Candidates are all non-done, non-running items across all buffers.
Annotations show queue position, target buffer, buffer state, item status,
and age."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let ((table (make-hash-table :test #'equal))
        (id-by-key (make-hash-table :test #'equal)))
    (dolist (pair (agent-shell-queue-store-items agent-shell-queue--store))
      (let* ((buf-name (car pair))
             (buf (get-buffer buf-name))
             (buf-state (cond
                         ((member buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue)) "paused")
                         ((and buf (with-current-buffer buf (shell-maker-busy))) "busy")
                         (t "idle")))
             (it-index 0))
        (seq-do
         (lambda (it)
           (unless (memq (agent-shell-queue-item-status it) '(done running))
             (let* ((id (agent-shell-queue-item-id it))
                    (prompt (agent-shell-queue-item-prompt it))
                    (status (agent-shell-queue--status-string it))
                    (age (agent-shell-queue--format-age
                          (time-since (agent-shell-queue-item-created it))))
                    (pos (1+ it-index))
                    (key (format "%s: %s" id
                                 (truncate-string-to-width prompt 60 nil nil "…")))
                    (ann (format "#%d · %s [%s] · %s · %s"
                                 pos buf-name buf-state status age)))
               (map-put! table key ann)
               (map-put! id-by-key key id)))
           (cl-incf it-index))
         (cdr pair))))
    (when (zerop (hash-table-count table))
      (user-error "No editable queued items"))
    (when-let* ((choice (annotated-completing-read table
                                                   :prompt "edit task: "
                                                   :category 'agent-shell-queue-item
                                                   :require-match t
                                                   :history 'agent-shell-queue-edit-task))
                (id (map-elt id-by-key choice)))
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
    (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)
          (delete id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)))
    (quit-window t)
    (unless (string-empty-p new-prompt)
      (agent-shell-queue-edit id new-prompt))
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-edit-cancel ()
  "Discard edits and close the popup."
  (interactive)
  (let ((id agent-shell-queue--editing-id))
    (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)
          (delete id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue))))
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
  (let* ((bucket-name (if target-buf
                          (buffer-name target-buf)
                        agent-shell-queue--unassigned-key))
         (capture-buf (get-buffer-create
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
        (insert initial-content))
      (let* ((bucket-items (cdr (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
             (depth (agent-shell-queue--active-item-count bucket-items))
             (state (agent-shell-queue--activity-state)))
        (setq-local header-line-format
                    (concat
                     (propertize (format " %s  |  " bucket-name) 'face 'shadow)
                     state
                     (propertize (format "  |  depth: %d" depth) 'face 'shadow)))))
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
        (when-let* ((pair (agent-shell-queue--item-by-id after-id))
                    (bucket-name (car pair))
                    (item (agent-shell-queue--make-item prompt bg)))
          (let ((items (cdr (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store)))))
            (if-let* ((idx (cl-position after-id items
                                        :key #'agent-shell-queue-item-id :test #'equal))
                      (cell (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
                (setcdr cell (append (cl-subseq items 0 (1+ idx))
                                     (list item)
                                     (cl-subseq items (1+ idx))))
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
    (if-let* ((draft-id agent-shell-queue--capture-draft-id)
              (_ (agent-shell-queue--item-by-id draft-id)))
        (progn
          (agent-shell-queue-edit draft-id prompt)
          (message "agent-shell-queue: draft updated (%s)" draft-id))
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
  (when-let* ((sel (ignore-errors (gui-get-selection 'CLIPBOARD))))
    (insert sel)))

(defun agent-shell-queue-capture-insert-thing-at-point ()
  "Insert the thing at point from the buffer that opened this capture."
  (interactive)
  (when-let* ((origin agent-shell-queue--capture-origin)
              (_ (buffer-live-p origin))
              (thing (with-current-buffer origin
                       (or (thing-at-point 'url t)
                           (thing-at-point 'filename t)
                           (thing-at-point 'symbol t)
                           (thing-at-point 'word t)))))
    (insert thing)))

(declare-function annotated-completing-read-context-from-point "annotated-completing-read")

(defun agent-shell-queue-capture-select-context ()
  "Select a string from the origin buffer's context via completing-read and insert it."
  (interactive)
  (when-let* ((origin agent-shell-queue--capture-origin)
              (_ (buffer-live-p origin))
              (text (with-current-buffer origin
                      (annotated-completing-read-context-from-point
                       :prompt "insert context: "
                       :history 'agent-shell-queue-capture-select-context)))
              (_ (not (string-empty-p text))))
    (insert text)))

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
  (when-let* ((buf (and prompt
                        (or buf
                            (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                            (agent-shell-queue--pick-buffer "Enqueue to: ")))))
    (with-current-buffer buf
      (if (shell-maker-busy)
          (agent-shell-queue-add prompt buf background)
        (agent-shell-insert
	 :text (if background
                   (concat agent-shell-queue-background-prefix prompt)
                 prompt)
         :submit t
	 :no-focus t)))))

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
    (let ((bucket-name (car pair)))
      (with-current-buffer
          (agent-shell-queue--open-capture
           (unless (equal bucket-name agent-shell-queue--unassigned-key)
             (get-buffer bucket-name))
           (current-buffer))
        (setq agent-shell-queue--capture-after-id id)))))


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
  (agent-shell-queue--open-capture buf (current-buffer)
                                   (when (use-region-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end)))))

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

  (let ((text (annotated-completing-read-context-from-point
               :prompt "seed capture: "
               :history 'agent-shell-queue-capture-from-context)))
    (agent-shell-queue--open-capture
     buf (current-buffer)
     (unless (string-empty-p text)
       text))))

;;;###autoload
(defun agent-shell-queue-capture-from-clipboard (&optional buf)
  "Open a capture buffer pre-seeded with the current clipboard contents.
BUF is the target agent-shell buffer; nil adds to the unassigned queue."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (agent-shell-queue--open-capture
   buf (current-buffer)
   (ignore-errors
     (gui-get-selection 'CLIPBOARD))))

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
  (let ((h (make-hash-table :test #'equal)))
    (map-put! h "id" (agent-shell-queue-item-id item))
    (map-put! h "prompt" (agent-shell-queue-item-prompt item))
    (map-put! h "status" (symbol-name (agent-shell-queue-item-status item)))
    (map-put! h "kind" (symbol-name (or (agent-shell-queue-item-kind item) 'prompt)))
    (map-put! h "background" (if (agent-shell-queue-item-background item) t nil))
    (map-put! h "created" (agent-shell-queue-item-created item))
    (when-let* ((d (agent-shell-queue-item-dispatched item)))
      (map-put! h "dispatched" d))
    (when-let* ((c (agent-shell-queue-item-completed item)))
      (map-put! h "completed" c))
    h))

(defun agent-shell-queue--render-to-yaml ()
  "Render active/deferred queue items to a YAML string for raw editing."
  (unless (fboundp 'yaml-encode)
    (error "yaml-encode not available; install the `yaml' package"))
  ;; if-let*
  (let ((buckets (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                              (seq-map (lambda (pair)
                                         (when-let* ((items (seq-remove
                                                             (lambda (item)
                                                               (memq (agent-shell-queue-item-status item)
                                                                     '(done running)))
                                                             (cdr pair)))
                                                     (h (make-hash-table :test #'equal)))
                                           (map-put! h "buffer" (car pair))
                                           (map-put! h "items"
                                                     (vconcat (seq-map #'agent-shell-queue--item-to-yaml-edit items)))
                                           h)))
                              (seq-remove #'null))))
    (if buckets
	(yaml-encode (vconcat buckets)) 
      "")))

(defun agent-shell-queue--make-edit-snapshot ()
  "Return a hash-table mapping item ID to item struct for all current items."
  (let ((table (make-hash-table :test #'equal)))
    (seq-do (lambda (it) (map-put! table (agent-shell-queue-item-id it) it))
            (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
              (seq-mapcat #'cdr)))
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
  (let* ((was-paused (agent-shell-queue-queue-paused agent-shell-queue--queue))
         (buf (get-buffer-create "*agent-shell-queue-raw-edit*")))
    (setf (agent-shell-queue-queue-paused agent-shell-queue--queue) t)
    (agent-shell-queue--refresh-buffer)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-shell-queue-raw-edit-mode)
        (setq agent-shell-queue--raw-edit-snapshot (agent-shell-queue--make-edit-snapshot)
              agent-shell-queue--raw-edit-was-paused was-paused)
        (insert (agent-shell-queue--render-to-yaml))))
    (pop-to-buffer buf '(display-buffer-below-selected (window-height . 0.5)))))

(defun agent-shell-queue--raw-edit-fail (text errors)
  "Save TEXT to a timestamped fail file, report ERRORS, leave queue paused."
  (let ((file (expand-file-name
               (format "agent-shell-queue-edit-failed-%s.yaml" (format-time-string "%Y%m%dT%H%M%S"))
               (file-name-directory (agent-shell-queue--state-file)))))
    (with-temp-file file
      (insert text))
    (dolist (it (nreverse errors))
      (message "agent-shell-queue raw edit: %s" it))
    (message "agent-shell-queue: %d error(s) — buffer saved to %s (queue remains paused)"
             (length errors) file)))

(defun agent-shell-queue--parse-yaml-item (item-h snapshot)
  "Validate hash-table ITEM-H against SNAPSHOT; return (item . errors) or (nil . errors)."
  (let* ((id (map-elt item-h "id"))
         (prompt (map-elt item-h "prompt"))
         (status-str (map-elt item-h "status" "active"))
         (kind-str (map-elt item-h "kind" "prompt"))
         (bg (map-elt item-h "background"))
         (created (map-elt item-h "created"))
         (dispatched (map-elt item-h "dispatched"))
         (completed (map-elt item-h "completed"))
         (status (condition-case nil (intern status-str) (error nil)))
         (kind (condition-case nil (intern kind-str) (error nil)))
         (orig (and id snapshot (map-elt snapshot id)))
         (errors nil))
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
      (when-let* ((od (agent-shell-queue-item-dispatched orig))
                  (_ (and dispatched (not (equal (float dispatched) (float od))))))
        (push (format "item '%s': 'dispatched' is immutable" id) errors))
      (when-let* ((oc (agent-shell-queue-item-completed orig))
                  (_ (and completed (not (equal (float completed) (float oc))))))
        (push (format "item '%s': 'completed' is immutable" id) errors))
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
              nil)))))

(defun agent-shell-queue--yaml-buckets (parsed)
  "Normalize PARSED (vector or list) to a list of bucket hash-tables."
  (cond
   ((vectorp parsed) (append parsed nil))
   ((listp parsed) parsed)
   (t (list parsed))))

(cl-defun agent-shell-queue-raw-edit-confirm ()
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
      (error (push (format "YAML parse error: %s" (cadr err)) errors)
             (cl-return-from agent-shell-queue-raw-edit-confirm
               (agent-shell-queue--raw-edit-fail text errors))))
    (let ((all-ids nil)
          (new-buckets nil))
      (thread-last (agent-shell-queue--yaml-buckets parsed)
        (seq-filter #'hash-table-p)
        (seq-do (lambda (bucket)
                  (let* ((buf-name (map-elt bucket "buffer"))
                         (items-raw (map-elt bucket "items"))
                         (items-list (if (vectorp items-raw)
                                         (append items-raw nil)
                                       items-raw))
                         (bucket-items))
                    (unless buf-name
                      (push "a bucket is missing the 'buffer' field" errors))
                    (dolist (item-h (seq-filter #'hash-table-p items-list))
                      (let ((id (map-elt item-h "id")))
                        (when (and id (member id all-ids))
                          (push (format "duplicate ID '%s'" id) errors))
                        (when id (push id all-ids))
                        (let ((result (agent-shell-queue--parse-yaml-item item-h snapshot)))
                          (if (cdr result)
                              (setq errors (append errors (cdr result)))
                            (push (car result) bucket-items)))))
                    (when (and buf-name bucket-items)
                      (push (cons buf-name (nreverse bucket-items)) new-buckets))))))
      (when errors
        (cl-return-from agent-shell-queue-raw-edit-confirm
          (agent-shell-queue--raw-edit-fail text errors)))
      ;; Preserve running/done items from current queue
      (let ((preserved nil))
        (dolist (it (agent-shell-queue-store-items agent-shell-queue--store))
          (when-let* ((kept (seq-filter (lambda (it)
                                          (memq (agent-shell-queue-item-status it) '(running done)))
                                        (cdr it))))
            (push (cons (car it) kept) preserved)))
        (let ((result (nreverse new-buckets)))
          (dolist (it (nreverse preserved))
            (if-let* ((cell (assoc (car it) result)))
                (setcdr cell (append (cdr cell) (cdr it)))
              (push it result)))
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (seq-remove #'agent-shell-queue--bucket-empty-p result))))
      (setf (agent-shell-queue-queue-paused agent-shell-queue--queue) was-paused)
      (agent-shell-queue--save)
      (quit-window t)
      (agent-shell-queue--refresh-buffer)
      (message "agent-shell-queue: raw edit applied%s"
               (if was-paused " (queue remains paused)" "")))))

(defun agent-shell-queue-raw-edit-cancel ()
  "Cancel raw edit; restore the queue pause state from before editing."
  (interactive)
  (let ((was-paused agent-shell-queue--raw-edit-was-paused))
    (quit-window t)
    (setf (agent-shell-queue-queue-paused agent-shell-queue--queue) was-paused)
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
    (thread-last (agent-shell-queue--yaml-buckets parsed)
      (seq-filter #'hash-table-p)
      (seq-do (lambda (bucket)
                (let* ((buf-name (map-elt bucket "buffer"))
                       (items-raw (map-elt bucket "items"))
                       (items-list (cond
                                    ((vectorp items-raw) (append items-raw nil))
                                    ((listp items-raw) items-raw)
                                    (t nil))))
                  (dolist (it (seq-filter #'hash-table-p items-list))
          (let* ((raw-id (map-elt it "id"))
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
            (cond
             ((eq final-id 'skip) (cl-incf skipped))
             (t
              (when (and (stringp final-id) (equal final-id raw-id) existing)
                (agent-shell-queue-remove raw-id))
              (let* ((prompt (map-elt it "prompt" ""))
                     (status-str (map-elt it "status" "active"))
                     (status (condition-case nil (intern status-str) (error 'active)))
                     (kind-str (map-elt it "kind" "prompt"))
                     (kind (condition-case nil (intern kind-str) (error 'prompt)))
                     (bg (eq t (map-elt it "background")))
                     (target-buf (and buf-name
                                      (not (equal buf-name agent-shell-queue--unassigned-key))
                                      (get-buffer buf-name)))
                     (item (agent-shell-queue-item--make
                            :id final-id
                            :prompt (if (string-empty-p (string-trim (or prompt "")))
                                        "(imported)" (string-trim prompt))
                            :status (if (memq status '(active deferred invalid)) status 'active)
                            :kind (if (memq kind '(prompt pause context emacs wait compact)) kind 'prompt)
                            :background bg
                            :created (or (map-elt it "created") (float-time)))))
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
    (let ((skip-note (if (> skipped 0) (format " (%d skipped)" skipped) "")))
      (message "agent-shell-queue: imported %d item(s)%s" added skip-note)))))


;;; Fork operations ─────────────────────────────────────────────────────────────

(defface agent-shell-queue-pending-fork-face
  '((t :foreground "mediumpurple3" :slant italic))
  "Face for queue items held pending a fork operation.")

(defvar agent-shell-queue-fork-default-mode 'new
  "Default mode for creating new sessions when forking a queue.
`new' creates a clean new session via `agent-shell-new-shell'.
`fork' uses the ACP fork session option via `agent-shell-fork'.")

(declare-function agent-shell-new-shell "agent-shell")
(declare-function agent-shell-fork "agent-shell")

(defmacro agent-shell-queue-with-paused-session (buf &rest body)
  "Execute BODY with BUF's queue session paused, then always resume it.
BUF can be a buffer object or buffer name string.
Directly manipulates the session-paused list to avoid spurious messages
during setup.  Always resumes and saves even if BODY signals an error."
  (declare (indent 1))
  (let ((bname (make-symbol "bname")))
    `(let ((,bname (if (bufferp ,buf) (buffer-name ,buf) ,buf)))
       (cl-pushnew ,bname
                   (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                   :test #'equal)
       (unwind-protect
           (progn ,@body)
         (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
               (delete ,bname
                       (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
         (agent-shell-queue--save)
         (agent-shell-queue--refresh-buffer)))))

(defun agent-shell-queue--fork-collect-items (buf-name from-id)
  "Return active items from BUF-NAME's queue at or after FROM-ID.
If FROM-ID is nil, returns all active/deferred/draft items.
Returns a list of items; does not modify the queue."
  (let* ((items (cdr (assoc buf-name
                            (agent-shell-queue-store-items agent-shell-queue--store))))
         (eligible-statuses '(active deferred draft)))
    (if (null from-id)
        (seq-filter (lambda (it) (memq (agent-shell-queue-item-status it) eligible-statuses)) items)
      ;; Search for from-id in the full list (it may be running/done, not just eligible)
      ;; then filter eligible items from that position onward.
      (when-let* ((pos (cl-position from-id items
                                    :key #'agent-shell-queue-item-id
                                    :test #'equal)))
        (seq-filter (lambda (it) (memq (agent-shell-queue-item-status it) eligible-statuses))
                    (nthcdr pos items))))))

(defun agent-shell-queue--fork-create-worktree (source-buf worktree-branch worktree-path)
  "Create a git worktree for a fork operation.
SOURCE-BUF provides the repo root (via `default-directory').
WORKTREE-BRANCH is the new branch name (auto-generated if nil).
WORKTREE-PATH is the worktree directory (auto-generated if nil).
Returns the worktree path string on success, nil on failure."
  (let* ((source-dir (if (buffer-live-p source-buf)
                         (buffer-local-value 'default-directory source-buf)
                       default-directory))
         (repo-root (string-trim
                     (shell-command-to-string
                      (format "git -C %s rev-parse --show-toplevel 2>/dev/null"
                              (shell-quote-argument source-dir)))))
         (branch (or worktree-branch
                     (format "queue-fork-%s"
                             (format-time-string "%Y%m%d-%H%M%S"))))
         (wt-path (or worktree-path
                      (expand-file-name branch (temporary-file-directory)))))
    (cond
     ((string-empty-p repo-root)
      (message "agent-shell-queue: not in a git repo, cannot create worktree")
      nil)
     ((file-exists-p wt-path)
      (message "agent-shell-queue: worktree path already exists: %s" wt-path)
      nil)
     (t
      (let ((exit-code (call-process "git" nil nil nil
                                     "-C" repo-root
                                     "worktree" "add" "-b" branch wt-path "HEAD")))
        (if (= exit-code 0)
            wt-path
          (message "agent-shell-queue: git worktree add failed (exit %d)" exit-code)
          nil))))))

(defun agent-shell-queue--fork-create-session (source-buf fork-mode target-dir)
  "Create a new agent-shell session and return the new buffer.
SOURCE-BUF is the session being forked (used for directory and fork mode).
FORK-MODE is `new' (create via `agent-shell-new-shell') or `fork' (via `agent-shell-fork').
TARGET-DIR, when non-nil, overrides the working directory for the new session.
Returns the newly created buffer on success, nil if none could be detected."
  (let* ((before-bufs (agent-shell-buffers))
         (dir (or target-dir
                  (and (buffer-live-p source-buf)
                       (buffer-local-value 'default-directory source-buf))
                  default-directory)))
    (pcase fork-mode
      ('fork
       (if (buffer-live-p source-buf)
           (with-current-buffer source-buf
             (let ((default-directory dir))
               (call-interactively #'agent-shell-fork)))
         (user-error "agent-shell-queue: `fork' mode requires a live source buffer")))
      (_
       (let ((default-directory dir))
         (call-interactively #'agent-shell-new-shell))))
    (sit-for 0.1)
    (let ((after-bufs (agent-shell-buffers)))
      (seq-find (lambda (it) (not (memq it before-bufs))) after-bufs))))

(defun agent-shell-queue--fork-elisp-form (buf-name opts)
  "Return an Emacs Lisp form string for a fork-queue emacs item.
BUF-NAME is the source session; OPTS is the fork options plist.
The form calls `agent-shell-queue--fork-session-from-running-emacs' at
dispatch time to dynamically determine which items to fork."
  (format "(agent-shell-queue--fork-session-from-running-emacs %S %S)"
          buf-name opts))

(defun agent-shell-queue--fork-session-from-running-emacs (buf-name opts)
  "Fork items after the currently-running emacs item in BUF-NAME's queue.
Called at dispatch time by an emacs-kind fork item to determine from-id
dynamically — handles reorderings that happened after the item was inserted.
OPTS is the fork options plist (see `agent-shell-queue-fork-session')."
  (let* ((items (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
         (running-idx (cl-position-if
                       (lambda (it) (eq (agent-shell-queue-item-status it) 'running))
                       items))
         (from-id (when running-idx
                    (let ((next (nth (1+ running-idx) items)))
                      (and next (agent-shell-queue-item-id next))))))
    (when-let* ((source-buf (get-buffer buf-name)))
      (agent-shell-queue-fork-session source-buf from-id opts))))

;;;###autoload
(defun agent-shell-queue-fork-session (source-buf &optional from-id opts)
  "Fork the queue for SOURCE-BUF starting at FROM-ID into a new agent-shell session.

Items at or after FROM-ID (by queue position among active/deferred/draft items)
are moved to the new session.  When FROM-ID is nil, all eligible items are
moved.  The original session is paused during session creation.

OPTS is a plist with these keys:
  :fork-mode       Symbol `new' (default) or `fork' — how to create the new session.
                   `new' calls `agent-shell-new-shell'; `fork' calls `agent-shell-fork'.
  :use-worktree    Non-nil — create a git worktree for the new session.
  :worktree-path   String — explicit worktree path (auto-generated when nil).
  :worktree-branch String — new branch name for the worktree.
  :capture-pending Non-nil — mark items at/after FROM-ID as `pending-fork' in the
                   original session instead of moving them, then leave the session
                   paused so new items can be inserted before the frozen ones."
  (interactive
   (list (agent-shell-queue--pick-buffer "Fork queue for session: ")))
  (agent-shell-queue--ensure-loaded)
  (let* ((source-name (buffer-name source-buf))
         (fork-mode (or (plist-get opts :fork-mode)
                        agent-shell-queue-fork-default-mode))
         (use-worktree (plist-get opts :use-worktree))
         (worktree-path (plist-get opts :worktree-path))
         (worktree-branch (plist-get opts :worktree-branch))
         (capture-pending (plist-get opts :capture-pending))
         (items-to-fork (agent-shell-queue--fork-collect-items source-name from-id))
         ;; Track whether we should resume source after the fork.
         ;; capture-pending intentionally leaves the source paused.
         (should-resume t))
    (unless items-to-fork
      (user-error "agent-shell-queue: no eligible items to fork in %s" source-name))
    ;; Pause source session while we create the new one.
    (cl-pushnew source-name
                (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                :test #'equal)
    (unwind-protect
        (let* ((target-dir (when use-worktree
                             (agent-shell-queue--fork-create-worktree
                              source-buf worktree-branch worktree-path)))
               (_ (when (and use-worktree (null target-dir))
                    (user-error "agent-shell-queue: worktree creation failed")))
               (new-buf (agent-shell-queue--fork-create-session
                         source-buf fork-mode target-dir)))
          (unless new-buf
            (user-error "agent-shell-queue: could not detect new session after creation"))
          (let ((new-name (buffer-name new-buf))
                (fork-ids (seq-map #'agent-shell-queue-item-id items-to-fork)))
            (if capture-pending
                ;; Mark affected items as pending-fork; leave them in source.
                ;; Keep session paused so the user can insert tasks before them.
                (progn
                  (dolist (it items-to-fork)
                    (setf (agent-shell-queue-item-status it) 'pending-fork))
                  (setq should-resume nil)
                  (agent-shell-queue--save)
                  (agent-shell-queue--refresh-buffer)
                  (message "agent-shell-queue: %d item(s) marked pending-fork in %s; new session %s created"
                           (length fork-ids) source-name new-name))
              ;; Normal mode: move items to the new session.
              (dolist (it fork-ids)
                (agent-shell-queue--assign-item it new-name))
              (agent-shell-queue--ensure-subscription new-buf)
              (agent-shell-queue--save)
              (agent-shell-queue--refresh-buffer)
              (message "agent-shell-queue: forked %d item(s) from %s → %s"
                       (length fork-ids) source-name new-name))
            new-buf))
      ;; Always clean up pause state unless capture-pending requested it stays.
      (when should-resume
        (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
              (delete source-name
                      (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
        (agent-shell-queue--save)
        (agent-shell-queue--refresh-buffer)))))

;;;###autoload
(defun agent-shell-queue-release-pending-fork (&optional buf)
  "Release all pending-fork items in BUF back to active status and resume dispatch.
BUF defaults to the current agent-shell session when called from one."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Release pending-fork items in: "))))
  (agent-shell-queue--ensure-loaded)
  (when buf
    (let* ((buf-name (buffer-name buf))
           (released 0))
      (dolist (it (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
        (when (eq (agent-shell-queue-item-status it) 'pending-fork)
          (setf (agent-shell-queue-item-status it) 'active)
          (cl-incf released)))
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (when (> released 0)
        (agent-shell-queue-session-resume buf))
      (message "agent-shell-queue: released %d pending-fork item(s) in %s" released buf-name))))

(defun agent-shell-queue--fork-insert-at (buf-name item idx)
  "Insert ITEM into BUF-NAME's queue at position IDX (0-based).
IDX nil or out-of-range appends to the end."
  (if-let* ((cell (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
      (let* ((items (cdr cell))
             (len (length items))
             (pos (if (and idx (>= idx 0) (< idx len)) idx len)))
        (setcdr cell (append (cl-subseq items 0 pos)
                             (list item)
                             (cl-subseq items pos))))
    (agent-shell-queue--add-item-to-bucket buf-name item)))

;;;###autoload
(defun agent-shell-queue-insert-fork-before (buf &optional item-id opts)
  "Insert a fork task into BUF's queue immediately before ITEM-ID.
When ITEM-ID is nil, appends to the end of the queue.
When the fork task is dispatched (as an emacs item), it forks the queue
starting at the item that follows the fork task in the queue at dispatch time.
OPTS is the fork options plist (see `agent-shell-queue-fork-session')."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-queue-mode)
                  (when-let* ((id (tabulated-list-get-id))
                              (pair (agent-shell-queue--item-by-id id)))
                    (get-buffer (car pair))))
             (agent-shell-queue--pick-buffer "Insert fork-before in: "))
         (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id))
         nil))
  (agent-shell-queue--ensure-loaded)
  (let* ((buf-name (buffer-name buf))
         (form (agent-shell-queue--fork-elisp-form buf-name opts))
         (fork-item (agent-shell-queue--make-item form nil 'emacs))
         (items (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
         (idx (when item-id
                (cl-position item-id items
                             :key #'agent-shell-queue-item-id :test #'equal))))
    (agent-shell-queue--fork-insert-at buf-name fork-item idx)
    (agent-shell-queue--ensure-subscription buf)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: fork task inserted before %s in %s"
             (or item-id "end") buf-name)
    fork-item))

;;;###autoload
(defun agent-shell-queue-insert-fork-after (buf &optional item-id opts)
  "Insert a fork task into BUF's queue immediately after ITEM-ID.
When ITEM-ID is nil, appends to the end of the queue.
When the fork task is dispatched, it forks the queue starting at the next
item after the fork task (determined dynamically at dispatch time).
OPTS is the fork options plist (see `agent-shell-queue-fork-session')."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-queue-mode)
                  (when-let* ((id (tabulated-list-get-id))
                              (pair (agent-shell-queue--item-by-id id)))
                    (get-buffer (car pair))))
             (agent-shell-queue--pick-buffer "Insert fork-after in: "))
         (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id))
         nil))
  (agent-shell-queue--ensure-loaded)
  (let* ((buf-name (buffer-name buf))
         (form (agent-shell-queue--fork-elisp-form buf-name opts))
         (fork-item (agent-shell-queue--make-item form nil 'emacs))
         (items (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
         (idx (when item-id
                (when-let* ((pos (cl-position item-id items
                                              :key #'agent-shell-queue-item-id
                                              :test #'equal)))
                  (1+ pos)))))
    (agent-shell-queue--fork-insert-at buf-name fork-item idx)
    (agent-shell-queue--ensure-subscription buf)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: fork task inserted after %s in %s"
             (or item-id "end") buf-name)
    fork-item))

(defun agent-shell-queue--fork-build-opts ()
  "Build fork options plist interactively using annotated-completing-read.
Prompts for fork mode, worktree settings, and capture-pending flag.
Returns a plist suitable for `agent-shell-queue-fork-session' or nil to abort."
  (let* ((mode-choice (annotated-completing-read
                       '(("new session" . "Create a clean new session via agent-shell-new-shell")
                         ("fork session (ACP)" . "Fork via agent-shell-fork (preserves context)"))
                       :prompt "fork mode: "
                       :category 'agent-shell-fork-mode
                       :require-match t
                       :history 'agent-shell-queue-fork-mode))
         (fork-mode (if (equal mode-choice "fork session (ACP)") 'fork 'new))
         (wt-choice (annotated-completing-read
                     '(("no worktree" . "New session opens in the same working directory")
                       ("create worktree" . "Run git worktree add and open the session in the new tree"))
                     :prompt "worktree: "
                     :category 'agent-shell-fork-worktree
                     :require-match t
                     :history 'agent-shell-queue-fork-worktree))
         (use-worktree (equal wt-choice "create worktree"))
         (worktree-branch (when use-worktree
                            (let ((b (read-string "Branch name (empty = auto): ")))
                              (unless (string-empty-p b) b))))
         (worktree-path (when use-worktree
                          (let ((p (read-string "Worktree path (empty = auto): ")))
                            (unless (string-empty-p p) p))))
         (cp-choice (annotated-completing-read
                     '(("move items to new session" . "Items are moved; original session resumes automatically")
                       ("capture pending (freeze & pause)" . "Items stay in original session as pending-fork; session stays paused"))
                     :prompt "after fork: "
                     :category 'agent-shell-fork-capture
                     :require-match t
                     :history 'agent-shell-queue-fork-capture))
         (capture-pending (equal cp-choice "capture pending (freeze & pause)")))
    (list :fork-mode fork-mode
          :use-worktree use-worktree
          :worktree-branch worktree-branch
          :worktree-path worktree-path
          :capture-pending capture-pending)))

;;;###autoload
(defun agent-shell-queue-buffer-fork ()
  "Fork the queue starting at the item at point into a new session.
Prompts interactively for fork options; uses annotated-completing-read when
called outside the queue buffer to build options without task-at-point context."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((id (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id)))
         (pair (and id (agent-shell-queue--item-by-id id)))
         (buf (if pair
                  (get-buffer (car pair))
                (agent-shell-queue--pick-buffer "Fork session: ")))
         (from-id (when pair id))
         (opts (agent-shell-queue--fork-build-opts)))
    (agent-shell-queue-fork-session buf from-id opts)))

;;;###autoload
(defun agent-shell-queue-buffer-insert-fork-before ()
  "Insert a fork queue item before the item at point.
Prompts for fork options interactively."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((id (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id)))
         (pair (and id (agent-shell-queue--item-by-id id)))
         (buf (if pair
                  (get-buffer (car pair))
                (agent-shell-queue--pick-buffer "Insert fork-before in: ")))
         (opts (agent-shell-queue--fork-build-opts)))
    (agent-shell-queue-insert-fork-before buf id opts)))

;;;###autoload
(defun agent-shell-queue-buffer-insert-fork-after ()
  "Insert a fork queue item after the item at point.
Prompts for fork options interactively."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((id (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id)))
         (pair (and id (agent-shell-queue--item-by-id id)))
         (buf (if pair
                  (get-buffer (car pair))
                (agent-shell-queue--pick-buffer "Insert fork-after in: ")))
         (opts (agent-shell-queue--fork-build-opts)))
    (agent-shell-queue-insert-fork-after buf id opts)))

;;; Initialize on load

(agent-shell-queue--setup-hooks)

(provide 'agent-shell-queue)

;;; agent-shell-queue.el ends here
