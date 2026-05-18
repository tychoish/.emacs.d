;;; agent-shell-queue-db.el --- SQLite backend for agent-shell-queue -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell

;; This file is not part of GNU Emacs

;;; Commentary:
;; Provides a SQLite persistence backend for agent-shell-queue.
;;
;; Activation:
;;   (require 'agent-shell-queue-db)
;;   (agent-shell-queue-db-enable)
;;
;; This replaces the default file-based save/load with direct SQLite I/O.
;; All other queue operations (dispatch, capture, reload, etc.) are unchanged.
;;
;; The schema stores one row per queue item with an explicit position column
;; to preserve intra-bucket ordering.  Done and running items are excluded
;; from the persistent state, matching the text-format behaviour.
;;
;; Requires Emacs 29+ (built-in sqlite support).

;;; Code:

(require 'cl-lib)
(require 'agent-shell-queue)

(declare-function agent-shell-queue--ensure-loaded "agent-shell-queue")
(declare-function agent-shell-queue--refresh-buffer "agent-shell-queue")
(declare-function agent-shell-queue-export "agent-shell-queue")
(declare-function agent-shell-queue-import "agent-shell-queue")
(declare-function agent-shell-queue--scope-label "agent-shell-queue")
(declare-function agent-shell-queue--item-to-yaml "agent-shell-queue")

;;; Variables

(defvar agent-shell-queue-db-file nil
  "Path to the SQLite database file used by the queue DB backend.
When nil, the path is derived from `user-emacs-directory' as
\"agent-shell-queue.db\".  Set before calling `agent-shell-queue-db-enable'
to store the database at a custom location.")

(defvar agent-shell-queue-db--connection nil
  "Live sqlite connection object, or nil when not open.")

(defvar agent-shell-queue-db--saved-state-file-function nil
  "Saved value of `agent-shell-queue-state-file-function' before DB enable.")

(defvar agent-shell-queue-db--saved-save-function nil
  "Saved value of `agent-shell-queue-save-function' before DB enable.")

(defvar agent-shell-queue-db--saved-load-function nil
  "Saved value of `agent-shell-queue-load-function' before DB enable.")

;;; Schema

(defconst agent-shell-queue-db--create-items
  "CREATE TABLE IF NOT EXISTS items (
     id         TEXT    PRIMARY KEY NOT NULL,
     bucket     TEXT    NOT NULL,
     prompt     TEXT    NOT NULL,
     status     TEXT    NOT NULL DEFAULT 'active',
     kind       TEXT    NOT NULL DEFAULT 'prompt',
     background INTEGER NOT NULL DEFAULT 0,
     created    REAL,
     dispatched REAL,
     completed  REAL,
     response   TEXT,
     position   INTEGER NOT NULL DEFAULT 0
   )"
  "DDL for the items table.")

(defconst agent-shell-queue-db--insert-item
  "INSERT OR REPLACE INTO items
     (id, bucket, prompt, status, kind, background,
      created, dispatched, completed, response, position)
   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  "Parameterized INSERT for a single queue item.")

(defconst agent-shell-queue-db--select-items
  "SELECT id, bucket, prompt, status, kind, background,
          created, dispatched, completed, response
   FROM items
   ORDER BY bucket, position"
  "SELECT to reconstruct the full items alist on load.")

;;; Connection management

(defun agent-shell-queue-db--default-file ()
  "Return the default SQLite database file path."
  (expand-file-name "agent-shell-queue.db" user-emacs-directory))

(defun agent-shell-queue-db--file ()
  "Return the database file path to use."
  (or agent-shell-queue-db-file (agent-shell-queue-db--default-file)))

(defun agent-shell-queue-db--ensure-connection ()
  "Return the open sqlite connection, opening and initialising it if necessary."
  (unless (fboundp 'sqlite-open)
    (error "agent-shell-queue-db requires Emacs 29+ (built-in sqlite support)"))
  (when (or (null agent-shell-queue-db--connection)
            (not (sqlitep agent-shell-queue-db--connection)))
    (let ((file (agent-shell-queue-db--file)))
      (make-directory (file-name-directory file) t)
      (setq agent-shell-queue-db--connection (sqlite-open file))
      (sqlite-execute agent-shell-queue-db--connection
                      "PRAGMA journal_mode = WAL")
      (sqlite-execute agent-shell-queue-db--connection
                      "PRAGMA busy_timeout = 5000")
      (sqlite-execute agent-shell-queue-db--connection
                      agent-shell-queue-db--create-items)))
  agent-shell-queue-db--connection)

(defun agent-shell-queue-db--close ()
  "Close the SQLite connection if open."
  (when (and agent-shell-queue-db--connection
             (sqlitep agent-shell-queue-db--connection))
    (sqlite-close agent-shell-queue-db--connection))
  (setq agent-shell-queue-db--connection nil))

;;; Save

(defun agent-shell-queue-db--save ()
  "Persist queue items to SQLite, excluding done and running items.
Called as `agent-shell-queue-save-function' when the DB backend is active."
  (let* ((conn (agent-shell-queue-db--ensure-connection))
         (items-to-save
          (cl-remove-if
           (lambda (pair) (null (cdr pair)))
           (mapcar (lambda (pair)
                     (cons (car pair)
                           (cl-remove-if
                            (lambda (item)
                              (memq (agent-shell-queue-item-status item)
                                    '(done running)))
                            (cdr pair))))
                   agent-shell-queue--items))))
    (condition-case err
        (progn
          (sqlite-execute conn "BEGIN TRANSACTION")
          (sqlite-execute conn "DELETE FROM items")
          (dolist (pair items-to-save)
            (let ((pos 0))
              (dolist (item (cdr pair))
                (sqlite-execute
                 conn
                 agent-shell-queue-db--insert-item
                 (list (agent-shell-queue-item-id item)
                       (car pair)
                       (agent-shell-queue-item-prompt item)
                       (symbol-name (agent-shell-queue-item-status item))
                       (symbol-name (or (agent-shell-queue-item-kind item) 'prompt))
                       (if (agent-shell-queue-item-background item) 1 0)
                       (agent-shell-queue-item-created item)
                       (agent-shell-queue-item-dispatched item)
                       (agent-shell-queue-item-completed item)
                       (agent-shell-queue-item-response item)
                       pos))
                (cl-incf pos))))
          (sqlite-execute conn "COMMIT"))
      (error
       (ignore-errors (sqlite-execute conn "ROLLBACK"))
       (error "agent-shell-queue-db: save failed: %s" err))))
  (setq agent-shell-queue--last-flush-time (float-time))
  (when (bound-and-true-p agent-shell-queue-auto-flush-interval)
    (setq agent-shell-queue--next-flush-time
          (time-add (current-time)
                    (seconds-to-time agent-shell-queue-auto-flush-interval)))))

;;; Load

(defun agent-shell-queue-db--load ()
  "Populate `agent-shell-queue--items' from the SQLite database.
Called as `agent-shell-queue-load-function' when the DB backend is active."
  (let ((conn (agent-shell-queue-db--ensure-connection))
        (result nil))
    (dolist (row (sqlite-select conn agent-shell-queue-db--select-items))
      (cl-destructuring-bind
          (id bucket prompt status-str kind-str bg created dispatched completed response)
          row
        (let* ((status (condition-case nil (intern (or status-str "active")) (error 'active)))
               (kind   (condition-case nil (intern (or kind-str "prompt"))   (error 'prompt)))
               (item   (agent-shell-queue-item--make
                        :id id
                        :prompt prompt
                        :status status
                        :kind kind
                        :background (eql bg 1)
                        :created created
                        :dispatched dispatched
                        :completed completed
                        :response response))
               (pair (assoc bucket result)))
          (if pair
              (setcdr pair (append (cdr pair) (list item)))
            (setq result (append result (list (list bucket item))))))))
    (setq agent-shell-queue--items result)))

;;; Enable / disable

;;;###autoload
(defun agent-shell-queue-db-enable (&optional db-file)
  "Activate the SQLite persistence backend for agent-shell-queue.
When DB-FILE is non-nil, use it as the database path; otherwise the default
path under `user-emacs-directory' is used (see `agent-shell-queue-db-file').
Sets `agent-shell-queue-save-function', `agent-shell-queue-load-function',
and `agent-shell-queue-state-file-function' to SQLite-aware variants.
Saves the previous values so `agent-shell-queue-db-disable' can restore them."
  (interactive (list (when current-prefix-arg
                       (read-file-name "SQLite database file: " user-emacs-directory
                                       nil nil "agent-shell-queue.db"))))
  (unless (fboundp 'sqlite-open)
    (error "agent-shell-queue-db requires Emacs 29+ (built-in sqlite support)"))
  (when db-file
    (setq agent-shell-queue-db-file db-file))
  (setq agent-shell-queue-db--saved-state-file-function
        agent-shell-queue-state-file-function
        agent-shell-queue-db--saved-save-function
        agent-shell-queue-save-function
        agent-shell-queue-db--saved-load-function
        agent-shell-queue-load-function)
  (setq agent-shell-queue-state-file-function #'agent-shell-queue-db--file
        agent-shell-queue-save-function       #'agent-shell-queue-db--save
        agent-shell-queue-load-function       #'agent-shell-queue-db--load)
  (agent-shell-queue-db--ensure-connection)
  (message "agent-shell-queue: SQLite backend enabled — %s"
           (agent-shell-queue-db--file)))

;;;###autoload
(defun agent-shell-queue-db-disable ()
  "Deactivate the SQLite backend and revert to the previous persistence settings.
Closes the database connection."
  (interactive)
  (setq agent-shell-queue-state-file-function
        (or agent-shell-queue-db--saved-state-file-function
            #'agent-shell-queue--default-state-file)
        agent-shell-queue-save-function
        agent-shell-queue-db--saved-save-function
        agent-shell-queue-load-function
        agent-shell-queue-db--saved-load-function)
  (agent-shell-queue-db--close)
  (message "agent-shell-queue: SQLite backend disabled"))

;;; Show state

;;;###autoload
(defun agent-shell-queue-db-show-state ()
  "Display the SQLite database contents in a read-only popup buffer.
Shows per-bucket item counts and a tabular dump of all persisted rows."
  (interactive)
  (let* ((conn (agent-shell-queue-db--ensure-connection))
         (counts (sqlite-select
                  conn
                  "SELECT bucket, status, count(*) FROM items GROUP BY bucket, status ORDER BY bucket, status"))
         (rows (sqlite-select conn agent-shell-queue-db--select-items))
         (buf (get-buffer-create "*agent-shell-queue-db*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "SQLite database: %s\n" (agent-shell-queue-db--file)))
        (insert (format "Items stored:    %d\n\n" (length rows)))
        (when counts
          (insert "Bucket summary:\n")
          (dolist (row counts)
            (insert (format "  %-40s %-10s %d\n" (nth 0 row) (nth 1 row) (nth 2 row))))
          (insert "\n"))
        (when rows
          (let ((sep (make-string 100 ?─)))
            (insert (format "%-8s %-30s %-10s %-8s %-6s %s\n"
                            "ID" "Bucket" "Status" "Kind" "BG" "Prompt"))
            (insert sep "\n")
            (dolist (row rows)
              (cl-destructuring-bind
                  (id bucket _prompt status-str kind-str bg _created
                      _dispatched _completed _response)
                  row
                (insert (format "%-8s %-30s %-10s %-8s %-6s %s\n"
                                id
                                (truncate-string-to-width bucket 30 nil nil "…")
                                status-str kind-str
                                (if (eql bg 1) "yes" "no")
                                (truncate-string-to-width (nth 2 row) 40 nil nil "…")))))))
        (goto-char (point-min)))
      (setq buffer-read-only t)
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (let ((inhibit-read-only t))
                      (call-interactively #'agent-shell-queue-db-show-state))))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;; Export

;;;###autoload
(defun agent-shell-queue-db-export ()
  "Export current queue items from SQLite to a YAML buffer.
Loads current state from the DB then delegates to `agent-shell-queue-export'."
  (interactive)
  (unless (fboundp 'yaml-encode)
    (error "agent-shell-queue-db-export requires the `yaml' package"))
  (agent-shell-queue--ensure-loaded)
  (agent-shell-queue-export))

;;; Import

;;;###autoload
(defun agent-shell-queue-db-import (&optional source)
  "Import queue items from YAML into the SQLite database.
SOURCE follows the same convention as `agent-shell-queue-import':
nil or `clipboard' reads from the clipboard; `file' prompts for a file.
After importing, the DB is flushed immediately."
  (interactive (list (if current-prefix-arg 'file 'clipboard)))
  (agent-shell-queue-import source)
  (agent-shell-queue-db--save)
  (message "agent-shell-queue-db: import complete and flushed to database"))

;;; Done-log hook (optional: record completed items in a separate DB table)

(defconst agent-shell-queue-db--create-done
  "CREATE TABLE IF NOT EXISTS done_items (
     id         TEXT    NOT NULL,
     bucket     TEXT    NOT NULL,
     prompt     TEXT    NOT NULL,
     kind       TEXT    NOT NULL DEFAULT 'prompt',
     background INTEGER NOT NULL DEFAULT 0,
     created    REAL,
     dispatched REAL,
     completed  REAL,
     archived   REAL    NOT NULL,
     instance   TEXT
   )"
  "DDL for the optional done_items history table.")

(defun agent-shell-queue-db-enable-done-log ()
  "Create the done_items table and register a hook to record completed items.
This supplements (does not replace) `agent-shell-queue-done-log-file'.
Call after `agent-shell-queue-db-enable'."
  (interactive)
  (let ((conn (agent-shell-queue-db--ensure-connection)))
    (sqlite-execute conn agent-shell-queue-db--create-done))
  (add-hook 'agent-shell-queue-item-done-hook #'agent-shell-queue-db--record-done))

(defvar agent-shell-queue-item-done-hook nil
  "Hook run when a queue item transitions to done status.
Each function is called with two arguments: BUF-NAME and ITEM.")

(defun agent-shell-queue-db--record-done (buf-name item)
  "Insert BUF-NAME / ITEM into the done_items table."
  (when (and agent-shell-queue-db--connection
             (sqlitep agent-shell-queue-db--connection))
    (condition-case err
        (let* ((conn agent-shell-queue-db--connection)
               (instance (let ((n (bound-and-true-p agent-shell-queue-instance-name)))
                           (if (functionp n) (funcall n) (or n "")))))
          (sqlite-execute
           conn
           "INSERT INTO done_items
              (id, bucket, prompt, kind, background, created, dispatched, completed, archived, instance)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
           (list (agent-shell-queue-item-id item)
                 buf-name
                 (agent-shell-queue-item-prompt item)
                 (symbol-name (or (agent-shell-queue-item-kind item) 'prompt))
                 (if (agent-shell-queue-item-background item) 1 0)
                 (agent-shell-queue-item-created item)
                 (agent-shell-queue-item-dispatched item)
                 (agent-shell-queue-item-completed item)
                 (float-time)
                 instance)))
      (error (message "agent-shell-queue-db: done-log write failed: %s" err)))))

(provide 'agent-shell-queue-db)

;;; agent-shell-queue-db.el ends here
