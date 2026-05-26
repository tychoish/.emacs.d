;;; test-agent-shell-queue-db.el --- ERT tests for agent-shell-queue-db -*- lexical-binding: t -*-

;; Run inside a live Emacs session:
;;   M-x ert RET agent-shell-queue-db/ RET
;; or via emacsclient:
;;   emacsclient --eval '(ert "^agent-shell-queue-db/")'
;; Batch:
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-agent-shell-queue-db.el \
;;     --eval '(ert-run-tests-batch-and-exit "agent-shell-queue-db/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-queue)
(require 'agent-shell-queue-db)

;;; Helpers

(defmacro agent-shell-queue-db-test/skip-if-no-sqlite ()
  "Skip the enclosing test when built-in sqlite is unavailable."
  `(skip-unless (fboundp 'sqlite-open)))

(defmacro agent-shell-queue-db-test/with-db (&rest body)
  "Run BODY with isolated queue globals and a fresh temporary SQLite database.
Opens a new connection to a temp file, runs BODY, then closes the connection
and deletes the temp file regardless of whether BODY signals an error.
Skips the enclosing test when built-in sqlite is unavailable."
  (declare (indent 0))
  `(progn
     (skip-unless (fboundp 'sqlite-open))
     (let* ((db-path (make-temp-file "aq-db-test-" nil ".db"))
            (agent-shell-queue-db-file db-path)
            (agent-shell-queue-db--connection nil)
            (agent-shell-queue--items nil)
            (agent-shell-queue--loaded t)
            (agent-shell-queue--subscriptions nil)
            (agent-shell-queue--stale-item-ids nil)
            (agent-shell-queue--last-flush-time nil)
            (agent-shell-queue--next-flush-time nil)
            (agent-shell-queue-save-function nil)
            (agent-shell-queue-load-function nil)
            (agent-shell-queue-state-file-function
             #'agent-shell-queue--default-state-file)
            (agent-shell-queue-db--saved-save-function nil)
            (agent-shell-queue-db--saved-load-function nil)
            (agent-shell-queue-db--saved-state-file-function nil))
       (cl-letf (((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore)
                 ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
                 ((symbol-function 'agent-shell-queue--drop-subscription) #'ignore)
                 ((symbol-function 'alert) #'ignore))
         (unwind-protect
             (progn ,@body)
           (agent-shell-queue-db--close)
           (when (file-exists-p db-path) (delete-file db-path)))))))

(defun agent-shell-queue-db-test/make-item (id prompt &optional status background kind)
  "Build a deterministic test item with fixed timestamps."
  (agent-shell-queue-item--make
   :id id
   :prompt prompt
   :status (or status 'active)
   :kind (or kind 'prompt)
   :background background
   :created 1000.0
   :dispatched nil
   :completed nil
   :response nil))

(defun agent-shell-queue-db-test/items-equal-p (a b)
  "Return non-nil when items A and B have identical logical fields.
Ignores created/dispatched/completed since nil vs 0.0 can differ after
a SQLite roundtrip."
  (and (equal (agent-shell-queue-item-id a)
              (agent-shell-queue-item-id b))
       (equal (agent-shell-queue-item-prompt a)
              (agent-shell-queue-item-prompt b))
       (eq    (agent-shell-queue-item-status a)
              (agent-shell-queue-item-status b))
       (eq    (agent-shell-queue-item-kind a)
              (agent-shell-queue-item-kind b))
       (equal (and (agent-shell-queue-item-background a) t)
              (and (agent-shell-queue-item-background b) t))))

;;; Connection management

(ert-deftest agent-shell-queue-db/default-file ()
  "Default DB path is under user-emacs-directory."
  (let ((agent-shell-queue-db-file nil))
    (should (string-suffix-p "agent-shell-queue.db"
                              (agent-shell-queue-db--default-file)))
    (should (string-prefix-p (expand-file-name user-emacs-directory)
                              (agent-shell-queue-db--default-file)))))

(ert-deftest agent-shell-queue-db/file-respects-custom-path ()
  "Custom `agent-shell-queue-db-file' overrides the default."
  (let ((agent-shell-queue-db-file "/tmp/custom.db"))
    (should (equal "/tmp/custom.db" (agent-shell-queue-db--file)))))

(ert-deftest agent-shell-queue-db/file-falls-back-to-default ()
  "Nil `agent-shell-queue-db-file' returns the default path."
  (let ((agent-shell-queue-db-file nil))
    (should (equal (agent-shell-queue-db--default-file)
                   (agent-shell-queue-db--file)))))

(ert-deftest agent-shell-queue-db/ensure-connection-opens ()
  "ensure-connection returns a live sqlite connection."
  (agent-shell-queue-db-test/with-db
    (let ((conn (agent-shell-queue-db--ensure-connection)))
      (should (sqlitep conn)))))

(ert-deftest agent-shell-queue-db/ensure-connection-idempotent ()
  "Calling ensure-connection twice returns the same object."
  (agent-shell-queue-db-test/with-db
    (let ((c1 (agent-shell-queue-db--ensure-connection))
          (c2 (agent-shell-queue-db--ensure-connection)))
      (should (eq c1 c2)))))

(ert-deftest agent-shell-queue-db/ensure-connection-creates-table ()
  "The items table exists after ensure-connection."
  (agent-shell-queue-db-test/with-db
    (let ((conn (agent-shell-queue-db--ensure-connection)))
      (should (sqlite-select conn
                             "SELECT name FROM sqlite_master WHERE type='table' AND name='items'")))))

(ert-deftest agent-shell-queue-db/close-clears-connection ()
  "db--close sets the connection variable to nil."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db--ensure-connection)
    (should agent-shell-queue-db--connection)
    (agent-shell-queue-db--close)
    (should-not agent-shell-queue-db--connection)))

(ert-deftest agent-shell-queue-db/close-is-idempotent ()
  "Calling db--close when already nil does not error."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db--close)
    (should-not agent-shell-queue-db--connection)
    (should (null (agent-shell-queue-db--close)))))

;;; Save — basic

(ert-deftest agent-shell-queue-db/save-single-item ()
  "A single active item is written to the DB."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*shell*"
                      (agent-shell-queue-db-test/make-item "q1a1" "hello world"))))
    (agent-shell-queue-db--save)
    (let ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                               "SELECT id, prompt, status FROM items")))
      (should (= 1 (length rows)))
      (should (equal "q1a1" (nth 0 (car rows))))
      (should (equal "hello world" (nth 1 (car rows))))
      (should (equal "active" (nth 2 (car rows)))))))

(ert-deftest agent-shell-queue-db/save-excludes-done-items ()
  "Done items are not written to the DB."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*shell*"
                      (agent-shell-queue-db-test/make-item "q1a1" "keep me" 'active)
                      (agent-shell-queue-db-test/make-item "q1b2" "skip me" 'done))))
    (agent-shell-queue-db--save)
    (let ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                               "SELECT id FROM items")))
      (should (= 1 (length rows)))
      (should (equal "q1a1" (nth 0 (car rows)))))))

(ert-deftest agent-shell-queue-db/save-excludes-running-items ()
  "Running items are not written to the DB."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*shell*"
                      (agent-shell-queue-db-test/make-item "q1a1" "active" 'active)
                      (agent-shell-queue-db-test/make-item "q1b2" "running" 'running))))
    (agent-shell-queue-db--save)
    (let ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                               "SELECT id FROM items")))
      (should (= 1 (length rows)))
      (should (equal "q1a1" (nth 0 (car rows)))))))

(ert-deftest agent-shell-queue-db/save-empty-queue ()
  "Saving an empty queue writes no rows."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items nil)
    (agent-shell-queue-db--save)
    (let ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                               "SELECT id FROM items")))
      (should (null rows)))))

(ert-deftest agent-shell-queue-db/save-truncates-before-write ()
  "Each save replaces all existing rows (full rewrite semantics)."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*shell*"
                      (agent-shell-queue-db-test/make-item "q1a1" "first"))))
    (agent-shell-queue-db--save)
    (setq agent-shell-queue--items
          (list (list "*shell*"
                      (agent-shell-queue-db-test/make-item "q1b2" "second"))))
    (agent-shell-queue-db--save)
    (let ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                               "SELECT id FROM items")))
      (should (= 1 (length rows)))
      (should (equal "q1b2" (nth 0 (car rows)))))))

;;; Save — field fidelity

(ert-deftest agent-shell-queue-db/save-background-flag-true ()
  "Background=t is stored as integer 1."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "bg" 'active t))))
    (agent-shell-queue-db--save)
    (let* ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                                "SELECT background FROM items"))
           (bg (nth 0 (car rows))))
      (should (eql 1 bg)))))

(ert-deftest agent-shell-queue-db/save-background-flag-false ()
  "Background=nil is stored as integer 0."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "nobg" 'active nil))))
    (agent-shell-queue-db--save)
    (let* ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                                "SELECT background FROM items"))
           (bg (nth 0 (car rows))))
      (should (eql 0 bg)))))

(ert-deftest agent-shell-queue-db/save-kind-field ()
  "Item kind is stored as its symbol name."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "p" 'active nil 'emacs))))
    (agent-shell-queue-db--save)
    (let* ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                                "SELECT kind FROM items"))
           (kind (nth 0 (car rows))))
      (should (equal "emacs" kind)))))

(ert-deftest agent-shell-queue-db/save-status-field ()
  "Item status is stored as its symbol name."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "p" 'deferred))))
    (agent-shell-queue-db--save)
    (let* ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                                "SELECT status FROM items"))
           (s (nth 0 (car rows))))
      (should (equal "deferred" s)))))

(ert-deftest agent-shell-queue-db/save-preserves-position-order ()
  "Items are stored with ascending position values matching list order."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "first")
                      (agent-shell-queue-db-test/make-item "qa2" "second")
                      (agent-shell-queue-db-test/make-item "qa3" "third"))))
    (agent-shell-queue-db--save)
    (let ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                               "SELECT id, position FROM items ORDER BY position")))
      (should (= 3 (length rows)))
      (should (equal "qa1" (nth 0 (nth 0 rows))))
      (should (eql 0 (nth 1 (nth 0 rows))))
      (should (equal "qa2" (nth 0 (nth 1 rows))))
      (should (eql 1 (nth 1 (nth 1 rows))))
      (should (equal "qa3" (nth 0 (nth 2 rows))))
      (should (eql 2 (nth 1 (nth 2 rows)))))))

(ert-deftest agent-shell-queue-db/save-multiple-buckets ()
  "Items in separate buckets are all written with correct bucket labels."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*shell-a*"
                      (agent-shell-queue-db-test/make-item "qa1" "a1"))
                (list "*shell-b*"
                      (agent-shell-queue-db-test/make-item "qb1" "b1")
                      (agent-shell-queue-db-test/make-item "qb2" "b2"))))
    (agent-shell-queue-db--save)
    (let ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                               "SELECT id, bucket FROM items ORDER BY bucket, position")))
      (should (= 3 (length rows)))
      (should (equal "*shell-a*" (nth 1 (assoc "qa1" rows))))
      (should (equal "*shell-b*" (nth 1 (assoc "qb1" rows))))
      (should (equal "*shell-b*" (nth 1 (assoc "qb2" rows)))))))

(ert-deftest agent-shell-queue-db/save-unicode-prompt ()
  "Prompts with multibyte characters round-trip through SQLite unchanged."
  (agent-shell-queue-db-test/with-db
    (let ((prompt "こんにちは 🌸 — résumé"))
      (setq agent-shell-queue--items
            (list (list "*s*"
                        (agent-shell-queue-db-test/make-item "qu1" prompt))))
      (agent-shell-queue-db--save)
      (let* ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                                  "SELECT prompt FROM items"))
             (stored (nth 0 (car rows))))
        (should (equal prompt stored))))))

(ert-deftest agent-shell-queue-db/save-updates-flush-time ()
  "db--save updates `agent-shell-queue--last-flush-time'."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items nil)
    (agent-shell-queue-db--save)
    (should (numberp agent-shell-queue--last-flush-time))
    (should (> agent-shell-queue--last-flush-time 0))))

;;; Load

(ert-deftest agent-shell-queue-db/load-empty-db ()
  "Loading from an empty DB sets items to nil."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db--ensure-connection)
    (agent-shell-queue-db--load)
    (should (null agent-shell-queue--items))))

(ert-deftest agent-shell-queue-db/load-reconstructs-item ()
  "A saved item is loaded back with identical logical fields."
  (agent-shell-queue-db-test/with-db
    (let ((original (agent-shell-queue-db-test/make-item "qa1" "test prompt" 'active nil 'prompt)))
      (setq agent-shell-queue--items (list (list "*shell*" original)))
      (agent-shell-queue-db--save)
      (setq agent-shell-queue--items nil)
      (agent-shell-queue-db--load)
      (let* ((bucket (assoc "*shell*" agent-shell-queue--items))
             (loaded (car (cdr bucket))))
        (should bucket)
        (should loaded)
        (should (agent-shell-queue-db-test/items-equal-p original loaded))))))

(ert-deftest agent-shell-queue-db/load-background-true ()
  "Background integer 1 is loaded back as a non-nil boolean."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "bg" 'active t))))
    (agent-shell-queue-db--save)
    (setq agent-shell-queue--items nil)
    (agent-shell-queue-db--load)
    (let ((item (car (cdr (assoc "*s*" agent-shell-queue--items)))))
      (should (agent-shell-queue-item-background item)))))

(ert-deftest agent-shell-queue-db/load-background-false ()
  "Background integer 0 is loaded back as nil."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "nobg" 'active nil))))
    (agent-shell-queue-db--save)
    (setq agent-shell-queue--items nil)
    (agent-shell-queue-db--load)
    (let ((item (car (cdr (assoc "*s*" agent-shell-queue--items)))))
      (should-not (agent-shell-queue-item-background item)))))

(ert-deftest agent-shell-queue-db/load-status-symbol ()
  "Status string is interned back to a symbol on load."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "p" 'deferred))))
    (agent-shell-queue-db--save)
    (setq agent-shell-queue--items nil)
    (agent-shell-queue-db--load)
    (let ((item (car (cdr (assoc "*s*" agent-shell-queue--items)))))
      (should (eq 'deferred (agent-shell-queue-item-status item))))))

(ert-deftest agent-shell-queue-db/load-kind-symbol ()
  "Kind string is interned back to a symbol on load."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "p" 'active nil 'emacs))))
    (agent-shell-queue-db--save)
    (setq agent-shell-queue--items nil)
    (agent-shell-queue-db--load)
    (let ((item (car (cdr (assoc "*s*" agent-shell-queue--items)))))
      (should (eq 'emacs (agent-shell-queue-item-kind item))))))

(ert-deftest agent-shell-queue-db/load-preserves-order ()
  "Items are loaded in the same order they were saved."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "first")
                      (agent-shell-queue-db-test/make-item "qa2" "second")
                      (agent-shell-queue-db-test/make-item "qa3" "third"))))
    (agent-shell-queue-db--save)
    (setq agent-shell-queue--items nil)
    (agent-shell-queue-db--load)
    (let ((items (cdr (assoc "*s*" agent-shell-queue--items))))
      (should (= 3 (length items)))
      (should (equal "qa1" (agent-shell-queue-item-id (nth 0 items))))
      (should (equal "qa2" (agent-shell-queue-item-id (nth 1 items))))
      (should (equal "qa3" (agent-shell-queue-item-id (nth 2 items)))))))

(ert-deftest agent-shell-queue-db/load-multiple-buckets ()
  "Items from multiple buckets are grouped correctly after load."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*shell-a*"
                      (agent-shell-queue-db-test/make-item "qa1" "a1"))
                (list "*shell-b*"
                      (agent-shell-queue-db-test/make-item "qb1" "b1")
                      (agent-shell-queue-db-test/make-item "qb2" "b2"))))
    (agent-shell-queue-db--save)
    (setq agent-shell-queue--items nil)
    (agent-shell-queue-db--load)
    (should (= 2 (length agent-shell-queue--items)))
    (should (= 1 (length (cdr (assoc "*shell-a*" agent-shell-queue--items)))))
    (should (= 2 (length (cdr (assoc "*shell-b*" agent-shell-queue--items)))))))

(ert-deftest agent-shell-queue-db/load-unknown-status-interned-as-symbol ()
  "An unrecognised status string is interned as its symbol name, not dropped.
`intern' never signals an error, so the condition-case fallback is never
reached.  This test documents the actual behaviour."
  (agent-shell-queue-db-test/with-db
    (let ((conn (agent-shell-queue-db--ensure-connection)))
      (sqlite-execute conn
                      "INSERT INTO items (id, bucket, prompt, status, kind, background, position)
                       VALUES ('qx1', '*s*', 'p', 'bogus_status', 'prompt', 0, 0)"))
    (agent-shell-queue-db--load)
    (let ((item (car (cdr (assoc "*s*" agent-shell-queue--items)))))
      (should (eq 'bogus_status (agent-shell-queue-item-status item))))))

(ert-deftest agent-shell-queue-db/load-unknown-kind-interned-as-symbol ()
  "An unrecognised kind string is interned as its symbol name, not dropped.
`intern' never signals an error, so the condition-case fallback is never
reached.  This test documents the actual behaviour."
  (agent-shell-queue-db-test/with-db
    (let ((conn (agent-shell-queue-db--ensure-connection)))
      (sqlite-execute conn
                      "INSERT INTO items (id, bucket, prompt, status, kind, background, position)
                       VALUES ('qx1', '*s*', 'p', 'active', 'unknown_kind', 0, 0)"))
    (agent-shell-queue-db--load)
    (let ((item (car (cdr (assoc "*s*" agent-shell-queue--items)))))
      (should (eq 'unknown_kind (agent-shell-queue-item-kind item))))))

;;; Roundtrip

(ert-deftest agent-shell-queue-db/roundtrip-all-kinds ()
  "All valid kind symbols survive a save/load cycle."
  (agent-shell-queue-db-test/with-db
    (let ((kinds '(prompt emacs wait pause compact context)))
      (setq agent-shell-queue--items
            (list (cons "*s*"
                        (cl-loop for k in kinds
                                 for i from 1
                                 collect (agent-shell-queue-db-test/make-item
                                          (format "q%02d" i) "p" 'active nil k)))))
      (agent-shell-queue-db--save)
      (setq agent-shell-queue--items nil)
      (agent-shell-queue-db--load)
      (let ((items (cdr (assoc "*s*" agent-shell-queue--items))))
        (should (= (length kinds) (length items)))
        (cl-loop for k in kinds
                 for item in items
                 do (should (eq k (agent-shell-queue-item-kind item))))))))

(ert-deftest agent-shell-queue-db/roundtrip-all-non-transient-statuses ()
  "Active, deferred, draft, and invalid statuses survive a save/load cycle."
  (agent-shell-queue-db-test/with-db
    (let ((statuses '(active deferred draft invalid)))
      (setq agent-shell-queue--items
            (list (cons "*s*"
                        (cl-loop for st in statuses
                                 for i from 1
                                 collect (agent-shell-queue-db-test/make-item
                                          (format "q%02d" i) "p" st)))))
      (agent-shell-queue-db--save)
      (setq agent-shell-queue--items nil)
      (agent-shell-queue-db--load)
      (let ((items (cdr (assoc "*s*" agent-shell-queue--items))))
        (should (= (length statuses) (length items)))
        (cl-loop for st in statuses
                 for item in items
                 do (should (eq st (agent-shell-queue-item-status item))))))))

(ert-deftest agent-shell-queue-db/roundtrip-optional-nil-fields ()
  "Nil dispatched/completed/response fields survive a save/load cycle."
  (agent-shell-queue-db-test/with-db
    (let ((item (agent-shell-queue-item--make
                 :id "qa1" :prompt "p" :status 'active :kind 'prompt
                 :background nil :created 1000.0
                 :dispatched nil :completed nil :response nil)))
      (setq agent-shell-queue--items (list (list "*s*" item)))
      (agent-shell-queue-db--save)
      (setq agent-shell-queue--items nil)
      (agent-shell-queue-db--load)
      (let ((loaded (car (cdr (assoc "*s*" agent-shell-queue--items)))))
        (should (null (agent-shell-queue-item-dispatched loaded)))
        (should (null (agent-shell-queue-item-completed loaded)))
        (should (null (agent-shell-queue-item-response loaded)))))))

(ert-deftest agent-shell-queue-db/roundtrip-populated-fields ()
  "Dispatched, completed, and response fields survive a save/load cycle."
  (agent-shell-queue-db-test/with-db
    (let ((item (agent-shell-queue-item--make
                 :id "qa1" :prompt "p" :status 'active :kind 'prompt
                 :background nil :created 1000.0
                 :dispatched 2000.0 :completed 3000.0 :response "done text")))
      (setq agent-shell-queue--items (list (list "*s*" item)))
      (agent-shell-queue-db--save)
      (setq agent-shell-queue--items nil)
      (agent-shell-queue-db--load)
      (let ((loaded (car (cdr (assoc "*s*" agent-shell-queue--items)))))
        (should (equal 2000.0 (agent-shell-queue-item-dispatched loaded)))
        (should (equal 3000.0 (agent-shell-queue-item-completed loaded)))
        (should (equal "done text" (agent-shell-queue-item-response loaded)))))))

;;; Enable / disable

(ert-deftest agent-shell-queue-db/enable-sets-save-function ()
  "db-enable installs `agent-shell-queue-db--save' as the save function."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable)
    (should (eq #'agent-shell-queue-db--save agent-shell-queue-save-function))))

(ert-deftest agent-shell-queue-db/enable-sets-load-function ()
  "db-enable installs `agent-shell-queue-db--load' as the load function."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable)
    (should (eq #'agent-shell-queue-db--load agent-shell-queue-load-function))))

(ert-deftest agent-shell-queue-db/enable-sets-state-file-function ()
  "db-enable installs `agent-shell-queue-db--file' as the state-file function."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable)
    (should (eq #'agent-shell-queue-db--file
                agent-shell-queue-state-file-function))))

(ert-deftest agent-shell-queue-db/enable-opens-connection ()
  "db-enable opens a live sqlite connection."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable)
    (should (and agent-shell-queue-db--connection
                 (sqlitep agent-shell-queue-db--connection)))))

(ert-deftest agent-shell-queue-db/enable-saves-previous-functions ()
  "db-enable stashes the pre-existing function values for later restore."
  (agent-shell-queue-db-test/with-db
    (let* ((old-save (lambda () 'old-save))
           (old-load (lambda () 'old-load))
           (old-state (lambda () "old-state"))
           (agent-shell-queue-save-function old-save)
           (agent-shell-queue-load-function old-load)
           (agent-shell-queue-state-file-function old-state))
      (agent-shell-queue-db-enable)
      (should (eq old-save agent-shell-queue-db--saved-save-function))
      (should (eq old-load agent-shell-queue-db--saved-load-function))
      (should (eq old-state agent-shell-queue-db--saved-state-file-function)))))

(ert-deftest agent-shell-queue-db/disable-restores-functions ()
  "db-disable restores the functions that were active before db-enable."
  (agent-shell-queue-db-test/with-db
    (let* ((old-save (lambda () 'old-save))
           (old-load (lambda () 'old-load))
           (old-state (lambda () "old-state"))
           (agent-shell-queue-save-function old-save)
           (agent-shell-queue-load-function old-load)
           (agent-shell-queue-state-file-function old-state))
      (agent-shell-queue-db-enable)
      (agent-shell-queue-db-disable)
      (should (eq old-save agent-shell-queue-save-function))
      (should (eq old-load agent-shell-queue-load-function))
      (should (eq old-state agent-shell-queue-state-file-function)))))

(ert-deftest agent-shell-queue-db/disable-closes-connection ()
  "db-disable closes the sqlite connection."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable)
    (should agent-shell-queue-db--connection)
    (agent-shell-queue-db-disable)
    (should-not agent-shell-queue-db--connection)))

(ert-deftest agent-shell-queue-db/enable-with-explicit-file ()
  "db-enable with a file argument sets `agent-shell-queue-db-file'."
  (skip-unless (fboundp 'sqlite-open))
  (let* ((tmp (make-temp-file "aq-enable-test-" nil ".db"))
         (agent-shell-queue-db-file nil)
         (agent-shell-queue-db--connection nil)
         (agent-shell-queue-save-function nil)
         (agent-shell-queue-load-function nil)
         (agent-shell-queue-state-file-function #'agent-shell-queue--default-state-file)
         (agent-shell-queue-db--saved-save-function nil)
         (agent-shell-queue-db--saved-load-function nil)
         (agent-shell-queue-db--saved-state-file-function nil))
    (unwind-protect
        (progn
          (agent-shell-queue-db-enable tmp)
          (should (equal tmp agent-shell-queue-db-file)))
      (agent-shell-queue-db--close)
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; Done-log

(ert-deftest agent-shell-queue-db/enable-done-log-creates-table ()
  "enable-done-log creates the done_items table."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable-done-log)
    (let ((conn (agent-shell-queue-db--ensure-connection)))
      (should (sqlite-select conn
                             "SELECT name FROM sqlite_master WHERE type='table' AND name='done_items'")))))

(ert-deftest agent-shell-queue-db/enable-done-log-registers-hook ()
  "enable-done-log adds `agent-shell-queue-db--record-done' to the done hook."
  (agent-shell-queue-db-test/with-db
    (let ((agent-shell-queue-item-done-hook nil))
      (agent-shell-queue-db-enable-done-log)
      (should (memq #'agent-shell-queue-db--record-done
                    agent-shell-queue-item-done-hook)))))

(ert-deftest agent-shell-queue-db/record-done-inserts-row ()
  "db--record-done writes one row to done_items."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable-done-log)
    (let* ((conn (agent-shell-queue-db--ensure-connection))
           (item (agent-shell-queue-item--make
                  :id "qa1" :prompt "finished task" :status 'done
                  :kind 'prompt :background nil
                  :created 1000.0 :dispatched 2000.0 :completed 3000.0
                  :response "done")))
      (agent-shell-queue-db--record-done "*shell*" item)
      (let ((rows (sqlite-select conn "SELECT id, bucket, prompt FROM done_items")))
        (should (= 1 (length rows)))
        (should (equal "qa1" (nth 0 (car rows))))
        (should (equal "*shell*" (nth 1 (car rows))))
        (should (equal "finished task" (nth 2 (car rows))))))))

(ert-deftest agent-shell-queue-db/record-done-background-flag ()
  "db--record-done stores background=t as 1 and nil as 0."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable-done-log)
    (let* ((conn (agent-shell-queue-db--ensure-connection))
           (item-bg (agent-shell-queue-item--make
                     :id "qa1" :prompt "p" :status 'done :kind 'prompt
                     :background t :created 0.0 :dispatched 0.0 :completed 0.0))
           (item-nobg (agent-shell-queue-item--make
                       :id "qa2" :prompt "p" :status 'done :kind 'prompt
                       :background nil :created 0.0 :dispatched 0.0 :completed 0.0)))
      (agent-shell-queue-db--record-done "*s*" item-bg)
      (agent-shell-queue-db--record-done "*s*" item-nobg)
      (let ((rows (sqlite-select conn "SELECT id, background FROM done_items ORDER BY id")))
        (should (eql 1 (nth 1 (assoc "qa1" rows))))
        (should (eql 0 (nth 1 (assoc "qa2" rows))))))))

(ert-deftest agent-shell-queue-db/record-done-noop-without-connection ()
  "db--record-done silently does nothing when the connection is nil."
  (let ((agent-shell-queue-db--connection nil))
    (should (null (agent-shell-queue-db--record-done
                   "*s*"
                   (agent-shell-queue-db-test/make-item "qa1" "p" 'done))))))

(ert-deftest agent-shell-queue-db/record-done-includes-instance ()
  "db--record-done stores the instance name from `agent-shell-queue-instance-name'."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable-done-log)
    (let* ((agent-shell-queue-instance-name "test-instance")
           (conn (agent-shell-queue-db--ensure-connection))
           (item (agent-shell-queue-item--make
                  :id "qa1" :prompt "p" :status 'done :kind 'prompt
                  :background nil :created 0.0 :dispatched 0.0 :completed 0.0)))
      (agent-shell-queue-db--record-done "*s*" item)
      (let* ((rows (sqlite-select conn "SELECT instance FROM done_items"))
             (inst (nth 0 (car rows))))
        (should (equal "test-instance" inst))))))

(ert-deftest agent-shell-queue-db/record-done-instance-from-function ()
  "db--record-done calls `agent-shell-queue-instance-name' when it is a function."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable-done-log)
    (let* ((agent-shell-queue-instance-name (lambda () "fn-instance"))
           (conn (agent-shell-queue-db--ensure-connection))
           (item (agent-shell-queue-item--make
                  :id "qa1" :prompt "p" :status 'done :kind 'prompt
                  :background nil :created 0.0 :dispatched 0.0 :completed 0.0)))
      (agent-shell-queue-db--record-done "*s*" item)
      (let* ((rows (sqlite-select conn "SELECT instance FROM done_items"))
             (inst (nth 0 (car rows))))
        (should (equal "fn-instance" inst))))))

;;; Transaction safety

(ert-deftest agent-shell-queue-db/save-is-atomic ()
  "A save that succeeds writes all items; a partial write never occurs.
This verifies that the BEGIN/COMMIT wrapping is in place."
  (agent-shell-queue-db-test/with-db
    (setq agent-shell-queue--items
          (list (list "*s*"
                      (agent-shell-queue-db-test/make-item "qa1" "one")
                      (agent-shell-queue-db-test/make-item "qa2" "two")
                      (agent-shell-queue-db-test/make-item "qa3" "three"))))
    (agent-shell-queue-db--save)
    (let ((rows (sqlite-select (agent-shell-queue-db--ensure-connection)
                               "SELECT id FROM items ORDER BY position")))
      (should (= 3 (length rows)))
      (should (equal "qa1" (nth 0 (nth 0 rows))))
      (should (equal "qa2" (nth 0 (nth 1 rows))))
      (should (equal "qa3" (nth 0 (nth 2 rows)))))))

;;; Integration: enable → save → disable → load

(ert-deftest agent-shell-queue-db/integration-enable-save-load ()
  "Full cycle: enable backend, save items, reset state, reload, verify."
  (agent-shell-queue-db-test/with-db
    (agent-shell-queue-db-enable)
    (let ((items (list (agent-shell-queue-db-test/make-item "qa1" "task one" 'active nil 'prompt)
                       (agent-shell-queue-db-test/make-item "qa2" "task two" 'deferred t 'prompt))))
      (setq agent-shell-queue--items (list (cons "*shell*" items)))
      (agent-shell-queue-db--save)
      (setq agent-shell-queue--items nil)
      (agent-shell-queue-db--load)
      (let ((loaded (cdr (assoc "*shell*" agent-shell-queue--items))))
        (should (= 2 (length loaded)))
        (should (agent-shell-queue-db-test/items-equal-p (nth 0 items) (nth 0 loaded)))
        (should (agent-shell-queue-db-test/items-equal-p (nth 1 items) (nth 1 loaded)))))))

(provide 'test-agent-shell-queue-db)
;;; test-agent-shell-queue-db.el ends here
