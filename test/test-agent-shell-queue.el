;;; test-agent-shell-queue.el --- ERT tests for agent-shell-queue -*- lexical-binding: t -*-

;; Run inside a live Emacs session with the full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^agent-shell-queue/")
;;
;; Batch run (requires agent-shell on the load path):
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-agent-shell-queue.el \
;;     --eval '(ert-run-tests-batch-and-exit "agent-shell-queue/")'

(require 'ert)
(require 'cl-lib)

(defvar agent-shell-queue-test--load-path
  (expand-file-name "lisp" (file-name-directory
                            (directory-file-name
                             (file-name-directory
                              (or load-file-name buffer-file-name))))))

(unless (featurep 'agent-shell-queue)
  (add-to-list 'load-path agent-shell-queue-test--load-path)
  (require 'agent-shell-queue))

;;; Test helpers

(defmacro agent-shell-queue-test/isolate (&rest body)
  "Execute BODY with fresh, isolated queue globals.
Mocks out disk I/O and buffer refresh so tests stay pure."
  `(let ((agent-shell-queue--items nil)
         (agent-shell-queue--counter 0)
         (agent-shell-queue--loaded t)
         (agent-shell-queue--subscriptions nil)
         (agent-shell-queue--editing-ids nil)
         (agent-shell-queue--buffer-paused nil)
         (agent-shell-queue-paused nil)
         (agent-shell-queue--last-flush-time nil))
     (cl-letf (((symbol-function 'agent-shell-queue--save) #'ignore)
               ((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore)
               ((symbol-function 'alert) #'ignore))
       ,@body)))

(defmacro agent-shell-queue-test/isolate-no-sub (&rest body)
  "Like `agent-shell-queue-test/isolate' but also stubs subscription management."
  `(agent-shell-queue-test/isolate
    (cl-letf (((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
              ((symbol-function 'agent-shell-queue--drop-subscription) #'ignore))
      ,@body)))

(defun agent-shell-queue-test/make-item (id prompt &optional status background)
  "Build a test item directly, bypassing the counter."
  (agent-shell-queue-item--make
   :id id
   :prompt prompt
   :status (or status 'active)
   :background background
   :created 1000.0
   :dispatched nil
   :completed nil))

(defun agent-shell-queue-test/populate (&rest specs)
  "Return a fresh `agent-shell-queue--items' alist from SPECS.
Each spec is (BUF-NAME (ID PROMPT STATUS BACKGROUND) ...)."
  (let (result)
    (dolist (spec specs)
      (let ((buf-name (car spec))
            (items (mapcar (lambda (i)
                             (agent-shell-queue-test/make-item
                              (nth 0 i) (nth 1 i) (nth 2 i) (nth 3 i)))
                           (cdr spec))))
        (push (cons buf-name items) result)))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--format-age (pure)

(ert-deftest agent-shell-queue/format-age-seconds ()
  (should (equal "30s" (agent-shell-queue--format-age 30.0))))

(ert-deftest agent-shell-queue/format-age-minutes ()
  (should (equal "5m" (agent-shell-queue--format-age (* 5 60.0)))))

(ert-deftest agent-shell-queue/format-age-hours ()
  (should (equal "2h" (agent-shell-queue--format-age (* 2 3600.0)))))

(ert-deftest agent-shell-queue/format-age-days ()
  (should (equal "3d" (agent-shell-queue--format-age (* 3 86400.0)))))

(ert-deftest agent-shell-queue/format-age-boundary-minute ()
  "59 seconds is still seconds; 60 seconds is 1m."
  (should (equal "59s" (agent-shell-queue--format-age 59.0)))
  (should (equal "1m"  (agent-shell-queue--format-age 60.0))))

(ert-deftest agent-shell-queue/format-age-boundary-hour ()
  (should (equal "59m" (agent-shell-queue--format-age (- 3600.0 60))))
  (should (equal "1h"  (agent-shell-queue--format-age 3600.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--status-string (pure)

(ert-deftest agent-shell-queue/status-string-active ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (should (equal "active" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-deferred ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'deferred nil)))
      (should (equal "deferred" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-active-background ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active t)))
      (should (equal "active/bg" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-deferred-background ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'deferred t)))
      (should (equal "deferred/bg" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-running ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'running nil)))
      (should (equal "running" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-running-background ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'running t)))
      (should (equal "running/bg" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-done ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'done nil)))
      (should (equal "done" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-editing ()
  "Items with ID in editing-ids show 'editing' regardless of their status."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (setq agent-shell-queue--editing-ids '("q-1"))
      (should (equal "editing" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-buf-blocked ()
  "Active item in a buffer-paused buffer shows 'buf-blocked'."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (setq agent-shell-queue--buffer-paused '("paused-buf"))
      (should (equal "buf-blocked"
                     (agent-shell-queue--status-string item "paused-buf"))))))

(ert-deftest agent-shell-queue/status-string-buf-blocked-not-deferred ()
  "Deferred items are not shown as buf-blocked even if buffer is paused."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'deferred nil)))
      (setq agent-shell-queue--buffer-paused '("paused-buf"))
      (should (equal "deferred"
                     (agent-shell-queue--status-string item "paused-buf"))))))

(ert-deftest agent-shell-queue/status-string-editing-takes-priority-over-blocked ()
  "Editing status takes priority over buf-blocked."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (setq agent-shell-queue--editing-ids '("q-1"))
      (setq agent-shell-queue--buffer-paused '("paused-buf"))
      (should (equal "editing"
                     (agent-shell-queue--status-string item "paused-buf"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--activity-state

(ert-deftest agent-shell-queue/activity-state-paused ()
  "Global pause overrides everything."
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue-paused t)
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("b" ("q-1" "p" running nil))))
    (should (equal "PAUSED"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

(ert-deftest agent-shell-queue/activity-state-running ()
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("b" ("q-1" "p" running nil))))
    (should (equal "running"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

(ert-deftest agent-shell-queue/activity-state-waiting ()
  "Active items with none running → waiting."
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("b" ("q-1" "p" active nil))))
    (should (equal "waiting"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

(ert-deftest agent-shell-queue/activity-state-idle ()
  "Empty queue → idle."
  (agent-shell-queue-test/isolate
    (should (equal "idle"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

(ert-deftest agent-shell-queue/activity-state-idle-only-done ()
  "Only done items → idle."
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("b" ("q-1" "p" done nil))))
    (should (equal "idle"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-instance-name

(ert-deftest agent-shell-queue/instance-name-string ()
  "A string value is returned directly."
  (let ((agent-shell-queue-instance-name "my-instance"))
    (should (equal "my-instance"
                   (let ((n agent-shell-queue-instance-name))
                     (if (functionp n) (funcall n) n))))))

(ert-deftest agent-shell-queue/instance-name-function ()
  "A function value is called to produce the name."
  (let ((agent-shell-queue-instance-name (lambda () "from-fn")))
    (should (equal "from-fn"
                   (let ((n agent-shell-queue-instance-name))
                     (if (functionp n) (funcall n) n))))))

(ert-deftest agent-shell-queue/instance-name-symbol-function ()
  "A function symbol is also callable."
  (let ((agent-shell-queue-instance-name #'agent-shell-queue--default-instance-name))
    (should (stringp (let ((n agent-shell-queue-instance-name))
                       (if (functionp n) (funcall n) n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--make-item

(ert-deftest agent-shell-queue/make-item-increments-counter ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue--make-item "hello")))
      (should (equal "q-1" (agent-shell-queue-item-id item)))
      (should (= 1 agent-shell-queue--counter))
      (agent-shell-queue--make-item "world")
      (should (= 2 agent-shell-queue--counter)))))

(ert-deftest agent-shell-queue/make-item-defaults ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue--make-item "hello")))
      (should (equal "hello" (agent-shell-queue-item-prompt item)))
      (should (eq 'active (agent-shell-queue-item-status item)))
      (should (null (agent-shell-queue-item-background item)))
      (should (null (agent-shell-queue-item-dispatched item)))
      (should (null (agent-shell-queue-item-completed item)))
      (should (numberp (agent-shell-queue-item-created item))))))

(ert-deftest agent-shell-queue/make-item-background-flag ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue--make-item "hello" t)))
      (should (agent-shell-queue-item-background item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--item-by-id

(ert-deftest agent-shell-queue/item-by-id-found ()
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "first" active nil) ("q-2" "second" active nil))
           '("buf2" ("q-3" "third" active nil))))
    (let ((result (agent-shell-queue--item-by-id "q-2")))
      (should result)
      (should (equal "buf1" (car result)))
      (should (equal "q-2" (agent-shell-queue-item-id (cdr result))))
      (should (equal "second" (agent-shell-queue-item-prompt (cdr result)))))))

(ert-deftest agent-shell-queue/item-by-id-not-found ()
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "first" active nil))))
    (should-not (agent-shell-queue--item-by-id "q-99"))))

(ert-deftest agent-shell-queue/item-by-id-empty-queue ()
  (agent-shell-queue-test/isolate
    (should-not (agent-shell-queue--item-by-id "q-1"))))

(ert-deftest agent-shell-queue/item-by-id-across-buckets ()
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil))
           '("buf2" ("q-2" "b" active nil))
           '("buf3" ("q-3" "c" active nil))))
    (should (equal "buf3" (car (agent-shell-queue--item-by-id "q-3"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-add

(ert-deftest agent-shell-queue/add-creates-bucket ()
  (agent-shell-queue-test/isolate-no-sub
    (let* ((buf (get-buffer-create " *asq-test-buf*"))
           (item (agent-shell-queue-add "hello" buf)))
      (unwind-protect
          (progn
            (should (agent-shell-queue-item-p item))
            (should (equal "hello" (agent-shell-queue-item-prompt item)))
            (should (eq 'active (agent-shell-queue-item-status item)))
            (should (= 1 (length agent-shell-queue--items)))
            (should (equal (buffer-name buf) (caar agent-shell-queue--items)))
            (should (= 1 (length (cdar agent-shell-queue--items)))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/add-appends-to-existing-bucket ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-test-buf*")))
      (unwind-protect
          (progn
            (agent-shell-queue-add "first" buf)
            (agent-shell-queue-add "second" buf)
            (should (= 1 (length agent-shell-queue--items)))
            (let ((items (cdar agent-shell-queue--items)))
              (should (= 2 (length items)))
              (should (equal "first"  (agent-shell-queue-item-prompt (nth 0 items))))
              (should (equal "second" (agent-shell-queue-item-prompt (nth 1 items))))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/add-background-flag-propagated ()
  (agent-shell-queue-test/isolate-no-sub
    (let* ((buf (get-buffer-create " *asq-test-buf*"))
           (item (agent-shell-queue-add "hello" buf t)))
      (unwind-protect
          (should (agent-shell-queue-item-background item))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/add-multiple-buffers ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf1 (get-buffer-create " *asq-test-buf1*"))
          (buf2 (get-buffer-create " *asq-test-buf2*")))
      (unwind-protect
          (progn
            (agent-shell-queue-add "for-one" buf1)
            (agent-shell-queue-add "for-two" buf2)
            (should (= 2 (length agent-shell-queue--items))))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

(ert-deftest agent-shell-queue/add-calls-ensure-subscription ()
  (agent-shell-queue-test/isolate
    (let ((subscribed-to nil)
          (buf (get-buffer-create " *asq-test-buf*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-queue--ensure-subscription)
                     (lambda (b) (setq subscribed-to b))))
            (agent-shell-queue-add "hello" buf)
            (should (eq buf subscribed-to)))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-remove

(ert-deftest agent-shell-queue/remove-single-item ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-remove "q-1")
    (should-not agent-shell-queue--items)))

(ert-deftest agent-shell-queue/remove-one-of-many ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-remove "q-2")
    (let ((items (cdar agent-shell-queue--items)))
      (should (= 2 (length items)))
      (should (equal "q-1" (agent-shell-queue-item-id (nth 0 items))))
      (should (equal "q-3" (agent-shell-queue-item-id (nth 1 items)))))))

(ert-deftest agent-shell-queue/remove-last-item-drops-subscription ()
  (agent-shell-queue-test/isolate
    (let ((dropped nil))
      (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription)
                 (lambda (name) (setq dropped name)))
                ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
        (setq agent-shell-queue--items
              (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
        (agent-shell-queue-remove "q-1")
        (should (equal "buf1" dropped))))))

(ert-deftest agent-shell-queue/remove-not-last-preserves-subscription ()
  (agent-shell-queue-test/isolate
    (let ((dropped nil))
      (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription)
                 (lambda (name) (setq dropped name)))
                ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
        (setq agent-shell-queue--items
              (agent-shell-queue-test/populate
               '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
        (agent-shell-queue-remove "q-1")
        (should-not dropped)))))

(ert-deftest agent-shell-queue/remove-unknown-id-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-remove "q-999")
    (should (= 1 (length agent-shell-queue--items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-defer

(ert-deftest agent-shell-queue/defer-active-to-deferred ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-defer "q-1")
    (should (eq 'deferred (agent-shell-queue-item-status (cadar agent-shell-queue--items))))))

(ert-deftest agent-shell-queue/defer-deferred-to-active ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" deferred nil))))
    (agent-shell-queue-defer "q-1")
    (should (eq 'active (agent-shell-queue-item-status (cadar agent-shell-queue--items))))))

(ert-deftest agent-shell-queue/defer-twice-returns-to-active ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-defer "q-1")
    (agent-shell-queue-defer "q-1")
    (should (eq 'active (agent-shell-queue-item-status (cadar agent-shell-queue--items))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-toggle-background

(ert-deftest agent-shell-queue/toggle-background-on ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-toggle-background "q-1")
    (should (agent-shell-queue-item-background (cadar agent-shell-queue--items)))))

(ert-deftest agent-shell-queue/toggle-background-off ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active t))))
    (agent-shell-queue-toggle-background "q-1")
    (should-not (agent-shell-queue-item-background (cadar agent-shell-queue--items)))))

(ert-deftest agent-shell-queue/toggle-background-idempotent ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-toggle-background "q-1")
    (agent-shell-queue-toggle-background "q-1")
    (should-not (agent-shell-queue-item-background (cadar agent-shell-queue--items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-edit

(ert-deftest agent-shell-queue/edit-replaces-prompt ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "old" active nil))))
    (agent-shell-queue-edit "q-1" "new prompt")
    (should (equal "new prompt"
                   (agent-shell-queue-item-prompt (cadar agent-shell-queue--items))))))

(ert-deftest agent-shell-queue/edit-unknown-id-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "old" active nil))))
    (agent-shell-queue-edit "q-999" "new")
    (should (equal "old"
                   (agent-shell-queue-item-prompt (cadar agent-shell-queue--items))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--move / move-up / move-down

(ert-deftest agent-shell-queue/move-up-middle ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-move-up "q-3")
    (should (equal '("q-1" "q-3" "q-2")
                   (mapcar #'agent-shell-queue-item-id (cdar agent-shell-queue--items))))))

(ert-deftest agent-shell-queue/move-down-middle ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-move-down "q-1")
    (should (equal '("q-2" "q-1" "q-3")
                   (mapcar #'agent-shell-queue-item-id (cdar agent-shell-queue--items))))))

(ert-deftest agent-shell-queue/move-up-at-top-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
    (agent-shell-queue-move-up "q-1")
    (should (equal '("q-1" "q-2")
                   (mapcar #'agent-shell-queue-item-id (cdar agent-shell-queue--items))))))

(ert-deftest agent-shell-queue/move-down-at-bottom-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
    (agent-shell-queue-move-down "q-2")
    (should (equal '("q-1" "q-2")
                   (mapcar #'agent-shell-queue-item-id (cdar agent-shell-queue--items))))))

(ert-deftest agent-shell-queue/move-only-item-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "a" active nil))))
    (agent-shell-queue-move-up "q-1")
    (agent-shell-queue-move-down "q-1")
    (should (equal '("q-1")
                   (mapcar #'agent-shell-queue-item-id (cdar agent-shell-queue--items))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pause: global and per-buffer

(ert-deftest agent-shell-queue/toggle-pause-sets-flag ()
  (agent-shell-queue-test/isolate
    (should-not agent-shell-queue-paused)
    (agent-shell-queue-toggle-pause)
    (should agent-shell-queue-paused)
    (agent-shell-queue-toggle-pause)
    (should-not agent-shell-queue-paused)))

(ert-deftest agent-shell-queue/buffer-toggle-pause-adds-name ()
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-pause-test*")))
      (unwind-protect
          (progn
            (agent-shell-queue-buffer-toggle-buffer-pause buf)
            (should (member (buffer-name buf) agent-shell-queue--buffer-paused)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/buffer-toggle-pause-removes-name ()
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-pause-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--buffer-paused (list (buffer-name buf)))
            (agent-shell-queue-buffer-toggle-buffer-pause buf)
            (should-not (member (buffer-name buf) agent-shell-queue--buffer-paused)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/unpause-all-clears-list ()
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--buffer-paused '("buf1" "buf2" "buf3"))
    (agent-shell-queue-unpause-all-buffers)
    (should-not agent-shell-queue--buffer-paused)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dispatch: editing-ids and buffer-paused prevent sends

(ert-deftest agent-shell-queue/send-next-skips-editing-item ()
  "An item in agent-shell-queue--editing-ids is never dispatched."
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-edit-skip-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "edit me" 'active nil))))
            (setq agent-shell-queue--editing-ids '("q-1"))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-next-skips-buffer-paused ()
  "Items targeting a buffer-paused buffer are not dispatched."
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-buf-pause-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "p" 'active nil))))
            (setq agent-shell-queue--buffer-paused (list (buffer-name buf)))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-next-skips-when-globally-paused ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-global-pause-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue-paused t)
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "p" 'active nil))))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-send-item

(ert-deftest agent-shell-queue/send-item-marks-running-and-sets-dispatched ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-send-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "prompt" 'active nil))))
            (cl-letf (((symbol-function 'agent-shell-insert) #'ignore)
                      ((symbol-function 'buffer-live-p) (lambda (_) t)))
              (agent-shell-queue-send-item "q-1")
              (let ((item (cadar agent-shell-queue--items)))
                (should (eq 'running (agent-shell-queue-item-status item)))
                (should (numberp (agent-shell-queue-item-dispatched item))))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-item-calls-insert-with-prompt ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((inserted-text nil)
          (inserted-buf nil)
          (buf (get-buffer-create " *asq-send-test2*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "the prompt" 'active nil))))
            (cl-letf (((symbol-function 'agent-shell-insert)
                       (lambda (&rest args)
                         (setq inserted-text (plist-get args :text)
                               inserted-buf  (plist-get args :shell-buffer))))
                      ((symbol-function 'buffer-live-p) (lambda (_) t)))
              (agent-shell-queue-send-item "q-1")
              (should (equal "the prompt" inserted-text))
              (should (eq buf inserted-buf))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-item-background-wraps-prompt ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((inserted-text nil)
          (agent-shell-queue-background-prefix "/bg ")
          (buf (get-buffer-create " *asq-bg-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "do thing" 'active t))))
            (cl-letf (((symbol-function 'agent-shell-insert)
                       (lambda (&rest args)
                         (setq inserted-text (plist-get args :text))))
                      ((symbol-function 'buffer-live-p) (lambda (_) t)))
              (agent-shell-queue-send-item "q-1")
              (should (equal "/bg do thing" inserted-text))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-item-dead-buffer-errors ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("dead-buf" ("q-1" "hello" active nil))))
    (cl-letf (((symbol-function 'get-buffer) (lambda (_) nil)))
      (should-error (agent-shell-queue-send-item "q-1") :type 'user-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--mark-running-done

(ert-deftest agent-shell-queue/mark-running-done-sets-status ()
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (should (eq 'done (agent-shell-queue-item-status (cadar agent-shell-queue--items))))))

(ert-deftest agent-shell-queue/mark-running-done-sets-completed-time ()
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (should (numberp (agent-shell-queue-item-completed (cadar agent-shell-queue--items))))))

(ert-deftest agent-shell-queue/mark-running-done-only-running-items ()
  "Active items are not affected."
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "p" running nil) ("q-2" "p2" active nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (let ((items (cdar agent-shell-queue--items)))
      (should (eq 'done   (agent-shell-queue-item-status (nth 0 items))))
      (should (eq 'active (agent-shell-queue-item-status (nth 1 items)))))))

(ert-deftest agent-shell-queue/mark-running-done-alert-only-when-marked ()
  "Alert fires only when at least one item was actually marked done."
  (agent-shell-queue-test/isolate
    (let ((alert-fired nil))
      (cl-letf (((symbol-function 'agent-shell-queue--alert-if-empty)
                 (lambda () (setq alert-fired t))))
        ;; No running items — mark should NOT fire alert
        (setq agent-shell-queue--items
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" active nil))))
        (agent-shell-queue--mark-running-done "buf1")
        (should-not alert-fired)
        ;; Running item — mark SHOULD fire alert
        (setq agent-shell-queue--items
              (agent-shell-queue-test/populate '("buf1" ("q-2" "p" running nil))))
        (agent-shell-queue--mark-running-done "buf1")
        (should alert-fired)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--alert-if-empty

(ert-deftest agent-shell-queue/alert-if-empty-fires-when-no-work ()
  (agent-shell-queue-test/isolate
    (let ((alerted nil))
      (cl-letf (((symbol-function 'alert)
                 (lambda (&rest _) (setq alerted t))))
        (setq agent-shell-queue--items
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" done nil))))
        (agent-shell-queue--alert-if-empty)
        (should alerted)))))

(ert-deftest agent-shell-queue/alert-if-empty-silent-when-active-work ()
  (agent-shell-queue-test/isolate
    (let ((alerted nil))
      (cl-letf (((symbol-function 'alert)
                 (lambda (&rest _) (setq alerted t))))
        (setq agent-shell-queue--items
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" active nil))))
        (agent-shell-queue--alert-if-empty)
        (should-not alerted)))))

(ert-deftest agent-shell-queue/alert-if-empty-silent-when-running ()
  (agent-shell-queue-test/isolate
    (let ((alerted nil))
      (cl-letf (((symbol-function 'alert)
                 (lambda (&rest _) (setq alerted t))))
        (setq agent-shell-queue--items
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
        (agent-shell-queue--alert-if-empty)
        (should-not alerted)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-reenqueue

(ert-deftest agent-shell-queue/reenqueue-creates-new-item ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-reenq-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "done prompt" 'done nil))))
            (setf (agent-shell-queue-item-completed (cadar agent-shell-queue--items)) 2000.0)
            (agent-shell-queue-reenqueue "q-1")
            ;; Original done item still there + new active item
            (let ((items (cdar agent-shell-queue--items)))
              (should (= 2 (length items)))
              (let ((new-item (nth 1 items)))
                (should (equal "done prompt" (agent-shell-queue-item-prompt new-item)))
                (should (eq 'active (agent-shell-queue-item-status new-item))))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/reenqueue-non-done-errors ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-reenq-err-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "active" 'active nil))))
            (should-error (agent-shell-queue-reenqueue "q-1") :type 'user-error))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--retarget-item

(ert-deftest agent-shell-queue/retarget-moves-item ()
  (agent-shell-queue-test/isolate
    (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription) #'ignore)
              ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
      (agent-shell-queue--retarget-item "q-1" "buf2")
      (should-not (assoc "buf1" agent-shell-queue--items))
      (let ((bucket (assoc "buf2" agent-shell-queue--items)))
        (should bucket)
        (should (equal "q-1" (agent-shell-queue-item-id (cadr bucket))))))))

(ert-deftest agent-shell-queue/retarget-same-buffer-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue--retarget-item "q-1" "buf1")
    (should (= 1 (length agent-shell-queue--items)))
    (should (assoc "buf1" agent-shell-queue--items))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistence: save/load roundtrip

(ert-deftest agent-shell-queue/save-load-roundtrip ()
  (let* ((tmp (make-temp-file "asq-test"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--items nil)
         (agent-shell-queue--counter 0)
         (agent-shell-queue--loaded t)
         (agent-shell-queue--last-flush-time nil))
    (unwind-protect
        (progn
          (setq agent-shell-queue--items
                (agent-shell-queue-test/populate
                 '("mybuf" ("q-5" "persisted" deferred t))))
          (agent-shell-queue--save)
          (setq agent-shell-queue--items nil
                agent-shell-queue--counter 0)
          (agent-shell-queue--load)
          (should (= 1 (length agent-shell-queue--items)))
          (let* ((pair (car agent-shell-queue--items))
                 (item (cadr pair)))
            (should (equal "mybuf" (car pair)))
            (should (equal "q-5" (agent-shell-queue-item-id item)))
            (should (equal "persisted" (agent-shell-queue-item-prompt item)))
            (should (eq 'deferred (agent-shell-queue-item-status item)))
            (should (agent-shell-queue-item-background item)))
          (should (>= agent-shell-queue--counter 5))
          ;; save sets flush time
          (should (numberp agent-shell-queue--last-flush-time)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/save-sets-flush-time ()
  (let* ((tmp (make-temp-file "asq-flush"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--items nil)
         (agent-shell-queue--last-flush-time nil))
    (unwind-protect
        (progn
          (agent-shell-queue--save)
          (should (numberp agent-shell-queue--last-flush-time)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/load-missing-file-is-noop ()
  (let* ((agent-shell-queue-state-file-function
          (lambda () "/tmp/asq-test-definitely-does-not-exist-xyz"))
         (agent-shell-queue--items nil))
    (should-not (condition-case err
                    (progn (agent-shell-queue--load) nil)
                  (error err)))
    (should-not agent-shell-queue--items)))

(ert-deftest agent-shell-queue/load-corrupt-file-is-noop ()
  (let* ((tmp (make-temp-file "asq-corrupt"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--items nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "this is not valid elisp )))"))
          (should-not (condition-case err
                          (progn (agent-shell-queue--load) nil)
                        (error err)))
          (should-not agent-shell-queue--items))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/load-restores-counter ()
  (let* ((tmp (make-temp-file "asq-counter"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--items nil)
         (agent-shell-queue--counter 0))
    (unwind-protect
        (progn
          (setq agent-shell-queue--items
                (agent-shell-queue-test/populate
                 '("buf" ("q-7" "a" active nil) ("q-12" "b" active nil))))
          (agent-shell-queue--save)
          (setq agent-shell-queue--items nil
                agent-shell-queue--counter 0)
          (agent-shell-queue--load)
          (should (>= agent-shell-queue--counter 12)))
      (ignore-errors (delete-file tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serialization: plist format

(ert-deftest agent-shell-queue/serialize-plist-roundtrip ()
  (agent-shell-queue-test/isolate
    (let ((agent-shell-queue-serialization-format 'plist))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate
             '("buf" ("q-3" "hello plist" deferred t))))
      (let* ((str   (agent-shell-queue--serialize))
             (items (agent-shell-queue--deserialize str))
             (item  (cadr (car items))))
        (should (equal "buf"         (caar items)))
        (should (equal "q-3"         (agent-shell-queue-item-id item)))
        (should (equal "hello plist" (agent-shell-queue-item-prompt item)))
        (should (eq 'deferred        (agent-shell-queue-item-status item)))
        (should (agent-shell-queue-item-background item))))))

(ert-deftest agent-shell-queue/serialize-plist-symbols-survive ()
  (agent-shell-queue-test/isolate
    (let ((agent-shell-queue-serialization-format 'plist))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate
             '("b" ("q-1" "a" active nil) ("q-2" "b" deferred nil))))
      (let* ((items (agent-shell-queue--deserialize (agent-shell-queue--serialize)))
             (list  (cdar items)))
        (should (eq 'active   (agent-shell-queue-item-status (nth 0 list))))
        (should (eq 'deferred (agent-shell-queue-item-status (nth 1 list))))
        (should-not (agent-shell-queue-item-background (nth 0 list)))))))

;;; Serialization: item plist struct accessors

(ert-deftest agent-shell-queue/item-to-plist-fields ()
  "to-plist includes all struct fields including dispatched."
  (let* ((item (agent-shell-queue-test/make-item "q-1" "prompt" 'active t))
         (pl   (agent-shell-queue-item-to-plist item)))
    (should (equal "q-1"    (plist-get pl :id)))
    (should (equal "prompt" (plist-get pl :prompt)))
    (should (eq 'active     (plist-get pl :status)))
    (should (eq t           (plist-get pl :background)))
    (should (numberp        (plist-get pl :created)))
    (should (null           (plist-get pl :dispatched)))
    (should (null           (plist-get pl :completed)))))

(ert-deftest agent-shell-queue/item-from-plist-roundtrip ()
  (let* ((orig (agent-shell-queue-test/make-item "q-7" "text" 'deferred nil))
         (item (agent-shell-queue-item-from-plist
                (agent-shell-queue-item-to-plist orig))))
    (should (equal (agent-shell-queue-item-id         orig) (agent-shell-queue-item-id         item)))
    (should (equal (agent-shell-queue-item-prompt     orig) (agent-shell-queue-item-prompt     item)))
    (should (eq    (agent-shell-queue-item-status     orig) (agent-shell-queue-item-status     item)))
    (should (eq    (agent-shell-queue-item-background orig) (agent-shell-queue-item-background item)))
    (should (=     (agent-shell-queue-item-created    orig) (agent-shell-queue-item-created    item)))))

;;; Serialization: JSON format

(ert-deftest agent-shell-queue/serialize-json-roundtrip ()
  (skip-unless (fboundp 'json-serialize))
  (agent-shell-queue-test/isolate
    (let ((agent-shell-queue-serialization-format 'json))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate
             '("buf" ("q-4" "hello json" deferred t))))
      (let* ((str   (agent-shell-queue--serialize))
             (items (agent-shell-queue--deserialize str))
             (item  (cadr (car items))))
        (should (equal "buf"        (caar items)))
        (should (equal "q-4"        (agent-shell-queue-item-id item)))
        (should (equal "hello json" (agent-shell-queue-item-prompt item)))
        (should (eq 'deferred       (agent-shell-queue-item-status item)))
        (should (agent-shell-queue-item-background item))))))

(ert-deftest agent-shell-queue/serialize-json-status-is-symbol ()
  (skip-unless (fboundp 'json-serialize))
  (agent-shell-queue-test/isolate
    (let ((agent-shell-queue-serialization-format 'json))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate '("b" ("q-1" "p" active nil))))
      (let ((item (cadr (car (agent-shell-queue--deserialize
                              (agent-shell-queue--serialize))))))
        (should (eq 'active (agent-shell-queue-item-status item)))
        (should (symbolp    (agent-shell-queue-item-status item)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Archive

(ert-deftest agent-shell-queue/write-archive-appends-jsonl ()
  "Write archive writes a valid JSON line to the file."
  (skip-unless (fboundp 'json-serialize))
  (let* ((tmp (make-temp-file "asq-archive"))
         (agent-shell-queue-archive-file tmp)
         (agent-shell-queue-instance-name "test-instance")
         (buf (get-buffer-create " *asq-archive-buf*")))
    (unwind-protect
        (progn
          (let* ((item (agent-shell-queue-test/make-item "q-1" "archive me" 'done nil)))
            (setf (agent-shell-queue-item-dispatched item) 1000.5)
            (setf (agent-shell-queue-item-completed  item) 1060.5)
            (cl-letf (((symbol-function 'lock-file) #'ignore)
                      ((symbol-function 'unlock-file) #'ignore))
              (agent-shell-queue--write-archive (buffer-name buf) item)))
          (let* ((content (with-temp-buffer
                            (insert-file-contents tmp)
                            (buffer-string)))
                 (parsed (json-parse-string (string-trim content)
                                            :object-type 'plist)))
            (should (equal "q-1"           (plist-get parsed :id)))
            (should (equal "archive me"    (plist-get parsed :prompt)))
            (should (equal "test-instance" (plist-get parsed :instance)))
            (should (eq t                  (plist-get parsed :ran)))
            (should (= 60.0                (plist-get parsed :runtime)))))
      (ignore-errors (delete-file tmp))
      (kill-buffer buf))))

(ert-deftest agent-shell-queue/write-archive-noop-when-nil ()
  "Write archive does nothing when archive-file is nil."
  (let ((agent-shell-queue-archive-file nil))
    (should-not (condition-case err
                    (progn
                      (agent-shell-queue--write-archive
                       "buf" (agent-shell-queue-test/make-item "q-1" "p" 'done nil))
                      nil)
                  (error err)))))

(ert-deftest agent-shell-queue/write-archive-ran-false-when-not-dispatched ()
  "Items never dispatched have :ran false in archive."
  (skip-unless (fboundp 'json-serialize))
  (let* ((tmp (make-temp-file "asq-archive-ran"))
         (agent-shell-queue-archive-file tmp)
         (agent-shell-queue-instance-name "ti"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'lock-file) #'ignore)
                    ((symbol-function 'unlock-file) #'ignore))
            (agent-shell-queue--write-archive
             "buf" (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
          (let* ((content (with-temp-buffer
                            (insert-file-contents tmp)
                            (buffer-string)))
                 (parsed (json-parse-string (string-trim content)
                                            :object-type 'plist
                                            :false-object nil)))
            (should-not (plist-get parsed :ran))))
      (ignore-errors (delete-file tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subscriptions

(ert-deftest agent-shell-queue/ensure-subscription-registers ()
  (agent-shell-queue-test/isolate
    (let ((subscribe-calls nil)
          (buf (get-buffer-create " *asq-sub-test*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                     (lambda (&rest args)
                       (push args subscribe-calls)
                       42)))
            (agent-shell-queue--ensure-subscription buf)
            (should (= 2 (length subscribe-calls)))
            (let ((events (mapcar (lambda (a) (plist-get a :event)) subscribe-calls)))
              (should (member 'turn-complete events))
              (should (member 'clean-up events)))
            (should (assoc (buffer-name buf) agent-shell-queue--subscriptions)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/ensure-subscription-no-duplicates ()
  (agent-shell-queue-test/isolate
    (let ((call-count 0)
          (buf (get-buffer-create " *asq-sub-dup-test*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                     (lambda (&rest _) (cl-incf call-count) call-count)))
            (agent-shell-queue--ensure-subscription buf)
            (agent-shell-queue--ensure-subscription buf)
            (should (= 2 call-count)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/drop-subscription-removes-entry ()
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-drop-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--subscriptions
                  (list (cons (buffer-name buf) 99)))
            (cl-letf (((symbol-function 'agent-shell-unsubscribe) #'ignore))
              (agent-shell-queue--drop-subscription (buffer-name buf))
              (should-not (assoc (buffer-name buf) agent-shell-queue--subscriptions))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/drop-subscription-safe-on-dead-buffer ()
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--subscriptions (list (cons "dead-buf" 99)))
    (should-not (condition-case err
                    (progn (agent-shell-queue--drop-subscription "dead-buf") nil)
                  (error err)))
    (should-not (assoc "dead-buf" agent-shell-queue--subscriptions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-send (backup timer)

(ert-deftest agent-shell-queue/auto-send-skips-deferred ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-auto-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "def" 'deferred nil))))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/auto-send-skips-busy-buffer ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-busy-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "hello" 'active nil))))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () t))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/auto-send-only-first-active ()
  "auto-send sends at most one item per bucket per call."
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent-count 0)
          (buf (get-buffer-create " *asq-first-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--items
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "a" 'active nil)
                              (agent-shell-queue-test/make-item "q-2" "b" 'active nil))))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (cl-incf sent-count))))
              (agent-shell-queue--auto-send)
              (should (= 1 sent-count))))
        (kill-buffer buf)))))

;;; test-agent-shell-queue.el ends here
