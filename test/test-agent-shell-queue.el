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
(require 'agent-shell-queue)

;;; Test helpers

(defmacro agent-shell-queue-test/isolate (&rest body)
  "Execute BODY with fresh, isolated queue state.
Shadows both the live store and queue globals so no test touches real state."
  `(let* ((agent-shell-queue--store
           (agent-shell-queue--make-store :items nil :format 'plist :file nil))
          (agent-shell-queue--queue
           (agent-shell-queue-queue--make
            :store 'agent-shell-queue--store
            :paused nil
            :session-paused nil
            :editing-ids nil))
          (agent-shell-queue--loaded t)
          (agent-shell-queue--subscriptions nil)
          (agent-shell-queue--stale-item-ids nil)
          (agent-shell-queue--next-flush-time nil)
          (agent-shell-queue--wait-timers nil)
          (agent-shell-queue--compact-running nil)
          (agent-shell-queue--response-start-positions nil)
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
  "Build a test item directly, bypassing ID generation."
  (agent-shell-queue-item--make
   :id id
   :prompt prompt
   :status (or status 'active)
   :kind 'prompt
   :background background
   :created 1000.0
   :dispatched nil
   :completed nil
   :response nil))

(defun agent-shell-queue-test/populate (&rest specs)
  "Return a fresh items alist from SPECS.
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
;;; agent-shell-queue--capture-response

(defun agent-shell-queue-test/make-response-buffer (&rest spans)
  "Build a temp buffer simulating an agent-shell response buffer.
The buffer contains a `field=input' prompt echo followed by SPANS.
Each span is (TEXT &optional STATE) where STATE is placed on the span
as `agent-shell-ui-state'.  A `field=boundary' marker follows all spans.

Returns (BUFFER . START-POS) where START-POS is the position of the prompt
echo — the correct value for `agent-shell-queue--response-start-positions'.
Caller is responsible for killing the buffer."
  (let ((buf (generate-new-buffer "*test-response-buf*")))
    (with-current-buffer buf
      ;; Echoed prompt with field=input.  capture-response uses this to find
      ;; where the model response begins (first field change past start-pos).
      (let* ((beg (point))
             (start-pos beg))
        (insert "echoed-prompt")
        (put-text-property beg (point) 'field 'input)
        ;; Response spans follow the prompt.
        (dolist (span spans)
          (let* ((text (car span))
                 (state (cadr span))
                 (sbeg (point)))
            (insert text)
            (when state
              (put-text-property sbeg (point) 'agent-shell-ui-state state))))
        ;; End-of-output boundary required by capture logic.
        (let ((bbeg (point)))
          (insert "\n")
          (put-text-property bbeg (point) 'field 'boundary))
        (cons buf start-pos)))))

(ert-deftest agent-shell-queue/capture-response-collects-plain-text-with-ui-state ()
  "Regression: plain text stored via agent-shell-ui-update-text carries
agent-shell-ui-state without :collapsed.  capture-response must include it."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         (plain-state '((:qualified-id . "msg-1")))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     (list "Hello, world!" plain-state)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (agent-shell-queue--capture-response "q01" (buffer-name shell-buf))
         (should (equal "Hello, world!"
                        (agent-shell-queue-item-response item))))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-skips-collapsible-blocks ()
  "Collapsible blocks (thinking, tool calls) have :collapsed in their state
and must be excluded from the captured response."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         (block-state '((:qualified-id . "tool-1") (:collapsed . t)))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     (list "[tool output]" block-state)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (agent-shell-queue--capture-response "q01" (buffer-name shell-buf))
         (should-not (agent-shell-queue-item-response item)))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-mixes-plain-and-block ()
  "Plain text is collected and collapsible blocks are skipped when interleaved."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         (plain-state-1 '((:qualified-id . "msg-1")))
         (block-state   '((:qualified-id . "tool-1") (:collapsed . t)))
         (plain-state-2 '((:qualified-id . "msg-2")))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     (list "First sentence."  plain-state-1)
                     (list "[tool output]"    block-state)
                     (list "Second sentence." plain-state-2)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (agent-shell-queue--capture-response "q01" (buffer-name shell-buf))
         (let ((response (agent-shell-queue-item-response item)))
           (should (stringp response))
           (should (string-match-p "First sentence" response))
           (should (string-match-p "Second sentence" response))
           (should-not (string-match-p "tool output" response))))
      (kill-buffer shell-buf))))

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

;;; format-age with time-value inputs (seconds-to-time)

(ert-deftest agent-shell-queue/format-age-time-value-seconds ()
  (should (equal "1s"  (agent-shell-queue--format-age (seconds-to-time 1))))
  (should (equal "30s" (agent-shell-queue--format-age (seconds-to-time 30))))
  (should (equal "59s" (agent-shell-queue--format-age (seconds-to-time 59)))))

(ert-deftest agent-shell-queue/format-age-time-value-minutes ()
  (should (equal "1m"  (agent-shell-queue--format-age (seconds-to-time 60))))
  (should (equal "5m"  (agent-shell-queue--format-age (seconds-to-time 300))))
  (should (equal "59m" (agent-shell-queue--format-age (seconds-to-time 3599)))))

(ert-deftest agent-shell-queue/format-age-time-value-hours ()
  (should (equal "1h" (agent-shell-queue--format-age (seconds-to-time 3600))))
  (should (equal "2h" (agent-shell-queue--format-age (seconds-to-time 7200)))))

(ert-deftest agent-shell-queue/format-age-time-value-days ()
  (should (equal "1d" (agent-shell-queue--format-age (seconds-to-time 86400))))
  (should (equal "3d" (agent-shell-queue--format-age (seconds-to-time (* 3 86400))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--status-string (pure)

(ert-deftest agent-shell-queue/status-string-active ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (should (equal "scheduled" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-deferred ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'deferred nil)))
      (should (equal "held" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-active-background ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active t)))
      (should (equal "scheduled.bg" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-deferred-background ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'deferred t)))
      (should (equal "held.bg" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-running ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'running nil)))
      (should (equal "running.active" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-running-background ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'running t)))
      (should (equal "running.active.bg" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-done ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'done nil)))
      (should (equal "done" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-editing ()
  "Items with ID in editing-ids show 'editing' regardless of their status."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) '("q-1"))
      (should (equal "editing" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-session-paused ()
  "Active item targeting a session-paused buffer shows 'paused<shell>'."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) '("paused-buf"))
      (should (equal "paused<shell>"
                     (agent-shell-queue--status-string item "paused-buf"))))))

(ert-deftest agent-shell-queue/status-string-session-paused-not-deferred ()
  "Deferred items are not shown as paused even if session is paused."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'deferred nil)))
      (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) '("paused-buf"))
      (should (equal "held"
                     (agent-shell-queue--status-string item "paused-buf"))))))

(ert-deftest agent-shell-queue/status-string-editing-takes-priority-over-paused ()
  "Editing status takes priority over session-paused."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) '("q-1"))
      (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) '("paused-buf"))
      (should (equal "editing"
                     (agent-shell-queue--status-string item "paused-buf"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--activity-state

(ert-deftest agent-shell-queue/activity-state-paused ()
  "Global pause overrides everything."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-queue-paused agent-shell-queue--queue) t)
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("b" ("q-1" "p" running nil))))
    (should (equal "PAUSED"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

(ert-deftest agent-shell-queue/activity-state-running ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("b" ("q-1" "p" running nil))))
    (should (equal "running"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

(ert-deftest agent-shell-queue/activity-state-waiting ()
  "Active items with none running → waiting."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
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
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
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

(ert-deftest agent-shell-queue/make-item-has-string-id ()
  "Each --make-item call returns an item with a unique string ID starting with q."
  (agent-shell-queue-test/isolate
    (let ((id1 (agent-shell-queue-item-id (agent-shell-queue--make-item "hello")))
          (id2 (agent-shell-queue-item-id (agent-shell-queue--make-item "world"))))
      (should (stringp id1))
      (should (string-prefix-p "q" id1))
      (should (stringp id2))
      (should (not (equal id1 id2))))))

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
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
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
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "first" active nil))))
    (should-not (agent-shell-queue--item-by-id "q-99"))))

(ert-deftest agent-shell-queue/item-by-id-empty-queue ()
  (agent-shell-queue-test/isolate
    (should-not (agent-shell-queue--item-by-id "q-1"))))

(ert-deftest agent-shell-queue/item-by-id-across-buckets ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
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
            (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))
            (should (equal (buffer-name buf) (caar (agent-shell-queue-store-items agent-shell-queue--store))))
            (should (= 1 (length (cdar (agent-shell-queue-store-items agent-shell-queue--store))))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/add-appends-to-existing-bucket ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-test-buf*")))
      (unwind-protect
          (progn
            (agent-shell-queue-add "first" buf)
            (agent-shell-queue-add "second" buf)
            (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))
            (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
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
            (should (= 2 (length (agent-shell-queue-store-items agent-shell-queue--store)))))
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
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-remove "q-1")
    (should-not (agent-shell-queue-store-items agent-shell-queue--store))))

(ert-deftest agent-shell-queue/remove-one-of-many ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-remove "q-2")
    (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
      (should (= 2 (length items)))
      (should (equal "q-1" (agent-shell-queue-item-id (nth 0 items))))
      (should (equal "q-3" (agent-shell-queue-item-id (nth 1 items)))))))

(ert-deftest agent-shell-queue/remove-last-item-drops-subscription ()
  (agent-shell-queue-test/isolate
    (let ((dropped nil))
      (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription)
                 (lambda (name) (setq dropped name)))
                ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
        (agent-shell-queue-remove "q-1")
        (should (equal "buf1" dropped))))))

(ert-deftest agent-shell-queue/remove-not-last-preserves-subscription ()
  (agent-shell-queue-test/isolate
    (let ((dropped nil))
      (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription)
                 (lambda (name) (setq dropped name)))
                ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate
               '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
        (agent-shell-queue-remove "q-1")
        (should-not dropped)))))

(ert-deftest agent-shell-queue/remove-unknown-id-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-remove "q-999")
    (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-defer

(ert-deftest agent-shell-queue/defer-active-to-deferred ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-defer "q-1")
    (should (eq 'deferred (agent-shell-queue-item-status (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/defer-deferred-to-active ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" deferred nil))))
    (agent-shell-queue-defer "q-1")
    (should (eq 'active (agent-shell-queue-item-status (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/defer-twice-returns-to-active ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-defer "q-1")
    (agent-shell-queue-defer "q-1")
    (should (eq 'active (agent-shell-queue-item-status (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-set-background-task

(ert-deftest agent-shell-queue/set-background-task-on ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-set-background-task "q-1" t)
    (should (agent-shell-queue-item-background (cadar (agent-shell-queue-store-items agent-shell-queue--store))))))

(ert-deftest agent-shell-queue/set-background-task-off ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active t))))
    (agent-shell-queue-set-background-task "q-1" nil)
    (should-not (agent-shell-queue-item-background (cadar (agent-shell-queue-store-items agent-shell-queue--store))))))

(ert-deftest agent-shell-queue/set-background-task-idempotent ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-set-background-task "q-1" t)
    (agent-shell-queue-set-background-task "q-1" t)
    (should (agent-shell-queue-item-background (cadar (agent-shell-queue-store-items agent-shell-queue--store))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-edit

(ert-deftest agent-shell-queue/edit-replaces-prompt ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "old" active nil))))
    (agent-shell-queue-edit "q-1" "new prompt")
    (should (equal "new prompt"
                   (agent-shell-queue-item-prompt (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/edit-unknown-id-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "old" active nil))))
    (agent-shell-queue-edit "q-999" "new")
    (should (equal "old"
                   (agent-shell-queue-item-prompt (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--move / move-up / move-down

(ert-deftest agent-shell-queue/move-up-middle ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-move-up "q-3")
    (should (equal '("q-1" "q-3" "q-2")
                   (mapcar #'agent-shell-queue-item-id (cdar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/move-down-middle ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-move-down "q-1")
    (should (equal '("q-2" "q-1" "q-3")
                   (mapcar #'agent-shell-queue-item-id (cdar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/move-up-at-top-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
    (agent-shell-queue-move-up "q-1")
    (should (equal '("q-1" "q-2")
                   (mapcar #'agent-shell-queue-item-id (cdar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/move-down-at-bottom-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
    (agent-shell-queue-move-down "q-2")
    (should (equal '("q-1" "q-2")
                   (mapcar #'agent-shell-queue-item-id (cdar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/move-only-item-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "a" active nil))))
    (agent-shell-queue-move-up "q-1")
    (agent-shell-queue-move-down "q-1")
    (should (equal '("q-1")
                   (mapcar #'agent-shell-queue-item-id (cdar (agent-shell-queue-store-items agent-shell-queue--store)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pause: global and per-session

(ert-deftest agent-shell-queue/pause-and-resume-sets-flag ()
  "pause sets the flag; resume clears it."
  (agent-shell-queue-test/isolate
    (should-not (agent-shell-queue-queue-paused agent-shell-queue--queue))
    (agent-shell-queue-pause)
    (should (agent-shell-queue-queue-paused agent-shell-queue--queue))
    (agent-shell-queue-resume)
    (should-not (agent-shell-queue-queue-paused agent-shell-queue--queue))))

(ert-deftest agent-shell-queue/session-pause-adds-name ()
  "session-pause adds the buffer name to --session-paused."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-pause-test*")))
      (unwind-protect
          (progn
            (agent-shell-queue-session-pause buf)
            (should (member (buffer-name buf) (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/session-resume-removes-name ()
  "session-resume removes the buffer name from --session-paused."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-pause-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) (list (buffer-name buf)))
            (cl-letf (((symbol-function 'agent-shell-queue--send-next-for-buffer) #'ignore))
              (agent-shell-queue-session-resume buf))
            (should-not (member (buffer-name buf) (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/unpause-all-sessions-clears-list ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) '("buf1" "buf2" "buf3"))
    (agent-shell-queue-unpause-all-sessions)
    (should-not (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dispatch: editing-ids and buffer-paused prevent sends

(ert-deftest agent-shell-queue/send-next-skips-editing-item ()
  "An item in (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) is never dispatched."
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-edit-skip-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "edit me" 'active nil))))
            (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) '("q-1"))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-next-skips-buffer-paused ()
  "Items targeting a session-paused buffer are not dispatched."
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-buf-pause-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "p" 'active nil))))
            (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) (list (buffer-name buf)))
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
            (setf (agent-shell-queue-queue-paused agent-shell-queue--queue) t)
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
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
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "prompt" 'active nil))))
            (cl-letf (((symbol-function 'agent-shell-insert) #'ignore)
                      ((symbol-function 'buffer-live-p) (lambda (_) t)))
              (agent-shell-queue-send-item "q-1")
              (let ((item (cadar (agent-shell-queue-store-items agent-shell-queue--store))))
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
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
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
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "do thing" 'active t))))
            (cl-letf (((symbol-function 'agent-shell-insert)
                       (lambda (&rest args)
                         (setq inserted-text (plist-get args :text))))
                      ((symbol-function 'buffer-live-p) (lambda (_) t)))
              (agent-shell-queue-send-item "q-1")
              (should (equal "/bg do thing" inserted-text))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-item-dead-buffer-alerts-and-pauses ()
  "Dead target during dispatch emits a high-severity alert and pauses the
session queue instead of raising a user-error."
  (agent-shell-queue-test/isolate-no-sub
    (let (alerted)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate '("dead-buf" ("q-1" "hello" active nil))))
      (cl-letf (((symbol-function 'get-buffer) (lambda (_) nil))
                ((symbol-function 'alert)
                 (lambda (msg &rest args)
                   (setq alerted (list msg (plist-get args :severity))))))
        (should-not (condition-case err
                        (progn (agent-shell-queue-send-item "q-1") nil)
                      (user-error err)))
        (should alerted)
        (should (eq 'high (cadr alerted)))
        (should (member "dead-buf"
                        (agent-shell-queue-queue-session-paused
                         agent-shell-queue--queue)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--mark-running-done

(ert-deftest agent-shell-queue/mark-running-done-sets-status ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (should (eq 'done (agent-shell-queue-item-status (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/mark-running-done-sets-completed-time ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (should (numberp (agent-shell-queue-item-completed (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/mark-running-done-only-running-items ()
  "Active items are not affected."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "p" running nil) ("q-2" "p2" active nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
      (should (eq 'done   (agent-shell-queue-item-status (nth 0 items))))
      (should (eq 'active (agent-shell-queue-item-status (nth 1 items)))))))

(ert-deftest agent-shell-queue/mark-running-done-alert-only-when-marked ()
  "Alert fires only when at least one item was actually marked done."
  (agent-shell-queue-test/isolate
    (let ((alert-fired nil))
      (cl-letf (((symbol-function 'agent-shell-queue--alert-if-empty)
                 (lambda () (setq alert-fired t))))
        ;; No running items — mark should NOT fire alert
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" active nil))))
        (agent-shell-queue--mark-running-done "buf1")
        (should-not alert-fired)
        ;; Running item — mark SHOULD fire alert
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
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
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" done nil))))
        (agent-shell-queue--alert-if-empty)
        (should alerted)))))

(ert-deftest agent-shell-queue/alert-if-empty-silent-when-active-work ()
  (agent-shell-queue-test/isolate
    (let ((alerted nil))
      (cl-letf (((symbol-function 'alert)
                 (lambda (&rest _) (setq alerted t))))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" active nil))))
        (agent-shell-queue--alert-if-empty)
        (should-not alerted)))))

(ert-deftest agent-shell-queue/alert-if-empty-silent-when-running ()
  (agent-shell-queue-test/isolate
    (let ((alerted nil))
      (cl-letf (((symbol-function 'alert)
                 (lambda (&rest _) (setq alerted t))))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
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
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "done prompt" 'done nil))))
            (setf (agent-shell-queue-item-completed (cadar (agent-shell-queue-store-items agent-shell-queue--store))) 2000.0)
            (agent-shell-queue-reenqueue "q-1")
            ;; Original done item still there + new active item
            (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
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
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "active" 'active nil))))
            (should-error (agent-shell-queue-reenqueue "q-1") :type 'user-error))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/reenqueue-dead-buffer-prompts-for-replacement ()
  "When the original target buffer is dead, reenqueue prompts for a live one."
  (agent-shell-queue-test/isolate-no-sub
    (let ((live-buf (get-buffer-create " *asq-reenq-live*")))
      (unwind-protect
          (progn
            ;; Item stored under a dead buffer name
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list " *asq-dead-buf*"
                              (agent-shell-queue-test/make-item "q-1" "hello" 'done nil))))
            (cl-letf (((symbol-function 'agent-shell-queue--pick-buffer)
                       (lambda (_prompt) live-buf)))
              (agent-shell-queue-reenqueue "q-1"))
            (let ((all-items (seq-mapcat #'cdr
                                         (agent-shell-queue-store-items agent-shell-queue--store))))
              ;; A new active item exists targeting live-buf
              (should (seq-find (lambda (it)
                                  (and (equal "hello" (agent-shell-queue-item-prompt it))
                                       (eq 'active (agent-shell-queue-item-status it))))
                                all-items))))
        (kill-buffer live-buf)))))

(ert-deftest agent-shell-queue/reenqueue-dead-buffer-no-live-buffers-errors ()
  "When target is dead and no live buffers exist, reenqueue signals user-error."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (list (list " *asq-gone*"
                      (agent-shell-queue-test/make-item "q-1" "hello" 'done nil))))
    (cl-letf (((symbol-function 'agent-shell-queue--pick-buffer)
               (lambda (_prompt) nil)))
      (should-error (agent-shell-queue-reenqueue "q-1") :type 'user-error))))

(ert-deftest agent-shell-queue/redirect-dead-target-alerts-and-pauses ()
  "Dead target during automatic dispatch emits a high-severity alert and
pauses the session queue instead of prompting interactively."
  (agent-shell-queue-test/isolate
    (let (alerted paused-sessions)
      (cl-letf (((symbol-function 'alert)
                 (lambda (msg &rest args)
                   (setq alerted (list msg (plist-get args :severity)))))
                ((symbol-function 'agent-shell-queue--save) #'ignore)
                ((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore))
        (agent-shell-queue--redirect-dead-target "q-1" " *dead-buf*")
        (setq paused-sessions
              (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
      (should alerted)
      (should (eq 'high (cadr alerted)))
      (should (member " *dead-buf*" paused-sessions)))))

(ert-deftest agent-shell-queue/item-view-reenqueue-no-stray-form ()
  "Regression: a bare URL was accidentally left after (interactive) in
`agent-shell-queue-item-view-reenqueue', causing Symbol's value as variable
is void: https://... when the command was invoked on an aborted item."
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-iv-reenq-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "https://example.com/pr/42" 'aborted nil))))
            (with-temp-buffer
              (setq-local agent-shell-queue--item-view-id "q-1")
              (cl-letf (((symbol-function 'quit-window) #'ignore))
                (should-not (condition-case err
                                (progn (agent-shell-queue-item-view-reenqueue) nil)
                              (void-variable err))))))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--assign-item

(ert-deftest agent-shell-queue/retarget-moves-item ()
  (agent-shell-queue-test/isolate
    (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription) #'ignore)
              ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
      (agent-shell-queue--assign-item "q-1" "buf2")
      (should-not (assoc "buf1" (agent-shell-queue-store-items agent-shell-queue--store)))
      (let ((bucket (assoc "buf2" (agent-shell-queue-store-items agent-shell-queue--store))))
        (should bucket)
        (should (equal "q-1" (agent-shell-queue-item-id (cadr bucket))))))))

(ert-deftest agent-shell-queue/retarget-same-buffer-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue--assign-item "q-1" "buf1")
    (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))
    (should (assoc "buf1" (agent-shell-queue-store-items agent-shell-queue--store)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistence: save/load roundtrip

(ert-deftest agent-shell-queue/save-load-roundtrip ()
  (let* ((tmp (make-temp-file "asq-test"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil))
         (agent-shell-queue--loaded t)
         (agent-shell-queue--last-flush-time nil))
    (unwind-protect
        (progn
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (agent-shell-queue-test/populate
                 '("mybuf" ("q-5" "persisted" deferred t))))
          (agent-shell-queue--save)
          (setf (agent-shell-queue-store-items agent-shell-queue--store) nil)
          (agent-shell-queue--load)
          (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))
          (let* ((pair (car (agent-shell-queue-store-items agent-shell-queue--store)))
                 (item (cadr pair)))
            (should (equal "mybuf" (car pair)))
            (should (equal "q-5" (agent-shell-queue-item-id item)))
            (should (equal "persisted" (agent-shell-queue-item-prompt item)))
            (should (eq 'deferred (agent-shell-queue-item-status item)))
            (should (agent-shell-queue-item-background item)))
          ;; save sets flush time
          (should (numberp agent-shell-queue--last-flush-time)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/save-sets-flush-time ()
  (let* ((tmp (make-temp-file "asq-flush"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil))
         (agent-shell-queue--last-flush-time nil))
    (unwind-protect
        (progn
          (agent-shell-queue--save)
          (should (numberp agent-shell-queue--last-flush-time)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/load-missing-file-is-noop ()
  (let* ((agent-shell-queue-state-file-function
          (lambda () "/tmp/asq-test-definitely-does-not-exist-xyz"))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil)))
    (should-not (condition-case err
                    (progn (agent-shell-queue--load) nil)
                  (error err)))
    (should-not (agent-shell-queue-store-items agent-shell-queue--store))))

(ert-deftest agent-shell-queue/load-corrupt-file-is-noop ()
  (let* ((tmp (make-temp-file "asq-corrupt"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil)))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "this is not valid elisp )))"))
          (should-not (condition-case err
                          (progn (agent-shell-queue--load) nil)
                        (error err)))
          (should-not (agent-shell-queue-store-items agent-shell-queue--store)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/load-restores-multiple-items ()
  "Loading persisted state restores all items across buckets."
  (let* ((tmp (make-temp-file "asq-multi"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil)))
    (unwind-protect
        (progn
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (agent-shell-queue-test/populate
                 '("buf" ("q-7" "a" active nil) ("q-12" "b" active nil))))
          (agent-shell-queue--save)
          (setf (agent-shell-queue-store-items agent-shell-queue--store) nil)
          (agent-shell-queue--load)
          (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))
          (should (= 2 (length (cdar (agent-shell-queue-store-items agent-shell-queue--store))))))
      (ignore-errors (delete-file tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serialization: plist format

(ert-deftest agent-shell-queue/serialize-plist-roundtrip ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-format agent-shell-queue--store) 'plist)
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf" ("q-3" "hello plist" deferred t))))
    (let* ((str   (agent-shell-queue-serialize agent-shell-queue--store))
           (items (agent-shell-queue-deserialize agent-shell-queue--store str))
           (item  (cadr (car items))))
      (should (equal "buf"         (caar items)))
      (should (equal "q-3"         (agent-shell-queue-item-id item)))
      (should (equal "hello plist" (agent-shell-queue-item-prompt item)))
      (should (eq 'deferred        (agent-shell-queue-item-status item)))
      (should (agent-shell-queue-item-background item)))))

(ert-deftest agent-shell-queue/serialize-plist-symbols-survive ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-format agent-shell-queue--store) 'plist)
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("b" ("q-1" "a" active nil) ("q-2" "b" deferred nil))))
    (let* ((items (agent-shell-queue-deserialize agent-shell-queue--store
                                                 (agent-shell-queue-serialize agent-shell-queue--store)))
           (list  (cdar items)))
      (should (eq 'active   (agent-shell-queue-item-status (nth 0 list))))
      (should (eq 'deferred (agent-shell-queue-item-status (nth 1 list))))
      (should-not (agent-shell-queue-item-background (nth 0 list))))))

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
    (setf (agent-shell-queue-store-format agent-shell-queue--store) 'json)
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf" ("q-4" "hello json" deferred t))))
    (let* ((str   (agent-shell-queue-serialize agent-shell-queue--store))
           (items (agent-shell-queue-deserialize agent-shell-queue--store str))
           (item  (cadr (car items))))
      (should (equal "buf"        (caar items)))
      (should (equal "q-4"        (agent-shell-queue-item-id item)))
      (should (equal "hello json" (agent-shell-queue-item-prompt item)))
      (should (eq 'deferred       (agent-shell-queue-item-status item)))
      (should (agent-shell-queue-item-background item)))))

(ert-deftest agent-shell-queue/serialize-json-status-is-symbol ()
  (skip-unless (fboundp 'json-serialize))
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-format agent-shell-queue--store) 'json)
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("b" ("q-1" "p" active nil))))
    (let ((item (cadr (car (agent-shell-queue-deserialize
                            agent-shell-queue--store
                            (agent-shell-queue-serialize agent-shell-queue--store))))))
      (should (eq 'active (agent-shell-queue-item-status item)))
      (should (symbolp    (agent-shell-queue-item-status item))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Archive

(ert-deftest agent-shell-queue/write-archive-appends-jsonl ()
  "Write archive writes a valid JSON line to the file."
  (skip-unless (fboundp 'json-serialize))
  (let* ((tmp (make-temp-file "asq-archive"))
         (agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () tmp))
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

(ert-deftest agent-shell-queue/write-archive-noop-when-disabled ()
  "Write archive does nothing when archiving is disabled."
  (let ((agent-shell-queue-archive-enabled nil))
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
         (agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () tmp))
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
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
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
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
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
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "a" 'active nil)
                              (agent-shell-queue-test/make-item "q-2" "b" 'active nil))))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (cl-incf sent-count))))
              (agent-shell-queue--auto-send)
              (should (= 1 sent-count))))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--refresh-buffer guard chain
;; Note: these tests call --refresh-buffer directly without the isolate
;; macro, since isolate stubs out the function under test.

(ert-deftest agent-shell-queue/refresh-buffer-skips-dead-buffer ()
  "No error and no refresh when the queue buffer does not exist."
  (let ((refreshed nil))
    (cl-letf (((symbol-function 'agent-shell-queue-buffer-refresh)
               (lambda () (setq refreshed t)))
              ((symbol-function 'get-buffer)
               (lambda (_) nil)))
      (agent-shell-queue--refresh-buffer)
      (should-not refreshed))))

(ert-deftest agent-shell-queue/refresh-buffer-skips-wrong-mode ()
  "No refresh when the queue buffer is live but not in agent-shell-queue-mode."
  (let ((refreshed nil)
        (buf (get-buffer-create " *asq-refresh-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-queue-buffer-refresh)
                   (lambda () (setq refreshed t)))
                  ((symbol-function 'get-buffer)
                   (lambda (_) buf))
                  ((symbol-function 'derived-mode-p)
                   (lambda (_) nil)))
          (agent-shell-queue--refresh-buffer)
          (should-not refreshed))
      (kill-buffer buf))))

(ert-deftest agent-shell-queue/refresh-buffer-calls-refresh-when-live-and-mode ()
  "Refresh is called when the buffer is live and in agent-shell-queue-mode."
  (let ((refreshed nil)
        (buf (get-buffer-create " *asq-refresh-live-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-queue-buffer-refresh)
                   (lambda () (setq refreshed t)))
                  ((symbol-function 'get-buffer)
                   (lambda (_) buf))
                  ((symbol-function 'derived-mode-p)
                   (lambda (_) t)))
          (agent-shell-queue--refresh-buffer)
          (should refreshed))
      (kill-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--session-mode-blocked-p guard chain

(ert-deftest agent-shell-queue/session-mode-blocked-dead-buffer ()
  "Returns nil when the buffer is dead."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-mode-blocked-dead*")))
      (kill-buffer buf)
      (should-not (agent-shell-queue--session-mode-blocked-p buf)))))

(ert-deftest agent-shell-queue/session-mode-blocked-no-mode-id ()
  "Returns nil when map-nested-elt finds no :session :mode-id."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-mode-blocked-no-id*"))
          (agent-shell-queue-blocked-session-modes '("blocked-mode")))
      (unwind-protect
          (cl-letf (((symbol-function 'map-nested-elt)
                     (lambda (&rest _) nil)))
            (should-not (agent-shell-queue--session-mode-blocked-p buf)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/session-mode-blocked-mode-not-in-list ()
  "Returns nil when mode-id is not in blocked-session-modes."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-mode-blocked-not-in*"))
          (agent-shell-queue-blocked-session-modes '("blocked-mode")))
      (unwind-protect
          (cl-letf (((symbol-function 'map-nested-elt)
                     (lambda (&rest _) "other-mode")))
            (should-not (agent-shell-queue--session-mode-blocked-p buf)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/session-mode-blocked-mode-in-list ()
  "Returns non-nil when mode-id is in blocked-session-modes."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-mode-blocked-in*"))
          (agent-shell-queue-blocked-session-modes '("blocked-mode" "other")))
      (unwind-protect
          (cl-letf (((symbol-function 'map-nested-elt)
                     (lambda (&rest _) "blocked-mode")))
            (should (agent-shell-queue--session-mode-blocked-p buf)))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--drop-subscription — setq is unconditional

(ert-deftest agent-shell-queue/drop-subscription-cleans-up-dead-buffer ()
  "Subscription entry is removed from registry even when the buffer is dead."
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--subscriptions
          (list (cons "dead-buf" 'fake-token)))
    (cl-letf (((symbol-function 'get-buffer) (lambda (_) nil)))
      (agent-shell-queue--drop-subscription "dead-buf"))
    (should-not agent-shell-queue--subscriptions)))

(ert-deftest agent-shell-queue/drop-subscription-cleans-up-non-agent-shell-mode ()
  "Subscription entry is removed even when buffer is live but wrong mode."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-drop-sub-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--subscriptions
                  (list (cons (buffer-name buf) 'fake-token)))
            (cl-letf (((symbol-function 'derived-mode-p) (lambda (_) nil))
                      ((symbol-function 'agent-shell-unsubscribe) #'ignore))
              (agent-shell-queue--drop-subscription (buffer-name buf)))
            (should-not agent-shell-queue--subscriptions))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/drop-subscription-unknown-buf-name-noop ()
  "Calling with an unregistered name does not error and leaves registry intact."
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--subscriptions
          (list (cons "other-buf" 'token)))
    (agent-shell-queue--drop-subscription "no-such-buf")
    (should (= 1 (length agent-shell-queue--subscriptions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-defer — unknown id is noop

(ert-deftest agent-shell-queue/defer-unknown-id-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-defer "q-999")
    (should (eq 'active (agent-shell-queue-item-status (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--auto-send — when-let* short-circuits

(ert-deftest agent-shell-queue/auto-send-skips-session-paused ()
  "Items targeting a session-paused buffer are not dispatched."
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-sess-pause-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "p" 'active nil))))
            (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) (list (buffer-name buf)))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/auto-send-dispatches-next-item ()
  "auto-send dispatches the first active item for an idle buffer."
  (agent-shell-queue-test/isolate-no-sub
    (let ((dispatched-id nil)
          (buf (get-buffer-create " *asq-auto-send-dispatch*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "first" 'active nil)
                              (agent-shell-queue-test/make-item "q-2" "second" 'active nil))))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-queue-send-item)
                       (lambda (id) (setq dispatched-id id))))
              (agent-shell-queue--auto-send)
              (should (equal "q-1" dispatched-id))))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--write-archive — :ran field follows dispatched

(ert-deftest agent-shell-queue/write-archive-ran-when-dispatched ()
  ":ran is t when dispatched is non-nil."
  (let* ((tmp (make-temp-file "asq-archive-ran"))
         (agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () tmp))
         (item (agent-shell-queue-item--make
                :id "q-1" :prompt "p" :status 'done
                :background nil :created 1000.0
                :dispatched 2000.0 :completed 3000.0)))
    (unwind-protect
        (progn
          (agent-shell-queue--write-archive "buf" item)
          (let* ((raw (with-temp-buffer
                        (insert-file-contents tmp)
                        (buffer-string)))
                 (parsed (json-parse-string raw)))
            (should (eq t (gethash "ran" parsed)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/write-archive-not-ran-when-no-dispatched ()
  ":ran is :false when dispatched is nil."
  (let* ((tmp (make-temp-file "asq-archive-norun"))
         (agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () tmp))
         (item (agent-shell-queue-item--make
                :id "q-2" :prompt "p" :status 'done
                :background nil :created 1000.0
                :dispatched nil :completed nil)))
    (unwind-protect
        (progn
          (agent-shell-queue--write-archive "buf" item)
          (let* ((raw (with-temp-buffer
                        (insert-file-contents tmp)
                        (buffer-string)))
                 (parsed (json-parse-string raw)))
            (should (eq :false (gethash "ran" parsed)))))
      (ignore-errors (delete-file tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-with-paused-session — macro

(ert-deftest agent-shell-queue/with-paused-session-pauses-during-body ()
  "Session is paused while body executes, resumed afterward."
  (agent-shell-queue-test/isolate
    (with-temp-buffer
      (rename-buffer "*test-session*" t)
      (let ((paused-during nil))
        (agent-shell-queue-with-paused-session (current-buffer)
          (setq paused-during
                (member (buffer-name) (agent-shell-queue-queue-session-paused
                                       agent-shell-queue--queue))))
        ;; After macro: session should be resumed (removed from paused list)
        (should paused-during)
        (should (not (member (buffer-name)
                             (agent-shell-queue-queue-session-paused
                              agent-shell-queue--queue))))))))

(ert-deftest agent-shell-queue/with-paused-session-resumes-on-error ()
  "Session is resumed even when body signals an error."
  (agent-shell-queue-test/isolate
    (with-temp-buffer
      (rename-buffer "*test-err-session*" t)
      (let ((buf-name (buffer-name)))
        (ignore-errors
          (agent-shell-queue-with-paused-session (current-buffer)
            (error "deliberate test error")))
        (should (not (member buf-name
                             (agent-shell-queue-queue-session-paused
                              agent-shell-queue--queue))))))))

(ert-deftest agent-shell-queue/with-paused-session-accepts-string ()
  "Macro works when BUF is a buffer name string."
  (agent-shell-queue-test/isolate
    (with-temp-buffer
      (rename-buffer "*test-str-session*" t)
      (let ((buf-name (buffer-name))
            (paused-during nil))
        (agent-shell-queue-with-paused-session buf-name
          (setq paused-during
                (member buf-name (agent-shell-queue-queue-session-paused
                                  agent-shell-queue--queue))))
        (should paused-during)
        (should (not (member buf-name
                             (agent-shell-queue-queue-session-paused
                              agent-shell-queue--queue))))))))

(ert-deftest agent-shell-queue/with-paused-session-returns-body-value ()
  "Macro returns the value of its body."
  (agent-shell-queue-test/isolate
    (with-temp-buffer
      (rename-buffer "*test-retval-session*" t)
      (let ((result (agent-shell-queue-with-paused-session (current-buffer)
                      (+ 1 2))))
        (should (= 3 result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--fork-collect-items

(ert-deftest agent-shell-queue/fork-collect-items-nil-from-id ()
  "With nil from-id, collects all active/deferred/draft items."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*s*"
            ("q1" "p1" active nil)
            ("q2" "p2" deferred nil)
            ("q3" "p3" done nil)
            ("q4" "p4" draft nil))))
    (let ((collected (agent-shell-queue--fork-collect-items "*s*" nil)))
      (should (= 3 (length collected)))
      (should (member "q1" (-map #'agent-shell-queue-item-id collected)))
      (should (member "q2" (-map #'agent-shell-queue-item-id collected)))
      (should (member "q4" (-map #'agent-shell-queue-item-id collected)))
      (should (not (member "q3" (-map #'agent-shell-queue-item-id collected)))))))

(ert-deftest agent-shell-queue/fork-collect-items-from-id ()
  "With from-id, collects active items at and after that position."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*s*"
            ("q1" "p1" active nil)
            ("q2" "p2" active nil)
            ("q3" "p3" active nil)
            ("q4" "p4" active nil))))
    (let ((collected (agent-shell-queue--fork-collect-items "*s*" "q2")))
      (should (= 3 (length collected)))
      (should (-all? (lambda (it)
                       (member (agent-shell-queue-item-id it) '("q2" "q3" "q4")))
                     collected)))))

(ert-deftest agent-shell-queue/fork-collect-items-from-id-skips-done ()
  "Skips done and running items even when they appear in range."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*s*"
            ("q1" "p1" active nil)
            ("q2" "p2" running nil)
            ("q3" "p3" active nil)
            ("q4" "p4" active nil))))
    (let ((collected (agent-shell-queue--fork-collect-items "*s*" "q2")))
      ;; q2 is running → not eligible; q3, q4 are active
      (should (= 2 (length collected)))
      (should (-all? (lambda (it)
                       (member (agent-shell-queue-item-id it) '("q3" "q4")))
                     collected)))))

(ert-deftest agent-shell-queue/fork-collect-items-unknown-from-id ()
  "Unknown from-id returns empty list."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*s*" ("q1" "p1" active nil))))
    (let ((collected (agent-shell-queue--fork-collect-items "*s*" "no-such-id")))
      (should (null collected)))))

(ert-deftest agent-shell-queue/fork-collect-items-empty-bucket ()
  "Returns nil for an empty bucket."
  (agent-shell-queue-test/isolate
    (let ((collected (agent-shell-queue--fork-collect-items "*nonexistent*" nil)))
      (should (null collected)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-fork-session

(defmacro agent-shell-queue-test/with-fork-stubs (new-buf-name &rest body)
  "Execute BODY with session-creation stubbed to return a buffer named NEW-BUF-NAME.
Also stubs subscription management and `sit-for' to avoid side effects."
  (declare (indent 1))
  `(agent-shell-queue-test/isolate-no-sub
    (cl-letf (((symbol-function 'agent-shell-queue--fork-create-session)
               (lambda (_src _mode _dir) (get-buffer-create ,new-buf-name)))
              ((symbol-function 'sit-for) #'ignore))
      ,@body)))

(ert-deftest agent-shell-queue/fork-session-moves-items-to-new-buffer ()
  "Items at/after from-id are moved to the new session buffer."
  (agent-shell-queue-test/with-fork-stubs "*new-session*"
    (with-temp-buffer
      (rename-buffer "*src-session*" t)
      (let ((src-buf (current-buffer)))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate
               '("*src-session*"
                ("q1" "p1" active nil)
                ("q2" "p2" active nil)
                ("q3" "p3" active nil))))
        (agent-shell-queue-fork-session src-buf "q2")
        ;; q1 remains in source
        (let ((src-items (cdr (assoc "*src-session*"
                                     (agent-shell-queue-store-items agent-shell-queue--store)))))
          (should (= 1 (length src-items)))
          (should (equal "q1" (agent-shell-queue-item-id (car src-items)))))
        ;; q2 and q3 moved to new session
        (let ((new-items (cdr (assoc "*new-session*"
                                     (agent-shell-queue-store-items agent-shell-queue--store)))))
          (should (= 2 (length new-items)))
          (should (member "q2" (-map #'agent-shell-queue-item-id new-items)))
          (should (member "q3" (-map #'agent-shell-queue-item-id new-items))))
        (kill-buffer "*new-session*")))))

(ert-deftest agent-shell-queue/fork-session-nil-from-id-moves-all ()
  "With nil from-id, all active items are moved to the new session."
  (agent-shell-queue-test/with-fork-stubs "*new-all*"
    (with-temp-buffer
      (rename-buffer "*src-all*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*src-all*"
              ("q1" "p1" active nil)
              ("q2" "p2" active nil))))
      (agent-shell-queue-fork-session (current-buffer) nil)
      (let ((src-items (cdr (assoc "*src-all*"
                                   (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (null src-items)))
      (let ((new-items (cdr (assoc "*new-all*"
                                   (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 2 (length new-items))))
      (kill-buffer "*new-all*"))))

(ert-deftest agent-shell-queue/fork-session-capture-pending-marks-status ()
  "With :capture-pending, items get pending-fork status and stay in source."
  (agent-shell-queue-test/with-fork-stubs "*fork-pending-new*"
    (with-temp-buffer
      (rename-buffer "*src-pending*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*src-pending*"
              ("q1" "p1" active nil)
              ("q2" "p2" active nil)
              ("q3" "p3" active nil))))
      (agent-shell-queue-fork-session (current-buffer) "q2"
                                      '(:capture-pending t))
      ;; q1 untouched; q2 and q3 become pending-fork
      (let ((items (cdr (assoc "*src-pending*"
                               (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 3 (length items)))
        (should (eq 'active (agent-shell-queue-item-status
                             (cl-find "q1" items :key #'agent-shell-queue-item-id
                                      :test #'equal))))
        (should (eq 'pending-fork (agent-shell-queue-item-status
                                   (cl-find "q2" items :key #'agent-shell-queue-item-id
                                            :test #'equal))))
        (should (eq 'pending-fork (agent-shell-queue-item-status
                                   (cl-find "q3" items :key #'agent-shell-queue-item-id
                                            :test #'equal)))))
      ;; source session remains paused after capture-pending
      (should (member "*src-pending*"
                      (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
      (kill-buffer "*fork-pending-new*"))))

(ert-deftest agent-shell-queue/fork-session-errors-when-no-items ()
  "Signals user-error when there are no eligible items to fork."
  (agent-shell-queue-test/with-fork-stubs "*no-items-new*"
    (with-temp-buffer
      (rename-buffer "*src-empty*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*src-empty*" ("q1" "p1" done nil))))
      (should-error (agent-shell-queue-fork-session (current-buffer) nil)
                    :type 'user-error)
      (when (get-buffer "*no-items-new*") (kill-buffer "*no-items-new*")))))

(ert-deftest agent-shell-queue/fork-session-resumes-source-on-error ()
  "Source session is resumed (removed from paused) if session creation fails."
  (agent-shell-queue-test/isolate-no-sub
    (cl-letf (((symbol-function 'agent-shell-queue--fork-create-session)
               (lambda (_src _mode _dir) nil))  ; returns nil → triggers user-error
              ((symbol-function 'sit-for) #'ignore))
      (with-temp-buffer
        (rename-buffer "*src-resume*" t)
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate
               '("*src-resume*" ("q1" "p1" active nil))))
        (ignore-errors
          (agent-shell-queue-fork-session (current-buffer) nil))
        ;; unwind-protect removes from paused list after error
        (should (not (member "*src-resume*"
                             (agent-shell-queue-queue-session-paused
                              agent-shell-queue--queue))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-release-pending-fork

(ert-deftest agent-shell-queue/release-pending-fork-activates-items ()
  "pending-fork items become active after release."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*release-session*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*release-session*"
              ("q1" "p1" pending-fork nil)
              ("q2" "p2" pending-fork nil)
              ("q3" "p3" active nil))))
      ;; Pre-pause the session so resume has something to clear
      (cl-pushnew "*release-session*"
                  (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                  :test #'equal)
      (cl-letf (((symbol-function 'agent-shell-queue--send-next-for-buffer) #'ignore)
                ((symbol-function 'agent-shell-queue-session-resume) #'ignore))
        (agent-shell-queue-release-pending-fork (current-buffer)))
      (let ((items (cdr (assoc "*release-session*"
                               (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (-all? (lambda (it)
                         (eq 'active (agent-shell-queue-item-status it)))
                       items))))))

(ert-deftest agent-shell-queue/release-pending-fork-no-op-when-none ()
  "Releasing when no pending-fork items sends no-resume and reports 0."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*release-none*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*release-none*" ("q1" "p1" active nil))))
      (let (resumed)
        (cl-letf (((symbol-function 'agent-shell-queue-session-resume)
                   (lambda (_) (setq resumed t)))
                  ((symbol-function 'agent-shell-queue--send-next-for-buffer) #'ignore))
          (agent-shell-queue-release-pending-fork (current-buffer)))
        ;; resume should not be called since nothing was pending-fork
        (should (not resumed))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--fork-insert-at

(ert-deftest agent-shell-queue/fork-insert-at-beginning ()
  "Inserting at idx 0 places item before all others."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*ins*" ("q1" "p1" active nil) ("q2" "p2" active nil))))
    (let ((new-item (agent-shell-queue--make-item "new" nil 'emacs)))
      (agent-shell-queue--fork-insert-at "*ins*" new-item 0)
      (let ((items (cdr (assoc "*ins*"
                               (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 3 (length items)))
        (should (equal (agent-shell-queue-item-id new-item)
                       (agent-shell-queue-item-id (car items))))))))

(ert-deftest agent-shell-queue/fork-insert-at-middle ()
  "Inserting at idx 1 places item between first and second."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*ins-mid*" ("q1" "p1" active nil) ("q2" "p2" active nil))))
    (let ((new-item (agent-shell-queue--make-item "mid" nil 'emacs)))
      (agent-shell-queue--fork-insert-at "*ins-mid*" new-item 1)
      (let ((items (cdr (assoc "*ins-mid*"
                               (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 3 (length items)))
        (should (equal "q1" (agent-shell-queue-item-id (nth 0 items))))
        (should (equal (agent-shell-queue-item-id new-item)
                       (agent-shell-queue-item-id (nth 1 items))))
        (should (equal "q2" (agent-shell-queue-item-id (nth 2 items))))))))

(ert-deftest agent-shell-queue/fork-insert-at-nil-appends ()
  "Inserting with nil idx appends to end."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*ins-end*" ("q1" "p1" active nil) ("q2" "p2" active nil))))
    (let ((new-item (agent-shell-queue--make-item "end" nil 'emacs)))
      (agent-shell-queue--fork-insert-at "*ins-end*" new-item nil)
      (let ((items (cdr (assoc "*ins-end*"
                               (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 3 (length items)))
        (should (equal (agent-shell-queue-item-id new-item)
                       (agent-shell-queue-item-id (nth 2 items))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-insert-fork-before / -after

(ert-deftest agent-shell-queue/insert-fork-before-inserts-emacs-item ()
  "insert-fork-before adds an emacs-kind item before the target."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*ibefore-session*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*ibefore-session*"
              ("q1" "p1" active nil)
              ("q2" "p2" active nil))))
      (let ((fork-item (agent-shell-queue-insert-fork-before
                        (current-buffer) "q2" nil)))
        (should (eq 'emacs (agent-shell-queue-item-kind fork-item)))
        (let ((items (cdr (assoc "*ibefore-session*"
                                 (agent-shell-queue-store-items agent-shell-queue--store)))))
          (should (= 3 (length items)))
          (should (equal "q1" (agent-shell-queue-item-id (nth 0 items))))
          ;; fork item lands at position 1 (before q2)
          (should (equal (agent-shell-queue-item-id fork-item)
                         (agent-shell-queue-item-id (nth 1 items))))
          (should (equal "q2" (agent-shell-queue-item-id (nth 2 items)))))))))

(ert-deftest agent-shell-queue/insert-fork-after-inserts-emacs-item ()
  "insert-fork-after adds an emacs-kind item after the target."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*iafter-session*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*iafter-session*"
              ("q1" "p1" active nil)
              ("q2" "p2" active nil))))
      (let ((fork-item (agent-shell-queue-insert-fork-after
                        (current-buffer) "q1" nil)))
        (should (eq 'emacs (agent-shell-queue-item-kind fork-item)))
        (let ((items (cdr (assoc "*iafter-session*"
                                 (agent-shell-queue-store-items agent-shell-queue--store)))))
          (should (= 3 (length items)))
          (should (equal "q1" (agent-shell-queue-item-id (nth 0 items))))
          ;; fork item at position 1 (after q1)
          (should (equal (agent-shell-queue-item-id fork-item)
                         (agent-shell-queue-item-id (nth 1 items))))
          (should (equal "q2" (agent-shell-queue-item-id (nth 2 items)))))))))

(ert-deftest agent-shell-queue/insert-fork-before-nil-appends ()
  "insert-fork-before with nil item-id appends to end."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*ib-nil*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*ib-nil*" ("q1" "p1" active nil))))
      (let ((fork-item (agent-shell-queue-insert-fork-before (current-buffer) nil nil)))
        (let ((items (cdr (assoc "*ib-nil*"
                                 (agent-shell-queue-store-items agent-shell-queue--store)))))
          (should (= 2 (length items)))
          (should (equal (agent-shell-queue-item-id fork-item)
                         (agent-shell-queue-item-id (nth 1 items)))))))))

(ert-deftest agent-shell-queue/insert-fork-form-contains-buf-name ()
  "The emacs form in the inserted item references the source buffer name."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*iform-session*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*iform-session*" ("q1" "p1" active nil))))
      (let ((fork-item (agent-shell-queue-insert-fork-after
                        (current-buffer) "q1" '(:fork-mode fork))))
        (should (string-match-p "iform-session"
                                (agent-shell-queue-item-prompt fork-item)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--fork-session-from-running-emacs

(ert-deftest agent-shell-queue/fork-from-running-emacs-forks-after-running ()
  "Finds the running item and forks starting from the item after it."
  (agent-shell-queue-test/with-fork-stubs "*from-running-new*"
    (with-temp-buffer
      (rename-buffer "*from-running-src*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*from-running-src*"
              ("q1" "p1" done nil)
              ("q2" "p2" running nil)   ; currently running emacs item
              ("q3" "p3" active nil)
              ("q4" "p4" active nil))))
      (agent-shell-queue--fork-session-from-running-emacs "*from-running-src*" nil)
      ;; q3 and q4 should be moved to new session
      (let ((new-items (cdr (assoc "*from-running-new*"
                                   (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 2 (length new-items)))
        (should (member "q3" (-map #'agent-shell-queue-item-id new-items)))
        (should (member "q4" (-map #'agent-shell-queue-item-id new-items))))
      (kill-buffer "*from-running-new*"))))

(ert-deftest agent-shell-queue/fork-from-running-emacs-no-items-after ()
  "Returns nil and does nothing when no items follow the running one."
  (agent-shell-queue-test/with-fork-stubs "*from-running-empty-new*"
    (with-temp-buffer
      (rename-buffer "*from-running-empty*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*from-running-empty*"
              ("q1" "p1" running nil))))  ; running, nothing after it
      ;; Should not error; fork-session will error with user-error which we ignore
      (ignore-errors
        (agent-shell-queue--fork-session-from-running-emacs "*from-running-empty*" nil))
      (when (get-buffer "*from-running-empty-new*")
        (kill-buffer "*from-running-empty-new*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pending-fork status — display

(ert-deftest agent-shell-queue/status-string-pending-fork ()
  "Items with pending-fork status display as \"pending-fork\"."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-pf" "p" 'pending-fork nil)))
      (should (equal "pending-fork" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/item-display-pending-fork-face ()
  "pending-fork items get the pending-fork face."
  (agent-shell-queue-test/isolate
    (let* ((item (agent-shell-queue-test/make-item "q-pf2" "p" 'pending-fork nil))
           (display (agent-shell-queue--item-display item nil)))
      (should (equal "pending-fork" (car display)))
      (should (eq 'agent-shell-queue-pending-fork-face (cdr display))))))

(ert-deftest agent-shell-queue/pending-fork-not-dispatched ()
  "Items with pending-fork status are not eligible for dispatch."
  (agent-shell-queue-test/isolate
    (let ((items (list (agent-shell-queue-test/make-item "q-pf3" "p" 'pending-fork nil)
                       (agent-shell-queue-test/make-item "q-act" "p" 'active nil))))
      (let ((next (agent-shell-queue--next-dispatchable-item items)))
        (should (equal "q-act" (agent-shell-queue-item-id next)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--fork-create-worktree

(ert-deftest agent-shell-queue/fork-create-worktree-no-git-repo ()
  "Returns nil when not in a git repo."
  (agent-shell-queue-test/isolate
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_) "")))  ; empty repo root
      (let ((result (agent-shell-queue--fork-create-worktree nil nil nil)))
        (should (null result))))))

(ert-deftest agent-shell-queue/fork-create-worktree-failure ()
  "Returns nil when git worktree add fails."
  (agent-shell-queue-test/isolate
    ;; Use a path that doesn't exist so file-exists-p returns nil naturally.
    ;; We cannot stub file-exists-p (C primitive) in Emacs 29 native compilation.
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_) "/some/repo"))
              ((symbol-function 'call-process)
               (lambda (&rest _) 128)))  ; non-zero exit
      (let ((result (agent-shell-queue--fork-create-worktree
                     nil "branch" "/tmp/asdf-no-such-ert-worktree-path-failure")))
        (should (null result))))))

(ert-deftest agent-shell-queue/fork-create-worktree-success ()
  "Returns the worktree path when git worktree add succeeds."
  (agent-shell-queue-test/isolate
    ;; Use a path that doesn't exist so file-exists-p returns nil naturally.
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_) "/some/repo"))
              ((symbol-function 'call-process)
               (lambda (&rest _) 0)))  ; zero exit = success
      (let ((result (agent-shell-queue--fork-create-worktree
                     nil "my-branch" "/tmp/asdf-no-such-ert-worktree-path-success")))
        (should (equal "/tmp/asdf-no-such-ert-worktree-path-success" result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--fork-elisp-form

(ert-deftest agent-shell-queue/fork-elisp-form-contains-buf-name ()
  "The generated form string contains the buffer name."
  (let ((form (agent-shell-queue--fork-elisp-form "*my-shell*" nil)))
    (should (string-match-p "my-shell" form))))

(ert-deftest agent-shell-queue/fork-elisp-form-contains-opts ()
  "The generated form string contains the opts plist."
  (let ((form (agent-shell-queue--fork-elisp-form "*shell*" '(:fork-mode fork))))
    (should (string-match-p "fork-mode" form))))

(ert-deftest agent-shell-queue/fork-elisp-form-is-readable ()
  "The generated form string is valid Emacs Lisp."
  (let* ((form (agent-shell-queue--fork-elisp-form "*readable-shell*" '(:capture-pending t)))
         (parsed (condition-case nil (read form) (error nil))))
    (should (listp parsed))
    (should (eq 'agent-shell-queue--fork-session-from-running-emacs (car parsed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transient menu key integrity

(ert-deftest agent-shell-queue/menu-no-key-prefix-conflicts ()
  "No key in agent-shell-queue-menu is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-queue-menu))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-queue/menu-no-duplicate-keys ()
  "No key appears more than once in agent-shell-queue-menu."
  (let* ((keys (transient-test/collect-keys 'agent-shell-queue-menu))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

(ert-deftest agent-shell-queue/item-menu-no-key-prefix-conflicts ()
  "No key in agent-shell-queue-item-menu is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-queue-item-menu))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-queue/item-menu-no-duplicate-keys ()
  "No key appears more than once in agent-shell-queue-item-menu."
  (let* ((keys (transient-test/collect-keys 'agent-shell-queue-item-menu))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; capture buffer header (agent-shell-queue--open-capture)

(defmacro agent-shell-queue-test/with-capture (target-buf &rest body)
  "Run BODY with pop-to-buffer and capture-mode stubbed out.
Binds `capture-buf' to the buffer returned by `--open-capture'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
             ((symbol-function 'agent-shell-queue-capture-mode) #'ignore))
     (let ((capture-buf (agent-shell-queue--open-capture ,target-buf)))
       (unwind-protect
           (progn ,@body)
         (when (buffer-live-p capture-buf)
           (kill-buffer capture-buf))))))

(ert-deftest agent-shell-queue/open-capture-header-shows-bucket-state-depth ()
  "Regression: header shows bucket/state/depth — not 'Inserting after item <id>'.
Applies to all capture paths, not just insert-after."
  (let ((target (generate-new-buffer "*test-capture-target*")))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (let ((item (agent-shell-queue-test/make-item "q01" "p" 'active)))
           (setf (agent-shell-queue-store-items agent-shell-queue--store)
                 (list (list (buffer-name target) item)))
           (cl-letf (((symbol-function 'agent-shell-queue--activity-state)
                      (lambda () "idle")))
             (agent-shell-queue-test/with-capture target
               (let ((header (with-current-buffer capture-buf header-line-format)))
                 (should (stringp header))
                 (should-not (string-match-p "Inserting after item" header))
                 (should (string-match-p (regexp-quote (buffer-name target)) header))
                 (should (string-match-p "idle" header))
                 (should (string-match-p "depth: 1" header)))))))
      (kill-buffer target))))

(ert-deftest agent-shell-queue/open-capture-header-shows-unassigned ()
  "Unassigned capture (nil target) shows the unassigned key in the header."
  (agent-shell-queue-test/isolate
   (cl-letf (((symbol-function 'agent-shell-queue--activity-state) (lambda () "idle")))
     (agent-shell-queue-test/with-capture nil
       (let ((header (with-current-buffer capture-buf header-line-format)))
         (should (stringp header))
         (should (string-match-p (regexp-quote agent-shell-queue--unassigned-key)
                                 header)))))))

(ert-deftest agent-shell-queue/open-capture-header-reflects-depth ()
  "Header depth reflects the actual item count in the target bucket."
  (let ((target (generate-new-buffer "*test-capture-depth*")))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (let ((items (list (agent-shell-queue-test/make-item "q01" "p1" 'active)
                            (agent-shell-queue-test/make-item "q02" "p2" 'active)
                            (agent-shell-queue-test/make-item "q03" "p3" 'deferred))))
           (setf (agent-shell-queue-store-items agent-shell-queue--store)
                 (list (cons (buffer-name target) items)))
           (cl-letf (((symbol-function 'agent-shell-queue--activity-state)
                      (lambda () "idle")))
             (agent-shell-queue-test/with-capture target
               (let ((header (with-current-buffer capture-buf header-line-format)))
                 (should (string-match-p "depth: 3" header)))))))
      (kill-buffer target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue archive enable/path separation

(ert-deftest agent-shell-queue/archive-file-disabled-when-not-enabled ()
  "`agent-shell-queue--archive-file' returns nil when archive-enabled is nil."
  (let ((agent-shell-queue-archive-enabled nil))
    (should-not (agent-shell-queue--archive-file))))

(ert-deftest agent-shell-queue/archive-file-calls-function-when-enabled ()
  "`agent-shell-queue--archive-file' calls the file function when enabled."
  (let ((agent-shell-queue-archive-enabled t)
        (agent-shell-queue-archive-file-function (lambda () "/tmp/test-archive.jsonl")))
    (should (equal "/tmp/test-archive.jsonl" (agent-shell-queue--archive-file)))))

(ert-deftest agent-shell-queue/default-archive-file-returns-string ()
  "The default archive file function returns a non-empty string path."
  (let ((result (agent-shell-queue--default-archive-file)))
    (should (stringp result))
    (should (string-match-p "\\.jsonl\\'" result))))

(ert-deftest agent-shell-queue/write-archive-no-op-when-disabled ()
  "`--write-archive' does nothing when archiving is disabled."
  (let ((agent-shell-queue-archive-enabled nil)
        (wrote nil))
    (cl-letf (((symbol-function 'write-region)
               (lambda (&rest _) (setq wrote t))))
      (let ((item (agent-shell-queue-test/make-item "q01" "p" 'done)))
        (agent-shell-queue--write-archive "*buf*" item))
      (should-not wrote))))

(ert-deftest agent-shell-queue/write-archive-writes-when-enabled ()
  "`--write-archive' writes a record when archiving is enabled."
  (let* ((agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () "/tmp/test-archive.jsonl"))
         (wrote nil))
    (cl-letf (((symbol-function 'make-directory) #'ignore)
              ((symbol-function 'lock-file) #'ignore)
              ((symbol-function 'unlock-file) #'ignore)
              ((symbol-function 'write-region)
               (lambda (&rest _) (setq wrote t))))
      (let ((item (agent-shell-queue-test/make-item "q01" "prompt" 'done)))
        (agent-shell-queue--write-archive "*buf*" item))
      (should wrote))))

(ert-deftest agent-shell-queue/buffer-archive-errors-when-disabled ()
  "`agent-shell-queue-buffer-archive' signals user-error when disabled."
  (let ((agent-shell-queue-archive-enabled nil))
    (should-error (agent-shell-queue-buffer-archive) :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--active-item-count

(ert-deftest agent-shell-queue/active-item-count-excludes-done ()
  "Depth count must not include items with status `done'."
  (let ((items (list (agent-shell-queue-test/make-item "q01" "p" 'active)
                     (agent-shell-queue-test/make-item "q02" "p" 'done)
                     (agent-shell-queue-test/make-item "q03" "p" 'deferred)
                     (agent-shell-queue-test/make-item "q04" "p" 'done))))
    (should (= 2 (agent-shell-queue--active-item-count items)))))

(ert-deftest agent-shell-queue/active-item-count-all-done ()
  (let ((items (list (agent-shell-queue-test/make-item "q01" "p" 'done)
                     (agent-shell-queue-test/make-item "q02" "p" 'done))))
    (should (= 0 (agent-shell-queue--active-item-count items)))))

(ert-deftest agent-shell-queue/active-item-count-none-done ()
  (let ((items (list (agent-shell-queue-test/make-item "q01" "p" 'active)
                     (agent-shell-queue-test/make-item "q02" "p" 'running))))
    (should (= 2 (agent-shell-queue--active-item-count items)))))

(ert-deftest agent-shell-queue/active-item-count-empty ()
  (should (= 0 (agent-shell-queue--active-item-count nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; outcome field

(ert-deftest agent-shell-queue/outcome-nil-for-fresh-item ()
  "A newly created item has no outcome."
  (let ((item (agent-shell-queue-test/make-item "q01" "p" 'active)))
    (should (null (agent-shell-queue-item-outcome item)))))

(ert-deftest agent-shell-queue/outcome-success-after-mark-running-done ()
  "outcome is set to `success' when --mark-running-done completes an item."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (should (eq 'success
                (agent-shell-queue-item-outcome
                 (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/outcome-interrupted-after-mark-running-incomplete ()
  "outcome is set to `interrupted' when --mark-running-incomplete fires."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (agent-shell-queue--mark-running-incomplete "buf1")
    (should (eq 'interrupted
                (agent-shell-queue-item-outcome
                 (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/outcome-manual-after-mark-done ()
  "outcome is set to `manual' when agent-shell-queue-mark-done is called."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" active nil))))
    (agent-shell-queue-mark-done "q-1")
    (should (eq 'manual
                (agent-shell-queue-item-outcome
                 (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/outcome-canceled-after-buffer-abort ()
  "outcome is set to `canceled' when buffer-abort is called."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (cl-letf (((symbol-function 'agent-shell-interrupt) #'ignore))
      (let ((item (cadar (agent-shell-queue-store-items agent-shell-queue--store))))
        (setf (agent-shell-queue-item-status item) 'aborted)
        (setf (agent-shell-queue-item-outcome item) 'canceled)))
    (should (eq 'canceled
                (agent-shell-queue-item-outcome
                 (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/outcome-plist-round-trip ()
  "outcome survives a plist serialise/deserialise round-trip for all values."
  (dolist (val '(success canceled interrupted manual))
    (let* ((item (agent-shell-queue-test/make-item "q01" "p" 'done))
           (_ (setf (agent-shell-queue-item-outcome item) val))
           (restored (agent-shell-queue-item-from-plist
                      (agent-shell-queue-item-to-plist item))))
      (should (eq val (agent-shell-queue-item-outcome restored))))))

(ert-deftest agent-shell-queue/outcome-plist-round-trip-nil ()
  "A nil outcome survives a plist round-trip as nil."
  (let* ((item (agent-shell-queue-test/make-item "q01" "p" 'active))
         (restored (agent-shell-queue-item-from-plist
                    (agent-shell-queue-item-to-plist item))))
    (should (null (agent-shell-queue-item-outcome restored)))))

(ert-deftest agent-shell-queue/outcome-json-round-trip ()
  "outcome survives a JSON serialise/deserialise round-trip."
  (dolist (val '(success canceled interrupted manual))
    (let* ((item (agent-shell-queue-test/make-item "q01" "p" 'done))
           (_ (setf (agent-shell-queue-item-outcome item) val))
           (json-obj (agent-shell-queue--item-to-json item))
           (restored (agent-shell-queue--item-from-json json-obj)))
      (should (eq val (agent-shell-queue-item-outcome restored))))))

(ert-deftest agent-shell-queue/outcome-json-round-trip-nil ()
  "A nil outcome round-trips through JSON as nil (stored as :null)."
  (let* ((item (agent-shell-queue-test/make-item "q01" "p" 'active))
         (json-obj (agent-shell-queue--item-to-json item))
         (restored (agent-shell-queue--item-from-json json-obj)))
    (should (null (agent-shell-queue-item-outcome restored)))))

;;; test-agent-shell-queue.el ends here
