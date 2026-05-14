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
         (agent-shell-queue--subscriptions nil))
     (cl-letf (((symbol-function 'agent-shell-queue--save) #'ignore)
               ((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore))
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
   :created 1000.0))

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
  (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
    (should (equal "[A] " (agent-shell-queue--status-string item)))))

(ert-deftest agent-shell-queue/status-string-deferred ()
  (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'deferred nil)))
    (should (equal "[D] " (agent-shell-queue--status-string item)))))

(ert-deftest agent-shell-queue/status-string-active-background ()
  (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active t)))
    (should (equal "[AB]" (agent-shell-queue--status-string item)))))

(ert-deftest agent-shell-queue/status-string-deferred-background ()
  (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'deferred t)))
    (should (equal "[DB]" (agent-shell-queue--status-string item)))))

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
  "item-by-id finds items in any bucket."
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
              (should (equal "first" (agent-shell-queue-item-prompt (nth 0 items))))
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
  "add registers a turn-complete subscription for the target buffer."
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
  "When the last item in a bucket is removed, its subscription is dropped."
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
  "Removing a non-last item does not drop the subscription."
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
    (let ((item (cadar agent-shell-queue--items)))
      (should (eq 'deferred (agent-shell-queue-item-status item))))))

(ert-deftest agent-shell-queue/defer-deferred-to-active ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" deferred nil))))
    (agent-shell-queue-defer "q-1")
    (let ((item (cadar agent-shell-queue--items)))
      (should (eq 'active (agent-shell-queue-item-status item))))))

(ert-deftest agent-shell-queue/defer-twice-returns-to-active ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-defer "q-1")
    (agent-shell-queue-defer "q-1")
    (let ((item (cadar agent-shell-queue--items)))
      (should (eq 'active (agent-shell-queue-item-status item))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-toggle-background

(ert-deftest agent-shell-queue/toggle-background-on ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-toggle-background "q-1")
    (let ((item (cadar agent-shell-queue--items)))
      (should (agent-shell-queue-item-background item)))))

(ert-deftest agent-shell-queue/toggle-background-off ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active t))))
    (agent-shell-queue-toggle-background "q-1")
    (let ((item (cadar agent-shell-queue--items)))
      (should-not (agent-shell-queue-item-background item)))))

(ert-deftest agent-shell-queue/toggle-background-idempotent ()
  "Two toggles restore original state."
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-toggle-background "q-1")
    (agent-shell-queue-toggle-background "q-1")
    (let ((item (cadar agent-shell-queue--items)))
      (should-not (agent-shell-queue-item-background item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-edit

(ert-deftest agent-shell-queue/edit-replaces-prompt ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "old" active nil))))
    (agent-shell-queue-edit "q-1" "new prompt")
    (let ((item (cadar agent-shell-queue--items)))
      (should (equal "new prompt" (agent-shell-queue-item-prompt item))))))

(ert-deftest agent-shell-queue/edit-unknown-id-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "old" active nil))))
    (agent-shell-queue-edit "q-999" "new")
    (let ((item (cadar agent-shell-queue--items)))
      (should (equal "old" (agent-shell-queue-item-prompt item))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--move / move-up / move-down

(ert-deftest agent-shell-queue/move-up-middle ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-move-up "q-3")
    (let ((ids (mapcar #'agent-shell-queue-item-id (cdar agent-shell-queue--items))))
      (should (equal '("q-1" "q-3" "q-2") ids)))))

(ert-deftest agent-shell-queue/move-down-middle ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-move-down "q-1")
    (let ((ids (mapcar #'agent-shell-queue-item-id (cdar agent-shell-queue--items))))
      (should (equal '("q-2" "q-1" "q-3") ids)))))

(ert-deftest agent-shell-queue/move-up-at-top-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
    (agent-shell-queue-move-up "q-1")
    (let ((ids (mapcar #'agent-shell-queue-item-id (cdar agent-shell-queue--items))))
      (should (equal '("q-1" "q-2") ids)))))

(ert-deftest agent-shell-queue/move-down-at-bottom-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
    (agent-shell-queue-move-down "q-2")
    (let ((ids (mapcar #'agent-shell-queue-item-id (cdar agent-shell-queue--items))))
      (should (equal '("q-1" "q-2") ids)))))

(ert-deftest agent-shell-queue/move-only-item-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "a" active nil))))
    (agent-shell-queue-move-up "q-1")
    (agent-shell-queue-move-down "q-1")
    (let ((ids (mapcar #'agent-shell-queue-item-id (cdar agent-shell-queue--items))))
      (should (equal '("q-1") ids)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--retarget-item

(ert-deftest agent-shell-queue/retarget-moves-item ()
  (agent-shell-queue-test/isolate
    (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription) #'ignore)
              ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate
             '("buf1" ("q-1" "hello" active nil))
             '("buf2")))
      ;; seed buf2 bucket so it exists
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
      (agent-shell-queue--retarget-item "q-1" "buf2")
      (should-not (assoc "buf1" agent-shell-queue--items))
      (let ((bucket (assoc "buf2" agent-shell-queue--items)))
        (should bucket)
        (should (equal "q-1" (agent-shell-queue-item-id (cadr bucket))))))))

(ert-deftest agent-shell-queue/retarget-drops-old-subscription-when-empty ()
  (agent-shell-queue-test/isolate
    (let ((dropped nil)
          (ensured nil))
      (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription)
                 (lambda (name) (setq dropped name)))
                ((symbol-function 'agent-shell-queue--ensure-subscription)
                 (lambda (buf) (setq ensured (buffer-name buf)))))
        (setq agent-shell-queue--items
              (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
        (let ((new-buf (get-buffer-create " *asq-new-buf*")))
          (unwind-protect
              (progn
                (agent-shell-queue--retarget-item "q-1" (buffer-name new-buf))
                (should (equal "buf1" dropped))
                (should (equal (buffer-name new-buf) ensured)))
            (kill-buffer new-buf)))))))

(ert-deftest agent-shell-queue/retarget-same-buffer-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue--retarget-item "q-1" "buf1")
    (should (= 1 (length agent-shell-queue--items)))
    (should (assoc "buf1" agent-shell-queue--items))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistence: agent-shell-queue--save and --load

(ert-deftest agent-shell-queue/save-load-roundtrip ()
  "Items survive a save/load cycle."
  (let* ((tmp (make-temp-file "asq-test"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--items nil)
         (agent-shell-queue--counter 0)
         (agent-shell-queue--loaded t))
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
          ;; Counter should be restored to at least 5
          (should (>= agent-shell-queue--counter 5)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/load-missing-file-is-noop ()
  "Loading a nonexistent file leaves items nil and does not error."
  (let* ((agent-shell-queue-state-file-function
          (lambda () "/tmp/asq-test-definitely-does-not-exist-xyz"))
         (agent-shell-queue--items nil))
    (should-not (condition-case err
                    (progn (agent-shell-queue--load) nil)
                  (error err)))
    (should-not agent-shell-queue--items)))

(ert-deftest agent-shell-queue/load-corrupt-file-is-noop ()
  "A corrupt state file is silently ignored."
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
  "Counter is updated to max id found in persisted items."
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
  "Items survive a plist serialize/deserialize cycle without I/O."
  (agent-shell-queue-test/isolate
    (let ((agent-shell-queue-serialization-format 'plist))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate
             '("buf" ("q-3" "hello plist" deferred t))))
      (let* ((str   (agent-shell-queue--serialize))
             (items (agent-shell-queue--deserialize str))
             (item  (cadr (car items))))
        (should (equal "buf" (caar items)))
        (should (equal "q-3" (agent-shell-queue-item-id item)))
        (should (equal "hello plist" (agent-shell-queue-item-prompt item)))
        (should (eq 'deferred (agent-shell-queue-item-status item)))
        (should (agent-shell-queue-item-background item))))))

(ert-deftest agent-shell-queue/serialize-plist-symbols-survive ()
  "Status symbols and nil background survive plist round-trip as correct types."
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

(ert-deftest agent-shell-queue/serialize-plist-corrupt-is-error ()
  "Deserializing a non-list plist string signals an error."
  (agent-shell-queue-test/isolate
    (let ((agent-shell-queue-serialization-format 'plist))
      (should-error (agent-shell-queue--deserialize "not-a-list")
                    :type 'error))))

;;; Serialization: item-to-plist / item-from-plist (macro-generated)

(ert-deftest agent-shell-queue/item-to-plist-fields ()
  "agent-shell-queue-item-to-plist includes all five fields."
  (let* ((item (agent-shell-queue-test/make-item "q-1" "prompt" 'active t))
         (pl   (agent-shell-queue-item-to-plist item)))
    (should (equal "q-1"    (plist-get pl :id)))
    (should (equal "prompt" (plist-get pl :prompt)))
    (should (eq 'active     (plist-get pl :status)))
    (should (eq t           (plist-get pl :background)))
    (should (numberp        (plist-get pl :created)))))

(ert-deftest agent-shell-queue/item-from-plist-roundtrip ()
  "from-plist inverts to-plist exactly."
  (let* ((orig  (agent-shell-queue-test/make-item "q-7" "text" 'deferred nil))
         (item  (agent-shell-queue-item-from-plist
                 (agent-shell-queue-item-to-plist orig))))
    (should (equal (agent-shell-queue-item-id       orig) (agent-shell-queue-item-id       item)))
    (should (equal (agent-shell-queue-item-prompt   orig) (agent-shell-queue-item-prompt   item)))
    (should (eq    (agent-shell-queue-item-status   orig) (agent-shell-queue-item-status   item)))
    (should (eq    (agent-shell-queue-item-background orig) (agent-shell-queue-item-background item)))
    (should (=     (agent-shell-queue-item-created  orig) (agent-shell-queue-item-created  item)))))

;;; Serialization: JSON format

(ert-deftest agent-shell-queue/serialize-json-roundtrip ()
  "Items survive a JSON serialize/deserialize cycle."
  (skip-unless (fboundp 'json-serialize))
  (agent-shell-queue-test/isolate
    (let ((agent-shell-queue-serialization-format 'json))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate
             '("buf" ("q-4" "hello json" deferred t))))
      (let* ((str   (agent-shell-queue--serialize))
             (items (agent-shell-queue--deserialize str))
             (item  (cadr (car items))))
        (should (equal "buf" (caar items)))
        (should (equal "q-4" (agent-shell-queue-item-id item)))
        (should (equal "hello json" (agent-shell-queue-item-prompt item)))
        (should (eq 'deferred (agent-shell-queue-item-status item)))
        (should (agent-shell-queue-item-background item))))))

(ert-deftest agent-shell-queue/serialize-json-status-is-symbol ()
  "JSON round-trip returns status as an interned symbol, not a string."
  (skip-unless (fboundp 'json-serialize))
  (agent-shell-queue-test/isolate
    (let ((agent-shell-queue-serialization-format 'json))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate '("b" ("q-1" "p" active nil))))
      (let* ((item (cadr (car (agent-shell-queue--deserialize
                               (agent-shell-queue--serialize))))))
        (should (eq 'active (agent-shell-queue-item-status item)))
        (should (symbolp   (agent-shell-queue-item-status item)))))))

(ert-deftest agent-shell-queue/serialize-json-background-false ()
  "JSON false background survives as nil (not some other falsy value)."
  (skip-unless (fboundp 'json-serialize))
  (agent-shell-queue-test/isolate
    (let ((agent-shell-queue-serialization-format 'json))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate '("b" ("q-1" "p" active nil))))
      (let* ((item (cadr (car (agent-shell-queue--deserialize
                               (agent-shell-queue--serialize))))))
        (should-not (agent-shell-queue-item-background item))))))

(ert-deftest agent-shell-queue/save-load-roundtrip-json ()
  "Full save/load cycle with JSON format."
  (skip-unless (fboundp 'json-serialize))
  (let* ((tmp (make-temp-file "asq-json"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue-serialization-format 'json)
         (agent-shell-queue--items nil)
         (agent-shell-queue--counter 0)
         (agent-shell-queue--loaded t))
    (unwind-protect
        (progn
          (setq agent-shell-queue--items
                (agent-shell-queue-test/populate
                 '("mybuf" ("q-9" "json persist" active t))))
          (agent-shell-queue--save)
          (setq agent-shell-queue--items nil
                agent-shell-queue--counter 0)
          (agent-shell-queue--load)
          (let* ((pair (car agent-shell-queue--items))
                 (item (cadr pair)))
            (should (equal "mybuf"        (car pair)))
            (should (equal "q-9"          (agent-shell-queue-item-id item)))
            (should (equal "json persist" (agent-shell-queue-item-prompt item)))
            (should (eq 'active           (agent-shell-queue-item-status item)))
            (should (agent-shell-queue-item-background item)))
          (should (>= agent-shell-queue--counter 9)))
      (ignore-errors (delete-file tmp)))))

;;; Serialization: YAML format

(ert-deftest agent-shell-queue/serialize-yaml-roundtrip ()
  "Items survive a YAML serialize/deserialize cycle."
  (skip-unless (fboundp 'yaml-encode))
  (skip-unless (fboundp 'yaml-parse-string))
  (agent-shell-queue-test/isolate
    (let ((agent-shell-queue-serialization-format 'yaml))
      (setq agent-shell-queue--items
            (agent-shell-queue-test/populate
             '("buf" ("q-6" "hello yaml" active nil))))
      (let* ((str   (agent-shell-queue--serialize))
             (items (agent-shell-queue--deserialize str))
             (item  (cadr (car items))))
        (should (equal "buf"        (caar items)))
        (should (equal "q-6"        (agent-shell-queue-item-id item)))
        (should (equal "hello yaml" (agent-shell-queue-item-prompt item)))
        (should (eq 'active         (agent-shell-queue-item-status item)))
        (should-not (agent-shell-queue-item-background item))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-send-item

(ert-deftest agent-shell-queue/send-item-calls-insert ()
  "send-item calls agent-shell-insert with the prompt and removes the item."
  (agent-shell-queue-test/isolate-no-sub
    (let ((inserted-text nil)
          (inserted-buf nil)
          (buf (get-buffer-create " *asq-send-test*")))
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
              (should (eq buf inserted-buf))
              (should-not agent-shell-queue--items)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-item-background-wraps-prompt ()
  "Background items are prefixed with `agent-shell-queue-background-prefix'."
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
  "Sending to a dead buffer signals user-error."
  (agent-shell-queue-test/isolate-no-sub
    (setq agent-shell-queue--items
          (agent-shell-queue-test/populate '("dead-buf" ("q-1" "hello" active nil))))
    (cl-letf (((symbol-function 'get-buffer) (lambda (_) nil)))
      (should-error (agent-shell-queue-send-item "q-1") :type 'user-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--ensure-subscription and --drop-subscription

(ert-deftest agent-shell-queue/ensure-subscription-registers ()
  "First call registers a turn-complete subscription."
  (agent-shell-queue-test/isolate
    (let ((subscribe-calls nil)
          (buf (get-buffer-create " *asq-sub-test*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                     (lambda (&rest args)
                       (push args subscribe-calls)
                       42)))
            (agent-shell-queue--ensure-subscription buf)
            ;; Two calls: one for turn-complete, one for clean-up
            (should (= 2 (length subscribe-calls)))
            (let ((events (mapcar (lambda (a) (plist-get a :event)) subscribe-calls)))
              (should (member 'turn-complete events))
              (should (member 'clean-up events)))
            (should (assoc (buffer-name buf) agent-shell-queue--subscriptions)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/ensure-subscription-no-duplicates ()
  "Second call to ensure-subscription does not register again."
  (agent-shell-queue-test/isolate
    (let ((call-count 0)
          (buf (get-buffer-create " *asq-sub-dup-test*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                     (lambda (&rest _) (cl-incf call-count) call-count)))
            (agent-shell-queue--ensure-subscription buf)
            (agent-shell-queue--ensure-subscription buf)
            ;; Both turn-complete and clean-up registered once each = 2 calls
            (should (= 2 call-count)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/drop-subscription-removes-entry ()
  "drop-subscription removes the registry entry."
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
  "drop-subscription does not error when the buffer is already dead."
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--subscriptions (list (cons "dead-buf" 99)))
    (should-not (condition-case err
                    (progn (agent-shell-queue--drop-subscription "dead-buf") nil)
                  (error err)))
    (should-not (assoc "dead-buf" agent-shell-queue--subscriptions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--auto-send (backup timer behavior)

(ert-deftest agent-shell-queue/auto-send-skips-deferred ()
  "auto-send never sends deferred items."
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
  "auto-send does not send when target buffer is busy."
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
