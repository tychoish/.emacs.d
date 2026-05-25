;;; test-agent-shell-queue-persistence.el --- Round-trip persistence tests -*- lexical-binding: t -*-

;; Tests that queue state survives save/load cycles as it would between
;; Emacs sessions.  The queue is always paused so no items are dispatched
;; unless a test explicitly unpauses and calls send-item.
;;
;; Run filtered:
;;   (ert "^agent-shell-queue/persist")

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defmacro agent-shell-queue-test/with-persist-file (&rest body)
  "Run BODY with an isolated queue backed by a real temp plist state file.
Unlike `agent-shell-queue-test/isolate', `agent-shell-queue--save' and
`agent-shell-queue--load' execute for real against the temp file.
The queue starts paused so nothing is dispatched automatically."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "asq-persist-" nil ".el"))
          (agent-shell-queue--store
           (agent-shell-queue--make-store :items nil :format 'plist :file tmp))
          (agent-shell-queue--queue
           (agent-shell-queue-queue--make :paused t))
          (agent-shell-queue--loaded t)
          (agent-shell-queue--subscriptions nil)
          (agent-shell-queue--stale-item-ids nil)
          (agent-shell-queue--next-flush-time nil)
          (agent-shell-queue--wait-timers nil)
          (agent-shell-queue--compact-running nil)
          (agent-shell-queue--response-start-positions nil)
          (agent-shell-queue--last-flush-time nil))
     (cl-letf (((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore)
               ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
               ((symbol-function 'agent-shell-queue--drop-subscription) #'ignore)
               ((symbol-function 'agent-shell-queue--alert-if-empty) #'ignore)
               ((symbol-function 'alert) #'ignore))
       (unwind-protect
           (progn ,@body)
         (ignore-errors (delete-file tmp))))))

(defun agent-shell-queue-test/persist-item (id prompt &optional kind status)
  "Build a minimal item for persistence tests."
  (agent-shell-queue-item--make
   :id id
   :prompt prompt
   :status (or status 'active)
   :kind (or kind 'emacs)
   :background nil
   :created 1000.0))

(defun agent-shell-queue-test/persist-save-and-reload ()
  "Save the live store to disk then reload it, simulating a session restart.
Clears in-memory items first so the load result is unambiguous."
  (agent-shell-queue--save)
  (setf (agent-shell-queue-store-items agent-shell-queue--store) nil)
  (agent-shell-queue--load))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic round-trip

(ert-deftest agent-shell-queue/persist-active-items-survive-round-trip ()
  "Active items written to disk are present after a load."
  (agent-shell-queue-test/with-persist-file
    (let ((item (agent-shell-queue-test/persist-item "q01" "test prompt")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*test-session*" item)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((restored (cdr (assoc "*test-session*"
                                  (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 1 (length restored)))
        (should (equal "q01" (agent-shell-queue-item-id (car restored))))))))

(ert-deftest agent-shell-queue/persist-deferred-items-survive ()
  "Deferred items persist across save/load."
  (agent-shell-queue-test/with-persist-file
    (let ((item (agent-shell-queue-test/persist-item "q01" "held" nil 'deferred)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*s*" item)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((restored (car (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store))))))
        (should restored)
        (should (eq 'deferred (agent-shell-queue-item-status restored)))))))

(ert-deftest agent-shell-queue/persist-empty-queue-round-trips ()
  "An empty queue produces no items after save/load."
  (agent-shell-queue-test/with-persist-file
    (agent-shell-queue-test/persist-save-and-reload)
    (should (null (agent-shell-queue-store-items agent-shell-queue--store)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filtering: only running items are not persisted; done items are kept

(ert-deftest agent-shell-queue/persist-done-items-are-saved ()
  "Done items are now persisted so their responses survive session reloads."
  (agent-shell-queue-test/with-persist-file
    (let ((done (agent-shell-queue-test/persist-item "q01" "finished" nil 'done))
          (active (agent-shell-queue-test/persist-item "q02" "pending")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*s*" done active)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((items (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 2 (length items)))
        (let ((restored-done (seq-find (lambda (it) (equal "q01" (agent-shell-queue-item-id it))) items))
              (restored-active (seq-find (lambda (it) (equal "q02" (agent-shell-queue-item-id it))) items)))
          (should restored-done)
          (should restored-active)
          (should (eq 'done (agent-shell-queue-item-status restored-done))))))))

(ert-deftest agent-shell-queue/persist-running-items-reload-as-active ()
  "Running items are saved and reload as active with dispatched cleared."
  (agent-shell-queue-test/with-persist-file
    (let ((running (agent-shell-queue-test/persist-item "q01" "in-flight" nil 'running))
          (active (agent-shell-queue-test/persist-item "q02" "queued")))
      (setf (agent-shell-queue-item-dispatched running) 999.0)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*s*" running active)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let* ((items (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store))))
             (restored-running (seq-find (lambda (it) (equal "q01" (agent-shell-queue-item-id it))) items))
             (restored-active (seq-find (lambda (it) (equal "q02" (agent-shell-queue-item-id it))) items)))
        (should (= 2 (length items)))
        (should restored-running)
        (should (eq 'active (agent-shell-queue-item-status restored-running)))
        (should (null (agent-shell-queue-item-dispatched restored-running)))
        (should restored-active)
        (should (eq 'active (agent-shell-queue-item-status restored-active)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Field fidelity

(ert-deftest agent-shell-queue/persist-item-fields-preserved ()
  "All serializable item fields survive a save/load round-trip."
  (agent-shell-queue-test/with-persist-file
    (let ((item (agent-shell-queue-item--make
                 :id "q42"
                 :prompt "(message \"hello\")"
                 :status 'active
                 :kind 'emacs
                 :background t
                 :created 12345.0
                 :dispatched nil
                 :completed nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*s*" item)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((r (car (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store))))))
        (should (equal "q42"               (agent-shell-queue-item-id r)))
        (should (equal "(message \"hello\")" (agent-shell-queue-item-prompt r)))
        (should (eq 'active                (agent-shell-queue-item-status r)))
        (should (eq 'emacs                 (agent-shell-queue-item-kind r)))
        (should (eq t                      (agent-shell-queue-item-background r)))
        (should (= 12345.0                 (agent-shell-queue-item-created r)))))))

(ert-deftest agent-shell-queue/persist-emacs-kind-item-survives ()
  "An emacs-kind item round-trips with kind and prompt intact."
  (agent-shell-queue-test/with-persist-file
    (let ((item (agent-shell-queue-test/persist-item "q01" "(+ 1 2)" 'emacs)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*s*" item)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((r (car (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store))))))
        (should (eq 'emacs    (agent-shell-queue-item-kind r)))
        (should (equal "(+ 1 2)" (agent-shell-queue-item-prompt r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multiple buckets

(ert-deftest agent-shell-queue/persist-multiple-buckets-survive ()
  "Items across multiple buckets all persist independently."
  (agent-shell-queue-test/with-persist-file
    (let ((a (agent-shell-queue-test/persist-item "qa" "prompt-a"))
          (b (agent-shell-queue-test/persist-item "qb" "prompt-b")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*session-a*" a)
                  (list "*session-b*" b)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((items-a (cdr (assoc "*session-a*" (agent-shell-queue-store-items agent-shell-queue--store))))
            (items-b (cdr (assoc "*session-b*" (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 1 (length items-a)))
        (should (= 1 (length items-b)))
        (should (equal "qa" (agent-shell-queue-item-id (car items-a))))
        (should (equal "qb" (agent-shell-queue-item-id (car items-b))))))))

(ert-deftest agent-shell-queue/persist-ordering-preserved ()
  "Items in a bucket preserve their insertion order across save/load."
  (agent-shell-queue-test/with-persist-file
    (let ((items (list (agent-shell-queue-test/persist-item "q01" "first")
                       (agent-shell-queue-test/persist-item "q02" "second")
                       (agent-shell-queue-test/persist-item "q03" "third"))))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (cons "*s*" items)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((restored (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (equal '("q01" "q02" "q03")
                       (seq-map #'agent-shell-queue-item-id restored)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paused queue persistence

(ert-deftest agent-shell-queue/persist-paused-queue-items-stay-active ()
  "Items added to a paused queue are never dispatched and remain active,
ensuring they survive the save/load cycle."
  (agent-shell-queue-test/with-persist-file
    ;; Queue is paused (the macro default); add three items.
    (let ((items (list (agent-shell-queue-test/persist-item "q01" "a")
                       (agent-shell-queue-test/persist-item "q02" "b")
                       (agent-shell-queue-test/persist-item "q03" "c"))))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (cons "*s*" items)))
      (should (agent-shell-queue-queue-paused agent-shell-queue--queue))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((restored (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 3 (length restored)))
        (should (seq-every-p (lambda (it) (eq 'active (agent-shell-queue-item-status it)))
                             restored))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs kind: dispatch without LLM

(defvar agent-shell-queue-persist-test--side-effect nil
  "Scratch variable written by emacs-kind test items.")

(ert-deftest agent-shell-queue/persist-emacs-kind-dispatches-without-llm ()
  "An emacs-kind item executes Lisp synchronously with no LLM interaction.
The item completes and the side effect is visible."
  (agent-shell-queue-test/with-persist-file
    (let* ((target (generate-new-buffer "*asq-persist-emacs-target*"))
           (buf-name (buffer-name target))
           (item (agent-shell-queue-item--make
                  :id "q01"
                  :prompt (format "(setq agent-shell-queue-persist-test--side-effect %S)" 42)
                  :status 'active
                  :kind 'emacs
                  :background nil
                  :created 1000.0)))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list buf-name item)))
            (setq agent-shell-queue-persist-test--side-effect nil)
            ;; Unpause and dispatch.
            (setf (agent-shell-queue-queue-paused agent-shell-queue--queue) nil)
            (cl-letf (((symbol-function 'run-with-timer) #'ignore)
                      ((symbol-function 'agent-shell-queue--append-done-log) #'ignore))
              (agent-shell-queue-send-item "q01"))
            ;; Side effect ran.
            (should (= 42 agent-shell-queue-persist-test--side-effect))
            ;; Item is done.
            (should (eq 'done (agent-shell-queue-item-status item))))
        (kill-buffer target)))))

(ert-deftest agent-shell-queue/persist-emacs-kind-survives-then-dispatches ()
  "An emacs-kind item survives a save/load cycle, then dispatches without LLM."
  (agent-shell-queue-test/with-persist-file
    (let* ((target (generate-new-buffer "*asq-persist-emacs-target2*"))
           (buf-name (buffer-name target))
           (item (agent-shell-queue-item--make
                  :id "q01"
                  :prompt (format "(setq agent-shell-queue-persist-test--side-effect %S)" 99)
                  :status 'active
                  :kind 'emacs
                  :background nil
                  :created 1000.0)))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list buf-name item)))
            ;; Save and reload while still paused.
            (agent-shell-queue-test/persist-save-and-reload)
            (let ((restored (car (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))))
              (should restored)
              (should (eq 'emacs (agent-shell-queue-item-kind restored)))
              ;; Now unpause and dispatch the restored item.
              (setq agent-shell-queue-persist-test--side-effect nil)
              (setf (agent-shell-queue-queue-paused agent-shell-queue--queue) nil)
              (cl-letf (((symbol-function 'run-with-timer) #'ignore)
                        ((symbol-function 'agent-shell-queue--append-done-log) #'ignore))
                (agent-shell-queue-send-item (agent-shell-queue-item-id restored)))
              (should (= 99 agent-shell-queue-persist-test--side-effect))
              (should (eq 'done (agent-shell-queue-item-status restored)))))
        (kill-buffer target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bulk archive

(ert-deftest agent-shell-queue/persist-archive-done-n-removes-oldest ()
  "archive-done-n removes the N oldest done items and leaves the rest."
  (agent-shell-queue-test/with-persist-file
    (let ((agent-shell-queue-archive-enabled t))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive) #'ignore))
        (let ((old1 (agent-shell-queue-item--make
                     :id "q01" :prompt "oldest" :status 'done
                     :kind 'emacs :background nil :created 100.0))
              (old2 (agent-shell-queue-item--make
                     :id "q02" :prompt "middle" :status 'done
                     :kind 'emacs :background nil :created 200.0))
              (new1 (agent-shell-queue-item--make
                     :id "q03" :prompt "newest" :status 'done
                     :kind 'emacs :background nil :created 300.0))
              (active (agent-shell-queue-test/persist-item "q04" "active")))
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (list (list "*s*" old1 old2 new1 active)))
          (agent-shell-queue-archive-done-n 2)
          (let ((items (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store)))))
            (should (= 2 (length items)))
            (should (seq-find (lambda (it) (equal "q03" (agent-shell-queue-item-id it))) items))
            (should (seq-find (lambda (it) (equal "q04" (agent-shell-queue-item-id it))) items))
            (should-not (seq-find (lambda (it) (equal "q01" (agent-shell-queue-item-id it))) items))
            (should-not (seq-find (lambda (it) (equal "q02" (agent-shell-queue-item-id it))) items))))))))

(ert-deftest agent-shell-queue/persist-archive-done-all-removes-all-done ()
  "archive-done-all removes every done item across all buckets."
  (agent-shell-queue-test/with-persist-file
    (let ((agent-shell-queue-archive-enabled t))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive) #'ignore))
        (let ((done-a (agent-shell-queue-item--make
                       :id "qa1" :prompt "done-a" :status 'done
                       :kind 'emacs :background nil :created 100.0))
              (done-b (agent-shell-queue-item--make
                       :id "qb1" :prompt "done-b" :status 'done
                       :kind 'emacs :background nil :created 200.0))
              (active-a (agent-shell-queue-test/persist-item "qa2" "active-a"))
              (active-b (agent-shell-queue-test/persist-item "qb2" "active-b")))
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (list (list "*session-a*" done-a active-a)
                      (list "*session-b*" done-b active-b)))
          (agent-shell-queue-archive-done-all)
          (let ((items-a (cdr (assoc "*session-a*" (agent-shell-queue-store-items agent-shell-queue--store))))
                (items-b (cdr (assoc "*session-b*" (agent-shell-queue-store-items agent-shell-queue--store)))))
            (should (= 1 (length items-a)))
            (should (= 1 (length items-b)))
            (should (equal "qa2" (agent-shell-queue-item-id (car items-a))))
            (should (equal "qb2" (agent-shell-queue-item-id (car items-b))))))))))

(ert-deftest agent-shell-queue/persist-archive-done-n-errors-when-none ()
  "archive-done-n signals user-error when no done items exist."
  (agent-shell-queue-test/with-persist-file
    (let ((agent-shell-queue-archive-enabled t))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive) #'ignore))
        (let ((active (agent-shell-queue-test/persist-item "q01" "active")))
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (list (list "*s*" active)))
          (should-error (agent-shell-queue-archive-done-n 5) :type 'user-error))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load archive

(ert-deftest agent-shell-queue/persist-load-archive-imports-as-active ()
  "load-archive reads a JSONL file and imports each line as an active item."
  (agent-shell-queue-test/with-persist-file
    (let* ((archive-file (make-temp-file "asq-archive-" nil ".jsonl"))
           (agent-shell-queue-archive-enabled t)
           (agent-shell-queue-archive-file-function (lambda () archive-file)))
      (unwind-protect
          (progn
            (with-temp-file archive-file
              (insert "{\"prompt\":\"hello world\",\"kind\":\"prompt\",\"background\":false,\"target\":\"*nonexistent*\"}\n")
              (insert "{\"prompt\":\"(+ 1 2)\",\"kind\":\"emacs\",\"background\":true,\"target\":\"*also-gone*\"}\n"))
            (agent-shell-queue-load-archive archive-file)
            (let* ((all-items
                    (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                                 (seq-mapcat #'cdr))))
              (should (= 2 (length all-items)))
              (should (seq-every-p (lambda (it) (eq 'active (agent-shell-queue-item-status it)))
                                   all-items))
              (should (seq-find (lambda (it) (equal "hello world" (agent-shell-queue-item-prompt it)))
                                all-items))
              (should (seq-find (lambda (it) (equal "(+ 1 2)" (agent-shell-queue-item-prompt it)))
                                all-items))
              (let ((emacs-item (seq-find (lambda (it) (eq 'emacs (agent-shell-queue-item-kind it)))
                                          all-items)))
                (should emacs-item)
                (should (eq t (agent-shell-queue-item-background emacs-item))))))
        (ignore-errors (delete-file archive-file))))))

(provide 'test-agent-shell-queue-persistence)
;;; test-agent-shell-queue-persistence.el ends here
