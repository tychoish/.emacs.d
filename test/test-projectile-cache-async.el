;;; test-projectile-cache-async.el --- Tests for non-blocking projectile cache purging -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Unit tests verifying non-blocking, debounced, and cooperative
;; cache purging in Projectile via setup-project.el.

;;; Code:

(require 'ert)
(require 'map)
(require 'projectile)
(require 'setup-project)

(defun projectile-test--wait-async (pred &optional timeout)
  "Spin the event loop until PRED is non-nil or TIMEOUT (default 3s) elapses."
  (let ((start (float-time))
        (limit (or timeout 3.0)))
    (while (and (not (funcall pred)) (< (- (float-time) start) limit))
      (sit-for 0.01))))

(ert-deftest projectile-cache-async/default-run-yields ()
  (let ((seen nil) (done nil) (interleaved nil))
    (let ((runner (projectile--run-incrementally
                   (lambda (item) (push item seen)) '(1 2)
                   :label "test" :on-done (lambda (_) (setq done t)))))
      (should (functionp runner))
      (should-not seen)
      (let ((timer (run-at-time 0 nil (lambda () (setq interleaved (not done))))))
        (unwind-protect
            (projectile-test--wait-async (lambda () done))
          (cancel-timer timer)))
      (should done)
      (should interleaved)
      (should (equal (nreverse seen) '(1 2))))))

(ert-deftest projectile-cache-async/concurrent-change-preserves-cache ()
  (let* ((root "/test/concurrent/")
         (projectile-projects-cache (make-hash-table :test #'equal))
         (original '("remove/a" "keep"))
         (updated '("remove/a" "keep" "new"))
         (done nil)
         (result 'pending))
    (setf (map-elt projectile-projects-cache root) original)
    (cl-letf (((symbol-function 'message) #'ignore))
      (projectile--filter-cache-incrementally
       (lambda (file)
         (setf (map-elt projectile-projects-cache root) updated)
         (string-prefix-p "remove/" file))
       root :chunk-size 1 :on-done (lambda (count) (setq result count done t)))
      (projectile-test--wait-async (lambda () done))
      (should done)
      (should-not result)
      (should (equal (map-elt projectile-projects-cache root) updated)))))

(ert-deftest projectile-cache-async/error-preserves-cache ()
  (let* ((root "/test/error/")
         (projectile-projects-cache (make-hash-table :test #'equal))
         (original '("remove/a" "bad" "keep"))
         (done nil)
         (result 'pending))
    (setf (map-elt projectile-projects-cache root) original)
    (cl-letf (((symbol-function 'message) #'ignore))
      (projectile--filter-cache-incrementally
       (lambda (file)
         (when (equal file "bad") (error "test failure"))
         (string-prefix-p "remove/" file))
       root :chunk-size 1 :on-done (lambda (count) (setq result count done t)))
      (projectile-test--wait-async (lambda () done))
      (should done)
      (should-not result)
      (should (equal (map-elt projectile-projects-cache root) original)))))

(ert-deftest projectile-cache-async/purge-file-debounces-flush ()
  "Purging a file removes it from in-memory cache and schedules flush rather than synchronous serialize."
  (let* ((proj-root "/test/fake-project/")
         (files '("a.txt" "b.txt" "c.txt"))
         (serialized-data nil)
         (flush-scheduled nil))
    (setf (map-elt projectile-projects-cache proj-root) files)
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _) proj-root))
              ((symbol-function 'projectile-persistent-cache-p) (lambda () t))
              ((symbol-function 'projectile-serialize) (lambda (data &rest _) (setq serialized-data data)))
              ((symbol-function 'projectile--schedule-cache-flush)
               (lambda (p) (setq flush-scheduled p))))
      (projectile-purge-file-from-cache "b.txt")
      ;; Should be removed immediately from in-memory cache
      (should (equal (map-elt projectile-projects-cache proj-root) '("a.txt" "c.txt")))
      ;; Should NOT call projectile-serialize synchronously
      (should-not serialized-data)
      ;; Should have scheduled cache flush for proj-root
      (should (equal flush-scheduled proj-root)))))

(ert-deftest projectile-cache-async/batch-deletion-coalesces-flush ()
  "Multiple file deletions remove from memory immediately and coalesce into scheduled flush."
  (let* ((proj-root "/test/fake-batch/")
         (files '("1.el" "2.el" "3.el" "4.el" "5.el"))
         (serialized-count 0)
         (flush-count 0))
    (setf (map-elt projectile-projects-cache proj-root) files)
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _) proj-root))
              ((symbol-function 'projectile-persistent-cache-p) (lambda () t))
              ((symbol-function 'projectile-serialize) (lambda (&rest _) (cl-incf serialized-count)))
              ((symbol-function 'projectile--schedule-cache-flush)
               (lambda (_) (cl-incf flush-count))))
      (dolist (f '("2.el" "4.el"))
        (projectile-purge-file-from-cache f))
      (should (equal (map-elt projectile-projects-cache proj-root) '("1.el" "3.el" "5.el")))
      (should (zerop serialized-count))
      (should (equal flush-count 2)))))

(ert-deftest projectile-cache-async/purge-dir-cooperative ()
  "Directory purge removes all matching prefix files incrementally without blocking."
  (let* ((proj-root "/test/fake-dir-proj/")
         (files '("src/main.rs" "src/lib.rs" "target/debug/app" "target/release/app" "README.md"))
         (done-count nil)
         (flush-scheduled nil))
    (setf (map-elt projectile-projects-cache proj-root) (copy-sequence files))
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _) proj-root))
              ((symbol-function 'projectile-persistent-cache-p) (lambda () t))
              ((symbol-function 'projectile--schedule-cache-flush)
               (lambda (p) (setq flush-scheduled p)))
              ((symbol-function 'message) #'ignore))
      (projectile-purge-dir-from-cache "target/"
                                       :on-done (lambda (count) (setq done-count count)))
      (projectile-test--wait-async (lambda () done-count))
      (should (equal done-count 2))
      (should (equal (map-elt projectile-projects-cache proj-root) '("src/main.rs" "src/lib.rs" "README.md")))
      (should (equal flush-scheduled proj-root)))))

(ert-deftest projectile-cache-async/purge-files-cooperative ()
  "Purging an explicit list of files removes them incrementally without blocking."
  (let* ((proj-root "/test/fake-files-proj/")
         (files '("a.el" "b.el" "c.el" "d.el"))
         (done-count nil)
         (flush-scheduled nil))
    (setf (map-elt projectile-projects-cache proj-root) (copy-sequence files))
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _) proj-root))
              ((symbol-function 'projectile-persistent-cache-p) (lambda () t))
              ((symbol-function 'projectile--schedule-cache-flush)
               (lambda (p) (setq flush-scheduled p)))
              ((symbol-function 'message) #'ignore))
      (projectile-purge-files-from-cache '("b.el" "d.el") proj-root
                                         :on-done (lambda (count) (setq done-count count)))
      (projectile-test--wait-async (lambda () done-count))
      (should (equal done-count 2))
      (should (equal (map-elt projectile-projects-cache proj-root) '("a.el" "c.el")))
      (should (equal flush-scheduled proj-root)))))

(ert-deftest projectile-cache-async/watches-reconciled-on-completion ()
  "Watches are re-derived on the shrunken cache when watches are enabled."
  (let* ((proj-root "/test/fake-watch-proj/")
         (files '("src/a.el" "src/b.el" "docs/guide.org"))
         (watched-files nil)
         (done nil))
    (setf (map-elt projectile-projects-cache proj-root) (copy-sequence files))
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _) proj-root))
              ((symbol-function 'projectile-persistent-cache-p) (lambda () nil))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'projectile--watch-project)
               (lambda (_root new-files) (setq watched-files new-files))))
      (let ((projectile-auto-update-cache-with-watches t)
            (projectile-enable-caching t))
        (projectile-purge-dir-from-cache "docs/" :on-done (lambda (_) (setq done t)))
        (projectile-test--wait-async (lambda () done))
        (should done)
        (should (equal watched-files '("src/a.el" "src/b.el")))))))

(ert-deftest projectile-cache-async/event-loop-progress-during-purge ()
  "Event loop timer fires while a large cache is purged incrementally."
  (let* ((proj-root "/test/large-project/")
         (files (let (lst)
                  (dotimes (i 2000)
                    (push (format "build/obj-%d.o" i) lst)
                    (push (format "src/code-%d.c" i) lst))
                  lst))
         (done nil)
         (timer-fired 0)
         (timer (run-at-time 0 0.005 (lambda () (cl-incf timer-fired)))))
    (setf (map-elt projectile-projects-cache proj-root) files)
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _) proj-root))
                  ((symbol-function 'projectile-persistent-cache-p) (lambda () t))
                  ((symbol-function 'projectile--schedule-cache-flush) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (projectile-purge-dir-from-cache "build/"
                                           :delay 0.005
                                           :on-done (lambda (_) (setq done t)))
          (projectile-test--wait-async (lambda () done) 5.0)
          (should done)
          (should (> timer-fired 0)))
      (cancel-timer timer))))

(provide 'test-projectile-cache-async)
;;; test-projectile-cache-async.el ends here
