;;; test-magit-gh-ci.el --- ERT tests for magit-gh-ci -*- lexical-binding: t -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-gh-ci/")
;;
;; Batch run:
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-magit-gh-ci.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-gh-ci/")'

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'magit-gh-ci)

;;; Test helpers

(defmacro magit-gh-ci-test/with-temp-dir (&rest body)
  "Execute BODY with `default-directory' set to a fresh temp directory."
  (declare (indent 0))
  `(let ((dir (make-temp-file "magit-gh-ci-test-" t)))
     (unwind-protect
         (let ((default-directory dir))
           ,@body)
       (delete-directory dir t))))

(defun magit-gh-ci-test/make-run (id name status conclusion workflow &optional sha)
  "Return a fake run alist."
  `((databaseId . ,id)
    (name . ,name)
    (status . ,status)
    (conclusion . ,conclusion)
    (workflowName . ,workflow)
    (createdAt . "2026-01-01T00:00:00Z")
    (headBranch . "feature/test")
    (headSha . ,(or sha "abc123def456"))))

;;; magit-gh-ci--failure-p

(ert-deftest magit-gh-ci/failure-p-failure ()
  (should (magit-gh-ci--failure-p "failure")))

(ert-deftest magit-gh-ci/failure-p-timed-out ()
  (should (magit-gh-ci--failure-p "timed_out")))

(ert-deftest magit-gh-ci/failure-p-startup-failure ()
  (should (magit-gh-ci--failure-p "startup_failure")))

(ert-deftest magit-gh-ci/failure-p-success ()
  (should-not (magit-gh-ci--failure-p "success")))

(ert-deftest magit-gh-ci/failure-p-cancelled ()
  (should-not (magit-gh-ci--failure-p "cancelled")))

(ert-deftest magit-gh-ci/failure-p-in-progress ()
  (should-not (magit-gh-ci--failure-p "in_progress")))

(ert-deftest magit-gh-ci/failure-p-nil ()
  (should-not (magit-gh-ci--failure-p nil)))

;;; magit-gh-ci--run-annotation

(ert-deftest magit-gh-ci/run-annotation-format ()
  (let* ((run (magit-gh-ci-test/make-run 1 "CI" "completed" "failure" "Test Suite"))
         (ann (magit-gh-ci--run-annotation run)))
    (should (stringp ann))
    (should (string-match-p "completed" ann))
    (should (string-match-p "failure" ann))
    (should (string-match-p "Test Suite" ann))))

(ert-deftest magit-gh-ci/run-annotation-in-progress ()
  (let* ((run `((databaseId . 99) (name . "CI") (status . "in_progress")
                (conclusion . nil) (workflowName . "Build")
                (createdAt . "2026-01-01T00:00:00Z")))
         (ann (magit-gh-ci--run-annotation run)))
    (should (string-match-p "in_progress" ann))))

;;; magit-gh-ci--select-run

(ert-deftest magit-gh-ci/select-run-single ()
  "Single run is returned directly without prompting."
  (let ((run (magit-gh-ci-test/make-run 42 "CI" "completed" "success" "Build")))
    (should (equal run (magit-gh-ci--select-run (list run))))))

(ert-deftest magit-gh-ci/select-run-multiple-prompts ()
  "Multiple runs invoke annotated-completing-read."
  (let* ((run1 (magit-gh-ci-test/make-run 1 "CI" "completed" "failure" "Build"))
         (run2 (magit-gh-ci-test/make-run 2 "CI" "completed" "success" "Build"))
         (called nil))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (table &rest _)
                 (setq called t)
                 "#2 CI")))
      (let ((result (magit-gh-ci--select-run (list run1 run2))))
        (should called)
        (should (= 2 (map-elt result 'databaseId)))))))

;;; Pipeline: step-finalize index structure

(ert-deftest magit-gh-ci/step-finalize-writes-index ()
  "Finalize writes an index.json with expected fields."
  (magit-gh-ci-test/with-temp-dir
    (let* ((run-info `((databaseId . 1)
                       (workflowName . "CI")
                       (status . "completed")
                       (conclusion . "failure")
                       (headBranch . "main")))
           (ctx (list :dir dir
                      :branch "main"
                      :run-info run-info
                      :files (list '(:path "run-info.json"  :type "metadata")
                                   '(:path "run-logs.ghlog"        :type "logs")
                                   '(:path "run-failed-logs.ghlog" :type "failed-logs")))))
      (magit-gh-ci--step-finalize ctx)
      (should (file-exists-p (expand-file-name "index.json" dir)))
      (let* ((raw (with-temp-buffer
                    (insert-file-contents (expand-file-name "index.json" dir))
                    (buffer-string)))
             (index (json-parse-string raw :object-type 'alist)))
        (should (equal "ci"        (map-elt index 'type)))
        (should (equal "failure"   (map-elt index 'conclusion)))
        (should (eq    t           (map-elt index 'has_failure)))
        (should (= 3               (map-elt index 'artifact_count)))
        (should (= 3               (length (map-elt index 'files))))))))

(ert-deftest magit-gh-ci/step-finalize-success-not-failure ()
  (magit-gh-ci-test/with-temp-dir
    (let* ((run-info `((databaseId . 2) (workflowName . "CI")
                       (status . "completed") (conclusion . "success")
                       (headBranch . "main")))
           (ctx (list :dir dir :branch "main"
                      :run-info run-info :files nil)))
      (magit-gh-ci--step-finalize ctx)
      (let* ((raw (with-temp-buffer
                    (insert-file-contents (expand-file-name "index.json" dir))
                    (buffer-string)))
             (index (json-parse-string raw :object-type 'alist)))
        (should (equal :false (map-elt index 'has_failure)))))))

;;; Pipeline: step-failed-logs skips when no failure

(ert-deftest magit-gh-ci/step-failed-logs-skips-on-success ()
  "When conclusion is success, step-failed-logs calls finalize directly."
  (magit-gh-ci-test/with-temp-dir
    (let* ((run-info `((databaseId . 3) (conclusion . "success")
                       (workflowName . "CI") (status . "completed")
                       (headBranch . "main")))
           (finalized nil)
           (ctx (list :dir dir :branch "main" :repo-dir dir
                      :run-info run-info :files nil)))
      (cl-letf (((symbol-function 'magit-gh-ci--step-finalize)
                 (lambda (_) (setq finalized t)))
                ((symbol-function 'magit-gh--run-process)
                 (lambda (&rest _) (error "should not be called"))))
        (magit-gh-ci--step-failed-logs ctx)
        (should finalized)))))

(ert-deftest magit-gh-ci/step-failed-logs-fetches-on-failure ()
  "When conclusion is failure and config enables it, gh is called."
  (magit-gh-ci-test/with-temp-dir
    (let* ((run-info `((databaseId . 4) (conclusion . "failure")
                       (workflowName . "CI") (status . "completed")
                       (headBranch . "main")))
           (gh-called nil)
           (ctx (list :dir dir :branch "main" :repo-dir dir
                      :run-info run-info :files nil))
           (magit-gh-ci-include-failed-log t))
      (cl-letf (((symbol-function 'magit-gh--run-process)
                 (lambda (args _dir on-success &optional _on-error)
                   (setq gh-called (car args))
                   (funcall on-success "failed log output"))))
        (magit-gh-ci--step-failed-logs ctx)
        (should (equal "run" gh-called))
        (should (file-exists-p (expand-file-name "run-failed-logs.ghlog" dir)))))))

;;; Pipeline: step-list-pr

(ert-deftest magit-gh-ci/step-list-pr-calls-gh-with-pr-flag ()
  "step-list-pr invokes gh run list with --pr flag."
  (magit-gh-ci-test/with-temp-dir
    (let* ((run (magit-gh-ci-test/make-run 99 "CI" "completed" "success" "Build"))
           (gh-args nil)
           (step-run-info-called nil)
           (ctx (list :pr-number 42 :branch "" :root dir :repo-dir dir :files nil)))
      (cl-letf (((symbol-function 'magit-gh--run-process)
                 (lambda (args _dir on-success &optional _on-error)
                   (setq gh-args args)
                   (funcall on-success (json-serialize (vector run)))))
                ((symbol-function 'magit-gh-ci--step-run-info)
                 (lambda (_) (setq step-run-info-called t))))
        (magit-gh-ci--step-list-pr ctx)
        (should step-run-info-called)
        (should (member "--pr" gh-args))
        (should (member "42" gh-args))
        (should (member "run" gh-args))
        (should (member "list" gh-args))))))

(ert-deftest magit-gh-ci/step-list-pr-passes-run-id-and-branch ()
  "step-list-pr forwards the selected run-id and headBranch to step-run-info."
  (magit-gh-ci-test/with-temp-dir
    (let* ((run (magit-gh-ci-test/make-run 77 "CI" "completed" "success" "Build"))
           (captured-ctx nil)
           (ctx (list :pr-number 7 :branch "" :root dir :repo-dir dir :files nil)))
      (cl-letf (((symbol-function 'magit-gh--run-process)
                 (lambda (_args _dir on-success &optional _on-error)
                   (funcall on-success (json-serialize (vector run)))))
                ((symbol-function 'magit-gh-ci--step-run-info)
                 (lambda (c) (setq captured-ctx c))))
        (magit-gh-ci--step-list-pr ctx)
        (should (= 77 (plist-get captured-ctx :run-id)))
        (should (equal "feature/test" (plist-get captured-ctx :branch)))))))

(ert-deftest magit-gh-ci/step-list-pr-errors-on-empty-runs ()
  "step-list-pr signals user-error when no runs are found."
  (magit-gh-ci-test/with-temp-dir
    (let ((ctx (list :pr-number 1 :branch "" :root dir :repo-dir dir :files nil)))
      (cl-letf (((symbol-function 'magit-gh--run-process)
                 (lambda (_args _dir on-success &optional _on-error)
                   (funcall on-success "[]"))))
        (should-error (magit-gh-ci--step-list-pr ctx) :type 'user-error)))))

(provide 'test-magit-gh-ci)
;;; test-magit-gh-ci.el ends here
