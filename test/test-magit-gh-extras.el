;;; test-magit-gh-extras.el --- ERT tests for magit-gh-extras -*- lexical-binding: t -*-

;; Run inside a live Emacs session with the full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^magit-gh-extras/")

(require 'ert)
(require 'cl-lib)
(require 'ht)
(require 'magit-gh-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--pr-closed-p (pure)

(ert-deftest magit-gh-extras/pr-closed-p-merged ()
  (should (magit-gh--pr-closed-p '((state . "MERGED")))))

(ert-deftest magit-gh-extras/pr-closed-p-closed ()
  (should (magit-gh--pr-closed-p '((state . "CLOSED")))))

(ert-deftest magit-gh-extras/pr-closed-p-open ()
  (should-not (magit-gh--pr-closed-p '((state . "OPEN")))))

(ert-deftest magit-gh-extras/pr-closed-p-draft ()
  (should-not (magit-gh--pr-closed-p '((state . "DRAFT")))))

(ert-deftest magit-gh-extras/pr-closed-p-missing-state ()
  (should-not (magit-gh--pr-closed-p '((number . 42)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-parse-branch-label (pure)

(ert-deftest magit-gh-extras/parse-branch-label-plain ()
  (should (equal "feat-x"
                 (magit-gh--prune-parse-branch-label "prune: feat-x"))))

(ert-deftest magit-gh-extras/parse-branch-label-marked ()
  (should (equal "feat-x"
                 (magit-gh--prune-parse-branch-label "prune: feat-x [marked]"))))

(ert-deftest magit-gh-extras/parse-branch-label-with-slashes ()
  (should (equal "user/feat-x"
                 (magit-gh--prune-parse-branch-label "prune: user/feat-x")))
  (should (equal "user/feat-x"
                 (magit-gh--prune-parse-branch-label "prune: user/feat-x [marked]"))))

(ert-deftest magit-gh-extras/parse-branch-label-non-prune ()
  (should-not (magit-gh--prune-parse-branch-label "exit menu"))
  (should-not (magit-gh--prune-parse-branch-label "refresh"))
  (should-not (magit-gh--prune-parse-branch-label "prune all branches (no prompt)"))
  (should-not (magit-gh--prune-parse-branch-label "mark branch for pruning")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-format-annotation (pure)

(ert-deftest magit-gh-extras/format-annotation ()
  (let ((pr '((number . 42) (state . "MERGED") (title . "Add feature X"))))
    (should (equal "PR #42 MERGED: Add feature X"
                   (magit-gh--prune-format-annotation pr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-build-menu (pure)

(ert-deftest magit-gh-extras/build-menu-empty ()
  "With no candidates, menu has only exit + refresh."
  (let ((table (magit-gh--prune-build-menu nil nil)))
    (should (ht-contains? table "exit menu"))
    (should (ht-contains? table "refresh"))
    (should-not (ht-contains? table "prune all branches (no prompt)"))
    (should-not (ht-contains? table "prune all branches (with prompt)"))
    (should-not (ht-contains? table "mark branch for pruning"))
    (should-not (ht-contains? table "prune marked branches"))
    (should (= 2 (ht-size table)))))

(ert-deftest magit-gh-extras/build-menu-with-candidates ()
  "With candidates, bulk options appear plus one entry per branch."
  (let* ((candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                       ("b" . ((number . 2) (state . "CLOSED") (title . "B")))))
         (table (magit-gh--prune-build-menu candidates nil)))
    (should (ht-contains? table "exit menu"))
    (should (ht-contains? table "refresh"))
    (should (ht-contains? table "prune all branches (no prompt)"))
    (should (ht-contains? table "prune all branches (with prompt)"))
    (should (ht-contains? table "mark branch for pruning"))
    (should (ht-contains? table "prune: a"))
    (should (ht-contains? table "prune: b"))
    (should-not (ht-contains? table "prune marked branches"))
    (should-not (ht-contains? table "prune: a [marked]"))))

(ert-deftest magit-gh-extras/build-menu-with-marked ()
  "Marked branches show [marked] suffix and surface the prune-marked entry."
  (let* ((candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                       ("b" . ((number . 2) (state . "CLOSED") (title . "B")))))
         (table (magit-gh--prune-build-menu candidates '("a"))))
    (should (ht-contains? table "prune marked branches"))
    (should (ht-contains? table "prune: a [marked]"))
    (should (ht-contains? table "prune: b"))
    (should-not (ht-contains? table "prune: a"))))

(ert-deftest magit-gh-extras/build-menu-annotations-have-pr-info ()
  (let* ((candidates '(("feat" . ((number . 99) (state . "MERGED") (title . "Big change")))))
         (table (magit-gh--prune-build-menu candidates nil)))
    (should (equal "PR #99 MERGED: Big change"
                   (ht-get table "prune: feat")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-scan

(defun magit-gh-extras-test--make-pr-table (alist)
  "Build a hash table of branch→pr-alist from ALIST for use in scan mocks."
  (let ((table (make-hash-table :test #'equal)))
    (seq-do (lambda (pair) (puthash (car pair) (cdr pair) table)) alist)
    table))

(ert-deftest magit-gh-extras/scan-collects-closed-prs ()
  "scan keeps only branches whose PR is merged or closed."
  (let ((prs (magit-gh-extras-test--make-pr-table
              '(("a" . ((number . 1) (state . "MERGED")))
                ("c" . ((number . 3) (state . "CLOSED"))))))
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-gh--repo-dir) (lambda () "/tmp/r"))
              ((symbol-function 'magit-gh--default-branch) (lambda () "main"))
              ((symbol-function 'magit-get-current-branch) (lambda () "current"))
              ((symbol-function 'magit-gh--fetch-closed-prs) (lambda (&optional _) prs))
              ((symbol-function 'magit-list-local-branch-names) (lambda () '("a" "b" "c" "d"))))
      (let ((result (magit-gh--prune-scan)))
        (should (equal '("a" "c") (seq-map #'car result)))
        (should (equal '("a" "c")
                       (seq-map #'car (plist-get (magit-gh--cache-get "/tmp/r" :prune-state) :candidates))))))))

(ert-deftest magit-gh-extras/scan-drops-stale-marked ()
  "Marked branches no longer in candidate set are dropped."
  (let ((prs (magit-gh-extras-test--make-pr-table
              '(("a" . ((number . 1) (state . "MERGED")))
                ("c" . ((number . 3) (state . "CLOSED"))))))
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-gh--repo-dir) (lambda () "/tmp/r"))
              ((symbol-function 'magit-gh--default-branch) (lambda () "main"))
              ((symbol-function 'magit-get-current-branch) (lambda () "current"))
              ((symbol-function 'magit-gh--fetch-closed-prs) (lambda (&optional _) prs))
              ((symbol-function 'magit-list-local-branch-names) (lambda () '("a" "b" "c"))))
      ;; 'gone' is stale (not a branch); 'b' has no closed PR (not a candidate)
      (magit-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates nil :marked '("a" "gone" "b")))
      (magit-gh--prune-scan)
      (should (equal '("a") (plist-get (magit-gh--cache-get "/tmp/r" :prune-state) :marked))))))

(ert-deftest magit-gh-extras/scan-empty ()
  "scan with no closed PRs yields empty candidates."
  (let ((prs (make-hash-table :test #'equal))
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-gh--repo-dir) (lambda () "/tmp/r"))
              ((symbol-function 'magit-gh--default-branch) (lambda () "main"))
              ((symbol-function 'magit-get-current-branch) (lambda () "current"))
              ((symbol-function 'magit-gh--fetch-closed-prs) (lambda (&optional _) prs))
              ((symbol-function 'magit-list-local-branch-names) (lambda () '("a"))))
      (let ((result (magit-gh--prune-scan)))
        (should-not result)
        (let ((state (magit-gh--cache-get "/tmp/r" :prune-state)))
          (should-not (plist-get state :candidates))
          (should-not (plist-get state :marked)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-delete-branches

(ert-deftest magit-gh-extras/delete-no-prompt ()
  "With PROMPT-P nil, all branches are deleted without read-char-choice."
  (let ((deleted nil)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (error "should not be called")))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (let ((result (magit-gh--prune-delete-branches '("a" "b") "/tmp/r" nil)))
        (should (equal '("a" "b") deleted))
        (should (= 2 (plist-get result :deleted)))
        (should (= 0 (plist-get result :skipped)))
        (should-not (plist-get result :quit))))))

(ert-deftest magit-gh-extras/delete-with-prompt-yes ()
  (let ((deleted nil)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) ?y))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (let ((result (magit-gh--prune-delete-branches '("a" "b") "/tmp/r" t)))
        (should (equal '("a" "b") deleted))
        (should (= 2 (plist-get result :deleted)))))))

(ert-deftest magit-gh-extras/delete-with-prompt-no-skips ()
  "All ?n answers leave the branches untouched."
  (let ((deleted nil)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) ?n))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (let ((result (magit-gh--prune-delete-branches '("a" "b") "/tmp/r" t)))
        (should-not deleted)
        (should (= 0 (plist-get result :deleted)))
        (should (= 2 (plist-get result :skipped)))))))

(ert-deftest magit-gh-extras/delete-with-prompt-quit ()
  "Quit terminates the loop after the current branch."
  (let ((deleted nil)
        (calls 0)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (setq calls (1+ calls)) (if (= calls 1) ?y ?q)))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (let ((result (magit-gh--prune-delete-branches '("a" "b" "c") "/tmp/r" t)))
        (should (equal '("a") deleted))
        (should (plist-get result :quit))
        (should (= 1 (plist-get result :deleted)))))))

(ert-deftest magit-gh-extras/delete-yes-to-all ()
  "Bang answer enables yes-to-all for the remainder."
  (let ((deleted nil)
        (calls 0)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (setq calls (1+ calls)) ?!))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (let ((result (magit-gh--prune-delete-branches '("a" "b" "c") "/tmp/r" t)))
        (should (equal '("a" "b" "c") deleted))
        (should (= 3 (plist-get result :deleted)))
        (should (= 1 calls))))))

(ert-deftest magit-gh-extras/delete-rescans-after ()
  "A re-scan is performed for PATH after deletion."
  (let ((scanned 0)
        (scanned-in-path nil)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete) (lambda (&rest _) nil))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () (setq scanned (1+ scanned)) (setq scanned-in-path (magit-gh--repo-dir)) nil))
              ((symbol-function 'magit-gh--repo-dir) (lambda () "/tmp/r")))
      (magit-gh--prune-delete-branches '("a") "/tmp/r" nil)
      (should (= 1 scanned))
      (should (equal "/tmp/r" scanned-in-path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-toggle-mark

(ert-deftest magit-gh-extras/toggle-mark-adds ()
  "Selecting an unmarked branch adds it to :marked."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (&rest _) "feat")))
      (magit-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F"))))
                                :marked nil))
      (magit-gh--prune-toggle-mark "/tmp/r")
      (should (equal '("feat") (plist-get (magit-gh--cache-get "/tmp/r" :prune-state) :marked))))))

(ert-deftest magit-gh-extras/toggle-mark-removes ()
  "Selecting a marked branch removes it from :marked."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (&rest _) "feat [marked]")))
      (magit-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F"))))
                                :marked '("feat")))
      (magit-gh--prune-toggle-mark "/tmp/r")
      (should-not (plist-get (magit-gh--cache-get "/tmp/r" :prune-state) :marked)))))

(ert-deftest magit-gh-extras/toggle-mark-preserves-other-marks ()
  "Toggling one branch leaves other marked branches in place."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (&rest _) "b")))
      (magit-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                              ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                                :marked '("a")))
      (magit-gh--prune-toggle-mark "/tmp/r")
      (let ((marked (plist-get (magit-gh--cache-get "/tmp/r" :prune-state) :marked)))
        (should (member "a" marked))
        (should (member "b" marked))))))

(ert-deftest magit-gh-extras/toggle-mark-errors-when-empty ()
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/r" :prune-state (list :candidates nil :marked nil))
    (should-error (magit-gh--prune-toggle-mark "/tmp/r")
                  :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-dispatch

(ert-deftest magit-gh-extras/dispatch-exit-throws ()
  "exit menu throws magit-gh--prune-exit so the loop terminates."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/r" :prune-state (list :candidates nil :marked nil))
    (let ((after-throw 'untouched))
      (catch 'magit-gh--prune-exit
        (magit-gh--prune-dispatch "exit menu" "/tmp/r")
        (setq after-throw 'reached))
      (should (eq 'untouched after-throw)))))

(ert-deftest magit-gh-extras/dispatch-refresh-calls-scan ()
  (let ((scanned 0)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-gh--prune-scan)
               (lambda () (setq scanned (1+ scanned)) nil)))
      (magit-gh--cache-set "/tmp/r" :prune-state (list :candidates nil :marked nil))
      (magit-gh--prune-dispatch "refresh" "/tmp/r")
      (should (= 1 scanned)))))

(ert-deftest magit-gh-extras/dispatch-prune-selected ()
  "Selecting a specific branch line deletes that branch only."
  (let ((deleted nil)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (magit-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F")))
                                              ("other" . ((number . 2) (state . "CLOSED") (title . "O"))))
                                :marked nil))
      (magit-gh--prune-dispatch "prune: feat" "/tmp/r")
      (should (equal '("feat") deleted)))))

(ert-deftest magit-gh-extras/dispatch-prune-selected-marked ()
  "A marked label parses correctly and dispatches deletion of that branch."
  (let ((deleted nil)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (magit-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F"))))
                                :marked '("feat")))
      (magit-gh--prune-dispatch "prune: feat [marked]" "/tmp/r")
      (should (equal '("feat") deleted)))))

(ert-deftest magit-gh-extras/dispatch-prune-all-no-prompt ()
  (let ((deleted nil)
        (read-calls 0)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (cl-incf read-calls) ?y))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (magit-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                              ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                                :marked nil))
      (magit-gh--prune-dispatch "prune all branches (no prompt)" "/tmp/r")
      (should (equal '("a" "b") deleted))
      (should (= 0 read-calls)))))

(ert-deftest magit-gh-extras/dispatch-prune-all-with-prompt ()
  (let ((deleted nil)
        (read-calls 0)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (cl-incf read-calls) ?y))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (magit-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                              ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                                :marked nil))
      (magit-gh--prune-dispatch "prune all branches (with prompt)" "/tmp/r")
      (should (equal '("a" "b") deleted))
      (should (= 2 read-calls)))))

(ert-deftest magit-gh-extras/dispatch-prune-marked ()
  "prune marked branches deletes only the marked subset."
  (let ((deleted nil)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (magit-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                              ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                                :marked '("b")))
      (magit-gh--prune-dispatch "prune marked branches" "/tmp/r")
      (should (equal '("b") deleted)))))

(ert-deftest magit-gh-extras/dispatch-mark-delegates ()
  "mark branch for pruning delegates to toggle-mark."
  (let ((called nil)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-gh--prune-toggle-mark)
               (lambda (path) (setq called path))))
      (magit-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A"))))
                                :marked nil))
      (magit-gh--prune-dispatch "mark branch for pruning" "/tmp/r")
      (should (equal "/tmp/r" called)))))

(ert-deftest magit-gh-extras/dispatch-unknown-errors ()
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/r" :prune-state (list :candidates nil :marked nil))
    (should-error (magit-gh--prune-dispatch "bogus" "/tmp/r")
                  :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--cache-get, magit-gh--cache-set, magit-gh--cache-remove

(ert-deftest magit-gh-extras/cache-set-and-get ()
  "set stores a value; get retrieves it."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/r" :foo "bar")
    (should (equal "bar" (magit-gh--cache-get "/tmp/r" :foo)))))

(ert-deftest magit-gh-extras/cache-get-missing-key ()
  "get returns nil for a key that was never set."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/r" :x 1)
    (should (null (magit-gh--cache-get "/tmp/r" :y)))))

(ert-deftest magit-gh-extras/cache-remove-key ()
  "remove with a key deletes only that key."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/r" :a 1)
    (magit-gh--cache-set "/tmp/r" :b 2)
    (magit-gh--cache-remove "/tmp/r" :a)
    (should (null (magit-gh--cache-get "/tmp/r" :a)))
    (should (= 2 (magit-gh--cache-get "/tmp/r" :b)))))

(ert-deftest magit-gh-extras/cache-remove-all ()
  "remove without a key deletes all data for the repo."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/r" :x 99)
    (magit-gh--cache-remove "/tmp/r")
    (should (null (magit-gh--cache-get "/tmp/r" :x)))))

(ert-deftest magit-gh-extras/cache-isolated-by-path ()
  "Different repo paths have independent cache entries."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/a" :k "a-value")
    (magit-gh--cache-set "/tmp/b" :k "b-value")
    (should (equal "a-value" (magit-gh--cache-get "/tmp/a" :k)))
    (should (equal "b-value" (magit-gh--cache-get "/tmp/b" :k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--add-file

(ert-deftest magit-gh-extras/add-file-empty ()
  "Adding to empty :files list produces a single-entry list."
  (let* ((ctx '(:files nil))
         (ctx2 (magit-gh--add-file ctx "pr-info.json" "metadata")))
    (should (equal '((:path "pr-info.json" :type "metadata"))
                   (plist-get ctx2 :files)))))

(ert-deftest magit-gh-extras/add-file-accumulates ()
  "Each add-file appends; order is preserved."
  (let* ((ctx '(:files nil))
         (ctx2 (magit-gh--add-file ctx "a.json" "x"))
         (ctx3 (magit-gh--add-file ctx2 "b.json" "y")))
    (should (= 2 (length (plist-get ctx3 :files))))
    (should (equal "a.json" (plist-get (nth 0 (plist-get ctx3 :files)) :path)))
    (should (equal "b.json" (plist-get (nth 1 (plist-get ctx3 :files)) :path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--branch-slug

(ert-deftest magit-gh-extras/branch-slug-simple ()
  (should (equal "main" (magit-gh--branch-slug "main"))))

(ert-deftest magit-gh-extras/branch-slug-slash ()
  (should (equal "fix-the-thing" (magit-gh--branch-slug "fix/the-thing"))))

(ert-deftest magit-gh-extras/branch-slug-uppercase ()
  (should (equal "feature-foo-123" (magit-gh--branch-slug "Feature/Foo-123"))))

(ert-deftest magit-gh-extras/branch-slug-multiple-separators ()
  "Consecutive non-alphanumeric chars collapse to a single hyphen."
  (should (equal "a-b" (magit-gh--branch-slug "a_/_b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--make-error-handler

(ert-deftest magit-gh-extras/make-error-handler-formats-message ()
  "The returned lambda emits a correctly formatted message."
  (let (msg)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (let ((handler (magit-gh--make-error-handler "magit-gh-test" "my-step")))
        (funcall handler "oops\n" 1)))
    (should (string-match-p "magit-gh-test" msg))
    (should (string-match-p "my-step" msg))
    (should (string-match-p "exit 1" msg))
    (should (string-match-p "oops" msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--index-table, magit-gh--file-table, magit-gh--write-index

(ert-deftest magit-gh-extras/index-table-basic ()
  "index-table builds a hash-table from alternating key-value pairs."
  (let ((ht (magit-gh--index-table "type" "ci" "count" 3)))
    (should (equal "ci" (gethash "type" ht)))
    (should (= 3 (gethash "count" ht)))))

(ert-deftest magit-gh-extras/file-table-basic ()
  "file-table builds a two-key hash-table."
  (let ((ht (magit-gh--file-table "run-info.json" "metadata")))
    (should (equal "run-info.json" (gethash "path" ht)))
    (should (equal "metadata" (gethash "type" ht)))))

(ert-deftest magit-gh-extras/write-index-json ()
  "write-index writes valid JSON to index.json inside DIR."
  (let ((dir (make-temp-file "magit-gh-extras-test" t)))
    (unwind-protect
        (let ((data (magit-gh--index-table "type" "test" "n" 7)))
          (magit-gh--write-index dir data)
          (let* ((file (expand-file-name "index.json" dir))
                 (raw (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string)))
                 (parsed (json-parse-string raw :object-type 'alist)))
            (should (file-exists-p file))
            (should (equal "test" (map-elt parsed 'type)))
            (should (= 7 (map-elt parsed 'n)))))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--collect-default-name

(ert-deftest magit-gh-extras/collect-default-name-basic ()
  (should (equal "ci-main-12345"
                 (magit-gh--collect-default-name 'ci "main" 12345))))

(ert-deftest magit-gh-extras/collect-default-name-slugifies ()
  (should (equal "ci-feature-my-thing-99"
                 (magit-gh--collect-default-name 'ci "feature/my-thing" 99))))

;;; test-magit-gh-extras.el ends here
