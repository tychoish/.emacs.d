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

(ert-deftest magit-gh-extras/scan-collects-closed-prs ()
  "scan keeps only branches whose PR is merged or closed."
  (cl-letf (((symbol-function 'magit-gh--repo-dir) (lambda () "/tmp/r"))
            ((symbol-function 'magit-gh--tracking-branches) (lambda () '("a" "b" "c" "d")))
            ((symbol-function 'magit-gh--pr-for-branch)
             (lambda (b) (pcase b
                           ("a" '((number . 1) (state . "MERGED")))
                           ("b" '((number . 2) (state . "OPEN")))
                           ("c" '((number . 3) (state . "CLOSED")))
                           ("d" nil)))))
    (with-temp-buffer
      (setq-local magit-gh--prune-state nil)
      (let ((result (magit-gh--prune-scan)))
        (should (equal '("a" "c") (mapcar #'car result)))
        (should (equal '("a" "c")
                       (mapcar #'car (plist-get magit-gh--prune-state :candidates))))))))

(ert-deftest magit-gh-extras/scan-drops-stale-marked ()
  "Marked branches no longer in candidate set are dropped."
  (cl-letf (((symbol-function 'magit-gh--repo-dir) (lambda () "/tmp/r"))
            ((symbol-function 'magit-gh--tracking-branches) (lambda () '("a" "b" "c")))
            ((symbol-function 'magit-gh--pr-for-branch)
             (lambda (b) (pcase b
                           ("a" '((number . 1) (state . "MERGED")))
                           ("b" '((number . 2) (state . "OPEN")))
                           ("c" '((number . 3) (state . "CLOSED")))))))
    (with-temp-buffer
      ;; 'gone' is stale (not a branch); 'b' is open (not a candidate)
      (setq-local magit-gh--prune-state
                  (list :candidates nil :marked '("a" "gone" "b")))
      (magit-gh--prune-scan)
      (should (equal '("a") (plist-get magit-gh--prune-state :marked))))))

(ert-deftest magit-gh-extras/scan-empty ()
  "scan with only open PRs yields empty candidates."
  (cl-letf (((symbol-function 'magit-gh--repo-dir) (lambda () "/tmp/r"))
            ((symbol-function 'magit-gh--tracking-branches) (lambda () '("a")))
            ((symbol-function 'magit-gh--pr-for-branch)
             (lambda (_) '((number . 1) (state . "OPEN")))))
    (with-temp-buffer
      (setq-local magit-gh--prune-state nil)
      (let ((result (magit-gh--prune-scan)))
        (should-not result)
        (should-not (plist-get magit-gh--prune-state :candidates))
        (should-not (plist-get magit-gh--prune-state :marked))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-state-buffer

(ert-deftest magit-gh-extras/state-buffer-creates-hidden ()
  "state-buffer returns a hidden buffer named for the repo dir."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () "/tmp/test-repo")))
    (let ((buf (magit-gh--prune-state-buffer)))
      (unwind-protect
          (progn
            (should (bufferp buf))
            (should (string-prefix-p " *magit-gh-prune:" (buffer-name buf)))
            (should (equal "/tmp/test-repo"
                           (with-current-buffer buf default-directory))))
        (kill-buffer buf)))))

(ert-deftest magit-gh-extras/state-buffer-returns-existing ()
  "Repeated calls in the same repo return the same buffer."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () "/tmp/test-repo")))
    (let* ((buf1 (magit-gh--prune-state-buffer))
           (buf2 (magit-gh--prune-state-buffer)))
      (unwind-protect
          (should (eq buf1 buf2))
        (kill-buffer buf1)))))

(ert-deftest magit-gh-extras/state-buffer-not-in-repo ()
  "Errors when magit-toplevel returns nil."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () nil)))
    (should-error (magit-gh--prune-state-buffer) :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-delete-branches

(ert-deftest magit-gh-extras/delete-no-prompt ()
  "With PROMPT-P nil, all branches are deleted without read-char-choice."
  (let ((deleted nil))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (error "should not be called")))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state nil)
        (let ((result (magit-gh--prune-delete-branches '("a" "b") (current-buffer) nil)))
          (should (equal '("a" "b") deleted))
          (should (= 2 (plist-get result :deleted)))
          (should (= 0 (plist-get result :skipped)))
          (should-not (plist-get result :quit)))))))

(ert-deftest magit-gh-extras/delete-with-prompt-yes ()
  (let ((deleted nil))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) ?y))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state nil)
        (let ((result (magit-gh--prune-delete-branches '("a" "b") (current-buffer) t)))
          (should (equal '("a" "b") deleted))
          (should (= 2 (plist-get result :deleted))))))))

(ert-deftest magit-gh-extras/delete-with-prompt-no-skips ()
  "All ?n answers leave the branches untouched."
  (let ((deleted nil))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) ?n))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state nil)
        (let ((result (magit-gh--prune-delete-branches '("a" "b") (current-buffer) t)))
          (should-not deleted)
          (should (= 0 (plist-get result :deleted)))
          (should (= 2 (plist-get result :skipped))))))))

(ert-deftest magit-gh-extras/delete-with-prompt-quit ()
  "Quit terminates the loop after the current branch."
  (let ((deleted nil)
        (calls 0))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (cl-incf calls) (if (= calls 1) ?y ?q)))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state nil)
        (let ((result (magit-gh--prune-delete-branches '("a" "b" "c") (current-buffer) t)))
          (should (equal '("a") deleted))
          (should (plist-get result :quit))
          (should (= 1 (plist-get result :deleted))))))))

(ert-deftest magit-gh-extras/delete-yes-to-all ()
  "Bang answer enables yes-to-all for the remainder."
  (let ((deleted nil)
        (calls 0))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (cl-incf calls) ?!))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state nil)
        (let ((result (magit-gh--prune-delete-branches '("a" "b" "c") (current-buffer) t)))
          (should (equal '("a" "b" "c") deleted))
          (should (= 3 (plist-get result :deleted)))
          (should (= 1 calls)))))))

(ert-deftest magit-gh-extras/delete-rescans-after ()
  "A re-scan is performed in BUF after deletion."
  (let ((scanned 0)
        (scanned-in nil))
    (cl-letf (((symbol-function 'magit-branch-delete) (lambda (&rest _) nil))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () (cl-incf scanned) (setq scanned-in (current-buffer)) nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state nil)
        (let ((target (current-buffer)))
          (with-temp-buffer
            (magit-gh--prune-delete-branches '("a") target nil)
            (should (= 1 scanned))
            (should (eq target scanned-in))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-toggle-mark

(ert-deftest magit-gh-extras/toggle-mark-adds ()
  "Selecting an unmarked branch adds it to :marked."
  (cl-letf (((symbol-function 'annotated-completing-read)
             (lambda (&rest _) "feat")))
    (with-temp-buffer
      (setq-local magit-gh--prune-state
                  (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F"))))
                        :marked nil))
      (magit-gh--prune-toggle-mark (current-buffer))
      (should (equal '("feat") (plist-get magit-gh--prune-state :marked))))))

(ert-deftest magit-gh-extras/toggle-mark-removes ()
  "Selecting a marked branch removes it from :marked."
  (cl-letf (((symbol-function 'annotated-completing-read)
             (lambda (&rest _) "feat [marked]")))
    (with-temp-buffer
      (setq-local magit-gh--prune-state
                  (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F"))))
                        :marked '("feat")))
      (magit-gh--prune-toggle-mark (current-buffer))
      (should-not (plist-get magit-gh--prune-state :marked)))))

(ert-deftest magit-gh-extras/toggle-mark-preserves-other-marks ()
  "Toggling one branch leaves other marked branches in place."
  (cl-letf (((symbol-function 'annotated-completing-read)
             (lambda (&rest _) "b")))
    (with-temp-buffer
      (setq-local magit-gh--prune-state
                  (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                      ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                        :marked '("a")))
      (magit-gh--prune-toggle-mark (current-buffer))
      (let ((marked (plist-get magit-gh--prune-state :marked)))
        (should (member "a" marked))
        (should (member "b" marked))))))

(ert-deftest magit-gh-extras/toggle-mark-errors-when-empty ()
  (with-temp-buffer
    (setq-local magit-gh--prune-state (list :candidates nil :marked nil))
    (should-error (magit-gh--prune-toggle-mark (current-buffer))
                  :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-gh--prune-dispatch

(ert-deftest magit-gh-extras/dispatch-exit-throws ()
  "exit menu throws magit-gh--prune-exit so the loop terminates."
  (with-temp-buffer
    (setq-local magit-gh--prune-state (list :candidates nil :marked nil))
    (let ((after-throw 'untouched))
      (catch 'magit-gh--prune-exit
        (magit-gh--prune-dispatch "exit menu" (current-buffer))
        (setq after-throw 'reached))
      (should (eq 'untouched after-throw)))))

(ert-deftest magit-gh-extras/dispatch-refresh-calls-scan ()
  (let ((scanned 0))
    (cl-letf (((symbol-function 'magit-gh--prune-scan)
               (lambda () (cl-incf scanned) nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state (list :candidates nil :marked nil))
        (magit-gh--prune-dispatch "refresh" (current-buffer))
        (should (= 1 scanned))))))

(ert-deftest magit-gh-extras/dispatch-prune-selected ()
  "Selecting a specific branch line deletes that branch only."
  (let ((deleted nil))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state
                    (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F")))
                                        ("other" . ((number . 2) (state . "CLOSED") (title . "O"))))
                          :marked nil))
        (magit-gh--prune-dispatch "prune: feat" (current-buffer))
        (should (equal '("feat") deleted))))))

(ert-deftest magit-gh-extras/dispatch-prune-selected-marked ()
  "A marked label parses correctly and dispatches deletion of that branch."
  (let ((deleted nil))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state
                    (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F"))))
                          :marked '("feat")))
        (magit-gh--prune-dispatch "prune: feat [marked]" (current-buffer))
        (should (equal '("feat") deleted))))))

(ert-deftest magit-gh-extras/dispatch-prune-all-no-prompt ()
  (let ((deleted nil)
        (read-calls 0))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (cl-incf read-calls) ?y))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state
                    (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                        ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                          :marked nil))
        (magit-gh--prune-dispatch "prune all branches (no prompt)" (current-buffer))
        (should (equal '("a" "b") deleted))
        (should (= 0 read-calls))))))

(ert-deftest magit-gh-extras/dispatch-prune-all-with-prompt ()
  (let ((deleted nil)
        (read-calls 0))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (cl-incf read-calls) ?y))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state
                    (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                        ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                          :marked nil))
        (magit-gh--prune-dispatch "prune all branches (with prompt)" (current-buffer))
        (should (equal '("a" "b") deleted))
        (should (= 2 read-calls))))))

(ert-deftest magit-gh-extras/dispatch-prune-marked ()
  "prune marked branches deletes only the marked subset."
  (let ((deleted nil))
    (cl-letf (((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'magit-gh--prune-scan)
               (lambda () nil)))
      (with-temp-buffer
        (setq-local magit-gh--prune-state
                    (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                        ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                          :marked '("b")))
        (magit-gh--prune-dispatch "prune marked branches" (current-buffer))
        (should (equal '("b") deleted))))))

(ert-deftest magit-gh-extras/dispatch-mark-delegates ()
  "mark branch for pruning delegates to toggle-mark."
  (let ((called nil))
    (cl-letf (((symbol-function 'magit-gh--prune-toggle-mark)
               (lambda (buf) (setq called buf))))
      (with-temp-buffer
        (setq-local magit-gh--prune-state
                    (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A"))))
                          :marked nil))
        (let ((buf (current-buffer)))
          (magit-gh--prune-dispatch "mark branch for pruning" buf)
          (should (eq buf called)))))))

(ert-deftest magit-gh-extras/dispatch-unknown-errors ()
  (with-temp-buffer
    (setq-local magit-gh--prune-state (list :candidates nil :marked nil))
    (should-error (magit-gh--prune-dispatch "bogus" (current-buffer))
                  :type 'user-error)))

;;; test-magit-gh-extras.el ends here
