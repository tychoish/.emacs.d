;;; test-magit-gh-repo-dashboard.el --- ERT tests for magit-gh-repo-dashboard -*- lexical-binding: t -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-gh-repo-dashboard/")
;;
;; Batch run:
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-magit-gh-repo-dashboard.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-gh-repo-dashboard/")'

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'magit-gh-repo-dashboard)

;;;; magit-gh-repo-register

(ert-deftest magit-gh-repo-dashboard/register-adds-to-list ()
  "register creates a struct and prepends it to `magit-gh-repo-list'."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "foo" "/tmp/foo")
    (should (= 1 (length magit-gh-repo-list)))
    (let ((r (car magit-gh-repo-list)))
      (should (magit-gh-repo-p r))
      (should (equal "foo" (magit-gh-repo-name r)))
      (should (equal "/tmp/foo" (magit-gh-repo-path r))))))

(ert-deftest magit-gh-repo-dashboard/register-expands-path ()
  "register expands the path with `expand-file-name'."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "bar" "~/bar")
    (should (string-prefix-p "/" (magit-gh-repo-path (car magit-gh-repo-list))))))

(ert-deftest magit-gh-repo-dashboard/register-replaces-existing ()
  "Registering the same name replaces the previous entry."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "dup" "/tmp/dup1")
    (magit-gh-repo-register "dup" "/tmp/dup2")
    (should (= 1 (length magit-gh-repo-list)))
    (should (equal "/tmp/dup2" (magit-gh-repo-path (car magit-gh-repo-list))))))

(ert-deftest magit-gh-repo-dashboard/register-multiple ()
  "Multiple distinct names accumulate in the list."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "a" "/tmp/a")
    (magit-gh-repo-register "b" "/tmp/b")
    (should (= 2 (length magit-gh-repo-list)))))

(ert-deftest magit-gh-repo-dashboard/register-defaults ()
  "Registering with only name+path yields correct defaults for new fields."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "r" "/tmp/r")
    (let ((r (car magit-gh-repo-list)))
      (should (null (magit-gh-repo-include-prs r)))
      (should (null (magit-gh-repo-auto-sync r)))
      (should (null (magit-gh-repo-tags r)))
      (should (null (magit-gh-repo-auto-commit r)))
      (should (null (magit-gh-repo-commands r)))
      (should (null (magit-gh-repo-sort-hint r))))))

(ert-deftest magit-gh-repo-dashboard/register-include-prs-true ()
  ":include-prs t is stored correctly."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "r" "/tmp/r" :include-prs t)
    (should (eq t (magit-gh-repo-include-prs (car magit-gh-repo-list))))))

(ert-deftest magit-gh-repo-dashboard/register-auto-sync-fetch ()
  ":auto-sync 'fetch is stored correctly."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "r" "/tmp/r" :auto-sync 'fetch)
    (should (eq 'fetch (magit-gh-repo-auto-sync (car magit-gh-repo-list))))))

(ert-deftest magit-gh-repo-dashboard/register-auto-sync-pull ()
  ":auto-sync 'pull is stored correctly."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "r" "/tmp/r" :auto-sync 'pull)
    (should (eq 'pull (magit-gh-repo-auto-sync (car magit-gh-repo-list))))))

(ert-deftest magit-gh-repo-dashboard/register-tags ()
  ":tags list of symbols is stored correctly."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "r" "/tmp/r" :tags '(work personal))
    (should (equal '(work personal) (magit-gh-repo-tags (car magit-gh-repo-list))))))

(ert-deftest magit-gh-repo-dashboard/register-auto-commit-bool ()
  ":auto-commit t is stored correctly."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "r" "/tmp/r" :auto-commit t)
    (should (eq t (magit-gh-repo-auto-commit (car magit-gh-repo-list))))))

(ert-deftest magit-gh-repo-dashboard/register-auto-commit-function ()
  ":auto-commit function is stored correctly."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "r" "/tmp/r" :auto-commit #'ignore)
    (should (eq #'ignore (magit-gh-repo-auto-commit (car magit-gh-repo-list))))))

(ert-deftest magit-gh-repo-dashboard/register-commands ()
  ":commands alist is stored correctly."
  (let ((magit-gh-repo-list nil)
        (cmds '(("run tests" . my-test-fn) ("lint" . my-lint-fn))))
    (magit-gh-repo-register "r" "/tmp/r" :commands cmds)
    (should (equal cmds (magit-gh-repo-commands (car magit-gh-repo-list))))))

(ert-deftest magit-gh-repo-dashboard/register-sort-hint ()
  ":sort-hint number is stored correctly."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "r" "/tmp/r" :sort-hint 10)
    (should (= 10 (magit-gh-repo-sort-hint (car magit-gh-repo-list))))))

;;;; magit-gh-repo-dashboard--sorted-repos

(ert-deftest magit-gh-repo-dashboard/sorted-repos-by-hint ()
  "Repos with sort-hints are ordered numerically."
  (let* ((r1 (magit-gh-repo--make :name "r1" :path "/tmp/r1" :sort-hint 20))
         (r2 (magit-gh-repo--make :name "r2" :path "/tmp/r2" :sort-hint 5))
         (r3 (magit-gh-repo--make :name "r3" :path "/tmp/r3" :sort-hint 10))
         (sorted (magit-gh-repo-dashboard--sorted-repos (list r1 r2 r3))))
    (should (equal "r2" (magit-gh-repo-name (nth 0 sorted))))
    (should (equal "r3" (magit-gh-repo-name (nth 1 sorted))))
    (should (equal "r1" (magit-gh-repo-name (nth 2 sorted))))))

(ert-deftest magit-gh-repo-dashboard/sorted-repos-hinted-before-unhinted ()
  "Repos with sort-hints appear before those without."
  (let* ((r1 (magit-gh-repo--make :name "r1" :path "/tmp/r1"))
         (r2 (magit-gh-repo--make :name "r2" :path "/tmp/r2" :sort-hint 1))
         (sorted (magit-gh-repo-dashboard--sorted-repos (list r1 r2))))
    (should (equal "r2" (magit-gh-repo-name (nth 0 sorted))))
    (should (equal "r1" (magit-gh-repo-name (nth 1 sorted))))))

(ert-deftest magit-gh-repo-dashboard/sorted-repos-unhinted-preserve-order ()
  "Repos without sort-hints preserve their relative order."
  (let* ((r1 (magit-gh-repo--make :name "r1" :path "/tmp/r1"))
         (r2 (magit-gh-repo--make :name "r2" :path "/tmp/r2"))
         (sorted (magit-gh-repo-dashboard--sorted-repos (list r1 r2))))
    (should (equal "r1" (magit-gh-repo-name (nth 0 sorted))))
    (should (equal "r2" (magit-gh-repo-name (nth 1 sorted))))))

;;;; magit-gh-repo-dashboard--format-age

(ert-deftest magit-gh-repo-dashboard/format-age-nil ()
  (should (equal "never" (magit-gh-repo-dashboard--format-age nil))))

(ert-deftest magit-gh-repo-dashboard/format-age-seconds ()
  (should (equal "30s" (magit-gh-repo-dashboard--format-age 30.0))))

(ert-deftest magit-gh-repo-dashboard/format-age-minutes ()
  (should (equal "5m" (magit-gh-repo-dashboard--format-age 300.0))))

(ert-deftest magit-gh-repo-dashboard/format-age-hours ()
  (should (equal "2h" (magit-gh-repo-dashboard--format-age 7200.0))))

(ert-deftest magit-gh-repo-dashboard/format-age-days ()
  (should (equal "3d" (magit-gh-repo-dashboard--format-age (* 3 86400.0)))))

(ert-deftest magit-gh-repo-dashboard/format-age-boundary-59s ()
  "59 seconds formats as seconds."
  (should (equal "59s" (magit-gh-repo-dashboard--format-age 59.0))))

(ert-deftest magit-gh-repo-dashboard/format-age-boundary-60s ()
  "60 seconds formats as 1 minute."
  (should (equal "1m" (magit-gh-repo-dashboard--format-age 60.0))))

;;;; magit-gh-repo-dashboard--format-behind

(ert-deftest magit-gh-repo-dashboard/format-behind-zero ()
  "Behind=0 returns empty string."
  (should (equal "" (magit-gh-repo-dashboard--format-behind 0))))

(ert-deftest magit-gh-repo-dashboard/format-behind-nonzero ()
  "Behind > 0 returns a propertized number string."
  (let ((result (magit-gh-repo-dashboard--format-behind 3)))
    (should (equal "3" (substring-no-properties result)))))

;;;; magit-gh-repo-dashboard--format-dirty

(ert-deftest magit-gh-repo-dashboard/format-dirty-clean ()
  (should (equal "" (magit-gh-repo-dashboard--format-dirty nil))))

(ert-deftest magit-gh-repo-dashboard/format-dirty-dirty ()
  "Dirty returns a non-empty propertized string."
  (let ((result (magit-gh-repo-dashboard--format-dirty t)))
    (should (not (string-empty-p result)))))

;;;; magit-gh-repo-dashboard--head-hash

(ert-deftest magit-gh-repo-dashboard/head-hash-nonexistent-path ()
  "head-hash returns nil for a path with no .git/HEAD."
  (should (null (magit-gh-repo-dashboard--head-hash "/tmp/nonexistent-no-git-here"))))

;;;; magit-gh-repo-dashboard--collect-stats (via mock)

(ert-deftest magit-gh-repo-dashboard/collect-stats-extracts-fields ()
  "collect-stats populates :branch :behind :dirty :head-hash :recent-log from shell output."
  (let ((repo (magit-gh-repo--make :name "test" :path "/tmp/test")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (cmd)
                 (cond
                  ((string-match-p "branch --show-current" cmd) "main\n")
                  ((string-match-p "rev-list" cmd) "2\n")
                  ((string-match-p "status --porcelain" cmd) " M foo.el\n")
                  ((string-match-p "log --oneline" cmd) "abc123 fix foo\ndef456 add bar\n")
                  (t ""))))
              ((symbol-function 'magit-gh-repo-dashboard--fetch-age)
               (lambda (_) 3600.0))
              ((symbol-function 'magit-gh-repo-dashboard--head-hash)
               (lambda (_) "abc123def456")))
      (let ((magit-gh-repo-dashboard--stats-cache (make-hash-table :test #'equal)))
        (let ((stats (magit-gh-repo-dashboard--collect-stats repo)))
          (should (equal "main" (plist-get stats :branch)))
          (should (= 2 (plist-get stats :behind)))
          (should (eq t (plist-get stats :dirty)))
          (should (= 3600.0 (plist-get stats :fetch-age)))
          (should (equal "abc123def456" (plist-get stats :head-hash)))
          (should (string-match-p "fix foo" (plist-get stats :recent-log))))))))

(ert-deftest magit-gh-repo-dashboard/collect-stats-clean-workdir ()
  "collect-stats sets :dirty nil when git status --porcelain is empty."
  (let ((repo (magit-gh-repo--make :name "test" :path "/tmp/test")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (cmd)
                 (cond
                  ((string-match-p "branch --show-current" cmd) "feat\n")
                  ((string-match-p "rev-list" cmd) "0\n")
                  ((string-match-p "status --porcelain" cmd) "")
                  ((string-match-p "log --oneline" cmd) "abc commit\n")
                  (t ""))))
              ((symbol-function 'magit-gh-repo-dashboard--fetch-age)
               (lambda (_) nil))
              ((symbol-function 'magit-gh-repo-dashboard--head-hash)
               (lambda (_) "deadbeef")))
      (let ((magit-gh-repo-dashboard--stats-cache (make-hash-table :test #'equal)))
        (let ((stats (magit-gh-repo-dashboard--collect-stats repo)))
          (should (eq nil (plist-get stats :dirty)))
          (should (= 0 (plist-get stats :behind))))))))

;;;; magit-gh-repo-dashboard--get-stats (cache invalidation)

(ert-deftest magit-gh-repo-dashboard/get-stats-uses-cache-on-same-hash ()
  "get-stats returns cached stats when HEAD hash matches."
  (let* ((repo (magit-gh-repo--make :name "test" :path "/tmp/test"))
         (cached (list :branch "main" :behind 0 :dirty nil
                       :fetch-age 60.0 :head-hash "abc123" :recent-log ""))
         (magit-gh-repo-dashboard--stats-cache (make-hash-table :test #'equal)))
    (puthash "/tmp/test" cached magit-gh-repo-dashboard--stats-cache)
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--head-hash)
               (lambda (_) "abc123")))
      (should (equal cached (magit-gh-repo-dashboard--get-stats repo))))))

(ert-deftest magit-gh-repo-dashboard/get-stats-invalidates-on-new-hash ()
  "get-stats collects fresh stats when HEAD hash has changed."
  (let* ((repo (magit-gh-repo--make :name "test" :path "/tmp/test"))
         (cached (list :branch "main" :behind 0 :dirty nil
                       :fetch-age 60.0 :head-hash "old123" :recent-log ""))
         (magit-gh-repo-dashboard--stats-cache (make-hash-table :test #'equal)))
    (puthash "/tmp/test" cached magit-gh-repo-dashboard--stats-cache)
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--head-hash)
               (lambda (_) "new456"))
              ((symbol-function 'magit-gh-repo-dashboard--collect-stats)
               (lambda (_) (list :branch "feat" :behind 1 :dirty t
                                 :fetch-age nil :head-hash "new456" :recent-log ""))))
      (let ((result (magit-gh-repo-dashboard--get-stats repo)))
        (should (equal "feat" (plist-get result :branch)))
        (should (= 1 (plist-get result :behind)))))))

;;;; magit-gh-repo-dashboard--auto-commit

(ert-deftest magit-gh-repo-dashboard/auto-commit-uses-default-message ()
  "With :auto-commit t, calls git add and git commit with the default message."
  (let ((repo (magit-gh-repo--make :name "test" :path "/tmp/test" :auto-commit t))
        (git-calls nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_prog _in _out _err &rest args)
                 (push args git-calls)
                 0)))
      (should (magit-gh-repo-dashboard--auto-commit repo)))
    ;; git-calls is (commit-args add-args) — commit was pushed last
    (should (= 2 (length git-calls)))
    (should (member "add" (cadr git-calls)))
    (let ((commit-args (car git-calls)))
      (should (equal "commit" (car commit-args)))
      (should (member (magit-gh-repo-dashboard--default-commit-message repo)
                      commit-args)))))

(ert-deftest magit-gh-repo-dashboard/auto-commit-calls-message-function ()
  "With :auto-commit as a function, calls it to produce the commit message."
  (let* ((custom-msg "custom: my message")
         (repo (magit-gh-repo--make :name "test" :path "/tmp/test"
                                    :auto-commit (lambda (_r) custom-msg)))
         (commit-msg nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_prog _in _out _err &rest args)
                 (when (equal "commit" (car args))
                   (setq commit-msg (cadr (member "-m" args))))
                 0)))
      (magit-gh-repo-dashboard--auto-commit repo)
      (should (equal custom-msg commit-msg)))))

(ert-deftest magit-gh-repo-dashboard/commit-user-error-when-not-configured ()
  "commit signals user-error when :auto-commit is nil."
  (let ((repo (magit-gh-repo--make :name "test" :path "/tmp/test")))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--repo-at-point)
               (lambda () repo)))
      (should-error (magit-gh-repo-dashboard-commit) :type 'user-error))))

;;;; magit-gh-repo-dashboard-sync-all

(ert-deftest magit-gh-repo-dashboard/sync-all-user-error-when-none-configured ()
  "sync-all signals user-error when no repos have :auto-sync."
  (let ((magit-gh-repo-list (list (magit-gh-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-gh-repo-dashboard-sync-all) :type 'user-error)))

(ert-deftest magit-gh-repo-dashboard/sync-all-skips-repos-without-auto-sync ()
  "sync-all calls interactively only for repos with :auto-sync set."
  (let* ((r1 (magit-gh-repo--make :name "r1" :path "/tmp/r1" :auto-sync 'fetch))
         (r2 (magit-gh-repo--make :name "r2" :path "/tmp/r2"))
         (magit-gh-repo-list (list r1 r2))
         (called-fns nil))
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn) (push fn called-fns)))
              ((symbol-function 'magit-gh-repo-dashboard-refresh)
               (lambda () nil)))
      (magit-gh-repo-dashboard-sync-all))
    (should (= 1 (length called-fns)))
    (should (memq #'magit-fetch called-fns))))

;;;; magit-gh-repo-dashboard--run-command-for

(ert-deftest magit-gh-repo-dashboard/run-command-user-error-when-no-commands ()
  "run-command-for signals user-error when repo has no commands."
  (let ((repo (magit-gh-repo--make :name "test" :path "/tmp/test")))
    (should-error (magit-gh-repo-dashboard--run-command-for repo) :type 'user-error)))

;;;; magit-gh-repo-dashboard-filter-by-tag

(defmacro magit-gh-repo-dashboard-test--with-refresh-stubs (&rest body)
  "Run BODY with tabulated-list side-effecting functions stubbed out."
  `(cl-letf (((symbol-function 'tabulated-list-print) (lambda (&rest _) nil))
             ((symbol-function 'tabulated-list-init-header) (lambda () nil)))
     ,@body))

(ert-deftest magit-gh-repo-dashboard/filter-by-tag-reduces-entries ()
  "Refresh with a tag filter shows only matching repos."
  (let* ((r1 (magit-gh-repo--make :name "r1" :path "/tmp/r1" :tags '(work)))
         (r2 (magit-gh-repo--make :name "r2" :path "/tmp/r2" :tags '(personal)))
         (magit-gh-repo-list (list r1 r2))
         (built nil))
    (magit-gh-repo-dashboard-test--with-refresh-stubs
      (cl-letf (((symbol-function 'magit-gh-repo-dashboard--build-entry)
                 (lambda (r) (push (magit-gh-repo-name r) built) nil)))
        (with-temp-buffer
          (setq-local magit-gh-repo-dashboard--tag-filter 'work)
          (magit-gh-repo-dashboard-refresh))))
    (should (= 1 (length built)))
    (should (member "r1" built))
    (should-not (member "r2" built))))

(ert-deftest magit-gh-repo-dashboard/filter-nil-shows-all ()
  "Refresh with no tag filter shows all repos."
  (let* ((r1 (magit-gh-repo--make :name "r1" :path "/tmp/r1" :tags '(work)))
         (r2 (magit-gh-repo--make :name "r2" :path "/tmp/r2" :tags '(personal)))
         (magit-gh-repo-list (list r1 r2))
         (built nil))
    (magit-gh-repo-dashboard-test--with-refresh-stubs
      (cl-letf (((symbol-function 'magit-gh-repo-dashboard--build-entry)
                 (lambda (r) (push (magit-gh-repo-name r) built) nil)))
        (with-temp-buffer
          (setq-local magit-gh-repo-dashboard--tag-filter nil)
          (magit-gh-repo-dashboard-refresh))))
    (should (= 2 (length built)))))

;;;; magit-gh-repo-dashboard--build-format

(ert-deftest magit-gh-repo-dashboard/build-format-elastic-width ()
  "Name column width equals the longest repo name in the list."
  (let* ((r1 (magit-gh-repo--make :name "short" :path "/tmp/r1"))
         (r2 (magit-gh-repo--make :name "a-much-longer-name" :path "/tmp/r2"))
         (fmt (magit-gh-repo-dashboard--build-format (list r1 r2))))
    (should (= (length "a-much-longer-name") (cadr (aref fmt 0))))))

(ert-deftest magit-gh-repo-dashboard/build-format-minimum-width ()
  "Name column is at least as wide as the header label \"Name\"."
  (let* ((r (magit-gh-repo--make :name "x" :path "/tmp/r"))
         (fmt (magit-gh-repo-dashboard--build-format (list r))))
    (should (>= (cadr (aref fmt 0)) (length "Name")))))

(ert-deftest magit-gh-repo-dashboard/build-format-empty-list ()
  "Empty repo list yields minimum (header label) width."
  (let ((fmt (magit-gh-repo-dashboard--build-format nil)))
    (should (= (length "Name") (cadr (aref fmt 0))))))

;;;; magit-gh-repo-dashboard--build-entry

(ert-deftest magit-gh-repo-dashboard/build-entry-structure ()
  "build-entry returns (REPO VECTOR) with 5 columns (no Path)."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (magit-gh-repo-dashboard--stats-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--get-stats)
               (lambda (_)
                 (list :branch "main" :behind 0 :dirty nil :fetch-age 120.0
                       :head-hash "abc" :recent-log ""))))
      (let* ((entry (magit-gh-repo-dashboard--build-entry repo))
             (id (car entry))
             (vec (cadr entry)))
        (should (magit-gh-repo-p id))
        (should (= 5 (length vec)))
        (should (string-match-p "myrep" (aref vec 0)))
        (should (string-match-p "main" (aref vec 1)))
        (should (equal "2m" (aref vec 2)))
        (should (equal "" (aref vec 3)))
        (should (equal "" (aref vec 4)))))))

;;;; magit-gh-pr-dashboard--build-args

(ert-deftest magit-gh-repo-dashboard/pr-build-args-default ()
  "Default filters produce a gh search prs command."
  (let ((filters (list :state "open" :author "@me" :repo nil :org nil)))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should (equal "search" (nth 0 args)))
      (should (equal "prs" (nth 1 args)))
      (should (member "--state" args))
      (should (member "open" args))
      (should (member "--author" args))
      (should (member "@me" args)))))

(ert-deftest magit-gh-repo-dashboard/pr-build-args-with-repo ()
  "When :repo is set, uses gh pr list -R REPO."
  (let ((filters (list :state "open" :author nil :repo "owner/myrepo" :org nil)))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should (equal "pr" (nth 0 args)))
      (should (equal "list" (nth 1 args)))
      (should (member "-R" args))
      (should (member "owner/myrepo" args)))))

(ert-deftest magit-gh-repo-dashboard/pr-build-args-with-org ()
  "When :org is set (no :repo), adds --owner to the search command."
  (let ((filters (list :state "open" :author "@me" :repo nil :org "myorg")))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should (equal "search" (nth 0 args)))
      (should (member "--owner" args))
      (should (member "myorg" args)))))

(ert-deftest magit-gh-repo-dashboard/pr-build-args-state-closed ()
  "State \"closed\" is passed through for both search and per-repo modes."
  (let ((filters (list :state "closed" :author nil :repo nil :org nil)))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should (member "closed" args)))))

(ert-deftest magit-gh-repo-dashboard/pr-build-args-state-unknown ()
  "Unknown state falls back to \"open\" in search mode."
  (let ((filters (list :state "merged" :author nil :repo nil :org nil)))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should (equal "search" (nth 0 args)))
      (should (member "open" args))
      (should-not (member "merged" args)))))

(ert-deftest magit-gh-repo-dashboard/pr-build-args-no-author ()
  "Nil :author omits --author from the command."
  (let ((filters (list :state "open" :author nil :repo nil :org nil)))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should-not (member "--author" args)))))

;;;; magit-gh-pr-dashboard--format-ci

(ert-deftest magit-gh-repo-dashboard/format-ci-success ()
  (should (equal "pass" (substring-no-properties
                         (magit-gh-pr-dashboard--format-ci "SUCCESS")))))

(ert-deftest magit-gh-repo-dashboard/format-ci-failure ()
  (should (equal "fail" (substring-no-properties
                         (magit-gh-pr-dashboard--format-ci "FAILURE")))))

(ert-deftest magit-gh-repo-dashboard/format-ci-pending ()
  (should (equal "pending" (substring-no-properties
                            (magit-gh-pr-dashboard--format-ci "PENDING")))))

(ert-deftest magit-gh-repo-dashboard/format-ci-nil ()
  "Nil or unknown CI state produces a dash."
  (should (equal "—" (magit-gh-pr-dashboard--format-ci nil)))
  (should (equal "—" (magit-gh-pr-dashboard--format-ci "UNKNOWN"))))

;;;; magit-gh-pr-dashboard--format-review

(ert-deftest magit-gh-repo-dashboard/format-review-approved ()
  (should (equal "approved" (substring-no-properties
                             (magit-gh-pr-dashboard--format-review "APPROVED")))))

(ert-deftest magit-gh-repo-dashboard/format-review-changes ()
  (should (equal "changes req" (substring-no-properties
                                (magit-gh-pr-dashboard--format-review "CHANGES_REQUESTED")))))

(ert-deftest magit-gh-repo-dashboard/format-review-needed ()
  (should (equal "needed" (substring-no-properties
                           (magit-gh-pr-dashboard--format-review "REVIEW_REQUIRED")))))

(ert-deftest magit-gh-repo-dashboard/format-review-nil ()
  "Nil review decision returns empty string."
  (should (equal "" (magit-gh-pr-dashboard--format-review nil))))

;;;; magit-gh-pr-dashboard--comments-count

(ert-deftest magit-gh-repo-dashboard/comments-count-integer ()
  "When comments field is already an integer, return it directly."
  (let ((pr '((comments . 7))))
    (should (= 7 (magit-gh-pr-dashboard--comments-count pr)))))

(ert-deftest magit-gh-repo-dashboard/comments-count-alist ()
  "When comments is an alist with totalCount, extract it."
  (let ((pr `((comments . ((totalCount . 3))))))
    (should (= 3 (magit-gh-pr-dashboard--comments-count pr)))))

(ert-deftest magit-gh-repo-dashboard/comments-count-nil ()
  "Nil comments field returns 0."
  (let ((pr '((title . "test"))))
    (should (= 0 (magit-gh-pr-dashboard--comments-count pr)))))

;;;; magit-gh-pr-dashboard--parse-output

(defun magit-gh-repo-dashboard-test--make-pr-json (&rest overrides)
  "Build a minimal PR JSON object string, optionally overriding fields."
  (let ((pr (append
             (list (cons 'number 42)
                   (cons 'title "Test PR")
                   (cons 'state "OPEN")
                   (cons 'author '((login . "alice")))
                   (cons 'updatedAt "2024-01-15T10:30:00Z")
                   (cons 'comments 5)
                   (cons 'reviewDecision "APPROVED")
                   (cons 'isDraft :false)
                   (cons 'url "https://github.com/owner/repo/pull/42"))
             overrides)))
    pr))

(ert-deftest magit-gh-repo-dashboard/parse-output-empty-array ()
  "Empty JSON array produces nil."
  (should (null (magit-gh-pr-dashboard--parse-output
                 "[]"
                 (list :repo "owner/myrepo")))))

(ert-deftest magit-gh-repo-dashboard/parse-output-non-json ()
  "Non-JSON output (e.g., error message) produces nil."
  (should (null (magit-gh-pr-dashboard--parse-output
                 "error: not found"
                 (list :state "open")))))

(ert-deftest magit-gh-repo-dashboard/parse-output-per-repo-mode ()
  "Per-repo mode uses :repo filter value as the repo-name."
  (let* ((pr (magit-gh-repo-dashboard-test--make-pr-json))
         (json (concat "[" (json-serialize pr) "]"))
         (filters (list :state "open" :repo "owner/myrepo"))
         (entries (magit-gh-pr-dashboard--parse-output json filters)))
    (should (= 1 (length entries)))
    (let* ((entry (car entries))
           (id (car entry))
           (vec (cadr entry)))
      (should (equal "owner/myrepo" (plist-get id :repo)))
      (should (= 42 (plist-get id :number)))
      (should (equal "owner/myrepo" (aref vec 0)))
      (should (equal "42" (aref vec 1))))))

(ert-deftest magit-gh-repo-dashboard/parse-output-search-mode ()
  "Search mode extracts repo name from the `repository' field."
  (let* ((pr (append (magit-gh-repo-dashboard-test--make-pr-json)
                     (list (cons 'repository '((nameWithOwner . "org/other"))))))
         (json (concat "[" (json-serialize pr) "]"))
         (filters (list :state "open" :author "@me"))
         (entries (magit-gh-pr-dashboard--parse-output json filters)))
    (should (= 1 (length entries)))
    (let* ((entry (car entries))
           (id (car entry)))
      (should (equal "org/other" (plist-get id :repo))))))

(ert-deftest magit-gh-repo-dashboard/parse-output-multiple-prs ()
  "Multiple PRs produce multiple entries."
  (let* ((pr1 (magit-gh-repo-dashboard-test--make-pr-json))
         (pr2 (append (magit-gh-repo-dashboard-test--make-pr-json) (list (cons 'number 99))))
         (json (concat "[" (json-serialize pr1) "," (json-serialize pr2) "]"))
         (filters (list :repo "owner/repo"))
         (entries (magit-gh-pr-dashboard--parse-output json filters)))
    (should (= 2 (length entries)))))

;;;; magit-gh-pr-dashboard--build-entry (column structure)

(ert-deftest magit-gh-repo-dashboard/build-entry-columns ()
  "build-entry produces a 7-element vector matching the column format."
  (let* ((pr '((number . 10)
               (title . "My PR")
               (updatedAt . "2024-06-01T12:00:00Z")
               (comments . 2)
               (reviewDecision . "APPROVED")
               (statusCheckRollup . ((state . "SUCCESS")))
               (url . "https://example.com/10")))
         (entry (magit-gh-pr-dashboard--build-entry pr "myorg/myrepo"))
         (id (car entry))
         (vec (cadr entry)))
    (should (= 7 (length vec)))
    (should (equal "myorg/myrepo" (aref vec 0)))
    (should (equal "10" (aref vec 1)))
    (should (equal "My PR" (aref vec 2)))
    (should (equal "pass" (substring-no-properties (aref vec 3))))
    (should (equal "approved" (substring-no-properties (aref vec 6))))
    (should (equal 10 (plist-get id :number)))
    (should (equal "myorg/myrepo" (plist-get id :repo)))))

(ert-deftest magit-gh-repo-dashboard/build-entry-truncates-long-title ()
  "Titles longer than 36 chars are truncated with an ellipsis."
  (let* ((pr `((number . 1)
               (title . ,(make-string 50 ?X))
               (updatedAt . "")
               (comments . 0)))
         (entry (magit-gh-pr-dashboard--build-entry pr "r"))
         (vec (cadr entry)))
    (should (<= (length (aref vec 2)) 37))))

(provide 'test-magit-gh-repo-dashboard)

;;; test-magit-gh-repo-dashboard.el ends here
