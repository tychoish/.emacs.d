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
      (should (null (magit-gh-repo-sort-hint r)))
      (should (null (magit-gh-repo-worktree r)))))

(ert-deftest magit-gh-repo-dashboard/register-worktree ()
  ":worktree t is stored correctly."
  (let ((magit-gh-repo-list nil))
    (magit-gh-repo-register "r" "/tmp/r" :worktree t)
    (should (eq t (magit-gh-repo-worktree (car magit-gh-repo-list)))))))

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
  "collect-stats populates all stat fields using magit-git functions."
  (let ((repo (magit-gh-repo--make :name "test" :path "/tmp/test")))
    (cl-letf (((symbol-function 'magit-git-string)
               (lambda (&rest args)
                 (cond
                  ((member "branch" args) "main")
                  ((member "rev-list" args) "2")
                  (t nil))))
              ((symbol-function 'magit-get)
               (lambda (&rest _) "git@github.com:user/test.git"))
              ((symbol-function 'magit-git-lines)
               (lambda (&rest args)
                 (cond
                  ((member "status" args) '(" M foo.el"))
                  ((member "log" args) '("abc123 fix foo" "def456 add bar"))
                  (t nil))))
              ((symbol-function 'magit-gh-repo-dashboard--fetch-age)
               (lambda (_) 3600.0))
              ((symbol-function 'magit-gh-repo-dashboard--head-hash)
               (lambda (_) "abc123def456")))
      (let ((magit-gh-repo-dashboard--stats-cache (make-hash-table :test #'equal)))
        (let ((stats (magit-gh-repo-dashboard--collect-stats repo)))
          (should (equal "main" (plist-get stats :branch)))
          (should (equal "git@github.com:user/test.git" (plist-get stats :remote-origin)))
          (should (= 2 (plist-get stats :behind)))
          (should (eq t (plist-get stats :dirty)))
          (should (equal '(" M foo.el") (plist-get stats :uncommitted-files)))
          (should (= 3600.0 (plist-get stats :fetch-age)))
          (should (equal "abc123def456" (plist-get stats :head-hash)))
          (should (string-match-p "fix foo" (plist-get stats :recent-log))))))))

(ert-deftest magit-gh-repo-dashboard/collect-stats-clean-workdir ()
  "collect-stats sets :dirty nil and :uncommitted-files nil when porcelain is empty."
  (let ((repo (magit-gh-repo--make :name "test" :path "/tmp/test")))
    (cl-letf (((symbol-function 'magit-git-string)
               (lambda (&rest args)
                 (cond
                  ((member "branch" args) "feat")
                  ((member "rev-list" args) "0")
                  (t nil))))
              ((symbol-function 'magit-get)
               (lambda (&rest _) nil))
              ((symbol-function 'magit-git-lines)
               (lambda (&rest _) nil))
              ((symbol-function 'magit-gh-repo-dashboard--fetch-age)
               (lambda (_) nil))
              ((symbol-function 'magit-gh-repo-dashboard--head-hash)
               (lambda (_) "deadbeef")))
      (let ((magit-gh-repo-dashboard--stats-cache (make-hash-table :test #'equal)))
        (let ((stats (magit-gh-repo-dashboard--collect-stats repo)))
          (should (eq nil (plist-get stats :dirty)))
          (should (null (plist-get stats :uncommitted-files)))
          (should (null (plist-get stats :remote-origin)))
          (should (= 0 (plist-get stats :behind))))))))

;;;; magit-gh-repo-dashboard--get-stats (cache invalidation)

(ert-deftest magit-gh-repo-dashboard/get-stats-uses-cache-on-same-hash ()
  "get-stats returns cached stats when HEAD hash matches."
  (let* ((repo (magit-gh-repo--make :name "test" :path "/tmp/test"))
         (cached (list :branch "main" :remote-origin nil :behind 0
                       :dirty nil :uncommitted-files nil
                       :fetch-age 60.0 :head-hash "abc123" :recent-log ""))
         (magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/test" :stats cached)
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--head-hash)
               (lambda (_) "abc123")))
      (should (equal cached (magit-gh-repo-dashboard--get-stats repo))))))

(ert-deftest magit-gh-repo-dashboard/get-stats-invalidates-on-new-hash ()
  "get-stats collects fresh stats when HEAD hash has changed."
  (let* ((repo (magit-gh-repo--make :name "test" :path "/tmp/test"))
         (cached (list :branch "main" :remote-origin nil :behind 0
                       :dirty nil :uncommitted-files nil
                       :fetch-age 60.0 :head-hash "old123" :recent-log ""))
         (magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/test" :stats cached)
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--head-hash)
               (lambda (_) "new456"))
              ((symbol-function 'magit-gh-repo-dashboard--collect-stats)
               (lambda (_) (list :branch "feat" :remote-origin nil :behind 1
                                 :dirty t :uncommitted-files nil
                                 :fetch-age nil :head-hash "new456" :recent-log ""))))
      (let ((result (magit-gh-repo-dashboard--get-stats repo)))
        (should (equal "feat" (plist-get result :branch)))
        (should (= 1 (plist-get result :behind)))))))

;;;; magit-gh-repo-dashboard--auto-commit

(ert-deftest magit-gh-repo-dashboard/auto-commit-uses-default-message ()
  "With :auto-commit t, calls magit-call-git add then commit with the default message."
  (let ((repo (magit-gh-repo--make :name "test" :path "/tmp/test" :auto-commit t))
        (git-calls nil))
    (cl-letf (((symbol-function 'magit-call-git)
               (lambda (&rest args)
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
    (cl-letf (((symbol-function 'magit-call-git)
               (lambda (&rest args)
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
  "build-entry returns (REPO VECTOR) with columns matching active column count."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (magit-gh-repo-dashboard--stats-cache (make-hash-table :test #'equal))
        (magit-gh-repo-dashboard-columns
         '((name . t) (branch . t) (fetched . t) (behind . t) (changes . t) (worktree . t)))
        (magit-gh-repo-dashboard--worktree-map (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--get-stats)
               (lambda (_)
                 (list :branch "main" :behind 0 :dirty nil :fetch-age 120.0
                       :head-hash "abc" :recent-log ""))))
      (let* ((entry (magit-gh-repo-dashboard--build-entry repo))
             (id (car entry))
             (vec (cadr entry)))
        (should (magit-gh-repo-p id))
        (should (= 6 (length vec)))
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

;;;; magit-gh-repo-overview--insert-kv

(ert-deftest magit-gh-repo-dashboard/insert-kv-format ()
  "insert-kv writes a bold padded key and value on one line."
  (with-temp-buffer
    (magit-gh-repo-overview--insert-kv "Branch" "main")
    (let ((text (buffer-string)))
      (should (string-match-p "Branch" text))
      (should (string-match-p "main" text))
      (should (string-match-p "\n" text)))))

(ert-deftest magit-gh-repo-dashboard/insert-kv-key-bold ()
  "insert-kv applies bold face to the key portion."
  (with-temp-buffer
    (magit-gh-repo-overview--insert-kv "Repository" "myrepo")
    (let ((face (get-text-property 0 'face (buffer-string))))
      (should (equal 'bold face)))))

(ert-deftest magit-gh-repo-dashboard/insert-kv-value-face ()
  "insert-kv applies VALUE-FACE to the value when supplied."
  (with-temp-buffer
    (magit-gh-repo-overview--insert-kv "Behind" "3 commits" 'warning)
    (let* ((text (buffer-string))
           (value-start (string-match "3 commits" text)))
      (should value-start)
      (should (equal 'warning (get-text-property value-start 'face text))))))

(ert-deftest magit-gh-repo-dashboard/insert-kv-action-property ()
  "insert-kv tags the line with `magit-gh-repo-overview-action' when action given."
  (with-temp-buffer
    (magit-gh-repo-overview--insert-kv "Repository" "r" nil (cons 'magit-status "/tmp/r"))
    (goto-char (point-min))
    (let ((action (get-text-property (point) 'magit-gh-repo-overview-action)))
      (should (equal 'magit-status (car action)))
      (should (equal "/tmp/r" (cdr action))))))

(ert-deftest magit-gh-repo-dashboard/insert-kv-no-action-when-nil ()
  "insert-kv does not set action property when ACTION is nil."
  (with-temp-buffer
    (magit-gh-repo-overview--insert-kv "Branch" "main")
    (goto-char (point-min))
    (should (null (get-text-property (point) 'magit-gh-repo-overview-action)))))

;;;; magit-gh-repo-overview--render

(defun magit-gh-repo-dashboard-test--make-stats (&rest overrides)
  "Return a minimal stats plist with OVERRIDES taking precedence over defaults.
Overrides are placed first so `plist-get' finds them before the defaults."
  (append overrides
          (list :branch "main"
                :remote-origin "git@github.com:user/repo.git"
                :behind 0
                :dirty nil
                :uncommitted-files nil
                :fetch-age 60.0
                :head-hash "abc123"
                :recent-log "abc123 fix foo\ndef456 add bar")))

(ert-deftest magit-gh-repo-dashboard/render-loading-state ()
  "render with nil stats inserts a loading message."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep")))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo nil nil)
      (should (string-match-p "[Ll]oading" (buffer-string))))))

(ert-deftest magit-gh-repo-dashboard/render-kv-fields ()
  "render with stats inserts Repository, Path, Remote, Branch as KV pairs."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-gh-repo-dashboard-test--make-stats)))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats nil)
      (let ((text (buffer-string)))
        (should (string-match-p "Repository" text))
        (should (string-match-p "myrep" text))
        (should (string-match-p "Path" text))
        (should (string-match-p "/tmp/myrep" text))
        (should (string-match-p "Remote" text))
        (should (string-match-p "git@github.com" text))
        (should (string-match-p "Branch" text))
        (should (string-match-p "main" text))))))

(ert-deftest magit-gh-repo-dashboard/render-pr-loading ()
  "render with nil pr-counts shows loading placeholder for PRs."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-gh-repo-dashboard-test--make-stats)))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats nil)
      (should (string-match-p "loading" (buffer-string))))))

(ert-deftest magit-gh-repo-dashboard/render-pr-counts ()
  "render with pr-counts shows total and mine."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-gh-repo-dashboard-test--make-stats)))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats (cons 5 2))
      (let ((text (buffer-string)))
        (should (string-match-p "5" text))
        (should (string-match-p "2" text))))))

(ert-deftest magit-gh-repo-dashboard/render-dirty-shows-files ()
  "render with dirty stats lists uncommitted files."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-gh-repo-dashboard-test--make-stats
                :dirty t
                :uncommitted-files '(" M foo.el" "?? bar.el"))))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats nil)
      (let ((text (buffer-string)))
        (should (string-match-p "foo.el" text))
        (should (string-match-p "bar.el" text))))))

(ert-deftest magit-gh-repo-dashboard/render-recent-commits ()
  "render inserts Recent Commits section when recent-log is non-empty."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-gh-repo-dashboard-test--make-stats)))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats nil)
      (let ((text (buffer-string)))
        (should (string-match-p "Recent Commits" text))
        (should (string-match-p "fix foo" text))))))

(ert-deftest magit-gh-repo-dashboard/render-no-remote-origin ()
  "render omits Remote row when :remote-origin is nil."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-gh-repo-dashboard-test--make-stats :remote-origin nil)))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats nil)
      (should-not (string-match-p "Remote" (buffer-string))))))

;;;; magit-gh-repo-overview--classify-files

(ert-deftest magit-gh-repo-dashboard/classify-files-staged ()
  "Staged modifications appear under 'staged."
  (let* ((lines '("M  foo.el" "A  bar.el"))
         (result (magit-gh-repo-overview--classify-files lines)))
    (should (member "foo.el" (alist-get 'staged result)))
    (should (member "bar.el" (alist-get 'staged result)))))

(ert-deftest magit-gh-repo-dashboard/classify-files-unstaged ()
  "Worktree-only modifications appear under 'unstaged."
  (let* ((lines '(" M foo.el"))
         (result (magit-gh-repo-overview--classify-files lines)))
    (should (member "foo.el" (alist-get 'unstaged result)))
    (should (null (alist-get 'staged result)))))

(ert-deftest magit-gh-repo-dashboard/classify-files-untracked ()
  "?? lines appear under 'untracked."
  (let* ((lines '("?? new.el"))
         (result (magit-gh-repo-overview--classify-files lines)))
    (should (member "new.el" (alist-get 'untracked result)))
    (should (null (alist-get 'staged result)))))

(ert-deftest magit-gh-repo-dashboard/classify-files-deleted-staged ()
  "Staged deletions appear under 'deleted, not 'staged."
  (let* ((lines '("D  gone.el"))
         (result (magit-gh-repo-overview--classify-files lines)))
    (should (member "gone.el" (alist-get 'deleted result)))
    (should (null (alist-get 'staged result)))))

(ert-deftest magit-gh-repo-dashboard/classify-files-deleted-unstaged ()
  "Unstaged deletions appear under 'deleted, not 'unstaged."
  (let* ((lines '(" D gone.el"))
         (result (magit-gh-repo-overview--classify-files lines)))
    (should (member "gone.el" (alist-get 'deleted result)))
    (should (null (alist-get 'unstaged result)))))

(ert-deftest magit-gh-repo-dashboard/classify-files-trims-filename ()
  "Filenames have leading/trailing whitespace stripped."
  (let* ((lines '("M  foo.el "))
         (result (magit-gh-repo-overview--classify-files lines)))
    (should (member "foo.el" (alist-get 'staged result)))))

(ert-deftest magit-gh-repo-dashboard/classify-files-empty-input ()
  "Empty input yields all-nil categories."
  (let ((result (magit-gh-repo-overview--classify-files nil)))
    (should (null (alist-get 'staged result)))
    (should (null (alist-get 'unstaged result)))
    (should (null (alist-get 'deleted result)))
    (should (null (alist-get 'untracked result)))))

;;;; Render: uncommitted file sections

(ert-deftest magit-gh-repo-dashboard/render-dirty-sections ()
  "render shows Staged/Unstaged/Untracked section headers when present."
  (let ((repo (magit-gh-repo--make :name "r" :path "/tmp/r"))
        (stats (magit-gh-repo-dashboard-test--make-stats
                :dirty t
                :uncommitted-files '("M  foo.el" " M bar.el" "?? baz.el"))))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats nil)
      (let ((text (buffer-string)))
        (should (string-match-p "Staged" text))
        (should (string-match-p "foo.el" text))
        (should (string-match-p "Unstaged" text))
        (should (string-match-p "bar.el" text))
        (should (string-match-p "Untracked" text))
        (should (string-match-p "baz.el" text))))))

;;;; Render: PR counts

(ert-deftest magit-gh-repo-dashboard/render-pr-counts-zero ()
  "render shows None when PR count is zero."
  (let ((repo (magit-gh-repo--make :name "r" :path "/tmp/r"))
        (stats (magit-gh-repo-dashboard-test--make-stats)))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats (cons 0 0))
      (should (string-match-p "None" (buffer-string))))))

(ert-deftest magit-gh-repo-dashboard/render-pr-counts-nonzero ()
  "render shows Open count and Yours when nonzero."
  (let ((repo (magit-gh-repo--make :name "r" :path "/tmp/r"))
        (stats (magit-gh-repo-dashboard-test--make-stats)))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats (cons 5 2))
      (let ((text (buffer-string)))
        (should (string-match-p "Open" text))
        (should (string-match-p "5" text))
        (should (string-match-p "Yours" text))
        (should (string-match-p "2" text))))))

(ert-deftest magit-gh-repo-dashboard/render-pr-counts-yours-zero ()
  "render omits Yours line when mine=0."
  (let ((repo (magit-gh-repo--make :name "r" :path "/tmp/r"))
        (stats (magit-gh-repo-dashboard-test--make-stats)))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats (cons 3 0))
      (let ((text (buffer-string)))
        (should (string-match-p "Open" text))
        (should-not (string-match-p "Yours" text))))))

;;;; Transient predicates

(ert-deftest magit-gh-repo-dashboard/dirty-or-unknown-p-when-no-stats ()
  "dirty-or-unknown-p returns t when stats not yet cached."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--repo-at-point)
               (lambda () (magit-gh-repo--make :name "r" :path "/tmp/r"))))
      (should (magit-gh-repo-dashboard--dirty-or-unknown-p)))))

(ert-deftest magit-gh-repo-dashboard/dirty-or-unknown-p-when-clean ()
  "dirty-or-unknown-p returns nil when stats are cached and clean."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/r" :stats (list :dirty nil :branch "main" :behind 0))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--repo-at-point)
               (lambda () (magit-gh-repo--make :name "r" :path "/tmp/r"))))
      (should-not (magit-gh-repo-dashboard--dirty-or-unknown-p)))))

(ert-deftest magit-gh-repo-dashboard/has-commands-p-when-registered ()
  "has-commands-p returns t when commands are registered."
  (cl-letf (((symbol-function 'magit-gh-repo-dashboard--repo-at-point)
             (lambda ()
               (magit-gh-repo--make :name "r" :path "/tmp/r"
                                    :commands '(("run" . my-fn))))))
    (should (magit-gh-repo-dashboard--has-commands-p))))

(ert-deftest magit-gh-repo-dashboard/has-commands-p-when-none ()
  "has-commands-p returns nil when no commands are registered."
  (cl-letf (((symbol-function 'magit-gh-repo-dashboard--repo-at-point)
             (lambda () (magit-gh-repo--make :name "r" :path "/tmp/r"))))
    (should-not (magit-gh-repo-dashboard--has-commands-p))))

(ert-deftest magit-gh-repo-dashboard/overview-has-changes-p-when-dirty ()
  "overview-has-changes-p returns t when stats show dirty."
  (with-temp-buffer
    (setq-local magit-gh-repo-overview--stats (list :dirty t :branch "main"))
    (should (magit-gh-repo-overview--has-changes-p))))

(ert-deftest magit-gh-repo-dashboard/overview-has-changes-p-when-clean ()
  "overview-has-changes-p returns nil when stats show clean."
  (with-temp-buffer
    (setq-local magit-gh-repo-overview--stats (list :dirty nil :branch "main"))
    (should-not (magit-gh-repo-overview--has-changes-p))))

;;;; magit-gh-repo-overview--render action properties

(ert-deftest magit-gh-repo-dashboard/render-repository-line-action ()
  "render tags the Repository line with a magit-status action."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-gh-repo-dashboard-test--make-stats)))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats nil)
      (goto-char (point-min))
      (let ((action (get-text-property (point) 'magit-gh-repo-overview-action)))
        (should (equal 'magit-status (car action)))
        (should (equal "/tmp/myrep" (cdr action)))))))

(ert-deftest magit-gh-repo-dashboard/render-path-line-action ()
  "render tags the Path line with a dired action."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-gh-repo-dashboard-test--make-stats)))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats nil)
      (goto-char (point-min))
      (search-forward "Path")
      (let ((action (get-text-property (point) 'magit-gh-repo-overview-action)))
        (should (equal 'dired (car action)))
        (should (equal "/tmp/myrep" (cdr action)))))))

(ert-deftest magit-gh-repo-dashboard/render-commit-line-action ()
  "render tags Recent Commits lines with magit-show-commit actions."
  (let ((repo (magit-gh-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-gh-repo-dashboard-test--make-stats
                :recent-log "abc1234 fix something")))
    (with-temp-buffer
      (magit-gh-repo-overview--render repo stats nil)
      (goto-char (point-min))
      (search-forward "abc1234")
      (let ((action (get-text-property (point) 'magit-gh-repo-overview-action)))
        (should (equal 'magit-show-commit (car action)))
        (should (equal "abc1234" (cdr action)))))))

;;;; magit-gh-repo-overview-follow

(ert-deftest magit-gh-repo-dashboard/follow-magit-status ()
  "follow opens magit status when action is 'magit-status."
  (let ((repo (magit-gh-repo--make :name "r" :path "/tmp/r"))
        (visited-path nil))
    (cl-letf (((symbol-function 'magit-status-setup-buffer)
               (lambda (path) (setq visited-path path))))
      (with-temp-buffer
        (setq-local magit-gh-repo-overview--repo repo)
        (let ((inhibit-read-only t))
          (magit-gh-repo-overview--insert-kv
           "Repository" "r" nil (cons 'magit-status "/tmp/r")))
        (goto-char (point-min))
        (magit-gh-repo-overview-follow)))
    (should (equal "/tmp/r" visited-path))))

(ert-deftest magit-gh-repo-dashboard/follow-dired ()
  "follow opens dired when action is 'dired."
  (let ((repo (magit-gh-repo--make :name "r" :path "/tmp/r"))
        (visited-path nil))
    (cl-letf (((symbol-function 'dired)
               (lambda (path) (setq visited-path path))))
      (with-temp-buffer
        (setq-local magit-gh-repo-overview--repo repo)
        (let ((inhibit-read-only t))
          (magit-gh-repo-overview--insert-kv
           "Path" "/tmp/r" nil (cons 'dired "/tmp/r")))
        (goto-char (point-min))
        (magit-gh-repo-overview-follow)))
    (should (equal "/tmp/r" visited-path))))

(ert-deftest magit-gh-repo-dashboard/follow-noop-on-untagged-line ()
  "follow does nothing on lines without an action property."
  (let ((repo (magit-gh-repo--make :name "r" :path "/tmp/r")))
    (with-temp-buffer
      (setq-local magit-gh-repo-overview--repo repo)
      (let ((inhibit-read-only t))
        (insert "no action here\n"))
      (goto-char (point-min))
      (should-not (get-text-property (point) 'magit-gh-repo-overview-action))
      (magit-gh-repo-overview-follow)))) ; should not error

;;;; magit-gh-repo-dashboard--batch-run

(ert-deftest magit-gh-repo-dashboard/batch-run-collects-ok ()
  "batch-run calls on-all-done with all results when each op returns 'ok."
  (let* ((repos (list (magit-gh-repo--make :name "r1" :path "/tmp/r1")
                      (magit-gh-repo--make :name "r2" :path "/tmp/r2")))
         (all-results nil))
    (magit-gh-repo-dashboard--batch-run
     repos
     (lambda (repo cb) (funcall cb 'ok))
     "test"
     (lambda (results) (setq all-results results)))
    (should (= 2 (length all-results)))
    (should (seq-every-p (lambda (r) (eq 'ok (cdr r))) all-results))))

(ert-deftest magit-gh-repo-dashboard/batch-run-mixed-statuses ()
  "batch-run handles a mix of 'ok, 'skipped, and 'error results."
  (let* ((statuses '(ok skipped error))
         (repos (list (magit-gh-repo--make :name "r1" :path "/tmp/r1")
                      (magit-gh-repo--make :name "r2" :path "/tmp/r2")
                      (magit-gh-repo--make :name "r3" :path "/tmp/r3")))
         (all-results nil)
         (idx 0))
    (magit-gh-repo-dashboard--batch-run
     repos
     (lambda (repo cb)
       (funcall cb (nth idx statuses))
       (setq idx (1+ idx)))
     "test"
     (lambda (results) (setq all-results results)))
    (should (= 1 (seq-count (lambda (r) (eq 'ok (cdr r))) all-results)))
    (should (= 1 (seq-count (lambda (r) (eq 'skipped (cdr r))) all-results)))
    (should (= 1 (seq-count (lambda (r) (eq 'error (cdr r))) all-results)))))

(ert-deftest magit-gh-repo-dashboard/batch-run-calls-on-all-done ()
  "batch-run calls on-all-done exactly once after the last repo completes."
  (let* ((repos (list (magit-gh-repo--make :name "r1" :path "/tmp/r1")))
         (call-count 0))
    (magit-gh-repo-dashboard--batch-run
     repos
     (lambda (_repo cb) (funcall cb 'ok))
     "test"
     (lambda (_) (setq call-count (1+ call-count))))
    (should (= 1 call-count))))

;;;; magit-gh-repo-dashboard--auto-commit-async

(ert-deftest magit-gh-repo-dashboard/auto-commit-async-skipped-when-clean ()
  "auto-commit-async returns 'skipped when git status --porcelain is empty."
  (let* ((repo (magit-gh-repo--make :name "r" :path "/tmp/r" :auto-commit t))
         (result nil))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--run-git)
               (lambda (_path args on-success &optional _on-error)
                 (when (member "status" args)
                   (funcall on-success "")))))
      (magit-gh-repo-dashboard--auto-commit-async repo (lambda (s) (setq result s))))
    (should (eq 'skipped result))))

(ert-deftest magit-gh-repo-dashboard/auto-commit-async-ok-when-dirty ()
  "auto-commit-async returns 'ok after successful add+commit."
  (let* ((repo (magit-gh-repo--make :name "r" :path "/tmp/r" :auto-commit t))
         (result nil)
         (git-calls nil))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--run-git)
               (lambda (_path args on-success &optional _on-error)
                 (push (car args) git-calls)
                 (cond
                  ((member "status" args) (funcall on-success " M foo.el"))
                  ((member "add" args) (funcall on-success ""))
                  ((member "commit" args) (funcall on-success ""))))))
      (magit-gh-repo-dashboard--auto-commit-async repo (lambda (s) (setq result s))))
    (should (eq 'ok result))
    (should (member "status" git-calls))
    (should (member "add" git-calls))
    (should (member "commit" git-calls))))

(ert-deftest magit-gh-repo-dashboard/auto-commit-async-error-on-add-failure ()
  "auto-commit-async returns 'error when git add fails."
  (let* ((repo (magit-gh-repo--make :name "r" :path "/tmp/r" :auto-commit t))
         (result nil))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--run-git)
               (lambda (_path args on-success on-error)
                 (cond
                  ((member "status" args) (funcall on-success " M foo.el"))
                  ((member "add" args) (funcall on-error "error" 1))))))
      (magit-gh-repo-dashboard--auto-commit-async repo (lambda (s) (setq result s))))
    (should (eq 'error result))))

(ert-deftest magit-gh-repo-dashboard/auto-commit-async-uses-message-function ()
  "auto-commit-async uses the :auto-commit function to generate the commit message."
  (let* ((custom-msg "custom: my message")
         (repo (magit-gh-repo--make :name "r" :path "/tmp/r"
                                    :auto-commit (lambda (_r) custom-msg)))
         (commit-msg nil))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--run-git)
               (lambda (_path args on-success &optional _on-error)
                 (cond
                  ((member "status" args) (funcall on-success " M foo.el"))
                  ((member "add" args) (funcall on-success ""))
                  ((member "commit" args)
                   (setq commit-msg (cadr (member "-m" args)))
                   (funcall on-success ""))))))
      (magit-gh-repo-dashboard--auto-commit-async repo #'ignore))
    (should (equal custom-msg commit-msg))))

;;;; magit-gh-repo-dashboard-commit-all (async)

(ert-deftest magit-gh-repo-dashboard/commit-all-async-user-error-when-none ()
  "commit-all signals user-error when no repos have :auto-commit configured."
  (let ((magit-gh-repo-list (list (magit-gh-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-gh-repo-dashboard-commit-all) :type 'user-error)))

(ert-deftest magit-gh-repo-dashboard/commit-all-async-runs-batch ()
  "commit-all dispatches --batch-run for repos with :auto-commit set."
  (let* ((magit-gh-repo-list
          (list (magit-gh-repo--make :name "r1" :path "/tmp/r1" :auto-commit t)
                (magit-gh-repo--make :name "r2" :path "/tmp/r2")))
         (batched-repos nil))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--batch-run)
               (lambda (repos _op _label &optional _done)
                 (setq batched-repos (seq-map #'magit-gh-repo-name repos)))))
      (magit-gh-repo-dashboard-commit-all))
    (should (= 1 (length batched-repos)))
    (should (equal "r1" (car batched-repos)))))

;;;; magit-gh-repo-dashboard-auto-sync

(ert-deftest magit-gh-repo-dashboard/auto-sync-user-error-when-none ()
  "autosync signals user-error when no repos have :auto-commit or :auto-sync."
  (let ((magit-gh-repo-list (list (magit-gh-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-gh-repo-dashboard-auto-sync) :type 'user-error)))

(ert-deftest magit-gh-repo-dashboard/auto-sync-dispatches-commit-and-sync ()
  "autosync runs commit batch for :auto-commit repos and sync batch for :auto-sync repos."
  (let* ((magit-gh-repo-list
          (list (magit-gh-repo--make :name "c1" :path "/tmp/c1" :auto-commit t)
                (magit-gh-repo--make :name "s1" :path "/tmp/s1" :auto-sync 'fetch)
                (magit-gh-repo--make :name "n1" :path "/tmp/n1")))
         (batch-labels nil))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--batch-run)
               (lambda (_repos _op label &optional _done)
                 (push label batch-labels))))
      (magit-gh-repo-dashboard-auto-sync))
    (should (= 2 (length batch-labels)))
    (should (member "magit-gh autosync commit" batch-labels))
    (should (member "magit-gh autosync sync" batch-labels))))

(ert-deftest magit-gh-repo-dashboard/auto-sync-commit-only-when-no-sync ()
  "autosync runs only the commit batch when no repos have :auto-sync."
  (let* ((magit-gh-repo-list
          (list (magit-gh-repo--make :name "c1" :path "/tmp/c1" :auto-commit t)))
         (batch-labels nil))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--batch-run)
               (lambda (_repos _op label &optional _done)
                 (push label batch-labels))))
      (magit-gh-repo-dashboard-auto-sync))
    (should (= 1 (length batch-labels)))
    (should (member "magit-gh autosync commit" batch-labels))))

;;;; Builder and agent-shell commands

(ert-deftest magit-gh-repo-dashboard/builder-delegates-to-builder ()
  "builder command invokes builder-compile-project in the repo directory."
  (let ((called-in nil))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--repo-at-point)
               (lambda () (magit-gh-repo--make :name "r" :path "/tmp/r")))
              ((symbol-function 'builder-compile-project)
               (lambda () (interactive) (setq called-in default-directory))))
      (magit-gh-repo-dashboard-builder))
    (should (string-prefix-p "/tmp/r" (or called-in "")))))

(ert-deftest magit-gh-repo-dashboard/agent-shell-queue-callable ()
  "agent-shell-queue command calls agent-shell-queue-buffer-open."
  (let ((called nil))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--repo-at-point)
               (lambda () (magit-gh-repo--make :name "r" :path "/tmp/r")))
              ((symbol-function 'agent-shell-queue-buffer-open)
               (lambda () (interactive) (setq called t))))
      (magit-gh-repo-dashboard-agent-shell-queue))
    (should called)))

(ert-deftest magit-gh-repo-dashboard/overview-builder-delegates ()
  "overview builder command invokes builder-compile-project in the repo directory."
  (let* ((repo (magit-gh-repo--make :name "r" :path "/tmp/r"))
         (called-in nil))
    (cl-letf (((symbol-function 'magit-gh-repo-overview--current-repo)
               (lambda () repo))
              ((symbol-function 'builder-compile-project)
               (lambda () (interactive) (setq called-in default-directory))))
      (magit-gh-repo-overview-builder))
    (should (string-prefix-p "/tmp/r" (or called-in "")))))

(ert-deftest magit-gh-repo-dashboard/overview-agent-shell-queue-callable ()
  "overview agent-shell-queue command calls agent-shell-queue-buffer-open."
  (let ((called nil))
    (cl-letf (((symbol-function 'magit-gh-repo-overview--current-repo)
               (lambda () (magit-gh-repo--make :name "r" :path "/tmp/r")))
              ((symbol-function 'agent-shell-queue-buffer-open)
               (lambda () (interactive) (setq called t))))
      (magit-gh-repo-overview-agent-shell-queue))
    (should called)))

;;;; magit-gh-repo-dashboard--parse-worktrees

(defconst magit-gh-repo-dashboard-test--worktree-output
  '("worktree /tmp/main"
    "HEAD abc123def456"
    "branch refs/heads/main"
    ""
    "worktree /tmp/wt1"
    "HEAD 111111aaaaaa"
    "branch refs/heads/feature-1"
    ""
    "worktree /tmp/wt2"
    "HEAD 222222bbbbbb"
    "detached"
    "")
  "Sample `git worktree list --porcelain' output as a list of lines.")

(ert-deftest magit-gh-repo-dashboard/parse-worktrees-skips-main ()
  "parse-worktrees omits the main worktree (first block)."
  (let ((result (magit-gh-repo-dashboard--parse-worktrees
                 "/tmp/main"
                 magit-gh-repo-dashboard-test--worktree-output)))
    (should (= 2 (length result)))
    (should-not (seq-find (lambda (r) (equal "/tmp/main" (magit-gh-repo-path r)))
                          result))))

(ert-deftest magit-gh-repo-dashboard/parse-worktrees-paths ()
  "parse-worktrees sets correct paths on returned structs."
  (let ((result (magit-gh-repo-dashboard--parse-worktrees
                 "/tmp/main"
                 magit-gh-repo-dashboard-test--worktree-output)))
    (should (equal "/tmp/wt1" (magit-gh-repo-path (nth 0 result))))
    (should (equal "/tmp/wt2" (magit-gh-repo-path (nth 1 result))))))

(ert-deftest magit-gh-repo-dashboard/parse-worktrees-names ()
  "parse-worktrees constructs names from main-repo basename and branch."
  (let ((result (magit-gh-repo-dashboard--parse-worktrees
                 "/tmp/main"
                 magit-gh-repo-dashboard-test--worktree-output)))
    (should (equal "main@feature-1" (magit-gh-repo-name (nth 0 result))))
    (should (equal "main@detached" (magit-gh-repo-name (nth 1 result))))))

(ert-deftest magit-gh-repo-dashboard/parse-worktrees-worktree-flag ()
  "parse-worktrees sets :worktree t on all returned structs."
  (let ((result (magit-gh-repo-dashboard--parse-worktrees
                 "/tmp/main"
                 magit-gh-repo-dashboard-test--worktree-output)))
    (should (seq-every-p #'magit-gh-repo-worktree result))))

(ert-deftest magit-gh-repo-dashboard/parse-worktrees-empty-output ()
  "parse-worktrees returns nil when only the main worktree is listed."
  (let ((result (magit-gh-repo-dashboard--parse-worktrees
                 "/tmp/main"
                 '("worktree /tmp/main" "HEAD abc123" "branch refs/heads/main" ""))))
    (should (null result))))

;;;; magit-gh-repo-dashboard--sorted-repos with worktrees

(ert-deftest magit-gh-repo-dashboard/sorted-repos-appends-worktrees ()
  "sorted-repos places discovered worktrees immediately after their parent."
  (let* ((main (magit-gh-repo--make :name "main" :path "/tmp/main"))
         (wt (magit-gh-repo--make :name "main@feat" :path "/tmp/wt" :worktree t))
         (magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/main" :worktrees (list wt))
    (let ((result (magit-gh-repo-dashboard--sorted-repos (list main))))
      (should (= 2 (length result)))
      (should (equal "main" (magit-gh-repo-name (nth 0 result))))
      (should (equal "main@feat" (magit-gh-repo-name (nth 1 result)))))))

;;;; Column configuration

(ert-deftest magit-gh-repo-dashboard/column-enabled-defaults ()
  "All columns are enabled by default."
  (let ((magit-gh-repo-dashboard-columns
         '((name . t) (branch . t) (fetched . t) (behind . t) (changes . t) (worktree . t))))
    (should (seq-every-p #'magit-gh-repo-dashboard--column-enabled-p
                         magit-gh-repo-dashboard--all-columns))))

(ert-deftest magit-gh-repo-dashboard/column-disabled ()
  "A disabled column is excluded from active-columns."
  (let ((magit-gh-repo-dashboard-columns
         '((name . t) (branch . nil) (fetched . t) (behind . t) (changes . t) (worktree . t))))
    (should-not (magit-gh-repo-dashboard--column-enabled-p 'branch))
    (should-not (member 'branch (magit-gh-repo-dashboard--active-columns)))))

(ert-deftest magit-gh-repo-dashboard/build-format-omits-disabled-columns ()
  "build-format produces a vector that excludes disabled columns."
  (let ((magit-gh-repo-dashboard-columns
         '((name . t) (branch . nil) (fetched . t) (behind . t) (changes . t) (worktree . nil)))
        (repos (list (magit-gh-repo--make :name "r" :path "/tmp/r"))))
    (let ((fmt (magit-gh-repo-dashboard--build-format repos)))
      (should (= 4 (length fmt)))
      (should-not (seq-find (lambda (col) (equal "Branch" (car col))) (append fmt nil)))
      (should-not (seq-find (lambda (col) (equal "WT" (car col))) (append fmt nil))))))

(ert-deftest magit-gh-repo-dashboard/build-entry-matches-format ()
  "build-entry vector length matches active-column count."
  (let ((magit-gh-repo-dashboard-columns
         '((name . t) (branch . t) (fetched . nil) (behind . nil) (changes . t) (worktree . nil)))
        (magit-gh-repo-dashboard--worktree-map (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--get-stats)
               (lambda (_)
                 (list :branch "main" :behind 0 :dirty nil :fetch-age 60.0
                       :head-hash "abc" :recent-log "" :remote-origin nil
                       :uncommitted-files nil))))
      (let* ((repo (magit-gh-repo--make :name "r" :path "/tmp/r"))
             (entry (magit-gh-repo-dashboard--build-entry repo))
             (active (magit-gh-repo-dashboard--active-columns)))
        (should (= (length active) (length (cadr entry))))))))

;;;; New command user-error conditions

(ert-deftest magit-gh-repo-dashboard/visit-buffer-user-error-when-none ()
  "visit-buffer signals user-error when no buffers visit the repo."
  (let ((magit-gh-repo-list nil))
    (cl-letf (((symbol-function 'magit-gh-repo-dashboard--repo-at-point)
               (lambda () (magit-gh-repo--make :name "r" :path "/nonexistent/path")))
              ((symbol-function 'buffer-list) (lambda () nil)))
      (should-error (magit-gh-repo-dashboard-visit-buffer) :type 'user-error))))

(ert-deftest magit-gh-repo-dashboard/worktree-delete-user-error-when-not-worktree ()
  "worktree-delete signals user-error when the entry is not a worktree."
  (cl-letf (((symbol-function 'magit-gh-repo-dashboard--repo-at-point)
             (lambda () (magit-gh-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-gh-repo-dashboard-worktree-delete) :type 'user-error)))

(ert-deftest magit-gh-repo-dashboard/worktree-add-user-error-when-at-worktree ()
  "worktree-add signals user-error when the entry is itself a worktree."
  (cl-letf (((symbol-function 'magit-gh-repo-dashboard--repo-at-point)
             (lambda () (magit-gh-repo--make :name "r@feat" :path "/tmp/wt" :worktree t))))
    (should-error (magit-gh-repo-dashboard-worktree-add) :type 'user-error)))

(ert-deftest magit-gh-repo-dashboard/overview-worktree-add-user-error-for-worktree ()
  "overview-worktree-add signals user-error when the overview IS a worktree."
  (let ((repo (magit-gh-repo--make :name "r@f" :path "/tmp/wt" :worktree t)))
    (cl-letf (((symbol-function 'magit-gh-repo-overview--current-repo) (lambda () repo)))
      (should-error (magit-gh-repo-overview-worktree-add) :type 'user-error))))

(ert-deftest magit-gh-repo-dashboard/overview-worktree-delete-user-error-for-main ()
  "overview-worktree-delete signals user-error when the overview is NOT a worktree."
  (let ((repo (magit-gh-repo--make :name "r" :path "/tmp/r")))
    (cl-letf (((symbol-function 'magit-gh-repo-overview--current-repo) (lambda () repo)))
      (should-error (magit-gh-repo-overview-worktree-delete) :type 'user-error))))

;;;; magit-gh-repo-overview--worktrees-for (lazy discovery)

(ert-deftest magit-gh-repo-dashboard/overview-worktrees-for-uses-cache ()
  "worktrees-for returns cached value without running git."
  (let* ((wt (magit-gh-repo--make :name "r@feat" :path "/tmp/wt" :worktree t))
         (magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/main" :worktrees (list wt))
    (let ((result (magit-gh-repo-overview--worktrees-for "/tmp/main")))
      (should (= 1 (length result)))
      (should (equal "r@feat" (magit-gh-repo-name (car result)))))))

(ert-deftest magit-gh-repo-dashboard/overview-worktrees-for-caches-nil ()
  "worktrees-for caches nil when git finds no worktrees, avoiding re-runs."
  (let* ((magit-gh--cache (make-hash-table :test #'equal))
         (call-count 0))
    (cl-letf (((symbol-function 'process-lines)
               (lambda (&rest _) (setq call-count (1+ call-count)) nil)))
      (magit-gh-repo-overview--worktrees-for "/tmp/main")
      (magit-gh-repo-overview--worktrees-for "/tmp/main"))
    (should (= 1 call-count))))

(provide 'test-magit-gh-repo-dashboard)

;;; test-magit-gh-repo-dashboard.el ends here
