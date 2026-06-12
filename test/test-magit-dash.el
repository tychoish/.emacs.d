;;; test-magit-dash.el --- ERT tests for magit-dash -*- lexical-binding: t -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-dash/")
;;
;; Batch run:
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-magit-dash.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash/")'

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'magit-dash)

;;;; magit-dash-register

(ert-deftest magit-dash/register-adds-to-list ()
  "register creates a struct and prepends it to `magit-dash-repo-list'."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "foo" :path "/tmp/foo")
    (should (= 1 (length magit-dash-repo-list)))
    (let ((r (car magit-dash-repo-list)))
      (should (magit-dash-repo-p r))
      (should (equal "foo" (magit-dash-repo-name r)))
      (should (equal "/tmp/foo" (magit-dash-repo-path r))))))

(ert-deftest magit-dash/register-expands-path ()
  "register expands the path with `expand-file-name'."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "bar" :path "~/bar")
    (should (string-prefix-p "/" (magit-dash-repo-path (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-replaces-existing ()
  "Registering the same name replaces the previous entry."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "dup" :path "/tmp/dup1")
    (magit-dash-register :name "dup" :path "/tmp/dup2")
    (should (= 1 (length magit-dash-repo-list)))
    (should (equal "/tmp/dup2" (magit-dash-repo-path (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-multiple ()
  "Multiple distinct names accumulate in the list."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "a" :path "/tmp/a")
    (magit-dash-register :name "b" :path "/tmp/b")
    (should (= 2 (length magit-dash-repo-list)))))

(ert-deftest magit-dash/register-defaults ()
  "Registering with only name+path yields correct defaults for new fields."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r")
    (let ((r (car magit-dash-repo-list)))
      (should (null (magit-dash-repo-include-prs r)))
      (should (null (magit-dash-repo-auto-sync r)))
      (should (null (magit-dash-repo-tags r)))
      (should (null (magit-dash-repo-auto-commit r)))
      (should (null (magit-dash-repo-commands r)))
      (should (null (magit-dash-repo-sort-hint r)))
      (should (null (magit-dash-repo-worktree r)))))

(ert-deftest magit-dash/register-worktree ()
  ":worktree t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :worktree t)
    (should (eq t (magit-dash-repo-worktree (car magit-dash-repo-list)))))))

(ert-deftest magit-dash/register-include-prs-true ()
  ":include-prs t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :include-prs t)
    (should (eq t (magit-dash-repo-include-prs (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-auto-fetch ()
  ":auto-fetch t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-fetch t)
    (should (magit-dash-repo-auto-fetch (car magit-dash-repo-list)))))

(ert-deftest magit-dash/register-auto-pull ()
  ":auto-pull t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-pull t)
    (should (magit-dash-repo-auto-pull (car magit-dash-repo-list)))))

(ert-deftest magit-dash/register-auto-push ()
  ":auto-push t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-push t)
    (should (magit-dash-repo-auto-push (car magit-dash-repo-list)))))

(ert-deftest magit-dash/auto-sync-steps-fetch-only ()
  ":auto-fetch produces a single fetch step."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-fetch t)
    (let ((steps (magit-dash--auto-sync-steps (car magit-dash-repo-list))))
      (should (= 1 (length steps)))
      (should (equal "fetch" (caar steps))))))

(ert-deftest magit-dash/auto-sync-steps-pull-implies-fetch ()
  ":auto-pull produces fetch then pull steps."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-pull t)
    (let ((steps (magit-dash--auto-sync-steps (car magit-dash-repo-list))))
      (should (= 2 (length steps)))
      (should (equal "fetch" (caar steps)))
      (should (equal "pull" (car (cadr steps)))))))

(ert-deftest magit-dash/register-tags ()
  ":tags list of symbols is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :tags '(work personal))
    (should (equal '(work personal) (magit-dash-repo-tags (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-auto-commit-bool ()
  ":auto-commit t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-commit t)
    (should (eq t (magit-dash-repo-auto-commit (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-auto-commit-function ()
  ":auto-commit function is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-commit #'ignore)
    (should (eq #'ignore (magit-dash-repo-auto-commit (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-commands ()
  ":commands alist is stored correctly."
  (let ((magit-dash-repo-list nil)
        (cmds '(("run tests" . my-test-fn) ("lint" . my-lint-fn))))
    (magit-dash-register :name "r" :path "/tmp/r" :commands cmds)
    (should (equal cmds (magit-dash-repo-commands (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-sort-hint ()
  ":sort-hint number is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :sort-hint 10)
    (should (= 10 (magit-dash-repo-sort-hint (car magit-dash-repo-list))))))

;;;; magit-dash--sorted-repos

(ert-deftest magit-dash/sorted-repos-by-hint ()
  "Repos with sort-hints are ordered numerically."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1" :sort-hint 20))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2" :sort-hint 5))
         (r3 (magit-dash-repo--make :name "r3" :path "/tmp/r3" :sort-hint 10))
         (sorted (magit-dash--sorted-repos (list r1 r2 r3))))
    (should (equal "r2" (magit-dash-repo-name (nth 0 sorted))))
    (should (equal "r3" (magit-dash-repo-name (nth 1 sorted))))
    (should (equal "r1" (magit-dash-repo-name (nth 2 sorted))))))

(ert-deftest magit-dash/sorted-repos-hinted-before-unhinted ()
  "Repos with sort-hints appear before those without."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2" :sort-hint 1))
         (sorted (magit-dash--sorted-repos (list r1 r2))))
    (should (equal "r2" (magit-dash-repo-name (nth 0 sorted))))
    (should (equal "r1" (magit-dash-repo-name (nth 1 sorted))))))

(ert-deftest magit-dash/sorted-repos-unhinted-preserve-order ()
  "Repos without sort-hints preserve their relative order."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2"))
         (sorted (magit-dash--sorted-repos (list r1 r2))))
    (should (equal "r1" (magit-dash-repo-name (nth 0 sorted))))
    (should (equal "r2" (magit-dash-repo-name (nth 1 sorted))))))

;;;; magit-dash--format-age

(ert-deftest magit-dash/format-age-nil ()
  (should (equal "never" (magit-dash--format-age nil))))

(ert-deftest magit-dash/format-age-seconds ()
  (should (equal "30s" (magit-dash--format-age 30.0))))

(ert-deftest magit-dash/format-age-minutes ()
  (should (equal "5m" (magit-dash--format-age 300.0))))

(ert-deftest magit-dash/format-age-hours ()
  (should (equal "2h" (magit-dash--format-age 7200.0))))

(ert-deftest magit-dash/format-age-days ()
  (should (equal "3d" (magit-dash--format-age (* 3 86400.0)))))

(ert-deftest magit-dash/format-age-boundary-59s ()
  "59 seconds formats as seconds."
  (should (equal "59s" (magit-dash--format-age 59.0))))

(ert-deftest magit-dash/format-age-boundary-60s ()
  "60 seconds formats as 1 minute."
  (should (equal "1m" (magit-dash--format-age 60.0))))

;;;; magit-dash--format-status

(ert-deftest magit-dash/format-status-all-clean ()
  "All-zero/nil returns empty string."
  (should (equal "" (magit-dash--format-status 0 0 nil))))

(ert-deftest magit-dash/format-status-dirty-only ()
  "Dirty with no divergence shows only \"!\"."
  (should (equal "!" (substring-no-properties
                      (magit-dash--format-status 0 0 t)))))

(ert-deftest magit-dash/format-status-ahead-only ()
  "Ahead count shows ↑N."
  (should (equal "↑3" (substring-no-properties
                       (magit-dash--format-status 3 0 nil)))))

(ert-deftest magit-dash/format-status-behind-only ()
  "Behind count shows ↓N."
  (should (equal "↓5" (substring-no-properties
                       (magit-dash--format-status 0 5 nil)))))

(ert-deftest magit-dash/format-status-all-set ()
  "All three indicators appear in order ↑ ↓ ! separated by spaces."
  (should (equal "↑2 ↓3 !" (substring-no-properties
                             (magit-dash--format-status 2 3 t)))))

;;;; magit-dash--head-hash

(ert-deftest magit-dash/head-hash-nonexistent-path ()
  "head-hash returns nil for a path with no .git/HEAD."
  (should (null (magit-dash--head-hash "/tmp/nonexistent-no-git-here"))))

;;;; magit-dash--collect-stats (via mock)

(ert-deftest magit-dash/collect-stats-extracts-fields ()
  "collect-stats populates all stat fields using magit-git functions."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test")))
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
              ((symbol-function 'magit-dash--fetch-age)
               (lambda (_) 3600.0))
              ((symbol-function 'magit-dash--head-hash)
               (lambda (_) "abc123def456")))
      (let ((magit-dash--stats-cache (make-hash-table :test #'equal)))
        (let ((stats (magit-dash--collect-stats repo)))
          (should (equal "main" (plist-get stats :branch)))
          (should (equal "git@github.com:user/test.git" (plist-get stats :remote-origin)))
          (should (= 2 (plist-get stats :behind)))
          (should (eq t (plist-get stats :dirty)))
          (should (equal '(" M foo.el") (plist-get stats :uncommitted-files)))
          (should (= 3600.0 (plist-get stats :fetch-age)))
          (should (equal "abc123def456" (plist-get stats :head-hash)))
          (should (string-match-p "fix foo" (plist-get stats :recent-log))))))))

(ert-deftest magit-dash/collect-stats-clean-workdir ()
  "collect-stats sets :dirty nil and :uncommitted-files nil when porcelain is empty."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test")))
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
              ((symbol-function 'magit-dash--fetch-age)
               (lambda (_) nil))
              ((symbol-function 'magit-dash--head-hash)
               (lambda (_) "deadbeef")))
      (let ((magit-dash--stats-cache (make-hash-table :test #'equal)))
        (let ((stats (magit-dash--collect-stats repo)))
          (should (eq nil (plist-get stats :dirty)))
          (should (null (plist-get stats :uncommitted-files)))
          (should (null (plist-get stats :remote-origin)))
          (should (= 0 (plist-get stats :behind))))))))

;;;; magit-dash--get-stats (cache invalidation)

(ert-deftest magit-dash/get-stats-uses-cache-on-same-hash ()
  "get-stats returns cached stats when HEAD hash matches."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (cached (list :branch "main" :remote-origin nil :behind 0
                       :dirty nil :uncommitted-files nil
                       :fetch-age 60.0 :head-hash "abc123" :recent-log ""))
         (magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/test" :stats cached)
    (cl-letf (((symbol-function 'magit-dash--head-hash)
               (lambda (_) "abc123")))
      (should (equal cached (magit-dash--get-stats repo))))))

(ert-deftest magit-dash/get-stats-invalidates-on-new-hash ()
  "get-stats collects fresh stats when HEAD hash has changed."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (cached (list :branch "main" :remote-origin nil :behind 0
                       :dirty nil :uncommitted-files nil
                       :fetch-age 60.0 :head-hash "old123" :recent-log ""))
         (magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/test" :stats cached)
    (cl-letf (((symbol-function 'magit-dash--head-hash)
               (lambda (_) "new456"))
              ((symbol-function 'magit-dash--collect-stats)
               (lambda (_) (list :branch "feat" :remote-origin nil :behind 1
                                 :dirty t :uncommitted-files nil
                                 :fetch-age nil :head-hash "new456" :recent-log ""))))
      (let ((result (magit-dash--get-stats repo)))
        (should (equal "feat" (plist-get result :branch)))
        (should (= 1 (plist-get result :behind)))))))

;;;; magit-dash--auto-commit

(ert-deftest magit-dash/auto-commit-uses-default-message ()
  "With :auto-commit t, calls magit-call-git add then commit with the default message."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test" :auto-commit t))
        (git-calls nil))
    (cl-letf (((symbol-function 'magit-call-git)
               (lambda (&rest args)
                 (push args git-calls)
                 0)))
      (should (magit-dash--auto-commit repo)))
    ;; git-calls is (commit-args add-args) — commit was pushed last
    (should (= 2 (length git-calls)))
    (should (member "add" (cadr git-calls)))
    (let ((commit-args (car git-calls)))
      (should (equal "commit" (car commit-args)))
      (should (member (magit-dash--default-commit-message repo)
                      commit-args)))))

(ert-deftest magit-dash/auto-commit-calls-message-function ()
  "With :auto-commit as a function, calls it to produce the commit message."
  (let* ((custom-msg "custom: my message")
         (repo (magit-dash-repo--make :name "test" :path "/tmp/test"
                                    :auto-commit (lambda (_r) custom-msg)))
         (commit-msg nil))
    (cl-letf (((symbol-function 'magit-call-git)
               (lambda (&rest args)
                 (when (equal "commit" (car args))
                   (setq commit-msg (cadr (member "-m" args))))
                 0)))
      (magit-dash--auto-commit repo)
      (should (equal custom-msg commit-msg)))))

(ert-deftest magit-dash/commit-user-error-when-not-configured ()
  "commit signals user-error when :auto-commit is nil."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test")))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () repo)))
      (should-error (magit-dash-commit) :type 'user-error))))

;;;; magit-dash-sync-all

(ert-deftest magit-dash/sync-all-user-error-when-none-configured ()
  "sync-all signals user-error when no repos have :auto-sync."
  (let ((magit-dash-repo-list (list (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-dash-sync-all) :type 'user-error)))

(ert-deftest magit-dash/sync-all-skips-repos-without-auto-sync ()
  "sync-all calls interactively only for repos with :auto-sync set."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1" :auto-sync 'fetch))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2"))
         (magit-dash-repo-list (list r1 r2))
         (called-fns nil))
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn) (push fn called-fns)))
              ((symbol-function 'magit-dash-refresh)
               (lambda () nil)))
      (magit-dash-sync-all))
    (should (= 1 (length called-fns)))
    (should (memq #'magit-fetch called-fns))))

;;;; magit-dash--run-command-for

(ert-deftest magit-dash/run-command-user-error-when-no-commands ()
  "run-command-for signals user-error when repo has no commands."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test")))
    (should-error (magit-dash--run-command-for repo) :type 'user-error)))

;;;; magit-dash-filter-by-tag

(defmacro magit-dash-test--with-refresh-stubs (&rest body)
  "Run BODY with tabulated-list side-effecting functions stubbed out."
  `(cl-letf (((symbol-function 'tabulated-list-print) (lambda (&rest _) nil))
             ((symbol-function 'tabulated-list-init-header) (lambda () nil)))
     ,@body))

(ert-deftest magit-dash/filter-by-tag-reduces-entries ()
  "Refresh with a tag filter shows only matching repos."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1" :tags '(work)))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2" :tags '(personal)))
         (magit-dash-repo-list (list r1 r2))
         (built nil))
    (magit-dash-test--with-refresh-stubs
      (cl-letf (((symbol-function 'magit-dash--build-entry)
                 (lambda (r) (push (magit-dash-repo-name r) built) nil)))
        (with-temp-buffer
          (setq-local magit-dash--tag-filter 'work)
          (magit-dash-refresh))))
    (should (= 1 (length built)))
    (should (member "r1" built))
    (should-not (member "r2" built))))

(ert-deftest magit-dash/filter-nil-shows-all ()
  "Refresh with no tag filter shows all repos."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1" :tags '(work)))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2" :tags '(personal)))
         (magit-dash-repo-list (list r1 r2))
         (built nil))
    (magit-dash-test--with-refresh-stubs
      (cl-letf (((symbol-function 'magit-dash--build-entry)
                 (lambda (r) (push (magit-dash-repo-name r) built) nil)))
        (with-temp-buffer
          (setq-local magit-dash--tag-filter nil)
          (magit-dash-refresh))))
    (should (= 2 (length built)))))

;;;; magit-dash--build-format

(ert-deftest magit-dash/build-format-elastic-width ()
  "Name column width equals the longest repo name in the list."
  (let* ((r1 (magit-dash-repo--make :name "short" :path "/tmp/r1"))
         (r2 (magit-dash-repo--make :name "a-much-longer-name" :path "/tmp/r2"))
         (fmt (magit-dash--build-format (list r1 r2))))
    (should (= (length "a-much-longer-name") (cadr (aref fmt 0))))))

(ert-deftest magit-dash/build-format-minimum-width ()
  "Name column is at least as wide as the header label \"Name\"."
  (let* ((r (magit-dash-repo--make :name "x" :path "/tmp/r"))
         (fmt (magit-dash--build-format (list r))))
    (should (>= (cadr (aref fmt 0)) (length "Name")))))

(ert-deftest magit-dash/build-format-empty-list ()
  "Empty repo list yields minimum (header label) width."
  (let ((fmt (magit-dash--build-format nil)))
    (should (= (length "Name") (cadr (aref fmt 0))))))

;;;; magit-dash--build-entry

(ert-deftest magit-dash/build-entry-structure ()
  "build-entry returns (REPO VECTOR) with columns matching active column count."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (magit-dash--stats-cache (make-hash-table :test #'equal))
        (magit-dash-columns
         '((name . t) (branch . t) (fetched . t) (status . t) (worktree . t)))
        (magit-dash--worktree-map (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--get-stats)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil :fetch-age 120.0
                       :head-hash "abc" :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (id (car entry))
             (vec (cadr entry)))
        (should (magit-dash-repo-p id))
        (should (= 5 (length vec)))
        (should (string-match-p "myrep" (aref vec 0)))
        (should (string-match-p "main" (aref vec 1)))
        (should (equal "2m" (aref vec 2)))
        (should (equal "" (aref vec 3)))))))

;;;; magit-gh-pr-dashboard--build-args

(ert-deftest magit-dash/pr-build-args-default ()
  "Default filters produce a gh search prs command."
  (let ((filters (list :state "open" :author "@me" :repo nil :org nil)))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should (equal "search" (nth 0 args)))
      (should (equal "prs" (nth 1 args)))
      (should (member "--state" args))
      (should (member "open" args))
      (should (member "--author" args))
      (should (member "@me" args)))))

(ert-deftest magit-dash/pr-build-args-with-repo ()
  "When :repo is set, uses gh pr list -R REPO."
  (let ((filters (list :state "open" :author nil :repo "owner/myrepo" :org nil)))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should (equal "pr" (nth 0 args)))
      (should (equal "list" (nth 1 args)))
      (should (member "-R" args))
      (should (member "owner/myrepo" args)))))

(ert-deftest magit-dash/pr-build-args-with-org ()
  "When :org is set (no :repo), adds --owner to the search command."
  (let ((filters (list :state "open" :author "@me" :repo nil :org "myorg")))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should (equal "search" (nth 0 args)))
      (should (member "--owner" args))
      (should (member "myorg" args)))))

(ert-deftest magit-dash/pr-build-args-state-closed ()
  "State \"closed\" is passed through for both search and per-repo modes."
  (let ((filters (list :state "closed" :author nil :repo nil :org nil)))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should (member "closed" args)))))

(ert-deftest magit-dash/pr-build-args-state-unknown ()
  "Unknown state falls back to \"open\" in search mode."
  (let ((filters (list :state "merged" :author nil :repo nil :org nil)))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should (equal "search" (nth 0 args)))
      (should (member "open" args))
      (should-not (member "merged" args)))))

(ert-deftest magit-dash/pr-build-args-no-author ()
  "Nil :author omits --author from the command."
  (let ((filters (list :state "open" :author nil :repo nil :org nil)))
    (let ((args (magit-gh-pr-dashboard--build-args filters)))
      (should-not (member "--author" args)))))

;;;; magit-gh-pr-dashboard--format-ci

(ert-deftest magit-dash/format-ci-success ()
  (should (equal "pass" (substring-no-properties
                         (magit-gh-pr-dashboard--format-ci "SUCCESS")))))

(ert-deftest magit-dash/format-ci-failure ()
  (should (equal "fail" (substring-no-properties
                         (magit-gh-pr-dashboard--format-ci "FAILURE")))))

(ert-deftest magit-dash/format-ci-pending ()
  (should (equal "pending" (substring-no-properties
                            (magit-gh-pr-dashboard--format-ci "PENDING")))))

(ert-deftest magit-dash/format-ci-nil ()
  "Nil or unknown CI state produces a dash."
  (should (equal "—" (magit-gh-pr-dashboard--format-ci nil)))
  (should (equal "—" (magit-gh-pr-dashboard--format-ci "UNKNOWN"))))

;;;; magit-gh-pr-dashboard--format-review

(ert-deftest magit-dash/format-review-approved ()
  (should (equal "approved" (substring-no-properties
                             (magit-gh-pr-dashboard--format-review "APPROVED")))))

(ert-deftest magit-dash/format-review-changes ()
  (should (equal "changes req" (substring-no-properties
                                (magit-gh-pr-dashboard--format-review "CHANGES_REQUESTED")))))

(ert-deftest magit-dash/format-review-needed ()
  (should (equal "needed" (substring-no-properties
                           (magit-gh-pr-dashboard--format-review "REVIEW_REQUIRED")))))

(ert-deftest magit-dash/format-review-nil ()
  "Nil review decision returns empty string."
  (should (equal "" (magit-gh-pr-dashboard--format-review nil))))

;;;; magit-gh-pr-dashboard--comments-count

(ert-deftest magit-dash/comments-count-integer ()
  "When comments field is already an integer, return it directly."
  (let ((pr '((comments . 7))))
    (should (= 7 (magit-gh-pr-dashboard--comments-count pr)))))

(ert-deftest magit-dash/comments-count-alist ()
  "When comments is an alist with totalCount, extract it."
  (let ((pr `((comments . ((totalCount . 3))))))
    (should (= 3 (magit-gh-pr-dashboard--comments-count pr)))))

(ert-deftest magit-dash/comments-count-nil ()
  "Nil comments field returns 0."
  (let ((pr '((title . "test"))))
    (should (= 0 (magit-gh-pr-dashboard--comments-count pr)))))

;;;; magit-gh-pr-dashboard--parse-output

(defun magit-dash-test--make-pr-json (&rest overrides)
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

(ert-deftest magit-dash/parse-output-empty-array ()
  "Empty JSON array produces nil."
  (should (null (magit-gh-pr-dashboard--parse-output
                 "[]"
                 (list :repo "owner/myrepo")))))

(ert-deftest magit-dash/parse-output-non-json ()
  "Non-JSON output (e.g., error message) produces nil."
  (should (null (magit-gh-pr-dashboard--parse-output
                 "error: not found"
                 (list :state "open")))))

(ert-deftest magit-dash/parse-output-per-repo-mode ()
  "Per-repo mode uses :repo filter value as the repo-name."
  (let* ((pr (magit-dash-test--make-pr-json))
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

(ert-deftest magit-dash/parse-output-search-mode ()
  "Search mode extracts repo name from the `repository' field."
  (let* ((pr (append (magit-dash-test--make-pr-json)
                     (list (cons 'repository '((nameWithOwner . "org/other"))))))
         (json (concat "[" (json-serialize pr) "]"))
         (filters (list :state "open" :author "@me"))
         (entries (magit-gh-pr-dashboard--parse-output json filters)))
    (should (= 1 (length entries)))
    (let* ((entry (car entries))
           (id (car entry)))
      (should (equal "org/other" (plist-get id :repo))))))

(ert-deftest magit-dash/parse-output-multiple-prs ()
  "Multiple PRs produce multiple entries."
  (let* ((pr1 (magit-dash-test--make-pr-json))
         (pr2 (append (magit-dash-test--make-pr-json) (list (cons 'number 99))))
         (json (concat "[" (json-serialize pr1) "," (json-serialize pr2) "]"))
         (filters (list :repo "owner/repo"))
         (entries (magit-gh-pr-dashboard--parse-output json filters)))
    (should (= 2 (length entries)))))

;;;; magit-gh-pr-dashboard--build-entry (column structure)

(ert-deftest magit-dash/build-entry-columns ()
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

(ert-deftest magit-dash/build-entry-truncates-long-title ()
  "Titles longer than 36 chars are truncated with an ellipsis."
  (let* ((pr `((number . 1)
               (title . ,(make-string 50 ?X))
               (updatedAt . "")
               (comments . 0)))
         (entry (magit-gh-pr-dashboard--build-entry pr "r"))
         (vec (cadr entry)))
    (should (<= (length (aref vec 2)) 37))))

;;;; magit-dash-overview--insert-kv

(ert-deftest magit-dash/insert-kv-format ()
  "insert-kv writes a bold padded key and value on one line."
  (with-temp-buffer
    (magit-dash-overview--insert-kv "Branch" "main")
    (let ((text (buffer-string)))
      (should (string-match-p "Branch" text))
      (should (string-match-p "main" text))
      (should (string-match-p "\n" text)))))

(ert-deftest magit-dash/insert-kv-key-bold ()
  "insert-kv applies bold face to the key portion."
  (with-temp-buffer
    (magit-dash-overview--insert-kv "Repository" "myrepo")
    (let ((face (get-text-property 0 'face (buffer-string))))
      (should (equal 'bold face)))))

(ert-deftest magit-dash/insert-kv-value-face ()
  "insert-kv applies VALUE-FACE to the value when supplied."
  (with-temp-buffer
    (magit-dash-overview--insert-kv "Behind" "3 commits" 'warning)
    (let* ((text (buffer-string))
           (value-start (string-match "3 commits" text)))
      (should value-start)
      (should (equal 'warning (get-text-property value-start 'face text))))))

(ert-deftest magit-dash/insert-kv-action-property ()
  "insert-kv tags the line with `magit-dash-overview-action' when action given."
  (with-temp-buffer
    (magit-dash-overview--insert-kv "Repository" "r" nil (cons 'magit-status "/tmp/r"))
    (goto-char (point-min))
    (let ((action (get-text-property (point) 'magit-dash-overview-action)))
      (should (equal 'magit-status (car action)))
      (should (equal "/tmp/r" (cdr action))))))

(ert-deftest magit-dash/insert-kv-no-action-when-nil ()
  "insert-kv does not set action property when ACTION is nil."
  (with-temp-buffer
    (magit-dash-overview--insert-kv "Branch" "main")
    (goto-char (point-min))
    (should (null (get-text-property (point) 'magit-dash-overview-action)))))

;;;; magit-dash-overview--render

(defun magit-dash-test--make-stats (&rest overrides)
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

(ert-deftest magit-dash/render-loading-state ()
  "render with nil stats inserts a loading message."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep")))
    (with-temp-buffer
      (magit-dash-overview--render repo nil nil)
      (should (string-match-p "[Ll]oading" (buffer-string))))))

(ert-deftest magit-dash/render-kv-fields ()
  "render with stats inserts Repository, Path, Remote, Branch as KV pairs."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-dash-test--make-stats)))
    (with-temp-buffer
      (magit-dash-overview--render repo stats nil)
      (let ((text (buffer-string)))
        (should (string-match-p "Repository" text))
        (should (string-match-p "myrep" text))
        (should (string-match-p "Path" text))
        (should (string-match-p "/tmp/myrep" text))
        (should (string-match-p "Remote" text))
        (should (string-match-p "git@github.com" text))
        (should (string-match-p "Branch" text))
        (should (string-match-p "main" text))))))

(ert-deftest magit-dash/render-pr-loading ()
  "render with nil pr-counts shows loading placeholder for PRs."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-dash-test--make-stats)))
    (with-temp-buffer
      (magit-dash-overview--render repo stats nil)
      (should (string-match-p "loading" (buffer-string))))))

(ert-deftest magit-dash/render-pr-counts ()
  "render with pr-counts shows total and mine."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-dash-test--make-stats)))
    (with-temp-buffer
      (magit-dash-overview--render repo stats (cons 5 2))
      (let ((text (buffer-string)))
        (should (string-match-p "5" text))
        (should (string-match-p "2" text))))))

(ert-deftest magit-dash/render-dirty-shows-files ()
  "render with dirty stats lists uncommitted files."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-dash-test--make-stats
                :dirty t
                :uncommitted-files '(" M foo.el" "?? bar.el"))))
    (with-temp-buffer
      (magit-dash-overview--render repo stats nil)
      (let ((text (buffer-string)))
        (should (string-match-p "foo.el" text))
        (should (string-match-p "bar.el" text))))))

(ert-deftest magit-dash/render-recent-commits ()
  "render inserts Recent Commits section when recent-log is non-empty."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-dash-test--make-stats)))
    (with-temp-buffer
      (magit-dash-overview--render repo stats nil)
      (let ((text (buffer-string)))
        (should (string-match-p "Recent Commits" text))
        (should (string-match-p "fix foo" text))))))

(ert-deftest magit-dash/render-no-remote-origin ()
  "render omits Remote row when :remote-origin is nil."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-dash-test--make-stats :remote-origin nil)))
    (with-temp-buffer
      (magit-dash-overview--render repo stats nil)
      (should-not (string-match-p "Remote" (buffer-string))))))

;;;; magit-dash-overview--classify-files

(ert-deftest magit-dash/classify-files-staged ()
  "Staged modifications appear under 'staged."
  (let* ((lines '("M  foo.el" "A  bar.el"))
         (result (magit-dash-overview--classify-files lines)))
    (should (member "foo.el" (alist-get 'staged result)))
    (should (member "bar.el" (alist-get 'staged result)))))

(ert-deftest magit-dash/classify-files-unstaged ()
  "Worktree-only modifications appear under 'unstaged."
  (let* ((lines '(" M foo.el"))
         (result (magit-dash-overview--classify-files lines)))
    (should (member "foo.el" (alist-get 'unstaged result)))
    (should (null (alist-get 'staged result)))))

(ert-deftest magit-dash/classify-files-untracked ()
  "?? lines appear under 'untracked."
  (let* ((lines '("?? new.el"))
         (result (magit-dash-overview--classify-files lines)))
    (should (member "new.el" (alist-get 'untracked result)))
    (should (null (alist-get 'staged result)))))

(ert-deftest magit-dash/classify-files-deleted-staged ()
  "Staged deletions appear under 'deleted, not 'staged."
  (let* ((lines '("D  gone.el"))
         (result (magit-dash-overview--classify-files lines)))
    (should (member "gone.el" (alist-get 'deleted result)))
    (should (null (alist-get 'staged result)))))

(ert-deftest magit-dash/classify-files-deleted-unstaged ()
  "Unstaged deletions appear under 'deleted, not 'unstaged."
  (let* ((lines '(" D gone.el"))
         (result (magit-dash-overview--classify-files lines)))
    (should (member "gone.el" (alist-get 'deleted result)))
    (should (null (alist-get 'unstaged result)))))

(ert-deftest magit-dash/classify-files-trims-filename ()
  "Filenames have leading/trailing whitespace stripped."
  (let* ((lines '("M  foo.el "))
         (result (magit-dash-overview--classify-files lines)))
    (should (member "foo.el" (alist-get 'staged result)))))

(ert-deftest magit-dash/classify-files-empty-input ()
  "Empty input yields all-nil categories."
  (let ((result (magit-dash-overview--classify-files nil)))
    (should (null (alist-get 'staged result)))
    (should (null (alist-get 'unstaged result)))
    (should (null (alist-get 'deleted result)))
    (should (null (alist-get 'untracked result)))))

;;;; Render: uncommitted file sections

(ert-deftest magit-dash/render-dirty-sections ()
  "render shows Staged/Unstaged/Untracked section headers when present."
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
        (stats (magit-dash-test--make-stats
                :dirty t
                :uncommitted-files '("M  foo.el" " M bar.el" "?? baz.el"))))
    (with-temp-buffer
      (magit-dash-overview--render repo stats nil)
      (let ((text (buffer-string)))
        (should (string-match-p "Staged" text))
        (should (string-match-p "foo.el" text))
        (should (string-match-p "Unstaged" text))
        (should (string-match-p "bar.el" text))
        (should (string-match-p "Untracked" text))
        (should (string-match-p "baz.el" text))))))

;;;; Render: PR counts

(ert-deftest magit-dash/render-pr-counts-zero ()
  "render shows None when PR count is zero."
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
        (stats (magit-dash-test--make-stats)))
    (with-temp-buffer
      (magit-dash-overview--render repo stats (cons 0 0))
      (should (string-match-p "None" (buffer-string))))))

(ert-deftest magit-dash/render-pr-counts-nonzero ()
  "render shows Open count and Yours when nonzero."
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
        (stats (magit-dash-test--make-stats)))
    (with-temp-buffer
      (magit-dash-overview--render repo stats (cons 5 2))
      (let ((text (buffer-string)))
        (should (string-match-p "Open" text))
        (should (string-match-p "5" text))
        (should (string-match-p "Yours" text))
        (should (string-match-p "2" text))))))

(ert-deftest magit-dash/render-pr-counts-yours-zero ()
  "render omits Yours line when mine=0."
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
        (stats (magit-dash-test--make-stats)))
    (with-temp-buffer
      (magit-dash-overview--render repo stats (cons 3 0))
      (let ((text (buffer-string)))
        (should (string-match-p "Open" text))
        (should-not (string-match-p "Yours" text))))))

;;;; Transient predicates

(ert-deftest magit-dash/dirty-or-unknown-p-when-no-stats ()
  "dirty-or-unknown-p returns t when stats not yet cached."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () (magit-dash-repo--make :name "r" :path "/tmp/r"))))
      (should (magit-dash--dirty-or-unknown-p)))))

(ert-deftest magit-dash/dirty-or-unknown-p-when-clean ()
  "dirty-or-unknown-p returns nil when stats are cached and clean."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/r" :stats (list :dirty nil :branch "main" :behind 0))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () (magit-dash-repo--make :name "r" :path "/tmp/r"))))
      (should-not (magit-dash--dirty-or-unknown-p)))))

(ert-deftest magit-dash/has-commands-p-when-registered ()
  "has-commands-p returns t when commands are registered."
  (cl-letf (((symbol-function 'magit-dash--repo-at-point)
             (lambda ()
               (magit-dash-repo--make :name "r" :path "/tmp/r"
                                    :commands '(("run" . my-fn))))))
    (should (magit-dash--has-commands-p))))

(ert-deftest magit-dash/has-commands-p-when-none ()
  "has-commands-p returns nil when no commands are registered."
  (cl-letf (((symbol-function 'magit-dash--repo-at-point)
             (lambda () (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (should-not (magit-dash--has-commands-p))))

(ert-deftest magit-dash/overview-has-changes-p-when-dirty ()
  "overview-has-changes-p returns t when stats show dirty."
  (with-temp-buffer
    (setq-local magit-dash-overview--stats (list :dirty t :branch "main"))
    (should (magit-dash-overview--has-changes-p))))

(ert-deftest magit-dash/overview-has-changes-p-when-clean ()
  "overview-has-changes-p returns nil when stats show clean."
  (with-temp-buffer
    (setq-local magit-dash-overview--stats (list :dirty nil :branch "main"))
    (should-not (magit-dash-overview--has-changes-p))))

;;;; magit-dash-overview--render action properties

(ert-deftest magit-dash/render-repository-line-action ()
  "render tags the Repository line with a magit-status action."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-dash-test--make-stats)))
    (with-temp-buffer
      (magit-dash-overview--render repo stats nil)
      (goto-char (point-min))
      (let ((action (get-text-property (point) 'magit-dash-overview-action)))
        (should (equal 'magit-status (car action)))
        (should (equal "/tmp/myrep" (cdr action)))))))

(ert-deftest magit-dash/render-path-line-action ()
  "render tags the Path line with a dired action."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-dash-test--make-stats)))
    (with-temp-buffer
      (magit-dash-overview--render repo stats nil)
      (goto-char (point-min))
      (search-forward "Path")
      (let ((action (get-text-property (point) 'magit-dash-overview-action)))
        (should (equal 'dired (car action)))
        (should (equal "/tmp/myrep" (cdr action)))))))

(ert-deftest magit-dash/render-commit-line-action ()
  "render tags Recent Commits lines with magit-show-commit actions."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (stats (magit-dash-test--make-stats
                :recent-log "abc1234 fix something")))
    (with-temp-buffer
      (magit-dash-overview--render repo stats nil)
      (goto-char (point-min))
      (search-forward "abc1234")
      (let ((action (get-text-property (point) 'magit-dash-overview-action)))
        (should (equal 'magit-show-commit (car action)))
        (should (equal "abc1234" (cdr action)))))))

;;;; magit-dash-overview-follow

(ert-deftest magit-dash/follow-magit-status ()
  "follow opens magit status when action is 'magit-status."
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
        (visited-path nil))
    (cl-letf (((symbol-function 'magit-status-setup-buffer)
               (lambda (path) (setq visited-path path))))
      (with-temp-buffer
        (setq-local magit-dash-overview--repo repo)
        (let ((inhibit-read-only t))
          (magit-dash-overview--insert-kv
           "Repository" "r" nil (cons 'magit-status "/tmp/r")))
        (goto-char (point-min))
        (magit-dash-overview-follow)))
    (should (equal "/tmp/r" visited-path))))

(ert-deftest magit-dash/follow-dired ()
  "follow opens dired when action is 'dired."
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
        (visited-path nil))
    (cl-letf (((symbol-function 'dired)
               (lambda (path) (setq visited-path path))))
      (with-temp-buffer
        (setq-local magit-dash-overview--repo repo)
        (let ((inhibit-read-only t))
          (magit-dash-overview--insert-kv
           "Path" "/tmp/r" nil (cons 'dired "/tmp/r")))
        (goto-char (point-min))
        (magit-dash-overview-follow)))
    (should (equal "/tmp/r" visited-path))))

(ert-deftest magit-dash/follow-noop-on-untagged-line ()
  "follow does nothing on lines without an action property."
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r")))
    (with-temp-buffer
      (setq-local magit-dash-overview--repo repo)
      (let ((inhibit-read-only t))
        (insert "no action here\n"))
      (goto-char (point-min))
      (should-not (get-text-property (point) 'magit-dash-overview-action))
      (magit-dash-overview-follow)))) ; should not error

;;;; magit-dash--batch-run

(ert-deftest magit-dash/batch-run-collects-ok ()
  "batch-run calls on-all-done with all results when each op returns 'ok."
  (let* ((repos (list (magit-dash-repo--make :name "r1" :path "/tmp/r1")
                      (magit-dash-repo--make :name "r2" :path "/tmp/r2")))
         (all-results nil))
    (magit-dash--batch-run
     repos
     (lambda (repo cb) (funcall cb 'ok))
     "test"
     (lambda (results) (setq all-results results)))
    (should (= 2 (length all-results)))
    (should (seq-every-p (lambda (r) (eq 'ok (cdr r))) all-results))))

(ert-deftest magit-dash/batch-run-mixed-statuses ()
  "batch-run handles a mix of 'ok, 'skipped, and 'error results."
  (let* ((statuses '(ok skipped error))
         (repos (list (magit-dash-repo--make :name "r1" :path "/tmp/r1")
                      (magit-dash-repo--make :name "r2" :path "/tmp/r2")
                      (magit-dash-repo--make :name "r3" :path "/tmp/r3")))
         (all-results nil)
         (idx 0))
    (magit-dash--batch-run
     repos
     (lambda (repo cb)
       (funcall cb (nth idx statuses))
       (setq idx (1+ idx)))
     "test"
     (lambda (results) (setq all-results results)))
    (should (= 1 (seq-count (lambda (r) (eq 'ok (cdr r))) all-results)))
    (should (= 1 (seq-count (lambda (r) (eq 'skipped (cdr r))) all-results)))
    (should (= 1 (seq-count (lambda (r) (eq 'error (cdr r))) all-results)))))

(ert-deftest magit-dash/batch-run-calls-on-all-done ()
  "batch-run calls on-all-done exactly once after the last repo completes."
  (let* ((repos (list (magit-dash-repo--make :name "r1" :path "/tmp/r1")))
         (call-count 0))
    (magit-dash--batch-run
     repos
     (lambda (_repo cb) (funcall cb 'ok))
     "test"
     (lambda (_) (setq call-count (1+ call-count))))
    (should (= 1 call-count))))

;;;; magit-dash--auto-commit-async

(ert-deftest magit-dash/auto-commit-async-skipped-when-clean ()
  "auto-commit-async returns 'skipped when git status --porcelain is empty."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r" :auto-commit t))
         (result nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path args on-success &optional _on-error)
                 (when (member "status" args)
                   (funcall on-success "")))))
      (magit-dash--auto-commit-async repo (lambda (s) (setq result s))))
    (should (eq 'skipped result))))

(ert-deftest magit-dash/auto-commit-async-ok-when-dirty ()
  "auto-commit-async returns 'ok after successful add+commit."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r" :auto-commit t))
         (result nil)
         (git-calls nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path args on-success &optional _on-error)
                 (push (car args) git-calls)
                 (cond
                  ((member "status" args) (funcall on-success " M foo.el"))
                  ((member "add" args) (funcall on-success ""))
                  ((member "commit" args) (funcall on-success ""))))))
      (magit-dash--auto-commit-async repo (lambda (s) (setq result s))))
    (should (eq 'ok result))
    (should (member "status" git-calls))
    (should (member "add" git-calls))
    (should (member "commit" git-calls))))

(ert-deftest magit-dash/auto-commit-async-error-on-add-failure ()
  "auto-commit-async returns 'error when git add fails."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r" :auto-commit t))
         (result nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path args on-success on-error)
                 (cond
                  ((member "status" args) (funcall on-success " M foo.el"))
                  ((member "add" args) (funcall on-error "error" 1))))))
      (magit-dash--auto-commit-async repo (lambda (s) (setq result s))))
    (should (eq 'error result))))

(ert-deftest magit-dash/auto-commit-async-uses-message-function ()
  "auto-commit-async uses the :auto-commit function to generate the commit message."
  (let* ((custom-msg "custom: my message")
         (repo (magit-dash-repo--make :name "r" :path "/tmp/r"
                                    :auto-commit (lambda (_r) custom-msg)))
         (commit-msg nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path args on-success &optional _on-error)
                 (cond
                  ((member "status" args) (funcall on-success " M foo.el"))
                  ((member "add" args) (funcall on-success ""))
                  ((member "commit" args)
                   (setq commit-msg (cadr (member "-m" args)))
                   (funcall on-success ""))))))
      (magit-dash--auto-commit-async repo #'ignore))
    (should (equal custom-msg commit-msg))))

;;;; magit-dash-commit-all (async)

(ert-deftest magit-dash/commit-all-async-user-error-when-none ()
  "commit-all signals user-error when no repos have :auto-commit configured."
  (let ((magit-dash-repo-list (list (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-dash-commit-all) :type 'user-error)))

(ert-deftest magit-dash/commit-all-async-runs-batch ()
  "commit-all dispatches --batch-run for repos with :auto-commit set."
  (let* ((magit-dash-repo-list
          (list (magit-dash-repo--make :name "r1" :path "/tmp/r1" :auto-commit t)
                (magit-dash-repo--make :name "r2" :path "/tmp/r2")))
         (batched-repos nil))
    (cl-letf (((symbol-function 'magit-dash--batch-run)
               (lambda (repos _op _label &optional _done)
                 (setq batched-repos (seq-map #'magit-dash-repo-name repos)))))
      (magit-dash-commit-all))
    (should (= 1 (length batched-repos)))
    (should (equal "r1" (car batched-repos)))))

;;;; magit-dash-auto-sync

(ert-deftest magit-dash/auto-sync-user-error-when-none ()
  "autosync signals user-error when no repos have :auto-commit or :auto-sync."
  (let ((magit-dash-repo-list (list (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-dash-auto-sync) :type 'user-error)))

(ert-deftest magit-dash/auto-sync-dispatches-single-batch ()
  "autosync runs one batch for all repos with any auto operation configured."
  (let* ((magit-dash-repo-list
          (list (magit-dash-repo--make :name "c1" :path "/tmp/c1" :auto-commit t)
                (magit-dash-repo--make :name "f1" :path "/tmp/f1" :auto-fetch t)
                (magit-dash-repo--make :name "n1" :path "/tmp/n1")))
         (batch-repos nil)
         (batch-labels nil))
    (cl-letf (((symbol-function 'magit-dash--batch-run)
               (lambda (repos _op label &optional _done)
                 (push label batch-labels)
                 (setq batch-repos repos))))
      (magit-dash-auto-sync))
    (should (= 1 (length batch-labels)))
    (should (equal "magit-gh autosync" (car batch-labels)))
    (should (= 2 (length batch-repos)))))

(ert-deftest magit-dash/auto-sync-commit-only-repo-included ()
  "autosync includes a repo with only :auto-commit in the single batch."
  (let* ((magit-dash-repo-list
          (list (magit-dash-repo--make :name "c1" :path "/tmp/c1" :auto-commit t)))
         (batch-repos nil))
    (cl-letf (((symbol-function 'magit-dash--batch-run)
               (lambda (repos _op _label &optional _done)
                 (setq batch-repos repos))))
      (magit-dash-auto-sync))
    (should (= 1 (length batch-repos)))))

;;;; Builder and agent-shell commands

(ert-deftest magit-dash/builder-delegates-to-builder ()
  "builder command invokes builder-compile-project in the repo directory."
  (let ((called-in nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () (magit-dash-repo--make :name "r" :path "/tmp/r")))
              ((symbol-function 'builder-compile-project)
               (lambda () (interactive) (setq called-in default-directory))))
      (magit-dash-builder))
    (should (string-prefix-p "/tmp/r" (or called-in "")))))

(ert-deftest magit-dash/agent-shell-queue-callable ()
  "agent-shell-queue command calls agent-shell-queue-buffer-open."
  (let ((called nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () (magit-dash-repo--make :name "r" :path "/tmp/r")))
              ((symbol-function 'agent-shell-queue-buffer-open)
               (lambda () (interactive) (setq called t))))
      (magit-dash-agent-shell-queue))
    (should called)))

(ert-deftest magit-dash/overview-builder-delegates ()
  "overview builder command invokes builder-compile-project in the repo directory."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (called-in nil))
    (cl-letf (((symbol-function 'magit-dash-overview--current-repo)
               (lambda () repo))
              ((symbol-function 'builder-compile-project)
               (lambda () (interactive) (setq called-in default-directory))))
      (magit-dash-overview-builder))
    (should (string-prefix-p "/tmp/r" (or called-in "")))))

(ert-deftest magit-dash/overview-agent-shell-queue-callable ()
  "overview agent-shell-queue command calls agent-shell-queue-buffer-open."
  (let ((called nil))
    (cl-letf (((symbol-function 'magit-dash-overview--current-repo)
               (lambda () (magit-dash-repo--make :name "r" :path "/tmp/r")))
              ((symbol-function 'agent-shell-queue-buffer-open)
               (lambda () (interactive) (setq called t))))
      (magit-dash-overview-agent-shell-queue))
    (should called)))

;;;; magit-dash--parse-worktrees

(defconst magit-dash-test--worktree-output
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

(ert-deftest magit-dash/parse-worktrees-skips-main ()
  "parse-worktrees omits the main worktree (first block)."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 magit-dash-test--worktree-output)))
    (should (= 2 (length result)))
    (should-not (seq-find (lambda (r) (equal "/tmp/main" (magit-dash-repo-path r)))
                          result))))

(ert-deftest magit-dash/parse-worktrees-paths ()
  "parse-worktrees sets correct paths on returned structs."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 magit-dash-test--worktree-output)))
    (should (equal "/tmp/wt1" (magit-dash-repo-path (nth 0 result))))
    (should (equal "/tmp/wt2" (magit-dash-repo-path (nth 1 result))))))

(ert-deftest magit-dash/parse-worktrees-names ()
  "parse-worktrees constructs names from main-repo basename and branch."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 magit-dash-test--worktree-output)))
    (should (equal "main@feature-1" (magit-dash-repo-name (nth 0 result))))
    (should (equal "main@detached" (magit-dash-repo-name (nth 1 result))))))

(ert-deftest magit-dash/parse-worktrees-worktree-flag ()
  "parse-worktrees sets :worktree t on all returned structs."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 magit-dash-test--worktree-output)))
    (should (seq-every-p #'magit-dash-repo-worktree result))))

(ert-deftest magit-dash/parse-worktrees-empty-output ()
  "parse-worktrees returns nil when only the main worktree is listed."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 '("worktree /tmp/main" "HEAD abc123" "branch refs/heads/main" ""))))
    (should (null result))))

;;;; magit-dash--sorted-repos with worktrees

(ert-deftest magit-dash/sorted-repos-appends-worktrees ()
  "sorted-repos places discovered worktrees immediately after their parent."
  (let* ((main (magit-dash-repo--make :name "main" :path "/tmp/main"))
         (wt (magit-dash-repo--make :name "main@feat" :path "/tmp/wt" :worktree t))
         (magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/main" :worktrees (list wt))
    (let ((result (magit-dash--sorted-repos (list main))))
      (should (= 2 (length result)))
      (should (equal "main" (magit-dash-repo-name (nth 0 result))))
      (should (equal "main@feat" (magit-dash-repo-name (nth 1 result)))))))

;;;; Column configuration

(ert-deftest magit-dash/column-enabled-defaults ()
  "All columns are enabled by default."
  (let ((magit-dash-columns
         '((name . t) (branch . t) (fetched . t) (status . t) (worktree . t))))
    (should (seq-every-p #'magit-dash--column-enabled-p
                         magit-dash--all-columns))))

(ert-deftest magit-dash/column-disabled ()
  "A disabled column is excluded from active-columns."
  (let ((magit-dash-columns
         '((name . t) (branch . nil) (fetched . t) (status . t) (worktree . t))))
    (should-not (magit-dash--column-enabled-p 'branch))
    (should-not (member 'branch (magit-dash--active-columns)))))

(ert-deftest magit-dash/build-format-omits-disabled-columns ()
  "build-format produces a vector that excludes disabled columns."
  (let ((magit-dash-columns
         '((name . t) (branch . nil) (fetched . t) (status . t) (worktree . nil)))
        (repos (list (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (let ((fmt (magit-dash--build-format repos)))
      (should (= 3 (length fmt)))
      (should-not (seq-find (lambda (col) (equal "Branch" (car col))) (append fmt nil)))
      (should-not (seq-find (lambda (col) (equal "Type" (car col))) (append fmt nil))))))

(ert-deftest magit-dash/build-entry-matches-format ()
  "build-entry vector length matches active-column count."
  (let ((magit-dash-columns
         '((name . t) (branch . t) (fetched . nil) (status . t) (worktree . nil)))
        (magit-dash--worktree-map (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--get-stats)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil :fetch-age 60.0
                       :head-hash "abc" :recent-log "" :remote-origin nil
                       :uncommitted-files nil))))
      (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
             (entry (magit-dash--build-entry repo))
             (active (magit-dash--active-columns)))
        (should (= (length active) (length (cadr entry))))))))

;;;; New command user-error conditions

(ert-deftest magit-dash/visit-buffer-user-error-when-none ()
  "visit-buffer signals user-error when no buffers visit the repo."
  (let ((magit-dash-repo-list nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () (magit-dash-repo--make :name "r" :path "/nonexistent/path")))
              ((symbol-function 'buffer-list) (lambda () nil)))
      (should-error (magit-dash-visit-buffer) :type 'user-error))))

(ert-deftest magit-dash/worktree-delete-user-error-when-not-worktree ()
  "worktree-delete signals user-error when the entry is not a worktree."
  (cl-letf (((symbol-function 'magit-dash--repo-at-point)
             (lambda () (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-dash-worktree-delete) :type 'user-error)))

(ert-deftest magit-dash/worktree-add-user-error-when-at-worktree ()
  "worktree-add signals user-error when the entry is itself a worktree."
  (cl-letf (((symbol-function 'magit-dash--repo-at-point)
             (lambda () (magit-dash-repo--make :name "r@feat" :path "/tmp/wt" :worktree t))))
    (should-error (magit-dash-worktree-add) :type 'user-error)))

(ert-deftest magit-dash/overview-worktree-add-user-error-for-worktree ()
  "overview-worktree-add signals user-error when the overview IS a worktree."
  (let ((repo (magit-dash-repo--make :name "r@f" :path "/tmp/wt" :worktree t)))
    (cl-letf (((symbol-function 'magit-dash-overview--current-repo) (lambda () repo)))
      (should-error (magit-dash-overview-worktree-add) :type 'user-error))))

(ert-deftest magit-dash/overview-worktree-delete-user-error-for-main ()
  "overview-worktree-delete signals user-error when the overview is NOT a worktree."
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r")))
    (cl-letf (((symbol-function 'magit-dash-overview--current-repo) (lambda () repo)))
      (should-error (magit-dash-overview-worktree-delete) :type 'user-error))))

;;;; magit-dash-overview--worktrees-for (lazy discovery)

(ert-deftest magit-dash/overview-worktrees-for-uses-cache ()
  "worktrees-for returns cached value without running git."
  (let* ((wt (magit-dash-repo--make :name "r@feat" :path "/tmp/wt" :worktree t))
         (magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/main" :worktrees (list wt))
    (let ((result (magit-dash-overview--worktrees-for "/tmp/main")))
      (should (= 1 (length result)))
      (should (equal "r@feat" (magit-dash-repo-name (car result)))))))

(ert-deftest magit-dash/overview-worktrees-for-caches-nil ()
  "worktrees-for caches nil when git finds no worktrees, avoiding re-runs."
  (let* ((magit-gh--cache (make-hash-table :test #'equal))
         (call-count 0))
    (cl-letf (((symbol-function 'process-lines)
               (lambda (&rest _) (setq call-count (1+ call-count)) nil)))
      (magit-dash-overview--worktrees-for "/tmp/main")
      (magit-dash-overview--worktrees-for "/tmp/main"))
    (should (= 1 call-count))))

;;;; worktree branch field

(ert-deftest magit-dash/parse-worktrees-stores-branch ()
  "parse-worktrees stores branch name in the :branch struct slot."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 magit-dash-test--worktree-output)))
    (should (equal "feature-1" (magit-dash-repo-branch (nth 0 result))))
    (should (equal "detached"  (magit-dash-repo-branch (nth 1 result))))))

(ert-deftest magit-dash/build-entry-branch-falls-back-to-struct ()
  "build-entry uses struct :branch when stats return empty string."
  (let ((magit-dash-columns
         '((name . t) (branch . t) (fetched . nil) (behind . nil) (changes . nil) (worktree . nil)))
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--get-stats)
               (lambda (_) (list :branch "" :ahead 0 :behind 0 :dirty nil :fetch-age nil
                                 :head-hash "abc" :recent-log "" :remote-origin nil
                                 :uncommitted-files nil))))
      (let* ((repo (magit-dash-repo--make :name "r@feat" :path "/tmp/wt"
                                        :worktree t :branch "feat"))
             (entry (magit-dash--build-entry repo))
             (branch-cell (aref (cadr entry) 1)))
        (should (equal "feat" (substring-no-properties branch-cell)))))))

(ert-deftest magit-dash/build-entry-branch-uses-stats-when-available ()
  "build-entry uses stats :branch when non-empty, even for worktrees."
  (let ((magit-dash-columns
         '((name . t) (branch . t) (fetched . nil) (behind . nil) (changes . nil) (worktree . nil)))
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--get-stats)
               (lambda (_) (list :branch "live-branch" :ahead 0 :behind 0 :dirty nil :fetch-age nil
                                 :head-hash "abc" :recent-log "" :remote-origin nil
                                 :uncommitted-files nil))))
      (let* ((repo (magit-dash-repo--make :name "r@old" :path "/tmp/wt"
                                        :worktree t :branch "old"))
             (entry (magit-dash--build-entry repo))
             (branch-cell (aref (cadr entry) 1)))
        (should (equal "live-branch" (substring-no-properties branch-cell)))))))

;;;; magit-dash--parse-submodules

(defconst magit-dash-test--submodule-output
  '(" abc1234def5 vendor/lib (v1.2)"
    " 0000000000a sub/other (HEAD)"
    "-deadbeef00b uninit-mod"
    "+cafebabe001 modified-sub (HEAD)")
  "Sample `git submodule status' output as a list of lines.")

(ert-deftest magit-dash/parse-submodules-count ()
  "parse-submodules returns one struct per accessible submodule."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
    (let ((result (magit-dash--parse-submodules
                   "/tmp/main"
                   magit-dash-test--submodule-output)))
      (should (= 4 (length result))))))

(ert-deftest magit-dash/parse-submodules-paths ()
  "parse-submodules sets absolute paths on returned structs."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
    (let ((result (magit-dash--parse-submodules
                   "/tmp/main"
                   magit-dash-test--submodule-output)))
      (should (equal "/tmp/main/vendor/lib" (magit-dash-repo-path (nth 0 result))))
      (should (equal "/tmp/main/sub/other"  (magit-dash-repo-path (nth 1 result)))))))

(ert-deftest magit-dash/parse-submodules-names ()
  "parse-submodules formats names as \"parent<submod>\"."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
    (let ((result (magit-dash--parse-submodules
                   "/tmp/main"
                   magit-dash-test--submodule-output)))
      (should (equal "main<lib>"      (magit-dash-repo-name (nth 0 result))))
      (should (equal "main<other>"    (magit-dash-repo-name (nth 1 result))))
      (should (equal "main<uninit-mod>"  (magit-dash-repo-name (nth 2 result))))
      (should (equal "main<modified-sub>" (magit-dash-repo-name (nth 3 result)))))))

(ert-deftest magit-dash/parse-submodules-submodule-flag ()
  "parse-submodules sets :submodule t on all returned structs."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
    (let ((result (magit-dash--parse-submodules
                   "/tmp/main"
                   magit-dash-test--submodule-output)))
      (should (seq-every-p #'magit-dash-repo-submodule result)))))

(ert-deftest magit-dash/parse-submodules-skips-missing-paths ()
  "parse-submodules omits entries whose paths don't exist on disk."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) nil)))
    (let ((result (magit-dash--parse-submodules
                   "/tmp/main"
                   magit-dash-test--submodule-output)))
      (should (null result)))))

(ert-deftest magit-dash/parse-submodules-empty-output ()
  "parse-submodules returns nil for empty output."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
    (should (null (magit-dash--parse-submodules "/tmp/main" '())))))

;;;; --sorted-repos with submodules

(ert-deftest magit-dash/sorted-repos-appends-submodules ()
  "sorted-repos places discovered submodules after their parent."
  (let* ((main (magit-dash-repo--make :name "main" :path "/tmp/main"))
         (mod (magit-dash-repo--make :name "main<lib>" :path "/tmp/main/lib" :submodule t))
         (magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/main" :worktrees nil)
    (magit-gh--cache-set "/tmp/main" :submodules (list mod))
    (let ((result (magit-dash--sorted-repos (list main))))
      (should (= 2 (length result)))
      (should (equal "main"      (magit-dash-repo-name (nth 0 result))))
      (should (equal "main<lib>" (magit-dash-repo-name (nth 1 result)))))))

(ert-deftest magit-dash/sorted-repos-appends-worktrees-and-submodules ()
  "sorted-repos appends both worktrees and submodules after parent, in that order."
  (let* ((main (magit-dash-repo--make :name "main" :path "/tmp/main"))
         (wt   (magit-dash-repo--make :name "main@feat" :path "/tmp/wt" :worktree t))
         (mod  (magit-dash-repo--make :name "main<lib>" :path "/tmp/main/lib" :submodule t))
         (magit-gh--cache (make-hash-table :test #'equal)))
    (magit-gh--cache-set "/tmp/main" :worktrees (list wt))
    (magit-gh--cache-set "/tmp/main" :submodules (list mod))
    (let ((result (magit-dash--sorted-repos (list main))))
      (should (= 3 (length result)))
      (should (equal "main"      (magit-dash-repo-name (nth 0 result))))
      (should (equal "main@feat" (magit-dash-repo-name (nth 1 result))))
      (should (equal "main<lib>" (magit-dash-repo-name (nth 2 result)))))))

(ert-deftest magit-dash/sorted-repos-deduplicates-registered-submodules ()
  "sorted-repos suppresses auto-discovered submodule rows whose path is already
in magit-dash-repo-list, preventing duplicate rows."
  (let* ((main (magit-dash-repo--make :name "main" :path "/tmp/main"))
         (lib  (magit-dash-repo--make :name "lib"  :path "/tmp/main/lib"))
         (mod  (magit-dash-repo--make :name "main<lib>" :path "/tmp/main/lib" :submodule t))
         (magit-gh--cache (make-hash-table :test #'equal))
         (magit-dash-repo-list (list main lib))
         (magit-dash-show-discovered-submodules t))
    (magit-gh--cache-set "/tmp/main" :worktrees nil)
    (magit-gh--cache-set "/tmp/main" :submodules (list mod))
    (let ((result (magit-dash--sorted-repos (list main lib))))
      (should (= 2 (length result)))
      (should (equal "main" (magit-dash-repo-name (nth 0 result))))
      (should (equal "lib"  (magit-dash-repo-name (nth 1 result)))))))

(ert-deftest magit-dash/sorted-repos-keeps-unregistered-submodules ()
  "sorted-repos still appends auto-discovered submodules that are not in magit-dash-repo-list."
  (let* ((main (magit-dash-repo--make :name "main" :path "/tmp/main"))
         (mod  (magit-dash-repo--make :name "main<lib>" :path "/tmp/main/lib" :submodule t))
         (magit-gh--cache (make-hash-table :test #'equal))
         (magit-dash-repo-list (list main))
         (magit-dash-show-discovered-submodules t))
    (magit-gh--cache-set "/tmp/main" :worktrees nil)
    (magit-gh--cache-set "/tmp/main" :submodules (list mod))
    (let ((result (magit-dash--sorted-repos (list main))))
      (should (= 2 (length result)))
      (should (equal "main"      (magit-dash-repo-name (nth 0 result))))
      (should (equal "main<lib>" (magit-dash-repo-name (nth 1 result)))))))

;;;; Transient menu key conflict detection

(defun test-magit-dash--transient-keys (prefix)
  "Collect all key strings from transient PREFIX layout.
Layout shape: [2 nil (ROW)] where ROW = [transient-columns nil (COL...)]
and each COL = [transient-column PROPS ((transient-suffix :key K ...) ...)]."
  (when-let* ((layout (get prefix 'transient--layout))
              ((vectorp layout))
              (rows (aref layout 2))
              (row (car rows))
              ((vectorp row))
              (cols (aref row 2)))
    (thread-last (if (vectorp cols) (append cols nil) cols)
      (seq-filter #'vectorp)
      (seq-mapcat (lambda (col) (aref col 2)))
      (seq-filter (lambda (s) (and (listp s) (eq (car s) 'transient-suffix))))
      (seq-map (lambda (s) (plist-get (cdr s) :key)))
      (seq-remove #'null))))

(ert-deftest magit-dash/transient-predicates-safe-with-no-repo ()
  "All transient :inapt-if-not predicates return nil (not error) with no repo at point."
  (let ((magit-dash-repo-list nil)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () nil))
              ((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
      (should (null (magit-dash--repo-at-point-p)))
      (should (null (magit-dash--dirty-or-unknown-p)))
      (should (null (magit-dash--has-auto-commit-p)))
      (should (null (magit-dash--has-commands-p)))
      (should (null (magit-dash--repo-at-point-behind-p)))
      (should (null (magit-dash--at-worktree-p)))
      (should (null (magit-dash--can-add-worktree-p))))))

(ert-deftest magit-dash/menu-no-key-prefix-conflicts ()
  "No key in the dashboard transient menu is a string prefix of another key.
A conflict (e.g. \"b\" and \"bp\" coexisting) causes transient to raise
\"Wrong type argument: command, (keymap ...), command\" when the menu is opened."
  (let ((keys (test-magit-dash--transient-keys
               'magit-dash-menu)))
    (should (> (length keys) 0))
    (seq-do
     (lambda (k)
       (let ((conflict (seq-find (lambda (other)
                                   (and (not (equal k other))
                                        (string-prefix-p k other)))
                                 keys)))
         (should (equal nil conflict))))
     keys)))

;;;; Ephemeral tag tests

(ert-deftest magit-dash/all-tags-for-permanent-only ()
  "all-tags-for returns permanent tags when no ephemeral tags exist."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "r" :path "/tmp/r" :tags '(work personal))
    (should (equal '(work personal)
                   (magit-dash--all-tags-for (car magit-dash-repo-list))))))

(ert-deftest magit-dash/all-tags-for-combines-ephemeral ()
  "all-tags-for appends ephemeral tags after permanent ones."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "r" :path "/tmp/r" :tags '(work))
    (puthash "/tmp/r" '(temp) magit-dash--ephemeral-tags)
    (should (equal '(work temp)
                   (magit-dash--all-tags-for (car magit-dash-repo-list))))))

(ert-deftest magit-dash/all-tags-for-ephemeral-only ()
  "all-tags-for returns ephemeral tags for a repo with no permanent tags."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "r" :path "/tmp/r")
    (puthash "/tmp/r" '(draft) magit-dash--ephemeral-tags)
    (should (equal '(draft)
                   (magit-dash--all-tags-for (car magit-dash-repo-list))))))

(ert-deftest magit-dash/permanent-tag-set-collects-all ()
  "permanent-tag-set returns deduplicated symbols from all repo :tags fields."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "a" :path "/tmp/a" :tags '(work))
    (magit-dash-register :name "b" :path "/tmp/b" :tags '(work personal))
    (let ((tags (magit-dash--permanent-tag-set)))
      (should (memq 'work tags))
      (should (memq 'personal tags))
      (should (= 2 (length tags))))))

(ert-deftest magit-dash/permanent-tag-set-excludes-ephemeral ()
  "permanent-tag-set does not include ephemeral-only tags."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "r" :path "/tmp/r" :tags '(work))
    (puthash "/tmp/r" '(ephemeral-only) magit-dash--ephemeral-tags)
    (let ((tags (magit-dash--permanent-tag-set)))
      (should (memq 'work tags))
      (should (null (memq 'ephemeral-only tags))))))

(ert-deftest magit-dash/build-tag-table-format ()
  "build-tag-table returns a dotted alist of (name . annotation) strings."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "alpha" :path "/tmp/a" :tags '(work))
    (magit-dash-register :name "beta"  :path "/tmp/b" :tags '(work personal))
    (let* ((table (magit-dash--build-tag-table))
           (work-entry (seq-find (lambda (e) (equal (car e) "work")) table))
           (personal-entry (seq-find (lambda (e) (equal (car e) "personal")) table)))
      (should (consp work-entry))
      (should (string-match-p "2 repos" (cdr work-entry)))
      (should (consp personal-entry))
      (should (string-match-p "1 repo:" (cdr personal-entry))))))

(ert-deftest magit-dash/build-tag-table-permanent-before-ephemeral ()
  "build-tag-table lists permanent tags before ephemeral-only tags."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "r" :path "/tmp/r" :tags '(permanent))
    (puthash "/tmp/r" '(ephemeral) magit-dash--ephemeral-tags)
    (let* ((table (magit-dash--build-tag-table))
           (names (seq-map #'car table)))
      (should (< (seq-position names "permanent")
                 (seq-position names "ephemeral"))))))

(ert-deftest magit-dash/transient-predicates-include-auto-sync ()
  "has-auto-sync-p returns nil when no repo is at point."
  (let ((magit-dash-repo-list nil)
        (magit-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () nil))
              ((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
      (should (null (magit-dash--has-auto-sync-p))))))

(provide 'test-magit-dash)

;;; test-magit-dash.el ends here

;;;; Cache reset and rendering (regression tests)

(ert-deftest magit-dash/cache-reset-rendering ()
  "Regression: rendering after cache reset should not fail.
Simulates the user's issue where a cache reset caused rendering problems."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (magit-dash-repo-list (list repo))
         (magit-gh--cache (make-hash-table :test #'equal))
         (build-entry-called nil))
    ;; Pre-populate cache with valid stats
    (magit-gh--cache-set "/tmp/test" :stats
                         (list :branch "main"
                               :remote-origin "git@github.com:user/test.git"
                               :behind 0
                               :ahead 0
                               :dirty nil
                               :uncommitted-files nil
                               :fetch-age 3600.0
                               :head-hash "abc123"
                               :recent-log "abc123 initial commit"))
    ;; Reset cache (this is what user did)
    (clrhash magit-gh--cache)
    ;; Mock git functions to return valid data
    (cl-letf (((symbol-function 'magit-git-string)
               (lambda (&rest args)
                 (cond
                  ((member "branch" args) "main")
                  ((member "rev-list" args) "0")
                  (t nil))))
              ((symbol-function 'magit-get)
               (lambda (&rest _) "git@github.com:user/test.git"))
              ((symbol-function 'magit-git-lines)
               (lambda (&rest args)
                 (cond
                  ((member "status" args) nil)
                  ((member "log" args) '("abc123 initial commit"))
                  (t nil))))
              ((symbol-function 'magit-dash--fetch-age)
               (lambda (_) 3600.0))
              ((symbol-function 'magit-dash--head-hash)
               (lambda (_) "abc123"))
              ((symbol-function 'magit-dash--build-entry)
               (lambda (r)
                 (setq build-entry-called t)
                 ;; Call the real function to test it
                 (let* ((stats (magit-dash--get-stats r))
                        (active '(name branch fetched status worktree)))
                   ;; Verify stats are valid
                   (should (plist-get stats :branch))
                   (should (plist-get stats :head-hash))
                   ;; Return a minimal valid entry
                   (list r (vector "test" "main" "1h" "" "REPO")))))
              ((symbol-function 'tabulated-list-print) (lambda (&rest _) nil))
              ((symbol-function 'tabulated-list-init-header) (lambda () nil)))
      (with-temp-buffer
        (magit-dash-mode)
        ;; This should not fail even though cache is empty
        (magit-dash-refresh)
        (should build-entry-called)))))

(ert-deftest magit-dash/cache-reset-all-repopulates ()
  "cache-reset-all should repopulate stats for all repos."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2"))
         (magit-dash-repo-list (list r1 r2))
         (magit-gh--cache (make-hash-table :test #'equal))
         (collect-calls nil))
    (cl-letf (((symbol-function 'magit-dash--collect-stats)
               (lambda (repo)
                 (push (magit-dash-repo-name repo) collect-calls)
                 (list :branch "main" :dirty nil :head-hash "abc123"
                       :behind 0 :ahead 0 :uncommitted-files nil
                       :fetch-age 60.0 :recent-log "")))
              ((symbol-function 'magit-dash--discover-worktrees)
               (lambda () nil))
              ((symbol-function 'magit-dash--discover-submodules)
               (lambda () nil))
              ((symbol-function 'magit-dash-refresh)
               (lambda () nil)))
      (magit-dash-cache-reset-all)
      ;; Should have collected stats for both repos
      (should (= 2 (length collect-calls)))
      (should (member "r1" collect-calls))
      (should (member "r2" collect-calls))
      ;; Cache should now have stats
      (should (magit-gh--cache-get "/tmp/r1" :stats))
      (should (magit-gh--cache-get "/tmp/r2" :stats)))))

(ert-deftest magit-dash/cache-diagnose-finds-missing-stats ()
  "cache-diagnose should detect repos with missing stats."
  (let* ((repo (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (magit-dash-repo-list (list repo))
         (magit-gh--cache (make-hash-table :test #'equal))
         (messages nil))
    ;; No stats in cache
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'pop-to-buffer) (lambda (_) nil))
              ((symbol-function 'view-mode) (lambda (_) nil)))
      (magit-dash-cache-diagnose)
      ;; Should report 1 warning
      (should (seq-some (lambda (msg) (string-match-p "1 warning" msg)) messages)))))

(ert-deftest magit-dash/cache-diagnose-finds-malformed-stats ()
  "cache-diagnose should detect stats missing required fields."
  (let* ((repo (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (magit-dash-repo-list (list repo))
         (magit-gh--cache (make-hash-table :test #'equal))
         (messages nil))
    ;; Add malformed stats (missing :head-hash)
    (magit-gh--cache-set "/tmp/r1" :stats (list :branch "main" :dirty nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'pop-to-buffer) (lambda (_) nil))
              ((symbol-function 'view-mode) (lambda (_) nil)))
      (magit-dash-cache-diagnose)
      ;; Should report errors
      (should (seq-some (lambda (msg) (string-match-p "error" msg)) messages)))))

(ert-deftest magit-dash/cache-stats-shows-summary ()
  "cache-stats should display a summary of cached data."
  (let* ((repo (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (magit-dash-repo-list (list repo))
         (magit-gh--cache (make-hash-table :test #'equal))
         (buffer-shown nil))
    (magit-gh--cache-set "/tmp/r1" :stats
                         (list :branch "main" :head-hash "abc123"
                               :dirty nil :behind 0 :ahead 0))
    (magit-gh--cache-set "/tmp/r1" :pr-counts (cons 5 2))
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buf) (setq buffer-shown buf)))
              ((symbol-function 'view-mode) (lambda (_) nil)))
      (magit-dash-cache-stats)
      (should buffer-shown)
      (with-current-buffer buffer-shown
        (let ((text (buffer-string)))
          (should (string-match-p "Repository: r1" text))
          (should (string-match-p "Stats cached: yes" text))
          (should (string-match-p "PR counts cached: yes" text)))))))

(ert-deftest magit-dash/build-entry-handles-missing-stats ()
  "build-entry should handle repos with no cached stats gracefully."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (magit-gh--cache (make-hash-table :test #'equal))
         (magit-dash--submodule-path-set (make-hash-table :test #'equal))
         (magit-dash-columns
          '((name . t) (branch . t) (fetched . t) (status . t) (worktree . t))))
    (cl-letf (((symbol-function 'magit-dash--get-stats)
               (lambda (_)
                 ;; Simulate fresh collection
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil
                       :fetch-age nil :head-hash "abc" :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (vec (cadr entry)))
        (should (= 5 (length vec)))
        (should (string-match-p "test" (aref vec 0)))
        (should (equal "main" (substring-no-properties (aref vec 1))))
        (should (equal "never" (aref vec 2)))))))

(ert-deftest magit-dash/cache-reset-at-point-refreshes ()
  "cache-reset-at-point should clear cache for one repo and refresh."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (magit-gh--cache (make-hash-table :test #'equal))
         (refresh-called nil)
         (collect-called nil))
    ;; Pre-populate cache
    (magit-gh--cache-set "/tmp/test" :stats (list :branch "main" :dirty nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () repo))
              ((symbol-function 'magit-dash--collect-stats)
               (lambda (r)
                 (setq collect-called t)
                 (list :branch "feat" :dirty t :head-hash "new")))
              ((symbol-function 'magit-dash--maybe-refresh)
               (lambda () (setq refresh-called t))))
      (magit-dash-cache-reset-at-point)
      (should collect-called)
      (should refresh-called)
      ;; Stats should be updated
      (let ((stats (magit-gh--cache-get "/tmp/test" :stats)))
        (should (equal "feat" (plist-get stats :branch)))))))

(ert-deftest magit-dash/format-age-consistent-after-cache-reset ()
  "Regression: format-age should work consistently after cache reset."
  ;; Test that formatting functions don't depend on cache state
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (should (equal "never" (magit-dash--format-age nil)))
    (should (equal "1h" (magit-dash--format-age 3600.0)))
    (clrhash magit-gh--cache)
    (should (equal "never" (magit-dash--format-age nil)))
    (should (equal "1h" (magit-dash--format-age 3600.0)))))

(ert-deftest magit-dash/format-status-consistent-after-cache-reset ()
  "Regression: format-status should work consistently after cache reset."
  (let ((magit-gh--cache (make-hash-table :test #'equal)))
    (should (equal "" (magit-dash--format-status 0 0 nil)))
    (should (equal "↑2 ↓3 !" (substring-no-properties
                              (magit-dash--format-status 2 3 t))))
    (clrhash magit-gh--cache)
    (should (equal "" (magit-dash--format-status 0 0 nil)))
    (should (equal "↑2 ↓3 !" (substring-no-properties
                              (magit-dash--format-status 2 3 t))))))

(ert-deftest magit-dash/build-entry-name-returns-string ()
  "Regression: name column must return a string, not the result of add-text-properties.
The bug was that add-text-properties returns t, not the modified string."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (magit-gh--cache (make-hash-table :test #'equal))
         (magit-dash--submodule-path-set (make-hash-table :test #'equal))
         (magit-dash--marked-paths nil)
         (magit-dash-columns
          '((name . t))))
    (cl-letf (((symbol-function 'magit-dash--get-stats)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil
                       :fetch-age nil :head-hash "abc" :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (vec (cadr entry))
             (name-col (aref vec 0)))
        ;; The name column must be a string
        (should (stringp name-col))
        (should (equal "test" (substring-no-properties name-col)))))))

(ert-deftest magit-dash/build-entry-all-columns-are-strings ()
  "All column values must be strings for tabulated-list-mode."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (magit-gh--cache (make-hash-table :test #'equal))
         (magit-dash--submodule-path-set (make-hash-table :test #'equal))
         (magit-dash-columns
          '((name . t) (branch . t) (fetched . t) (status . t) (worktree . t))))
    (cl-letf (((symbol-function 'magit-dash--get-stats)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil
                       :fetch-age 3600.0 :head-hash "abc123" :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (vec (cadr entry)))
        (should (= 5 (length vec)))
        ;; Every column value must be a string
        (dotimes (i (length vec))
          (let ((val (aref vec i)))
            (should (stringp val))))))))

(ert-deftest magit-dash/head-hash-nil-is-valid ()
  "Repos with no commits can have nil :head-hash without being malformed."
  (let* ((repo (magit-dash-repo--make :name "empty" :path "/tmp/empty"))
         (magit-dash-repo-list (list repo))
         (magit-gh--cache (make-hash-table :test #'equal))
         (messages nil))
    ;; Set up stats with nil head-hash (valid for repos with no commits)
    (magit-gh--cache-set "/tmp/empty" :stats
                         (list :branch "" :head-hash nil :dirty nil
                               :behind 0 :ahead 0 :uncommitted-files nil
                               :fetch-age nil :recent-log ""))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'pop-to-buffer) (lambda (_) nil))
              ((symbol-function 'view-mode) (lambda (_) nil))
              ((symbol-function 'magit-dash--head-hash)
               (lambda (_) nil)))
      (magit-dash-cache-diagnose)
      ;; Should not report this as an error (nil is in the plist, just has nil value)
      (should (seq-some (lambda (msg) (string-match-p "0 error" msg)) messages)))))

(ert-deftest magit-dash/parse-submodules-includes-missing ()
  "parse-submodules should include uninitialized (missing) submodules."
  (cl-letf (((symbol-function 'file-directory-p)
             (lambda (path)
               ;; Only "present-mod" directory exists
               (string-match-p "present-mod" path))))
    (let* ((lines '("-abc123 external/missing-mod"
                    " def456 external/present-mod"))
           (repos (magit-dash--parse-submodules "/tmp/parent" lines)))
      (should (= 2 (length repos)))
      (let ((missing (seq-find (lambda (r) (string-match-p "missing-mod" (magit-dash-repo-name r))) repos))
            (present (seq-find (lambda (r) (string-match-p "present-mod" (magit-dash-repo-name r))) repos)))
        (should (eq 'missing (magit-dash-repo-submodule missing)))
        (should (eq t (magit-dash-repo-submodule present)))))))

(ert-deftest magit-dash/format-worktree-missing-submodule ()
  "Missing submodules should display as SUBM.EMPTY with warning face."
  (let ((repo (magit-dash-repo--make :name "test<sub>" :path "/tmp/missing"
                                   :submodule 'missing)))
    (let ((result (magit-dash--format-worktree repo)))
      (should (equal "SUBM.EMPTY" (substring-no-properties result)))
      (should (equal 'warning (get-text-property 0 'face result))))))

(ert-deftest magit-dash/format-worktree-present-submodule ()
  "Initialized submodules should display as SUBM."
  (let ((repo (magit-dash-repo--make :name "test<sub>" :path "/tmp/present"
                                   :submodule t)))
    (let ((result (magit-dash--format-worktree repo)))
      (should (equal "SUBM" (substring-no-properties result)))
      (should (equal 'magit-dash-repo-branch-face (get-text-property 0 'face result))))))

(ert-deftest magit-dash/get-stats-missing-submodule ()
  "Missing submodules should return placeholder stats without calling git."
  (let ((repo (magit-dash-repo--make :name "test<sub>" :path "/nonexistent"
                                   :submodule 'missing))
        (magit-gh--cache (make-hash-table :test #'equal))
        (collect-called nil))
    (cl-letf (((symbol-function 'magit-dash--collect-stats)
               (lambda (_) (setq collect-called t) (error "Should not be called"))))
      (let ((stats (magit-dash--get-stats repo)))
        (should-not collect-called)
        (should (plist-get stats :branch))
        (should (equal "" (plist-get stats :branch)))
        (should (eq nil (plist-get stats :head-hash)))))))

(ert-deftest magit-dash/build-entry-missing-submodule-strikethrough ()
  "Missing submodules should have strikethrough face on the name."
  (let* ((repo (magit-dash-repo--make :name "parent<missing>" :path "/tmp/missing"
                                    :submodule 'missing))
         (magit-gh--cache (make-hash-table :test #'equal))
         (magit-dash--submodule-path-set (make-hash-table :test #'equal))
         (magit-dash--marked-paths nil)
         (magit-dash-columns '((name . t) (worktree . t))))
    ;; Add to submodule path set so it gets the special display name
    (puthash "/tmp/missing" "parent<missing>" magit-dash--submodule-path-set)
    (cl-letf (((symbol-function 'magit-dash--get-stats)
               (lambda (_)
                 (list :branch "" :ahead 0 :behind 0 :dirty nil
                       :fetch-age nil :head-hash nil :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (vec (cadr entry))
             (name-col (aref vec 0))
             (type-col (aref vec 1)))
        ;; Check name has strikethrough
        (should (stringp name-col))
        (should (equal "parent<missing>" (substring-no-properties name-col)))
        (let ((face (get-text-property 0 'face name-col)))
          (should (listp face))
          (should (member '(:strike-through t) face)))
        ;; Check type is SUBM.EMPTY
        (should (equal "SUBM.EMPTY" (substring-no-properties type-col)))))))

(ert-deftest magit-dash/parse-submodules-prefix-detection ()
  "parse-submodules should detect missing submodules by - prefix."
  (let* ((lines '("-abc123 missing/sub1"
                  "+def456 modified/sub2"
                  " 123abc current/sub3"
                  "Uabc123 conflict/sub4"))
         (repos (magit-dash--parse-submodules "/tmp/test" lines)))
    (should (= 4 (length repos)))
    ;; - prefix means missing
    (let ((missing (seq-find (lambda (r) (string-match-p "sub1" (magit-dash-repo-name r))) repos)))
      (should (eq 'missing (magit-dash-repo-submodule missing))))
    ;; Other prefixes should still create repos, marked as missing if dir doesn't exist
    (should (seq-every-p #'magit-dash-repo-p repos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; magit-dash--repo-type-rank

(ert-deftest magit-dash/repo-type-rank-plain-repo ()
  "Plain repo (no worktree, no submodule) gets rank 0."
  (let ((r (magit-dash-repo--make :name "r" :path "/tmp/r")))
    (should (= 0 (magit-dash--repo-type-rank r)))))

(ert-deftest magit-dash/repo-type-rank-worktree ()
  "Worktree gets rank 1."
  (let ((r (magit-dash-repo--make :name "r" :path "/tmp/r" :worktree t)))
    (should (= 1 (magit-dash--repo-type-rank r)))))

(ert-deftest magit-dash/repo-type-rank-tracked-submodule ()
  "Tracked submodule (non-missing :submodule) gets rank 2."
  (let ((r (magit-dash-repo--make :name "r" :path "/tmp/r" :submodule "/tmp/parent")))
    (should (= 2 (magit-dash--repo-type-rank r)))))

(ert-deftest magit-dash/repo-type-rank-missing-submodule ()
  "Missing submodule gets rank 3."
  (let ((r (magit-dash-repo--make :name "r" :path "/tmp/r" :submodule 'missing)))
    (should (= 3 (magit-dash--repo-type-rank r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; magit-dash--sorted-repos type-based ordering

(ert-deftest magit-dash/sorted-repos-type-order-no-hints ()
  "Without sort-hints, repos are ordered by type: repo < wt < subm < missing."
  (let* ((repo (magit-dash-repo--make :name "repo" :path "/tmp/repo"))
         (wt (magit-dash-repo--make :name "wt" :path "/tmp/wt" :worktree t))
         (subm (magit-dash-repo--make :name "subm" :path "/tmp/subm" :submodule "/p"))
         (miss (magit-dash-repo--make :name "miss" :path "/tmp/miss" :submodule 'missing))
         (magit-gh--cache (make-hash-table :test #'equal))
         (result (magit-dash--sorted-repos (list miss wt repo subm))))
    (should (equal "repo" (magit-dash-repo-name (nth 0 result))))
    (should (equal "wt"   (magit-dash-repo-name (nth 1 result))))
    (should (equal "subm" (magit-dash-repo-name (nth 2 result))))
    (should (equal "miss" (magit-dash-repo-name (nth 3 result))))))

(ert-deftest magit-dash/sorted-repos-type-order-within-same-hint ()
  "Repos sharing the same sort-hint are ordered by type as secondary key."
  (let* ((repo (magit-dash-repo--make :name "repo" :path "/tmp/repo" :sort-hint 5))
         (wt (magit-dash-repo--make :name "wt" :path "/tmp/wt" :worktree t :sort-hint 5))
         (magit-gh--cache (make-hash-table :test #'equal))
         (result (magit-dash--sorted-repos (list wt repo))))
    (should (equal "repo" (magit-dash-repo-name (nth 0 result))))
    (should (equal "wt"   (magit-dash-repo-name (nth 1 result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; magit-dash--push-async

(ert-deftest magit-dash/push-async-calls-run-git-with-push ()
  "Calls --run-git with the repo path and (\"push\") args."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (captured-path nil)
         (captured-args nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (path args _on-success &optional _on-error)
                 (setq captured-path path
                       captured-args args))))
      (magit-dash--push-async repo #'ignore))
    (should (equal "/tmp/r" captured-path))
    (should (equal '("push") captured-args))))

(ert-deftest magit-dash/push-async-calls-callback-ok-on-success ()
  "Callback receives `ok' when --run-git calls on-success."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (result nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path _args on-success &optional _on-error)
                 (funcall on-success ""))))
      (magit-dash--push-async repo (lambda (status &rest _) (setq result status))))
    (should (eq 'ok result))))

(ert-deftest magit-dash/push-async-calls-callback-error-on-failure ()
  "Callback receives `error' when --run-git calls on-error."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (result nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path _args _on-success &optional on-error)
                 (funcall on-error "remote error" 1))))
      (magit-dash--push-async repo (lambda (status &rest _) (setq result status))))
    (should (eq 'error result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; magit-dash-push-all

(ert-deftest magit-dash/push-all-errors-when-no-repos ()
  "Signals user-error when --effective-repos returns nil."
  (cl-letf (((symbol-function 'magit-dash--effective-repos)
             (lambda () nil)))
    (should-error (magit-dash-push-all) :type 'user-error)))

(ert-deftest magit-dash/push-all-calls-batch-run-with-push-async ()
  "Calls --batch-run with the effective repos and --push-async as op-fn."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (batch-repos nil)
         (batch-op nil))
    (cl-letf (((symbol-function 'magit-dash--effective-repos)
               (lambda () (list repo)))
              ((symbol-function 'magit-dash--batch-run)
               (lambda (repos op-fn _label &optional _done)
                 (setq batch-repos repos
                       batch-op op-fn))))
      (magit-dash-push-all))
    (should (equal (list repo) batch-repos))
    (should (eq #'magit-dash--push-async batch-op))))
