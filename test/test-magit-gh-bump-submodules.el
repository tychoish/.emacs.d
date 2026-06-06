;;; test-magit-gh-bump-submodules.el --- ERT tests for magit-gh-bump-submodules -*- lexical-binding: t -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-gh-bump/")
;;
;; Batch run:
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-magit-gh-bump-submodules.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-gh-bump/")'

(require 'ert)
(require 'magit-gh-bump-submodules)

;;;; magit-gh-bump--make-bump-message

(ert-deftest magit-gh-bump/make-bump-message-single ()
  "Single dirty submodule yields \"bump: name\"."
  (should (equal "bump: foo"
                 (magit-gh-bump--make-bump-message '(("vendor/foo" . "foo"))))))

(ert-deftest magit-gh-bump/make-bump-message-multiple ()
  "Multiple dirty submodules are joined with \", \"."
  (should (equal "bump: foo, bar"
                 (magit-gh-bump--make-bump-message
                  '(("vendor/foo" . "foo") ("vendor/bar" . "bar"))))))

(ert-deftest magit-gh-bump/make-bump-message-nested-relpath ()
  "Name is the basename of the relative path, not the full path."
  (should (equal "bump: mylib"
                 (magit-gh-bump--make-bump-message
                  '(("packages/mylib" . "mylib"))))))

;;;; magit-gh-bump--path-depth

(ert-deftest magit-gh-bump/path-depth-root ()
  "Root path has depth 0 (no named components)."
  (should (= 0 (magit-gh-bump--path-depth "/"))))

(ert-deftest magit-gh-bump/path-depth-two ()
  "One-segment path has depth 1."
  (should (= 1 (magit-gh-bump--path-depth "/home"))))

(ert-deftest magit-gh-bump/path-depth-nested ()
  "Three-segment path has depth 3."
  (should (= 3 (magit-gh-bump--path-depth "/home/user/projects"))))

(ert-deftest magit-gh-bump/path-depth-trailing-slash ()
  "Trailing slash does not add an extra component."
  (should (= 2 (magit-gh-bump--path-depth "/home/user/"))))

;;;; magit-gh-bump--args-flag

(ert-deftest magit-gh-bump/args-flag-present ()
  "Returns t when the flag is in the args list."
  (should (magit-gh-bump--args-flag '("--fetch" "--push") "--fetch")))

(ert-deftest magit-gh-bump/args-flag-absent ()
  "Returns nil when the flag is not in the args list."
  (should-not (magit-gh-bump--args-flag '("--fetch") "--pull")))

(ert-deftest magit-gh-bump/args-flag-empty-list ()
  "Returns nil for an empty args list."
  (should-not (magit-gh-bump--args-flag nil "--push")))

;;;; magit-gh-bump--parse-dirty-submodule-lines

(ert-deftest magit-gh-bump/parse-dirty-lines-empty ()
  "Empty input returns nil."
  (should (null (magit-gh-bump--parse-dirty-submodule-lines nil))))

(ert-deftest magit-gh-bump/parse-dirty-lines-all-clean ()
  "Lines without `+' prefix return nil."
  (should (null (magit-gh-bump--parse-dirty-submodule-lines
                 '(" abc123 vendor/foo (v1.0)"
                   "-000000 vendor/bar")))))

(ert-deftest magit-gh-bump/parse-dirty-lines-one-dirty ()
  "A single `+' line produces one (rel-path . name) pair."
  (let ((result (magit-gh-bump--parse-dirty-submodule-lines
                 '("+deadbeef vendor/foo (v1.1)"))))
    (should (= 1 (length result)))
    (should (equal "vendor/foo" (caar result)))
    (should (equal "foo" (cdar result)))))

(ert-deftest magit-gh-bump/parse-dirty-lines-mixed ()
  "Only `+' lines are returned when input contains mixed statuses."
  (let ((result (magit-gh-bump--parse-dirty-submodule-lines
                 '(" abc123 vendor/clean"
                   "+deadbeef vendor/dirty"
                   "-000000 vendor/uninit"))))
    (should (= 1 (length result)))
    (should (equal "vendor/dirty" (caar result)))))

(ert-deftest magit-gh-bump/parse-dirty-lines-nested-relpath ()
  "Submodule with a nested relative path uses basename as name."
  (let ((result (magit-gh-bump--parse-dirty-submodule-lines
                 '("+abc123 packages/mylib (v2.0)"))))
    (should (equal "packages/mylib" (caar result)))
    (should (equal "mylib" (cdar result)))))

(ert-deftest magit-gh-bump/parse-dirty-lines-multiple-dirty ()
  "Multiple `+' lines each produce a pair."
  (let ((result (magit-gh-bump--parse-dirty-submodule-lines
                 '("+aaa vendor/a"
                   "+bbb vendor/b"
                   " ccc vendor/c"))))
    (should (= 2 (length result)))))

;;;; magit-gh-bump--parse-all-submodule-relpaths

(ert-deftest magit-gh-bump/parse-all-submodule-relpaths-empty ()
  "Empty input returns nil."
  (should (null (magit-gh-bump--parse-all-submodule-relpaths nil))))

(ert-deftest magit-gh-bump/parse-all-submodule-relpaths-mixed ()
  "All initialized submodules are returned regardless of status prefix."
  (let ((result (magit-gh-bump--parse-all-submodule-relpaths
                 '(" abc123 vendor/clean"
                   "+deadbeef vendor/dirty"
                   "Uabc123 vendor/conflict"))))
    (should (= 3 (length result)))
    (should (member "vendor/clean" result))
    (should (member "vendor/dirty" result))
    (should (member "vendor/conflict" result))))

(ert-deftest magit-gh-bump/parse-all-submodule-relpaths-blank-lines ()
  "Blank lines in input are ignored."
  (let ((result (magit-gh-bump--parse-all-submodule-relpaths
                 '(" abc123 vendor/foo" "" " def456 vendor/bar"))))
    (should (= 2 (length result)))))

;;;; magit-gh-bump--parse-porcelain-tracked

(ert-deftest magit-gh-bump/parse-porcelain-tracked-empty ()
  "Empty input returns nil."
  (should (null (magit-gh-bump--parse-porcelain-tracked nil))))

(ert-deftest magit-gh-bump/parse-porcelain-tracked-modified ()
  "Modified file returns its path."
  (should (equal '("src/main.go")
                 (magit-gh-bump--parse-porcelain-tracked '(" M src/main.go")))))

(ert-deftest magit-gh-bump/parse-porcelain-excludes-untracked ()
  "`??' lines are excluded from the result."
  (let ((result (magit-gh-bump--parse-porcelain-tracked
                 '(" M src/main.go"
                   "?? untracked.txt"))))
    (should (= 1 (length result)))
    (should (equal "src/main.go" (car result)))))

(ert-deftest magit-gh-bump/parse-porcelain-tracked-submodule ()
  "A modified submodule entry appears as its directory path."
  (should (equal '("vendor/foo")
                 (magit-gh-bump--parse-porcelain-tracked '(" M vendor/foo")))))

(ert-deftest magit-gh-bump/parse-porcelain-tracked-staged ()
  "Staged files (index status) are also included."
  (let ((result (magit-gh-bump--parse-porcelain-tracked
                 '("M  staged.go" " M unstaged.go"))))
    (should (= 2 (length result)))))

;;;; magit-gh-bump--all-changes-are-submodules-p

(ert-deftest magit-gh-bump/all-changes-submodules-empty-tracked ()
  "No tracked changes means all changes are trivially submodule-only."
  (should (magit-gh-bump--all-changes-are-submodules-p
           nil '("vendor/foo"))))

(ert-deftest magit-gh-bump/all-changes-submodules-exact-match ()
  "Tracked paths that exactly match dirty submodule paths return t."
  (should (magit-gh-bump--all-changes-are-submodules-p
           '("vendor/foo" "vendor/bar")
           '("vendor/foo" "vendor/bar"))))

(ert-deftest magit-gh-bump/all-changes-submodules-subset ()
  "Tracked paths that are a subset of dirty submodule paths return t."
  (should (magit-gh-bump--all-changes-are-submodules-p
           '("vendor/foo")
           '("vendor/foo" "vendor/bar"))))

(ert-deftest magit-gh-bump/all-changes-submodules-non-submodule ()
  "A tracked path not in dirty submodules returns nil."
  (should-not (magit-gh-bump--all-changes-are-submodules-p
               '("vendor/foo" "src/main.go")
               '("vendor/foo"))))

(ert-deftest magit-gh-bump/all-changes-submodules-no-dirty ()
  "Tracked changes with an empty dirty list returns nil."
  (should-not (magit-gh-bump--all-changes-are-submodules-p
               '("vendor/foo") nil)))

;;;; Integration tests using real temporary git repositories

(defun magit-gh-bump-test--run-git (dir &rest args)
  "Run git ARGS synchronously in DIR; return the exit code."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (apply #'call-process "git" nil nil nil args)))

(defun magit-gh-bump-test--init-repo (dir)
  "Create a minimal git repository at DIR with one commit."
  (make-directory dir t)
  (magit-gh-bump-test--run-git dir "init")
  (magit-gh-bump-test--run-git dir "config" "user.email" "test@test.test")
  (magit-gh-bump-test--run-git dir "config" "user.name" "Test")
  (with-temp-file (expand-file-name "README" dir) (insert "init\n"))
  (magit-gh-bump-test--run-git dir "add" ".")
  (magit-gh-bump-test--run-git dir "commit" "-m" "init"))

;; git submodule add with a local file:// path requires protocol.file.allow=always
;; on git >= 2.38.1 (security restriction CVE-2022-39253).
(defun magit-gh-bump-test--add-submodule (parent-dir sub-path rel-name)
  "Add SUB-PATH as a submodule named REL-NAME in PARENT-DIR."
  (magit-gh-bump-test--run-git
   parent-dir "-c" "protocol.file.allow=always"
   "submodule" "add" sub-path rel-name))

(ert-deftest magit-gh-bump/repo-root-p-valid ()
  "Returns t for a real git repository root."
  (let ((dir (make-temp-file "magit-gh-bump-test" t)))
    (unwind-protect
        (progn
          (magit-gh-bump-test--init-repo dir)
          (should (magit-gh-bump--repo-root-p dir)))
      (delete-directory dir t))))

(ert-deftest magit-gh-bump/repo-root-p-non-repo ()
  "Returns nil for a plain directory without a .git entry."
  (let ((dir (make-temp-file "magit-gh-bump-test" t)))
    (unwind-protect
        (should-not (magit-gh-bump--repo-root-p dir))
      (delete-directory dir t))))

(ert-deftest magit-gh-bump/dirty-submodule-pairs-no-submodules ()
  "Returns nil for a repo with no submodules."
  (let ((dir (make-temp-file "magit-gh-bump-test" t)))
    (unwind-protect
        (progn
          (magit-gh-bump-test--init-repo dir)
          (should (null (magit-gh-bump--dirty-submodule-pairs dir))))
      (delete-directory dir t))))

(ert-deftest magit-gh-bump/walk-tree-no-submodules ()
  "Returns a list containing only the root when there are no submodules."
  (let ((dir (make-temp-file "magit-gh-bump-test" t)))
    (unwind-protect
        (progn
          (magit-gh-bump-test--init-repo dir)
          (let ((tree (magit-gh-bump--walk-tree dir)))
            (should (= 1 (length tree)))
            (should (equal dir (car tree)))))
      (delete-directory dir t))))

(ert-deftest magit-gh-bump/parent-repo-standalone ()
  "Returns nil for a repo that is not a submodule of anything."
  (let ((dir (make-temp-file "magit-gh-bump-test" t)))
    (unwind-protect
        (progn
          (magit-gh-bump-test--init-repo dir)
          (should (null (magit-gh-bump--parent-repo dir))))
      (delete-directory dir t))))

(ert-deftest magit-gh-bump/dirty-submodule-pairs-with-new-commit ()
  "Returns the submodule as dirty after a new commit is made inside it."
  (let* ((base (make-temp-file "magit-gh-bump-test" t))
         (sub (expand-file-name "sub" base))
         (parent (expand-file-name "parent" base)))
    (unwind-protect
        (progn
          (magit-gh-bump-test--init-repo sub)
          (magit-gh-bump-test--init-repo parent)
          (magit-gh-bump-test--add-submodule parent sub "sub")
          (magit-gh-bump-test--run-git parent "commit" "-m" "add submodule")
          ;; Commit in the CHECKED-OUT submodule (parent/sub), not the source
          (let ((sub-in-parent (expand-file-name "sub" parent)))
            (magit-gh-bump-test--run-git sub-in-parent "config" "user.email" "t@t.t")
            (magit-gh-bump-test--run-git sub-in-parent "config" "user.name" "T")
            (with-temp-file (expand-file-name "extra.txt" sub-in-parent)
              (insert "new content\n"))
            (magit-gh-bump-test--run-git sub-in-parent "add" ".")
            (magit-gh-bump-test--run-git sub-in-parent "commit" "-m" "new commit")
            ;; Parent's recorded hash is now behind the checked-out submodule HEAD
            (let ((dirty (magit-gh-bump--dirty-submodule-pairs parent)))
              (should (= 1 (length dirty)))
              (should (equal "sub" (caar dirty)))
              (should (equal "sub" (cdar dirty))))))
      (delete-directory base t))))

(ert-deftest magit-gh-bump/parent-repo-finds-parent ()
  "Returns the parent path when PATH is a known submodule."
  (let* ((base (make-temp-file "magit-gh-bump-test" t))
         (sub (expand-file-name "sub" base))
         (parent (expand-file-name "parent" base)))
    (unwind-protect
        (progn
          (magit-gh-bump-test--init-repo sub)
          (magit-gh-bump-test--init-repo parent)
          (magit-gh-bump-test--add-submodule parent sub "sub")
          (magit-gh-bump-test--run-git parent "commit" "-m" "add submodule")
          (let ((sub-abs (expand-file-name "sub" parent)))
            (should (equal parent (magit-gh-bump--parent-repo sub-abs)))))
      (delete-directory base t))))

(ert-deftest magit-gh-bump/topmost-parent-no-parent ()
  "Returns PATH itself when there is no parent."
  (let ((dir (make-temp-file "magit-gh-bump-test" t)))
    (unwind-protect
        (progn
          (magit-gh-bump-test--init-repo dir)
          (should (equal dir (magit-gh-bump--topmost-parent dir))))
      (delete-directory dir t))))

(ert-deftest magit-gh-bump/topmost-parent-one-level ()
  "Returns the parent when PATH is a direct submodule."
  (let* ((base (make-temp-file "magit-gh-bump-test" t))
         (sub (expand-file-name "sub" base))
         (parent (expand-file-name "parent" base)))
    (unwind-protect
        (progn
          (magit-gh-bump-test--init-repo sub)
          (magit-gh-bump-test--init-repo parent)
          (magit-gh-bump-test--add-submodule parent sub "sub")
          (magit-gh-bump-test--run-git parent "commit" "-m" "add submodule")
          (let ((sub-abs (expand-file-name "sub" parent)))
            (should (equal parent (magit-gh-bump--topmost-parent sub-abs)))))
      (delete-directory base t))))

(ert-deftest magit-gh-bump/only-submodule-changes-clean-repo ()
  "A repo with no changes is considered submodule-only (vacuously true)."
  (let ((dir (make-temp-file "magit-gh-bump-test" t)))
    (unwind-protect
        (progn
          (magit-gh-bump-test--init-repo dir)
          (should (magit-gh-bump--only-submodule-changes-p dir nil)))
      (delete-directory dir t))))

(provide 'test-magit-gh-bump-submodules)
;;; test-magit-gh-bump-submodules.el ends here
