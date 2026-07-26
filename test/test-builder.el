;;; test-builder.el --- ERT tests for builder.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for compilation candidate structs, table operations,
;; directory helpers, registration macro, and cache management.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ht)

(require 'builder)
(require 'sprite)
(require 'sprite-direct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder-candidate struct

(ert-deftest builder-test/candidate-make-with-command ()
  "make-builder-candidate sets :command and derives :name from it."
  (let ((c (make-builder-candidate :command "go build ./...")))
    (should (builder-candidate-p c))
    (should (equal "go build ./..." (builder-candidate-command c)))
    (should (stringp (builder-candidate-name c)))))

(ert-deftest builder-test/candidate-explicit-name ()
  ":name overrides the default derived from :command."
  (let ((c (make-builder-candidate :command "make all" :name "build")))
    (should (equal "build" (builder-candidate-name c)))
    (should (equal "make all" (builder-candidate-command c)))))

(ert-deftest builder-test/candidate-name-truncated ()
  "Long commands are truncated in the default :name."
  (let* ((cmd (make-string 64 ?x))
         (c (make-builder-candidate :command cmd)))
    (should (<= (length (builder-candidate-name c)) 35))))

(ert-deftest builder-test/candidate-default-directory ()
  ":directory defaults to `default-directory'."
  (let* ((default-directory "/tmp/")
         (c (make-builder-candidate :command "echo hi")))
    (should (equal "/tmp/" (builder-candidate-directory c)))))

(ert-deftest builder-test/candidate-explicit-directory ()
  "Explicit :directory is stored verbatim."
  (let ((c (make-builder-candidate :command "echo" :directory "/home/user/")))
    (should (equal "/home/user/" (builder-candidate-directory c)))))

(ert-deftest builder-test/candidate-default-annotation ()
  "Default annotation is non-nil and a string."
  (let ((c (make-builder-candidate :command "cargo test")))
    (should (stringp (builder-candidate-annotation c)))))

(ert-deftest builder-test/candidate-explicit-annotation ()
  "Explicit :annotation overrides the default."
  (let ((c (make-builder-candidate :command "cargo test" :annotation "run tests")))
    (should (equal "run tests" (builder-candidate-annotation c)))))

(ert-deftest builder-test/candidate-default-hook-is-nil ()
  "Default :hook value is nil."
  (let ((c (make-builder-candidate :command "make")))
    (should (null (builder-candidate-hook c)))))

(ert-deftest builder-test/candidate-hook-stored ()
  "Stored :hook is retrievable."
  (let* ((fn (lambda () nil))
         (c (make-builder-candidate :command "make" :hook fn)))
    (should (eq fn (builder-candidate-hook c)))))

(ert-deftest builder-test/candidate-default-notification-threshold ()
  ":notification-threshold defaults to 30."
  (let ((c (make-builder-candidate :command "make")))
    (should (equal 30 (builder-candidate-notification-threshold c)))))

(ert-deftest builder-test/candidate-predicate ()
  "`builder-candidate-p' returns t for structs and nil for other values."
  (should (builder-candidate-p (make-builder-candidate :command "x")))
  (should-not (builder-candidate-p nil))
  (should-not (builder-candidate-p "string"))
  (should-not (builder-candidate-p 42))
  (should-not (builder-candidate-p '(a b c))))

(ert-deftest builder-test/candidate-annotation-differs-when-name-differs ()
  "When name != command, annotation embeds the command string."
  (let ((c (make-builder-candidate :command "go build ./..." :name "build")))
    (should (string-match-p "go build" (builder-candidate-annotation c)))))

(ert-deftest builder-test/candidate-annotation-same-when-name-equals-command ()
  "When name equals command, annotation is just the command itself."
  (let ((c (make-builder-candidate :command "make" :name "make")))
    (should (equal "make" (builder-candidate-annotation c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder--add-candidate / builder--add-candidates

(ert-deftest builder-test/add-candidate-inserts-by-name ()
  "Adding a candidate stores it under its name key."
  (let ((tbl (make-hash-table :test #'equal))
        (c (make-builder-candidate :command "make" :name "build")))
    (builder--add-candidate tbl c)
    (should (ht-contains-p tbl "build"))
    (should (eq c (ht-get tbl "build")))))

(ert-deftest builder-test/add-candidate-overwrites-existing ()
  "Adding a candidate with an existing name replaces the previous entry."
  (let ((tbl (make-hash-table :test #'equal))
        (c1 (make-builder-candidate :command "make" :name "build"))
        (c2 (make-builder-candidate :command "make -j4" :name "build")))
    (builder--add-candidate tbl c1)
    (builder--add-candidate tbl c2)
    (should (eq c2 (ht-get tbl "build")))))

(ert-deftest builder-test/add-candidates-filters-non-candidates ()
  "Non-candidate values in the list are silently ignored."
  (let ((tbl (make-hash-table :test #'equal))
        (c (make-builder-candidate :command "make" :name "build")))
    (builder--add-candidates tbl (list c nil "not-a-candidate" 42))
    (should (= 1 (ht-size tbl)))
    (should (ht-contains-p tbl "build"))))

(ert-deftest builder-test/add-candidates-all-valid ()
  "All valid candidates are inserted."
  (let ((tbl (make-hash-table :test #'equal))
        (c1 (make-builder-candidate :command "make" :name "build"))
        (c2 (make-builder-candidate :command "make test" :name "test"))
        (c3 (make-builder-candidate :command "make lint" :name "lint")))
    (builder--add-candidates tbl (list c1 c2 c3))
    (should (= 3 (ht-size tbl)))
    (should (ht-contains-p tbl "build"))
    (should (ht-contains-p tbl "test"))
    (should (ht-contains-p tbl "lint"))))

(ert-deftest builder-test/add-candidates-empty-list ()
  "Empty input list leaves the table unchanged."
  (let ((tbl (make-hash-table :test #'equal)))
    (builder--add-candidates tbl nil)
    (should (= 0 (ht-size tbl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder--project-compilation-buffers

(ert-deftest builder-test/project-compilation-buffers-returns-hash ()
  "`builder--project-compilation-buffers' returns a hash table."
  (should (ht-p (builder--project-compilation-buffers))))

(ert-deftest builder-test/project-compilation-buffers-has-default-names ()
  "Default buffer name patterns are present in the returned table."
  (let* ((project "test-proj")
         (tbl (builder--project-compilation-buffers :project project))
         (keys (ht-keys tbl)))
    (should (cl-some (lambda (k) (string-match-p "test" k)) keys))
    (should (cl-some (lambda (k) (string-match-p "build" k)) keys))))

(ert-deftest builder-test/project-compilation-buffers-includes-name ()
  "When :name is provided, a buffer with that name is included."
  (let* ((tbl (builder--project-compilation-buffers :name "mycmd" :project "p"))
         (keys (ht-keys tbl)))
    (should (cl-some (lambda (k) (string-match-p "mycmd" k)) keys))))

(ert-deftest builder-test/project-compilation-buffers-annotations-are-strings ()
  "All annotations in the table are strings."
  (let ((tbl (builder--project-compilation-buffers :project "p")))
    (ht-each (lambda (_k v) (should (stringp v))) tbl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder-register-candidates macro

(ert-deftest builder-test/register-candidates-defines-function ()
  "The macro defines a function named `builder-candidates-for-<name>'."
  (builder-register-candidates
   :name "test-reg-simple"
   :pipeline (list (make-builder-candidate :command "echo hi" :name "hi")))
  (should (fboundp 'builder-candidates-for-test-reg-simple)))

(ert-deftest builder-test/register-candidates-function-populates-table ()
  "The generated function adds candidates to the provided table."
  (builder-register-candidates
   :name "test-reg-pop"
   :pipeline (list (make-builder-candidate :command "echo pop" :name "pop")))
  (let ((tbl (make-hash-table :test #'equal)))
    (builder-candidates-for-test-reg-pop "/" "proj" (list "/") tbl)
    (should (ht-contains-p tbl "pop"))))

(ert-deftest builder-test/register-candidates-returns-t ()
  "The generated function always returns t."
  (builder-register-candidates
   :name "test-reg-ret"
   :pipeline nil)
  (let ((tbl (make-hash-table :test #'equal)))
    (should (eq t (builder-candidates-for-test-reg-ret "/" "proj" (list "/") tbl)))))

(ert-deftest builder-test/register-candidates-with-false-predicate ()
  "When predicate is nil, no candidates are added to the table."
  (builder-register-candidates
   :name "test-reg-pred"
   :predicate nil
   :pipeline (list (make-builder-candidate :command "never" :name "never")))
  (let ((tbl (make-hash-table :test #'equal)))
    (builder-candidates-for-test-reg-pred "/" "proj" (list "/") tbl)
    (should (= 0 (ht-size tbl)))))

(ert-deftest builder-test/register-candidates-adds-to-global-hook-no-hooks ()
  "Without :hooks, the candidate function is added to `builder-candidate-functions'."
  (let ((builder-candidate-functions nil))
    (builder-register-candidates
     :name "test-reg-glob"
     :pipeline nil)
    (should (memq 'builder-candidates-for-test-reg-glob builder-candidate-functions))))

(ert-deftest builder-test/register-candidates-with-hooks-defines-registrar ()
  "With :hooks, a registrar function named `builder-candidate-registrar-for-<name>' is defined."
  (builder-register-candidates
   :name "test-reg-hook"
   :hooks '(some-hook)
   :pipeline nil)
  (should (fboundp 'builder-candidate-registrar-for-test-reg-hook)))

(ert-deftest builder-test/register-candidates-multiple-candidates ()
  "Multiple candidates from pipeline are all added."
  (builder-register-candidates
   :name "test-reg-multi"
   :pipeline (list (make-builder-candidate :command "cmd1" :name "c1")
                   (make-builder-candidate :command "cmd2" :name "c2")
                   (make-builder-candidate :command "cmd3" :name "c3")))
  (let ((tbl (make-hash-table :test #'equal)))
    (builder-candidates-for-test-reg-multi "/" "proj" (list "/") tbl)
    (should (= 3 (ht-size tbl)))
    (should (ht-contains-p tbl "c1"))
    (should (ht-contains-p tbl "c2"))
    (should (ht-contains-p tbl "c3"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder-candidate :priority slot

(ert-deftest builder-test/candidate-default-priority ()
  "Default :priority is 2."
  (let ((c (make-builder-candidate :command "make")))
    (should (= 2 (builder-candidate-priority c)))))

(ert-deftest builder-test/candidate-explicit-priority ()
  "Explicit :priority is stored verbatim."
  (let ((c (make-builder-candidate :command "vale --output=line f.org" :priority 0)))
    (should (= 0 (builder-candidate-priority c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder--candidate-priority

(ert-deftest builder-test/candidate-priority-from-table ()
  "Returns the candidate's priority when the name is in the table."
  (let ((tbl (make-hash-table :test #'equal))
        (c (make-builder-candidate :command "make" :name "build" :priority 1)))
    (ht-set tbl "build" c)
    (should (= 1 (builder--candidate-priority tbl "build")))))

(ert-deftest builder-test/candidate-priority-default-when-missing ()
  "Returns 2 for names not present in the table."
  (should (= 2 (builder--candidate-priority (make-hash-table :test #'equal) "unknown"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder--make-sort-fn

(ert-deftest builder-test/make-sort-fn-returns-function ()
  "`builder--make-sort-fn' returns a callable function."
  (should (functionp (builder--make-sort-fn (make-hash-table :test #'equal) 'test-key))))

(ert-deftest builder-test/make-sort-fn-history-before-non-history ()
  "Items present in history sort before items not in history."
  (let* ((annotated-completing-read-history (make-hash-table :test #'equal))
         (tbl (make-hash-table :test #'equal)))
    (ht-set tbl "fresh" (make-builder-candidate :command "x" :name "fresh" :priority 2))
    (ht-set tbl "prior" (make-builder-candidate :command "y" :name "prior" :priority 2))
    (ht-set annotated-completing-read-history 'h '("prior"))
    (let ((sorted (funcall (builder--make-sort-fn tbl 'h) '("fresh" "prior"))))
      (should (equal "prior" (car sorted))))))

(ert-deftest builder-test/make-sort-fn-history-recency-order ()
  "More recent history entries (lower index) sort before older ones."
  (let* ((annotated-completing-read-history (make-hash-table :test #'equal))
         (tbl (make-hash-table :test #'equal)))
    (ht-set tbl "a" (make-builder-candidate :command "a" :name "a" :priority 2))
    (ht-set tbl "b" (make-builder-candidate :command "b" :name "b" :priority 2))
    ;; history list: index 0 = most recent = "a", index 1 = "b"
    (ht-set annotated-completing-read-history 'h '("a" "b"))
    (let ((sorted (funcall (builder--make-sort-fn tbl 'h) '("b" "a"))))
      (should (equal "a" (car sorted))))))

(ert-deftest builder-test/make-sort-fn-priority-order-among-non-history ()
  "Lower priority number sorts first among items not in history."
  (let* ((annotated-completing-read-history (make-hash-table :test #'equal))
         (tbl (make-hash-table :test #'equal)))
    (ht-set tbl "tier0" (make-builder-candidate :command "a" :name "tier0" :priority 0))
    (ht-set tbl "tier2" (make-builder-candidate :command "b" :name "tier2" :priority 2))
    (ht-set tbl "tier3" (make-builder-candidate :command "c" :name "tier3" :priority 3))
    (let ((sorted (funcall (builder--make-sort-fn tbl 'empty) '("tier3" "tier2" "tier0"))))
      (should (equal "tier0" (car sorted)))
      (should (equal "tier3" (car (last sorted)))))))

(ert-deftest builder-test/make-sort-fn-length-tiebreaker ()
  "Among same-priority non-history items, shorter key sorts first."
  (let* ((annotated-completing-read-history (make-hash-table :test #'equal))
         (tbl (make-hash-table :test #'equal)))
    (ht-set tbl "short" (make-builder-candidate :command "x" :name "short" :priority 2))
    (ht-set tbl "longer-name" (make-builder-candidate :command "y" :name "longer-name" :priority 2))
    (let ((sorted (funcall (builder--make-sort-fn tbl 'empty) '("longer-name" "short"))))
      (should (equal "short" (car sorted))))))

(ert-deftest builder-test/make-sort-fn-does-not-mutate-input ()
  "The input list is not mutated (uses copy-sequence internally)."
  (let* ((annotated-completing-read-history (make-hash-table :test #'equal))
         (tbl (make-hash-table :test #'equal))
         (input (list "b" "a")))
    (ht-set tbl "a" (make-builder-candidate :command "a" :name "a"))
    (ht-set tbl "b" (make-builder-candidate :command "b" :name "b"))
    (funcall (builder--make-sort-fn tbl 'empty) input)
    (should (equal "b" (car input)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder-add-candidates

(ert-deftest builder-test/add-candidates-registers-fn ()
  "`builder-add-candidates' adds the function to `builder-candidate-functions'."
  (let ((builder-candidate-functions nil))
    (defun builder-test--gen-reg (root name dirs table)
      (ignore root name dirs table) t)
    (builder-add-candidates #'builder-test--gen-reg)
    (should (memq #'builder-test--gen-reg builder-candidate-functions))))

(ert-deftest builder-test/add-candidates-multiple-independent ()
  "Multiple `builder-add-candidates' calls each register independently."
  (let ((builder-candidate-functions nil))
    (defun builder-test--gen-x (root name dirs table) (ignore root name dirs table) t)
    (defun builder-test--gen-y (root name dirs table) (ignore root name dirs table) t)
    (builder-add-candidates #'builder-test--gen-x)
    (builder-add-candidates #'builder-test--gen-y)
    (should (memq #'builder-test--gen-x builder-candidate-functions))
    (should (memq #'builder-test--gen-y builder-candidate-functions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-local candidate cache

(ert-deftest builder-test/candidate-cache-p-false-when-empty ()
  "`builder--candidate-cache-p' returns nil when buffer has no cached candidates."
  (with-temp-buffer
    (setq-local builder--cached-candidates nil)
    (should-not (builder--candidate-cache-p (current-buffer)))))

(ert-deftest builder-test/candidate-cache-p-true-when-set ()
  "`builder--candidate-cache-p' returns t when buffer has cached candidates."
  (with-temp-buffer
    (setq-local builder--cached-candidates (make-hash-table :test #'equal))
    (should (builder--candidate-cache-p (current-buffer)))))

(ert-deftest builder-test/clear-candidate-cache-removes-entry ()
  "`builder--clear-candidate-cache' clears cached candidates for the buffer."
  (with-temp-buffer
    (setq-local builder--cached-candidates (make-hash-table :test #'equal))
    (builder--clear-candidate-cache (current-buffer))
    (should-not (buffer-local-value 'builder--cached-candidates (current-buffer)))))

(ert-deftest builder-test/clear-candidate-cache-leaves-other-buffers ()
  "Clearing cache in one buffer does not affect another buffer's cache."
  (let ((buf-a (generate-new-buffer " *builder-test-a*"))
        (buf-b (generate-new-buffer " *builder-test-b*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (setq-local builder--cached-candidates (make-hash-table :test #'equal)))
          (with-current-buffer buf-b
            (setq-local builder--cached-candidates (make-hash-table :test #'equal)))
          (builder--clear-candidate-cache buf-a)
          (should-not (buffer-local-value 'builder--cached-candidates buf-a))
          (should (buffer-local-value 'builder--cached-candidates buf-b)))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(ert-deftest builder-test/clear-all-caches-clears-all-buffers ()
  "`builder-clear-all-caches' clears cached candidates in every buffer that has them."
  (let ((buf-a (generate-new-buffer " *builder-test-a*"))
        (buf-b (generate-new-buffer " *builder-test-b*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (setq-local builder--cached-candidates (make-hash-table :test #'equal)))
          (with-current-buffer buf-b
            (setq-local builder--cached-candidates (make-hash-table :test #'equal)))
          (builder-clear-all-caches)
          (should-not (buffer-local-value 'builder--cached-candidates buf-a))
          (should-not (buffer-local-value 'builder--cached-candidates buf-b)))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder--buffer-ran-command-p / builder--buffer-for-command

(ert-deftest builder-test/buffer-ran-command-p-true-when-matches ()
  "Returns t when `compilation-arguments' matches the command."
  (with-temp-buffer
    (setq-local compilation-arguments '("make test"))
    (should (builder--buffer-ran-command-p (current-buffer) "make test"))))

(ert-deftest builder-test/buffer-ran-command-p-false-when-different ()
  "Returns nil when `compilation-arguments' holds a different command."
  (with-temp-buffer
    (setq-local compilation-arguments '("make build"))
    (should-not (builder--buffer-ran-command-p (current-buffer) "make test"))))

(ert-deftest builder-test/buffer-ran-command-p-trims-whitespace ()
  "Leading/trailing whitespace is ignored in the comparison."
  (with-temp-buffer
    (setq-local compilation-arguments '("  make test  "))
    (should (builder--buffer-ran-command-p (current-buffer) "make test"))))

(ert-deftest builder-test/buffer-ran-command-p-nil-args ()
  "Returns nil when `compilation-arguments' is nil."
  (with-temp-buffer
    (setq-local compilation-arguments nil)
    (should-not (builder--buffer-ran-command-p (current-buffer) "make test"))))

(ert-deftest builder-test/buffer-for-command-nil-when-no-match ()
  "`builder--buffer-for-command' returns nil when no buffer ran the command."
  (cl-letf (((symbol-function 'mode-buffers-for-project) (lambda (&rest _) nil)))
    (should (null (builder--buffer-for-command "make test")))))

(ert-deftest builder-test/buffer-for-command-returns-name-of-matching-buffer ()
  "Returns the buffer name when a compilation buffer ran the command."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (rename-buffer "*test-compile*" t)
      (setq-local compilation-arguments '("cargo test"))
      (cl-letf (((symbol-function 'mode-buffers-for-project)
                 (lambda (&rest _) (list buf))))
        (should (equal "*test-compile*" (builder--buffer-for-command "cargo test")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder--read-command (unit: table construction path)

(ert-deftest builder-test/read-command-cons-pair-from-existing-table ()
  "`builder--read-command' with an explicit table returns a cons of
the selected candidate (resolved via ACR's target support) and the table."
  (let* ((tbl (make-hash-table :test #'equal))
         (c (make-builder-candidate :command "make" :name "build")))
    (ht-set tbl "build" c)
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest _) c)))
      (let ((result (builder--read-command nil tbl)))
        (should (consp result))
        (should (eq c (car result)))
        (should (eq tbl (cdr result)))))))

(ert-deftest builder-test/read-command-unknown-selection-adds-candidate ()
  "When the user types a string not in the table, a new candidate is created."
  (let* ((tbl (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest _) "custom-cmd"))
              ((symbol-function 'builder--get-candidates)
               (lambda (&rest _) tbl)))
      (let ((result (builder--read-command nil tbl)))
        (should (consp result))
        (should (builder-candidate-p (car result)))
        (should (equal "custom-cmd" (builder-candidate-command (car result))))
        (should (ht-contains-p (cdr result) "custom-cmd"))))))

(ert-deftest builder-test/read-command-unknown-long-selection-name-matches-key ()
  "Regression: for a typed command over 32 chars, the new candidate is stored
under its truncated name and the same candidate object is returned directly,
so callers no longer need to look it back up with `map-elt'."
  (let* ((tbl (make-hash-table :test #'equal))
         (long-cmd (make-string 64 ?x)))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest _) long-cmd))
              ((symbol-function 'builder--get-candidates)
               (lambda (&rest _) tbl)))
      (let* ((result (builder--read-command nil tbl))
             (candidate (car result))
             (candidates (cdr result))
             (name (builder-candidate-name candidate)))
        (should (ht-contains-p candidates name))
        (should (eq candidate (map-elt candidates name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder-reset-finish-hooks

(ert-deftest builder-test/reset-finish-hooks-clears-local-hooks ()
  "`builder-reset-finish-hooks' clears `compilation-finish-functions' locally."
  (with-temp-buffer
    (compilation-mode)
    (setq-local compilation-finish-functions (list (lambda () nil)))
    (cl-letf (((symbol-function 'mode-buffers)
               (lambda (_mode) (list (current-buffer)))))
      (builder-reset-finish-hooks))
    (should (null compilation-finish-functions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; isolated subprocess checks (byte-compile/load/test)

(defun builder-test--write-temp-el (contents)
  "Write CONTENTS to a fresh temp .el file and return its absolute path.
Since it is already absolute, `expand-file-name' against
`user-emacs-directory' (as callers do with FILE) leaves it unchanged."
  (make-temp-file "builder-test-isolated-" nil ".el"
                  (concat ";; -*- lexical-binding: t; -*-\n" contents)))

(ert-deftest builder-test/report-log-creates-and-replaces-buffer ()
  "`builder-emacs-conf--report-log' (re)populates a named buffer with OUTPUT."
  (unwind-protect
      (progn
        (builder-emacs-conf--report-log "*builder-test-log*" "first")
        (should (equal "first" (with-current-buffer "*builder-test-log*" (buffer-string))))
        (builder-emacs-conf--report-log "*builder-test-log*" "second")
        (should (equal "second" (with-current-buffer "*builder-test-log*" (buffer-string)))))
    (kill-buffer "*builder-test-log*")))

(ert-deftest builder-test/report-log-works-on-read-only-buffer ()
  "Regression: a real `*Compile-Log*' is left read-only by `byte-compile-file',
so `builder-emacs-conf--report-log' must not error out on one."
  (unwind-protect
      (progn
        (with-current-buffer (get-buffer-create "*builder-test-log*")
          (setq buffer-read-only t))
        (builder-emacs-conf--report-log "*builder-test-log*" "replaced")
        (should (equal "replaced" (with-current-buffer "*builder-test-log*" (buffer-string)))))
    (kill-buffer "*builder-test-log*")))

(ert-deftest builder-test/byte-compile-and-delete-artifact-still-compiles-no-byte-compile-file ()
  "A file declaring `no-byte-compile: t' is still validated, not silently skipped."
  (let ((file (make-temp-file "builder-test-nbc-" nil ".el"
                              ";; -*- lexical-binding: t; no-byte-compile: t; -*-\n(defun builder-test-nbc-fn () (undefined-and-unmatched-paren\n")))
    (unwind-protect
        (should-not (builder-emacs-conf-byte-compile-and-delete-artifact file))
      (delete-file file))))

(ert-deftest builder-test/byte-compile-and-delete-artifact-passes-clean-file ()
  "A file with no compile errors returns t and leaves no .elc behind."
  (let ((file (builder-test--write-temp-el "(defun builder-test-clean-fn (x) (+ x 1))\n(provide 'builder-test-clean)\n")))
    (unwind-protect
        (progn
          (should (builder-emacs-conf-byte-compile-and-delete-artifact file))
          (should-not (file-exists-p (concat (file-name-sans-extension
                                              (expand-file-name file user-emacs-directory))
                                             ".elc"))))
      (delete-file (expand-file-name file user-emacs-directory)))))

(ert-deftest builder-test/byte-compile-and-delete-artifact-fails-on-syntax-error ()
  "A file with invalid read syntax returns nil, and the log is populated."
  (let ((file (builder-test--write-temp-el "(defun builder-test-broken-fn (x (+ x 1))\n")))
    (unwind-protect
        (progn
          (should-not (builder-emacs-conf-byte-compile-and-delete-artifact file))
          (should (> (length (with-current-buffer "*Compile-Log*" (buffer-string))) 0)))
      (delete-file (expand-file-name file user-emacs-directory)))))

(ert-deftest builder-test/load-check-passes-clean-file ()
  "A file that loads without error returns t."
  (let ((file (builder-test--write-temp-el "(defun builder-test-load-ok-fn () t)\n(provide 'builder-test-load-ok)\n")))
    (unwind-protect
        (should (builder-emacs-conf-load-check file))
      (delete-file (expand-file-name file user-emacs-directory)))))

(ert-deftest builder-test/load-check-fails-on-undefined-function-call ()
  "A file that calls an undefined function at load time returns nil.
This is the isolation regression case: the isolated subprocess has none
of this session's already-loaded features, so a call to a function this
file never requires fails to load instead of silently succeeding."
  (let ((file (builder-test--write-temp-el "(builder-test-totally-undefined-fn-xyz)\n")))
    (unwind-protect
        (progn
          (should-not (builder-emacs-conf-load-check file))
          (should (string-match-p "builder-test-totally-undefined-fn-xyz"
                                  (with-current-buffer "*builder-load-check-log*" (buffer-string)))))
      (delete-file (expand-file-name file user-emacs-directory)))))

(ert-deftest builder-test/elisp-package-test-isolated-passes-and-fails ()
  "Runs a throwaway elisp package's ERT suite in isolation, both ways."
  (let* ((root (file-name-as-directory (make-temp-file "builder-test-pkg-" t)))
         (test-dir (expand-file-name "test" root)))
    (unwind-protect
        (progn
          (make-directory test-dir t)
          (with-temp-file (expand-file-name (concat (file-name-nondirectory (directory-file-name root)) ".el") root)
            (insert ";; -*- lexical-binding: t; -*-\n(defun builder-test-pkg-add (a b) (+ a b))\n(provide '"
                    (file-name-nondirectory (directory-file-name root)) ")\n"))
          (with-temp-file (expand-file-name "test-pkg.el" test-dir)
            (insert "(ert-deftest builder-test-pkg/add () (should (= 3 (builder-test-pkg-add 1 2))))\n"))
          (should (builder-emacs-conf-elisp-package-test-isolated root))

          (with-temp-file (expand-file-name "test-pkg.el" test-dir)
            (insert "(ert-deftest builder-test-pkg/add () (should (= 4 (builder-test-pkg-add 1 2))))\n"))
          (should-not (builder-emacs-conf-elisp-package-test-isolated root)))
      (delete-directory root t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sprite transport (mocked -- no live daemon; see the `sprite' package's
;; own integration suite for real socket coverage of `sprite-direct')

(ert-deftest builder-test/sprite-eval-resolves-target-and-wraps-form ()
  "`builder-emacs-conf-sprite-eval' opens the sprite's target and sends
FORM wrapped in `(progn (require \\='builder) FORM)'."
  (let (sent-target sent-form
        (fake-sprite (sprite--make :name "worker-0")))
    (cl-letf (((symbol-function 'sprite-get-or-create-next) (lambda () fake-sprite))
              ((symbol-function 'sprite-direct-open)
               (lambda (target) (setq sent-target target) target))
              ((symbol-function 'sprite-direct-eval-non-blocking)
               (lambda (conn form) (setq sent-form form) (list 'promise conn form))))
      (let ((promise (builder-emacs-conf-sprite-eval '(+ 1 2))))
        (should (equal "worker-0" sent-target))
        (should (equal '(progn (require 'builder) (+ 1 2)) sent-form))
        (should (equal '(promise "worker-0" (progn (require 'builder) (+ 1 2))) promise))))))

(ert-deftest builder-test/sprite-eval-callback-wired-via-promise-then ()
  "CALLBACK is wired via `sprite-direct-promise-then' when given, and left
untouched when omitted."
  (let (then-called
        (fake-sprite (sprite--make :name "worker-0")))
    (cl-letf (((symbol-function 'sprite-get-or-create-next) (lambda () fake-sprite))
              ((symbol-function 'sprite-direct-open) #'identity)
              ((symbol-function 'sprite-direct-eval-non-blocking) (lambda (&rest _) 'fake-promise))
              ((symbol-function 'sprite-direct-promise-then)
               (lambda (promise callback) (setq then-called (list promise callback)))))
      (builder-emacs-conf-sprite-eval '(+ 1 1))
      (should-not then-called)
      (let ((cb (lambda (_state _value) nil)))
        (builder-emacs-conf-sprite-eval '(+ 1 1) :callback cb)
        (should (equal (list 'fake-promise cb) then-called))))))

(ert-deftest builder-test/sprite-byte-compile-check-sends-do-check-form ()
  "`builder-emacs-conf-sprite-byte-compile-check' evaluates
`builder-emacs-conf--do-byte-compile-check' with the expanded file path."
  (let (sent-form)
    (cl-letf (((symbol-function 'builder-emacs-conf-sprite-eval)
               (lambda (form &rest _) (setq sent-form form) 'fake-promise)))
      (should (eq 'fake-promise (builder-emacs-conf-sprite-byte-compile-check "lisp/foo.el")))
      (should (equal `(builder-emacs-conf--do-byte-compile-check
                       ,(expand-file-name "lisp/foo.el" user-emacs-directory))
                     sent-form)))))

(ert-deftest builder-test/sprite-load-check-sends-do-check-form ()
  "`builder-emacs-conf-sprite-load-check' evaluates
`builder-emacs-conf--do-load-check' with the expanded file path."
  (let (sent-form)
    (cl-letf (((symbol-function 'builder-emacs-conf-sprite-eval)
               (lambda (form &rest _) (setq sent-form form) 'fake-promise)))
      (should (eq 'fake-promise (builder-emacs-conf-sprite-load-check "lisp/foo.el")))
      (should (equal `(builder-emacs-conf--do-load-check
                       ,(expand-file-name "lisp/foo.el" user-emacs-directory))
                     sent-form)))))

(ert-deftest builder-test/sprite-test-check-sends-do-check-form-with-root ()
  "`builder-emacs-conf-sprite-test-check' evaluates
`builder-emacs-conf--do-elisp-package-test-check' with the expanded root."
  (let (sent-form)
    (cl-letf (((symbol-function 'builder-emacs-conf-sprite-eval)
               (lambda (form &rest _) (setq sent-form form) 'fake-promise)))
      (should (eq 'fake-promise (builder-emacs-conf-sprite-test-check "external/foo")))
      (should (equal `(builder-emacs-conf--do-elisp-package-test-check
                       ,(expand-file-name "external/foo"))
                     sent-form)))))

(ert-deftest builder-test/sprite-check-callback-reports-log-and-passes ()
  "The default sprite-check callback populates BUFFER-NAME and reports success."
  (let (alert-message)
    (cl-letf (((symbol-function 'alert) (lambda (msg &rest _kw) (setq alert-message msg))))
      (unwind-protect
          (progn
            (funcall (builder-emacs-conf--sprite-check-callback "*builder-test-sprite-log*" "some-file")
                     :resolved (cons t "all good"))
            (should (equal "all good" (with-current-buffer "*builder-test-sprite-log*" (buffer-string))))
            (should (string-match-p "passed" alert-message)))
        (kill-buffer "*builder-test-sprite-log*")))))

(ert-deftest builder-test/sprite-check-callback-reports-failure ()
  "The default sprite-check callback reports FAILED on a rejected promise
or a resolved-but-unsuccessful result alike."
  (let (alert-message)
    (cl-letf (((symbol-function 'alert) (lambda (msg &rest _kw) (setq alert-message msg))))
      (unwind-protect
          (progn
            (funcall (builder-emacs-conf--sprite-check-callback "*builder-test-sprite-log*" "some-file")
                     :rejected (cons nil "boom"))
            (should (string-match-p "FAILED" alert-message)))
        (kill-buffer "*builder-test-sprite-log*")))))

(provide 'test-builder)
;;; test-builder.el ends here
