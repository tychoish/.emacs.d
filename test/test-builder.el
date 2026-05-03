;;; test-builder.el --- ERT tests for builder.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for compilation candidate structs, table operations,
;; directory helpers, registration macro, and cache management.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ht)

;; Load builder; skip the top-level macro expansions that call xlib
;; functions not available in isolation by stubbing dependencies.

(defvar builder-test--load-path
  (expand-file-name "lisp" (file-name-directory
                             (directory-file-name
                              (file-name-directory (or load-file-name buffer-file-name))))))

(unless (featurep 'builder)
  (add-to-list 'load-path builder-test--load-path)
  (require 'builder))

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
  (let ((tbl (ht-create))
        (c (make-builder-candidate :command "make" :name "build")))
    (builder--add-candidate tbl c)
    (should (ht-contains-p tbl "build"))
    (should (eq c (ht-get tbl "build")))))

(ert-deftest builder-test/add-candidate-overwrites-existing ()
  "Adding a candidate with an existing name replaces the previous entry."
  (let ((tbl (ht-create))
        (c1 (make-builder-candidate :command "make" :name "build"))
        (c2 (make-builder-candidate :command "make -j4" :name "build")))
    (builder--add-candidate tbl c1)
    (builder--add-candidate tbl c2)
    (should (eq c2 (ht-get tbl "build")))))

(ert-deftest builder-test/add-candidates-filters-non-candidates ()
  "Non-candidate values in the list are silently ignored."
  (let ((tbl (ht-create))
        (c (make-builder-candidate :command "make" :name "build")))
    (builder--add-candidates tbl (list c nil "not-a-candidate" 42))
    (should (= 1 (ht-size tbl)))
    (should (ht-contains-p tbl "build"))))

(ert-deftest builder-test/add-candidates-all-valid ()
  "All valid candidates are inserted."
  (let ((tbl (ht-create))
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
  (let ((tbl (ht-create)))
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
  (let ((tbl (ht-create)))
    (builder-candidates-for-test-reg-pop "/" "proj" (list "/") tbl)
    (should (ht-contains-p tbl "pop"))))

(ert-deftest builder-test/register-candidates-returns-t ()
  "The generated function always returns t."
  (builder-register-candidates
   :name "test-reg-ret"
   :pipeline nil)
  (let ((tbl (ht-create)))
    (should (eq t (builder-candidates-for-test-reg-ret "/" "proj" (list "/") tbl)))))

(ert-deftest builder-test/register-candidates-with-false-predicate ()
  "When predicate is nil, no candidates are added to the table."
  (builder-register-candidates
   :name "test-reg-pred"
   :predicate nil
   :pipeline (list (make-builder-candidate :command "never" :name "never")))
  (let ((tbl (ht-create)))
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
  (let ((tbl (ht-create)))
    (builder-candidates-for-test-reg-multi "/" "proj" (list "/") tbl)
    (should (= 3 (ht-size tbl)))
    (should (ht-contains-p tbl "c1"))
    (should (ht-contains-p tbl "c2"))
    (should (ht-contains-p tbl "c3"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder--candidate-cache-p / builder--clear-candidate-cache / builder-clear-cache

(ert-deftest builder-test/candidate-cache-p-false-when-empty ()
  "`builder--candidate-cache-p' returns nil when cache is empty."
  (with-temp-buffer
    (setq-local builder--cached-candidates nil)
    (should-not (builder--candidate-cache-p (current-buffer)))))

(ert-deftest builder-test/candidate-cache-p-true-when-set ()
  "`builder--candidate-cache-p' returns t when cache is populated."
  (with-temp-buffer
    (setq-local builder--cached-candidates (ht-create))
    (should (builder--candidate-cache-p (current-buffer)))))

(ert-deftest builder-test/clear-candidate-cache-resets-cache ()
  "`builder--clear-candidate-cache' sets the cache to nil."
  (with-temp-buffer
    (setq-local builder--cached-candidates (ht-create))
    (builder--clear-candidate-cache (current-buffer))
    (should (null builder--cached-candidates))))

(ert-deftest builder-test/clear-candidate-cache-default-buffer ()
  "Default buffer argument clears the current buffer's cache."
  (with-temp-buffer
    (setq-local builder--cached-candidates (ht-create))
    (builder--clear-candidate-cache)
    (should (null builder--cached-candidates))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builder--read-command (unit: table construction path)

(ert-deftest builder-test/read-command-cons-pair-from-existing-table ()
  "`builder--read-command' with an explicit table returns a cons of
the selected name and the candidate table."
  (let* ((tbl (ht-create))
         (c (make-builder-candidate :command "make" :name "build")))
    (ht-set tbl "build" c)
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest _) "build")))
      (let ((result (builder--read-command nil tbl)))
        (should (consp result))
        (should (equal "build" (car result)))
        (should (eq tbl (cdr result)))))))

(ert-deftest builder-test/read-command-unknown-selection-adds-candidate ()
  "When the user types a string not in the table, a new candidate is created."
  (let* ((tbl (ht-create)))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest _) "custom-cmd"))
              ((symbol-function 'builder--get-candidates)
               (lambda (&rest _) tbl)))
      (let ((result (builder--read-command nil tbl)))
        (should (consp result))
        (should (equal "custom-cmd" (car result)))
        (should (ht-contains-p (cdr result) "custom-cmd"))))))

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

(provide 'test-builder)
;;; test-builder.el ends here
