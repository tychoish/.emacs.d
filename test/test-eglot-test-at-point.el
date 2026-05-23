;;; test-eglot-test-at-point.el --- ERT tests for eglot-test-at-point -*- lexical-binding: t -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^eglot-tap/")

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'eglot-test-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defun eglot-tap/make-go-lens (tests benchmarks)
  "Build a mock gopls code lens plist."
  (list :command (list :command "gopls.run_tests"
                       :arguments (vector (list :Tests (apply #'vector tests)
                                                :Benchmarks (apply #'vector benchmarks))))
        :range 'mock-range))

(defun eglot-tap/python-sym (name kind &optional children)
  "Build a mock LSP documentSymbol plist."
  (let ((sym (list :name name :kind kind :range 'mock-range)))
    (when children (setq sym (plist-put sym :children children)))
    sym))

;; eglot--TextDocumentIdentifier calls eglot internals that require a live LSP
;; connection and errors in temp buffers.  All tests that exercise the request
;; path must use this macro to stub it out.
(defmacro eglot-tap/with-lsp-stubs (request-fn &rest body)
  "Run BODY with eglot server/capability/request/TextDocumentIdentifier stubbed.
REQUEST-FN replaces `eglot--request'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'eglot-current-server) (lambda () t))
             ((symbol-function 'eglot-server-capable) (lambda (&rest _) t))
             ((symbol-function 'eglot--TextDocumentIdentifier)
              (lambda () '(:uri "file:///mock")))
             ((symbol-function 'eglot--request) ,request-fn))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Go setup: buffer-local variables

(ert-deftest eglot-tap/setup-go-sets-command ()
  (with-temp-buffer
    (eglot-test-at-point-setup-go)
    (should (equal "gopls.run_tests" eglot-test-at-point-command))))

(ert-deftest eglot-tap/setup-go-sets-list-fn ()
  (with-temp-buffer
    (eglot-test-at-point-setup-go)
    (should (functionp eglot-test-at-point-list-fn))))

(ert-deftest eglot-tap/setup-go-sets-name-fn ()
  (with-temp-buffer
    (eglot-test-at-point-setup-go)
    (should (functionp eglot-test-at-point-name-fn))))

(ert-deftest eglot-tap/setup-go-sets-run-command-fn ()
  (with-temp-buffer
    (eglot-test-at-point-setup-go)
    (should (functionp eglot-test-at-point-run-command-fn))))

(ert-deftest eglot-tap/setup-go-run-command-formats-test ()
  (with-temp-buffer
    (eglot-test-at-point-setup-go)
    (should (equal "go test -v -run=^TestFoo$ ."
                   (funcall eglot-test-at-point-run-command-fn "TestFoo")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Go name-fn lambda

(ert-deftest eglot-tap/go-name-fn-returns-first-test ()
  (with-temp-buffer
    (eglot-test-at-point-setup-go)
    (should (equal "TestFoo"
                   (funcall eglot-test-at-point-name-fn
                            (vector (list :Tests ["TestFoo"] :Benchmarks [])))))))

(ert-deftest eglot-tap/go-name-fn-prefers-tests-over-benchmarks ()
  (with-temp-buffer
    (eglot-test-at-point-setup-go)
    (should (equal "TestFoo"
                   (funcall eglot-test-at-point-name-fn
                            (vector (list :Tests ["TestFoo"]
                                          :Benchmarks ["BenchmarkBar"])))))))

(ert-deftest eglot-tap/go-name-fn-falls-back-to-benchmark ()
  (with-temp-buffer
    (eglot-test-at-point-setup-go)
    (should (equal "BenchmarkBar"
                   (funcall eglot-test-at-point-name-fn
                            (vector (list :Tests [] :Benchmarks ["BenchmarkBar"])))))))

(ert-deftest eglot-tap/go-name-fn-returns-nil-when-both-empty ()
  (with-temp-buffer
    (eglot-test-at-point-setup-go)
    (should-not (funcall eglot-test-at-point-name-fn
                         (vector (list :Tests [] :Benchmarks []))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rust setup and name-fn lambda

(ert-deftest eglot-tap/setup-rust-sets-command ()
  (with-temp-buffer
    (eglot-test-at-point-setup-rust)
    (should (equal "rust-analyzer.runSingle" eglot-test-at-point-command))))

(ert-deftest eglot-tap/setup-rust-run-command-formats-correctly ()
  (with-temp-buffer
    (eglot-test-at-point-setup-rust)
    (should (equal "cargo test my_test -- --nocapture"
                   (funcall eglot-test-at-point-run-command-fn "my_test")))))

(ert-deftest eglot-tap/rust-name-fn-extracts-test-label ()
  (with-temp-buffer
    (eglot-test-at-point-setup-rust)
    (should (equal "my_mod::my_test"
                   (funcall eglot-test-at-point-name-fn
                            (vector (list :label "test my_mod::my_test"
                                          :kind "test")))))))

(ert-deftest eglot-tap/rust-name-fn-extracts-bench-label ()
  (with-temp-buffer
    (eglot-test-at-point-setup-rust)
    (should (equal "my_bench"
                   (funcall eglot-test-at-point-name-fn
                            (vector (list :label "bench my_bench"
                                          :kind "bench")))))))

(ert-deftest eglot-tap/rust-name-fn-returns-nil-for-unknown-kind ()
  (with-temp-buffer
    (eglot-test-at-point-setup-rust)
    (should-not (funcall eglot-test-at-point-name-fn
                         (vector (list :label "run binary" :kind "bin"))))))

(ert-deftest eglot-tap/rust-name-fn-returns-nil-when-label-absent ()
  (with-temp-buffer
    (eglot-test-at-point-setup-rust)
    (should-not (funcall eglot-test-at-point-name-fn
                         (vector (list :kind "test"))))))

(ert-deftest eglot-tap/rust-name-fn-returns-nil-when-kind-absent ()
  (with-temp-buffer
    (eglot-test-at-point-setup-rust)
    (should-not (funcall eglot-test-at-point-name-fn
                         (vector (list :label "test my_test"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python setup

(ert-deftest eglot-tap/setup-python-clears-command ()
  (with-temp-buffer
    (setq-local eglot-test-at-point-command "gopls.run_tests")
    (eglot-test-at-point-setup-python)
    (should-not eglot-test-at-point-command)))

(ert-deftest eglot-tap/setup-python-sets-get-name-fn ()
  (with-temp-buffer
    (eglot-test-at-point-setup-python)
    (should (functionp eglot-test-at-point-get-name-fn))))

(ert-deftest eglot-tap/setup-python-get-name-fn-is-correct-function ()
  (with-temp-buffer
    (eglot-test-at-point-setup-python)
    (should (eq #'eglot-test-at-point--python-name-at-point
                eglot-test-at-point-get-name-fn))))

(ert-deftest eglot-tap/setup-python-run-command-formats-correctly ()
  (with-temp-buffer
    (eglot-test-at-point-setup-python)
    (cl-letf (((symbol-function 'buffer-file-name) (lambda () "/src/test_foo.py")))
      (should (equal "python -m pytest -v /src/test_foo.py::test_bar"
                     (funcall eglot-test-at-point-run-command-fn "test_bar"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eglot-test-at-point--go-names-in-buffer

(ert-deftest eglot-tap/go-names-returns-nil-without-server ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'eglot-current-server) #'ignore))
      (should-not (eglot-test-at-point--go-names-in-buffer)))))

(ert-deftest eglot-tap/go-names-returns-nil-without-codelens-capability ()
  (with-temp-buffer
    (eglot-tap/with-lsp-stubs #'ignore
      (cl-letf (((symbol-function 'eglot-server-capable) #'ignore))
        (should-not (eglot-test-at-point--go-names-in-buffer))))))

(ert-deftest eglot-tap/go-names-returns-test-pairs ()
  (with-temp-buffer
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _) (vector (eglot-tap/make-go-lens '("TestFoo" "TestBar") '())))
      (let ((pairs (eglot-test-at-point--go-names-in-buffer)))
        (should (= 2 (length pairs)))
        (should (equal (cons "TestFoo" 'test) (assoc "TestFoo" pairs)))
        (should (equal (cons "TestBar" 'test) (assoc "TestBar" pairs)))))))

(ert-deftest eglot-tap/go-names-returns-benchmark-pairs ()
  (with-temp-buffer
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _) (vector (eglot-tap/make-go-lens '() '("BenchmarkFoo"))))
      (let ((pairs (eglot-test-at-point--go-names-in-buffer)))
        (should (= 1 (length pairs)))
        (should (equal (cons "BenchmarkFoo" 'benchmark) (car pairs)))))))

(ert-deftest eglot-tap/go-names-combines-tests-and-benchmarks ()
  (with-temp-buffer
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _) (vector (eglot-tap/make-go-lens '("TestFoo") '("BenchmarkBar"))))
      (let ((pairs (eglot-test-at-point--go-names-in-buffer)))
        (should (= 2 (length pairs)))
        (should (eq 'test (cdr (assoc "TestFoo" pairs))))
        (should (eq 'benchmark (cdr (assoc "BenchmarkBar" pairs))))))))

(ert-deftest eglot-tap/go-names-filters-non-gopls-lenses ()
  (with-temp-buffer
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _)
          (vector (list :command (list :command "other.command" :arguments []))))
      (should-not (eglot-test-at-point--go-names-in-buffer)))))

(ert-deftest eglot-tap/go-names-handles-empty-arrays ()
  (with-temp-buffer
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _) (vector (eglot-tap/make-go-lens '() '())))
      (should-not (eglot-test-at-point--go-names-in-buffer)))))

(ert-deftest eglot-tap/go-names-accumulates-across-multiple-lenses ()
  (with-temp-buffer
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _)
          (vector (eglot-tap/make-go-lens '("TestA") '())
                  (eglot-tap/make-go-lens '("TestB") '())))
      (let ((pairs (eglot-test-at-point--go-names-in-buffer)))
        (should (= 2 (length pairs)))
        (should (assoc "TestA" pairs))
        (should (assoc "TestB" pairs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eglot-test-at-point--python-name-at-point

(ert-deftest eglot-tap/python-name-returns-nil-without-server ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'eglot-current-server) #'ignore))
      (should-not (eglot-test-at-point--python-name-at-point)))))

(ert-deftest eglot-tap/python-name-finds-toplevel-test-function ()
  (with-temp-buffer
    (insert (make-string 100 ?\s))
    (goto-char 40)
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _) (vector (eglot-tap/python-sym "test_foo" 12)))
      (cl-letf (((symbol-function 'eglot-range-region) (lambda (_) (cons 10 80))))
        (should (equal "test_foo"
                       (eglot-test-at-point--python-name-at-point)))))))

(ert-deftest eglot-tap/python-name-skips-non-test-function ()
  (with-temp-buffer
    (insert (make-string 100 ?\s))
    (goto-char 40)
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _) (vector (eglot-tap/python-sym "helper_fn" 12)))
      (cl-letf (((symbol-function 'eglot-range-region) (lambda (_) (cons 10 80))))
        (should-not (eglot-test-at-point--python-name-at-point))))))

(ert-deftest eglot-tap/python-name-skips-function-when-point-before-range ()
  (with-temp-buffer
    (insert (make-string 100 ?\s))
    (goto-char 5)
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _) (vector (eglot-tap/python-sym "test_foo" 12)))
      (cl-letf (((symbol-function 'eglot-range-region) (lambda (_) (cons 10 80))))
        (should-not (eglot-test-at-point--python-name-at-point))))))

(ert-deftest eglot-tap/python-name-skips-function-when-point-at-range-end ()
  ;; range check is (< pos end), so point at end is excluded
  (with-temp-buffer
    (insert (make-string 100 ?\s))
    (goto-char 80)
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _) (vector (eglot-tap/python-sym "test_foo" 12)))
      (cl-letf (((symbol-function 'eglot-range-region) (lambda (_) (cons 10 80))))
        (should-not (eglot-test-at-point--python-name-at-point))))))

(ert-deftest eglot-tap/python-name-finds-method-in-test-class ()
  (with-temp-buffer
    (insert (make-string 100 ?\s))
    (goto-char 40)
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _)
          (vector (eglot-tap/python-sym
                   "TestMyClass" 5
                   (vector (eglot-tap/python-sym "test_method" 6)))))
      (cl-letf (((symbol-function 'eglot-range-region) (lambda (_) (cons 10 80))))
        (should (equal "TestMyClass::test_method"
                       (eglot-test-at-point--python-name-at-point)))))))

(ert-deftest eglot-tap/python-name-skips-method-in-non-test-class ()
  (with-temp-buffer
    (insert (make-string 100 ?\s))
    (goto-char 40)
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _)
          (vector (eglot-tap/python-sym
                   "MyHelper" 5
                   (vector (eglot-tap/python-sym "test_method" 6)))))
      (cl-letf (((symbol-function 'eglot-range-region) (lambda (_) (cons 10 80))))
        (should-not (eglot-test-at-point--python-name-at-point))))))

(ert-deftest eglot-tap/python-name-skips-non-test-method-in-test-class ()
  (with-temp-buffer
    (insert (make-string 100 ?\s))
    (goto-char 40)
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _)
          (vector (eglot-tap/python-sym
                   "TestMyClass" 5
                   (vector (eglot-tap/python-sym "helper_method" 6)))))
      (cl-letf (((symbol-function 'eglot-range-region) (lambda (_) (cons 10 80))))
        (should-not (eglot-test-at-point--python-name-at-point))))))

(ert-deftest eglot-tap/python-name-skips-method-outside-range ()
  (with-temp-buffer
    (insert (make-string 100 ?\s))
    (goto-char 5)
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _)
          (vector (eglot-tap/python-sym
                   "TestMyClass" 5
                   (vector (eglot-tap/python-sym "test_method" 6)))))
      (cl-letf (((symbol-function 'eglot-range-region) (lambda (_) (cons 10 80))))
        (should-not (eglot-test-at-point--python-name-at-point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eglot-test-at-point-name dispatch

(ert-deftest eglot-tap/name-calls-get-name-fn-when-set ()
  (with-temp-buffer
    (setq-local eglot-test-at-point-get-name-fn (lambda () "test_via_fn"))
    (should (equal "test_via_fn" (eglot-test-at-point-name)))))

(ert-deftest eglot-tap/name-returns-nil-when-no-command ()
  (with-temp-buffer
    (setq-local eglot-test-at-point-get-name-fn nil)
    (setq-local eglot-test-at-point-command nil)
    (should-not (eglot-test-at-point-name))))

(ert-deftest eglot-tap/name-returns-nil-when-no-name-fn ()
  (with-temp-buffer
    (setq-local eglot-test-at-point-get-name-fn nil)
    (setq-local eglot-test-at-point-command "gopls.run_tests")
    (setq-local eglot-test-at-point-name-fn nil)
    (should-not (eglot-test-at-point-name))))

(ert-deftest eglot-tap/name-returns-nil-without-server ()
  (with-temp-buffer
    (setq-local eglot-test-at-point-get-name-fn nil)
    (setq-local eglot-test-at-point-command "gopls.run_tests")
    (setq-local eglot-test-at-point-name-fn #'identity)
    (cl-letf (((symbol-function 'eglot-current-server) #'ignore))
      (should-not (eglot-test-at-point-name)))))

(ert-deftest eglot-tap/name-picks-last-lens-at-or-before-point ()
  (with-temp-buffer
    (insert (make-string 100 ?\s))
    (goto-char 50)
    (setq-local eglot-test-at-point-get-name-fn nil)
    (setq-local eglot-test-at-point-command "gopls.run_tests")
    (setq-local eglot-test-at-point-name-fn
                (lambda (args) (aref (plist-get (aref args 0) :Tests) 0)))
    (eglot-tap/with-lsp-stubs
        (lambda (&rest _)
          (vector
           ;; range-a: beg=10 < point=50 → matches
           (list :command (list :command "gopls.run_tests"
                                :arguments (vector (list :Tests ["TestFirst"])))
                 :range 'range-a)
           ;; range-b: beg=60 > point=50 → skipped
           (list :command (list :command "gopls.run_tests"
                                :arguments (vector (list :Tests ["TestSecond"])))
                 :range 'range-b)))
      (cl-letf (((symbol-function 'eglot-range-region)
                 (lambda (r) (if (eq r 'range-a) (cons 10 80) (cons 60 90)))))
        (should (equal "TestFirst" (eglot-test-at-point-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error handling for interactive commands

(ert-deftest eglot-tap/eglot-test-at-point-errors-without-run-command ()
  (with-temp-buffer
    (setq-local eglot-test-at-point-run-command-fn nil)
    (should-error (eglot-test-at-point) :type 'user-error)))

(ert-deftest eglot-tap/eglot-test-at-point-select-errors-without-run-command ()
  (with-temp-buffer
    (setq-local eglot-test-at-point-run-command-fn nil)
    (should-error (eglot-test-at-point-select) :type 'user-error)))

(ert-deftest eglot-tap/eglot-test-at-point-select-errors-without-list-fn ()
  (with-temp-buffer
    (setq-local eglot-test-at-point-run-command-fn #'identity)
    (setq-local eglot-test-at-point-list-fn nil)
    (should-error (eglot-test-at-point-select) :type 'user-error)))

(provide 'test-eglot-test-at-point)
;;; test-eglot-test-at-point.el ends here
