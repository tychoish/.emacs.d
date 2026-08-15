;;; run-checks.el --- Run all local checks for an Emacs package -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ert)
(require 'seq)

(defun acr-run-all-checks ()
  "Run byte-compile, package-lint, checkdoc, check-parens, and ERT tests on the current package."
  (interactive)
  (let* ((el-files (directory-files default-directory nil "\\.el\\'"))
         (pkg-files (seq-remove (lambda (f)
                                  (or (string-match-p "test" f)
                                      (string-match-p "spec" f)
                                      (string-match-p "run-checks" f)
                                      (string-suffix-p "-autoloads.el" f)
                                      (string-suffix-p "-pkg.el" f)))
                                el-files))
         (pkg-file (car pkg-files))
         (test-dir (cond ((file-directory-p "test") "test")
                         ((file-directory-p "tests") "tests")
                         (t nil)))
         (test-files (if test-dir
                         (directory-files test-dir t "\\.el\\'")
                       (seq-filter (lambda (f)
                                     (or (string-prefix-p "test-" f)
                                         (string-suffix-p "-test.el" f)
                                         (string-suffix-p "-tests.el" f)))
                                   el-files)))
         (test-file (car test-files))
         (errors nil)
         (warnings nil))

    (unless pkg-file
      (error "No main Emacs package file found in %s" default-directory))

    (let* ((pkg-name (file-name-sans-extension pkg-file))
           (file-name (file-name-nondirectory pkg-file)))

      (message "Target Package: %s (%s)" pkg-name pkg-file)

      ;; 1. Check Parens
      (message "1. Running check-parens...")
      (with-temp-buffer
        (insert-file-contents pkg-file)
        (condition-case err
            (check-parens)
          (error (push (format "check-parens: %s" (error-message-string err)) errors))))

      ;; 2. Checkdoc
      (message "2. Running checkdoc...")
      (condition-case err
          (progn
            (require 'checkdoc)
            (let ((checkdoc-spellcheck-documentation-flag nil))
              (let ((warn-buf (get-buffer "*warn*")))
                (when warn-buf (kill-buffer warn-buf)))
              (checkdoc-file pkg-file)
              (let ((warn-buf (get-buffer "*warn*")))
                (when warn-buf
                  (let ((warn-str (with-current-buffer warn-buf (buffer-string))))
                    (when (string-match-p (concat (regexp-quote file-name) ":[0-9]+:") warn-str)
                      (push (format "checkdoc:\n%s" warn-str) warnings)))))))
        (error (push (format "checkdoc failed: %s" (error-message-string err)) errors)))

      ;; 3. Package Lint
      (message "3. Running package-lint...")
      (condition-case err
          (progn
            (require 'package-lint nil t)
            (if (not (fboundp 'package-lint-buffer))
                (push "package-lint is not installed/loaded" warnings)
              (with-current-buffer (find-file-noselect pkg-file)
                (let ((issues (package-lint-buffer)))
                  (when issues
                    (push (format "package-lint:\n%s"
                                  (mapconcat (lambda (issue)
                                               (format "  Line %d Col %d: %s (%s)"
                                                       (car issue) (cadr issue) (nth 2 issue) (nth 3 issue)))
                                             issues "\n"))
                          warnings))))))
        (error (push (format "package-lint failed: %s" (error-message-string err)) errors)))

      ;; 4. Byte Compile
      (message "4. Running byte-compilation...")
      (let ((byte-compile-log-buffer (generate-new-buffer " *acr-compile-log*")))
        (unwind-protect
            (progn
              (let ((byte-compile-log-buffer byte-compile-log-buffer))
                (byte-compile-file pkg-file))
              (let ((compile-output (with-current-buffer byte-compile-log-buffer (buffer-string))))
                (when (string-match-p "Warning:" compile-output)
                  (push (format "byte-compile warnings:\n%s" compile-output) warnings))
                (when (string-match-p "Error:" compile-output)
                  (push (format "byte-compile errors:\n%s" compile-output) errors))))
          (kill-buffer byte-compile-log-buffer)))

      ;; 5. ERT Tests
      (if (null test-file)
          (message "5. No test file found. Skipping ERT tests.")
        (message "5. Running ERT tests on %s..." test-file)
        (condition-case err
            (progn
              (load-file test-file)
              (let* ((selector (concat "^" (regexp-quote pkg-name) "\\(?:/\\|-\\)"))
                     (tests (ert-select-tests selector t)))
                (if (null tests)
                    (message "No ERT tests matched '%s'. Skipping tests." selector)
                  (let* ((stats (ert-run-tests selector #'(lambda (&rest _))))
                         (failed (ert--stats-failed-unexpected stats))
                         (passed (ert--stats-passed-expected stats)))
                    (message "ERT tests results: %d passed, %d failed" passed failed)
                    (when (> failed 0)
                      (push (format "ERT tests: %d test(s) failed" failed) errors))))))
          (error (push (format "ERT tests failed to run: %s" (error-message-string err)) errors)))))

    ;; Summary
    (message "\n================ SUMMARY ================")
    (if (and (null errors) (null warnings))
        (message "All checks passed successfully! 🎉")
      (when warnings
        (message "\n--- WARNINGS ---")
        (dolist (w warnings)
          (message "%s" w)))
      (when errors
        (message "\n--- ERRORS ---")
        (dolist (e errors)
          (message "%s" e))
        (error "Some checks failed! Check output above.")))
    t))

(provide 'run-checks)
