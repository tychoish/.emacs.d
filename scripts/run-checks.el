;;; run-checks.el --- Run local preflight checks for Emacs packages -*- lexical-binding: t -*-

;; Author: tychoish
;; Keywords: tools, lisp, test, lint

;;; Commentary:
;; Provides comprehensive, isolated preflight quality validation for Emacs
;; Lisp packages.  Runs check-parens, checkdoc, package-lint, byte-compilation,
;; and ERT test suites, returning structured diagnostic results.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'subr-x)

(defgroup run-checks nil
  "Preflight checks for Emacs Lisp packages."
  :group 'development)

(defun run-checks--find-package-files (dir)
  "Find main package .el files in DIR."
  (let* ((el-files (if (file-directory-p dir)
                       (directory-files dir t "\\.el\\'")
                     nil))
         (candidates (seq-remove
                      (lambda (f)
                        (let ((base (file-name-nondirectory f)))
                          (or (string-match-p "\\`\\.#" base)
                              (string-prefix-p "test-" base)
                              (string-suffix-p "-test.el" base)
                              (string-suffix-p "-tests.el" base)
                              (string-match-p "spec" base)
                              (string-match-p "run-checks" base)
                              (string-suffix-p "-autoloads.el" base)
                              (and (string-suffix-p "-pkg.el" base)
                                   (> (length base) 7)
                                   (file-exists-p (expand-file-name (concat (substring base 0 (- (length base) 7)) ".el") dir))))))
                      el-files)))
    candidates))

(defun run-checks--find-test-files (dir &optional custom-test-dir)
  "Find ERT test files in DIR or CUSTOM-TEST-DIR."
  (let ((test-subdir (or (and custom-test-dir (expand-file-name custom-test-dir dir))
                         (let ((t1 (expand-file-name "test" dir))
                               (t2 (expand-file-name "tests" dir)))
                           (cond ((and (file-directory-p t1)) t1)
                                 ((and (file-directory-p t2)) t2)
                                 (t nil))))))
    (if (and test-subdir (file-directory-p test-subdir))
        (directory-files test-subdir t "\\.el\\'")
      (let ((el-files (if (file-directory-p dir) (directory-files dir t "\\.el\\'") nil)))
        (seq-filter
         (lambda (f)
           (let ((base (file-name-nondirectory f)))
             (or (string-prefix-p "test-" base)
                 (string-suffix-p "-test.el" base)
                 (string-suffix-p "-tests.el" base))))
         el-files)))))

(defun run-checks--check-parens (pkg-files verbose)
  "Run check-parens on PKG-FILES. Returns list of error strings."
  (when verbose (message "[run-checks] 1. Running check-parens..."))
  (let ((errs nil))
    (dolist (f pkg-files)
      (with-temp-buffer
        (insert-file-contents f)
        (condition-case err
            (check-parens)
          (error (push (format "check-parens (%s): %s"
                               (file-name-nondirectory f)
                               (error-message-string err))
                       errs)))))
    (nreverse errs)))

(defun run-checks--checkdoc (pkg-file file-name verbose)
  "Run checkdoc on PKG-FILE. Returns cons (ERRORS . WARNINGS)."
  (when verbose (message "[run-checks] 2. Running checkdoc..."))
  (let ((errs nil)
        (warns nil))
    (condition-case err
        (progn
          (require 'checkdoc)
          (let ((checkdoc-spellcheck-documentation-flag nil)
                (warn-buf (get-buffer "*warn*")))
            (when warn-buf (kill-buffer warn-buf))
            (checkdoc-file pkg-file)
            (setq warn-buf (get-buffer "*warn*"))
            (when warn-buf
              (let ((warn-str (with-current-buffer warn-buf (buffer-string))))
                (when (string-match-p (concat (regexp-quote file-name) ":[0-9]+:") warn-str)
                  (push (format "checkdoc:\n%s" warn-str) warns))))))
      (error (push (format "checkdoc failed: %s" (error-message-string err)) errs)))
    (cons (nreverse errs) (nreverse warns))))

(defun run-checks--package-lint (pkg-file verbose)
  "Run package-lint on PKG-FILE. Returns cons (ERRORS . WARNINGS)."
  (when verbose (message "[run-checks] 3. Running package-lint..."))
  (let ((errs nil)
        (warns nil))
    (condition-case err
        (progn
          (require 'package-lint nil t)
          (if (not (fboundp 'package-lint-buffer))
              (when verbose (push "package-lint is not installed/loaded" warns))
            (with-current-buffer (find-file-noselect pkg-file)
              (let ((issues (package-lint-buffer)))
                (when issues
                  (push (format "package-lint:\n%s"
                                (mapconcat (lambda (issue)
                                             (format "  Line %d Col %d: %s (%s)"
                                                     (car issue) (cadr issue) (nth 2 issue) (nth 3 issue)))
                                           issues "\n"))
                        warns))))))
      (error (push (format "package-lint failed: %s" (error-message-string err)) errs)))
    (cons (nreverse errs) (nreverse warns))))

(defun run-checks--byte-compile (pkg-files verbose)
  "Byte-compile PKG-FILES. Returns cons (ERRORS . WARNINGS)."
  (when verbose (message "[run-checks] 4. Running byte-compilation..."))
  (let ((errs nil)
        (warns nil)
        (byte-compile-log-buffer (generate-new-buffer " *run-checks-compile-log*")))
    (unwind-protect
        (progn
          (let ((byte-compile-log-buffer byte-compile-log-buffer)
                (byte-compile-dest-file-function (lambda (_) (make-temp-file "run-checks-elc-"))))
            (dolist (f pkg-files)
              (byte-compile-file f)))
          (let ((compile-output (with-current-buffer byte-compile-log-buffer (buffer-string))))
            (when (string-match-p "Warning:" compile-output)
              (push (format "byte-compile warnings:\n%s" compile-output) warns))
            (when (string-match-p "Error:" compile-output)
              (push (format "byte-compile errors:\n%s" compile-output) errs))))
      (kill-buffer byte-compile-log-buffer))
    (cons (nreverse errs) (nreverse warns))))

(defun run-checks--ert (test-files pkg-name verbose)
  "Run ERT on TEST-FILES for PKG-NAME. Returns list of error strings."
  (when verbose (message "[run-checks] 5. Running ERT tests (%d files)..." (length test-files)))
  (let ((errs nil))
    (dolist (tf test-files)
      (condition-case err
          (progn
            (load-file tf)
            (let* ((selector (if pkg-name
                                 (concat "^\\(?:" (regexp-quote pkg-name) "\\|test-\\)")
                               t))
                   (tests (ert-select-tests selector t)))
              (if (null tests)
                  (when verbose (message "No ERT tests matched '%s' in %s" selector tf))
                (let* ((stats (ert-run-tests selector #'(lambda (&rest _))))
                       (failed (ert-stats-completed-unexpected stats))
                       (passed (ert-stats-completed-expected stats)))
                  (when verbose
                    (message "ERT tests results for %s: %d passed, %d failed"
                             (file-name-nondirectory tf) passed failed))
                  (when (> failed 0)
                    (push (format "ERT tests (%s): %d test(s) failed"
                                  (file-name-nondirectory tf) failed)
                          errs))))))
        (error (push (format "ERT tests failed in %s: %s"
                             (file-name-nondirectory tf)
                             (error-message-string err))
                     errs))))
    (nreverse errs)))

;;;###autoload
(cl-defun run-checks-package (&optional dir &key main-file test-dir skip-checks verbose)
  "Run preflight quality checks for package located at DIR.
MAIN-FILE explicitly overrides main file detection.
TEST-DIR explicitly overrides test directory detection.
SKIP-CHECKS can be a list of symbols (e.g. \\='(checkdoc package-lint)) or t to skip all.
VERBOSE enables verbose log messages.

Returns a plist: (:passed BOOLEAN :errors LIST :warnings LIST :package STRING)."
  (let* ((package-dir (file-name-as-directory (expand-file-name (or dir default-directory))))
         (default-directory package-dir)
         (pkg-files (if main-file
                        (list (expand-file-name main-file package-dir))
                      (run-checks--find-package-files package-dir)))
         (pkg-file (car pkg-files))
         (test-files (run-checks--find-test-files package-dir test-dir))
         (skip-list (if (listp skip-checks) skip-checks (if skip-checks '(all) nil)))
         (pkg-name-str (or (and pkg-file (file-name-sans-extension (file-name-nondirectory pkg-file)))
                           (file-name-nondirectory (directory-file-name package-dir))))
         (errors nil)
         (warnings nil))

    (when (memq 'all skip-list)
      (cl-return-from run-checks-package
        (list :passed t :package pkg-name-str :errors nil :warnings nil)))

    (unless pkg-file
      (cl-return-from run-checks-package
        (list :passed nil :package pkg-name-str
              :errors (list (format "No main Emacs package file found in %s" package-dir))
              :warnings nil)))

    (let ((file-name (file-name-nondirectory pkg-file))
          (pkg-name (file-name-sans-extension (file-name-nondirectory pkg-file))))
      (when (and verbose pkg-name)
        (message "[run-checks] Target Package: %s (%s)" pkg-name pkg-file))

      ;; 1. Check Parens
      (unless (memq 'check-parens skip-list)
        (setq errors (append errors (run-checks--check-parens pkg-files verbose))))

      ;; 2. Checkdoc
      (unless (memq 'checkdoc skip-list)
        (let ((res (run-checks--checkdoc pkg-file file-name verbose)))
          (setq errors (append errors (car res))
                warnings (append warnings (cdr res)))))

      ;; 3. Package Lint
      (unless (memq 'package-lint skip-list)
        (let ((res (run-checks--package-lint pkg-file verbose)))
          (setq errors (append errors (car res))
                warnings (append warnings (cdr res)))))

      ;; 4. Byte Compile
      (unless (memq 'byte-compile skip-list)
        (let ((res (run-checks--byte-compile pkg-files verbose)))
          (setq errors (append errors (car res))
                warnings (append warnings (cdr res)))))

      ;; 5. ERT Tests
      (unless (or (memq 'ert skip-list) (null test-files))
        (setq errors (append errors (run-checks--ert test-files pkg-name verbose)))))

    (let* ((passed (null errors))
           (result (list :passed passed
                         :package pkg-name-str
                         :errors errors
                         :warnings warnings)))
      (when verbose
        (if passed
            (message "[run-checks] All checks passed for %s" pkg-name-str)
          (message "[run-checks] FAILED for %s with %d error(s)"
                   pkg-name-str (length errors))))
      result)))

;;;###autoload
(defun acr-run-all-checks ()
  "Run byte-compile, package-lint, checkdoc, check-parens, and ERT tests on the current package."
  (interactive)
  (let* ((res (run-checks-package default-directory :verbose t))
         (passed (plist-get res :passed))
         (errors (plist-get res :errors))
         (warnings (plist-get res :warnings)))
    (message "\n================ SUMMARY ================")
    (if (and passed (null warnings))
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
    passed))

(provide 'run-checks)
;;; run-checks.el ends here
