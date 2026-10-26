;;; test-helper.el --- ERT test infrastructure -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Loaded by ert-runner before any test files.
;; Adds lisp/ and elpa/* to load-path so test files can require local modules.

;;; Code:

(require 'cl-lib)

(defvar test-helper-root nil
  "Repository root directory, set once `test-helper' loads.")

(let* ((test-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory test-file))
       (root (file-name-directory (directory-file-name test-dir))))
  (setq test-helper-root root)
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (let ((elpa-dir (expand-file-name "elpa" root)))
    (when (file-directory-p elpa-dir)
      (dolist (dir (directory-files elpa-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir)
          (let* ((pkg-base (file-name-nondirectory dir))
                 (pkg-name (replace-regexp-in-string "-[0-9].*\\'" "" pkg-base))
                 (af (expand-file-name (concat pkg-name "-autoloads") dir)))
            (when (or (file-exists-p (concat af ".el"))
                      (file-exists-p (concat af ".elc")))
              (load af t t))))))))

;;; Code coverage (undercover.el)
;;
;; Set EMACS_COVERAGE=1 (or UNDERCOVER_FORCE, which undercover.el also
;; honors) before running the batch test runner to instrument lisp/*.el
;; and write an lcov report that `cov-mode' can render as overlays.

(when (or (getenv "EMACS_COVERAGE") (getenv "UNDERCOVER_FORCE"))
  (require 'undercover)
  (undercover "lisp/*.el"
              (:report-format 'lcov)
              (:report-file (expand-file-name "coverage/lcov.info" test-helper-root))
              (:send-report nil)))

;;; Transient key introspection helpers

(defun transient-test/collect-keys (prefix-sym)
  "Return a list of all :key strings in PREFIX-SYM's transient layout.
Uses an iterative walk so no recursion limit applies."
  (let ((queue (list (get prefix-sym 'transient--layout)))
        keys)
    (while queue
      (let ((node (pop queue)))
        (cond
          ((and (vectorp node) (not (byte-code-function-p node)))
           (setq queue (append (seq-into node 'list) queue)))
          ((and (consp node) (eq (car node) 'transient-suffix))
           (when-let* ((key (plist-get (cdr node) :key)))
             (push key keys)))
          ((consp node)
           (setq queue (append node queue))))))
    (nreverse keys)))

(defun transient-test/key-sequence-prefix-p (short long)
  "Return non-nil if SHORT key string is a strict prefix of LONG as a key sequence.
Uses `kbd' to parse both strings so modifier chords like \"C-r\" (a single
key event) are never confused with multi-character sequences like \"dv\"."
  (when (not (equal short long))
    (let ((sv (kbd short))
          (lv (kbd long)))
      (and (< (length sv) (length lv))
           (equal sv (substring lv 0 (length sv)))))))

(defun transient-test/key-prefix-conflicts (keys)
  "Return a list of (SHORT LONG) pairs where SHORT is a strict prefix of LONG in KEYS."
  (let (conflicts)
    (dolist (short keys)
      (dolist (long keys)
        (when (transient-test/key-sequence-prefix-p short long)
          (push (list short long) conflicts))))
    conflicts))

(defun transient-test/duplicate-keys (keys)
  "Return a list of keys that appear more than once in KEYS."
  (let (seen dups)
    (dolist (k keys)
      (if (member k seen)
          (unless (member k dups)
            (push k dups))
        (push k seen)))
    dups))

;;; Package testing helpers

(cl-defmacro with-temporary-package-require (feature &rest body)
  "Install FEATURE to a temporary package directory, require it, then eval BODY.
FEATURE is a quoted symbol naming the package/feature (e.g. \\='package-lint).
BODY may start with a `:path PATH' pair naming the directory to use as
`package-user-dir'; it defaults to a per-instance subdir of
`temporary-file-directory'.  Remaining BODY forms are evaluated after the
package is installed and required."
  (declare (indent 1))
  (let (path)
    (when (eq (car body) :path)
      (setq path (cadr body)
            body (cddr body)))
    `(let* ((package-user-dir (or ,path
                                  (file-name-concat temporary-file-directory
                                                    (if (fboundp 'sprite-instance-name)
                                                        (sprite-instance-name)
                                                      "emacs"))))
            (load-path load-path)
            (package-activated-list package-activated-list))
       (unless (file-exists-p package-user-dir)
         (make-directory package-user-dir t))
       (require 'package)
       (package-initialize)
       (unless (package-installed-p ,feature)
         (package-install ,feature))
       (require ,feature)
       ,@body)))

(provide 'test-helper)
;;; test-helper.el ends here
