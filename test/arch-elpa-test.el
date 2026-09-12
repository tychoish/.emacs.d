;;; arch-elpa-test.el --- Tests for arch-elpa -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT unit tests for arch-elpa's package.el-backed data model: struct
;; construction, upgradeability detection, filters, and mark bookkeeping.
;; Installs/removals are not exercised here since they mutate the real
;; package.el state of the running Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'arch-elpa)

(defmacro arch-elpa-test--with-package-state (installed available &rest body)
  "Run BODY with `package-alist' bound to INSTALLED and
`package-archive-contents' bound to AVAILABLE, both alists of
\(name . (package-desc)) shaped like the real package.el tables."
  (declare (indent 2))
  `(let ((package-alist ,installed)
         (package-archive-contents ,available))
     ,@body))

(defun arch-elpa-test--desc (version &optional archive summary)
  "Build a `package-desc' with VERSION (a string), ARCHIVE, and SUMMARY."
  (package-desc-create :version (version-to-list version)
                       :archive archive
                       :summary summary))

(ert-deftest arch-elpa-test-pkg-for-name-installed-up-to-date ()
  "arch-elpa--pkg-for-name reports an installed, non-upgradeable package."
  (arch-elpa-test--with-package-state
      `((foo . (,(arch-elpa-test--desc "1.0" "melpa" "a foo package"))))
      `((foo . (,(arch-elpa-test--desc "1.0" "melpa" "a foo package"))))
    (let ((pkg (arch-elpa--pkg-for-name 'foo)))
      (should (arch-elpa-pkg-installed-p pkg))
      (should-not (arch-elpa-pkg-upgradeable-p pkg))
      (should (equal (arch-elpa-pkg-archive pkg) "melpa"))
      (should (equal (arch-elpa-pkg-version pkg) "1.0")))))

(ert-deftest arch-elpa-test-pkg-for-name-upgradeable ()
  "arch-elpa--pkg-for-name detects an available version newer than installed."
  (arch-elpa-test--with-package-state
      `((foo . (,(arch-elpa-test--desc "1.0" "melpa"))))
      `((foo . (,(arch-elpa-test--desc "2.0" "melpa"))))
    (let ((pkg (arch-elpa--pkg-for-name 'foo)))
      (should (arch-elpa-pkg-installed-p pkg))
      (should (arch-elpa-pkg-upgradeable-p pkg)))))

(ert-deftest arch-elpa-test-pkg-for-name-available-not-installed ()
  "arch-elpa--pkg-for-name reports an available, uninstalled package."
  (arch-elpa-test--with-package-state
      nil
      `((bar . (,(arch-elpa-test--desc "3.1" "gnu" "a bar package"))))
    (let ((pkg (arch-elpa--pkg-for-name 'bar)))
      (should-not (arch-elpa-pkg-installed-p pkg))
      (should-not (arch-elpa-pkg-upgradeable-p pkg))
      (should (equal (arch-elpa-pkg-archive pkg) "gnu")))))

(ert-deftest arch-elpa-test-pkg-for-name-built-in ()
  "arch-elpa--pkg-for-name reports a built-in package with no desc as built-in."
  (arch-elpa-test--with-package-state nil nil
    (cl-letf (((symbol-function 'package-built-in-p) (lambda (_name) t)))
      (let ((pkg (arch-elpa--pkg-for-name 'seq)))
        (should (arch-elpa-pkg-built-in-p pkg))
        (should (equal (arch-elpa-pkg-archive pkg) "builtin"))
        (should (equal (arch-elpa-pkg-version pkg) "-"))))))

(ert-deftest arch-elpa-test-all-names-unions-installed-and-available ()
  "arch-elpa--all-names is the sorted union of installed and available names."
  (arch-elpa-test--with-package-state
      `((zeta . (,(arch-elpa-test--desc "1.0"))))
      `((zeta . (,(arch-elpa-test--desc "1.0"))) (alpha . (,(arch-elpa-test--desc "1.0"))))
    (should (equal (arch-elpa--all-names) '(alpha zeta)))))

(ert-deftest arch-elpa-test-filter-predicates ()
  "The upgradeable/installed/available/built-in filter predicates classify a pkg correctly."
  (let ((upgradeable (arch-elpa-pkg--make :installed-p t :upgradeable-p t :built-in-p nil))
        (installed   (arch-elpa-pkg--make :installed-p t :upgradeable-p nil :built-in-p nil))
        (available   (arch-elpa-pkg--make :installed-p nil :upgradeable-p nil :built-in-p nil))
        (built-in    (arch-elpa-pkg--make :installed-p nil :upgradeable-p nil :built-in-p t)))
    (should (arch-elpa--filter-upgradeable-p upgradeable))
    (should-not (arch-elpa--filter-upgradeable-p installed))
    (should (arch-elpa--filter-installed-p installed))
    (should-not (arch-elpa--filter-installed-p available))
    (should (arch-elpa--filter-available-p available))
    (should-not (arch-elpa--filter-available-p installed))
    (should (arch-elpa--filter-built-in-p built-in))
    (should-not (arch-elpa--filter-built-in-p installed))))

(ert-deftest arch-elpa-test-build-entry-marks-bold-face ()
  "arch-elpa--build-entry uses the marked-link face when the package is marked."
  (let* ((pkg (arch-elpa-pkg--make :name 'foo :version "1.0" :archive "melpa"
                                   :summary "" :installed-p t :upgradeable-p nil))
         (arch-elpa--marked (make-hash-table :test #'equal)))
    (setf (map-elt arch-elpa--marked 'foo) t)
    (let* ((entry (arch-elpa--build-entry pkg))
           (name-cell (aref (cadr entry) 0)))
      (should (eq (get-text-property 0 'face name-cell) 'arch-face-pkg-link-marked)))))

(ert-deftest arch-elpa-test-build-entry-version-old-face-when-upgradeable ()
  "arch-elpa--build-entry highlights the version column when upgradeable."
  (let* ((pkg (arch-elpa-pkg--make :name 'foo :version "1.0" :archive "melpa"
                                   :summary "" :installed-p t :upgradeable-p t))
         (arch-elpa--marked (make-hash-table :test #'equal))
         (entry (arch-elpa--build-entry pkg))
         (version-cell (aref (cadr entry) 3)))
    (should (eq (get-text-property 0 'face version-cell) 'arch-face-version-old))))

(ert-deftest arch-elpa-test-list-mode-derived-from-tabulated-list-mode ()
  "arch-elpa-list-mode is derived from tabulated-list-mode."
  (with-temp-buffer
    (arch-elpa-list-mode)
    (should (derived-mode-p 'tabulated-list-mode))))

(ert-deftest arch-elpa-test-pkg-at-point-signals-when-empty ()
  "arch-elpa--pkg-at-point signals a user-error with no package at point."
  (with-temp-buffer
    (arch-elpa-list-mode)
    (should-error (arch-elpa--pkg-at-point) :type 'user-error)))

(provide 'arch-elpa-test)
;;; arch-elpa-test.el ends here
