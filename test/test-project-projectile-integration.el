;;; project-projectile-integration-test.el --- ERT tests for project.el + projectile integration -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; Tests that verify project.el generic functions work correctly with
;; projectile as a backend, including the project-name gap.
;;
;; Run with:
;;   M-x ert RET project-projectile RET
;; or from the shell:
;;   emacsclient -e '(ert-run-tests-batch "project-projectile")'

;;; Code:

(require 'ert)
(require 'project)
(require 'projectile)

(cl-defmethod project-name ((project (head projectile)))
  (projectile-project-name (project-root project)))

(defmacro project-test--with-project-dir (marker &rest body)
  "Create a temp directory containing MARKER file, bind `default-directory' to it, run BODY."
  (declare (indent defun))
  `(let* ((dir (file-name-as-directory (make-temp-file "project-test-" t)))
          (default-directory dir))
     (unwind-protect
         (progn
           (write-region "" nil (expand-file-name ,marker dir))
           ,@body)
       (delete-directory dir t))))

;; ---------------------------------------------------------------------------
;; project-current uses projectile when projectile-mode is active

(ert-deftest project-projectile/project-current-returns-projectile-object ()
  "project-current returns a (projectile . root) cons when projectile-mode is on."
  (project-test--with-project-dir ".projectile"
    (let ((project-find-functions '(project-projectile)))
      (let ((proj (project-current nil dir)))
        (should proj)
        (should (consp proj))
        (should (eq (car proj) 'projectile))))))

(ert-deftest project-projectile/project-current-uses-projectile-when-mode-on ()
  "Enabling projectile-mode adds project-projectile to project-find-functions."
  (unwind-protect
      (progn
        (projectile-mode 1)
        (should (member #'project-projectile project-find-functions)))
    (projectile-mode -1)))

;; ---------------------------------------------------------------------------
;; project-root delegates to projectile

(ert-deftest project-projectile/project-root-matches-projectile-project-root ()
  "project-root on a projectile project object matches projectile-project-root."
  (project-test--with-project-dir ".projectile"
    (let ((project-find-functions '(project-projectile)))
      (let* ((proj (project-current nil dir))
             (via-project-el (and proj (project-root proj)))
             (via-projectile (projectile-project-root dir)))
        (should via-project-el)
        (should via-projectile)
        (should (file-equal-p via-project-el via-projectile))))))

;; ---------------------------------------------------------------------------
;; project-name: default behaviour (no projectile override)

(ert-deftest project-projectile/project-name-default-returns-basename ()
  "Without a cl-defmethod override, project-name returns the root directory basename.
This documents the gap: it ignores projectile-project-name / projectile-project-name-function."
  (project-test--with-project-dir ".projectile"
    (let ((project-find-functions '(project-projectile)))
      (let* ((proj (project-current nil dir))
             (name (and proj (project-name proj)))
             (expected (file-name-nondirectory (directory-file-name (project-root proj)))))
        (should (stringp name))
        (should (string-equal name expected))))))

;; ---------------------------------------------------------------------------
;; project-name: with cl-defmethod override to delegate to projectile
;;
;; To close the gap between project-name and projectile-project-name, add
;; this to your init AFTER projectile is loaded:
;;
;;   (with-eval-after-load 'projectile
;;     (cl-defmethod project-name ((project (head projectile)))
;;       (projectile-project-name (project-root project))))
;;
;; The tests below verify that this override behaves correctly.

(defun project-test--projectile-name-method-defined-p ()
  "Return non-nil if the project-name method for (head projectile) is registered."
  (let* ((g (cl--generic 'project-name))
         (mt (and g (cl--generic-method-table g))))
    (cl-some (lambda (m)
               (equal (cl--generic-method-specializers m) '((head projectile))))
             mt)))

(ert-deftest project-projectile/project-name-with-override-uses-projectile ()
  "When the cl-defmethod override is in place, project-name delegates to projectile-project-name."
  (skip-unless (project-test--projectile-name-method-defined-p))
  (project-test--with-project-dir ".projectile"
    (let ((project-find-functions '(project-projectile)))
      (let* ((proj (project-current nil dir))
             (via-project-el (and proj (project-name proj)))
             (via-projectile (projectile-project-name (project-root proj))))
        (should (stringp via-project-el))
        (should (string-equal via-project-el via-projectile))))))

(ert-deftest project-projectile/project-name-override-differs-from-basename-when-custom ()
  "When projectile-project-name-function is customised, the override diverges from basename."
  (skip-unless (project-test--projectile-name-method-defined-p))
  (project-test--with-project-dir ".projectile"
    (let* ((project-find-functions '(project-projectile))
           (projectile-project-name-function (lambda (_root) "custom-name")))
      (let* ((proj (project-current nil dir))
             (name (and proj (project-name proj))))
        (should (string-equal name "custom-name"))))))

;; ---------------------------------------------------------------------------
;; project-buffers: default behaviour (no projectile override)
;;
;; Projectile does not define cl-defmethod project-buffers for (head projectile),
;; so project-buffers falls through to the default project.el implementation,
;; which filters buffer-list by file-in-directory-p.  projectile-project-buffers
;; may return a different (larger or smaller) set depending on its own filtering.

(ert-deftest project-projectile/project-buffers-returns-list ()
  "project-buffers returns a list (possibly empty) for a projectile project."
  (project-test--with-project-dir ".projectile"
    (let ((project-find-functions '(project-projectile)))
      (let* ((proj (project-current nil dir)))
        (should proj)
        (should (listp (project-buffers proj)))))))

;; ---------------------------------------------------------------------------
;; projectile cache fallback for non-writable locations

(ert-deftest project-projectile/cache-file-in-tree-when-writable ()
  "projectile-project-cache-file returns in-tree path when project root is writable."
  (project-test--with-project-dir ".projectile"
    (let ((cache-file (projectile-project-cache-file dir)))
      (should (equal cache-file (expand-file-name ".projectile-cache.eld" dir))))))

(ert-deftest project-projectile/cache-file-fallback-when-non-writable ()
  "projectile-project-cache-file returns fallback state path when project root is not writable."
  (let* ((non-writable-root "/usr/lib/go/src")
         (cache-file (projectile-project-cache-file non-writable-root)))
    (should (string-prefix-p (expand-file-name projectile-cache-fallback-directory) cache-file))
    (should (string-match-p "src-" cache-file))
    (should (string-suffix-p ".eld" cache-file))))

(ert-deftest project-projectile/cache-project-and-load-on-non-writable-root ()
  "Caching and loading on a non-writable root uses the fallback cache without warnings."
  (let* ((non-writable-root "/usr/lib/go/src")
         (dummy-files '("pkg/a.go" "pkg/b.go"))
         (cache-file (projectile-project-cache-file non-writable-root)))
    (unwind-protect
        (progn
          (projectile-cache-project non-writable-root dummy-files)
          (should (file-exists-p cache-file))
          (should (equal (projectile-load-project-cache non-writable-root) dummy-files)))
      (when (file-exists-p cache-file)
        (delete-file cache-file)))))

(ert-deftest project-projectile/serialize-silent-on-non-writable ()
  "projectile-serialize silently writes to fallback if passed a non-writable path directly."
  (let* ((bad-file "/usr/lib/go/src/.unwritable-test-cache.eld")
         (warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (projectile-serialize '("test") bad-file)
      (should-not warned))))
(provide 'project-projectile-integration-test)
;;; project-projectile-integration-test.el ends here
