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
(require 'setup-project)

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
  (let ((was-on (bound-and-true-p projectile-mode)))
    (unwind-protect
        (progn
          (unless was-on (projectile-mode 1))
          (should (member #'project-projectile project-find-functions)))
      (unless was-on
        (projectile-mode -1)))))

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
;; ---------------------------------------------------------------------------
;; incremental helpers

(defun project-test--wait (pred)
  "Spin the event loop until PRED is non-nil or 3s elapse."
  (let ((start (float-time)))
    (while (and (not (funcall pred)) (< (- (float-time) start) 3.0))
      (sit-for 0.01))))

(ert-deftest project-projectile/run-incrementally-counts-handled-items ()
  "on-done receives the number of items for which FN returned non-nil."
  (let ((done nil)
        (seen nil))
    (cl-letf (((symbol-function 'message) #'ignore))
      (projectile--run-incrementally
       (lambda (item) (push item seen) (cl-oddp item))
       '(1 2 3 4 5)
       :label "test" :on-done (lambda (n) (setq done n)))
      (project-test--wait (lambda () done)))
    (should (equal done 3))
    (should (equal (nreverse seen) '(1 2 3 4 5)))))

(ert-deftest project-projectile/enable-all-buffers-skips-dead-buffers ()
  "Killed buffers are not counted and do not abort the run."
  (let ((live (generate-new-buffer " *pp-live*"))
        (dead (generate-new-buffer " *pp-dead*"))
        (done nil))
    (kill-buffer dead)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'buffer-list) (lambda (&optional _) (list live dead))))
          (projectile-enable-all-buffers :on-done (lambda (n) (setq done n)))
          (project-test--wait (lambda () done))
          (should (equal done 1))
          (with-current-buffer live (should projectile-mode)))
      (kill-buffer live))))

(ert-deftest project-projectile/load-project-buffers-errors ()
  "Signal `user-error' with no root, or with no files."
  (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _) nil)))
    (should-error (projectile-load-project-buffers) :type 'user-error))
  (cl-letf (((symbol-function 'projectile-project-files) (lambda (_) nil)))
    (should-error (projectile-load-project-buffers "/tmp/") :type 'user-error)))

(ert-deftest project-projectile/session-restore-async-recreates-buffers ()
  "Each saved buffer is recreated and the window state is restored once."
  (let ((done nil)
        (recreated nil)
        (window-put nil))
    (cl-letf (((symbol-function 'message) #'ignore)
              ((symbol-function 'projectile-session--read)
               (lambda (_) (list :buffers '(a b) :window-state 'ws)))
              ((symbol-function 'projectile-session--recreate-buffer)
               (lambda (saved) (push saved recreated) (current-buffer)))
              ((symbol-function 'projectile-session--sanitize-window-state) #'identity)
              ((symbol-function 'window-state-put) (lambda (&rest args) (setq window-put args))))
      (projectile-session-restore-async "/fake/" :on-done (lambda (n) (setq done n)))
      (project-test--wait (lambda () done)))
    (should (equal done 2))
    (should (equal (nreverse recreated) '(a b)))
    (should (eq (car window-put) 'ws))))

(ert-deftest project-projectile/session-restore-async-no-session ()
  "Without saved data, noninteractive calls are silent no-ops."
  (cl-letf (((symbol-function 'projectile-session--read) (lambda (_) nil)))
    (should-not (projectile-session-restore-async "/fake/"))))

;; ---------------------------------------------------------------------------
;; cache fallback

(ert-deftest project-projectile/cache-fallback-file-is-stable-and-in-fallback-dir ()
  (let ((projectile-cache-fallback-directory (make-temp-file "pp-cache-" t)))
    (unwind-protect
        (let ((f (projectile-cache--fallback-file "/some/where/")))
          (should (equal f (projectile-cache--fallback-file "/some/where")))
          (should (string-prefix-p projectile-cache-fallback-directory f))
          (should (string-match-p "\\`where-[0-9a-f]+\\.eld\\'" (file-name-nondirectory f))))
      (delete-directory projectile-cache-fallback-directory t))))

(ert-deftest project-projectile/cache-file-uses-fallback-when-unwritable ()
  (let ((projectile-cache-fallback-directory (make-temp-file "pp-cache-" t)))
    (unwind-protect
        (progn
          (should (equal "/x/.cache" (cl-letf (((symbol-function 'file-writable-p) (lambda (_) t)))
                                       (projectile-project-cache-file-fallback (lambda (_) "/x/.cache") "/x/"))))
          (should (string-prefix-p projectile-cache-fallback-directory
                                   (cl-letf (((symbol-function 'file-writable-p) (lambda (_) nil)))
                                     (projectile-project-cache-file-fallback (lambda (_) "/x/.cache") "/x/")))))
      (delete-directory projectile-cache-fallback-directory t))))

(provide 'project-projectile-integration-test)
;;; project-projectile-integration-test.el ends here
