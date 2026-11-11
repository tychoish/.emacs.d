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
(require 'eglox-project)

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

(ert-deftest project-projectile/cache-file-is-central ()
  "Writable and unwritable project roots both use the central cache."
  (let ((eglox-project-cache-directory (make-temp-file "pp-cache-" t)))
    (unwind-protect
        (project-test--with-project-dir ".projectile"
          (let ((file (eglox-project--cache-file nil dir)))
            (should (string-prefix-p eglox-project-cache-directory file))
            (should-not (file-exists-p (expand-file-name ".projectile-cache.eld" dir)))
            (should-not (equal file (eglox-project--cache-file nil "/usr/lib/go/src")))))
      (delete-directory eglox-project-cache-directory t))))

(ert-deftest project-projectile/serialize-warns-on-non-writable ()
  "Unwritable non-cache data is not silently redirected to an unread path."
  (let* ((bad-file "/usr/lib/go/src/.unwritable-test-cache.eld")
         (warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (projectile-serialize '("test") bad-file)
      (should warned))))
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
  "Killed buffers are skipped without aborting the operation."
  (let ((live (generate-new-buffer " *pp-live*"))
        (dead (generate-new-buffer " *pp-dead*")))
    (kill-buffer dead)
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-mode) #'ignore))
          (should (projectile--set-mode-in-buffer 1 live))
          (should-not (projectile--set-mode-in-buffer 1 dead)))
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
        (window-put nil)
        (original-window (frame-root-window (selected-frame))))
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
    (should (eq (car window-put) 'ws))
    (should (eq (cadr window-put) original-window))))

(ert-deftest project-projectile/session-restore-async-no-session ()
  "Without saved data, noninteractive calls are silent no-ops."
  (cl-letf (((symbol-function 'projectile-session--read) (lambda (_) nil)))
    (should-not (projectile-session-restore-async "/fake/"))))

(provide 'project-projectile-integration-test)
;;; project-projectile-integration-test.el ends here
