;;; test-setup-packages.el --- Tests for setup-packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'seq)
(require 'elpaish-install)

(defun test-setup-packages--read-forms ()
  "Read all top-level forms from `setup-packages.el'."
  (let ((file (locate-file "setup-packages" load-path '(".el")))
        (forms nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun test-setup-packages--get-upgrade-list ()
  "Extract package symbols from the upgrade registration form."
  (let* ((forms (test-setup-packages--read-forms))
         (form (seq-find (lambda (f)
                           (and (consp f)
                                (eq (car f) 'elpaish-install-register-upgrade-package)))
                         forms)))
    (cadr (cadr form))))

(defun test-setup-packages--get-install-list ()
  "Extract package symbols from the install registration form."
  (let* ((forms (test-setup-packages--read-forms))
         (form (seq-find (lambda (f)
                           (and (consp f)
                                (eq (car f) 'elpaish-install-register-package)))
                         forms)))
    (cadr (cadr form))))

(ert-deftest setup-packages/no-duplicates-in-upgrade-list ()
  "Ensure the upgrade packages catalog has no duplicate package symbols."
  (let ((pkgs (test-setup-packages--get-upgrade-list)))
    (should pkgs)
    (should (equal pkgs (delete-dups (copy-sequence pkgs))))))

(ert-deftest setup-packages/no-duplicates-in-install-list ()
  "Ensure the install packages catalog has no duplicate package symbols."
  (let ((pkgs (test-setup-packages--get-install-list)))
    (should pkgs)
    (should (equal pkgs (delete-dups (copy-sequence pkgs))))))

(ert-deftest setup-packages/disjoint-upgrade-and-install-lists ()
  "Ensure upgrade and install package catalogs are completely disjoint."
  (let ((upgrades (test-setup-packages--get-upgrade-list))
        (installs (test-setup-packages--get-install-list)))
    (should upgrades)
    (should installs)
    (should-not (seq-intersection upgrades installs))))

(ert-deftest setup-packages/no-local-packages-in-catalogs ()
  "Ensure packages local to .emacs.d/lisp are not registered with elpaish."
  (let ((upgrades (test-setup-packages--get-upgrade-list))
        (installs (test-setup-packages--get-install-list))
        (local-pkgs '(eglox grepx hud-modeline hud minaid)))
    (dolist (pkg local-pkgs)
      (should-not (memq pkg upgrades))
      (should-not (memq pkg installs)))))

(ert-deftest setup-packages/registration-integration ()
  "Ensure loading `setup-packages' registers all packages in elpaish."
  (let ((elpaish-install-packages-list nil)
        (elpaish-install-upgrade-packages-list nil)
        (upgrades (test-setup-packages--get-upgrade-list))
        (installs (test-setup-packages--get-install-list)))
    (load (locate-file "setup-packages" load-path '(".el")) nil t t)
    ;; Upgrade packages must be in upgrade list and install list
    (should (equal elpaish-install-upgrade-packages-list upgrades))
    (dolist (pkg upgrades)
      (should (memq pkg elpaish-install-packages-list)))
    ;; Install packages must be in install list but NOT upgrade list
    (dolist (pkg installs)
      (should (memq pkg elpaish-install-packages-list))
      (should-not (memq pkg elpaish-install-upgrade-packages-list)))))

(provide 'test-setup-packages)
;;; test-setup-packages.el ends here
