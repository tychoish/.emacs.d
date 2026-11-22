;;; test-setup-packages.el --- Tests for package catalogs in bootstrap.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'seq)
(require 'bootstrap)
(require 'elpaish-install)

(defun test-setup-packages--get-upgrade-list ()
  "Extract package symbols from all upgrade groups in `bootstrap-packages'."
  (seq-mapcat #'cdr (seq-remove (lambda (entry) (memq (car entry) '(install install-only)))
                                bootstrap-packages)))

(defun test-setup-packages--get-install-list ()
  "Extract package symbols from install group in `bootstrap-packages'."
  (alist-get 'install bootstrap-packages))

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
  "Ensure configuration modules local to .emacs.d/lisp are not registered with elpaish."
  (let ((upgrades (test-setup-packages--get-upgrade-list))
        (installs (test-setup-packages--get-install-list))
        (local-modules '(bootstrap setup-defaults
                         setup-modes setup-packages setup-project)))
    (dolist (mod local-modules)
      (should-not (memq mod upgrades))
      (should-not (memq mod installs)))))

(ert-deftest setup-packages/registration-integration ()
  "Ensure calling `bootstrap-register-packages' registers all packages in elpaish."
  (let ((elpaish-install-packages-list nil)
        (elpaish-install-upgrade-packages-list nil)
        (upgrades (test-setup-packages--get-upgrade-list))
        (installs (test-setup-packages--get-install-list)))
    (bootstrap-register-packages)
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
