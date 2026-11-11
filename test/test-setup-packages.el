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

(defun test-setup-packages--registered-packages (function)
  "Extract package symbols from all registration forms calling FUNCTION."
  (seq-reduce (lambda (packages form)
                (if (and (consp form) (eq (car form) function))
                    (append packages (cadr (cadr form)))
                  packages))
              (test-setup-packages--read-forms) nil))

(defun test-setup-packages--get-upgrade-list ()
  "Extract package symbols from all upgrade registration forms."
  (test-setup-packages--registered-packages 'elpaish-install-register-upgrade-package))

(defun test-setup-packages--get-install-list ()
  "Extract package symbols from all install registration forms."
  (test-setup-packages--registered-packages 'elpaish-install-register-package))

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
        (local-modules '(bootstrap setup-core setup-defaults
                         setup-modes setup-packages setup-mail setup-project setup-agent-shell)))
    (dolist (mod local-modules)
      (should-not (memq mod upgrades))
      (should-not (memq mod installs)))))

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
