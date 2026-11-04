;;; test-chex.el --- Tests for checking integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for chex mode and checker integration.

;;; Code:
(require 'ert)
(require 'chex)

(ert-deftest chex-mode-hooks-and-keys ()
  "Enabling and disabling Chex controls hooks and global bindings."
  (let ((was-enabled chex-global-mode))
    (unwind-protect
        (progn
          (chex-global-mode 1)
          (should (memq #'chex--flyspell-prog prog-mode-hook))
          (should (memq #'chex--flyspell-text text-mode-hook))
          (should (eq (lookup-key chex-global-mode-map (kbd "M-g f")) 'flycheck-mode))
          (chex-global-mode -1)
          (should-not (memq #'chex--flyspell-prog prog-mode-hook))
          (should-not (memq #'chex--flyspell-text text-mode-hook))
          (should-not (advice-member-p #'chex--ispell-cache-params 'ispell-set-spellchecker-params)))
      (chex-global-mode (if was-enabled 1 -1)))))

(ert-deftest chex-checkers-available-after-flycheck-load ()
  "Aspell checkers and the Flycheck command keys are registered."
  (let ((was-enabled chex-global-mode))
    (unwind-protect
        (progn
          (chex-global-mode 1)
          (require 'flycheck)
          (should (memq 'org-aspell-dynamic flycheck-checkers))
          (should (memq 'rst-aspell-dynamic flycheck-checkers))
          (should (flycheck-valid-checker-p 'org-aspell-dynamic))
          (should (flycheck-valid-checker-p 'rst-aspell-dynamic))
          (should (eq (lookup-key flycheck-command-map (kbd ";")) 'consult-flycheck))
          (should (eq (lookup-key flycheck-mode-map (kbd "C-c f")) flycheck-command-map)))
      (chex-global-mode (if was-enabled 1 -1)))))

(ert-deftest chex-pending-spell-check-is-cancelled ()
  "Disabling Chex cancels scheduled spelling checks."
  (let ((was-enabled chex-global-mode))
    (unwind-protect
        (with-temp-buffer
          (chex-global-mode 1)
          (chex--flyspell-text)
          (should (timerp chex--flyspell-timer))
          (chex-global-mode -1)
          (should-not chex--flyspell-timer))
      (chex-global-mode (if was-enabled 1 -1)))))

(ert-deftest chex-eglot-bridge-configuration ()
  "Chex configures the Eglot checker through the bridge mode hook."
  (let ((was-enabled chex-global-mode))
    (unwind-protect
        (progn
          (chex-global-mode 1)
          (should (memq #'chex--eglot-setup flycheck-eglot-mode-hook))
          (should-not (default-value 'flycheck-eglot-exclusive))
          (should-not flycheck-eglot-enable-diagnostic-tags)
          (flycheck-eglot-mode -1)
          (should (memq 'eglot-check flycheck-checkers))
          (should (memq 'go-gofmt (flycheck-checker-get 'eglot-check 'next-checkers)))
          (chex-global-mode -1)
          (should-not (memq #'chex--eglot-setup flycheck-eglot-mode-hook)))
      (chex-global-mode (if was-enabled 1 -1)))))

(ert-deftest chex-managed-defaults-restore-and-preserve-local-values ()
  "Mode toggles restore defaults without overwriting buffer-local values."
  (let ((was-enabled chex-global-mode))
    (unwind-protect
        (progn
          (chex-global-mode -1)
          (let ((old (default-value 'flycheck-idle-change-delay)))
            (unwind-protect
                (with-temp-buffer
                  (setq-local flycheck-idle-change-delay 9)
                  (set-default 'flycheck-idle-change-delay 7)
                  (chex-global-mode 1)
                  (should (= (default-value 'flycheck-idle-change-delay) 1))
                  (should (= flycheck-idle-change-delay 9))
                  (chex-global-mode 1)
                  (chex-global-mode -1)
                  (should (= (default-value 'flycheck-idle-change-delay) 7))
                  (should (= flycheck-idle-change-delay 9)))
              (set-default 'flycheck-idle-change-delay old))))
      (chex-global-mode (if was-enabled 1 -1)))))

(ert-deftest chex-restores-buffer-local-settings ()
  "Disabling Chex restores prior local values and its Eldoc hook."
  (let ((was-enabled chex-global-mode))
    (unwind-protect
        (with-temp-buffer
          (chex-global-mode 1)
          (setq-local flycheck-disabled-checkers '(existing))
          (chex--go-setup)
          (chex--flycheck-eldoc-setup)
          (should (memq #'chex-flycheck-eldoc eldoc-documentation-functions))
          (chex-global-mode -1)
          (should (equal flycheck-disabled-checkers '(existing)))
          (should-not (memq #'chex-flycheck-eldoc eldoc-documentation-functions)))
      (chex-global-mode (if was-enabled 1 -1)))))

(provide 'test-chex)
;;; test-chex.el ends here
