;;; test-setup-modes.el --- Tests for setup-modes.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'seq)
(require 'sprite-state)

(defvar dired-mode-hook)
(defvar sqlite-mode-hook)
(defvar LaTeX-mode-hook)

(ert-deftest setup-modes/direct-settings-configured ()
  "Ensure major-mode-remap-alist, term-file-aliases, auto-modes, and hooks are registered."
  (should (eq (cdr (assoc 'yaml-ts-mode major-mode-remap-alist)) 'yaml-mode))
  (should (equal (cdr (assoc "alacritty" term-file-aliases)) "xterm"))
  (should (equal (cdr (assoc "ghostty" term-file-aliases)) "xterm-ghostty"))
  (should (eq (cdr (assoc "\\.txt\\'" sprite-state-auto-mode-alist)) 'markdown-mode))
  (should (eq (cdr (assoc "\\.service\\'" sprite-state-auto-mode-alist)) 'conf-unix-mode))
  (should (eq (cdr (assoc "\\.rst\\'" sprite-state-auto-mode-alist)) 'rst-mode))
  (should (eq (cdr (assq 'dired-mode-hook sprite-state-mode-hooks)) 'nerd-icons-dired-mode))
  (should (eq (cdr (assq 'sqlite-mode-hook sprite-state-mode-hooks)) 'sqlite-extras-minor-mode))
  (should (equal (cdr (assq 'LaTeX-mode-hook sprite-state-mode-hooks))
                 '(turn-on-reftex visual-line-mode turn-off-auto-fill)))
  (should (eq (cdr (assq 'emacs-lisp-mode-hook sprite-state-mode-hooks)) 'builder-elisp-minor-mode)))

(ert-deftest setup-modes/registration-preserves-existing-entries ()
  "Ensure registration functions merge without overriding unrelated entries."
  (let ((sprite-state-global-mode nil)
        (sprite-state-auto-mode-alist '(("\\.custom\\'" . custom-mode)))
        (sprite-state-mode-hooks '((custom-mode-hook . custom-fn)
                                   (dired-mode-hook . pre-dired-fn))))
    (sprite-state-register-auto-modes '(("\\.txt\\'" . markdown-mode)))
    (sprite-state-register-mode-hooks '((dired-mode-hook . nerd-icons-dired-mode)))
    (should (eq (cdr (assoc "\\.custom\\'" sprite-state-auto-mode-alist)) 'custom-mode))
    (should (eq (cdr (assoc "\\.txt\\'" sprite-state-auto-mode-alist)) 'markdown-mode))
    (should (eq (cdr (assq 'custom-mode-hook sprite-state-mode-hooks)) 'custom-fn))
    (should (equal (cdr (assq 'dired-mode-hook sprite-state-mode-hooks))
                   '(pre-dired-fn nerd-icons-dired-mode)))))

(ert-deftest setup-modes/initialize-merges-auto-mode-alist-and-hooks ()
  "Ensure `sprite-state' initializes mode associations and hooks."
  (let ((auto-mode-alist '(("\\.txt\\'" . text-mode)))
        (dired-mode-hook nil)
        (sqlite-mode-hook nil)
        (LaTeX-mode-hook nil)
        (sprite-state--owned-hooks nil))
    (unwind-protect
        (progn
          (sprite-state-modes-initialize)
          (sprite-state--add-hooks)
          (should (eq (cdr (assoc "\\.txt\\'" auto-mode-alist)) 'markdown-mode))
          (should (memq 'nerd-icons-dired-mode dired-mode-hook))
          (should (memq 'sqlite-extras-minor-mode sqlite-mode-hook))
          (should (memq 'turn-on-reftex LaTeX-mode-hook))
          (should (memq 'visual-line-mode LaTeX-mode-hook))
          (should (memq 'turn-off-auto-fill LaTeX-mode-hook)))
      (sprite-state--remove-hooks))))

(provide 'test-setup-modes)
;;; test-setup-modes.el ends here
