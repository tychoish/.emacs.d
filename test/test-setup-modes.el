;;; test-setup-modes.el --- Tests for setup-modes.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'seq)
(require 'setup-modes)

(defvar dired-mode-hook)
(defvar sqlite-mode-hook)
(defvar LaTeX-mode-hook)

(ert-deftest setup-modes/direct-settings-configured ()
  "Ensure major-mode-remap-alist, term-file-aliases, and hooks are set directly."
  (should (eq (cdr (assoc 'yaml-ts-mode major-mode-remap-alist)) 'yaml-mode))
  (should (equal (cdr (assoc "alacritty" term-file-aliases)) "xterm"))
  (should (equal (cdr (assoc "ghostty" term-file-aliases)) "xterm-ghostty"))
  (should (memq 'nerd-icons-dired-mode dired-mode-hook))
  (should (memq 'sqlite-extras-minor-mode sqlite-mode-hook))
  (should (memq 'turn-on-reftex LaTeX-mode-hook))
  (should (memq 'visual-line-mode LaTeX-mode-hook))
  (should (memq 'turn-off-auto-fill LaTeX-mode-hook)))

(ert-deftest setup-modes/merge-alist-overrides-existing ()
  "Ensure `setup-modes-merge-alist' overrides existing keys and preserves distinct keys."
  (let* ((current '(("\\.txt\\'" . text-mode)
                    ("\\.el\\'" . emacs-lisp-mode)
                    ("\\.py\\'" . python-mode)))
         (new '(("\\.txt\\'" . markdown-mode)
                ("\\.py\\'" . python-ts-mode)
                ("\\.go\\'" . go-ts-mode)))
         (merged (setup-modes-merge-alist current new)))
    ;; Check overridden values
    (should (eq (cdr (assoc "\\.txt\\'" merged)) 'markdown-mode))
    (should (eq (cdr (assoc "\\.py\\'" merged)) 'python-ts-mode))
    ;; Check preserved value
    (should (eq (cdr (assoc "\\.el\\'" merged)) 'emacs-lisp-mode))
    ;; Check new value
    (should (eq (cdr (assoc "\\.go\\'" merged)) 'go-ts-mode))
    ;; Check deduplication: only 1 entry per key
    (should (= 1 (seq-count (lambda (x) (equal (car x) "\\.txt\\'")) merged)))
    (should (= 1 (seq-count (lambda (x) (equal (car x) "\\.py\\'")) merged)))
    (should (= 4 (length merged)))))

(ert-deftest setup-modes/merge-alist-prepends-new-entries ()
  "Ensure `setup-modes-merge-alist' places new entries at the head of the list."
  (let* ((current '(("a" . 1) ("b" . 2)))
         (new '(("c" . 3) ("d" . 4)))
         (merged (setup-modes-merge-alist current new)))
    (should (equal (mapcar #'car (seq-take merged 2)) '("c" "d")))))

(ert-deftest setup-modes/initialize-merges-auto-mode-alist ()
  "Ensure `setup-modes-initialize' merges `setup-modes-auto-mode-alist' into `auto-mode-alist'."
  (let ((auto-mode-alist '(("\\.txt\\'" . text-mode))))
    (setup-modes-initialize)
    (should (eq (cdr (assoc "\\.txt\\'" auto-mode-alist)) 'markdown-mode))))

(ert-deftest setup-modes/registered-in-after-init-hook ()
  "Ensure `setup-modes-initialize' is registered on `after-init-hook'."
  (should (memq #'setup-modes-initialize after-init-hook)))

(provide 'test-setup-modes)
;;; test-setup-modes.el ends here
