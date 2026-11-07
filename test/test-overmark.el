;;; test-overmark.el --- ERT tests for overmark.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Regression tests for the font-lock keyword generators moved from hud-mode.

;;; Code:

(require 'ert)
(require 'overmark)

(ert-deftest overmark-test/global-mode-manages-trailing-whitespace ()
  "Manage whitespace in existing and future text and programming buffers."
  (let ((prog-mode-hook nil)
        (text-mode-hook nil)
        (enable-theme-functions nil)
        (overmark-global-mode nil)
        (code (generate-new-buffer " *overmark-whitespace-code*"))
        (text (generate-new-buffer " *overmark-whitespace-text*"))
        (other (generate-new-buffer " *overmark-whitespace-other*")))
    (unwind-protect
        (progn
          (with-current-buffer code (prog-mode))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&rest _) (list code text other))))
            (unwind-protect
                (progn
                  (overmark-global-mode 1)
                  (should (buffer-local-value 'show-trailing-whitespace code))
                  (should-not (buffer-local-value 'show-trailing-whitespace other))
                  (with-current-buffer text (text-mode))
                  (should (buffer-local-value 'show-trailing-whitespace text))
                  (should (buffer-local-value 'overmark-mode text))
                  (overmark-global-mode -1)
                  (should-not (buffer-local-value 'show-trailing-whitespace code))
                  (should-not (buffer-local-value 'show-trailing-whitespace text))
                  (should-not (buffer-local-value 'overmark-mode text))
                  (should-not (memq #'overmark--set-trailing-whitespace prog-mode-hook))
                  (should-not (memq #'overmark--set-trailing-whitespace text-mode-hook)))
              (overmark-global-mode -1))))
      (kill-buffer code)
      (kill-buffer text)
      (kill-buffer other))))

(ert-deftest overmark-test/annotations-toggle ()
  (with-temp-buffer
    (text-mode)
    (insert "TK(one) TODO(two) ordinary")
    (font-lock-mode 1)
    (overmark-mode 1)
    (font-lock-ensure)
    (should (eq (get-text-property 1 'face) 'overmark-delimiter-face))
    (should (eq (get-text-property 4 'face) 'overmark-content-face))
    (should (eq (get-text-property 9 'face) 'overmark-delimiter-face))
    (overmark-mode -1)
    (font-lock-ensure)
    (should-not (get-text-property 1 'face))
    (should-not (get-text-property 9 'face))))

(ert-deftest overmark-test/global-mode-selects-text-buffers ()
  (unwind-protect
      (progn
        (overmark-global-mode 1)
        (with-temp-buffer
          (text-mode)
          (should overmark-mode)
          (emacs-lisp-mode)
          (should-not overmark-mode)))
    (overmark-global-mode -1)))

(ert-deftest overmark-test/global-mode-manages-existing-buffers ()
  (let ((buffer (generate-new-buffer "overmark-test-existing")))
    (unwind-protect
        (progn
          (overmark-global-mode -1)
          (with-current-buffer buffer (text-mode))
          (overmark-global-mode 1)
          (with-current-buffer buffer (should overmark-mode))
          (overmark-global-mode -1)
          (with-current-buffer buffer (should-not overmark-mode)))
      (overmark-global-mode -1)
      (kill-buffer buffer))))

(ert-deftest overmark-test/global-mode-text-derived-hooks ()
  (require 'org)
  (require 'markdown-mode)
  (unwind-protect
      (progn
        (overmark-global-mode 1)
        (should (memq #'overmark--turn-on text-mode-hook))
        (seq-do (lambda (mode)
                  (with-temp-buffer
                    (funcall mode)
                    (should overmark-mode)))
                '(text-mode org-mode markdown-mode gfm-mode)))
    (overmark-global-mode -1))
  (should-not (memq #'overmark--turn-on text-mode-hook)))

(ert-deftest overmark-test/global-mode-manages-theme-hook ()
  (let ((enable-theme-functions nil)
        (calls 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'overmark--setup-faces)
                     (lambda (&rest _) (setq calls (1+ calls)))))
            (overmark-global-mode 1)
            (should (= calls 1))
            (should (memq #'overmark--setup-faces enable-theme-functions))
            (run-hook-with-args 'enable-theme-functions 'test-theme)
            (should (= calls 2))
            (overmark-global-mode -1)
            (should-not (memq #'overmark--setup-faces enable-theme-functions))))
      (overmark-global-mode -1))))

(defvar features)

(ert-deftest overmark-test/theme-faces-ignore-unspecified-colors ()
  (let ((features (cons 'modus-themes features))
        (changes nil))
    (cl-letf (((symbol-function 'modus-themes-get-color-value)
               (lambda (color)
                 (if (eq color 'blue) "blue" 'unspecified)))
              ((symbol-function 'set-face-attribute)
               (lambda (&rest args) (push args changes))))
      (overmark--setup-faces)
      (should (equal changes '((overmark-delimiter-face nil
                                :foreground "blue" :weight bold)))))))

(ert-deftest overmark-test/show-tabs-matches-tab-only ()
  (let ((pattern (caar (overmark-font-lock-show-tabs))))
    (should (string-match pattern "\t"))
    (should-not (string-match pattern " "))
    (should (equal (overmark-font-lock-show-tabs) (overmark-font-lock-show-tabs)))))

(ert-deftest overmark-test/show-tabs-face ()
  (should (equal '(("\t" 0 'trailing-whitespace prepend)) (overmark-font-lock-show-tabs))))

(ert-deftest overmark-test/width-keyword-matches-overflow ()
  (let ((pattern (caar (overmark-font-lock-width-keyword 5))))
    (should (string-match pattern "abcdefg"))
    (should (equal "fg" (match-string 1 "abcdefg")))
    (should-not (string-match pattern "abcde"))
    (should-not (equal pattern (caar (overmark-font-lock-width-keyword 6))))))

(ert-deftest overmark-test/width-keyword-face ()
  (should (equal '(1 font-lock-warning-face t)
                 (cadar (overmark-font-lock-width-keyword 10)))))

(ert-deftest overmark-test/load-installs-no-hooks ()
  (let ((snapshot nil))
    (mapatoms (lambda (s)
                (when (and (boundp s) (string-suffix-p "-hook" (symbol-name s)))
                  (push (cons s (copy-tree (symbol-value s))) snapshot))))
    (load (locate-library "overmark") nil t)
    (seq-do (lambda (cell)
              (should (equal (cdr cell) (symbol-value (car cell)))))
            snapshot)))

(provide 'test-overmark)
;;; test-overmark.el ends here
