;;; test-minaid.el --- ERT tests for minaid.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'test-helper)
(require 'cl-lib)
(require 'hud-mode)
(require 'minaid)
(require 'comint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alias and Keymap Tests

(ert-deftest minaid-test/minaid-map-alias ()
  "Test that minaid-map aliases minaid-completion-map, and all keymaps are defined."
  (should (boundp 'minaid-completion-map))
  (should (boundp 'minaid-map))
  (should (eq minaid-map minaid-completion-map))
  (should (boundp 'minaid-consult-map))
  (should (boundp 'minaid-consult-search-map))
  (should (boundp 'minaid-global-map)))

(ert-deftest minaid-test/completion-bindings-installed ()
  "Test that completion bindings are registered top-level in minaid keymaps."
  (should (eq (keymap-lookup minaid-completion-map "TAB") #'completion-at-point))
  (should (eq (keymap-lookup minaid-completion-map "/") #'dabbrev-expand))
  (should (eq (keymap-lookup minaid-completion-map "c") #'dabbrev-completion))
  (should (eq (keymap-lookup minaid-completion-map "i") #'tempel-insert))
  (should (eq (keymap-lookup minaid-completion-map "d") #'cape-dabbrev))
  (should (eq (keymap-lookup minaid-completion-map "h") #'cape-history))
  (should (eq (keymap-lookup minaid-completion-map "f") #'cape-file))
  (should (eq (keymap-lookup minaid-completion-map ";") #'corfu-at-point))
  (should (eq (keymap-lookup minaid-consult-map "s") #'tempel-insert))
  (should (eq (keymap-lookup minaid-consult-map "h") #'consult-history))
  (should (eq (keymap-lookup minaid-consult-search-map "l") #'consult-line))
  (should (eq (keymap-lookup minaid-global-map "C-.") #'embark-act))
  (should (eq (keymap-lookup minaid-global-map "C-x b") #'consult-buffer))
  (should (eq (keymap-lookup minaid-global-map "M-g r") #'consult-rg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode Lifecycle Tests

(ert-deftest minaid-test/lifecycle-toggle ()
  "Test that enabling and disabling minaid-mode toggles settings symmetrically."
  (let ((orig-styles completion-styles)
        (orig-overrides completion-category-overrides))
    ;; Enable mode
    (minaid-mode 1)
    (should minaid-mode)
    (should global-corfu-mode)
    (should (equal completion-styles '(orderless basic)))
    (should (assq 'file completion-category-overrides))
    (should (memq #'minaid-elisp-capf-setup emacs-lisp-mode-hook))
    (should (memq #'minaid-corfu-prog-mode-setup prog-mode-hook))
    (should (memq #'minaid-corfu-prog-mode-setup comint-mode-hook))
    (should (eq (lookup-key minaid-mode-map (kbd "C-.")) #'embark-act))

    ;; Disable mode
    (minaid-mode -1)
    (should-not minaid-mode)
    (should-not global-corfu-mode)
    (should (equal completion-styles orig-styles))
    (should (equal completion-category-overrides orig-overrides))
    (should-not (memq #'minaid-elisp-capf-setup emacs-lisp-mode-hook))
    (should-not (memq #'minaid-corfu-prog-mode-setup prog-mode-hook))
    (should-not (memq #'minaid-corfu-prog-mode-setup comint-mode-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Capf Configuration & Dispatch Tests

(defun test--dummy-wrapper (inner)
  (funcall inner))

(defun test--dummy-inner ()
  'test-result)

(ert-deftest minaid-test/cape-capf-wrapper-macro ()
  "Test that cape-capf-wrapper creates a callable symbol."
  (eval '(cape-capf-wrapper test--dummy-wrapper test--dummy-inner))
  (let ((sym (intern "test--dummy-inner-<test--dummy-wrapper>")))
    (should (fboundp sym))
    (should (eq (funcall sym) 'test-result))))

(ert-deftest minaid-test/setup-capfs-elisp ()
  "Test setup-capfs in an emacs-lisp-mode buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (minaid-elisp-capf-setup)
    (should (memq #'cape-elisp-symbol completion-at-point-functions))
    (should (memq #'tempel-complete completion-at-point-functions))
    (should (memq #'cape-dabbrev completion-at-point-functions))))

(ert-deftest minaid-test/setup-capfs-prog ()
  "Test setup-capfs in a prog-mode buffer."
  (with-temp-buffer
    (prog-mode)
    (minaid-prog-mode-capf-setup)
    (should (memq #'tempel-complete completion-at-point-functions))
    (should (memq #'cape-file completion-at-point-functions))
    (should (memq #'cape-dabbrev completion-at-point-functions))))

(ert-deftest minaid-test/setup-capfs-text ()
  "Test setup-capfs in a text-mode buffer."
  (with-temp-buffer
    (text-mode)
    (minaid-text-mode-capf-setup)
    (should (memq #'tempel-complete completion-at-point-functions))
    (should (memq #'cape-file completion-at-point-functions))
    (should (memq #'cape-dabbrev completion-at-point-functions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Corfu Hook Tests

(ert-deftest minaid-test/corfu-mode-prefixes ()
  "Test corfu auto prefixes in text vs prog modes."
  (with-temp-buffer
    (minaid-corfu-text-mode-setup)
    (should (= corfu-auto-prefix 2)))
  (with-temp-buffer
    (minaid-corfu-prog-mode-setup)
    (should (= corfu-auto-prefix 3))))

(ert-deftest minaid-test/corfu-enabled-in-all-buffers ()
  "Test that minaid-mode enables corfu in text, prog, fundamental, comint buffers."
  (let ((noninteractive nil))
    (minaid-mode 1)
    (unwind-protect
        (dolist (mode '(fundamental-mode text-mode prog-mode comint-mode))
          (let ((buf (generate-new-buffer (format "test-corfu-%s" mode))))
            (unwind-protect
                (with-current-buffer buf
                  (funcall mode)
                  (should corfu-mode))
              (kill-buffer buf))))
      (minaid-mode -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion In-Region Wrapper Tests

(ert-deftest minaid-test/corfu-at-point-handler ()
  "Test that corfu-at-point binds completion-in-region-function."
  (let ((invoked nil))
    (cl-letf (((symbol-function 'completion-at-point)
               (lambda ()
                 (setq invoked completion-in-region-function))))
      (corfu-at-point)
      (should (eq invoked #'corfu--in-region)))))

(ert-deftest minaid-test/consult-at-point-handler ()
  "Test that consult-at-point binds completion-in-region-function."
  (let ((invoked nil))
    (cl-letf (((symbol-function 'completion-at-point)
               (lambda ()
                 (setq invoked completion-in-region-function))))
      (consult-at-point)
      (should (eq invoked #'consult-completion-in-region)))))

(provide 'test-minaid)
;;; test-minaid.el ends here
