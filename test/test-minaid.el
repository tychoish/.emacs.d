;;; test-minaid.el --- ERT tests for minaid.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'test-helper)
(require 'cl-lib)
(require 'minaid)
(require 'grepx)
(require 'comint)
(require 'corfu-quick)

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
  ;; Search submap mounted under minaid-consult-map "s"
  (should (eq (keymap-lookup minaid-consult-map "s l") #'consult-line))
  (should (eq (keymap-lookup minaid-consult-map "s m") #'consult-line-multi))
  (should (eq (keymap-lookup minaid-consult-map "s k") #'consult-keep-lines))
  (should (eq (keymap-lookup minaid-consult-map "s f") #'consult-focus-lines))
  (should (eq (keymap-lookup minaid-consult-map "t") #'tempel-insert))
  (should (eq (keymap-lookup minaid-consult-map "h") #'consult-history))
  (should (eq (keymap-lookup minaid-consult-map "d") #'grepx-live-pwd))
  (should (eq (keymap-lookup minaid-consult-map "r") #'grepx-live))
  (should (eq (keymap-lookup minaid-consult-map "g") #'grepx-project))
  (should (eq (keymap-lookup minaid-consult-map "m") #'grepx-menu))
  (should (eq (keymap-lookup minaid-consult-search-map "l") #'consult-line))
  (should (eq (keymap-lookup minaid-consult-search-map "r") #'grepx-live))
  (should (eq (keymap-lookup minaid-consult-search-map "g") #'grepx-project))
  (should (eq (keymap-lookup minaid-consult-search-map "d") #'grepx-directory))
  (should (eq (keymap-lookup minaid-consult-search-map "?") #'grepx-menu))
  (should (eq (keymap-lookup minaid-global-map "C-.") #'embark-act))
  (should (eq (keymap-lookup minaid-global-map "C-x b") #'consult-buffer))
  (should (eq (keymap-lookup minaid-global-map "M-g r") #'grepx-live))
  (should (eq (keymap-lookup minaid-global-map "M-s r") #'grepx-live))
  (should (eq (keymap-lookup minaid-global-map "M-s g") #'grepx-project))
  (should (eq (keymap-lookup minaid-global-map "M-s d") #'grepx-directory))
  (should (eq (keymap-lookup minaid-global-map "M-s f") #'consult-find-at-point))
  (should (eq (keymap-lookup minaid-global-map "M-s m") #'grepx-menu)))

(ert-deftest minaid-test/corfu-quick-bindings-installed ()
  "Test that corfu-quick functions are bound and defined in corfu-map."
  (should (fboundp 'corfu-quick-jump))
  (should (fboundp 'corfu-quick-complete))
  (should (fboundp 'corfu-quick-insert))
  (minaid-mode 1)
  (unwind-protect
      (progn
        (should (eq (keymap-lookup corfu-map "C-j") #'corfu-quick-jump))
        (should (eq (keymap-lookup corfu-map "C-<tab>") #'corfu-quick-complete))
        (should (eq (keymap-lookup corfu-map "C-<return>") #'corfu-quick-insert)))
    (minaid-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode Lifecycle Tests

(ert-deftest minaid-test/lifecycle-toggle ()
  "Test that enabling and disabling minaid-mode toggles settings symmetrically."
  (let ((orig-styles completion-styles)
        (orig-overrides completion-category-overrides)
        (orig-xref-xrefs xref-show-xrefs-function)
        (orig-xref-defs xref-show-definitions-function)
        (orig-marginalia-hooks (copy-sequence marginalia-mode-hook))
        (orig-corfu-hooks (copy-sequence corfu-mode-hook)))
    ;; Enable mode
    (minaid-mode 1)
    (should minaid-mode)
    (should global-corfu-mode)
    (should (equal completion-styles '(orderless basic)))
    (should (assq 'file completion-category-overrides))
    (should (eq xref-show-xrefs-function 'consult-xref))
    (should (eq xref-show-definitions-function 'consult-xref))
    (should (memq #'minaid-elisp-capf-setup emacs-lisp-mode-hook))
    (should (memq #'minaid-corfu-prog-mode-setup prog-mode-hook))
    (should (memq #'minaid-corfu-prog-mode-setup comint-mode-hook))
    (should (memq #'minaid-corfu-agent-shell-setup agent-shell-mode-hook))
    (should (eq (lookup-key minaid-mode-map (kbd "C-.")) #'embark-act))

    ;; Disable mode
    (minaid-mode -1)
    (should-not minaid-mode)
    (should-not global-corfu-mode)
    (should (equal completion-styles orig-styles))
    (should (equal completion-category-overrides orig-overrides))
    (should (eq xref-show-xrefs-function orig-xref-xrefs))
    (should (eq xref-show-definitions-function orig-xref-defs))
    (should-not (memq #'minaid-elisp-capf-setup emacs-lisp-mode-hook))
    (should-not (memq #'minaid-corfu-prog-mode-setup prog-mode-hook))
    (should-not (memq #'minaid-corfu-prog-mode-setup comint-mode-hook))
    (should-not (memq #'minaid-corfu-agent-shell-setup agent-shell-mode-hook))
    (should-not (memq #'corfu-history-mode corfu-mode-hook))
    (should-not (memq #'corfu-indexed-mode corfu-mode-hook))
    (should-not (memq #'corfu-popupinfo-mode corfu-mode-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Capf Configuration & Dispatch Tests

(defun test--dummy-wrapper (inner)
  (lambda () (cons 'wrapped (funcall inner))))

(defun test--dummy-inner ()
  'test-result)

(ert-deftest minaid-test/cape-capf-wrapper ()
  "Test that cape-capf-wrapper wraps the inner function cleanly without macro expansion."
  (let ((wrapped (cape-capf-wrapper #'test--dummy-wrapper #'test--dummy-inner)))
    (should (functionp wrapped))
    (should (equal (funcall wrapped) '(wrapped . test-result)))))

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

(ert-deftest minaid-test/setup-capfs-eglot-minor-mode-detection ()
  "Test that setup-capfs detects bound-and-true-p eglot--managed-mode."
  (with-temp-buffer
    (prog-mode)
    (setq-local eglot--managed-mode t)
    (setup-capfs)
    ;; Should have configured eglot capf, not plain prog-mode capf
    (should (assq 'completion-category-defaults (buffer-local-variables)))
    (should (memq #'tempel-complete completion-at-point-functions))
    (should (memq #'cape-dabbrev completion-at-point-functions))))

(ert-deftest minaid-test/eglot-capf-setup-cape-wrap-buster ()
  "Test that minaid-eglot-capf-setup wraps eglot-completion-at-point directly."
  (with-temp-buffer
    (minaid-eglot-capf-setup)
    (should completion-at-point-functions)
    ;; The first function is either eglot-completion-at-point or a cape-wrap-buster closure
    (let ((first-capf (car completion-at-point-functions)))
      (should (functionp first-capf)))))

(ert-deftest minaid-test/setup-capfs-non-file-buffer-error ()
  "Test that setup-capfs signals user-error when buffer is not visiting a file."
  (with-temp-buffer
    ;; Fundamental-mode buffer without a file
    (setq-local buffer-file-name nil)
    (should-error (setup-capfs) :type 'user-error)))

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

(ert-deftest minaid-test/corfu-agent-shell-setup ()
  "Test that agent-shell corfu setup disables ambient auto-completion."
  (with-temp-buffer
    (minaid-corfu-agent-shell-setup)
    (should corfu-mode)
    (should-not corfu-auto)
    (should corfu-quit-no-match)
    (should corfu-quit-at-boundary)))

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

(ert-deftest minaid-test/minibuffer-and-isearch-bindings ()
  "Test that minaid binds built-in minibuffer-local-map and isearch-mode-map at top level."
  (should (eq (lookup-key minibuffer-local-map (kbd "M-s")) #'consult-history))
  (should (eq (lookup-key minibuffer-local-map (kbd "M-r")) #'consult-history))
  (should (eq (lookup-key minibuffer-local-map (kbd "C-c a")) #'marginalia-cycle))
  (should (eq (lookup-key isearch-mode-map (kbd "M-e")) #'consult-isearch-history))
  (should (eq (lookup-key isearch-mode-map (kbd "M-s e")) #'consult-isearch-history))
  (should (eq (lookup-key isearch-mode-map (kbd "M-s l")) #'consult-line))
  (should (eq (lookup-key isearch-mode-map (kbd "M-s L")) #'consult-line-multi)))

(ert-deftest minaid-test/hud-docs-map-bindings-in-hud-mode ()
  "Test that hud-docs-map in hud-mode binds documentation commands."
  (require 'hud-mode)
  (should (eq (lookup-key hud-docs-map "i") #'consult-info))
  (should (eq (lookup-key hud-docs-map "m") #'consult-man))
  (should (eq (lookup-key hud-docs-map "a") #'consult-eglot-symbols)))
