;;; setup-defaults.el --- Standard library and built-in defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Global settings and configuration for built-in Emacs subsystems with no
;; use-package declarations.

;;; Code:

(require 'cl-lib)

(setq jit-lock-defer-time 0.2)
(setq jit-lock-stealth-nice 0.2)
(setq jit-lock-stealth-load 100)
(setq indicate-empty-lines t)
(setq tooltip-resize-echo-area t)

(setq backup-by-copying t)
(setq delete-old-versions t)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq find-file-visit-truename t)
(setq auto-revert-verbose nil)
(setq auto-revert-avoid-polling t)
(setq auto-revert-interval 60)

(setq recentf-auto-cleanup 'never)
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-menu-items 100)

(setq which-key-idle-delay .25)
(setq which-key-idle-secondary-delay 0.125)
(setq which-key-lighter "")

(setq frame-title-format '(:eval (format "%s:%s" sprite-instance-id (buffer-name))))
(setq sprite-mode-map-prefix (cons "s" 'hud-core-map))

(setq create-lockfiles nil)

(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

(setq cursor-in-non-selected-windows nil)
(setq scroll-conservatively 25)
(setq scroll-preserve-screen-position t)

(setq ring-bell-function #'ignore)
(setq truncate-lines t)
(setq use-dialog-box nil)
(setq use-short-answers t)
(setq load-prefer-newer t)
(setq indent-tabs-mode nil) ; (setq tab-width 4)
(setq shell-command-dont-erase-buffer 'end-last-out)
(setq undo-auto-current-boundary-timer t)
(setq comment-auto-fill-only-comments t)
(setq select-enable-clipboard nil) ;; select-enable-primary already defaults to nil.
(setq lpr-add-switches "-T ''")
(setq tex-dvi-view-command "(f=*; pdflatex \"${f%.dvi}.tex\" && open \"${f%.dvi}.pdf\")")

(setq save-abbrevs t)
(setq text-mode-ispell-word-completion nil)
(setq completion-cycle-threshold 2)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq makefile-electric-keys t)

(setq switch-to-prev-buffer-skip 'visible)
(setq split-height-threshold 100)
(setq window-sides-vertical t)
(setq ad-redefinition-action 'accept)

(setq checkdoc-force-docstrings-flag nil)
(setq checkdoc-spellcheck-documentation-flag t)

(setq show-paren-delay 0.25)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq byte-compile-warnings
      ;; OMIT: free-vars docstrings-wide
      '(callargs
        constants
        docstrings
        docstrings-non-ascii-quotes
        docstrings-control-chars
        empty-body
        ignored-return-value
        interactive-only
        lexical
        lexical-dynamic
        make-local
        mutate-constant
        noruntime
        not-unused
        obsolete
        redefine
        suspicious
        unresolved))

(defvar bootstrap-fallback-buffer-name "*scratch*"
  "Buffer name used as a last-resort fallback when no other buffer is available.
Override in user/*.el to customize per machine or instance.")

(defvar electric-pair-inhibition nil)
(defvar electric-pair-eagerness t)

(setq electric-indent-chars '(?\n ?:))

(defun bootstrap-electric-pair-inhibition (char)
  (if electric-pair-inhibition
      nil
    (if electric-pair-eagerness
        (electric-pair-default-inhibit char)
      (electric-pair-conservative-inhibit char))))

(cl-defmethod project-root ((project (head cmake-root))) (cdr project))

(defun project-find-cmake-project (dir)
  (when-let* ((root (locate-dominating-file dir "CMakeLists.txt")))
    (cons 'cmake-root root)))

(add-hook 'project-find-functions #'project-find-cmake-project)

(add-hook 'which-key-mode-hook #'which-key-setup-side-window-bottom)

(add-hook 'sqlite-mode-hook #'sqlite-extras-minor-mode)
(add-hook 'LaTeX-mode-hook #'turn-on-reftex)
(add-hook 'LaTeX-mode-hook #'visual-line-mode)
(add-hook 'LaTeX-mode-hook #'turn-off-auto-fill)

(add-to-list 'auto-mode-alist '("\\.xml$'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))
(add-to-list 'auto-mode-alist '("\\(?:^\\|/\\)[Mm]akefile\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-unix-mode))

(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.ninja\\'" . ninja-mode))

(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))
;; -- js/web
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
;; -- c/c++
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-or-c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-ts-mode))
;; -- jvm
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

(add-to-list 'term-file-aliases '("alacritty" . "xterm"))
(add-to-list 'term-file-aliases '("ghostty" . "xterm-ghostty"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; system -- darwin or linux specific settings

(when (eq system-type 'darwin)
  (setq read-process-output-max (* 64 1024))
  (setq ns-function-modifier 'hyper)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq ns-use-srgb-colorspace nil)
  (setq display-highres t)
  (add-hook 'after-make-frame-functions #'contextual-menubar))

(when (eq system-type 'gnu/linux)
  (setq read-process-output-max (* 1024 1024))
  (setq x-alt-keysym 'meta)
  (setq x-super-keysym 'super))

(setq system-uses-terminfo t)

(provide 'setup-defaults)
;;; setup-defaults.el ends here
