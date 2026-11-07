;;; setup-defaults.el --- Standard library and built-in defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Global settings and configuration for built-in Emacs subsystems with no
;; use-package declarations.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'sprite)
(require 'cmdr-keys)

(defvar modus-themes-deuteranopia)
(defvar modus-themes-common-palette-overrides)

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

(setq vc-follow-symlinks t)
(setq version-control t)

(setq recentf-auto-cleanup 'never)
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-menu-items 100)

(setq which-key-idle-delay .25)
(setq which-key-idle-secondary-delay 0.125)
(setq which-key-lighter "")

(setq frame-title-format '(:eval (format "%s:%s" sprite-instance-id (buffer-name))))

(defvar tychoish-core-map (cmdr-keys-prefix cmdr-mode-map "C-c t" "core")
  "The cmdr core prefix map under C-c t.")

(setq sprite-mode-map-prefix (cons "z" 'tychoish-core-map))

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
(setq ollama-tailnet-fallback-strategy :auto-local)
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

(setq modus-themes-deuteranopia t)
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive)
        (message-separator bg-main)))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'warning-suppress-types '(files missing-lexbind-cookie))

(advice-add 'display-startup-echo-area-message :override #'ignore)
(advice-add 'ask-user-about-supersession-threat :override #'ignore)

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

(add-hook 'which-key-mode-hook #'which-key-setup-side-window-bottom)

(add-hook 'dired-mode-hook 'dired-async-mode)
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

(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))


(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.ninja\\'" . ninja-mode))

(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.tfvars\\'" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.tfvars\\.example\\'" . terraform-mode))

(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))

(add-to-list 'auto-mode-alist '("\\.lisp'" . lisp-mode))

;; -- yaml
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'major-mode-remap-alist '(yaml-ts-mode . yaml-mode))

;; -- go
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.work\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mod-mode . go-mod-ts-mode))

;; -- rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
(add-to-list 'auto-mode-alist '("/Cargo\\.lock\\'" . toml-ts-mode))

;; -- python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

;; -- js/web
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame & buffer display policies

(defvar-local tychoish--buffer-home-frame nil
  "Frame where this buffer was first shown; nil means no frame restriction.")

(defun tychoish--record-home-frame ()
  "Record the selected frame as this buffer's home frame (first visit only)."
  (unless tychoish--buffer-home-frame
    (setq-local tychoish--buffer-home-frame (selected-frame))))

(defun tychoish--frame-buffer-predicate (buf)
  "Unified predicate controlling next-buffer and switch-to-buffer candidates."
  (let* ((current (current-buffer))
         (current-file (buffer-file-name current))
         (current-readonly (buffer-local-value 'buffer-read-only current)))
    (with-current-buffer buf
      (and
       ;; Rule 1: frame-sticky buffers stay on their home frame
       (or (null tychoish--buffer-home-frame)
           (eq tychoish--buffer-home-frame (selected-frame)))
       ;; Rules 2 & 3: file-based navigation restrictions
       (cond
        ;; Not in a file buffer — no restriction
        ((null current-file) t)
        ;; In a read-only file (reference buffer) — only writable files
        (current-readonly (and (buffer-file-name) (not buffer-read-only)))
        ;; In a writable file — only file buffers (any)
        (t (buffer-file-name)))))))

(defun tychoish--install-buffer-predicate (frame)
  "Install the unified buffer predicate on FRAME."
  (set-frame-parameter frame 'buffer-predicate
                       #'tychoish--frame-buffer-predicate))

(defalias 'hud--record-home-frame #'tychoish--record-home-frame)
(defalias 'hud--frame-buffer-predicate #'tychoish--frame-buffer-predicate)
(defalias 'hud--install-buffer-predicate #'tychoish--install-buffer-predicate)

(defun tychoish-run-current-major-mode-hooks (&optional buffer)
  "Run all mode-hooks for the current major mode."
  (interactive)
  (with-current-buffer (or (when (bufferp buffer) buffer)
                           (when (and (stringp buffer) (get-buffer buffer)) buffer)
                           (current-buffer))
    (apply #'run-mode-hooks (seq-keep (lambda (it) (intern-soft (format "%s-hook" it))) (derived-mode-all-parents major-mode)))))

(defalias 'hud-run-current-major-mode-hooks #'tychoish-run-current-major-mode-hooks)

;; display-buffer: reference (read-only file) buffers reuse an existing
;; read-only window rather than displacing a writable project-file window
(defun tychoish--readonly-file-buffer-p (buf _action)
  "Return non-nil if BUF is a read-only file-visiting buffer."
  (with-current-buffer buf
    (and buffer-read-only (buffer-file-name))))

(defun tychoish--reuse-readonly-file-window (buffer _alist)
  "Action: display BUFFER in an existing read-only file window if one exists."
  (when-let* ((win (seq-find
                    (lambda (w)
                      (and (not (eq w (selected-window)))
                           (with-current-buffer (window-buffer w)
                             (and buffer-read-only (buffer-file-name)))))
                    (window-list nil 'nomini))))
    (set-window-buffer win buffer)
    win))

(defconst tychoish--readonly-display-buffer-entry
  '(tychoish--readonly-file-buffer-p
    (tychoish--reuse-readonly-file-window
     display-buffer-use-some-window))
  "`display-buffer-alist' entry for read-only file buffers.")

(add-to-list 'display-buffer-alist tychoish--readonly-display-buffer-entry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TTY and display settings

(defun gui-p () "Return non-nil when may have GUI frames." (when (or (daemonp) (window-system)) t))

(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if graphical, hide if TTY."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))

(defun frame-unset-background-for-tty (frame)
  "Unset default face background and foreground on TTY frame FRAME."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)
    (set-face-foreground 'default "unspecified-fg" frame)))

(defun current-frame-unset-background-for-tty ()
  "Reset the background on the current frame, but only if its a TTY frame."
  (interactive)
  (frame-unset-background-for-tty (selected-frame)))

(defun ad:unset-background-for-tty-frames-after-theme-enable (&rest _theme)
  "Re-apply the TTY background unset to all TTY frames after a theme is enabled."
  (seq-do #'frame-unset-background-for-tty (frame-list)))

(defun frame-enable-xterm-mouse-for-tty (frame)
  "Enable `xterm-mouse-mode' when FRAME is a tty frame."
  (unless (display-graphic-p frame)
    (xterm-mouse-mode 1)))

(defun current-frame-enable-xterm-mouse-for-tty ()
  "Enable `xterm-mouse-mode' if the current frame is a tty."
  (frame-enable-xterm-mouse-for-tty (selected-frame)))

(defun ad:kill-new-reject-empty (string &optional _replace)
  "Prevent empty STRING from being added to the kill ring."
  (not (string-empty-p string)))

(advice-add 'kill-new :before-while #'ad:kill-new-reject-empty)

;; Hook installations
(add-hook 'after-make-frame-functions #'frame-unset-background-for-tty)
(add-hook 'after-make-frame-functions #'tychoish--install-buffer-predicate)
(add-hook 'after-make-frame-functions #'frame-enable-xterm-mouse-for-tty)
(add-hook 'server-after-make-frame-hook #'current-frame-unset-background-for-tty)
(add-hook 'server-after-make-frame-hook #'current-frame-enable-xterm-mouse-for-tty)
(add-hook 'window-setup-hook #'current-frame-unset-background-for-tty)
(add-hook 'window-setup-hook #'current-frame-enable-xterm-mouse-for-tty)
(add-hook 'enable-theme-functions #'ad:unset-background-for-tty-frames-after-theme-enable)

(provide 'setup-defaults)
;;; setup-defaults.el ends here
