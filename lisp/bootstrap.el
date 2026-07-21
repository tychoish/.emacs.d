;;; bootstrap.el --- Utilities used during emacs setup -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0-pre
;; Package-Requires: ((emacs "24.4") (xtdlib "0.1"))
;; Keywords: internal maint emacs startup dotemacs config
;; Homepage: https://github.com/bootstrap-.eamcs.d

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package holds the functions I want to call directly during
;; start up, as well as functions useful for building configuration
;; that don't make sense to define elsewhere.  As this file needs to
;; load during startup my goal is to keep it as short as possible, and
;; acknowledging that the collection of functionality might need to be
;; slightly more eclectic than many other packages.

;;; Code:

(use-package f
  :ensure t
  :commands (f-glob f-collapse-homedir f-entries f-ancestor-of-p))

(eval-when-compile
  (require 'xtd-macro))

(declare-function which-key-add-key-based-replacements "which-key")

(defvar bootstrap-fallback-buffer-name "*scratch*"
  "Buffer name used as a last-resort fallback when no other buffer is available.
Override in user/*.el to customize per machine or instance.")

(bind-keys
 ("C-x m" . execute-extended-command)
 ("C-x C-m" . execute-extended-command)
 ("M-X" . execute-extended-command-for-buffer)
 ("C-x l" . goto-line)
 ("C-x f" . find-file)
 ("C-x C-f" . find-file)
 ("C-c h h" . help)
 ("C-x h" . mark-whole-buffer) ;; default
 ("C-x C-x" . exchange-point-and-mark)
 ;; ("C-x C-u w" . upcase-word)
 ;; ("C-x C-u t" . upcase-initials-region)
 ;; ("C-x C-u r" . upcase-region)
 ("C-x C-n" . count-words)
 ("C-c i" . indent-region)
 ("C-c c" . comment-region)
 ("C-x C-r" . recentf)
 ("C-<backspace>" . backward-kill-word)
 ("C-h" . backward-kill-word)
 ("C-c C-w" . whitespace-cleanup)
 ("C-c C-f" . set-fill-column)
 ("C-c C-p" . set-mark-command)
 ("C-c C-r" . rename-buffer)
 ("M-<SPC>" . set-mark-command)
 ("C-c h a" . mark-whole-buffer)
 ("s-c" . clipboard-kill-ring-save) ;; (CUA/macOS) copy
 ("s-v" . clipboard-yank)           ;; (CUA/macOS) paste
 ("s-x" . clipboard-kill-region)    ;; (CUA/macOS) cut
 ("<mouse-2>" . clipboard-yank)
 ("C-z" . undo)
 ("C-w" . kill-region)
 ("C-<tab>" . completion-at-point)
 ("M-h" . windmove-left)
 ("M-j" . windmove-down)
 ("M-k" . windmove-up)
 ("M-l" . windmove-right)
 ("S-<left>" . windmove-left)
 ("S-<down>" . windmove-down)
 ("S-<right>" . windmove-right)
 ("S-<up>" . windmove-up)
 ("s-h" . increase-window-left)
 ("s-j" . increase-window-down)
 ("s-k" . increase-window-up)
 ("s-l" . increase-window-right)
 ("s-<left>" . increase-window-left)
 ("s-<down>" . increase-window-down)
 ("s-<up>" . increase-window-up)
 ("s-<right>" . increase-window-right)
 ("M-." . xref-find-definitions)
 ("M-/" . dabbrev-completion)
 ("C-M-/" . dabbrev-expand)
 ("M-<up>" . move-text-up)
 ("M-<down>" . move-text-down))

;; top level C-c <> maps
(defvar-keymap tychoish/core-map  ;; "C-c t"
  "w" #'toggle-local-whitespace-cleanup
  "s" #'whitespace-cleanup
  "k" #'execute-extended-clipboard-command
  "p" #'toggle-electric-pair-inhibition
  "e" #'toggle-electric-pair-eagerness)

(defvar-keymap tychoish/display-map ;; "C-c f"
  "=" #'text-scale-increase
  "-" #'text-scale-decrease
  "0" #'text-scale-reset
  "h" #'auto-fill-mode
  "s" #'visual-line-mode)

(defvar-keymap tychoish/kill-map ;; "C-c k"
  "s" #'backward-kill-sentence
  "p" #'backward-kill-paragraph
  "f" #'backward-kill-sexp
  "d" #'delete-region
  "w" #'delete-trailing-whitespace)

(defvar-keymap tychoish/web-browser-map ;; "C-c w"
  "d" #'browse-url-generic
  "e" #'browse-url
  "f" #'browse-url-firefox
  "c" #'browse-url-chrome
  "g" #'eww-search-words)

(defvar-keymap tychoish/ecclectic-grep-map ;; "C-c g"
  "o" #'occur
  "g" #'grep)

(defvar-keymap tychoish/completion-map ;; "C-c ."
  "TAB" #'completion-at-point
  "." #'completion-at-point
  "/" #'dabbrev-completion
  "p" #'completion-at-point)

(defvar-keymap tychoish/ide-map ;; "C-c l"
  "m" #'imenu
  "c" #'xref-find-references
  "d" #'xref-find-definitions
  "p" #'xref-go-back
  "n" #'xref-go-forward
  "o" #'xref-find-definitions-other-window)

(defvar-keymap tychoish/docs-map ;; "C-c h"
  "s" #'tychoish-describe-symbol-dwim
  "v" #'describe-variable
  "q" #'kill-eldoc-and-help-buffers
  "j" #'jump-to-elisp-help
  "e" #'eldoc
  "b" #'eldoc-doc-buffer)

(defvar-keymap tychoish/buffer-control-map ;; "C-x C-b"
  "k" #'kill-this-buffer)

(defvar-keymap tychoish/shell-map ;; "C-c s"
  "m" #'eshell)

(defvar-keymap tychoish/robot-map) ;; "C-c r"
(defvar-keymap tychoish/magit-map) ;; "C-x g"
(defvar-keymap tychoish/anzu-map) ;; "C-c q"
(defvar-keymap tychoish/consult-mode-map) ;; "C-c C-;"
(defvar-keymap tychoish/denote-map) ;; "C-c d"
(defvar-keymap tychoish/docker-map) ;; "C-x d"
(defvar-keymap orgx-global-map) ;; "C-c o"
(defvar-keymap tychoish/mail-map) ;; "C-c m"

;; nested keymaps
(defvar-keymap tychoish/blogging-map ;; "C-c t b"
  "m" #'bootstrap-insert-date)

(defvar-keymap tychoish/theme-map ;; "C-c t t"
  "r" #'disable-all-themes ;; reset
  "d" #'bootstrap-load-dark-theme
  "l" #'bootstrap-load-light-theme)

(defvar-keymap tychoish/display-opacity-map ;; "C-c f o"
  "=" #'opacity-increase
  "-" #'opacity-decrease
  "0" #'opacity-reset)

(defvar-keymap tychoish/ecclectic-grep-project-map ;; "C-c g p"
  "f" #'find-grep)

(defvar-keymap tychoish/ecclectic-rg-map) ;; "C-c g r"
(defvar-keymap tychoish/consult-search-map) ;; "C-c g s"
(defvar-keymap tychoish/smerge-map) ;; "C-x g m"
(defvar-keymap tychoish/denote-sequence-map) ;; "C-c d s"
(defvar-keymap tychoish/denote-org-map) ;; "C-c d o"
(defvar-keymap tychoish/denote-explore-map) ;; "C-c d e"
(defvar-keymap tychoish/denote-review-map) ;; "C-c d c"
(defvar-keymap orgx-link-map) ;; "C-c o l"
(defvar-keymap tychoish/eglot-global-map) ;; "C-c l l"
(defvar-keymap tychoish/robot-gptel-map) ;; "C-c r g"
(defvar-keymap tychoish/robot-gptel-set-default-model-map) ;; "C-c r g m"
(defvar-keymap tychoish/robot-agent-shell-map) ;; "C-c r s"
(defvar-keymap tychoish/shell-eat-map) ;; "C-c s e"

(keymap-set minibuffer-local-map "C-g" #'bootstrap-super-abort-minibuffers)
(keymap-set minibuffer-local-map "C-l" #'backward-kill-word)
(keymap-set global-map "C-c t" tychoish/core-map)
(keymap-set tychoish/core-map "b" tychoish/blogging-map)
(keymap-set tychoish/core-map "t" tychoish/theme-map)
(keymap-set global-map "C-c f" tychoish/display-map)
(keymap-set tychoish/display-map "o" tychoish/display-opacity-map)
(keymap-set global-map "C-c k" tychoish/kill-map)
(keymap-set global-map "C-c w" tychoish/web-browser-map)
(keymap-set global-map "C-c g" tychoish/ecclectic-grep-map)
(keymap-set tychoish/ecclectic-grep-map "p" tychoish/ecclectic-grep-project-map)
(keymap-set tychoish/ecclectic-grep-map "r" tychoish/ecclectic-rg-map)
(keymap-set tychoish/ecclectic-grep-map "s" tychoish/consult-search-map)
(keymap-set global-map "C-c ." tychoish/completion-map)
(keymap-set global-map "C-c l" tychoish/ide-map)
(keymap-set tychoish/ide-map "l" tychoish/eglot-global-map)
(keymap-set global-map "C-c h" tychoish/docs-map)
(keymap-set global-map "C-x C-b" tychoish/buffer-control-map)
(keymap-set global-map "C-c s" tychoish/shell-map)
(keymap-set tychoish/shell-map "e" tychoish/shell-eat-map)
(keymap-set global-map "C-c r" tychoish/robot-map)
(keymap-set tychoish/robot-map "s" tychoish/robot-agent-shell-map)
(keymap-set tychoish/robot-map "g" tychoish/robot-gptel-map)
(keymap-set tychoish/robot-gptel-map "m" tychoish/robot-gptel-set-default-model-map)
(keymap-set global-map "C-x g" tychoish/magit-map)
(keymap-set tychoish/magit-map "m" tychoish/smerge-map)
(keymap-set global-map "C-c q" tychoish/anzu-map)
(keymap-set global-map "C-c C-;" tychoish/consult-mode-map)
(keymap-set global-map "C-c d" tychoish/denote-map)
(keymap-set tychoish/denote-map "s" tychoish/denote-sequence-map)
(keymap-set tychoish/denote-map "o" tychoish/denote-org-map)
(keymap-set tychoish/denote-map "e" tychoish/denote-explore-map)
(keymap-set tychoish/denote-map "c" tychoish/denote-review-map)
(keymap-set global-map "C-x d" tychoish/docker-map)
(keymap-set global-map "C-c o" orgx-global-map)
(keymap-set orgx-global-map "l" orgx-link-map)
(keymap-set global-map "C-c m" tychoish/mail-map)

(declare-function electric-pair-default-inhibit "elec-pair")
(declare-function electric-pair-conservative-inhibit "elec-pair")

(defvar electric-pair-inhibition nil)
(defvar electric-pair-eagerness t)

(defun bootstrap-electric-pair-inhibition (char)
  (if electric-pair-inhibition
      nil
    (if electric-pair-eagerness
        (electric-pair-default-inhibit char)
      (electric-pair-conservative-inhibit char))))

(with-eval-after-load 'elec-pair
  (setq electric-pair-inhibit-predicate #'bootstrap-electric-pair-inhibition)
  (add-to-list 'electric-pair-pairs '(?< . ?>)))

(setq electric-indent-chars '(?\n ?:))

(with-eval-after-load 'transient
  (setq transient-values-file (file-name-concat user-emacs-directory sprite--conf-state-directory (sprite-state-file-prefix "transient-values.el"))))

(defvar bootstrap-abbrev-files-cache nil
  "cache mapping file names to files' mtime to avoid re-importing files")

(defun should-read-abbrev-file-p (path)
  (unless bootstrap-abbrev-files-cache
    (setq bootstrap-abbrev-files-cache (make-hash-table :test #'equal)))

  (or (not (map-contains-key bootstrap-abbrev-files-cache path))
      (time-less-p (map-elt bootstrap-abbrev-files-cache path)
                   (file-attribute-modification-time (file-attributes path)))))

(defun bootstrap-load-abbrev-files ()
  (unless bootstrap-abbrev-files-cache
    (setq bootstrap-abbrev-files-cache (make-hash-table :test #'equal)))

  (thread-last
    (f-entries (file-name-concat user-emacs-directory "abbrev"))
    (seq-filter (lambda (it) (string-equal (file-name-extension it) "el")))
    (seq-filter #'file-exists-p)
    (seq-filter #'should-read-abbrev-file-p)
    (seq-map (lambda (path) (let ((quietly t)) (read-abbrev-file path quietly) path)))
    (mapc (lambda (it) (setf (map-elt bootstrap-abbrev-files-cache it)
                             (file-attribute-modification-time (file-attributes it)))))))

(setq save-abbrevs t)

(setq jit-lock-defer-time 0.2)
(setq jit-lock-stealth-nice 0.2)
(setq jit-lock-stealth-load 100)

(setq backup-by-copying t)
(setq delete-old-versions t)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq find-file-visit-truename t)
(setq auto-revert-verbose nil)
(setq auto-revert-avoid-polling t)
(setq auto-revert-interval 60)

(setq ring-bell-function #'ignore)
(setq truncate-lines t)
(setq use-dialog-box nil)
(setq cursor-in-non-selected-windows nil)
(setq scroll-conservatively 25)
(setq scroll-preserve-screen-position t)
(setq indicate-empty-lines t)
(setq use-short-answers t)
(setq load-prefer-newer t)
(setq completion-ignore-case t)
(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(setq read-buffer-completion-ignore-case t)
(setq indent-tabs-mode nil) ; (setq tab-width 4)
(setq shell-command-dont-erase-buffer 'end-last-out)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq undo-auto-current-boundary-timer t)
(setq comment-auto-fill-only-comments t)
(setq tooltip-resize-echo-area t)
(setq select-enable-clipboard nil) ;; select-enable-primary already defaults to nil.
(setq text-mode-ispell-word-completion nil)
(setq lpr-add-switches "-T ''")
(setq completion-cycle-threshold 2)
(setq read-file-name-completion-ignore-case t)

(setq switch-to-prev-buffer-skip 'visible)
(setq split-height-threshold 100)
(setq window-sides-vertical t)
(setq ad-redefinition-action 'accept)

(setq checkdoc-force-docstrings-flag nil)
(setq checkdoc-spellcheck-documentation-flag t)

(setq show-paren-delay 0.25)

(make-read-extended-command-for-prefix  "clipboard"
  :bind-key "C-x x c")

(add-hook 'abbrev-mode-hook #'bootstrap-load-abbrev-files)
(add-hook 'prog-mode-hook #'bootstrap-set-up-show-whitespace)
(add-hook 'text-mode-hook #'bootstrap-set-up-show-whitespace)
(add-hook 'auto-save-mode-hook #'bootstrap-set-up-auto-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; id-state -- emacs daemon/instance identification for state config

(defun gui-p ()
  "Return t when the current session is or may be a GUI session."
  (when (or (daemonp) (window-system))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state -- setup desktop/bookmarks/savehist

(defvar desktop/last-save-time nil)
(defvar desktop-dirname nil)

(defun bootstrap-set-up-emacs-instance-persistence ()
  ;; `package-quickstart-file'/`package-quickstart' are set in init.el,
  ;; before `package-activate-all' runs -- this is deliberately shared
  ;; across instances, not per-instance state, so it does not belong here.
  (setq project-list-file (sprite-state-path "projects.el"))
  (setq savehist-file (sprite-state-path "history.el"))
  (setq bookmark-default-file (sprite-state-path "bookmarks.el"))
  (setq tramp-persistency-file-name (sprite-state-path "tramp.el"))
  (setq transient-history-file (sprite-state-path "transient-history.el"))

  (setq auto-save-list-file-prefix (sprite-state-path "auto-save-list/"))
  (setq request-storage-directory (sprite-state-path "request/"))
  (setq url-configuration-directory (sprite-state-path "url/"))

  (setq bookmark-save-flag 1)
  (setq savehist-coding-system 'utf-8-emacs)
  (setq recentf-auto-cleanup 'never)
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-menu-items 100)
  (setq recentf-save-file (sprite-state-path "recentf.el"))

  (if (equal "solo" sprite-instance-id)
      (bootstrap-set-up-ephemeral-instance-file-locks)
    (bootstrap-set-up-named-instance-file-locks))

  (with-silence
    (recentf-mode 1)
    (savehist-mode 1))

  (with-eval-after-load 'consult
    (bind-key "C-x C-r" 'consult-recent-file 'global-map)))

(defun bootstrap-desktop-save ()
  "Save desktop... sometimes"
  (interactive)
  (unless (equal "solo" sprite-instance-id)
    (when (or (> 40 (random 100))
              (< 150 (float-time (time-since desktop/last-save-time))))
      (desktop-save desktop-dirname)
      (setq desktop/last-save-time (current-time)))))

(defun bootstrap-desktop-read-init ()
  ;; only read the desktop if we're not in the "solo" (no ID) emacs
  ;; instance.
  (unless (equal "solo" sprite-instance-id)
    (setq desktop-dirname (file-name-concat user-emacs-directory sprite--conf-state-directory))
    (setq desktop-base-file-name (sprite-state-file-prefix "desktop.el"))
    (setq desktop-base-lock-name (sprite-state-file-prefix (format "desktop-%d.lock" (emacs-pid))))
    (setq desktop-path (list desktop-dirname user-emacs-directory (expand-file-name "~/")))

    (setq desktop-save t)
    (setq desktop/last-save-time (current-time))
    (setq desktop-restore-frames nil)
    (setq desktop-restore-in-current-display nil)

    (if (daemonp)
        (setq desktop-restore-eager nil
              desktop-load-locked-desktop t)
      (setq desktop-restore-eager t
            desktop-load-locked-desktop nil))

    (with-gc-suppressed
     (require 'desktop)
     (when (file-exists-p (file-name-concat desktop-dirname desktop-base-file-name))
       (with-file-name-handler-disabled
	(with-silence (desktop-read)))))

    (run-with-idle-timer 120 t #'bootstrap-desktop-save)

    (add-to-list 'desktop-globals-to-save 'register-alist)
    (add-to-list 'desktop-globals-to-save 'file-name-history)
    (add-to-list 'desktop-modes-not-to-save 'dired-mode)
    (add-to-list 'desktop-modes-not-to-save 'Info-mode)
    (add-to-list 'desktop-modes-not-to-save 'org-mode)
    (add-to-list 'desktop-modes-not-to-save 'eww-mode)
    (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
    (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

    (setq desktop-buffers-not-to-save
          (concat "\\("
                  "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|"
                  "\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                  "\\)$"))

    (setq desktop-files-not-to-save
          (concat "\\(\\`/[^/:]*:\\|(ftp)\\'\\)" ;; default
                  "^/usr/lib/go/.*\\|"
                  "^/usr/lib/rustlib/.*\\|"
                  "^/home.+go/pkg/mod\\|"
                  "^/home.+\\.cargo"))))

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

;; silent startup -- avoid printing or using the Messages buffer

(defun display-startup-echo-area-message ()
  "Called during setup, intentially a noop, which omit the message."  nil)

;; TODO remove these after emacs 31
(defun fixed-native--compile-async-skip-p (native--compile-async-skip-p file load selector)
  "Hacky fix to resolve issue with native comp."
  ;; https://emacs.stackexchange.com/questions/82010/why-is-emacs-recompiling-some-packages-on-every-startup
  (let* ((naive-elc-file (file-name-with-extension file "elc"))
         (elc-file (replace-regexp-in-string "\\.el\\.elc$" ".elc" naive-elc-file)))
    (or (map-elt comp--no-native-compile elc-file)
        (funcall native--compile-async-skip-p file load selector))))

(advice-add 'native--compile-async-skip-p :around 'fixed-native--compile-async-skip-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hooks -- functions that run in hooks configured in 'bootstrap-core

(defun with-hook-timing (inner &rest args)
  (mapc (lambda (it)
          (with-slow-op-timer (format "<hook> %s" it)
            (funcall inner it)))
        args))

(when slow-op-reporting
  (advice-add 'run-hooks :around 'with-hook-timing)
  (advice-add 'run-hooks-with-args :around 'with-hook-timing))

(defun bootstrap-init-late-enable-modes ()
  (column-number-mode 1)
  (delete-selection-mode 1)
  (winner-mode 1)
  (electric-pair-mode 1)
  (set-fringe-mode '(4 . 4))
  (with-silence
    (repeat-mode 1)))

(add-lazy-init
 :name "<bootstrap> late enable modes"
 :operation 'bootstrap-init-late-enable-modes
 :delay 0.34)

(add-one-shot-hook
 :name "<bootstrap> enable which-key"
 :hook after-first-frame-created
 :form (which-key-mode 1))

(add-one-shot-hook
 :name "<bootstrap> alias mouse mode"
 :hook after-first-frame-created
 :form (unless (fboundp 'mouse-major-mode-menu)
	 (defalias 'mouse-major-mode-menu 'mouse-menu-major-mode-map)))

(add-one-shot-hook
 :name "<bootstrap> display-buffer-alist"
 :hook 'after-first-frame-created
 :form (add-to-list 'display-buffer-alist
		    '(bootstrap--readonly-file-buffer-p
		      (bootstrap--reuse-readonly-file-window
                       display-buffer-use-some-window))))

(add-one-shot-hook
 :name "<bootstrap> enable popper"
 :hook '(compilation-mode-hook help-mode-hook special-mode-hook)
 :form (when (boundp 'popper-mode)
         (popper-mode +1)))

(add-one-shot-hook
 :name "<bootstrap> auto-revert after first file"
 :hook find-file-hook
 :form (global-auto-revert-mode 1))

(add-lazy-init
 :name "<bootstrap> ensure default font"
 :operation 'bootstrap-ensure-default-font
 :delay 0.075)

(add-lazy-init
 :name "restore-desktop"
 :operation 'bootstrap-desktop-read-init
 :delay 0.5)

(add-lazy-init
 :name "emacs-instance-persistence"
 :operation 'bootstrap-set-up-emacs-instance-persistence
 :delay 0.25)

(add-one-shot-hook
 :name "ssh-agent"
 :form (bootstrap-set-up-ssh-agent)
 :hook '(eat-mode-hook magit-mode-hook telega-root-mode-hook))

(add-lazy-init
 :name "native-compile-async"
 :delay 60
 :operation #'tychoish-native-compile-all-local)

(defun bootstrap--load-user-file (feat)
  (with-slow-op-timer
    (format "<init> [user] %s.el" feat)
    (require feat)))

(defun bootstrap-set-up-user-local-config ()
  "Ensure that all config files in the `user-emacs-directory' + '/user' path are loaded."
  (thread-last
    (file-name-concat user-emacs-directory "user")
    (funcall (lambda (path) (when (file-directory-p path) (f-entries path))))
    (seq-filter (lambda (it) (string-suffix-p ".el" it)))
    (seq-map (lambda (file) (bootstrap--load-user-file
                             (intern
                              (file-name-sans-extension
                               (file-name-nondirectory file))))))))

(defun bootstrap-set-up-auto-save ()
  (let ((path (sprite-state-path "backup/")))
    (setq auto-save-file-name-transforms `((".*" ,path t)))
    (add-to-list 'backup-directory-alist (cons "." path))

    (unless (file-exists-p path)
      (make-directory path t))
    (chmod path #o700)))

(defun bootstrap-set-up-named-instance-file-locks ()
  (let ((path (sprite-state-path "locks/")))
    (setq lock-file-name-transforms
          `(("\\`/.*/\\([^/]+\\)\\'" ,(concat path "\\1") t)))

    (unless (file-exists-p path)
      (make-directory path t))
    (chmod path #o700)))

(defun bootstrap-set-up-ephemeral-instance-file-locks ()
  (let* ((path (car (thread-last (list (format "/run/user/%d" (user-uid))
				       "/var/tmp"
				       (temporary-file-directory))
				 (seq-filter #'file-exists-p))))
	 (solo-lock-path (file-name-concat path (format "emacs-%d" (emacs-pid)))))

    (setq lock-file-name-transforms
          `(("\\`/.*/\\([^/]+\\)\\'" ,(concat solo-lock-path "\\1") t)))

    (unless (file-exists-p solo-lock-path)
      (make-directory solo-lock-path t))
    (chmod solo-lock-path #o700)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; elisp tools

(setq package-archive-priorities '(("melpa" . 100)
                                   ("nongnu" . 50)
                                   ("gnu" . 25)
                                   ("jcs-elpa" . 10)))

(defun bootstrap-byte-recompile-emacs-directory ()
  "Recompile all `.el' files in `user-emacs-directory' and its direct subdirectories.
With a prefix argument, force recompilation of every file regardless of timestamps.
Returns the list of files that were recompiled."
  (interactive)
  (thread-last (cons user-emacs-directory
                     (seq-filter #'file-directory-p
                                 (directory-files user-emacs-directory t "^[^.]")))
               (seq-mapcat (lambda (dir) (directory-files dir t "\\.el\\'")))
               (seq-filter #'file-regular-p)
               (seq-keep (lambda (f)
                           (unless (eq 'no-byte-compile
                                       (byte-recompile-file f current-prefix-arg))
                             f)))))

(defun bootstrap-recompile-vendored-packages ()
  "Recompile all `.el' files in each `bootstrap-vendored-packages' checkout.
Only scans each package's top-level directory, not its test/ subdirectory
\(matching `bootstrap-byte-recompile-emacs-directory''s scope\). Useful
after `git pull'-ing one of these external/ checkouts, since
`bootstrap-package' only installs/compiles a package the first time it
sees the checkout and never notices later updates on its own.
With a prefix argument, force recompilation of every file regardless of
timestamps. Returns the list of files that were recompiled."
  (interactive)
  (thread-last bootstrap-vendored-packages
               (seq-map (lambda (spec) (expand-file-name (nth 1 spec) user-emacs-directory)))
               (seq-filter #'file-directory-p)
               (seq-mapcat (lambda (dir) (directory-files dir t "\\.el\\'")))
               (seq-filter #'file-regular-p)
               (seq-keep (lambda (f)
                           (unless (eq 'no-byte-compile
                                       (byte-recompile-file f current-prefix-arg))
                             f)))))

(defun package-install-async (pkgs)
  (interactive (list (intern (completing-read "async-install-package =>" package-archive-contents))))
  (async-package-operation 'install pkgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; frame/window -- setup and manage frames and windows

;;;; Frame-local buffer display policies
;;
;; Unified buffer-predicate covering three rules for next-buffer /
;; switch-to-buffer:
;;
;;   1. mu4e sticky — mu4e buffers stay on the frame they first appeared on.
;;   2. File → file only — when the current buffer visits a file, only
;;      file-visiting buffers are eligible for cycling (excludes *compilation*,
;;      *Messages*, etc.).
;;   3. Reference → writable only — when the current buffer is read-only (an
;;      external library or xref target), only writable file buffers appear;
;;      another reference buffer is never the next stop.
;;
;; display-buffer-alist entries complement the predicate for call sites that
;; go through display-buffer rather than next-buffer.

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

(defun tychoish-describe-symbol-dwim (prefix)
  "Look up symbol at point contextually.
With PREFIX arg, always use `describe-symbol'.
Otherwise: use `slime-describe-symbol' if slime is connected,
`consult-eglot-symbols' if in an eglot-managed buffer,
or `describe-symbol' as fallback."
  (interactive "P")
  (cond
   (prefix
    (call-interactively #'describe-symbol))
   ((and (boundp 'slime-describe-symbol)
         (boundp 'slime-connected-p)
         (slime-connected-p))
    (call-interactively #'slime-describe-symbol))
   ((and (boundp 'eglot-current-server)
         (boundp 'consult-eglot-symbols)
         (eglot-current-server))
    (consult-eglot-symbols))
   (t
    (call-interactively #'describe-symbol))))

(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))

(defun increase-window-up () (interactive) (enlarge-window 1 nil))
(defun increase-window-down () (interactive) (enlarge-window -1 nil))
(defun increase-window-left () (interactive) (enlarge-window 1 t))
(defun increase-window-right () (interactive) (enlarge-window -1 t))

(defun frame-unset-background-for-tty (frame)
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (unless (display-graphic-p frame)
    (set-face-background 'default "undefined" frame)
    (set-face-background 'default "undefined" frame)
    (set-face-attribute 'default frame :background 'unspecified :foreground 'unspecified)))

(defun current-frame-unset-background-for-tty ()
  "Reset the background on the current frame, but only if its a TTY frame."
  (interactive)
  (frame-unset-background-for-tty (selected-frame)))

(add-hook 'after-make-frame-functions #'frame-unset-background-for-tty)
(add-hook 'after-make-frame-functions #'tychoish--install-buffer-predicate)
(add-hook 'server-after-make-frame-hook #'current-frame-unset-background-for-tty)
(add-hook 'window-setup-hook #'current-frame-unset-background-for-tty)

(defun frame-enable-xterm-mouse-for-tty (frame)
  "Enable xterm-mouse-mode when FRAME is a tty frame."
  (unless (display-graphic-p frame)
    (xterm-mouse-mode 1)))

(defun current-frame-enable-xterm-mouse-for-tty ()
  "Enable xterm-mouse-mode if the current frame is a tty."
  (frame-enable-xterm-mouse-for-tty (selected-frame)))

(add-hook 'after-make-frame-functions #'frame-enable-xterm-mouse-for-tty)
(add-hook 'server-after-make-frame-hook #'current-frame-enable-xterm-mouse-for-tty)
(add-hook 'window-setup-hook #'current-frame-enable-xterm-mouse-for-tty)

;; display -- manage fonts, rendering, themes, for (mostly) gui emacs

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defun djcb-opacity-modify (&optional dec)
  "Modify frame transparency by 5% steps."
  (let* ((raw (frame-parameter nil 'alpha))
         (current (cond
                   ((null raw) 1.0)
                   ((floatp raw) raw)
                   (t (/ raw 100.0))))
         (next (if dec (- current 0.025) (+ current 0.025))))
    (when (and (>= next 0.2) (<= next 1.0))
      (modify-frame-parameters nil (list (cons 'alpha next))))))

(defun opacity-increase ()
  (interactive)
  (djcb-opacity-modify))

(defun opacity-decrease ()
  (interactive)
  (djcb-opacity-modify t))

(defun opacity-reset ()
  (interactive)
  (modify-frame-parameters nil '((alpha . 0.95))))

(defvar-keymap tychoish/opacity-repeat-map
  :repeat t
  "=" #'opacity-increase
  "-" #'opacity-decrease
  "0" #'opacity-reset)

(defun bootstrap-setup-font (font-face-name size)
  (interactive "sName: \nNSize: ")
  (let ((new-font-name (concat font-face-name "-" (number-to-string size)))
	(font-cell (assoc 'font default-frame-alist)))
    (if font-cell
	(setcdr font-cell new-font-name)
      (add-to-list 'default-frame-alist (cons 'font new-font-name)))
    (when (display-graphic-p)
      (set-frame-font new-font-name nil t)))
  (assoc 'font default-frame-alist))

(defun bootstrap-ensure-font (font-face-name size)
  (unless (assoc 'font default-frame-alist)
    (bootstrap-setup-font font-face-name size)))

(defun bootstrap-ensure-default-font ()
  (bootstrap-ensure-font "Source Code Pro" 13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bulk buffer killing -- kill groups of buffers efficiently

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t t))

(defun kill-eldoc-and-help-buffers ()
  "Kills all eldoc and help buffers"
  (interactive)
  (kill-matching-buffers "\\*Help\\*\\|\\*eldoc.*\\*" nil t))

(defun buffers-matching-path (regexp &optional internal-too)
  (seq-filter (lambda (buf)
                (when-let* ((name (buffer-file-name buf)))
                  (and (not (string-equal name ""))
                       (or internal-too (/= (aref name 0) ?\s))
                       (string-match regexp name))))
              (buffer-list)))

(defun buffers-matching-mode (mode)
  (seq-filter (lambda (buf) (with-current-buffer buf (eq major-mode mode)))
              (buffer-list)))

(defun kill-buffers-in-directory (&optional directory)
  "Kill all buffers in `directory'. When not defined, a directory can be selected interactively."
  (interactive)

  (unless directory
    (setq directory (annotated-completing-read-directory)))

  (let ((killed (thread-last (buffer-list)
			     (seq-filter #'buffer-file-name)
			     (seq-filter (lambda (buf) (f-ancestor-of-p directory (buffer-file-name buf))))
			     (seq-map (lambda (buf) (cons (buffer-file-name buf) (kill-buffer buf))))
			     (seq-filter #'cdr)
			     (seq-map (lambda (c) (f-collapse-homedir (car c)))))))

    (if (called-interactively-p 'any)
	(message "killed %d buffers in subdirectory %s: '%S'" (length killed) (f-collapse-homedir directory) (string-join killed ", "))
      killed)))

(defalias 'kill-buffers-matching-name 'kill-matching-buffers)

(defun force-kill-buffers-matching-path (regexp)
  (interactive "sKill buffers visiting a path matching this regular expression: \n")
  (kill-buffers-matching-path regexp t t))

(defun kill-buffers-matching-path (regexp &optional internal-too no-ask)
  "Kill buffers whose name matches the specified REGEXP.
Ignores buffers whose name starts with a space, unless optional
prefix argument INTERNAL-TOO is non-nil.  Asks before killing
each buffer, unless NO-ASK is non-nil."
  (interactive "sKill buffers visiting a path matching this regular expression: \n")
  (let ((killed (thread-last
		  (buffers-matching-path regexp internal-too)
		  (seq-map (lambda (buf)
			     (cons (buffer-file-name buf)
				   (funcall (if no-ask #'kill-buffer #'kill-buffer-ask) buf))))
		  (seq-filter #'cdr)
		  (seq-map #'car))))

    (if (called-interactively-p 'any)
	(message "killed %d buffers matching '%S'" (length killed) (string-join killed ", "))
      killed)))

(defun kill-all-reference-and-source-buffers ()
  "Kill all buffers for files in external (upstream) sources, likely opened
by jump-to-definition."
  (interactive)
  (let ((killed (thread-last
		  '("/usr/share/emacs/.*" "/usr/lib/go/.*" ".*/src/emacs.*/src/.*")
		  (append (cons package-user-dir package-directory-list))
		  (seq-map #'force-kill-buffers-matching-path)
		  (seq-filter 'identity)
                  (seq-filter #'stringp)
		  (seq-map #'f-collapse-homedir))))
    (if (called-interactively-p 'any)
	(message "killed %s refrence/source buffers [%s]" (length killed) (string-join killed ", "))
      killed)))

(defun kill-buffers-matching-mode (mode)
  "Kill all buffers matching the symbol defined by MODE.
Returns the number of buffers killed."
  (interactive
   (list (intern
          (completing-read
           "mode: " ;; prompt
           obarray  ;; collection
           (lambda (symbol) (string-suffix-p "-mode" (symbol-name symbol)))
           t nil nil major-mode))))
  (let* ((buffers (buffers-matching-mode mode))
	 (count (length buffers)))
    (message "killing all buffers (%d) with mode \"%s\"" count mode)
    (mapc #'kill-buffer buffers)
    count))

(defun kill-buffers-visiting-missing-files ()
  "Kill buffers visiting files that no longer exist on disk.
Prompts before killing each buffer.  Returns the list of killed file paths
when called non-interactively."
  (interactive)
  (let ((killed (thread-last (buffer-list)
                             (seq-filter (lambda (buf)
                                           (when-let* ((file (buffer-file-name buf)))
                                             (not (file-exists-p file)))))
                             (seq-map (lambda (buf)
                                        (cons (buffer-file-name buf) (kill-buffer-ask buf))))
                             (seq-filter #'cdr)
                             (seq-map #'car))))
    (if (called-interactively-p 'any)
        (message "killed %d buffers visiting missing files%s"
                 (length killed)
                 (if killed
                     (format ": %s" (string-join (seq-map #'f-collapse-homedir killed) ", "))
                   ""))
      killed)))

(defun kill-buffer-and-delete-file (&optional buffer)
  "Kill BUFFER (default the current buffer), then delete the file it visits.
Errors when BUFFER is not visiting a file.  The buffer is killed
before the file is deleted, so declining Emacs's built-in \"buffer
modified; kill anyway?\" prompt, or its \"save and then kill\" choice,
leaves the file on disk.  Never prompts when called non-interactively."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer)))
    (unless file
      (user-error "Buffer '%s' is not visiting a file" (buffer-name buffer)))
    (if (called-interactively-p 'any)
        (unless (kill-buffer buffer)
          (user-error "Aborted: '%s' was not killed" (buffer-name buffer)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    (delete-file file)
    (when (called-interactively-p 'any)
      (message "Deleted %s" (f-collapse-homedir file)))))

(defun bootstrap-super-abort-minibuffers ()
  (interactive)
  (if (not (minibuffer-selected-window))
      (keyboard-quit)
    (abort-minibuffers)
    (minibuffer-keyboard-quit))
  (when (minibuffer-selected-window)
    (move-beginning-of-line nil)
    (kill-line)
    (abort-minibuffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; buffer/frame management -- helper functions.

(defun pin-buffer-to-window-toggle ()
  "pin buffer to window, most useful in keeping chat buffers under control"
  (interactive)
  (let* ((buf (current-buffer))
	 (window (selected-window))
	 (current-state (window-dedicated-p window))
	 (buf-name (buffer-name buf)))

    (set-window-dedicated-p window (not current-state))

    (if current-state
	(message "pinned %s to window" buf-name)
      (message "unpinned %s from window" buf-name))))

(defun buffer-line-count (&optional buf)
  "Return the number of lines in the specified buffer (name or buffer), defaulting to the current buffer."
  (car (buffer-line-statistics buf)))

(defun bootstrap-run-current-major-mode-hooks (&optional buffer)
  "Run all mode-hooks for the current major mode."
  (interactive)
  (with-current-buffer (or (when (bufferp buffer) buffer)
			   (when (and (stringp buffer) (get-buffer buffer)) buffer)
			   (current-buffer))
    (apply #'run-mode-hooks (seq-keep (lambda (it) (intern-soft (format "%s-hook" it))) (derived-mode-all-parents major-mode)))))

(defun buffer-directory (buf)
  "Return the `default-directory' of the provide buffer."
  (when (bufferp buf)
    (with-current-buffer buf
      (let ((file-name (buffer-file-name buf)))
	(cond ((null file-name) nil)
	      ((file-directory-p file-name) file-name)
	      ((file-regular-p file-name) (file-name-directory file-name))
	      (t default-directory))))))

;; display-buffer: reference (read-only file) buffers reuse an existing
;; read-only window rather than displacing a writable project-file window
(defun bootstrap--readonly-file-buffer-p (buf _action)
  "Return non-nil if BUF is a read-only file-visiting buffer."
  (with-current-buffer buf
    (and buffer-read-only (buffer-file-name))))

(defun bootstrap--reuse-readonly-file-window (buffer _alist)
  "Action: display BUFFER in an existing read-only file window if one exists."
  (when-let* ((win (seq-find
                    (lambda (w)
                      (and (not (eq w (selected-window)))
                           (with-current-buffer (window-buffer w)
                             (and buffer-read-only (buffer-file-name)))))
                    (window-list nil 'nomini))))
    (set-window-buffer win buffer)
    win))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editing -- text editing, experience, and manipulation

;; move-text  -- use arrow keys to move whole

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; word-wrapping  --

(defalias 'turn-on-hard-wrap 'turn-off-soft-wrap)
(defalias 'turn-off-hard-wrap 'turn-on-soft-wrap)
(defalias 'toggle-soft-wrap 'toggle-on-soft-wrap)
(defalias 'toggle-hard-wrap 'toggle-off-soft-wrap)

(defun turn-on-soft-wrap ()
  (interactive)
  (let ((was-hard-wrapping auto-fill-function))
    (auto-fill-mode -1)
    (visual-line-mode 1)
    (when was-hard-wrapping
      (bootstrap-show-wrapping-mode))))

(defun turn-off-soft-wrap ()
  (interactive)
  (let ((was-soft-wrapping (not auto-fill-function)))
    (visual-line-mode -1)
    (auto-fill-mode 1)
    (when was-soft-wrapping
      (bootstrap-show-wrapping-mode))))

(defun toggle-word-wrap (&optional arg)
  (interactive)
  (when arg
    (user-error "ambiguous argument to `toggle-word-wrap'"))
  (if auto-fill-function
      (turn-on-soft-wrap)
    (turn-on-hard-wrap)))

(defun bootstrap-show-wrapping-mode ()
  (let ((buf (current-buffer))
	(wrapping-mode (if auto-fill-function
                           "hard"
                         "soft")))
    (message "wrapping mode `%s' for %s <%s>"
	     wrapping-mode
	     (buffer-local-value 'major-mode buf)
	     (buffer-name buf))))

(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp-in-region "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" begin end))

;; whitespace  --

(defmacro bootstrap-set-tab-width (num)
  (unless (integerp num)
    (signal 'wrong-type-argument num))
  (unless (< num 32)
    (warn "INVALID cannot create tab width hook function to >= 32 (%s)" num))

  (let ((generated-name (intern (format "bootstrap-set-local-tab-width-%d" num))))
    `(defun ,generated-name ()
       (set-tab-width ,num))))

(defun set-tab-width (num-spaces)
  (interactive "nTab width: ")
  (setq-local tab-width num-spaces))

(defun font-lock-show-tabs ()
  "Return a font-lock style keyword for tab characters."
  '(("\t" 0 'trailing-whitespace prepend)))

(defun toggle-local-whitespace-cleanup ()
  "Reset the before-save hook to preven cleaning up."
  (interactive)
  (if (setq-local show-trailing-whitespace (not show-trailing-whitespace))
      (progn
	(add-hook 'before-save-hook 'whitespace-cleanup nil t)
	(message "turned on whitespace-cleanup for '%s'" (buffer-file-name (current-buffer))))
    (remove-hook 'before-save-hook 'whitespace-cleanup)
    (message "turned off whitespace-cleanup for '%s'" (buffer-file-name (current-buffer)))))

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for strings beyond WIDTH that use `font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

;; line manipulation

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines between BEG and END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;; files and notes

(defun bootstrap-insert-date ()
  "Insert date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun jump-to-elisp-help ()
  (interactive)
  (apropos-documentation (symbol-name (intern-soft (thing-at-point 'symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clean kill ring -- "deduplicate kill-ring"

(defun ad:kill-new-reject-empty (string &optional _replace)
  "Prevent empty STRING from being added to the kill ring."
  (not (string-empty-p string)))

(advice-add 'kill-new :before-while #'ad:kill-new-reject-empty)

(defvar clean-kill-ring-filters '(string-blank-p))
(defvar clean-kill-ring-prevent-duplicates t)

(defun clean-kill-ring-filter-catch-p (string)
  "T if STRING satisfies at least one of `clean-kill-ring-filters'."
  (let ((s (substring-no-properties string)))
    (and (seq-some (lambda (filter) (funcall filter s))
                   clean-kill-ring-filters)
         t)))

(defun clean-kill-ring-clean (&optional remove-dups)
  "Remove `kill-ring' members that satisfy one of`clean-kill-ring-filters'.

If REMOVE-DUPS or `clean-kill-ring-prevent-duplicates' is non-nil, or if called
interactively then remove duplicate items from the `kill-ring'."
  (interactive (list t))
  (let ((cleaned (seq-remove #'clean-kill-ring-filter-catch-p kill-ring)))
    (setq kill-ring
          (if (or remove-dups clean-kill-ring-prevent-duplicates)
              (delete-dups cleaned)
            cleaned))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ssh-agent -- tools to make sure emacs session can connect to ssh-agent

(defun find-ssh-agent-socket-candidates ()
  (thread-last
    (f-glob (file-name-concat temporary-file-directory "ssh-*/agent.*"))
    (append (list (format "/run/user/%d/ssh-agent.socket" (user-uid))))
    (seq-sort #'string-lessp)
    (seq-uniq)
    (seq-remove #'null)
    (seq-filter #'file-writable-p)
    (nreverse)))

(defun bootstrap-set-up-ssh-agent ()
  (interactive)
  (let (env-value sockets)
    (unless (setq env-value (getenv "SSH_AUTH_SOCK"))
      (setq sockets (find-ssh-agent-socket-candidates))
      (when (and sockets
		 (<= 1 (length sockets)))
	(setq env-value (setenv "SSH_AUTH_SOCK" (car sockets)))))
    env-value))

(create-toggle-functions slow-op-reporting)
(create-toggle-functions electric-pair-inhibition)
(create-toggle-functions electric-pair-eagerness)

(defun bootstrap-run-ci-tests (&optional timeout)
  "Discover and run all ERT tests under test/, then exit.
Intended for CI invocations via --fg-daemon --eval.
Installs a TIMEOUT-second kill guard (default 240) before running."
  (let ((test-dir (expand-file-name "test" user-emacs-directory))
        (noninteractive t))
    (add-to-list 'load-path test-dir)
    (load (expand-file-name "test-helper" test-dir) nil t)
    (run-with-timer (or timeout 240) nil (lambda () (kill-emacs 1)))
    (condition-case err
        (seq-do (lambda (file) (load file nil t))
                (directory-files test-dir t "\\`test-.*\\.el\\'"))
      (error
       (message "bootstrap-run-ci-tests: error loading test files: %S" err)
       (kill-emacs 1)))
    ;; ert-run-tests-batch-and-exit requires noninteractive=t (--batch only).
    ;; In --fg-daemon mode we call ert-run-tests-batch directly and kill-emacs
    ;; ourselves based on the result.
    (let ((stats (ert-run-tests-batch t)))
      (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))))

;;; Byte-compile check

(defun tychoish-byte-compile-and-delete-artifact (file)
  "Byte-compile FILE for error checking, then delete the .elc artifact.
FILE is resolved relative to `user-emacs-directory'.

Returns t if compilation produced no errors, nil otherwise; see
`*Compile-Log*' for warnings and errors.  Intended for use by agent skills
via emacsclient:
  emacsclient --eval \\='(tychoish-byte-compile-and-delete-artifact \"lisp/foo.el\")\\='"
  (let* ((expanded (expand-file-name file user-emacs-directory))
         (scratch (make-temp-file "tychoish-byte-compile-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert-file-contents expanded)
            (goto-char (point-min))
            (when (re-search-forward "-\\*-.*-\\*-" (line-end-position) t)
              (replace-match
               (replace-regexp-in-string
                "[ \t]*;[ \t]*no-byte-compile:[ \t]*t[ \t]*" ""
                (match-string 0))
               t t))
            (write-region (point-min) (point-max) scratch nil 'silent))
          (byte-compile-file scratch))
      (let ((elc (concat (file-name-sans-extension scratch) ".elc")))
        (when (file-exists-p elc)
          (delete-file elc)))
      (delete-file scratch))))

;;; Native compilation idle warm-up

(defun tychoish-native-compile-all-local ()
  "Queue native compilation of all .el files in lisp/ and elpa/, then prune cache.
Reports the number of files queued and the time taken to dispatch them.
Runs once on a 60-second idle timer after startup; also callable interactively.
Compilation itself is async — Emacs stays responsive while .eln files are built."
  (interactive)
  (if (not (or (string-match "NATIVE_COMP" system-configuration-features)
	       (fboundp 'native-compile-async)))
      (message "Native compilation not available in this build")
    (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
           (elpa-dir (expand-file-name "elpa" user-emacs-directory))
           (files (seq-filter
                   #'file-regular-p
                   (append
                    (when (file-directory-p lisp-dir)
		      (directory-files-recursively lisp-dir "\\.el\\'"))
                    (when (file-directory-p elpa-dir)
		      (directory-files-recursively elpa-dir "\\.el\\'")))))
           (n (length files)))
      (with-slow-op-timer "native-compile-all-local: dispatch"
        (native-compile-async files))
      (condition-case err
          (when (fboundp 'native-compile-prune-cache)
            (native-compile-prune-cache))
        (error (message "native-compile-prune-cache error: %s"
                        (error-message-string err))))
      (message "native-compile-all-local: queued %d .el files" n))))

(provide 'bootstrap)
;;; bootstrap.el ends here
