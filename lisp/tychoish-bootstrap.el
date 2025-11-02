;;; tychoish-bootstrap.el --- Utilities used during emacs setup -*- lexical-binding: t; -*-

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0-pre
;; Package-Requires: ((emacs "24.4"))
;; Keywords: internal maint emacs startup dotemacs config
;; Homepage: https://github.com/tychoish/.eamcs.d

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

(require 'f)
(require 's)
(require 'ht)
(require 'dash)

(declare-function browse-url-chrome "browse-url")

(bind-keys
 ("C-x m" . execute-extended-command)
 ("C-x C-m" . execute-extended-command)
 ("M-X" . execute-extended-command-for-buffer)
 ("C-x b" . switch-to-buffer) ;; vs consult-buffer
 ("C-x l" . goto-line)
 ("C-x f" . find-file)
 ("C-x C-f" . find-file)
 ("C-x h" . help)
 ("C-x C-x" . exchange-point-and-mark)
 ;; ("C-x C-u w" . upcase-word)
 ;; ("C-x C-u t" . upcase-initials-region)
 ;; ("C-x C-u r" . upcase-region)
 ("C-x C-d" . dired)
 ("C-x d" . dired)
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
 ("C-c s e" . eshell)
 ("M-h" . windmove-left)
 ("M-j" . windmove-down)
 ("M-k" . windmove-up)
 ("M-l" . windmove-right)
 ("S-<left>" . windmove-left)
 ("S-<down>" . windmove-down)
 ("S-<right>" . windmove-right)
 ("S-<up>" . windmove-up)
 ("M-H" . increase-window-left)
 ("M-J" . increase-window-down)
 ("M-K" . increase-window-up)
 ("M-L" . increase-window-right)
 ("M-<left>" . increase-window-left)
 ("M-<down>" . increase-window-down)
 ("M-<up>" . increase-window-up)
 ("M-<right>" . increase-window-right)
 ("M-/" . dabbrev-completion)
 ("C-M-/" . dabbrev-expand))

(bind-keys
 ;; these are all from tychoish-common.el
 ("M-<up>" . move-text-up)
 ("M-<down>" . move-text-down)
 :prefix "C-c f"
 :prefix-map tychoish/display-map
 ("=" . text-scale-increase)
 ("-" . text-scale-decrease)
 ("0" . text-scale-reset)
 :map tychoish/display-map ;; "C-c f"
 :prefix "o"
 :prefix-map tychoish/display-opacity-map
 ("=" . opacity-increase)
 ("-" . opacity-decrease)
 ("0" . opacity-reset))

(bind-keys
 ;; these are all from tychoish-common.el
 :prefix "C-c t"
 :prefix-map tychoish-core-map
 ("w" . toggle-local-whitespace-cleanup)
 :map tychoish-core-map ;; "C-c t"
 :prefix "t"
 :prefix-map tychoish/theme-map
 ("r" . disable-all-themes) ;; reset
 ("d" . tychoish-load-dark-theme)
 ("l" . tychoish-load-light-theme))

(bind-keys
 :map tychoish-core-map
 ("p" . toggle-electric-pair-inhibition)
 ("e" . toggle-electric-pair-eagerness))

(bind-keys
 :map minibuffer-local-map
 ("C-l" . backward-kill-word))

(bind-keys
 :prefix "C-c d"
 :prefix-map tychoish/docs-map
 ("s" . describe-symbol)
 ("v" . describe-variable)
 ("q" . kill-eldoc-and-help-buffers)
 ("j" . jump-to-elisp-help)
 ("e" . eldoc)
 ("b" . eldoc-doc-buffer))

(bind-keys
 :prefix "C-c k"
 :prefix-map tychoish/kill-map
 ("s" . backward-kill-sentence)
 ("p" . backward-kill-paragraph)
 ("f" . backward-kill-sexp)
 ("d" . delete-region)
 ("w" . delete-trailing-whitespace))

(bind-keys
 :prefix "C-c w"
 :prefix-map tychoish/web-browser-map ;; C-c w
 ("d" . browse-url-generic)
 ("e" . browse-url)
 ("f" . browse-url-firefox)
 ("c" . browse-url-chrome)
 ("g" . eww-search-words))

(bind-keys
 :prefix "C-c g"
 :prefix-map tychoish/ecclectic-grep-map ;;  "C-c g"
 ("o" . occur)
 ("g" . grep))

(bind-keys
 :map tychoish/ecclectic-grep-map
 :prefix "p"
 :prefix-map tychoish/ecclectic-grep-project-map ;; "C-c g p"
 ("f" . find-grep))

(bind-keys
 :prefix "C-c o"
 :prefix-map tychoish/global-org-map
 ("a" . org-agenda)
 ("k" . org-capture))

(bind-keys
 :map tychoish/global-org-map
 :prefix "l"
 :prefix-map tychoish/org-link-mode-map
 ("s" . org-store-link)
 ("i" . org-insert-link))

(bind-keys
 :prefix "C-c ."
 :prefix-map tychoish/completion-map
 ("TAB" . completion-at-point)
 ("." . completion-at-point)
 ("/" . dabbrev-completion)
 ("p" . completion-at-point))

(bind-keys
 :map global-map
 ("M-." . xref-find-definitions)
 :prefix "C-c l"
 :prefix-map tychoish/ide-map
 ("m" . imenu)
 ("c" . xref-find-references)
 ("d" . xref-find-definitions)
 ("p" . xref-go-back)
 ("n" . xref-go-forward)
 ("o" . xref-find-definitions-other-window))

(defvar-keymap tychoish/robot-gptel-set-default-model-map
  :name "default model setters"
  :doc "set default model for gtpel")

(which-key-add-keymap-based-replacements tychoish/ecclectic-grep-map
  "p" '("project-grep" . tychoish/ecclectic-grep-project-map))

(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'list-threads 'disabled nil)

(with-eval-after-load "warnings"
 (add-to-list 'warning-suppress-log-types '(frameset)))

(setq ad-redefinition-action 'accept)

(setq fringe-mode 0)
(setq ring-bell-function (lambda () nil))
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time nil)
(setq jit-lock-defer-time 0.2)
(setq jit-lock-stealth-nice 0.2)
(setq jit-lock-stealth-load 100)

(setq truncate-lines t)
(setq use-dialog-box nil)
(setq indent-tabs-mode nil) ; (setq tab-width 4)
(setq tab-always-indent t)
(setq cursor-in-non-selected-windows nil)
(setq comment-auto-fill-only-comments t)

(setq backup-by-copying t)
(setq make-backup-files t)
(setq delete-old-versions t)
(setq auto-revert-verbose nil)
(setq auto-revert-avoid-polling t)
(setq auto-revert-interval 60)

(setq split-height-threshold 100)
(setq scroll-conservatively 25)
(setq scroll-preserve-screen-position t)
(setq indicate-empty-lines t)
(setq use-short-answers t) ;; (fset 'yes-or-no-p 'y-or-n-p)
(setq shell-command-dont-erase-buffer 'end-last-out)
(setq show-paren-delay 0.25)

(setq checkdoc-force-docstrings-flag nil)
(setq checkdoc-spellcheck-documentation-flag t)
(setq ansi-color-for-comint-mode t)
(setq makefile-electric-keys t)

(setq which-key-idle-delay .25)
(setq which-key-idle-secondary-delay 0.125)
(setq which-key-lighter "")

(setq completion-cycle-threshold 2)
(setq completion-ignore-case t)
(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(setq read-buffer-completion-ignore-case t)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq read-file-name-completion-ignore-case t)
(setq text-mode-ispell-word-completion nil)

(setq next-line-add-newlines nil)
(setq undo-auto-current-boundary-timer t)
(setq read-file-name-completion-ignore-case t)

(setq select-enable-primary nil)
(setq select-enable-clipboard nil)

(setq query-replace-highlight t)
(setq search-highlight t)

(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq confirm-kill-emacs nil)

(setq browse-url-browser-function 'eww-browse-url)
(setq browse-url-generic-program "chrome")
(setq shr-color-visible-luminance-min 80)
(setq shr-use-colors nil)
(setq shr-use-fonts nil)
(setq eww-search-prefix "https://lite.duckduckgo.com/search?q=")

(setq eldoc-minor-mode-string "")
(setq eldoc-echo-area-use-multiline-p t)
(setq eldoc-echo-area-prefer-doc-buffer nil)
(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

(setq package-user-dir (concat user-emacs-directory "elpa"))

(setq lpr-add-switches "-T ''")

(setq electric-pair-inhibit-predicate #'tychoish/electric-pair-inhibition)

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

(setq whitespace-style
      '(face
        trailing
        tabs
        spaces
        lines
        newline
        missing-newline-at-eof
        empty
        space-mark
        tab-mark
        newline-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state -- setup desktop/bookmarks/savehist

(defvar desktop/last-save-time nil)

(defun tychoish/set-up-emacs-instance-persistence ()
  (with-silence
   (recentf-mode 1)
   (savehist-mode 1))

  (with-eval-after-load 'consult
    (bind-key "C-x C-r" 'consult-recent-file 'global-map))

  (setq project-list-file (tychoish/conf-state-path "projects.el"))
  (setq auto-save-list-file-prefix (tychoish/conf-state-path (concat "auto-safe-list" (f-path-separator))))
  (setq-default savehist-file (tychoish/conf-state-path "savehist.el"))
  (setq project-list-file (tychoish/conf-state-path "projects.el"))
  (setq bookmark-default-file (tychoish/conf-state-path "bookmarks.el"))
  (setq tramp-persistency-file-name (tychoish/conf-state-path "tramp.el"))

  (setq bookmark-save-flag 1)
  (setq savehist-coding-system 'utf-8-emacs)
  (setq recentf-auto-cleanup 'never)
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-menu-items 100)
  (setq recentf-save-file (tychoish/conf-state-path "recentf.el")))

(defun tychoish/desktop-save ()
  "Save desktop... sometimes"
  (interactive)
  (unless (equal "solo" tychoish/emacs-instance-id)
    (when (or (> 40 (random 100))
              (< 150 (float-time (time-since desktop/last-save-time))))
      (desktop-save desktop-dirname)
      (setq desktop/time-since-last-save (current-time)))))

(defun tychoish/desktop-read-init ()
  ;; only read the desktop if we're not in the "solo" (no ID) emacs
  ;; instance.
  (unless (equal "solo" tychoish/emacs-instance-id)
    ;; TODO This should get a better runtime/feature flag (and have
    ;; a list of instance names that are epehemral)
    (setq desktop-dirname (f-join user-emacs-directory tychoish/conf-state-directory-name))
    (setq desktop-base-file-name (tychoish-get-config-file-prefix "desktop.el"))
    (setq desktop-base-lock-name (tychoish-get-config-file-prefix (format "desktop-%d.lock" (emacs-pid))))
    (setq desktop-path (list desktop-dirname user-emacs-directory (f-expand "~")))

    (if (daemonp)
        (progn
          (setq desktop-restore-frames t)
          (setq desktop-load-locked-desktop t)
          (setq desktop-restore-eager nil))
      (setq desktop-restore-eager t)
      (setq desktop-load-locked-desktop nil))

    (when (file-exists-p (f-join desktop-dirname desktop-base-file-name))
      (with-gc-suppressed
       (with-file-name-handler-disabled
	(with-silence (desktop-read)))))

    (require 'desktop)
    (setq desktop-save t)
    (setq desktop/last-save-time (current-time))

    (add-hook 'after-save-hook 'tychoish/desktop-save)

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

;; silent startup -- avoid printing or using the Messages buffer

(defun display-startup-echo-area-message ()
  "Called during setup, intentially a noop, which omit the message."  nil)

(defun emacs-repository-version-git (_dir)
  "Noop definition of function to speed up startup" "")

(defun emacs-repository-get-version (&optional _dir _ext)
  "Noop definition of function to speed up startup" "")

(defun ad:suppress-message (f &rest arg)
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply f arg)))

(advice-add 'emacs-repository-branch-git :around #'ad:suppress-message)
(advice-add 'emacs-repository-version-git :around #'ad:suppress-message)

(defun fixed-native--compile-async-skip-p (native--compile-async-skip-p file load selector)
    "Hacky fix to resolve issue with native comp."
  ;; https://emacs.stackexchange.com/questions/82010/why-is-emacs-recompiling-some-packages-on-every-startup
  (let* ((naive-elc-file (file-name-with-extension file "elc"))
         (elc-file (replace-regexp-in-string "\\.el\\.elc$" ".elc" naive-elc-file)))
    (or (gethash elc-file comp--no-native-compile)
        (funcall native--compile-async-skip-p file load selector))))

(advice-add 'native--compile-async-skip-p :around 'fixed-native--compile-async-skip-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hooks -- functions that run in hooks configured in 'tychoish-core

(defun with-hook-timing (inner &rest args)
  (with-slow-op-timer (format "hook-runtime-<%s>" args)
   (apply inner args)))

(advice-add 'run-hooks :around 'with-hook-timing)

(setq default-frame-alist (unless (gui-p) '((background-color . nil))))

(defun tychoish/init-late-disable-modes ()
  (with-slow-op-timer
   "<bootstrap.el> after-init [disable modes]"
   (when (boundp 'scroll-bar-mode)
     (scroll-bar-mode -1))
   (tool-bar-mode -1)
   (indent-tabs-mode -1)
   (menu-bar-mode -1)))

(defun tychoish/init-late-set-up-theme ()
  (with-slow-op-timer
   "<bootstrap.el> after-init [theme setup]"
    (setq frame-title-format '(:eval (format "%s:%s" tychoish/emacs-instance-id (buffer-name))))
    (add-to-list 'mode-line-misc-info '(:eval (format "[%s]" tychoish/emacs-instance-id)))
    (tychoish/ensure-light-theme)
    (tychoish/ensure-default-font)))

(defun tychoish/init-late-enable-modes ()
  (with-slow-op-timer
   "<bootstrap.el> after-init [enable-modes]"
   (global-auto-revert-mode 1)
   (column-number-mode 1)
   (delete-selection-mode 1)
   (winner-mode 1)
   (transient-mark-mode 1)
   (xterm-mouse-mode 1)
   (electric-pair-mode 1)
   (which-key-mode 1)
   (repeat-mode 1)))

(defun tychoish/set-up-delightful-mode-lighters ()
  (with-slow-op-timer
   "<bootstrap.el> after-init [delight]"
   (delight 'org-mode "org")
   (delight 'org-agenda-mode "agenda")
   (delight 'auto-revert-mode)
   (delight 'eldoc-mode)
   (delight 'emacs-lisp-mode '("el" (lexical-binding ":l" ":d")) :major)
   (delight 'auto-fill-function " afm")
   (delight 'overwrite-mode "om")
   (delight 'refill-mode "rf")
   (delight 'visual-line-mode " wr")
   (delight 'fundamental-mode "fun")))

(add-hygenic-one-shot-hook
 :name "delight-modeline"
 :operation 'tychoish/set-up-delightful-mode-lighters
 :hook '(doom-modeline-mode-hook nerd-icons-completion-mode-hook))

(add-hygenic-one-shot-hook
 :name "restore-desktop"
 :function tychoish/desktop-read-init
 :hook after-first-frame-created)

(add-hygenic-one-shot-hook
 :name "emacs-lockfile-setup"
 :form (if (equal "solo" tychoish/emacs-instance-id)
	       (tychoish/set-up-ephemeral-instance-file-locks)
	   (tychoish/set-up-named-instance-file-locks))
 :depth 50
 :hook after-first-frame-created)

(add-hygenic-one-shot-hook
 :name "emacs-instance-persistence"
 :function (tychoish/set-up-emacs-instance-persistence)
 :depth 75
 :hook after-first-frame-created)

(add-hygenic-one-shot-hook
 :name "enable-modes"
 :function tychoish/init-late-enable-modes
 :hook (prog-mode-hook text-mode-hook))

(add-hygenic-one-shot-hook
 :name "ssh-agent"
 :function (tychoish/set-up-ssh-agent)
 :hook '(eat-mode-hook magit-mode-hook telega-root-mode-hook))

(add-hook 'emacs-startup-hook #'tychoish/init-late-disable-modes)
(add-hook 'after-init-hook #'tychoish/init-late-set-up-theme)
(add-hook 'auto-save-mode-hook 'tychoish/set-up-auto-save)

(defun tychoish--load-user-file (feat)
  (with-slow-op-timer
   (format "<%s.el> load user directory file" feat)
   (require feat)))

(defun tychoish-set-up-user-local-config ()
  "Ensure that all config files in the `user-emacs-directory' + '/user' path are loaded."
  (->> (f-entries (f-join user-emacs-directory "user"))
       (--filter (s-suffix-p ".el" it))
       (-map #'f-filename)
       (-map #'f-base)
       (-map #'intern)
       (-map #'tychoish--load-user-file)))

(defvar tychoish/abbrev-files-cache (ht-create)
  "cache mapping file names to files' mtime to avoid re-importing files")

(defun should-read-abbrev-file-p (path)
  (or (not (ht-contains-p tychoish/abbrev-files-cache path))
      (time-less-p (ht-get tychoish/abbrev-files-cache path) (f-mtime path))))

(defun tychoish/load-abbrev-files ()
  (->> (f-entries (f-join user-emacs-directory "abbrev"))
       (--filter (f-ext-p it "el"))
       (-filter #'f-exists-p)
       (-filter #'should-read-abbrev-file-p)
       (--map (let ((path it) (quietly t)) (read-abbrev-file path quietly) path))
       (--mapc (ht-set tychoish/abbrev-files-cache it (f-mtime it))))

  (delight 'abbrev-mode "abb")
  (setq save-abbrevs t))

(defun tychoish/set-up-auto-save ()
  (let ((path (tychoish/conf-state-path "backup/")))
    (setq auto-save-file-name-transforms `((".*" ,path t)))
    (add-to-list 'backup-directory-alist (cons "." path))

    (unless (file-exists-p path)
      (make-directory path))
    (chmod path #o700)))

(defun tychoish/set-up-named-instance-file-locks ()
  (let ((path (tychoish/conf-state-path "locks/")))
    (setq lock-file-name-transforms
          `(("\\`/.*/\\([^/]+\\)\\'" ,(concat path "\\1") t)))

    (unless (file-exists-p path)
      (make-directory path))
    (chmod path #o700)))

(defun tychoish/set-up-ephemeral-instance-file-locks ()
  (let* ((run-path (format "/run/user/%d" (user-uid)))
	 (path (cond
		((f-when-file-exists run-path))
		((f-when-file-exists "/var/tmp"))
		((f-when-file-exists (temporary-file-directory)))))
	 (solo-lock-path (f-join path (format "emacs-%d" (emacs-pid)))))
    (setq lock-file-name-transforms
          `(("\\`/.*/\\([^/]+\\)\\'" ,(concat solo-lock-path "\\1") t)))

    (unless (file-exists-p solo-lock-path)
      (make-directory solo-lock-path))
    (chmod solo-lock-path #o700)))

(defun tychoish/set-up-show-whitespace ()
  (setq-local show-trailing-whitespace t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; frame/window -- setup and manage frames and windows

(setq window-sides-vertical t)

(defun kill-eldoc-and-help-buffers ()
  "Kills all eldoc and help buffers"
  (interactive)
  (kill-matching-buffers "\*Help\\*\\|*eldoc.*\\*" nil t))

(defun increase-window-up () (interactive) (enlarge-window 1 nil))
(defun increase-window-down () (interactive) (enlarge-window -1 nil))
(defun increase-window-left () (interactive) (enlarge-window 1 t))
(defun increase-window-right () (interactive) (enlarge-window -1 t))

(defun frame-unset-background-for-tty (frame)
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (unless (display-graphic-p frame)
    (set-background-color "unspecified-bg")
    (set-face-attribute 'default frame :background 'unspecified :foreground 'unspecified)))

(defun current-frame-unset-background-for-tty ()
  "Reset the background on the current frame, but only if its a TTY frame."
  (interactive)
  (frame-unset-background-for-tty (selected-frame)))

(add-hook 'after-make-frame-functions #'frame-unset-background-for-tty)
(add-hook 'server-after-make-frame-hook #'current-frame-unset-background-for-tty)
(add-hook 'window-setup-hook #'current-frame-unset-background-for-tty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editing -- text editing, experience, and manipulation

(declare-function electric-pair-default-inhibit "elec-pair")
(declare-function electric-pair-conservative-inhibit "elec-pair")

(defvar electric-pair-inhibition nil)
(defvar electric-pair-eagerness t)

(defun tychoish/electric-pair-inhibition (char)
  (if electric-pair-inhibition
      nil
    (if electric-pair-eagerness
        (electric-pair-default-inhibit char)
      (electric-pair-conservative-inhibit char))))

(create-toggle-functions electric-pair-inhibition)
(create-toggle-functions electric-pair-eagerness)

(with-eval-after-load 'elec-pair
  (add-to-list 'electric-pair-pairs '(?< . ?>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; project.el -- groups of related files

(defun project-find-go-module (dir)
  (when-let ((root (or (locate-dominating-file dir "go.work")
                       (locate-dominating-file dir "go.mod"))))
    (cons 'go-module root)))

(defun project-find-cmake-project (dir)
  (when-let ((root (locate-dominating-file dir "CMakeLists.txt")))
    (cons 'cmake-root root)))

(cl-defmethod project-root ((project (head go-module))) (cdr project))
(cl-defmethod project-root ((project (head cmake-root))) (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)
(add-hook 'project-find-functions #'project-find-cmake-project)

(defun jump-to-elisp-help ()
  (interactive)
  (apropos-documentation (symbol-name (intern-soft (thing-at-point 'symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; macros -- configuration and setup

(cl-defmacro tychoish-define-project-notes (&key project path)
  (let ((symbol (intern (format "tychoish/create-%s-note" project)))
	(path (expand-file-name path)))
    `(defun ,symbol (name)
       ,(format "Create a date prefixed note file in the %s project in %s." project path)
       (interactive "sName: ")
       (tychoish-create-note-file name :path ,path))))


(cl-defmacro tychoish/gptel-set-up-backend (&key name model backend key api-key)
  (let ((local-function-symbol (intern (format "tychoish/gptel-set-backend-%s" name)))
        (default-function-symbol (intern (format "tychoish/gptel-set-default-backend-%s" name))))
    `(progn
       (defun ,local-function-symbol ()
         (interactive)
         (setq-local gptel-model ,model)
         ,(when api-key
            `(setq-local gptel-api-key (fn ,api-key)))
         (setq-local gptel-backend ,backend)
         (message "[gptel] set backend to %s for the local buffer" name))

       (defun ,default-function-symbol ()
         (interactive)
         (setq-default gptel-model ,model)
         ,(when api-key
            `(setq-default gptel-api-key (fn ,api-key)))
         (setq-default gptel-backend ,backend)
         (message "[gptel] set default backend to %s" name))

       (bind-keys :map gptel-mode-map
		  (,(format "C-c r a m %s" (upcase key)) . ,default-function-symbol)
		  (,(format "C-c r a m %s" (downcase key)) . ,local-function-symbol)
                  :map tychoish/robot-gptel-set-default-model-map
		  (,(downcase key) . ,default-function-symbol)))))

(defun tychoish/set-up-aider-env-vars ()
  (when (boundp 'anthropic-api-key)
    (setenv "ANTHROPIC_API_KEY" anthropic-api-key))

  (when (boundp 'gemini-api-key)
    (setenv "GEMINI_API_KEY" gemini-api-key))

  (when (boundp 'openai-api-key)
    (setenv "OPENAI_API_KEY" openai-api-key))

  (setenv "AIDER_CHAT_HISTORY" (tychoish/conf-state-path "aider.chat-history.md"))

  (when-let* ((uv-bin-path (expand-file-name "~/.local/bin"))
	      (_ (f-exists-p uv-bin-path))
	      (aider-bin-path (f-join uv-bin-path "aider"))
	      (search-path (getenv "PATH")))
    (unless (s-contains-p uv-bin-path search-path)
      (setenv "PATH" (format "%s:%s" search-path uv-bin-path)))
    (add-to-list 'exec-path uv-bin-path)))

(defun tychoish-set-notes-directory (&optional path)
  (when path
    (setq local-notes-directory (expand-file-name path)))

  (setq org-directory (f-join local-notes-directory "org"))
  (setq org-agenda-files (->> (list org-directory user-org-directories)
                              (-flatten)
                              (-map #'expand-file-name)
                              (-keep #'trimmed-string-or-nil)
                              (-distinct)))
  (setq org-annotate-file-storage-file (f-join org-directory "records.org"))
  (setq org-default-notes-file (f-join org-directory "records.org"))
  (setq org-archive-location (f-join org-directory "archive/%s::datetree/"))
  (setq deft-directory (f-join local-notes-directory "deft"))
  local-notes-directory)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stdlib -- configuration of default/included emacs packages

;;; no need for use-package for minimal configurations of packages
;;; that are included with emacs by default and that already have
;;; appropriate autoloads.

(add-hook 'text-mode-hook 'tychoish/set-up-show-whitespace)
(add-hook 'prog-mode-hook 'tychoish/set-up-show-whitespace)

(add-hook 'which-key-mode-hook 'which-key-setup-side-window-bottom)
(add-hook 'abbrev-mode-hook 'tychoish/load-abbrev-files)

(setq tex-dvi-view-command "(f=*; pdflatex \"${f%.dvi}.tex\" && open \"${f%.dvi}.pdf\")")

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'turn-off-auto-fill)

(add-to-list 'auto-mode-alist '("\\.tex'" . LaTeX-mode))

(add-to-list 'auto-mode-alist '("\\.el$'" . emacs-lisp-mode))

(add-to-list 'auto-mode-alist '("makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.mk$'" . makefile-mode))

(add-to-list 'auto-mode-alist '("\\.service$'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer$'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target$'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount$'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount$'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice$'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket$'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path$'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.conf$'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.org$'" . org-mode))

(add-to-list 'auto-mode-alist '("\\.zsh$'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc$'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_profile$'" . sh-mode))
(add-to-list 'auto-mode-alist '(".*mutt.*"  message-mode))

(with-eval-after-load "em-cmpl"
  (add-hook 'eshell-mode 'eshell-cmpl-initialize))

(with-eval-after-load 'comint
  (bind-keys
   :map comint-mode-map
   ("M-n" . comint-next-input)
   ("M-p" . 'comint-previous-input)
   ([down] . comint-next-matching-input-from-input)
   ([up] . 'comint-previous-matching-input-from-input)))

(with-eval-after-load 'dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'archive-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'image-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(provide 'tychoish-bootstrap)
;;; tychoish-bootstrap.el ends here
