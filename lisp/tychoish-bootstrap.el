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

(require 'xlib)
(require 'fn)
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
 :map minibuffer-local-map
 ("C-g" . tychoish/super-abort-minibuffers))

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
 :prefix-map tychoish/core-map
 ("w" . toggle-local-whitespace-cleanup)
 ("k" . execute-extended-clipboard-command)
 :map tychoish/core-map ;; "C-c t"
 :prefix "b"
 :prefix-map tychoish/blogging-map
 ("m" . tychoish-insert-date)
 ("p" . tychoish-blog-publish-post)
 ("n" . tychoish-blog-create-post)
 ("d" . tychoish-blog-open-drafts-dired)
 :map tychoish/core-map ;; "C-c t"
 :prefix "t"
 :prefix-map tychoish/theme-map
 ("r" . disable-all-themes) ;; reset
 ("d" . tychoish-load-dark-theme)
 ("l" . tychoish-load-light-theme))

(bind-keys
 :map tychoish/core-map
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
 ("p" . completion-at-point)
 ("f" . tychoish/completion-select-flavor))

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

(bind-keys
 :prefix "C-c s"
 :prefix-map tychoish/shell-map
 ("m" . eshell))

(bind-keys
 :prefix "C-c r"
 :prefix-map tychoish/robot-map
 :map tychoish/robot-map
 :prefix "g"
 :prefix-map tychoish/robot-gptel-map
 :map tychoish/robot-gptel-map
 :prefix "m"
 :prefix-map tychoish/robot-gptel-set-default-model-map)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements tychoish/ecclectic-grep-map
    "p" '("project-grep" . tychoish/ecclectic-grep-project-map)))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

(with-eval-after-load "warnings"
  (add-to-list 'warning-suppress-log-types '(frameset)))

(setq ad-redefinition-action 'accept)

(setq fringe-mode 0)
(setq ring-bell-function #'ignore)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time nil)
(setq jit-lock-defer-time 0.2)
(setq jit-lock-stealth-nice 0.2)
(setq jit-lock-stealth-load 100)
(setq tooltip-resize-echo-area t)

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
(setq eldoc-documentation-strategy #'eldoc-documentation-compose)
(setq eldoc-echo-area-display-truncation-message nil)
(setq max-mini-window-height 0.5)

(setq package-install-upgrade-built-in t)
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

(make-read-extended-command-for-prefix  "clipboard"
                                        :bind-key "C-x x c")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; id-state -- emacs daemon/instance identification for state config

(defun gui-p ()
  "Return t when the current session is or may be a GUI session."
  (when (or (daemonp) (window-system))
    t))

(defun tychoish-system-name ()
  (interactive)
  (message "system: %s" (system-name)))

(defconst tychoish-cache--buffer-name " tychoish-cache-buffer")

(defvar-local tychoish-cache--resolved-instance-id nil)
(defun tychoish/resolve-instance-id ()
  (with-current-buffer (get-buffer-create tychoish-cache--buffer-name)
    (or tychoish-cache--resolved-instance-id
	(setq tychoish-cache--resolved-instance-id
	      (let ((daemon (daemonp)))
		(or (when (eq daemon t) "primary")
		    daemon
		    cli/instance-id
		    tychoish/emacs-instance-id
		    "solo"))))))

(defun tychoish/set-up-instance-name ()
  (unless tychoish/emacs-instance-id
    (setq tychoish/emacs-instance-id (tychoish/resolve-instance-id))))

(defvar-local tychoish-cache--conf-emacs-host-and-instance nil)
(defun tychoish/conf-emacs-host-and-instance ()
  (with-current-buffer (get-buffer-create tychoish-cache--buffer-name)
    (or tychoish-cache--conf-emacs-host-and-instance
	(setq-local tychoish-cache--conf-emacs-host-and-instance
	            (list
	             (if (eq system-type 'darwin)
		         (car (s-split "\\." (system-name)))
		       (system-name))
	             (or tychoish/emacs-instance-id
		         (tychoish/resolve-instance-id)))))))

(defconst tychoish/conf-state-directory-name "state")

(defun tychoish/conf-state-path (name)
  (f-join user-emacs-directory
	  tychoish/conf-state-directory-name
	  (tychoish-get-config-file-prefix name)))

(defun tychoish-get-config-file-prefix (name)
  "Build a config file basename, for NAME.
This combines the host name and the dameon name."
  (s-join "-" (->> (tychoish/conf-emacs-host-and-instance)
		   (reverse)
		   (-concat (-l (when (or (equal "root" user-login-name)
					  (f-symlink-p user-emacs-directory))
				  user-login-name)
				name))
		   (reverse)
		   (-non-nil))))

(with-eval-after-load 'eshell
  (setq eshell-history-file-name (f-join user-emacs-directory tychoish/conf-state-directory-name (tychoish-get-config-file-prefix "eshell"))))

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
      (setq desktop/last-save-time (current-time)))))

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

    (require 'desktop)

    (when (file-exists-p (f-join desktop-dirname desktop-base-file-name))
      (with-gc-suppressed
       (with-file-name-handler-disabled
	(with-silence (desktop-read)))))

    (setq desktop-save t)
    (setq desktop/last-save-time (current-time))

    (run-with-idle-timer 120 t #'tychoish/desktop-save)

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

(cl-defmacro set-to-current-time-on-startup (variable &optional (depth 75))
  (let ((operation (intern (format "set-%s-to-current-time" (symbol-name variable)))))
    `(progn
       (add-hook 'emacs-startup-hook ',operation ,depth)
       (defun ,operation ()
	 (setq ,variable (current-time))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hooks -- functions that run in hooks configured in 'tychoish-core

(defun with-hook-timing (inner &rest args)
  (->> args
       (--mapc (with-slow-op-timer (format "<hook> %s" it)
	         (funcall inner it)))))

(when slow-op-reporting
  (advice-add 'run-hooks :around 'with-hook-timing)
  (advice-add 'run-hooks-with-args :around 'with-hook-timing))

(defun tychoish/init-force-relaod ()
  (load "tychoish-bootstrap.el")
  (load "tychoish-core.el")
  (load "tychoish-mail.el")
  (load "tychoish-org.el")
  (tychoish/init-late-disable-modes)
  (tychoish/init-late-enable-modes)
  (tychoish/init-late-set-up-naming)
  (tychoish/ensure-default-font)
  (tychoish/ensure-light-theme)
  (tychoish-set-up-user-local-config))


(defun tychoish/init-late-set-up-naming ()
  (with-slow-op-timer
    "<bootstrap.el> after-init [theme setup]"
    (tychoish/set-up-instance-name)
    (setq frame-title-format '(:eval (format "%s:%s" tychoish/emacs-instance-id (buffer-name))))
    (add-to-list 'mode-line-misc-info '(:eval (format "[%s]" tychoish/emacs-instance-id)))))

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

    (with-silence
      (repeat-mode 1))))

(unless (gui-p)
  (push '(background-color . nil) default-frame-alist))

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

(add-one-shot-hook
 :name "delight-modeline"
 :function tychoish/set-up-delightful-mode-lighters
 :hook '(doom-modeline-mode-hook nerd-icons-completion-mode-hook)
 :idle-timer 0.2)

(add-one-shot-hook
 :name "restore-desktop"
 :function tychoish/desktop-read-init
 :hook after-first-frame-created
 :idle-timer 0.2)

(add-one-shot-hook
 :name "emacs-lockfile-setup"
 :form (progn
         (tychoish/init-late-set-up-naming)
         (if (equal "solo" tychoish/emacs-instance-id)
	     (tychoish/set-up-ephemeral-instance-file-locks)
	   (tychoish/set-up-named-instance-file-locks)))
 :hook emacs-startup-hook)

(add-one-shot-hook
 :name "emacs-instance-persistence"
 :form (tychoish/set-up-emacs-instance-persistence)
 :depth 75
 :hook after-first-frame-created
 :idle-timer 0.5)

(add-one-shot-hook
 :name "enable-modes"
 :function tychoish/init-late-enable-modes
 :hook 'emacs-startup-hook
 :idle-timer 0.3)

(add-one-shot-hook
 :name "ssh-agent"
 :form (tychoish/set-up-ssh-agent)
 :hook '(eat-mode-hook magit-mode-hook telega-root-mode-hook))

(add-one-shot-hook
 :name "ensure-default-font"
 :function tychoish/ensure-default-font
 :hook after-first-frame-created
 :idle-timer 0.1)

(add-one-shot-hook
 :name "completion-flavor-init"
 :form (with-silence
	 (tychoish/completion-use-hybrid))
 :hook '(vertico-prescient-mode-hook corfu-prescient-mode-hook))

(add-hook 'emacs-startup-hook #'tychoish/ensure-light-theme)
(add-hook 'auto-save-mode-hook #'tychoish/set-up-auto-save)

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

;; package.el management and elisp tools

(defun byte-compile-all-user-emacs-files ()
  "Recompile all most relevant emacs lisp files in the current
installation that need to be recompiled. Call with a prefix argument to
forcibly recompile all emacs files. Returns a list of all files that
were recompiled."
  (interactive)
  (->> (list user-emacs-directory
	     (f-join user-emacs-directory "lisp")
	     (f-join user-emacs-directory "user"))
       (--flat-map (f-entries it #'f-file-p))
       (--filter (f-ext-p it "el"))
       (--keep (when (not (eq 'no-byte-compile (byte-recompile-file it current-prefix-arg))) it))))

(declare-function package-installed-p "package")
(declare-function package-desc-p "package")

(autoload 'async-package-do-action "async-package")

(defun async-package-operation (op pkgs)
  (let* ((ops '(install upgrade 'reinstall))
	 (valid-packages (--filter (or (symbolp it) (package-desc-p it)) pkgs))
	 (filename (concat (f-join temporary-file-directory
			           (s-join "-" (list
					        "emacs" tychoish/emacs-instance-id
					        "async-package"
					        (symbol-name op)))) ".log")))
    (unless (member op ops)
      (user-error "%s is not a valid operation %S" op ops))

    (unless valid-packages
      (user-error "must define one or more valid packages %s [%s]" valid-packages pkgs))

    (async-package-do-action op valid-packages filename)))

(defun package-install-async (pkgs)
  (interactive (list (intern (completing-read "async-install-package =>" package-archive-contents))))
  (async-package-operation 'install pkgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; frame/window -- setup and manage frames and windows

(setq window-sides-vertical t)

(defun kill-eldoc-and-help-buffers ()
  "Kills all eldoc and help buffers"
  (interactive)
  (kill-matching-buffers "\\*Help\\*\\|\\*eldoc.*\\*" nil t))

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
    (set-face-attribute 'default frame :background 'unspecified :foreground 'unspecified)))

(defun current-frame-unset-background-for-tty ()
  "Reset the background on the current frame, but only if its a TTY frame."
  (interactive)
  (frame-unset-background-for-tty (selected-frame)))

(add-hook 'after-make-frame-functions #'frame-unset-background-for-tty)
(add-hook 'server-after-make-frame-hook #'current-frame-unset-background-for-tty)
(add-hook 'window-setup-hook #'current-frame-unset-background-for-tty)

;; display -- manage fonts, rendering, themes, for (mostly) gui emacs

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defun djcb-opacity-modify (&optional dec)
  "Modify the transparency of the frame.
If DEC is t, decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 2) (+ oldalpha 2))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(defun opacity-increase ()
  (interactive)
  (djcb-opacity-modify))

(defun opacity-decrease ()
  (interactive)
  (djcb-opacity-modify t))

(defun opacity-reset ()
  (interactive)
  (modify-frame-parameters nil `((alpha . 95))))

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun tychoish-load-light-theme ()
  (interactive)

  (unless (member 'modus-operandi custom-enabled-themes)
    (when custom-enabled-themes
      (disable-all-themes))

    (if (custom-theme-p 'modus-operandi)
	(enable-theme 'modus-operandi)
      (load-theme 'modus-operandi t nil)))

  (unless (alist-get 'alpha default-frame-alist)
    (add-to-list 'default-frame-alist '(alpha . 97))))

(defun tychoish/ensure-light-theme ()
  (unless custom-enabled-themes
    (tychoish-load-light-theme)))

(defun tychoish/ensure-dark-theme ()
  (unless custom-enabled-themes
    (tychoish-load-dark-theme)))

(defun tychoish-load-dark-theme ()
  (interactive)
  (disable-all-themes)
  (when (load-theme 'modus-vivendi t t)
    (enable-theme 'modus-vivendi))
  (add-to-list 'default-frame-alist '(alpha . 95)))

(defun tychoish-setup-font (font-face-name size)
  (interactive "sName: \nNSize: ")
  (let ((new-font-name (concat font-face-name "-" (number-to-string size)))
	(font-cell (assoc 'font default-frame-alist)))
    (if font-cell
	(setcdr font-cell new-font-name)
      (add-to-list 'default-frame-alist (cons 'font new-font-name)))
    (when (display-graphic-p)
      (set-frame-font new-font-name nil t)))
  (assoc 'font default-frame-alist))

(defun tychoish/ensure-font (font-face-name size)
  (unless (assoc 'font default-frame-alist)
    (tychoish-setup-font font-face-name size)))

(defun tychoish/ensure-default-font ()
  (tychoish/ensure-font "Source Code Pro" 13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bulk buffer killing -- kill groups of buffers efficiently

(defun buffer-derived-mode-p (buffer mode)
  (with-current-buffer buffer
    (when (derived-mode-p mode)
      t)))

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t t))

(defun buffers-matching-path (regexp &optional internal-too)
  (->> (buffer-list)
       (--keep (let* ((buffer it)
		      (name (buffer-file-name buffer)))
		 (when (and name (not (string-equal name ""))
			    (or internal-too (/= (aref name 0) ?\s))
			    (string-match regexp name))
		   buffer)))))

(defun buffers-matching-mode (mode)
  (->> (buffer-list)
       (--select (with-current-buffer it (eq major-mode mode)))))

(defun kill-buffers-in-directory (&optional directory)
  "Kill all buffers in `directory'. When not defined, a directory can be selected interactively."
  (interactive)

  (unless directory
    (setq directory (annotated-completing-read-directory)))

  (let ((killed (->> (buffer-list)
		     (--filter (buffer-file-name it))
		     (--select (f-ancestor-of-p directory (buffer-file-name it)))
		     (--map (cons (buffer-file-name it) (kill-buffer it)))
		     (--filter (cdr it))
		     (--keep (car it))
		     (-unwind)
		     (-non-nil)
		     (--map (f-collapse-homedir it)))))

    (if (called-interactively-p 'any)
	(message "killed %d buffers in subdirectory %s: '%S'" (length killed) (f-collapse-homedir directory) (s-join ", " killed))
      killed)))


(defun buffers-matching-project (thing)
  (cond
   ((or (bufferp thing) (and (stringp thing) (get-buffer thing)))
    (with-current-buffer thing
      (buffers-matching-path (approximate-project-root))))
   ((and (stringp thing)
	 (f-exists-p thing))
    (->> (buffer-list)
	 (--keep (f-equal-p thing (buffer-file-name it)))
	 (-distinct)
	 (--flat-map (with-current-buffer it (buffers-matching-path (approximate-project-root))))))))

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
  (let* ((buffers (buffers-matching-path regexp internal-too))
	 (killed (->> buffers
		      (--map (cons (buffer-file-name it) (funcall (if no-ask 'kill-buffer 'kill-buffer-ask) it)))
		      (--filter (cdr it))
		      (--keep (car it))
		      (-unwind)
		      (-non-nil))))

    (if (called-interactively-p 'any)
	(message "killed %d buffers matching '%S'" (length killed) (s-join ", " killed))
      killed)))

(defconst reference-source-paths
  (append (cons package-user-dir package-directory-list) (list "/usr/share/emacs/.*" "/usr/lib/go/.*" ".*/src/emacs.*/src/.*"))
  "paths of reference files, typically opened by jump-to-definition")

(defun kill-all-reference-and-source-buffers ()
  "Kill all buffers for files in external (upstream) sources, likely opened
by jump-to-definition."
  (interactive)
  (let ((killed (->> reference-source-paths
		     (-flat-map #'force-kill-buffers-matching-path)
		     (-non-nil)
		     (-map #'f-collapse-homedir))))
    (if (called-interactively-p 'any)
	(message "killed %s refrence/source buffers [%s]" (length killed) (s-join ", " killed))
      killed)))

(defun kill-buffers-matching-mode (mode)
  "Kill all buffers matching the symbol defined by MODE.
Returns the number of buffers killed."
  (interactive
   (list (intern
          (completing-read
           "mode: " ;; prompt
           obarray  ;; collection
           (lambda (symbol) (s-ends-with? "-mode" (symbol-name symbol)))
           t nil nil major-mode))))
  (let* ((buffers (buffers-matching-mode mode))
	 (count (length buffers)))
    (message "killing all buffers (%d) with mode \"%s\"" count mode)
    (mapc #'kill-buffer buffers)
    count))

(defun tychoish/super-abort-minibuffers ()
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

(defun tychoish-run-current-major-mode-hooks (&optional buffer)
  "Run all mode-hooks for the current major mode."
  (interactive)
  (with-current-buffer (or (when (bufferp buffer) buffer)
			   (when (and (stringp buffer) (get-buffer buffer)) buffer)
			   (current-buffer))
    (apply #'run-mode-hooks (--keep (-concat (intern-soft (format "%s-hook" it))) (derived-mode-all-parents major-mode)))))

(defun buffer-directory (buf)
  "Return the `default-directory' of the provide buffer."
  (when (bufferp buf)
    (with-current-buffer buf
      (let ((file-name (buffer-file-name buf)))
	(cond ((null file-name) nil)
	      ((f-directory-p file-name) file-name)
	      ((f-file-p file-name) (f-dirname file-name))
	      (t default-directory))))))


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

;; move-text  -- use arrow keys to move whole

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
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

(declare-function visual-fill-column-mode "`visual-fill-column'")

(defun turn-on-soft-wrap ()
  (interactive)
  (let ((was-hard-wrapping auto-fill-function))
    (auto-fill-mode -1)
    (visual-fill-column-mode 1)
    (visual-line-mode 1)
    (when was-hard-wrapping
      (tychoish-show-wrapping-mode))))

(defun turn-off-soft-wrap ()
  (interactive)
  (let ((was-soft-wrapping (not auto-fill-function)))
    (visual-fill-column-mode -1)
    (visual-line-mode -1)
    (auto-fill-mode 1)
    (when was-soft-wrapping
      (tychoish-show-wrapping-mode))))

(defun toggle-word-wrap (&optional arg)
  (interactive)
  (when arg (user-error "ambiguous argument to `toggle-word-wrap'"))
  (if auto-fill-function
      (turn-on-soft-wrap)
    (turn-on-hard-wrap)))

(defun tychoish-show-wrapping-mode ()
  (let ((buf (current-buffer))
	(wrapping-mode (if auto-fill-function
                           "hard"
                         "soft")))
    (message "wrapping mode `%s' for %s <%s>"
	     wrapping-mode
	     (buffer-local-value 'major-mode buf)
	     (buffer-name buf))))

(advice-add 'set-fill-column :around #'ad:set-fill-column-locally)

(defun ad:set-fill-column-locally (f &rest arg)
  (let ((had-default (default-boundp 'fill-column))
	(previous-default (default-value 'fill-column))
	(new-value (apply f arg)))
    (when had-default
      (setq-default fill-column previous-default))
    (setq-local fill-column new-value)))

(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp-in-region "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" begin end))

;; whitespace  --

(defmacro tychoish/set-tab-width (num)
  (unless (integerp num)
    (signal 'wrong-type-argument num))
  (unless (< num 32)
    (warn "INVALID cannot create tab width hook function to >= 32 (%s)" num))

  (let ((generated-name (intern (format "tychoish/set-local-tab-width-%d" num))))
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

(defun tychoish-insert-date ()
  "Insert date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defvar tychoish-blog-path (expand-file-name "~/blog")
  "Path to the blog's project directory.")

(defvar tychoish-blog-extension ".md"
  "File extension for the blog files.")

(defun tychoish-blog-create-post (title)
  "Create a new file for a post of with the specified TITLE."
  (interactive "sPost Title: ")
  (let* ((slug (f-make-slug title))
         (draft-fn (f-join tychoish-blog-path (concat slug "-" tychoish-blog-extension))))
    (if (file-exists-p draft-fn)
        (find-file draft-fn)
      (kill-new title)
      (find-file draft-fn)
      (yas-expand-snippet
       (yas-lookup-snippet "hugo")))
    (message "working on post: %s" draft-fn)))

(defun tychoish-create-note-file (title &optional &key path)
  "Create a new file for a post of with the specified TITLE."
  (interactive "sName: ")
  (let* ((slug (f-make-slug title))
         (datetime (format-time-string "%Y-%02m-%02d"))
         (draft-fn (f-join (or path
			       (annotated-completing-read-directory))
			   (concat datetime "." slug "." tychoish-blog-extension))))
    (if (file-exists-p draft-fn)
        (find-file draft-fn)
      (find-file draft-fn)
      (insert (concat "# " title))
      (goto-char (point-max))
      (whitespace-cleanup)
      (insert "\n"))
    (message "new note: %s" draft-fn)))

(defun tychoish-blog-publish-post ()
  "Move the blog post in the current buffer to the publication location.
Does nothing if the current post is not in the drafts folder."
  (interactive)
  (let* ((publish-directory (f-join tychoish-blog-path "content" "post"))
         (original-file-name (buffer-file-name (current-buffer)))
         (published-file-name (f-join publish-directory (file-name-nondirectory original-file-name)))
         (current-point (point)))
    (cond
     ((not (equal (file-name-extension original-file-name t) tychoish-blog-extension))
      (message "post %s has incorrect extension" original-file-name))
     ((buffer-modified-p)
      (message "file %s is modified. please save before publishing" original-file-name))
     ((file-exists-p published-file-name)
      (message "published file exists with same name. not publishing"))
     (t
      (message "publishing: %s" published-file-name)
      (rename-file original-file-name published-file-name)
      (kill-buffer nil)
      (find-file published-file-name)
      (set-window-point (selected-window) current-point)
      (message "published %s to %s" original-file-name publish-directory)))))

(defun tychoish-blog-open-drafts-dired ()
  "Open a dired buffer for the drafts folder."
  (interactive)
  (find-file (expand-file-name tychoish-blog-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clean kill ring -- "deduplicate kill-ring"

(defvar clean-kill-ring-filters '(string-blank-p))
(defvar clean-kill-ring-prevent-duplicates t)

(defun clean-kill-ring-filter-catch-p (string)
  "T if STRING satisfies at least one of `clean-kill-ring-filters'."
  (let ((caught nil)
        (s (substring-no-properties string)))
    (catch 'loop
      (dolist (filter clean-kill-ring-filters)
        (when (funcall filter s)
          (setq caught t)
          (throw 'loop t))))
    caught))

(defun clean-kill-ring-clean (&optional remove-dups)
  "Remove `kill-ring' members that satisfy one of`clean-kill-ring-filters'.

If REMOVE-DUPS or `clean-kill-ring-prevent-duplicates' is non-nil, or if called
interactively then remove duplicate items from the `kill-ring'."
  ;; from: https://github.com/NicholasBHubbard/clean-kill-ring.el/blob/main/clean-kill-ring.el
  (interactive (list t))
  (let ((new-kill-ring nil)
        (this-kill-ring-member nil)
        (i (1- (length kill-ring))))
    (while (>= i 0)
      (setq this-kill-ring-member (nth i kill-ring))
      (unless (clean-kill-ring-filter-catch-p this-kill-ring-member)
        (push this-kill-ring-member new-kill-ring))
      (setq i (1- i)))
    (if (or remove-dups clean-kill-ring-prevent-duplicates)
        (setq kill-ring (delete-dups new-kill-ring))
      (setq kill-ring new-kill-ring))))


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

(cl-defmacro make-aidermacs-model-selection-function (&optional &key name default-model weak-model editor-model architect-model copilot)
  "Define a command to switch the aider model settings, including changing the live session."
  (unless (and name default-model)
    (user-error "must specify both name (%s) and default-model (%s)" name default-model))
  (let ((symbol-name (format "aidermacs-model-use-%s" name)))
    `(defun ,(intern symbol-name) ()
       ,(format "Switch to using `%s' (%s) as the default model for aidermacs." default-model name)
       (interactive)
       (if ,copilot
           (progn
             (message "setup environment for copilot")
             (setenv "OPENAI_API_KEY" github-oauth-token)
             (setenv "OPENAI_API_BASE" "https://api.githubcopilot.com"))
         (setenv "OPENAI_API_BASE" nil)
         (setenv "OPENAI_API_KEY" openai-api-key))
       (setq aidermacs-default-model ,default-model)
       (setq aidermacs-architect-model ,(or architect-model default-model))
       (setq aidermacs-editor-model ,(or editor-model default-model))
       (setq aidermacs-weak-model ,weak-model)
       (when-let* ((buf (aidermacs-select-buffer-name)))
	 (with-current-buffer buf
	   (aidermacs-send-command-with-prefix "/model " (or aidermacs-architect-model aidermacs-default-model))
	   (aidermacs-send-command-with-prefix "/weak-model " aidermacs-weak-model)
	   (aidermacs-send-command-with-prefix "/editor-model " aidermacs-editor-model))))))

(cl-defmacro make-gptel-set-up-backend-functions (&key name model backend key api-key)
  (let ((local-function-symbol (intern (format "gptel-set-backend-%s" name)))
        (default-function-symbol (intern (format "gptel-set-backend-default-%s" name))))
    `(progn
       (defun ,local-function-symbol ()
         ,(format "Set LLM backend for the current buffer to `%s'" model)
         (interactive)
         (setq-local gptel-model ,model)
         ,(when api-key
            `(setq-local gptel-api-key (fn ,api-key)))
         (setq-local gptel-backend ,backend)
         (message "[gptel] set backend to %s for the local buffer" ,name))

       (defun ,default-function-symbol ()
         ,(format "Set the default LLM backend for the current session to `%s'" model)
         (interactive)
         (setq-default gptel-model ,model)
         ,(when api-key
            `(setq-default gptel-api-key (fn ,api-key)))
         (setq-default gptel-backend ,backend)
         (message "[gptel] set default backend to %s" ,name))

       (bind-keys :map gptel-mode-map
		  (,(format "C-c r a m %s" (upcase key)) . ,default-function-symbol)
		  (,(format "C-c r a m %s" (downcase key)) . ,local-function-symbol)
                  :map tychoish/robot-gptel-set-default-model-map
		  (,(downcase key) . ,default-function-symbol)))))

(defun tychoish-set-notes-directory (&optional path)
  (when path
    (setq local-notes-directory (expand-file-name path)))

  (setq org-directory (f-join local-notes-directory "org"))
  (setq org-agenda-files (->> (list org-directory user-org-directories)
                              (-flatten)
                              (-map #'expand-file-name)
                              (-keep #'s-trimmed-or-nil)
                              (-distinct)))
  (setq org-annotate-file-storage-file (f-join org-directory "records.org"))
  (setq org-default-notes-file (f-join org-directory "records.org"))
  (setq org-archive-location (f-join org-directory "archive/%s::datetree/"))
  (setq deft-directory (f-join local-notes-directory "deft"))
  local-notes-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ssh-agent -- tools to make sure emacs session can connect to ssh-agent

(defun find-ssh-agent-socket-candidates ()
  (->> (-value-to-list (format "/run/user/%d/ssh-agent.socket" (user-uid)))
       (-concat (-sort #'s-less? (f-glob (f-join temporary-file-directory "ssh-*/agent.*" ))))
       (-distinct)
       (-non-nil)
       (-filter #'f-writable?)
       (nreverse)))

(defun tychoish/set-up-ssh-agent ()
  (let (env-value sockets)
    (unless (setq env-value (getenv "SSH_AUTH_SOCK"))
      (setq sockets (find-ssh-agent-socket-candidates))
      (when (and sockets
		 (<= 1 (length sockets)))
	(setq env-value (setenv "SSH_AUTH_SOCK" (car sockets)))))
    env-value))

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
;;
;; completion flavor -- switch between orderless / prescient / hybrid at runtime.

(defvar tychoish/completion-flavor 'hybrid
  "Currently active completion flavor.
One of `hybrid', `orderless', `prescient'.  Set by the
`tychoish/completion-use-*' commands; do not setq directly.")

(defvar tychoish/completion-flavors
  '((hybrid tychoish/completion-use-hybrid
	    "orderless filter + prescient sort (frecency)")
    (orderless tychoish/completion-use-orderless
	       "pure orderless filter; default sort, no frecency")
    (prescient tychoish/completion-use-prescient
	       "prescient filter + sort (frecency)"))
  "Alist of (NAME ACTIVATOR DESCRIPTION) for completion flavors.
ACTIVATOR is the interactive command that installs the flavor.")

(defvar tychoish/completion--applying nil
  "Re-entry guard for the `tychoish/completion-use-*' functions.
Cycling `vertico-prescient-mode' / `corfu-prescient-mode' fires their
mode hooks, which can re-invoke a flavor function (e.g. via the
startup one-shot hook below).  The guard makes the inner call a no-op.")

(defun tychoish/completion--set-category-overrides (kind)
  "Set `completion-category-overrides' for KIND (`orderless' or `prescient')."
  (setq completion-category-overrides
	(pcase kind
	  ('orderless '((file (styles basic partial-completion))
			(consult-grep (styles basic))
			(buffer (styles orderless basic))
			(command (styles orderless basic))
			(symbol (styles orderless basic))))
	  ('prescient '((file (styles basic partial-completion))
			(consult-grep (styles basic)))))))

(defun tychoish/completion--reload-prescient-mode (mode-symbol)
  "Cycle MODE-SYMBOL off and back on so it picks up new `*-enable-*' values.
`vertico-prescient' and `corfu-prescient' read the filtering/sorting flags
only at mode activation, so changing the variables alone has no effect
on an already-enabled mode."
  (when (fboundp mode-symbol)
    (when (symbol-value mode-symbol)
      (funcall mode-symbol -1))
    (funcall mode-symbol 1)))

(defmacro tychoish/completion--with-guard (&rest body)
  "Run BODY with `tychoish/completion--applying' bound non-nil.
If already non-nil (we are re-entering from a prescient mode hook),
BODY is skipped."
  (declare (indent defun))
  `(unless tychoish/completion--applying
     (let ((tychoish/completion--applying t))
       ,@body)))

(defun tychoish/completion-use-hybrid ()
  "Install the hybrid flavor: orderless filters, prescient sorts."
  (interactive)
  (tychoish/completion--with-guard
    (setq completion-styles '(orderless basic))
    (tychoish/completion--set-category-overrides 'orderless)
    (when (boundp 'vertico-prescient-enable-filtering)
      (setq vertico-prescient-enable-filtering nil
	    vertico-prescient-enable-sorting   t))
    (when (boundp 'corfu-prescient-enable-filtering)
      (setq corfu-prescient-enable-filtering nil
	    corfu-prescient-enable-sorting   t))
    (setq completion-preview-sort-function #'prescient-completion-sort)
    (tychoish/completion--reload-prescient-mode 'vertico-prescient-mode)
    (tychoish/completion--reload-prescient-mode 'corfu-prescient-mode)
    (setq tychoish/completion-flavor 'hybrid)
    (message "completion: orderless filter + prescient sort")))

(defun tychoish/completion-use-orderless ()
  "Install pure orderless; prescient disabled (no frecency)."
  (interactive)
  (tychoish/completion--with-guard
    (setq completion-styles '(orderless basic))
    (tychoish/completion--set-category-overrides 'orderless)
    (setq completion-preview-sort-function nil)
    (when (fboundp 'vertico-prescient-mode) (vertico-prescient-mode -1))
    (when (fboundp 'corfu-prescient-mode)   (corfu-prescient-mode -1))
    (setq tychoish/completion-flavor 'orderless)
    (message "completion: pure orderless")))

(defun tychoish/completion-use-prescient ()
  "Install prescient for both filter and sort; orderless inert."
  (interactive)
  (tychoish/completion--with-guard
    (setq completion-styles '(basic partial-completion emacs22))
    (tychoish/completion--set-category-overrides 'prescient)
    (when (boundp 'vertico-prescient-enable-filtering)
      (setq vertico-prescient-enable-filtering t
	    vertico-prescient-enable-sorting   t))
    (when (boundp 'corfu-prescient-enable-filtering)
      (setq corfu-prescient-enable-filtering t
	    corfu-prescient-enable-sorting   t))
    (setq completion-preview-sort-function #'prescient-completion-sort)
    (tychoish/completion--reload-prescient-mode 'vertico-prescient-mode)
    (tychoish/completion--reload-prescient-mode 'corfu-prescient-mode)
    (setq tychoish/completion-flavor 'prescient)
    (message "completion: prescient filter + sort")))

(defun tychoish/completion-select-flavor ()
  "Pick a completion flavor via `annotated-completing-read'."
  (interactive)
  (let ((table (ht-create)))
    (dolist (entry tychoish/completion-flavors)
      (ht-set table (symbol-name (car entry))
	      (concat (if (eq (car entry) tychoish/completion-flavor) "[active] " "")
		      (nth 2 entry))))
    (let* ((name (annotated-completing-read table
		  :prompt "completion flavor => "
		  :category 'tychoish-completion-flavor
		  :require-match t))
	   (entry (assq (intern name) tychoish/completion-flavors)))
      (when entry (funcall (nth 1 entry))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mcp configuration / setup 

(defvar tychoish/gopls-mcp-port 38713
  "TCP port for the gopls MCP HTTP endpoint (shared or standalone).")

(defvar tychoish/gopls-mcp-backends '(shared standalone stdio)
  "Ordered fallback list of gopls MCP backends to try.
Resolution stops at the first viable backend.  Recognized symbols:

  `shared': connect to a running `gopls serve -mcp.listen=...'
     daemon (typically managed by systemd).  Viable when
     something is listening on `tychoish/gopls-mcp-port'.
  `standalone': spawn (and reuse) a dedicated `gopls mcp -listen=...'
     process from Emacs.  Viable when `gopls' is on PATH.
  `stdio': declare a per-client `gopls mcp' stdio command; each
     MCP client launches its own gopls.  Always viable when
     `gopls' is on PATH.
  `auto-remote': declare a stdio entry that runs `gopls -remote=auto
     mcp', letting gopls's daemon-discovery logic find or
     spawn the lsp daemon.  Note: as of current gopls, the
     `mcp' subcommand does not actually consume `-remote',
     so this behaves the same as `stdio' but is kept as a
     documented option.
  `none': disable the gopls entry entirely.")

(defvar tychoish/gopls-mcp--standalone-process nil
  "Process handle for an Emacs-spawned `gopls mcp -listen' instance.")

(defvar tychoish/mcp-servers nil
  "Normalized MCP server specs shared by mcp.el and agent-shell.
Populated lazily by `tychoish/mcp-servers-init' the first time either
`mcp-hub' or `agent-shell' loads, so PATH lookups don't run at startup.
The gopls entry is resolved dynamically via `tychoish/gopls-mcp-resolve'
and prepended by `tychoish/mcp-build-servers'.
Each entry is a plist with one of:
  (:name NAME :command CMD :args (ARGS...) [:env ((K . V) ...)])
  (:name NAME :url URL [:transport http|sse] [:headers ((K . V) ...)])")

(defun tychoish/mcp-servers-init ()
  "Populate `tychoish/mcp-servers'.  Idempotent; safe to call repeatedly."
  (unless tychoish/mcp-servers
    (setq tychoish/mcp-servers
          `((:name "time" :command "uvx" :args ("mcp-server-time"))
            (:name "fetch" :command "uvx" :args ("mcp-server-fetch"))
            (:name "godoc" :command "godoc-mcpr")
            (:name "awsdoc" :command "awslabs.aws-documentation-mcp-server")
            (:name "lsp-mcp-rust" :command "npx" :args ("tritlo/lsp-mcp" "rust" ,(executable-find "rust-analyzer")))
            (:name "lsp-mcp-bash" :command "npx" :args ("tritlo/lsp-mcp" "bash" ,(executable-find "bash-language-server") "start"))
            (:name "lsp-mcp-yaml" :command "npx" :args ("tritlo/lsp-mcp" "yaml" ,(executable-find "yaml-language-server") "--stdio"))
            (:name "git" :command "uvx" :args ("mcp-server-git"))
            (:name "rg" :command "npx" :args ("-y" "mcp-ripgrep@latest"))
            (:name "linear" :command "npx" :args ("-y" "mcp-remote" "https://mcp.linear.app/mcp"))
            (:name "github" :command "npx" :args ("-y" "mcp-remote" "https://api.githubcopilot.com/mcp"))
            (:name "notion" :command "npx" :args ("-y" "mcp-remote" "https://mcp.notion.com/mcp"))
            (:name "google-workspace" :command "uvx" :args ("workspace-mcp"))))))

(defun tychoish/gopls-mcp--port-alive-p ()
  "Return non-nil if something is accepting TCP on `tychoish/gopls-mcp-port'."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "gopls-mcp-probe"
                   :host "127.0.0.1"
                   :service tychoish/gopls-mcp-port
                   :nowait nil
                   :noquery t
                   :buffer nil)))
        (delete-process proc)
        t)
    (error nil)))

(defun tychoish/gopls-mcp--ensure-standalone ()
  "Start a standalone `gopls mcp -listen' on `tychoish/gopls-mcp-port'.
Reuses any existing live process.  Returns the process or nil on failure."
  (unless (and tychoish/gopls-mcp--standalone-process
               (process-live-p tychoish/gopls-mcp--standalone-process))
    (when (executable-find "gopls")
      (setq tychoish/gopls-mcp--standalone-process
            (make-process
             :name "gopls-mcp"
             :buffer (get-buffer-create " *gopls-mcp*")
             :command (list "gopls" "mcp"
                            (format "-listen=127.0.0.1:%d" tychoish/gopls-mcp-port))
             :noquery t))))
  tychoish/gopls-mcp--standalone-process)

(defun tychoish/gopls-mcp--resolve-one (backend)
  "Return a normalized MCP server spec for BACKEND, or nil if not viable."
  (pcase backend
    ('none nil)
    ('shared (when (tychoish/gopls-mcp--port-alive-p)
	       `(:name "gopls" :url ,(format "http://127.0.0.1:%d" tychoish/gopls-mcp-port) :transport http)))
    ('standalone (when (tychoish/gopls-mcp--ensure-standalone)
		   `(:name "gopls" :url ,(format "http://127.0.0.1:%d" tychoish/gopls-mcp-port) :transport http)))
    ('stdio (when (executable-find "gopls")
	      `(:name "gopls" :command "gopls" :args ("mcp"))))
    ('auto-remote (when (executable-find "gopls")
		    `(:name "gopls" :command "gopls" :args ("-remote=auto" "mcp"))))))

(defun tychoish/gopls-mcp-resolve ()
  "Resolve a gopls MCP spec by walking `tychoish/gopls-mcp-backends'."
  (cl-some #'tychoish/gopls-mcp--resolve-one tychoish/gopls-mcp-backends))

(defun tychoish/mcp-build-servers ()
  "Build the MCP server list with a freshly-resolved gopls entry."
  (tychoish/mcp-servers-init)
  (let ((gopls (tychoish/gopls-mcp-resolve)))
    (if gopls (cons gopls tychoish/mcp-servers) tychoish/mcp-servers)))

(defun tychoish/mcp-spec->hub (spec)
  "Translate SPEC to a `mcp-hub-servers' alist entry: (NAME . PLIST)."
  (let* ((name (plist-get spec :name))
         (url  (plist-get spec :url)))
    (cons name
          (if url
              (list :url url)
            (let ((plist (list :command (plist-get spec :command))))
              (when-let* ((args (plist-get spec :args)))
                (setq plist (plist-put plist :args args)))
              plist)))))

(defun tychoish/mcp-spec->acp (spec)
  "Translate SPEC to an `agent-shell-mcp-servers' ACP McpServer alist."
  (let ((name (plist-get spec :name))
        (url  (plist-get spec :url)))
    (if url
        `((name . ,name)
          (type . ,(symbol-name (or (plist-get spec :transport) 'http)))
          (url . ,url)
          (headers . ,(or (plist-get spec :headers) '())))
      `((name . ,name)
        (command . ,(plist-get spec :command))
        (args . ,(or (plist-get spec :args) '()))
        (env . ,(or (plist-get spec :env) '()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stdlib -- configuration of default/included emacs packages

;;; no need for use-package for minimal configurations of packages
;;; that are included with emacs by default and that already have
;;; appropriate autoloads.

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'go-test)
  (add-to-list 'compilation-error-regexp-alist 'go-panic)

  (setq compilation-error-regexp-alist-alist ; first remove the standard conf; it's not good.
        (remove 'go-panic (remove 'go-test compilation-error-regexp-alist-alist)))

  (add-to-list 'compilation-error-regexp-alist-alist
               ;; '(go-test . ("^\\s-+\\k([^()\t\n]+\\):\\([0-9]+\\):? .*$" 1 2)) t) ;; the standard, it works (ish)
               '(go-test . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\):" 1 2)))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(go-panic . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\):" 1 2))))

(add-hook 'text-mode-hook 'tychoish/set-up-show-whitespace)
(add-hook 'prog-mode-hook 'tychoish/set-up-show-whitespace)

(add-hook 'which-key-mode-hook 'which-key-setup-side-window-bottom)
(add-hook 'abbrev-mode-hook 'tychoish/load-abbrev-files)

(setq tex-dvi-view-command "(f=*; pdflatex \"${f%.dvi}.tex\" && open \"${f%.dvi}.pdf\")")

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'turn-off-auto-fill)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))

(add-to-list 'auto-mode-alist '("makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-mode))

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))

(with-eval-after-load "em-cmpl"
  (add-hook 'eshell-mode 'eshell-cmpl-initialize))

(with-eval-after-load 'comint
  (bind-keys
   :map comint-mode-map
   ("M-n" . comint-next-input)
   ("M-p" . comint-previous-input)
   ([down] . comint-next-matching-input-from-input)
   ([up] . comint-previous-matching-input-from-input)))

(with-eval-after-load 'dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'archive-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'image-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(create-toggle-functions slow-op-reporting)

(declare-function magit-list-module-paths "magit-submodule")
(declare-function magit-run-git "magit-process")

(defun tychoish/elpa-pull-submodules ()
  "Run `git pull origin' in each submodule under `.emacs.d/elpa/'.
Submodules are enumerated via `magit-list-module-paths' against the
elpa repository.  For each one, prompts y/n/a (yes/no/abort).  Pulls
run synchronously via `magit-run-git'; per-pull output lands in the
magit process buffer for that submodule."
  (interactive)
  (require 'magit-submodule)
  (require 'magit-process)
  (let* ((elpa-root (file-name-as-directory
                     (expand-file-name "elpa" user-emacs-directory)))
         (default-directory elpa-root)
         (modules (magit-list-module-paths)))
    (unless modules
      (user-error "no submodules registered under %s" elpa-root))
    (catch 'abort
      (dolist (sub modules)
        (pcase (car (read-multiple-choice
                     (format "pull %s? " sub)
                     '((?y "yes"   "git pull origin in this submodule")
                       (?n "no"    "skip this submodule")
                       (?a "abort" "stop iterating"))))
          (?a (message "elpa submodule pull aborted")
              (throw 'abort nil))
          (?n (message "skip %s" sub))
          (?y (let ((default-directory
                     (file-name-as-directory
                      (expand-file-name sub elpa-root))))
                (message "pulling %s..." sub)
                (magit-run-git "pull" "origin"))))))
    (message "elpa submodule pull complete")))

(provide 'tychoish-bootstrap)
;;; tychoish-bootstrap.el ends here
