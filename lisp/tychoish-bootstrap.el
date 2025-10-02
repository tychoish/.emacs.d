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

(require 'ht)

(bind-keys :prefix "C-c m"
           :prefix-map tychoish/mail-map)

(bind-keys ("C-x m" . execute-extended-command)
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
           ("C-c t p" . #'toggle-electric-pair-inhibition)
           ("C-c t e" . #'toggle-electric-pair-eagerness)
           ("C-c d k s" . backward-kill-sentence)
           ("C-c d k p" . backward-kill-paragraph)
           ("C-c d k f" . backward-kill-sexp)
           ("C-c d k d" . delete-region)
           ("C-c d k w" . delete-trailing-whitespace)
           ("C-c d s" . describe-symbol)
           ("C-c d v" . describe-variable)
           ("C-c d q" . #'kill-eldoc-and-help-buffers)
           ("C-c d j" . jump-to-elisp-help)
           ("C-c d d" . eldoc)
           ("C-c d e" . eldoc-doc-buffer)
           ("C-c C-f" . set-fill-column)
           ("C-c C-p" . set-mark-command)
           ("C-c C-r" . rename-buffer)
           ("M-<SPC>" . set-mark-command)
           ;; ("C-c h a" . mark-whole-buffer)
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
           :prefix "C-c w"
           :prefix-map tychoish/web-browser-map ;; C-c w
           ("d" . browse-url-generic)
           ("e" . browse-url)
           ("f" . browse-url-firefox)
           ("c" . browse-url-chrome)
           ("g" . eww-search-words)
           :prefix "C-c g"
           :prefix-map tychoish/ecclectic-grep-map ;;  "C-c g"
           ("o" . occur)
           ("g" . grep)
           :map tychoish/ecclectic-grep-map
           :prefix "p"
           :prefix-map tychoish/ecclectic-grep-project-map ;; "C-c g p"
           ("f" . find-grep)
           :map minibuffer-local-map
           ("C-l" . backward-kill-word))

;; (put 'list-timers 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
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

(setq lpr-add-switches "-T ''")

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
   (recentf-mode t)
   (savehist-mode 1))

  (with-eval-after-load 'consult
    (bind-key "C-x C-r" #'consult-recent-file 'global-map))

  (setq project-list-file (tychoish/conf-state-path "projects.el"))
  (setq auto-save-list-file-prefix (tychoish/conf-state-path (concat "auto-safe-list" (f-path-separator))))
  (setq-default savehist-file (tychoish/conf-state-path "savehist.el"))
  (setq bookmark-default-file (tychoish/conf-state-path "bookmarks.el"))
  (setq custom-file (tychoish/conf-state-path "custom.el"))
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
  (setq desktop-dirname (f-join user-emacs-directory tychoish/conf-state-directory-name))
  (setq desktop-base-file-name (tychoish-get-config-file-prefix "desktop.el"))
  (setq desktop-base-lock-name (tychoish-get-config-file-prefix (format "desktop-%d.lock" (emacs-pid))))
  (setq desktop-path (list desktop-dirname user-emacs-directory (f-expand "~")))

  ;; only read the desktop if we're not in the "solo" (no ID) emacs
  ;; instance.
  ;;
  ;; TODO This should get a better runtime/feature flag (and have
  ;; a list of instance names that are epehemral)
  (unless (equal "solo" tychoish/emacs-instance-id)
    (if (daemonp)
        (progn
          (setq desktop-restore-frames t)
          (setq desktop-load-locked-desktop t)
          (setq desktop-restore-eager nil))
      (setq desktop-restore-eager t)
      (setq desktop-load-locked-desktop nil))

    (when (file-exists-p (f-join desktop-dirname desktop-base-file-name))
      (let ((gc-cons-threshold 80000000)
            (with-silence (desktop-read)))))

    (set-to-current-time-on-startup desktop/last-save-time)
    (setq desktop-save t)
    (add-hook 'after-save-hook 'tychoish/desktop-save))

  ;; the desktop package loads lazily during the desktop-read function
  ;; call, so there are init errors if we don't call this in the block
  (with-eval-after-load 'desktop
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
                  "^/home.+\\.cargo")))

  ;; now remove this function from the hook so that we don't reload on
  ;; frame creation, accidentally
  (if (daemonp)
      (remove-hook 'server-after-make-frame-hook #'tychoish/desktop-read-init)
    (remove-hook 'window-setup-hook #'tychoish/desktop-read-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; silent startup -- avoid printing or using the Messages buffer


(defun display-startup-echo-area-message ()
  "Called during setup, intentially a noop, which omit the message."  nil)

(defun emacs-repository-version-git (dir)
  "Noop definition of function to speed up startup" "")

(defun emacs-repository-get-version (&optional dir ext)
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

(setq default-frame-alist nil)

(defun tychoish/after-init-operations ()
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  (tychoish/ensure-light-theme)
  (tychoish/ensure-default-font)
  (tychoish/set-up-ssh-agent)

  (indent-tabs-mode -1)
  (column-number-mode 1)
  (delete-selection-mode 1)
  (transient-mark-mode 1)
  (xterm-mouse-mode 1))

(defun tychoish/emacs-startup-operations ()
  (global-auto-revert-mode 1)


  (delight 'auto-revert-mode)
  (delight 'eldoc-mode)
  (delight 'emacs-lisp-mode '("el" (lexical-binding ":l" ":d")) :major)
  (delight 'auto-fill-function " afm")
  (delight 'overwrite-mode "om")
  (delight 'refill-mode "rf")
  (delight 'visual-line-mode " wr")
  (delight 'fundamental-mode "fun"))

(defun tychoish-set-up-user-local-config ()
  "Ensure that all config files in the `user-emacs-directory' + '/user' path are loaded."
  (let ((dirname (f-join (f-expand user-emacs-directory) "user")))
    (when (file-accessible-directory-p dirname)
      (add-to-list 'load-path dirname)
      (mapc (lambda (fn)
              (when (and (string-match-p "\\.el$" fn)
                         (not (string-match-p "^flycheck_.*\\.el$" fn)))
                 (require (intern (string-remove-suffix ".el" fn)))))
            (directory-files dirname))) t))

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
       (--map (ht-set tychoish/abbrev-files-cache it (f-mtime it))))

  (delight 'abbrev-mode "abb")
  (setq save-abbrevs t))

(defun tychoish/set-up-auto-save ()
  (let ((path (tychoish/conf-state-path "backup/")))
    (setq auto-save-file-name-transforms `((".*" ,path t)))
    (add-to-list 'backup-directory-alist (cons "." path))

    (unless (file-exists-p path)
      (make-directory path))
    (chmod path #o700)))

(defun tychoish/set-up-show-whitespace ()
  (setq-local show-trailing-whitespace t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; frame/window -- setup and manage frames and windows

(defun kill-eldoc-and-help-buffers ()
  "Kills all eldoc and help buffers"
  (interactive)
  (kill-matching-buffers "\*Help\\*\\|*eldoc.*\\*" nil t))

(defun increase-window-up () (interactive) (enlarge-window 1 nil))
(defun increase-window-down () (interactive) (enlarge-window -1 nil))
(defun increase-window-left () (interactive) (enlarge-window 1 t))
(defun increase-window-right () (interactive) (enlarge-window -1 t))

(defun on-frame-open (frame)
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (unless (display-graphic-p frame)
    (set-face-attribute 'default frame :background 'unspecified :foreground 'unspecified)))

(defun on-after-init ()
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (on-frame-open (selected-frame)))

(add-hook 'after-make-frame-functions #'on-frame-open)
(add-hook 'window-setup-hook #'on-after-init)
(add-hook 'window-setup-hook 'winner-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editing -- text editing, experience, and manipulation

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

;; system -- darwin or linux specific settings

(when (eq system-type 'darwin)
  (setq ns-function-modifier 'hyper)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq ns-use-srgb-colorspace nil)
  (setq display-highres t)
  (add-hook 'after-make-frame-functions #'contextual-menubar))

(when (eq system-type 'gnu/linux)
  (setq x-alt-keysym 'meta)
  (setq x-super-keysym 'super))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stdlib -- configuration of default/included emacs packages

;;; no need for use-package for minimal configurations of packages
;;; that are included with emacs by default and that already have
;;; appropriate autoloads.

(add-hook 'emacs-startup-hook 'electric-pair-mode)
(add-hook 'emacs-startup-hook 'which-key-mode)
(add-hook 'which-key-mode-hook 'which-key-setup-side-window-bottom)
(add-hook 'abbrev-mode-hook 'tychoish/load-abbrev-files)

(setq tex-dvi-view-command "(f=*; pdflatex \"${f%.dvi}.tex\" && open \"${f%.dvi}.pdf\")")
(setq electric-pair-inhibit-predicate #'tychoish/electric-pair-inhibition)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'turn-off-auto-fill)
(add-hook 'eshell-mode #'eshell-cmpl-initialize)

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

(add-to-list 'auto-mode-alist '("\\.zsh$'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc$'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_profile$'" . sh-mode))

(with-eval-after-load 'comint
  (bind-keys :map comint-mode-map
             ("M-n" . comint-next-input)
             ("M-p" . comint-previous-input)
             ([down] . comint-next-matching-input-from-input)
             ([up] . comint-previous-matching-input-from-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; macros -- configuration and setup

(cl-defmacro tychoish/gptel-set-up-backend (&key name model backend key)
  (let ((local-function-symbol (intern (format "tychoish/gptel-set-backend-%s" name)))
        (default-function-symbol (intern (format "tychoish/gptel-set-default-backend-%s" name))))
    `(progn
       (defun ,local-function-symbol ()
         (interactive)
         (setq-local gptel-model ,model)
         (setq-local gptel-backend ,backend))

       (defun ,default-function-symbol ()
         (interactive)
         (setq-default gptel-model ,model)
         (setq-default gptel-backend ,backend))

       (bind-keys :map gptel-mode-map
                  (,(format "C-c r a m %s" (downcase key)) . ,local-function-symbol)
                  (,(format "C-c r a m %s" (upcase key)) . ,default-function-symbol)))))

(defvar tychoish/mail-accounts (ht-create #'equal))
(defvar tychoish/mail-account-current nil)

(cl-deftype signature-source '(signature-file
                               signature-directory
                               signature-text))

(cl-defstruct (tychoish--mail-account
               (:constructor tychoish--make--mail-account
                             (&key id maildir name address keybinding signature signature-kind signature fetchmail
                              &aux (maildir (cond
                                             ((null maildir) (f-expand "~/mail"))
                                             ((not (stringp maildir)) (user-error "maildir must be a string"))
                                             ((not (f-directory-p maildir)) (user-error "maildir does not exist"))
                                             (t (f-expand maildir))))g
                                   (signature-kind (cond
                                                    ((eq (type-of signature) 'signature-source) signature)
                                                    ((not (eq (type-of signature) 'string)) (user-error "invalid type for signature"))
                                                    ((f-directory-p signature) 'signature-directory)
                                                    ((f-exists-p signature) 'signature-file)
                                                    (t 'signature-text)))
                                   (signature (setq signature (and (when (trimmed-string-or-nil signature)
                                                                    (cond ((eq signature-kind 'signature-directory)
                                                                           (f-join maildir ".sig"))
                                                                          ((eq signature-kind 'signature-file)
                                                                           (f-join maildir ".sig" address))
                                                                          ((eq signature-kind 'signature-text)
                                                                           (user-error "signature text is not defined"))
                                                                          ((null signature-kind)
                                                                           (user-error "signature configuration is not supported"))
                                                                          (t signature)))
                                                                   signature)
                                                    ;; now validate
                                                    signature (cond
                                                               ((eq signature-kind 'signature-directory)
                                                                (if (or (null signature) (not (f-directory-p signature)))
                                                                    (user-error "signature directory does not exist")
                                                                  signature))
                                                               ((eq signature-kind 'signature-file)
                                                                (if (not (f-file-p signature))
                                                                    (user-error "signature file does not exist")
                                                                  signature))
                                                               ((eq signature-kind 'signature-text)
                                                                (or (when (not (s-contains-p "\n" signature))
                                                                      (warn "signature string does not contain newlines")
                                                                      signature)
                                                                    signature))
                                                               (t signature)))))))

  "track mail account configurations. used internally by tychoish/define-mail-account"
  (maildir (expand-file-name "~/mail")
   :documentation "path for maildirs"
   :type 'string)
  (id nil
   :documentation "symbol of function that activates this account"
   :type 'symbol)
  (address user-mail-address
   :documentation "email address"
   :type '(string t))
  (name (user-full-name) ;; from /etc/password
   :documentation "(given) name, used to populate `USER-FULL-NAME'"
   :type 'string)
  (keybinding "m"
   :documentation "keybinding in the tychoish/mail-map keymap"
   :type 'char)
  (fetchmail mu4e-get-mail-command
   :documentation "external command to run to fetch mail."
   :type 'string)
  (signature-kind 'signature-directory
   :documentation "determines how signatures are configured"
   :type 'signature-source)
  (signature ""
   :documentation "content or filename of signature"
   :type 'string))

(defconst tychoish/mail-id-template "tychoish-mail-%s")

(defvar mu4e-get-mail-command "true")

(cl-defmacro tychoish/define-mail-account
    (&key name address key id (command mu4e-get-mail-command) (maildir (expand-file-name "~/mail")) (instances '()) (systems '()))

  (let* ((account-name (format tychoish/mail-id-template id))
         (configure-account-symbol (intern account-name)))

    (define-key 'tychoish/mail-map (kbd key) configure-account-symbol)

    (ht-set tychoish/mail-accounts account-name
            (tychoish--make--mail-account
             :name name
             :address address
             :keybinding key
             :maildir maildir
             :fetchmail command
             :id id
             :signature-kind 'signature-directory
             :signature (f-join maildir "tools" "signatures")))

    (dolist (instance instances)
      (when (and (stringp instance) (string-equal instance tychoish/emacs-instance-id))
        (add-hook 'emacs-startup-hook configure-account-symbol)))

    (dolist (sysn systems)
      (when (and (stringp) (string-equal sysn (system-name)))
        (add-hook 'emacs-startup-hook configure-account-symbol)))

  `(defun ,configure-account-symbol ()
     (interactive)

     (let* ((account-id ,id)
            (account-name ,account-name)
            ;; nothing beyond this point should access compilation env ->
            (conf (ht-get tychoish/mail-accounts account-name))
            (maildir (tychoish--mail-account-maildir conf)))

       (setq tychoish/mail-account-current account-name)
       (setq message-directory maildir)
       (setq smtpmail-queue-dir (f-join maildir "queue" "cur"))
       (setq mu4e-mu-home (f-join maildir ".mu"))
       (setq message-auto-save-directory (f-join maildir "drafts"))

       (let ((signature-kind (tychoish--mail-account-signature-kind conf))
             (signature (tychoish--mail-account-signature conf))
             (address (tychoish--mail-account-address conf))
             (given-name (tychoish--mail-account-name conf)))

         (cond
          ((eq signature-kind 'signature-directory)
           (setq message-signature-directory (or signature (f-join maildir "tools" "signatures")))
           (setq message-signature-file (or address account-id account-name))
           (setq message-signature t))
          ((eq (tychoish--mail-account-signature-kind conf) 'signature-file)
           (setq message-signature-directory nil)
           (setq message-signature-file signature)
           (setq message-signature t))
          ((eq (tychoish--mail-account-signature-kind conf) 'signature-text)
           (setq message-signature-directory nil)
           (setq message-signature-file nil)
           (setq message-signature signature)))

         (setq user-mail-address address)
         (setq message-signature-file address)
         (setq user-full-name given-name)
         (setq mu4e-compose-reply-to-address address)
         (setq mu4e-reply-to-address address)

         (setq mail-host-address (s-replace-regexp ".*@" "" address))
         (setq message-sendmail-extra-arguments (list "-a" address))

         (when (eq major-mode 'mu4e-compose-mode)
           (goto-char (point-min))
           (let ((new-from (format "From: %s <%s>" given-name address)))
             (while (re-search-forward "^From:.*$" nil t 1)
               (replace-match new-from))))

         (setq mu4e-get-mail-command (tychoish--mail-account-fetchmail conf))

         (message (format "mail: configured address [%s]" address)))))))

(provide 'tychoish-bootstrap)
;;; tychoish-bootstrap.el ends here
