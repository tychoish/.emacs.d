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

(bind-keys ("C-x m" . execute-extended-command)
	   ("C-x C-m" . execute-extended-command)
	   ("M-X" . execute-extended-command-for-buffer)
           ("C-x b" . switch-to-buffer) ;; vs consult-buffer
           ("C-x l" . goto-line)
           ("C-x f" . find-file)
           ("C-x C-f" . find-file)
           ("C-x d" . dired)
           ("C-x h" . help)
           ("C-x C-x" . exchange-point-and-mark)
	   ;; ("C-x C-u w" . upcase-word)
	   ;; ("C-x C-u t" . upcase-initials-region)
	   ;; ("C-x C-u r" . upcase-region)
           ("C-x C-d" . dired)
           ("C-x C-n" . count-words)
           ("C-c i" . indent-region)
           ("C-c c" . comment-region)
           ("C-<backspace>" . backward-kill-word)
           ("C-h" . backward-kill-word)
           ("C-c d k s" . backward-kill-sentence)
           ("C-c d k p" . backward-kill-paragraph)
           ("C-c d k f" . backward-kill-sexp)
           ("C-c d k d" . delete-region)
           ("C-c d k w" . delete-trailing-whitespace)
           ("C-c d s" . describe-symbol)
           ("C-c d v" . describe-variable)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; silent startup --

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

(defun tychoish/late-init-opertions ()
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
  (xterm-mouse-mode 1)

  (delight 'eldoc-mode)
  (delight 'emacs-lisp-mode '("el" (lexical-binding ":l" ":d")) :major)
  (delight 'auto-fill-function " afm")
  (delight 'overwrite-mode "om")
  (delight 'refill-mode "rf")
  (delight 'visual-line-mode " wr")
  (delight 'fundamental-mode "fun"))

(defun tychoish-set-up-user-local-config ()
  "Ensure that all config files in the `user-emacs-directory' + '/user' path are loaded."
  (let ((dirname (concat (expand-file-name user-emacs-directory) "user")))
    (when (file-accessible-directory-p dirname)
      (add-to-list 'load-path dirname)
      (mapc (lambda (fn)
              (when (and (string-match-p "\\.el$" fn)
                         (not (string-match-p "^flycheck_.*\\.el$" fn)))
                 (require (intern (string-remove-suffix ".el" fn)))))
            (directory-files dirname))) t))

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

(setq tex-dvi-view-command "(f=*; pdflatex \"${f%.dvi}.tex\" && open \"${f%.dvi}.pdf\")")

(add-to-list 'auto-mode-alist '("\\.tex'" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'turn-off-auto-fill)
(add-hook 'eshell-mode #'eshell-cmpl-initialize)

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

(provide 'tychoish-bootstrap)
;;; tychoish-bootstrap.el ends here
