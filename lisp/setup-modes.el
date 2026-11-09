;;; setup-modes.el --- Mode associations and file mappings -*- lexical-binding: t; -*-

;;; Commentary:
;; File extension mappings for `auto-mode-alist', `major-mode-remap-alist',
;; terminal emulator mappings, and core major mode hooks.
;; Merged early in `after-init-hook' before desktop restore.

;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function dired-async-mode "dired-async")
(declare-function nerd-icons-dired-mode "nerd-icons-dired")
(declare-function sqlite-extras-minor-mode "sqlite-extras")
(declare-function anzu-mode "anzu")
(declare-function nerd-icons-xref-mode "nerd-icons-completion")
(declare-function orgx-mode-turn-on "orgx")
(declare-function orgx-agenda-minor-mode-turn-on "orgx")
(declare-function popper-echo-mode "popper")
(declare-function magit-dash-gh-prune-lazy-prefetch "magit-dash-gh")
(declare-function yaml-pro-mode "yaml-pro")
(declare-function cov-mode "cov")
(declare-function builder-elisp-mode "builder-elisp")

(defvar setup-modes-auto-mode-alist
  '(("\\.xml\\'" . nxml-mode)
    ("\\.rst\\'" . rst-mode)
    ("\\(?:^\\|/\\)[Mm]akefile\\'" . makefile-mode)
    ("\\.mk\\'" . makefile-mode)
    ("\\.tex\\'" . LaTeX-mode)
    ("\\.service\\'" . conf-unix-mode)
    ("\\.timer\\'" . conf-unix-mode)
    ("\\.target\\'" . conf-unix-mode)
    ("\\.mount\\'" . conf-unix-mode)
    ("\\.automount\\'" . conf-unix-mode)
    ("\\.slice\\'" . conf-unix-mode)
    ("\\.socket\\'" . conf-unix-mode)
    ("\\.path\\'" . conf-unix-mode)
    ("\\.conf\\'" . conf-unix-mode)
    ("\\.mdwn\\'" . markdown-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode)
    ("\\.txt\\'" . markdown-mode)
    ("PKGBUILD\\'" . pkgbuild-mode)
    ("\\.proto\\'" . protobuf-mode)
    ("\\.ninja\\'" . ninja-mode)
    ("\\.tf\\'" . terraform-mode)
    ("\\.tfvars\\'" . terraform-mode)
    ("\\.tfvars\\.example\\'" . terraform-mode)
    ("\\.sh\\'" . bash-ts-mode)
    ("\\.bash\\'" . bash-ts-mode)
    ("\\.bashrc\\'" . bash-ts-mode)
    ("Dockerfile" . dockerfile-ts-mode)
    ("\\.lisp\\'" . lisp-mode)
    ("\\.ya?ml\\'" . yaml-mode)
    ("\\.go\\'" . go-ts-mode)
    ("/go\\.work\\'" . go-mod-ts-mode)
    ("/go\\.mod\\'" . go-mod-ts-mode)
    ("\\.rs\\'" . rustic-mode)
    ("/Cargo\\.lock\\'" . toml-ts-mode)
    ("\\.py\\'" . python-ts-mode)
    ("\\.html\\'" . web-mode)
    ("\\.htm\\'" . web-mode)
    ("\\.as[cp]x\\'" . web-mode)
    ("\\.djhtml\\'" . web-mode)
    ("\\.erb\\'" . web-mode)
    ("\\.jsp\\'" . web-mode)
    ("\\.mustache\\'" . web-mode)
    ("\\.phtml\\'" . web-mode)
    ("\\.tpl\\.php\\'" . web-mode)
    ("\\.ts\\'" . typescript-ts-mode)
    ("\\.js\\'" . js-ts-mode)
    ("\\.tsx\\'" . tsx-ts-mode)
    ("\\.css\\'" . css-ts-mode)
    ("\\.json\\'" . json-ts-mode)
    ("\\.toml\\'" . toml-ts-mode)
    ("CMakeLists.txt" . cmake-ts-mode)
    ("\\.h\\'" . c-or-c++-mode)
    ("\\.c\\'" . c-ts-mode)
    ("\\.cpp\\'" . c++-ts-mode)
    ("\\.cc\\'" . c++-ts-mode)
    ("\\.hh\\'" . c++-ts-mode)
    ("\\.cxx\\'" . c++-ts-mode)
    ("\\.java\\'" . java-ts-mode))
  "List of (REGEXP . MODE) associations to merge into `auto-mode-alist'.")

;;; Direct major mode remaps
(add-to-list 'major-mode-remap-alist '(yaml-ts-mode . yaml-mode))

;;; Direct terminal file aliases
(add-to-list 'term-file-aliases '("alacritty" . "xterm"))
(add-to-list 'term-file-aliases '("ghostty" . "xterm-ghostty"))

;;; Direct mode setup helpers

(defvar markdown-imenu-generic-expression
  '(("title"  "^\\(.*\\)[\n]=+$" 1)
    ("h2-"    "^\\(.*\\)[\n]-+$" 1)
    ("h1"     "^# \\(.*\\)$" 1)
    ("h2"     "^## \\(.*\\)$" 1)
    ("h3"     "^### \\(.*\\)$" 1)
    ("h4"     "^#### \\(.*\\)$" 1)
    ("h5"     "^##### \\(.*\\)$" 1)
    ("h6"     "^###### \\(.*\\)$" 1)
    ("fn"     "^\\[^\\(.*\\)\\]" 1))
  "Generic expression for `imenu' in markdown buffers.")

(defun tychoish-set-local-fill-column-100 ()
  "Set `fill-column' to 100 in the current buffer."
  (setq-local fill-column 100))

(defun tychoish-set-local-tab-width-4 ()
  "Set `tab-width' to 4 in the current buffer."
  (setq-local tab-width 4))

(defun tychoish-yaml-set-tab-width ()
  "Set `tab-width' to 2 in the current buffer."
  (setq-local tab-width 2))

(defun tychoish-go-mode-setup ()
  "Buffer-local setup for Go buffers."
  (setq-local compilation-error-screen-columns nil))

(defun tychoish/markdown-setup-imenu ()
  "Configure `imenu-generic-expression' for markdown buffers."
  (setq imenu-generic-expression markdown-imenu-generic-expression))

(defun tychoish/markdown-disable-colon-electric-indent ()
  "Prevent `:' from triggering `markdown-indent-line', which
resets point to column 0 outside of definition-list context."
  (setq-local electric-indent-chars (remq ?: electric-indent-chars)))

(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))

;;; Direct mode hooks
(add-hook 'dired-mode-hook #'dired-async-mode)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
(add-hook 'sqlite-mode-hook #'sqlite-extras-minor-mode)
(add-hook 'LaTeX-mode-hook #'turn-on-reftex)
(add-hook 'LaTeX-mode-hook #'visual-line-mode)
(add-hook 'LaTeX-mode-hook #'turn-off-auto-fill)
(add-hook 'isearch-mode-hook #'anzu-mode)
(add-hook 'nerd-icons-completion-mode-hook #'nerd-icons-xref-mode)
(add-hook 'org-mode-hook #'orgx-mode-turn-on)
(add-hook 'org-agenda-mode-hook #'orgx-agenda-minor-mode-turn-on)
(add-hook 'popper-mode-hook #'popper-echo-mode)
(add-hook 'magit-status-mode-hook #'magit-dash-gh-prune-lazy-prefetch)
(add-hook 'markdown-mode-hook #'turn-off-auto-fill)
(add-hook 'markdown-mode-hook #'tychoish/markdown-setup-imenu)
(add-hook 'markdown-mode-hook #'tychoish/markdown-disable-colon-electric-indent)
(add-hook 'yaml-mode-hook #'yaml-pro-mode)
(add-hook 'yaml-mode-hook #'tychoish-yaml-set-tab-width)
(add-hook 'go-ts-mode-hook #'cov-mode)
(add-hook 'go-ts-mode-hook #'tychoish-set-local-fill-column-100)
(add-hook 'go-ts-mode-hook #'tychoish-go-mode-setup)
(add-hook 'python-ts-mode-hook #'tychoish-set-local-fill-column-100)
(add-hook 'python-ts-mode-hook #'tychoish-set-local-tab-width-4)
(add-hook 'rustic-mode-hook #'rustic-mode-auto-save-hook)
(add-hook 'emacs-lisp-mode-hook #'builder-elisp-mode)

(defun setup-modes-merge-alist (current new-entries)
  "Merge NEW-ENTRIES into CURRENT alist, overriding existing keys."
  (thread-last current
    (seq-remove (lambda (entry) (assoc (car entry) new-entries)))
    (append new-entries)
    seq-uniq))

(defun setup-modes-initialize ()
  "Merge `setup-modes-auto-mode-alist' into `auto-mode-alist', overriding
any existing entries from earlier in the session."
  (setq auto-mode-alist
        (setup-modes-merge-alist auto-mode-alist setup-modes-auto-mode-alist)))

(add-hook 'after-init-hook #'setup-modes-initialize -80)

(when after-init-time
  (setup-modes-initialize))

(provide 'setup-modes)
;;; setup-modes.el ends here
