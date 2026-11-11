;;; setup-modes.el --- Mode associations and file mappings -*- lexical-binding: t; -*-

;;; Commentary:
;; File extension mappings for `auto-mode-alist', `major-mode-remap-alist',
;; terminal emulator mappings, and core major mode hooks.
;; Merged early in `after-init-hook' before desktop restore.

;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function nerd-icons-dired-mode "nerd-icons-dired")
(declare-function sqlite-extras-minor-mode "sqlite-extras")
(declare-function orgx-mode-turn-on "orgx")
(declare-function orgx-agenda-minor-mode-turn-on "orgx")
(declare-function popper-echo-mode "popper")
(declare-function magit-dash-gh-prune-lazy-prefetch "magit-dash-gh")

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

;;; Direct mode hooks
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
(add-hook 'sqlite-mode-hook #'sqlite-extras-minor-mode)
(add-hook 'LaTeX-mode-hook #'turn-on-reftex)
(add-hook 'LaTeX-mode-hook #'visual-line-mode)
(add-hook 'LaTeX-mode-hook #'turn-off-auto-fill)
(add-hook 'org-mode-hook #'orgx-mode-turn-on)
(add-hook 'org-agenda-mode-hook #'orgx-agenda-minor-mode-turn-on)
(add-hook 'popper-mode-hook #'popper-echo-mode)
(add-hook 'magit-status-mode-hook #'magit-dash-gh-prune-lazy-prefetch)
(add-hook 'markdown-mode-hook #'turn-off-auto-fill)

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
