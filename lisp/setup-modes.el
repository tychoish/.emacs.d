;;; setup-modes.el --- Mode associations and file mappings -*- lexical-binding: t; -*-

;;; Commentary:
;; File extension mappings for `auto-mode-alist', `major-mode-remap-alist',
;; terminal emulator mappings, and core major mode hooks.
;; Merged early in `after-init-hook' before desktop restore.

;;; Code:

(require 'seq)

(defvaralias 'setup-modes-alist 'setup-modes-auto-mode-alist)

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

(defvar setup-modes-major-mode-remap-alist
  '((yaml-ts-mode . yaml-mode))
  "List of (ORIGINAL-MODE . NEW-MODE) pairs to merge into `major-mode-remap-alist'.")

(defvar setup-modes-term-file-aliases
  '(("alacritty" . "xterm")
    ("ghostty" . "xterm-ghostty"))
  "List of (TERMINAL-NAME . ALIAS) pairs to merge into `term-file-aliases'.")

(defvar setup-modes-hooks
  '((dired-mode-hook . dired-async-mode)
    (sqlite-mode-hook . sqlite-extras-minor-mode)
    (LaTeX-mode-hook . turn-on-reftex)
    (LaTeX-mode-hook . visual-line-mode)
    (LaTeX-mode-hook . turn-off-auto-fill))
  "Alist of (HOOK . FUNCTION) pairs to register during `setup-modes-initialize'.")

(defun setup-modes-merge-alist (current new-entries)
  "Merge NEW-ENTRIES into CURRENT alist, overriding existing keys."
  (thread-last current
    (seq-remove (lambda (entry) (assoc (car entry) new-entries)))
    (append new-entries)
    seq-uniq))

(defun setup-modes-initialize ()
  "Apply mode associations, terminal aliases, and major mode hooks.
Merges `setup-modes-auto-mode-alist' into `auto-mode-alist', overriding
any existing entries from earlier in the session, and registers mode hooks."
  (setq auto-mode-alist
        (setup-modes-merge-alist auto-mode-alist setup-modes-auto-mode-alist))
  (setq major-mode-remap-alist
        (setup-modes-merge-alist major-mode-remap-alist setup-modes-major-mode-remap-alist))
  (setq term-file-aliases
        (setup-modes-merge-alist term-file-aliases setup-modes-term-file-aliases))
  (dolist (hook-spec setup-modes-hooks)
    (add-hook (car hook-spec) (cdr hook-spec))))

(add-hook 'after-init-hook #'setup-modes-initialize -80)

(when after-init-time
  (setup-modes-initialize))

(provide 'setup-modes)
;;; setup-modes.el ends here
