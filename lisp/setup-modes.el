;;; setup-modes.el --- Mode associations and file mappings -*- lexical-binding: t; -*-

;;; Commentary:
;; File extension mappings for `auto-mode-alist', `major-mode-remap-alist',
;; terminal emulator mappings, and core major mode hooks.

;;; Code:

;;; Mode associations

(dolist (mapping '(("\\.xml\\'" . nxml-mode)
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
                   ("\\.java\\'" . java-ts-mode)))
  (add-to-list 'auto-mode-alist mapping))

(add-to-list 'major-mode-remap-alist '(yaml-ts-mode . yaml-mode))

;;; Terminal file aliases

(add-to-list 'term-file-aliases '("alacritty" . "xterm"))
(add-to-list 'term-file-aliases '("ghostty" . "xterm-ghostty"))

;;; Mode hooks

(add-hook 'dired-mode-hook 'dired-async-mode)
(add-hook 'sqlite-mode-hook #'sqlite-extras-minor-mode)
(add-hook 'LaTeX-mode-hook #'turn-on-reftex)
(add-hook 'LaTeX-mode-hook #'visual-line-mode)
(add-hook 'LaTeX-mode-hook #'turn-off-auto-fill)

(provide 'setup-modes)
;;; setup-modes.el ends here
