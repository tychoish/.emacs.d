;;; setup-packages.el --- Configured package catalogs for elpaish -*- lexical-binding: t; -*-

;;; Commentary:
;; Package catalogs registered directly with elpaish.
;;
;; Packages passed to `elpaish-install-register-upgrade-package' are kept
;; continuously upgraded to latest.  `elpaish' automatically ensures that
;; all upgrade packages are also present in `elpaish-install-packages-list',
;; so no manual synchronization or superset maintenance is needed.
;;
;; Packages local to .emacs.d/lisp (such as `eglox', `grepx', and `hud-modeline')
;; are loaded directly from the local directory and must not be registered with
;; elpaish.

;;; Code:

(require 'elpaish-install)

(elpaish-install-register-upgrade-package
 '(acp
   agent-shell
   agent-shell-notifications
   agent-shell-queue
   agent-shell-workflow
   annotated-completing-read
   arch
   builder
   cape
   cond-let
   consult
   corfu
   denote
   denote-convert
   denote-dash
   denote-mcp
   denote-sync
   denote-tree
   eglot-test-at-point
   elisp-autofmt
   elpaish
   elpaish-keyring
   flycheck
   gen
   gptel
   gptel-agent
   gptel-aibo
   gptel-mcp
   gptel-pup
   hitl
   jarchive
   magit
   magit-dash
   marginalia
   mcpkit
   modus-themes
   ollama-tailnet
   orderless
   org-docsgen
   shell-maker
   sprite
   tailscale
   telega-bot
   tempel
   transient
   vertico
   xtdlib))

(elpaish-install-register-package
 '(anzu
   async
   consult-denote
   consult-eglot
   consult-flycheck
   consult-flyspell
   cov
   dape
   deadgrep
   denote-explore
   denote-journal-capture
   denote-markdown
   denote-org
   denote-review
   denote-sequence
   denote-silo
   docker
   eat
   eglot-tempel
   embark-consult
   emojify
   f
   flycheck-eglot
   flyspell-correct
   gist
   git-link
   journalctl-mode
   magit-gh
   nerd-icons
   nerd-icons-corfu
   nerd-icons-dired
   nerd-icons-xref
   ninja-mode
   ollama
   package-build
   pkgbuild-mode
   popper
   protobuf-mode
   revbufs
   slime
   tempel-collection
   tracking
   undercover
   wgrep
   yaml-pro))

(provide 'setup-packages)
;;; setup-packages.el ends here
