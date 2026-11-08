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
   chex
   cmdr
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
   eglox
   elpaish
   elpaish-keyring
   flycheck
   flycheck-aspell
   flycheck-vale
   gen
   gptel
   gptel-agent
   gptel-aibo
   gptel-mcp
   gptel-pup
   grepx
   hitl
   magit
   magit-dash
   marginalia
   mcpkit
   minaid
   modus-themes
   ollama-tailnet
   orderless
   org-docsgen
   orgx
   overmark
   shell-maker
   sprite
   tailscale
   telega-bot
   tempel
   transient
   treesit-auto
   vertico
   xtdlib))

(elpaish-install-register-package
 '(alert
   anzu
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
   ob-go
   ob-mermaid
   ob-rust
   ollama
   org-contrib
   ox-gfm
   ox-gist
   ox-hugo
   ox-leanpub
   ox-rst
   package-build
   pkgbuild-mode
   popper
   protobuf-mode
   revbufs
   slime
   telega
   tempel-collection
   toc-org
   tracking
   undercover
   wgrep
   yaml-pro))

(provide 'setup-packages)
;;; setup-packages.el ends here
