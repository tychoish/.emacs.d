;;; setup-packages.el --- Configured package catalogs for elpaish -*- lexical-binding: t; -*-

;;; Commentary:
;; Package catalogs registered directly with elpaish.
;;
;; Packages passed to `elpaish-install-register-upgrade-package' are kept
;; continuously upgraded to latest.  `elpaish' automatically ensures that
;; all upgrade packages are also present in `elpaish-install-packages-list',
;; so no manual synchronization or superset maintenance is needed.
;;
;; Standalone packages extracted from .emacs.d/lisp are registered with
;; elpaish and kept continuously upgraded.

;;; Code:

(require 'elpaish-install)

(elpaish-install-register-upgrade-package
 '(acp
   agent-shell
   agent-shell-notifications
   agent-shell-queue
   agent-shell-workflow))

(elpaish-install-register-upgrade-package
 '(sprite
   annotated-completing-read
   overmark
   clix
   gen
   hitl
   hud-modeline
   mcpkit
   xtdlib))

(elpaish-install-register-upgrade-package
 '(orgx
   grepx
   cmdr
   minaid
   eglox
   chex
   telegax))

(elpaish-install-register-upgrade-package
 '(arch
   builder
   daemons-dash
   magit-dash
   tailscale
   telega-bot
   ollama-tailnet
   org-docsgen))

(elpaish-install-register-upgrade-package
 '(gptel-mcp
   gptel-pup
   denote-convert
   denote-dash
   denote-mcp
   denote-sync
   denote-tree))

(elpaish-install-register-upgrade-package
 '(cape
   orderless
   marginalia
   consult
   corfu
   flycheck
   flycheck-aspell
   flycheck-vale
   tempel
   vertico))

(elpaish-install-register-upgrade-package
 '(gptel
   magit
   code-review
   denote))

(elpaish-install-register-upgrade-package
 '(gptel-agent
   gptel-aibo
   eglot-test-at-point
   shell-maker
   modus-themes))

(elpaish-install-register-package
 '(alert
   anzu
   async
   cond-let
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
   transient
   treesit-auto
   undercover
   wgrep
   yaml-pro))

(provide 'setup-packages)
;;; setup-packages.el ends here
