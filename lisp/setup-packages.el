;;; setup-packages.el --- Configured package catalogs for elpaish -*- lexical-binding: t; -*-

;;; Commentary:
;; Package catalog for automated installation and upgrade via elpaish.
;; Compatible with both modern and older elpaish releases.

;;; Code:

(defvar setup-packages-list
  '(agent-shell-workflow
    annotated-completing-read
    anzu
    arch
    async
    builder
    cond-let
    consult-denote
    consult-eglot
    consult-flycheck
    consult-flyspell
    cov
    dape
    deadgrep
    denote-convert
    denote-dash
    denote-explore
    denote-journal-capture
    denote-markdown
    denote-mcp
    denote-org
    denote-review
    denote-sequence
    denote-silo
    denote-sync
    denote-tree
    docker
    eat
    eglot-tempel
    eglot-test-at-point
    embark-consult
    emojify
    f
    flycheck-eglot
    flyspell-correct
    gist
    git-link
    gptel-agent
    gptel-aibo
    gptel-mcp
    hitl
    journalctl-mode
    magit
    magit-gh
    marginalia
    mcpkit
    nerd-icons
    nerd-icons-corfu
    nerd-icons-dired
    nerd-icons-xref
    ninja-mode
    ollama
    ollama-tailnet
    org-docsgen
    package-build
    pkgbuild-mode
    popper
    protobuf-mode
    revbufs
    shell-maker
    slime
    sprite
    tailscale
    telega-bot
    tempel
    tempel-collection
    tracking
    undercover
    wgrep
    yaml-pro)
  "List of packages to install via elpaish.")

(defvar setup-packages-upgrade-list
  '(acp
    agent-shell
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
    eglox
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
    grepx
    hitl
    hud-modeline
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
    xtdlib)
  "List of packages to keep upgraded to latest via elpaish.")

;; Auto-register whenever elpaish-install is loaded or if already loaded
(with-eval-after-load 'elpaish-install
  (elpaish-install-register-package setup-packages-list)
  (elpaish-install-register-upgrade-package setup-packages-upgrade-list))

(provide 'setup-packages)
;;; setup-packages.el ends here
