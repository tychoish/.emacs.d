(cl-defmacro set-to-current-time-on-startup (variable &optional (depth 75))
  (let ((operation (intern (format "set-%s-to-current-time" (symbol-name variable)))))
    `(progn
       (add-hook 'emacs-startup-hook ',operation ,depth)
       (defun ,operation ()
	 (setq ,variable (current-time))))))

(use-package ctags-update
  :ensure t
  :bind (("C-c E" . ctags-update))
  :commands (turn-on-ctags-auto-update-mode create-tags)
  :delight ctags-auto-update-mode
  :config
  (setq tags-add-tables nil)
  (setq etags-table-search-up-depth 10)
  (setq ctags-update-delay-seconds 300)
  (defun create-tags (dir-name)
    "Create tags file for the DIR-NAME directory."
    (interactive "DDirectory: ")
    (let ((cmd-str (format "%s -e -u -f %s/TAGS %s -R %s" path-to-ctags dir-name dir-name (directory-file-name dir-name))))
      (message cmd-str)
      (shell-command cmd-str)))
  (setq path-to-ctags (executable-find "ctags"))
  (add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode))


(defun tychoish/go-test-filename-from-package (pkg)
  "Convert go package path to full file path."
  (let ((gopath (getenv "GOPATH")))
    (when gopath
      (expand-file-name (concat gopath "/src/" pkg)))))

(use-package cargo
  :ensure t
  :after (rustic)
  :config
  (setq cargo-process--command-fmt "+nightly fmt --all")
  (add-hook 'rustic-mode-hook 'cargo-minor-mode))

(use-package jinja2-mode
  :ensure t
  :mode "\\.jinja\\'")


(use-package just-mode
  :ensure t
  :after (builder)
  :mode (("justfile" . just-mode)
	 ("Justfile" . just-mode)
	 ("\\.just%" . just-mode)))

(use-package graphviz-dot-mode
  :ensure t
  :mode ("\\.gv" "\\.dot")
  :commands (graphviz graphviz-dot-mode)
  :init
  (setq graphviz-dot-indent-width 4))

(use-package deft
  :ensure t
  :bind (:map tychoish/docs-map
	      ("o" . deft)
	      ("n" . tychoish-deft-create)
	      ("f" . deft-find-file))
  :init
  (defun deft-find-file ()
    (interactive)
    (find-file deft-directory))

  (defun deft-file-make-slug (s)
    "Turn a string into a slug."
    (replace-regexp-in-string
     " " "-" (downcase
	      (replace-regexp-in-string
	       "[^A-Za-z0-9 ]" "" s))))

  (defun tychoish-deft-create (title)
    "Create a new deft entry."
    (interactive "sNote Title: ")
    (let ((draft-file (concat deft-directory
			      (deft-file-make-slug title)
			      "."
			      deft-extensions)))
      (if (file-exists-p draft-file)
	  (find-file draft-file)
	(find-file draft-file)
	(insert title))))
  :config
  (setq deft-extensions '("md" "mdwn" "markdown" "txt" "text" "rst"))
  (setq deft-new-file-format "%Y-%m-%dT.%H%M")
  (setq deft-default-extension "md")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0)
  (setq deft-auto-save-interval nil))

(use-package google-this
  :ensure t
  :delight google-this-mode
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :commands (google-this-mode)
  :config
  (which-key-customize "google-this" :key "C-c /")
  (setq google-this-browse-url-function 'browse-url-default-browser)
  (google-this-mode 1))

(use-package page-break-lines
  :ensure t
  :delight page-break-lines-mode
  :hook ((text-mode prog-mode) . page-break-lines-mode)
  :commands (global-page-break-lines-mode)
  :config
  (setq page-break-lines-modes '(text-mode prog-mode)))

(use-package winum
  :ensure t
  :bind (("C-x w n" . winum-select-window-by-number)
	 ("C-x w w" . winum-mode))
  :commands (winum-mode)
  :config
  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local))h

(use-package git-grep
  :ensure t
  :bind (:map tychoish/ecclectic-grep-project-map
	      ("g" . git-grep)))

(use-package grammarly
  :ensure t
  :defer t
  :defines (grammarly-on-message-function-list
	    grammarly-on-open-function-list
	    grammarly-on-close-function-list))

(use-package flycheck-grammarly
  :ensure t
  :after (flycheck flycheck-aspell grammarly)
  :commands (flycheck-grammarly-setup tychoish/flycheck-grammarly-enable tychoish/flycheck-grammarly-disable)
  :functions (flycheck-remove-next-checker flycheck-add-next-checker)
  :config
  (setq flycheck-grammarly-check-time 0.8)

  (defun tychoish/flycheck-grammarly-enable ()
    (interactive)
    (add-to-list 'flycheck-checkers 'grammarly)
    (add-to-list 'grammarly-on-open-function-list 'flycheck-grammarly--on-open)
    (add-to-list 'grammarly-on-message-function-list 'flycheck-grammarly--on-message)
    (add-to-list 'grammarly-on-close-function-list 'flycheck-grammarly--on-close)
    (flycheck-add-next-checker 'markdown-aspell-dynamic 'grammarly)
    (flycheck-add-next-checker 'mail-aspell-dynamic 'grammarly))

  (defun tychoish/flycheck-grammarly-disable ()
    (interactive)
    (flycheck-remove-next-checker 'markdown-aspell-dynamic 'grammarly)
    (flycheck-remove-next-checker 'mail-aspell-dynamic 'grammarly)
    (setq flycheck-checkers (remove 'grammarly flycheck-checkers))
    (setq grammarly-on-open-function-list (remove 'flycheck-grammarly--on-open 'grammarly-on-open-function-list))
    (setq grammarly-on-message-function-list (remove 'flycheck-grammarly--on-message 'grammarly-on-open-function-list))
    (setq grammarly-on-close-function-list (remove 'flycheck-grammarly--on-close 'grammarly-on-close-function-list))))

(use-package writeroom-mode
  :ensure t
  :bind (:map tychoish/display-map
	      ("i" . writeroom-mode)))

(use-package modus-themes-exporter
  :after modus-themes
  :commands (modus-themes-exporter-export))

(use-package vterm
  :ensure t
  :defer t
  :init
  (bind-keys
   :map tychoish/shell-map
   :prefix "v"
   :prefix-map tychoish/shell-vterm-map
   ("v" . vterm)
   ("e" . vterm-send-escape))

  (make-read-extended-command-for-prefix "vterm"
    :bind-key "m"
    :bind-map tychoish/shell-vterm-map))
