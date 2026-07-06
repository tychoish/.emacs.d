(use-package emacsql
  :ensure t
  :defer t)
(use-package ghub
  :ensure t
  :defer t)

(use-package forge
  :ensure t
  :after (ghub magit)
  :commands (forge-dispatch forge-configure)
  :config
  (setq forge-database-file (expand-file-name (f-join user-emacs-directory sprite--conf-state-directory "forge-database.sqlite")))
  (make-read-extended-command-for-prefix  "forge"
    :bind-map tychoish/magit-map
    :bind-key "r")

  (bind-keys
   :map magit-command-mode-map
   ("r" . execute-extended-forge-command))
  (which-key-customize "forge-commands" :map 'magit-command-mode-map :key "r"))
