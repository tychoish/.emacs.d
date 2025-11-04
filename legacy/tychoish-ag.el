(use-package ag
  :ensure t
  :bind (:map tychoish/ecclectic-grep-map ;; "C-c g"
	 :prefix "a"
	 :prefix-map tychoish/ecclectic-ag-grep-map
	 ("s" . ag)
	 ("f" . ag-files)
	 ("p" . ag-project)
	 :map tychoish/ecclectic-ag-grep-map
	 :prefix "d"
	 :prefix-map tychoish/ecclectic-ag-dired-map
	 ("f" . ag-dired)
	 ("p" . ag-project-dired))
  :init
  (which-key-add-keymap-based-replacements tychoish/ecclectic-grep-map
    "a" '("ag-grep" . tychoish/ecclectic-grep-map))
  :config
  (setq ag-highlight-search t))

(use-package consult-ag
  :vc (:url "https://github.com/abrochard/consult-ag" :rev "cf740cc")
  :ensure t
  :bind (("M-g a" . consult-ag)
	 :map tychoish/ecclectic-ag-grep-map ;; C-c g a
	 ("g" . consult-ag)
         :map tychoish/consult-mode-map ; "C-c C-."
         ("a" . consult-ag))
  :commands (consult-ag)
  :config
  (consult-customize consult-ag
   :require-match nil
   :group nil
   :keymap
   (with-temp-keymap map
     (define-key map (kbd "C-l") #'consult-ripgrep-up-directory)))))
