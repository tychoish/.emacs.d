(use-package prescient
  :ensure t
  :commands (prescient-persist-mode)
  :init
  (add-lazy-init
   :name "<core> prescient-persist"
   :delay 1
   :operation 'prescient-persist-mode)
  :config
  (setq prescient-completion-highlight-matches t)
  (setq prescient-filter-method '(literal prefix initialism anchored fuzzy regexp))
  (setq prescient-save-file (sprite-state-path "prescient.el"))
  ;; (setq completion-preview-sort-function #'prescient-completion-sort)
  (setq prescient-sort-full-matches-first t)
  (setq prescient-sort-length-enable nil))

(use-package vertico-prescient
  :ensure t
  :defer t
  :init
  (add-lazy-init
   :name "<core> vertico prescient"
   :delay 1
   :operation 'vertico-prescient-mode)
  :config
  (setq vertico-prescient-override-sorting t)
  ;; (setq vertico-prescient-enable-filtering t)
  (setq vertico-prescient-enable-sorting t))

(use-package corfu-prescient
  :ensure t
  :defer t
  :after (prescient)
  :init
  (add-lazy-init
   :name "<core> corfu prescient"
   :delay 1
   :operation 'corfu-prescient-mode)
  :config
  ;; (setq corfu-prescient-enable-filtering t)
  (setq corfu-prescient-override-sorting t)
  (setq corfu-prescient-enable-sorting t))
