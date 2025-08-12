(use-package helm-company
  :ensure t
  :bind (:map tychoish/helm-center-menu-map
         ("c" . helm-company)
         :map tychoish/company-ext-map
         ("h" . helm-company)
         ("d" . helm-company)
         :map company-active-map
         ("C-," . helm-company)
         :map company-mode-map
         ("C-," . helm-company))
  :commands (helm-company)
  :config
  (setq helm-company-initialize-pattern-with-prefix t))

(use-package company
  :ensure t
  :bind (
         :prefix "C-c ."
         :prefix-map tychoish/company-ext-map
         ("." . company-complete)
         ("s" . company-yasnippet)
         ("w" . company-ispell)
         ("f" . company-files)
         ("o" . company-other-backend)
         ("n" . company-other-backend)
         ("r" . company-powercycle)
         :map company-active-map
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("C-d" . company-b)
         ("M-." . company-show-location)
	 :map telega-chat-mode-map
	 ("<tab>" . company-indent-or-complete-common))
  :hook ((telega-chat-mode . company-mode-on))
  ;; (add-hook 'text-mode-hook 'tychoish/company-text-mode-hook)
  ;; (add-hook 'prog-mode-hook 'tychoish/company-prog-mode-hook)
  :commands (company-mode company-complete global-company-mode)
  :defines (company-backends)
  :diminish (company-mode . "cm")
  :init
  (setq-default company-idle-delay 0.01)
  (setq-default company-minimum-prefix-length 2)
  (setq-default company-backends '((company-capf company-dabbrev company-keywords company-ispell company-wordfreq
				    :with company-yasnippet company-emojify company-files)))

  (defun tychoish/company-text-mode-hook ()
    (setq-local company-minimum-prefix-length 3)
    (setq-local company-idle-delay 0.05)
    (setq-local company-backends '((company-ispell company-wordfreq company-dabbrev company-capf
 				    :with company-yasnippet company-emojify))))

  (defun tychoish/company-prog-mode-hook ()
    (setq-local company-minimum-prefix-length 2)
    (setq-local company-idle-delay 0)
    (setq-local company-backends '((company-capf company-keywords company-dabbrev-code
        			   :with company-yasnippet company-files))))

  (defun company-powercycle ()
    (interactive)
    (company-mode 1)
    (company-mode -1)
    (company-mode 1))

  (setq company-echo-delay 0)
  (setq company-show-numbers t)
  (setq company-tooltip-minimum-width 32)
  (setq company-tooltip-minimum 4)
  (setq company-tooltip-limit 16)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-annotation-padding 2)
  (setq company-tooltip-flip-when-above nil)
  (setq company-ispell-dictionary (expand-file-name "~/.aspell.en.pws"))
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-code-modes t)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-code-buffers t))

(use-package company-statistics
  :ensure t
  :after (company)
  :hook (company-mode . company-statistics-mode)
  :functions (company-statistics--maybe-save)
  :config
  (setq company-statistics-file (tychoish-get-config-file-path "company-statistics.el"))
  (setq company-statistics-size 1024)
  (setq company-statistics-auto-save t)
  (setq company-statistics-auto-restore t))

(use-package company-quickhelp
  :ensure t
  :after (company modus-themes)
  :hook ((company-mode . company-quickhelp-mode)
	 (company-mode . tychoish/theme-tweak-for-company-mode))
  :bind (:map company-active-map
	 ("C-?" . company-quickhelp-manual-begin))
  :defines (company-quickhelp-mode
	    company-quickhelp-color-background
	    company-quickhelp-color-foreground)
  :init
  (defun tychoish/theme-tweak-for-company-mode (&optional theme-name)
    (when (or (eq theme-name 'modus-operandi)
	      (eq theme-name 'modus-vivendi))
      (setq company-quickhelp-color-background (modus-themes-color 'bg-completion-subtle))
      (setq company-quickhelp-color-foreground (modus-themes-color 'fg-main))))
  :config
  (add-hook 'enable-theme-functions 'tychoish/theme-tweak-for-company-mode)
  (setq company-quickhelp-delay 0.25))

(use-package company-quickhelp-terminal
  :ensure t
  :after (company company-quickhelp)
  :bind (:map company-active-map
	 ("C-t" . company-quickhelp-terminal-mode))
  :commands (company-quickhelp-terminal-mode))

(use-package company-posframe
  :ensure t
  :diminish company-posframe-mode
  :after (company company-quickhelp)
  :init
  (defvar tychoish/use-company-posframe-when-possible nil)
  (defvar tychoish--company-quickhelp-was-enabled nil)
  (setq company-posframe-quickhelp-delay company-quickhelp-delay)

  (add-hook 'company-completion-started-hook 'tychoish/company-posframe-terminal-switch)
  (add-hook 'company-completion-started-hook 'tychoish/company-quickhelp-terminal-switch)

  (bind-keys :map tychoish/company-ext-map
	     :prefix "w"
	     :prefix-map tychoish/company-posframe-map
	     ("p" . toggle-company-posframe)
	     ("[" . turn-on-company-posframe)
	     ("]" . turn-off-company-posframe)
	     ("e" . enable-company-posframe)
	     ("d" . disable-company-posframe))

  (defun tychoish/company-posframe-terminal-switch (&optional manual-entry)
    "Enables or disables `company-posframe-mode' and `company-quickhelp-mode'
when switching between gui and terminal frames. For use in the `company-completion-started-hook'."
    (let ((toggle-state tychoish/use-company-posframe-when-possible))
      (when toggle-state
        (if (display-graphic-p)
            (turn-on-company-posframe)
          (turn-off-company-posframe)))
      (setq tychoish/use-company-posframe-when-possible toggle-state)))

  (defun tychoish/company-quickhelp-terminal-switch (manual-entry)
    "Enables or disables `company-quickhelp-terminal' mode, as needed based on
the current frame."
    (when company-quickhelp-mode
      (if (display-graphic-p)
	  (company-quickhelp-terminal-mode -1)
	(company-quickhelp-terminal-mode 1))))

  (defun turn-on-company-posframe ()
    "enables `company-posframe-mode' and disables quickhelp if necessary"
    (interactive)
    (company-posframe-mode 1)
    (diminish 'company-mode "cmpf")
    (when company-quickhelp-mode
      (setq tychoish--company-quickhelp-was-enabled t)
      (company-quickhelp-mode -1))
    (message "company-posframe activated!"))

  (defun turn-off-company-posframe ()
    "disables `company-posframe-mode'."
    (interactive)
    (company-posframe-mode -1)
    (diminish 'company-mode "cm")
    (unless tychoish--company-quickhelp-was-enabled
      (company-quickhelp-mode 1))
    (message "company-posframe deactivated"))

  (defun enable-company-posframe ()
    (interactive)
    (setq tychoish/use-company-posframe-when-possible t)
    (message "company-posframe enabled"))

  (defun disable-company-posframe ()
    (interactive)
    (setq tychoish/use-company-posframe-when-possible nil)
    (message "company-posframe disabled"))

  (defun toggle-company-posframe ()
    "Toggles between "
    (interactive)
    (if tychoish/use-company-posframe-when-possible
        (turn-off-company-posframe)
      (turn-on-company-posframe))))

(use-package company-wordfreq
  :ensure t
  :after company
  :commands (company-wordfreq company-wordfreq-download-list))

(use-package slime-company
  :ensure t
  :defer t
  :after (slime company)
  :hook ((common-lisp-mode lisp-mode slime-mode) . tychoish/company-slime-setup)
  :init
  (defun tychoish/company-slime-setup ()
    (setq-local company-backends
		'((company-slime company-keywords company-capf company-dabbrev-code company-files
				 :with company-yasnippet)))))

(use-package company-emojify
  :ensure t
  :after (company emojify)
  :commands (company-emojify)
  :bind (:map tychoish/company-ext-map
         ("e" . company-emojify))
  :config
  (setq company-emojify-emoji-styles '(ascii github unicode)))

(use-package company-solidity
  :ensure t
  :after (company solidity-mode)
  :hook (solidity-mode . tychoish/company-solidity-backend)
  :init
  (defun tychoish/company-solidity-backend ()
    (setq-local company-backends '((company-solidity company-capf company-dabbrev-code
				       :with company-yasnippet)))))

(provide 'tychoish-company)
