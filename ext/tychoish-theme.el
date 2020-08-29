;;; tychoish-theme -- display configuration
;;; Commentary:

;; This file collects a bunch of theme and GUI rendering
;; configuration.

;; Relevant keybindings are defined in programming.el

;;; Code:

(let ((theme-directory (concat (expand-file-name user-emacs-directory) "theme")))
  (setq custom-theme-directory theme-directory)
  (add-to-list 'custom-theme-load-path theme-directory)
  (add-to-list 'load-path theme-directory))

(defvar after-theme-change-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-theme-change-hook))

(defadvice enable-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-theme-change-hook))

(defadvice disable-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-theme-change-hook))

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun on-frame-open (frame)
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (unless (display-graphic-p frame)
    (set-face-foreground 'default "unspecified-fg" frame)
    (set-face-background 'default "unspecified-bg" frame)))

(defun on-after-init ()
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (unless (display-graphic-p (selected-frame))
    (set-face-foreground 'default "unspecified-fg" frame)
    (set-face-background 'default "unspecified-bg" (selected-frame))))

;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
(add-hook 'after-make-frame-functions #'on-frame-open)
(add-hook 'window-setup-hook #'on-after-init)

(defun tychoish-load-dark-theme ()
  (interactive)
  (load-theme 'modus-vivendi t)
  (add-to-list 'default-frame-alist '(alpha . 85))
  (tychoish-doom-modeline-setup))

(defun tychoish-load-light-theme ()
  (interactive)
  (load-theme 'modus-operandi t)
  (add-to-list 'default-frame-alist '(alpha . 90))
  (tychoish-doom-modeline-setup))

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defun djcb-opacity-modify (&optional dec)
  "Modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	  (oldalpha (if alpha-or-nil alpha-or-nil 100))
	  (newalpha (if dec (- oldalpha 2) (+ oldalpha 2))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(defun opacity-increase ()
  (interactive)
  (djcb-opacity-modify))

(defun opacity-decrease ()
  (interactive)
  (djcb-opacity-modify t))

(defun opacity-reset ()
  (interactive)
  (modify-frame-parameters nil `((alpha . 100))))

(defvar *tychoish-current-font* nil)

(defun tychoish-font-setup (name number)
  (interactive "sName: \nNNumber:")
  (let ((new-font-name (concat name "-" (number-to-string number))))
    (set-face-attribute 'default nil :font new-font-name)
    (add-to-list 'default-frame-alist (cons 'font new-font-name))
    (unless (equal *tychoish-current-font* new-font-name)
      (setq *tychoish-current-font* new-font-name))))

(provide 'tychoish-theme)
;;; tychoish-theme.el ends here
