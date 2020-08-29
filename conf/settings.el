(defvar tychoish-emacs-identifier (default-string "solo" (daemonp)))

(setq frame-title-format '(:eval (if (stringp (daemonp))
				     (format "%s:%s" (daemonp) (buffer-name))
				   (concat "solo:" (buffer-name)))))

(setq server-use-tcp t)
(setq bookmark-save-flag 1)
(setq bookmark-default-file (tychoish-get-config-file-path "bookmarks"))

(setq custom-file (tychoish-get-config-file-path "custom.el"))
(when (and custom-file (file-exists-p custom-file))
  (load-file custom-file))

(when (eq system-type 'darwin)
  (setq ns-function-modifier 'hyper)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq ns-use-srgb-colorspace nil)
  (setq display-highres t))

(setq starttls-use-gnutls t)
(setq gnutls-log-level 0)
(setq confirm-kill-processes nil)

(setq inhibit-startup-echo-area-message (user-login-name))
(setq inhibit-startup-message t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq fringe-mode 'half-width)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq split-height-threshold 100)
(setq display-time-mode nil)
(setq indent-tabs-mode nil)

(setq size-indication-mode t)
(setq scroll-conservatively 25)
(setq scroll-preserve-screen-position 1)
(setq cursor-in-non-selected-windows nil)
(setq show-paren-delay 0.25)

(setq use-dialog-box nil)
(setq auto-revert-verbose nil)
(setq ring-bell-function (lambda () nil))

(setq version-control t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq vc-follow-symlinks t)
(setq vc-handled-backends nil)
(setq find-file-visit-truename t)

(setq compilation-ask-about-save nil)
(setq makefile-electric-keys t)
(setq compilation-scroll-output t)

(setq ansi-color-for-comint-mode t)
(setq auto-revert-interval 5)

(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

(setq query-replace-highlight t)
(setq search-highlight t)

(setq lpr-command "/usr/bin/lpr")
(setq lpr-add-switches "-T ''")

(set-default 'truncate-lines t)
(setq indicate-empty-lines t)

(setq tramp-default-method "ssh")
(setq font-lock-support-mode 'jit-lock-mode)
(setq ping-program-options '("-c" "4"))
(setq next-line-add-newlines nil)
(setq safe-local-variable-values '((encoding . utf-8)))
(setq undo-auto-current-boundary-timer t)

(setq jit-lock-stealth-time nil)
(setq jit-lock-defer-time 0.2)
(setq jit-lock-stealth-nice 0.2)
(setq jit-lock-stealth-load 100)

(setq delete-old-versions t)

(setq warnings-to-ignore '())
(add-to-list 'warnings-to-ignore '((free-vars) (nresolved) (callargs)
				   (redefine) (obsolete) (noruntine)
				   (cl-functions) (interactive-only)))
(setq byte-compile-warnings warnings-to-ignore)

(set-face-attribute 'header-line nil :background nil :weight 'bold)

(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))
(add-to-list 'auto-mode-alist '("/mutt" . message-mode))
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.bash$'" . sh-mode))
(add-to-list 'auto-mode-alist '("zsh'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh$'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.xml" . xml-mode))
(add-to-list 'auto-mode-alist '("makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.mk$" . makefile-mode))
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))

(fset 'yes-or-no-p 'y-or-n-p)
(put 'list-timers 'disabled nil)

(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)

(global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))

(global-set-key "(" 'tychoish-electric-pair)
(global-set-key "[" 'tychoish-electric-pair)
(global-set-key "{" 'tychoish-electric-pair)
(global-set-key "<" 'tychoish-electric-pair)
(global-set-key "\"" 'tychoish-electric-pair)
(global-set-key (kbd "RET") 'electrify-return-if-match)

;; Copy-Cut-Paste From Clipboard With Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

(global-set-key (kbd "C-c s w") 'ispell-word)

(global-set-key (kbd "C-c h f") 'find-function)
(global-set-key (kbd "C-c h v") 'find-variable)
(global-set-key (kbd "C-c h m") 'manual-entry)

(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "M-<SPC>") 'set-mark-command)
(global-set-key (kbd "C-c C-p") 'set-mark-command)
(global-set-key (kbd "C-x C-x") 'exchange-dot-and-mark)
(global-set-key (kbd "C-c c") 'comment-region)

(global-set-key (kbd "C-c t w") 'tycho-toggle-hooks)
(global-set-key (kbd "C-c t w") 'tycho-toggle-hooks)
(global-set-key (kbd "C-c t c") 'tychoish-compile-project)
(global-set-key (kbd "C-c C-t c") 'compile)
(global-set-key (kbd "C-x <escape>") 'next-error)
(global-set-key (kbd "C-c C-r") 'rename-buffer)
(global-set-key (kbd "C-c C-w") 'whitespace-cleanup)

(global-set-key (kbd "C-w") 'kill-region)
(global-set-key (kbd "C-h") 'backward-kill-word)
(global-set-key (kbd "M-C-q") 'fill-region)
(global-set-key (kbd "C-c C-f") 'set-fill-column)

(global-unset-key (kbd "C-x C-u"))
(global-set-key (kbd "C-x C-u t") 'upcase-initials-region)
(global-set-key (kbd "C-x C-u r") 'upcase-region)
(global-set-key (kbd "C-x C-u w") 'upcase-word)

(global-set-key (kbd "C-x C-n") 'word-count)
(global-set-key (kbd "C-x l") 'goto-line)

(global-set-key (kbd "C-c C-o") 'occur)
(global-set-key (kbd "C-c g f") 'find-grep)
(global-set-key (kbd "C-c g g") 'git-grep)
(global-set-key (kbd "C-c g r") 'git-grep-repo)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(eval-after-load 'comint
  '(progn
     (define-key comint-mode-map (kbd "M-n") 'comint-next-input)
     (define-key comint-mode-map (kbd "M-p") 'comint-previous-input)
     (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
     (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)))

(defalias 'rb 'revert-buffer)
(defalias 'revert 'revert-buffer)
(defalias 'srr 'string-replace-regexp)
(defalias 'sr 'string-replace)
(defalias 'eb 'eval-buffer)
(defalias 'dr 'delete-region)
(defalias 'hlm 'hl-line-mode)
(defalias 'dw 'delete-trailing-witespace)

(provide 'settings)
