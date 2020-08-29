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
(put 'list-timers 'disabled nil)

(setq split-height-threshold 100)
(setq display-time-mode nil)
(setq indent-tabs-mode nil)

(setq size-indication-mode t)
(setq scroll-conservatively 25)
(setq scroll-preserve-screen-position 1)
(setq cursor-in-non-selected-windows nil)
(setq show-paren-delay 0.25)

(setq use-dialog-box nil)
(setq ring-bell-function (lambda () nil))

(setq version-control t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq vc-follow-symlinks t)
(setq vc-handled-backends nil)
(setq find-file-visit-truename t)

(setq compilation-ask-about-save nil)
(setq compilation-scroll-output t)

(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

(setq lpr-command "/usr/bin/lpr")
(setq lpr-add-switches "-T ''")

(set-default 'truncate-lines t)
(setq indicate-empty-lines t)

(setq tramp-default-method "ssh")
(setq ping-program-options '("-c" "4"))
(setq next-line-add-newlines nil)
(setq safe-local-variable-values '((encoding . utf-8)))
(setq undo-auto-current-boundary-timer t)

(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time nil)
(setq jit-lock-defer-time 0.2)
(setq jit-lock-stealth-nice 0.2)
(setq jit-lock-stealth-load 100)

(setq warnings-to-ignore '())
(add-to-list 'warnings-to-ignore '((free-vars) (nresolved) (callargs)
				   (redefine) (obsolete) (noruntine)
				   (cl-functions) (interactive-only)))
(setq byte-compile-warnings warnings-to-ignore)

(set-face-attribute 'header-line nil :background nil :weight 'bold)

(fset 'yes-or-no-p 'y-or-n-p)

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

(defalias 'eb 'eval-buffer)
(defalias 'dr 'delete-region)
(defalias 'dw 'delete-trailing-witespace)

(provide 'settings)
