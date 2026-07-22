;;; hud-mode.el --- global minor mode holding tycho's custom keybindings -*- lexical-binding: t; -*-

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0-pre
;; Package-Requires: ((emacs "29.1"))
;; Keywords: internal maint emacs convenience

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A single global minor mode (`hud-mode') that owns every custom prefix
;; keymap and general-purpose editing/window/buffer command that used to
;; live directly in `global-map' via bootstrap.el.  Enabling `hud-mode'
;; (done once, from bootstrap's late-enable-modes) makes `hud-mode-map'
;; active everywhere; disabling it removes all of these bindings without
;; touching `global-map' itself.
;;
;; This file intentionally does not depend on bootstrap.el -- keymaps here
;; may reference commands still defined in bootstrap.el by symbol (that's
;; fine, keymaps just store symbols), but nothing here calls a bootstrap.el
;; function at load time.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; keymap definitions -- top level C-c <> maps

(defvar-keymap hud-core-map  ;; "C-c t"
  "k" #'execute-extended-clipboard-command
  "p" #'toggle-electric-pair-inhibition
  "e" #'toggle-electric-pair-eagerness)

(defvar-keymap hud-display-map ;; "C-c f"
  "=" #'text-scale-increase
  "-" #'text-scale-decrease
  "0" #'text-scale-reset
  "h" #'auto-fill-mode
  "s" #'visual-line-mode)

(defvar-keymap hud-kill-map ;; "C-c k"
  "s" #'backward-kill-sentence
  "p" #'backward-kill-paragraph
  "f" #'backward-kill-sexp
  "d" #'delete-region
  "w" #'delete-trailing-whitespace)

(defvar-keymap hud-web-browser-map ;; "C-c w"
  "d" #'browse-url-generic
  "e" #'browse-url
  "f" #'browse-url-firefox
  "c" #'browse-url-chrome
  "g" #'eww-search-words)

(defvar-keymap hud-docs-map ;; "C-c h"
  "s" #'hud-describe-symbol-dwim
  "v" #'describe-variable
  "q" #'kill-eldoc-and-help-buffers
  "j" #'jump-to-elisp-help
  "e" #'eldoc
  "b" #'eldoc-doc-buffer)

(defvar-keymap hud-ecclectic-grep-map ;; "C-c g"
  "o" #'occur
  "g" #'grep)

(defvar-keymap hud-ide-map ;; "C-c l"
  "m" #'imenu)

(defvar-keymap hud-completion-map ;; "C-c ."
  "TAB" #'completion-at-point
  "." #'completion-at-point
  "p" #'completion-at-point)

(defvar-keymap hud-shell-map) ;; "C-c s"
(defvar-keymap hud-denote-map) ;; "C-c d"
(defvar-keymap hud-robot-map) ;; "C-c r"
(defvar-keymap hud-anzu-map) ;; "C-c q"
(defvar-keymap hud-mail-map) ;; "C-c m"
(defvar-keymap hud-magit-map) ;; "C-x g"
(defvar-keymap hud-docker-map) ;; "C-x d"
(defvar-keymap orgx-global-map) ;; "C-c o"

;; nested keymaps
(defvar-keymap hud-display-opacity-map ;; "C-c f o"
  "=" #'hud-opacity-increase
  "-" #'hud-opacity-decrease
  "0" #'hud-opacity-reset)

(defvar-keymap hud-buffer-control-map ;; "C-x C-b"
  "k" #'kill-this-buffer)

(defvar-keymap hud-blogging-map ;; "C-c t b"
  "m" #'hud-insert-date)

(defvar-keymap hud-theme-map) ;; "C-c t t"

(defvar-keymap hud-ecclectic-grep-project-map ;; "C-c g p"
  "f" #'find-grep)

(defvar-keymap hud-ecclectic-rg-map) ;; "C-c g r"
(defvar-keymap hud-consult-search-map) ;; "C-c g s"
(defvar-keymap hud-eglot-global-map) ;; "C-c l l"
(defvar-keymap hud-robot-agent-shell-map) ;; "C-c r s"
(defvar-keymap hud-shell-eat-map) ;; "C-c s e"
(defvar-keymap hud-denote-sequence-map) ;; "C-c d s"
(defvar-keymap hud-denote-org-map) ;; "C-c d o"
(defvar-keymap hud-denote-explore-map) ;; "C-c d e"
(defvar-keymap hud-denote-review-map) ;; "C-c d c"
(defvar-keymap hud-consult-mode-map) ;; "C-c C-;"
(defvar-keymap hud-smerge-map) ;; "C-x g m"
(defvar-keymap orgx-link-map) ;; "C-c o l"
(defvar-keymap hud-robot-gptel-map) ;; "C-c r g"
(defvar-keymap hud-robot-gptel-set-default-model-map) ;; "C-c r g m"
(defvar-keymap hud-yasnippet-map) ;; "C-c &" (in `yas-minor-mode-map')

;; the mode's own container map -- populated below
(defvar-keymap hud-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hud-mode-map wiring -- everything attaches here instead of global-map

(keymap-set minibuffer-local-map "C-g" #'hud-super-abort-minibuffers)
(keymap-set minibuffer-local-map "C-l" #'backward-kill-word)
(keymap-set hud-mode-map "C-c t" (cons "core" hud-core-map))
(keymap-set hud-core-map "b" (cons "blogging" hud-blogging-map))
(keymap-set hud-core-map "t" (cons "theme" hud-theme-map))
(keymap-set hud-mode-map "C-c f" (cons "display" hud-display-map))
(keymap-set hud-display-map "o" (cons "opacity" hud-display-opacity-map))
(keymap-set hud-mode-map "C-c k" (cons "kill" hud-kill-map))
(keymap-set hud-mode-map "C-c w" (cons "web-browser" hud-web-browser-map))
(keymap-set hud-mode-map "C-c g" (cons "grep" hud-ecclectic-grep-map))
(keymap-set hud-ecclectic-grep-map "p" (cons "project-grep" hud-ecclectic-grep-project-map))
(keymap-set hud-ecclectic-grep-map "r" (cons "+ripgrep" hud-ecclectic-rg-map))
(keymap-set hud-ecclectic-grep-map "s" (cons "consult-search" hud-consult-search-map))
(keymap-set hud-mode-map "C-c ." (cons "completion" hud-completion-map))
(keymap-set hud-mode-map "C-c l" (cons "ide" hud-ide-map))
(keymap-set hud-ide-map "l" (cons "eglot" hud-eglot-global-map))
(keymap-set hud-mode-map "C-c h" (cons "docs" hud-docs-map))
(keymap-set hud-mode-map "C-x C-b" (cons "buffer-control" hud-buffer-control-map))
(keymap-set hud-mode-map "C-c s" (cons "shell" hud-shell-map))
(keymap-set hud-shell-map "e" (cons "shell-eat" hud-shell-eat-map))
(keymap-set hud-mode-map "C-c r" (cons "robot" hud-robot-map))
(keymap-set hud-robot-map "s" (cons "agent-shell" hud-robot-agent-shell-map))
(keymap-set hud-robot-map "g" (cons "gptel" hud-robot-gptel-map))
(keymap-set hud-robot-gptel-map "m" (cons "gptel-set-default-model" hud-robot-gptel-set-default-model-map))

(keymap-set hud-mode-map "C-c q" (cons "anzu" hud-anzu-map))
(keymap-set hud-mode-map "C-x g" (cons "magit" hud-magit-map))
(keymap-set hud-mode-map "C-x d" (cons "docker" hud-docker-map))

(keymap-set hud-mode-map "C-c o" (cons "org" orgx-global-map))
(keymap-set orgx-global-map "l" (cons "org-link" orgx-link-map))
(keymap-set hud-mode-map "C-c m" (cons "mail" hud-mail-map))

(keymap-set hud-mode-map "C-c d" (cons "denote" hud-denote-map))
(keymap-set hud-denote-map "o" (cons "denote-org" hud-denote-org-map))
(keymap-set hud-denote-map "s" (cons "denote-sequence" hud-denote-sequence-map))
(keymap-set hud-denote-map "e" (cons "denote-explore" hud-denote-explore-map))
(keymap-set hud-denote-map "c" (cons "denote-review" hud-denote-review-map))

(keymap-set hud-mode-map "C-c C-;" (cons "consult" hud-consult-mode-map))
(keymap-set hud-magit-map "m" (cons "smerge" hud-smerge-map))

;; general bindings that used to go straight into global-map
(keymap-set hud-mode-map "C-x m" #'execute-extended-command)
(keymap-set hud-mode-map "C-x C-m" #'execute-extended-command)
(keymap-set hud-mode-map "M-X" #'execute-extended-command-for-buffer)
(keymap-set hud-mode-map "C-x l" #'goto-line)
(keymap-set hud-mode-map "C-x f" #'find-file)
(keymap-set hud-mode-map "C-x C-f" #'find-file)
(keymap-set hud-docs-map "h" #'help)
(keymap-set hud-mode-map "C-x h" #'mark-whole-buffer) ;; default
(keymap-set hud-mode-map "C-x C-x" #'exchange-point-and-mark)
(keymap-set hud-mode-map "C-c u w" #'upcase-word)
(keymap-set hud-mode-map "C-c u t" #'upcase-initials-region)
(keymap-set hud-mode-map "C-c u r" #'upcase-region)
(keymap-set hud-mode-map "C-x C-n" #'count-words)
(keymap-set hud-mode-map "C-c i" #'indent-region)
(keymap-set hud-mode-map "C-c c" #'comment-region)
(keymap-set hud-mode-map "C-h" #'backward-kill-word)
(keymap-set hud-mode-map "C-c C-f" #'set-fill-column)
(keymap-set hud-mode-map "C-c C-p" #'set-mark-command)
(keymap-set hud-mode-map "C-c C-r" #'rename-buffer)
(keymap-set hud-docs-map "a" #'mark-whole-buffer)
(keymap-set hud-mode-map "C-z" #'undo)
(keymap-set hud-mode-map "C-w" #'kill-region)
(keymap-set hud-mode-map "C-<backspace>" #'backward-kill-word)
(keymap-set hud-mode-map "M-<SPC>" #'set-mark-command)
(keymap-set hud-mode-map "C-<tab>" #'completion-at-point)
(keymap-set hud-mode-map "s-h" #'hud-increase-window-left)
(keymap-set hud-mode-map "s-j" #'hud-increase-window-down)
(keymap-set hud-mode-map "s-k" #'hud-increase-window-up)
(keymap-set hud-mode-map "s-l" #'hud-increase-window-right)
(keymap-set hud-mode-map "<mouse-2>" #'clipboard-yank)
(keymap-set hud-mode-map "s-<left>" #'hud-increase-window-left)
(keymap-set hud-mode-map "s-<down>" #'hud-increase-window-down)
(keymap-set hud-mode-map "s-<up>" #'hud-increase-window-up)
(keymap-set hud-mode-map "s-<right>" #'hud-increase-window-right)
(keymap-set hud-mode-map "s-c" #'clipboard-kill-ring-save) ;; (CUA/macOS) copy
(keymap-set hud-mode-map "s-v" #'clipboard-yank)            ;; (CUA/macOS) paste
(keymap-set hud-mode-map "s-x" #'clipboard-kill-region)     ;; (CUA/macOS) cut
(keymap-set hud-mode-map "M-<up>" #'move-text-up)
(keymap-set hud-mode-map "M-<down>" #'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; window management

(defun hud-increase-window-up () (interactive) (enlarge-window 1 nil))
(defun hud-increase-window-down () (interactive) (enlarge-window -1 nil))
(defun hud-increase-window-left () (interactive) (enlarge-window 1 t))
(defun hud-increase-window-right () (interactive) (enlarge-window -1 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; frame management

(defvar-local hud--buffer-home-frame nil
  "Frame where this buffer was first shown; nil means no frame restriction.")

(defun hud--record-home-frame ()
  "Record the selected frame as this buffer's home frame (first visit only)."
  (unless hud--buffer-home-frame
    (setq-local hud--buffer-home-frame (selected-frame))))

(defun hud--frame-buffer-predicate (buf)
  "Unified predicate controlling next-buffer and switch-to-buffer candidates."
  (let* ((current (current-buffer))
         (current-file (buffer-file-name current))
         (current-readonly (buffer-local-value 'buffer-read-only current)))
    (with-current-buffer buf
      (and
       ;; Rule 1: frame-sticky buffers stay on their home frame
       (or (null hud--buffer-home-frame)
           (eq hud--buffer-home-frame (selected-frame)))
       ;; Rules 2 & 3: file-based navigation restrictions
       (cond
        ;; Not in a file buffer — no restriction
        ((null current-file) t)
        ;; In a read-only file (reference buffer) — only writable files
        (current-readonly (and (buffer-file-name) (not buffer-read-only)))
        ;; In a writable file — only file buffers (any)
        (t (buffer-file-name)))))))

(defun hud--install-buffer-predicate (frame)
  "Install the unified buffer predicate on FRAME."
  (set-frame-parameter frame 'buffer-predicate
                        #'hud--frame-buffer-predicate))

;;;###autoload
(defun hud-run-current-major-mode-hooks (&optional buffer)
  "Run all mode-hooks for the current major mode."
  (interactive)
  (with-current-buffer (or (when (bufferp buffer) buffer)
			   (when (and (stringp buffer) (get-buffer buffer)) buffer)
			   (current-buffer))
    (apply #'run-mode-hooks (seq-keep (lambda (it) (intern-soft (format "%s-hook" it))) (derived-mode-all-parents major-mode)))))

;; display-buffer: reference (read-only file) buffers reuse an existing
;; read-only window rather than displacing a writable project-file window
(defun hud--readonly-file-buffer-p (buf _action)
  "Return non-nil if BUF is a read-only file-visiting buffer."
  (with-current-buffer buf
    (and buffer-read-only (buffer-file-name))))

(defun hud--reuse-readonly-file-window (buffer _alist)
  "Action: display BUFFER in an existing read-only file window if one exists."
  (when-let* ((win (seq-find
                    (lambda (w)
                      (and (not (eq w (selected-window)))
                           (with-current-buffer (window-buffer w)
                             (and buffer-read-only (buffer-file-name)))))
                    (window-list nil 'nomini))))
    (set-window-buffer win buffer)
    win))

(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))

(defun frame-unset-background-for-tty (frame)
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (unless (display-graphic-p frame)
    (set-face-background 'default "undefined" frame)
    (set-face-background 'default "undefined" frame)
    (set-face-attribute 'default frame :background 'unspecified :foreground 'unspecified)))

(defun current-frame-unset-background-for-tty ()
  "Reset the background on the current frame, but only if its a TTY frame."
  (interactive)
  (frame-unset-background-for-tty (selected-frame)))

(defun frame-enable-xterm-mouse-for-tty (frame)
  "Enable xterm-mouse-mode when FRAME is a tty frame."
  (unless (display-graphic-p frame)
    (xterm-mouse-mode 1)))

(defun current-frame-enable-xterm-mouse-for-tty ()
  "Enable xterm-mouse-mode if the current frame is a tty."
  (frame-enable-xterm-mouse-for-tty (selected-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editing -- text editing, experience, and manipulation

;; move-text  -- use arrow keys to move whole

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

;;;###autoload
(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

;;;###autoload
(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; word-wrapping  --

(defalias 'turn-on-hard-wrap 'turn-off-soft-wrap)
(defalias 'turn-off-hard-wrap 'turn-on-soft-wrap)
(defalias 'toggle-soft-wrap 'toggle-on-soft-wrap)
(defalias 'toggle-hard-wrap 'toggle-off-soft-wrap)

;;;###autoload
(defun turn-on-soft-wrap ()
  (interactive)
  (let ((was-hard-wrapping auto-fill-function))
    (auto-fill-mode -1)
    (visual-line-mode 1)
    (when was-hard-wrapping
      (hud-show-wrapping-mode))))

;;;###autoload
(defun turn-off-soft-wrap ()
  (interactive)
  (let ((was-soft-wrapping (not auto-fill-function)))
    (visual-line-mode -1)
    (auto-fill-mode 1)
    (when was-soft-wrapping
      (hud-show-wrapping-mode))))

;;;###autoload
(defun toggle-word-wrap (&optional arg)
  (interactive)
  (when arg
    (user-error "ambiguous argument to `toggle-word-wrap'"))
  (if auto-fill-function
      (turn-on-soft-wrap)
    (turn-on-hard-wrap)))

(defun hud-show-wrapping-mode ()
  (let ((buf (current-buffer))
	(wrapping-mode (if auto-fill-function
                           "hard"
                         "soft")))
    (message "wrapping mode `%s' for %s <%s>"
	     wrapping-mode
	     (buffer-local-value 'major-mode buf)
	     (buffer-name buf))))

;;;###autoload
(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp-in-region "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" begin end))

;; tab width --

(defmacro hud-set-tab-width (num)
  (unless (integerp num)
    (signal 'wrong-type-argument num))
  (unless (< num 32)
    (warn "INVALID cannot create tab width hook function to >= 32 (%s)" num))

  (let ((generated-name (intern (format "hud-set-local-tab-width-%d" num))))
    `(defun ,generated-name ()
       (set-tab-width ,num))))

;;;###autoload
(defun set-tab-width (num-spaces)
  (interactive "nTab width: ")
  (setq-local tab-width num-spaces))

(defun font-lock-show-tabs ()
  "Return a font-lock style keyword for tab characters."
  '(("\t" 0 'trailing-whitespace prepend)))

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for strings beyond WIDTH that use `font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

;; line manipulation

;;;###autoload
(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines between BEG and END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

;;;###autoload
(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;; files and notes

;;;###autoload
(defun hud-insert-date ()
  "Insert date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun jump-to-elisp-help ()
  (interactive)
  (apropos-documentation (symbol-name (intern-soft (thing-at-point 'symbol)))))

(declare-function consult-eglot-symbols "consult-eglot")

;;;###autoload
(defun hud-describe-symbol-dwim (prefix)
  "Look up symbol at point contextually.
With PREFIX arg, always use `describe-symbol'.
Otherwise: use `slime-describe-symbol' if slime is connected,
`consult-eglot-symbols' if in an eglot-managed buffer,
or `describe-symbol' as fallback."
  (interactive "P")
  (cond
   (prefix
    (call-interactively #'describe-symbol))
   ((and (boundp 'slime-describe-symbol)
         (boundp 'slime-connected-p)
         (slime-connected-p))
    (call-interactively #'slime-describe-symbol))
   ((and (boundp 'eglot-current-server)
         (boundp 'consult-eglot-symbols)
         (eglot-current-server))
    (consult-eglot-symbols))
   (t
    (call-interactively #'describe-symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display

;;;###autoload
(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; opacity

(defun djcb-opacity-modify (&optional dec)
  "Modify frame transparency by 5% steps."
  (let* ((raw (frame-parameter nil 'alpha))
         (current (cond
                   ((null raw) 1.0)
                   ((floatp raw) raw)
                   (t (/ raw 100.0))))
         (next (if dec (- current 0.025) (+ current 0.025))))
    (when (and (>= next 0.2) (<= next 1.0))
      (modify-frame-parameters nil (list (cons 'alpha next))))))

;;;###autoload
(defun hud-opacity-increase ()
  (interactive)
  (djcb-opacity-modify))

;;;###autoload
(defun hud-opacity-decrease ()
  (interactive)
  (djcb-opacity-modify t))

;;;###autoload
(defun hud-opacity-reset ()
  (interactive)
  (modify-frame-parameters nil '((alpha . 0.95))))

(defvar-keymap hud-opacity-repeat-map
  :repeat t
  "=" #'hud-opacity-increase
  "-" #'hud-opacity-decrease
  "0" #'hud-opacity-reset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extra buffer management tools

(declare-function annotated-completing-read-directory "annotated-completing-read")
(declare-function f-ancestor-of-p "f")

(defun hud--minimize-path (path)
  "Replace the expanded home directory prefix in PATH with `~/'."
  (string-replace (expand-file-name "~/") "~/" path))

;;;###autoload
(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t t))

;;;###autoload
(defun kill-eldoc-and-help-buffers ()
  "Kills all eldoc and help buffers"
  (interactive)
  (kill-matching-buffers "\\*Help\\*\\|\\*eldoc.*\\*" nil t))

(defun buffers-matching-path (regexp &optional internal-too)
  (seq-filter (lambda (buf)
                (when-let* ((name (buffer-file-name buf)))
                  (and (not (string-equal name ""))
                       (or internal-too (/= (aref name 0) ?\s))
                       (string-match regexp name))))
              (buffer-list)))

(defun buffers-matching-mode (mode)
  (seq-filter (lambda (buf) (with-current-buffer buf (eq major-mode mode)))
              (buffer-list)))

;;;###autoload
(defun kill-buffers-in-directory (&optional directory)
  "Kill all buffers in `directory'. When not defined, a directory can be selected interactively."
  (interactive)

  (unless directory
    (setq directory (annotated-completing-read-directory)))

  (let ((killed (thread-last (buffer-list)
			     (seq-filter #'buffer-file-name)
			     (seq-filter (lambda (buf) (f-ancestor-of-p directory (buffer-file-name buf))))
			     (seq-map (lambda (buf) (cons (buffer-file-name buf) (kill-buffer buf))))
			     (seq-filter #'cdr)
			     (seq-map (lambda (c) (hud--minimize-path (car c)))))))

    (if (called-interactively-p 'any)
	(message "killed %d buffers in subdirectory %s: '%S'" (length killed) (hud--minimize-path directory) (string-join killed ", "))
      killed)))

(defalias 'kill-buffers-matching-name 'kill-matching-buffers)

;;;###autoload
(defun force-kill-buffers-matching-path (regexp)
  (interactive "sKill buffers visiting a path matching this regular expression: \n")
  (kill-buffers-matching-path regexp t t))

;;;###autoload
(defun kill-buffers-matching-path (regexp &optional internal-too no-ask)
  "Kill buffers whose name matches the specified REGEXP.
Ignores buffers whose name starts with a space, unless optional
prefix argument INTERNAL-TOO is non-nil.  Asks before killing
each buffer, unless NO-ASK is non-nil."
  (interactive "sKill buffers visiting a path matching this regular expression: \n")
  (let ((killed (thread-last
		  (buffers-matching-path regexp internal-too)
		  (seq-map (lambda (buf)
			     (cons (buffer-file-name buf)
				   (funcall (if no-ask #'kill-buffer #'kill-buffer-ask) buf))))
		  (seq-filter #'cdr)
		  (seq-map #'car))))

    (if (called-interactively-p 'any)
	(message "killed %d buffers matching '%S'" (length killed) (string-join killed ", "))
      killed)))

;;;###autoload
(defun kill-all-reference-and-source-buffers ()
  "Kill all buffers for files in external (upstream) sources, likely opened
by jump-to-definition."
  (interactive)
  (let ((killed (thread-last
		  '("/usr/share/emacs/.*" "/usr/lib/go/.*" ".*/src/emacs.*/src/.*")
		  (append (cons package-user-dir package-directory-list))
		  (seq-map #'force-kill-buffers-matching-path)
		  (seq-filter 'identity)
                  (seq-filter #'stringp)
		  (seq-map #'hud--minimize-path))))
    (if (called-interactively-p 'any)
	(message "killed %s refrence/source buffers [%s]" (length killed) (string-join killed ", "))
      killed)))

;;;###autoload
(defun kill-buffers-matching-mode (mode)
  "Kill all buffers matching the symbol defined by MODE.
Returns the number of buffers killed."
  (interactive
   (list (intern
          (completing-read
           "mode: " ;; prompt
           obarray  ;; collection
           (lambda (symbol) (string-suffix-p "-mode" (symbol-name symbol)))
           t nil nil major-mode))))
  (let* ((buffers (buffers-matching-mode mode))
	 (count (length buffers)))
    (message "killing all buffers (%d) with mode \"%s\"" count mode)
    (mapc #'kill-buffer buffers)
    count))

;;;###autoload
(defun kill-buffers-visiting-missing-files ()
  "Kill buffers visiting files that no longer exist on disk.
Prompts before killing each buffer.  Returns the list of killed file paths
when called non-interactively."
  (interactive)
  (let ((killed (thread-last (buffer-list)
                             (seq-filter (lambda (buf)
                                           (when-let* ((file (buffer-file-name buf)))
                                             (not (file-exists-p file)))))
                             (seq-map (lambda (buf)
                                        (cons (buffer-file-name buf) (kill-buffer-ask buf))))
                             (seq-filter #'cdr)
                             (seq-map #'car))))
    (if (called-interactively-p 'any)
        (message "killed %d buffers visiting missing files%s"
                 (length killed)
                 (if killed
                     (format ": %s" (string-join (seq-map #'hud--minimize-path killed) ", "))
                   ""))
      killed)))

;;;###autoload
(defun kill-buffer-and-delete-file (&optional buffer)
  "Kill BUFFER (default the current buffer), then delete the file it visits.
Errors when BUFFER is not visiting a file.  The buffer is killed
before the file is deleted, so declining Emacs's built-in \"buffer
modified; kill anyway?\" prompt, or its \"save and then kill\" choice,
leaves the file on disk.  Never prompts when called non-interactively."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer)))
    (unless file
      (user-error "Buffer '%s' is not visiting a file" (buffer-name buffer)))
    (if (called-interactively-p 'any)
        (unless (kill-buffer buffer)
          (user-error "Aborted: '%s' was not killed" (buffer-name buffer)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    (delete-file file)
    (when (called-interactively-p 'any)
      (message "Deleted %s" (hud--minimize-path file)))))

(defun hud-super-abort-minibuffers ()
  (interactive)
  (if (not (minibuffer-selected-window))
      (keyboard-quit)
    (abort-minibuffers)
    (minibuffer-keyboard-quit))
  (when (minibuffer-selected-window)
    (move-beginning-of-line nil)
    (kill-line)
    (abort-minibuffers)))

;;;###autoload
(defun pin-buffer-to-window-toggle ()
  "pin buffer to window, most useful in keeping chat buffers under control"
  (interactive)
  (let* ((buf (current-buffer))
	 (window (selected-window))
	 (current-state (window-dedicated-p window))
	 (buf-name (buffer-name buf)))

    (set-window-dedicated-p window (not current-state))

    (if current-state
	(message "pinned %s to window" buf-name)
      (message "unpinned %s from window" buf-name))))

;;;###autoload
(defun buffer-line-count (&optional buf)
  "Return the number of lines in the specified buffer (name or buffer), defaulting to the current buffer."
  (car (buffer-line-statistics buf)))

;;;###autoload
(defun buffer-directory (buf)
  "Return the `default-directory' of the provide buffer."
  (when (bufferp buf)
    (with-current-buffer buf
      (let ((file-name (buffer-file-name buf)))
	(cond ((null file-name) nil)
	      ((file-directory-p file-name) file-name)
	      ((file-regular-p file-name) (file-name-directory file-name))
	      (t default-directory))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kill ring

(defvar clean-kill-ring-filters '(string-blank-p))
(defvar clean-kill-ring-prevent-duplicates t)

(defun clean-kill-ring-filter-catch-p (string)
  "T if STRING satisfies at least one of `clean-kill-ring-filters'."
  (let ((s (substring-no-properties string)))
    (and (seq-some (lambda (filter) (funcall filter s))
                   clean-kill-ring-filters)
         t)))

;;;###autoload
(defun clean-kill-ring-clean (&optional remove-dups)
  "Remove `kill-ring' members that satisfy one of`clean-kill-ring-filters'.

If REMOVE-DUPS or `clean-kill-ring-prevent-duplicates' is non-nil, or if called
interactively then remove duplicate items from the `kill-ring'."
  (interactive (list t))
  (let ((cleaned (seq-remove #'clean-kill-ring-filter-catch-p kill-ring)))
    (setq kill-ring
          (if (or remove-dups clean-kill-ring-prevent-duplicates)
              (delete-dups cleaned)
            cleaned))))

(defun ad:kill-new-reject-empty (string &optional _replace)
  "Prevent empty STRING from being added to the kill ring."
  (not (string-empty-p string)))

(advice-add 'kill-new :before-while #'ad:kill-new-reject-empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the hooks

(add-hook 'after-make-frame-functions #'frame-unset-background-for-tty)
(add-hook 'after-make-frame-functions #'hud--install-buffer-predicate)
(add-hook 'server-after-make-frame-hook #'current-frame-unset-background-for-tty)
(add-hook 'window-setup-hook #'current-frame-unset-background-for-tty)

(add-hook 'after-make-frame-functions #'frame-enable-xterm-mouse-for-tty)
(add-hook 'server-after-make-frame-hook #'current-frame-enable-xterm-mouse-for-tty)
(add-hook 'window-setup-hook #'current-frame-enable-xterm-mouse-for-tty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the mode

;;;###autoload
(define-minor-mode hud-mode
  "Global minor mode holding all of tycho's custom keybindings.
See `hud-mode-map'."
  :global t
  :group 'convenience
  :keymap hud-mode-map)

(provide 'hud-mode)
;;; hud-mode.el ends here
