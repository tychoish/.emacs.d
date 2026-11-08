;;; overmark.el --- buffer-rendering overlay and markup helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Owns buffer-rendering markup behavior: font-lock keyword generators
;; that make otherwise-invisible text (tabs, over-long lines) visible.
;; Loading this file defines functions only; it installs no hooks.

;;; Code:

(require 'seq)

(declare-function modus-themes-get-color-value "modus-themes" (color))

;;;###autoload
(defcustom overmark-max-width 100
  "Maximum allowed line width before highlighting overflow.
When nil or <= 0, line overflow highlighting is disabled in the buffer."
  :type '(choice (integer :tag "Maximum width")
                 (const :tag "Disabled" nil))
  :group 'font-lock)

(make-variable-buffer-local 'overmark-max-width)

;;;###autoload
(defcustom overmark-line-width-enabled t
  "Whether line-width guides are enabled by default in `overmark-global-mode'."
  :type 'boolean
  :group 'font-lock)

;;;###autoload
(defun overmark-font-lock-show-tabs ()
  "Return a font-lock style keyword for tab characters."
  '(("\t" 0 'trailing-whitespace prepend)))

;;;###autoload
(defun overmark-font-lock-width-keyword (width)
  "Return a warning-face font-lock keyword for strings beyond WIDTH."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(defun overmark--match-line-overflow (limit)
  "Match text exceeding `overmark-max-width' on lines up to LIMIT."
  (when (and overmark-max-width (> overmark-max-width 0))
    (let ((found nil))
      (while (and (not found) (< (point) limit))
        (let ((eol (line-end-position)))
          (move-to-column overmark-max-width)
          (if (< (point) eol)
              (progn
                (set-match-data (list (point) eol (point) eol))
                (goto-char (min (1+ eol) (point-max)))
                (setq found t))
            (goto-char (min (1+ eol) (point-max))))))
      found)))

(defconst overmark--width-font-lock-keywords
  '((overmark--match-line-overflow (1 font-lock-warning-face t)))
  "Font-lock keywords for dynamic line-width overflow highlighting.")

;;;###autoload
(define-minor-mode overmark-width-mode
  "Highlight line width overflow beyond `overmark-max-width' in this buffer."
  :lighter ""
  (if overmark-width-mode
      (font-lock-add-keywords nil overmark--width-font-lock-keywords 'append)
    (font-lock-remove-keywords nil overmark--width-font-lock-keywords))
  (when font-lock-mode
    (font-lock-flush)))

;;;###autoload
(defun overmark-toggle-width (&optional arg)
  "Toggle line-length guides in the current buffer.
With prefix ARG, toggle line-length guides globally across all buffers."
  (interactive "P")
  (if arg
      (progn
        (setq overmark-line-width-enabled (not overmark-line-width-enabled))
        (seq-do (lambda (buf)
                  (with-current-buffer buf
                    (when (derived-mode-p 'prog-mode 'text-mode)
                      (overmark-width-mode (if overmark-line-width-enabled 1 -1)))))
                (buffer-list))
        (message "Overmark line-width guides %s globally"
                 (if overmark-line-width-enabled "enabled" "disabled")))
    (overmark-width-mode (if overmark-width-mode -1 1))
    (message "Overmark line-width guides %s in buffer"
             (if overmark-width-mode "enabled" "disabled"))))

;;;###autoload
(defun overmark-set-width (arg)
  "Set `overmark-max-width' to ARG in the current buffer.
With a numeric prefix ARG (\\[universal-argument] N), set width to N.
With a bare prefix ARG (\\[universal-argument]), use the current column.
Without an ARG, prompt for a number in the minibuffer.
A value of 0 or negative disables overflow highlighting in this buffer."
  (interactive "P")
  (let ((col (cond ((numberp arg) (prefix-numeric-value arg))
                   ((consp arg) (current-column))
                   (t (read-number "Set overmark line width: "
                                   (or overmark-max-width 100))))))
    (if (> col 0)
        (progn
          (setq-local overmark-max-width col)
          (overmark-width-mode 1)
          (message "overmark-max-width set to %d" overmark-max-width))
      (setq-local overmark-max-width nil)
      (overmark-width-mode -1)
      (message "overmark line-width highlighting disabled"))))

(defface overmark-delimiter-face
  '((((class color) (background light)) :foreground "#0031a9" :weight bold)
    (((class color) (background dark))  :foreground "#2fafff" :weight bold)
    (t :weight bold))
  "Face for the \='TK(\=' and \=')\=' delimiters in TK(\"...\") annotations."
  :group 'font-lock-faces)

(defface overmark-content-face
  '((((class color) (background light)) :foreground "#595959" :weight bold)
    (((class color) (background dark))  :foreground "#989898" :weight bold)
    (t :weight bold))
  "Face for the explanation text in TK(\"...\") annotations."
  :group 'font-lock-faces)

(defconst overmark--font-lock-keywords
  '(("\\b\\(\\(?:TK\\|TODO\\)(\\)\\(\"[^\"]*\"\\|[^)]*\\)\\()\\)"
     (1 'overmark-delimiter-face t)
     (2 'overmark-content-face t)
     (3 'overmark-delimiter-face t)))
  "Font-lock keywords for TK and TODO annotations.")

(defun overmark--turn-on ()
  "Enable `overmark-mode' in text buffers."
  (when (derived-mode-p 'text-mode)
    (overmark-mode 1)))

(defun overmark--set-trailing-whitespace ()
  "Apply the global mode's trailing whitespace setting to this buffer."
  (setq-local show-trailing-whitespace overmark-global-mode))

(defun overmark--turn-on-width ()
  "Enable `overmark-width-mode' in code and text buffers when enabled."
  (when (and overmark-line-width-enabled
             (derived-mode-p 'prog-mode 'text-mode))
    (overmark-width-mode 1)))

(defun overmark--setup-faces (&rest _)
  "Set annotation faces from the current Modus theme."
  (when (featurep 'modus-themes)
    (let ((delimiter (modus-themes-get-color-value 'blue))
          (content (modus-themes-get-color-value 'fg-dim)))
      (when (stringp delimiter)
        (set-face-attribute 'overmark-delimiter-face nil
                            :foreground delimiter :weight 'bold))
      (when (stringp content)
        (set-face-attribute 'overmark-content-face nil
                            :foreground content :weight 'bold)))))

;;;###autoload
(define-minor-mode overmark-mode
  "Highlight TK(...) and TODO(...) annotations in this buffer."
  :lighter " Overmark"
  (if overmark-mode
      (font-lock-add-keywords nil overmark--font-lock-keywords 'append)
    (font-lock-remove-keywords nil overmark--font-lock-keywords))
  (font-lock-flush))

;;;###autoload
(define-minor-mode overmark-global-mode
  "Highlight annotations in text, line width overflow, and trailing whitespace."
  :global t :group 'font-lock
  (if overmark-global-mode
      (progn
        (add-hook 'text-mode-hook #'overmark--turn-on)
        (add-hook 'text-mode-hook #'overmark--set-trailing-whitespace)
        (add-hook 'prog-mode-hook #'overmark--set-trailing-whitespace)
        (add-hook 'prog-mode-hook #'overmark--turn-on-width)
        (add-hook 'text-mode-hook #'overmark--turn-on-width)
        (add-hook 'enable-theme-functions #'overmark--setup-faces)
        (overmark--setup-faces)
        (seq-do (lambda (buffer)
                  (with-current-buffer buffer
                    (overmark--turn-on)
                    (when (derived-mode-p 'prog-mode 'text-mode)
                      (overmark--set-trailing-whitespace)
                      (overmark--turn-on-width))))
                (buffer-list)))
    (remove-hook 'text-mode-hook #'overmark--turn-on)
    (remove-hook 'text-mode-hook #'overmark--set-trailing-whitespace)
    (remove-hook 'prog-mode-hook #'overmark--set-trailing-whitespace)
    (remove-hook 'prog-mode-hook #'overmark--turn-on-width)
    (remove-hook 'text-mode-hook #'overmark--turn-on-width)
    (remove-hook 'enable-theme-functions #'overmark--setup-faces)
    (seq-do (lambda (buffer)
              (with-current-buffer buffer
                (when overmark-mode (overmark-mode -1))
                (when overmark-width-mode (overmark-width-mode -1))
                (when (derived-mode-p 'prog-mode 'text-mode)
                  (overmark--set-trailing-whitespace))))
            (buffer-list))))

(provide 'overmark)
;;; overmark.el ends here
