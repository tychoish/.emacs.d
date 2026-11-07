;;; overmark.el --- buffer-rendering overlay and markup helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Owns buffer-rendering markup behavior: font-lock keyword generators
;; that make otherwise-invisible text (tabs, over-long lines) visible.
;; Loading this file defines functions only; it installs no hooks.

;;; Code:

(require 'seq)

(declare-function modus-themes-get-color-value "modus-themes" (color))

;;;###autoload
(defun overmark-font-lock-show-tabs ()
  "Return a font-lock style keyword for tab characters."
  '(("\t" 0 'trailing-whitespace prepend)))

;;;###autoload
(defun overmark-font-lock-width-keyword (width)
  "Return a warning-face font-lock keyword for strings beyond WIDTH."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

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
  "Highlight annotations in text and trailing whitespace in text and code."
  :global t :group 'font-lock
  (if overmark-global-mode
      (progn
        (add-hook 'text-mode-hook #'overmark--turn-on)
        (add-hook 'text-mode-hook #'overmark--set-trailing-whitespace)
        (add-hook 'prog-mode-hook #'overmark--set-trailing-whitespace)
        (add-hook 'enable-theme-functions #'overmark--setup-faces)
        (overmark--setup-faces)
        (seq-do (lambda (buffer)
                  (with-current-buffer buffer
                    (overmark--turn-on)
                    (when (derived-mode-p 'prog-mode 'text-mode)
                      (overmark--set-trailing-whitespace))))
                (buffer-list)))
    (remove-hook 'text-mode-hook #'overmark--turn-on)
    (remove-hook 'text-mode-hook #'overmark--set-trailing-whitespace)
    (remove-hook 'prog-mode-hook #'overmark--set-trailing-whitespace)
    (remove-hook 'enable-theme-functions #'overmark--setup-faces)
    (seq-do (lambda (buffer)
              (with-current-buffer buffer
                (when overmark-mode (overmark-mode -1))
                (when (derived-mode-p 'prog-mode 'text-mode)
                  (overmark--set-trailing-whitespace))))
            (buffer-list))))

(provide 'overmark)
;;; overmark.el ends here
