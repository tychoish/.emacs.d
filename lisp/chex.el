;;; chex.el --- Checking and spelling integration -*- lexical-binding: t; -*-

;; Author: Tycho Garen
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (flycheck "32") (flycheck-aspell "0.3") (flycheck-eglot "0.1") (flycheck-vale "0.1") (flyspell-correct "0.6") (consult-flycheck "0.1") (consult-flyspell "0.1"))
;; Keywords: convenience, tools, languages

;;; Commentary:
;; `chex-global-mode' configures spelling, syntax checking, Eldoc, Vale,
;; Eglot, and checking commands.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 'flycheck)
(require 'flyspell)

(require 'flycheck-aspell)

(declare-function flyspell-correct-at-point "flyspell-correct")
(declare-function consult-flyspell "consult-flyspell")
(declare-function sprite-state-path "sprite")
(declare-function flycheck-vale-setup "flycheck-vale")

(defgroup chex nil "Checking and spelling integration." :group 'tools)

(defvar chex-global-mode nil)

(defvar consult-flyspell-select-function)
(defvar ispell-extra-args)
(defvar ispell-last-program-name)
(defvar ispell-list-command)
(defvar ispell-program-name)

(defcustom chex-ispell-cache-file
  (if (fboundp 'sprite-state-path)
      (sprite-state-path "ispell-aspell-cache.el")
    (expand-file-name "ispell-aspell-cache.el" user-emacs-directory))
  "File containing cached Aspell dictionary detection results."
  :type 'file :group 'chex)

(defconst chex--ispell-cache-vars
  '(ispell-last-program-name ispell-library-directory ispell-really-aspell
    ispell-really-hunspell ispell-really-enchant ispell-encoding8-command
    ispell-dictionary-alist ispell-aspell-dictionary-alist
    ispell--aspell-found-dictionaries ispell-aspell-dict-dir ispell-aspell-data-dir))

(defun chex--ispell-cache-params (orig-fn &rest args)
  "Call ORIG-FN with ARGS using cached Aspell detection results."
  (when (file-exists-p chex-ispell-cache-file)
    (load chex-ispell-cache-file nil t))
  (let ((already-cached (equal ispell-last-program-name ispell-program-name)))
    (prog1 (apply orig-fn args)
      (unless already-cached
        (with-temp-file chex-ispell-cache-file
          (insert ";; -*- lexical-binding: t; -*-\n")
          (seq-do (lambda (var) (insert (format "(setq %s '%S)\n" var (symbol-value var))))
                  chex--ispell-cache-vars))))))

(defun chex-ispell-clear-cache ()
  "Delete the cached Aspell dictionary detection result."
  (interactive)
  (when (file-exists-p chex-ispell-cache-file)
    (delete-file chex-ispell-cache-file))
  (setq ispell-last-program-name nil))

(defvar-local chex--flyspell-timer nil
  "Pending idle timer for spelling in this buffer.")

(defun chex--flyspell-run (buffer function)
  "Enable spelling FUNCTION in BUFFER if the buffer still exists."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq chex--flyspell-timer nil)
      (if (eq function #'flyspell-prog-mode)
          (funcall function)
        (funcall function 1)))))

(defun chex--flyspell-schedule (function)
  "Schedule spelling FUNCTION in the current buffer."
  (when (timerp chex--flyspell-timer)
    (cancel-timer chex--flyspell-timer))
  (setq chex--flyspell-timer
        (run-with-idle-timer 0.2 nil #'chex--flyspell-run (current-buffer) function)))

(defun chex--flyspell-text ()
  "Schedule spelling for the current text buffer."
  (chex--flyspell-schedule #'flyspell-mode))

(defun chex--flyspell-prog ()
  "Schedule spelling for the current programming buffer."
  (chex--flyspell-schedule #'flyspell-prog-mode))

(defun chex-consult-flyspell-round-trip ()
  "Correct a word and return to the spelling list."
  (flyspell-correct-at-point)
  (consult-flyspell))

(defun chex-flycheck-eldoc (callback &rest _ignored)
  "Pass Flycheck diagnostics at point to CALLBACK."
  (when-let* ((errors (flycheck-overlay-errors-at (point))))
    (seq-do (lambda (err)
              (funcall callback
                       (format "%s: %s"
                               (let ((level (flycheck-error-level err)))
                                 (cond ((eq level 'info) (propertize "I" 'face 'flycheck-error-list-info))
                                       ((eq level 'error) (propertize "E" 'face 'flycheck-error-list-error))
                                       ((eq level 'warning) (propertize "W" 'face 'flycheck-error-list-warning))
                                       (t level)))
                               (flycheck-error-message err))
                       :thing (or (flycheck-error-id err) (flycheck-error-group err))
                       :face 'font-lock-doc-face))
            errors)))

(defvar-local chex--saved-local-settings nil
  "Local bindings saved before Chex configured this buffer.")

(defvar-local chex--added-eldoc-hook nil
  "Non-nil when Chex added its Eldoc function in this buffer.")

(defun chex--set-local (symbol value)
  "Save SYMBOL's local binding and set it to VALUE in this buffer."
  (unless (assq symbol chex--saved-local-settings)
    (push (cons symbol (if (local-variable-p symbol)
                           (cons t (symbol-value symbol))
                         (cons nil nil)))
          chex--saved-local-settings))
  (set (make-local-variable symbol) value))

(defun chex--restore-local-settings ()
  "Restore local bindings and remove the Eldoc function installed by Chex."
  (seq-do (lambda (entry)
            (if (cadr entry)
                (set (make-local-variable (car entry)) (cddr entry))
              (kill-local-variable (car entry))))
          chex--saved-local-settings)
  (setq chex--saved-local-settings nil)
  (when chex--added-eldoc-hook
    (remove-hook 'eldoc-documentation-functions #'chex-flycheck-eldoc t)
    (setq chex--added-eldoc-hook nil)))

(defun chex--flycheck-eldoc-setup ()
  "Show Flycheck diagnostics through Eldoc in the current buffer."
  (unless (memq #'chex-flycheck-eldoc eldoc-documentation-functions)
    (add-hook 'eldoc-documentation-functions #'chex-flycheck-eldoc nil t)
    (setq chex--added-eldoc-hook t))
  (chex--set-local 'eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (chex--set-local 'flycheck-display-errors-function nil)
  (chex--set-local 'flycheck-help-echo nil))

(defconst chex-vale-checkers
  '(mail-aspell-dynamic markdown-aspell-dynamic org-aspell-dynamic rst-aspell-dynamic)
  "Aspell checkers followed by Vale when enabled.")

(defconst chex-aspell-checkers
  '(c-aspell-dynamic org-aspell-dynamic rst-aspell-dynamic html-aspell-dynamic
    markdown-aspell-dynamic mail-aspell-dynamic)
  "Aspell checkers registered with Flycheck.")

(defun chex-flycheck-vale-enable ()
  "Enable Vale and chain it after the Aspell checkers."
  (interactive)
  (flycheck-vale-setup)
  (setq-default flycheck-vale-enabled t)
  (seq-do (lambda (checker) (flycheck-add-next-checker checker 'vale))
          chex-vale-checkers))
(defun chex-flycheck-vale-disable ()
  "Disable Vale and remove its Aspell checker chains."
  (interactive)
  (setq flycheck-checkers (remove 'vale flycheck-checkers))
  (setq-default flycheck-vale-enabled nil)
  (seq-do (lambda (checker) (flycheck-remove-next-checker checker 'vale))
          chex-vale-checkers))

(defun chex--go-setup ()
  "Disable redundant Go checkers in the current buffer."
  (chex--set-local 'flycheck-disabled-checkers
                   '(go-unconvert go-errcheck go-staticcheck go-vet go-build go-test go-gofmt golangci-lint)))

(defvar-keymap chex-global-mode-map
  :doc "Keys enabled by `chex-global-mode'."
  "C-c [" #'flyspell-correct-next
  "C-c ]" #'flyspell-correct-previous
  "M-$" #'flyspell-correct-at-point
  "C-;" #'flyspell-correct-previous
  "M-g f" #'flycheck-mode
  "C-c f f" #'flycheck-mode)

(flycheck-aspell-define-checker "org" "Org" ("--add-filter" "url") (org-mode))
(flycheck-aspell-define-checker "rst" "reStructuredText" ("--add-filter" "url") (rst-mode))

(defun chex--eglot-setup ()
  "Add the Eglot checker and chain gofmt after it."
  (add-to-list 'flycheck-checkers 'eglot-check)
  (flycheck-add-next-checker 'eglot-check 'go-gofmt))

(defun chex--rustic-setup ()
  "Register Rustic's Clippy checker."
  (add-to-list 'flycheck-checkers 'rustic-clippy))

(defvar chex-global--managed-settings
  `((ispell-list-command . "list")
    (ispell-program-name . "aspell")
    (ispell-extra-args . ("--sug-mode=ultra" "--run-together"))
    (consult-flyspell-select-function . chex-consult-flyspell-round-trip)
    (flyspell-issue-message-flag . nil)
    (flyspell-sort-corrections . nil)
    (flyspell-dictionaries . ("english"))
    (flyspell-guess-size . 76)
    (flyspell-guess-slots . 3)
    (flyspell-focus . 50)
    (flyspell-timeout . 3)
    (flyspell-polling-timeout . 10)
    (flyspell-delay . 2)
    (flyspell-indicator-format . "[%s]--")
    (flyspell-min-buffer-size . 228)
    (flyspell-guess-indicator . nil)
    (flycheck-indication-mode . nil)
    (flycheck-check-syntax-automatically . (save new-line idle-change idle-buffer-switch))
    (flycheck-idle-change-delay . 1)
    (flycheck-idle-buffer-switch-delay . 1)
    (flycheck-checker-error-threshold . nil)
    (flycheck-display-errors-delay . 0.5)
    (flycheck-go-vet-shadow . t)
    (flycheck-go-build-install-deps . nil)
    (flycheck-flake8-maximum-line-length . 100)
    (flycheck-eglot-exclusive . nil)
    (flycheck-eglot-enable-diagnostic-tags . nil)
    (flycheck-keymap-prefix . ,(kbd "C-c f")))
  "Default values installed while `chex-global-mode' is active.")

(defvar chex-global--saved-settings nil
  "Previous default bindings saved by `chex-global-mode'.")

(defun chex-global--apply-settings ()
  "Save and apply the defaults in `chex-global--managed-settings'."
  (unless chex-global--saved-settings
    (seq-do (lambda (pair)
              (let ((symbol (car pair)))
                (push (cons symbol (if (default-boundp symbol)
                                       (cons t (default-value symbol))
                                     (cons nil nil)))
                      chex-global--saved-settings)
                (set-default symbol (cdr pair))))
            chex-global--managed-settings)))

(defun chex-global--restore-settings ()
  "Restore default bindings saved when Chex was enabled."
  (seq-do (lambda (entry)
            (if (cadr entry)
                (set-default (car entry) (cddr entry))
              (makunbound (car entry))))
          chex-global--saved-settings)
  (setq chex-global--saved-settings nil))

;;;###autoload
(define-minor-mode chex-global-mode
  "Enable checking and spelling configuration globally."
  :global t :group 'chex :keymap chex-global-mode-map
  (if chex-global-mode
      (progn
        (chex-global--apply-settings)
        (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
        (keymap-set flycheck-command-map ";" #'consult-flycheck)
        (keymap-set flycheck-command-map "m" #'consult-flycheck)
        (seq-do (lambda (checker) (add-to-list 'flycheck-checkers checker))
                chex-aspell-checkers)
        (add-hook 'prog-mode-hook #'chex--flyspell-prog)
        (add-hook 'text-mode-hook #'chex--flyspell-text)
        (add-hook 'telega-chat-mode-hook #'chex--flyspell-text)
        (add-hook 'go-ts-mode-hook #'chex--go-setup)
        (add-hook 'flycheck-mode-hook #'chex--flycheck-eldoc-setup)
        (add-hook 'flycheck-eglot-mode-hook #'chex--eglot-setup)
        (add-hook 'rustic-mode-hook #'chex--rustic-setup)
        (add-hook 'org-mode-hook #'flycheck-vale-setup)
        (add-hook 'markdown-mode-hook #'flycheck-vale-setup)
        (advice-add 'ispell-set-spellchecker-params :around #'chex--ispell-cache-params))
    (remove-hook 'prog-mode-hook #'chex--flyspell-prog)
    (remove-hook 'text-mode-hook #'chex--flyspell-text)
    (remove-hook 'telega-chat-mode-hook #'chex--flyspell-text)
    (remove-hook 'go-ts-mode-hook #'chex--go-setup)
    (remove-hook 'flycheck-mode-hook #'chex--flycheck-eldoc-setup)
    (remove-hook 'flycheck-eglot-mode-hook #'chex--eglot-setup)
    (remove-hook 'rustic-mode-hook #'chex--rustic-setup)
    (remove-hook 'org-mode-hook #'flycheck-vale-setup)
    (remove-hook 'markdown-mode-hook #'flycheck-vale-setup)
    (seq-do (lambda (buffer)
              (with-current-buffer buffer
                (when (timerp chex--flyspell-timer)
                  (cancel-timer chex--flyspell-timer)
                  (setq chex--flyspell-timer nil))
                (chex--restore-local-settings)))
            (buffer-list))
    (advice-remove 'ispell-set-spellchecker-params #'chex--ispell-cache-params)
    (chex-global--restore-settings)))

(provide 'chex)
;;; chex.el ends here
