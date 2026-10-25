;;; minaid.el --- Unified minad completion and selection stack -*- lexical-binding: t; -*-

;; Author: Tycho Garen
;; Keywords: convenience, completion, matching, tools
;; Package-Requires: ((emacs "29.1") (vertico "1.0") (marginalia "1.0") (orderless "1.0") (corfu "1.0") (cape "1.0") (tempel "1.0") (consult "1.0") (embark "1.0") (annotated-completing-read "0.1"))

;;; Commentary:
;;
;; Consolidated configuration and unified lifecycle management for the "minad"
;; completion ecosystem:
;;   - Vertico: vertical minibuffer completion UI with multiform views
;;   - Marginalia: rich annotations for minibuffer completion candidates
;;   - Orderless: space-separated component completion style
;;   - Corfu: in-buffer popup completion overlay with history and indexing
;;   - Cape: completion-at-point function extensions and composition wrappers
;;   - Consult: practical search and navigation commands (lazily loaded)
;;   - Embark: contextual minibuffer and in-buffer actions (lazily loaded)
;;
;; Architectural principles:
;; 1. Unified lifecycle: `minaid-mode' coordinates activation, buffer hooks,
;;    and teardown across all component packages in a single switch.
;; 2. Eager UI core: Vertico, Marginalia, Orderless, Corfu, and Cape are
;;    hard-required for instant minibuffer and in-buffer completion.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)

;; Hard-required core minad dependencies
(require 'vertico)
(require 'vertico-multiform)
(require 'marginalia)
(require 'orderless)
(require 'corfu)
(require 'corfu-history)
(require 'corfu-indexed)
(require 'corfu-popupinfo)
(require 'cape)
(require 'tempel)
(require 'annotated-completing-read)

;; Forward declarations for functions from lazily loaded packages called directly in Lisp
(declare-function consult-find "consult")
(declare-function consult-ripgrep "consult")
(declare-function consult-completion-in-region "consult")
(declare-function nerd-icons-corfu-formatter "nerd-icons-corfu")
(declare-function nerd-icons-completion-marginalia-setup "nerd-icons-completion")
(declare-function eglot-completion-at-point "eglot")

(defvar embark-prompter)
(defvar embark-quit-after-action)
(defvar consult-preview-key)
(defvar consult-narrow-key)
(defvar consult-async-split-style)
(defvar consult-async-min-input)
(defvar consult-async-input-debounce)
(defvar consult-async-input-throttle)
(defvar consult-async-refresh-delay)
(defvar consult-project-function)
(defvar register-preview-delay)
(defvar xref-show-xrefs-function)
(defvar xref-show-definitions-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization

(defgroup minaid nil
  "Consolidated completion and selection suite."
  :group 'convenience
  :group 'completion
  :prefix "minaid-")

(defcustom minaid-corfu-text-auto-prefix 2
  "Number of characters before Corfu popup triggers in text modes."
  :type 'integer
  :group 'minaid)

(defcustom minaid-corfu-prog-auto-prefix 3
  "Number of characters before Corfu popup triggers in programming modes."
  :type 'integer
  :group 'minaid)

(defcustom minaid-corfu-auto-delay 0.15
  "Idle delay in seconds before Corfu popup appears."
  :type 'number
  :group 'minaid)

(defcustom minaid-vertico-count 25
  "Maximum candidate count shown in Vertico minibuffer."
  :type 'integer
  :group 'minaid)

;;;###autoload
(defvaralias 'minaid-map 'minaid-completion-map
  "Alias for `minaid-completion-map', container for completion-at-point shortcuts.")

;;;###autoload
(defvar-keymap minaid-completion-map
  :doc "Completion and template commands under C-c .")

(keymap-set minaid-completion-map "TAB" #'completion-at-point)
(keymap-set minaid-completion-map "." #'completion-at-point)
(keymap-set minaid-completion-map "p" #'completion-at-point)
(keymap-set minaid-completion-map "/" #'dabbrev-expand)
(keymap-set minaid-completion-map "c" #'dabbrev-completion)
(keymap-set minaid-completion-map "i" 'tempel-insert)
(keymap-set minaid-completion-map "s" 'tempel-complete)
(keymap-set minaid-completion-map "x" 'tempel-expand)
(keymap-set minaid-completion-map "v" 'tempel-open-custom-file)
(keymap-set minaid-completion-map "d" #'cape-dabbrev)
(keymap-set minaid-completion-map "h" #'cape-history)
(keymap-set minaid-completion-map "f" #'cape-file)
(keymap-set minaid-completion-map "n" #'cape-elisp-symbol)
(keymap-set minaid-completion-map "b" #'cape-elisp-block)
(keymap-set minaid-completion-map "a" #'cape-abbrev)
(keymap-set minaid-completion-map "l" #'cape-line)
(keymap-set minaid-completion-map "w" #'cape-dict)
(keymap-set minaid-completion-map "k" #'cape-keyword)
(keymap-set minaid-completion-map ":" #'cape-emoji)
(keymap-set minaid-completion-map "e" #'cape-emoji)
(keymap-set minaid-completion-map "m" #'cape-sgml)
(keymap-set minaid-completion-map "u" #'cape-rfc1345)
(keymap-set minaid-completion-map ";" #'corfu-at-point)
(keymap-set minaid-completion-map "\\" #'cape-tex)

;;;###autoload
(defvar-keymap minaid-consult-map
  :doc "Consult commands under C-c C-;")

(keymap-set minaid-consult-map "s" 'tempel-insert)
(keymap-set minaid-consult-map "h" 'consult-history)
(keymap-set minaid-consult-map "d" #'consult-rg-pwd)
(keymap-set minaid-consult-map "r" #'consult-rg)
(keymap-set minaid-consult-map "m" 'consult-kmacro)

;;;###autoload
(defvar-keymap minaid-consult-search-map
  :doc "Consult search commands under C-c g s")

(keymap-set minaid-consult-search-map "l" 'consult-line)
(keymap-set minaid-consult-search-map "m" 'consult-line-multi)
(keymap-set minaid-consult-search-map "k" 'consult-keep-lines)
(keymap-set minaid-consult-search-map "f" 'consult-focus-lines)

;;;###autoload
(defvaralias 'minaid-mode-map 'minaid-global-map
  "Alias for `minaid-global-map' matching conventional minor-mode keymap name.")

;;;###autoload
(defvar-keymap minaid-global-map
  :doc "Global keybindings managed by `minaid-mode'.")

(keymap-set minaid-global-map "C-c ." (cons "completion" minaid-completion-map))
(keymap-set minaid-global-map "C-c C-;" (cons "consult" minaid-consult-map))
(keymap-set minaid-global-map "M-/" #'dabbrev-completion)
(keymap-set minaid-global-map "C-M-/" #'dabbrev-expand)
(keymap-set minaid-global-map "C-<tab>" #'completion-at-point)
(keymap-set minaid-global-map "C-." 'embark-act)
(keymap-set minaid-global-map "C-c H" 'embark-bindings)
(keymap-set minaid-global-map "M-;" 'embark-dwim)
(keymap-set minaid-global-map "<remap> <Info-search>" 'consult-info)
(keymap-set minaid-global-map "C-x :" 'consult-complex-command)
(keymap-set minaid-global-map "C-x b" 'consult-buffer)
(keymap-set minaid-global-map "C-x 4 b" 'consult-buffer-other-window)
(keymap-set minaid-global-map "C-x 5 b" 'consult-buffer-other-frame)
(keymap-set minaid-global-map "C-x t b" 'consult-buffer-other-tab)
(keymap-set minaid-global-map "C-x r b" 'consult-bookmark)
(keymap-set minaid-global-map "C-x p b" 'consult-project-buffer)
(keymap-set minaid-global-map "C-x C-r" 'consult-recent-file)
(keymap-set minaid-global-map "C-c C-x r r" 'consult-register)
(keymap-set minaid-global-map "M-'" 'consult-register-store)
(keymap-set minaid-global-map "M-#" 'consult-register-load)
(keymap-set minaid-global-map "C-M-#" 'consult-register)
(keymap-set minaid-global-map "M-y" 'consult-yank-from-kill-ring)
(keymap-set minaid-global-map "C-M-I" 'consult-imenu-multi)
(keymap-set minaid-global-map "M-g e" 'consult-compile-error)
(keymap-set minaid-global-map "M-g g" 'consult-goto-line)
(keymap-set minaid-global-map "M-g M-g" 'consult-goto-line)
(keymap-set minaid-global-map "M-g m" 'consult-mark)
(keymap-set minaid-global-map "M-g k" 'consult-global-mark)
(keymap-set minaid-global-map "M-g i" 'consult-imenu)
(keymap-set minaid-global-map "M-g s" 'consult-line)
(keymap-set minaid-global-map "M-g r" #'consult-rg)
(keymap-set minaid-global-map "M-i" 'consult-imenu)
(keymap-set minaid-global-map "M-s d" #'consult-find-at-point)
(keymap-set minaid-global-map "M-s c" 'consult-locate)
(keymap-set minaid-global-map "M-s e" 'consult-isearch-history)

;; State tracking for clean mode reversals
(defvar minaid--saved-completion-styles nil
  "Saved `completion-styles' prior to enabling `minaid-mode'.")

(defvar minaid--saved-category-overrides nil
  "Saved `completion-category-overrides' prior to enabling `minaid-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cape Capf Infrastructure & Composition

(eval-and-compile
  (defmacro cape-capf-wrapper (wrapper inner)
    "Construct and define a wrapped Capf combining WRAPPER and INNER."
    (when inner
      (let* ((wrapper-sym (if (stringp wrapper) (intern wrapper) wrapper))
             (wrapper-name (symbol-name wrapper-sym))
             (capf-name (symbol-name inner))
             (symb (intern (format "%s-<%s>" capf-name wrapper-name))))
        `(defun ,symb ()
           (funcall ',wrapper-sym ',inner))))))

(defun minaid--word-capfs ()
  "Return available word completion backends wrapped for comments and strings."
  (when (and (boundp 'cape-dict-file) (file-exists-p cape-dict-file))
    (list (cape-capf-wrapper cape-capf-inside-comment cape-dict)
          (cape-capf-wrapper cape-capf-inside-string cape-dict))))

;;;###autoload
(defun minaid-prog-mode-capf-setup ()
  "Configure buffer-local `completion-at-point-functions' for programming modes."
  (setq-local completion-at-point-functions
              (thread-last
                (list #'tempel-complete
                      #'cape-file
                      #'cape-rfc1345
                      #'cape-emoji
                      #'cape-dabbrev
                      (and (boundp 'cape-dict-file) (file-exists-p cape-dict-file) #'cape-dict))
                (seq-filter #'identity))))

;;;###autoload
(defun minaid-text-mode-capf-setup ()
  "Configure buffer-local `completion-at-point-functions' for text modes."
  (setq-local completion-at-point-functions
              (thread-last
                (list #'tempel-complete
                      #'cape-rfc1345
                      #'cape-emoji
                      #'cape-file
                      #'cape-dabbrev
                      (and (boundp 'cape-dict-file) (file-exists-p cape-dict-file) #'cape-dict))
                (seq-filter #'identity))))

;;;###autoload
(defun minaid-elisp-capf-setup ()
  "Configure buffer-local `completion-at-point-functions' for Emacs Lisp mode."
  (setq-local completion-at-point-functions
              (thread-last
                (list #'cape-elisp-symbol
                      (cape-capf-wrapper cape-capf-inside-code cape-elisp-block)
                      #'tempel-complete
                      #'cape-dabbrev
                      (cape-capf-wrapper cape-capf-inside-code cape-keyword)
                      (minaid--word-capfs)
                      #'cape-emoji)
                (flatten-tree)
                (seq-filter #'identity)
                (seq-uniq))))

;;;###autoload
(defun minaid-eglot-capf-setup ()
  "Configure Capf for Eglot-managed buffers."
  (setq-local completion-category-defaults nil)
  (setq-local completion-at-point-functions
              (thread-last
                (list #'eglot-completion-at-point
                      (minaid--word-capfs)
                      #'tempel-complete
                      #'cape-dabbrev
                      #'cape-emoji
                      #'cape-file)
                (flatten-tree)
                (seq-filter #'identity)
                (seq-uniq))))

;;;###autoload
(defun setup-capfs ()
  "Configure local `completion-at-point-functions' for the current buffer."
  (interactive)
  (cond
   ((derived-mode-p 'eglot--managed-mode) (minaid-eglot-capf-setup))
   ((derived-mode-p 'emacs-lisp-mode) (minaid-elisp-capf-setup))
   ((derived-mode-p 'prog-mode) (minaid-prog-mode-capf-setup))
   ((derived-mode-p 'text-mode 'telega-chat-mode) (minaid-text-mode-capf-setup))
   ((not (buffer-file-name)) (user-error "Buffer '%s' is not visiting a file" (buffer-name)))
   (t (message "No Capf configuration for %s [%s]: %s"
               (buffer-name) major-mode completion-at-point-functions))))

;;;###autoload
(defun setup-all-capfs ()
  "Set local `completion-at-point-functions' for all open file-visiting buffers."
  (interactive)
  (thread-last (buffer-list)
               (seq-filter #'buffer-file-name)
               (seq-map (lambda (buf) (with-current-buffer buf (setup-capfs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Corfu In-Buffer Completion

;;;###autoload
(defun minaid-corfu-text-mode-setup ()
  "Enable `corfu-mode' in text buffers with appropriate prefix length."
  (corfu-mode 1)
  (setq-local corfu-auto-prefix minaid-corfu-text-auto-prefix))

;;;###autoload
(defun minaid-corfu-prog-mode-setup ()
  "Enable `corfu-mode' in programming and shell buffers."
  (corfu-mode 1)
  (setq-local corfu-auto-prefix minaid-corfu-prog-auto-prefix))

;;;###autoload
(defun corfu-at-point ()
  "Run `completion-at-point' forcing Corfu in-region completion UI."
  (interactive)
  (let ((completion-in-region-function #'corfu--in-region))
    (completion-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Consult Custom Commands & Ripgrep Helpers

;;;###autoload
(defun consult-at-point ()
  "Run `completion-at-point' forcing Consult in-region completion UI."
  (interactive)
  (let ((completion-in-region-function #'consult-completion-in-region))
    (completion-at-point)))

(defun consult-ripgrep--up-directory ()
  "Navigate one directory level up during an active `consult-ripgrep' session."
  (interactive)
  (let ((parent-dir (file-name-directory (directory-file-name default-directory))))
    (when parent-dir
      (run-at-time 0 nil
                   #'consult-ripgrep
                   parent-dir
                   (ignore-errors
                     (buffer-substring-no-properties
                      (1+ (minibuffer-prompt-end)) (point-max))))))
  (minibuffer-quit-recursive-edit))

(defvar minaid-ripgrep-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-l" #'consult-ripgrep--up-directory)
    map)
  "Keymap active in minibuffer during `consult-ripgrep'.")

(defun minaid--consult-grep-wrapper (orig-fn prompt make-builder &optional dir initial)
  "Around advice configuring prompt and keymap for Consult grep family."
  (let ((prompt (if (equal prompt "Ripgrep") "rg: " prompt)))
    (minibuffer-with-setup-hook
        (:append (lambda ()
                   (when (derived-mode-p 'minibuffer-mode)
                     (use-local-map (make-composed-keymap minaid-ripgrep-map (current-local-map))))))
      (funcall orig-fn prompt make-builder dir initial))))

;;;###autoload
(defun tempel-open-custom-file ()
  "Open the user Tempel templates file."
  (interactive)
  (find-file (expand-file-name "templates" user-emacs-directory)))

(defun minaid--project-root ()
  "Return the project root directory.
Prefers Projectile root, falling back to `project.el' and `default-directory'."
  (or (when (fboundp 'projectile-project-root)
        (projectile-project-root))
      (when-let* ((pr (and (fboundp 'project-current) (project-current))))
        (project-root pr))
      default-directory))

;;;###autoload
(defun consult-find-at-point (&optional dir initial)
  "Run `consult-find' defaulting INITIAL input to symbol/filename at point."
  (interactive)
  (let ((init (or initial
                  (thing-at-point 'existing-filename)
                  (thing-at-point 'filename)
                  "./")))
    (consult-find dir init)))

;;;###autoload
(defun consult-rg (&optional directory initial context)
  "Start an iterative Ripgrep session.
If DIRECTORY is nil, prompt for it using ACR directory picker,
falling back to the project root.  If CONTEXT is non-nil (or if called
interactively with a prefix argument), prompt for the initial query
using ACR context from point."
  (interactive)
  (let* ((dir (or directory
                  (if (fboundp 'annotated-completing-read-directory)
                      (annotated-completing-read-directory)
                    (read-directory-name "Ripgrep directory: " (minaid--project-root)))
                  (minaid--project-root)
                  default-directory))
         (query (if (or context (and (called-interactively-p 'any) current-prefix-arg))
                    (if (fboundp 'annotated-completing-read-context-from-point)
                        (annotated-completing-read-context-from-point :prompt "rg:")
                      (read-string "rg: " (thing-at-point 'symbol t)))
                  initial))
         (this-command 'consult-ripgrep))
    (consult-ripgrep dir query)))

;;;###autoload
(defun consult-rg-project (&optional initial context)
  "Start an iterative Ripgrep session in the project root, falling back as needed."
  (interactive)
  (consult-rg (or (minaid--project-root)
                  (if (fboundp 'annotated-completing-read-directory)
                      (annotated-completing-read-directory)
                    default-directory))
              initial
              (or context current-prefix-arg)))

;;;###autoload
(defun consult-rg-pwd (&optional initial context)
  "Start an iterative Ripgrep session for the current default directory."
  (interactive)
  (consult-rg (or default-directory
                  (if (fboundp 'annotated-completing-read-directory)
                      (annotated-completing-read-directory)
                    default-directory))
              initial
              (or context current-prefix-arg)))

;;;###autoload
(defun consult-rg-pwd-wizard (&optional initial)
  "Start iterative Ripgrep session for `default-directory' with context prompting."
  (interactive)
  (consult-rg-pwd initial t))

;;;###autoload
(defun consult-rg-project-wizard (&optional initial)
  "Start an iterative Ripgrep session in project root with context prompting."
  (interactive)
  (consult-rg-project initial t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration & Keymap Wiring Helpers

(defun minaid--configure-vertico ()
  "Configure Vertico variables and multiform rules."
  (setq vertico-resize t)
  (setq vertico-count minaid-vertico-count)
  (setq vertico-cycle t)
  (add-to-list 'vertico-multiform-commands '(yank (vertico-sort-function . nil) (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '(yank-from-kill-ring (vertico-sort-function . nil) (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '(consult-yank-from-kill-ring (vertico-sort-function . nil) (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '(consult-yank-pop (vertico-sort-function . nil) (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '("\\`execute-extended-command" (vertico-flat-annotate . t) (marginalia-annotators (command marginalia-annotate-command marginalia-annotate-binding))))
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(defun minaid--configure-orderless ()
  "Configure Orderless completion styles and category overrides."
  (setq minaid--saved-completion-styles (or minaid--saved-completion-styles completion-styles 'empty))
  (setq minaid--saved-category-overrides (or minaid--saved-category-overrides completion-category-overrides 'empty))
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles orderless basic partial-completion))
                                        (consult-grep (styles basic))
                                        (buffer (styles orderless basic))
                                        (command (styles orderless basic))
                                        (symbol (styles orderless basic))))
  (setq completion-preview-sort-function nil)
  (setq orderless-matching-styles '(orderless-literal orderless-prefixes orderless-initialism orderless-regexp)))

(defun minaid--configure-corfu ()
  "Configure Corfu variables and sub-modes."
  (setq corfu-cycle t)
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match t)
  (setq corfu-preview-current t)
  (setq corfu-preselect nil)
  (setq corfu-on-exact-match nil)
  (setq corfu-auto t)
  (setq corfu-auto-delay minaid-corfu-auto-delay)
  (setq corfu-auto-prefix minaid-corfu-prog-auto-prefix)
  (setq corfu-popupinfo-delay 0.125)
  (setq corfu-indexed-start 1)
  (setq global-corfu-minibuffer nil)
  (add-hook 'corfu-mode-hook #'corfu-history-mode)
  (add-hook 'corfu-mode-hook #'corfu-indexed-mode)
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
  (when (fboundp 'nerd-icons-corfu-formatter)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
  (keymap-set corfu-map "C-<tab>" #'corfu-quick-complete)
  (keymap-set corfu-map "<tab>" #'corfu-complete)
  (keymap-set corfu-map "<return>" #'corfu-insert)
  (keymap-set corfu-map "C-<return>" #'corfu-quick-insert)
  (keymap-set corfu-map "M-SPC" #'corfu-insert-separator)
  (keymap-set corfu-map "C-SPC" #'corfu-insert-separator)
  (keymap-set corfu-map "<backtab>" #'corfu-insert-separator)
  (keymap-set corfu-map "C-j" #'corfu-quick-jump)
  (keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle))

(defun minaid--configure-embark ()
  "Configure Embark prompter and buffer display rules."
  (setq embark-prompter 'embark-keymap-prompter)
  (setq embark-quit-after-action '((kill-buffer . nil)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(defun minaid--configure-consult ()
  "Configure Consult variables, xref hooks, and deferred customizations."
  (setq register-preview-delay 0.1)
  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref)
  (setq consult-narrow-key "C-+")
  (setq consult-async-split-style 'semicolon)
  (setq consult-async-min-input 2)
  (setq consult-async-input-debounce 0.05)
  (setq consult-async-input-throttle 0.125)
  (setq consult-async-refresh-delay 0.05)
  (setq consult-project-function #'minaid--project-root)
  (setq consult-preview-key 'any)
  (advice-add 'register-preview :override 'consult-register-window)
  (advice-add 'consult--grep :around #'minaid--consult-grep-wrapper))

(defun minaid--install-external-keybindings ()
  "Mount sub-maps and hooks into HUD maps, minibuffer, and isearch."
  (when (boundp 'hud-docs-map)
    (keymap-set hud-docs-map "i" 'consult-info)
    (keymap-set hud-docs-map "m" 'consult-man)
    (keymap-set hud-docs-map "a" 'consult-eglot-symbols))

  (when (boundp 'hud-ecclectic-grep-map)
    (keymap-set hud-ecclectic-grep-map "s" (cons "consult-search" minaid-consult-search-map))
    (keymap-set hud-ecclectic-grep-map "f" 'consult-grep))

  (when (boundp 'hud-ecclectic-grep-project-map)
    (keymap-set hud-ecclectic-grep-project-map "g" 'consult-git-grep)
    (keymap-set hud-ecclectic-grep-project-map "r" #'consult-rg-project))

  (when (boundp 'hud-ecclectic-rg-map)
    (keymap-set hud-ecclectic-rg-map "g" #'consult-rg)
    (keymap-set hud-ecclectic-rg-map "s" #'consult-rg-pwd)
    (keymap-set hud-ecclectic-rg-map "l" #'consult-rg-pwd-wizard)
    (keymap-set hud-ecclectic-rg-map "r" #'consult-rg-project)
    (keymap-set hud-ecclectic-rg-map "p" #'consult-rg-project-wizard))

  ;; Minibuffer and Isearch integration
  (when (boundp 'minibuffer-local-map)
    (keymap-set minibuffer-local-map "M-s" 'consult-history)
    (keymap-set minibuffer-local-map "M-r" 'consult-history)
    (keymap-set minibuffer-local-map "C-c a" #'marginalia-cycle))

  (when (boundp 'isearch-mode-map)
    (keymap-set isearch-mode-map "M-e" 'consult-isearch-history)
    (keymap-set isearch-mode-map "M-s e" 'consult-isearch-history)
    (keymap-set isearch-mode-map "M-s l" 'consult-line)
    (keymap-set isearch-mode-map "M-s L" 'consult-line-multi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode Lifecycle (Enablement / Teardown)

(defun minaid--enable ()
  "Perform enablement of all minad completion components."
  (minaid--configure-vertico)
  (minaid--configure-orderless)
  (minaid--configure-corfu)
  (minaid--configure-embark)
  (minaid--configure-consult)
  (minaid--install-external-keybindings)

  ;; Activate Vertico, Marginalia, & Corfu
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (marginalia-mode 1)
  (global-corfu-mode 1)

  ;; Cape wrap buster for Eglot
  (when (fboundp 'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  ;; Global default Capfs
  (add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions
            (cape-capf-wrapper cape-capf-inside-code cape-keyword))

  ;; Capf major mode hooks
  (add-hook 'eglot-managed-mode-hook #'minaid-eglot-capf-setup)
  (add-hook 'emacs-lisp-mode-hook #'minaid-elisp-capf-setup)
  (add-hook 'telega-chat-mode-hook #'minaid-text-mode-capf-setup)
  (add-hook 'text-mode-hook #'minaid-text-mode-capf-setup)
  (add-hook 'prog-mode-hook #'minaid-prog-mode-capf-setup)

  ;; Corfu buffer hooks (add-hook works even before modes load)
  (add-hook 'text-mode-hook #'minaid-corfu-text-mode-setup)
  (add-hook 'prog-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'comint-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'eat-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'eshell-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'shell-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'agent-shell-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'agent-shell-queue-capture-mode-hook #'minaid-corfu-text-mode-setup)

  ;; Marginalia command category overrides & nerd icons
  (add-to-list 'marginalia-command-categories '(consult-completion-in-region . imenu))
  (when (fboundp 'nerd-icons-completion-marginalia-setup)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

(defun minaid--disable ()
  "Tear down all minad completion components and restore original state."

  ;; Remove Consult advice
  (advice-remove 'register-preview 'consult-register-window)
  (advice-remove 'consult--grep #'minaid--consult-grep-wrapper)

  ;; Deactivate Vertico, Marginalia, & Corfu
  (vertico-mode -1)
  (vertico-multiform-mode -1)
  (marginalia-mode -1)
  (global-corfu-mode -1)

  ;; Restore completion styles
  (when minaid--saved-completion-styles
    (setq completion-styles (if (eq minaid--saved-completion-styles 'empty) nil minaid--saved-completion-styles))
    (setq minaid--saved-completion-styles nil))
  (when minaid--saved-category-overrides
    (setq completion-category-overrides (if (eq minaid--saved-category-overrides 'empty) nil minaid--saved-category-overrides))
    (setq minaid--saved-category-overrides nil))

  ;; Remove Capf hooks
  (advice-remove 'eglot-completion-at-point #'cape-wrap-buster)
  (remove-hook 'completion-at-point-functions #'cape-rfc1345)
  (remove-hook 'completion-at-point-functions #'cape-emoji)
  (remove-hook 'completion-at-point-functions #'cape-file)
  (remove-hook 'completion-at-point-functions #'cape-history)
  (remove-hook 'eglot-managed-mode-hook #'minaid-eglot-capf-setup)
  (remove-hook 'emacs-lisp-mode-hook #'minaid-elisp-capf-setup)
  (remove-hook 'telega-chat-mode-hook #'minaid-text-mode-capf-setup)
  (remove-hook 'text-mode-hook #'minaid-text-mode-capf-setup)
  (remove-hook 'prog-mode-hook #'minaid-prog-mode-capf-setup)

  ;; Remove Corfu hooks
  (remove-hook 'text-mode-hook #'minaid-corfu-text-mode-setup)
  (remove-hook 'prog-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'comint-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'eat-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'eshell-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'shell-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'agent-shell-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'agent-shell-queue-capture-mode-hook #'minaid-corfu-text-mode-setup)
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (bound-and-true-p corfu-mode)
          (corfu-mode -1))))))

;;;###autoload
(define-minor-mode minaid-mode
  "Global minor mode coordinating the consolidated Minad completion suite.
Activates and manages Vertico, Marginalia, Orderless, Corfu, Cape, Consult,
and Embark integrations."
  :global t
  :keymap minaid-global-map
  :group 'minaid
  :lighter " minaid"
  (if minaid-mode
      (minaid--enable)
    (minaid--disable)))

(provide 'minaid)
;;; minaid.el ends here

