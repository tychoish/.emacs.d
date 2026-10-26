;;; grepx.el --- Unified grep and search module -*- lexical-binding: t; -*-

;; Author: Tychoish <sam@tychoish.com>
;; Keywords: matching, tools
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1") (deadgrep "0.5") (consult "1.0") (annotated-completing-read "0.1"))

;;; Commentary:

;; Grepx consolidates all grep, ripgrep, and multi-file search tooling
;; across the configuration into a cohesive interface.
;;
;; 1. Unified Persistent Search: Combines interactive search buffers with
;;    a compact 1-line header and direct match editing via `w' (edit mode).
;; 2. Window-Safe Navigation: Advises next-error so successive calls to
;;    `next-error' (M-g n) and `previous-error' (M-g p) update the target file
;;    in the dedicated source window without replacing the results buffer.
;; 3. Density Controls: Compact 1-line header toggled interactively with
;;    the `?' key.
;; 4. Live Search: Consult-driven iterative search with directory navigation
;;    (C-l) and bridging to persistent buffers (M-d).
;; 5. Keymap Ownership: Owns `grepx-map', `grepx-rg-map', `grepx-project-map',
;;    and `grepx-search-map'.
;; 6. Interactive Dispatch Menu: Provides `grepx-menu' (C-c g m) via
;;    `annotated-completing-read'.
;; 7. Pluggable Backend Registry: Decoupled providers for persistent and live
;;    search with graceful degradation (deadgrep, compile-rg, grep, occur;
;;    consult-ripgrep, acr-wizard).

;;; Code:

(require 'cl-lib)
(require 'deadgrep)
(require 'consult)
(require 'annotated-completing-read)
(require 'isearch)

(defgroup grepx nil
  "Consolidated grep and search tools."
  :group 'matching
  :group 'tools
  :prefix "grepx-")

(defcustom grepx-compact-header t
  "When non-nil, render a compact 1-line header in search results buffers."
  :type 'boolean
  :group 'grepx)

(defcustom grepx-ripgrep-config-path (expand-file-name "~/.ripgreprc")
  "Path to the ripgrep configuration file."
  :type 'string
  :group 'grepx)

(defcustom grepx-preferred-persistent-backend nil
  "Explicit preference for persistent backend name, overriding priority."
  :type '(choice (const :tag "Default (Priority)" nil)
                 (symbol :tag "Backend Name"))
  :group 'grepx)

(defcustom grepx-preferred-live-backend nil
  "Explicit preference for live backend name, overriding priority."
  :type '(choice (const :tag "Default (Priority)" nil)
                 (symbol :tag "Backend Name"))
  :group 'grepx)

(defcustom grepx-ag-args "ag --vimgrep --smart-case --numbers"
  "Command line arguments for ag live search."
  :type 'string
  :group 'grepx)

(defcustom grepx-ugrep-args "ugrep --vimgrep --smart-case -k -j -I"
  "Command line arguments for ugrep live search."
  :type 'string
  :group 'grepx)

(defvar grepx-regexp-history nil
  "History list for search regular expressions.")

(defvar grepx-menu-history nil
  "History list for `grepx-menu' selections.")

(defvar deadgrep--search-type)

(defvar grepx-backends nil
  "List of registered Grepx backends.
Each entry is a plist with keys:
:name (symbol), :type (:persistent or :live), :priority (integer),
:available-p (function returning boolean),
:run-fn (function taking dir and query), and :description (string).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backend Registry API

(cl-defun grepx-register-backend (name &key type priority available-p run-fn description)
  "Register backend NAME with search attributes.
TYPE is :persistent or :live.
PRIORITY is an integer determining selection order.
AVAILABLE-P is a nullary function returning non-nil when backend can run.
RUN-FN is a binary function taking (dir query).
DESCRIPTION is a human-readable string."
  (let ((entry (list :name name
                     :type type
                     :priority (or priority 50)
                     :available-p (or available-p (lambda () t))
                     :run-fn run-fn
                     :description description)))
    (setq grepx-backends (cons entry (seq-remove (lambda (b) (eq (plist-get b :name) name))
                                                 grepx-backends)))))

(defun grepx-find-backend (type &optional name)
  "Find backend of TYPE (:persistent or :live).
If NAME is provided, find backend with that name.
If preferred backend is configured for TYPE, return that backend if found.
Otherwise, return the highest-priority available backend of TYPE."
  (if name
      (seq-find (lambda (b) (and (eq (plist-get b :type) type)
                                 (eq (plist-get b :name) name)))
                grepx-backends)
    (let ((pref (if (eq type :persistent)
                    grepx-preferred-persistent-backend
                  grepx-preferred-live-backend)))
      (or (when pref
            (seq-find (lambda (b) (and (eq (plist-get b :type) type)
                                       (eq (plist-get b :name) pref)))
                      grepx-backends))
          (let ((candidates (seq-filter (lambda (b)
                                          (and (eq (plist-get b :type) type)
                                               (funcall (plist-get b :available-p))))
                                        grepx-backends)))
            (car (seq-sort-by (lambda (b) (plist-get b :priority)) #'> candidates)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymaps Owned by Grepx

;;;###autoload
(defvar-keymap grepx-rg-map
  :name "+ripgrep"
  :doc "Ripgrep commands under C-c g r.")

;;;###autoload
(defvar-keymap grepx-project-map
  :name "project-grep"
  :doc "Project search commands under C-c g p.")

;;;###autoload
(defvar-keymap grepx-search-map
  :name "search"
  :doc "In-buffer and multi-buffer search commands under C-c g s.")

;;;###autoload
(defvar-keymap grepx-map
  :name "grep"
  :doc "Root search commands under C-c g.")

;; Wire grepx-search-map
(keymap-set grepx-search-map "l" #'consult-line)
(keymap-set grepx-search-map "m" #'consult-line-multi)
(keymap-set grepx-search-map "k" #'consult-keep-lines)

;; Wire grepx-rg-map
(keymap-set grepx-rg-map "d" #'grepx-find)
(keymap-set grepx-rg-map "c" #'grepx-directory)
(keymap-set grepx-rg-map "p" #'grepx-project)
(keymap-set grepx-rg-map "m" #'grepx-merge-conflicts)
(keymap-set grepx-rg-map "s" #'grepx-live-pwd)
(keymap-set grepx-rg-map "r" #'grepx-live-project)
(keymap-set grepx-rg-map "g" #'grepx-live)
(keymap-set grepx-rg-map "k" #'grepx-compile)

;; Wire grepx-project-map
(keymap-set grepx-project-map "p" #'grepx-project)
(keymap-set grepx-project-map "r" #'grepx-live-project)
(keymap-set grepx-project-map "a" #'grepx-ag-project)
(keymap-set grepx-project-map "g" #'grepx-git-grep)
(keymap-set grepx-project-map "u" #'grepx-ugrep-project)
(keymap-set grepx-project-map "f" #'find-grep)
(keymap-set grepx-project-map "c" #'grepx-compile)

;; Wire root grepx-map
(keymap-set grepx-map "o" #'occur)
(keymap-set grepx-map "g" #'grepx-grep)
(keymap-set grepx-map "d" #'grepx-find)
(keymap-set grepx-map "c" #'grepx-compile)
(keymap-set grepx-map "m" #'grepx-menu)
(keymap-set grepx-map "s" (cons "search" grepx-search-map))
(keymap-set grepx-map "a" #'grepx-ag-pwd)
(keymap-set grepx-map "u" #'grepx-ugrep-pwd)
(keymap-set grepx-map "r" (cons "+ripgrep" grepx-rg-map))
(keymap-set grepx-map "p" (cons "+project" grepx-project-map))

;;;###autoload
(defvar-keymap grepx-mode-map
  :doc "Keymap active when `grepx-mode' is enabled."
  "C-c g" (cons "grep" grepx-map))

(defvar grepx-live-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-l" #'grepx-live-up-directory)
    (keymap-set map "M-d" #'grepx-live-to-buffer)
    map)
  "Keymap active in minibuffer during live ripgrep sessions.")

;; Install isearch bindings
(define-key isearch-mode-map (kbd "M-s r") #'grepx-live)
(define-key isearch-mode-map (kbd "M-s g") #'grepx-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project and Context Helpers

(defun grepx--project-root (&optional may-prompt)
  "Return current project root, checking Projectile, project.el, or default.
If MAY-PROMPT is non-nil and no project is found, prompt using ACR."
  (or (when (fboundp 'projectile-project-root)
        (projectile-project-root))
      (when-let* ((pr (and (fboundp 'project-current) (project-current))))
        (if (fboundp 'project-root)
            (project-root pr)
          (with-no-warnings (car (project-roots pr)))))
      (if (and may-prompt (fboundp 'annotated-completing-read-directory))
          (annotated-completing-read-directory :prompt "Project directory: ")
        default-directory)))

(cl-defun grepx--resolve-regexp (&key initial directory)
  "Resolve search query using ACR context-at-point or INITIAL in DIRECTORY."
  (if (fboundp 'annotated-completing-read-context-from-point)
      (annotated-completing-read-context-from-point
       :prompt (format "[%s]<rg>: " (abbreviate-file-name (or directory default-directory)))
       :history 'grepx-regexp-history
       :initial-input initial)
    (let ((default (thing-at-point 'symbol t)))
      (read-string (if default (format "Search (%s): " default) "Search: ")
                   initial
                   'grepx-regexp-history
                   default))))

(defun grepx--target-source-window (search-win)
  "Find or create a window to visit search results, never returning SEARCH-WIN."
  (let ((candidates (delq search-win (window-list nil 'no-minibuf))))
    (cond
     ((null candidates)
      (split-window-sensibly search-win))
     ((let ((visible-source
             (cl-find-if (lambda (w)
                           (let ((b (window-buffer w)))
                             (and (not (eq w search-win))
                                  (not (string-prefix-p " *" (buffer-name b)))
                                  (not (eq (buffer-local-value 'major-mode b) 'deadgrep-mode)))))
                         candidates)))
        (or visible-source (car candidates))))
     (t (car candidates)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Header Density and Toggle Controls

(define-button-type 'grepx-toggle-header-button
  'action (lambda (_button) (grepx-toggle-header))
  'help-echo "Toggle compact/full header (?)")

(defun grepx--write-compact-heading ()
  "Write a compact 1-line summary header in search results buffers."
  (let ((start-pos (point))
        (inhibit-read-only t))
    (insert
     (propertize "[deadgrep] " 'face 'font-lock-keyword-face)
     (propertize (format "%S" (bound-and-true-p deadgrep--search-term)) 'face 'deadgrep-search-term-face)
     (propertize " in " 'face 'deadgrep-meta-face)
     (if (fboundp 'deadgrep--button)
         (deadgrep--button (abbreviate-file-name default-directory) 'deadgrep-directory)
       (propertize (abbreviate-file-name default-directory) 'face 'deadgrep-meta-face))
     (propertize " " 'face 'deadgrep-meta-face)
     (propertize
      (format "[%s, %s, files: %s] "
              (bound-and-true-p deadgrep--search-type)
              (bound-and-true-p deadgrep--search-case)
              (let ((ft (bound-and-true-p deadgrep--file-type)))
                (cond
                 ((null ft) "all")
                 ((eq ft 'all) "all")
                 ((eq (car-safe ft) 'glob) (cdr ft))
                 ((eq (car-safe ft) 'type) (cdr ft))
                 (t (format "%s" ft)))))
      'face 'deadgrep-meta-face)
     (if (fboundp 'deadgrep--button)
         (deadgrep--button "[?]" 'grepx-toggle-header-button)
       (propertize "[?]" 'face 'font-lock-keyword-face))
     "\n\n")
    (put-text-property start-pos (point) 'read-only t)
    (put-text-property start-pos (point) 'grepx-heading t)))

(defun grepx-toggle-header (&optional global)
  "Toggle between compact 1-line header and button header in search results.
With prefix arg GLOBAL, toggle default `grepx-compact-header'."
  (interactive "P")
  (if global
      (setq-default grepx-compact-header (not (default-value 'grepx-compact-header)))
    (setq-local grepx-compact-header (not grepx-compact-header)))
  (when (eq major-mode 'deadgrep-mode)
    (save-excursion
      (let ((inhibit-read-only t)
            (start-pos (point-min)))
        (goto-char (point-min))
        (let ((heading-end (or (next-single-property-change (point-min) 'grepx-heading)
                               (next-single-property-change (point-min) 'deadgrep-heading)
                               (point-min))))
          (when (> heading-end (point-min))
            (delete-region (point-min) heading-end)))
        (goto-char (point-min))
        (if grepx-compact-header
            (grepx--write-compact-heading)
          (if (fboundp 'deadgrep--write-heading)
              (deadgrep--write-heading)
            (insert (propertize "Search settings\n\n" 'face 'deadgrep-meta-face)))
          (put-text-property start-pos (point) 'grepx-heading t))))))

(defun grepx--heading-wrapper (orig-fn &rest args)
  "Around advice for `deadgrep--write-heading' (ORIG-FN with ARGS)."
  (let ((start-pos (point)))
    (if (and (boundp 'grepx-compact-header)
             grepx-compact-header)
        (progn
          (grepx--write-compact-heading)
          (put-text-property start-pos (point) 'grepx-heading t))
      (apply orig-fn args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window-Safe Navigation Advice

(defun grepx-next-error (arg reset)
  "Move to the ARGth search result, reusing the target window.
RESET specifies whether to start from the beginning.
Ensures successive invocations do not replace or hide the search buffer."
  (interactive "P")
  (let ((search-win (selected-window))
        (count (prefix-numeric-value (or arg 1))))
    (when reset
      (goto-char (point-min)))
    (beginning-of-line)
    (let ((direction (> count 0)))
      (setq count (abs count))
      (while (and (not (zerop count)) (not (eobp)))
        (if direction
            (forward-line 1)
          (forward-line -1))
        (when (get-text-property (point) 'deadgrep-line-number)
          (cl-decf count))))
    (deadgrep--visit-result
     (lambda (target-buffer)
       (let ((target-win (grepx--target-source-window search-win)))
         (select-window target-win)
         (switch-to-buffer target-buffer)
         (select-window search-win))))
    (when (fboundp 'deadgrep--update-highlight)
      (deadgrep--update-highlight))))

(defun grepx--next-error-wrapper (orig-fn arg reset)
  "Around advice redirecting `deadgrep-next-error' (ORIG-FN with ARG, RESET)."
  (if (bound-and-true-p grepx-mode)
      (grepx-next-error arg reset)
    (funcall orig-fn arg reset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Consult Integration Advice & Builders

(defun grepx--consult-grep-wrapper (orig-fn prompt make-builder &optional dir initial)
  "Around advice configuring prompt and keymap for Consult grep family.
ORIG-FN is called with PROMPT, MAKE-BUILDER, DIR, and INITIAL."
  (let ((prompt (if (equal prompt "Ripgrep") "rg: " prompt)))
    (minibuffer-with-setup-hook
        (:append (lambda ()
                   (when (derived-mode-p 'minibuffer-mode)
                     (use-local-map (make-composed-keymap grepx-live-minibuffer-map (current-local-map))))))
      (funcall orig-fn prompt make-builder dir initial))))

(defun grepx--ag-make-builder (paths)
  "Create ag command line builder given PATHS."
  (if (fboundp 'consult--build-args)
      (let ((cmd (consult--build-args grepx-ag-args)))
        (lambda (input)
          (pcase-let* ((`(,arg . ,opts) (if (fboundp 'consult--command-split)
                                            (consult--command-split input)
                                          (cons input nil))))
            (when (and arg (not (string-empty-p arg)))
              (cons (append cmd (list arg) opts paths)
                    (if (fboundp 'consult--highlight-literals)
                        (apply-partially #'consult--highlight-literals arg nil)
                      (lambda (_) nil)))))))
    (lambda (input)
      (cons (append '("ag" "--vimgrep") (list input) paths)
            (lambda (_) nil)))))

(defun grepx--ugrep-make-builder (paths)
  "Create ugrep command line builder given PATHS."
  (if (fboundp 'consult--build-args)
      (let ((cmd (consult--build-args grepx-ugrep-args)))
        (lambda (input)
          (pcase-let* ((`(,arg . ,opts) (if (fboundp 'consult--command-split)
                                            (consult--command-split input)
                                          (cons input nil))))
            (when (and arg (not (string-empty-p arg)))
              (cons (append cmd (list arg) opts paths)
                    (if (fboundp 'consult--highlight-literals)
                        (apply-partially #'consult--highlight-literals arg nil)
                      (lambda (_) nil)))))))
    (lambda (input)
      (cons (append '("ugrep" "--vimgrep") (list input) paths)
            (lambda (_) nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search Commands

;;;###autoload
(defun grepx-compile (&optional search-term directory)
  "Run search for SEARCH-TERM in DIRECTORY in a standard compilation buffer.
Presents one match per line in standard `grep-mode'."
  (interactive)
  (let* ((dir (or directory (grepx--project-root) default-directory))
         (term (or search-term (grepx--resolve-regexp :directory dir)))
         (cmd (format "rg --color=always --no-heading --line-number --smart-case %s"
                      (shell-quote-argument term))))
    (compilation-start cmd 'grep-mode (lambda (_) (format "*rg: %s*" term)))))

;;;###autoload
(defun grepx--run-acr-wizard (&optional directory initial)
  "Two-stage interactive search wizard for DIRECTORY with query INITIAL."
  (interactive)
  (let* ((dir (or directory
                  (if (fboundp 'annotated-completing-read-directory)
                      (annotated-completing-read-directory)
                    (read-directory-name "Search directory: " (or (grepx--project-root) default-directory)))))
         (query (or initial
                    (if (fboundp 'annotated-completing-read-context-from-point)
                        (annotated-completing-read-context-from-point :prompt "Search query: ")
                      (read-string "Search query: " (thing-at-point 'symbol))))))
    (grepx-find query dir)))

;;;###autoload
(defun grepx-find (&optional search-term directory)
  "Run persistent search for SEARCH-TERM in DIRECTORY (or `default-directory')."
  (interactive)
  (let* ((dir (or directory default-directory))
         (term (or search-term (grepx--resolve-regexp :directory dir)))
         (backend (grepx-find-backend :persistent)))
    (if backend
        (funcall (plist-get backend :run-fn) dir term)
      (user-error "No persistent search backend available for grepx-find"))))

;;;###autoload
(defun grepx-directory (&optional search-term directory)
  "Run persistent search for SEARCH-TERM in DIRECTORY (prompted via ACR)."
  (interactive)
  (let* ((dir (or directory
                  (if (fboundp 'annotated-completing-read-directory)
                      (annotated-completing-read-directory)
                    (read-directory-name "Search directory: " default-directory))))
         (term (or search-term (grepx--resolve-regexp :directory dir))))
    (grepx-find term dir)))

;;;###autoload
(defun grepx-project (&optional search-term)
  "Run persistent search for SEARCH-TERM in the project root."
  (interactive)
  (let* ((root (or (grepx--project-root) default-directory))
         (term (or search-term (grepx--resolve-regexp :directory root))))
    (grepx-find term root)))

;;;###autoload
(defun grepx-merge-conflicts ()
  "Search for git conflict markers across the project in a persistent buffer."
  (interactive)
  (let* ((root (or (grepx--project-root) default-directory))
         (deadgrep--search-type 'regexp))
    (if (fboundp 'deadgrep)
        (deadgrep "^(=======$|<<<<<<<|>>>>>>>)" root)
      (grepx-compile "^(=======$|<<<<<<<|>>>>>>>)" root))))

;;;###autoload
(defun grepx-ag-pwd (&optional search-term)
  "Run `ag' for SEARCH-TERM in `default-directory'."
  (interactive)
  (let ((term (or search-term (grepx--resolve-regexp :directory default-directory))))
    (if (fboundp 'ag)
        (funcall 'ag term default-directory)
      (compilation-start (format "ag --nocolor --nogroup %s" (shell-quote-argument term))
                         'grep-mode))))

;;;###autoload
(defun grepx-ag-project (&optional search-term)
  "Run `ag' for SEARCH-TERM in the current project root."
  (interactive)
  (let* ((root (or (grepx--project-root) default-directory))
         (term (or search-term (grepx--resolve-regexp :directory root))))
    (if (fboundp 'ag-project)
        (funcall 'ag-project term)
      (compilation-start (format "ag --nocolor --nogroup %s %s"
                                 (shell-quote-argument term)
                                 (shell-quote-argument root))
                         'grep-mode))))

;;;###autoload
(defun grepx-ugrep-pwd (&optional search-term)
  "Run `ugrep' for SEARCH-TERM in `default-directory'."
  (interactive)
  (let ((term (or search-term (grepx--resolve-regexp :directory default-directory))))
    (if (executable-find "ugrep")
        (compilation-start (format "ugrep -rn -k -j -I %s ." (shell-quote-argument term))
                           'grep-mode)
      (user-error "Ugrep executable not found on exec-path"))))

;;;###autoload
(defun grepx-ugrep-project (&optional search-term)
  "Run `ugrep' for SEARCH-TERM in the project root."
  (interactive)
  (let* ((root (or (grepx--project-root) default-directory))
         (term (or search-term (grepx--resolve-regexp :directory root))))
    (if (executable-find "ugrep")
        (let ((default-directory root))
          (compilation-start (format "ugrep -rn -k -j -I %s ." (shell-quote-argument term))
                             'grep-mode))
      (user-error "Ugrep executable not found on exec-path"))))

;;;###autoload
(defun grepx-git-grep (&optional dir initial)
  "Run git-grep search in DIR with INITIAL.
Dispatches to `consult-git-grep' if available, otherwise `vc-git-grep'."
  (interactive)
  (if (fboundp 'consult-git-grep)
      (consult-git-grep dir initial)
    (let ((default-directory (or dir default-directory)))
      (if (commandp 'vc-git-grep)
          (call-interactively 'vc-git-grep)
        (user-error "Neither consult-git-grep nor vc-git-grep is available")))))

;;;###autoload
(defun grepx-grep (&optional dir initial)
  "Run standard grep in DIR with INITIAL.
Dispatches to `consult-grep' if available, otherwise `grep'."
  (interactive)
  (if (fboundp 'consult-grep)
      (consult-grep dir initial)
    (let ((default-directory (or dir default-directory)))
      (if (and initial (not (string-empty-p initial)))
          (grep (format "grep -nH -e %s" (shell-quote-argument initial)))
        (call-interactively #'grep)))))

;;;###autoload
(defun grepx-consult-ag (&optional dir initial)
  "Run interactive Ag search in DIR with INITIAL using Consult."
  (interactive)
  (if (fboundp 'consult--grep)
      (consult--grep "Ag: " #'grepx--ag-make-builder dir initial)
    (grepx-ag-pwd initial)))

;;;###autoload
(defun grepx-consult-ugrep (&optional dir initial)
  "Run interactive Ugrep search in DIR with INITIAL using Consult."
  (interactive)
  (if (fboundp 'consult--grep)
      (consult--grep "Ugrep: " #'grepx--ugrep-make-builder dir initial)
    (grepx-ugrep-pwd initial)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Live Search & Bridging

;;;###autoload
(defun grepx-live-up-directory ()
  "Navigate one directory level up during an active live ripgrep session."
  (interactive)
  (let ((parent-dir (file-name-directory (directory-file-name default-directory))))
    (when parent-dir
      (run-at-time 0 nil
                   #'consult-ripgrep
                   parent-dir
                   (ignore-errors
                     (buffer-substring-no-properties
                      (minibuffer-prompt-end) (point-max))))))
  (minibuffer-quit-recursive-edit))

;;;###autoload
(defun grepx-live-to-buffer ()
  "Handoff from active live search to a persistent search buffer."
  (interactive)
  (let* ((raw-input (ignore-errors
                      (buffer-substring-no-properties
                       (minibuffer-prompt-end) (point-max))))
         (query (if (and raw-input (string-match "\\`#?\\(.*?\\)\\s-*\\'" raw-input))
                    (match-string 1 raw-input)
                  ""))
         (dir default-directory))
    (minibuffer-quit-recursive-edit)
    (run-at-time 0.05 nil #'grepx-find query dir)))

;;;###autoload
(defun grepx-live (&optional directory initial context)
  "Start an iterative Ripgrep session in DIRECTORY.
INITIAL is the initial search input.  If CONTEXT is non-nil,
prompt for the initial query using ACR context-at-point."
  (interactive)
  (let* ((dir (or directory
                  (if (fboundp 'annotated-completing-read-directory)
                      (annotated-completing-read-directory)
                    (read-directory-name "Ripgrep directory: " (grepx--project-root)))
                  (grepx--project-root)
                  default-directory))
         (query (if (or context (and (called-interactively-p 'any) current-prefix-arg))
                    (grepx--resolve-regexp :directory dir :initial initial)
                  initial))
         (backend (grepx-find-backend :live)))
    (if backend
        (funcall (plist-get backend :run-fn) dir query)
      (user-error "No live search backend available for grepx-live"))))

;;;###autoload
(defun grepx-live-project (&optional initial context)
  "Start an iterative Ripgrep session in the project root with INITIAL and CONTEXT."
  (interactive)
  (grepx-live (or (grepx--project-root) default-directory)
              initial
              (or context current-prefix-arg)))

;;;###autoload
(defun grepx-live-pwd (&optional initial context)
  "Start iterative Ripgrep in `default-directory' with INITIAL and CONTEXT."
  (interactive)
  (grepx-live default-directory
              initial
              (or context current-prefix-arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACR Menu Dispatcher

(defconst grepx-menu--commands
  '(("Search (pwd)"                "Persistent search buffer in default-directory" . grepx-find)
    ("Search (directory)"          "Persistent search buffer in chosen directory" . grepx-directory)
    ("Search (project)"            "Persistent search buffer in project root" . grepx-project)
    ("Compile Grep (1 line/match)" "Ripgrep in classic grep-mode buffer (one line per match)" . grepx-compile)
    ("Git Conflict Markers"        "Search for git conflict markers across project" . grepx-merge-conflicts)
    ("Live Search (dwim)"          "Iterative search session" . grepx-live)
    ("Live Search (pwd)"           "Iterative search in default-directory" . grepx-live-pwd)
    ("Live Search (project)"       "Iterative search in project root" . grepx-live-project)
    ("Live Search (ag)"            "Iterative silver searcher session" . grepx-consult-ag)
    ("Live Search (ugrep)"         "Iterative ugrep session" . grepx-consult-ugrep)
    ("Search Wizard (ACR)"         "Two-stage interactive search wizard" . grepx--run-acr-wizard)
    ("Occur"                       "Search lines matching regexp in current buffer" . occur)
    ("Standard Grep"               "Run standard grep command" . grepx-grep)
    ("Find-Grep (recursive)"       "Run find and grep recursively" . find-grep)
    ("Deadgrep (raw)"              "Start raw deadgrep session" . deadgrep)
    ("Projectile Grep"             "Run grep in current project via Projectile" . projectile-grep)
    ("Projectile Ag"               "Run silver searcher (ag) in project via Projectile" . projectile-ag)
    ("Projectile Ripgrep"          "Run ripgrep in project via Projectile" . projectile-ripgrep)
    ("Silver Searcher (pwd)"       "Run ag in default-directory" . grepx-ag-pwd)
    ("Silver Searcher (project)"   "Run ag in project root" . grepx-ag-project)
    ("Ugrep (pwd)"                 "Run ugrep in default-directory" . grepx-ugrep-pwd)
    ("Ugrep (project)"             "Run ugrep in project root" . grepx-ugrep-project)
    ("Git Grep"                    "Run git grep in repository" . grepx-git-grep)
    ("Consult Line"                "Search lines in current buffer" . consult-line)
    ("Consult Line Multi"          "Search lines across matching buffers" . consult-line-multi))
  "Available commands and descriptions for `grepx-menu'.")

;;;###autoload
(defun grepx-menu ()
  "Interactively dispatch a grep or search command using ACR."
  (interactive)
  (let* ((choice (if (fboundp 'annotated-completing-read)
                     (annotated-completing-read grepx-menu--commands
                                                :prompt "Grep/Search: "
                                                :category 'command
                                                :history 'grepx-menu-history
                                                :require-match t)
                   (completing-read "Grep/Search: " (mapcar #'car grepx-menu--commands) nil t)))
         (target (if (fboundp 'annotated-completing-read--resolve-target)
                     (let ((map (annotated-completing-read--to-map grepx-menu--commands)))
                       (annotated-completing-read--resolve-target map choice))
                   (cdr (assoc choice grepx-menu--commands)))))
    (when (and target (commandp target))
      (call-interactively target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default Backend Registrations

(defun grepx--register-default-backends ()
  "Register built-in search backends in `grepx-backends'."
  (grepx-register-backend 'deadgrep
    :type :persistent
    :priority 100
    :available-p (lambda () (featurep 'deadgrep))
    :run-fn (lambda (dir term) (deadgrep term dir))
    :description "Deadgrep interactive persistent search buffer")
  (grepx-register-backend 'compile-rg
    :type :persistent
    :priority 80
    :available-p (lambda () (and (executable-find "rg") t))
    :run-fn (lambda (dir term) (grepx-compile term dir))
    :description "Ripgrep in compilation-mode buffer")
  (grepx-register-backend 'grep
    :type :persistent
    :priority 50
    :available-p (lambda () t)
    :run-fn (lambda (dir term)
              (let ((default-directory (or dir default-directory)))
                (grep (format "grep -nH -e %s" (shell-quote-argument (or term ""))))))
    :description "Standard grep compilation buffer")
  (grepx-register-backend 'occur
    :type :persistent
    :priority 30
    :available-p (lambda () t)
    :run-fn (lambda (_dir term) (occur (or term "")))
    :description "In-buffer regexp search via occur")
  (grepx-register-backend 'consult-ripgrep
    :type :live
    :priority 100
    :available-p (lambda () (fboundp 'consult-ripgrep))
    :run-fn (lambda (dir initial) (consult-ripgrep dir initial))
    :description "Consult live interactive ripgrep")
  (grepx-register-backend 'acr-wizard
    :type :live
    :priority 80
    :available-p (lambda () t)
    :run-fn (lambda (dir initial) (grepx--run-acr-wizard dir initial))
    :description "Annotated completing-read search wizard"))

(grepx--register-default-backends)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grepx Global Minor Mode

(defun grepx--setup-keybindings ()
  "Install Grepx bindings into search modes."
  (when (boundp 'deadgrep-mode-map)
    (keymap-set deadgrep-mode-map "?" #'grepx-toggle-header)
    (keymap-set deadgrep-mode-map "w" #'deadgrep-edit-mode))
  (when (boundp 'consult-async-map)
    (keymap-set consult-async-map "C-l" #'grepx-live-up-directory)
    (keymap-set consult-async-map "M-d" #'grepx-live-to-buffer))
  (when (boundp 'isearch-mode-map)
    (define-key isearch-mode-map (kbd "M-s r") #'grepx-live)
    (define-key isearch-mode-map (kbd "M-s g") #'grepx-project)))

;;;###autoload
(define-minor-mode grepx-mode
  "Global minor mode consolidating grep, ripgrep, and search interfaces."
  :global t
  :group 'grepx
  (if grepx-mode
      (progn
        (when (and grepx-ripgrep-config-path
                   (file-exists-p grepx-ripgrep-config-path))
          (setenv "RIPGREP_CONFIG_PATH" grepx-ripgrep-config-path))
        (advice-add 'deadgrep--write-heading :around #'grepx--heading-wrapper)
        (advice-add 'deadgrep-next-error :around #'grepx--next-error-wrapper)
        (advice-add 'consult--grep :around #'grepx--consult-grep-wrapper)
        (grepx--setup-keybindings))
    (advice-remove 'deadgrep--write-heading #'grepx--heading-wrapper)
    (advice-remove 'deadgrep-next-error #'grepx--next-error-wrapper)
    (advice-remove 'consult--grep #'grepx--consult-grep-wrapper)))

(provide 'grepx)
;;; grepx.el ends here
