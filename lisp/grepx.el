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

;;; Code:

(require 'cl-lib)
(require 'deadgrep nil t)
(require 'consult nil t)
(require 'annotated-completing-read nil t)

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

(defvar grepx-regexp-history nil
  "History list for search regular expressions.")

(defvar grepx-menu-history nil
  "History list for `grepx-menu' selections.")

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
(keymap-set grepx-map "g" #'grep)
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
;;; Consult Integration Advice

(defun grepx--consult-grep-wrapper (orig-fn prompt make-builder &optional dir initial)
  "Around advice configuring prompt and keymap for Consult grep family.
ORIG-FN is called with PROMPT, MAKE-BUILDER, DIR, and INITIAL."
  (let ((prompt (if (equal prompt "Ripgrep") "rg: " prompt)))
    (minibuffer-with-setup-hook
        (:append (lambda ()
                   (when (derived-mode-p 'minibuffer-mode)
                     (use-local-map (make-composed-keymap grepx-live-minibuffer-map (current-local-map))))))
      (funcall orig-fn prompt make-builder dir initial))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent Search Commands

;;;###autoload
(defun grepx-find (&optional search-term directory)
  "Run persistent search for SEARCH-TERM in DIRECTORY (or `default-directory')."
  (interactive)
  (let* ((dir (or directory default-directory))
         (term (or search-term (grepx--resolve-regexp :directory dir))))
    (deadgrep term dir)))

;;;###autoload
(defun grepx-directory (&optional search-term directory)
  "Run persistent search for SEARCH-TERM in DIRECTORY (prompted via ACR)."
  (interactive)
  (let* ((dir (or directory
                  (if (fboundp 'annotated-completing-read-directory)
                      (annotated-completing-read-directory)
                    (read-directory-name "Search directory: " default-directory))))
         (term (or search-term (grepx--resolve-regexp :directory dir))))
    (deadgrep term dir)))

;;;###autoload
(defun grepx-project (&optional search-term)
  "Run persistent search for SEARCH-TERM in the project root."
  (interactive)
  (let* ((root (or (grepx--project-root) default-directory))
         (term (or search-term (grepx--resolve-regexp :directory root))))
    (deadgrep term root)))

;;;###autoload
(defun grepx-merge-conflicts ()
  "Search for git conflict markers across the project in a persistent buffer."
  (interactive)
  (let* ((root (or (grepx--project-root) default-directory))
         (deadgrep--search-type 'regexp))
    (deadgrep "^(=======$|<<<<<<<|>>>>>>>)" root)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Secondary Search Backends

;;;###autoload
(defun grepx-ag-pwd (&optional search-term)
  "Run `ag' for SEARCH-TERM in `default-directory'."
  (interactive)
  (let ((term (or search-term (grepx--resolve-regexp :directory default-directory))))
    (if (fboundp 'ag)
        (ag term default-directory)
      (compilation-start (format "ag --nocolor --nogroup %s" (shell-quote-argument term))
                         'grep-mode))))

;;;###autoload
(defun grepx-ag-project (&optional search-term)
  "Run `ag' for SEARCH-TERM in the current project root."
  (interactive)
  (let* ((root (or (grepx--project-root) default-directory))
         (term (or search-term (grepx--resolve-regexp :directory root))))
    (if (fboundp 'ag-project)
        (ag-project term)
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
(defun grepx-git-grep ()
  "Run `vc-git-grep' in current Git repository."
  (interactive)
  (if (fboundp 'consult-git-grep)
      (consult-git-grep)
    (call-interactively #'vc-git-grep)))

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
    (run-at-time 0.05 nil #'deadgrep query dir)))

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
         (this-command 'consult-ripgrep))
    (consult-ripgrep dir query)))

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
  '(("Search (pwd)"                grepx-find            "Persistent deadgrep buffer in default-directory")
    ("Search (directory)"          grepx-directory       "Persistent deadgrep buffer in chosen directory")
    ("Search (project)"            grepx-project         "Persistent deadgrep buffer in project root")
    ("Compile Grep (1 line/match)" grepx-compile         "Ripgrep in classic grep-mode buffer (one line per match)")
    ("Git Conflict Markers"        grepx-merge-conflicts "Search for git conflict markers across project")
    ("Live Search (dwim)"          grepx-live            "Iterative ripgrep search session")
    ("Live Search (pwd)"           grepx-live-pwd        "Iterative ripgrep in default-directory")
    ("Live Search (project)"       grepx-live-project    "Iterative ripgrep in project root")
    ("Occur"                       occur                 "Search lines matching regexp in current buffer")
    ("Standard Grep"               grep                  "Run standard grep command")
    ("Find-Grep (recursive)"       find-grep             "Run find and grep recursively")
    ("Deadgrep (raw)"              deadgrep              "Start raw deadgrep session")
    ("Projectile Grep"             projectile-grep       "Run grep in current project via Projectile")
    ("Projectile Ag"               projectile-ag         "Run silver searcher (ag) in project via Projectile")
    ("Projectile Ripgrep"          projectile-ripgrep    "Run ripgrep in project via Projectile")
    ("Silver Searcher (pwd)"       grepx-ag-pwd          "Run ag in default-directory")
    ("Silver Searcher (project)"   grepx-ag-project      "Run ag in project root")
    ("Ugrep (pwd)"                 grepx-ugrep-pwd       "Run ugrep in default-directory")
    ("Ugrep (project)"             grepx-ugrep-project   "Run ugrep in project root")
    ("Git Grep"                    grepx-git-grep        "Run git grep in repository")
    ("Consult Line"                consult-line          "Search lines in current buffer")
    ("Consult Line Multi"          consult-line-multi    "Search lines across matching buffers"))
  "Available commands and descriptions for `grepx-menu'.")

;;;###autoload
(defun grepx-menu ()
  "Interactively dispatch a grep or search command using ACR."
  (interactive)
  (let* ((table (seq-map (lambda (e) (cons (car e) (nth 2 e))) grepx-menu--commands))
         (choice (if (fboundp 'annotated-completing-read)
                     (annotated-completing-read table
                                                :prompt "Grep/Search: "
                                                :category 'command
                                                :history 'grepx-menu-history
                                                :require-match t)
                   (completing-read "Grep/Search: " (seq-map #'car grepx-menu--commands) nil t)))
         (cmd (nth 1 (assoc choice grepx-menu--commands))))
    (when cmd
      (call-interactively cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grepx Global Minor Mode

(defun grepx--setup-keybindings ()
  "Install Grepx bindings into search modes."
  (when (boundp 'deadgrep-mode-map)
    (keymap-set deadgrep-mode-map "?" #'grepx-toggle-header)
    (keymap-set deadgrep-mode-map "w" #'deadgrep-edit-mode))
  (when (boundp 'consult-async-map)
    (keymap-set consult-async-map "C-l" #'grepx-live-up-directory)
    (keymap-set consult-async-map "M-d" #'grepx-live-to-buffer)))

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
