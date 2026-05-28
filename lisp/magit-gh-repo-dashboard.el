;;; magit-gh-repo-dashboard.el --- Repository and PR dashboards for magit-gh -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit "4.0") (transient "0.4") (magit-gh-extras "0.1"))
;; Keywords: vc, tools, magit, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Provides two tabular dashboards:
;;
;; `magit-gh-repo-dashboard-open' — a tabulated-list view of registered
;; repositories showing branch, fetch time, behind status, and dirty state.
;; Press RET to open a per-repo overview buffer with PR counts and magit action
;; shortcuts.  Press m or ? for the transient actions menu.
;;
;; `magit-gh-pr-dashboard-open' — a tabulated-list view of pull requests with
;; filters for state, author, repo, and org.  Supports CI outcome, age, comment
;; count, and review decision columns.
;;
;; Register repositories with `magit-gh-repo-register' or by adding structs
;; directly to `magit-gh-repo-list' with `add-to-list'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'magit)
(require 'magit-gh-extras)

(declare-function annotated-completing-read "annotated-completing-read")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function magit-diff-dwim "magit-diff")
(declare-function magit-diff "magit-diff")
(declare-function magit-commit-create "magit-commit")
(declare-function magit-fetch "magit-fetch")
(declare-function magit-pull-from-upstream "magit-pull")
(declare-function magit-log-current "magit-log")
(declare-function magit-log "magit-log")

;;;; Repository registry

(cl-defstruct (magit-gh-repo (:constructor magit-gh-repo--make) (:copier nil))
  "Registry entry for a local git repository."
  name
  path
  (include-prs nil)
  (auto-sync nil)
  (tags nil)
  (auto-commit nil)
  (commands nil)
  (sort-hint nil))

(defvar magit-gh-repo-list nil
  "List of `magit-gh-repo' structs registered for dashboard display.
Use `magit-gh-repo-register' to add entries, or `add-to-list' directly:
  (add-to-list \\='magit-gh-repo-list
               (magit-gh-repo--make :name \"myrepo\" :path \"/path/to/repo\"))")

(cl-defun magit-gh-repo-register (name path &key
                                          (include-prs nil)
                                          (auto-sync nil)
                                          (tags nil)
                                          (auto-commit nil)
                                          (commands nil)
                                          (sort-hint nil))
  "Register or replace a repository with NAME at absolute PATH.
Replaces any existing entry with the same name.

Keyword arguments:
  :include-prs  include in PR dashboard fetches.
  :auto-sync    nil | `fetch' | `pull' — participate in dashboard-wide sync.
  :tags         list of symbols for filtering in the dashboard.
  :auto-commit  nil | t | FUNCTION — FUNCTION receives the repo struct and
                returns a commit message string.
  :commands     alist of (LABEL . FUNCTION) for the repo command picker.
  :sort-hint    number controlling display order; lower values appear first.
                Repos without a sort-hint appear after all sorted repos."
  (setq magit-gh-repo-list
        (cons (magit-gh-repo--make
               :name name
               :path (expand-file-name path)
               :include-prs include-prs
               :auto-sync auto-sync
               :tags tags
               :auto-commit auto-commit
               :commands commands
               :sort-hint sort-hint)
              (seq-remove (lambda (r) (equal name (magit-gh-repo-name r)))
                          magit-gh-repo-list))))

;;;; Registry helpers

(defun magit-gh-repo-dashboard--default-commit-message (repo)
  "Return a default auto-commit message for REPO."
  (format "chore: auto-commit changes in %s" (magit-gh-repo-name repo)))

(defun magit-gh-repo-dashboard--auto-commit (repo)
  "Stage all changes in REPO and commit using its :auto-commit message.
Returns t when the commit succeeds, nil otherwise."
  (let* ((auto-commit (magit-gh-repo-auto-commit repo))
         (message (if (functionp auto-commit)
                      (funcall auto-commit repo)
                    (magit-gh-repo-dashboard--default-commit-message repo))))
    (magit-gh--with-repo-dir (magit-gh-repo-path repo)
      (and (= 0 (call-process "git" nil nil nil "add" "-A"))
           (= 0 (call-process "git" nil nil nil "commit" "-m" message))))))

(defun magit-gh-repo-dashboard--run-command-for (repo)
  "Open an ACR command picker for REPO and invoke the selected command."
  (let ((commands (magit-gh-repo-commands repo)))
    (unless commands
      (user-error "No commands registered for %s" (magit-gh-repo-name repo)))
    (let* ((table (let ((ht (make-hash-table :test #'equal)))
                    (seq-do (lambda (cmd) (map-put! ht (car cmd) (symbol-name (cdr cmd))))
                             commands)
                    ht))
           (label (annotated-completing-read table
                                             :prompt (format "%s command: " (magit-gh-repo-name repo))
                                             :require-match t)))
      (when-let* ((fn (map-elt commands label)))
        (magit-gh--with-repo-dir (magit-gh-repo-path repo)
          (call-interactively fn))))))

;;;; Stats collection

(defvar magit-gh-repo-dashboard--stats-cache (make-hash-table :test #'equal)
  "Hash table mapping repo path strings to stats plists.")

(defun magit-gh-repo-dashboard--fetch-age (path)
  "Return seconds since last git fetch for repo at PATH, or nil if never fetched."
  (when-let* ((fetch-head (expand-file-name ".git/FETCH_HEAD" path))
              (attrs (and (file-exists-p fetch-head) (file-attributes fetch-head))))
    (float-time (time-since (file-attribute-modification-time attrs)))))

(defun magit-gh-repo-dashboard--head-hash (path)
  "Return the current HEAD commit hash for repo at PATH without spawning a process.
Reads .git/HEAD directly and resolves symbolic refs via file I/O.
Returns nil if the repo has no commits yet."
  (let ((head-file (expand-file-name ".git/HEAD" path)))
    (when-let* ((head (and (file-exists-p head-file)
                           (string-trim (with-temp-buffer
                                          (insert-file-contents head-file)
                                          (buffer-string))))))
      (if (string-prefix-p "ref: " head)
          (let ((ref-path (expand-file-name (concat ".git/" (substring head 5)) path)))
            (when (file-exists-p ref-path)
              (string-trim (with-temp-buffer
                             (insert-file-contents ref-path)
                             (buffer-string)))))
        head))))

(defun magit-gh-repo-dashboard--collect-stats (repo)
  "Synchronously collect git stats for REPO and store them in the cache.
Returns a plist :branch :behind :dirty :fetch-age :head-hash :recent-log."
  (let* ((path (magit-gh-repo-path repo))
         (default-directory path)
         (branch (string-trim (shell-command-to-string "git branch --show-current")))
         (behind-str (string-trim
                      (shell-command-to-string
                       "git rev-list --count HEAD..@{u} 2>/dev/null || echo 0")))
         (behind (string-to-number (if (string-empty-p behind-str) "0" behind-str)))
         (dirty (not (string-empty-p
                      (string-trim (shell-command-to-string "git status --porcelain")))))
         (recent-log (string-trim
                      (shell-command-to-string "git log --oneline -10 2>/dev/null")))
         (stats (list :branch branch
                      :behind behind
                      :dirty dirty
                      :fetch-age (magit-gh-repo-dashboard--fetch-age path)
                      :head-hash (magit-gh-repo-dashboard--head-hash path)
                      :recent-log recent-log)))
    (map-put! magit-gh-repo-dashboard--stats-cache path stats)
    stats))

(defun magit-gh-repo-dashboard--get-stats (repo)
  "Return cached stats for REPO, collecting synchronously if absent or stale.
The cache is invalidated when the HEAD commit hash changes."
  (let* ((path (magit-gh-repo-path repo))
         (cached (gethash path magit-gh-repo-dashboard--stats-cache)))
    (if (and cached
             (equal (magit-gh-repo-dashboard--head-hash path)
                    (plist-get cached :head-hash)))
        cached
      (magit-gh-repo-dashboard--collect-stats repo))))

;;;; Formatting helpers

(defun magit-gh-repo-dashboard--format-age (seconds)
  "Format SECONDS duration as a compact string, or \"never\" if nil."
  (cond
   ((null seconds) "never")
   ((< seconds 60) (format "%ds" (round seconds)))
   ((< seconds 3600) (format "%dm" (round (/ seconds 60))))
   ((< seconds 86400) (format "%dh" (round (/ seconds 3600))))
   (t (format "%dd" (round (/ seconds 86400))))))

(defun magit-gh-repo-dashboard--format-behind (n)
  "Format behind commit count N; empty string when zero."
  (if (> n 0) (propertize (number-to-string n) 'face 'warning) ""))

(defun magit-gh-repo-dashboard--format-dirty (dirty)
  "Format DIRTY flag as a short display string."
  (if dirty (propertize "*" 'face 'warning) ""))

;;;; Repo dashboard mode

(defface magit-gh-repo-name-face
  '((t :inherit font-lock-keyword-face))
  "Face for repository names in the repo dashboard.")

(defface magit-gh-repo-branch-face
  '((t :inherit font-lock-string-face))
  "Face for branch names in the repo dashboard.")

(defun magit-gh-repo-dashboard--build-format (repos)
  "Return the tabulated-list format vector for REPOS.
The Name column width is elastic: wide enough for the longest name in REPOS."
  (let ((name-width (seq-reduce (lambda (w r) (max w (length (magit-gh-repo-name r))))
                                repos
                                (length "Name"))))
    (vector `("Name" ,name-width t)
            '("Branch" 18 t)
            '("Fetched" 8 nil)
            '("Behind" 6 nil)
            '("Changes" 7 nil))))

(defvar magit-gh-repo-dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'magit-gh-repo-dashboard-view)
    (define-key m (kbd "g")   #'magit-gh-repo-dashboard-refresh)
    (define-key m (kbd "r")   #'magit-gh-repo-dashboard-refresh)
    (define-key m (kbd "s")   #'magit-gh-repo-dashboard-magit-status)
    (define-key m (kbd "d")   #'magit-gh-repo-dashboard-magit-diff)
    (define-key m (kbd "D")   #'magit-gh-repo-dashboard-magit-diff-full)
    (define-key m (kbd "l")   #'magit-gh-repo-dashboard-magit-log)
    (define-key m (kbd "L")   #'magit-gh-repo-dashboard-magit-log-full)
    (define-key m (kbd "c")   #'magit-gh-repo-dashboard-magit-commit)
    (define-key m (kbd "C")   #'magit-gh-repo-dashboard-commit)
    (define-key m (kbd "f")   #'magit-gh-repo-dashboard-fetch)
    (define-key m (kbd "u")   #'magit-gh-repo-dashboard-pull)
    (define-key m (kbd "S")   #'magit-gh-repo-dashboard-sync-all)
    (define-key m (kbd "A")   #'magit-gh-repo-dashboard-commit-all)
    (define-key m (kbd "x")   #'magit-gh-repo-dashboard-run-command)
    (define-key m (kbd "t")   #'magit-gh-repo-dashboard-filter-by-tag)
    (define-key m (kbd "p")   #'magit-gh-pr-dashboard-open)
    (define-key m (kbd "m")   #'magit-gh-repo-dashboard-menu)
    (define-key m (kbd "?")   #'magit-gh-repo-dashboard-menu)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for `magit-gh-repo-dashboard-mode'.")

(defvar-local magit-gh-repo-dashboard--tag-filter nil
  "When non-nil, a symbol: only repos tagged with this symbol are shown.")

(define-derived-mode magit-gh-repo-dashboard-mode tabulated-list-mode "Repos"
  "Major mode for the registered repository dashboard."
  (setq tabulated-list-format (magit-gh-repo-dashboard--build-format magit-gh-repo-list))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun magit-gh-repo-dashboard--build-entry (repo)
  "Return a `tabulated-list-entries' entry for REPO."
  (let ((stats (magit-gh-repo-dashboard--get-stats repo)))
    (list repo
          (vector
           (propertize (magit-gh-repo-name repo) 'face 'magit-gh-repo-name-face)
           (propertize (or (plist-get stats :branch) "?") 'face 'magit-gh-repo-branch-face)
           (magit-gh-repo-dashboard--format-age (plist-get stats :fetch-age))
           (magit-gh-repo-dashboard--format-behind (or (plist-get stats :behind) 0))
           (magit-gh-repo-dashboard--format-dirty (plist-get stats :dirty))))))

(defun magit-gh-repo-dashboard--sorted-repos (repos)
  "Return REPOS sorted by :sort-hint; repos without a hint follow sorted ones."
  (seq-sort (lambda (a b)
              (let ((ha (magit-gh-repo-sort-hint a))
                    (hb (magit-gh-repo-sort-hint b)))
                (cond
                 ((and ha hb) (< ha hb))
                 (ha t)
                 (hb nil)
                 (t nil))))
            repos))

(defun magit-gh-repo-dashboard-refresh ()
  "Invalidate the stats cache and repopulate the repository table.
When `magit-gh-repo-dashboard--tag-filter' is set, shows only matching repos.
Repos are ordered by :sort-hint when set, otherwise by insertion order."
  (interactive)
  (clrhash magit-gh-repo-dashboard--stats-cache)
  (let ((repos (magit-gh-repo-dashboard--sorted-repos
                (if magit-gh-repo-dashboard--tag-filter
                    (seq-filter (lambda (r)
                                  (memq magit-gh-repo-dashboard--tag-filter
                                        (magit-gh-repo-tags r)))
                                magit-gh-repo-list)
                  magit-gh-repo-list))))
    (setq tabulated-list-format (magit-gh-repo-dashboard--build-format repos))
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (seq-map #'magit-gh-repo-dashboard--build-entry repos))
    (tabulated-list-print t)))

(defun magit-gh-repo-dashboard--repo-at-point ()
  "Return the `magit-gh-repo' struct at point or signal `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No repository at point")))

(defun magit-gh-repo-dashboard-view ()
  "Open the overview buffer for the repository at point."
  (interactive)
  (magit-gh-repo-overview--open (magit-gh-repo-dashboard--repo-at-point)))

(defun magit-gh-repo-dashboard-magit-status ()
  "Open a magit status buffer for the repository at point."
  (interactive)
  (magit-status-setup-buffer
   (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))))

(defun magit-gh-repo-dashboard-magit-diff ()
  "Open a magit diff (dwim) buffer for the repository at point."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'magit-diff-dwim)))

(defun magit-gh-repo-dashboard-magit-diff-full ()
  "Open the full magit diff menu for the repository at point."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'magit-diff)))

(defun magit-gh-repo-dashboard-magit-log ()
  "Open magit log for the current branch in the repository at point."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'magit-log-current)))

(defun magit-gh-repo-dashboard-magit-log-full ()
  "Open the full magit log menu for the repository at point."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'magit-log)))

(defun magit-gh-repo-dashboard-magit-commit ()
  "Open a magit commit buffer for the repository at point."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'magit-commit-create)))

(defun magit-gh-repo-dashboard-fetch ()
  "Run git fetch for the repository at point via magit."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'magit-fetch)))

(defun magit-gh-repo-dashboard-pull ()
  "Pull from upstream for the repository at point via magit."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'magit-pull-from-upstream)))

(defun magit-gh-repo-dashboard-commit ()
  "Auto-commit changes in the repository at point.
Signals `user-error' when :auto-commit is not configured for this repo."
  (interactive)
  (let ((repo (magit-gh-repo-dashboard--repo-at-point)))
    (unless (magit-gh-repo-auto-commit repo)
      (user-error "Auto-commit is not configured for %s" (magit-gh-repo-name repo)))
    (if (magit-gh-repo-dashboard--auto-commit repo)
        (message "magit-gh: committed changes in %s" (magit-gh-repo-name repo))
      (message "magit-gh: nothing to commit or commit failed in %s"
               (magit-gh-repo-name repo)))))

(defun magit-gh-repo-dashboard-commit-all ()
  "Auto-commit dirty repos that have :auto-commit configured."
  (interactive)
  (let ((repos (seq-filter #'magit-gh-repo-auto-commit magit-gh-repo-list)))
    (when (null repos)
      (user-error "No repositories have :auto-commit configured"))
    (seq-do (lambda (repo)
               (when (plist-get (magit-gh-repo-dashboard--get-stats repo) :dirty)
                 (if (magit-gh-repo-dashboard--auto-commit repo)
                     (message "magit-gh: committed %s" (magit-gh-repo-name repo))
                   (message "magit-gh: commit failed in %s" (magit-gh-repo-name repo)))))
             repos)
    (magit-gh-repo-dashboard-refresh)))

(defun magit-gh-repo-dashboard-sync-all ()
  "Fetch or pull all repos that have :auto-sync configured, then refresh."
  (interactive)
  (let ((repos (seq-filter #'magit-gh-repo-auto-sync magit-gh-repo-list)))
    (when (null repos)
      (user-error "No repositories have :auto-sync configured"))
    (seq-do (lambda (repo)
               (magit-gh--with-repo-dir (magit-gh-repo-path repo)
                 (pcase (magit-gh-repo-auto-sync repo)
                   ('fetch (call-interactively #'magit-fetch))
                   ('pull (call-interactively #'magit-pull-from-upstream)))))
             repos))
  (magit-gh-repo-dashboard-refresh))

(defun magit-gh-repo-dashboard-run-command ()
  "Open ACR picker for the repo at point and invoke the selected command."
  (interactive)
  (magit-gh-repo-dashboard--run-command-for (magit-gh-repo-dashboard--repo-at-point)))

(defun magit-gh-repo-dashboard-filter-by-tag ()
  "Filter the dashboard to show repos with a specific tag.
With an empty selection, clears the filter and shows all repos."
  (interactive)
  (let* ((all-tags (delete-dups
                    (seq-mapcat #'magit-gh-repo-tags magit-gh-repo-list)))
         (choice (completing-read "Filter by tag (empty to clear): "
                                  (seq-map #'symbol-name all-tags)
                                  nil nil)))
    (setq magit-gh-repo-dashboard--tag-filter
          (if (string-empty-p choice) nil (intern choice)))
    (magit-gh-repo-dashboard-refresh)))

;;;###autoload
(defun magit-gh-repo-dashboard-open ()
  "Open the repository dashboard buffer.
Signals `user-error' when `magit-gh-repo-list' is empty."
  (interactive)
  (when (null magit-gh-repo-list)
    (user-error "No repositories registered; use `magit-gh-repo-register'"))
  (let ((buf (get-buffer-create "*magit-gh-repos*")))
    (with-current-buffer buf
      (magit-gh-repo-dashboard-mode)
      (magit-gh-repo-dashboard-refresh))
    (pop-to-buffer buf)))

;;;; Repo overview buffer

(defvar-local magit-gh-repo-overview--repo nil
  "The `magit-gh-repo' struct displayed in this overview buffer.")

(defvar magit-gh-repo-overview--pr-cache (make-hash-table :test #'equal)
  "Hash table mapping repo path to (TOTAL . MINE) open PR count cons.")

(defvar magit-gh-repo-overview-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s")   #'magit-gh-repo-overview-magit-status)
    (define-key m (kbd "d")   #'magit-gh-repo-overview-magit-diff)
    (define-key m (kbd "D")   #'magit-gh-repo-overview-magit-diff-full)
    (define-key m (kbd "l")   #'magit-gh-repo-overview-magit-log)
    (define-key m (kbd "L")   #'magit-gh-repo-overview-magit-log-full)
    (define-key m (kbd "c")   #'magit-gh-repo-overview-magit-commit)
    (define-key m (kbd "C")   #'magit-gh-repo-overview-commit)
    (define-key m (kbd "f")   #'magit-gh-repo-overview-fetch)
    (define-key m (kbd "u")   #'magit-gh-repo-overview-pull)
    (define-key m (kbd "x")   #'magit-gh-repo-overview-run-command)
    (define-key m (kbd "g")   #'magit-gh-repo-overview-refresh)
    (define-key m (kbd "m")   #'magit-gh-repo-overview-menu)
    (define-key m (kbd "?")   #'magit-gh-repo-overview-menu)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for repo overview buffers.")

(defun magit-gh-repo-overview--current-repo ()
  "Return the repo for the current overview buffer or signal `user-error'."
  (or magit-gh-repo-overview--repo
      (user-error "No repository associated with this buffer")))

(defun magit-gh-repo-overview-magit-status ()
  "Open magit status for this overview's repository."
  (interactive)
  (magit-status-setup-buffer
   (magit-gh-repo-path (magit-gh-repo-overview--current-repo))))

(defun magit-gh-repo-overview-magit-diff ()
  "Open magit diff (dwim) for this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'magit-diff-dwim)))

(defun magit-gh-repo-overview-magit-diff-full ()
  "Open the full magit diff menu for this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'magit-diff)))

(defun magit-gh-repo-overview-magit-log ()
  "Open magit log for the current branch in this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'magit-log-current)))

(defun magit-gh-repo-overview-magit-log-full ()
  "Open the full magit log menu for this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'magit-log)))

(defun magit-gh-repo-overview-magit-commit ()
  "Open magit commit for this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'magit-commit-create)))

(defun magit-gh-repo-overview-fetch ()
  "Run git fetch for this overview's repository via magit."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'magit-fetch)))

(defun magit-gh-repo-overview-pull ()
  "Pull from upstream for this overview's repository via magit."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'magit-pull-from-upstream)))

(defun magit-gh-repo-overview-commit ()
  "Auto-commit changes in this overview's repository.
Signals `user-error' when :auto-commit is not configured for this repo."
  (interactive)
  (let ((repo (magit-gh-repo-overview--current-repo)))
    (unless (magit-gh-repo-auto-commit repo)
      (user-error "Auto-commit is not configured for %s" (magit-gh-repo-name repo)))
    (if (magit-gh-repo-dashboard--auto-commit repo)
        (progn
          (message "magit-gh: committed changes in %s" (magit-gh-repo-name repo))
          (magit-gh-repo-overview-refresh))
      (message "magit-gh: nothing to commit or commit failed in %s"
               (magit-gh-repo-name repo)))))

(defun magit-gh-repo-overview-run-command ()
  "Open ACR picker for this overview's repository and invoke the selected command."
  (interactive)
  (magit-gh-repo-dashboard--run-command-for (magit-gh-repo-overview--current-repo)))

(defun magit-gh-repo-overview-refresh ()
  "Re-render the overview buffer with fresh stats."
  (interactive)
  (when-let* ((repo (magit-gh-repo-overview--current-repo))
              (inhibit-read-only t))
    (map-delete magit-gh-repo-dashboard--stats-cache (magit-gh-repo-path repo))
    (map-delete magit-gh-repo-overview--pr-cache (magit-gh-repo-path repo))
    (erase-buffer)
    (magit-gh-repo-overview--render repo)
    (goto-char (point-min))))

(defun magit-gh-repo-overview--pr-counts (path)
  "Return a cons (TOTAL . MINE) of open PR counts for the repo at PATH.
Results are cached; call `magit-gh-repo-overview-refresh' to invalidate."
  (or (gethash path magit-gh-repo-overview--pr-cache)
      (let* ((default-directory path)
             (output (string-trim
                      (shell-command-to-string
                       "gh pr list --json number,author --state open --limit 200 2>/dev/null")))
             (viewer (string-trim
                      (shell-command-to-string "gh api user --jq .login 2>/dev/null")))
             (counts
              (if (string-prefix-p "[" output)
                  (let ((prs (json-parse-string output :array-type 'list :object-type 'alist)))
                    (cons (length prs)
                          (seq-count (lambda (pr)
                                       (equal viewer (map-elt (map-elt pr 'author) 'login)))
                                     prs)))
                (cons 0 0))))
        (map-put! magit-gh-repo-overview--pr-cache path counts)
        counts)))

(defun magit-gh-repo-overview--render (repo)
  "Insert overview content for REPO into the current buffer."
  (let* ((name (magit-gh-repo-name repo))
         (path (magit-gh-repo-path repo))
         (stats (magit-gh-repo-dashboard--get-stats repo))
         (branch (or (plist-get stats :branch) "?"))
         (behind (or (plist-get stats :behind) 0))
         (dirty (plist-get stats :dirty))
         (recent-log (plist-get stats :recent-log))
         (pr-counts (magit-gh-repo-overview--pr-counts path)))
    (insert (propertize name 'face 'bold) "\n\n")
    (insert (format "Path:    %s\n" path))
    (insert (format "Branch:  %s\n" branch))
    (insert (format "Behind:  %s\n"
                    (if (> behind 0)
                        (propertize (format "%d commits" behind) 'face 'warning)
                      "up to date")))
    (insert (format "Changes: %s\n"
                    (if dirty (propertize "uncommitted" 'face 'warning) "clean")))
    (insert "\n")
    (insert (propertize "Pull Requests\n" 'face 'bold))
    (insert (format "  Open: %d total, %d yours\n" (car pr-counts) (cdr pr-counts)))
    (when (and recent-log (not (string-empty-p recent-log)))
      (insert "\n")
      (insert (propertize "Recent Commits\n" 'face 'bold))
      (seq-do (lambda (line) (insert (format "  %s\n" line)))
              (split-string recent-log "\n")))))

(defun magit-gh-repo-overview--open (repo)
  "Pop to a read-only overview buffer for REPO."
  (let* ((buf-name (format "*magit-gh-repo: %s*" (magit-gh-repo-name repo)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local magit-gh-repo-overview--repo repo)
        (use-local-map magit-gh-repo-overview-mode-map)
        (magit-gh-repo-overview--render repo)
        (goto-char (point-min)))
      (setq buffer-read-only t))
    (pop-to-buffer buf)))

;;;; PR dashboard mode

(defvar-local magit-gh-pr-dashboard--filters
  (list :state "open" :author "@me" :repo nil :org nil)
  "Current PR dashboard filters plist.
Keys: :state (\"open\"/\"closed\"), :author, :repo (OWNER/NAME), :org.")

(defface magit-gh-pr-ci-pass-face
  '((t :inherit success))
  "Face for passing CI status in the PR dashboard.")

(defface magit-gh-pr-ci-fail-face
  '((t :inherit error))
  "Face for failing CI status in the PR dashboard.")

(defface magit-gh-pr-ci-pending-face
  '((t :inherit warning))
  "Face for pending CI status in the PR dashboard.")

(defconst magit-gh-pr-dashboard--format
  [("Repo" 22 t)
   ("PR#" 5 nil)
   ("Title" 38 t)
   ("CI" 8 t)
   ("Age" 6 nil)
   ("Cmts" 4 nil)
   ("Review" 14 t)]
  "Column format for `magit-gh-pr-dashboard-mode'.")

(defvar magit-gh-pr-dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'magit-gh-pr-dashboard-open-browser)
    (define-key m (kbd "g")   #'magit-gh-pr-dashboard-refresh)
    (define-key m (kbd "r")   #'magit-gh-pr-dashboard-refresh)
    (define-key m (kbd "f")   #'magit-gh-pr-dashboard-set-filter)
    (define-key m (kbd "F")   #'magit-gh-pr-dashboard-clear-filters)
    (define-key m (kbd "m")   #'magit-gh-pr-dashboard-menu)
    (define-key m (kbd "?")   #'magit-gh-pr-dashboard-menu)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for `magit-gh-pr-dashboard-mode'.")

(define-derived-mode magit-gh-pr-dashboard-mode tabulated-list-mode "PRs"
  "Major mode for the pull request dashboard."
  (setq tabulated-list-format magit-gh-pr-dashboard--format)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (setq-local magit-gh-pr-dashboard--filters
              (list :state "open" :author "@me" :repo nil :org nil)))

(defun magit-gh-pr-dashboard--format-ci (rollup-state)
  "Format CI ROLLUP-STATE string for display."
  (pcase rollup-state
    ("SUCCESS" (propertize "pass" 'face 'magit-gh-pr-ci-pass-face))
    ("FAILURE" (propertize "fail" 'face 'magit-gh-pr-ci-fail-face))
    ("PENDING" (propertize "pending" 'face 'magit-gh-pr-ci-pending-face))
    ("ERROR" (propertize "error" 'face 'magit-gh-pr-ci-fail-face))
    (_ "—")))

(defun magit-gh-pr-dashboard--format-review (decision)
  "Format review DECISION string for display."
  (pcase decision
    ("APPROVED" (propertize "approved" 'face 'success))
    ("CHANGES_REQUESTED" (propertize "changes req" 'face 'error))
    ("REVIEW_REQUIRED" (propertize "needed" 'face 'warning))
    (_ "")))

(defun magit-gh-pr-dashboard--format-age (updated-at)
  "Format UPDATED-AT ISO timestamp as a human-readable age string."
  (when (and updated-at (not (equal updated-at "")))
    (condition-case nil
        (magit-gh-repo-dashboard--format-age
         (float-time (time-since (date-to-time updated-at))))
      (error "?"))))

(defun magit-gh-pr-dashboard--comments-count (pr)
  "Return the comment count from PR alist, handling both number and object forms."
  (let ((c (map-elt pr 'comments)))
    (cond
     ((numberp c) c)
     ((listp c) (or (map-elt c 'totalCount) 0))
     (t 0))))

(defun magit-gh-pr-dashboard--build-args (filters)
  "Return a gh args list for fetching PRs matching FILTERS plist.
When :repo is set, uses `gh pr list -R REPO' (includes CI status).
Otherwise uses `gh search prs' for cross-repo listing."
  (let* ((state (or (plist-get filters :state) "open"))
         (repo (plist-get filters :repo))
         (author (plist-get filters :author))
         (org (plist-get filters :org)))
    (if repo
        (append
         (list "pr" "list" "-R" repo "--state" state
               "--json"
               "number,title,state,author,updatedAt,comments,reviewDecision,statusCheckRollup,isDraft,url")
         (when author (list "--author" author)))
      (append
       (list "search" "prs"
             "--state" (if (member state '("open" "closed")) state "open")
             "--json"
             "number,title,state,repository,author,updatedAt,comments,reviewDecision,isDraft,url")
       (when author
	 (list "--author" author))
       (when org
	 (list "--owner" org))))))

(defconst magit-gh-pr-dashboard--title-width 36
  "Maximum display width for PR title column.")

(defun magit-gh-pr-dashboard--build-entry (pr repo-name)
  "Return a `tabulated-list-entries' entry for PR alist in REPO-NAME."
  (let ((number (map-elt pr 'number)))
    (list (list :number number :repo repo-name :pr pr)
          (vector
           repo-name
           (number-to-string number)
           (truncate-string-to-width
            (or (map-elt pr 'title) "") magit-gh-pr-dashboard--title-width nil nil "…")
           (magit-gh-pr-dashboard--format-ci
            (map-elt (map-elt pr 'statusCheckRollup) 'state))
           (or (magit-gh-pr-dashboard--format-age (map-elt pr 'updatedAt)) "?")
           (number-to-string (magit-gh-pr-dashboard--comments-count pr))
           (magit-gh-pr-dashboard--format-review (map-elt pr 'reviewDecision))))))

(defun magit-gh-pr-dashboard--parse-output (output filters)
  "Parse gh JSON OUTPUT into tabulated list entries using FILTERS for context.
Returns nil when OUTPUT is not a JSON array."
  ;; TOOD flatten tested lets
  (let ((trimmed (string-trim output)))
    (when (string-prefix-p "[" trimmed)
      (when-let* ((prs (json-parse-string trimmed :array-type 'list :object-type 'alist)))
        (let ((per-repo-p (plist-get filters :repo)))
          (seq-map (lambda (pr)
                     (magit-gh-pr-dashboard--build-entry
                      pr
                      (if per-repo-p
                          (plist-get filters :repo)
                        (or (map-elt (map-elt pr 'repository) 'nameWithOwner)
                            "unknown"))))
                   prs))))))

(defun magit-gh-pr-dashboard-refresh ()
  "Fetch PRs matching current filters and repopulate the PR table."
  (interactive)
  (let* ((filters magit-gh-pr-dashboard--filters)
         (args (magit-gh-pr-dashboard--build-args filters))
         (buf (current-buffer))
         (dir (or (ignore-errors (magit-gh--repo-dir))
                  (expand-file-name "~"))))
    (setq tabulated-list-entries nil)
    (tabulated-list-print t)
    (message "magit-gh: fetching PRs...")
    (magit-gh--run-process
     args dir
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq tabulated-list-entries
                 (or (magit-gh-pr-dashboard--parse-output output filters) nil))
           (tabulated-list-print t)
           (message "magit-gh: %d PR(s)"
                    (length tabulated-list-entries))))))))

(defun magit-gh-pr-dashboard--entry-at-point ()
  "Return the PR entry plist at point or signal `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No pull request at point")))

(defun magit-gh-pr-dashboard-open-browser ()
  "Open the pull request at point in a web browser."
  (interactive)
  (when-let* ((entry (magit-gh-pr-dashboard--entry-at-point))
              (pr (plist-get entry :pr))
              (url (map-elt pr 'url)))
    (browse-url url)))

(defun magit-gh-pr-dashboard-set-filter ()
  "Interactively set one PR dashboard filter field."
  (interactive)
  (let* ((current magit-gh-pr-dashboard--filters)
         (field (completing-read "Filter field: "
                                 '("state" "author" "repo" "org") nil t))
         (value (pcase field
                  ("state"
                   (completing-read "State: " '("open" "closed") nil t))
                  ("author"
                   (read-string "Author (@me for current user): "
                                (or (plist-get current :author) "@me")))
                  ("repo"
                   (read-string "Repo (OWNER/NAME, empty to clear): "
                                (or (plist-get current :repo) "")))
                  ("org"
                   (read-string "Org (empty to clear): "
                                (or (plist-get current :org) "")))))
         (key (intern (format ":%s" field)))
         (new-val (if (string-empty-p value) nil value)))
    (setq magit-gh-pr-dashboard--filters
          (plist-put (copy-sequence current) key new-val))
    (magit-gh-pr-dashboard-refresh)))

(defun magit-gh-pr-dashboard-clear-filters ()
  "Reset PR dashboard filters to defaults (open PRs, current user)."
  (interactive)
  (setq magit-gh-pr-dashboard--filters
        (list :state "open" :author "@me" :repo nil :org nil))
  (magit-gh-pr-dashboard-refresh))

;;;###autoload
(defun magit-gh-pr-dashboard-open ()
  "Open the pull request dashboard buffer."
  (interactive)
  (magit-gh--check-gh)
  (let ((buf (get-buffer-create "*magit-gh-prs*")))
    (with-current-buffer buf
      (magit-gh-pr-dashboard-mode)
      (magit-gh-pr-dashboard-refresh))
    (pop-to-buffer buf)))

;;;; Transient menus

(transient-define-prefix magit-gh-repo-dashboard-menu ()
  "Actions for the repository at point in the repo dashboard."
  [["Repository"
    ("RET" "Open overview" magit-gh-repo-dashboard-view)
    ("s" "Status" magit-gh-repo-dashboard-magit-status)
    ("d" "Diff (dwim)" magit-gh-repo-dashboard-magit-diff)
    ("D" "Diff…" magit-gh-repo-dashboard-magit-diff-full)
    ("l" "Log (current)" magit-gh-repo-dashboard-magit-log)
    ("L" "Log…" magit-gh-repo-dashboard-magit-log-full)
    ("c" "Commit" magit-gh-repo-dashboard-magit-commit)
    ("C" "Auto-commit" magit-gh-repo-dashboard-commit)
    ("f" "Fetch" magit-gh-repo-dashboard-fetch)
    ("u" "Pull" magit-gh-repo-dashboard-pull)
    ("x" "Run command" magit-gh-repo-dashboard-run-command)]
   ["Dashboard"
    ("p" "Open PR dashboard" magit-gh-pr-dashboard-open)
    ("S" "Sync all" magit-gh-repo-dashboard-sync-all)
    ("A" "Commit all" magit-gh-repo-dashboard-commit-all)
    ("t" "Filter by tag" magit-gh-repo-dashboard-filter-by-tag)
    ("g" "Refresh" magit-gh-repo-dashboard-refresh)
    ("q" "Quit" quit-window)]])

(transient-define-prefix magit-gh-repo-overview-menu ()
  "Magit actions for the repository shown in this overview buffer."
  [["Magit"
    ("s" "Status" magit-gh-repo-overview-magit-status)
    ("d" "Diff (dwim)" magit-gh-repo-overview-magit-diff)
    ("D" "Diff…" magit-gh-repo-overview-magit-diff-full)
    ("l" "Log (current)" magit-gh-repo-overview-magit-log)
    ("L" "Log…" magit-gh-repo-overview-magit-log-full)
    ("c" "Commit" magit-gh-repo-overview-magit-commit)
    ("C" "Auto-commit" magit-gh-repo-overview-commit)
    ("f" "Fetch" magit-gh-repo-overview-fetch)
    ("u" "Pull" magit-gh-repo-overview-pull)
    ("x" "Run command" magit-gh-repo-overview-run-command)]
   ["View"
    ("g" "Refresh" magit-gh-repo-overview-refresh)
    ("q" "Quit" quit-window)]])

(transient-define-prefix magit-gh-pr-dashboard-menu ()
  "Actions for the PR dashboard."
  [["Pull Request"
    ("RET" "Open in browser" magit-gh-pr-dashboard-open-browser)]
   ["Filter"
    ("f" "Set filter…" magit-gh-pr-dashboard-set-filter)
    ("F" "Clear all filters" magit-gh-pr-dashboard-clear-filters)]
   ["Dashboard"
    ("g" "Refresh" magit-gh-pr-dashboard-refresh)
    ("q" "Quit" quit-window)]])

(provide 'magit-gh-repo-dashboard)

;;; magit-gh-repo-dashboard.el ends here
