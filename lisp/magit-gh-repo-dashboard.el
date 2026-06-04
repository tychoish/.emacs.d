;;; magit-gh-repo-dashboard.el --- Repository and PR dashboards for magit-gh -*- lexical-binding: t -*-

;; Author: sam kleinman
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
(declare-function magit-push-current-to-pushremote "magit-push")
(declare-function magit-log-current "magit-log")
(declare-function magit-log "magit-log")
(declare-function magit-show-commit "magit-diff")
(declare-function magit-checkout "magit-branch")
(declare-function magit-worktree-checkout "magit-worktree")
(declare-function magit-worktree-delete "magit-worktree")
(declare-function builder-compile-project "builder")
(declare-function agent-shell-switch-buffer "agent-shell-menu")
(declare-function agent-shell-switch-project-session "agent-shell-menu")
(declare-function agent-shell-new-shell "agent-shell")
(declare-function agent-shell-queue-buffer-open "agent-shell-queue")

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
  (sort-hint nil)
  (worktree nil)
  (submodule nil)
  (branch nil)
  (sync-branches nil)
  (sync-command nil))

(defvar magit-gh-repo-list '()
  "List of `magit-gh-repo' structs registered for dashboard display.
Use `magit-gh-repo-register' to add entries.")

(cl-defun magit-gh-repo-register (&key name path include-prs auto-sync tags auto-commit commands sort-hint worktree sync-branches sync-command)
  "Register or replace a repository with NAME at absolute PATH.
Replaces any existing entry with the same name or path.

Keyword arguments:
  :include-prs    include in PR dashboard fetches.
  :auto-sync      nil | `auto-fetch' | `auto-pull' | `auto-commit-and-push' |
                  `auto-sync-command' — participate in dashboard-wide sync.
                  `auto-fetch' runs git fetch --all.
                  `auto-pull' runs git pull (requires :sync-branches).
                  `auto-commit-and-push' pulls, commits, and pushes
                  (requires :sync-branches and :auto-commit).
                  `auto-sync-command' runs :sync-command.
  :sync-branches  list of branch names on which auto-pull and
                  auto-commit-and-push are permitted; nil means any branch.
  :sync-command   string or function for `auto-sync-command'.
                  A string is run as a shell command in the repo directory.
                  A function is called with (REPO CALLBACK).
  :tags           list of symbols for filtering in the dashboard.
  :auto-commit    nil | t | FUNCTION — FUNCTION receives the repo struct and
                  returns a commit message string.
  :commands       alist of (LABEL . FUNCTION) for the repo command picker.
  :sort-hint      number controlling display order; lower values appear first.
                  Repos without a sort-hint appear after all sorted repos.
  :worktree       non-nil when this entry represents a git worktree."
  (unless (and name path)
    (user-error "must specify name (%s) and path (%s)" name path))

  (let ((abs-path (expand-file-name path)))
    (setq magit-gh-repo-list
          (thread-last magit-gh-repo-list
            (seq-remove (lambda (r)
                          (or (equal name (magit-gh-repo-name r))
                              (equal abs-path (magit-gh-repo-path r)))))
            (append (list (magit-gh-repo--make
                           :name name
                           :path abs-path
                           :include-prs include-prs
                           :auto-sync auto-sync
                           :tags tags
                           :auto-commit auto-commit
                           :commands commands
                           :sort-hint sort-hint
                           :worktree worktree
                           :sync-branches sync-branches
                           :sync-command sync-command)))))))

;;;; Registry helpers

(defvar magit-gh-repo-dashboard-sync-trigger 'interactive
  "How the current auto-sync was triggered.
Bind to `timer' when invoking auto-sync from a timer; defaults to `interactive'.")

(defun magit-gh-repo-dashboard--default-commit-message (repo)
  "Return a default auto-commit message for REPO.
Includes hostname, Emacs instance ID, and the current sync trigger."
  (format "chore: auto-commit changes in %s [%s:%s, %s]"
          (magit-gh-repo-name repo)
          (system-name)
          (or (and (boundp 'sprite-instance-id) sprite-instance-id) "unknown")
          (symbol-name magit-gh-repo-dashboard-sync-trigger)))

(defun magit-gh-repo-dashboard--stage-all (repo)
  "Stage all changes in REPO. Returns t on success."
  (magit-gh--with-repo-dir (magit-gh-repo-path repo)
    (= 0 (magit-call-git "add" "-A"))))

(defun magit-gh-repo-dashboard--auto-commit (repo)
  "Stage all changes in REPO and commit using its :auto-commit message.
Returns t when the commit succeeds, nil otherwise."
  (let* ((auto-commit (magit-gh-repo-auto-commit repo))
         (message (if (functionp auto-commit)
                      (funcall auto-commit repo)
                    (magit-gh-repo-dashboard--default-commit-message repo))))
    (and (magit-gh-repo-dashboard--stage-all repo)
         (magit-gh--with-repo-dir (magit-gh-repo-path repo)
           (= 0 (magit-call-git "commit" "-m" message))))))

(defun magit-gh-repo-dashboard--run-command-for (repo)
  "Open an ACR command picker for REPO and invoke the selected command."
  (let ((commands (magit-gh-repo-commands repo)))
    (unless commands
      (user-error "No commands registered for %s" (magit-gh-repo-name repo)))
    (let* ((table (seq-map (lambda (cmd)
                             (cons (format "%s" (car cmd)) (symbol-name (cdr cmd))))
                           commands))
           (label (annotated-completing-read table
                                             :prompt (format "%s command: " (magit-gh-repo-name repo))
                                             :require-match t)))
      (when-let* ((fn (cdr (seq-find (lambda (cmd)
                                      (equal label (format "%s" (car cmd))))
                                    commands))))
        (magit-gh--with-repo-dir (magit-gh-repo-path repo)
          (call-interactively fn))))))

;;;; Stats collection

(defun magit-gh-repo-dashboard--fetch-age (path)
  "Return seconds since last git fetch for repo at PATH, or nil if never fetched."
  (when-let* ((fetch-head (expand-file-name ".git/FETCH_HEAD" path))
              (attrs (and (file-exists-p fetch-head) (file-attributes fetch-head))))
    (float-time (time-since (file-attribute-modification-time attrs)))))

(defun magit-gh-repo-dashboard--head-hash (path)
  "Return the current HEAD commit hash for repo at PATH without spawning a process.
Reads .git/HEAD directly and resolves symbolic refs via file I/O.
Returns nil if the repo has no commits yet."
  (when-let* ((head-file (expand-file-name ".git/HEAD" path))
	      (head (and (file-exists-p head-file)
                         (string-trim (with-temp-buffer
                                        (insert-file-contents head-file)
                                        (buffer-string))))))
    (if-let* ((_ (string-prefix-p "ref: " head))
	      (ref-path (expand-file-name (concat ".git/" (substring head 5)) path))
              (_ (file-exists-p ref-path)))
        (string-trim (with-temp-buffer
                       (insert-file-contents ref-path)
                       (buffer-string))))
      head))

(defun magit-gh-repo-dashboard--collect-stats (repo)
  "Synchronously collect git stats for REPO and store them in the cache.
Returns a plist with keys :branch :remote-origin :behind :ahead :dirty
:uncommitted-files :fetch-age :head-hash :recent-log."
  (let* ((path (magit-gh-repo-path repo))
         (default-directory path)
         (branch (or (magit-git-string "branch" "--show-current") ""))
         (remote-origin (magit-get "remote" "origin" "url"))
         (behind (string-to-number
                  (or (ignore-errors (magit-git-string "rev-list" "--count" "HEAD..@{u}"))
                      "0")))
         (ahead (string-to-number
                 (or (ignore-errors (magit-git-string "rev-list" "--count" "@{u}..HEAD"))
                     "0")))
         (porcelain-lines (magit-git-lines "status" "--porcelain"))
         (dirty (not (null porcelain-lines)))
         (uncommitted-files (when dirty porcelain-lines))
         (recent-log (mapconcat #'identity
                                (magit-git-lines "log" "--oneline" "-10")
                                "\n"))
         (stats (list :branch branch
                      :remote-origin remote-origin
                      :behind behind
                      :ahead ahead
                      :dirty dirty
                      :uncommitted-files uncommitted-files
                      :fetch-age (magit-gh-repo-dashboard--fetch-age path)
                      :head-hash (magit-gh-repo-dashboard--head-hash path)
                      :recent-log recent-log)))
    (magit-gh--cache-set path :stats stats)
    stats))

(defun magit-gh-repo-dashboard--collect-stats-async (repo callback)
  "Collect git stats for REPO asynchronously; call CALLBACK with a stats plist.
Runs five git subcommands sequentially via `magit-gh-repo-dashboard--run-git',
accumulating their outputs before assembling the stats plist."
  (let* ((path (magit-gh-repo-path repo))
         (commands (list '("branch" "--show-current")
                         '("remote" "get-url" "origin")
                         '("rev-list" "--count" "HEAD..@{u}")
                         '("rev-list" "--count" "@{u}..HEAD")
                         '("status" "--porcelain")
                         '("log" "--oneline" "-10")))
	 outputs run)
    (setq run
          (lambda (remaining)
            (if (null remaining)
                (let* ((branch (string-trim (or (nth 0 outputs) "")))
                       (raw-origin (string-trim (or (nth 1 outputs) "")))
                       (remote-origin (unless (string-empty-p raw-origin) raw-origin))
                       (behind-str (string-trim (or (nth 2 outputs) "0")))
                       (behind (string-to-number
                                (if (string-empty-p behind-str) "0" behind-str)))
                       (ahead-str (string-trim (or (nth 3 outputs) "0")))
                       (ahead (string-to-number
                               (if (string-empty-p ahead-str) "0" ahead-str)))
                       (porcelain (or (nth 4 outputs) ""))
                       (porcelain-lines (seq-remove #'string-empty-p
                                                    (split-string porcelain "\n")))
                       (dirty (not (null porcelain-lines)))
                       (uncommitted-files (when dirty porcelain-lines))
                       (recent-log (string-trim (or (nth 5 outputs) "")))
                       (stats (list :branch branch
                                    :remote-origin remote-origin
                                    :behind behind
                                    :ahead ahead
                                    :dirty dirty
                                    :uncommitted-files uncommitted-files
                                    :fetch-age (magit-gh-repo-dashboard--fetch-age path)
                                    :head-hash (magit-gh-repo-dashboard--head-hash path)
                                    :recent-log recent-log)))
                  (magit-gh--cache-set path :stats stats)
                  (funcall callback stats))
              (magit-gh-repo-dashboard--run-git
               path (car remaining)
               (lambda (output)
                 (setq outputs (append outputs (list output)))
                 (funcall run (cdr remaining)))
               (lambda (_ _)
                 (setq outputs (append outputs (list "")))
                 (funcall run (cdr remaining)))))))
    (funcall run commands)))

(defun magit-gh-repo-overview--pr-counts-async (path callback)
  "Fetch open PR counts for repo at PATH asynchronously.
Checks the in-memory cache first; calls CALLBACK with (TOTAL . MINE)."
  (if-let* ((cached (magit-gh--cache-get path :pr-counts)))
      (funcall callback cached)
    (magit-gh--run-process
     '("api" "user" "--jq" ".login")
     path
     (lambda (viewer-output)
       (let ((viewer (string-trim viewer-output)))
         (magit-gh--run-process
          (list "pr" "list" "--json" "number,author"
                "--state" "open" "--limit" "200")
          path
          (lambda (pr-output)
            (let* ((trimmed (string-trim pr-output))
                   (counts
                    (if (string-prefix-p "[" trimmed)
                        (let ((prs (json-parse-string trimmed
                                                      :array-type 'list
                                                      :object-type 'alist)))
                          (cons (length prs)
                                (seq-count
                                 (lambda (pr)
                                   (equal viewer
                                          (map-elt (map-elt pr 'author) 'login)))
                                 prs)))
                      (cons 0 0))))
              (magit-gh--cache-set path :pr-counts counts)
              (funcall callback counts)))))))))


(defun magit-gh-repo-dashboard--get-stats (repo)
  "Return cached stats for REPO, collecting synchronously if absent or stale.
The cache is invalidated when the HEAD commit hash changes."
  (let* ((path (magit-gh-repo-path repo))
         (cached (magit-gh--cache-get path :stats)))
    (if (and cached
             (equal (magit-gh-repo-dashboard--head-hash path)
                    (plist-get cached :head-hash)))
        cached
      (magit-gh-repo-dashboard--collect-stats repo))))

;;;; Async git operations

(defun magit-gh-repo-dashboard--run-git (path args on-success &optional on-error)
  "Run git ARGS in PATH asynchronously using magit's configured git executable.
ON-SUCCESS is called with right-trimmed stdout on exit 0.
ON-ERROR is called with stdout and exit-code on non-zero exit; defaults to a message."
  (let* ((default-directory path)
         (proc-buf (generate-new-buffer " *magit-gh-git*")))
    (with-current-buffer proc-buf
      (setq default-directory path))
    (make-process
     :name "magit-gh-git"
     :buffer proc-buf
     :command (cons magit-git-executable args)
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((output (with-current-buffer (process-buffer proc)
                         (string-trim-right (buffer-string))))
               (code (process-exit-status proc)))
           (kill-buffer (process-buffer proc))
           (if (= code 0)
               (funcall on-success output)
             (if on-error
                 (funcall on-error output code)
               (message "magit-gh: git %s failed (%d): %s"
                        (car args) code output)))))))))

(defun magit-gh-repo-dashboard--fetch-async (repo on-complete)
  "Run git fetch for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text on failure."
  (magit-gh-repo-dashboard--run-git
   (magit-gh-repo-path repo)
   '("fetch")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-gh-repo-dashboard--pull-async (repo on-complete)
  "Run git pull for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text on failure."
  (magit-gh-repo-dashboard--run-git
   (magit-gh-repo-path repo)
   '("pull")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-gh-repo-dashboard--auto-commit-async (repo on-complete)
  "Stage all changes in REPO and commit using its :auto-commit message function.
Calls ON-COMPLETE with `ok' when committed, `skipped' when workdir is clean,
or `error' when git add or commit fails."
  (let* ((path (magit-gh-repo-path repo))
         (auto-commit (magit-gh-repo-auto-commit repo))
         (msg (if (functionp auto-commit)
                  (funcall auto-commit repo)
                (magit-gh-repo-dashboard--default-commit-message repo))))
    (magit-gh-repo-dashboard--run-git
     path '("status" "--porcelain")
     (lambda (porcelain)
       (if (string-empty-p porcelain)
           (funcall on-complete 'skipped)
         (magit-gh-repo-dashboard--run-git
          path '("add" "-A")
          (lambda (_)
            (magit-gh-repo-dashboard--run-git
             path (list "commit" "-m" msg)
             (lambda (_) (funcall on-complete 'ok))
             (lambda (output code)
               (funcall on-complete 'error (format "commit failed, exit %d: %s" code output)))))
          (lambda (output code)
            (funcall on-complete 'error (format "add failed, exit %d: %s" code output))))))
     (lambda (output code)
       (funcall on-complete 'error (format "status failed, exit %d: %s" code output))))))

(defun magit-gh-repo-dashboard--current-branch (path)
  "Return the current branch name for the repo at PATH synchronously."
  (let ((default-directory path))
    (string-trim (or (magit-git-string "branch" "--show-current") ""))))

(defun magit-gh-repo-dashboard--branch-allowed-p (repo)
  "Return current branch name if allowed by REPO's sync-branches, nil otherwise.
When sync-branches is nil any branch is allowed and the current branch is returned."
  (let* ((allowed (magit-gh-repo-sync-branches repo))
         (current (magit-gh-repo-dashboard--current-branch (magit-gh-repo-path repo))))
    (if (null allowed)
        current
      (and (member current allowed) current))))

(defun magit-gh-repo-dashboard--auto-fetch-async (repo on-complete)
  "Run git fetch --all for REPO asynchronously.
Calls ON-COMPLETE with `ok' on success or `error' and error text on failure."
  (magit-gh-repo-dashboard--run-git
   (magit-gh-repo-path repo)
   '("fetch" "--all")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-gh-repo-dashboard--auto-pull-async (repo on-complete)
  "Run git pull for REPO if current branch is in sync-branches.
Calls ON-COMPLETE with `ok', `skipped' (branch not allowed), or `error'."
  (if-let* ((branch (magit-gh-repo-dashboard--branch-allowed-p repo)))
      (magit-gh-repo-dashboard--run-git
       (magit-gh-repo-path repo)
       '("pull")
       (lambda (_) (funcall on-complete 'ok))
       (lambda (output code)
         (funcall on-complete 'error (format "exit %d: %s" code output))))
    (funcall on-complete 'skipped
             (format "branch %s not in sync-branches"
                     (magit-gh-repo-dashboard--current-branch
                      (magit-gh-repo-path repo))))))

(defun magit-gh-repo-dashboard--run-git-chain (path steps on-success on-complete)
  "Run git STEPS sequentially in PATH.
STEPS is a list of (ARGS . LABEL) pairs.  On success of all steps call
ON-SUCCESS with no args.  On any failure call ON-COMPLETE with `error'
and a message of the form \"LABEL failed, exit N: output\"."
  (if (null steps)
      (funcall on-success)
    (let* ((step (car steps))
           (args (car step))
           (label (cdr step)))
      (magit-gh-repo-dashboard--run-git
       path args
       (lambda (_)
         (magit-gh-repo-dashboard--run-git-chain
          path (cdr steps) on-success on-complete))
       (lambda (output code)
         (funcall on-complete 'error
                  (format "%s failed, exit %d: %s" label code output)))))))

(defun magit-gh-repo-dashboard--auto-commit-and-push-async (repo on-complete)
  "Pull --rebase, stage, commit, and push REPO if branch is in sync-branches.
Calls ON-COMPLETE with `ok', `skipped' (branch not allowed or workdir
clean after pull), or `error' with error text."
  (if-let* ((branch (magit-gh-repo-dashboard--branch-allowed-p repo)))
      (let* ((path (magit-gh-repo-path repo))
             (auto-commit (magit-gh-repo-auto-commit repo))
             (msg (if (functionp auto-commit)
                      (funcall auto-commit repo)
                    (magit-gh-repo-dashboard--default-commit-message repo))))
        (magit-gh-repo-dashboard--run-git
         path '("pull" "--rebase")
         (lambda (_)
           (magit-gh-repo-dashboard--run-git
            path '("status" "--porcelain")
            (lambda (porcelain)
              (if (string-empty-p porcelain)
                  (funcall on-complete 'skipped "workdir clean after pull")
                (magit-gh-repo-dashboard--run-git-chain
                 path
                 `((("add" "-A") . "add")
                   (,(list "commit" "-m" msg) . "commit")
                   (("push") . "push"))
                 (lambda () (funcall on-complete 'ok))
                 on-complete)))
            (lambda (output code)
              (funcall on-complete 'error
                       (format "status failed, exit %d: %s" code output)))))
         (lambda (output code)
           (funcall on-complete 'error
                    (format "pull --rebase failed, exit %d: %s" code output)))))
    (funcall on-complete 'skipped
             (format "branch %s not in sync-branches"
                     (magit-gh-repo-dashboard--current-branch
                      (magit-gh-repo-path repo))))))

(defun magit-gh-repo-dashboard--auto-sync-command-async (repo on-complete)
  "Run REPO's sync-command asynchronously.
sync-command may be a string (run as shell command in the repo directory)
or a function called with (REPO ON-COMPLETE).
Calls ON-COMPLETE with `ok', `skipped', or `error' and optional error text."
  (let ((cmd (magit-gh-repo-sync-command repo)))
    (cond
     ((null cmd)
      (funcall on-complete 'skipped "no sync-command configured"))
     ((functionp cmd)
      (funcall cmd repo on-complete))
     ((stringp cmd)
      (let* ((path (magit-gh-repo-path repo))
             (proc-buf (generate-new-buffer " *magit-gh-sync-cmd*")))
        (with-current-buffer proc-buf
          (setq default-directory path))
        (make-process
         :name "magit-gh-sync-cmd"
         :buffer proc-buf
         :command (list shell-file-name shell-command-switch cmd)
         :connection-type 'pipe
         :noquery t
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let ((output (with-current-buffer (process-buffer proc)
                             (string-trim-right (buffer-string))))
                   (code (process-exit-status proc)))
               (kill-buffer (process-buffer proc))
               (if (= code 0)
                   (funcall on-complete 'ok)
                 (funcall on-complete 'error
                          (format "exit %d: %s" code output)))))))))
     (t
      (funcall on-complete 'error "sync-command must be a string or function")))))

(defun magit-gh-repo-dashboard--dispatch-sync-op (repo cb)
  "Dispatch REPO's auto-sync operation asynchronously; call CB with result."
  (pcase (magit-gh-repo-auto-sync repo)
    ('auto-fetch (magit-gh-repo-dashboard--auto-fetch-async repo cb))
    ('auto-pull (magit-gh-repo-dashboard--auto-pull-async repo cb))
    ('auto-commit-and-push (magit-gh-repo-dashboard--auto-commit-and-push-async repo cb))
    ('auto-sync-command (magit-gh-repo-dashboard--auto-sync-command-async repo cb))
    ('fetch (magit-gh-repo-dashboard--auto-fetch-async repo cb))
    ('pull (magit-gh-repo-dashboard--auto-pull-async repo cb))
    (_ (funcall cb 'skipped))))

(defun magit-gh-repo-dashboard--log-operation (repo-name operation status &optional error-text)
  "Log REPO-NAME OPERATION with STATUS to *Messages*.
The current timestamp is attached as a tooltip (help-echo) on REPO-NAME.
When ERROR-TEXT is non-nil it is appended to the message."
  (let* ((ts (format-time-string "%Y-%m-%d %H:%M:%S"))
         (name (propertize repo-name 'help-echo ts))
         (detail (if error-text (format " — %s" error-text) "")))
    (message "magit-gh: %s %s → %s%s" name operation (symbol-name status) detail)))

(defun magit-gh-repo-dashboard--batch-run (repos op-fn label &optional on-all-done)
  "Run OP-FN asynchronously on each repo in REPOS.
OP-FN is called as (op-fn REPO CALLBACK) where CALLBACK receives a status
symbol: `ok', `skipped', or `error'.
When all repos finish, display a LABEL summary message and optionally call
ON-ALL-DONE with an alist of (NAME . STATUS)."
  (let* ((remaining (list (length repos)))
         (results nil))
    (message "%s: starting %d repo(s)..." label (length repos))
    (seq-do
     (lambda (repo)
       (funcall op-fn repo
                (lambda (status &optional error-text)
                  (magit-gh-repo-dashboard--log-operation
                   (magit-gh-repo-name repo) label status error-text)
                  (push (cons (magit-gh-repo-name repo) status) results)
                  (setcar remaining (1- (car remaining)))
                  (when (= 0 (car remaining))
                    (let* ((ok (seq-count (lambda (r) (eq 'ok (cdr r))) results))
                           (skipped (seq-count (lambda (r) (eq 'skipped (cdr r))) results))
                           (errors (seq-filter (lambda (r) (eq 'error (cdr r))) results)))
                      (message "%s: %d ok%s%s" label ok
                               (if (> skipped 0) (format ", %d skipped" skipped) "")
                               (if errors
                                   (format ", %d failed (%s)"
                                           (length errors)
                                           (mapconcat #'car errors ", "))
                                 ""))
                      (when on-all-done
                        (funcall on-all-done results)))))))
     repos)))

(defun magit-gh-repo-dashboard--maybe-refresh ()
  "Refresh the repo dashboard buffer if it is currently live."
  (when-let* ((buf (get-buffer "*magit-gh-repos*"))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (magit-gh-repo-dashboard-refresh))))

;;;; Worktree support

(defun magit-gh-repo-dashboard--parse-worktrees (main-path lines)
  "Parse LINES from `git worktree list --porcelain' for repo at MAIN-PATH.
Returns a list of `magit-gh-repo' structs for additional worktrees.
The first block (the main worktree) is always skipped."
  (let* ((main-name (file-name-nondirectory (directory-file-name main-path)))
         (blocks nil)
         (current nil))
    (seq-do (lambda (line)
              (if (string-empty-p line)
                  (progn
                    (when current (push (nreverse current) blocks))
                    (setq current nil))
                (push line current)))
            lines)
    (when current (push (nreverse current) blocks))
    (thread-last (cdr (nreverse blocks))
      (seq-map
       (lambda (block)
         (let* ((path-line (seq-find (lambda (l) (string-prefix-p "worktree " l)) block))
                (branch-line (seq-find (lambda (l) (string-prefix-p "branch " l)) block))
                (wt-path (when path-line (substring path-line 9)))
                (branch (when branch-line
                          (replace-regexp-in-string
                           "^refs/heads/" "" (substring branch-line 7)))))
           (when wt-path
             (magit-gh-repo--make
              :name (format "%s@%s" main-name (or branch "detached"))
              :path wt-path
              :worktree t
              :branch (or branch "detached"))))))
      (seq-remove #'null))))

(defun magit-gh-repo-dashboard--discover-worktrees ()
  "Populate the unified cache with worktrees for all registered main repos."
  (seq-do
   (lambda (repo)
     (unless (magit-gh-repo-worktree repo)
       (let* ((path (magit-gh-repo-path repo))
              (lines (ignore-errors
                       (let ((default-directory path))
                         (process-lines magit-git-executable
                                        "worktree" "list" "--porcelain"))))
              (found (when lines (magit-gh-repo-dashboard--parse-worktrees path lines))))
         (magit-gh--cache-set path :worktrees found))))
   magit-gh-repo-list))

(defun magit-gh-repo-dashboard--parse-submodules (main-path lines)
  "Parse LINES from `git submodule status' for repo at MAIN-PATH.
Returns a list of `magit-gh-repo' structs, one per initialized submodule.
Name is formatted as \"parent<submod>\" where parent is the basename of MAIN-PATH."
  (let ((main-name (file-name-nondirectory (directory-file-name main-path))))
    (thread-last lines
      (seq-filter (lambda (l) (not (string-empty-p l))))
      (seq-map
       (lambda (line)
         (when (string-match "^[-+U ]\\([0-9a-f]+\\) \\([^ ]+\\)" line)
           (let* ((rel-path (match-string 2 line))
                  (abs-path (expand-file-name rel-path main-path))
                  (submod-name (file-name-nondirectory (directory-file-name rel-path))))
             (when (file-directory-p abs-path)
               (magit-gh-repo--make
                :name (format "%s<%s>" main-name submod-name)
                :path abs-path
                :submodule t))))))
      (seq-remove #'null))))

(defun magit-gh-repo-dashboard--discover-submodules ()
  "Populate the unified cache with submodules for all registered main repos."
  (seq-do
   (lambda (repo)
     (unless (or (magit-gh-repo-worktree repo) (magit-gh-repo-submodule repo))
       (let* ((path (magit-gh-repo-path repo))
              (lines (ignore-errors
                       (let ((default-directory path))
                         (process-lines magit-git-executable
                                        "submodule" "status"))))
              (found (when lines (magit-gh-repo-dashboard--parse-submodules path lines))))
         (magit-gh--cache-set path :submodules found))))
   magit-gh-repo-list))

(defun magit-gh-repo-overview--worktrees-for (path)
  "Return worktree structs for the main repo at PATH, discovering lazily if needed."
  (let ((cached (magit-gh--cache-get path :worktrees)))
    (cond
     ((eq cached 'none) nil)
     (cached cached)
     (t
      (let* ((lines (ignore-errors
                      (let ((default-directory path))
                        (process-lines magit-git-executable
                                       "worktree" "list" "--porcelain"))))
             (found (when lines (magit-gh-repo-dashboard--parse-worktrees path lines))))
        (magit-gh--cache-set path :worktrees (or found 'none))
        found)))))

;;;; Column configuration

(defvar magit-gh-repo-dashboard-columns
  '((name . t) (branch . t) (fetched . t) (status . t) (worktree . t))
  "Alist of (COLUMN-SYMBOL . ENABLED) for the repository dashboard.
Persisted across sessions via `savehist-additional-variables'.")

(defconst magit-gh-repo-dashboard--all-columns
  '(name branch fetched status worktree)
  "All available dashboard columns in display order.")

(defconst magit-gh-repo-dashboard--column-defs
  '((branch   . ("Branch"  18 t))
    (fetched  . ("Fetched"  8 nil))
    (status   . ("Status"  10 nil))
    (worktree . ("Type"    10 nil)))
  "Alist of COLUMN-SYMBOL to (LABEL WIDTH SORTABLE) for non-name columns.")

(defun magit-gh-repo-dashboard--column-enabled-p (col)
  "Return non-nil when column COL is enabled in `magit-gh-repo-dashboard-columns'."
  (alist-get col magit-gh-repo-dashboard-columns t))

(defun magit-gh-repo-dashboard--active-columns ()
  "Return column symbols that are currently enabled, in display order."
  (seq-filter #'magit-gh-repo-dashboard--column-enabled-p
              magit-gh-repo-dashboard--all-columns))

(defun magit-gh-repo-dashboard-toggle-column (col)
  "Toggle visibility of column COL in the dashboard and refresh."
  (interactive
   (list (intern (completing-read "Toggle column: "
                                  (seq-map #'symbol-name
                                           magit-gh-repo-dashboard--all-columns)
                                  nil t))))
  (setf (alist-get col magit-gh-repo-dashboard-columns)
        (not (magit-gh-repo-dashboard--column-enabled-p col)))
  (magit-gh-repo-dashboard-refresh))

(defun magit-gh-repo-dashboard-toggle-discovered-submodules ()
  "Toggle visibility of auto-discovered submodules in the dashboard and refresh."
  (interactive)
  (setq magit-gh-repo-dashboard-show-discovered-submodules
        (not magit-gh-repo-dashboard-show-discovered-submodules))
  (magit-gh-repo-dashboard-refresh))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'magit-gh-repo-dashboard-columns))

;;;; Formatting helpers

(defun magit-gh-repo-dashboard--format-age (seconds)
  "Format SECONDS duration as a compact string, or \"never\" if nil."
  (cond
   ((null seconds) "never")
   ((< seconds 60) (format "%ds" (round seconds)))
   ((< seconds 3600) (format "%dm" (round (/ seconds 60))))
   ((< seconds 86400) (format "%dh" (round (/ seconds 3600))))
   (t (format "%dd" (round (/ seconds 86400))))))

(defun magit-gh-repo-dashboard--format-status (ahead behind dirty)
  "Format AHEAD, BEHIND, and DIRTY into a compact status indicator.
Each non-zero/non-nil value contributes a segment; segments are joined with
a single space.  Returns an empty string when everything is clean and synced."
  (let ((parts nil))
    (when (> ahead 0)
      (push (propertize (format "↑%d" ahead) 'face 'warning) parts))
    (when (> behind 0)
      (push (propertize (format "↓%d" behind) 'face 'font-lock-comment-face) parts))
    (when dirty
      (push (propertize "!" 'face 'error) parts))
    (mapconcat #'identity (nreverse parts) " ")))

(defun magit-gh-repo-dashboard--format-worktree (repo)
  "Format the type indicator for REPO.
Shows \"WT\" for worktrees, \"SUBM\" for submodules (auto-discovered or
explicitly-registered), and \"REPO\" for ordinary working-tree repos."
  (let ((path (magit-gh-repo-path repo)))
    (cond
     ((magit-gh-repo-worktree repo)
      (propertize "WT" 'face 'magit-gh-repo-branch-face))
     ((magit-gh-repo-submodule repo)
      (propertize "SUBM" 'face 'magit-gh-repo-branch-face))
     ((and magit-gh-repo-dashboard--submodule-path-set
           (gethash path magit-gh-repo-dashboard--submodule-path-set))
      (propertize "SUBM.TRACK" 'face 'magit-gh-repo-branch-face))
     (t (propertize "REPO" 'face 'shadow)))))

;;;; Repo dashboard mode

(defface magit-gh-repo-name-face
  '((t :inherit font-lock-keyword-face))
  "Face for repository names in the repo dashboard.")

(defface magit-gh-repo-branch-face
  '((t :inherit font-lock-string-face))
  "Face for branch names in the repo dashboard.")

(defun magit-gh-repo-dashboard--build-format (repos)
  "Return the tabulated-list format vector for REPOS using enabled columns.
The Name column width is elastic: wide enough for the longest name in REPOS."
  (let* ((name-width (seq-reduce (lambda (w r) (max w (length (magit-gh-repo-name r))))
                                 repos (length "Name")))
         (active (magit-gh-repo-dashboard--active-columns)))
    (apply #'vector
           (seq-map (lambda (col)
                      (if (eq col 'name)
                          `("Name" ,name-width t)
                        (alist-get col magit-gh-repo-dashboard--column-defs)))
                    active))))

(defvar magit-gh-repo-dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'magit-gh-repo-dashboard-view)
    (define-key m (kbd "g")   #'magit-gh-repo-dashboard-refresh)
    (define-key m (kbd "r")   #'magit-gh-repo-dashboard-refresh)
    (define-key m (kbd "!")   #'magit-gh-repo-dashboard-magit-dispatch)
    (define-key m (kbd "s")   #'magit-gh-repo-dashboard-magit-status)
    (define-key m (kbd "d")   #'magit-gh-repo-dashboard-magit-diff)
    (define-key m (kbd "D")   #'magit-gh-repo-dashboard-magit-diff-full)
    (define-key m (kbd "l")   #'magit-gh-repo-dashboard-magit-log)
    (define-key m (kbd "L")   #'magit-gh-repo-dashboard-magit-log-full)
    (define-key m (kbd "c")   #'magit-gh-repo-dashboard-magit-commit)
    (define-key m (kbd "C")   #'magit-gh-repo-dashboard-commit)
    (define-key m (kbd "f")   #'magit-gh-repo-dashboard-fetch)
    (define-key m (kbd "u")   #'magit-gh-repo-dashboard-pull)
    (define-key m (kbd "a")   #'magit-gh-repo-dashboard-auto-sync)
    (define-key m (kbd "n")   #'magit-gh-repo-dashboard-sync)
    (define-key m (kbd "S")   #'magit-gh-repo-dashboard-sync-all)
    (define-key m (kbd "A")   #'magit-gh-repo-dashboard-commit-all)
    (define-key m (kbd "x")   #'magit-gh-repo-dashboard-run-command)
    (define-key m (kbd "t")   #'magit-gh-repo-dashboard-filter-by-tag)
    (define-key m (kbd "i")   #'magit-gh-repo-dashboard-add-tag)
    (define-key m (kbd "p")   #'magit-gh-pr-dashboard-open)
    (define-key m (kbd "b")   #'magit-gh-repo-dashboard-visit-buffer)
    (define-key m (kbd "e")   #'magit-gh-repo-dashboard-find-file)
    (define-key m (kbd "B")   #'magit-gh-repo-dashboard-switch-branch)
    (define-key m (kbd "y")   #'magit-gh-repo-dashboard-prune-branches)
    (define-key m (kbd "P")   #'magit-gh-repo-dashboard-push)
    (define-key m (kbd "G")   #'magit-gh-repo-dashboard-stage-all)
    (define-key m (kbd "w")   #'magit-gh-repo-dashboard-worktree-add)
    (define-key m (kbd "k")   #'magit-gh-repo-dashboard-worktree-delete)
    (define-key m (kbd "T")   #'magit-gh-repo-dashboard-toggle-column)
    (define-key m (kbd "M-s") #'magit-gh-repo-dashboard-toggle-discovered-submodules)
    (define-key m (kbd "j")   #'magit-gh-repo-dashboard-builder)
    (define-key m (kbd "z")   #'magit-gh-repo-dashboard-agent-shell)
    (define-key m (kbd "Z")   #'magit-gh-repo-dashboard-agent-shell-new)
    (define-key m (kbd "Q")   #'magit-gh-repo-dashboard-agent-shell-queue)
    (define-key m (kbd "SPC") #'magit-gh-repo-dashboard-toggle-mark)
    (define-key m (kbd "*")   #'magit-gh-repo-dashboard-unmark-all)
    (define-key m (kbd "F")   #'magit-gh-repo-dashboard-fetch-all)
    (define-key m (kbd "U")   #'magit-gh-repo-dashboard-pull-all)
    (define-key m (kbd "m")   #'magit-gh-repo-dashboard-menu)
    (define-key m (kbd "?")   #'magit-gh-repo-dashboard-menu)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for `magit-gh-repo-dashboard-mode'.")

(defvar-local magit-gh-repo-dashboard--tag-filter nil
  "When non-nil, a symbol: only repos tagged with this symbol are shown.")

(defvar magit-gh-repo-dashboard--ephemeral-tags (make-hash-table :test #'equal)
  "Hash table mapping repo path strings to lists of ephemeral tag symbols.
These tags are session-local and are not saved to the repo registry.")

(defun magit-gh-repo-dashboard--all-tags-for (repo)
  "Return the combined permanent and ephemeral tags for REPO."
  (append (magit-gh-repo-tags repo)
          (gethash (magit-gh-repo-path repo) magit-gh-repo-dashboard--ephemeral-tags)))

(defun magit-gh-repo-dashboard--permanent-tag-set ()
  "Return deduplicated list of all permanent tag symbols across registered repos."
  (delete-dups (seq-mapcat #'magit-gh-repo-tags magit-gh-repo-list)))

(defvar-local magit-gh-repo-dashboard--marked-paths nil
  "List of repo paths currently marked for batch operations.")

(defvar magit-gh-repo-dashboard-show-discovered-submodules t
  "When non-nil, auto-discovered submodules appear below their parent in the dashboard.")

(defvar magit-gh-repo-dashboard--submodule-path-set nil
  "Hash table mapping auto-discovered submodule path → \"parent<mod>\" display name.
Rebuilt on each refresh. Used to detect explicitly-registered repos that are also
submodules and to derive their parent<mod> display name.")

(define-derived-mode magit-gh-repo-dashboard-mode tabulated-list-mode "Repos"
  "Major mode for the registered repository dashboard."
  (setq tabulated-list-format (magit-gh-repo-dashboard--build-format magit-gh-repo-list))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun magit-gh-repo-dashboard--build-entry (repo)
  "Return a `tabulated-list-entries' entry for REPO using enabled columns."
  (let* ((stats (magit-gh-repo-dashboard--get-stats repo))
         (active (magit-gh-repo-dashboard--active-columns)))
    (list repo
          (apply #'vector
                 (seq-map
                  (lambda (col)
                    (pcase col
                      ('name
                       (let* ((subm-name (and magit-gh-repo-dashboard--submodule-path-set
                                              (gethash (magit-gh-repo-path repo)
                                                       magit-gh-repo-dashboard--submodule-path-set)))
                              (display (or subm-name (magit-gh-repo-name repo))))
                         (propertize display
                                     'face (if (member (magit-gh-repo-path repo)
                                                       magit-gh-repo-dashboard--marked-paths)
                                               '(bold magit-gh-repo-name-face)
                                             'magit-gh-repo-name-face))))
                      ('branch
                       (propertize (let ((b (plist-get stats :branch)))
                                     (if (and b (not (string-empty-p b)))
                                         b
                                       (or (magit-gh-repo-branch repo) "?")))
                                   'face 'magit-gh-repo-branch-face))
                      ('fetched
                       (magit-gh-repo-dashboard--format-age (plist-get stats :fetch-age)))
                      ('status
                       (magit-gh-repo-dashboard--format-status
                        (or (plist-get stats :ahead) 0)
                        (or (plist-get stats :behind) 0)
                        (plist-get stats :dirty)))
                      ('worktree
                       (magit-gh-repo-dashboard--format-worktree repo))))
                  active)))))

(defun magit-gh-repo-dashboard--sorted-repos (repos)
  "Return REPOS sorted by :sort-hint, with discovered worktrees following each parent.
Repos without a sort-hint follow all sorted ones.
Auto-discovered submodules whose path is already in `magit-gh-repo-list' are
suppressed to avoid duplicate rows — the registered entry is shown instead."
  (let* ((sorted (seq-sort (lambda (a b)
                             (let ((ha (magit-gh-repo-sort-hint a))
                                   (hb (magit-gh-repo-sort-hint b)))
                               (cond
                                ((and ha hb) (< ha hb))
                                (ha t)
                                (hb nil)
                                (t nil))))
                           repos))
         (registered-paths (let ((paths (make-hash-table :test #'equal)))
                             (seq-do (lambda (r)
                                       (puthash (magit-gh-repo-path r) t paths))
                                     magit-gh-repo-list)
                             paths)))
    (seq-mapcat (lambda (repo)
                  (cons repo
                        (append (magit-gh--cache-get (magit-gh-repo-path repo) :worktrees)
                                (when magit-gh-repo-dashboard-show-discovered-submodules
                                  (seq-remove
                                   (lambda (sm)
                                     (gethash (magit-gh-repo-path sm) registered-paths))
                                   (magit-gh--cache-get (magit-gh-repo-path repo) :submodules))))))
                sorted)))

(defun magit-gh-repo-dashboard-refresh ()
  "Discover worktrees, invalidate the stats cache, and repopulate the table.
When `magit-gh-repo-dashboard--tag-filter' is set, shows only matching repos.
Repos are ordered by :sort-hint; discovered worktrees follow their parent."
  (interactive)
  (clrhash magit-gh--cache)
  (magit-gh-repo-dashboard--discover-worktrees)
  (magit-gh-repo-dashboard--discover-submodules)
  (setq magit-gh-repo-dashboard--submodule-path-set
        (let ((paths (make-hash-table :test #'equal)))
          (seq-do (lambda (repo)
                    (seq-do (lambda (sm)
                              (puthash (magit-gh-repo-path sm) (magit-gh-repo-name sm) paths))
                            (or (magit-gh--cache-get (magit-gh-repo-path repo) :submodules) '())))
                  magit-gh-repo-list)
          paths))
  (let ((repos (magit-gh-repo-dashboard--sorted-repos
                (if magit-gh-repo-dashboard--tag-filter
                    (seq-filter (lambda (r)
                                  (memq magit-gh-repo-dashboard--tag-filter
                                        (magit-gh-repo-dashboard--all-tags-for r)))
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

(defun magit-gh-repo-dashboard--repo-at-point-p ()
  "Return t if current point is on a `magit-gh-repo-dashboard' repo."
  (and
   (derived-mode-p (current-buffer) 'magit-gh-repo-dashboard-mode)
   (tabulated-list-get-id)
   t))

(defun magit-gh-repo-dashboard-view ()
  "Open the overview buffer for the repository at point."
  (interactive)
  (magit-gh-repo-overview--open (magit-gh-repo-dashboard--repo-at-point)))

(defun magit-gh-repo-dashboard-magit-dispatch ()
  "Open `magit-dispatch' in the context of the repository at point."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'magit-dispatch)))

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

(defun magit-gh-repo-dashboard-stage-all ()
  "Stage all changes in the repository at point."
  (interactive)
  (let ((repo (magit-gh-repo-dashboard--repo-at-point)))
    (if (magit-gh-repo-dashboard--stage-all repo)
        (message "magit-gh: staged all changes in %s" (magit-gh-repo-name repo))
      (message "magit-gh: stage all failed in %s" (magit-gh-repo-name repo)))))

(defun magit-gh-repo-dashboard-push ()
  "Push current branch to its push remote for the repository at point."
  (interactive)
  (let ((default-directory (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))))
    (magit-push-current-to-pushremote nil)))

(defun magit-gh-repo-dashboard-sync ()
  "Run the auto-sync operation for the repository at point asynchronously.
Signals `user-error' when :auto-sync is not configured for this repo."
  (interactive)
  (let ((repo (magit-gh-repo-dashboard--repo-at-point)))
    (unless (magit-gh-repo-auto-sync repo)
      (user-error "Auto-sync is not configured for %s" (magit-gh-repo-name repo)))
    (magit-gh-repo-dashboard--dispatch-sync-op
     repo
     (lambda (status &optional error-text)
       (magit-gh-repo-dashboard--log-operation
        (magit-gh-repo-name repo) "sync" status error-text)
       (magit-gh-repo-dashboard--maybe-refresh)))))

(defun magit-gh-repo-dashboard-commit-all ()
  "Auto-commit repos with :auto-commit configured, asynchronously.
Uses each repo's :auto-commit message function (or the default chore message).
Displays a summary message and refreshes the dashboard when all complete."
  (interactive)
  (let ((repos (seq-filter #'magit-gh-repo-auto-commit magit-gh-repo-list)))
    (when (null repos)
      (user-error "No repositories have :auto-commit configured"))
    (magit-gh-repo-dashboard--batch-run
     repos
     #'magit-gh-repo-dashboard--auto-commit-async
     "magit-gh commit"
     (lambda (_) (magit-gh-repo-dashboard--maybe-refresh)))))

(defun magit-gh-repo-dashboard-sync-all ()
  "Run auto-sync operation for all configured repos asynchronously, then refresh."
  (interactive)
  (let ((repos (seq-filter #'magit-gh-repo-auto-sync magit-gh-repo-list)))
    (when (null repos)
      (user-error "No repositories have :auto-sync configured"))
    (magit-gh-repo-dashboard--batch-run
     repos
     #'magit-gh-repo-dashboard--dispatch-sync-op
     "magit-gh sync"
     (lambda (_) (magit-gh-repo-dashboard--maybe-refresh)))))

(defun magit-gh-repo-dashboard-auto-sync ()
  "Auto-commit and auto-sync all configured repos asynchronously.
Runs git commit for repos with :auto-commit set (using their message function)
and the configured auto-sync operation for repos with :auto-sync set, concurrently.
Each batch displays a summary message; the dashboard refreshes when sync completes."
  (interactive)
  (let ((commit-repos (seq-filter #'magit-gh-repo-auto-commit magit-gh-repo-list))
        (sync-repos (seq-filter #'magit-gh-repo-auto-sync magit-gh-repo-list)))
    (when (and (null commit-repos) (null sync-repos))
      (user-error "No repos have :auto-commit or :auto-sync configured"))
    (when commit-repos
      (magit-gh-repo-dashboard--batch-run
       commit-repos
       #'magit-gh-repo-dashboard--auto-commit-async
       "magit-gh autosync commit"))
    (when sync-repos
      (magit-gh-repo-dashboard--batch-run
       sync-repos
       #'magit-gh-repo-dashboard--dispatch-sync-op
       "magit-gh autosync sync"
       (lambda (_) (magit-gh-repo-dashboard--maybe-refresh))))))

(defun magit-gh-repo-dashboard-run-command ()
  "Open ACR picker for the repo at point and invoke the selected command."
  (interactive)
  (magit-gh-repo-dashboard--run-command-for (magit-gh-repo-dashboard--repo-at-point)))

(defun magit-gh-repo-dashboard--build-tag-table ()
  "Build an ACR alist mapping tag-name strings to annotation strings.
Each annotation lists the count of repos using the tag and up to four names.
Permanent tags (from repo :tags fields) are sorted before ephemeral-only tags."
  (let* ((permanent-set (magit-gh-repo-dashboard--permanent-tag-set))
         (tag-repo-map (let ((ht (make-hash-table :test #'eq)))
                         (seq-do
                          (lambda (repo)
                            (seq-do
                             (lambda (tag)
                               (puthash tag (cons repo (gethash tag ht)) ht))
                             (magit-gh-repo-dashboard--all-tags-for repo)))
                          magit-gh-repo-list)
                         ht))
         (all-tags (delete-dups
                    (seq-mapcat #'magit-gh-repo-dashboard--all-tags-for
                                magit-gh-repo-list)))
         (sorted-tags (seq-sort
                       (lambda (a b)
                         (let ((a-perm (memq a permanent-set))
                               (b-perm (memq b permanent-set)))
                           (cond
                            ((and a-perm (not b-perm)) t)
                            ((and b-perm (not a-perm)) nil)
                            (t (string< (symbol-name a) (symbol-name b))))))
                       all-tags)))
    (seq-map
     (lambda (tag)
       (let* ((repos (nreverse (gethash tag tag-repo-map)))
              (count (length repos))
              (shown (seq-take (seq-map #'magit-gh-repo-name repos) 4)))
         (cons (symbol-name tag)
               (format "%d repo%s: %s%s"
                       count
                       (if (= count 1) "" "s")
                       (mapconcat #'identity shown ", ")
                       (if (> count (length shown)) "…" "")))))
     sorted-tags)))

(cl-defun magit-gh-repo-dashboard--read-tag (prompt &key include-clear require-match)
  "Read a tag using annotated-completing-read with PROMPT.
Shows permanent tags (from repo :tags fields) before ephemeral-only tags.
Each candidate is annotated with a repo count and up to four repo names.

When INCLUDE-CLEAR is non-nil a \"(clear)\" option is prepended; selecting it
returns the symbol `clear'.  When REQUIRE-MATCH is non-nil only existing tags
are accepted; otherwise arbitrary input is allowed for new ephemeral tags.
Returns an interned symbol, `clear', or nil on quit."
  (let* ((permanent-set (magit-gh-repo-dashboard--permanent-tag-set))
         (full-table (if include-clear
                         (cons '("(clear)" . "remove tag filter")
                               (magit-gh-repo-dashboard--build-tag-table))
                       (magit-gh-repo-dashboard--build-tag-table)))
         (group-fn (lambda (candidate)
                     (cond
                      ((equal candidate "(clear)") " ")
                      ((memq (intern candidate) permanent-set) "permanent")
                      (t "ephemeral"))))
         (sort-fn (lambda (candidates)
                    (let ((perm permanent-set))
                      (seq-sort
                       (lambda (a b)
                         (cond
                          ((equal a "(clear)") t)
                          ((equal b "(clear)") nil)
                          (t (let ((a-perm (memq (intern a) perm))
                                   (b-perm (memq (intern b) perm)))
                               (cond
                                ((and a-perm (not b-perm)) t)
                                ((and b-perm (not a-perm)) nil)
                                (t (string< a b)))))))
                       candidates))))
         (result (annotated-completing-read
                  full-table
                  :prompt prompt
                  :group-name group-fn
                  :sort-fn sort-fn
                  :require-match require-match
                  :or-nil t)))
    (cond
     ((null result) nil)
     ((equal result "(clear)") 'clear)
     (t (intern result)))))

(defun magit-gh-repo-dashboard-filter-by-tag ()
  "Filter the dashboard by tag using annotated completion.
Select \"(clear)\" to show all repos; quitting leaves the current filter unchanged."
  (interactive)
  (when-let* ((tag (magit-gh-repo-dashboard--read-tag "Filter by tag: "
                                                       :include-clear t)))
    (setq magit-gh-repo-dashboard--tag-filter
          (unless (eq tag 'clear) tag))
    (magit-gh-repo-dashboard-refresh)))

(defun magit-gh-repo-dashboard-add-tag ()
  "Add an ephemeral session-local tag to the repository at point.
The tag is stored in memory for this session and is not saved to the registry.
Pick from existing tags or type a new symbol name."
  (interactive)
  (let* ((repo (magit-gh-repo-dashboard--repo-at-point))
         (tag (magit-gh-repo-dashboard--read-tag
               (format "Add tag to %s: " (magit-gh-repo-name repo)))))
    (when tag
      (let* ((path (magit-gh-repo-path repo))
             (existing (gethash path magit-gh-repo-dashboard--ephemeral-tags)))
        (unless (memq tag existing)
          (puthash path (cons tag existing) magit-gh-repo-dashboard--ephemeral-tags))
        (magit-gh-repo-dashboard-refresh)))))

(defun magit-gh-repo-dashboard-mark-by-tag ()
  "Mark all repos sharing a tag chosen via annotated completion.
Adds to any existing marks rather than replacing them."
  (interactive)
  (when-let* ((tag (magit-gh-repo-dashboard--read-tag "Mark by tag: " :require-match t)))
    (let* ((tagged (seq-filter
                    (lambda (r)
                      (memq tag (magit-gh-repo-dashboard--all-tags-for r)))
                    magit-gh-repo-list))
           (paths (seq-map #'magit-gh-repo-path tagged)))
      (setq magit-gh-repo-dashboard--marked-paths
            (delete-dups (append paths magit-gh-repo-dashboard--marked-paths)))
      (magit-gh-repo-dashboard-refresh)
      (message "Marked %d repo%s with tag '%s"
               (length paths)
               (if (= (length paths) 1) "" "s")
               (symbol-name tag)))))

(defun magit-gh-repo-dashboard-visit-buffer ()
  "Switch to a buffer visiting a file in the repository at point."
  (interactive)
  (let* ((repo (magit-gh-repo-dashboard--repo-at-point))
         (path (magit-gh-repo-path repo))
         (bufs (seq-filter (lambda (b)
                             (string-prefix-p path (or (buffer-file-name b) "")))
                           (buffer-list))))
    (when (null bufs)
      (user-error "No open buffers visiting files in %s" (magit-gh-repo-name repo)))
    (switch-to-buffer
     (completing-read "Visit buffer: " (seq-map #'buffer-name bufs) nil t))))

(defun magit-gh-repo-dashboard-find-file ()
  "Open a file in the repository at point."
  (interactive)
  (find-file
   (read-file-name "Find file: "
                   (file-name-as-directory
                    (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))))))

(defun magit-gh-repo-dashboard-switch-branch ()
  "Switch branch in the repository at point via magit."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'magit-checkout)))

(defun magit-gh-repo-dashboard-prune-branches ()
  "Prune merged branches in the repository at point."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (magit-gh-prune-merged-branches)))

(defun magit-gh-repo-dashboard--at-worktree-p ()
  "Return non-nil when the repo at point is a worktree."
  (when-let* ((repo (ignore-errors (magit-gh-repo-dashboard--repo-at-point))))
    (magit-gh-repo-worktree repo)))

(defun magit-gh-repo-dashboard-worktree-add ()
  "Add a new worktree for the repository at point via magit."
  (interactive)
  (let ((repo (magit-gh-repo-dashboard--repo-at-point)))
    (when (magit-gh-repo-worktree repo)
      (user-error "Cannot add a worktree from a worktree entry"))
    (magit-gh--with-repo-dir (magit-gh-repo-path repo)
      (call-interactively #'magit-worktree-checkout))
    (magit-gh-repo-dashboard-refresh)))

(defun magit-gh-repo-dashboard-worktree-delete ()
  "Delete the worktree at point via magit.
Signals `user-error' when the current row is not a worktree."
  (interactive)
  (let ((repo (magit-gh-repo-dashboard--repo-at-point)))
    (unless (magit-gh-repo-worktree repo)
      (user-error "Not a worktree row; use 'k' only on worktree entries"))
    (magit-gh--with-repo-dir (magit-gh-repo-path repo)
      (call-interactively #'magit-worktree-delete))
    (magit-gh-repo-dashboard-refresh)))

(defun magit-gh-repo-dashboard-builder ()
  "Run `builder-compile-project' in the repository at point."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'builder-compile-project)))

(defun magit-gh-repo-dashboard-agent-shell ()
  "Switch to an agent-shell session for the repository at point."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'agent-shell-switch-project-session)))

(defun magit-gh-repo-dashboard-agent-shell-new ()
  "Start a new agent-shell session in the repository at point."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))
    (call-interactively #'agent-shell-new-shell)))

(defun magit-gh-repo-dashboard-agent-shell-queue ()
  "Open the agent-shell queue buffer."
  (interactive)
  (call-interactively #'agent-shell-queue-buffer-open))


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

(defvar-local magit-gh-repo-overview--stats nil
  "Cached stats plist for this overview buffer, or nil when loading.")

(defvar-local magit-gh-repo-overview--pr-counts nil
  "Cached PR counts cons (TOTAL . MINE) for this overview buffer, or nil when loading.")

(defvar magit-gh-repo-overview-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'magit-gh-repo-overview-follow)
    (define-key m (kbd "!")   #'magit-gh-repo-overview-magit-dispatch)
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
    (define-key m (kbd "b")   #'magit-gh-repo-overview-visit-buffer)
    (define-key m (kbd "e")   #'magit-gh-repo-overview-find-file)
    (define-key m (kbd "B")   #'magit-gh-repo-overview-switch-branch)
    (define-key m (kbd "y")   #'magit-gh-repo-overview-prune-branches)
    (define-key m (kbd "P")   #'magit-gh-repo-overview-push)
    (define-key m (kbd "G")   #'magit-gh-repo-overview-stage-all)
    (define-key m (kbd "w")   #'magit-gh-repo-overview-worktree-add)
    (define-key m (kbd "k")   #'magit-gh-repo-overview-worktree-delete)
    (define-key m (kbd "j")   #'magit-gh-repo-overview-builder)
    (define-key m (kbd "z")   #'magit-gh-repo-overview-agent-shell)
    (define-key m (kbd "Z")   #'magit-gh-repo-overview-agent-shell-new)
    (define-key m (kbd "Q")   #'magit-gh-repo-overview-agent-shell-queue)
    (define-key m (kbd "m")   #'magit-gh-repo-overview-menu)
    (define-key m (kbd "?")   #'magit-gh-repo-overview-menu)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for repo overview buffers.")

(defun magit-gh-repo-overview--current-repo ()
  "Return the repo for the current overview buffer or signal `user-error'."
  (or magit-gh-repo-overview--repo
      (user-error "No repository associated with this buffer")))

(defun magit-gh-repo-overview-magit-dispatch ()
  "Open `magit-dispatch' in the context of this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'magit-dispatch)))

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

(defun magit-gh-repo-overview-stage-all ()
  "Stage all changes in the current overview's repository."
  (interactive)
  (let ((repo (magit-gh-repo-overview--current-repo)))
    (if (magit-gh-repo-dashboard--stage-all repo)
        (progn
          (message "magit-gh: staged all changes in %s" (magit-gh-repo-name repo))
          (magit-gh-repo-overview-refresh))
      (message "magit-gh: stage all failed in %s" (magit-gh-repo-name repo)))))

(defun magit-gh-repo-overview-push ()
  "Push current branch to its push remote for this overview's repository."
  (interactive)
  (let ((default-directory (magit-gh-repo-path (magit-gh-repo-overview--current-repo))))
    (magit-push-current-to-pushremote nil)))

(defun magit-gh-repo-overview-run-command ()
  "Open ACR picker for this overview's repository and invoke the selected command."
  (interactive)
  (magit-gh-repo-dashboard--run-command-for (magit-gh-repo-overview--current-repo)))

(defun magit-gh-repo-overview-visit-buffer ()
  "Switch to a buffer visiting a file in this overview's repository."
  (interactive)
  (let* ((repo (magit-gh-repo-overview--current-repo))
         (path (magit-gh-repo-path repo))
         (bufs (seq-filter (lambda (b)
                             (string-prefix-p path (or (buffer-file-name b) "")))
                           (buffer-list))))
    (when (null bufs)
      (user-error "No open buffers visiting files in %s" (magit-gh-repo-name repo)))
    (switch-to-buffer
     (completing-read "Visit buffer: " (seq-map #'buffer-name bufs) nil t))))

(defun magit-gh-repo-overview-find-file ()
  "Open a file in this overview's repository."
  (interactive)
  (find-file
   (read-file-name "Find file: "
                   (file-name-as-directory
                    (magit-gh-repo-path (magit-gh-repo-overview--current-repo))))))

(defun magit-gh-repo-overview-switch-branch ()
  "Switch branch in this overview's repository via magit."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'magit-checkout)))

(defun magit-gh-repo-overview-prune-branches ()
  "Prune merged branches in this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (magit-gh-prune-merged-branches)))

(defun magit-gh-repo-overview--is-worktree-p ()
  "Return non-nil when the current overview buffer shows a worktree."
  (when-let* ((repo (ignore-errors (magit-gh-repo-overview--current-repo))))
    (magit-gh-repo-worktree repo)))

(defun magit-gh-repo-overview-worktree-add ()
  "Add a new worktree for this overview's repository via magit.
Signals `user-error' when already viewing a worktree."
  (interactive)
  (let ((repo (magit-gh-repo-overview--current-repo)))
    (when (magit-gh-repo-worktree repo)
      (user-error "Cannot add a worktree from a worktree overview"))
    (magit-gh--with-repo-dir (magit-gh-repo-path repo)
      (call-interactively #'magit-worktree-checkout))
    (magit-gh-repo-overview-refresh)))

(defun magit-gh-repo-overview-worktree-delete ()
  "Delete this worktree via magit.
Signals `user-error' when the current overview is not a worktree."
  (interactive)
  (let ((repo (magit-gh-repo-overview--current-repo)))
    (unless (magit-gh-repo-worktree repo)
      (user-error "Not a worktree; use 'k' only on worktree overviews"))
    (magit-gh--with-repo-dir (magit-gh-repo-path repo)
      (call-interactively #'magit-worktree-delete))
    (quit-window)))

(defun magit-gh-repo-overview-builder ()
  "Run `builder-compile-project' in this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'builder-compile-project)))

(defun magit-gh-repo-overview-agent-shell ()
  "Switch to an agent-shell session for this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'agent-shell-switch-project-session)))

(defun magit-gh-repo-overview-agent-shell-new ()
  "Start a new agent-shell session in this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
    (call-interactively #'agent-shell-new-shell)))

(defun magit-gh-repo-overview-agent-shell-queue ()
  "Open the agent-shell queue buffer."
  (interactive)
  (call-interactively #'agent-shell-queue-buffer-open))


(defun magit-gh-repo-overview--classify-files (lines)
  "Classify git status porcelain LINES into a categorised alist.
Each LINE has the form \"XY filename\" where XY is the two-character status.
Returns alist with keys `staged', `unstaged', `deleted', `untracked'.
Staged/unstaged deletions appear only under `deleted', not the other buckets."
  (let ((staged nil) (unstaged nil) (deleted nil) (untracked nil))
    (seq-do
     (lambda (raw)
       (when (>= (length raw) 3)
         (let* ((x (aref raw 0))
                (y (aref raw 1))
                (file (string-trim (substring raw 3))))
           (cond
            ((and (eq x ?\?) (eq y ?\?)) (push file untracked))
            (t
             (when (or (eq x ?D) (eq y ?D)) (push file deleted))
             (when (and (not (eq x ? )) (not (eq x ?D)) (not (eq x ?\?)))
               (push file staged))
             (when (and (not (eq y ? )) (not (eq y ?D)) (not (eq y ?\?)))
               (push file unstaged)))))))
     lines)
    (list (cons 'staged    (nreverse staged))
          (cons 'unstaged  (nreverse unstaged))
          (cons 'deleted   (nreverse deleted))
          (cons 'untracked (nreverse untracked)))))

(defun magit-gh-repo-overview--insert-file-section (label files)
  "Insert section LABEL followed by each file in FILES, indented.
Inserts nothing when FILES is empty."
  (when files
    (insert (format "  %s:\n" label))
    (seq-do (lambda (f) (insert (format "    %s\n" f))) files)))

(defun magit-gh-repo-overview--insert-kv (key value &optional value-face action)
  "Insert bold KEY (padded to 16 chars) followed by VALUE and a newline.
When ACTION is non-nil, tag the entire line with `magit-gh-repo-overview-action'
so `magit-gh-repo-overview-follow' can dispatch on it."
  (let ((start (point)))
    (insert (propertize (format "%-16s" (concat key ":")) 'face 'bold))
    (if value-face
        (insert (propertize (or value "") 'face value-face))
      (insert (or value "")))
    (insert "\n")
    (when action
      (put-text-property start (point) 'magit-gh-repo-overview-action action))))

(defun magit-gh-repo-overview--render (repo stats pr-counts)
  "Insert overview content for REPO into the current buffer.
STATS is a plist from `magit-gh-repo-dashboard--collect-stats-async', or nil
when still loading.  PR-COUNTS is a cons (TOTAL . MINE), or nil when loading."
  (if (null stats)
      (insert (propertize "Loading repository data...\n" 'face 'shadow))
    (let* ((path (magit-gh-repo-path repo))
           (branch (or (plist-get stats :branch) "?"))
           (behind (or (plist-get stats :behind) 0))
           (dirty (plist-get stats :dirty))
           (remote-origin (plist-get stats :remote-origin))
           (uncommitted-files (plist-get stats :uncommitted-files))
           (recent-log (plist-get stats :recent-log)))
      (magit-gh-repo-overview--insert-kv
       "Repository" (magit-gh-repo-name repo) nil (cons 'magit-status path))
      (magit-gh-repo-overview--insert-kv
       "Path" path nil (cons 'dired path))
      (when remote-origin
        (magit-gh-repo-overview--insert-kv "Remote" remote-origin))
      (magit-gh-repo-overview--insert-kv
       "Branch" branch 'magit-gh-repo-branch-face)
      (magit-gh-repo-overview--insert-kv
       "Behind"
       (if (> behind 0) (format "%d commits" behind) "up to date")
       (when (> behind 0) 'warning))
      (insert "\n")
      (if dirty
          (let* ((classified (magit-gh-repo-overview--classify-files uncommitted-files)))
            (insert (propertize "Uncommitted Files:\n" 'face 'bold))
            (magit-gh-repo-overview--insert-file-section "Staged"    (alist-get 'staged    classified))
            (magit-gh-repo-overview--insert-file-section "Unstaged"  (alist-get 'unstaged  classified))
            (magit-gh-repo-overview--insert-file-section "Deleted"   (alist-get 'deleted   classified))
            (magit-gh-repo-overview--insert-file-section "Untracked" (alist-get 'untracked classified))
            (unless (seq-some #'cdr classified)
              (insert (propertize "  (none)\n" 'face 'shadow))))
        (magit-gh-repo-overview--insert-kv "Changes" "clean"))
      (insert "\n")
      (insert (propertize "Pull Requests:\n" 'face 'bold))
      (cond
       ((null pr-counts)
        (insert (propertize "  loading...\n" 'face 'shadow)))
       ((= (car pr-counts) 0)
        (insert (propertize "  None\n" 'face 'shadow)))
       (t
        (insert (format "  Open:   %d\n" (car pr-counts)))
        (when (> (cdr pr-counts) 0)
          (insert (format "  Yours:  %d\n" (cdr pr-counts))))))
      (when (and recent-log (not (string-empty-p recent-log)))
        (insert "\n")
        (insert (propertize "Recent Commits:\n" 'face 'bold))
        (seq-do
         (lambda (line)
           (let ((start (point))
                 (hash (car (split-string line))))
             (insert (format "  %s\n" line))
             (when (and hash (not (string-empty-p hash)))
               (put-text-property start (point)
                                  'magit-gh-repo-overview-action
                                  (cons 'magit-show-commit hash)))))
         (split-string recent-log "\n")))
      (unless (magit-gh-repo-worktree repo)
        (when-let* ((worktrees (magit-gh-repo-overview--worktrees-for path)))
          (insert "\n")
          (insert (propertize "Worktrees:\n" 'face 'bold))
          (seq-do
           (lambda (wt)
             (let ((start (point)))
               (insert (format "  %-24s  %s\n"
                               (magit-gh-repo-name wt)
                               (magit-gh-repo-path wt)))
               (put-text-property start (point)
                                  'magit-gh-repo-overview-action
                                  (cons 'magit-status (magit-gh-repo-path wt)))))
           worktrees))))))

(defun magit-gh-repo-overview-follow ()
  "Perform the context-sensitive action for the line at point.
On the Repository line: open magit status for this repository.
On the Path line: open dired for this repository's directory.
On a Recent Commits line: show the commit in magit."
  (interactive)
  (when-let* ((action (get-text-property (point) 'magit-gh-repo-overview-action))
              (type (car action))
              (data (cdr action)))
    (pcase type
      ('magit-status (magit-status-setup-buffer data))
      ('dired (dired data))
      ('magit-show-commit
       (magit-gh--with-repo-dir (magit-gh-repo-path (magit-gh-repo-overview--current-repo))
         (magit-show-commit data))))))

(defun magit-gh-repo-overview--rerender ()
  "Clear and re-render the current overview buffer using buffer-local state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-gh-repo-overview--render
     magit-gh-repo-overview--repo
     magit-gh-repo-overview--stats
     magit-gh-repo-overview--pr-counts)
    (goto-char (point-min))))

(defun magit-gh-repo-overview--start-async-load (repo buf)
  "Start async stats then PR-count fetch for REPO; update BUF as each arrives."
  (magit-gh-repo-dashboard--collect-stats-async
   repo
   (lambda (stats)
     (when (buffer-live-p buf)
       (with-current-buffer buf
         (setq-local magit-gh-repo-overview--stats stats)
         (magit-gh-repo-overview--rerender)
         (magit-gh-repo-overview--pr-counts-async
          (magit-gh-repo-path repo)
          (lambda (counts)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq-local magit-gh-repo-overview--pr-counts counts)
                (magit-gh-repo-overview--rerender))))))))))

(defun magit-gh-repo-overview-refresh ()
  "Re-render the overview buffer with fresh stats fetched asynchronously."
  (interactive)
  (when-let* ((repo (magit-gh-repo-overview--current-repo)))
    (magit-gh--cache-remove (magit-gh-repo-path repo) :stats)
    (magit-gh--cache-remove (magit-gh-repo-path repo) :pr-counts)
    (setq-local magit-gh-repo-overview--stats nil)
    (setq-local magit-gh-repo-overview--pr-counts nil)
    (magit-gh-repo-overview--rerender)
    (magit-gh-repo-overview--start-async-load repo (current-buffer))))

(defun magit-gh-repo-overview--open (repo)
  "Pop to a read-only overview buffer for REPO, loading stats asynchronously."
  (let* ((buf-name (format "*magit-gh-repo: %s*" (magit-gh-repo-name repo)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory (magit-gh-repo-path repo))
        (setq-local magit-gh-repo-overview--repo repo)
        (setq-local magit-gh-repo-overview--stats nil)
        (setq-local magit-gh-repo-overview--pr-counts nil)
        (use-local-map magit-gh-repo-overview-mode-map)
        (magit-gh-repo-overview--render repo nil nil)
        (goto-char (point-min)))
      (setq buffer-read-only t))
    (pop-to-buffer buf)
    (magit-gh-repo-overview--start-async-load repo buf)) )

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
  "Return the comment count from PR alist."
  (let ((c (map-elt pr 'commentsCount)))
    (if (numberp c) c 0)))

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
               "number,title,state,author,updatedAt,commentsCount,reviewDecision,statusCheckRollup,isDraft,url")
         (when author (list "--author" author)))
      (append
       (list "search" "prs"
             "--state" (if (member state '("open" "closed")) state "open")
             "--json"
             "number,title,state,repository,author,updatedAt,commentsCount,reviewDecision,isDraft,url")
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

;;;; Transient predicates

(defun magit-gh-repo-dashboard--dirty-or-unknown-p ()
  "Return non-nil when the repo at point is dirty or its stats are not yet cached."
  (when-let* ((repo (ignore-errors (magit-gh-repo-dashboard--repo-at-point))))
    (let ((stats (magit-gh--cache-get (magit-gh-repo-path repo) :stats)))
      (or (null stats) (plist-get stats :dirty)))))

(defun magit-gh-repo-dashboard--has-auto-commit-p ()
  "Return non-nil when the repo at point has :auto-commit configured."
  (when-let* ((repo (ignore-errors (magit-gh-repo-dashboard--repo-at-point))))
    (not (null (magit-gh-repo-auto-commit repo)))))

(defun magit-gh-repo-dashboard--has-auto-sync-p ()
  "Return non-nil when the repo at point has :auto-sync configured."
  (when-let* ((repo (ignore-errors (magit-gh-repo-dashboard--repo-at-point))))
    (not (null (magit-gh-repo-auto-sync repo)))))

(defun magit-gh-repo-dashboard--has-commands-p ()
  "Return non-nil when the repo at point has commands registered."
  (when-let* ((repo (ignore-errors (magit-gh-repo-dashboard--repo-at-point))))
    (not (null (magit-gh-repo-commands repo)))))

(defun magit-gh-repo-overview--has-changes-p ()
  "Return non-nil when this overview's repository has uncommitted changes."
  (and magit-gh-repo-overview--stats
       (plist-get magit-gh-repo-overview--stats :dirty)))

(defun magit-gh-repo-overview--ahead-p ()
  "Return non-nil when this overview's repository has commits ahead of upstream."
  (and magit-gh-repo-overview--stats
       (> (or (plist-get magit-gh-repo-overview--stats :ahead) 0) 0)))

(defun magit-gh-repo-overview--has-auto-commit-p ()
  "Return non-nil when this overview's repository has :auto-commit configured."
  (when-let* ((repo (ignore-errors (magit-gh-repo-overview--current-repo))))
    (not (null (magit-gh-repo-auto-commit repo)))))

(defun magit-gh-repo-overview--has-commands-p ()
  "Return non-nil when this overview's repository has commands registered."
  (when-let* ((repo (ignore-errors (magit-gh-repo-overview--current-repo))))
    (not (null (magit-gh-repo-commands repo)))))

(defun magit-gh-repo-dashboard--repo-at-point-behind-p ()
  "Return non-nil when the repo at point has commits behind its upstream."
  (when-let* ((repo (ignore-errors (magit-gh-repo-dashboard--repo-at-point)))
              (stats (magit-gh-repo-dashboard--get-stats repo)))
    (and (> (or (plist-get stats :behind) 0) 0) t)))

(defun magit-gh-repo-dashboard--repo-at-point-ahead-p ()
  "Return non-nil when the repo at point has commits ahead of its upstream."
  (when-let* ((repo (ignore-errors (magit-gh-repo-dashboard--repo-at-point)))
              (stats (magit-gh-repo-dashboard--get-stats repo)))
    (and (> (or (plist-get stats :ahead) 0) 0) t)))

(defun magit-gh-repo-dashboard--can-add-worktree-p ()
  "Return non-nil when at a registered non-worktree repo (can add a worktree)."
  (when-let* ((repo (ignore-errors (magit-gh-repo-dashboard--repo-at-point))))
    (not (magit-gh-repo-worktree repo))))

;;;; Mark/select support

(defun magit-gh-repo-dashboard--update-entry-for (repo)
  "Regenerate the tabulated-list entry for REPO in place in `tabulated-list-entries'."
  (let ((new-entry (magit-gh-repo-dashboard--build-entry repo))
        (path (magit-gh-repo-path repo)))
    (setq tabulated-list-entries
          (seq-map (lambda (entry)
                     (if (equal (magit-gh-repo-path (car entry)) path)
                         new-entry
                       entry))
                   tabulated-list-entries))))

(defun magit-gh-repo-dashboard-toggle-mark ()
  "Toggle the mark on the repository at point and advance to the next line."
  (interactive)
  (let* ((repo (magit-gh-repo-dashboard--repo-at-point))
         (path (magit-gh-repo-path repo)))
    (setq magit-gh-repo-dashboard--marked-paths
          (if (member path magit-gh-repo-dashboard--marked-paths)
              (delete path magit-gh-repo-dashboard--marked-paths)
            (cons path magit-gh-repo-dashboard--marked-paths)))
    (magit-gh-repo-dashboard--update-entry-for repo)
    (tabulated-list-print t)
    (forward-line 1)))

(defun magit-gh-repo-dashboard-unmark-all ()
  "Clear all marks from the dashboard."
  (interactive)
  (setq magit-gh-repo-dashboard--marked-paths nil)
  (setq tabulated-list-entries
        (seq-map (lambda (entry)
                   (magit-gh-repo-dashboard--build-entry (car entry)))
                 tabulated-list-entries))
  (tabulated-list-print t))

(defun magit-gh-repo-dashboard--effective-repos ()
  "Return marked repos if any are marked, else all repos currently in the table."
  (let ((all (seq-map #'car tabulated-list-entries)))
    (if magit-gh-repo-dashboard--marked-paths
        (seq-filter (lambda (r)
                      (member (magit-gh-repo-path r)
                              magit-gh-repo-dashboard--marked-paths))
                    all)
      all)))

(defun magit-gh-repo-dashboard--has-marks-p ()
  "Return non-nil when at least one repository is marked."
  (not (null magit-gh-repo-dashboard--marked-paths)))

(defun magit-gh-repo-dashboard-fetch-all ()
  "Asynchronously fetch marked repos, or all visible repos when none are marked."
  (interactive)
  (let ((repos (magit-gh-repo-dashboard--effective-repos)))
    (when (null repos)
      (user-error "No repositories to fetch"))
    (magit-gh-repo-dashboard--batch-run
     repos
     #'magit-gh-repo-dashboard--fetch-async
     "magit-gh fetch"
     (lambda (_) (magit-gh-repo-dashboard--maybe-refresh)))))

(defun magit-gh-repo-dashboard-pull-all ()
  "Asynchronously pull marked repos, or all visible repos when none are marked."
  (interactive)
  (let ((repos (magit-gh-repo-dashboard--effective-repos)))
    (when (null repos)
      (user-error "No repositories to pull"))
    (magit-gh-repo-dashboard--batch-run
     repos
     #'magit-gh-repo-dashboard--pull-async
     "magit-gh pull"
     (lambda (_) (magit-gh-repo-dashboard--maybe-refresh)))))

;;;; Transient menus

(transient-define-prefix magit-gh-repo-dashboard-menu ()
  "Actions for the repository at point in the repo dashboard."
  [["Repository"
    ("!"  "Magit dispatch"   magit-gh-repo-dashboard-magit-dispatch
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("RET" "Open overview"   magit-gh-repo-dashboard-view
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("gs"   "Status"          magit-gh-repo-dashboard-magit-status
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("d"   "Diff (dwim)"     magit-gh-repo-dashboard-magit-diff
     :inapt-if-not magit-gh-repo-dashboard--dirty-or-unknown-p)
    ("D"   "Diff…"           magit-gh-repo-dashboard-magit-diff-full
     :inapt-if-not magit-gh-repo-dashboard--dirty-or-unknown-p)
    ("lc"   "Log (current)"   magit-gh-repo-dashboard-magit-log
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("lf"   "Log…"            magit-gh-repo-dashboard-magit-log-full
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("c"    "Commit"          magit-gh-repo-dashboard-magit-commit
     :inapt-if-not magit-gh-repo-dashboard--dirty-or-unknown-p)
    ("sa"   "Stage all"       magit-gh-repo-dashboard-stage-all
     :inapt-if-not magit-gh-repo-dashboard--dirty-or-unknown-p)
    ("fr"   "Fetch"            magit-gh-repo-dashboard-fetch
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("rp"   "Pull"             magit-gh-repo-dashboard-pull
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("rs"   "Push (repo send)" magit-gh-repo-dashboard-push
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-ahead-p)]
   ["Navigate"
    ("b"   "Visit buffer"    magit-gh-repo-dashboard-visit-buffer
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("ff"  "Find file"       magit-gh-repo-dashboard-find-file
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("gb"  "Switch branch"   magit-gh-repo-dashboard-switch-branch
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("y"   "Prune branches"  magit-gh-repo-dashboard-prune-branches
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)]
   ["Manage"
    ("ac"   "Auto-commit"     magit-gh-repo-dashboard-commit
     :inapt-if-not magit-gh-repo-dashboard--has-auto-commit-p)
    ("sy"   "Sync"            magit-gh-repo-dashboard-sync
     :inapt-if-not magit-gh-repo-dashboard--has-auto-sync-p)
    ("et"   "Add tag"         magit-gh-repo-dashboard-add-tag
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("t"    "Compile Project (builder)"  magit-gh-repo-dashboard-builder
     :inapt-if-not magit-gh-repo-dashboard--has-auto-commit-p)
    ("x"   "Run command"     magit-gh-repo-dashboard-run-command
     :inapt-if-not magit-gh-repo-dashboard--has-commands-p)]
   ["Build & Shell"
    ("as"   "Agent shell (project)"      magit-gh-repo-dashboard-agent-shell
     :inapt-if-not agent-shell-extras--same-project-buffers)
    ("an"   "New agent shell"            magit-gh-repo-dashboard-agent-shell-new)
    ("aq"   "Agent shell queue"          magit-gh-repo-dashboard-agent-shell-queue)]
   ["Worktree"
    ("w"   "Add worktree"    magit-gh-repo-dashboard-worktree-add
     :inapt-if-not magit-gh-repo-dashboard--can-add-worktree-p)
    ("k"   "Delete worktree" magit-gh-repo-dashboard-worktree-delete
     :inapt-if-not magit-gh-repo-dashboard--at-worktree-p)]
   ["Batch"
    ("SPC"  "Toggle mark"       magit-gh-repo-dashboard-toggle-mark
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("mt"   "Toggle mark"       magit-gh-repo-dashboard-toggle-mark
     :inapt-if-not magit-gh-repo-dashboard--repo-at-point-p)
    ("mu"   "Clear marks"       magit-gh-repo-dashboard-unmark-all
     :inapt-if-not magit-gh-repo-dashboard--has-marks-p)
    ("mfa"  "Fetch all/marked"  magit-gh-repo-dashboard-fetch-all)
    ("mpa"  "Pull all/marked"   magit-gh-repo-dashboard-pull-all)
    ("msa"  "Sync all"          magit-gh-repo-dashboard-sync-all)
    ("mca"  "Commit all"        magit-gh-repo-dashboard-commit-all)
    ("maa"  "Autosync all"      magit-gh-repo-dashboard-auto-sync)
    ("mbt"  "Mark by tag"       magit-gh-repo-dashboard-mark-by-tag)]
   ["Dashboard"
    ("mbpr" "Open PR dashboard" magit-gh-pr-dashboard-open)
    ("nt"   "Filter by tag"     magit-gh-repo-dashboard-filter-by-tag)
    ("C-t"  "Toggle column"     magit-gh-repo-dashboard-toggle-column)
    ("M-s"  "Toggle submodules" magit-gh-repo-dashboard-toggle-discovered-submodules)
    ("gg"   "Refresh"           magit-gh-repo-dashboard-refresh)
    ("q"    "Quit"              quit-window)]])

(transient-define-prefix magit-gh-repo-overview-menu ()
  "Magit actions for the repository shown in this overview buffer."
  [["Magit"
    ("!"  "Magit dispatch"   magit-gh-repo-overview-magit-dispatch)
    ("gs"   "Status"          magit-gh-repo-overview-magit-status)
    ("d"   "Diff (dwim)"     magit-gh-repo-overview-magit-diff
     :if magit-gh-repo-overview--has-changes-p)
    ("D"   "Diff…"           magit-gh-repo-overview-magit-diff-full
     :if magit-gh-repo-overview--has-changes-p)
    ("lc"  "Log (current)"   magit-gh-repo-overview-magit-log)
    ("lf"  "Log…"            magit-gh-repo-overview-magit-log-full)
    ("c"   "Commit"          magit-gh-repo-overview-magit-commit
     :if magit-gh-repo-overview--has-changes-p)
    ("sa"   "Stage all"       magit-gh-repo-overview-stage-all
     :if magit-gh-repo-overview--has-changes-p)
    ("fr"   "Fetch"            magit-gh-repo-overview-fetch)
    ("rp"   "Pull"             magit-gh-repo-overview-pull)
    ("rs"   "Push (repo send)" magit-gh-repo-overview-push
     :inapt-if-not magit-gh-repo-overview--ahead-p)]
   ["Navigate"
    ("b"   "Visit buffer"    magit-gh-repo-overview-visit-buffer)
    ("ff"  "Find file"       magit-gh-repo-overview-find-file)
    ("gb"  "Switch branch"   magit-gh-repo-overview-switch-branch)]
   ["Manage"
    ("t"   "Compile project (builder)"  magit-gh-repo-overview-builder
     :if (lambda () (featurep 'builder)))
    ("rx"   "Run command"                magit-gh-repo-overview-run-command
     :if magit-gh-repo-overview--has-commands-p)
    ("mp"   "Prune branches"  magit-gh-repo-overview-prune-branches)
    ("mc"   "Auto-commit"     magit-gh-repo-overview-commit
     :if magit-gh-repo-overview--has-auto-commit-p)]
   ["Agent Shell"
    ("as"   "Agent shell (project)"  magit-gh-repo-overview-agent-shell
     :if agent-shell-extras--same-project-buffers)
    ("an"   "New agent shell"        magit-gh-repo-overview-agent-shell-new)
    ("aq"   "Agent shell queue"      magit-gh-repo-overview-agent-shell-queue)]
   ["Worktree"
    ("w"   "Add worktree"    magit-gh-repo-overview-worktree-add
     :if-not magit-gh-repo-overview--is-worktree-p)
    ("k"   "Delete worktree" magit-gh-repo-overview-worktree-delete
     :if magit-gh-repo-overview--is-worktree-p)]
   ["View"
    ("gg"   "Refresh"        magit-gh-repo-overview-refresh)
    ("q"   "Quit"            quit-window)]])

(transient-define-prefix magit-gh-pr-dashboard-menu ()
  "Actions for the PR dashboard."
  [["Pull Request"
    ("RET" "Open in browser" magit-gh-pr-dashboard-open-browser)]
   ["Filter"
    ("fs" "Set filter…" magit-gh-pr-dashboard-set-filter)
    ("fc" "Clear all filters" magit-gh-pr-dashboard-clear-filters)]
   ["Dashboard"
    ("g" "Refresh" magit-gh-pr-dashboard-refresh)
    ("q" "Quit" quit-window)]])

(defun ad:magit-gh-repo-dashboard--quit-window (orig-fn &optional kill window)
  "Around advice for `quit-window': delete split window in dashboard buffers.
Only applies in `magit-gh-repo-dashboard-mode', `magit-gh-repo-overview-mode',
and `magit-gh-pr-dashboard-mode'.  Falls through otherwise."
  (if (or kill (one-window-p)
          (not (derived-mode-p 'magit-gh-repo-dashboard-mode
                               'magit-gh-repo-overview-mode
                               'magit-gh-pr-dashboard-mode)))
      (funcall orig-fn kill window)
    (delete-window (or window (selected-window)))))

(advice-add 'quit-window :around #'ad:magit-gh-repo-dashboard--quit-window)

(provide 'magit-gh-repo-dashboard)

;;; magit-gh-repo-dashboard.el ends here
