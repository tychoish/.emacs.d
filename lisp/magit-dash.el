;;; magit-dash.el --- Repository and PR dashboards for magit-gh -*- lexical-binding: t -*-

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
;; `magit-dash-open' — a tabulated-list view of registered
;; repositories showing branch, fetch time, behind status, and dirty state.
;; Press RET to open a per-repo overview buffer with PR counts and magit action
;; shortcuts.  Press m or ? for the transient actions menu.
;;
;; `magit-gh-pr-dashboard-open' — a tabulated-list view of pull requests with
;; filters for state, author, repo, and org.  Supports CI outcome, age, comment
;; count, and review decision columns.
;;
;; Register repositories with `magit-dash-register' or by adding structs
;; directly to `magit-dash-repo-list' with `add-to-list'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'magit)
(require 'magit-gh-extras)

(require 'annotated-completing-read)
(require 'sprite)

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
(declare-function agent-shell-switch-buffer "agent-shell-menu")
(declare-function agent-shell-switch-project-session "agent-shell-menu")
(declare-function agent-shell-new-shell "agent-shell")
(declare-function agent-shell-queue-buffer-open "agent-shell-queue")
(declare-function agent-shell-menu-project-buffers "agent-shell-menu")

(declare-function builder-compile-project "builder")
(declare-function magit-dash-bump-submodules-menu "magit-dash-bump-submodules")
(declare-function magit-gh-pr-dashboard-open "magit-gh-pr")
(declare-function magit-gh-pr-dashboard-mode "magit-gh-pr")

(defconst magit-dash-buffer-name "*magit-dash-repos*")

;;;; Repository registry

(cl-defstruct (magit-dash-repo (:constructor magit-dash-repo--make) (:copier nil))
  "Registry entry for a local git repository."
  name
  path
  (include-prs nil)
  (auto-fetch nil)
  (auto-pull nil)
  (auto-commit nil)
  (auto-push nil)
  (auto-sync-command nil)
  (tags nil)
  (commands nil)
  (sort-hint nil)
  (worktree nil)
  (submodule nil)
  (branch nil)
  (sync-branches nil))

(defvar magit-dash-repo-list '()
  "List of `magit-dash-repo' structs registered for dashboard display.
Use `magit-dash-register' to add entries.")

(cl-defun magit-dash-register (&key name path include-prs auto-fetch auto-pull auto-commit auto-push auto-sync-command tags commands sort-hint worktree sync-branches)
  "Register or replace a repository with NAME at absolute PATH.
Replaces any existing entry with the same name or path.

Keyword arguments:
  :include-prs    include in PR dashboard fetches.
  :auto-fetch     non-nil — run git fetch --all during auto-sync.
  :auto-pull      non-nil — run git pull during auto-sync (implies fetch).
                  Respects :sync-branches.
  :auto-commit    nil | t | FUNCTION — stage and commit dirty working tree.
                  FUNCTION receives the repo struct and returns a commit
                  message string.
  :auto-push      non-nil — run git push during auto-sync.
                  Respects :sync-branches.
  :auto-sync-command  nil | SYMBOL | STRING | FUNCTION — run a custom command
                  as the final auto-sync step.
                  SYMBOL: looked up as a label in :commands, runs its target.
                  STRING: executed as a shell command in the repo directory.
                  FUNCTION: called with (REPO ON-COMPLETE) where ON-COMPLETE
                  accepts `ok', `skipped', or `error'.
  :sync-branches  list of branch names on which auto-pull and auto-push are
                  permitted; nil means any branch.
  :tags           list of symbols for filtering in the dashboard.
  :commands       alist of (LABEL . FUNCTION) for the repo command picker.
  :sort-hint      number controlling display order; lower values appear first.
                  Repos without a sort-hint appear after all sorted repos.
  :worktree       non-nil when this entry represents a git worktree."
  (unless (and name path)
    (user-error "must specify name (%s) and path (%s)" name path))

  (let ((abs-path (expand-file-name path)))
    (setq magit-dash-repo-list
          (thread-last magit-dash-repo-list
            (seq-remove (lambda (r)
                          (or (equal name (magit-dash-repo-name r))
                              (equal abs-path (magit-dash-repo-path r)))))
            (append (list (magit-dash-repo--make
                           :name name
                           :path abs-path
                           :include-prs include-prs
                           :auto-fetch auto-fetch
                           :auto-pull auto-pull
                           :auto-commit auto-commit
                           :auto-push auto-push
                           :auto-sync-command auto-sync-command
                           :tags tags
                           :commands commands
                           :sort-hint sort-hint
                           :worktree worktree
                           :sync-branches sync-branches)))))))

;;;; Registry helpers

(defvar magit-dash-sync-trigger 'interactive
  "How the current auto-sync was triggered.
Bind to `timer' when invoking auto-sync from a timer; defaults to `interactive'.")

(defun magit-dash--default-commit-message (repo)
  "Return a default auto-commit message for REPO.
Includes hostname, Emacs instance ID, and the current sync trigger."
  (format "chore: auto-commit changes in %s [%s:%s, %s]"
          (magit-dash-repo-name repo)
          (sprite-system-name)
          (or (and (boundp 'sprite-instance-id) sprite-instance-id) "unknown")
          (symbol-name magit-dash-sync-trigger)))

(defun magit-dash--stage-all (repo)
  "Stage all changes in REPO. Returns t on success."
  (magit-gh--with-repo-dir (magit-dash-repo-path repo)
    (= 0 (magit-call-git "add" "-A"))))

(defun magit-dash--auto-commit (repo)
  "Stage all changes in REPO and commit using its :auto-commit message.
Returns t when the commit succeeds, nil otherwise."
  (let* ((auto-commit (magit-dash-repo-auto-commit repo))
         (message (if (functionp auto-commit)
                      (funcall auto-commit repo)
                    (magit-dash--default-commit-message repo))))
    (and (magit-dash--stage-all repo)
         (magit-gh--with-repo-dir (magit-dash-repo-path repo)
           (= 0 (magit-call-git "commit" "-m" message))))))

(defun magit-dash--run-command-for (repo)
  "Open an ACR command picker for REPO and invoke the selected command."
  (let ((commands (magit-dash-repo-commands repo)))
    (unless commands
      (user-error "No commands registered for %s" (magit-dash-repo-name repo)))
    (let* ((table (seq-map (lambda (cmd)
                             (cons (format "%s" (car cmd)) (symbol-name (cdr cmd))))
                           commands))
           (label (annotated-completing-read table
                                             :prompt (format "%s command: " (magit-dash-repo-name repo))
                                             :require-match t)))
      (when-let* ((fn (cdr (seq-find (lambda (cmd)
                                      (equal label (format "%s" (car cmd))))
                                    commands))))
        (magit-gh--with-repo-dir (magit-dash-repo-path repo)
          (call-interactively fn))))))

;;;; Stats collection

(defun magit-dash--fetch-age (path)
  "Return seconds since last git fetch for repo at PATH, or nil if never fetched."
  (when-let* ((fetch-head (expand-file-name ".git/FETCH_HEAD" path))
              (attrs (and (file-exists-p fetch-head) (file-attributes fetch-head))))
    (float-time (time-since (file-attribute-modification-time attrs)))))

(defun magit-dash--head-hash (path)
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
                       (buffer-string)))
      head)))

(defun magit-dash--collect-stats (repo)
  "Synchronously collect git stats for REPO and store them in the cache.
Returns a plist with keys :branch :remote-origin :behind :ahead :dirty
:uncommitted-files :fetch-age :head-hash :recent-log."
  (let* ((path (magit-dash-repo-path repo))
         (default-directory path)
         (branch (or (magit-git-string "branch" "--show-current") ""))
         (remote-origin (magit-git-string "config" "remote.origin.url"))
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
                      :fetch-age (magit-dash--fetch-age path)
                      :head-hash (magit-dash--head-hash path)
                      :recent-log recent-log)))
    (magit-gh--cache-set path :stats stats)
    stats))

(defun magit-dash--collect-stats-async (repo callback)
  "Collect git stats for REPO asynchronously; call CALLBACK with a stats plist.
Runs five git subcommands sequentially via `magit-dash--run-git',
accumulating their outputs before assembling the stats plist."
  (let* ((path (magit-dash-repo-path repo))
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
                                    :fetch-age (magit-dash--fetch-age path)
                                    :head-hash (magit-dash--head-hash path)
                                    :recent-log recent-log)))
                  (magit-gh--cache-set path :stats stats)
                  (funcall callback stats))
              (magit-dash--run-git
               path (car remaining)
               (lambda (output)
                 (setq outputs (append outputs (list output)))
                 (funcall run (cdr remaining)))
               (lambda (_ _)
                 (setq outputs (append outputs (list "")))
                 (funcall run (cdr remaining)))))))
    (funcall run commands)))

(defun magit-dash-overview--pr-counts-async (path callback)
  "Fetch open PR counts for repo at PATH asynchronously.
Checks the in-memory cache first; calls CALLBACK with (TOTAL . MINE)."
  (cond ((magit-gh--cache-get path :include-prs)
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
	(t 'disabled)))


(defun magit-dash--get-stats (repo)
  "Return cached stats for REPO, collecting synchronously if absent or stale.
The cache is invalidated when the HEAD commit hash changes.
For missing submodules, returns minimal placeholder stats."
  (let* ((path (magit-dash-repo-path repo))
         (cached (magit-gh--cache-get path :stats)))
    (cond
     ;; Missing submodules get placeholder stats
     ((eq (magit-dash-repo-submodule repo) 'missing)
      (list :branch "" :remote-origin nil :behind 0 :ahead 0
            :dirty nil :uncommitted-files nil :fetch-age nil
            :head-hash nil :recent-log ""))
     ;; Use cached if valid
     ((and cached
           (equal (magit-dash--head-hash path)
                  (plist-get cached :head-hash)))
      cached)
     ;; Otherwise collect fresh stats
     (t (condition-case err
            (magit-dash--collect-stats repo)
          (error
           (message "magit-dash: failed to collect stats for %s: %s"
                    (magit-dash-repo-name repo) (error-message-string err))
           ;; Return minimal stats on error
           (list :branch "?" :remote-origin nil :behind 0 :ahead 0
                 :dirty nil :uncommitted-files nil :fetch-age nil
                 :head-hash nil :recent-log "")))))))

;;;; Async git operations

(defun magit-dash--run-git (path args on-success &optional on-error)
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
               (message "magit-dash: git %s failed (%d): %s"
                        (car args) code output)))))))))

(defun magit-dash--fetch-async (repo on-complete)
  "Run git fetch for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text on failure."
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("fetch")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--pull-async (repo on-complete)
  "Run git pull for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text on failure."
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("pull")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--push-async (repo on-complete)
  "Run git push for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text on failure."
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("push")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--auto-commit-async (repo on-complete)
  "Stage all changes in REPO and commit using its :auto-commit message function.
Calls ON-COMPLETE with `ok' when committed, `skipped' when workdir is clean,
or `error' when git add or commit fails."
  (let* ((path (magit-dash-repo-path repo))
         (auto-commit (magit-dash-repo-auto-commit repo))
         (msg (if (functionp auto-commit)
                  (funcall auto-commit repo)
                (magit-dash--default-commit-message repo))))
    (magit-dash--run-git
     path '("status" "--porcelain")
     (lambda (porcelain)
       (if (string-empty-p porcelain)
           (funcall on-complete 'skipped)
         (magit-dash--run-git
          path '("add" "-A")
          (lambda (_)
            (magit-dash--run-git
             path (list "commit" "-m" msg)
             (lambda (_) (funcall on-complete 'ok))
             (lambda (output code)
               (funcall on-complete 'error (format "commit failed, exit %d: %s" code output)))))
          (lambda (output code)
            (funcall on-complete 'error (format "add failed, exit %d: %s" code output))))))
     (lambda (output code)
       (funcall on-complete 'error (format "status failed, exit %d: %s" code output))))))

(defun magit-dash--current-branch (path)
  "Return the current branch name for the repo at PATH synchronously."
  (let ((default-directory path))
    (string-trim (or (magit-git-string "branch" "--show-current") ""))))

(defun magit-dash--branch-allowed-p (repo)
  "Return current branch name if allowed by REPO's sync-branches, nil otherwise.
When sync-branches is nil any branch is allowed and the current branch is returned."
  (let* ((allowed (magit-dash-repo-sync-branches repo))
         (current (magit-dash--current-branch (magit-dash-repo-path repo))))
    (if (null allowed)
        current
      (and (member current allowed) current))))

(defun magit-dash--auto-fetch-async (repo on-complete)
  "Run git fetch --all for REPO asynchronously.
Calls ON-COMPLETE with `ok' on success or `error' and error text on failure."
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("fetch" "--all")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--auto-pull-async (repo on-complete)
  "Run git pull for REPO if current branch is in sync-branches.
Calls ON-COMPLETE with `ok', `skipped' (branch not allowed), or `error'."
  (if-let* ((branch (magit-dash--branch-allowed-p repo)))
      (magit-dash--run-git
       (magit-dash-repo-path repo)
       '("pull")
       (lambda (_) (funcall on-complete 'ok))
       (lambda (output code)
         (funcall on-complete 'error (format "exit %d: %s" code output))))
    (funcall on-complete 'skipped
             (format "branch %s not in sync-branches"
                     (magit-dash--current-branch
                      (magit-dash-repo-path repo))))))

(defun magit-dash--run-git-chain (path steps on-success on-complete)
  "Run git STEPS sequentially in PATH.
STEPS is a list of (ARGS . LABEL) pairs.  On success of all steps call
ON-SUCCESS with no args.  On any failure call ON-COMPLETE with `error'
and a message of the form \"LABEL failed, exit N: output\"."
  (if (null steps)
      (funcall on-success)
    (let* ((step (car steps))
           (args (car step))
           (label (cdr step)))
      (magit-dash--run-git
       path args
       (lambda (_)
         (magit-dash--run-git-chain
          path (cdr steps) on-success on-complete))
       (lambda (output code)
         (funcall on-complete 'error
                  (format "%s failed, exit %d: %s" label code output)))))))

(defun magit-dash--auto-push-async (repo on-complete)
  "Run git push for REPO if current branch is in sync-branches.
Calls ON-COMPLETE with `ok', `skipped' (branch not allowed), or `error'."
  (if-let* ((branch (magit-dash--branch-allowed-p repo)))
      (magit-dash--run-git
       (magit-dash-repo-path repo)
       '("push")
       (lambda (_) (funcall on-complete 'ok))
       (lambda (output code)
         (funcall on-complete 'error (format "exit %d: %s" code output))))
    (funcall on-complete 'skipped
             (format "branch %s not in sync-branches"
                     (magit-dash--current-branch
                      (magit-dash-repo-path repo))))))

(defun magit-dash--run-shell-string-async (cmd path on-complete)
  "Run shell command string CMD in PATH asynchronously.
Calls ON-COMPLETE with `ok' on exit 0, or `error' and output on failure."
  (let ((proc-buf (generate-new-buffer " *magit-gh-cmd*")))
    (with-current-buffer proc-buf
      (setq default-directory path))
    (make-process
     :name "magit-gh-cmd"
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
             (funcall on-complete 'error (format "exit %d: %s" code output)))))))))

(defun magit-dash--auto-sync-command-async (repo on-complete)
  "Run REPO's :auto-sync-command asynchronously.
Dispatches by type:
  symbol  — looked up as a label in :commands; runs its target (string or fn).
  string  — executed as a shell command in the repo directory.
  function — called with (REPO ON-COMPLETE)."
  (let ((cmd (magit-dash-repo-auto-sync-command repo))
        (path (magit-dash-repo-path repo)))
    (cond
     ((null cmd)
      (funcall on-complete 'skipped "no auto-sync-command configured"))
     ((and (symbolp cmd) (not (functionp cmd)))
      (if-let* ((entry (seq-find (lambda (c) (eq (car c) cmd))
                                 (magit-dash-repo-commands repo)))
                (target (cdr entry)))
          (cond
           ((stringp target)
            (magit-dash--run-shell-string-async target path on-complete))
           ((functionp target)
            (funcall target repo on-complete))
           (t (funcall on-complete 'error
                       (format "command %s has unsupported target type" cmd))))
        (funcall on-complete 'error
                 (format "command %s not found in :commands" cmd))))
     ((stringp cmd)
      (magit-dash--run-shell-string-async cmd path on-complete))
     ((functionp cmd)
      (funcall cmd repo on-complete))
     (t
      (funcall on-complete 'error "auto-sync-command must be a symbol, string, or function")))))

(defun magit-dash--auto-sync-steps (repo)
  "Return an ordered list of (LABEL . FN) pairs for REPO's configured auto ops.
Steps are: fetch (when :auto-fetch or :auto-pull), pull (when :auto-pull),
commit (when :auto-commit), push (when :auto-push), cmd (when :auto-sync-command)."
  (seq-filter
   #'identity
   (list
    (when (or (magit-dash-repo-auto-fetch repo) (magit-dash-repo-auto-pull repo))
      (cons "fetch" (lambda (cb) (magit-dash--auto-fetch-async repo cb))))
    (when (magit-dash-repo-auto-pull repo)
      (cons "pull" (lambda (cb) (magit-dash--auto-pull-async repo cb))))
    (when (magit-dash-repo-auto-commit repo)
      (cons "commit" (lambda (cb) (magit-dash--auto-commit-async repo cb))))
    (when (magit-dash-repo-auto-push repo)
      (cons "push" (lambda (cb) (magit-dash--auto-push-async repo cb))))
    (when (magit-dash-repo-auto-sync-command repo)
      (cons "cmd" (lambda (cb) (magit-dash--auto-sync-command-async repo cb)))))))

(defun magit-dash--run-step-chain (repo-name steps on-complete)
  "Run STEPS sequentially, logging each with bold REPO-NAME.
STEPS is a list of (LABEL . FN) pairs; FN is called with a callback.
Aborts on `error'; continues on `ok' or `skipped'.
Calls ON-COMPLETE with `ok' after all steps, or `error' on first failure."
  (if (null steps)
      (funcall on-complete 'ok)
    (let* ((step (car steps))
           (label (car step))
           (fn (cdr step)))
      (funcall fn
               (lambda (status &optional error-text)
                 (magit-dash--log-operation repo-name label status error-text)
                 (pcase status
                   ('error (funcall on-complete 'error error-text))
                   (_ (magit-dash--run-step-chain
                       repo-name (cdr steps) on-complete))))))))

(defun magit-dash--auto-sync-async (repo on-complete)
  "Run all configured auto operations for REPO sequentially.
Steps run in order: fetch, pull, commit, push — each only when configured.
auto-pull implies fetch. Each step is logged individually with the repo name.
Calls ON-COMPLETE with `ok', `skipped', or `error'."
  (let ((steps (magit-dash--auto-sync-steps repo)))
    (if (null steps)
        (funcall on-complete 'skipped "no auto operations configured")
      (magit-dash--run-step-chain
       (magit-dash-repo-name repo) steps on-complete))))

(defun magit-dash--log-operation (repo-name operation status &optional error-text)
  "Log REPO-NAME OPERATION with STATUS to *Messages*.
The current timestamp is attached as a tooltip (help-echo) on REPO-NAME.
When ERROR-TEXT is non-nil it is appended to the message."
  (let* ((ts (format-time-string "%Y-%m-%d %H:%M:%S"))
         (name (propertize repo-name 'face 'bold 'help-echo ts))
         (detail (if error-text (format " — %s" error-text) "")))
    (message "magit-dash: %s %s → %s%s" name operation (symbol-name status) detail)))

(defun magit-dash--batch-run (repos op-fn label &optional on-all-done)
  "Run OP-FN asynchronously on each repo in REPOS.
OP-FN is called as (op-fn REPO CALLBACK) where CALLBACK receives a status
symbol: `ok', `skipped', or `error'.
When all repos finish, display a LABEL summary message and optionally call
ON-ALL-DONE with an alist of (NAME . STATUS)."
  (let* ((remaining (list (length repos)))
         (results nil))
    (message "magit-dash: starting %s batch operation" label)
    (seq-do
     (lambda (repo)
       (funcall op-fn repo
                (lambda (status &optional error-text)
                  (magit-dash--log-operation
                   (magit-dash-repo-name repo) label status error-text)
                  (push (cons (magit-dash-repo-name repo) status) results)
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
                      (message "magit-dash: ending %s batch operation" label)
                      (when on-all-done
                        (funcall on-all-done results)))))))
     repos)))

(defun magit-dash--maybe-refresh ()
  "Refresh the repo dashboard buffer if it is currently live."
  (when-let* ((buf (get-buffer magit-dash-buffer-name))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (magit-dash-refresh))))

;;;; Cache management

(defun magit-dash-cache-info ()
  "Display cache statistics in the minibuffer."
  (interactive)
  (let* ((total (length magit-dash-repo-list))
         (discovered-wt (seq-count #'magit-dash-repo-worktree magit-dash-repo-list))
         (discovered-subm (seq-count (lambda (r) 
                                       (and (magit-dash-repo-submodule r)
                                            (not (eq (magit-dash-repo-submodule r) 'missing))))
                                     magit-dash-repo-list))
         (configured (- total discovered-wt discovered-subm))
         (cached (hash-table-count magit-gh--cache)))
    (message "Repos: %d configured + %d worktrees + %d submodules = %d tracked | Cache: %d entries"
             configured discovered-wt discovered-subm total cached)))

(defun magit-dash-cache-reset (&optional repo-path)
  "Reset cache for REPO-PATH (or all repos if nil)."
  (interactive)
  (if repo-path
      (progn
        (magit-gh--cache-remove repo-path)
        (message "Cleared cache for %s" repo-path))
    (clrhash magit-gh--cache)
    (message "Cleared entire cache")))

(defun magit-dash-cache-reset-all ()
  "Clear cache and repopulate stats for all repos."
  (interactive)
  (clrhash magit-gh--cache)
  (magit-dash--discover-worktrees)
  (magit-dash--discover-submodules)
  (let ((failed nil))
    (dolist (repo magit-dash-repo-list)
      (condition-case err
          (magit-dash--collect-stats repo)
        (error
         (push (cons (magit-dash-repo-name repo) (error-message-string err)) failed))))
    (message "Cache reset: %d/%d repos updated%s"
             (- (length magit-dash-repo-list) (length failed))
             (length magit-dash-repo-list)
             (if failed (format " (%d failed)" (length failed)) "")))
  (when (get-buffer magit-dash-buffer-name)
    (magit-dash-refresh)))

(defun magit-dash-cache-reset-at-point ()
  "Reset cache for repository at point."
  (interactive)
  (when-let ((repo (magit-dash--repo-at-point)))
    (let ((path (magit-dash-repo-path repo))
          (name (magit-dash-repo-name repo)))
      (magit-gh--cache-remove path)
      (condition-case err
          (progn
            (magit-dash--collect-stats repo)
            (magit-dash--maybe-refresh)
            (message "Cache reset for %s" name))
        (error
         (message "Failed to collect stats for %s: %s" name (error-message-string err)))))))

;;;; Worktree support

(defun magit-dash--parse-worktrees (main-path lines)
  "Parse LINES from `git worktree list --porcelain' for repo at MAIN-PATH.
Returns a list of `magit-dash-repo' structs for additional worktrees.
The first block (the main worktree) is always skipped."
  (let ((main-name (file-name-nondirectory (directory-file-name main-path)))
	blocks current)
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
             (magit-dash-repo--make
              :name (format "%s@%s" main-name (or branch "detached"))
              :path wt-path
              :worktree t
              :branch (or branch "detached"))))))
      (seq-remove #'null))))

(defun magit-dash--discover-worktrees ()
  "Populate the unified cache with worktrees for all registered main repos."
  (seq-do
   (lambda (repo)
     (unless (magit-dash-repo-worktree repo)
       (let* ((path (magit-dash-repo-path repo))
              (lines (ignore-errors
                       (let ((default-directory path))
                         (process-lines magit-git-executable
                                        "worktree" "list" "--porcelain"))))
              (found (when lines (magit-dash--parse-worktrees path lines))))
         (magit-gh--cache-set path :worktrees found))))
   magit-dash-repo-list))

(defun magit-dash--parse-submodules (main-path lines)
  "Parse LINES from `git submodule status' for repo at MAIN-PATH.
Returns a list of `magit-dash-repo' structs, one per submodule (initialized or not).
Name is formatted as \"parent<submod>\" where parent is the basename of MAIN-PATH.
Missing/uninitialized submodules are marked with :submodule 'missing."
  (let ((main-name (file-name-nondirectory (directory-file-name main-path))))
    (thread-last lines
      (seq-filter (lambda (l) (not (string-empty-p l))))
      (seq-map
       (lambda (line)
         (when (string-match "^\\([-+U ]\\)\\([0-9a-f]+\\) \\([^ ]+\\)" line)
           (let* ((prefix (match-string 1 line))
                  (rel-path (match-string 3 line))
                  (abs-path (expand-file-name rel-path main-path))
                  (submod-name (file-name-nondirectory (directory-file-name rel-path)))
                  (missing-p (or (string= prefix "-")
                                 (not (file-directory-p abs-path)))))
             (magit-dash-repo--make
              :name (format "%s<%s>" main-name submod-name)
              :path abs-path
              :submodule (if missing-p 'missing t))))))
      (seq-remove #'null))))

(defun magit-dash--discover-submodules ()
  "Populate the unified cache with submodules for all registered main repos."
  (thread-last
    magit-dash-repo-list
    (seq-remove (lambda (repo) (or (magit-dash-repo-worktree repo) (magit-dash-repo-submodule repo))))
    (seq-do
     (lambda (repo)
       (let* ((path (magit-dash-repo-path repo))
              (lines (ignore-errors
                       (let ((default-directory path))
                         (process-lines magit-git-executable
                                        "submodule" "status")))))
         (magit-gh--cache-set path
	  :submodules (when lines
			(magit-dash--parse-submodules path lines))))))))

(defun magit-dash-overview--worktrees-for (path)
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
             (found (when lines (magit-dash--parse-worktrees path lines))))
        (magit-gh--cache-set path :worktrees (or found 'none))
        found)))))

;;;; Column configuration

(defvar magit-dash-columns
  '((name . t) (branch . t) (fetched . t) (status . t) (worktree . t) (sync . t) (cached . nil))
  "Alist of (COLUMN-SYMBOL . ENABLED) for the repository dashboard.
Persisted across sessions via `savehist-additional-variables'.")

(defconst magit-dash--all-columns
  '(name branch fetched status worktree sync cached)
  "All available dashboard columns in display order.")

(defconst magit-dash--column-defs
  '((branch   . ("Branch"  18 t))
    (fetched  . ("Fetched"  8 nil))
    (status   . ("Status"  10 nil))
    (worktree . ("Type"    10 nil))
    (sync     . ("Sync"     8 nil))
    (cached   . ("Cached"   7 nil)))
  "Alist of COLUMN-SYMBOL to (LABEL WIDTH SORTABLE) for non-name columns.")

(defun magit-dash--column-enabled-p (col)
  "Return non-nil when column COL is enabled in `magit-dash-columns'."
  (alist-get col magit-dash-columns t))

(defun magit-dash--active-columns ()
  "Return column symbols that are currently enabled, in display order."
  (seq-filter #'magit-dash--column-enabled-p
              magit-dash--all-columns))

(defun magit-dash-toggle-column (col)
  "Toggle visibility of column COL in the dashboard and refresh."
  (interactive
   (list (intern (completing-read "Toggle column: "
                                  (seq-map #'symbol-name
                                           magit-dash--all-columns)
                                  nil t))))
  (setf (alist-get col magit-dash-columns)
        (not (magit-dash--column-enabled-p col)))
  (magit-dash-refresh))

(defun magit-dash-toggle-discovered-submodules ()
  "Toggle visibility of auto-discovered submodules in the dashboard and refresh."
  (interactive)
  (setq magit-dash-show-discovered-submodules
        (not magit-dash-show-discovered-submodules))
  (magit-dash-refresh))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'magit-dash-columns))

;;;; Formatting helpers

(defun magit-dash--format-age (seconds)
  "Format SECONDS duration as a compact string, or \"never\" if nil."
  (cond
   ((null seconds) "never")
   ((< seconds 60) (format "%ds" (round seconds)))
   ((< seconds 3600) (format "%dm" (round (/ seconds 60))))
   ((< seconds 86400) (format "%dh" (round (/ seconds 3600))))
   (t (format "%dd" (round (/ seconds 86400))))))

(defun magit-dash--format-status (ahead behind dirty)
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

(defun magit-dash--format-worktree (repo)
  "Format the type indicator for REPO.
Shows \"WT\" for worktrees, \"SUBM\" for initialized submodules,
\"SUBM.EMPTY\" for missing/uninitialized submodules, \"SUBM.TRACK\" for
explicitly-registered submodules, and \"REPO\" for ordinary working-tree repos."
  (let ((path (magit-dash-repo-path repo))
        (submodule (magit-dash-repo-submodule repo)))
    (cond
     ((magit-dash-repo-worktree repo)
      (propertize "WT" 'face 'magit-dash-repo-branch-face))
     ((eq submodule 'missing)
      (propertize "SUBM.EMPTY" 'face 'warning))
     (submodule
      (propertize "SUBM" 'face 'magit-dash-repo-branch-face))
     ((and magit-dash--submodule-path-set
           (gethash path magit-dash--submodule-path-set))
      (propertize "SUBM.TRACK" 'face 'magit-dash-repo-branch-face))
     (t (propertize "REPO" 'face 'shadow)))))

(defun magit-dash--format-sync (repo)
  "Format a compact sync indicator for REPO based on configured auto operations.
Each enabled operation contributes its name; names are joined with \"+\".
Returns an empty string when nothing is set."
  (let ((parts (seq-filter
                #'identity
                (list
                 (when (magit-dash-repo-auto-fetch repo)        "fetch")
                 (when (magit-dash-repo-auto-pull repo)         "pull")
                 (when (magit-dash-repo-auto-commit repo)       "commit")
                 (when (magit-dash-repo-auto-push repo)         "push")
                 (when (magit-dash-repo-auto-sync-command repo) "cmd")))))
    (if parts
        (propertize (mapconcat #'identity parts "+") 'face 'magit-dash-repo-branch-face)
      "")))

;;;; Repo dashboard mode

(defface magit-dash-repo-name-face
  '((t :inherit font-lock-keyword-face))
  "Face for repository names in the repo dashboard.")

(defface magit-dash-repo-branch-face
  '((t :inherit font-lock-string-face))
  "Face for branch names in the repo dashboard.")

(defun magit-dash--build-format (repos)
  "Return the tabulated-list format vector for REPOS using enabled columns.
The Name column width is elastic: wide enough for the longest name in REPOS."
  (let* ((name-width (seq-reduce (lambda (w r) (max w (length (magit-dash-repo-name r))))
                                 repos (length "Name")))
         (active (magit-dash--active-columns)))
    (apply #'vector
           (seq-map (lambda (col)
                      (if (eq col 'name)
                          `("Name" ,name-width t)
                        (alist-get col magit-dash--column-defs)))
                    active))))

(defvar magit-dash-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'magit-dash-view)
    (define-key m (kbd "g")   #'magit-dash-refresh)
    (define-key m (kbd "r")   #'magit-dash-refresh)
    (define-key m (kbd "!")   #'magit-dash-magit-dispatch)
    (define-key m (kbd "s")   #'magit-dash-magit-status)
    (define-key m (kbd "d")   #'magit-dash-magit-diff)
    (define-key m (kbd "l")   #'magit-dash-magit-log)
    (define-key m (kbd "L")   #'magit-dash-magit-log-full)
    (define-key m (kbd "c")   #'magit-dash-magit-commit)
    (define-key m (kbd "C")   #'magit-dash-commit)
    (define-key m (kbd "f")   #'magit-dash-fetch)
    (define-key m (kbd "u")   #'magit-dash-pull)
    (define-key m (kbd "a")   #'magit-dash-auto-sync)
    (define-key m (kbd "n")   #'magit-dash-sync)
    (define-key m (kbd "S")   #'magit-dash-sync-all)
    (define-key m (kbd "A")   #'magit-dash-commit-all)
    (define-key m (kbd "x")   #'magit-dash-run-command)
    (define-key m (kbd "t")   #'magit-dash-filter-by-tag)
    (define-key m (kbd "i")   #'magit-dash-add-tag)
    (define-key m (kbd "p")   #'magit-gh-pr-dashboard-open)
    (define-key m (kbd "b")   #'magit-dash-visit-buffer)
    (define-key m (kbd "e")   #'magit-dash-find-file)
    (define-key m (kbd "B")   #'magit-dash-switch-branch)
    (define-key m (kbd "y")   #'magit-dash-prune-branches)
    (define-key m (kbd "P")   #'magit-dash-push)
    (define-key m (kbd "G")   #'magit-dash-stage-all)
    (define-key m (kbd "w")   #'magit-dash-worktree-add)
    (define-key m (kbd "k")   #'magit-dash-worktree-delete)
    (define-key m (kbd "T")   #'magit-dash-toggle-column)
    (define-key m (kbd "M-s") #'magit-dash-toggle-discovered-submodules)
    (define-key m (kbd "j")   #'magit-dash-builder)
    (define-key m (kbd "z")   #'magit-dash-agent-shell)
    (define-key m (kbd "Z")   #'magit-dash-agent-shell-new)
    (define-key m (kbd "Q")   #'magit-dash-agent-shell-queue)
    (define-key m (kbd "SPC") #'magit-dash-toggle-mark)
    (define-key m (kbd "*")   #'magit-dash-unmark-all)
    (define-key m (kbd "F")   #'magit-dash-fetch-all)
    (define-key m (kbd "U")   #'magit-dash-pull-all)
    (define-key m (kbd "m")   #'magit-dash-menu)
    (define-key m (kbd "?")   #'magit-dash-menu)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for `magit-dash-mode'.")

(defvar-local magit-dash--tag-filter nil
  "When non-nil, a symbol: only repos tagged with this symbol are shown.")

(defvar magit-dash--ephemeral-tags (make-hash-table :test #'equal)
  "Hash table mapping repo path strings to lists of ephemeral tag symbols.
These tags are session-local and are not saved to the repo registry.")

(defun magit-dash--all-tags-for (repo)
  "Return the combined permanent and ephemeral tags for REPO."
  (append (magit-dash-repo-tags repo)
          (gethash (magit-dash-repo-path repo) magit-dash--ephemeral-tags)))

(defun magit-dash--permanent-tag-set ()
  "Return deduplicated list of all permanent tag symbols across registered repos."
  (delete-dups (seq-mapcat #'magit-dash-repo-tags magit-dash-repo-list)))

(defvar-local magit-dash--marked-paths nil
  "List of repo paths currently marked for batch operations.")

(defvar magit-dash-show-discovered-submodules t
  "When non-nil, auto-discovered submodules appear below their parent in the dashboard.")

(defvar magit-dash--submodule-path-set nil
  "Hash table mapping auto-discovered submodule path → \"parent<mod>\" display name.
Rebuilt on each refresh. Used to detect explicitly-registered repos that are also
submodules and to derive their parent<mod> display name.")

(define-derived-mode magit-dash-mode tabulated-list-mode "Repos"
  "Major mode for the registered repository dashboard."
  (setq tabulated-list-format (magit-dash--build-format magit-dash-repo-list))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun magit-dash--build-entry (repo)
  "Return a `tabulated-list-entries' entry for REPO using enabled columns."
  (let* ((stats (magit-dash--get-stats repo))
         (active (magit-dash--active-columns)))
    (list repo
          (apply #'vector
                 (seq-map
                  (lambda (col)
                    (pcase col
                      ('name
                       (let* ((subm-name (and magit-dash--submodule-path-set
                                              (gethash (magit-dash-repo-path repo) magit-dash--submodule-path-set)))
                              (is-missing (eq (magit-dash-repo-submodule repo) 'missing))
                              (is-marked (member (magit-dash-repo-path repo) magit-dash--marked-paths))
                              (display (if subm-name subm-name (magit-dash-repo-name repo)))
                              (base-face (if is-marked
                                            '(bold magit-dash-repo-name-face)
                                          'magit-dash-repo-name-face))
                              (final-face (if (and subm-name is-missing)
                                             (list '(:strike-through t) base-face)
                                           base-face)))
                         (propertize display 'face final-face)))
                      ('branch
                       (propertize (let ((b (plist-get stats :branch)))
                                     (if (and b (not (string-empty-p b)))
                                         b
                                       (or (magit-dash-repo-branch repo) "?")))
                                   'face 'magit-dash-repo-branch-face))
                      ('fetched
                       (magit-dash--format-age (plist-get stats :fetch-age)))
                      ('status
                       (magit-dash--format-status
                        (or (plist-get stats :ahead) 0)
                        (or (plist-get stats :behind) 0)
                        (plist-get stats :dirty)))
                      ('worktree
                       (magit-dash--format-worktree repo))
                      ('sync
                       (magit-dash--format-sync repo))
                      ('cached
                       (if (magit-gh--cache-get (magit-dash-repo-path repo) :stats)
                           (propertize "✓" 'face 'success)
                         (propertize "·" 'face 'shadow)))))
                  active)))))

(defun magit-dash--repo-type-rank (repo)
  "Return a sort rank for REPO based on its git context type.
0 = plain repo, 1 = worktree, 2 = tracked submodule, 3 = missing submodule."
  (+ (magit-dash-repo-sort-hint repo)
     (cond
      ((magit-dash-repo-worktree repo) 1)
      ((eq (magit-dash-repo-submodule repo) 'missing) 5)
      ((and magit-dash--submodule-path-set
            (map-elt magit-dash--submodule-path-set (magit-dash-repo-path repo))) 5)
      ((magit-dash-repo-submodule repo) 2)
      (t 0))))

(defun magit-dash--sorted-repos (repos)
  "Return REPOS sorted by :sort-hint then type, with discovered worktrees following each parent.
Primary sort is :sort-hint ascending (nil hints follow all sorted ones).
Secondary sort within equal hints is by type: repo < worktree < submodule < missing.
Auto-discovered submodules whose path is already in `magit-dash-repo-list' are
suppressed to avoid duplicate rows — the registered entry is shown instead."
  (let* ((sorted (seq-sort (lambda (a b)
                             (let ((ha (magit-dash-repo-sort-hint a))
                                   (hb (magit-dash-repo-sort-hint b)))
                               (cond
                                ((and ha hb)
                                 (if (= ha hb)
                                     (< (magit-dash--repo-type-rank a)
                                        (magit-dash--repo-type-rank b))
                                   (< ha hb)))
                                (ha t)
                                (hb nil)
                                (t (< (magit-dash--repo-type-rank a)
                                      (magit-dash--repo-type-rank b))))))
                           repos))
         (registered-paths (let ((paths (make-hash-table :test #'equal)))
                             (seq-do (lambda (r)
                                       (puthash (magit-dash-repo-path r) t paths))
                                     magit-dash-repo-list)
                             paths)))
    (seq-mapcat (lambda (repo)
                  (cons repo
                        (append (magit-gh--cache-get (magit-dash-repo-path repo) :worktrees)
                                (when magit-dash-show-discovered-submodules
                                  (seq-remove
                                   (lambda (sm)
                                     (gethash (magit-dash-repo-path sm) registered-paths))
                                   (magit-gh--cache-get (magit-dash-repo-path repo) :submodules))))))
                sorted)))

(defun magit-dash-refresh ()
  "Discover worktrees, invalidate the stats cache, and repopulate the table.
When `magit-dash--tag-filter' is set, shows only matching repos.
Repos are ordered by :sort-hint; discovered worktrees follow their parent."
  (interactive)
  (clrhash magit-gh--cache)
  (magit-dash--discover-worktrees)
  (magit-dash--discover-submodules)
  (setq magit-dash--submodule-path-set
        (let ((paths (make-hash-table :test #'equal)))
          (seq-do (lambda (repo)
                    (seq-do (lambda (sm)
                              (puthash (magit-dash-repo-path sm) (magit-dash-repo-name sm) paths))
                            (or (magit-gh--cache-get (magit-dash-repo-path repo) :submodules) '())))
                  magit-dash-repo-list)
          paths))
  (let ((repos (magit-dash--sorted-repos
                (if magit-dash--tag-filter
                    (seq-filter (lambda (r)
                                  (memq magit-dash--tag-filter
                                        (magit-dash--all-tags-for r)))
                                magit-dash-repo-list)
                  magit-dash-repo-list))))
    (setq tabulated-list-format (magit-dash--build-format repos))
    (tabulated-list-init-header)
    (setq tabulated-list-entries (seq-map #'magit-dash--build-entry repos))
    (tabulated-list-print t)))

(defun magit-dash--repo-at-point ()
  "Return the `magit-dash-repo' struct at point or signal `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No repository at point")))

(defun magit-dash--repo-at-point-p ()
  "Return t if current point is on a `magit-dash' repo."
  (and
   (derived-mode-p (current-buffer) 'magit-dash-mode)
   (tabulated-list-get-id)
   t))

(defun magit-dash-view ()
  "Open the overview buffer for the repository at point."
  (interactive)
  (magit-dash-overview--open (magit-dash--repo-at-point)))

(defun magit-dash-magit-dispatch ()
  "Open `magit-dispatch' in the context of the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-dispatch)))

(defun magit-dash-magit-status ()
  "Open a magit status buffer for the repository at point."
  (interactive)
  (magit-status-setup-buffer
   (magit-dash-repo-path (magit-dash--repo-at-point))))

(defun magit-dash-magit-diff ()
  "Open a magit diff (dwim) buffer for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-diff)))

(defun magit-dash-magit-log ()
  "Open magit log for the current branch in the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-log-current)))

(defun magit-dash-magit-log-full ()
  "Open the full magit log menu for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-log)))

(defun magit-dash-magit-commit ()
  "Open a magit commit buffer for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-commit-create)))

(defun magit-dash-fetch ()
  "Run git fetch for the repository at point via magit."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-fetch)))

(defun magit-dash-pull ()
  "Pull from upstream for the repository at point via magit."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-pull-from-upstream)))

(defun magit-dash-commit ()
  "Auto-commit changes in the repository at point.
Signals `user-error' when :auto-commit is not configured for this repo."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash-repo-auto-commit repo)
      (user-error "Auto-commit is not configured for %s" (magit-dash-repo-name repo)))
    (if (magit-dash--auto-commit repo)
        (message "magit-dash: committed changes in %s" (magit-dash-repo-name repo))
      (message "magit-dash: nothing to commit or commit failed in %s"
               (magit-dash-repo-name repo)))))

(defun magit-dash-stage-all ()
  "Stage all changes in the repository at point."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (if (magit-dash--stage-all repo)
        (message "magit-dash: staged all changes in %s" (magit-dash-repo-name repo))
      (message "magit-dash: stage all failed in %s" (magit-dash-repo-name repo)))))

(defun magit-dash-push ()
  "Push current branch to its push remote for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (magit-push-current-to-pushremote nil)))

(defun magit-dash-sync ()
  "Run the configured auto operations for the repository at point asynchronously.
Signals `user-error' when no auto operations are configured for this repo."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash--auto-sync-steps repo)
      (user-error "No auto operations configured for %s" (magit-dash-repo-name repo)))
    (magit-dash--auto-sync-async
     repo
     (lambda (_status &optional _error-text)
       (magit-dash--maybe-refresh)))))

(defun magit-dash-commit-all ()
  "Auto-commit repos with :auto-commit configured, asynchronously.
Uses each repo's :auto-commit message function (or the default chore message).
Displays a summary message and refreshes the dashboard when all complete."
  (interactive)
  (let ((repos (seq-filter #'magit-dash-repo-auto-commit magit-dash-repo-list)))
    (unless repos
      (user-error "No repositories have :auto-commit configured"))
    (magit-dash--batch-run
     repos
     #'magit-dash--auto-commit-async
     "magit-dash commit"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-sync-all ()
  "Run auto operations for all configured repos asynchronously, then refresh."
  (interactive)
  (let ((repos (seq-filter #'magit-dash--auto-sync-steps magit-dash-repo-list)))
    (unless repos
      (user-error "No repositories have auto operations configured"))
    (magit-dash--batch-run
     repos
     #'magit-dash--auto-sync-async
     "magit-dash sync"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-auto-sync ()
  "Run all configured auto operations for every eligible repo asynchronously.
Each repo's steps (fetch, pull, commit, push) run sequentially; each step is
logged individually. Dashboard refreshes when all repos complete."
  (interactive)
  (let ((repos (seq-filter #'magit-dash--auto-sync-steps magit-dash-repo-list)))
    (unless repos
      (user-error "No repos have auto operations configured"))
    (magit-dash--batch-run
     repos
     #'magit-dash--auto-sync-async
     "magit-dash autosync"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-run-command ()
  "Open ACR picker for the repo at point and invoke the selected command."
  (interactive)
  (magit-dash--run-command-for (magit-dash--repo-at-point)))

(defun magit-dash--build-tag-table ()
  "Build an ACR alist mapping tag-name strings to annotation strings.
Each annotation lists the count of repos using the tag and up to four names.
Permanent tags (from repo :tags fields) are sorted before ephemeral-only tags."
  (let* ((permanent-set (magit-dash--permanent-tag-set))
         (tag-repo-map (let ((ht (make-hash-table :test #'eq)))
                         (seq-do
                          (lambda (repo)
                            (seq-do
                             (lambda (tag)
                               (puthash tag (cons repo (gethash tag ht)) ht))
                             (magit-dash--all-tags-for repo)))
                          magit-dash-repo-list)
                         ht))
         (all-tags (delete-dups
                    (seq-mapcat #'magit-dash--all-tags-for
                                magit-dash-repo-list)))
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
              (shown (seq-take (seq-map #'magit-dash-repo-name repos) 4)))
         (cons (symbol-name tag)
               (format "%d repo%s: %s%s"
                       count
                       (if (= count 1) "" "s")
                       (mapconcat #'identity shown ", ")
                       (if (> count (length shown)) "…" "")))))
     sorted-tags)))

(cl-defun magit-dash--read-tag (prompt &key include-clear require-match)
  "Read a tag using annotated-completing-read with PROMPT.
Shows permanent tags (from repo :tags fields) before ephemeral-only tags.
Each candidate is annotated with a repo count and up to four repo names.

When INCLUDE-CLEAR is non-nil a \"(clear)\" option is prepended; selecting it
returns the symbol `clear'.  When REQUIRE-MATCH is non-nil only existing tags
are accepted; otherwise arbitrary input is allowed for new ephemeral tags.
Returns an interned symbol, `clear', or nil on quit."
  (let* ((permanent-set (magit-dash--permanent-tag-set))
         (full-table (if include-clear
                         (cons '("(clear)" . "remove tag filter")
                               (magit-dash--build-tag-table))
                       (magit-dash--build-tag-table)))
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

(defun magit-dash-filter-by-tag ()
  "Filter the dashboard by tag using annotated completion.
Select \"(clear)\" to show all repos; quitting leaves the current filter unchanged."
  (interactive)
  (when-let* ((tag (magit-dash--read-tag "Filter by tag: "
                                                       :include-clear t)))
    (setq magit-dash--tag-filter
          (unless (eq tag 'clear) tag))
    (magit-dash-refresh)))

(defun magit-dash-add-tag ()
  "Add an ephemeral session-local tag to the repository at point.
The tag is stored in memory for this session and is not saved to the registry.
Pick from existing tags or type a new symbol name."
  (interactive)
  (when-let* ((repo (magit-dash--repo-at-point))
              (tag (magit-dash--read-tag
                    (format "Add tag to %s: " (magit-dash-repo-name repo)))))
    (let* ((path (magit-dash-repo-path repo))
           (existing (map-elt magit-dash--ephemeral-tags path)))
      (unless (memq tag existing)
        (setf (map-elt magit-dash--ephemeral-tags path) (cons tag existing)))
      (magit-dash-refresh))))

(defun magit-dash-mark-by-tag ()
  "Mark all repos sharing a tag chosen via annotated completion.
Adds to any existing marks rather than replacing them."
  (interactive)
  (when-let* ((tag (magit-dash--read-tag "Mark by tag: " :require-match t)))
    (let* ((tagged (seq-filter
                    (lambda (r)
                      (memq tag (magit-dash--all-tags-for r)))
                    magit-dash-repo-list))
           (paths (seq-map #'magit-dash-repo-path tagged)))
      (setq magit-dash--marked-paths
            (delete-dups (append paths magit-dash--marked-paths)))
      (magit-dash-refresh)
      (message "Marked %d repo%s with tag '%s"
               (length paths)
               (if (= (length paths) 1) "" "s")
               (symbol-name tag)))))

(defun magit-dash-visit-buffer ()
  "Switch to a buffer visiting a file in the repository at point."
  (interactive)
  (let* ((repo (magit-dash--repo-at-point))
         (path (magit-dash-repo-path repo))
         (bufs (seq-filter (lambda (b)
                             (string-prefix-p path (or (buffer-file-name b) "")))
                           (buffer-list))))
    (unless bufs
      (user-error "No open buffers visiting files in %s" (magit-dash-repo-name repo)))
    (switch-to-buffer
     (completing-read "Visit buffer: " (seq-map #'buffer-name bufs) nil t))))

(defun magit-dash-find-file ()
  "Open a file in the repository at point."
  (interactive)
  (find-file
   (read-file-name "Find file: "
                   (file-name-as-directory
                    (magit-dash-repo-path (magit-dash--repo-at-point))))))

(defun magit-dash-switch-branch ()
  "Switch branch in the repository at point via magit."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-checkout)))

(defun magit-dash-prune-branches ()
  "Prune merged branches in the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (magit-gh-prune-merged-branches)))

(defun magit-dash--at-worktree-p ()
  "Return non-nil when the repo at point is a worktree."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (magit-dash-repo-worktree repo)))

(defun magit-dash-worktree-add ()
  "Add a new worktree for the repository at point via magit."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (when (magit-dash-repo-worktree repo)
      (user-error "Cannot add a worktree from a worktree entry"))
    (with-magit-from-dashboard repo
      (call-interactively #'magit-worktree-checkout))
    (magit-dash-refresh)))

(defun magit-dash-worktree-delete ()
  "Delete the worktree at point via magit.
Signals `user-error' when the current row is not a worktree."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash-repo-worktree repo)
      (user-error "Not a worktree row; use 'k' only on worktree entries"))
    (with-magit-from-dashboard repo
      (call-interactively #'magit-worktree-delete))
    (magit-dash-refresh)))

(defun magit-dash-builder ()
  "Run `builder-compile-project' in the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'builder-compile-project)))

(defun magit-dash-agent-shell ()
  "Switch to an agent-shell session for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'agent-shell-switch-project-session)))

(defun magit-dash-agent-shell-new ()
  "Start a new agent-shell session in the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'agent-shell-new-shell)))

(defun magit-dash-agent-shell-queue ()
  "Open the agent-shell queue buffer."
  (interactive)
  (call-interactively #'agent-shell-queue-buffer-open))

;;;###autoload
(defun magit-dash-open-other-window ()
  "Open the repository dashboard buffer in another window.
Signals `user-error' when `magit-dash-repo-list' is empty."
  (let ((current-prefix-arg t))
    (magit-dash-open)))

;;;###autoload
(defun magit-dash-open ()
  "Open the repository dashboard buffer in another window.
Signals `user-error' when `magit-dash-repo-list' is empty."
  (interactive)

  (with-magit-dash
   (if current-prefix-arg
       (pop-to-buffer buf))
   (pop-to-buffer-same-window buf)))

(defun magit-dash-repo-ensure-configuration ()
  "Raises `user-error' when there is not `magit-dash-repo-list' defined."
  (unless magit-dash-repo-list
    (user-error "No repositories registered; use `magit-dash-register'")))

(defun magit-dash-create ()
  (with-magit-dash
   (message "magit-dash-repo-dashobard: your (miss)adventure awaits!")))

(defmacro with-magit-dash (&rest body)
  `(progn
     (magit-dash-repo-ensure-configuration)
     (let* ((buf (get-buffer-create magit-dash-buffer-name))
	    (default-directory (if (eq (current-buffer) buf)
				   (magit-dash-repo-path (magit-dash--repo-at-point))
				 default-directory)))
     (with-current-buffer buf
       (unless (derived-mode-p 'magit-dash-mode)
	 (magit-dash-mode))
       (magit-dash-refresh)
       ,@body))))

(defmacro with-magit-from-dashboard (repo &rest body)
  "Execute BODY with default-directory set to REPO path.
Designed for magit operations invoked from the dashboard.
Lighter than `with-magit-dash'; repo is already fetched."
  (declare (indent 1))
  `(let ((default-directory (magit-dash-repo-path ,repo)))
     ,@body))

;;;; Repo overview buffer

(defvar-local magit-dash-overview--repo nil
  "The `magit-dash-repo' struct displayed in this overview buffer.")

(defvar-local magit-dash-overview--stats nil
  "Cached stats plist for this overview buffer, or nil when loading.")

(defvar-local magit-dash-overview--pr-counts nil
  "Cached PR counts cons (TOTAL . MINE) for this overview buffer, or nil when loading.")

(defvar magit-dash-overview-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'magit-dash-overview-follow)
    (define-key m (kbd "!")   #'magit-dash-overview-magit-dispatch)
    (define-key m (kbd "s")   #'magit-dash-overview-magit-status)
    (define-key m (kbd "d")   #'magit-dash-overview-magit-diff)
    (define-key m (kbd "l")   #'magit-dash-overview-magit-log)
    (define-key m (kbd "L")   #'magit-dash-overview-magit-log-full)
    (define-key m (kbd "c")   #'magit-dash-overview-magit-commit)
    (define-key m (kbd "C")   #'magit-dash-overview-commit)
    (define-key m (kbd "f")   #'magit-dash-overview-fetch)
    (define-key m (kbd "u")   #'magit-dash-overview-pull)
    (define-key m (kbd "n")   #'magit-dash-overview-sync)
    (define-key m (kbd "x")   #'magit-dash-overview-run-command)
    (define-key m (kbd "g")   #'magit-dash-overview-refresh)
    (define-key m (kbd "b")   #'magit-dash-overview-visit-buffer)
    (define-key m (kbd "e")   #'magit-dash-overview-find-file)
    (define-key m (kbd "B")   #'magit-dash-overview-switch-branch)
    (define-key m (kbd "y")   #'magit-dash-overview-prune-branches)
    (define-key m (kbd "P")   #'magit-dash-overview-push)
    (define-key m (kbd "G")   #'magit-dash-overview-stage-all)
    (define-key m (kbd "w")   #'magit-dash-overview-worktree-add)
    (define-key m (kbd "k")   #'magit-dash-overview-worktree-delete)
    (define-key m (kbd "j")   #'magit-dash-overview-builder)
    (define-key m (kbd "z")   #'magit-dash-overview-agent-shell)
    (define-key m (kbd "Z")   #'magit-dash-overview-agent-shell-new)
    (define-key m (kbd "Q")   #'magit-dash-overview-agent-shell-queue)
    (define-key m (kbd "m")   #'magit-dash-overview-menu)
    (define-key m (kbd "?")   #'magit-dash-overview-menu)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for repo overview buffers.")

(defun magit-dash-overview--current-repo ()
  "Return the repo for the current overview buffer or signal `user-error'."
  (or magit-dash-overview--repo
      (user-error "No repository associated with this buffer")))

(defun magit-dash-overview-magit-dispatch ()
  "Open `magit-dispatch' in the context of this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'magit-dispatch)))

(defun magit-dash-overview-magit-status ()
  "Open magit status for this overview's repository."
  (interactive)
  (magit-status-setup-buffer
   (magit-dash-repo-path (magit-dash-overview--current-repo))))

(defun magit-dash-overview-magit-diff ()
  "Open magit diff (dwim) for this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'magit-diff)))

(defun magit-dash-overview-magit-log ()
  "Open magit log for the current branch in this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'magit-log-current)))

(defun magit-dash-overview-magit-log-full ()
  "Open the full magit log menu for this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'magit-log)))

(defun magit-dash-overview-magit-commit ()
  "Open magit commit for this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'magit-commit-create)))

(defun magit-dash-overview-fetch ()
  "Run git fetch for this overview's repository via magit."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'magit-fetch)))

(defun magit-dash-overview-pull ()
  "Pull from upstream for this overview's repository via magit."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'magit-pull-from-upstream)))

(defun magit-dash-overview-commit ()
  "Auto-commit changes in this overview's repository.
Signals `user-error' when :auto-commit is not configured for this repo."
  (interactive)
  (let ((repo (magit-dash-overview--current-repo)))
    (unless (magit-dash-repo-auto-commit repo)
      (user-error "Auto-commit is not configured for %s" (magit-dash-repo-name repo)))
    (if (magit-dash--auto-commit repo)
        (progn
          (message "magit-dash: committed changes in %s" (magit-dash-repo-name repo))
          (magit-dash-overview-refresh))
      (message "magit-dash: nothing to commit or commit failed in %s"
               (magit-dash-repo-name repo)))))

(defun magit-dash-overview-sync ()
  "Run the configured auto operations for this overview's repository asynchronously.
Signals `user-error' when no auto operations are configured for this repo."
  (interactive)
  (let ((repo (magit-dash-overview--current-repo)))
    (unless (magit-dash--auto-sync-steps repo)
      (user-error "No auto operations configured for %s" (magit-dash-repo-name repo)))
    (magit-dash--auto-sync-async
     repo
     (lambda (_status &optional _error-text)
       (magit-dash-overview-refresh)))))

(defun magit-dash-overview-stage-all ()
  "Stage all changes in the current overview's repository."
  (interactive)
  (let ((repo (magit-dash-overview--current-repo)))
    (if (magit-dash--stage-all repo)
        (progn
          (message "magit-dash: staged all changes in %s" (magit-dash-repo-name repo))
          (magit-dash-overview-refresh))
      (message "magit-dash: stage all failed in %s" (magit-dash-repo-name repo)))))

(defun magit-dash-overview-push ()
  "Push current branch to its push remote for this overview's repository."
  (interactive)
  (let ((default-directory (magit-dash-repo-path (magit-dash-overview--current-repo))))
    (magit-push-current-to-pushremote nil)))

(defun magit-dash-overview-run-command ()
  "Open ACR picker for this overview's repository and invoke the selected command."
  (interactive)
  (magit-dash--run-command-for (magit-dash-overview--current-repo)))

(defun magit-dash-overview-visit-buffer ()
  "Switch to a buffer visiting a file in this overview's repository."
  (interactive)
  (let* ((repo (magit-dash-overview--current-repo))
         (path (magit-dash-repo-path repo))
         (bufs (seq-filter (lambda (b)
                             (string-prefix-p path (or (buffer-file-name b) "")))
                           (buffer-list))))
    (unless bufs
      (user-error "No open buffers visiting files in %s" (magit-dash-repo-name repo)))
    (switch-to-buffer
     (completing-read "Visit buffer: " (seq-map #'buffer-name bufs) nil t))))

(defun magit-dash-overview-find-file ()
  "Open a file in this overview's repository."
  (interactive)
  (find-file
   (read-file-name "Find file: "
                   (file-name-as-directory
                    (magit-dash-repo-path (magit-dash-overview--current-repo))))))

(defun magit-dash-overview-switch-branch ()
  "Switch branch in this overview's repository via magit."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'magit-checkout)))

(defun magit-dash-overview-prune-branches ()
  "Prune merged branches in this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (magit-gh-prune-merged-branches)))

(defun magit-dash-overview--is-worktree-p ()
  "Return non-nil when the current overview buffer shows a worktree."
  (when-let* ((repo (ignore-errors (magit-dash-overview--current-repo))))
    (magit-dash-repo-worktree repo)))

(defun magit-dash-overview-worktree-add ()
  "Add a new worktree for this overview's repository via magit.
Signals `user-error' when already viewing a worktree."
  (interactive)
  (let ((repo (magit-dash-overview--current-repo)))
    (when (magit-dash-repo-worktree repo)
      (user-error "Cannot add a worktree from a worktree overview"))
    (magit-gh--with-repo-dir (magit-dash-repo-path repo)
      (call-interactively #'magit-worktree-checkout))
    (magit-dash-overview-refresh)))

(defun magit-dash-overview-worktree-delete ()
  "Delete this worktree via magit.
Signals `user-error' when the current overview is not a worktree."
  (interactive)
  (let ((repo (magit-dash-overview--current-repo)))
    (unless (magit-dash-repo-worktree repo)
      (user-error "Not a worktree; use 'k' only on worktree overviews"))
    (magit-gh--with-repo-dir (magit-dash-repo-path repo)
      (call-interactively #'magit-worktree-delete))
    (quit-window)))

(defun magit-dash-overview-builder ()
  "Run `builder-compile-project' in this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'builder-compile-project)))

(defun magit-dash-overview-agent-shell ()
  "Switch to an agent-shell session for this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'agent-shell-switch-project-session)))

(defun magit-dash-overview-agent-shell-new ()
  "Start a new agent-shell session in this overview's repository."
  (interactive)
  (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
    (call-interactively #'agent-shell-new-shell)))

(defun magit-dash-overview-agent-shell-queue ()
  "Open the agent-shell queue buffer."
  (interactive)
  (call-interactively #'agent-shell-queue-buffer-open))


(defun magit-dash-overview--classify-files (lines)
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

(defun magit-dash-overview--insert-file-section (label files)
  "Insert section LABEL followed by each file in FILES, indented.
Inserts nothing when FILES is empty."
  (when files
    (insert (format "  %s:\n" label))
    (seq-do (lambda (f) (insert (format "    %s\n" f))) files)))

(defun magit-dash-overview--insert-kv (key value &optional value-face action)
  "Insert bold KEY (padded to 16 chars) followed by VALUE and a newline.
When ACTION is non-nil, tag the entire line with `magit-dash-overview-action'
so `magit-dash-overview-follow' can dispatch on it."
  (let ((start (point)))
    (insert (propertize (format "%-16s" (concat key ":")) 'face 'bold))
    (if value-face
        (insert (propertize (or value "") 'face value-face))
      (insert (or value "")))
    (insert "\n")
    (when action
      (put-text-property start (point) 'magit-dash-overview-action action))))

(defun magit-dash-overview--render (repo stats pr-counts)
  "Insert overview content for REPO into the current buffer.
STATS is a plist from `magit-dash--collect-stats-async', or nil
when still loading.  PR-COUNTS is a cons (TOTAL . MINE), or nil when loading."
  (if (null stats)
      (insert (propertize "Loading repository data...\n" 'face 'shadow))
    (let* ((path (magit-dash-repo-path repo))
           (branch (or (plist-get stats :branch) "?"))
           (behind (or (plist-get stats :behind) 0))
           (dirty (plist-get stats :dirty))
           (remote-origin (plist-get stats :remote-origin))
           (uncommitted-files (plist-get stats :uncommitted-files))
           (recent-log (plist-get stats :recent-log)))
      (magit-dash-overview--insert-kv
       "Repository" (magit-dash-repo-name repo) nil (cons 'magit-status path))
      (magit-dash-overview--insert-kv
       "Path" path nil (cons 'dired path))
      (when remote-origin
        (magit-dash-overview--insert-kv "Remote" remote-origin))
      (magit-dash-overview--insert-kv
       "Branch" branch 'magit-dash-repo-branch-face)
      (magit-dash-overview--insert-kv
       "Behind"
       (if (> behind 0)
	   (format "%d commits" behind)
	 "up to date")
       (when (> behind 0) 'warning))
      (if dirty
          (let* ((classified (magit-dash-overview--classify-files uncommitted-files)))
	    (insert "\n")
            (insert (propertize "Uncommitted Files:\n" 'face 'bold))
            (magit-dash-overview--insert-file-section "Staged"    (alist-get 'staged    classified))
            (magit-dash-overview--insert-file-section "Unstaged"  (alist-get 'unstaged  classified))
            (magit-dash-overview--insert-file-section "Deleted"   (alist-get 'deleted   classified))
            (magit-dash-overview--insert-file-section "Untracked" (alist-get 'untracked classified))
            (unless (seq-some #'cdr classified)
              (insert (propertize "  (none)\n" 'face 'shadow))))
        (magit-dash-overview--insert-kv "Changes" "clean"))
      (cond
       ((eq pr-counts 'disabled) t)
       ((null pr-counts)
	(insert (propertize "\nPull Requests:\n" 'face 'bold))
        (insert (propertize "  loading...\n" 'face 'shadow)))
       ((= (car pr-counts) 0)
	(insert (propertize "\nPull Requests:\n" 'face 'bold))
        (insert (propertize "  None\n" 'face 'shadow)))
       (t
	(insert (propertize "\nPull Requests:\n" 'face 'bold))
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
                                  'magit-dash-overview-action
                                  (cons 'magit-show-commit hash)))))
         (split-string recent-log "\n")))
      (unless (magit-dash-repo-worktree repo)
        (when-let* ((worktrees (magit-dash-overview--worktrees-for path)))
          (insert "\n")
          (insert (propertize "Worktrees:\n" 'face 'bold))
          (seq-do
           (lambda (wt)
             (let ((start (point)))
               (insert (format "  %-24s  %s\n"
                               (magit-dash-repo-name wt)
                               (magit-dash-repo-path wt)))
               (put-text-property start (point)
                                  'magit-dash-overview-action
                                  (cons 'magit-status (magit-dash-repo-path wt)))))
           worktrees))))))

(defun magit-dash-overview-follow ()
  "Perform the context-sensitive action for the line at point.
On the Repository line: open magit status for this repository.
On the Path line: open dired for this repository's directory.
On a Recent Commits line: show the commit in magit."
  (interactive)
  (when-let* ((action (get-text-property (point) 'magit-dash-overview-action))
              (type (car action))
              (data (cdr action)))
    (pcase type
      ('magit-status (magit-status-setup-buffer data))
      ('dired (dired data))
      ('magit-show-commit
       (magit-gh--with-repo-dir (magit-dash-repo-path (magit-dash-overview--current-repo))
         (magit-show-commit data))))))

(defun magit-dash-overview--rerender ()
  "Clear and re-render the current overview buffer using buffer-local state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-dash-overview--render
     magit-dash-overview--repo
     magit-dash-overview--stats
     (or (when (magit-dash-repo-include-prs magit-dash-overview--repo)
	   magit-dash-overview--pr-counts)
	 'disabled))
    (goto-char (point-min))))

(defun magit-dash-overview--start-async-load (repo buf)
  "Start async stats then PR-count fetch for REPO; update BUF as each arrives."
  (magit-dash--collect-stats-async
   repo
   (lambda (stats)
     (when (buffer-live-p buf)
       (with-current-buffer buf
         (setq-local magit-dash-overview--stats stats)
         (magit-dash-overview--rerender)
         (magit-dash-overview--pr-counts-async
          (magit-dash-repo-path repo)
          (lambda (counts)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq-local magit-dash-overview--pr-counts counts)
                (magit-dash-overview--rerender))))))))))

(defun magit-dash-overview-refresh ()
  "Re-render the overview buffer with fresh stats fetched asynchronously."
  (interactive)
  (when-let* ((repo (magit-dash-overview--current-repo)))
    (magit-gh--cache-remove (magit-dash-repo-path repo) :stats)
    (magit-gh--cache-remove (magit-dash-repo-path repo) :pr-counts)
    (setq-local magit-dash-overview--stats nil)
    (setq-local magit-dash-overview--pr-counts nil)
    (magit-dash-overview--rerender)
    (magit-dash-overview--start-async-load repo (current-buffer))))

(defun magit-dash-overview--open (repo)
  "Pop to a read-only overview buffer for REPO, loading stats asynchronously."
  (let* ((buf-name (format "*magit-dash: %s*" (magit-dash-repo-name repo)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory (magit-dash-repo-path repo))
        (setq-local magit-dash-overview--repo repo)
        (setq-local magit-dash-overview--stats nil)
        (setq-local magit-dash-overview--pr-counts nil)
        (use-local-map magit-dash-overview-mode-map)
        (magit-dash-overview--render repo nil nil)
        (goto-char (point-min)))
      (setq buffer-read-only t))
    (pop-to-buffer buf)
    (magit-dash-overview--start-async-load repo buf)) )

;;;; Transient predicates

(defun magit-dash--dirty-or-unknown-p ()
  "Return non-nil when the repo at point is dirty or its stats are not yet cached."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (if-let* ((stats (magit-gh--cache-get (magit-dash-repo-path repo) :stats)))
        (plist-get stats :dirty)
      t)))

(defun magit-dash--has-auto-commit-p ()
  "Return non-nil when the repo at point has :auto-commit configured."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (and (magit-dash-repo-auto-commit repo) t)))

(defun magit-dash--has-auto-sync-p ()
  "Return non-nil when the repo at point has any auto operation configured."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (and (magit-dash--auto-sync-steps repo) t)))

(defun magit-dash--has-commands-p ()
  "Return non-nil when the repo at point has commands registered."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (and (magit-dash-repo-commands repo) t)))

(defun magit-dash-overview--has-changes-p ()
  "Return non-nil when this overview's repository has uncommitted changes."
  (and magit-dash-overview--stats
       (plist-get magit-dash-overview--stats :dirty)))

(defun magit-dash-overview--ahead-p ()
  "Return non-nil when this overview's repository has commits ahead of upstream."
  (and magit-dash-overview--stats
       (> (or (plist-get magit-dash-overview--stats :ahead) 0) 0)))

(defun magit-dash-overview--has-auto-commit-p ()
  "Return non-nil when this overview's repository has :auto-commit configured."
  (when-let* ((repo (ignore-errors (magit-dash-overview--current-repo))))
    (and (magit-dash-repo-auto-commit repo) t)))

(defun magit-dash-overview--has-auto-sync-p ()
  "Return non-nil when this overview's repository has any auto operation configured."
  (when-let* ((repo (ignore-errors (magit-dash-overview--current-repo))))
    (and (magit-dash--auto-sync-steps repo) t)))

(defun magit-dash-overview--has-commands-p ()
  "Return non-nil when this overview's repository has commands registered."
  (when-let* ((repo (ignore-errors (magit-dash-overview--current-repo))))
    (and (magit-dash-repo-commands repo) t)))

(defun magit-dash--repo-at-point-behind-p ()
  "Return non-nil when the repo at point has commits behind its upstream."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point)))
              (stats (magit-dash--get-stats repo)))
    (and (> (or (plist-get stats :behind) 0) 0) t)))

(defun magit-dash--repo-at-point-ahead-p ()
  "Return non-nil when the repo at point has commits ahead of its upstream."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point)))
              (stats (magit-dash--get-stats repo)))
    (and (> (or (plist-get stats :ahead) 0) 0) t)))

(defun magit-dash--can-add-worktree-p ()
  "Return non-nil when at a registered non-worktree repo (can add a worktree)."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (not (magit-dash-repo-worktree repo))))

;;;; Mark/select support

(defun magit-dash--update-entry-for (repo)
  "Regenerate the tabulated-list entry for REPO in place in `tabulated-list-entries'."
  (let ((new-entry (magit-dash--build-entry repo))
        (path (magit-dash-repo-path repo)))
    (setq tabulated-list-entries
          (seq-map (lambda (entry)
                     (if (equal (magit-dash-repo-path (car entry)) path)
                         new-entry
                       entry))
                   tabulated-list-entries))))

(defun magit-dash-toggle-mark ()
  "Toggle the mark on the repository at point and advance to the next line."
  (interactive)
  (let* ((repo (magit-dash--repo-at-point))
         (path (magit-dash-repo-path repo)))
    (setq magit-dash--marked-paths
          (if (member path magit-dash--marked-paths)
              (delete path magit-dash--marked-paths)
            (cons path magit-dash--marked-paths)))
    (magit-dash--update-entry-for repo)
    (tabulated-list-print t)
    (forward-line 1)))

(defun magit-dash-unmark-all ()
  "Clear all marks from the dashboard."
  (interactive)
  (setq magit-dash--marked-paths nil)
  (setq tabulated-list-entries
        (seq-map (lambda (entry)
                   (magit-dash--build-entry (car entry)))
                 tabulated-list-entries))
  (tabulated-list-print t))

(defun magit-dash--effective-repos ()
  "Return marked repos if any are marked, else all repos currently in the table."
  (let ((all (seq-map #'car tabulated-list-entries)))
    (if magit-dash--marked-paths
        (seq-filter (lambda (r)
                      (member (magit-dash-repo-path r)
                              magit-dash--marked-paths))
                    all)
      all)))

(defun magit-dash--has-marks-p ()
  "Return non-nil when at least one repository is marked."
  (and magit-dash--marked-paths t))

(defun magit-dash-fetch-all ()
  "Asynchronously fetch marked repos, or all visible repos when none are marked."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to fetch"))
    (magit-dash--batch-run
     repos
     #'magit-dash--fetch-async
     "magit-gh fetch"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-pull-all ()
  "Asynchronously pull marked repos, or all visible repos when none are marked."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to pull"))
    (magit-dash--batch-run
     repos
     #'magit-dash--pull-async
     "magit-gh pull"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-push-all ()
  "Asynchronously push marked repos, or all visible repos when none are marked."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to push"))
    (magit-dash--batch-run
     repos
     #'magit-dash--push-async
     "magit-gh push"
     (lambda (_) (magit-dash--maybe-refresh)))))

;;;; Transient menus

(transient-define-prefix magit-dash-menu ()
  "Actions for the repository at point in the repo dashboard."
  [["Repository"
    ("!"  "Magit dispatch"   magit-dash-magit-dispatch
     :inapt-if-not magit-dash--repo-at-point-p)
    ("RET" "Open overview"   magit-dash-view
     :inapt-if-not magit-dash--repo-at-point-p)
    ("gs"   "Status"          magit-dash-magit-status
     :inapt-if-not magit-dash--repo-at-point-p)
    ("d"   "Diff…"           magit-dash-magit-diff
     :inapt-if-not magit-dash--dirty-or-unknown-p)
    ("lc"   "Log (current)"  magit-dash-magit-log
     :inapt-if-not magit-dash--repo-at-point-p)
    ("lf"   "Log…"           magit-dash-magit-log-full
     :inapt-if-not magit-dash--repo-at-point-p)
    ("C-c"    "Commit"       magit-dash-magit-commit
     :inapt-if-not magit-dash--dirty-or-unknown-p)
    ("sa"   "Stage all"      magit-dash-stage-all
     :inapt-if-not magit-dash--dirty-or-unknown-p)
    ("fr"   "Fetch"          magit-dash-fetch
     :inapt-if-not magit-dash--repo-at-point-p)
    ("rp"   "Pull"            magit-dash-pull
     :inapt-if-not magit-dash--repo-at-point-p)
    ("rs"   "Push (repo send)" magit-dash-push
     :inapt-if-not magit-dash--repo-at-point-ahead-p)]
   ["Navigate"
    ("b"   "Visit buffer"    magit-dash-visit-buffer
     :inapt-if-not magit-dash--repo-at-point-p)
    ("ff"  "Find file"       magit-dash-find-file
     :inapt-if-not magit-dash--repo-at-point-p)
    ("gb"  "Switch branch"   magit-dash-switch-branch
     :inapt-if-not magit-dash--repo-at-point-p)
    ("y"   "Prune branches"  magit-dash-prune-branches
     :inapt-if-not magit-dash--repo-at-point-p)]
   ["Manage"
    ("ac"   "Auto-commit"     magit-dash-commit
     :inapt-if-not magit-dash--has-auto-commit-p)
    ("sy"   "Sync"            magit-dash-sync
     :inapt-if-not magit-dash--has-auto-sync-p)
    ("et"   "Add tag"         magit-dash-add-tag
     :inapt-if-not magit-dash--repo-at-point-p)
    ("t"    "Compile Project (builder)"  magit-dash-builder
     :inapt-if-not magit-dash--has-auto-commit-p)
    ("x"   "Run command"     magit-dash-run-command
     :inapt-if-not magit-dash--has-commands-p)
    ("sb"  "Bump submodules..." magit-dash-bump-submodules-menu
     :inapt-if-not magit-dash--repo-at-point-p)]
   ["Build & Shell"
    ("as"   "Agent shell (project)"      magit-dash-agent-shell
     :inapt-if-not agent-shell-menu-project-buffers)
    ("an"   "New agent shell"            magit-dash-agent-shell-new)
    ("aq"   "Agent shell queue"          magit-dash-agent-shell-queue)]
   ["Worktree"
    ("w"   "Add worktree"    magit-dash-worktree-add
     :inapt-if-not magit-dash--can-add-worktree-p)
    ("k"   "Delete worktree" magit-dash-worktree-delete
     :inapt-if-not magit-dash--at-worktree-p)]
   ["Batch"
    ("SPC"  "Toggle mark"       magit-dash-toggle-mark
     :inapt-if-not magit-dash--repo-at-point-p)
    ("mt"   "Toggle mark"       magit-dash-toggle-mark
     :inapt-if-not magit-dash--repo-at-point-p)
    ("mu"   "Clear marks"       magit-dash-unmark-all
     :inapt-if-not magit-dash--has-marks-p)
    ("mfa"  "Fetch all/marked"  magit-dash-fetch-all)
    ("mpa"  "Pull all/marked"   magit-dash-pull-all)
    ("mpu"  "Push all/marked"   magit-dash-push-all)
    ("msa"  "Sync all"          magit-dash-sync-all)
    ("mca"  "Commit all"        magit-dash-commit-all)
    ("maa"  "Autosync all"      magit-dash-auto-sync)
    ("mbt"  "Mark by tag"       magit-dash-mark-by-tag)]
   ["Dashboard"
    ("mbpr" "Open PR dashboard" magit-gh-pr-dashboard-open)
    ("nt"   "Filter by tag"     magit-dash-filter-by-tag)
    ("C-t"  "Toggle column"     magit-dash-toggle-column)
    ("M-s"  "Toggle submodules" magit-dash-toggle-discovered-submodules)
    ("gg"   "Refresh"           magit-dash-refresh)
    ("q"    "Quit"              quit-window)]
   ["Cache"
    ("ci"   "Cache info"        magit-dash-cache-info)
    ("ccr"   "Reset cache"      magit-dash-cache-reset-at-point
     :inapt-if-not magit-dash--repo-at-point-p)
    ("cca"   "Reset all caches"     magit-dash-cache-reset-all)]])

(transient-define-prefix magit-dash-overview-menu ()
  "Magit actions for the repository shown in this overview buffer."
  [["Magit"
    ("!"  "Magit dispatch"   magit-dash-overview-magit-dispatch)
    ("gs"   "Status"         magit-dash-overview-magit-status)
    ("d"   "Diff"            magit-dash-overview-magit-diff
     :if magit-dash-overview--has-changes-p)
    ("lc"  "Log (current)"   magit-dash-overview-magit-log)
    ("lf"  "Log…"            magit-dash-overview-magit-log-full)
    ("c"   "Commit"          magit-dash-overview-magit-commit
     :if magit-dash-overview--has-changes-p)
    ("sa"   "Stage all"       magit-dash-overview-stage-all
     :if magit-dash-overview--has-changes-p)
    ("fr"   "Fetch"            magit-dash-overview-fetch)
    ("rp"   "Pull"             magit-dash-overview-pull)
    ("rs"   "Push (repo send)" magit-dash-overview-push
     :inapt-if-not magit-dash-overview--ahead-p)]
   ["Navigate"
    ("b"   "Visit buffer"    magit-dash-overview-visit-buffer)
    ("ff"  "Find file"       magit-dash-overview-find-file)
    ("gb"  "Switch branch"   magit-dash-overview-switch-branch)]
   ["Manage"
    ("t"   "Compile project (builder)"  magit-dash-overview-builder
     :if (lambda () (featurep 'builder)))
    ("rx"   "Run command"                magit-dash-overview-run-command
     :if magit-dash-overview--has-commands-p)
    ("mp"   "Prune branches"  magit-dash-overview-prune-branches)
    ("mc"   "Auto-commit"     magit-dash-overview-commit
     :if magit-dash-overview--has-auto-commit-p)
    ("sy"   "Sync"            magit-dash-overview-sync
     :if magit-dash-overview--has-auto-sync-p)
    ("sb"  "Bump submodules..." magit-dash-bump-submodules-menu)]
   ["Batch"
    ("msa"  "Sync all"          magit-dash-sync-all)
    ("mca"  "Commit all"        magit-dash-commit-all)
    ("maa"  "Autosync all"      magit-dash-auto-sync)
    ("mpa"  "Push all"          magit-dash-push-all)]
   ["Agent Shell"
    ("as"   "Agent shell (project)"  magit-dash-overview-agent-shell
     :if agent-shell-menu-project-buffers)
    ("an"   "New agent shell"        magit-dash-overview-agent-shell-new)
    ("aq"   "Agent shell queue"      magit-dash-overview-agent-shell-queue)]
   ["Worktree"
    ("w"   "Add worktree"    magit-dash-overview-worktree-add
     :if-not magit-dash-overview--is-worktree-p)
    ("k"   "Delete worktree" magit-dash-overview-worktree-delete
     :if magit-dash-overview--is-worktree-p)]
   ["View"
    ("gg"   "Refresh"        magit-dash-overview-refresh)
    ("q"   "Quit"            quit-window)]])

(defun ad:magit-dash--quit-window (orig-fn &optional kill window)
  "Around advice for `quit-window': delete split window in dashboard buffers.
Only applies in `magit-dash-mode', `magit-dash-overview-mode',
and `magit-gh-pr-dashboard-mode'.  Falls through otherwise."
  (if (or kill (one-window-p)
          (not (derived-mode-p 'magit-dash-mode
                               'magit-dash-overview-mode
                               'magit-gh-pr-dashboard-mode)))
      (funcall orig-fn kill window)
    (delete-window (or window (selected-window)))))

(advice-add 'quit-window :around #'ad:magit-dash--quit-window)

(provide 'magit-dash)

;;; magit-dash.el ends here
