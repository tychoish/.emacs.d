;;; magit-open-repo.el --- Interactive git repository picker -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides `magit-open-repo', a context-aware repository picker using
;; `annotated-completing-read'.  Behaviour adapts to the calling context:
;;
;;   - Current directory is a git repo (non-magit buffer):
;;       Only the repo's worktrees and submodules are offered.
;;   - Current buffer is a magit-status buffer:
;;       Scans from the parent of the current repo; also merges `~/' at
;;       depth 1 so sibling repos and home-level repos appear together.
;;   - Otherwise:
;;       Scans subdirectories of `default-directory'.
;;
;; In all cases live magit-status buffers are included in the candidate list
;; and marked "[open]"; selecting one switches to it directly.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)

(require 'magit)
(require 'projectile)

(require 'annotated-completing-read)

;;; Configuration

(defvar magit-open-repo-scan-depth 2
  "How many directory levels to scan for git repositories.
Worktrees and submodules are always enumerated via git regardless of depth.")

;;; Caching

(defvar magit-open-repo--info-cache (make-hash-table :test #'equal)
  "Per-repo git info cache.
Keys are absolute paths.  Values are plists:
  :head-hash        HEAD commit hash at cache-fill time
  :branch           current branch name
  :upstream         configured upstream ref or nil
  :upstream-display short display string for upstream (handles origin/ fallback)
  :ahead            commits ahead of upstream (integer)
  :behind           commits behind upstream (integer)
  :dirty            non-nil when working tree has uncommitted changes")

(defun magit-open-repo-cache-reset ()
  "Clear the git info cache used by `magit-open-repo'."
  (interactive)
  (setq magit-open-repo--info-cache (make-hash-table :test #'equal))
  (message "magit-open-repo: cache cleared"))

;;; Git subprocess helpers

(defun magit-open-repo--run-git (dir &rest args)
  "Run git in DIR with ARGS; return output lines or nil on error."
  (ignore-errors
    (let ((default-directory dir))
      (apply #'magit-git-lines args))))

(defun magit-open-repo--git-p (dir)
  "Return non-nil if DIR is the root of a git repository or worktree."
  (file-exists-p (expand-file-name ".git" dir)))

;;; Git info collection

(defun magit-open-repo--git-info (dir)
  "Return a git info plist for the repo at DIR.
Cached keyed by HEAD hash; stale entries are evicted on the next call.
The plist includes :upstream-display, which handles the origin/BRANCH
fallback so the annotation function never calls git directly."
  (let* ((head-hash (car (magit-open-repo--run-git dir "rev-parse" "HEAD")))
         (cached (map-elt magit-open-repo--info-cache dir)))
    (if (and cached (equal (map-elt cached :head-hash) head-hash))
        cached
      (let* ((branch (or (let ((default-directory dir))
                           (magit-get-current-branch))
                         "HEAD"))
             (upstream-raw (car (magit-open-repo--run-git
                                  dir "rev-parse" "--abbrev-ref"
                                  "--symbolic-full-name" "@{u}")))
             (upstream (when (and upstream-raw (not (string= upstream-raw "@{u}")))
                         upstream-raw))
             (upstream-display
              (or (when upstream
                    (replace-regexp-in-string "^origin/" "" upstream))
                  (when (magit-open-repo--run-git
                          dir "rev-parse" "--verify" "--quiet"
                          (format "origin/%s" branch))
                    branch)))
             (ahead 0)
             (behind 0))
        (when upstream
          (when-let* ((count-line (car (magit-open-repo--run-git
                                         dir "rev-list" "--count" "--left-right"
                                         (format "HEAD...%s" upstream)))))
            (when (string-match "\\([0-9]+\\)\t\\([0-9]+\\)" count-line)
              (setq ahead (string-to-number (match-string 1 count-line))
                    behind (string-to-number (match-string 2 count-line))))))
        (let ((info (list :head-hash head-hash
                          :branch branch
                          :upstream upstream
                          :upstream-display upstream-display
                          :ahead ahead
                          :behind behind
                          :dirty (let ((default-directory dir))
                                   (magit-anything-modified-p)))))
          (map-put! magit-open-repo--info-cache dir info)
          info)))))

;;; Directory scanning

(defun magit-open-repo--subdirs (dir)
  "Return absolute paths of non-hidden immediate subdirectories of DIR."
  (when (file-accessible-directory-p dir)
    (thread-last (directory-files dir nil "^[^.]" t)
      (seq-map (lambda (name) (expand-file-name name dir)))
      (seq-filter #'file-directory-p))))

(defun magit-open-repo--worktree-paths (dir)
  "Return list of additional worktree absolute paths for git repo at DIR."
  (thread-last (magit-open-repo--run-git dir "worktree" "list" "--porcelain")
               (seq-filter (lambda (l) (string-prefix-p "worktree " l)))
               (cdr)
               (seq-map (lambda (l) (substring l 9)))))

(defun magit-open-repo--submodule-paths (dir)
  "Return list of initialized submodule absolute paths for git repo at DIR."
  (thread-last (magit-open-repo--run-git dir "submodule" "status")
    (seq-filter (lambda (l) (not (string-empty-p l))))
    (seq-map (lambda (line)
               (when (string-match "^[ +U][0-9a-f]+ \\([^ ]+\\)" line)
                 (expand-file-name (match-string 1 line) dir))))
    (seq-remove #'null)))

(defun magit-open-repo--collect-self (dir)
  "Return (path . kind) pairs for worktrees and submodules of git repo at DIR."
  (append
   (seq-map (lambda (p) (cons p 'worktree)) (magit-open-repo--worktree-paths dir))
   (seq-map (lambda (p) (cons p 'submodule)) (magit-open-repo--submodule-paths dir))))

(defun magit-open-repo--collect-deep (root depth)
  "Return (path . kind) pairs for all candidates under ROOT up to DEPTH levels.
Kind is one of: `repo', `worktree', `submodule', `dir'.
Worktrees and submodules are enumerated for every repo found regardless of depth."
  (let* ((subdirs (magit-open-repo--subdirs root))
         (repos (seq-filter #'magit-open-repo--git-p subdirs))
         (plain (seq-remove #'magit-open-repo--git-p subdirs)))
    (append
     (seq-map (lambda (p) (cons p 'repo)) repos)
     (seq-map (lambda (p) (cons p 'dir)) plain)
     (seq-mapcat (lambda (r)
                   (seq-map (lambda (p) (cons p 'worktree))
                            (magit-open-repo--worktree-paths r)))
                 repos)
     (seq-mapcat (lambda (r)
                   (seq-map (lambda (p) (cons p 'submodule))
                            (magit-open-repo--submodule-paths r)))
                 repos)
     (when (> depth 0)
       (seq-mapcat (lambda (d) (magit-open-repo--collect-deep d (1- depth)))
                   plain)))))

;;; Projectile integration

(defun magit-open-repo--projectile-candidates ()
  "Return (path . `repo') pairs from projectile known-projects, limited to git repos."
  (when (and (fboundp 'projectile-load-known-projects)
             (boundp 'projectile-known-projects))
    (ignore-errors (projectile-load-known-projects))
    (thread-last projectile-known-projects
      (seq-map (lambda (p) (directory-file-name (expand-file-name p))))
      (seq-filter #'file-directory-p)
      (seq-filter #'magit-open-repo--git-p)
      (seq-map (lambda (p) (cons p 'repo))))))

;;; Open-buffer detection

(defun magit-open-repo--open-status-buffers ()
  "Return alist of (normalized-path . buffer) for all live magit-status buffers."
  (thread-last (buffer-list)
    (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) 'magit-status-mode)))
    (seq-map (lambda (b)
               (cons (file-name-as-directory
                      (expand-file-name
                       (buffer-local-value 'default-directory b)))
                     b)))))

(defun magit-open-repo--find-open-buffer (path open-buffers)
  "Return the live magit-status buffer for PATH from OPEN-BUFFERS, or nil."
  (let ((norm (file-name-as-directory (expand-file-name path))))
    (cdr (seq-find (lambda (pair) (file-equal-p (car pair) norm)) open-buffers))))

;;; Annotation

(defun magit-open-repo--annotation (path kind)
  "Return annotation string for PATH with KIND.
KIND is one of: `repo', `worktree', `submodule', `dir'."
  (if (eq kind 'dir)
      "directory"
    (let* ((info (condition-case nil (magit-open-repo--git-info path) (error nil)))
           (branch (or (map-elt info :branch) "?"))
           (upstream-display (map-elt info :upstream-display))
           (ahead (or (map-elt info :ahead) 0))
           (behind (or (map-elt info :behind) 0))
           (dirty (map-elt info :dirty))
           (kind-str (symbol-name kind))
           (arrow-part (if upstream-display
                           (format "%s → %s" branch upstream-display)
                         branch))
           (count-part (cond
                        ((and (> ahead 0) (> behind 0)) (format "+%d/-%d" ahead behind))
                        ((> ahead 0) (format "+%d" ahead))
                        ((> behind 0) (format "-%d" behind))
                        (t nil)))
           (dirty-part (when dirty "*")))
      (string-join (seq-remove #'null (list kind-str arrow-part count-part dirty-part))
                   "  "))))

;;; Entry point

;;;###autoload
(defun magit-open-repo (&optional root)
  "Select a repository and open its magit status buffer.

Context-aware collection:
- In a magit-status buffer: scans from the parent of the current repo and
  merges `~/' at depth 1, so sibling repos and home-level repos appear together.
- In a directory that is itself a git repo: only the repo's worktrees and
  submodules are offered (use a prefix arg to override and scan subdirs).
- Otherwise: scans subdirectories of `default-directory'.

Live magit-status buffers are always included and marked \"[open]\"; selecting
one switches to it directly without reopening it.

With a prefix argument, prompt for ROOT and scan it normally regardless of
the above context rules."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "Root directory: " default-directory))))
  (let* ((forced-root (when root (expand-file-name root)))
         (in-magit-p (and (not forced-root) (derived-mode-p 'magit-status-mode)))
         (current-dir (expand-file-name default-directory))
         (current-repo-p (and (not forced-root) (magit-open-repo--git-p current-dir)))
         ;; Determine scan root and collect primary entries
         (entries
          (cond
           (forced-root
            (magit-open-repo--collect-deep forced-root magit-open-repo-scan-depth))
           (in-magit-p
            (let* ((parent (file-name-directory (directory-file-name current-dir)))
                   (home (expand-file-name "~/"))
                   (from-parent (magit-open-repo--collect-deep
                                 parent magit-open-repo-scan-depth))
                   (from-home (unless (file-equal-p parent home)
                                (magit-open-repo--collect-deep home 1))))
              (append from-parent from-home)))
           (current-repo-p
            (let ((self (magit-open-repo--collect-self current-dir)))
              ;; Fall back to subdirectory scan if no worktrees or submodules exist
              (or self (magit-open-repo--collect-deep
                        current-dir magit-open-repo-scan-depth))))
           (t (magit-open-repo--collect-deep current-dir magit-open-repo-scan-depth))))
         ;; Always merge projectile and live status buffers
         (projectile-entries (when (and (featurep 'projectile) projectile-mode)
                               (magit-open-repo--projectile-candidates)))
         (open-buffers (magit-open-repo--open-status-buffers))
         (path-map (make-hash-table :test #'equal)))
    ;; Priority: local scan > projectile; open buffers fill gaps
    (map-do (lambda (path kind) (map-put! path-map path kind)) entries)
    (map-do (lambda (path kind)
              (unless (map-contains-key path-map path)
                (map-put! path-map path kind)))
            projectile-entries)
    ;; Ensure every open magit-status buffer appears, even if outside the scan
    (map-do (lambda (norm-path _buf)
              (let ((dir (directory-file-name norm-path)))
                (unless (map-contains-key path-map dir)
                  (map-put! path-map dir 'repo))))
            open-buffers)
    (when (map-empty-p path-map)
      (user-error "No directories found"))
    (let* ((abbrev-map (make-hash-table :test #'equal))
           (table (make-hash-table :test #'equal)))
      (map-do (lambda (path kind)
                (let* ((display (abbreviate-file-name path))
                       (already-open (magit-open-repo--find-open-buffer path open-buffers))
                       (base (magit-open-repo--annotation path kind))
                       (annotation (if already-open (concat base "  [open]") base)))
                  (map-put! abbrev-map display path)
                  (map-put! table display annotation)))
              path-map)
      (when-let* ((chosen (annotated-completing-read
                           table
                           :prompt "Open repo: "
                           :require-match t
                           :category 'file)))
        (let* ((full-path (or (map-elt abbrev-map chosen)
                              (expand-file-name chosen)))
               (existing (magit-open-repo--find-open-buffer full-path open-buffers)))
          (if existing
              (switch-to-buffer existing)
            (magit-status-setup-buffer full-path)))))))

(provide 'magit-open-repo)
;;; magit-open-repo.el ends here
