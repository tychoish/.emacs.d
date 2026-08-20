;;; builder-elpa.el --- Multi-track ELPA repository builder and CI automation -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maintenance, tools, local, package, elpa
;; Package-Requires: ((emacs "27.1") (magit "3.0.0") (map "3.0") (seq "2.0"))

;;; Commentary:
;; This package manages, builds, signs, and publishes a multi-track MELPA-style
;; ELPA package repository hosted on GitHub Pages or local web servers.
;;
;; Architecture & Tracks:
;; - `elpaish`: Primary snapshot archive tracking the TIP of the default branch
;;   with pure date-based version strings (YYYYMMDD.HHMMSS).
;; - `elpaish-stable`: Official releases built strictly from clean semver Git tags
;;   (vX.Y.Z -> X.Y.Z). Repositories lacking clean tags are omitted entirely.
;; - `elpaish-staging`: Pre-release builds and release candidates derived from
;;   non-stable Git tags (e.g. -rc, -pre, -beta) and `git describe` versions.
;;
;; Features:
;; - Pure Emacs Lisp orchestration without external build tool dependencies.
;; - In-memory version header injection and dynamic <pkg>-pkg.el tarball generation
;;   without mutating or dirtying upstream Git trees.
;; - Subkey GPG signing pipeline supporting headless CI with loopback pinentry.
;; - Full GPG key lifecycle tooling, automated secret synchronization via GitHub CLI
;;   (`gh secret set`), and emergency revocation publishing.
;; - Preflight package validation gates (check-parens, checkdoc, package-lint,
;;   isolated byte-compilation, ERT tests) with automatic package quarantine.
;; - Built-in local HTTP preview server for testing in isolated `emacs -Q` sessions.
;; - Interactive `tabulated-list-mode` status UI (`*builder-elpa-status*`).

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'epg)
(require 'magit)
(require 'map)
(require 'package)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'timer)
(require 'annotated-completing-read)

(defgroup builder-elpa nil
  "Multi-track ELPA package repository builder."
  :group 'development)

(defcustom builder-elpa-output-dir (expand-file-name "public/" default-directory)
  "Root directory where package archives, keys, and indexes are written."
  :type 'directory
  :group 'builder-elpa)

(defcustom builder-elpa-work-dir (expand-file-name "repos/" default-directory)
  "Directory where remote Git repositories are cloned."
  :type 'directory
  :group 'builder-elpa)

(defcustom builder-elpa-sign-packages nil
  "When non-nil, sign generated package archives and `archive-contents' with GPG."
  :type 'boolean
  :group 'builder-elpa)

(defcustom builder-elpa-gpg-key nil
  "GPG key ID, fingerprint, or email used to sign packages.
If nil, checks `ELPAISH_SIGNING_KEY' or `ELPAISH_GPG_KEY' environment variables,
falling back to default GPG key."
  :type '(choice (const :tag "Default / Environment Key" nil)
                 (string :tag "Key ID or Fingerprint"))
  :group 'builder-elpa)

(defcustom builder-elpa-gpg-passphrase nil
  "Optional passphrase for GPG signing key (or from `ELPAISH_GPG_PASSPHRASE')."
  :type '(choice (const :tag "None / GPG Agent" nil)
                 (string :tag "Passphrase"))
  :group 'builder-elpa)

(defcustom builder-elpa-release-mode 'all
  "Default release mode / track for building packages.
Can be `all' (builds elpaish, elpaish-stable, and elpaish-staging),
`elpaish' (snapshot date versions), `elpaish-stable' (semver tags only),
or `elpaish-staging' (pre-release tags and git describe)."
  :type '(choice (const :tag "All Tracks (elpaish, stable, staging)" all)
                 (const :tag "elpaish (Snapshot date versions)" elpaish)
                 (const :tag "elpaish-stable (Clean semver tags only)" elpaish-stable)
                 (const :tag "elpaish-staging (Pre-release & describe)" elpaish-staging))
  :group 'builder-elpa)

(defcustom builder-elpa-run-preflight t
  "When non-nil, execute preflight quality gates before building packages."
  :type 'boolean
  :group 'builder-elpa)

(defcustom builder-elpa-default-branch "main"
  "Default Git branch to track for recipes that do not specify one."
  :type 'string
  :group 'builder-elpa)

(defconst builder-elpa-tracks '(elpaish elpaish-stable elpaish-staging)
  "List of supported package archive tracks.")

;;; Registry Data Structure

(cl-defstruct (builder-elpa-recipe (:constructor builder-elpa-recipe-create))
  "Structure representing an ELPA package build recipe."
  (name nil :type string :documentation "Package name.")
  (repo nil :type string :documentation "Local directory path or Git URL.")
  (branch "main" :type string :documentation "Git branch to track.")
  (files '("*.el") :type list :documentation "List of file patterns to include.")
  (test-dir nil :type (choice null string) :documentation "Optional custom test directory.")
  (preflight-skip nil :type (choice boolean list) :documentation "Checks to skip in preflight.")
  (summary nil :type (choice null string) :documentation "Package summary description.")
  (url nil :type (choice null string) :documentation "Upstream homepage or repository URL.")
  (keywords nil :type list :documentation "List of keywords.")
  (requires nil :type list :documentation "Declared dependencies ((dep min-ver) ...).")
  (built-version-elpaish nil :type (choice null string) :documentation "Last built version for elpaish track.")
  (built-version-stable nil :type (choice null string) :documentation "Last built version for stable track.")
  (built-version-staging nil :type (choice null string) :documentation "Last built version for staging track.")
  (built-hash nil :type (choice null string) :documentation "Git commit hash when last built.")
  (built-type 'single :type symbol :documentation "Package archive type ('single or 'tar)."))

(defvar builder-elpa-registry (make-hash-table :test 'equal)
  "Registry storing package recipes keyed by package name string.")

(defvar builder-elpa-timer nil
  "Timer object for scheduled repository auto-rebuilds.")

(defvar builder-elpa-server-process nil
  "Process handle for local preview HTTP server.")

;; Compatibility accessors for single built-version references
(defun builder-elpa-recipe-built-version (recipe)
  "Return most recent built version for RECIPE across tracks."
  (or (builder-elpa-recipe-built-version-elpaish recipe)
      (builder-elpa-recipe-built-version-stable recipe)
      (builder-elpa-recipe-built-version-staging recipe)))

(gv-define-setter builder-elpa-recipe-built-version (val recipe)
  `(setf (builder-elpa-recipe-built-version-elpaish ,recipe) ,val))

;;;###autoload
(cl-defun builder-elpa-register-package (name repo &key (branch "main") (files '("*.el"))
                                              test-dir preflight-skip summary url keywords requires)
  "Register package NAME with REPO local directory path or remote Git URL.
BRANCH defaults to \"main\" and FILES defaults to \\='(\"*.el\").
TEST-DIR, PREFLIGHT-SKIP, SUMMARY, URL, KEYWORDS, and REQUIRES provide metadata."
  (let* ((name-str (if (symbolp name) (symbol-name name) (string-trim name)))
         (recipe (builder-elpa-recipe-create
                  :name name-str
                  :repo (if (and (stringp repo) (not (string-match-p "\\`https?://" repo)) (not (string-match-p "\\`git@" repo)))
                            (expand-file-name repo)
                          repo)
                  :branch (or branch builder-elpa-default-branch)
                  :files (or files '("*.el"))
                  :test-dir test-dir
                  :preflight-skip preflight-skip
                  :summary (or summary "No description")
                  :url url
                  :keywords (or keywords '("tools"))
                  :requires requires
                  :built-version-elpaish nil
                  :built-version-stable nil
                  :built-version-staging nil
                  :built-hash nil
                  :built-type 'single)))
    (puthash name-str recipe builder-elpa-registry)
    recipe))

(defun builder-elpa-clear-registry ()
  "Clear all registered recipes from the registry."
  (interactive)
  (clrhash builder-elpa-registry))

;;; Track & Directory Resolution

(defun builder-elpa-canonical-track (track)
  "Return canonical track symbol for TRACK (`elpaish', `elpaish-stable', `elpaish-staging')."
  (pcase track
    ((or 'elpaish 'snapshot 'unstable) 'elpaish)
    ((or 'elpaish-stable 'stable) 'elpaish-stable)
    ((or 'elpaish-staging 'staging 'pre) 'elpaish-staging)
    ('all 'all)
    (_ 'elpaish)))

(defun builder-elpa-track-dir (track &optional root-dir)
  "Return destination directory for TRACK under ROOT-DIR (or `builder-elpa-output-dir')."
  (let ((base (file-name-as-directory (or root-dir builder-elpa-output-dir)))
        (canon (builder-elpa-canonical-track track)))
    (if (eq canon 'all)
        base
      (expand-file-name (symbol-name canon) base))))

(defun builder-elpa-recipe-version-for-track (recipe track)
  "Return stored built version string for RECIPE on TRACK."
  (pcase (builder-elpa-canonical-track track)
    ('elpaish (builder-elpa-recipe-built-version-elpaish recipe))
    ('elpaish-stable (builder-elpa-recipe-built-version-stable recipe))
    ('elpaish-staging (builder-elpa-recipe-built-version-staging recipe))
    (_ (builder-elpa-recipe-built-version recipe))))

;;; Git & Path Resolution

(defun builder-elpa--resolve-repo-path (recipe)
  "Return working directory for RECIPE, cloning or fetching if remote Git URL."
  (let ((repo-target (builder-elpa-recipe-repo recipe)))
    (if (and (stringp repo-target)
             (file-directory-p (expand-file-name repo-target)))
        (expand-file-name repo-target)
      ;; Remote Git repository target
      (let* ((name (builder-elpa-recipe-name recipe))
             (branch (or (builder-elpa-recipe-branch recipe) "main"))
             (pkg-dir (expand-file-name name builder-elpa-work-dir)))
        (make-directory builder-elpa-work-dir t)
        (if (file-exists-p (expand-file-name ".git" pkg-dir))
            (let ((default-directory pkg-dir))
              (magit-git-string "fetch" "origin")
              (magit-git-string "checkout" branch)
              (magit-git-string "reset" "--hard" (concat "origin/" branch)))
          (magit-git-string "clone" "--branch" branch repo-target pkg-dir))
        pkg-dir))))

(defun builder-elpa--current-hash (repo-dir)
  "Get current HEAD hash in REPO-DIR."
  (let ((default-directory repo-dir))
    (or (magit-git-string "rev-parse" "HEAD") "uncommitted")))

(defun builder-elpa--commit-delta (repo-dir built-hash)
  "Calculate commit count between BUILT-HASH and HEAD in REPO-DIR."
  (let ((default-directory repo-dir))
    (if (seq-contains-p '(nil "" "uncommitted") built-hash)
        "New"
      (or (magit-git-string "rev-list" "--count" (concat built-hash "..HEAD"))
          "0"))))

;;; Track Version Derivation Engine

(defun builder-elpa--get-snapshot-version (repo-dir)
  "Return pure UTC date version string (YYYYMMDD.HHMMSS) for REPO-DIR."
  (let ((default-directory repo-dir))
    (or (and (file-directory-p (expand-file-name ".git" repo-dir))
             (magit-git-string "log" "-1" "--format=%cd" "--date=format-local:%Y%m%d.%H%M%S"))
        (format-time-string "%Y%m%d.%H%M%S" nil t))))

(defun builder-elpa--stable-tag-p (tag)
  "Return non-nil if TAG is a clean semver release tag (excluding pre-releases)."
  (and (stringp tag)
       (string-match-p "\\`v?[0-9]+\\.[0-9]+\\(?:\\.[0-9]+\\)*\\'" tag)
       (not (string-match-p "[-._]\\(?:rc\\|pre\\|beta\\|alpha\\|dev\\|preview\\)" tag))))

(defun builder-elpa--clean-semver-string (tag)
  "Strip leading \\='v\\=' from TAG."
  (if (string-prefix-p "v" tag)
      (substring tag 1)
    tag))

(defun builder-elpa--get-stable-version (repo-dir)
  "Return highest clean stable semver tag version in REPO-DIR, or nil if none."
  (let* ((default-directory repo-dir))
    (when (file-directory-p (expand-file-name ".git" repo-dir))
      (let* ((raw-tags-str (or (magit-git-string "tag" "-l" "--sort=-v:refname") ""))
             (all-tags (split-string raw-tags-str "\n" t))
             (stable-tags (seq-filter #'builder-elpa--stable-tag-p all-tags)))
        (when stable-tags
          (builder-elpa--clean-semver-string (car stable-tags)))))))

(defun builder-elpa--normalize-staging-version (raw-ver)
  "Normalize RAW-VER string so it parses cleanly into a valid `version-to-list'."
  (let ((clean (if (string-prefix-p "v" raw-ver) (substring raw-ver 1) raw-ver)))
    ;; Handle git-describe format: 1.2.0-4-gabcdef -> 1.2.0.4
    (if (string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)*\\)[-. ]+\\([0-9]+\\)[-. ]+g[0-9a-fA-F]+\\'" clean)
        (format "%s.%s" (match-string 1 clean) (match-string 2 clean))
      ;; Replace hyphens with dots
      (setq clean (replace-regexp-in-string "-+" "." clean))
      ;; Clean up double dots or dotted pre-release: 1.2.0.rc.1 -> 1.2.0.rc1
      (setq clean (replace-regexp-in-string "\\.\\(rc\\|pre\\|beta\\|alpha\\)\\." ".\\1" clean))
      ;; Validate with version-to-list
      (condition-case nil
          (progn (version-to-list clean) clean)
        (error
         (let ((nums (seq-filter (lambda (s) (string-match-p "\\`[0-9]+\\'" s))
                                 (split-string clean "[^0-9a-zA-Z]+" t))))
           (if nums
               (string-join nums ".")
             (format-time-string "%Y%m%d.%H%M%S" nil t))))))))

(cl-defun builder-elpa--get-staging-version (repo-dir)
  "Return pre-release or git-describe version string for REPO-DIR."
  (let ((default-directory repo-dir))
    (unless (file-directory-p (expand-file-name ".git" repo-dir))
      (cl-return-from builder-elpa--get-staging-version
        (format "0.0.0.%s" (builder-elpa--get-snapshot-version repo-dir))))
    ;; 1. Check if any pre-release tags exist
    (let* ((raw-tags-str (or (magit-git-string "tag" "-l" "--sort=-v:refname") ""))
           (all-tags (split-string raw-tags-str "\n" t))
           (pre-tags (seq-filter (lambda (tg)
                                   (and (string-match-p "\\`v?[0-9]" tg)
                                        (string-match-p "[-._]\\(?:rc\\|pre\\|beta\\|alpha\\)" tg)))
                                 all-tags)))
      (if pre-tags
          (builder-elpa--normalize-staging-version (car pre-tags))
        ;; 2. Fall back to git describe or commit count
        (let ((desc (or (magit-git-string "describe" "--tags" "--always" "--long")
                        (magit-git-string "describe" "--always"))))
          (cond
           ((and desc (string-match "\\`v?\\([0-9]+\\.[0-9]+\\(?:\\.[0-9]+\\)*\\)-\\([0-9]+\\)-g\\([0-9a-fA-F]+\\)\\'" desc))
            (let ((tag-part (match-string 1 desc))
                  (commits-ahead (match-string 2 desc)))
              (if (string= commits-ahead "0")
                  (builder-elpa--clean-semver-string tag-part)
                (format "%s.%s" tag-part commits-ahead))))
           (t
            (let ((count (or (magit-git-string "rev-list" "--count" "HEAD") "1")))
              (format "0.0.0.%s" count)))))))))

(defun builder-elpa-derive-version (recipe track)
  "Derive the package version string for RECIPE on TRACK.
TRACK is one of `elpaish', `elpaish-stable', or `elpaish-staging'.
Returns nil for `elpaish-stable' if no clean stable tag is present."
  (let* ((repo-dir (builder-elpa--resolve-repo-path recipe))
         (canon (builder-elpa-canonical-track track)))
    (pcase canon
      ('elpaish
       (builder-elpa--get-snapshot-version repo-dir))
      ('elpaish-stable
       (builder-elpa--get-stable-version repo-dir))
      ('elpaish-staging
       (builder-elpa--get-staging-version repo-dir))
      (_
       (builder-elpa--get-snapshot-version repo-dir)))))

;;; In-Memory Version Header Injection & Packaging

(defun builder-elpa--inject-version-header (version-str)
  "Ensure current buffer has a `;; Version: VERSION-STR' header line."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^;;\\s-*\\(?:Package-\\)?Version:\\s-*.*$" nil t)
        (replace-match (format ";; Version: %s" version-str))
      (goto-char (point-min))
      (if (re-search-forward "^;;\\s-*Author:" nil t)
          (beginning-of-line)
        (forward-line 1))
      (insert (format ";; Version: %s\n" version-str)))))
(defun builder-elpa--collect-files (repo-dir patterns &optional pkg-name)
  "Collect relative file paths in REPO-DIR matching PATTERNS, excluding tests and generated descriptor files."
  (let ((default-directory repo-dir)
        (name-str (and pkg-name (if (symbolp pkg-name) (symbol-name pkg-name) pkg-name))))
    (thread-last (or patterns '("*.el"))
      (seq-mapcat #'file-expand-wildcards)
      (seq-filter #'file-regular-p)
      (seq-remove (lambda (f)
                    (let ((base (file-name-nondirectory f)))
                      (or (string-match-p "\\`\\.#" base)
                          (string-suffix-p ".elc" base)
                          (string-suffix-p "-autoloads.el" base)
                          (and name-str (string= base (format "%s-pkg.el" name-str)))
                          (string-match-p "\\`test/" f)
                          (string-match-p "\\`tests/" f)
                          (string-prefix-p "test-" base)
                          (string-suffix-p "-test.el" base)
                          (string-suffix-p "-tests.el" base)))))
      (seq-uniq))))

(defun builder-elpa--generate-pkg-file (dest-file name version-str summary reqs url keywords)
  "Write `<pkg>-pkg.el' descriptor at DEST-FILE."
  (with-temp-file dest-file
    (insert ";; -*- no-byte-compile: t -*-\n")
    (let ((req-forms (mapcar (lambda (r)
                               (let ((dep (car r))
                                     (ver (cadr r)))
                                 (list dep (if (stringp ver) ver (package-version-join ver)))))
                             reqs))
          (extra-kws (append (when url `(:url ,url))
                             (when keywords `(:keywords ,@keywords)))))
      (insert (format "(define-package %S %S %S\n  '%S\n"
                      name version-str (or summary "No description") req-forms))
      (when extra-kws
        (insert (format "  %s" (mapconcat (lambda (x) (format "%S" x)) extra-kws " "))))
      (insert ")\n"))))

(defun builder-elpa--create-tar-package (repo-dir dest-file pkg-name-ver files
                                                  name version-str summary reqs url keywords)
  "Create a tar package at `DEST-FILE' for `FILES' in `REPO-DIR' named `PKG-NAME-VER'."
  (let* ((temp-dir (make-temp-file "builder-elpa-pkg-" t))
         (pkg-subdir (expand-file-name pkg-name-ver temp-dir)))
    (unwind-protect
        (progn
          (make-directory pkg-subdir t)
          (dolist (rel-file files)
            (let ((dst (expand-file-name rel-file pkg-subdir)))
              (make-directory (file-name-directory dst) t)
              (copy-file (expand-file-name rel-file repo-dir) dst t)))
          ;; Generate <pkg>-pkg.el inside tarball root
          (let ((pkg-file (expand-file-name (format "%s-pkg.el" name) pkg-subdir)))
            (builder-elpa--generate-pkg-file pkg-file name version-str summary reqs url keywords))
          (let ((default-directory temp-dir))
            (call-process "tar" nil nil nil "-cf" dest-file pkg-name-ver)))
      (delete-directory temp-dir t))))

;;; GPG Package & Archive Signing Pipeline

(defun builder-elpa--get-signing-key ()
  "Resolve active GPG key ID from custom var or environment."
  (or builder-elpa-gpg-key
      (getenv "ELPAISH_SIGNING_KEY")
      (getenv "ELPAISH_GPG_KEY")
      nil))

(defun builder-elpa--get-signing-passphrase ()
  "Resolve GPG passphrase from custom var or environment."
  (or builder-elpa-gpg-passphrase
      (getenv "ELPAISH_GPG_PASSPHRASE")
      nil))

(defun builder-elpa--sign-with-gpg-cli (file sig-file key-id passphrase)
  "Sign FILE generating detached signature SIG-FILE using `gpg' CLI.
KEY-ID is the signing key. PASSPHRASE is the optional passphrase."
  (let* ((args (list "--batch" "--yes" "--detach-sign" "--pinentry-mode" "loopback"))
         (args (if key-id (append args (list "--default-key" key-id)) args))
         (args (if passphrase (append args (list "--passphrase-fd" "0")) args))
         (args (append args (list "--output" sig-file file))))
    (with-temp-buffer
      (when passphrase (insert passphrase "\n"))
      (apply #'call-process-region (point-min) (point-max) "gpg" t t nil args))))

(defun builder-elpa--sign-with-epg (file sig-file key-id)
  "Sign FILE generating detached signature SIG-FILE using EPG.
KEY-ID is the signing key."
  (let ((context (epg-make-context 'OpenPGP)))
    (when key-id
      (setf (epg-context-signers context) (epg-list-keys context key-id)))
    (epg-sign-file context file sig-file 'detached)))

(defun builder-elpa--sign-file (file)
  "Generate a detached GPG signature `FILE.sig' for FILE if signing is enabled."
  (let ((key-id (builder-elpa--get-signing-key))
        (passphrase (builder-elpa--get-signing-passphrase))
        (sig-file (concat file ".sig")))
    (when (or builder-elpa-sign-packages key-id)
      (when (file-exists-p sig-file)
        (delete-file sig-file))
      (let ((signed-p nil))
        (when (executable-find "gpg")
          (let ((exit-code (builder-elpa--sign-with-gpg-cli file sig-file key-id passphrase)))
            (setq signed-p (and (numberp exit-code) (zerop exit-code) (file-exists-p sig-file)))))
        (unless signed-p
          ;; Fall back to epg if CLI signing did not produce signature
          (condition-case nil
              (progn
                (builder-elpa--sign-with-epg file sig-file key-id)
                (setq signed-p (file-exists-p sig-file)))
            (t nil)))
        (if (file-exists-p sig-file)
            (message "Signed %s -> %s"
                     (file-name-nondirectory file)
                     (file-name-nondirectory sig-file))
          (message "Warning: Failed to sign %s" (file-name-nondirectory file)))))))

;;;###autoload
(defun builder-elpa-setup-signing ()
  "Interactive wizard to guide the user through selecting a GPG signing key."
  (interactive)
  (unless (executable-find "gpg")
    (user-error "GPG executable not found in system PATH"))
  (let* ((keys (or (epg-list-keys (epg-make-context 'OpenPGP) "" t)
                   (user-error "No secret GPG keys found. Please generate a GPG key first using `gpg --full-generate-key'")))
         (table (thread-last keys
                  (seq-map (lambda (k)
                             (cons (epg-sub-key-id (car (epg-key-sub-key-list k)))
                                   (epg-user-id-string (car (epg-key-user-id-list k))))))))
         (key-id (annotated-completing-read table
                                            :prompt "Select GPG key for signing ELPA packages: "
                                            :require-match t)))
    (setq builder-elpa-gpg-key key-id
          builder-elpa-sign-packages t)
    (message "GPG package signing enabled! Selected Key ID: %s." key-id)))

;;; Key Lifecycle, Subkey Rotation & Secret Sync

(defun builder-elpa-export-keyring (&optional output-dir key-id)
  "Export binary `elpaish-keyring.gpg' and armored `elpaish.pub.asc' to OUTPUT-DIR."
  (let* ((target-dir (or output-dir builder-elpa-output-dir))
         (key (or key-id (builder-elpa--get-signing-key) ""))
         (gpg-bin (executable-find "gpg")))
    (when gpg-bin
      (make-directory target-dir t)
      (let ((binary-ring (expand-file-name "elpaish-keyring.gpg" target-dir))
            (armor-pub (expand-file-name "elpaish.pub.asc" target-dir)))
        (call-process gpg-bin nil nil nil "--batch" "--yes" "--output" binary-ring "--export" key)
        (call-process gpg-bin nil nil nil "--batch" "--yes" "--armor" "--output" armor-pub "--export" key)
        (message "Exported public keyrings to %s and %s" binary-ring armor-pub)))))

;;;###autoload
(cl-defun builder-elpa-rotate-keys (&key master-key-id repo-slug (output-dir builder-elpa-output-dir))
  "Rotate GPG signing subkey [S], sync with GitHub secrets, and export updated keyring.
MASTER-KEY-ID defaults to the primary certification key.
REPO-SLUG defaults to \"tychoish/elpaish\"."
  (interactive)
  (unless (executable-find "gpg")
    (user-error "GPG binary not found in PATH"))
  (let* ((master (or master-key-id
                     (if (called-interactively-p 'interactive)
                         (read-string "Primary / Master GPG Key ID or Fingerprint: " (builder-elpa--get-signing-key))
                       (builder-elpa--get-signing-key))
                     (user-error "No master key ID specified")))
         (target-repo (or repo-slug "tychoish/elpaish"))
         (gpg-bin (executable-find "gpg"))
         (gh-bin (executable-find "gh")))

    (message "Generating new 1-year signing subkey for %s..." master)
    (call-process gpg-bin nil nil nil "--batch" "--quick-add-key" master "ed25519" "sign" "1y")
    (builder-elpa-export-keyring output-dir master)

    (if (not gh-bin)
        (message "GitHub CLI `gh' not found; skipped automated secret sync.")
      (with-temp-buffer
        (let ((export-code (call-process gpg-bin nil t nil "--batch" "--armor" "--export-secret-subkeys" master)))
          (if (not (zerop export-code))
              (message "Warning: Failed to export secret subkeys for GitHub secret sync.")
            (let ((secret-str (buffer-string)))
              (with-temp-buffer
                (insert secret-str)
                (let ((gh-code (call-process-region (point-min) (point-max) gh-bin nil nil nil
                                                    "secret" "set" "ELPAISH_SIGNING_KEY" "-R" target-repo)))
                  (if (zerop gh-code)
                      (message "Successfully synchronized ELPAISH_SIGNING_KEY secret to %s!" target-repo)
                    (message "Warning: `gh secret set` failed with exit code %d" gh-code)))))))))

    (message "Key rotation complete for %s." master)))

;;;###autoload
(defun builder-elpa-revoke-key (key-id &optional output-dir)
  "Publish revocation certificate for KEY-ID to `elpaish.rev.asc' in OUTPUT-DIR."
  (interactive "sKey ID or Fingerprint to revoke: ")
  (unless (executable-find "gpg")
    (user-error "GPG binary not found in PATH"))
  (unless (and key-id (not (string-empty-p key-id)))
    (user-error "No key ID provided for revocation"))
  (let* ((target-dir (or output-dir builder-elpa-output-dir))
         (rev-file (expand-file-name "elpaish.rev.asc" target-dir))
         (gpg-bin (executable-find "gpg")))
    (make-directory target-dir t)
    (call-process gpg-bin nil nil nil "--batch" "--yes" "--armor" "--output" rev-file "--gen-revoke" key-id)
    (builder-elpa-export-keyring target-dir)
    (message "Published revocation certificate to %s" rev-file)))

;;; Preflight Package Quality Gates

(defun builder-elpa--resolve-checks-file ()
  "Locate `run-checks.el' across load-path, default-directory, and user configuration."
  (or (locate-library "run-checks")
      (let ((p (expand-file-name "scripts/run-checks.el" default-directory)))
        (and (file-exists-p p) p))
      (when-let* ((lib-loc (locate-library "builder-elpa"))
                  (p (expand-file-name "../scripts/run-checks.el" (file-name-directory lib-loc))))
        (and (file-exists-p p) p))
      (and (boundp 'user-emacs-directory)
           (let ((p (expand-file-name "scripts/run-checks.el" user-emacs-directory)))
             (and (file-exists-p p) p)))))

(defun builder-elpa-preflight-package (recipe)
  "Execute preflight quality gates on RECIPE.
Returns t if checks pass, nil if quarantined."
  (if (not builder-elpa-run-preflight)
      t
    (let* ((repo-dir (builder-elpa--resolve-repo-path recipe))
           (checks-file (builder-elpa--resolve-checks-file)))
      (when (and checks-file (file-exists-p checks-file))
        (load checks-file nil t))
      (if (fboundp 'run-checks-package)
          (let* ((skip (builder-elpa-recipe-preflight-skip recipe))
                 (tdir (builder-elpa-recipe-test-dir recipe))
                 (res (run-checks-package repo-dir :test-dir tdir :skip-checks skip))
                 (passed (plist-get res :passed))
                 (errs (plist-get res :errors)))
            (unless passed
              (message "PREFLIGHT QUARANTINE for %s: %d error(s)"
                       (builder-elpa-recipe-name recipe) (length errs))
              (dolist (e errs)
                (message "   - %s" e)))
            passed)
        t))))

;;; Package Build Engine

(defun builder-elpa--extract-buffer-metadata (recipe)
  "Extract package metadata from current buffer or fallback to RECIPE defaults.
Returns a plist with :summary, :reqs, :url, and :keywords."
  (let ((pkg-info (condition-case nil (package-buffer-info) (error nil))))
    (list :summary (or (and pkg-info (package-desc-summary pkg-info))
                       (builder-elpa-recipe-summary recipe)
                       "No description")
          :reqs (or (and pkg-info (package-desc-reqs pkg-info))
                    (builder-elpa-recipe-requires recipe))
          :url (or (and pkg-info (cdr (assoc :url (package-desc-extras pkg-info))))
                   (builder-elpa-recipe-url recipe))
          :keywords (or (and pkg-info (cdr (assoc :keywords (package-desc-extras pkg-info))))
                        (builder-elpa-recipe-keywords recipe)
                        '("tools")))))

(cl-defun builder-elpa-build-package (recipe &optional track output-dir)
  "Build, package, sign, and record status for RECIPE on TRACK.
TRACK is `elpaish', `elpaish-stable', or `elpaish-staging'.
OUTPUT-DIR defaults to track directory under `builder-elpa-output-dir'."
  (let* ((effective-track (builder-elpa-canonical-track (or track builder-elpa-release-mode)))
         (target-dir (or output-dir (builder-elpa-track-dir effective-track)))
         (repo-dir (builder-elpa--resolve-repo-path recipe))
         (name (builder-elpa-recipe-name recipe))
         (main-file (expand-file-name (concat name ".el") repo-dir)))

    (unless (file-exists-p main-file)
      (error "Main file %s not found in %s" (concat name ".el") repo-dir))

    ;; 1. Preflight Validation Gate
    (unless (builder-elpa-preflight-package recipe)
      (message "Skipping %s due to preflight quarantine." name)
      (cl-return-from builder-elpa-build-package nil))

    ;; 2. Derive Version
    (let ((version-str (builder-elpa-derive-version recipe effective-track)))
      (unless version-str
        (when (eq effective-track 'elpaish-stable)
          (message "Omitting %s from elpaish-stable: No clean semver Git tag." name))
        (cl-return-from builder-elpa-build-package nil))

      ;; 3. Build artifact
      (make-directory target-dir t)
      (let* ((files (builder-elpa--collect-files repo-dir (builder-elpa-recipe-files recipe) name))
             (is-tar (> (length files) 1))
             (pkg-type (if is-tar 'tar 'single))
             (pkg-name-ver (format "%s-%s" name version-str))
             (dest-file (expand-file-name (format "%s.%s" pkg-name-ver (if is-tar "tar" "el"))
                                          target-dir))
             meta summary reqs url keywords)

        (with-temp-buffer
          (insert-file-contents main-file)
          (builder-elpa--inject-version-header version-str)
          (setq meta (builder-elpa--extract-buffer-metadata recipe))
          (setq summary (plist-get meta :summary)
                reqs (plist-get meta :reqs)
                url (plist-get meta :url)
                keywords (plist-get meta :keywords))

          (if is-tar
              (builder-elpa--create-tar-package repo-dir dest-file pkg-name-ver files
                                                name version-str summary reqs url keywords)
            (write-region (point-min) (point-max) dest-file nil 'silent)))

        ;; 4. Sign artifact
        (builder-elpa--sign-file dest-file)

        ;; 5. Update recipe metadata & status
        (pcase effective-track
          ('elpaish (setf (builder-elpa-recipe-built-version-elpaish recipe) version-str))
          ('elpaish-stable (setf (builder-elpa-recipe-built-version-stable recipe) version-str))
          ('elpaish-staging (setf (builder-elpa-recipe-built-version-staging recipe) version-str)))

        (setf (builder-elpa-recipe-built-type recipe) pkg-type
              (builder-elpa-recipe-summary recipe) summary
              (builder-elpa-recipe-url recipe) url
              (builder-elpa-recipe-keywords recipe) keywords
              (builder-elpa-recipe-requires recipe) reqs
              (builder-elpa-recipe-built-hash recipe) (builder-elpa--current-hash repo-dir))

        (message "Successfully built %s version %s on %s" name version-str effective-track)
        dest-file))))

;;; Archive Contents & Static HTML Generation

(defun builder-elpa-generate-archive-contents (&optional track output-dir)
  "Generate `archive-contents' and its signature for TRACK in OUTPUT-DIR."
  (let* ((effective-track (builder-elpa-canonical-track (or track builder-elpa-release-mode)))
         (target-dir (or output-dir (builder-elpa-track-dir effective-track)))
         (archive-file (expand-file-name "archive-contents" target-dir))
         (recipes (hash-table-values builder-elpa-registry))
         (entries
          (delq nil
                (mapcar
                 (lambda (recipe)
                   (when-let* ((ver-str (builder-elpa-recipe-version-for-track recipe effective-track)))
                     (let* ((name (intern (builder-elpa-recipe-name recipe)))
                            (ver (version-to-list ver-str))
                            (summary (or (builder-elpa-recipe-summary recipe) "No description"))
                            (pkg-type (or (builder-elpa-recipe-built-type recipe) 'single))
                            (reqs (builder-elpa-recipe-requires recipe))
                            (url (builder-elpa-recipe-url recipe))
                            (keywords (builder-elpa-recipe-keywords recipe))
                            (commit (builder-elpa-recipe-built-hash recipe))
                            (extras (delq nil
                                          (list (when url (cons :url url))
                                                (when commit (cons :commit commit))
                                                (when keywords (cons :keywords keywords))))))
                       `(,name . [,ver ,reqs ,summary ,pkg-type ,extras]))))
                 recipes))))
    (make-directory target-dir t)
    (with-temp-file archive-file
      (insert ";; -*- no-byte-compile: t -*-\n")
      (pp `(1 ,@entries) (current-buffer)))
    (builder-elpa--sign-file archive-file)
    archive-file))

(defun builder-elpa-generate-github-index (&optional track output-dir title)
  "Generate static `index.html' package catalog for TRACK in OUTPUT-DIR."
  (let* ((effective-track (builder-elpa-canonical-track (or track builder-elpa-release-mode)))
         (target-dir (or output-dir (builder-elpa-track-dir effective-track)))
         (page-title (or title (format "ELPAish Repository — %s" effective-track)))
         (recipes (hash-table-values builder-elpa-registry))
         (rows
          (delq nil
                (mapcar
                 (lambda (recipe)
                   (when-let* ((ver-str (builder-elpa-recipe-version-for-track recipe effective-track)))
                     (let* ((name (builder-elpa-recipe-name recipe))
                            (summary (or (builder-elpa-recipe-summary recipe) "No description"))
                            (is-tar (eq (builder-elpa-recipe-built-type recipe) 'tar))
                            (artifact (format "%s-%s.%s" name ver-str (if is-tar "tar" "el"))))
                       `(tr nil
                            (td nil (b nil ,name))
                            (td nil (a ((href . ,artifact)) ,ver-str))
                            (td nil ,summary)
                            (td nil (a ((href . ,(concat artifact ".sig"))) "sig"))))))
                 recipes))))
    (make-directory target-dir t)
    (with-temp-file (expand-file-name "index.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print
       `(html nil
              (head nil
                    (meta ((charset . "utf-8")))
                    (title nil ,page-title)
                    (style nil "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:40px auto;max-width:900px;line-height:1.6;color:#222;} table{border-collapse:collapse;width:100%;margin-top:20px;} th,td{padding:10px 14px;border-bottom:1px solid #eee;text-align:left;} th{background:#f8f9fa;} tr:hover{background:#f5f8ff;} a{color:#0969da;text-decoration:none;} a:hover{text-decoration:underline;} .header{margin-bottom:30px;border-bottom:2px solid #eaecef;padding-bottom:15px;}"))
              (body nil
                    (div ((class . "header"))
                         (h1 nil ,page-title)
                         (p nil "Track URL: " (code nil ,(format "https://tychoish.github.io/elpaish/%s/" effective-track)))
                         (p nil (a ((href . "../index.html")) "← Back to Archive Setup & Overview")))
                    (h2 nil "Packages")
                    ,(if (null rows)
                         '(p nil "No packages published in this track.")
                       `(table nil
                               (tr nil
                                   (th nil "Package")
                                   (th nil "Version")
                                   (th nil "Description")
                                   (th nil "Signature"))
                               ,@rows))))))))

(defun builder-elpa-generate-top-index (&optional output-dir)
  "Generate top-level static `index.html' landing page in OUTPUT-DIR."
  (let ((target-dir (or output-dir builder-elpa-output-dir)))
    (make-directory target-dir t)
    (with-temp-file (expand-file-name "index.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print
       `(html nil
              (head nil
                    (meta ((charset . "utf-8")))
                    (title nil "ELPAish: Tychoish Emacs Lisp Package Archives")
                    (style nil "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:40px auto;max-width:960px;line-height:1.6;color:#24292f;padding:0 20px;} .track-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(280px,1fr));gap:20px;margin:30px 0;} .card{border:1px solid #d0d7de;border-radius:6px;padding:20px;background:#f6f8fa;box-shadow:0 1px 3px rgba(0,0,0,0.05);} .card h2{margin-top:0;font-size:1.3em;} pre{background:#24292e;color:#f6f8fa;padding:16px;border-radius:6px;overflow-x:auto;} code{font-family:ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace;} a{color:#0969da;text-decoration:none;} a:hover{text-decoration:underline;} .btn{display:inline-block;padding:8px 16px;background:#2da44e;color:#fff;border-radius:6px;font-weight:600;margin-top:10px;} .btn:hover{background:#2c974b;text-decoration:none;color:#fff;}"))
              (body nil
                    (h1 nil "ELPAish Emacs Package Archives")
                    (p nil "Automated, GPG-signed package publishing tracks for packages authored across the " (b nil "tychoish") " ecosystem.")

                    (div ((class . "track-grid"))
                         (div ((class . "card"))
                              (h2 nil (a ((href . "elpaish/index.html")) "elpaish (Snapshots)"))
                              (p nil "Continuous development snapshots built from the default branch head with pure UTC date versioning (" (code nil "YYYYMMDD.HHMMSS") ").")
                              (a ((class . "btn") (href . "elpaish/index.html")) "Browse Snapshots"))
                         (div ((class . "card"))
                              (h2 nil (a ((href . "elpaish-stable/index.html")) "elpaish-stable (Releases)"))
                              (p nil "Official release builds strictly from clean semver Git tags (" (code nil "vX.Y.Z") "). Repositories without clean tags are omitted.")
                              (a ((class . "btn") (href . "elpaish-stable/index.html")) "Browse Stable"))
                         (div ((class . "card"))
                              (h2 nil (a ((href . "elpaish-staging/index.html")) "elpaish-staging (Pre-release)"))
                              (p nil "Staging release candidates (" (code nil "-rc") ", " (code nil "-pre") ") and " (code nil "git describe") " builds for integration testing.")
                              (a ((class . "btn") (href . "elpaish-staging/index.html")) "Browse Staging")))

                    (h2 nil "Emacs Configuration")
                    (p nil "Add your preferred track to " (code nil "package-archives") " in your " (code nil "init.el") ":")
                    (pre nil
                         (code nil
                               ";; Primary development snapshot track:\n(add-to-list 'package-archives '(\"elpaish\" . \"https://tychoish.github.io/elpaish/elpaish/\") t)\n\n;; Production stable release track:\n(add-to-list 'package-archives '(\"elpaish-stable\" . \"https://tychoish.github.io/elpaish/elpaish-stable/\") t)\n\n;; Pre-release / staging track:\n(add-to-list 'package-archives '(\"elpaish-staging\" . \"https://tychoish.github.io/elpaish/elpaish-staging/\") t)"))

                    (h2 nil "GPG Keyring Verification")
                    (p nil "Packages and index files are GPG signed. Import the public keyring or trust anchor:")
                    (ul nil
                        (li nil (a ((href . "elpaish-keyring.gpg")) "elpaish-keyring.gpg") " — Binary public keyring")
                        (li nil (a ((href . "elpaish.pub.asc")) "elpaish.pub.asc") " — Armored ASCII public key")
                        (li nil (a ((href . "elpaish.rev.asc")) "elpaish.rev.asc") " — Published revocation certificates (if any)"))
                    (pre nil
                         (code nil "gpg --import < elpaish.pub.asc"))))))))

;;; Build Orchestration

;;;###autoload
(defun builder-elpa-build-all (&optional mode output-dir)
  "Build registered packages, generate indexes, and sign archives.
MODE can be `all', `elpaish', `elpaish-stable', or `elpaish-staging'.
Defaults to `builder-elpa-release-mode'. OUTPUT-DIR defaults to `builder-elpa-output-dir'."
  (interactive)
  (let* ((effective-mode (or mode builder-elpa-release-mode))
         (target-root (or output-dir builder-elpa-output-dir))
         (tracks (if (eq effective-mode 'all)
                     builder-elpa-tracks
                   (list (builder-elpa-canonical-track effective-mode)))))
    (make-directory target-root t)
    (dolist (track tracks)
      (let ((track-dir (builder-elpa-track-dir track target-root)))
        (make-directory track-dir t)
        (dolist (recipe (hash-table-values builder-elpa-registry))
          (builder-elpa-build-package recipe track track-dir))
        (builder-elpa-generate-archive-contents track track-dir)
        (builder-elpa-generate-github-index track track-dir)))

    ;; Generate top-level landing page and public keyrings
    (builder-elpa-generate-top-index target-root)
    (builder-elpa-export-keyring target-root)

    (message "ELPAish repository successfully generated at %s" target-root)
    (when (eq major-mode 'builder-elpa-status-mode)
      (builder-elpa-status-refresh))))

;;; Local Preview HTTP Server

;;;###autoload
(defun builder-elpa--http-mime-type (path)
  "Return MIME content-type string for PATH."
  (cond
   ((string-suffix-p ".html" path) "text/html; charset=utf-8")
   ((string-suffix-p ".el" path) "text/plain; charset=utf-8")
   ((string-suffix-p ".sig" path) "application/pgp-signature")
   ((string-suffix-p ".asc" path) "application/pgp-keys")
   ((string-suffix-p ".gpg" path) "application/pgp-keys")
   ((string-suffix-p ".tar" path) "application/x-tar")
   (t "text/plain")))

(defun builder-elpa--handle-http-request (request-str doc-root)
  "Process HTTP REQUEST-STR for document root DOC-ROOT.
Returns a cons cell (HEADERS . BODY-BYTES)."
  (if (string-match "\\`\\(GET\\|HEAD\\)[ \t]+\\([^ \t\r\n?]+\\)" request-str)
      (let* ((method (match-string 1 request-str))
             (req-path (match-string 2 request-str))
             (rel-path (if (or (string= req-path "/") (string-suffix-p "/" req-path))
                           (concat (string-remove-prefix "/" req-path) "index.html")
                         (string-remove-prefix "/" req-path)))
             (file-path (expand-file-name rel-path doc-root)))
        (if (and (file-exists-p file-path) (file-regular-p file-path))
            (let* ((content (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally file-path)
                              (buffer-string)))
                   (mime (builder-elpa--http-mime-type file-path))
                   (headers (format "HTTP/1.1 200 OK\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n"
                                    mime (length content))))
              (cons headers (if (string= method "HEAD") "" content)))
          (cons "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 9\r\nConnection: close\r\n\r\n"
                "Not Found")))
    (cons "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 11\r\nConnection: close\r\n\r\n"
          "Bad Request")))

;;;###autoload
(defun builder-elpa-serve-local (&optional port output-dir)
  "Start a local HTTP server serving the generated `public/' directory at PORT (default 8080)."
  (interactive "P")
  (builder-elpa-stop-server)
  (let* ((server-port (or port 8080))
         (doc-root (file-name-as-directory (or output-dir builder-elpa-output-dir))))
    (unless (file-directory-p doc-root)
      (make-directory doc-root t))
    (setq builder-elpa-server-process
          (make-network-process
           :name "elpaish-preview-server"
           :service server-port
           :server t
           :family 'ipv4
           :host "127.0.0.1"
           :filter
           (lambda (proc string)
             (let ((res (builder-elpa--handle-http-request string doc-root)))
               (process-send-string proc (car res))
               (when (and (cdr res) (not (string-empty-p (cdr res))))
                 (process-send-string proc (cdr res)))
               (delete-process proc)))))
    (message "ELPAish preview server running at http://127.0.0.1:%d/ (root: %s)" server-port doc-root)))

;;;###autoload
(defun builder-elpa-stop-server ()
  "Stop local preview HTTP server if active."
  (interactive)
  (when (process-live-p builder-elpa-server-process)
    (delete-process builder-elpa-server-process)
    (setq builder-elpa-server-process nil)
    (message "ELPAish preview server stopped.")))

;;; Automated Rebuild Timer

(defconst builder-elpa-auto-build-intervals
  '("1 min" "5 mins" "10 mins" "30 mins" "1 hour" "2 hours" "4 hours" "8 hours" "12 hours")
  "Preset interval options for `builder-elpa-start-auto-build'.")

;;;###autoload
(defun builder-elpa-start-auto-build (interval &optional idle)
  "Start scheduled background rebuilds of the ELPA repository.
INTERVAL can be seconds or a time string (e.g. \"1 hour\").
If IDLE is non-nil, run rebuilds when Emacs is idle for INTERVAL."
  (interactive (list (annotated-completing-read
                      (thread-last builder-elpa-auto-build-intervals
                        (seq-map (lambda (i) (cons i (format "Rebuild every %s" i)))))
                      :prompt "Auto-build interval: "
                      :require-match nil
                      :default "1 hour")
                     current-prefix-arg))
  (builder-elpa-stop-auto-build)
  (let* ((secs (cond
                ((numberp interval) interval)
                ((and (stringp interval) (string-match-p "^[0-9]+$" interval))
                 (string-to-number interval))
                ((stringp interval)
                 (or (timer-duration interval) (string-to-number interval)))
                (t (error "Invalid interval: %S" interval)))))
    (setq builder-elpa-timer
          (if idle
              (run-with-idle-timer secs t #'builder-elpa-build-all)
            (run-at-time secs secs #'builder-elpa-build-all)))
    (message "ELPA auto-build started (%s, running every %s)."
             (if idle "when idle" "periodic")
             interval)))

;;;###autoload
(defun builder-elpa-stop-auto-build ()
  "Stop the periodic background rebuild timer if active."
  (interactive)
  (when (timerp builder-elpa-timer)
    (cancel-timer builder-elpa-timer)
    (setq builder-elpa-timer nil)
    (message "ELPA auto-build timer stopped.")))

;;; UI: Tabulated List Registry View

(defvar builder-elpa-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'builder-elpa-status-refresh)
    (define-key map (kbd "b") #'builder-elpa-status-build-at-point)
    (define-key map (kbd "a") #'builder-elpa-build-all)
    (define-key map (kbd "p") #'builder-elpa-status-preflight-at-point)
    (define-key map (kbd "P") #'builder-elpa-status-preflight-all)
    (define-key map (kbd "s") #'builder-elpa-setup-signing)
    (define-key map (kbd "r") #'builder-elpa-rotate-keys)
    (define-key map (kbd "w") #'builder-elpa-serve-local)
    map)
  "Keymap for `builder-elpa-status-mode'.")

(define-derived-mode builder-elpa-status-mode tabulated-list-mode "ELPAish-Builder"
  "Major mode for inspecting and managing ELPAish package tracks."
  (setq tabulated-list-format
        [("Package Name" 24 t)
         ("Path / Repository" 30 t)
         ("Snapshot (elpaish)" 18 t)
         ("Stable" 10 t)
         ("Staging" 14 t)
         ("Hash" 9 nil)
         ("Delta" 10 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun builder-elpa-status-refresh ()
  "Refresh the package registry status table."
  (interactive)
  (setq tabulated-list-entries
        (thread-last (hash-table-values builder-elpa-registry)
          (seq-map (lambda (recipe)
                     (let* ((name (builder-elpa-recipe-name recipe))
                            (repo-dir (builder-elpa--resolve-repo-path recipe))
                            (exists (file-exists-p repo-dir))
                            (curr-hash (if exists (substring (builder-elpa--current-hash repo-dir) 0 7) "N/A"))
                            (delta (if exists (builder-elpa--commit-delta repo-dir (builder-elpa-recipe-built-hash recipe)) "Uncloned"))
                            (snap-ver (or (builder-elpa-recipe-built-version-elpaish recipe) "—"))
                            (stab-ver (or (builder-elpa-recipe-built-version-stable recipe) "—"))
                            (stage-ver (or (builder-elpa-recipe-built-version-staging recipe) "—")))
                       (list name
                             (vector name
                                     (builder-elpa-recipe-repo recipe)
                                     snap-ver
                                     stab-ver
                                     stage-ver
                                     curr-hash
                                     (format "+%s" delta))))))))
  (tabulated-list-print t))

;;;###autoload
(defun builder-elpa-status-build-at-point ()
  "Build the package at point across all tracks."
  (interactive)
  (if-let* ((name (tabulated-list-get-id))
            (recipe (gethash name builder-elpa-registry)))
      (progn
        (dolist (track builder-elpa-tracks)
          (builder-elpa-build-package recipe track)
          (builder-elpa-generate-archive-contents track)
          (builder-elpa-generate-github-index track))
        (builder-elpa-generate-top-index)
        (builder-elpa-status-refresh)
        (message "Rebuilt %s across all tracks." name))
    (user-error "No recipe found at point")))

;;;###autoload
(defun builder-elpa-status-preflight-at-point ()
  "Run preflight checks for package at point."
  (interactive)
  (if-let* ((name (tabulated-list-get-id))
            (recipe (gethash name builder-elpa-registry)))
      (if (builder-elpa-preflight-package recipe)
          (message "✓ Preflight checks passed for %s" name)
        (message "✗ Preflight checks failed for %s" name))
    (user-error "No recipe found at point")))

;;;###autoload
(defun builder-elpa-status-preflight-all ()
  "Run preflight checks for all registered packages."
  (interactive)
  (let ((passed-count 0)
        (failed-count 0))
    (dolist (recipe (hash-table-values builder-elpa-registry))
      (if (builder-elpa-preflight-package recipe)
          (cl-incf passed-count)
        (cl-incf failed-count)))
    (message "Preflight results: %d passed, %d quarantined." passed-count failed-count)))

;;;###autoload
(defun builder-elpa-status ()
  "Open the ELPAish Builder management buffer."
  (interactive)
  (let ((buf (get-buffer-create "*builder-elpa-status*")))
    (with-current-buffer buf
      (builder-elpa-status-mode)
      (builder-elpa-status-refresh))
    (switch-to-buffer buf)))

;;;###autoload
(defun builder-elpa-run-checks ()
  "Run package quality checks from scripts/run-checks.el."
  (interactive)
  (let ((checks-file (builder-elpa--resolve-checks-file)))
    (if (and checks-file (file-exists-p checks-file))
        (progn
          (load checks-file nil t)
          (if (fboundp 'acr-run-all-checks)
              (call-interactively #'acr-run-all-checks)
            (error "Function `acr-run-all-checks' not found in %s" checks-file)))
      (error "Checks script not found at %s" checks-file))))

(provide 'builder-elpa)
;;; builder-elpa.el ends here
