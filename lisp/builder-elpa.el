;;; builder-elpa.el --- Build local ELPA repos for GitHub Pages -*- lexical-binding: t; -*-

;; Author: Custom ELPA Builder
;; Keywords: maintenance, tools, local
;; Package-Requires: ((emacs "27.1") (magit "3.0.0") (map "3.0") (seq "2.0"))

;;; Commentary:
;; This package manages, builds, and publishes a MELPA-style ELPA package
;; repository hostable on GitHub Pages or local web servers.
;;
;; =============================================================================
;; Setting Up the Target Repository & CI Workflow
;; =============================================================================
;;
;; 1. CREATE THE REPOSITORY
;;    Create a Git repository (e.g. `my-custom-elpa`) on GitHub or your local
;;    git host.
;;
;; 2. DEFINE PACKAGES IN AN ELISP FILE (`packages.el`)
;;    Create a file named `packages.el` in the root of your repository listing
;;    your local or remote packages:
;;
;;      (require 'builder-elpa)
;;
;;      ;; Register local package repositories
;;      (builder-elpa-register-package 'my-local-mode "../my-local-mode"
;;                                      :branch "main")
;;
;;      ;; Register remote package repositories
;;      (builder-elpa-register-package 'another-pkg "https://github.com/user/another-pkg.git"
;;                                      :branch "develop"
;;                                      :files '("*.el" "extensions/*.el"))
;;
;; 3. CREATE A BOOTSTRAP / CI SCRIPT (`build.el`)
;;    Create a headless Elisp build script to run in CI (e.g., GitHub Actions):
;;
;;      ;; build.el
;;      (require 'package)
;;      (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;      (package-initialize)
;;      (unless (package-installed-p 'magit)
;;        (package-refresh-contents)
;;        (package-install 'magit))
;;
;;      (add-to-list 'load-path default-directory)
;;      (require 'builder-elpa)
;;
;;      ;; Configure CI output path
;;      (setq builder-elpa-output-dir (expand-file-name "public/" default-directory))
;;      (setq builder-elpa-sign-packages nil) ; Skip signing in un-trusted CI
;;
;;      ;; Load definitions and build archive
;;      (load (expand-file-name "packages.el" default-directory))
;;      (builder-elpa-build-all)
;;
;; 4. GITHUB ACTIONS WORKFLOW (`.github/workflows/pages.yml`)
;;    Configure GitHub Actions to run `emacs -Q --script build.el` and deploy
;;    the `public/` directory to GitHub Pages.
;;
;; 5. LOCAL REBUILDING & SIGNING WORKFLOW
;;    For local development with GPG package signing:
;;
;;      ;; Run interactively in Emacs:
;;      M-x builder-elpa-setup-signing  ; Configure local GPG key
;;      M-x builder-elpa-status         ; Open management buffer
;;
;;    Keybindings in `*builder-elpa-status*`:
;;      `b` - Fetch & build package at point
;;      `a` - Build all registered packages and publish archive
;;      `g` - Refresh table view and Git commit deltas
;;
;;    Automated rebuilding:
;;      (builder-elpa-start-auto-build "1 hour")
;;      (builder-elpa-stop-auto-build)

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
  "Custom ELPA package repository builder."
  :group 'development)

(defcustom builder-elpa-output-dir (expand-file-name "public/" default-directory)
  "Directory where built packages, signatures, and `archive-contents' are written."
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
  "GPG key ID or email used to sign packages. If nil, default GPG key is used."
  :type '(choice (const :tag "Default Key" nil)
                 (string :tag "Key ID or Email"))
  :group 'builder-elpa)

(defcustom builder-elpa-release-mode 'unstable
  "Release mode for building packages and `archive-contents'.
Can be `unstable' (date-based snapshots from tip), `stable' (semver tags),
or `both' (builds separate stable/ and unstable/ subdirectories)."
  :type '(choice (const :tag "Unstable (Date-based snapshots)" unstable)
                 (const :tag "Stable (Semver tags)" stable)
                 (const :tag "Both (Separate subfolders)" both))
  :group 'builder-elpa)

(defcustom builder-elpa-version-include-header t
  "When non-nil, include package header version as prefix in unstable date versions."
  :type 'boolean
  :group 'builder-elpa)

;;; Registry Data Structure

(cl-defstruct (builder-elpa-recipe (:constructor builder-elpa-recipe-create))
  "Structure representing an ELPA package build recipe."
  (name
   nil
   :type string
   :documentation "Package name.")
  (repo
   nil
   :type string
   :documentation "Local directory path or Git URL.")
  (branch
   "main"
   :type string
   :documentation "Git branch to track.")
  (files
   '("*.el")
   :type list
   :documentation "List of file patterns to include.")
  (built-version
   nil
   :type (choice null string)
   :documentation "Version string when last built.")
  (built-hash
   nil
   :type (choice null string)
   :documentation "Git commit hash when last built.")
  (built-type
   'single
   :type symbol
   :documentation "Package archive type ('single or 'tar).")
  (summary
   "No description"
   :type string
   :documentation "Short package description."))

(defvar builder-elpa-registry (make-hash-table :test 'equal)
  "Registry storing package recipes keyed by package name.")

(defvar builder-elpa-timer nil
  "Timer object for scheduled repository auto-rebuilds.")

;;;autoload
(cl-defun builder-elpa-register-package (name repo &key (branch "main") (files '("*.el")))
  "Register package NAME with REPO local directory path or remote Git URL.
BRANCH defaults to \"main\" and FILES defaults to \\='(\"*.el\")."
  (let ((name-str (if (symbolp name) (symbol-name name) name)))
    (setf (map-elt builder-elpa-registry name-str)
          (builder-elpa-recipe-create
           :name name-str
           :repo (expand-file-name repo)
           :branch branch
           :files files
           :built-version nil
           :built-hash nil
           :summary "No description"))))
;;; GPG Package & Archive Signing

(defun builder-elpa--sign-file (file)
  "Generate a detached GPG signature `FILE.sig' for FILE if signing is enabled."
  (when-let* ((_ builder-elpa-sign-packages)
              (sig-file (concat file ".sig"))
              (context (epg-make-context 'OpenPGP)))
    (when builder-elpa-gpg-key
      (if-let* ((keys (epg-list-keys context builder-elpa-gpg-key)))
          (setf (epg-context-signers context) keys)
        (error "GPG Key '%s' not found" builder-elpa-gpg-key)))
    (when (file-exists-p sig-file)
      (delete-file sig-file))
    (epg-sign-file context file sig-file 'detached)
    (message "Signed %s -> %s"
             (file-name-nondirectory file)
             (file-name-nondirectory sig-file))))

;;;autoload
(defun builder-elpa-setup-signing ()
  "Interactive wizard to guide the user through enabling package signing."
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

;;; Git & Path Helpers

(defun builder-elpa--resolve-repo-path (recipe)
  "Return working directory for RECIPE, cloning/fetching if remote Git URL."
  (let ((repo-target (builder-elpa-recipe-repo recipe)))
    (unless (file-directory-p repo-target)
      (let* ((name (builder-elpa-recipe-name recipe))
             (branch (or (builder-elpa-recipe-branch recipe) "main"))
             (pkg-dir (expand-file-name name builder-elpa-work-dir)))
        (make-directory builder-elpa-work-dir t)
        (if-let* ((_ (file-exists-p pkg-dir)))
            (let ((default-directory pkg-dir))
              (magit-git-string "fetch" "origin")
              (magit-git-string "checkout" branch)
              (magit-git-string "reset" "--hard" (concat "origin/" branch)))
          (magit-git-string "clone" "--branch" branch repo-target pkg-dir))
        (setq repo-target pkg-dir)))
    repo-target))

(defun builder-elpa--current-hash (repo-dir)
  "Get current HEAD hash in REPO-DIR."
  (let ((default-directory repo-dir))
    (or (magit-git-string "rev-parse" "HEAD") "uncommitted")))

(defun builder-elpa--get-latest-tag (repo-dir)
  "Return raw highest tag in REPO-DIR, or nil if none."
  (let* ((default-directory repo-dir)
         (raw-tag (or (magit-git-string "describe" "--tags" "--abbrev=0")
                      (car (split-string (or (magit-git-string "tag" "--sort=-v:refname") "") "\n" t)))))
    (when (and raw-tag (not (string= raw-tag "")))
      raw-tag)))

(defun builder-elpa--get-unstable-version (repo-dir header-version)
  "Return date-based version string for REPO-DIR given HEADER-VERSION."
  (let* ((default-directory repo-dir)
         (date-str (or (magit-git-string "log" "-1" "--format=%cd" "--date=format:%Y%m%d.%H%M%S")
                       (format-time-string "%Y%m%d.%H%M%S"))))
    (if (and builder-elpa-version-include-header header-version)
        (format "%s.%s" header-version date-str)
      date-str)))

(defun builder-elpa--commit-delta (repo-dir built-hash)
  "Calculate commit count between BUILT-HASH and HEAD in REPO-DIR."
  (let ((default-directory repo-dir))
    (if (seq-contains-p '(nil "" "uncommitted") built-hash)
        "New"
      (or (magit-git-string "rev-list" "--count" (concat built-hash "..HEAD"))
          "0"))))

(defun builder-elpa--inject-version-header (version-str)
  "Ensure current buffer has a `;; Version: VERSION-STR' header line."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^;;\\s-*Version:\\s-*.*$" nil t)
        (replace-match (format ";; Version: %s" version-str))
      (goto-char (point-min))
      (if (re-search-forward "^;;\\s-*Author:" nil t)
          (beginning-of-line)
        (forward-line 1))
      (insert (format ";; Version: %s\n" version-str)))))

(defun builder-elpa--collect-files (repo-dir patterns)
  "Collect relative file paths in REPO-DIR matching PATTERNS."
  (let ((default-directory repo-dir))
    (thread-last (or patterns '("*.el"))
      (seq-mapcat #'file-expand-wildcards)
      (seq-filter #'file-regular-p)
      (seq-uniq))))

(defun builder-elpa--create-tar-package (repo-dir dest-file pkg-name-ver files)
  "Create a tar package at `DEST-FILE' for `FILES' in `REPO-DIR' named `PKG-NAME-VER'."
  (let* ((temp-dir (make-temp-file "builder-elpa-pkg-" t))
         (pkg-subdir (expand-file-name pkg-name-ver temp-dir)))
    (make-directory pkg-subdir t)
    (dolist (rel-file files)
      (let ((dst (expand-file-name rel-file pkg-subdir)))
        (make-directory (file-name-directory dst) t)
        (copy-file (expand-file-name rel-file repo-dir) dst t)))
    (let ((default-directory temp-dir))
      (call-process "tar" nil nil nil "-cf" dest-file pkg-name-ver))
    (delete-directory temp-dir t)))

(defun builder-elpa-build-package (recipe &optional mode output-dir)
  "Fetch/locate, parse metadata, build, sign, and record status for RECIPE.
MODE can be `stable' or `unstable' (defaults to `builder-elpa-release-mode').
OUTPUT-DIR defaults to `builder-elpa-output-dir'."
  (let* ((mode (or mode (if (eq builder-elpa-release-mode 'both) 'unstable builder-elpa-release-mode)))
         (target-dir (or output-dir builder-elpa-output-dir))
         (repo-dir (builder-elpa--resolve-repo-path recipe))
         (name (builder-elpa-recipe-name recipe))
         (main-file (expand-file-name (concat name ".el") repo-dir)))
    (unless (file-exists-p main-file)
      (error "Main file %s not found in %s" (concat name ".el") repo-dir))
    (make-directory target-dir t)
    (with-temp-buffer
      (insert-file-contents main-file)
      (let* ((pkg-info (package-buffer-info))
             (header-version (package-version-join (package-desc-version pkg-info)))
             (raw-tag (when (eq mode 'stable) (builder-elpa--get-latest-tag repo-dir)))
             (tag-version (when raw-tag
                            (if (string-match "\\([0-9]+\\.[0-9]+\\(?:\\.[0-9]+\\)*\\)" raw-tag)
                                (match-string 1 raw-tag)
                              raw-tag)))
             (version-str (cond
                           ((and (eq mode 'stable) raw-tag)
                            (let ((default-directory repo-dir))
                              (magit-git-string "checkout" raw-tag))
                            tag-version)
                           ((eq mode 'stable)
                            header-version)
                           (t
                            (builder-elpa--get-unstable-version repo-dir header-version))))
             (files (builder-elpa--collect-files repo-dir (builder-elpa-recipe-files recipe)))
             (is-tar (> (length files) 1))
             (pkg-type (if is-tar 'tar 'single))
             (dest-file (expand-file-name (format "%s-%s.%s" name version-str (if is-tar "tar" "el"))
                                          target-dir)))
        (if is-tar
            (builder-elpa--create-tar-package repo-dir dest-file (format "%s-%s" name version-str) files)
          (builder-elpa--inject-version-header version-str)
          (write-region (point-min) (point-max) dest-file nil 'silent))
        (builder-elpa--sign-file dest-file)
        (setf (builder-elpa-recipe-built-version recipe) version-str
              (builder-elpa-recipe-built-type recipe) pkg-type
              (builder-elpa-recipe-summary recipe) (package-desc-summary pkg-info))
        (message "Successfully built %s version %s (%s)" name version-str mode)))))

(defun builder-elpa-generate-archive-contents (&optional output-dir)
  "Generate `archive-contents' and its GPG signature in OUTPUT-DIR (or `builder-elpa-output-dir')."
  (let* ((target-dir (or output-dir builder-elpa-output-dir))
         (archive-file (expand-file-name "archive-contents" target-dir))
         (entries (thread-last (map-values builder-elpa-registry)
                    (seq-filter #'builder-elpa-recipe-built-version)
                    (seq-map (lambda (recipe)
                               (let ((name (intern (builder-elpa-recipe-name recipe)))
                                     (ver (version-to-list (builder-elpa-recipe-built-version recipe)))
                                     (summary (builder-elpa-recipe-summary recipe))
                                     (pkg-type (or (builder-elpa-recipe-built-type recipe) 'single)))
                                 `(,name . [,ver nil ,summary ,pkg-type])))))))
    (with-temp-file archive-file
      (insert ";; -*- no-byte-compile: t -*-\n")
      (pp `(1 ,@entries) (current-buffer)))
    (builder-elpa--sign-file archive-file)))
(defun builder-elpa-generate-github-index (&optional output-dir title)
  "Generate static `index.html' in OUTPUT-DIR with optional TITLE."
  (let ((target-dir (or output-dir builder-elpa-output-dir))
        (page-title (or title "Custom ELPA Repository"))
        (rows (thread-last (map-values builder-elpa-registry)
                (seq-filter #'builder-elpa-recipe-built-version)
                (seq-map (lambda (recipe)
                           `(tr nil
                                (td nil (b nil ,(builder-elpa-recipe-name recipe)))
                                (td nil ,(builder-elpa-recipe-built-version recipe))
                                (td nil ,(builder-elpa-recipe-summary recipe))))))))
    (with-temp-file (expand-file-name "index.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print `(html nil
                        (head nil
                              (title nil ,page-title)
                              (style nil "body{font-family:sans-serif;margin:40px;} table{border-collapse:collapse;width:100%;} th,td{padding:8px 12px;border:1px solid #ddd;text-align:left;}"))
                        (body nil
                              (h1 nil ,page-title)
                              (table nil
                                     (tr nil
                                         (th nil "Package")
                                         (th nil "Version")
                                         (th nil "Description"))
                                     ,@rows)))))))
(defun builder-elpa-generate-top-index (output-dir)
  "Generate top-level static `index.html' linking to stable/ and unstable/ subdirectories."
  (with-temp-file (expand-file-name "index.html" output-dir)
    (insert "<!DOCTYPE html>\n")
    (dom-print `(html nil
                      (head nil
                            (title nil "Custom ELPA Repository")
                            (style nil "body{font-family:sans-serif;margin:40px;} .card{border:1px solid #ddd;padding:20px;margin-bottom:20px;border-radius:4px;} a{color:#0066cc;text-decoration:none;font-size:1.2em;}"))
                      (body nil
                            (h1 nil "Package Archive")
                            (div ((class . "card"))
                                 (h2 nil (a ((href . "stable/index.html")) "Stable Releases"))
                                 (p nil "Packages built from tagged semver releases."))
                            (div ((class . "card"))
                                 (h2 nil (a ((href . "unstable/index.html")) "Unstable Snapshots"))
                                 (p nil "Packages built automatically from the main repository tip.")))))))
;;;autoload
(defun builder-elpa-build-all (&optional mode output-dir)
  "Build all registered packages, generate metadata, signatures, and static HTML.
MODE can be `stable', `unstable', or `both'. Defaults to `builder-elpa-release-mode'.
OUTPUT-DIR defaults to `builder-elpa-output-dir'."
  (interactive)
  (let ((effective-mode (or mode builder-elpa-release-mode))
        (target-dir (or output-dir builder-elpa-output-dir)))
    (if (eq effective-mode 'both)
        (let ((stable-dir (expand-file-name "stable/" target-dir))
              (unstable-dir (expand-file-name "unstable/" target-dir)))
          (builder-elpa-build-all 'stable stable-dir)
          (builder-elpa-build-all 'unstable unstable-dir)
          (builder-elpa-generate-top-index target-dir)
          (message "ELPA repository (stable & unstable) successfully generated at %s" target-dir))
      (make-directory target-dir t)
      (mapc (lambda (recipe) (builder-elpa-build-package recipe effective-mode target-dir))
            (map-values builder-elpa-registry))
      (builder-elpa-generate-archive-contents target-dir)
      (builder-elpa-generate-github-index target-dir (format "Custom ELPA Repository (%s)" effective-mode))
      (message "ELPA repository (%s) successfully generated at %s" effective-mode target-dir)
      (when (eq major-mode 'builder-elpa-status-mode)
        (builder-elpa-status-refresh)))))
;;; Automated Rebuild Timer

;;;autoload
(defconst builder-elpa-auto-build-intervals
  '("1 min" "5 mins" "10 mins" "30 mins" "1 hour" "2 hours" "4 hours" "8 hours" "12 hours")
  "Preset interval options for `builder-elpa-start-auto-build'.")

;;;autoload
(defun builder-elpa-start-auto-build (interval &optional idle)
  "Start scheduled background rebuilds of the ELPA repository.
INTERVAL can be seconds or a time string.
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

;;;autoload
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
    map)
  "Keymap for `builder-elpa-status-mode'.")

(define-derived-mode builder-elpa-status-mode tabulated-list-mode "ELPA-Builder"
  "Major mode for inspecting and building custom ELPA packages."
  (setq tabulated-list-format
        [("Package Name" 18 t)
         ("Path / Repository" 35 t)
         ("Built Version" 14 t)
         ("Current Hash" 12 nil)
         ("Delta (+Commits)" 15 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;autoload
(defun builder-elpa-status-refresh ()
  "Refresh the package registry status table."
  (interactive)
  (setq tabulated-list-entries
        (thread-last (map-values builder-elpa-registry)
          (seq-map (lambda (recipe)
                     (let* ((name (builder-elpa-recipe-name recipe))
                            (repo-dir (builder-elpa--resolve-repo-path recipe))
                            (exists (file-exists-p repo-dir))
                            (curr-hash (if exists (substring (builder-elpa--current-hash repo-dir) 0 7) "N/A"))
                            (delta (if exists (builder-elpa--commit-delta repo-dir (builder-elpa-recipe-built-hash recipe)) "Uncloned")))
                       (list name
                             (vector name
                                     (builder-elpa-recipe-repo recipe)
                                     (or (builder-elpa-recipe-built-version recipe) "Unbuilt")
                                     curr-hash
                                     (format "+%s commit(s)" delta))))))))
  (tabulated-list-print t))

;;;autoload
(defun builder-elpa-status-build-at-point ()
  "Build the package at current line."
  (interactive)
  (if-let* ((name (tabulated-list-get-id))
            (recipe (map-elt builder-elpa-registry name)))
      (progn
        (builder-elpa-build-package recipe)
        (builder-elpa-generate-archive-contents)
        (builder-elpa-generate-github-index)
        (builder-elpa-status-refresh))
    (user-error "No recipe found at point")))

;;;autoload
(defun builder-elpa-status ()
  "Open the ELPA Builder status buffer."
  (interactive)
  (switch-to-buffer "*builder-elpa-status*")
  (builder-elpa-status-mode)
  (builder-elpa-status-refresh))

(provide 'builder-elpa)
;;; builder-elpa.el ends here
