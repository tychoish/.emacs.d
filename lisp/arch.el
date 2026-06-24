;;; arch.el --- Arch Linux package management -*- lexical-binding: t; -*-

;;; Commentary:
;; Package management UI for Arch Linux with a pluggable backend registry.
;; Backends register search/install/remove/info operations; the pacman and
;; yay backends are provided.  Additional backends can be registered via
;; `arch-register-backend'.
;;
;; Package operations run in per-package buffers named *arch:<pkg>* that
;; accumulate output across rebuilds without clearing.

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)
(require 'ansi-color)
(require 'tabulated-list)
(require 'transient)
(require 'annotated-completing-read)

;;; Backend protocol

(cl-defstruct (arch-backend (:constructor arch-backend--make) (:copier nil))
  "A package manager backend."
  name            ; string identifier, e.g. "pacman" or "yay"
  label           ; display label for UI
  search-fn       ; (fn query) → list of arch-pkg
  info-fn         ; (fn pkg-name) → plist of metadata, nil on failure
  files-fn        ; (fn pkg-name) → list of strings, nil if unavailable
  list-fn         ; (fn) → list of arch-pkg (installed packages)
  list-all-fn       ; (fn) → list of all sync-DB arch-pkg (installed + available, with descriptions)
  foreign-fn        ; (fn) → hash table of foreign package names (string → t)
  upgradeable-fn    ; (fn) → hash table of upgradeable package names (string → t)
  populate-cache-fn      ; (fn) → populate arch--info-cache; nil means use pacman fallback
  aur-list-fn            ; (fn) → list of AUR package name strings (for widened list); nil = unsupported
  install-methods        ; alist of (symbol . fn) — method dispatch table, e.g. ((direct . fn) (abs . fn))
  default-install-method ; symbol — preferred method for this backend; nil defers to prompt
  remove-fn       ; (fn pkg-name)
  upgrade-fn      ; (fn pkg-name) — nil means unsupported
  upgrade-all-fn) ; (fn) — nil means unsupported

(defvar arch--backends (make-hash-table :test #'equal)
  "Registry of arch backends keyed by name string.")

(defun arch-register-backend (backend)
  "Register BACKEND in the arch backend registry."
  (setf (map-elt arch--backends (arch-backend-name backend)) backend))

(defvar arch-default-backend "pacman"
  "Name of the backend used when none is specified.")

(defcustom arch-aur-backend "yay"
  "Name of the backend used for AUR search and upgrade-all operations.
Change this to use an alternative AUR helper such as \"paru\"."
  :type 'string
  :group 'arch)

(defun arch--default-backend ()
  "Return the default backend or signal a user error."
  (or (map-elt arch--backends arch-default-backend)
      (user-error "No arch backend registered for %S" arch-default-backend)))

(defun arch--aur-backend ()
  "Return the AUR backend or signal a user error."
  (or (map-elt arch--backends arch-aur-backend)
      (user-error "AUR backend %S not registered" arch-aur-backend)))

(defcustom arch-default-install-method nil
  "Default install method: `direct', `abs', or `rebuild'.
nil means prompt when more than one method is available.
With a prefix argument, always prompt regardless of this setting."
  :type '(choice (const :tag "Prompt" nil)
                 (const :tag "Package manager" direct)
                 (const :tag "AUR source build" abs)
                 (const :tag "Rebuild existing clone" rebuild))
  :group 'arch)

;;; Package struct

(cl-defstruct (arch-pkg (:constructor arch-pkg--make) (:copier nil))
  "Represents a package known to a backend."
  name          ; string
  version       ; string
  repo          ; string, e.g. "core", "extra", "aur"
  description   ; string
  installed-p   ; boolean — package is installed
  upgradeable-p ; boolean — upgrade available in sync db
  aur-p         ; boolean — foreign/AUR package (not in sync db)
  explicit-p    ; boolean — explicitly installed (not as a dependency)
  required-by-p) ; boolean — required by other installed packages

;;; Faces

(defface arch-face-installed
  '((t :inherit success))
  "Face for installed (official repo, current) packages."
  :group 'arch)

(defface arch-face-upgradeable
  '((t :inherit warning :weight bold))
  "Face for official-repo packages with an available upgrade."
  :group 'arch)

(defface arch-face-aur
  '((t :inherit font-lock-keyword-face))
  "Face for AUR (foreign) packages that are current."
  :group 'arch)

(defface arch-face-aur-stale
  '((t :inherit warning :weight bold))
  "Face for AUR packages that have an available upgrade."
  :group 'arch)

(defface arch-face-available
  '((t :inherit shadow))
  "Face for packages available in the sync db but not installed."
  :group 'arch)

(defface arch-face-orphan
  '((t :inherit error :weight bold))
  "Face for orphaned packages (installed as dep, not required by anyone)."
  :group 'arch)

(defface arch-face-version-old
  '((t :inherit error :weight bold))
  "Face for version numbers of packages that have an available upgrade."
  :group 'arch)

(defface arch-face-pkg-name
  '((t :inherit link :weight bold))
  "Face for package names in the arch info buffer."
  :group 'arch)

(defface arch-face-yaml-key
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for YAML keys in the arch info buffer."
  :group 'arch)

(defface arch-face-pkg-link
  '((t :inherit link))
  "Face for package names in the arch list, styled as hyperlinks."
  :group 'arch)

(defface arch-face-pkg-link-marked
  '((t :inherit link :weight bold))
  "Face for marked package names in the arch list."
  :group 'arch)

;;; Info cache

(defvar arch--info-cache (make-hash-table :test #'equal)
  "Cache mapping package name string → info plist from pacman -Qi/-Si.")

(defun arch--cache-get (pkg-name)
  "Return cached info plist for PKG-NAME, or nil."
  (map-elt arch--info-cache pkg-name))

(defun arch--cache-put (pkg-name plist)
  "Store PLIST in the info cache under PKG-NAME."
  (setf (map-elt arch--info-cache pkg-name) plist))

;;;###autoload
(defun arch-cache-drop ()
  "Clear all cached package info."
  (interactive)
  (clrhash arch--info-cache)
  (message "arch: package cache cleared"))

;;;###autoload
(defun arch-cache-reload ()
  "Clear and repopulate the package info cache from pacman -Qi."
  (interactive)
  (clrhash arch--info-cache)
  (arch--populate-cache)
  (message "arch: cache reloaded (%d packages)" (hash-table-count arch--info-cache)))

(defun arch--pacman-populate-cache ()
  "Populate the info cache from `pacman -Qi', cross-referencing `pacman -Sl' for repo."
  (let ((repo-index
         (map-into
          (seq-map (lambda (pkg) (cons (arch-pkg-name pkg) (arch-pkg-repo pkg)))
                   (arch--parse-sync-list (arch--run-sync (list "pacman" "-Sl"))))
          '(hash-table :test equal))))
    (seq-do
     (lambda (plist)
       (when-let* ((name (plist-get plist 'name)))
         (arch--cache-put name
                          (if-let* ((repo (map-elt repo-index name)))
                              (plist-put plist 'repository repo)
                            plist))))
     (arch--parse-multi-info (arch--run-sync (list "pacman" "-Qi"))))))

(defun arch--populate-cache ()
  "Populate the package info cache via the current backend's populate-cache-fn."
  (if-let* ((fn (arch-backend-populate-cache-fn (arch--default-backend))))
      (funcall fn)
    (arch--pacman-populate-cache)))

(defun arch--cached-info (pkg-name)
  "Return info for PKG-NAME from cache, fetching via backend if absent."
  (or (arch--cache-get pkg-name)
      (when-let* ((plist (funcall (arch-backend-info-fn (arch--default-backend)) pkg-name)))
        (arch--cache-put pkg-name plist)
        plist)))

;;; Shell helpers

(defun arch--run-sync (args &optional require-success)
  "Run ARGS synchronously and return stdout as a string.
If REQUIRE-SUCCESS is non-nil, return nil when the command exits non-zero."
  (with-temp-buffer
    (let ((exit (apply #'call-process (car args) nil t nil (cdr args))))
      (unless (and require-success (not (zerop exit)))
        (buffer-string)))))

(defun arch--pkg-buffer (pkg-name)
  "Return the dedicated operation buffer for PKG-NAME, creating it if needed."
  (get-buffer-create (format "*arch:%s*" pkg-name)))

(defun arch--pkg-run (pkg-name args)
  "Run ARGS in PKG-NAME's dedicated buffer, appending to existing content."
  (let ((buf (arch--pkg-buffer pkg-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "\n$ %s\n" (mapconcat #'identity args " "))
                            'face 'bold))))
    (make-process
     :name (format "arch-%s" pkg-name)
     :buffer buf
     :command args
     :filter #'arch--pkg-filter
     :sentinel #'arch--pkg-sentinel)
    (display-buffer buf)))

(defun arch--pkg-filter (proc output)
  "Insert OUTPUT into PROC's buffer, rendering ANSI escape sequences as faces."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (save-excursion
        (let ((start (marker-position (process-mark proc))))
          (goto-char start)
          (insert output)
          (ansi-color-apply-on-region start (point-max))))
      (set-marker (process-mark proc) (point-max)))))

(defun arch--pkg-sentinel (proc event)
  "Append completion notice for PROC to its buffer."
  (when (memq (process-status proc) '(exit signal))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "[%s]\n" (string-trim event))
                            'face 'font-lock-comment-face))))))

;;; Parsers

(defun arch--parse-search-output (output)
  "Parse `pacman -Ss' OUTPUT into a list of arch-pkg structs."
  (let (pkgs)
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (when (string-match "^\\([^/[:space:]]+\\)/\\([^[:space:]]+\\) \\([^[:space:]]+\\)" line)
            (let ((repo (match-string 1 line))
                  (name (match-string 2 line))
                  (version (match-string 3 line))
                  (installed-p (string-match-p "\\[installed" line)))
              (forward-line 1)
              (let ((desc (string-trim
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
                (push (arch-pkg--make
                       :name name
                       :version version
                       :repo repo
                       :description desc
                       :installed-p installed-p)
                      pkgs))))
          (forward-line 1))))
    (nreverse pkgs)))

(defun arch--parse-info-output (output)
  "Parse `pacman -Qi' or `pacman -Si' OUTPUT for one package into a plist."
  (let (result last-key)
    (seq-do
     (lambda (line)
       (cond
        ((string-match "^\\([^[:space:]][^:]*?\\)\\s-*:\\s-*\\(.*\\)$" line)
         (setq last-key
               (intern (downcase (replace-regexp-in-string
                                  "[[:space:]]+" "-"
                                  (string-trim (match-string 1 line))))))
         (setq result (plist-put result last-key (string-trim (match-string 2 line)))))
        ((and last-key (string-match "^[[:space:]]+\\(\\S-.*\\)$" line))
         (let ((existing (or (plist-get result last-key) "")))
           (setq result (plist-put result last-key
                                   (if (string-empty-p existing)
                                       (match-string 1 line)
                                     (concat existing "  " (match-string 1 line)))))))))
     (split-string output "\n"))
    result))

(defun arch--parse-multi-info (output)
  "Parse `pacman -Qi' OUTPUT for multiple packages into a list of plists."
  (thread-last (split-string output "\n\n" t)
    (seq-map #'arch--parse-info-output)
    (seq-filter #'identity)))

(defun arch--parse-installed-output (output)
  "Parse `pacman -Q' OUTPUT into a list of arch-pkg structs."
  (thread-last (split-string output "\n" t)
    (seq-map (lambda (line)
               (let ((parts (split-string line " " t)))
                 (arch-pkg--make
                  :name (car parts)
                  :version (cadr parts)
                  :repo "local"
                  :installed-p t))))))

(defun arch--foreign-packages ()
  "Return hash table of foreign (non-sync-db, typically AUR) package names."
  (map-into
   (thread-last (split-string (arch--run-sync (list "pacman" "-Qm")) "\n" t)
     (seq-map (lambda (line) (car (split-string line " " t))))
     (seq-filter #'identity)
     (seq-map (lambda (name) (cons name t))))
   '(hash-table :test equal)))

(defun arch--upgradeable-packages ()
  "Return hash table of package names with available upgrades.
Parses `pacman -Qu' output, which has the form \"name old -> new\".
Lines not matching this format (warnings, blank lines) are skipped."
  (map-into
   (thread-last (split-string (arch--run-sync (list "pacman" "-Qu")) "\n" t)
     (seq-filter (lambda (line) (string-match-p "^[^ ]+ [^ ]+ -> " line)))
     (seq-map (lambda (line) (cons (car (split-string line " " t)) t))))
   '(hash-table :test equal)))

;;; Pacman backend implementation

(defun arch--pacman-search (query)
  "Search for QUERY via pacman; return list of arch-pkg."
  (arch--parse-search-output (arch--run-sync (list "pacman" "-Ss" query))))

(defun arch--yay-search (query)
  "Search for QUERY via yay; return list of arch-pkg."
  (arch--parse-search-output (arch--run-sync (list "yay" "-Ss" query))))

(defun arch--pacman-info (pkg-name)
  "Return info plist for PKG-NAME, trying -Qi then -Si."
  (when-let* ((output (or (arch--run-sync (list "pacman" "-Qi" pkg-name) t)
                          (arch--run-sync (list "pacman" "-Si" pkg-name) t))))
    (arch--parse-info-output output)))

(defun arch--pacman-files (pkg-name)
  "Return list of files owned by PKG-NAME, or nil if not installed."
  (when-let* ((output (arch--run-sync (list "pacman" "-Ql" pkg-name) t)))
    (thread-last (split-string output "\n" t)
      (seq-map (lambda (line) (cadr (split-string line " " t))))
      (seq-filter #'identity)
      (seq-remove #'directory-name-p))))

(defun arch--pacman-list ()
  "Return list of installed packages as arch-pkg structs."
  (arch--parse-installed-output (arch--run-sync (list "pacman" "-Q"))))

(defun arch--parse-sync-list (output)
  "Parse `pacman -Sl' OUTPUT into a list of arch-pkg structs."
  (thread-last (split-string output "\n" t)
    (seq-filter (lambda (line) (>= (length (split-string line " " t)) 3)))
    (seq-map (lambda (line)
               (let ((parts (split-string line " " t)))
                 (arch-pkg--make
                  :name (cadr parts)
                  :version (caddr parts)
                  :repo (car parts)
                  :installed-p (string-match-p "\\[installed" line)))))))

(defun arch--pacman-list-all ()
  "Return all packages in the sync DB as arch-pkg structs."
  (arch--parse-sync-list (arch--run-sync (list "pacman" "-Sl"))))

(defun arch--pacman-list-all-with-desc ()
  "Return all sync-DB packages as arch-pkg structs, including descriptions."
  (arch--parse-search-output (arch--run-sync (list "pacman" "-Ss" "."))))

(defun arch--yay-aur-list ()
  "Return list of all AUR package names from `yay -Slaq'."
  (seq-filter #'identity
              (split-string (or (arch--run-sync (list "yay" "-Slaq")) "") "\n" t)))

(defun arch--yay-list-all ()
  "Return all sync-DB packages plus AUR name stubs from `yay -Slaq'.
AUR stubs use version \"<aur>\" and blank description; they update in the
list display once info is fetched and cached via `arch-list-fetch-info'."
  (let* ((sync-pkgs (arch--pacman-list-all-with-desc))
         (sync-names (map-into (seq-map (lambda (p) (cons (arch-pkg-name p) t)) sync-pkgs)
                               '(hash-table :test equal))))
    (append sync-pkgs
            (thread-last (arch--yay-aur-list)
              (seq-remove (lambda (name) (map-elt sync-names name)))
              (seq-map (lambda (name)
                         (arch-pkg--make
                          :name name
                          :version "<aur>"
                          :repo "aur"
                          :description ""
                          :installed-p nil)))))))


(defun arch--pacman-install (pkg-name)
  "Install PKG-NAME via pacman."
  (arch--pkg-run pkg-name (list "sudo" "pacman" "--noconfirm" "--noprogressbar" "-S" pkg-name)))

(defun arch--pacman-remove (pkg-name)
  "Remove PKG-NAME via pacman."
  (arch--pkg-run pkg-name (list "sudo" "pacman" "--noconfirm" "--noprogressbar" "-Rns" pkg-name)))

(defun arch--pacman-upgrade (pkg-name)
  "Upgrade PKG-NAME via pacman."
  (arch--pkg-run pkg-name (list "sudo" "pacman" "--noconfirm" "--noprogressbar" "-S" pkg-name)))

(defun arch--pacman-upgrade-all ()
  "Full system upgrade via pacman."
  (arch--pkg-run "pacman" (list "sudo" "pacman" "--noconfirm" "--noprogressbar" "-Syu")))

(defun arch--yay-install (pkg-name)
  "Install PKG-NAME via yay."
  (arch--pkg-run pkg-name (list "yay" "--noconfirm" "--noprogressbar" "-S" pkg-name)))

(defun arch--yay-remove (pkg-name)
  "Remove PKG-NAME via yay."
  (arch--pkg-run pkg-name (list "yay" "--noconfirm" "--noprogressbar" "-Rns" pkg-name)))

(defun arch--yay-upgrade-all ()
  "Full system upgrade including AUR via yay."
  (arch--pkg-run "yay" (list "yay" "--noconfirm" "--noprogressbar" "-Syu")))

;;; Pacman system operations

;;;###autoload
(defun arch-sync ()
  "Sync package databases (`pacman -Sy')."
  (interactive)
  (arch--pkg-run "pacman" (list "sudo" "pacman" "--noprogressbar" "-Sy")))

;;;###autoload
(defun arch-sync-force ()
  "Force re-download of all package databases (`pacman -Syy')."
  (interactive)
  (arch--pkg-run "pacman" (list "sudo" "pacman" "--noprogressbar" "-Syy")))

;;;###autoload
(defun arch-upgrade-system ()
  "Upgrade installed packages without syncing databases (`pacman -Su')."
  (interactive)
  (when (yes-or-no-p "Upgrade system without syncing databases? ")
    (arch--pkg-run "pacman" (list "sudo" "pacman" "--noconfirm" "--noprogressbar" "-Su"))))

;;; AUR abs build

(defcustom arch-abs-directory (expand-file-name "~/abs/")
  "Directory where AUR packages are cloned for building via makepkg."
  :type 'directory
  :group 'arch)

(defun arch--abs-pkg-dir (pkg-name)
  "Return the abs clone directory for PKG-NAME."
  (expand-file-name pkg-name arch-abs-directory))

(defun arch--abs-clone-url (pkg-name)
  "Return the AUR git URL for PKG-NAME."
  (format "https://aur.archlinux.org/%s.git" pkg-name))

;;;###autoload
(defun arch-abs-install (pkg-name)
  "Install PKG-NAME from AUR: clone to abs dir, then run makepkg -sfi."
  (interactive "sAUR package name: ")
  (make-directory arch-abs-directory t)
  (let* ((pkg-dir (arch--abs-pkg-dir pkg-name))
         (fetch-cmd (if (file-directory-p pkg-dir)
                        (format "git -C %s pull" (shell-quote-argument pkg-dir))
                      (format "git clone %s %s"
                              (shell-quote-argument (arch--abs-clone-url pkg-name))
                              (shell-quote-argument pkg-dir)))))
    (arch--pkg-run pkg-name
                   (list "bash" "-c"
                         (format "%s && cd %s && makepkg -sfi"
                                 fetch-cmd
                                 (shell-quote-argument pkg-dir))))))

;;;###autoload
(defun arch-abs-rebuild (pkg-name)
  "Rebuild PKG-NAME in its existing abs directory without pulling."
  (interactive "sAUR package name: ")
  (let ((pkg-dir (arch--abs-pkg-dir pkg-name)))
    (unless (file-directory-p pkg-dir)
      (user-error "No abs directory for %s; use arch-abs-install first" pkg-name))
    (arch--pkg-run pkg-name
                   (list "bash" "-c"
                         (format "cd %s && makepkg -sfi"
                                 (shell-quote-argument pkg-dir))))))

;;; ACR search/select

(defun arch--select-from-pkgs (pkgs prompt)
  "ACR-select from PKGS list with PROMPT; return selected name or nil."
  (let ((index (map-into
                (seq-map (lambda (p) (cons (arch-pkg-name p) p)) pkgs)
                '(hash-table :test equal))))
    (annotated-completing-read
     (seq-map (lambda (pkg)
                (cons (arch-pkg-name pkg)
                      (format "[%s] %s%s"
                              (or (arch-pkg-repo pkg) "?")
                              (or (arch-pkg-description pkg) "")
                              (if (arch-pkg-installed-p pkg) " [installed]" ""))))
              pkgs)
     :prompt prompt
     :require-match t
     :category 'arch-package
     :group-name (lambda (name)
                   (when-let* ((pkg (map-elt index name)))
                     (or (arch-pkg-repo pkg) "other"))))))

(defconst arch--install-method-labels
  '((direct  . "Install via package manager")
    (abs     . "Build from AUR source (clone + makepkg)")
    (rebuild . "Rebuild existing AUR source clone"))
  "Display labels for install method symbols used in `arch--install-dispatch'.")

(defun arch--install-dispatch (pkg-name)
  "Install PKG-NAME using the active backend's install-methods alist.
Filters out `rebuild' when no abs directory exists.  Method selection:
prefer `arch-default-install-method' (user override), then the backend's
`default-install-method'; auto-selects when only one method is available;
always prompts when called with a prefix argument."
  (let* ((backend (arch--default-backend))
         (all-methods (arch-backend-install-methods backend))
         (methods (seq-filter (lambda (m)
                                (or (not (eq (car m) 'rebuild))
                                    (file-directory-p (arch--abs-pkg-dir pkg-name))))
                              all-methods))
         (effective-default (or arch-default-install-method
                                (arch-backend-default-install-method backend)))
         (method
          (cond
           ((null (cdr methods)) (caar methods))
           ((and effective-default
                 (not current-prefix-arg)
                 (assq effective-default methods))
            effective-default)
           (t (intern (annotated-completing-read
                       (seq-map (lambda (m)
                                  (cons (symbol-name (car m))
                                        (or (cdr (assq (car m) arch--install-method-labels))
                                            (symbol-name (car m)))))
                                methods)
                       :prompt (format "Install %s: " pkg-name)
                       :require-match t))))))
    (when-let* ((fn (cdr (assq method methods))))
      (funcall fn pkg-name))))

;;;###autoload
(defun arch-search (query)
  "Search all registered backends for QUERY and show info for the selection.
Results from all backends are merged and grouped by repo in the ACR interface."
  (interactive "sSearch packages: ")
  (let* ((seen (make-hash-table :test #'equal))
         (pkgs (seq-filter
                (lambda (pkg)
                  (unless (map-elt seen (arch-pkg-name pkg))
                    (setf (map-elt seen (arch-pkg-name pkg)) t)))
                (seq-mapcat
                 (lambda (backend)
                   (when-let* ((fn (arch-backend-search-fn backend)))
                     (funcall fn query)))
                 (map-values arch--backends)))))
    (unless pkgs
      (user-error "No packages found for %S" query))
    (when-let* ((selected (arch--select-from-pkgs pkgs (format "Search [%s]: " query))))
      (arch-show-info selected))))

(defalias 'arch-search-yay #'arch-search)

;;;###autoload
(defun arch-find-package ()
  "ACR-select from all sync-DB packages and open arch-info.
Annotation mirrors the package list columns: [repo]  Stat  (version)  description."
  (interactive)
  (when (zerop (hash-table-count arch--info-cache))
    (arch--populate-cache))
  (let* ((backend (arch--default-backend))
         (pkgs (if-let* ((fn (arch-backend-list-all-fn backend)))
                    (funcall fn)
                  (arch--pacman-list-all)))
         (index (map-into (seq-map (lambda (p) (cons (arch-pkg-name p) p)) pkgs)
                          '(hash-table :test equal)))
         (candidates
          (seq-map (lambda (pkg)
                     (let* ((name (arch-pkg-name pkg))
                            (plist (arch--cache-get name))
                            (status (if plist
                                        (let* ((explicit (equal (plist-get plist 'install-reason)
                                                                "Explicitly installed"))
                                               (req (plist-get plist 'required-by))
                                               (req-p (and req (not (equal req "None")))))
                                          (concat (if explicit "E" "D")
                                                  (if req-p "+req" "   ")))
                                      "     ")))
                       (cons name
                             (format "[%s]  %s  (%s)  %s"
                                     (arch-pkg-repo pkg)
                                     status
                                     (arch-pkg-version pkg)
                                     (or (plist-get plist 'description)
                                         (arch-pkg-description pkg)
                                         "")))))
                   pkgs))
         (selected (annotated-completing-read
                    candidates
                    :prompt "Package: "
                    :require-match t
                    :category 'arch-package
                    :group-name (lambda (name)
                                  (when-let* ((pkg (map-elt index name)))
                                    (arch-pkg-repo pkg))))))
    (when selected
      (arch-show-info selected))))

;;; Package info view

(defun arch--info-buffer-name (pkg-name)
  "Return the buffer name for PKG-NAME's info buffer."
  (format "*arch-info<%s>*" pkg-name))

(defvar-local arch--info-package nil
  "Package name displayed in the current arch-info buffer.")

;;; YAML rendering helpers

(defconst arch--info-fields
  '(("name"           name           nil)
    ("version"        version         nil)
    ("repository"     repository      nil)
    ("description"    description     nil)
    ("url"            url             nil)
    ("licenses"       licenses        nil)
    ("depends-on"     depends-on      t)
    ("optional-deps"  optional-deps   t)
    ("required-by"    required-by     t)
    ("install-reason" install-reason  nil)
    ("install-date"   install-date    nil)
    ("installed-size" installed-size  nil)
    ("packager"       packager        nil))
  "Fields shown in arch-info: (yaml-key plist-symbol multi-value-p).")

(defun arch--yaml-split (value)
  "Split a pacman space-separated multi-value string into a list."
  (seq-filter #'identity (split-string value "[[:space:]]+" t)))

(defun arch--yaml-key-str (key)
  "Return KEY as a propertized YAML key string."
  (propertize (concat key ":") 'face 'arch-face-yaml-key))

(defun arch--yaml-insert-pair (key value)
  "Insert a YAML `key: value' line."
  (insert (arch--yaml-key-str key) " " value "\n"))

(defun arch--yaml-insert-list (key items)
  "Insert a YAML key with a block sequence of ITEMS.
List items containing ': ' are single-quoted to avoid YAML mapping ambiguity."
  (insert (arch--yaml-key-str key) "\n")
  (seq-do (lambda (item)
            (insert "  - "
                    (if (string-match-p ": " item)
                        (format "'%s'" (replace-regexp-in-string "'" "''" item))
                      item)
                    "\n"))
          items))

(defun arch--yaml-insert-url (key url)
  "Insert a YAML key with URL as a clickable hyperlink button."
  (insert (arch--yaml-key-str key) " ")
  (insert-text-button url
                      'action (lambda (_b) (browse-url url))
                      'follow-link t
                      'help-echo "mouse-1: open in browser")
  (insert "\n"))

(defun arch--yaml-insert-eww-url (key url)
  "Insert a YAML key with URL as a button that opens in eww."
  (insert (arch--yaml-key-str key) " ")
  (insert-text-button url
                      'action (lambda (_b) (eww url))
                      'follow-link t
                      'help-echo "mouse-1: open in eww")
  (insert "\n"))

(defun arch--info-render (pkg-name plist files)
  "Render PKG-NAME info from PLIST and FILES as YAML into the current buffer."
  (let ((inhibit-read-only t)
        (aur-p (and (plist-get plist 'install-date)
                    (null (plist-get plist 'repository)))))
    (erase-buffer)
    (seq-do
     (lambda (field)
       (when-let* ((val (plist-get plist (cadr field))))
         (unless (equal val "None")
           (cond
            ((caddr field)
             (arch--yaml-insert-list (car field) (arch--yaml-split val)))
            ((equal (car field) "url")
             (arch--yaml-insert-url (car field) val))
            (t
             (arch--yaml-insert-pair (car field) val))))))
     arch--info-fields)
    (when aur-p
      (arch--yaml-insert-url     "aur-url"
                                 (format "https://aur.archlinux.org/packages/%s" pkg-name))
      (arch--yaml-insert-url     "aur-git"
                                 (format "https://aur.archlinux.org/%s.git" pkg-name))
      (arch--yaml-insert-eww-url "pkgbuild"
                                 (format "https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=%s"
                                         pkg-name)))
    (when files
      (arch--yaml-insert-list "files" files))))

;;;###autoload
(defun arch-show-info (pkg-name)
  "Display detailed info for PKG-NAME in a read-only buffer."
  (interactive "sPackage name: ")
  (let* ((backend (arch--default-backend))
         (plist (arch--cached-info pkg-name))
         (files (funcall (arch-backend-files-fn backend) pkg-name))
         (buf (get-buffer-create (arch--info-buffer-name pkg-name))))
    (unless plist
      (user-error "No info found for package %S" pkg-name))
    (with-current-buffer buf
      (arch-info-mode)
      (setq arch--info-package pkg-name)
      (arch--info-render pkg-name plist files)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defvar arch-info-mode-map
  (make-sparse-keymap)
  "Keymap for `arch-info-mode'.")

(define-key arch-info-mode-map (kbd "q") #'quit-window)
(define-key arch-info-mode-map (kbd "i") #'arch-info-install)
(define-key arch-info-mode-map (kbd "r") #'arch-info-remove)
(define-key arch-info-mode-map (kbd "u") #'arch-info-upgrade)
(define-key arch-info-mode-map (kbd "s") #'arch-search)
(define-key arch-info-mode-map (kbd "l") #'arch-list)
(define-key arch-info-mode-map (kbd "p") #'arch-find-package)
(define-key arch-info-mode-map (kbd "K") #'arch-kill-info-buffers)
(define-key arch-info-mode-map (kbd "?") #'arch-info-menu)

(define-derived-mode arch-info-mode special-mode "arch-info"
  "Read-only view of an Arch Linux package's details.

\\{arch-info-mode-map}")

(transient-define-prefix arch-info-menu ()
  "Actions for the arch package info buffer."
  [["Install"
    ("i"  "Install"     arch-info-install)]
   ["Modify"
    ("r"  "Remove"      arch-info-remove)
    ("u"  "Upgrade"     arch-info-upgrade)]
   ["Navigate"
    ("s"  "Search"               arch-search)
    ("p"  "Find package"         arch-find-package)
    ("l"  "Package list"         arch-list)
    ("ki" "Kill info buffers"    arch-kill-info-buffers)
    ("q"  "Quit"                 quit-window)]])

;;;###autoload
(defun arch-kill-info-buffers ()
  "Kill all *arch-info<...>* package info buffers."
  (interactive)
  (let ((killed 0))
    (seq-do (lambda (buf)
              (when (string-match-p "^\\*arch-info<" (buffer-name buf))
                (kill-buffer buf)
                (setq killed (1+ killed))))
            (buffer-list))
    (message "arch: killed %d info buffer%s" killed (if (= killed 1) "" "s"))))

(defun arch-info-install ()
  "Install the package shown in this buffer, selecting method via `arch--install-dispatch'."
  (interactive)
  (arch--install-dispatch arch--info-package))

(defun arch-info-remove ()
  "Remove the package shown in this buffer."
  (interactive)
  (when (yes-or-no-p (format "Remove %s? " arch--info-package))
    (funcall (arch-backend-remove-fn (arch--default-backend)) arch--info-package)))

(defun arch-info-upgrade ()
  "Upgrade the package shown in this buffer."
  (interactive)
  (when-let* ((fn (arch-backend-upgrade-fn (arch--default-backend))))
    (funcall fn arch--info-package)))

;;; Tabulated list view

(defconst arch--list-buffer-name "*arch-packages*"
  "Name of the arch package list buffer.")

(defvar-local arch--list-backend nil
  "Backend used by the current arch-list buffer.")

(defvar-local arch--marked (make-hash-table :test #'equal)
  "Hash table of marked package names in the current arch-list buffer.")

(defvar-local arch--filter nil
  "Current filter predicate (arch-pkg → bool) for arch-list, or nil for no filter.")

(defvar-local arch--all-entries nil
  "Full unfiltered tabulated-list entries for the current arch-list buffer.")

(defvar-local arch--list-wide nil
  "When non-nil, arch-list shows all sync-DB packages rather than only installed ones.")

(defun arch--enrich-pkg (pkg foreign upgradeable)
  "Set aur-p, upgradeable-p, explicit-p, required-by-p on PKG from context tables and cache."
  (let* ((name (arch-pkg-name pkg))
         (plist (arch--cache-get name))
         (req (when plist (plist-get plist 'required-by))))
    (setf (arch-pkg-aur-p pkg) (and (map-elt foreign name) t))
    (setf (arch-pkg-upgradeable-p pkg) (and (map-elt upgradeable name) t))
    (when plist
      (setf (arch-pkg-explicit-p pkg)
            (equal (plist-get plist 'install-reason) "Explicitly installed"))
      (setf (arch-pkg-required-by-p pkg)
            (and req (not (equal req "None"))))))
  pkg)

(defun arch--pkg-repo-display (pkg)
  "Return the display string for PKG's repository."
  (cond
   ((arch-pkg-aur-p pkg) "aur")
   ((plist-get (arch--cache-get (arch-pkg-name pkg)) 'repository))
   ((equal (arch-pkg-repo pkg) "local") "")
   (t (arch-pkg-repo pkg))))

(defun arch--pkg-status (pkg)
  "Return a 5-char propertized status string for PKG.
avail=not installed  expl=explicit  E+req=explicit+required by others
dep=dependency  D+req=dep+required by others  orphn=orphaned dependency"
  (if (not (arch-pkg-installed-p pkg))
      (propertize "avail" 'face 'arch-face-available
                  'help-echo "available in repository, not installed")
    (let* ((explicit (arch-pkg-explicit-p pkg))
           (req (arch-pkg-required-by-p pkg))
           (orphan (and (not explicit) (not req)))
           (str (cond
                 (orphan "orphn")
                 ((and explicit req) "E+req")
                 (explicit "expl")
                 (req "D+req")
                 (t "dep")))
           (face (if orphan 'arch-face-orphan 'arch-face-installed))
           (desc (string-join
                  (list (if (arch-pkg-aur-p pkg) "AUR" "official repo")
                        (if explicit "explicitly installed" "installed as dependency")
                        (if req "required by other packages" "not required by other packages"))
                  " · ")))
      (propertize str 'face face 'help-echo desc))))

(defun arch--list-name-string (name markedp)
  "Return NAME propertized as a hyperlink, bold when MARKEDP."
  (propertize name 'face (if markedp 'arch-face-pkg-link-marked 'arch-face-pkg-link)))

(defun arch--build-list-entry (pkg marked)
  "Build a tabulated-list entry for arch-pkg PKG.
MARKED is a hash table of marked package names."
  (let ((desc (or (plist-get (arch--cache-get (arch-pkg-name pkg)) 'description)
                  (arch-pkg-description pkg)
                  ""))
        (ver (or (plist-get (arch--cache-get (arch-pkg-name pkg)) 'version)
                 (arch-pkg-version pkg) "")))
    (list pkg
          (vector
           (arch--list-name-string (arch-pkg-name pkg) (map-elt marked (arch-pkg-name pkg)))
           (arch--pkg-repo-display pkg)
           (arch--pkg-status pkg)
           (if (arch-pkg-upgradeable-p pkg)
               (propertize ver 'face 'arch-face-version-old)
             ver)
           desc))))

(defvar arch-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    map)
  "Keymap for `arch-list-mode'.")

(define-key arch-list-mode-map (kbd "i")   #'arch-list-install)
(define-key arch-list-mode-map (kbd "I")   #'arch-list-fetch-info)
(define-key arch-list-mode-map (kbd "a")   #'arch-list-actions)
(define-key arch-list-mode-map (kbd "r")   #'arch-list-remove)
(define-key arch-list-mode-map (kbd "f")   #'arch-list-filter)
(define-key arch-list-mode-map (kbd "F")   #'arch-list-filter-clear)
(define-key arch-list-mode-map (kbd "u")   #'arch-list-upgrade)
(define-key arch-list-mode-map (kbd "U")   #'arch-list-upgrade-all)
(define-key arch-list-mode-map (kbd "SPC") #'arch-list-toggle-mark)
(define-key arch-list-mode-map (kbd "m")   #'arch-list-menu)
(define-key arch-list-mode-map (kbd "M")   #'arch-list-unmark-all)
(define-key arch-list-mode-map (kbd "DEL") #'arch-list-unmark)
(define-key arch-list-mode-map (kbd "s")   #'arch-search)
(define-key arch-list-mode-map (kbd "S")   #'arch-search-yay)
(define-key arch-list-mode-map (kbd "/")   #'arch-list-find)
(define-key arch-list-mode-map (kbd "w")   #'arch-list-toggle-wide)
(define-key arch-list-mode-map (kbd "K")   #'arch-kill-info-buffers)
(define-key arch-list-mode-map (kbd "g")   #'arch-list-refresh)
(define-key arch-list-mode-map (kbd "C")   #'arch-cache-reload)
(define-key arch-list-mode-map (kbd "RET") #'arch-list-show-info)
(define-key arch-list-mode-map (kbd "?")   #'arch-list-menu)

(define-derived-mode arch-list-mode tabulated-list-mode "arch"
  "Major mode for browsing and managing Arch Linux packages.

Columns: Name | Repo | Stat | Version | Description
  Repo: repository (core/extra/aur/…)
  Stat: expl=explicit  E+req=explicit+required  dep=dependency  D+req=dep+required
        orphn=orphaned dep  avail=not installed
  Version: shown in bold red when an upgrade is available

Marks: SPC toggles, m marks, DEL unmarks, M unmarks all.  Marked names appear bold.
Filter: f to set, F to clear.  a for package actions menu.
Wide mode: w toggles all-packages view (installed-only vs full sync DB).

\\{arch-list-mode-map}"
  (setq tabulated-list-format
        (vector
         '("Name"        30 t)
         '("Repo"         7 t)
         '("Stat"         5 t)
         '("Version"     15 t)
         '("Description" 44 nil)))
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

(defun arch-list-toggle-wide ()
  "Toggle between installed-only and all-packages (sync DB) view."
  (interactive)
  (setq arch--list-wide (not arch--list-wide))
  (arch-list-refresh)
  (message "arch: %s" (if arch--list-wide "showing all packages" "showing installed only")))

(defun arch-list-refresh ()
  "Refresh the arch package list buffer."
  (interactive)
  (when (zerop (hash-table-count arch--info-cache))
    (arch--populate-cache))
  (let* ((backend (or arch--list-backend (arch--default-backend)))
         (foreign (if-let* ((fn (arch-backend-foreign-fn backend)))
                      (funcall fn)
                    (arch--foreign-packages)))
         (upgradeable (if-let* ((fn (arch-backend-upgradeable-fn backend)))
                          (funcall fn)
                        (arch--upgradeable-packages)))
         (pkgs (seq-map (lambda (pkg) (arch--enrich-pkg pkg foreign upgradeable))
                        (if arch--list-wide
                            (if-let* ((fn (arch-backend-list-all-fn backend)))
                                (funcall fn)
                              (arch--pacman-list-all))
                          (funcall (arch-backend-list-fn backend))))))
    (setq arch--all-entries
          (seq-map (lambda (pkg) (arch--build-list-entry pkg arch--marked)) pkgs))
    (setq tabulated-list-entries
          (if arch--filter
              (seq-filter (lambda (e) (funcall arch--filter (car e))) arch--all-entries)
            arch--all-entries))
    (tabulated-list-print t)))

;;; Filter predicates

(defun arch--filter-upgradeable-p (pkg)    (arch-pkg-upgradeable-p pkg))
(defun arch--filter-aur-p (pkg)            (arch-pkg-aur-p pkg))
(defun arch--filter-explicit-p (pkg)       (arch-pkg-explicit-p pkg))
(defun arch--filter-dep-p (pkg)            (not (arch-pkg-explicit-p pkg)))
(defun arch--filter-orphan-p (pkg)
  (and (not (arch-pkg-explicit-p pkg)) (not (arch-pkg-required-by-p pkg))))
(defun arch--filter-required-p (pkg)       (arch-pkg-required-by-p pkg))
(defun arch--filter-not-installed-p (pkg)  (not (arch-pkg-installed-p pkg)))

(defconst arch--filter-options
  '(("upgradeable"    arch--filter-upgradeable-p   "has an available upgrade")
    ("aur"            arch--filter-aur-p            "AUR/foreign packages")
    ("explicit"       arch--filter-explicit-p       "explicitly installed")
    ("dep"            arch--filter-dep-p            "installed as a dependency")
    ("orphan"         arch--filter-orphan-p         "dep packages not required by anyone")
    ("required"       arch--filter-required-p       "required by other packages")
    ("not-installed"  arch--filter-not-installed-p  "available but not installed (wide mode)"))
  "Alist of (label predicate-symbol description) for arch-list filters.")

(defun arch-list-filter ()
  "Set a filter on the arch package list via ACR."
  (interactive)
  (let* ((choice (annotated-completing-read
                  (seq-map (lambda (opt) (cons (car opt) (caddr opt)))
                           arch--filter-options)
                  :prompt "Filter packages: "
                  :require-match t
                  :category 'arch-filter)))
    (setq arch--filter (symbol-function (cadr (assoc choice arch--filter-options))))
    (setq tabulated-list-entries
          (seq-filter (lambda (e) (funcall arch--filter (car e))) arch--all-entries))
    (tabulated-list-print t)
    (message "arch: filter: %s" choice)))

(defun arch-list-filter-clear ()
  "Remove the active filter and show all packages."
  (interactive)
  (setq arch--filter nil)
  (setq tabulated-list-entries arch--all-entries)
  (tabulated-list-print t)
  (message "arch: filter cleared"))

;;; Package actions

(defun arch-list-actions ()
  "ACR interface of actions for the package at point."
  (interactive)
  (let* ((name (arch-pkg-name (arch-list--pkg-at-point)))
         (backend (or arch--list-backend (arch--default-backend)))
         (actions
          (list
           (cons "show info"
                 (cons #'arch-list-show-info     "Display package details"))
           (cons "install"
                 (cons (lambda () (arch--install-dispatch name))
                       "Install (prompts for method: direct/abs/rebuild)"))
           (cons "fetch info"
                 (cons #'arch-list-fetch-info
                       "Fetch and cache full info for this entry"))
           (cons "remove"
                 (cons (lambda () (when (yes-or-no-p (format "Remove %s? " name))
                                    (funcall (arch-backend-remove-fn backend) name)))
                       "Remove package"))
           (cons "upgrade"
                 (cons (lambda () (when-let* ((fn (arch-backend-upgrade-fn backend)))
                                    (funcall fn name)))
                       "Upgrade to latest version"))
           (cons "mark"
                 (cons #'arch-list-mark          "Add to marked set"))
           (cons "unmark"
                 (cons #'arch-list-unmark        "Remove from marked set"))))
         (choice (annotated-completing-read
                  (seq-map (lambda (a) (cons (car a) (cddr a))) actions)
                  :prompt (format "[%s]: " name)
                  :require-match t
                  :category 'arch-action)))
    (when-let* ((entry (assoc choice actions)))
      (funcall (cadr entry)))))

(defun arch-list--pkg-at-point ()
  "Return arch-pkg at point or signal a user error."
  (or (tabulated-list-get-id)
      (user-error "No package at point")))

(defun arch-list--pkg-at-point-p ()
  "Return non-nil if there is a package at point."
  (tabulated-list-get-id))

(defun arch-list-toggle-mark ()
  "Toggle mark on the package at point without moving point."
  (interactive)
  (when-let* ((pkg (tabulated-list-get-id)))
    (let* ((name (arch-pkg-name pkg))
           (now-marked (not (map-elt arch--marked name))))
      (if now-marked
          (setf (map-elt arch--marked name) t)
        (map-delete arch--marked name))
      (tabulated-list-set-col 0 (arch--list-name-string name now-marked) t))))

(defun arch-list-mark ()
  "Mark the package at point and advance to the next line."
  (interactive)
  (when-let* ((pkg (tabulated-list-get-id)))
    (setf (map-elt arch--marked (arch-pkg-name pkg)) t)
    (tabulated-list-set-col 0 (arch--list-name-string (arch-pkg-name pkg) t) t)
    (forward-line 1)))

(defun arch-list-unmark ()
  "Unmark the package at point and move to the previous line."
  (interactive)
  (when-let* ((pkg (tabulated-list-get-id)))
    (map-delete arch--marked (arch-pkg-name pkg))
    (tabulated-list-set-col 0 (arch--list-name-string (arch-pkg-name pkg) nil) t)
    (forward-line -1)))

(defun arch-list-unmark-all ()
  "Unmark all packages in the list."
  (interactive)
  (clrhash arch--marked)
  (tabulated-list-print t))

(defun arch--list-marked-names ()
  "Return list of marked package name strings."
  (map-keys arch--marked))

(defun arch-list-install-marked ()
  "Install all marked packages."
  (interactive)
  (let* ((names (arch--list-marked-names))
         (backend (or arch--list-backend (arch--default-backend)))
         (fn (cdr (assq 'direct (arch-backend-install-methods backend)))))
    (when (null names)
      (user-error "No packages marked"))
    (when (yes-or-no-p (format "Install %d marked packages? " (length names)))
      (seq-do (lambda (name) (funcall fn name)) names))))

(defun arch-list-remove-marked ()
  "Remove all marked packages."
  (interactive)
  (let ((names (arch--list-marked-names))
        (fn (arch-backend-remove-fn (or arch--list-backend (arch--default-backend)))))
    (when (null names)
      (user-error "No packages marked"))
    (when (yes-or-no-p (format "Remove %d marked packages? " (length names)))
      (seq-do (lambda (name) (funcall fn name)) names))))

(defun arch-list-find ()
  "Jump to a package in the current list using ACR with table-column annotations."
  (interactive)
  (let* ((pkg-index (map-into
                     (seq-map (lambda (e) (cons (arch-pkg-name (car e)) (car e)))
                              tabulated-list-entries)
                     '(hash-table :test equal)))
         (candidates
          (seq-map (lambda (e)
                     (let* ((pkg (car e))
                            (name (arch-pkg-name pkg)))
                       (cons name
                             (format "[%s]  %s  (%s)  %s"
                                     (arch--pkg-repo-display pkg)
                                     (arch--pkg-status pkg)
                                     (or (arch-pkg-version pkg) "")
                                     (or (plist-get (arch--cache-get name) 'description)
                                         (arch-pkg-description pkg) "")))))
                   tabulated-list-entries))
         (name (annotated-completing-read
                candidates
                :prompt "Jump to package: "
                :require-match t
                :category 'arch-package
                :group-name (lambda (n)
                              (when-let* ((pkg (map-elt pkg-index n)))
                                (arch--pkg-repo-display pkg))))))
    (when name
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (when-let* ((pkg (tabulated-list-get-id)))
                                (arch-pkg-name pkg))
                              name)))
        (forward-line 1)))))

(defun arch-list-install ()
  "Install the package at point, selecting method via `arch--install-dispatch'."
  (interactive)
  (arch--install-dispatch (arch-pkg-name (arch-list--pkg-at-point))))

(defun arch-list-fetch-info ()
  "Fetch and cache full info for the package at point; update its list entry.
Useful for AUR stub entries (version \"<aur>\") added by the widened list."
  (interactive)
  (let* ((pkg (arch-list--pkg-at-point))
         (name (arch-pkg-name pkg)))
    (message "arch: fetching info for %s..." name)
    (arch--cached-info name)
    (let ((new-entry (arch--build-list-entry pkg arch--marked)))
      (setq arch--all-entries
            (seq-map (lambda (e) (if (equal (arch-pkg-name (car e)) name) new-entry e))
                     arch--all-entries))
      (unless arch--filter
        (setq tabulated-list-entries arch--all-entries)))
    (tabulated-list-print t)
    (message "arch: info fetched for %s" name)))

(defun arch-list-remove ()
  "Remove the package at point."
  (interactive)
  (let ((name (arch-pkg-name (arch-list--pkg-at-point))))
    (when (yes-or-no-p (format "Remove %s? " name))
      (funcall (arch-backend-remove-fn (or arch--list-backend (arch--default-backend))) name))))

(defun arch-list-upgrade ()
  "Upgrade the package at point."
  (interactive)
  (when-let* ((fn (arch-backend-upgrade-fn (or arch--list-backend (arch--default-backend)))))
    (funcall fn (arch-pkg-name (arch-list--pkg-at-point)))))

(defun arch-list-upgrade-all ()
  "Upgrade all packages via the current backend."
  (interactive)
  (when-let* ((fn (arch-backend-upgrade-all-fn (or arch--list-backend (arch--default-backend)))))
    (when (yes-or-no-p "Upgrade all packages? ")
      (funcall fn))))

(defun arch-list-show-info ()
  "Show package info for the entry at point."
  (interactive)
  (arch-show-info (arch-pkg-name (arch-list--pkg-at-point))))

;;; Transient menus

(transient-define-prefix arch-list-menu ()
  "Actions for the arch package list buffer."
  [["Package"
    ("a"   "Actions menu"    arch-list-actions
     :inapt-if-not arch-list--pkg-at-point-p)
    ("i"   "Install"         arch-list-install
     :inapt-if-not arch-list--pkg-at-point-p)
    ("r"   "Remove"          arch-list-remove
     :inapt-if-not arch-list--pkg-at-point-p)
    ("u"   "Upgrade"         arch-list-upgrade
     :inapt-if-not arch-list--pkg-at-point-p)
    ("xa"  "Upgrade all"     arch-list-upgrade-all)]
   ["Marked"
    ("SPC" "Toggle mark"     arch-list-toggle-mark)
    ("m"   "Mark"            arch-list-mark)
    ("DEL" "Unmark"          arch-list-unmark)
    ("xu"  "Unmark all"      arch-list-unmark-all)
    ("xi"  "Install marked"  arch-list-install-marked)
    ("xr"  "Remove marked"   arch-list-remove-marked)]
   ["View"
    ("RET" "Package info"    arch-list-show-info
     :inapt-if-not arch-list--pkg-at-point-p)
    ("I"   "Fetch info"      arch-list-fetch-info
     :inapt-if-not arch-list--pkg-at-point-p)
    ("p"   "Find package"    arch-find-package)
    ("/"   "Find in list"    arch-list-find)
    ("w"   "Toggle wide"     arch-list-toggle-wide)
    ("f"   "Filter"          arch-list-filter)
    ("xc"  "Clear filter"    arch-list-filter-clear)
    ("s"   "Search"          arch-search)
    ("g"   "Refresh"         arch-list-refresh)]
   ["System"
    ("y"  "Sync databases"       arch-sync)
    ("xf" "Force sync databases" arch-sync-force)
    ("ki" "Kill info buffers"    arch-kill-info-buffers)
    ("cr" "Reload cache"         arch-cache-reload)]])

;;;###autoload
(transient-define-prefix arch-dispatch ()
  "Entry point for Arch Linux package management."
  [["Search"
    ("s"  "Search"  arch-search)]
   ["View"
    ("l"  "List installed"    arch-list)
    ("fp" "Find package"      arch-find-package)
    ("i"  "Package info"      arch-show-info)
    ("ki" "Kill info buffers" arch-kill-info-buffers)]
   ["Install"
    ("xp" "Install"  arch-install)]
   ["System"
    ("y"  "Sync databases (-Sy)"        arch-sync)
    ("xf" "Force sync (-Syy)"           arch-sync-force)
    ("p"  "Upgrade installed (-Su)"     arch-upgrade-system)
    ("u"  "Full upgrade (pacman -Syu)"  arch-upgrade-all)
    ("xa" "Full upgrade (yay -Syu)"     arch-upgrade-all-yay)]])

;;; Top-level commands

;;;###autoload
(defun arch-list ()
  "Open the arch package list showing installed packages."
  (interactive)
  (let ((buf (get-buffer-create arch--list-buffer-name)))
    (with-current-buffer buf
      (arch-list-mode)
      (setq arch--list-backend (arch--default-backend))
      (arch-list-refresh))
    (pop-to-buffer buf)))

;;;###autoload
(defun arch-install (pkg-name)
  "Install PKG-NAME via the default backend, selecting via ACR when interactive."
  (interactive
   (let* ((backend (arch--default-backend))
          (pkgs (if-let* ((fn (arch-backend-list-all-fn backend)))
                    (funcall fn)
                  (arch--pacman-list-all)))
          (index (map-into (seq-map (lambda (p) (cons (arch-pkg-name p) p)) pkgs)
                           '(hash-table :test equal))))
     (list (annotated-completing-read
            (seq-map (lambda (pkg)
                       (cons (arch-pkg-name pkg)
                             (format "[%s] (%s)" (arch-pkg-repo pkg) (arch-pkg-version pkg))))
                     pkgs)
            :prompt "Install package: "
            :require-match t
            :category 'arch-package
            :group-name (lambda (name)
                          (when-let* ((pkg (map-elt index name)))
                            (arch-pkg-repo pkg)))))))
  (arch--install-dispatch pkg-name))

;;;###autoload
(defun arch-remove (pkg-name)
  "Remove PKG-NAME via the default backend, selecting via ACR when interactive."
  (interactive
   (progn
     (when (zerop (hash-table-count arch--info-cache))
       (arch--populate-cache))
     (list (annotated-completing-read
            (map-apply (lambda (name plist)
                         (cons name
                               (format "[%s] %s"
                                       (or (plist-get plist 'repository) "aur")
                                       (or (plist-get plist 'description) ""))))
                       arch--info-cache)
            :prompt "Remove package: "
            :require-match t
            :category 'arch-package
            :group-name (lambda (name)
                          (or (plist-get (arch--cache-get name) 'repository) "aur"))))))
  (when (yes-or-no-p (format "Remove %s? " pkg-name))
    (funcall (arch-backend-remove-fn (arch--default-backend)) pkg-name)))

;;;###autoload
(defun arch-upgrade (pkg-name)
  "Upgrade PKG-NAME via the default backend, selecting via ACR when interactive."
  (interactive
   (progn
     (when (zerop (hash-table-count arch--info-cache))
       (arch--populate-cache))
     (let* ((upgradeable (map-keys (arch--upgradeable-packages))))
       (when (null upgradeable)
         (user-error "No packages with available upgrades"))
       (list (annotated-completing-read
              (seq-map (lambda (name)
                         (cons name
                               (format "[%s] %s"
                                       (or (plist-get (arch--cache-get name) 'repository) "aur")
                                       (or (plist-get (arch--cache-get name) 'description) ""))))
                       upgradeable)
              :prompt "Upgrade package: "
              :require-match t
              :category 'arch-package
              :group-name (lambda (name)
                            (or (plist-get (arch--cache-get name) 'repository) "aur")))))))
  (when-let* ((fn (arch-backend-upgrade-fn (arch--default-backend))))
    (funcall fn pkg-name)))

;;;###autoload
(defun arch-upgrade-all ()
  "Upgrade all packages via the default (pacman) backend."
  (interactive)
  (when-let* ((fn (arch-backend-upgrade-all-fn (arch--default-backend))))
    (when (yes-or-no-p "Upgrade all packages? ")
      (funcall fn))))

;;;###autoload
(defun arch-upgrade-all-yay ()
  "Upgrade all packages including AUR via `arch-aur-backend'."
  (interactive)
  (let ((backend (arch--aur-backend)))
    (when-let* ((fn (arch-backend-upgrade-all-fn backend)))
      (when (yes-or-no-p "Upgrade all packages (including AUR)? ")
        (funcall fn)))))

;;; Backend registration

(arch-register-backend
 (arch-backend--make
  :name "pacman"
  :label "pacman"
  :search-fn #'arch--pacman-search
  :info-fn #'arch--pacman-info
  :files-fn #'arch--pacman-files
  :list-fn #'arch--pacman-list
  :list-all-fn #'arch--pacman-list-all-with-desc
  :foreign-fn #'arch--foreign-packages
  :upgradeable-fn #'arch--upgradeable-packages
  :populate-cache-fn #'arch--pacman-populate-cache
  :install-methods '((direct . arch--pacman-install))
  :default-install-method 'direct
  :remove-fn #'arch--pacman-remove
  :upgrade-fn #'arch--pacman-upgrade
  :upgrade-all-fn #'arch--pacman-upgrade-all))

(when (executable-find "yay")
  (arch-register-backend
   (arch-backend--make
    :name "yay"
    :label "yay (AUR)"
    :search-fn #'arch--yay-search
    :info-fn #'arch--pacman-info
    :files-fn #'arch--pacman-files
    :list-fn #'arch--pacman-list
    :list-all-fn #'arch--yay-list-all
    :foreign-fn #'arch--foreign-packages
    :upgradeable-fn #'arch--upgradeable-packages
    :populate-cache-fn #'arch--pacman-populate-cache
    :aur-list-fn #'arch--yay-aur-list
    :install-methods '((direct  . arch--yay-install)
                       (abs     . arch-abs-install)
                       (rebuild . arch-abs-rebuild))
    :default-install-method nil
    :remove-fn #'arch--yay-remove
    :upgrade-fn #'arch--yay-install
    :upgrade-all-fn #'arch--yay-upgrade-all)))

;;; HUD registration

(with-eval-after-load 'hud
  (hud-register-command
   :category 'arch
   :command 'arch-find-package
   :description "arch package info"
   :transient-key "li")
  (hud-register-command
   :category 'arch
   :command 'arch-list
   :description "arch package list"
   :transient-key "ll"))

(provide 'arch)
;;; arch.el ends here
