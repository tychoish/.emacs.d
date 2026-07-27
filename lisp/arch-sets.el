;;; arch-sets.el --- Package-set export/import for arch.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Export the packages explicitly installed on one machine to a portable
;; YAML file, carry it to another machine, and import it there.  Import
;; cross-references the file against the local system in `arch-set-mode'
;; and only installs missing packages on an explicit batch confirmation —
;; it never installs as a side effect of opening a file.

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)
(require 'yaml)
(require 'tabulated-list)
(require 'transient)
(require 'arch)

;;; File format

(defconst arch-sets-format-version 1
  "Value written to the `version' key of exported package-set files.")

(defcustom arch-sets-directory nil
  "Directory package-set files are read from and written to.
No default is provided since these files are meant to be checked into
whatever repo the user chooses.  Commands that need a directory (rather
than a fully-specified path) signal a `user-error' when this is unset."
  :type '(choice (const :tag "Unset" nil) directory)
  :group 'arch)

(defun arch-sets--directory ()
  "Return `arch-sets-directory' or signal a `user-error' if unset."
  (or arch-sets-directory
      (user-error "arch-sets-directory is not set; customize it before exporting/importing package sets")))

(defun arch-sets--entry-to-alist (entry)
  "Convert an (name . source) ENTRY from `arch-explicit-packages' to an alist."
  (list (cons 'name (car entry))
        (cons 'source (cdr entry))))

(defun arch-sets--parse-file (file)
  "Parse package-set FILE and return its top-level alist."
  (with-temp-buffer
    (insert-file-contents file)
    (yaml-parse-string (buffer-string)
                       :object-type 'alist
                       :sequence-type 'list
                       :string-values t)))

(defun arch-sets--parse-file-packages (file)
  "Return the `packages' entry list parsed from package-set FILE."
  (alist-get 'packages (arch-sets--parse-file file)))

;;; Export

;;;###autoload
(defun arch-sets-export-file (file)
  "Export explicitly-installed packages to FILE as a package-set YAML file."
  (interactive
   (list (read-file-name "Export package set to: " (arch-sets--directory))))
  (let* ((packages (seq-map #'arch-sets--entry-to-alist (arch-explicit-packages)))
         (data (list (cons 'version arch-sets-format-version)
                     (cons 'host (system-name))
                     (cons 'packages packages))))
    (with-temp-file file
      (insert (yaml-encode data) "\n"))
    (message "arch-sets: exported %d packages to %s" (length packages) file)))

;;; Backend resolution + batch install

(defun arch-sets--resolve-backend-name (entry)
  "Return the registered backend name for ENTRY's source, or nil.
`pacman' resolves to the pacman backend, `aur' to `arch-aur-backend',
and `db' to ENTRY's own `backend' field.  A source that doesn't resolve
to a registered backend returns nil so the caller can warn and skip it,
rather than treating an unknown/newer backend as an error."
  (let ((source (alist-get 'source entry)))
    (cond
     ((equal source "pacman") "pacman")
     ((equal source "aur") arch-aur-backend)
     ((equal source "db") (alist-get 'backend entry))
     (t nil))))

(defun arch-sets--resolve-backend (entry)
  "Resolve ENTRY's source to a registered `arch-backend', or nil with a warning."
  (let ((backend-name (arch-sets--resolve-backend-name entry)))
    (or (and backend-name (map-elt arch--backends backend-name))
        (progn
          (message "arch-sets: skipping %s — unregistered backend for source %S"
                   (alist-get 'name entry) (alist-get 'source entry))
          nil))))

(defun arch-sets--group-by-backend (entries)
  "Group ENTRIES by resolved backend name.
Return a hash table of backend name string → list of package names.
Entries whose source doesn't resolve to a registered backend are
skipped with a warning, not an error."
  (let ((groups (make-hash-table :test #'equal)))
    (seq-do
     (lambda (entry)
       (when-let* ((backend (arch-sets--resolve-backend entry))
                   (name (alist-get 'name entry)))
         (setf (map-elt groups (arch-backend-name backend))
               (cons name (map-elt groups (arch-backend-name backend))))))
     entries)
    groups))

(defun arch-sets--install-group (backend-name names)
  "Install NAMES for BACKEND-NAME, batching when the backend supports it."
  (let ((backend (map-elt arch--backends backend-name)))
    (if-let* ((fn (arch-backend-install-batch-fn backend)))
        (funcall fn names)
      (seq-do #'arch--install-dispatch names))))

(defun arch-sets--install-entries (entries)
  "Group ENTRIES by resolved backend and install each group as one batch."
  (map-do #'arch-sets--install-group (arch-sets--group-by-backend entries)))

;;; Import

;;;###autoload
(defun arch-sets-import-file (file)
  "Install every package listed in package-set FILE.
Entries are grouped by resolved backend and each group installed as a
single batch when the backend supports it.  Prefer `arch-sets-open-file'
to review install status before installing anything."
  (interactive (list (read-file-name "Import package set: " (arch-sets--directory))))
  (arch-sets--install-entries (arch-sets--parse-file-packages file)))

;;; Viewer: arch-set-mode

(defun arch-sets--installed-names ()
  "Return a hash table of every currently-installed package name."
  (map-into (seq-map (lambda (p) (cons (arch-pkg-name p) t)) (arch--pacman-list))
            '(hash-table :test equal)))

(defun arch-sets--entry-installed-p (entry installed)
  "Return non-nil if ENTRY's package name is present in the INSTALLED table."
  (and (map-elt installed (alist-get 'name entry)) t))

(defun arch-sets--build-entry-row (entry installed)
  "Build a tabulated-list entry for package-set ENTRY against the INSTALLED table."
  (let ((installed-p (arch-sets--entry-installed-p entry installed)))
    (list entry
          (vector (alist-get 'name entry)
                  (or (alist-get 'source entry) "")
                  (if installed-p
                      (propertize "installed" 'face 'arch-face-installed)
                    (propertize "missing" 'face 'arch-face-available))
                  (or (alist-get 'backend entry) "")))))

(defvar-local arch-sets--file nil
  "Path of the package-set file loaded into the current `arch-set-mode' buffer.")

(defvar-local arch-sets--entries nil
  "Parsed package entries for the current `arch-set-mode' buffer.")

(defun arch-set--entry-at-point ()
  "Return the package-set entry at point, or signal a `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No package at point")))

(defun arch-set-refresh ()
  "Recompute install status for the loaded package-set file against this system."
  (interactive)
  (let ((installed (arch-sets--installed-names)))
    (setq tabulated-list-entries
          (seq-map (lambda (entry) (arch-sets--build-entry-row entry installed))
                   arch-sets--entries))
    (tabulated-list-print t)))

(defun arch-set-show-info ()
  "Show package info for the entry at point."
  (interactive)
  (arch-show-info (alist-get 'name (arch-set--entry-at-point))))

(defun arch-set-install-at-point ()
  "Install the package at point if it is missing on this system."
  (interactive)
  (let* ((entry (arch-set--entry-at-point))
         (name (alist-get 'name entry)))
    (when (arch-sets--entry-installed-p entry (arch-sets--installed-names))
      (user-error "%s is already installed" name))
    (when (yes-or-no-p (format "Install %s? " name))
      (arch-sets--install-entries (list entry))
      (arch-set-refresh))))

(defun arch-set-install-missing ()
  "Install every missing package in the loaded file, batched by backend.
This is the one explicit confirmation step that installs anything —
loading a file into `arch-set-mode' never installs on its own."
  (interactive)
  (let* ((installed (arch-sets--installed-names))
         (missing (or (seq-remove (lambda (e) (arch-sets--entry-installed-p e installed))
                                  arch-sets--entries)
                      (user-error "No missing packages"))))
    (when (yes-or-no-p (format "Install %d missing package%s? "
                               (length missing) (if (= (length missing) 1) "" "s")))
      (arch-sets--install-entries missing)
      (arch-set-refresh))))

(defvar arch-set-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    map)
  "Keymap for `arch-set-mode'.")

(define-key arch-set-mode-map (kbd "i")   #'arch-set-install-at-point)
(define-key arch-set-mode-map (kbd "I")   #'arch-set-install-missing)
(define-key arch-set-mode-map (kbd "g")   #'arch-set-refresh)
(define-key arch-set-mode-map (kbd "RET") #'arch-set-show-info)
(define-key arch-set-mode-map (kbd "l")   #'arch-list)
(define-key arch-set-mode-map (kbd "K")   #'arch-kill-buffers)
(define-key arch-set-mode-map (kbd "q")   #'quit-window)
(define-key arch-set-mode-map (kbd "?")   #'arch-set-menu)

(define-derived-mode arch-set-mode tabulated-list-mode "arch-set"
  "Major mode for viewing a package-set file's install status.

Columns: Name | Source | Status | Notes
  Status: installed / missing, cross-referenced against the live system.
  Notes: the `backend' field for `db'-sourced entries.

\\{arch-set-mode-map}"
  (setq tabulated-list-format
        (vector
         '("Name"    30 t)
         '("Source"   8 t)
         '("Status"   9 t)
         '("Notes"   30 nil)))
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

(transient-define-prefix arch-set-menu ()
  "Actions for the arch package-set viewer buffer."
  [["Package"
    ("i"   "Install at point" arch-set-install-at-point)
    ("RET" "Show info"        arch-set-show-info)]
   ["Batch"
    ("I" "Install all missing" arch-set-install-missing)]
   ["View"
    ("g" "Refresh"       arch-set-refresh)
    ("l" "Package list"  arch-list)
    ("K" "Kill buffers"  arch-kill-buffers)
    ("q" "Quit"          quit-window)]])

;;;###autoload
(defun arch-sets-open-file (file)
  "Load package-set FILE into `arch-set-mode', showing install status.
Never installs anything on its own; use `arch-set-install-at-point' or
`arch-set-install-missing' in the resulting buffer to do that."
  (interactive (list (read-file-name "Open package set: " (arch-sets--directory))))
  (let ((entries (arch-sets--parse-file-packages file))
        (buf (get-buffer-create (format "*arch-set:%s*" (file-name-nondirectory file)))))
    (with-current-buffer buf
      (arch-set-mode)
      (setq arch-sets--file file)
      (setq arch-sets--entries entries)
      (arch-set-refresh))
    (pop-to-buffer buf)))

(provide 'arch-sets)
;;; arch-sets.el ends here
