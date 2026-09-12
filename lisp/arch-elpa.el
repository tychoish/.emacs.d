;;; arch-elpa.el --- Package management UI for Emacs Lisp packages -*- lexical-binding: t; -*-

;;; Commentary:
;; A tabulated-list package manager for ELPA/MELPA packages, mirroring the
;; browsing/search/install/upgrade UX of `arch.el' (persistent package list,
;; ACR-driven search and selection, marks for batch operations) but built
;; directly on `package.el'.  Unlike `arch.el', installs and removals run
;; synchronously in this Emacs process, so there is no compile/progress
;; buffer machinery here; package detail views delegate to the built-in
;; `describe-package' rather than reimplementing one.

;;; Code:

(require 'package)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'annotated-completing-read)
(require 'arch)

;;; Package info helpers

(defun arch-elpa--ensure-initialized ()
  "Ensure `package-alist' and `package-archive-contents' are populated.
This config sets `package-enable-at-startup' to nil and boots via
`package-quickstart' instead, so packages are on `load-path' and active
from startup but `package.el's own bookkeeping tables are never populated
until something asks for them — mirrors what `package-install' does
internally via `package--archives-initialize'."
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (condition-case err
        (package-refresh-contents)
      (error (message "arch-elpa: could not refresh archive contents: %s"
                      (error-message-string err))))))

(defun arch-elpa--installed-desc (name)
  "Return the active installed `package-desc' for NAME, or nil."
  (car (map-elt package-alist name)))

(defun arch-elpa--available-desc (name)
  "Return the best available `package-desc' for NAME from archive contents, or nil."
  (car (map-elt package-archive-contents name)))

(defun arch-elpa--upgradeable-p (name)
  "Return non-nil if NAME has an installed version older than the available one."
  (when-let* ((installed (arch-elpa--installed-desc name))
              (available (arch-elpa--available-desc name)))
    (version-list-< (package-desc-version installed) (package-desc-version available))))

(defun arch-elpa--all-names ()
  "Return a sorted list of all known package name symbols.
Union of installed packages and packages available from configured archives."
  (arch-elpa--ensure-initialized)
  (thread-last (append (map-keys package-alist) (map-keys package-archive-contents))
    (delete-dups)
    (seq-sort #'string-lessp)))

(cl-defstruct (arch-elpa-pkg (:constructor arch-elpa-pkg--make) (:copier nil))
  "Represents an Emacs Lisp package known to `package.el'."
  name          ; symbol
  version       ; string, "-" when unknown
  archive       ; string, e.g. "melpa", "gnu", "builtin", "unknown"
  summary       ; string
  installed-p   ; boolean
  upgradeable-p ; boolean
  built-in-p)   ; boolean — no installed or available desc, but Emacs ships it

(defun arch-elpa--pkg-for-name (name)
  "Build an `arch-elpa-pkg' struct for package NAME symbol."
  (let* ((installed (arch-elpa--installed-desc name))
         (available (arch-elpa--available-desc name))
         (desc (or installed available))
         (archive (cond
                   ((and desc (package-desc-archive desc)))
                   ((package-built-in-p name) "builtin")
                   (t "unknown"))))
    (arch-elpa-pkg--make
     :name name
     :version (if desc (package-version-join (package-desc-version desc)) "-")
     :archive archive
     :summary (or (and desc (package-desc-summary desc)) "")
     :installed-p (and installed t)
     :upgradeable-p (arch-elpa--upgradeable-p name)
     :built-in-p (and (not installed) (not available) (package-built-in-p name) t))))

;;; Tabulated list view

(defconst arch-elpa--list-buffer-name "*arch-elpa-packages*"
  "Name of the arch-elpa package list buffer.")

(defvar-local arch-elpa--marked (make-hash-table :test #'equal)
  "Hash table of marked package name symbols in the current arch-elpa list buffer.")

(defvar-local arch-elpa--filter nil
  "Current filter predicate (arch-elpa-pkg → bool), or nil for no filter.")

(defvar-local arch-elpa--all-entries nil
  "Full unfiltered tabulated-list entries for the current arch-elpa list buffer.")

(defvar-local arch-elpa--list-wide t
  "When non-nil, show all known packages; when nil, installed packages only.")

(defun arch-elpa--pkg-status (pkg)
  "Return a propertized status string for PKG."
  (cond
   ((arch-elpa-pkg-built-in-p pkg) (propertize "built-in" 'face 'arch-face-available))
   ((arch-elpa-pkg-installed-p pkg) (propertize "installed" 'face 'arch-face-installed))
   (t (propertize "avail" 'face 'arch-face-available))))

(defun arch-elpa--build-entry (pkg)
  "Build a tabulated-list entry for `arch-elpa-pkg' PKG."
  (list pkg
        (vector
         (propertize (symbol-name (arch-elpa-pkg-name pkg))
                     'face (if (map-elt arch-elpa--marked (arch-elpa-pkg-name pkg))
                               'arch-face-pkg-link-marked
                             'arch-face-pkg-link))
         (arch-elpa-pkg-archive pkg)
         (arch-elpa--pkg-status pkg)
         (if (arch-elpa-pkg-upgradeable-p pkg)
             (propertize (arch-elpa-pkg-version pkg) 'face 'arch-face-version-old)
           (arch-elpa-pkg-version pkg))
         (arch-elpa-pkg-summary pkg))))

(defvar arch-elpa-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    map)
  "Keymap for `arch-elpa-list-mode'.")

(define-key arch-elpa-list-mode-map (kbd "i")   #'arch-elpa-list-install)
(define-key arch-elpa-list-mode-map (kbd "a")   #'arch-elpa-list-actions)
(define-key arch-elpa-list-mode-map (kbd "r")   #'arch-elpa-list-remove)
(define-key arch-elpa-list-mode-map (kbd "u")   #'arch-elpa-list-upgrade)
(define-key arch-elpa-list-mode-map (kbd "U")   #'arch-elpa-list-upgrade-all)
(define-key arch-elpa-list-mode-map (kbd "f")   #'arch-elpa-list-filter)
(define-key arch-elpa-list-mode-map (kbd "F")   #'arch-elpa-list-filter-clear)
(define-key arch-elpa-list-mode-map (kbd "w")   #'arch-elpa-list-toggle-wide)
(define-key arch-elpa-list-mode-map (kbd "s")   #'arch-elpa-search)
(define-key arch-elpa-list-mode-map (kbd "/")   #'arch-elpa-list-find)
(define-key arch-elpa-list-mode-map (kbd "SPC") #'arch-elpa-list-toggle-mark)
(define-key arch-elpa-list-mode-map (kbd "m")   #'arch-elpa-list-mark)
(define-key arch-elpa-list-mode-map (kbd "M")   #'arch-elpa-list-unmark-all)
(define-key arch-elpa-list-mode-map (kbd "DEL") #'arch-elpa-list-unmark)
(define-key arch-elpa-list-mode-map (kbd "g")   #'arch-elpa-list-refresh)
(define-key arch-elpa-list-mode-map (kbd "C")   #'arch-elpa-list-refresh-contents)
(define-key arch-elpa-list-mode-map (kbd "RET") #'arch-elpa-list-show-info)
(define-key arch-elpa-list-mode-map (kbd "?")   #'arch-elpa-list-menu)

(define-derived-mode arch-elpa-list-mode tabulated-list-mode "arch-elpa"
  "Major mode for browsing and managing Emacs Lisp packages via `package.el'.

Columns: Name | Archive | Status | Version | Summary
  Status: installed / avail / built-in
  Version: shown in bold red when an upgrade is available

Marks: SPC toggles, m marks, DEL unmarks, M unmarks all.
Marked names appear bold.
Filter: f to set, F to clear.  a for package actions menu.
Wide mode: w toggles all-packages view (installed-only vs installed+available).

\\{arch-elpa-list-mode-map}"
  (setq tabulated-list-format
        (vector
         '("Name"     30 t)
         '("Archive"  10 t)
         '("Status"    9 t)
         '("Version"  14 t)
         '("Summary"  44 nil)))
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

(defun arch-elpa-list-toggle-wide ()
  "Toggle between installed-only and installed+available package view."
  (interactive)
  (setq arch-elpa--list-wide (not arch-elpa--list-wide))
  (arch-elpa-list-refresh)
  (message "arch-elpa: %s" (if arch-elpa--list-wide "showing all packages" "showing installed only")))

(defun arch-elpa-list-refresh ()
  "Refresh the arch-elpa package list buffer from current package.el state."
  (interactive)
  (arch-elpa--ensure-initialized)
  (let* ((names (if arch-elpa--list-wide (arch-elpa--all-names) (map-keys package-alist)))
         (pkgs (seq-map #'arch-elpa--pkg-for-name names)))
    (setq arch-elpa--all-entries (seq-map #'arch-elpa--build-entry pkgs))
    (setq tabulated-list-entries
          (if arch-elpa--filter
              (seq-filter (lambda (e) (funcall arch-elpa--filter (car e))) arch-elpa--all-entries)
            arch-elpa--all-entries))
    (tabulated-list-print t)))

(defun arch-elpa-list-refresh-contents ()
  "Refresh archive contents from configured archives, then rebuild the list."
  (interactive)
  (message "arch-elpa: refreshing archive contents...")
  (package-refresh-contents)
  (arch-elpa-list-refresh)
  (message "arch-elpa: archive contents refreshed"))

;;; Filter predicates

(defun arch-elpa--filter-upgradeable-p (pkg) (arch-elpa-pkg-upgradeable-p pkg))
(defun arch-elpa--filter-installed-p (pkg)   (arch-elpa-pkg-installed-p pkg))
(defun arch-elpa--filter-available-p (pkg)   (not (arch-elpa-pkg-installed-p pkg)))
(defun arch-elpa--filter-built-in-p (pkg)    (arch-elpa-pkg-built-in-p pkg))

(defconst arch-elpa--filter-options
  '(("upgradeable" arch-elpa--filter-upgradeable-p "has an available upgrade")
    ("installed"   arch-elpa--filter-installed-p   "currently installed")
    ("available"   arch-elpa--filter-available-p   "available but not installed")
    ("built-in"    arch-elpa--filter-built-in-p     "ships with Emacs, no package.el entry"))
  "Alist of (label predicate-symbol description) for arch-elpa-list filters.")

(defun arch-elpa-list-filter ()
  "Set a filter on the arch-elpa package list via ACR."
  (interactive)
  (let ((choice (annotated-completing-read
                 (seq-map (lambda (opt) (cons (car opt) (caddr opt))) arch-elpa--filter-options)
                 :prompt "Filter packages: "
                 :require-match t
                 :category 'arch-elpa-filter)))
    (setq arch-elpa--filter (symbol-function (cadr (assoc choice arch-elpa--filter-options))))
    (setq tabulated-list-entries
          (seq-filter (lambda (e) (funcall arch-elpa--filter (car e))) arch-elpa--all-entries))
    (tabulated-list-print t)
    (message "arch-elpa: filter: %s" choice)))

(defun arch-elpa-list-filter-clear ()
  "Remove the active filter and show all packages."
  (interactive)
  (setq arch-elpa--filter nil)
  (setq tabulated-list-entries arch-elpa--all-entries)
  (tabulated-list-print t)
  (message "arch-elpa: filter cleared"))

;;; Package actions

(defun arch-elpa--pkg-at-point ()
  "Return arch-elpa-pkg at point or signal a user error."
  (or (tabulated-list-get-id)
      (user-error "No package at point")))

(defun arch-elpa-list--pkg-at-point-p ()
  "Return non-nil if there is a package at point."
  (tabulated-list-get-id))

(defun arch-elpa--select-package (prompt &optional pkgs)
  "ACR-select an elpa package with PROMPT; return the selected name string or nil.
PKGS restricts the candidate set; defaults to all known packages."
  (let* ((pkgs (or pkgs (seq-map #'arch-elpa--pkg-for-name (arch-elpa--all-names))))
         (index (map-into (seq-map (lambda (p) (cons (symbol-name (arch-elpa-pkg-name p)) p)) pkgs)
                          '(hash-table :test equal))))
    (annotated-completing-read
     (seq-map (lambda (pkg)
                (cons (symbol-name (arch-elpa-pkg-name pkg))
                      (format "[%s] %s %s"
                              (arch-elpa-pkg-archive pkg)
                              (arch-elpa-pkg-version pkg)
                              (arch-elpa-pkg-summary pkg))))
              pkgs)
     :prompt prompt
     :require-match t
     :category 'arch-elpa-package
     :group-name (lambda (name) (arch-elpa-pkg-archive (map-elt index name))))))

;;;###autoload
(defun arch-elpa-search ()
  "ACR-search all known elisp packages and show info for the selection."
  (interactive)
  (when-let* ((name (arch-elpa--select-package "Search elisp packages: ")))
    (describe-package (intern name))))

;;;###autoload
(defun arch-elpa-install ()
  "ACR-select and install an elisp package via `package-install'."
  (interactive)
  (let* ((pkgs (seq-remove #'arch-elpa-pkg-installed-p
                          (seq-map #'arch-elpa--pkg-for-name (arch-elpa--all-names))))
         (name (arch-elpa--select-package "Install elisp package: " pkgs)))
    (when name
      (package-install (intern name))
      (message "arch-elpa: installed %s" name))))

(defun arch-elpa-list-install ()
  "Install the package at point via `package-install'."
  (interactive)
  (let* ((pkg (arch-elpa--pkg-at-point))
         (name (arch-elpa-pkg-name pkg)))
    (when (arch-elpa-pkg-installed-p pkg)
      (user-error "%s is already installed; use `arch-elpa-list-upgrade' instead" name))
    (package-install name)
    (arch-elpa-list-refresh)
    (message "arch-elpa: installed %s" name)))

(defun arch-elpa-list-remove ()
  "Remove the installed package at point via `package-delete'."
  (interactive)
  (let* ((pkg (arch-elpa--pkg-at-point))
         (name (arch-elpa-pkg-name pkg))
         (desc (or (arch-elpa--installed-desc name)
                  (user-error "%s is not installed" name))))
    (when (yes-or-no-p (format "Remove %s? " name))
      (package-delete desc)
      (arch-elpa-list-refresh)
      (message "arch-elpa: removed %s" name))))

(defun arch-elpa-list-upgrade ()
  "Upgrade the package at point via `package-upgrade'."
  (interactive)
  (let* ((pkg (arch-elpa--pkg-at-point))
         (name (arch-elpa-pkg-name pkg)))
    (unless (arch-elpa-pkg-upgradeable-p pkg)
      (user-error "%s has no available upgrade" name))
    (package-upgrade name)
    (arch-elpa-list-refresh)
    (message "arch-elpa: upgraded %s" name)))

;;;###autoload
(defun arch-elpa-list-upgrade-all ()
  "Upgrade all elisp packages with an available upgrade via `package-upgrade-all'."
  (interactive)
  (when (yes-or-no-p "Upgrade all elisp packages? ")
    (package-upgrade-all)
    (when (derived-mode-p 'arch-elpa-list-mode)
      (arch-elpa-list-refresh))
    (message "arch-elpa: upgrade complete")))

(defun arch-elpa-list-show-info ()
  "Show package info for the entry at point via `describe-package'."
  (interactive)
  (describe-package (arch-elpa-pkg-name (arch-elpa--pkg-at-point))))

(defun arch-elpa-list-find ()
  "Jump to a package in the current list using ACR."
  (interactive)
  (let* ((pkg-index (map-into
                     (seq-map (lambda (e) (cons (symbol-name (arch-elpa-pkg-name (car e))) (car e)))
                              tabulated-list-entries)
                     '(hash-table :test equal)))
         (candidates
          (seq-map (lambda (e)
                     (let* ((pkg (car e))
                            (name (symbol-name (arch-elpa-pkg-name pkg))))
                       (cons name
                             (format "[%s] %s %s"
                                     (arch-elpa-pkg-archive pkg)
                                     (arch-elpa-pkg-version pkg)
                                     (arch-elpa-pkg-summary pkg)))))
                   tabulated-list-entries))
         (name (annotated-completing-read
                candidates
                :prompt "Jump to package: "
                :require-match t
                :category 'arch-elpa-package
                :group-name (lambda (n) (arch-elpa-pkg-archive (map-elt pkg-index n))))))
    (when name
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (when-let* ((pkg (tabulated-list-get-id)))
                                (symbol-name (arch-elpa-pkg-name pkg)))
                              name)))
        (forward-line 1)))))

;;; Marks & batch operations

(defun arch-elpa-list-toggle-mark ()
  "Toggle mark on the package at point without moving point."
  (interactive)
  (when-let* ((pkg (tabulated-list-get-id))
              (name (arch-elpa-pkg-name pkg)))
    (if (map-elt arch-elpa--marked name)
        (map-delete arch-elpa--marked name)
      (setf (map-elt arch-elpa--marked name) t))
    (tabulated-list-set-col 0
      (propertize (symbol-name name)
                  'face (if (map-elt arch-elpa--marked name)
                            'arch-face-pkg-link-marked
                          'arch-face-pkg-link))
      t)))

(defun arch-elpa-list-mark ()
  "Mark the package at point and advance to the next line."
  (interactive)
  (when-let* ((pkg (tabulated-list-get-id)))
    (setf (map-elt arch-elpa--marked (arch-elpa-pkg-name pkg)) t)
    (tabulated-list-set-col 0
      (propertize (symbol-name (arch-elpa-pkg-name pkg)) 'face 'arch-face-pkg-link-marked)
      t)
    (forward-line 1)))

(defun arch-elpa-list-unmark ()
  "Unmark the package at point and move to the previous line."
  (interactive)
  (when-let* ((pkg (tabulated-list-get-id)))
    (map-delete arch-elpa--marked (arch-elpa-pkg-name pkg))
    (tabulated-list-set-col 0
      (propertize (symbol-name (arch-elpa-pkg-name pkg)) 'face 'arch-face-pkg-link)
      t)
    (forward-line -1)))

(defun arch-elpa-list-unmark-all ()
  "Unmark all packages in the list."
  (interactive)
  (clrhash arch-elpa--marked)
  (tabulated-list-print t))

(defun arch-elpa-list-install-marked ()
  "Install all marked packages via `package-install'."
  (interactive)
  (let ((names (or (map-keys arch-elpa--marked) (user-error "No packages marked"))))
    (when (yes-or-no-p (format "Install %d marked packages? " (length names)))
      (seq-do #'package-install names)
      (clrhash arch-elpa--marked)
      (arch-elpa-list-refresh))))

(defun arch-elpa-list-remove-marked ()
  "Remove all marked, installed packages via `package-delete'."
  (interactive)
  (let ((names (or (map-keys arch-elpa--marked) (user-error "No packages marked"))))
    (when (yes-or-no-p (format "Remove %d marked packages? " (length names)))
      (seq-do (lambda (name)
                (when-let* ((desc (arch-elpa--installed-desc name)))
                  (package-delete desc)))
              names)
      (clrhash arch-elpa--marked)
      (arch-elpa-list-refresh))))

(defun arch-elpa-list-actions ()
  "ACR interface of actions for the package at point."
  (interactive)
  (let* ((pkg (arch-elpa--pkg-at-point))
         (name (arch-elpa-pkg-name pkg))
         (actions
          (list
           (cons "show info"    (cons #'arch-elpa-list-show-info "Display package details"))
           (cons "install"      (cons #'arch-elpa-list-install   "Install this package"))
           (cons "remove"       (cons #'arch-elpa-list-remove    "Remove this package"))
           (cons "upgrade"      (cons #'arch-elpa-list-upgrade   "Upgrade to latest version"))
           (cons "mark"         (cons #'arch-elpa-list-mark      "Add to marked set"))
           (cons "unmark"       (cons #'arch-elpa-list-unmark    "Remove from marked set"))))
         (choice (annotated-completing-read
                  (seq-map (lambda (a) (cons (car a) (cddr a))) actions)
                  :prompt (format "[%s]: " name)
                  :require-match t
                  :category 'arch-elpa-action)))
    (when-let* ((entry (assoc choice actions)))
      (funcall (cadr entry)))))

;;; Transient menus

(transient-define-prefix arch-elpa-list-menu ()
  "Actions for the arch-elpa package list buffer."
  [["Package"
    ("a"   "Actions menu"    arch-elpa-list-actions
     :inapt-if-not arch-elpa-list--pkg-at-point-p)
    ("i"   "Install"         arch-elpa-list-install
     :inapt-if-not arch-elpa-list--pkg-at-point-p)
    ("r"   "Remove"          arch-elpa-list-remove
     :inapt-if-not arch-elpa-list--pkg-at-point-p)
    ("u"   "Upgrade"         arch-elpa-list-upgrade
     :inapt-if-not arch-elpa-list--pkg-at-point-p)
    ("U"   "Upgrade all"     arch-elpa-list-upgrade-all)]
   ["Marked"
    ("SPC" "Toggle mark"     arch-elpa-list-toggle-mark)
    ("m"   "Mark"            arch-elpa-list-mark)
    ("DEL" "Unmark"          arch-elpa-list-unmark)
    ("M"   "Unmark all"      arch-elpa-list-unmark-all)
    ("xi"  "Install marked"  arch-elpa-list-install-marked)
    ("xr"  "Remove marked"   arch-elpa-list-remove-marked)]
   ["View"
    ("RET" "Package info"    arch-elpa-list-show-info
     :inapt-if-not arch-elpa-list--pkg-at-point-p)
    ("/"   "Find in list"    arch-elpa-list-find)
    ("w"   "Toggle wide"     arch-elpa-list-toggle-wide)
    ("f"   "Filter"          arch-elpa-list-filter)
    ("xc"  "Clear filter"    arch-elpa-list-filter-clear)
    ("s"   "Search"          arch-elpa-search)
    ("g"   "Refresh"         arch-elpa-list-refresh)
    ("C"   "Refresh archives" arch-elpa-list-refresh-contents)]])

;;; Top-level commands

;;;###autoload
(defun arch-elpa-list ()
  "Open the arch-elpa package list."
  (interactive)
  (let ((buf (get-buffer-create arch-elpa--list-buffer-name)))
    (with-current-buffer buf
      (arch-elpa-list-mode)
      (arch-elpa-list-refresh))
    (pop-to-buffer buf)))

(provide 'arch-elpa)
;;; arch-elpa.el ends here
