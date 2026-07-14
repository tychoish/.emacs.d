;;; arch-test.el --- Tests for arch.el -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the arch package management module.

;;; Code:

(require 'ert)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name))))
(require 'arch)

;;; Backend registry

(ert-deftest arch-test-register-and-lookup-backend ()
  "Registering a backend makes it retrievable by name."
  (let ((arch--backends (make-hash-table :test #'equal))
        (b (arch-backend--make :name "test" :label "Test")))
    (arch-register-backend b)
    (should (eq (map-elt arch--backends "test") b))))

(ert-deftest arch-test-default-backend-error-when-missing ()
  "Requesting default backend when none registered signals a user-error."
  (let ((arch--backends (make-hash-table :test #'equal))
        (arch-default-backend "none"))
    (should-error (arch--default-backend) :type 'user-error)))

;;; Search output parser

(ert-deftest arch-test-parse-search-output-basic ()
  "Parse a minimal pacman -Ss output into arch-pkg structs."
  (let* ((output "extra/ripgrep 14.1.0-1\n    Recursively search directories for regex\ncore/bash 5.2.021-2\n    The GNU Bourne Again shell\n")
         (pkgs (arch--parse-search-output output)))
    (should (= (length pkgs) 2))
    (let ((rg (car pkgs)))
      (should (equal (arch-pkg-name rg) "ripgrep"))
      (should (equal (arch-pkg-version rg) "14.1.0-1"))
      (should (equal (arch-pkg-repo rg) "extra"))
      (should (equal (arch-pkg-description rg) "Recursively search directories for regex"))
      (should (null (arch-pkg-installed-p rg))))))

(ert-deftest arch-test-parse-search-output-installed-flag ()
  "Parse installed flag from pacman -Ss output."
  (let* ((output "extra/ripgrep 14.1.0-1 [installed]\n    Recursively search directories for regex\n")
         (pkgs (arch--parse-search-output output)))
    (should (= (length pkgs) 1))
    (should (arch-pkg-installed-p (car pkgs)))))

(ert-deftest arch-test-parse-search-output-empty ()
  "Empty output produces an empty list."
  (should (null (arch--parse-search-output ""))))

;;; Info output parser

(ert-deftest arch-test-parse-info-output-basic ()
  "Parse key-value pairs from pacman -Qi output into a plist."
  (let* ((output "Name            : ripgrep\nVersion         : 14.1.0-1\nDescription     : Recursively search directories for regex\nURL             : https://github.com/BurntSushi/ripgrep\n")
         (plist (arch--parse-info-output output)))
    (should (equal (plist-get plist 'name) "ripgrep"))
    (should (equal (plist-get plist 'version) "14.1.0-1"))
    (should (equal (plist-get plist 'description) "Recursively search directories for regex"))
    (should (equal (plist-get plist 'url) "https://github.com/BurntSushi/ripgrep"))))

(ert-deftest arch-test-parse-info-output-hyphenated-keys ()
  "Multi-word keys are normalized to hyphenated lowercase symbols."
  (let* ((output "Install Date    : Mon 01 Jan 2024 12:00:00\nDepends On      : gcc-libs\n")
         (plist (arch--parse-info-output output)))
    (should (equal (plist-get plist 'install-date) "Mon 01 Jan 2024 12:00:00"))
    (should (equal (plist-get plist 'depends-on) "gcc-libs"))))

(ert-deftest arch-test-parse-info-output-continuation-lines ()
  "Continuation lines (leading whitespace) are appended to the previous key."
  (let* ((output "Optional Deps   : perl: for scripts\n                  python: for analysis\n")
         (plist (arch--parse-info-output output)))
    (should (string-match-p "perl" (plist-get plist 'optional-deps)))
    (should (string-match-p "python" (plist-get plist 'optional-deps)))))

;;; Multi-info parser

(ert-deftest arch-test-parse-multi-info ()
  "Parse pacman -Qi output for multiple packages."
  (let* ((output "Name            : bash\nVersion         : 5.2.021-2\n\nName            : ripgrep\nVersion         : 14.1.0-1\n\n")
         (plists (arch--parse-multi-info output)))
    (should (= (length plists) 2))
    (should (equal (plist-get (car plists) 'name) "bash"))
    (should (equal (plist-get (cadr plists) 'name) "ripgrep"))))

;;; Installed package list parser

(ert-deftest arch-test-parse-installed-output ()
  "Parse pacman -Q output into arch-pkg structs."
  (let* ((output "bash 5.2.021-2\nripgrep 14.1.0-1\n")
         (pkgs (arch--parse-installed-output output)))
    (should (= (length pkgs) 2))
    (should (equal (arch-pkg-name (car pkgs)) "bash"))
    (should (equal (arch-pkg-version (car pkgs)) "5.2.021-2"))
    (should (arch-pkg-installed-p (car pkgs)))
    (should (equal (arch-pkg-repo (car pkgs)) "local"))))

;;; Info cache

(ert-deftest arch-test-cache-put-and-get ()
  "Cache stores and retrieves plists by package name."
  (let ((arch--info-cache (make-hash-table :test #'equal)))
    (arch--cache-put "bash" '(version "5.2"))
    (should (equal (arch--cache-get "bash") '(version "5.2")))
    (should (null (arch--cache-get "missing")))))

(ert-deftest arch-test-cache-drop ()
  "arch-cache-drop clears the cache."
  (let ((arch--info-cache (make-hash-table :test #'equal)))
    (arch--cache-put "bash" '(version "5.2"))
    (arch-cache-drop)
    (should (zerop (hash-table-count arch--info-cache)))))

;;; arch--cached-info

(ert-deftest arch-test-cached-info-cache-hit ()
  "Cache hit returns stored plist without calling backend."
  (let ((arch--info-cache (make-hash-table :test #'equal))
        (called nil))
    (arch--cache-put "bash" '(name "bash"))
    (cl-letf (((symbol-function 'arch--default-backend)
               (lambda () (setq called t) nil)))
      (should (equal (arch--cached-info "bash") '(name "bash")))
      (should-not called))))

(ert-deftest arch-test-cached-info-cache-miss-fetches-and-stores ()
  "Cache miss calls backend info-fn, stores result, and returns it."
  (let* ((arch--info-cache (make-hash-table :test #'equal))
         (plist '(name "ripgrep"))
         (backend (arch-backend--make :name "test" :info-fn (lambda (_) plist))))
    (cl-letf (((symbol-function 'arch--default-backend) (lambda () backend)))
      (should (equal (arch--cached-info "ripgrep") plist))
      (should (equal (arch--cache-get "ripgrep") plist)))))

(ert-deftest arch-test-cached-info-cache-miss-backend-nil ()
  "Cache miss with backend returning nil yields nil and stores nothing."
  (let* ((arch--info-cache (make-hash-table :test #'equal))
         (backend (arch-backend--make :name "test" :info-fn (lambda (_) nil))))
    (cl-letf (((symbol-function 'arch--default-backend) (lambda () backend)))
      (should (null (arch--cached-info "missing")))
      (should (null (arch--cache-get "missing"))))))

;;; Status column — 5-char label: avail / expl / E+req / D+req / orphn

(ert-deftest arch-test-pkg-status-available ()
  "Not-installed package returns \"avail\"."
  (let ((pkg (arch-pkg--make :name "foo" :installed-p nil)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "avail"))))

(ert-deftest arch-test-pkg-status-explicit ()
  "Explicit package, not required by others: \"expl\"."
  (let ((pkg (arch-pkg--make :name "bash" :installed-p t :explicit-p t)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "expl"))))

(ert-deftest arch-test-pkg-status-explicit-upgradeable ()
  "Upgradeable flag does not change status label: still \"expl\"."
  (let ((pkg (arch-pkg--make :name "bash" :installed-p t :explicit-p t :upgradeable-p t)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "expl"))))

(ert-deftest arch-test-pkg-status-explicit-required ()
  "Explicit package also required by others: \"E+req\"."
  (let ((pkg (arch-pkg--make :name "gcc" :installed-p t
                              :explicit-p t :required-by-p t)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "E+req"))))

(ert-deftest arch-test-pkg-status-dep-required ()
  "Dependency required by others: \"D+req\"."
  (let ((pkg (arch-pkg--make :name "glibc" :installed-p t
                              :explicit-p nil :required-by-p t)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "D+req"))))

(ert-deftest arch-test-pkg-status-orphan ()
  "Orphan (dep, not required): \"orphn\" with arch-face-orphan."
  (let ((pkg (arch-pkg--make :name "foo" :installed-p t
                              :explicit-p nil :required-by-p nil)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "orphn"))
    (should (eq (get-text-property 0 'face (arch--pkg-status pkg)) 'arch-face-orphan))))

(ert-deftest arch-test-pkg-status-aur-explicit ()
  "AUR explicit package: \"expl\" (repo shown elsewhere, not in status)."
  (let ((pkg (arch-pkg--make :name "myaur" :installed-p t :aur-p t :explicit-p t)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "expl"))))

;;; Sync-list parser

(ert-deftest arch-test-parse-sync-list-basic ()
  "Parse pacman -Sl output into arch-pkg structs."
  (let* ((output "core bash 5.2.021-2\nextra ripgrep 14.1.0-1 [installed]\n")
         (pkgs (arch--parse-sync-list output)))
    (should (= (length pkgs) 2))
    (let ((bash (car pkgs)))
      (should (equal (arch-pkg-name bash) "bash"))
      (should (equal (arch-pkg-repo bash) "core"))
      (should (equal (arch-pkg-version bash) "5.2.021-2"))
      (should (null (arch-pkg-installed-p bash))))
    (let ((rg (cadr pkgs)))
      (should (equal (arch-pkg-name rg) "ripgrep"))
      (should (arch-pkg-installed-p rg)))))

(ert-deftest arch-test-parse-sync-list-empty ()
  "Empty output produces empty list."
  (should (null (arch--parse-sync-list ""))))

;;; Kill info buffers

(ert-deftest arch-test-kill-info-buffers ()
  "arch-kill-info-buffers kills only *arch-info<...>* buffers."
  (let* ((info-buf (get-buffer-create "*arch-info<testpkg>*"))
         (other-buf (get-buffer-create "*arch-other*")))
    (unwind-protect
        (progn
          (arch-kill-info-buffers)
          (should (null (get-buffer "*arch-info<testpkg>*")))
          (should (buffer-live-p other-buf)))
      (when (buffer-live-p other-buf)
        (kill-buffer other-buf)))))

;;; Candidate building

(ert-deftest arch-test-select-from-pkgs-candidates-format ()
  "arch--select-from-pkgs builds an alist with name and annotation."
  (let* ((pkgs (list (arch-pkg--make :name "foo" :version "1.0"
                                     :repo "extra" :description "A foo package"
                                     :installed-p nil)
                     (arch-pkg--make :name "bar" :version "2.0"
                                     :repo "aur" :description "A bar package"
                                     :installed-p t)))
         (captured nil))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (table &rest _) (setq captured table) "foo")))
      (arch--select-from-pkgs pkgs "test: "))
    (should (= (length captured) 2))
    (let ((foo-entry (assoc "foo" captured)))
      (should foo-entry)
      (should (string-match-p "extra" (cdr foo-entry)))
      (should (string-match-p "A foo package" (cdr foo-entry)))
      (should-not (string-match-p "installed" (cdr foo-entry))))
    (let ((bar-entry (assoc "bar" captured)))
      (should bar-entry)
      (should (string-match-p "\\[installed\\]" (cdr bar-entry))))))

;;; Filter predicates

(ert-deftest arch-test-filter-orphan ()
  "Orphan filter matches dep packages not required by others."
  (let ((orphan (arch-pkg--make :name "o" :installed-p t :explicit-p nil :required-by-p nil))
        (dep-req (arch-pkg--make :name "d" :installed-p t :explicit-p nil :required-by-p t))
        (explicit (arch-pkg--make :name "e" :installed-p t :explicit-p t)))
    (should (arch--filter-orphan-p orphan))
    (should-not (arch--filter-orphan-p dep-req))
    (should-not (arch--filter-orphan-p explicit))))

(ert-deftest arch-test-filter-upgradeable ()
  "Upgradeable filter matches packages with upgradeable-p set."
  (let ((up (arch-pkg--make :name "u" :installed-p t :upgradeable-p t))
        (ok (arch-pkg--make :name "o" :installed-p t :upgradeable-p nil)))
    (should (arch--filter-upgradeable-p up))
    (should-not (arch--filter-upgradeable-p ok))))

(ert-deftest arch-test-filter-not-installed ()
  "Not-installed filter matches packages with installed-p nil."
  (let ((inst (arch-pkg--make :name "i" :installed-p t))
        (avail (arch-pkg--make :name "a" :installed-p nil)))
    (should (arch--filter-not-installed-p avail))
    (should-not (arch--filter-not-installed-p inst))))

;;; AUR abs helpers

(ert-deftest arch-test-abs-clone-url ()
  "abs clone URL follows the AUR git pattern."
  (should (equal (arch--abs-clone-url "mypackage")
                 "https://aur.archlinux.org/mypackage.git")))

(ert-deftest arch-test-abs-pkg-dir ()
  "abs package directory is under arch-abs-directory."
  (let ((arch-abs-directory "/tmp/abs/"))
    (should (equal (arch--abs-pkg-dir "mypkg") "/tmp/abs/mypkg"))))

;;; Info buffer (with-help-window migration)

(defmacro arch-test--with-info-buf (pkg plist &rest body)
  "Run BODY with PKG mocked to return PLIST, cleaning up the info buffer after."
  (declare (indent 2))
  `(let ((backend (arch-backend--make :name "test" :files-fn (lambda (_) nil))))
     (cl-letf (((symbol-function 'arch--default-backend) (lambda () backend))
               ((symbol-function 'arch--cached-info) (lambda (_) ,plist)))
       (unwind-protect
           (progn (arch-show-info ,pkg) ,@body)
         (when-let* ((buf (get-buffer (arch--info-buffer-name ,pkg))))
           (kill-buffer buf))))))

(ert-deftest arch-test-show-info-buffer-name ()
  "arch-show-info creates a buffer with the canonical *arch-info<PKG>* name."
  (arch-test--with-info-buf "testpkg" '(name "testpkg" version "1.0")
    (should (get-buffer (arch--info-buffer-name "testpkg")))))

(ert-deftest arch-test-show-info-uses-help-mode ()
  "arch-show-info buffer is in help-mode."
  (arch-test--with-info-buf "testpkg" '(name "testpkg" version "1.0")
    (when-let* ((buf (get-buffer (arch--info-buffer-name "testpkg"))))
      (with-current-buffer buf
        (should (derived-mode-p 'help-mode))))))

(ert-deftest arch-test-show-info-local-map-is-arch-info-map ()
  "arch-show-info sets the local keymap to arch-info-map."
  (arch-test--with-info-buf "testpkg" '(name "testpkg" version "1.0")
    (when-let* ((buf (get-buffer (arch--info-buffer-name "testpkg"))))
      (with-current-buffer buf
        (should (eq (current-local-map) arch-info-map))))))

(ert-deftest arch-test-info-map-inherits-help-mode-map ()
  "arch-info-map has help-mode-map as an ancestor."
  (require 'help-mode)
  (let ((map arch-info-map))
    (while (and map (not (eq map help-mode-map)))
      (setq map (keymap-parent map)))
    (should (eq map help-mode-map))))

;;; AUR detection and abs actions in the info buffer

(ert-deftest arch-test-pkg-aur-source-p-true-for-foreign-package ()
  "arch--pkg-aur-source-p is non-nil for a package in the foreign set."
  (cl-letf (((symbol-function 'arch--foreign-packages)
             (lambda () (map-into '(("aurpkg" . t)) '(hash-table :test equal)))))
    (should (arch--pkg-aur-source-p "aurpkg"))))

(ert-deftest arch-test-pkg-aur-source-p-false-for-repo-package ()
  "arch--pkg-aur-source-p is nil for a package not in the foreign set.
Regression test: the prior check relied on the sync-db `repository' field
being absent, but `pacman -Qi' never includes that field, so it was always
nil and every installed package was misreported as AUR."
  (cl-letf (((symbol-function 'arch--foreign-packages)
             (lambda () (map-into '(("aurpkg" . t)) '(hash-table :test equal)))))
    (should (null (arch--pkg-aur-source-p "bash")))))

(ert-deftest arch-test-show-info-sets-aur-p-for-foreign-package ()
  "arch-show-info marks the info buffer as AUR when the package is foreign."
  (cl-letf (((symbol-function 'arch--foreign-packages)
             (lambda () (map-into '(("aurpkg" . t)) '(hash-table :test equal)))))
    (arch-test--with-info-buf "aurpkg" '(name "aurpkg" version "1.0")
      (with-current-buffer (arch--info-buffer-name "aurpkg")
        (should arch--info-aur-p)))))

(ert-deftest arch-test-show-info-clears-aur-p-for-repo-package ()
  "arch-show-info does not mark the info buffer as AUR for a repo package."
  (cl-letf (((symbol-function 'arch--foreign-packages)
             (lambda () (map-into '(("aurpkg" . t)) '(hash-table :test equal)))))
    (arch-test--with-info-buf "bash" '(name "bash" version "1.0")
      (with-current-buffer (arch--info-buffer-name "bash")
        (should (null arch--info-aur-p))))))

(ert-deftest arch-test-info-abs-install-dispatches-to-abs-install ()
  "arch-info-abs-install calls arch-abs-install with the buffer's package."
  (let (called-with)
    (cl-letf (((symbol-function 'arch--foreign-packages)
               (lambda () (map-into '(("aurpkg" . t)) '(hash-table :test equal))))
              ((symbol-function 'arch-abs-install)
               (lambda (name) (setq called-with name))))
      (arch-test--with-info-buf "aurpkg" '(name "aurpkg" version "1.0")
        (with-current-buffer (arch--info-buffer-name "aurpkg")
          (arch-info-abs-install))))
    (should (equal called-with "aurpkg"))))

(ert-deftest arch-test-info-abs-rebuild-dispatches-to-abs-rebuild ()
  "arch-info-abs-rebuild calls arch-abs-rebuild with the buffer's package."
  (let (called-with)
    (cl-letf (((symbol-function 'arch--foreign-packages)
               (lambda () (map-into '(("aurpkg" . t)) '(hash-table :test equal))))
              ((symbol-function 'arch-abs-rebuild)
               (lambda (name) (setq called-with name))))
      (arch-test--with-info-buf "aurpkg" '(name "aurpkg" version "1.0")
        (with-current-buffer (arch--info-buffer-name "aurpkg")
          (arch-info-abs-rebuild))))
    (should (equal called-with "aurpkg"))))

;;; Progress buffer window placement

(defmacro arch-test--with-buffers (names &rest body)
  "Create a buffer for each name expression in NAMES, run BODY, then kill them."
  (declare (indent 1))
  `(progn
     ,@(seq-map (lambda (n) `(get-buffer-create ,n)) names)
     (unwind-protect
         (progn ,@body)
       ,@(seq-map (lambda (n) `(when (get-buffer ,n) (kill-buffer ,n))) names))))

(ert-deftest arch-test-progress-buffer-p-matches-progress-buffer ()
  "arch--progress-buffer-p recognizes *arch:<pkg>* buffers."
  (arch-test--with-buffers ("*arch:foopkg*")
    (should (arch--progress-buffer-p (get-buffer "*arch:foopkg*") nil))))

(ert-deftest arch-test-progress-buffer-p-excludes-package-list ()
  "arch--progress-buffer-p does not match the *arch-packages* list buffer."
  (arch-test--with-buffers (arch--list-buffer-name)
    (should (null (arch--progress-buffer-p (get-buffer arch--list-buffer-name) nil)))))

(defmacro arch-test--with-frame (frame-var &rest body)
  "Bind FRAME-VAR to an invisible test frame, run BODY, then delete it."
  (declare (indent 1))
  `(let ((,frame-var (make-frame '((visibility . nil)))))
     (unwind-protect
         (with-selected-frame ,frame-var
           (delete-other-windows)
           ,@body)
       (delete-frame ,frame-var))))

(ert-deftest arch-test-takeover-window-never-uses-package-list-window ()
  "arch--takeover-window never displaces the *arch-packages* window."
  (arch-test--with-buffers (arch--list-buffer-name "*arch-test-other*" "*arch:takeover-test*")
    (arch-test--with-frame frame
      (let* ((list-buf (get-buffer arch--list-buffer-name))
             (other-buf (get-buffer "*arch-test-other*"))
             (win1 (selected-window))
             (win2 (split-window win1)))
        (set-window-buffer win1 list-buf)
        (set-window-buffer win2 other-buf)
        (select-window win2)
        (let* ((new-buf (get-buffer "*arch:takeover-test*"))
               (chosen (arch--takeover-window new-buf nil)))
          (should (eq chosen win2))
          (should (equal (buffer-name (window-buffer win1)) arch--list-buffer-name))
          (should (eq (window-buffer win2) new-buf)))))))

(ert-deftest arch-test-takeover-window-prefers-existing-progress-window ()
  "arch--takeover-window reuses a window already showing another progress buffer."
  (arch-test--with-buffers (arch--list-buffer-name "*arch-test-other2*" "*arch:existing*" "*arch:new*")
    (arch-test--with-frame frame
      (let* ((list-buf (get-buffer arch--list-buffer-name))
             (progress-buf (get-buffer "*arch:existing*"))
             (other-buf (get-buffer "*arch-test-other2*"))
             (win1 (selected-window))
             (win2 (split-window win1))
             (win3 (split-window win2)))
        (set-window-buffer win1 list-buf)
        (set-window-buffer win2 other-buf)
        (set-window-buffer win3 progress-buf)
        (select-window win2)
        (let* ((new-buf (get-buffer "*arch:new*"))
               (chosen (arch--takeover-window new-buf nil)))
          (should (eq chosen win3))
          (should (eq (window-buffer win3) new-buf)))))))

(provide 'arch-test)
;;; arch-test.el ends here
