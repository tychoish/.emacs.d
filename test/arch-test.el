;;; arch-test.el --- Tests for arch.el -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the arch package management module.

;;; Code:

(require 'ert)
(require 'seq)
(require 'map)

(let ((load-path (cons (expand-file-name "lisp" (file-name-directory
                                                  (directory-file-name
                                                   (file-name-directory load-file-name))))
                       load-path)))
  (require 'arch))

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

;;; Status column — now 2-char [E/D][*/space]

(ert-deftest arch-test-pkg-status-installed ()
  "Explicit package, not required by others: \"E \"."
  (let* ((pkg (arch-pkg--make :name "bash" :installed-p t :explicit-p t)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "E "))))

(ert-deftest arch-test-pkg-status-upgradeable ()
  "Upgrade does not change status string (shown via red version instead): \"E \"."
  (let* ((pkg (arch-pkg--make :name "bash" :installed-p t :explicit-p t :upgradeable-p t)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "E "))))

(ert-deftest arch-test-pkg-status-required ()
  "Dep required by others: \"D*\"."
  (let* ((pkg (arch-pkg--make :name "glibc" :installed-p t
                               :explicit-p nil :required-by-p t)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "D*"))))

(ert-deftest arch-test-pkg-status-aur ()
  "AUR package, explicit: \"E \" (repo shown in Repo column, not status)."
  (let* ((pkg (arch-pkg--make :name "myaur" :installed-p t :aur-p t :explicit-p t)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "E "))))

(ert-deftest arch-test-pkg-status-aur-stale ()
  "AUR package with upgrade: status still \"E \"; upgrade shown via red version."
  (let* ((pkg (arch-pkg--make :name "myaur" :installed-p t :aur-p t
                               :explicit-p t :upgradeable-p t)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "E "))))

(ert-deftest arch-test-pkg-status-orphan ()
  "Orphan (dep, not required): \"D \" with orphan face."
  (let* ((pkg (arch-pkg--make :name "foo" :installed-p t
                               :explicit-p nil :required-by-p nil)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "D "))
    (should (eq (get-text-property 0 'face (arch--pkg-status pkg)) 'arch-face-orphan))))

(ert-deftest arch-test-pkg-status-available ()
  "Not-installed packages return two spaces."
  (let* ((pkg (arch-pkg--make :name "foo" :installed-p nil)))
    (should (equal (substring-no-properties (arch--pkg-status pkg)) "  "))))

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

(provide 'arch-test)
;;; arch-test.el ends here
