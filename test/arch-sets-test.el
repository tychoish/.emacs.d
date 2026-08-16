;;; arch-sets-test.el --- Tests for arch-sets.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; ERT tests for the arch-sets package-set export/import module.

;;; Code:

(require 'ert)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name))))
(require 'arch-sets)

;;; arch-sets-directory guard

(ert-deftest arch-sets-test-directory-error-when-unset ()
  "arch-sets--directory signals a user-error when arch-sets-directory is nil."
  (let ((arch-sets-directory nil))
    (should-error (arch-sets--directory) :type 'user-error)))

(ert-deftest arch-sets-test-directory-returns-value-when-set ()
  "arch-sets--directory returns the configured directory."
  (let ((arch-sets-directory "/tmp/pkg-sets"))
    (should (equal (arch-sets--directory) "/tmp/pkg-sets"))))

;;; Entry conversion + YAML round trip

(ert-deftest arch-sets-test-entry-to-alist ()
  "An (name . source) pair converts to a name/source alist."
  (let ((alist (arch-sets--entry-to-alist (cons "ripgrep" "pacman"))))
    (should (equal (alist-get 'name alist) "ripgrep"))
    (should (equal (alist-get 'source alist) "pacman"))))

(ert-deftest arch-sets-test-yaml-round-trip ()
  "Encoding then parsing a package-set alist round-trips its data."
  (let* ((data (list (cons 'version 1)
                     (cons 'host "architect")
                     (cons 'packages
                           (list (list (cons 'name "emacs-nativecomp") (cons 'source "pacman"))
                                 (list (cons 'name "brave-bin") (cons 'source "aur"))
                                 (list (cons 'name "some-flatpak-app")
                                       (cons 'source "db")
                                       (cons 'backend "flatpak"))))))
         (parsed (yaml-parse-string (yaml-encode data)
                                    :object-type 'alist
                                    :sequence-type 'list
                                    :string-values t))
         (packages (alist-get 'packages parsed)))
    (should (equal (alist-get 'host parsed) "architect"))
    (should (= (length packages) 3))
    (should (equal (alist-get 'name (nth 0 packages)) "emacs-nativecomp"))
    (should (equal (alist-get 'source (nth 0 packages)) "pacman"))
    (should (equal (alist-get 'source (nth 1 packages)) "aur"))
    (should (equal (alist-get 'backend (nth 2 packages)) "flatpak"))))

;;; Backend resolution

(ert-deftest arch-sets-test-resolve-backend-name-pacman ()
  "A pacman-sourced entry resolves to the \"pacman\" backend name."
  (should (equal (arch-sets--resolve-backend-name '((name . "ripgrep") (source . "pacman")))
                 "pacman")))

(ert-deftest arch-sets-test-resolve-backend-name-aur ()
  "An aur-sourced entry resolves to the configured `arch-aur-backend'."
  (let ((arch-aur-backend "yay"))
    (should (equal (arch-sets--resolve-backend-name '((name . "brave-bin") (source . "aur")))
                   "yay"))))

(ert-deftest arch-sets-test-resolve-backend-name-db ()
  "A db-sourced entry resolves to its own `backend' field."
  (should (equal (arch-sets--resolve-backend-name
                  '((name . "some-flatpak-app") (source . "db") (backend . "flatpak")))
                 "flatpak")))

(ert-deftest arch-sets-test-resolve-backend-name-unknown-source ()
  "An entry with an unrecognized source resolves to nil."
  (should (null (arch-sets--resolve-backend-name '((name . "x") (source . "brew"))))))

(ert-deftest arch-sets-test-resolve-backend-unregistered-warns-and-skips ()
  "A source naming an unregistered backend resolves to nil, not an error."
  (let ((arch--backends (make-hash-table :test #'equal)))
    (should (null (arch-sets--resolve-backend '((name . "x") (source . "db") (backend . "flatpak")))))))

;;; Grouping

(ert-deftest arch-sets-test-group-by-backend ()
  "Entries are grouped by resolved backend name, preserving each group's names."
  (let* ((arch--backends (make-hash-table :test #'equal))
         (arch-aur-backend "yay"))
    (arch-register-backend (arch-backend--make :name "pacman" :label "pacman"))
    (arch-register-backend (arch-backend--make :name "yay" :label "yay"))
    (let* ((entries (list '((name . "ripgrep") (source . "pacman"))
                          '((name . "bash") (source . "pacman"))
                          '((name . "brave-bin") (source . "aur"))))
           (groups (arch-sets--group-by-backend entries)))
      (should (equal (sort (copy-sequence (map-elt groups "pacman")) #'string<)
                     '("bash" "ripgrep")))
      (should (equal (map-elt groups "yay") '("brave-bin"))))))

(ert-deftest arch-sets-test-group-by-backend-skips-unresolved ()
  "Entries whose backend can't be resolved are omitted from every group."
  (let* ((arch--backends (make-hash-table :test #'equal)))
    (arch-register-backend (arch-backend--make :name "pacman" :label "pacman"))
    (let* ((entries (list '((name . "ripgrep") (source . "pacman"))
                          '((name . "mystery") (source . "brew"))))
           (groups (arch-sets--group-by-backend entries)))
      (should (equal (map-elt groups "pacman") '("ripgrep")))
      (should (= (hash-table-count groups) 1)))))

;;; Install-batch-fn wiring on the pacman backend (defined in arch.el)

(ert-deftest arch-sets-test-pacman-backend-has-install-batch-fn ()
  "The registered pacman backend exposes an install-batch-fn."
  (should (eq (arch-backend-install-batch-fn (map-elt arch--backends "pacman"))
             #'arch--pacman-install-batch)))

(ert-deftest arch-sets-test-pacman-install-batch-command ()
  "arch--pacman-install-batch runs one sync invocation naming every package."
  (let (captured-args)
    (cl-letf (((symbol-function 'arch--worker-run)
               (lambda (args) (setq captured-args args))))
      (arch--pacman-install-batch '("ripgrep" "bash")))
    (should (equal captured-args
                   '("sudo" "pacman" "--noconfirm" "--noprogressbar" "--sync" "ripgrep" "bash")))))

;;; Install status against a live-system snapshot

(ert-deftest arch-sets-test-entry-installed-p ()
  "An entry is installed exactly when its name is present in the installed table."
  (let ((installed (map-into '(("ripgrep" . t)) '(hash-table :test equal))))
    (should (arch-sets--entry-installed-p '((name . "ripgrep") (source . "pacman")) installed))
    (should (null (arch-sets--entry-installed-p '((name . "bash") (source . "pacman")) installed)))))

(provide 'arch-sets-test)
;;; arch-sets-test.el ends here
