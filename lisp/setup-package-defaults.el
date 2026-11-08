;;; setup-package-defaults.el --- Package manager defaults and settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Default configuration for package.el, archives, priorities,
;; and package-quickstart bootstrap entry point.

;;; Code:

(require 'seq)

(defvar autoload-compute-prefixes)

(setq autoload-compute-prefixes nil)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpaish" . "https://tychoish.github.io/elpaish/snapshot/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))

(setq package-archive-priorities '(("melpa" . 100)
                                   ("nongnu" . 50)
                                   ("gnu" . 25)
                                   ("jcs-elpa" . 10)))

(setq package-quickstart-file (file-name-concat user-emacs-directory "state/package-quickstart.el"))

(autoload 'elpaish-install-packages "elpaish-install")
(autoload 'elpaish-install-refresh-package-quickstart "elpaish-install")
(autoload 'elpaish-install-package-quickstart-stale-p "elpaish-install")
(autoload 'elpaish-install-optimize-quickstart-file "elpaish-install")
(autoload 'elpaish-install-schedule-quickstart-refresh "elpaish-install")
(autoload 'elpaish-install-flush-package-quickstart "elpaish-install")
(autoload 'elpaish-install-sync-package-load-paths "elpaish-install")
(autoload 'elpaish-install--package-quickstart-refresh-optimize "elpaish-install")
(autoload 'elpaish-install--on-bootstrap-finished "elpaish-install")

(defun bootstrap-initialize-packages ()
  "Initialize packages from quickstart file, or perform cold bootstrap."
  (let ((quickstart-target (let ((elc (concat (file-name-sans-extension package-quickstart-file) ".elc")))
                             (if (file-exists-p elc) elc package-quickstart-file))))
    (if (file-exists-p quickstart-target)
        (load quickstart-target t t t t)
      (require 'package)
      (package-activate-all)
      (unless (package-installed-p 'elpaish)
        (package-install 'elpaish))
      (require 'setup-packages)
      (elpaish-install-packages :sync t)
      (elpaish-install-refresh-package-quickstart t))))

;; Backward-compatibility wrappers for bootstrap and test suites
(defalias 'bootstrap-optimize-quickstart-file #'elpaish-install-optimize-quickstart-file)
(defalias 'ad:package-quickstart-refresh--optimize #'elpaish-install--package-quickstart-refresh-optimize)
(defalias 'bootstrap-sync-package-load-paths #'elpaish-install-sync-package-load-paths)
(defalias 'bootstrap-flush-package-quickstart #'elpaish-install-flush-package-quickstart)
(defalias 'bootstrap-force-refresh-package-quickstart #'elpaish-install-refresh-package-quickstart)

(defun bootstrap-package-quickstart-stale-p ()
  "Return non-nil when quickstart file or its .elc is missing or stale."
  (elpaish-install-package-quickstart-stale-p))

(defun bootstrap-refresh-package-quickstart (&optional force)
  "Regenerate `package-quickstart-file' and clear the debounce timer.
When FORCE is nil, do nothing unless quickstart file is stale."
  (interactive "P")
  (when (or force (bootstrap-package-quickstart-stale-p))
    (require 'package)
    (bootstrap-sync-package-load-paths)
    (package-quickstart-refresh)))

(defun ad:refresh-package-quickstart (&rest _)
  "Debounce regeneration of `package-quickstart-file' after package operations."
  (elpaish-install-schedule-quickstart-refresh))

(defun bootstrap-refresh-quickstart-after-elpaish (results _action)
  "Schedule quickstart refresh when RESULTS include changes."
  (when (seq-some (lambda (result)
                    (memq (plist-get result :status) '(:installed :upgraded)))
                  results)
    (ad:refresh-package-quickstart)))

(provide 'setup-package-defaults)
;;; setup-package-defaults.el ends here
