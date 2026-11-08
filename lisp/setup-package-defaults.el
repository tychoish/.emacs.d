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

(declare-function elpaish-install-packages "elpaish-install")
(declare-function elpaish-install-refresh-package-quickstart "elpaish-install")
(declare-function elpaish-install-package-quickstart-stale-p "elpaish-install")

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

(defun bootstrap-package-quickstart-stale-p ()
  "Return non-nil when quickstart file or its .elc is missing or stale."
  (elpaish-install-package-quickstart-stale-p))

(defun bootstrap-refresh-package-quickstart (&optional force)
  "Regenerate `package-quickstart-file' and clear the debounce timer.
When FORCE is nil, do nothing unless quickstart file is stale."
  (interactive "P")
  (elpaish-install-refresh-package-quickstart force))

(provide 'setup-package-defaults)
;;; setup-package-defaults.el ends here
