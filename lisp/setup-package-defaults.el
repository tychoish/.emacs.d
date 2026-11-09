;;; setup-package-defaults.el --- Package manager defaults and settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Default configuration for package.el, archives, priorities,
;; and package-quickstart bootstrap entry point.

;;; Code:

(require 'seq)

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
  (let* ((elc (concat (file-name-sans-extension package-quickstart-file) ".elc"))
         (target (cond ((file-exists-p elc) elc)
                       ((file-exists-p package-quickstart-file) package-quickstart-file))))
    (if target
        (load target t t t t)
      (require 'package)
      (package-activate-all)
      (require 'elpaish-install)
      (when (elpaish-install-package-quickstart-stale-p)
        (elpaish-install-packages 'core)))))

(provide 'setup-package-defaults)
;;; setup-package-defaults.el ends here
