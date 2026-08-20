;;; bootstrap-elpaish.el --- Bootstrap dependencies for ELPAish CI -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa, ci

;;; Commentary:
;; Initializes package archives and installs prerequisite packages (magit,
;; package-lint) required for running ELPAish builds in headless CI environments.

;;; Code:

(require 'package)

(setq package-user-dir (expand-file-name "elpa-ci/" default-directory))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'package-lint)
  (package-refresh-contents)
  (condition-case nil
      (package-install 'package-lint)
    (error (message "Warning: package-lint installation skipped or failed."))))

(unless (package-installed-p 'magit)
  (unless package-archive-contents (package-refresh-contents))
  (condition-case nil
      (package-install 'magit)
    (error (message "Warning: magit installation skipped or failed."))))

(provide 'bootstrap-elpaish)
;;; bootstrap-elpaish.el ends here
