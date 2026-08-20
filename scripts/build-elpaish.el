;;; build-elpaish.el --- Headless build runner for ELPAish archives -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa, ci

;;; Commentary:
;; Headless entry point for building ELPAish multi-track package archives.
;; Invoked by GitHub Actions CI workflows or local batch runs.

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
  (add-to-list 'load-path (expand-file-name "../lisp" dir))
  (add-to-list 'load-path (expand-file-name "../scripts" dir))
  (add-to-list 'load-path (expand-file-name "lisp" default-directory))
  (add-to-list 'load-path (expand-file-name "scripts" default-directory)))

;; Initialize package infrastructure so dependencies installed by bootstrap-elpaish.el
;; or present in elpa/ are activated.
(require 'package)
(let ((ci-elpa (expand-file-name "elpa-ci" default-directory))
      (local-elpa (expand-file-name "elpa" default-directory)))
  (cond
   ((file-directory-p ci-elpa)
    (setq package-user-dir ci-elpa)
    (package-initialize))
   ((file-directory-p local-elpa)
    (setq package-user-dir local-elpa)
    (package-initialize))
   (t
    (package-initialize))))
(require 'builder-elpa)
(require 'elpaish-recipes)

;; Configure output directory from environment or default to public/
(setq builder-elpa-output-dir
      (or (getenv "ELPAISH_OUTPUT_DIR")
          (expand-file-name "public/" default-directory)))

;; Preflight quality gates can be toggled via ELPAISH_RUN_PREFLIGHT
(when (equal (getenv "ELPAISH_RUN_PREFLIGHT") "0")
  (setq builder-elpa-run-preflight nil))
;; If secret key is provided in environment, import it into temporary GPG keyring
(let ((key-armor (getenv "ELPAISH_SIGNING_KEY"))
      (passphrase (getenv "ELPAISH_GPG_PASSPHRASE")))
  (when (and key-armor (not (string-empty-p key-armor)) (executable-find "gpg"))
    (with-temp-buffer
      (insert key-armor)
      (call-process-region (point-min) (point-max) "gpg" nil nil nil "--batch" "--import"))
    (setq builder-elpa-sign-packages t)
    (when passphrase
      (setq builder-elpa-gpg-passphrase passphrase))))

(message "[elpaish] Building ELPAish multi-track repository into %s..." builder-elpa-output-dir)
(builder-elpa-build-all 'all builder-elpa-output-dir)
(message "[elpaish] Multi-track build complete!")

;;; build-elpaish.el ends here
