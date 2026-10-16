;;; setup-package-defaults.el --- Package manager defaults and settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Default configuration for package.el, archives, priorities, and
;; debounced quickstart management.

;;; Code:

(require 'package)
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

(defvar bootstrap--package-quickstart-refresh-timer nil
  "Timer used to debounce calls to `package-quickstart-refresh'.")

(defvar bootstrap-package-quickstart-refresh-delay 1.0
  "Idle time in seconds to wait before regenerating `package-quickstart-file'.")

(defun bootstrap-sync-package-load-paths ()
  "Prune stale package directories from `load-path'.
Also ensures current ELPA directories and autoloads are loaded."
  (let ((elpa-dir (expand-file-name "elpa" user-emacs-directory)))
    (when (file-directory-p elpa-dir)
      (setq load-path (seq-filter #'file-directory-p load-path))
      (dolist (dir (directory-files elpa-dir t "^[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir)
          (let* ((pkg-base (file-name-nondirectory dir))
                 (pkg-name (replace-regexp-in-string "-[0-9].*\\'" "" pkg-base))
                 (autoloads-file (expand-file-name (concat pkg-name "-autoloads") dir)))
            (when (or (file-exists-p (concat autoloads-file ".el"))
                      (file-exists-p (concat autoloads-file ".elc")))
              (load autoloads-file t t))))))))

(defun bootstrap-refresh-package-quickstart ()
  "Regenerate `package-quickstart-file' and clear the debounce timer."
  (setq bootstrap--package-quickstart-refresh-timer nil)
  (bootstrap-sync-package-load-paths)
  (package-quickstart-refresh))

(defun bootstrap-flush-package-quickstart ()
  "Ensure any pending `package-quickstart-refresh' runs immediately."
  (when (timerp bootstrap--package-quickstart-refresh-timer)
    (cancel-timer bootstrap--package-quickstart-refresh-timer)
    (bootstrap-refresh-package-quickstart)))

(defun ad:refresh-package-quickstart (&rest _)
  "Debounce regeneration of `package-quickstart-file' after package operations."
  (if noninteractive
      (package-quickstart-refresh)
    (when (timerp bootstrap--package-quickstart-refresh-timer)
      (cancel-timer bootstrap--package-quickstart-refresh-timer))
    (setq bootstrap--package-quickstart-refresh-timer
          (run-with-idle-timer bootstrap-package-quickstart-refresh-delay
                               nil
                               #'bootstrap-refresh-package-quickstart))))

(seq-do (lambda (fn) (advice-add fn :after #'ad:refresh-package-quickstart))
        '(package-install
          package-reinstall
          package-delete
          package-upgrade
          package-upgrade-all
          package-autoremove
          package-vc-install
          package-vc-upgrade
          package-vc-upgrade-all))

(add-hook 'async-pkg-install-after-hook #'ad:refresh-package-quickstart)
(add-hook 'kill-emacs-hook #'bootstrap-flush-package-quickstart)

(provide 'setup-package-defaults)
;;; setup-package-defaults.el ends here
