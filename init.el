;; init.el -- tycho's emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple, but complete configuration, with a focus on
;; usability and fast start up times.

;;; Code:

(with-gc-suppressed
 (with-file-name-handler-disabled
  (eval-when-compile
    (require 'subr-x))

  (defvar tychoish/startup-complete-time nil
    "Timestamp when startup finished.")
  (defvar tychoish/eglot-default-server-configuration nil
    "Eglot server configuration, populated later.")

  (defvar sprite-instance-id nil
    "Name of this Emacs instance, e.g. `work', `personal', `hud'.")

  (defvar local-notes-directory nil
    "Directory where notes (org, roam, deft, etc.) are stored.")

  (defvar user-org-directories nil
    "Additional directories that may contain org files.")

  (defvar tychoish-disable-external-notifications nil
    "Disable external notifications when non-nil.")

  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)
  (setq inhibit-startup-message t)
  (setq user-emacs-directory (expand-file-name user-emacs-directory))
  (setq server-use-tcp t)
  ;; (setq server-host "127.0.0.1")
  ;; (setq server-port 2286)

  (defun cli/time-reporting ()
    ;; `early-init.el' already set `slow-op-reporting' and
    ;; `use-package-compute-statistics'; this just acknowledges the flag.
    (when (string-prefix-p "--with-slow-op-timing" argi)
      (message "[op]: enabling time reporting")))

  (defvar cli/org-exec-file nil
    "Org file to execute via `builder-org-babel-execute-file'; set by --org-exec.")

  (defun cli/org-exec ()
    "Handle --org-exec FILE command-line flag."
    (when (string-prefix-p "--org-exec" argi)
      (setq cli/org-exec-file (pop argv))
      t))

  (defun cli/org-exec-run ()
    "Execute `cli/org-exec-file', then exit Emacs.
Runs from `after-init-hook', after the full config has loaded."
    (when cli/org-exec-file
      (builder-org-babel-execute-file cli/org-exec-file)
      (kill-emacs 0)))

  (defvar cli/org-exec-dir nil
    "Directory to execute all org babel blocks in; set by --org-exec-dir.")

  (defun cli/org-exec-dir ()
    "Handle --org-exec-dir DIR command-line flag."
    (when (string-prefix-p "--org-exec-dir" argi)
      (setq cli/org-exec-dir (pop argv))
      t))

  (defun cli/org-exec-dir-run ()
    "Execute all org files in `cli/org-exec-dir', then exit Emacs.
Runs from `after-init-hook', after the full config has loaded."
    (when cli/org-exec-dir
      (builder-org-babel-execute-directory cli/org-exec-dir)
      (kill-emacs 0)))

  (add-to-list 'command-line-functions 'cli/time-reporting)
  (add-to-list 'command-line-functions 'cli/org-exec)
  (add-to-list 'command-line-functions 'cli/org-exec-dir)
  (add-hook 'after-init-hook #'cli/org-exec-run)
  (add-hook 'after-init-hook #'cli/org-exec-dir-run)

  (defun tychoish/startup-report-timing ()
    (let ((startup-time (float-time (time-subtract tychoish/startup-complete-time before-init-time)))
	  (init-time (float-time (time-subtract after-init-time before-init-time)))
	  (wall-time (float-time (time-since before-init-time))))
      (message "[emacs]: <%s> init time %s" sprite-instance-id init-time)
      (message "[emacs]: <%s> user time %s" sprite-instance-id startup-time)
      (message "[emacs]: <%s> wall time %s" sprite-instance-id wall-time)
      (alert (format "started (pid=%d) [init=%s user=%s wall=%s]" (emacs-pid) init-time startup-time wall-time)
	     :title (format "emacs-%s" sprite-instance-id))))

  (defun tychoish/startup-mark-complete ()
    (unless tychoish/startup-complete-time
      (setq tychoish/startup-complete-time (current-time))))

  (add-hook 'emacs-startup-hook 'tychoish/startup-mark-complete 99)
  (add-hook (if (daemonp) 'emacs-startup-hook 'window-setup-hook) 'tychoish/startup-report-timing 100)

  (defvar bootstrap-vendored-packages
    '((xtdlib                    "external/xtdlib"                    "https://github.com/tychoish/xtdlib.el")
      (sprite                    "external/sprite"                    "https://github.com/tychoish/sprite")
      (annotated-completing-read "external/annotated-completing-read" "https://github.com/tychoish/annotated-completing-read")
      (consult-mu                "external/consult-mu"                "https://github.com/armindarvish/consult-mu.git")
      (agent-shell-manager       "external/agent-shell-manager"       "https://github.com/ElleNajt/agent-shell-manager.git")
      (magit-dash                "external/magit-dash"                "https://github.com/tychoish/magit-dash.git")
      (agent-shell-notifications "external/agent-shell-notifications" "") ;; disabled: upstream dependency bug
      (agent-shell-queue         "external/agent-shell-queue"         "https://github.com/tychoish/agent-shell-queue"))
    "(PACKAGE PATH URL) entries bootstrapped via `bootstrap-package'.
PATH is relative to `user-emacs-directory'. Each is a git checkout under
`external/'; URL is a fallback for machines where the checkout is missing.")

  (defvar bootstrap--package-contents-refreshed nil
    "Non-nil once `package-refresh-contents' has run during this bootstrap.")

  (defun bootstrap-ensure-melpa-dependencies (main-file)
    "Install any missing dependency declared in MAIN-FILE's `Package-Requires'."
    (when (file-exists-p main-file)
      (let* ((desc (with-temp-buffer
                     (insert-file-contents main-file)
                     (package-buffer-info)))
             (deps (seq-map #'car (package-desc-reqs desc))))
	(seq-do
         (lambda (dep)
           (unless (or (eq dep 'emacs)
                       (package-installed-p dep)
                       (assq dep bootstrap-vendored-packages))
             (unless bootstrap--package-contents-refreshed
               (package-refresh-contents)
               (setq bootstrap--package-contents-refreshed t))
             (package-install dep)))
         deps))))

  (defun bootstrap-package-quickstart-stale-p ()
    "Return non-nil when `package-quickstart-file' is missing an installed package."
    (and (file-exists-p package-quickstart-file)
         (file-directory-p package-user-dir)
         (let ((quickstart-contents (with-temp-buffer
                                      (insert-file-contents package-quickstart-file)
                                      (buffer-string))))
           (seq-some (lambda (pkg-dir)
                       (and (file-directory-p pkg-dir)
                            (file-exists-p (expand-file-name (package--description-file pkg-dir) pkg-dir))
                            (not (string-search pkg-dir quickstart-contents))))
                     (directory-files package-user-dir t "\\`[^.]" t)))))

  (defun bootstrap-package (package path url)
    "Ensure PACKAGE is installed and activated."
    (with-slow-op-timer (format "<init> [external] %s" package)
      (let ((pkg-dir (expand-file-name (symbol-name package) package-user-dir)))
	(unless (or (package-installed-p package) (file-exists-p pkg-dir))
	  (let ((checkout (expand-file-name path user-emacs-directory)))
            (if (file-directory-p checkout)
		(progn
                  (bootstrap-ensure-melpa-dependencies
                   (expand-file-name (format "%s.el" package) checkout))
                  (package-vc-install-from-checkout checkout (symbol-name package)))
              (package-vc-install `(,package :url ,url))))))))

  (with-slow-op-timer "<init> package"
    (require 'package)
    (setq package-quickstart t)
    (setq package-quickstart-file (file-name-concat user-emacs-directory "state/package-quickstart.el"))
    (when (bootstrap-package-quickstart-stale-p)
      (message "[bootstrap] package-quickstart-file is stale relative to package-user-dir; refreshing")
      (package-quickstart-refresh))
    (package-activate-all)

    (setq package-archives
	  '(("melpa" . "https://melpa.org/packages/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("gnu" . "https://elpa.gnu.org/packages/")
            ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))))

  (with-slow-op-timer "<init> [external]"
    (mapc (lambda (spec)
	    (unless (package-installed-p (car spec))
	      (apply #'bootstrap-package spec)))
	  bootstrap-vendored-packages))

  (with-slow-op-timer "<init> [local]"
    (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
    (with-slow-op-timer "<init> [local] bootstrap"
      (require 'bootstrap))

    ;; remaining use-package declarations.
    (with-slow-op-timer "<init> [local] tychoish-core"
      (require 'tychoish-core)))

  ;; load the user/*.el files
  (with-slow-op-timer "<init> [user]"
    (add-to-list 'load-path (expand-file-name "user" user-emacs-directory))
    (bootstrap-set-up-user-local-config))))

(provide 'init)
