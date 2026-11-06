;;; bootstrap.el --- Utilities used during emacs setup -*- lexical-binding: t; -*-

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0-pre
;; Package-Requires: ((emacs "24.4") (xtdlib "0.1"))
;; Keywords: internal maint emacs startup dotemacs config
;; Homepage: https://github.com/bootstrap-.eamcs.d

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package holds the functions I want to call directly during
;; start up, as well as functions useful for building configuration
;; that don't make sense to define elsewhere.  As this file needs to
;; load during startup my goal is to keep it as short as possible, and
;; acknowledging that the collection of functionality might need to be
;; slightly more eclectic than many other packages.

;;; Code:

(require 'sprite)
(require 'xtd-macro)
(require 'hud-mode)

(eval-and-compile
  (unless (fboundp 'with-gc-suppressed)
    (defmacro with-gc-suppressed (&rest body)
      "Execute BODY with garbage collection temporarily suppressed."
      (declare (indent 0) (debug t))
      `(let ((gc-cons-threshold 800000000000000)
             (gc-cons-percentage 1.0))
         (prog1
             (progn ,@body)
           (if (fboundp 'bootstrap-schedule-idle-gc)
               (bootstrap-schedule-idle-gc 5)
             (unless noninteractive
               (run-with-idle-timer 5 nil #'garbage-collect))))))))

(autoload 'minaid-mode "minaid" "Global minor mode coordinating the consolidated Minad completion suite." t)
(autoload 'overmark-global-mode "overmark" "Highlight annotations in text buffers." t)
(autoload 'eglox-global-mode "eglox" "Global minor mode managing Eglot LSP lifecycle." t)
(autoload 'grepx-mode "grepx" "Minor mode for persistent grepx search buffers." t)
(autoload 'chex-global-mode "chex" "Global checking and spelling mode." t)
(autoload 'hud-modeline-mode "hud-modeline" "Minor mode for lightweight HUD modeline." t)
(autoload 'bootstrap-refresh-package-quickstart "setup-package-defaults")

(defun bootstrap-check-package-quickstart ()
  "Refresh the package quickstart if it is stale."
  (bootstrap-refresh-package-quickstart))
(autoload 'eglox-mode "eglox")
(autoload 'telegax-mode "telegax" "Global minor mode managing Telega lifecycle integrations." t)
(autoload 'telegax-global-mode "telegax" "Global minor mode anchoring the Telega prefix keys." t)
(autoload 'telegax-switch-to-root "telegax" nil t)
(autoload 'telegax-force-kill "telegax" nil t)
(autoload 'telegax-disconnect "telegax" nil t)

(autoload 'hud-dispatch "hud" nil t)
(autoload 'hud-select "hud" nil t)

(autoload 'tychoish-mail-select-account "setup-mail" nil t)
(autoload 'tychoish-define-mail-account "setup-mail")
(autoload 'tychoish-mail-add-refile-rule "setup-mail")

(autoload 'daemons-dash "daemons-dash" nil t)
(autoload 'daemons-dash-dispatch "daemons-dash" nil t)

(defvar desktop-base-file-name)
(defvar desktop-base-lock-name)
(defvar desktop-path)
(defvar desktop-save)
(defvar desktop-restore-frames)
(defvar desktop-restore-in-current-display)
(defvar desktop-restore-eager)
(defvar desktop-load-locked-desktop)
(defvar desktop-globals-to-save)
(defvar desktop-modes-not-to-save)
(defvar desktop-buffers-not-to-save)
(defvar desktop-files-not-to-save)
(declare-function desktop-save "desktop")
(declare-function desktop-read "desktop")

(defvar project-list-file)
(defvar savehist-file)
(defvar savehist-coding-system)
(defvar savehist-additional-variables)
(defvar bookmark-default-file)
(defvar bookmark-save-flag)
(defvar tramp-persistency-file-name)
(defvar transient-history-file)
(defvar request-storage-directory)
(defvar url-configuration-directory)
(defvar denote-sync-cache-directory)
(defvar org-directory)
(defvar org-agenda-files)
(defvar org-annotate-file-storage-file)
(defvar org-default-notes-file)
(defvar org-archive-location)
(defvar deft-directory)
(defvar denote-directory)
(defvar modus-themes-deuteranopia)
(defvar modus-themes-common-palette-overrides)

(declare-function modus-themes-get-color-value "modus-themes")

(defalias 'mouse-major-mode-menu 'mouse-menu-major-mode-map)

(defvar tychoish--suppress-supersession-prompt nil
  "Non-nil while handling an `emacsclient --eval' request or a timer callback.
Bound by advice on `server-eval-and-print' and `timer-event-handler' --
between them, every path with no human watching to answer
`ask-user-about-supersession-threat' (which would otherwise hang the
daemon indefinitely, e.g. an idle-timer-driven desktop/abbrev autosave
racing a file an agent just rewrote on disk). A live interactive save,
triggered directly by a keypress, still prompts normally.")

(with-eval-after-load 'server
  (advice-add 'server-eval-and-print :around
              (lambda (orig-fn expr proc)
                (let ((tychoish--suppress-supersession-prompt t))
                  (funcall orig-fn expr proc)))))

(advice-add 'timer-event-handler :around
            (lambda (orig-fn timer)
              (let ((tychoish--suppress-supersession-prompt t))
                (funcall orig-fn timer))))

(advice-add 'ask-user-about-supersession-threat :around
            (lambda (orig-fn filename)
              (if tychoish--suppress-supersession-prompt
                  (message "%s changed on disk, editing file anyway" filename)
                (funcall orig-fn filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; id-state -- emacs daemon/instance identification for state config

(defun gui-p ()
  "Return t when the current session is or may be a GUI session."
  (when (or (daemonp) (window-system))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state -- setup desktop/bookmarks/savehist

(defvar desktop/last-save-time nil)
(defvar desktop-dirname nil)

(defun bootstrap-set-up-emacs-instance-persistence ()
  ;; `package-quickstart-file'/`package-quickstart' are set in init.el,
  ;; before `package-activate-all' runs -- this is deliberately shared
  ;; across instances, not per-instance state, so it does not belong here.
  (setq project-list-file (sprite-state-path "projects.el"))
  (setq savehist-file (sprite-state-path "history.el"))
  (setq bookmark-default-file (sprite-state-path "bookmarks.el"))
  (setq tramp-persistency-file-name (sprite-state-path "tramp.el"))
  (setq transient-history-file (sprite-state-path "transient-history.el"))

  (setq auto-save-list-file-prefix (sprite-state-path "auto-save-list/"))
  (setq request-storage-directory (sprite-state-path "request/"))
  (setq url-configuration-directory (sprite-state-path "url/"))
  (setq denote-sync-cache-directory (sprite-state-path "denote-sync/"))

  (setq bookmark-save-flag 1)
  (setq savehist-coding-system 'utf-8-emacs)

  (if (equal "solo" sprite-instance-id)
      (bootstrap-set-up-ephemeral-instance-file-locks)
    (bootstrap-set-up-named-instance-file-locks))

  (annotated-completing-read-setup-history)
  (add-to-list 'savehist-additional-variables 'compile-history)

  (with-silence
    (recentf-mode 1)
    (savehist-mode 1)))

(defun bootstrap-desktop-save ()
  "Save desktop... sometimes"
  (interactive)
  (unless (equal "solo" sprite-instance-id)
    (when (or (> 40 (random 100))
              (< 150 (float-time (time-since desktop/last-save-time))))
      (desktop-save desktop-dirname)
      (setq desktop/last-save-time (current-time)))))

(defun bootstrap-desktop-read-init ()
  ;; only read the desktop if we're not in the "solo" (no ID) emacs
  ;; instance.
  (with-gc-suppressed
   (require 'desktop)
   (unless (equal "solo" sprite-instance-id)
     (setq desktop-dirname (file-name-concat user-emacs-directory sprite--conf-state-directory))
     (setq desktop-base-file-name (sprite-state-file-prefix "desktop.el"))
     (setq desktop-base-lock-name (sprite-state-file-prefix (format "desktop-%d.lock" (emacs-pid))))
     (setq desktop-path (list desktop-dirname user-emacs-directory (expand-file-name "~/")))

     (setq desktop-save t)
     (setq desktop/last-save-time (current-time))
     (setq desktop-restore-frames nil)
     (setq desktop-restore-in-current-display nil)

     ;; Fully eager everywhere, daemon or not. Lazy restore
     ;; (`desktop-idle-create-buffers') runs on an idle timer that competes
     ;; with everything else, including agent-shell/ACP's own message
     ;; draining, for minutes after every restart, since Emacs is
     ;; single-threaded. A slower, blocking startup beats that contention.
     (setq desktop-restore-eager t)
     (if (daemonp)
         (setq desktop-load-locked-desktop t)
       (setq desktop-load-locked-desktop nil))

     (when (file-exists-p (file-name-concat desktop-dirname desktop-base-file-name))
       (with-silence (desktop-read)))

     (run-with-idle-timer 120 t #'bootstrap-desktop-save))

   (add-to-list 'desktop-globals-to-save 'register-alist)
   (add-to-list 'desktop-globals-to-save 'file-name-history)
   (add-to-list 'desktop-modes-not-to-save 'dired-mode)
   (add-to-list 'desktop-modes-not-to-save 'Info-mode)
   (add-to-list 'desktop-modes-not-to-save 'org-mode)
   (add-to-list 'desktop-modes-not-to-save 'eww-mode)
   (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
   (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

   (setq desktop-buffers-not-to-save
         (concat "\\("
                 "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|"
                 "\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                 "\\)$"))

   (setq desktop-files-not-to-save
         (concat "\\(\\`/[^/:]*:\\|(ftp)\\'\\)" ;; default
                 "^/usr/lib/go/.*\\|"
                 "^/usr/lib/rustlib/.*\\|"
                 "^/home.+go/pkg/mod\\|"
                 "^/home.+\\.cargo"))))


(defun bootstrap-ensure-packages-installed ()
  (elpaish-install-packages :async t))

(defmacro bootstrap-require-package (pkg)
  "Define `bootstrap-require-pkg-PKG', a startup operation that requires PKG.
PKG is a feature symbol, given bare or quoted.  The defined function signals
an error if requiring PKG does not leave it `featurep'.  Return the function
symbol, so the form can be evaluated where the symbol is wanted."
  (declare (indent 0))
  (let ((feature (if (eq (car-safe pkg) 'quote) (cadr pkg) pkg)))
    (unless (and feature (symbolp feature))
      (error "bootstrap-require-package: expected a feature symbol, got %S" pkg))
    `(defun ,(intern (format "bootstrap-require-pkg-%s" feature)) ()
       ,(format "Require `%s' as a startup operation." feature)
       (require ',feature)
       (unless (featurep ',feature)
         (error "Feature `%s' not provided after require" ',feature)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; silent startup -- avoid printing or using the Messages buffer

(defun display-startup-echo-area-message ()
  "Called during setup, intentially a noop, which omit the message."  nil)

(add-to-list 'warning-suppress-types '(files missing-lexbind-cookie))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hooks -- functions that run in hooks configured in 'bootstrap-core

(declare-function sprite-instance-name "sprite")
(defvar mcpkit-instance-name-function)
(setq mcpkit-instance-name-function #'sprite-instance-name)

(declare-function orgx-global-mode "orgx")

(defun bootstrap--execute-startup-operation (fn &optional frame)
  "Execute startup operation FN in FRAME, reporting failures."
  (condition-case err
      (when (or (null frame) (frame-live-p frame))
        (with-selected-frame (or frame (selected-frame))
          (with-slow-op-timer (format "<bootstrap> [startup] %s" fn)
            (if (equal (cdr (func-arity fn)) 0)
                (funcall fn)
              (funcall fn 1)))))
    (error (message "[bootstrap] %s failed: %s" fn (error-message-string err)))))

(defun bootstrap-run-startup-operations (operations &rest args)
  "Run OPERATIONS in order, yielding between them in interactive sessions.
DELAY defaults to 0.1 seconds.  FRAME selects the frame for operations.
Call ON-DONE after the last operation.  Return a cancellation function."
  (let ((delay (or (plist-get args :delay) 0.1))
        (on-done (plist-get args :on-done))
        (frame (plist-get args :frame))
        (remaining (copy-sequence operations))
        (timer nil))
    (cond
     ((null remaining)
      (when on-done (funcall on-done))
      #'ignore)
     (noninteractive
      (seq-do (lambda (fn) (bootstrap--execute-startup-operation fn frame))
              remaining)
      (when on-done (funcall on-done))
      #'ignore)
     (t
      (letrec ((step
                (lambda ()
                  (setq timer nil)
                  (when remaining
                    (bootstrap--execute-startup-operation (pop remaining) frame)
                    (if remaining
                        (setq timer (run-with-idle-timer delay nil step))
                      (when on-done (funcall on-done)))))))
        (setq timer (run-with-idle-timer delay nil step))
        (lambda ()
          (setq remaining nil)
          (when (timerp timer)
            (cancel-timer timer)
            (setq timer nil))))))))

(bootstrap-require-package 'setup-project)
(bootstrap-require-package 'setup-packages)

(defun bootstrap-set-up-fringe-mode ()
  (set-fringe-mode '(4 . 4)))

(defcustom bootstrap-early-startup-operations
  '(bootstrap-set-up-fringe-mode
    bootstrap-trailing-whitespace-mode
    column-number-mode
    delete-selection-mode
    winner-mode
    electric-pair-mode
    pixel-scroll-precision-mode
    repeat-mode
    hud-mode
    sprite-mode
    which-key-mode
    eglox-global-mode
    minaid-mode
    telegax-global-mode
    bootstrap-set-up-emacs-instance-persistence)
  "UI, key binding, and persistence operations started early in bootstrap."
  :type '(repeat function)
  :group 'bootstrap)

(defcustom bootstrap-late-startup-operations
  '(bootstrap-require-pkg-setup-project
    popper-mode
    grepx-mode
    denote-dash-global-mode
    denote-tree-mode
    magit-dash-global-mode
    builder-mode
    chex-global-mode
    overmark-global-mode
    agent-shell-queue-global-mode
    nerd-icons-completion-mode
    orgx-global-mode
    global-auto-revert-mode
    orgx-capture-setup-standard-templates
    mcpkit-init-services
    bootstrap-desktop-read-init)
  "Behavior, integration, and session restore operations started later."
  :type '(repeat function)
  :group 'bootstrap)

(defcustom bootstrap-after-first-frame-operations
  '(bootstrap-ensure-default-font
    bootstrap-ensure-frame-alpha
    disable-all-themes
    bootstrap-default-theme
    hud-modeline-mode)
  "Appearance and modeline operations started after the first frame."
  :type '(repeat function)
  :group 'bootstrap)

(defcustom bootstrap-post-late-startup-operations
  '(bootstrap-require-pkg-setup-packages
    bootstrap-ensure-packages-installed
    bootstrap-check-package-quickstart
    builder-emacs-conf-native-compile-all)
  "Package setup and maintenance operations started after late startup."
  :type '(repeat function)
  :group 'bootstrap)

(add-lazy-init
 :name "<bootstrap> startup"
 :delay 0.1
 :form (bootstrap-run-startup-operations
        (append bootstrap-early-startup-operations
                bootstrap-late-startup-operations
                bootstrap-post-late-startup-operations)))

(add-one-shot-hook
 :name "<bootstrap> first-frame setup"
 :hook 'after-first-frame-created
 :form (bootstrap-run-startup-operations
	bootstrap-after-first-frame-operations
        :frame (selected-frame)))

(defvar bootstrap-startup-complete-time nil
  "Timestamp when startup finished.")

(defun bootstrap-report-startup-timing ()
  (let ((startup-time (float-time (time-subtract bootstrap-startup-complete-time before-init-time)))
	(init-time (float-time (time-subtract after-init-time before-init-time)))
	(wall-time (float-time (time-since before-init-time))))
    (message "[emacs]: <%s> init time %s" sprite-instance-id init-time)
    (message "[emacs]: <%s> user time %s" sprite-instance-id startup-time)
    (message "[emacs]: <%s> wall time %s" sprite-instance-id wall-time)
    (when (fboundp 'alert)
      (alert (format "started (pid=%d) [init=%s user=%s wall=%s]" (emacs-pid) init-time startup-time wall-time)
	     :title (format "emacs-%s" sprite-instance-id)))))

(defun bootstrap-startup-mark-complete ()
  (unless bootstrap-startup-complete-time
    (setq bootstrap-startup-complete-time (current-time))))

(defun bootstrap-setup-custom-file ()
  (setq custom-file (sprite-state-path "custom.el")))

(add-hook 'emacs-startup-hook #'bootstrap-startup-mark-complete 99)
(add-hook 'after-init-hook #'bootstrap-setup-custom-file -90)
(add-hook (if (daemonp) 'emacs-startup-hook 'window-setup-hook) #'bootstrap-report-startup-timing 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bootstrap load user files

(defun bootstrap--load-user-file (name)
  (with-slow-op-timer (format "<init> [user] %s.el" name)
    (require (intern name))))

(defun bootstrap-set-up-user-local-config (path)
  "Ensure that all config files in the `user-emacs-directory' + '/user' path are loaded."
  (dolist (file (directory-files path :full "\\.el$") nil)
    (bootstrap--load-user-file
     (file-name-sans-extension (file-name-nondirectory file)))))

(defvar local-notes-directory nil
  "Directory where notes (org, roam, deft, etc.) are stored.")

(defvar user-org-directories nil
  "Additional directories that may contain org files.")

(defun bootstrap-set-notes-directory (&optional path)
  "Set the notes directory to PATH and update note and org-mode paths."
  (interactive
   (list (read-directory-name "Notes directory: " (or local-notes-directory "~/notes"))))
  (when path
    (setq local-notes-directory (expand-file-name path)))
  (unless local-notes-directory
    (error "Must define `local-notes-directory'"))
  (setq org-directory (file-name-concat local-notes-directory "org"))
  (setq org-agenda-files (thread-last (list org-directory user-org-directories)
                                      (flatten-tree)
                                      (seq-map #'expand-file-name)
                                      (seq-filter #'identity)
                                      (seq-map #'string-trim)
                                      (seq-remove #'string-empty-p)
                                      (seq-uniq)))
  (setq org-annotate-file-storage-file (file-name-concat org-directory "records.org"))
  (setq org-default-notes-file (file-name-concat org-directory "records.org"))
  (setq org-archive-location (file-name-concat org-directory "archive/%s::datetree/"))
  (setq deft-directory (file-name-concat local-notes-directory "deft"))
  (setq denote-directory (file-name-concat local-notes-directory "denote"))
  local-notes-directory)

(defun bootstrap-set-up-auto-save ()
  (let ((path (sprite-state-path "backup/")))
    (setq auto-save-file-name-transforms `((".*" ,path t)))
    (add-to-list 'backup-directory-alist (cons "." path))

    (unless (file-exists-p path)
      (make-directory path t))
    (chmod path #o700)))

(defun bootstrap-set-up-named-instance-file-locks ()
  (let ((path (sprite-state-path "locks/")))
    (setq lock-file-name-transforms
          `(("\\`/.*/\\([^/]+\\)\\'" ,(concat path "\\1") t)))

    (unless (file-exists-p path)
      (make-directory path t))
    (chmod path #o700)))

(defun bootstrap-set-up-ephemeral-instance-file-locks ()
  (let* ((path (car (thread-last (list (format "/run/user/%d" (user-uid))
				       "/var/tmp"
				       (temporary-file-directory))
				 (seq-filter #'file-exists-p))))
	 (solo-lock-path (file-name-concat path (format "emacs-%d" (emacs-pid)))))

    (setq lock-file-name-transforms
          `(("\\`/.*/\\([^/]+\\)\\'" ,(concat solo-lock-path "\\1") t)))

    (unless (file-exists-p solo-lock-path)
      (make-directory solo-lock-path t))
    (chmod solo-lock-path #o700)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; frame/window -- setup and manage frames and windows

;; display -- manage fonts, rendering, themes, for (mostly) gui emacs

(defun bootstrap-setup-font (font-face-name size)
  (interactive "sName: \nNSize: ")
  (let ((new-font-name (concat font-face-name "-" (number-to-string size)))
	(font-cell (assoc 'font default-frame-alist)))
    (if font-cell
	(setcdr font-cell new-font-name)
      (add-to-list 'default-frame-alist (cons 'font new-font-name)))
    (when (display-graphic-p)
      (set-frame-font new-font-name nil t)))
  (assoc 'font default-frame-alist))

(defvar bootstrap-trailing-whitespace-mode)

(defun bootstrap--set-trailing-whitespace ()
  "Apply `bootstrap-trailing-whitespace-mode' in the current buffer."
  (setq-local show-trailing-whitespace bootstrap-trailing-whitespace-mode))

(define-minor-mode bootstrap-trailing-whitespace-mode
  "Show trailing whitespace in programming and text buffers."
  :global t
  :group 'bootstrap
  (if bootstrap-trailing-whitespace-mode
      (progn
        (add-hook 'prog-mode-hook #'bootstrap--set-trailing-whitespace)
        (add-hook 'text-mode-hook #'bootstrap--set-trailing-whitespace))
    (remove-hook 'prog-mode-hook #'bootstrap--set-trailing-whitespace)
    (remove-hook 'text-mode-hook #'bootstrap--set-trailing-whitespace))
  (seq-do (lambda (buffer)
            (with-current-buffer buffer
              (when (derived-mode-p 'prog-mode 'text-mode)
                (bootstrap--set-trailing-whitespace))))
          (buffer-list)))

(setq modus-themes-deuteranopia t)
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive)
        (message-separator bg-main)))

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun bootstrap-ensure-font (font-face-name size)
  (unless (assoc 'font default-frame-alist)
    (bootstrap-setup-font font-face-name size)))

(defun bootstrap-ensure-default-font ()
  (bootstrap-ensure-font "Source Code Pro" 13))

(defun bootstrap-ensure-frame-alpha ()
  "Default frame opacity to 95 percent, including the selected frame."
  (unless (map-elt default-frame-alist 'alpha)
    (add-to-list 'default-frame-alist '(alpha . 95)))
  (when (and (display-graphic-p) (not (frame-parameter nil 'alpha)))
    (set-frame-parameter nil 'alpha (map-elt default-frame-alist 'alpha))))

(defun bootstrap-default-theme ()
  (interactive)
  (when (load-theme 'modus-operandi t t)
    (enable-theme 'modus-operandi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ssh-agent -- tools to make sure emacs session can connect to ssh-agent

(add-one-shot-hook
 :name "ssh-agent"
 :form (bootstrap-set-up-ssh-agent)
 :hook '(eat-mode-hook magit-mode-hook telega-root-mode-hook))

(defun find-ssh-agent-socket-candidates ()
  (thread-last
    (directory-files temporary-file-directory t ".*ssh-.*/agent.*" :nosort)
    (append (list (format "/run/user/%d/ssh-agent.socket" (user-uid))))
    (seq-sort #'string-lessp)
    (seq-uniq)
    (seq-remove #'null)
    (seq-filter #'file-writable-p)
    (nreverse)))

(defun bootstrap-set-up-ssh-agent ()
  (interactive)
  (let (env-value sockets)
    (unless (setq env-value (getenv "SSH_AUTH_SOCK"))
      (setq sockets (find-ssh-agent-socket-candidates))
      (when (and sockets
		 (<= 1 (length sockets)))
	(setq env-value (setenv "SSH_AUTH_SOCK" (car sockets)))))
    env-value))

(provide 'bootstrap)
;;; bootstrap.el ends here
