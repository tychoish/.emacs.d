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

(autoload 'hud-mode "hud-mode" "Global HUD minor mode." t)

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
(autoload 'eglox-global-mode "eglox" "Global minor mode managing Eglot LSP lifecycle." t)
(autoload 'grepx-mode "grepx" "Minor mode for persistent grepx search buffers." t)
(autoload 'chex-global-mode "chex" "Global checking and spelling mode." t)
(autoload 'hud-modeline-mode "hud-modeline" "Minor mode for lightweight HUD modeline." t)
(autoload 'bootstrap-refresh-package-quickstart "setup-package-defaults")
(autoload 'eglox-mode "eglox")

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
(defvar mcpkit-default-port)
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

(defvar bootstrap-abbrev-files-cache (make-hash-table :test #'equal)
  "Cache mapping file names to files' mtime to avoid re-importing files.")
(unless (hash-table-p bootstrap-abbrev-files-cache)
  (setq bootstrap-abbrev-files-cache (make-hash-table :test #'equal)))

(defun should-read-abbrev-file-p (path)
  (or (not (map-contains-key bootstrap-abbrev-files-cache path))
      (time-less-p (map-elt bootstrap-abbrev-files-cache path)
                   (file-attribute-modification-time (file-attributes path)))))

(defun bootstrap-load-abbrev-files ()
  (thread-last
    (directory-files (file-name-concat user-emacs-directory "abbrev") t ".el*" :nosort)
    (seq-filter #'should-read-abbrev-file-p)
    (seq-map (lambda (path) (let ((quietly t)) (read-abbrev-file path quietly) path)))
    (mapc (lambda (it) (setf (map-elt bootstrap-abbrev-files-cache it)
                             (file-attribute-modification-time (file-attributes it)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; silent startup -- avoid printing or using the Messages buffer

(defun display-startup-echo-area-message ()
  "Called during setup, intentially a noop, which omit the message."  nil)

(add-to-list 'warning-suppress-types '(files missing-lexbind-cookie))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hooks -- functions that run in hooks configured in 'bootstrap-core

(defvar bootstrap-mcp-packages
  '(mcpkit
    mcpkit-emacs
    mcpkit-editing
    denote-mcp
    agent-shell-queue-mcp
    builder-mcp
    hitl-mcp)
  "List of MCP package and feature symbols to require during bootstrap.")

(defun bootstrap-port-available-p (port)
  "Return non-nil if local TCP PORT is available to bind."
  (condition-case nil
      (let ((proc (make-network-process
                   :name " mcpkit-port-probe"
                   :server t
                   :host 'local
                   :service port
                   :noquery t
                   :family 'ipv4)))
        (delete-process proc)
        t)
    (file-error nil)))

(defun bootstrap-find-available-port (base-port &optional max-tries)
  "Find an available port starting at BASE-PORT, probing up to MAX-TRIES."
  (let ((port base-port)
        (tries (or max-tries 50)))
    (while (and (> tries 0) (not (bootstrap-port-available-p port)))
      (setq port (1+ port))
      (setq tries (1- tries)))
    (if (> tries 0)
        port
      (if (fboundp 'mcpkit-allocate-free-port)
          (mcpkit-allocate-free-port)
        (+ base-port (random 1000))))))

(defun bootstrap-mcpkit-instance-port ()
  "Return a TCP port derived from `sprite-instance-name', stable across restarts.
For named instances, the port is deterministic based on the instance name.
For \"solo\" instances (or if the preferred port is already bound by another
process), sequential probing starting at the base port assigns distinct,
deterministic ports (base, base+1, base+2, ...) so concurrent instances
never collide or fail to start."
  (let* ((name (or (ignore-errors (sprite-instance-name))
                   (bound-and-true-p sprite-instance-id)
                   (system-name)
                   "solo"))
         (base-port (+ 20000 (mod (abs (sxhash-equal name)) 10000))))
    (bootstrap-find-available-port base-port 50)))

(defvar mcpkit-default-port nil
  "Default TCP port for MCPKit server.
Computed dynamically on demand by `bootstrap-mcpkit-instance-port'.")

(defun bootstrap-load-mcp-services (&rest args)
  "Require all packages in `bootstrap-mcp-packages' incrementally with `gen.el'.
Wraps each load operation in `with-slow-op-timer' and emits individual
registration messages without batch logging. Calls ON-DONE when complete.
Keyword arguments: :on-done, :delay."
  (interactive)
  (let ((on-done (plist-get args :on-done))
        (delay (or (plist-get args :delay) 0.01)))
    (if (and (require 'gen nil t) (fboundp 'gen-run-async))
      (gen-run-async
       (lambda (pkg done-cb)
         (with-slow-op-timer (format "<bootstrap> [mcpkit] load %s" pkg)
           (require pkg nil t))
         (funcall done-cb))
       bootstrap-mcp-packages
         :concurrency 1
         :delay (or delay 0.01)
         :on-done (lambda ()
                    (with-slow-op-timer "<bootstrap> [mcpkit] post-load"
                      (when (and (fboundp 'mcpkit-editing-register-tools)
                                 (fboundp 'mcpkit-get-service)
                                 (mcpkit-get-service 'emacs))
                        (mcpkit-editing-register-tools 'emacs)))
                    (when on-done (funcall on-done)))
         :on-error (lambda (err)
                    (message "[mcpkit]: load error: %S" err)))
    ;; Synchronous fallback
    (dolist (pkg bootstrap-mcp-packages)
      (with-slow-op-timer (format "<bootstrap> [mcpkit] load %s" pkg)
        (require pkg nil t)))
    (with-slow-op-timer "<bootstrap> [mcpkit] post-load"
      (when (and (fboundp 'mcpkit-editing-register-tools)
                 (fboundp 'mcpkit-get-service)
                 (mcpkit-get-service 'emacs))
        (mcpkit-editing-register-tools 'emacs)))
    (when on-done (funcall on-done)))))

(defun bootstrap-start-mcp-services ()
  "Start all MCP services on this instance's stable port."
  (with-slow-op-timer "<bootstrap> [mcpkit] start-services"
    (let ((port (or (and (fboundp 'mcpkit-active-port) (mcpkit-active-port))
                    mcpkit-default-port
                    (setq mcpkit-default-port (bootstrap-mcpkit-instance-port)))))
      (when (fboundp 'mcpkit-start-all-services)
        (mcpkit-start-all-services port)))))

(defun bootstrap-init-mcp-services (&rest args)
  "Initialize and start MCPKit services configured in `bootstrap-mcp-packages'.
If :sync is non-nil or running noninteractively, load and start synchronously."
  (interactive)
  (let ((sync (plist-get args :sync)))
    (unless (and (fboundp 'mcpkit-active-port) (mcpkit-active-port))
      (if (or sync noninteractive)
        (progn
          (dolist (pkg bootstrap-mcp-packages)
            (with-slow-op-timer (format "<bootstrap> [mcpkit] load %s" pkg)
              (require pkg nil t)))
          (with-slow-op-timer "<bootstrap> [mcpkit] post-load"
            (when (and (fboundp 'mcpkit-editing-register-tools)
                       (fboundp 'mcpkit-get-service)
                       (mcpkit-get-service 'emacs))
              (mcpkit-editing-register-tools 'emacs)))
          (bootstrap-start-mcp-services))
      (bootstrap-load-mcp-services
       :on-done #'bootstrap-start-mcp-services)))))

(declare-function orgx-global-mode "orgx")

(defun bootstrap--execute-mode-function (fn)
  "Execute mode or setup function FN idempotently with slow-op reporting."
  (let ((name (cond
               ((symbolp fn) (symbol-name fn))
               ((byte-code-function-p fn) "<bytecode>")
               (t "<function>"))))
    (with-slow-op-timer (format "<bootstrap> [modes] %s" name)
      (condition-case err
          (condition-case nil
              (funcall fn 1)
            (wrong-number-of-arguments
             (funcall fn)))
        (error
         (message "[bootstrap] error enabling mode %s: %s" name (error-message-string err)))))))

(defun bootstrap--resolve-hook-functions (hook)
  "Return a flat list of callable functions from HOOK symbol or list."
  (let ((val (cond
              ((functionp hook) (list hook))
              ((and (symbolp hook) (boundp hook)) (symbol-value hook))
              ((listp hook) hook)
              (t nil))))
    (seq-filter (lambda (fn) (and fn (not (eq fn t))))
                (flatten-tree (if (listp val) val (list val))))))

(defun bootstrap-run-enable-modes-hook (hook &rest args)
  "Run HOOK functions non-blockingly, yielding to the event loop between each.
In interactive sessions, yields execution between functions via idle timers of
DELAY seconds (default 0.05), giving the Emacs event loop time to process input and redraw.
In `noninteractive' sessions, drains synchronously.
Calls ON-DONE when iteration completes.
Returns a cancellation function that halts pending iterations."
  (interactive (list 'bootstrap-late-enable-modes-hook))
  (let* ((delay (or (plist-get args :delay) 0.05))
         (on-done (plist-get args :on-done))
         (remaining (bootstrap--resolve-hook-functions hook))
         (timer nil))
    (cond
     ((null remaining)
      (when on-done (funcall on-done))
      #'ignore)
     (noninteractive
      (dolist (fn remaining)
        (bootstrap--execute-mode-function fn))
      (when on-done (funcall on-done))
      #'ignore)
     (t
      (letrec ((step
                (lambda ()
                  (setq timer nil)
                  (when remaining
                    (bootstrap--execute-mode-function (pop remaining))
                    (if remaining
                        (setq timer (run-with-idle-timer delay nil step))
                      (when on-done (funcall on-done)))))))
        (setq timer (run-with-idle-timer delay nil step))
        (lambda ()
          (setq remaining nil)
          (when (timerp timer)
            (cancel-timer timer)
            (setq timer nil))))))))

(defcustom bootstrap-early-enable-modes-hook
  '(bootstrap-init-early-ui
    bootstrap-global-show-whitespace-mode
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
    bootstrap-init-early-instance)
  "Hook run during early bootstrap to enable core UI and editing modes."
  :type 'hook
  :group 'bootstrap)

(defun bootstrap-init-early-ui ()
  "Initialize early fringe, delight module, and default font."
  (set-fringe-mode '(4 . 4))
  (require 'delight)
  (bootstrap-ensure-default-font))

(defun bootstrap-init-early-instance ()
  "Set up instance persistence."
  (bootstrap-set-up-emacs-instance-persistence))

(defun bootstrap-init-early-modes (&optional on-done)
  "Initialize core editor UI, display modes, font, and instance persistence."
  (interactive)
  (bootstrap-run-enable-modes-hook 'bootstrap-early-enable-modes-hook
                                   :delay 0.02
                                   :on-done on-done))

(defcustom bootstrap-late-enable-modes-hook
  '(bootstrap-init-project
    popper-mode
    grepx-mode
    denote-dash-global-mode
    denote-tree-mode
    magit-dash-global-mode
    builder-mode
    chex-global-mode
    agent-shell-queue-global-mode
    nerd-icons-completion-mode
    orgx-global-mode)
  "Hook run during late bootstrap to enable tool and integration modes."
  :type 'hook
  :group 'bootstrap)

(defun bootstrap-init-project ()
  "Load project management, projectile, and grepx modules."
  (require 'setup-project)
  (require 'projectile)
  (require 'grepx))

(defun bootstrap-init-integrations (&optional on-done)
  "Initialize secondary tool integrations and modes via `bootstrap-late-enable-modes-hook'."
  (interactive)
  (bootstrap-run-enable-modes-hook
   'bootstrap-late-enable-modes-hook
   :delay 0.05
   :on-done on-done))

(defun bootstrap-init-late-services ()
  "Initialize secondary tool integrations, services, and background maintenance synchronously."
  (interactive)
  (bootstrap-init-integrations)
  (bootstrap-desktop-read-init)
  (bootstrap-init-mcp-services :sync t)
  (require 'setup-packages)
  (elpaish-install-packages :async t)
  (bootstrap-refresh-package-quickstart))

(add-lazy-init
 :name "<bootstrap> [early] ui & modes"
 :operation #'bootstrap-init-early-modes
 :delay 0.1)

(add-lazy-init
 :name "<bootstrap> [services] mcpkit"
 :operation #'bootstrap-init-mcp-services
 :delay 0.5)

(add-lazy-init
 :name "<bootstrap> [late] integrations"
 :operation #'bootstrap-init-integrations
 :delay 0.8)

(add-lazy-init
 :name "<bootstrap> [desktop] restore"
 :operation #'bootstrap-desktop-read-init
 :delay 1.8)

(add-lazy-init
 :name "<bootstrap> [packages] elpaish ensure"
 :operation (lambda ()
              (require 'setup-packages)
              (elpaish-install-packages :async t))
 :delay 6.0)

(add-lazy-init
 :name "<bootstrap> [packages] quickstart check"
 :operation #'bootstrap-refresh-package-quickstart
 :delay 10.0)

(add-one-shot-hook
 :name "<bootstrap> first-frame setup"
 :hook 'after-first-frame-created
 :form (progn
	 (with-slow-op-timer "<bootstrap> [hook] enable hud-modeline"
           (hud-modeline-mode 1))
	 (with-slow-op-timer "<bootstrap> [hook] enable light theme"
           (bootstrap-ensure-light-theme))))

(defvar tychoish/startup-complete-time nil
  "Timestamp when startup finished.")

(defun tychoish/startup-report-timing ()
  (let ((startup-time (float-time (time-subtract tychoish/startup-complete-time before-init-time)))
	(init-time (float-time (time-subtract after-init-time before-init-time)))
	(wall-time (float-time (time-since before-init-time))))
    (message "[emacs]: <%s> init time %s" sprite-instance-id init-time)
    (message "[emacs]: <%s> user time %s" sprite-instance-id startup-time)
    (message "[emacs]: <%s> wall time %s" sprite-instance-id wall-time)
    (when (fboundp 'alert)
      (alert (format "started (pid=%d) [init=%s user=%s wall=%s]" (emacs-pid) init-time startup-time wall-time)
	     :title (format "emacs-%s" sprite-instance-id)))))

(defun tychoish/startup-mark-complete ()
  (unless tychoish/startup-complete-time
    (setq tychoish/startup-complete-time (current-time))))

(add-hook 'emacs-startup-hook #'tychoish/startup-mark-complete 99)
(add-hook (if (daemonp) 'emacs-startup-hook 'window-setup-hook) #'tychoish/startup-report-timing 100)

(add-one-shot-hook
 :name "<bootstrap> set-custom-file"
 :hook 'after-init-hook
 :form (setq custom-file (sprite-state-path "custom.el"))
 ;; Depth below 0: must run before `package--save-selected-packages'.
 :depth -90)

(add-one-shot-hook
 :name "<bootstrap> auto-revert after first file"
 :hook 'find-file-hook
 :form (global-auto-revert-mode 1))

(add-one-shot-hook
 :name "ssh-agent"
 :form (bootstrap-set-up-ssh-agent)
 :hook '(eat-mode-hook magit-mode-hook telega-root-mode-hook))

(add-lazy-init
 :name "native-compile-async"
 :delay 60
 :operation #'builder-emacs-conf-native-compile-all)

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

(defun bootstrap-ensure-font (font-face-name size)
  (unless (assoc 'font default-frame-alist)
    (bootstrap-setup-font font-face-name size)))

(defun bootstrap-ensure-default-font ()
  (bootstrap-ensure-font "Source Code Pro" 13))

(define-minor-mode bootstrap-show-whitespace-mode
  "Buffer-local minor mode to show trailing whitespace."
  :lighter ""
  (setq-local show-trailing-whitespace (if bootstrap-show-whitespace-mode t nil)))

(defun bootstrap-set-up-show-whitespace ()
  "Enable whitespace visualization in current buffer."
  (interactive)
  (bootstrap-show-whitespace-mode 1)
  t)

(define-globalized-minor-mode bootstrap-global-show-whitespace-mode
  bootstrap-show-whitespace-mode
  (lambda ()
    (when (derived-mode-p 'prog-mode 'text-mode)
      (bootstrap-show-whitespace-mode 1)))
  :group 'bootstrap)

(setq modus-themes-deuteranopia t)
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive)
        (message-separator bg-main)))

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun bootstrap-load-light-theme ()
  (interactive)
  (disable-all-themes)
  (when (load-theme 'modus-operandi t t)
    (enable-theme 'modus-operandi))
  (unless (map-elt default-frame-alist 'alpha)
    (add-to-list 'default-frame-alist '(alpha . 95))))

(defun bootstrap-ensure-light-theme ()
  (unless custom-enabled-themes
    (with-slow-op-timer "<bootstrap> [theme] load light theme"
      (bootstrap-load-light-theme))))

(defun bootstrap-load-dark-theme ()
  (interactive)
  (disable-all-themes)
  (when (load-theme 'modus-vivendi t t)
    (enable-theme 'modus-vivendi))
  (unless (map-elt default-frame-alist 'alpha)
    (add-to-list 'default-frame-alist '(alpha . 95))))

(defun bootstrap-ensure-dark-theme ()
  (unless custom-enabled-themes
    (with-slow-op-timer "<bootstrap> [theme] load dark theme"
      (bootstrap-load-dark-theme))))

(defun tk-faces-setup (&rest _)
  (when (featurep 'modus-themes)
    (when (facep 'tk-delimiter-face)
      (set-face-attribute 'tk-delimiter-face nil
                          :foreground (modus-themes-get-color-value 'blue)
                          :weight 'bold))
    (when (facep 'tk-content-face)
      (set-face-attribute 'tk-content-face nil
                          :foreground (modus-themes-get-color-value 'fg-dim)
                          :weight 'bold))))

(add-hook 'enable-theme-functions #'tk-faces-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ssh-agent -- tools to make sure emacs session can connect to ssh-agent

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
