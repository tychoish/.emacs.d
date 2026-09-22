;;; bootstrap.el --- Utilities used during emacs setup -*- lexical-binding: t; -*- no-byte-compile: t;  -*-

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

(require 'xtd-macro)
(require 'cl-lib)
(require 'sprite)

(elpaish-install-packages
 '(f
   async
   cond-let
   uuidgen
   popon
   package-build
   journalctl-mode
   gist
   mcpkit
   consult-gh
   consult-flycheck
   consult-flyspell
   consult-eglot
   flyspell-correct
   marginalia
   magit-gh
   tempel
   eglot-tempel
   tempel-collection
   embark-consult
   gptel-aibo
   gptel-agent
   telega-bot
   denote-notion
   sprite
   tailscale
   docker
   sqlite-mode-extras
   nerd-icons
   nerd-icons-dired
   nerd-icons-corfu
   nerd-icons-xref
   deadgrep
   annotated-completing-read
   org-docsgen
   undercover
   pkgbuild-mode
   protobuf-mode
   ninja-mode))

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

  (setq bookmark-save-flag 1)
  (setq savehist-coding-system 'utf-8-emacs)

  (if (equal "solo" sprite-instance-id)
      (bootstrap-set-up-ephemeral-instance-file-locks)
    (bootstrap-set-up-named-instance-file-locks))

  (annotated-completing-read-setup-history)

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

;; TODO remove these after emacs 31
(defun fixed-native--compile-async-skip-p (native--compile-async-skip-p file load selector)
  "Hacky fix to resolve issue with native comp."
  ;; https://emacs.stackexchange.com/questions/82010/why-is-emacs-recompiling-some-packages-on-every-startup
  (let* ((naive-elc-file (file-name-with-extension file "elc"))
         (elc-file (replace-regexp-in-string "\\.el\\.elc$" ".elc" naive-elc-file)))
    (or (map-elt comp--no-native-compile elc-file)
        (funcall native--compile-async-skip-p file load selector))))

(advice-add 'native--compile-async-skip-p :around 'fixed-native--compile-async-skip-p)

(autoload 'hud-modeline-mode "hud-modeline.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hooks -- functions that run in hooks configured in 'bootstrap-core

(defun bootstrap-init-late-enable-modes ()
  (with-slow-op-timer "<bootstrap> [modes] set-fringe-mode"
    (set-fringe-mode '(4 . 4)))
  (with-slow-op-timer "<bootstrap> [modes] column-number-mode"
    (column-number-mode 1))
  (with-slow-op-timer "<bootstrap> [modes] delete-selection-mode"
    (delete-selection-mode 1))
  (with-slow-op-timer "<bootstrap> [modes] winner-mode"
    (winner-mode 1))
  (with-slow-op-timer "<bootstrap> [modes] electric-pair-mode"
    (electric-pair-mode 1))
  (with-slow-op-timer "<bootstrap> [modes] hud-mode"
    (hud-mode 1))
  (with-slow-op-timer "<bootstrap> [modes] pixel-scroll-precision-mode"
    (pixel-scroll-precision-mode 1))
  (with-slow-op-timer "<bootstrap> [modes] sprite-mode"
    (sprite-mode 1))
  (with-slow-op-timer "<bootstrap> [modes] which-key"
    (which-key-mode 1))
  (with-slow-op-timer "<bootstrap> [modes] repeat"
    (with-silence
      (repeat-mode 1)))
  (with-slow-op-timer "<bootstrap> [modes] nerd-icons-completion"
    (nerd-icons-completion-mode 1))
  (with-slow-op-timer "<bootstrap> [modes] delight"
    (require 'delight)))

(add-lazy-init
 :name "<bootstrap> [modes] late batch"
 :operation 'bootstrap-init-late-enable-modes
 :delay 0.1275)

(add-one-shot-hook
 :name "<bootstrap> hud-modeline"
 :form (run-with-idle-timer 0.1 nil #'hud-modeline-mode 1)
 :hook 'after-first-frame-created)

(add-one-shot-hook
 :name "<bootstrap> set-custom-file"
 :hook 'after-init-hook
 :form (setq custom-file (sprite-state-path "custom.el"))
 ;; Depth below 0: must run before `package--save-selected-packages'.
 :depth -90)

(add-one-shot-hook
 :name "<bootstrap> marginalia"
 :function marginalia-mode
 :hook 'minibuffer-setup-hook)

(add-one-shot-hook
 :name "<bootstrap> alias mouse mode"
 :hook after-first-frame-created
 :form (unless (fboundp 'mouse-major-mode-menu)
	 (defalias 'mouse-major-mode-menu 'mouse-menu-major-mode-map)))

(add-one-shot-hook
 :name "<bootstrap> display-buffer-alist"
 :hook 'after-first-frame-created
 :form (add-to-list 'display-buffer-alist
		    '(hud--readonly-file-buffer-p
		      (hud--reuse-readonly-file-window
                       display-buffer-use-some-window))))

(add-one-shot-hook
 :name "<bootstrap> enable popper"
 :hook '(compilation-mode-hook help-mode-hook special-mode-hook)
 :form (when (boundp 'popper-mode)
         (popper-mode +1)))

(add-one-shot-hook
 :name "<bootstrap> auto-revert after first file"
 :hook find-file-hook
 :form (global-auto-revert-mode 1))

(add-lazy-init
 :name "<bootstrap> ensure default font"
 :operation 'bootstrap-ensure-default-font
 :delay 0.075)

(add-lazy-init
 :name "restore-desktop"
 :operation 'bootstrap-desktop-read-init
 :delay 0.5)

(add-lazy-init
 :name "emacs-instance-persistence"
 :operation 'bootstrap-set-up-emacs-instance-persistence
 :delay 0.25)

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
  (dolist (file (directory-files path :full ".el$") nil)
    (bootstrap--load-user-file
     (file-name-sans-extension (file-name-nondirectory file)))))

(defun bootstrap-set-notes-directory (&optional path)
  (when path
    (setq local-notes-directory (expand-file-name path)))

  (unless local-notes-directory
    (error "must have defined the `local-notes-directory'"))

  (setq org-directory (file-name-concat local-notes-directory "org"))
  (setq org-agenda-files (thread-last (list org-directory user-org-directories)
                                      (flatten-tree)
                                      (seq-map #'expand-file-name)
			              (seq-filter 'identity)
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
  (unless (member 'modus-operandi custom-enabled-themes)
    (when custom-enabled-themes
      (disable-all-themes))
    (if (custom-theme-p 'modus-operandi)
        (enable-theme 'modus-operandi)
      (load-theme 'modus-operandi t nil)))
  (unless (map-elt default-frame-alist 'alpha)
    (add-to-list 'default-frame-alist '(alpha . 97))))

(defun bootstrap-ensure-light-theme ()
  (unless custom-enabled-themes
    (bootstrap-load-light-theme)))

(defun bootstrap-load-dark-theme ()
  (interactive)
  (disable-all-themes)
  (when (load-theme 'modus-vivendi t t)
    (enable-theme 'modus-vivendi))
  (add-to-list 'default-frame-alist '(alpha . 95)))

(defun bootstrap-ensure-dark-theme ()
  (unless custom-enabled-themes
    (bootstrap-load-dark-theme)))

(add-one-shot-hook
 :name "<modus-themes> ensure light theme"
 :hook after-first-frame-created
 :form (bootstrap-ensure-light-theme)
 :idle-timer 0.01)

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
