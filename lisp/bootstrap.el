;;; bootstrap.el --- Utilities used during emacs setup -*- lexical-binding: t -*-

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

(declare-function which-key-add-key-based-replacements "which-key")
(declare-function f-glob "f")
(declare-function f-entries "f")

(setq jit-lock-defer-time 0.2)
(setq jit-lock-stealth-nice 0.2)
(setq jit-lock-stealth-load 100)
(setq indicate-empty-lines t)
(setq tooltip-resize-echo-area t)

(setq backup-by-copying t)
(setq delete-old-versions t)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq find-file-visit-truename t)
(setq auto-revert-verbose nil)
(setq auto-revert-avoid-polling t)
(setq auto-revert-interval 60)

(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

(setq cursor-in-non-selected-windows nil)
(setq scroll-conservatively 25)
(setq scroll-preserve-screen-position t)

(setq ring-bell-function #'ignore)
(setq truncate-lines t)
(setq use-dialog-box nil)
(setq use-short-answers t)
(setq load-prefer-newer t)
(setq indent-tabs-mode nil) ; (setq tab-width 4)
(setq shell-command-dont-erase-buffer 'end-last-out)
(setq undo-auto-current-boundary-timer t)
(setq comment-auto-fill-only-comments t)
(setq select-enable-clipboard nil) ;; select-enable-primary already defaults to nil.
(setq lpr-add-switches "-T ''")

(setq save-abbrevs t)
(setq text-mode-ispell-word-completion nil)
(setq completion-cycle-threshold 2)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(setq switch-to-prev-buffer-skip 'visible)
(setq split-height-threshold 100)
(setq window-sides-vertical t)
(setq ad-redefinition-action 'accept)

(setq checkdoc-force-docstrings-flag nil)
(setq checkdoc-spellcheck-documentation-flag t)

(setq show-paren-delay 0.25)

(defvar bootstrap-fallback-buffer-name "*scratch*"
  "Buffer name used as a last-resort fallback when no other buffer is available.
Override in user/*.el to customize per machine or instance.")

(defvar electric-pair-inhibition nil)
(defvar electric-pair-eagerness t)

(setq electric-indent-chars '(?\n ?:))

(defun bootstrap-electric-pair-inhibition (char)
  (if electric-pair-inhibition
      nil
    (if electric-pair-eagerness
        (electric-pair-default-inhibit char)
      (electric-pair-conservative-inhibit char))))

(with-eval-after-load 'elec-pair
  (setq electric-pair-inhibit-predicate #'bootstrap-electric-pair-inhibition)
  (add-to-list 'electric-pair-pairs '(?< . ?>)))

(with-eval-after-load 'transient
  (setq transient-values-file (file-name-concat user-emacs-directory sprite--conf-state-directory (sprite-state-file-prefix "transient-values.el"))))

(add-hook 'abbrev-mode-hook #'bootstrap-load-abbrev-files)
(add-hook 'auto-save-mode-hook #'bootstrap-set-up-auto-save)

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
  (unless (equal "solo" sprite-instance-id)
    (setq desktop-dirname (file-name-concat user-emacs-directory sprite--conf-state-directory))
    (setq desktop-base-file-name (sprite-state-file-prefix "desktop.el"))
    (setq desktop-base-lock-name (sprite-state-file-prefix (format "desktop-%d.lock" (emacs-pid))))
    (setq desktop-path (list desktop-dirname user-emacs-directory (expand-file-name "~/")))

    (setq desktop-save t)
    (setq desktop/last-save-time (current-time))
    (setq desktop-restore-frames nil)
    (setq desktop-restore-in-current-display nil)

    (if (daemonp)
        (setq desktop-restore-eager nil
              desktop-load-locked-desktop t)
      (setq desktop-restore-eager t
            desktop-load-locked-desktop nil))

    (with-gc-suppressed
     (require 'desktop)
     (when (file-exists-p (file-name-concat desktop-dirname desktop-base-file-name))
       (with-silence (desktop-read))))

    (run-with-idle-timer 120 t #'bootstrap-desktop-save)

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

(defvar bootstrap-abbrev-files-cache nil
  "cache mapping file names to files' mtime to avoid re-importing files")

(defun should-read-abbrev-file-p (path)
  (unless bootstrap-abbrev-files-cache
    (setq bootstrap-abbrev-files-cache (make-hash-table :test #'equal)))

  (or (not (map-contains-key bootstrap-abbrev-files-cache path))
      (time-less-p (map-elt bootstrap-abbrev-files-cache path)
                   (file-attribute-modification-time (file-attributes path)))))

(defun bootstrap-load-abbrev-files ()
  (unless bootstrap-abbrev-files-cache
    (setq bootstrap-abbrev-files-cache (make-hash-table :test #'equal)))

  (thread-last
    (f-entries (file-name-concat user-emacs-directory "abbrev"))
    (seq-filter (lambda (it) (string-equal (file-name-extension it) "el")))
    (seq-filter #'file-exists-p)
    (seq-filter #'should-read-abbrev-file-p)
    (seq-map (lambda (path) (let ((quietly t)) (read-abbrev-file path quietly) path)))
    (mapc (lambda (it) (setf (map-elt bootstrap-abbrev-files-cache it)
                             (file-attribute-modification-time (file-attributes it)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; system -- darwin or linux specific settings

(when (eq system-type 'darwin)
  (setq read-process-output-max (* 64 1024))
  (setq ns-function-modifier 'hyper)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq ns-use-srgb-colorspace nil)
  (setq display-highres t)
  (add-hook 'after-make-frame-functions #'contextual-menubar))

(when (eq system-type 'gnu/linux)
  (setq read-process-output-max (* 1024 1024))
  (setq x-alt-keysym 'meta)
  (setq x-super-keysym 'super))

(setq system-uses-terminfo t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; silent startup -- avoid printing or using the Messages buffer

(defun display-startup-echo-area-message ()
  "Called during setup, intentially a noop, which omit the message."  nil)

;; TODO remove these after emacs 31
(defun fixed-native--compile-async-skip-p (native--compile-async-skip-p file load selector)
  "Hacky fix to resolve issue with native comp."
  ;; https://emacs.stackexchange.com/questions/82010/why-is-emacs-recompiling-some-packages-on-every-startup
  (let* ((naive-elc-file (file-name-with-extension file "elc"))
         (elc-file (replace-regexp-in-string "\\.el\\.elc$" ".elc" naive-elc-file)))
    (or (map-elt comp--no-native-compile elc-file)
        (funcall native--compile-async-skip-p file load selector))))

(advice-add 'native--compile-async-skip-p :around 'fixed-native--compile-async-skip-p)

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
  (with-slow-op-timer "<bootstrap> [modes] which-key"
    (which-key-mode 1))
  (with-slow-op-timer "<bootstrap> [modes] repeat"
    (with-silence
      (repeat-mode 1))))

(add-lazy-init
 :name "<bootstrap> [modes] late batch"
 :operation 'bootstrap-init-late-enable-modes
 :delay 0.1275)

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

(defun bootstrap--load-user-file (feat)
  (with-slow-op-timer
    (format "<init> [user] %s.el" feat)
    (require feat)))

(defun bootstrap-set-up-user-local-config ()
  "Ensure that all config files in the `user-emacs-directory' + '/user' path are loaded."
  (thread-last
    (file-name-concat user-emacs-directory "user")
    (funcall (lambda (path) (when (file-directory-p path) (f-entries path))))
    (seq-filter (lambda (it) (string-suffix-p ".el" it)))
    (seq-map (lambda (file) (bootstrap--load-user-file
                             (intern
                              (file-name-sans-extension
                               (file-name-nondirectory file))))))))

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

;; elisp tools

(setq package-archive-priorities '(("melpa" . 100)
                                   ("nongnu" . 50)
                                   ("gnu" . 25)
                                   ("jcs-elpa" . 10)))

(defun package-install-async (pkgs)
  (interactive (list (intern (completing-read "async-install-package =>" package-archive-contents))))
  (async-package-operation 'install pkgs))

(with-eval-after-load 'use-package-core
  (defun ad:use-package-statistics-convert--higher-precision-time (result)
    "Reformat the duration column in RESULT with higher precision.
`use-package-statistics-convert' only reports two decimal places;
this widens it to four for finer-grained startup profiling."
    (let* ((package (car result))
	   (statistics (map-elt use-package-statistics package)))
      (setf (aref (cadr result) 3)
	    (format "%.4f" (use-package-statistics-time statistics)))
      result))

  (advice-add #'use-package-statistics-convert :filter-return
	      #'ad:use-package-statistics-convert--higher-precision-time))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ssh-agent -- tools to make sure emacs session can connect to ssh-agent

(defun find-ssh-agent-socket-candidates ()
  (thread-last
    (f-glob (file-name-concat temporary-file-directory "ssh-*/agent.*"))
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
