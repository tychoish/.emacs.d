;;; setup-package-hooks.el --- Non-use-package package glue -*- lexical-binding: t; -*-

;;; Commentary:
;; Package hooks, with-eval-after-load forms, and package/use-package
;; meta-tooling for packages configured outside use-package.

;;; Code:

(require 'cl-lib)
(require 'xtd-macro)

(with-eval-after-load 'elec-pair
  (setq electric-pair-inhibit-predicate #'bootstrap-electric-pair-inhibition)
  (add-to-list 'electric-pair-pairs '(?< . ?>)))

(with-eval-after-load 'transient
  (setq transient-values-file (file-name-concat user-emacs-directory sprite--conf-state-directory (sprite-state-file-prefix "transient-values.el"))))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (keymap-set dired-mode-map "w" #'wdired-change-to-wdired-mode))

(with-eval-after-load 'recentf
  (setq recentf-save-file (sprite-state-path "recentf.el")))

(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-log-types '(frameset)))

(with-eval-after-load 'dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(add-hook 'abbrev-mode-hook #'bootstrap-load-abbrev-files)
(add-hook 'auto-save-mode-hook #'bootstrap-set-up-auto-save)
(add-hook 'nerd-icons-completion-mode-hook #'nerd-icons-xref-mode)

(defun tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-complete completion-at-point-functions)))

(defun tempel-open-custom-file ()
  (interactive)
  (find-file (expand-file-name "templates" user-emacs-directory)))

(add-hook 'conf-mode-hook #'tempel-setup-capf)
(add-hook 'prog-mode-hook #'tempel-setup-capf)
(add-hook 'text-mode-hook #'tempel-setup-capf)

(with-eval-after-load 'tempel
  (setq tempel-path (expand-file-name "templates" user-emacs-directory)))

(with-eval-after-load 'nerd-icons
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

  (add-to-list 'nerd-icons-mode-icon-alist
               '(agent-shell-queue-item-view-mode nerd-icons-codicon "nf-cod-checklist" :face nerd-icons-green))

  (defun ad:nerd-icons-icon-for-buffer-safe (orig &rest args)
    "Return empty string instead of signaling for an unresolvable buffer icon."
    (condition-case nil
	(apply orig args)
      (error "")))

  (advice-add 'nerd-icons-icon-for-buffer :around #'ad:nerd-icons-icon-for-buffer-safe))

(with-eval-after-load 'nerd-icons-completion
  (cl-defmethod nerd-icons-completion-get-icon :around (cand (_cat (eql buffer)))
    "Skip icon lookup when CAND names a killed buffer.
`get-buffer' returns the buffer object even when dead, and the primary
method has no liveness check, so it errors in `set-buffer' once a
completion candidate outlives its buffer."
    (if (buffer-live-p (get-buffer cand))
        (cl-call-next-method)
      "")))

(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-command-categories '(consult-completion-in-region . imenu)))

(with-eval-after-load 'smerge-mode
  (keymap-set hud-smerge-map "r" #'smerge-kill-and-vc-next-conflict)

  (defun smerge-kill-and-vc-next-conflict ()
    "Kill the current conflict option and move to the next conflict."
    (interactive)
    (smerge-kill-current)
    (smerge-vc-next-conflict)))

(with-eval-after-load 'docker
  (setq docker-terminal-backend 'eat)
  (tychoish-transient-insert-suffix-once 'docker '(-1 0) '("m" "emacs docker commands" execute-extended-docker-command)))

(defun package-install-async (pkgs)
  (interactive (list (intern (completing-read "async-install-package =>" package-archive-contents))))
  (async-package-operation 'install pkgs))

(defun tychoish-transient-insert-suffix-once (prefix loc suffix)
  "Insert SUFFIX at LOC in transient PREFIX unless already present.
No-op if PREFIX has no `transient--layout' defined yet, or if a suffix
with the same key already exists in PREFIX."
  (when (get prefix 'transient--layout)
    (let ((key (if (consp suffix) (car suffix) suffix)))
      (unless (ignore-errors (transient-get-suffix prefix key))
        (transient-insert-suffix prefix loc suffix)))))


(cl-defmacro with-temporary-package-require (feature &rest body)
  "Install FEATURE to a temporary package directory, require it, then eval BODY.
FEATURE is a quoted symbol naming the package/feature (e.g. `'package-lint').
BODY may start with a `:path PATH' pair naming the directory to use as
`package-user-dir'; it defaults to a per-instance subdir of
`temporary-file-directory'.  Remaining BODY forms are evaluated after the
package is installed and required."
  (declare (indent 1))
  (let (path)
    (when (eq (car body) :path)
      (setq path (cadr body)
            body (cddr body)))
    `(let* ((package-user-dir (or ,path
                                  (file-name-concat temporary-file-directory
                                                    (if (fboundp 'sprite-instance-name)
                                                        (sprite-instance-name)
                                                      "emacs"))))
            (load-path load-path)
            (package-activated-list package-activated-list))
       (unless (file-exists-p package-user-dir)
         (make-directory package-user-dir t))
       (require 'package)
       (package-initialize)
       (unless (package-installed-p ,feature)
         (package-install ,feature))
       (require ,feature)
       ,@body)))

(defun tychoish-bind-deferred-keymap (map key package keymap-symbol name)
  "Bind KEY in MAP to load PACKAGE on first use, then dispatch through it.
KEYMAP-SYMBOL is the variable PACKAGE defines to hold its real keymap; it
isn't bound until PACKAGE loads, so this binds KEY to a stand-in command
that requires PACKAGE, rebinds KEY to the now-defined keymap, then replays
the keystroke so it falls through to that keymap.  NAME annotates the
binding (see `keymap-set')."
  (keymap-set map key
    (cons name
          (lambda ()
            (interactive)
            (require package)
            (keymap-set map key (symbol-value keymap-symbol))
            (setq unread-command-events
                  (listify-key-sequence (this-command-keys-vector)))))))

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
	      #'ad:use-package-statistics-convert--higher-precision-time)

  (let ((pos (member :autoload use-package-keywords)))
    (if pos
        (setcdr pos (cons :ensure-lazy (cdr pos)))
      (add-to-list 'use-package-keywords :ensure-lazy t)))

  (defun ad:use-package-handler/:ensure-skip-lazy (orig-fn name-symbol keyword ensure rest state)
    "Skip standard ensure if :ensure-lazy is present in the REST arguments."
    (if (plist-get rest :ensure-lazy)
        (use-package-process-keywords name-symbol rest state)
      (funcall orig-fn name-symbol keyword ensure rest state)))

  (advice-add 'use-package-handler/:ensure :around #'ad:use-package-handler/:ensure-skip-lazy)

  (defun use-package-handler/:ensure-lazy (name-symbol _keyword _args rest state)
    (let* ((body (use-package-process-keywords name-symbol rest state))
           (commands (plist-get state :commands)))
      (if (null commands)
          ;; Fallback to standard check if no autoload commands are specified
          `(progn
             (unless (package-installed-p ',name-symbol)
               (package-install ',name-symbol))
             (require ',name-symbol)
             ,@body)
        ;; Generate wrapper autoloads that install-then-invoke
        (let ((wrapper-defs
               (mapcar
                (lambda (cmd)
                  `(defun ,cmd (&rest args)
                     ,(format "On-demand wrapper to install and call `%s'." cmd)
                     (interactive)
                     (unless (package-installed-p ',name-symbol)
                       (message "Installing %s on-demand..." ',name-symbol)
                       (package-install ',name-symbol))
                     (fmakunbound ',cmd)
                     (load (symbol-name ',name-symbol) nil t)
                     (apply ',cmd args)))
                commands)))
          `(progn
             ,@wrapper-defs
             ,@body))))))

(declare-function package-desc-p "package")
(autoload 'async-package-do-action "async-package")

(defun async-package-operation (op pkgs)
  (let* ((ops '(install upgrade 'reinstall))
         (valid-packages (seq-filter (lambda (it) (or (symbolp it)kage-desc-p it)) pkgs))
         (filename (concat (file-name-concat temporary-file-directory
                                             (string-join (list
                                                           "emacs" sprite-instance-id
                                                           "async-package"
                                                           (symbol-name op))
                                                          "-")) ".log")))
    (unless (member op ops)
      (user-error "%s is not a valid operation %S" op ops))

    (unless valid-packages
      (user-error "must define one or more valid packages %s [%s]" valid-packages pkgs))

    (async-package-do-action op valid-packages filename)))

(add-hook 'package--post-download-archives-hook 'async-bytecomp-package-mode)
(add-hook 'dired-mode-hook 'dired-async-mode)

(provide 'setup-package-hooks)
;;; setup-package-hooks.el ends here
