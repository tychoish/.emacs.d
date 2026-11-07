;;; setup-package-defaults.el --- Package manager defaults and settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Default configuration for package.el, archives, priorities,
;; use-package integration, and debounced quickstart management.

;;; Code:

(require 'seq)
(require 'map)
(require 'cl-lib)

(defvar autoload-compute-prefixes)
(defvar sprite-instance-id)
(defvar warning-inhibit-types nil)
(defvar warning-suppress-types nil)
(defvar warning-suppress-log-types nil)

(setq autoload-compute-prefixes nil)

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

(defun bootstrap-optimize-quickstart-file (file)
  "Transform FILE (package-quickstart.el) into a streamlined format.
Consolidates `load-path' additions into a static pre-expanded list,
strips docstrings, removes definition prefixes, strips unused let* scopes,
removes autoload feature provisions, and eliminates extraneous whitespace."
  (when (file-exists-p file)
    (with-temp-buffer
      (delay-mode-hooks (emacs-lisp-mode))
      (insert-file-contents file)
      (let ((dirs nil))
        ;; 1. Collect directories and delete individual (add-to-list 'load-path ...) forms
        (goto-char (point-min))
        (while (re-search-forward "(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))" nil t)
          (let ((match-beg (match-beginning 0))
                (match-end (match-end 0)))
            (save-excursion
              (when (re-search-backward "load-file-name \"\\([^\"]+\\)\"" nil t)
                (push (file-name-directory (match-string 1)) dirs)))
            (delete-region match-beg match-end)))

        ;; 2. Strip definition prefixes (register-definition-prefixes ...)
        (goto-char (point-min))
        (while (re-search-forward "register-definition-prefixes" nil t)
          (while (> (car (syntax-ppss)) 1)
            (backward-up-list 1))
          (let ((beg (point)))
            (forward-sexp 1)
            (when (looking-at "\n") (forward-char 1))
            (delete-region beg (point))))

        ;; 3. Strip docstrings from autoload definitions:
        ;; Match (autoload ... \"file\" \"docstring...\") and replace \"docstring...\" with nil
        (goto-char (point-min))
        (while (re-search-forward "(autoload[ \t\n\r]+['#][^ \t\n\r]+[ \t\n\r]+\"[^\"]+\"[ \t\n\r]+\"" nil t)
          (backward-char 1)
          (let ((beg (point)))
            (forward-sexp 1)
            (delete-region beg (point))
            (insert "nil")))

        ;; 4. Strip (provide '...-autoloads) forms
        (goto-char (point-min))
        (while (re-search-forward "(provide '[^)]+-autoloads)" nil t)
          (delete-region (match-beginning 0) (match-end 0)))

        ;; 5. Replace (let* ((load-file-name ...)(load-true-file-name load-file-name)) with (progn
        (goto-char (point-min))
        (while (re-search-forward "(let\\*\\s-*((\\s-*load-file-name\\s-+\"[^\"]+\"\\s-*)\\s-*(\\s-*load-true-file-name\\s-+load-file-name\\s-*))" nil t)
          (replace-match "(progn"))

        ;; 6. Clean up form-feeds and collapse blank lines
        (goto-char (point-min))
        (while (re-search-forward "[\x0c\r]" nil t)
          (replace-match ""))
        (goto-char (point-min))
        (while (re-search-forward "\n[ \t]*\n[ \t]*\n+" nil t)
          (replace-match "\n\n"))

        ;; 7. Insert pre-expanded, deduplicated static load-path list at the top
        (goto-char (point-min))
        (forward-line 3)
        (let ((clean-dirs (delete-dups (mapcar #'expand-file-name (mapcar #'directory-file-name (nreverse dirs))))))
          (insert "(setq load-path\n      (append '")
          (prin1 clean-dirs (current-buffer))
          (insert "\n              load-path))\n\n"))

        ;; Save the optimized file
        (write-region (point-min) (point-max) file nil 'silent)))))

(defun ad:package-quickstart-refresh--optimize (orig-fn &rest args)
  "Optimize quickstart file format and compile bytecode."
  (let ((real-byte-compile (symbol-function 'byte-compile-file)))
    (cl-letf (((symbol-function 'byte-compile-file)
               (lambda (file)
                 (bootstrap-optimize-quickstart-file file)
                 (with-temp-buffer
                   (let ((byte-compile-log-buffer (buffer-name))
                         (display-buffer-alist
                          (cons (cons (regexp-quote (buffer-name))
                                      '(display-buffer-no-window))
                                display-buffer-alist))
                         (warning-inhibit-types (cons '(bytecomp) (and (boundp 'warning-inhibit-types) warning-inhibit-types)))
                         (warning-suppress-types (cons '(bytecomp) (and (boundp 'warning-suppress-types) warning-suppress-types)))
                         (warning-suppress-log-types (cons '(bytecomp) (and (boundp 'warning-suppress-log-types) warning-suppress-log-types)))
                         (inhibit-message t)
                         (message-log-max nil))
                     (funcall real-byte-compile file)))
                 file)))
      (apply orig-fn args))))

(defun bootstrap-package-quickstart-stale-p ()
  "Return non-nil when quickstart file or its .elc is missing or stale."
  (let ((elc (concat (file-name-sans-extension package-quickstart-file) ".elc")))
    (or (not (file-exists-p package-quickstart-file))
        (not (file-exists-p elc))
        (time-less-p (file-attribute-modification-time (file-attributes elc))
                     (file-attribute-modification-time (file-attributes package-quickstart-file)))
        (and (file-directory-p package-user-dir)
             (let ((quickstart-mtime (file-attribute-modification-time
                                      (file-attributes elc))))
               (seq-some (lambda (pkg-dir)
                           (and (file-directory-p pkg-dir)
                                (file-exists-p (expand-file-name (package--description-file pkg-dir) pkg-dir))
                                (time-less-p quickstart-mtime
                                             (file-attribute-modification-time (file-attributes pkg-dir)))))
                         (directory-files package-user-dir t "\\`[^.]" t)))))))

(defun bootstrap-refresh-package-quickstart (&optional force)
  "Regenerate `package-quickstart-file' and clear the debounce timer.
When FORCE is nil, do nothing unless quickstart file is stale."
  (interactive "P")
  (setq bootstrap--package-quickstart-refresh-timer nil)
  (when (or force (bootstrap-package-quickstart-stale-p))
    (message "[bootstrap] %s; refreshing package quickstart"
             (if force "Package changes detected or refresh requested"
               "Package quickstart is stale"))
    (require 'package)
    (bootstrap-sync-package-load-paths)
    (package-quickstart-refresh)))

(defun bootstrap-force-refresh-package-quickstart ()
  "Unconditionally regenerate `package-quickstart-file'.
Intended as a magit-dash post-fetch hook, after submodules have changed."
  (bootstrap-refresh-package-quickstart t))

(autoload 'magit-run-git-async "magit-process")

(defun bootstrap-pull-submodules (_repo on-complete)
  "Run `git submodule foreach git pull' via Magit, then regenerate the quickstart.
Intended as a magit-dash command: runs in `default-directory' and calls
ON-COMPLETE with `ok', or `error' when git fails (output is in the
*magit-process* buffer)."
  (let ((proc (magit-run-git-async "submodule" "foreach" "git" "pull")))
    (add-function :after (process-sentinel proc)
                  (lambda (p _event)
                    (unless (process-live-p p)
                      (cond
                       ((zerop (process-exit-status p))
                        (bootstrap-force-refresh-package-quickstart)
                        (funcall on-complete 'ok))
                       (t
                        (funcall on-complete 'error "git submodule foreach pull failed; see *magit-process*"))))))
    proc))

(defun bootstrap-flush-package-quickstart ()
  "Ensure any pending `package-quickstart-refresh' runs immediately."
  (when (timerp bootstrap--package-quickstart-refresh-timer)
    (cancel-timer bootstrap--package-quickstart-refresh-timer)
    (bootstrap-refresh-package-quickstart t)))

(defun ad:refresh-package-quickstart (&rest _)
  "Debounce regeneration of `package-quickstart-file' after package operations."
  (if noninteractive
      (bootstrap-refresh-package-quickstart t)
    (when (timerp bootstrap--package-quickstart-refresh-timer)
      (cancel-timer bootstrap--package-quickstart-refresh-timer))
    (setq bootstrap--package-quickstart-refresh-timer
          (run-with-idle-timer bootstrap-package-quickstart-refresh-delay
                               nil
                               (lambda () (bootstrap-refresh-package-quickstart t))))))

(with-eval-after-load 'package
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
  (advice-add 'package-quickstart-refresh :around #'ad:package-quickstart-refresh--optimize)
  (add-hook 'package--post-download-archives-hook 'async-bytecomp-package-mode))

(add-hook 'async-pkg-install-after-hook #'ad:refresh-package-quickstart)
(add-hook 'kill-emacs-hook #'bootstrap-flush-package-quickstart)

(defun bootstrap-refresh-quickstart-after-elpaish (results _action)
  "Schedule a quickstart refresh when RESULTS include package changes."
  (when (seq-some (lambda (result)
                    (memq (plist-get result :status) '(:installed :upgraded)))
                  results)
    (ad:refresh-package-quickstart)))

(with-eval-after-load 'elpaish-install
  (require 'setup-packages)
  (add-hook 'elpaish-install-bootstrap-finished-functions
            #'bootstrap-refresh-quickstart-after-elpaish))

;;;; Async package installation

(autoload 'async-package-do-action "async-package")

(defun async-package-operation (op pkgs)
  (let* ((ops '(install upgrade 'reinstall))
         (valid-packages (seq-filter (lambda (it) (or (symbolp it) (package-desc-p it))) pkgs))
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

(defun package-install-async (pkgs)
  "Install PKGS asynchronously using `async-package-operation'."
  (interactive (list (intern (completing-read "async-install-package =>" package-archive-contents))))
  (async-package-operation 'install pkgs))

(provide 'setup-package-defaults)
;;; setup-package-defaults.el ends here
