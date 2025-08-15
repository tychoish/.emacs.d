;;; tychoish-bootstrap.el --- Utilities used during emacssetup

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0-pre
;; Package-Requires: ((emacs "24.4"))
;; Keywords: internal maint emacs startup dotemacs config
;; Homepage: https://github.com/tychoish/.eamcs.d

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

(setq default-frame-alist nil)

(defun tychoish/bootstrap-after-init-hook-fn ()
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (add-to-list 'default-frame-alist '(menu-bar-lines . nil))
  (add-to-list 'default-frame-alist '(tool-bar-lines . nil))

  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  (indent-tabs-mode -1)
  (column-number-mode 1)
  (delete-selection-mode 1)
  (transient-mark-mode 1)
  (xterm-mouse-mode 1))

(defun tychoish/setup-auto-save ()
  (let ((path (tychoish-get-config-file-path "backup/")))
    (setq auto-save-file-name-transforms `((".*" ,path t)))
    (add-to-list 'backup-directory-alist (cons "." path))

    (unless (file-exists-p path)
      (make-directory path))

    (chmod path #o700)))

(defun tychoish/setup-show-whitespace ()
  (setq-local show-trailing-whitespace t))

(defmacro tychoish/set-tab-width (num)
  (unless (integerp num)
    (signal 'wrong-type-argument num))
  (unless (< num 32)
    (warn "INVALID cannot create tab width hook function to >= 32 (%s)" num))

  (let ((generated-name (intern (format "tychoish/set-local-tab-width-%d" num))))
    `(defun ,generated-name ()
       (setq-local tab-width ,num))))

(defmacro create-run-hooks-function-for (mode)
  (let* ((mode-name (symbol-name mode))
	 (hook-name (concat mode-name "-hook"))
	 (function-name (intern (concat "run-hooks-for-" mode-name))))
    `(defun ,function-name nil
       (run-hooks (intern ,hook-name)))))

(defmacro create-local-toggle-functions (value)
  (let* ((name (symbol-name value))
	 (ops (list
	       `(,(intern (concat "turn-on-" name "-local")) . t)
	       `(,(intern (concat "turn-off-" name "-local")) . nil)
	      `(,(intern (concat "toggle-" name "-local")) . (not ,value)))))

  `(progn
     ,@(mapcar (lambda (def)
                 `(defun ,(car def) ()
		    (interactive)
		    (setq-local ,value ,(cdr def))))
	       ops))))

(defmacro create-toggle-functions (value)
  (let* ((name (symbol-name value))
	 (ops (list
	       `(,(intern (concat "turn-on-" name)) . t)
	       `(,(intern (concat "turn-off-" name)) . nil)
	      `(,(intern (concat "toggle-" name )) . (not ,value)))))

  `(progn
     ,@(mapcar (lambda (def)
                 `(defun ,(car def) ()
		    (interactive)
		    (setq ,value ,(cdr def))))
	       ops))))

(cl-defmacro add-hygenic-one-shot-hook-variadic (&key name hook function (local nil))
  (let ((cleanup (intern (format "hygenic-one-shot-%s-%s" name (gensym)))))
    `(progn
       (add-hook ',hook ',cleanup nil ,local)
       (defun ,cleanup (&opional _ignored)
         (funcall ,function)
         (remove-hook ',hook ',cleanup)
         (unintern ',cleanup))
       #',cleanup)))

(cl-defmacro add-hygenic-one-shot-hook (&key name hook function (local nil))
  (let ((cleanup (intern (format "hygenic-one-shot-%s-%s" name (gensym)))))
    `(progn
       (add-hook ',hook ',cleanup nil ,local)
       (defun ,cleanup ()
         (funcall ,function)
         (remove-hook ',hook ',cleanup)
         (unintern ',cleanup))
       #',cleanup)))


(defun set-tab-width (num-spaces)
  (interactive "nTab width: ")
  (setq-local tab-width num-spaces))

(defun tychoish--threshold-logger (threshold duration name)
  "Send a message with the DURATION and NAME when the duration is over the THRESHOLD."
  (when (>  duration threshold)
    (message "%s: %.06fs" name duration)))

(defun gui-p ()
  "Return t when the current session is or may be a GUI session."
  (when (or (daemonp) (window-system))
    t))

(defun default-string (default input)
  "Return the DEFAULT value if the INPUT is empty or nil."
  (cond
   ((string-equal default input)
    default)
   ((eq input nil)
    default)
   ((string-equal input "")
    default)
   (t
    input)))

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun on-frame-open (frame)
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (unless (display-graphic-p frame)
    (set-face-foreground 'default "unspecified-fg" frame)
    (set-face-background 'default "unspecified-bg" frame)))

(defun on-after-init ()
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  (on-frame-open (selected-frame)))

(add-hook 'after-make-frame-functions #'on-frame-open)
(add-hook 'window-setup-hook #'on-after-init)

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defun djcb-opacity-modify (&optional dec)
  "Modify the transparency of the frame.
If DEC is t, decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 2) (+ oldalpha 2))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(defun opacity-increase ()
  (interactive)
  (djcb-opacity-modify))

(defun opacity-decrease ()
  (interactive)
  (djcb-opacity-modify t))

(defun opacity-reset ()
  (interactive)
  (modify-frame-parameters nil `((alpha . 95))))

(defun tychoish-load-light-theme ()
  (interactive)
  (disable-all-themes)
  (when (load-theme 'modus-operandi t t)
    (enable-theme 'modus-operandi))
  (add-to-list 'default-frame-alist '(alpha . 97)))

(defun tychoish-load-dark-theme ()
  (interactive)
  (disable-all-themes)
  (when (load-theme 'modus-vivendi t t)
    (enable-theme 'modus-vivendi))
  (add-to-list 'default-frame-alist '(alpha . 95)))

(defun tychoish-setup-font (name number)
  (interactive "sName: \nNSize: ")
  (let ((new-font-name (concat name "-" (number-to-string number))))
    (face-remap-add-relative new-font-name)
    (add-to-list 'default-frame-alist (cons 'font new-font-name))))

(defun tychoish-get-config-file-prefix (name)
  "Build a config file basename, for NAME.
This combines the host name and the dameon name."
  (format "%s-%s-%s" (system-name) (default-string "generic" (daemonp)) name))

(defun tychoish-get-config-file-path (name)
  "Return an absolute path for NAME in the configuration directory.
The is unique to the system and daemon instance."
  (concat (expand-file-name user-emacs-directory) (tychoish-get-config-file-prefix name)))

(defun tychoish-set-up-user-local-config ()
  "Ensure that all config files in the `user-emacs-directory' + '/user' path are loaded."
  (let ((dirname (concat (expand-file-name user-emacs-directory) "user")))
    (when (file-accessible-directory-p dirname)
      (add-to-list 'load-path dirname)
      (mapc (lambda (fn)
              (when (and (string-match-p "\\.el$" fn)
                         (not (string-match-p "^flycheck_.*\\.el$" fn)))
                 (require (intern (string-remove-suffix ".el" fn)))))
            (directory-files dirname))) t))

(defalias 'kill-buffers-matching-name 'kill-matching-buffers)

(defun force-kill-buffers-matching-path (regexp)
  (interactive "sKill buffers visiting a path matching this regular expression: \nP")
  (kill-buffers-matching-path regexp t t))

(defun kill-buffers-matching-path (regexp &optional internal-too no-ask)
  "Kill buffers whose name matches the specified REGEXP.
Ignores buffers whose name starts with a space, unless optional
prefix argument INTERNAL-TOO is non-nil.  Asks before killing
each buffer, unless NO-ASK is non-nil."
  (interactive "sKill buffers visiting a path matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-file-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (funcall (if no-ask 'kill-buffer 'kill-buffer-ask) buffer)))))

(defun kill-buffers-matching-mode (mode)
  "Kill all buffers matching the symbol defined by MODE.
Returns the number of buffers killed."
  (interactive (list (intern
    (completing-read
     "mode: " obarray
     (lambda (symbol) (s-ends-with? "-mode" (symbol-name symbol)))
     t nil nil major-mode))))
 (message "killing all buffers with mode \"%s\"" mode)
 (length (->> (buffer-list)
	      (--select (with-current-buffer it (eq major-mode mode)))
	      (mapc #'kill-buffer))))

(defun pin-buffer-to-window-toggle ()
  "pin buffer to window, most useful in keeping chat buffers under control"
  (interactive)
  (let* ((buf (current-buffer))
	 (window (selected-window))
	 (current-state (window-dedicated-p window))
	 (buf-name (buffer-name)))

    (set-window-dedicated-p window (not current-state))

    (if current-state
	(message "pinned %s to window" buf-name)
      (message "unpinned %s from window" buf-name))))

(defun display-startup-echo-area-message ()  "Called during setup, intentially a noop, which omit the message."  nil)
(defun emacs-repository-version-git (dir)  "Noop definition of function to speed up startup" "")
(defun emacs-repository-get-version (&optional dir ext)  "Noop definition of function to speed up startup" "")

(defun ad:suppress-message (f &rest arg)
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply f arg)))

(advice-add 'emacs-repository-branch-git :around #'ad:suppress-message)
(advice-add 'emacs-repository-version-git :around #'ad:suppress-message)

(defun fixed-native--compile-async-skip-p (native--compile-async-skip-p file load selector)
    "Hacky fix to resolve issue with native comp."
  ;; https://emacs.stackexchange.com/questions/82010/why-is-emacs-recompiling-some-packages-on-every-startup
  (let* ((naive-elc-file (file-name-with-extension file "elc"))
         (elc-file (replace-regexp-in-string "\\.el\\.elc$" ".elc" naive-elc-file)))
    (or (gethash elc-file comp--no-native-compile)
        (funcall native--compile-async-skip-p file load selector))))
(advice-add 'native--compile-async-skip-p :around 'fixed-native--compile-async-skip-p)

(defvar tychoish-xterm-mouse-state nil)
(defun xterm-mouse-mode-toggle ()
  (interactive)
  (if tychoish-xterm-mouse-state
      (xterm-mouse-mode -1)
    (xterm-mouse-mode 1))
  (setq tychoish-xterm-mouse-state (not tychoish-xterm-mouse-state)))

(defun tychoish-system-name ()
  (interactive)
  (message (s-join " " (list "system:" (system-name)))))

(defun turn-on-soft-wrap ()
  (interactive)
  (let ((was-hard-wrapping auto-fill-function))
    (auto-fill-mode -1)
    (visual-fill-column-mode 1)
    (visual-line-mode 1)
    (when was-hard-wrapping
      (tychoish-show-wrapping-mode))))

(defun turn-off-soft-wrap ()
  (interactive)
  (let ((was-soft-wrapping (not auto-fill-function)))
    (visual-fill-column-mode -1)
    (visual-line-mode -1)
    (auto-fill-mode 1)
    (when was-soft-wrapping
      (tychoish-show-wrapping-mode))))

(defun toggle-word-wrap ()
  (interactive)
  (if auto-fill-function
      (turn-on-soft-wrap)
    (turn-on-hard-wrap)))

(defun tychoish-show-wrapping-mode ()
  (let ((buf (current-buffer))
	(wrapping-mode (if auto-fill-function
                           "hard"
                         "soft")))
  (message "wrapping mode `%s' for $s <%s>"
	   wrapping-mode
	   (buffer-local-value 'major-mode buf)
	   (buffer-name buf))))

(defalias 'turn-on-hard-wrap 'turn-off-soft-wrap)
(defalias 'turn-off-hard-wrap 'turn-on-soft-wrap)
(defalias 'toggle-soft-wrap 'toggle-on-soft-wrap)
(defalias 'toggle-hard-wrap 'toggle-off-soft-wrap)

;; formerly tychoish-editing.el

(defun toggle-local-whitespace-cleanup ()
  "Reset the before-save hook to preven cleaning up."
  (interactive)
  (if (setq-local show-trailing-whitespace (not show-trailing-whitespace))
      (progn
	(add-hook 'before-save-hook 'whitespace-cleanup nil t)
	(message "turned on whitespace-cleanup for '%s'" (buffer-file-name (current-buffer))))
    (remove-hook 'before-save-hook 'whitespace-cleanup)
    (message "turned off whitespace-cleanup for '%s'" (buffer-file-name (current-buffer)))))

(defun font-lock-show-tabs ()
  "Return a font-lock style keyword for tab characters."
  '(("\t" 0 'trailing-whitespace prepend)))

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for strings beyond WIDTH that use 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines between BEG and END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(defvar clean-kill-ring-filters '(string-blank-p))
(defvar clean-kill-ring-prevent-duplicates t)

(defun clean-kill-ring-filter-catch-p (string)
  "T if STRING satisfies at least one of `clean-kill-ring-filters'."
  (let ((caught nil)
        (s (substring-no-properties string)))
    (catch 'loop
      (dolist (filter clean-kill-ring-filters)
        (when (funcall filter s)
          (setq caught t)
          (throw 'loop t))))
    caught))

(defun clean-kill-ring-clean (&optional remove-dups)
  "Remove `kill-ring' members that satisfy one of`clean-kill-ring-filters'.

If REMOVE-DUPS or `clean-kill-ring-prevent-duplicates' is non-nil, or if called
interactively then remove duplicate items from the `kill-ring'."
  ;; from: https://github.com/NicholasBHubbard/clean-kill-ring.el/blob/main/clean-kill-ring.el
  (interactive (list t))
  (let ((new-kill-ring nil)
        (this-kill-ring-member nil)
        (i (1- (length kill-ring))))
    (while (>= i 0)
      (setq this-kill-ring-member (nth i kill-ring))
      (unless (clean-kill-ring-filter-catch-p this-kill-ring-member)
        (push this-kill-ring-member new-kill-ring))
      (setq i (1- i)))
    (if (or remove-dups clean-kill-ring-prevent-duplicates)
        (setq kill-ring (delete-dups new-kill-ring))
      (setq kill-ring new-kill-ring))))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defmacro with-silence (&rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((inhibit-message t)
         (message-log-max nil))
     (null ,@body)))

(defmacro without-messages (&rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((inhibit-message t))
     (null ,@body)))


(defmacro with-temp-keymap (map &rest body)
  "Create a temporary MAP and return it after evaluating it in the BODY."
  `(let ((,map (make-sparse-keymap)))
     ,@body
     map))

(provide 'tychoish-bootstrap)
;;; tychoish-bootstrap.el ends here
