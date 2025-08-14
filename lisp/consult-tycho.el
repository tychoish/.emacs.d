;;; consult-tycho.el --- tycho(ish) consult helpers -*- lexical-binding: t -*-

;; Author: sam kleinman
;; Maintainer: tychoish

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These are mostly helpers to for writing my own set of consult
;; helpers largely for "better" incremental (rip)grep tools.

;;; Code:

(require 'consult)

(defun consult-apropos--get-all-symbols ()
  "In progress consult source function.
Builds and returns a list of (name kind symbol) values in persuit of a
helm-appropos replacement."
  (let ((symbs '()))
    (mapatoms (lambda (symb)
                (let ((name (symbol-name symb)))
                  (cond
                   ((local-variable-p symb) (push `(,name 'local-variable ,symb) symbs))
                   ((custom-variable-p symb) (push `(,name 'custom-variable ,symb) symbs))
                   ((commandp symb) (push `(,name 'command ,symb) symbs))
                   ((functionp symb) (push `(,name 'function ,symb) symbs))
                   ((facep symb) (push `(,name 'face ,symb) symbs))
                   ((string-match-p "%" name))
                   (t (push `(,name 'variable ,symb) symbs))))))
    symbs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun string-with-non-whitespace-content-p (value)
  "Return t when `VALUE' is a string with non-whitespace content and nil otherwise."
  (and (stringp value)
       (not (string-empty-p (string-trim value)))))

(defun trimmed-string-or-nil (value)
  (and (stringp value)
       (unless (string-empty-p (setq value (string-trim value))) value)
       value))

(defmacro -add-if-empty (op list-val)
  `(if ,list-val
       ,list-val
     (list ,op)))

(defun consult-tycho--clean-options-for-selection (input)
  "Process `INPUT' list removing: duplicates, nils, and empty or whitespace elements."
  (->> input
       (-filter #'stringp)
       (-map #'string-trim)
       (-remove #'string-empty-p)
       (-map #'expand-file-name)
       (-sort #'string-greaterp)
       (-distinct)))

(defun consult-tycho--resolve-initial-grep (prompt prompt-annotation initial)
  "Return the initial text for a query, processing `INITIAL' as needed."
  ;; if the string is empty or only whitespace, it's undefined,
  ;; otherwise use it.
  (or (trimmed-string-or-nil initial)
      ;; with the prefix argument, ask the user
      (when current-prefix-arg
	(consult-tycho--get-seed-for-operation
	 (format "%s<init:%s>: " "TEST" "xxx") (trimmed-string-or-nil "")))
      ;; otherwise, provide the empty string...
      ""))

(defun consult-tycho--get-seed-for-operation (prompt initial)
  "Generate a list of contextual seeds if `INITIAL' is unspecified with the provided `PROMPT'."
  (or initial (consult-tycho--select-context-for-operation prompt)))

(defun consult-tycho--select-context-for-operation (prompt)
  "Pick string to use as context in a follow up operation using PROMPT."
  (let* ((current-selection (s-trim (buffer-substring (region-beginning) (region-end))))
         (this-command this-command)
         (selections (->> (list (when (and (not current-prefix-arg)
                                           (<= 2 (length current-selection)))
                                  current-selection))
                          (-concat
                           (cond ((derived-mode-p 'text-mode)
                                  (list (thing-at-point 'word)
                                        (thing-at-point 'email)
                                        (thing-at-point 'url)
                                        (thing-at-point 'sentence)))
                                 ((derived-mode-p 'prog-mode)
                                  (list (thing-at-point 'symbol)
                                        (thing-at-point 'word)
                                        (thing-at-point 'sexp)
                                        (thing-at-point 'defun)))
                                 (t '())))
                          (-keep #'trimmed-string-or-nil)
                          (-add-if-empty (thing-at-point 'line))
                          (-filter (lambda (elem) (> 32 (length elem))))
                          (-sort #'string-greaterp)
                          (-distinct))))

    (or (when (length= selections 1) (nth 0 selections))
        (when (length> selections 1)
          (consult--read selections
           :sort nil
           :command this-command
           :require-match nil
           :prompt (format "%s: " prompt))))))

(defun get-parent-dirs (start stop)
  "Generate list of intermediate paths between `START' and `STOP'."
  (let* ((stop-path (expand-file-name (string-trim stop)))
         (current (expand-file-name (string-trim start)))
         (output (list stop-path current)))
    (while (or (not (string= current stop-path))
               (not (string-prefix-p stop-path current)))
      (push (setq current (file-name-parent-directory current)) output))
    output))

(defun consult-tycho--select-directory (&optional input-dirs)
  "Select a directory from a provided or likely set of `INPUT-DIRS`'."
  (setq input-dirs (or input-dirs default-directory))
  (consult--read
   (or (when (listp input-dirs)
         (consult-tycho--filter-directory-candidate-list input-dirs))
       (if (stringp input-dirs)
           (listp (setq input-dirs (expand-file-name (string-trim input-dirs))))
         (list input-dirs))
       (consult-tycho--clean-options-for-selection
        (append (get-parent-dirs default-directory (or (projectile-project-root) ""))
                (list user-emacs-directory "~/")))
       (list default-directory (projectile-project-root)))
   :sort nil
   :prompt "in directory: "))

(defun consult-tycho--discover-directory (dir)
  "Expand or produce a non-zero directory for DIR."
  (or (when current-prefix-arg
        (consult-tycho--select-directory dir))
      (trimmed-string-or-nil dir)
      (or (projectile-project-root)
          default-directory)))

(defun consult-tycho--filter-directory-candidate-list (input-dirs)
  "Expand all paths in `INPUT-DIRS' removing non-string, duplicate and whitespace options."
  (mapcar #'expand-file-name (consult-tycho--clean-options-for-selection input-dirs)))

(defun consult-org--setup-template ()
  "Add special template to capture to a target selectable via consult. Named for clarity in hooks."
  (add-to-list 'org-capture-templates
               '("c" "consult-org-capture: select heading interactively ..." entry (consult-org--capture-target 'agenda)
                  "* TODO %?\n  %i"
		  :prepend t
		  :kill-buffer t
		  :empty-lines-after 1)))

(defun consult-org--capture-target ()
  "Choose a capture target interactively.
This function returns a value suitable for use as the `target'
entry of `org-capture-templates'."
  (list 'function
        (lambda ()
	  (unless (derived-mode-p #'org-mode)
                 (user-error "Must call from an Org buffer.")
          (let ((consult--read-config
                 `((,this-command
                    :prompt "org-apture target: "
                    :preview-key "M-."))))
            (set-buffer
             (save-window-excursion
               (consult-org-heading nil 'agenda)
               (current-buffer))))))))

(defun consult-tycho--incremental-grep (prompt builder initial)
  "Do incremental grep-type operation. Like the `consult-grep' operation
upon which it was based, permits interoperability between git-grep ag, ack, and rg"
  (let ((consult-async-input-debounce 0.025)
	(consult-async-input-throttle 0.05)
	(consult-async-refresh-delay 0.025)
	(this-command this-command))
    (consult--read
     (consult--process-collection builder
       :transform (consult--grep-format builder)
       :file-handler t)
     :prompt prompt
     :lookup #'consult--lookup-member
     :state (consult--grep-state)
     :initial initial
     :add-history (thing-at-point 'symbol)
     :require-match nil
     :category 'consult-grep
     :group nil ;; #'consult--prefix-group <- this groups results by common prefix (e.g. file)
     :history '(:input consult--grep-history)
     :sort nil)))

;;;###autoload
(defun consult-rg-pwd (&optional initial)
  "Start an iterative rg session with context.
DIR and INITIAL integrate with the consult-grep API."
  (interactive "P")
  (let ((dir-base (f-base default-directory)))
    (consult-tycho--incremental-grep
     (format "rg<pwd:%s>: " dir-base)
     (consult--ripgrep-make-builder (list (expand-file-name default-directory)))
     (consult-tycho--resolve-initial-grep "rg" (f-base default-directory) initial))))

;;;###autoload
(defun consult-rg-for-thing (&optional dir initial)
  "Start an iterative rg session with context.
DIR and INITIAL integrate with the consult-grep API."
  (interactive "P")
  (consult-rg dir (consult-tycho--select-context-for-operation "rg<thing>")))

;;;###autoload
(defun consult-rg (&optional dir initial)
  "Start and iterative rg session.
DIR and INITIAL integrate with the consult-grep API."
  (interactive "P")
  ;; `consult--directory-prompt' --> '(prompt paths <default>-dir)
  (let* ((prompt-paths-dir (consult--directory-prompt "rg" (consult-tycho--discover-directory dir)))
         (default-directory (nth 2 prompt-paths-dir)))
    (consult-tycho--incremental-grep
     ;; prompt:
     (nth 0 prompt-paths-dir)
     ;; builder:
     (consult--ripgrep-make-builder (nth 1 prompt-paths-dir))
     ;; initial:
     (consult-tycho--resolve-initial-grep "rg" "regex" initial))))

;;;###autoload
(defun consult-org-capture ()
  "Select a capture template interactively."
  (interactive)
  ;; TODO remove this hack so that things are loaded in time
  (require 'tychoish-org)
  (let* ((templ (cl-loop for template in org-capture-templates
                         when (> (length template) 2)
                           collect (cons (nth 1 template) (nth 0 template))))
         (_ (message "OPTIONS: %s" templ))
         (_ (message "SOURCE: %s" org-capture-templates))
         (capture-template
          (consult--read templ
                         :prompt "org-capture-templates=>: "
                         :require-match nil
                         :group (consult--type-group templ)
                         :narrow (consult--type-narrow templ)
			 :annotate (lambda (selection) (format " --> [%s]" (cdr (assoc selection templ))))
                         :lookup (lambda (selection candidates &rest _) (cdr (assoc selection candidates)))
                         :category 'org-capture
                         :history '(:input consult-org--capture-history))))
    (unless (string-with-non-whitespace-content-p capture-template)
      (user-error "must select a valid templae %s (%s)" capture-template (type-of capture-template)))
    (org-capture nil capture-template)))

;;;###autoload
(defun consult-org-capture-target ()
  (interactive)
  (org-capture nil "c"))

(add-hook 'org-capture-mode-hook #'consult-org--setup-template)

(provide 'consult-tycho)
;;; consult-tycho.el ends here
