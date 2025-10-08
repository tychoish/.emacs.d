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
(require 'ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: org-capture

(defun consult-org--setup-template ()
  "Add special template to capture to a target selectable via consult. Named for clarity in hooks."

  (unless (and (boundp 'consult-org--capture-template-setup) consult-org--capture-template-setup)
    (setq-local org-capture-templates
		(append '("c" "consult-org-capture: select heading interactively ..." entry (consult-org--capture-target 'agenda)
			  "* TODO %?\n  %i"
			  :prepend t
			  :kill-buffer t
			  :empty-lines-after 1)
			org-capture-templates)))
  (setq-local consult-org--capture-template-setup t))

(defun consult-org--capture-target ()
  "Choose a capture target interactively.
This function returns a value suitable for use as the `target'
entry of `org-capture-templates'."
  (list 'function
        (lambda ()
          (unless (derived-mode-p #'org-mode)
            (user-error "Must call from an Org buffer."))
		 (let ((consult--read-config
			`((,this-command
			   :prompt "org-apture target: "
			   :preview-key "M-."))))
		   (ignore consult--read-config)
		   (set-buffer
		    (save-window-excursion
		      (consult-org-heading nil 'agenda)
		      (current-buffer)))))))

;;;###autoload
(defun consult-org-capture ()
  "Select a capture template interactively."
  (interactive)
  ;; TODO remove this hack so that things are loaded in time
  (require 'tychoish-org)

  (let ((table (ht-create)))
    (->> org-capture-templates
	 (--filter (< 3 (length it)))
	 (--mapc (ht-set table
			(nth 1 it)
			(cons
			 (format "[%s]%s<%s> '%s'"
				 (nth 0 it)
				 (prefix-padding-for-annotation (nth 0 it) 0)
				 (f-filename (cadr (nth 3 it)))
				 (s-trim (s-truncate 32 (string-replace "\n" " " (nth 4 it)))))
			 (nth 0 it)))))
    (let* ((keys (ht-keys table))
	   (longest (length-of-longest-item keys))
	   (capture-template-key
	    (consult--read
	     keys
	     :prompt "org-capture => "
	     :annotate (lambda (candidate) (concat (prefix-padding-for-annotation candidate longest) (car (ht-get table candidate))))
	     :lookup (lambda (selection _candidates &rest _) (cdr (ht-get table selection)))
	     :category 'org-capture
                         :require-match nil
	     :command 'consult-org-capture
             :history '(:input consult-org--capture-history))))

      (org-capture nil capture-template-key))))

;;;###autoload
(defun consult-org-capture-target ()
  (interactive)
  (org-capture nil "c"))

(add-hook 'org-capture-mode-hook #'consult-org--setup-template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: increment-grep

(defun consult-tycho--resolve-initial-grep (prompt prompt-annotation initial &key context)
  "Return the initial text for a query, processing `INITIAL' as needed."
  ;; if the string is empty or only whitespace, it's undefined,
  ;; otherwise use it.
  (or (trimmed-string-or-nil initial)
      ;; with the prefix argument, ask the user
      (when (or current-prefix-arg context)
        (consult-tycho--select-context-for-operation
         (format "%s<init:%s>: " prompt prompt-annotation)))
      ;; otherwise, provide the empty string...
      ""))

(defun consult-tycho--context-base-list (&optional seed)
  (let ((table (ht-create)))

    (->> (or (when (listp seed) seed)
             (when (stringp seed) (list seed)))
         (-keep #'trimmed-string-or-nil)
         (--filter (length> it 64))
         (--mapc (ht-set table it "user provided input (seed)")))

    (->> (-join (->> '(word email url sentence)
                     (--map (cons 'text-mode it)))
                (->> '(symbol word sexp defun)
                     (--map (cons 'prog-mode it))))
         (--keep (when (derived-mode-p (car it))
                   (when-let* ((content (thing-at-point (cdr it)))
                               (content (trimmed-string-or-nil content))
                               (content (substring-no-properties content)))
                     (cons content it))))
         (--filter (< (length (car it)) 64))
         (--mapc (ht-set table (car it) (format "%s at point (%ss)" (cddr it) (cadr it)))))

    (when-let* ((mark-pos (mark))
                (start (or (region-beginning) (min (point) mark-pos)))
                (end (or (region-end) (max mark-pos (point))))
                (selection (trimmed-string-or-nil (buffer-substring-no-properties start end)))
                (is-oversized (< (length selection) 32)))
      (ht-set table selection (format "current selection <%s>" (current-buffer))))

    (when-let* ((line (trimmed-string-or-nil (thing-at-point 'line)))
		(line (substring-no-properties line))
		(is-oversized (< (length line) 32)))
      (ht-set table line (format "current line <%s>" (buffer-name))))

    (let ((count 0))
      (->> kill-ring
           (-map #'substring-no-properties)
           (-keep #'trimmed-string-or-nil)
           (--filter (< (length it) 64))
           (-take 10)
           (--mapc (ht-set table it (format "kill ring [idx=%d]" (cl-incf count))))))

    table))

(defun consult-tycho--select-context-for-operation (&optional prompt seed-list)
  "Pick string to use as context in a follow up operation."
  (let* ((this-command this-command)
         (selections (consult-tycho--context-base-list seed-list))
	 (prompt (or prompt "grep =>>")))

    (if (> (ht-size selections) 1)
	(consult-tycho--read-annotated
	 selections
	 :command this-command
	 :require-match nil
	 :prompt prompt)
      (message "skipping context selection because %d" (ht-size selectiongs))
      (car (ht-keys selections)))))

;;;###autoload
(defun consult-rg (&optional dir initial &key context)
  "Start and iterative rg session. DIR and INITIAL integrate with the consult-grep API."
  (interactive "P")
  ;; `consult--directory-prompt' --> '(prompt paths <default>-dir)
  (let ((consult-async-input-debounce 0.025)
        (consult-async-input-throttle 0.05)
        (consult-async-refresh-delay 0.025)
	(consult-async-min-input 2)
	(consult--prefix-group nil))
    (ignore consult--prefix-group)

    (consult--grep
     ;; prompt
     "rg"
     ;; builder
     #'consult--ripgrep-make-builder
     ;; directory
     (or (trimmed-string-or-nil dir)
	 (consult--select-directory)
	 (approximate-project-root))
     ;; initial
     (if (and (or context (not initial)) (not (eq context 'override)))
	 (consult-tycho--select-context-for-operation (format "rg(init) =>> "))
       initial))))

;;;###autoload
(defun consult-rg-project (&optional initial &key context)
  "Start an iterative rg session in the project root, if possible, falling back as necessary."
  (interactive "P")
  (consult-rg
   (or (approximate-project-root) (consult--select-directory))
   initial
   :context (or context current-prefix-arg 'override)))

;;;###autoload
(defun consult-rg-pwd (&optional initial &key context)
  "Start an iterative rg session for the current directory."
  ;; (let ((base-directory (f-base default-directory)))
  (interactive "P")

  (consult-rg
   (or default-directory (consult--select-directory))
   initial
   :context (or context current-prefix-arg 'override)))

;;;###autoload
(defun consult-rg-pwd-wizard (&optional initial)
  "Start an iterative rg session with context, with prompting to start a query for a collection of likely candidates."
  ;; (let ((base-directory (f-base default-directory)))
  (interactive "P")
  (consult-rg-pwd initial :context t))

;;;###autoload
(defun consult-rg-project-wizard (&optional initial)
  "Start an iterative rg session with context. Always run the search in the project root, falling back if there isn't a discernable root."
  ;; (let ((base-directory (f-base default-directory)))
  (interactive "P")
  (consult-rg-project initial :context t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: mail

;;;###autoload
(defun tychoish-mail-select-account (account-id)
  "Use consult to select an account/mail configuration."

  (interactive
   (list (let ((table (ht-create)))
	   (ht-map (lambda (key account)
		     (ht-set table key (format "%s <%s>%s"
					       (tychoish--mail-account-name account)
					       (tychoish--mail-account-address account)
					       (or (when (equal tychoish/mail-account-current key) " -- CURRENT") ""))))
		   tychoish/mail-accounts)
	   (consult-tycho--read-annotated
	    table
	    :prompt "mail-account => "
	    :require-match nil
	    :command 'tychoish-mail-select-account
	    :category 'consult-mu))))

  (let ((select-account-operation (intern account-id)))
    (funcall select-account-operation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: helpers for working with consult

(cl-defun consult-tycho--read-annotated (table &key (prompt "=> ") require-match category (command this-command))
  (unless (ht-p table)
    (user-error "must specify annotated table as a hashmap of options to annotations"))

  (let ((longest (length-of-longest-item (ht-keys table))))
    (consult--read
     table
     :prompt prompt
     :require-match require-match
     :command command
     :annotate (lambda (candidate) (concat (prefix-padding-for-annotation candidate longest) (ht-get table candidate)))
     :category category)))

(provide 'consult-tycho)
;;; consult-tycho.el ends here
