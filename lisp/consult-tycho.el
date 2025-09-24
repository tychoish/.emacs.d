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

;; consult-tycho: org-capture

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

;;;###autoload
(defun consult-org-capture ()
  "Select a capture template interactively."
  (interactive)
  ;; TODO remove this hack so that things are loaded in time
  (require 'tychoish-org)
  (let* ((templ (cl-loop for template in org-capture-templates
                         when (> (length template) 2)
                           collect (cons (nth 1 template) (nth 0 template))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: increment-grep

(defun consult-tycho--resolve-initial-grep (prompt prompt-annotation initial &key context)
  "Return the initial text for a query, processing `INITIAL' as needed."
  ;; if the string is empty or only whitespace, it's undefined,
  ;; otherwise use it.
  (or (trimmed-string-or-nil initial)
      ;; with the prefix argument, ask the user
      (when current-prefix-arg
        (consult-tycho--select-context-for-operation
         (format "%s<init:%s>: " prompt prompt-annotation)))
      ;; otherwise, provide the empty string...
      ""))

(defun consult-tycho--select-context-for-operation (&optional prompt seed-list)
  "Pick string to use as context in a follow up operation."
  (let ((this-command this-command)
        (selections (consult-tycho--context-base-list seed-list))
	(prompt (or prompt "grep =>>")))
    (or (when (length= selections 1) (nth 0 selections))
        (when (length> selections 1)
          (consult--read selections
           :sort nil
           :command this-command
           :require-match nil
           :prompt prompt)))))

(defun consult-tycho--context-base-list (&optional seed)
  (->> (-join
        (if (listp seed)
            seed
          (list seed))
        (if-let* ((mark-pos (mark))
			(start (or (region-beginning) (min (point) mark-pos)))
			(end (or (region-end) (max mark-pos (point)))))
	    (s-lines (s-trim (buffer-substring start end)))
	  '())
        (-take 10 kill-ring)
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
              (t '()))
        (list (thing-at-point 'line)))
       (-keep #'trimmed-string-or-nil)
       (-distinct)))

(cl-defun consult-tycho--incremental-grep (&key (prompt "=>> ") (builder '()) (initial ""))
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
     :command this-command
     :sort nil
     :group nil ;; #'consult--prefix-group <- this groups results by common prefix (e.g. file)
     :history '(:input consult--grep-history))))

;;;###autoload
(defun consult-rg (&optional dir initial &key context)
  "Start and iterative rg session. DIR and INITIAL integrate with the consult-grep API."
  (interactive "P")
  ;; `consult--directory-prompt' --> '(prompt paths <default>-dir)
  (let* ((prompt-paths-dir (consult--directory-prompt "rg" (or (trimmed-string-or-nil dir)
							       (consult--select-directory)
							       (approximate-project-root))))
         (default-directory (nth 2 prompt-paths-dir))
         (prompt (nth 0 prompt-paths-dir))
         (initial (if (and (or context (not initial)) (not (eq context 'override)))
		      (consult-tycho--select-context-for-operation (format "rg(init) =>> "))
		    initial)))

    (consult-tycho--incremental-grep
     :prompt prompt
     :builder (consult--ripgrep-make-builder (nth 1 prompt-paths-dir))
     :initial initial)))

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

;; consult-tycho: file object processing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: mail

;;;###autoload
(defun tychoish-mail-select-account (account-id)
  "Use consult to select an account/mail configuration."
  (interactive
   (list
    (let* ((accounts (ht-keys tychoish/mail-accounts))
	   (longest-key (length-of-longest-item accounts)))
      (consult--read
       accounts
       :prompt "mail-account => "
       :require-match nil
       :annotate (tychoish/mail-get-account-annotation-function longest-key)))))

  (let ((select-account-operation (intern account-id)))
    (funcall select-account-operation)))

(provide 'consult-tycho)
;;; consult-tycho.el ends here
