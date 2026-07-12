;;; hud.el --- Session wide command interface -*- lexical-binding: t; -*-

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (transient "0.4") (annotated-completing-read "0.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; A registry-driven command menu for session-wide operations.
;;
;; Register commands with `hud-register-command'; they are exposed through
;; a transient menu (`hud-dispatch') and a completing-read selector (`hud-select').
;;
;; Commands are grouped by category.  When :category is omitted,
;; `hud--derive-category' infers it from the command's defining file.
;; The annotated-completing-read selector annotates each candidate
;; with the description and groups candidates by category, so
;; vertico/marginalia display them correctly.
;;
;; The menu is split into two rows controlled by the :row keyword on each
;; registration (default 1).  All commands in a category should use the
;; same :row value.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)

(require 'annotated-completing-read)

;;;; Registry

(defvar hud-command-table '()
  "Alist mapping category symbols to lists of `hud-command' structs.
Populated by `hud-register-command'.  Reset with `hud-reset-command-table'.")

(cl-defstruct (hud-command
               (:copier nil)
               (:constructor make-hud-command
                             (&key command description category transient-key display-name row visible inapt exclude-from-transient
                                   &aux (command      (when command (intern (symbol-name command))))
                                        (category     (or category (hud--derive-category command)))
                                        (display-name (or display-name
                                                          (when command (symbol-name command))))
                                        (row          (or row 1)))))
  "A command entry in the HUD registry."
  (command       nil :type symbol  :documentation "Interactive command symbol to invoke.")
  (description   nil :type string  :documentation "Short label shown in menus and selectors.")
  (category      nil :type symbol  :documentation "Grouping key, e.g. \\='agent-shell.")
  (transient-key nil :type string  :documentation "Key string for the transient menu suffix.")
  (display-name  nil :type string  :documentation "Candidate string shown in `hud-select'.")
  (row           1   :type integer :documentation "Row (1 or 2) in the two-row transient layout.")
  (visible       nil               :documentation "Predicate (:if): nil means always shown in ACR and transient.")
  (inapt         nil               :documentation "Predicate (:ifapt): non-nil means grayed out in transient.")
  (exclude-from-transient  nil               :documentation "When non-nil, item appears in ACR but not in the transient menu."))

(defun hud--derive-category (command)
  "Derive a category symbol from COMMAND's defining file, or \\='hud."
  (or (when-let* ((file (symbol-file command 'defun)))
        (intern (file-name-base file)))
      'hud))

(defun hud--find-transient-key-conflict (category command transient-key)
  "Return conflicting (CATEGORY . HUD-COMMAND) pair if TRANSIENT-KEY is already
claimed by a different category+command combination, or nil."
  (seq-find (lambda (pair)
              (and (equal transient-key (hud-command-transient-key (cdr pair)))
                   (not (and (eq category (car pair))
                             (eq command (hud-command-command (cdr pair)))))))
            (hud--flat-entries)))

(cl-defun hud-register-command (&key category command description transient-key display-name (row 1) if ifapt exclude-from-transient)
  "Add or replace a `hud-command' entry in `hud-command-table'.

:ROW (integer, default 1) assigns the command to a row in the two-row
transient layout.  All commands in a category should share the same :row.

:IF is a predicate function; when it returns nil the item is hidden from
both the ACR selector and the transient menu.

:IFAPT is a predicate function; when it returns non-nil the transient
suffix is shown but marked inapt (grayed out, not invokable).

:EXCLUDE-FROM-TRANSIENT when non-nil the item appears in `hud-select'
but is omitted from the `hud-dispatch' transient menu entirely.

Signals `user-error' when TRANSIENT-KEY is already claimed by a different
category+command combination."
  (let* ((entry (make-hud-command :command command
                                  :description description
                                  :category category
                                  :transient-key transient-key
                                  :display-name display-name
                                  :row row
                                  :visible if
                                  :inapt ifapt
                                  :exclude-from-transient exclude-from-transient))
         (cat (hud-command-category entry)))
    (when-let* ((conflict (hud--find-transient-key-conflict cat command transient-key)))
      (user-error "Transient key %S is already used by %S in category %S"
                  transient-key
                  (hud-command-command (cdr conflict))
                  (car conflict)))
    (let ((bucket (assq cat hud-command-table)))
      (cond
       ((null bucket)
        (setq hud-command-table
              (append hud-command-table (list (cons cat (list entry))))))
       ((seq-find (lambda (cmd) (eq command (hud-command-command cmd))) (cdr bucket))
        (setcdr bucket (seq-map (lambda (cmd)
                                  (if (eq command (hud-command-command cmd)) entry cmd))
                                (cdr bucket))))
       (t
        (setcdr bucket (append (cdr bucket) (list entry))))))))

(defun hud-reset-command-table ()
  "Clear `hud-command-table', removing all registered commands."
  (interactive)
  (setq hud-command-table '()))

;;;; Transient menu

(defun hud--bucket-to-column (bucket)
  "Convert a BUCKET (category . commands) to a transient column vector."
  (apply #'vector
         (symbol-name (car bucket))
         (thread-last (cdr bucket)
           (seq-remove #'hud-command-exclude-from-transient)
           (seq-map (lambda (cmd)
                      (append (list (hud-command-transient-key cmd)
                                    (hud-command-description cmd)
                                    (hud-command-command cmd))
                              (when-let* ((pred (hud-command-visible cmd)))
                                (list :if pred))
                              (when-let* ((pred (hud-command-inapt cmd)))
                                (list :inapt-if pred))))))))

(defun hud--setup-row-children (row)
  "Build transient column suffixes for categories whose first command is in ROW."
  (transient-parse-suffixes
   'hud-dispatch
   (seq-map #'hud--bucket-to-column
            (seq-filter (lambda (bucket)
                          (= row (hud-command-row (car (cdr bucket)))))
                        hud-command-table))))

;;;###autoload
(transient-define-prefix hud-dispatch ()
  "Session-wide HUD command menu."
  [:class transient-columns
   :setup-children (lambda (_) (hud--setup-row-children 1))]
  [:class transient-columns
   :setup-children (lambda (_) (hud--setup-row-children 2))])

;;;; annotated-completing-read selector

(defun hud--flat-entries ()
  "Return a flat list of (CATEGORY . HUD-COMMAND) pairs from `hud-command-table'."
  (seq-mapcat 
   (lambda (bucket)
     (seq-map (lambda (cmd) (cons (car bucket) cmd))
              (cdr bucket)))
   hud-command-table))

(defun hud--entry-lookup ()
  "Return a hash table mapping display names to visible (CATEGORY . HUD-COMMAND) pairs.
Entries whose :if predicate returns nil are excluded."
  (map-into
   (thread-last (hud--flat-entries)
     (seq-filter (lambda (pair)
                   (let ((pred (hud-command-visible (cdr pair))))
                     (or (null pred) (funcall pred)))))
     (seq-map (lambda (pair)
                (cons (hud-command-display-name (cdr pair)) pair))))
   '(hash-table :test equal)))

;;;###autoload
(defun hud-select ()
  "Select and invoke a HUD command via `annotated-completing-read'."
  (interactive)
  (let* ((lookup (hud--entry-lookup))
         (label  (annotated-completing-read
                  (map-apply (lambda (name pair)
                               (cons name (hud-command-description (cdr pair))))
                             lookup)
                  :prompt "hud:"
                  :require-match t
                  :category 'hud-select
                  :group-name (lambda (candidate)
                                (when-let* ((pair (map-elt lookup candidate)))
                                  (symbol-name (car pair)))))))
    (when-let* ((pair (and label (map-elt lookup label))))
      (call-interactively (hud-command-command (cdr pair))))))

;;;; Command registrations — Row 1

(hud-reset-command-table)

(hud-register-command
 :category 'agent-shell
 :command #'agent-shell-new-shell
 :description "new agent shell"
 :transient-key "an")

(hud-register-command
 :category 'agent-shell
 :command #'agent-shell-menu-new-shell-in-dir
 :description "new agent shell (dir)"
 :transient-key "ad")

(hud-register-command
 :category 'agent-shell
 :command #'agent-shell-switch-buffer
 :transient-key "ab"
 :description "switch agent shell buffer")

(hud-register-command
 :category 'agent-shell
 :command #'agent-shell-queue-buffer-open
 :description "open queue"
 :transient-key "aq")

(hud-register-command
 :category 'org-mode
 :command #'consult-org-capture
 :description "org-capture with template"
 :transient-key "oc")

(hud-register-command
 :category 'org-mode
 :command #'org-agenda
 :description "org-agenda view dispatch"
 :transient-key "oa")

(hud-register-command
 :category 'org-mode
 :command #'tychoish-org-jump-to-heading
 :description "jump to org heading"
 :transient-key "oh")

(hud-register-command
 :category 'mu4e
 :command #'mu4e-compose-mail
 :description "compose new email"
 :transient-key "mc")

(hud-register-command
 :category 'mu4e
 :command #'mu4e
 :description "open mu4e dashboard"
 :transient-key "mm")

(hud-register-command
 :category 'mu4e
 :description "select mu4e bookmark"
 :command #'consult-mu-bookmark
 :transient-key "mb")

(hud-register-command
 :category 'mu4e
 :command #'tychoish-mail-select-account
 :display-name "mail-select-account"
 :description "select mail account"
 :transient-key "ma")

(hud-register-command
 :category 'denote
 :command #'denote-dash
 :description "denote list"
 :transient-key "dv")

(hud-register-command
 :category 'denote
 :command #'denote-dash-dispatch
 :description "denote dispatch"
 :transient-key "dd")

(hud-register-command
 :category 'denote
 :command #'denote
 :description "new note"
 :transient-key "dn")

(hud-register-command
 :category 'denote
 :command #'denote-journal-new-entry
 :description "new journal entry"
 :transient-key "dj")

(hud-register-command
 :category 'denote
 :command #'denote-sequence-view-hierarchy
 :description "denote sequence hierarchy"
 :transient-key "ds")

;;;; Command registrations — Row 2

(hud-register-command
 :row 2
 :command #'magit-dash-open
 :description "repo dashboard"
 :transient-key "rd")

(hud-register-command
 :row 2
 :category 'magit-dash
 :command #'magit-dash-open-repo
 :description "magit open repo"
 :transient-key "rr")

(hud-register-command
 :row 2
 :command #'magit-dash-sync-all
 :description "sync all dashboard repos"
 :transient-key "rs")

(hud-register-command
 :row 2
 :command #'magit-dash-sync-repo
 :description "sync one dashboard repo"
 :transient-key "ru")

(hud-register-command
 :row 2
 :category 'sprite
 :command #'sprite-list
 :description "sprite dashboard"
 :transient-key "sd")

(hud-register-command
 :row 2
 :category 'sprite
 :command #'sprite-create
 :description "spawn sprite"
 :transient-key "ss")

(hud-register-command
 :row 2
 :category 'sprite
 :command #'sprite-open-frame
 :description "open frame in sprite"
 :transient-key "sf")

(hud-register-command
 :row 2
 :category 'arch
 :command 'arch-find-package
 :description "arch package info"
 :transient-key "li")

(hud-register-command
 :row 2
 :category 'arch
 :command 'arch-list
 :description "arch package list"
 :transient-key "ll")

(provide 'hud)
;;; hud.el ends here
