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
;; a transient menu (`hud-menu') and a completing-read selector (`hud-select').
;;
;; Commands are grouped by category.  When :category is omitted,
;; `hud--derive-category' infers it from the command's defining file.
;; The ACR selector annotates each candidate with the description and groups
;; candidates by category, so vertico/marginalia display them correctly.

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
                             (&key command description category transient-key display-name
                                   &aux (command      (when command (intern (symbol-name command))))
                                        (category     (or category (hud--derive-category command)))
                                        (display-name (or display-name
                                                          (when command (symbol-name command)))))))
  "A command entry in the HUD registry."
  (command       nil :type symbol :documentation "Interactive command symbol to invoke.")
  (description   nil :type string :documentation "Short label shown in menus and selectors.")
  (category      nil :type symbol :documentation "Grouping key, e.g. \\='agent-shell.
Derived automatically from the command's defining file when not supplied.")
  (transient-key nil :type string :documentation "Key string for the transient menu suffix.
Must be unique across all registered commands; enforced by `hud-register-command'.")
  (display-name  nil :type string :documentation "Candidate string shown in `hud-select'.
Defaults to the command symbol name; set explicitly to show a shorter alias."))

(defun hud--derive-category (command)
  "Derive a category symbol from COMMAND's defining file, or \\='hud.
Uses `symbol-file' to find the file that defines COMMAND, then interns
its base name as the category symbol."
  (or (when-let* ((file (symbol-file command 'defun)))
        (intern (file-name-base file)))
      'hud))

(defun hud--find-transient-key-conflict (category command transient-key)
  "Return the conflicting (CATEGORY . HUD-COMMAND) pair if TRANSIENT-KEY is
already claimed by a different category+command combination, or nil."
  (seq-find (lambda (pair)
              (and (equal transient-key (hud-command-transient-key (cdr pair)))
                   (not (and (eq category (car pair))
                             (eq command (hud-command-command (cdr pair)))))))
            (hud--flat-entries)))

(cl-defun hud-register-command (&key category command description transient-key display-name)
  "Add or replace a `hud-command' entry in `hud-command-table'.

CATEGORY and DISPLAY-NAME default to values derived by the `hud-command'
constructor when not supplied.  If an entry with the same COMMAND already
exists in the resolved category it is replaced in-place, preserving order.
Otherwise the new entry is appended.

Signals `user-error' when TRANSIENT-KEY is already claimed by a different
category+command combination."
  (let* ((entry (make-hud-command :command command
                                  :description description
                                  :category category
                                  :transient-key transient-key
                                  :display-name display-name))
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

;;;###autoload
(transient-define-prefix hud-menu ()
  "Session-wide HUD command menu."
  [:class transient-columns
   :setup-children hud--setup-children])

(defun hud--setup-children (_)
  "Build `hud-menu' columns from `hud-command-table'."
  (transient-parse-suffixes
   'hud-menu
   (seq-map (lambda (bucket)
              (apply #'vector
                     (symbol-name (car bucket))
                     (seq-map (lambda (cmd)
                                (list (hud-command-transient-key cmd)
                                      (hud-command-description cmd)
                                      (hud-command-command cmd)))
                              (cdr bucket))))
            hud-command-table)))

;;;; ACR selector

(defun hud--flat-entries ()
  "Return a flat list of (CATEGORY . HUD-COMMAND) pairs from `hud-command-table'."
  (seq-mapcat (lambda (bucket)
                (seq-map (lambda (cmd) (cons (car bucket) cmd))
                         (cdr bucket)))
              hud-command-table))

(defun hud--entry-lookup ()
  "Return a hash table mapping display names to (CATEGORY . HUD-COMMAND) pairs."
  (map-into
   (seq-map (lambda (pair)
              (cons (hud-command-display-name (cdr pair)) pair))
            (hud--flat-entries))
   '(hash-table :test equal)))

;;;###autoload
(defun hud-select ()
  "Select and invoke a HUD command via `annotated-completing-read'.
Candidates are command symbol names grouped by category; annotations show
the description."
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

;;;; Command registrations

(hud-register-command
 :category 'agent-shell
 :command 'agent-shell-new-shell
 :description "new agent shell"
 :transient-key "an")

(hud-register-command
 :category 'agent-shell
 :command 'agent-shell-queue-buffer-open
 :description "open queue"
 :transient-key "aq")

(hud-register-command
 :command 'magit-dash-open
 :description "repo dashboard"
 :transient-key "rd")

(hud-register-command
 :category 'magit-dash
 :command 'magit-dash-open-repo
 :description "magit open repo"
 :transient-key "rr")

(hud-register-command
 :command 'magit-dash-sync-all
 :description "sync all configured repos"
 :transient-key "rs")

(hud-register-command
 :command 'magit-dash-sync-repo
 :description "sync one configured repo"
 :transient-key "ru")

(hud-register-command
 :category 'org-mode
 :command 'consult-org-capture
 :description "org-capture with template"
 :transient-key "oc")

(hud-register-command
 :category 'org-mode
 :command 'org-agenda
 :description "org-agenda view dispatch"
 :transient-key "oa")

(hud-register-command
 :category 'org-mode
 :command 'tychoish-org-jump-to-heading
 :description "jump to org heading"
 :transient-key "oh")

(hud-register-command
 :category 'mu4e
 :command 'mu4e-compose-mail
 :description "compose new email"
 :transient-key "mc")

(hud-register-command
 :command 'mu4e
 :description "open mu4e dashboard"
 :transient-key "mm")

(hud-register-command
 :category 'mu4e
 :command 'consult-mu-bookmark
 :description "select mu4e bookmark"
 :transient-key "mb")

(hud-register-command
 :category 'mu4e
 :command 'tychoish-mail-select-account
 :display-name "mail-select-account"
 :description "select mail account"
 :transient-key "ma")

(hud-register-command
 :command 'sprite-list
 :description "sprite dashboard"
 :transient-key "sd")

(hud-register-command
 :command 'sprite-create
 :description "spawn sprite"
 :transient-key "ss")

(hud-register-command
 :command 'sprite-open-frame
 :description "open frame in sprite"
 :transient-key "sf")

(hud-register-command
 :category 'arch
 :command 'arch-find-package
 :description "arch package info"
 :transient-key "li")

(hud-register-command
 :category 'arch
 :command 'arch-list
 :description "arch package list"
 :transient-key "ll")

(provide 'hud)
;;; hud.el ends here
