;;; orgx.el --- personal org-mode configuration and extensions -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal org-mode setup, split into two sections:
;;
;;   1. Configuration: package declarations, `setq' settings, hooks, and
;;      keybindings (both in upstream maps like `org-mode-map' and in the
;;      `orgx-' prefix maps defined here).
;;   2. Functionality: the commands, helpers, and capture-template machinery
;;      implemented for this configuration.
;;
;; Section 1 refers to symbols defined in section 2; forward `declare-function'
;; declarations near the top keep the byte-compiler quiet.

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'xtdlib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Section 1: Configuration and keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Autoloads and forward declarations

(autoload 'org-agenda-files "org")
(autoload 'org-save-all-org-buffers "org")
(autoload 'org-store-link "ol")
(autoload 'org-insert-link "ol")
(autoload 'org-annotate-file "org-annotate-file")
(autoload 'annotated-completing-read "annotated-completing-read")

(declare-function org-agenda-goto "org-agenda")
(declare-function org-archive-set-tag "org-archive")
(declare-function denote-org-extract-org-subtree "denote-org")
(declare-function denote-format-link "denote")
(declare-function denote-directory-files "denote")
(declare-function denote-directories "denote")
(declare-function denote-retrieve-filename-signature "denote")
(declare-function agent-shell-queue-org-refile-from-heading "agent-shell-queue-org")

;; Declared special so the `let'-binding in `orgx-mark-done-and-archive'
;; takes effect dynamically and the byte compiler doesn't warn about an unused
;; lexical variable.
(defvar org-archive-sibling-heading)

;; `org-capture-templates' is populated by whatever loads capture templates
;; into it (`orgx-capture', `org-capture' itself, etc.); `orgx-capture' below
;; only ever reads it, so this file has no load-time dependency on any of
;; them. Defaults to nil (matching `org-capture''s own default) so reading
;; it here doesn't signal void-variable before anything has populated it.
(defvar org-capture-templates nil)

;; Supporting export/format packages.

(use-package org-contrib
  :ensure t
  :defer t)

(use-package ox-hugo
  :ensure t
  :defer t)

(use-package ox-gfm
  :ensure t
  :defer t)

(use-package ox-gist
  :ensure t
  :commands (org-gist-export-to-gist))

(use-package toc-org
  :ensure t
  :commands (toc-org-insert-toc))

(use-package ox-leanpub
  :ensure t
  :commands (org-leanpub-book-export-markdown
	     org-leanpub-book-export-markua
	     org-leanpub-markua-export-to-markua
	     org-leanpub-markua-export-as-markua
	     org-leanpub-markdown-export-to-markdown
	     org-leanpub-markdown-export-as-markdown))

(use-package ox-rst
  :ensure t
  :defer t
  :commands (org-rst-export-to-rst org-rst-export-as-rst)
  :config
  (setq org-rst-headline-underline-characters (list ?= ?- ?~ ?' ?^ ?`)))

;; org-mode configuration, hooks, and keybindings.

(with-eval-after-load 'org
  (add-hook 'org-ctrl-c-ctrl-c-hook 'orgx-set-weekday-of-timestamp)
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (setq org-modules
	'(org-capture
          org-datetree
          org-annotate-file
          org-depend
          org-habit))

  (org-load-modules-maybe t)

  ;; org-faces
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("INPROGRESS" . "orange")
          ("INCOMPLETE" . "orange")
          ("SCHEDULED" . "green")
          ("BACKLOG" . (:foreground "orange" :weight bold))
          ("PROJECT" . (:foreground "blue" :weight bold))))

  ;; org.el
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d!)")
          (sequence "BLOCKED(s)" "BACKLOG(b)" "INPROGRESS(p)" "|" "SKIPPED" "GONEAWAY(g@)" "INCOMPLETE(i@)")))
  (setq org-tag-alist
        '((:startgroup . nil)
          ("inbox" . ?i)
          ("backlog" . ?b)
          (:endgroup . nil)
          (:startgroup . nil)
          ("@desk" . ?d)
          ("@personal" . ?p)
          ("@work" . ?w)
          (:endgroup . nil)))

  (setq org-tags-column -70)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-fast-tag-selection-include-todo t)
  (setq org-fontify-done-headline t)
  (setq org-footnote-auto-label nil)
  (setq org-footnote-define-inline nil)
  (setq org-footnote-section nil)
  (setq org-log-into-drawer t)
  (setq org-provide-todo-statistics t)
  (setq org-reverse-note-order t)
  (setq org-startup-folded 'content)
  (setq org-startup-indented nil)
  (setq org-tags-exclude-from-inheritance '("project"))
  (setq org-track-ordered-property-with-tag t)
  (setq org-use-fast-tag-selection 'auto)
  (setq org-use-fast-todo-selection 'auto)

  ;; org-refile.el
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
  (setq org-refile-use-outline-path 'file)

  ;; org-keys.el
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  (setq org-use-speed-commands #'orgx--use-speed-commands))

(defvar-keymap orgx-gist-map
  :name "org-gist"
  :doc "keymap for org-gist commands"
  "p" #'org-gist-export-private-gist
  "g" #'org-gist-export-public-gist)

;; org-agenda keybindings and configuration.

(with-eval-after-load 'org-agenda
  (setq org-agenda-skip-function-global #'orgx-skip-child-of-project-tag)

  (setq org-agenda-custom-commands
        '(("b" "Backlog" tags "+backlog|+inbox-ITEM=\"Inbox\"|TODO=BLOCKED"
           ((org-agenda-skip-function-global nil)))
          ("d" "Denote TODOs" todo ""
           ((org-agenda-files (orgx-denote-files))
            (org-agenda-skip-function-global nil)
            (org-agenda-overriding-header "TODO items across the denote tree")
            (org-agenda-prefix-format
             '((todo . " %i %-16(orgx-denote-agenda-category) ")))))
          ("u" "Untagged TODOs (local)" todo ""
           ((org-agenda-skip-function #'orgx-skip-unless-untagged)
            (org-agenda-overriding-header "TODOs with no local tags")))
          ("h" "Untagged headings (local)" tags "LEVEL>=1-TODO={.+}"
           ((org-agenda-skip-function #'orgx-skip-unless-untagged)
            (org-agenda-overriding-header "Headings with no local tags")))
	  ("i" . "including inherited")
          ("iu" "Untagged TODOs (incl. inherited)" todo ""
           ((org-agenda-skip-function #'orgx-skip-unless-untagged)
            (org-agenda-overriding-header "TODOs with no local or inherited tags")
            (orgx-agenda-include-inherited-tags t)))
          ("ih" "Untagged headings (incl. inherited)" tags "LEVEL>=1-TODO={.+}"
           ((org-agenda-skip-function #'orgx-skip-unless-untagged)
            (org-agenda-overriding-header "Headings with no local or inherited tags")
            (orgx-agenda-include-inherited-tags t)))))

  (setq org-agenda-include-diary nil)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-default-appointment-duration 60)
  (setq org-agenda-inhibit-startup nil)
  (setq org-agenda-mouse-1-follows-link t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown nil)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-start-on-weekday nil))

(setq org-archive-default-command #'org-archive-to-archive-sibling)

;; Startup hooks and advice are registered in the `use-package orgx' :init
;; block in `tychoish-core.el' so they can trigger this file's deferred load.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Section 2: Custom functionality
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; date formatting

(defconst orgx-date-spec-datetime "<%Y-%02m-%02d %02H:%02M:%02S %Z>")
(defconst orgx-date-spec-date "<%Y-%02m-%02d>")

(cl-defun orgx-date-now (&key short)
  (interactive)
  (format-time-string (if short
			  orgx-date-spec-date
			orgx-date-spec-datetime) (current-time)))

;; buffer setup helpers

(defun orgx--set-up-buffer ()
  (add-hook 'write-contents-functions 'orgx--add-toc-op nil t)
  (setq-local fill-column 80)
  (turn-on-visual-line-mode))

(defun orgx--add-toc-op ()
  (when (require 'toc-org nil t)
    (save-excursion (toc-org-insert-toc))))

(defun orgx--background-revbufs ()
  "Run `revbufs' without disturbing the current window configuration."
  (save-window-excursion (revbufs)))

(defun orgx--use-speed-commands ()
  (and (looking-at org-outline-regexp) (looking-back "^\\**" nil)))

;; agenda file open/reload

;;;###autoload
(defun orgx-agenda-files-open ()
  "Open all agenda files if not already open."
  (interactive)
  (let* ((files (thread-last (org-agenda-files)
                             (seq-mapcat (lambda (it)
                                           (if (file-directory-p it)
                                               (f-glob "*.org" it)
                                             (list it))))
                             (seq-remove (lambda (it)
                                           (string-suffix-p "archive.org" it)))))
         (buffers (thread-last files
                               (seq-map (lambda (it)
                                          (or (get-file-buffer it)
                                              (find-file-noselect it t)))))))
    (message "opened %d agenda files [%s]" (length files) (string-join files ", "))
    buffers))

;;;###autoload
(defun orgx-agenda-files-reload ()
  "Open all agenda files, and reverting to the version on disk as needed."
  (interactive)
  (thread-last (orgx-agenda-files-open)
               (seq-map (lambda (it)
                          (with-current-buffer it
                            (revert-buffer nil (or current-prefix-arg (not (called-interactively-p 'interactive))) t)
                            (buffer-file-name))))))

(defun ad:org-agenda--open-files (&rest _)
  "Pre-load all agenda files before `org-agenda'."
  (orgx-agenda-files-open))

;; gist export (ox-gist integration)

(defun org-gist-export-private-gist ()
  (interactive)
  (org-gist-export-to-gist nil 'open))

(defun org-gist-export-public-gist ()
  (interactive)
  (org-gist-export-to-gist 'public))

;; timestamps and archiving

(defun orgx-mark-done-and-archive ()
  "Mark the current entry done and archive it under the \"Completed\" sibling."
  (interactive)
  (require 'org-archive)
  (org-todo 'done)
  (let ((org-archive-sibling-heading "Completed"))
    (org-archive-to-archive-sibling)))

(defun orgx-set-weekday-of-timestamp ()
  "Re-normalize the timestamp at point.
Used to add the weekday to a bare numeric date like <2026-05-10>: the
`org-timestamp-change' call with a zero delta rewrites the timestamp in
canonical form, which has the side effect of appending the weekday."
  (interactive)
  (when (org-at-timestamp-p t)
    (org-timestamp-change 0 'year)
    t))

(defvar orgx-project-tags '("PROJECT" "EPIC")
  "Tags that suppress their children from all agenda views.
A heading carrying any of these tags acts as a project boundary: its
descendant entries are hidden from agenda while the heading itself stays
visible.  Changes take effect after the next agenda rebuild.")

(defun orgx-skip-child-of-project-tag ()
  "Skip the current entry if any ancestor carries a project grouping tag.
Returns the end-of-subtree position to skip past, or nil to keep the entry.
The tagged ancestor itself is never skipped — only its descendants are.
Intended for `org-agenda-skip-function-global'."
  (save-excursion
    (let (skip)
      (while (and (not skip) (org-up-heading-safe))
        (when (seq-intersection orgx-project-tags
                                (org-get-tags nil t))
          (setq skip (save-excursion (org-end-of-subtree t) (point)))))
      skip)))

(defun orgx-done-state-match ()
  "Return an org-map-entries match string for all completed todo states.
Derives the set from `org-todo-keywords-1' (buffer-local when set) and
falls back to `org-done-keywords' which org populates from the keyword
sequences after the \"|\" separator."
  (let ((done-states (or (and (boundp 'org-done-keywords) org-done-keywords)
                         '("DONE"))))
    (concat "/" (mapconcat #'identity done-states "|"))))

(defun orgx-archive-completed-tasks (archive-fn label)
  "Collect all completed tasks in scope and archive each with ARCHIVE-FN.
Scope is the current subtree when point is inside a heading, else the
full file.  Skips any entry whose tree already carries the :ARCHIVE: tag
\(directly or inherited).  Reports count with LABEL in the echo area."
  (let ((scope (if (org-before-first-heading-p) 'file 'tree))
        markers)
    (org-map-entries
     (fn (push (point-marker) markers))
     (orgx-done-state-match)
     scope
     'archive)
    (let ((count (length markers)))
      (dolist (marker markers)
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (funcall archive-fn)
          (set-marker marker nil)))
      (message "Archived %d completed task(s)%s" count label))))

(defun orgx-archive-done-tasks-to-archive-sibling ()
  "Archive all completed tasks in scope to the archive sibling heading."
  (interactive)
  (orgx-archive-completed-tasks #'org-archive-to-archive-sibling ""))

(defun orgx-archive-done-tasks-to-archive-file ()
  "Archive all completed tasks in scope to the org archive file."
  (interactive)
  (orgx-archive-completed-tasks #'org-archive-subtree " to file"))

;;;###autoload
(defun orgx-capture ()
  "Select a capture template interactively."
  (interactive)
  (let* ((key-table (make-hash-table :test #'equal))
         (annotation-table (make-hash-table :test #'equal))
         (group-table (make-hash-table :test #'equal))
         (prefix-map (map-into
                      (seq-map (lambda (it) (cons (nth 0 it) (nth 1 it)))
                               (seq-filter (lambda (it) (<= (length it) 4))
                                           org-capture-templates))
                      '(hash-table :test equal))))
    (seq-do
     (lambda (template)
       (let* ((key-char (nth 0 template))
              (description (nth 1 template))
              (target-loc (cadr (nth 3 template)))
              (target-file (if (stringp target-loc) (file-name-nondirectory target-loc) ""))
              (content (nth 4 template))
              (raw (if (stringp content) (string-replace "\n" " " content) ""))
              (preview (if (> (length raw) 32) (concat (substring raw 0 29) "...") raw))
              (group (or (map-elt prefix-map (substring key-char 0 1)) "root capture (other)")))
         (setf (map-elt key-table description) key-char)
         (setf (map-elt annotation-table description)
               (format "[%s] <%s> '%s'" key-char target-file preview))
         (setf (map-elt group-table description) group)))
     (seq-filter (lambda (it) (< 4 (length it))) org-capture-templates))
    (org-capture nil (map-elt key-table
                              (annotated-completing-read
                               annotation-table
                               :prompt "org-capture => "
                               :category 'org-capture
                               :require-match nil
                               :group-name (lambda (candidate)
                                             (map-elt group-table candidate "")))))))

;; org-agenda: untagged filter

(defvar orgx-agenda-required-tag nil
  "Dynamic binding used by `orgx-skip-unless-untagged'.
When nil: skip entries that have any local tag (show only fully-untagged items).
When set to a tag string: skip entries that possess that tag (show items missing it).")

(defvar orgx-agenda-include-inherited-tags nil
  "Dynamic binding used by `orgx-skip-unless-untagged'.
When nil (default): only local tags are considered — headings that merely
inherit tags from ancestors are treated as untagged.
When non-nil: inherited tags are included — a heading is considered tagged
if it or any ancestor carries a tag.")

(defconst orgx-datetree-heading-re
  (rx bol
      (or (seq (= 4 digit) eol)
          (seq (= 4 digit) "-" (= 2 digit) " " (+ alpha))
          (seq (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) " " (+ alpha))))
  "Regexp matching org datetree auto-generated headings (year, month, day).")

(defun orgx-skip-unless-untagged ()
  "Skip agenda entries that carry tags, match datetree headings, or have
`orgx-agenda-required-tag'.
When `orgx-agenda-required-tag' is nil, keeps only entries with no
tags at all.  When it is a tag string, keeps only entries missing that tag.
Datetree structural headings are always skipped.
Respects `orgx-agenda-include-inherited-tags': when nil, only local
tags are tested; when non-nil, inherited tags are included in the check."
  (let ((tags (if orgx-agenda-include-inherited-tags
                  (org-get-tags)
                (org-get-tags nil t)))
        (heading (org-get-heading t t t t)))
    (when (or (string-match-p orgx-datetree-heading-re heading)
              (if orgx-agenda-required-tag
                  (member orgx-agenda-required-tag tags)
                tags))
      (or (outline-next-heading) (point-max)))))

(defun orgx-agenda-untagged-in-file (file &optional tag todo-only inherited)
  "Show an agenda for FILE restricted to items lacking TAG.
When TAG is nil or empty, show items with no tags at all.
When TODO-ONLY is non-nil, restrict to TODO-keyword headings.
When INHERITED is non-nil, headings that inherit tags from ancestors are
also considered tagged and excluded.
Interactively, prompts for file and tag; \\[universal-argument] toggles
TODO-only, \\[universal-argument] \\[universal-argument] adds inherited-tag checking."
  (interactive
   (list (read-file-name "Org file: " nil nil t nil
                         (lambda (n) (or (file-directory-p n)
                                         (string-suffix-p ".org" n))))
         (let ((input (completing-read
                       "Missing tag (empty = no tags at all): "
                       (org-global-tags-completion-table (org-agenda-files))
                       nil nil nil nil "")))
           (unless (string-empty-p input) input))
         (equal current-prefix-arg '(4))
         (equal current-prefix-arg '(16))))
  (let* ((tag (if (and tag (string-empty-p tag)) nil tag))
         (scope (cond ((and todo-only inherited) "TODOs (inherited)")
                      (todo-only "TODOs")
                      (inherited "Headings (inherited)")
                      (t "Headings")))
         (header (format "%s in %s %s"
                         scope
                         (file-name-nondirectory file)
                         (if tag (format "missing :%s:" tag) "with no tags")))
         (block (if todo-only
                    `(todo ""
                           ((org-agenda-skip-function #'orgx-skip-unless-untagged)
                            (org-agenda-overriding-header ,header)))
                  `(tags "LEVEL>=1-TODO={.+}"
                         ((org-agenda-skip-function #'orgx-skip-unless-untagged)
                          (org-agenda-overriding-header ,header)))))
         (orgx-agenda-required-tag tag)
         (orgx-agenda-include-inherited-tags inherited)
         (org-agenda-custom-commands `(("V" ,header (,block)))))
    (org-agenda nil "V")))

(defun orgx-agenda-for-file (file)
  "Run a full org agenda restricted to FILE.
FILE is selected from `org-agenda-files' with completion."
  (interactive
   (list (completing-read "Agenda for file: "
                          (mapcar #'file-name-nondirectory (org-agenda-files))
                          nil t)))
  (let ((org-agenda-files
         (seq-filter (lambda (f) (string= (file-name-nondirectory f) file))
                     (org-agenda-files))))
    (org-agenda nil "a")))

;; denote agenda integration

(defun orgx-denote-files ()
  "Return every .org file across all directories in `denote-directories'.
Computed fresh on each call so newly added or renamed notes, and any
change to `denote-directory', are always picked up — do not cache the
result."
  (thread-last (denote-directories)
               (seq-mapcat (lambda (dir) (directory-files-recursively dir "\\.org\\'")))))

(defconst orgx-denote-agenda-category-width 16
  "Max width, in characters, of `orgx-denote-agenda-category'.")

(defun orgx-denote-agenda-category ()
  "Sequence-number label for the denote agenda.
Denote filenames encode the identifier, signature, and keywords and are
much too long for the agenda's category column, so show only the file's
Folgezettel sequence (its `denote-sequence' signature, e.g. \"3d2b\") —
no title or file name attached. Empty when the file has no sequence.
Always truncated to `orgx-denote-agenda-category-width' characters."
  (let* ((file (buffer-file-name))
         (seq (or (and file (denote-retrieve-filename-signature file)) "")))
    (truncate-string-to-width
     seq orgx-denote-agenda-category-width nil nil "…")))

;;;###autoload
(defun orgx-agenda-denote-todos ()
  "Show all TODO-keyword items across every org file in the denote tree.
Convenience entry point for the \"D\" custom agenda command, which scans
`orgx-denote-files' (recursively, including denote/journal/ and
any other subdirectories) rather than the usual `org-agenda-files'."
  (interactive)
  (org-agenda nil "D"))

(defconst orgx-agenda-builtin-views
  '(("a" "Agenda (week/day)")
    ("t" "All TODOs")
    ("m" "Match tags / props / todo")
    ("s" "Search keywords"))
  "Standard org-agenda built-in views included in `orgx-agenda-view'.")

;;;###autoload
(defun orgx-agenda-view ()
  "Select an org-agenda view via annotated completing read.
Includes both the standard built-in views and any entries in
`org-agenda-custom-commands'.  Each candidate is annotated with its
key and, for custom commands, the match string or filter function
name.  Candidates are grouped by command type (built-in, tags, todo,
etc.)."
  (interactive)
  (require 'org-agenda)
  (let* ((customs (seq-filter (lambda (e)
				(and (proper-list-p e)
				     (stringp (cadr e))))
                              org-agenda-custom-commands))
         (all (append orgx-agenda-builtin-views customs))
         (desc->entry (seq-map (lambda (e) (cons (cadr e) e)) all))
         (acr-table
          (seq-map
           (lambda (e)
             (let* ((match (when (>= (length e) 4) (nth 3 e)))
                    (match-label
                     (cond
                      ((and (symbolp match) (functionp match)) (symbol-name match))
                      ((and (stringp match) (not (string-empty-p match))) match)))
                    (annotation (string-join
                                 (seq-filter #'identity
                                             (list (format "[%s]" (car e))
                                                   match-label))
                                 "  ")))
               (cons (cadr e) annotation)))
           all))
         (choice
          (annotated-completing-read
           acr-table
           :prompt "Agenda view: "
           :require-match t
           :category 'org-agenda
           :group-name (lambda (desc)
                         (when-let* ((entry (cdr (assoc desc desc->entry))))
                           (if (< (length entry) 3)
                               "Built-in"
                             (capitalize (format "%s" (nth 2 entry)))))))))
    (when-let* ((entry (cdr (assoc choice desc->entry))))
      (org-agenda nil (car entry)))))

;; auxiliary package installation

(defvar orgx--auxiliary-packages
  '(org-contrib toc-org ox-gist ox-hugo ox-rst ox-leanpub)
  "Supporting org packages that should be installed when org-mode loads the first time.")

(defun orgx--install-auxiliary-packages ()
  "Install all of the auxiliary packages."
  (thread-last orgx--auxiliary-packages
	       (seq-remove #'package-installed-p)
	       (mapcar #'package-install-async)
	       (length)))

;; heading navigation

;;;###autoload
(defun orgx-jump-to-heading ()
  "Jump to any org heading across all agenda files via `org-refile-targets'."
  (interactive)
  (let* ((targets (org-refile-get-targets))
         (choice (annotated-completing-read
                  (seq-map (lambda (target)
                             (cons (car target)
                                   (format "%s:%d" (nth 1 target) (nth 3 target))))
                           targets)
                  :prompt "heading:"
                  :require-match t)))
    (when-let* ((target (seq-find (lambda (tgt) (equal (car tgt) choice)) targets)))
      (find-file (nth 1 target))
      (goto-char (nth 3 target))
      (org-fold-show-context 'agenda))))

;; denote subtree migration

(defun orgx--parse-heading-date (heading)
  "Return an Emacs time value for the first org timestamp in HEADING, or nil."
  (when (string-match org-ts-regexp-both heading)
    (condition-case nil
        (org-time-string-to-time (match-string 0 heading))
      (error nil))))

;;;###autoload
(defun orgx-migrate-subtree-to-denote ()
  "Extract the current Org subtree to a new denote note, replacing the heading with a link.
Works from an `org-agenda-mode' buffer or an `org-mode' buffer: in the
former, first jumps to the underlying entry via `org-agenda-goto'.

The original heading is re-inserted at its level with the heading text
replaced by a denote link to the new note.

If the heading text contains an org timestamp and the entry has no DATE,
CREATED, or CLOSED property, the timestamp is injected as CREATED so the
new note's identifier reflects that date."
  (interactive)
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-goto))
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be in an org-mode or org-agenda-mode buffer"))
  (let* ((source-buf (current-buffer))
         (insert-marker (copy-marker (org-entry-beginning-position)))
         (level (org-current-level))
         (heading (org-get-heading t t t t))
         (heading-date (orgx--parse-heading-date heading)))
    (when (and heading-date
               (not (or (org-entry-get nil "DATE")
                        (org-entry-get nil "CREATED")
                        (org-entry-get nil "CLOSED"))))
      (org-set-property "CREATED"
                        (format-time-string "[%Y-%m-%d %a %H:%M]" heading-date)))
    (when-let* ((path (denote-org-extract-org-subtree)))
      (with-current-buffer source-buf
        (save-excursion
          (goto-char insert-marker)
          (insert (concat (make-string level ?*)
                          " "
                          (denote-format-link path heading 'org nil)
                          "\n"))
          (goto-char insert-marker)
          (org-toggle-tag "denoted" 'on)))
      (set-marker insert-marker nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Section 3: Minor modes
;;;
;;; C-c o audit (personal map vs global map)
;;;
;;; Personal (orgx-minor-mode-commands-map, active in org buffers via minor mode):
;;;   s=org-agenda  a=orgx-agenda-view  u=orgx-agenda-untagged-in-file
;;;   h=consult-org-heading  k=org-capture  f=orgx-agenda-files-open
;;;   r=orgx-agenda-files-reload  /=orgx-agenda-for-file
;;;   t=org-set-tags-command  n=org-narrow-to-subtree
;;;   p=org-insert-property-drawer  w=org-refile  d=orgx-date-now
;;;   q=agent-shell-queue-org-refile-from-heading (set lazily on package load)
;;;   C-s=org-save-all-org-buffers
;;;   c → orgx-minor-mode-capture-map (submap)
;;;   C-f → orgx-minor-mode-archive-map (submap; f was archive before — moved to
;;;          C-f to unblock f=orgx-agenda-files-open which was previously shadowed)
;;;
;;; Global (orgx-global-map, active everywhere):
;;;   a=orgx-agenda-view  c=orgx-capture  4=org-agenda
;;;   k=org-capture  f=orgx-agenda-files-open  s=org-save-all-org-buffers
;;;   r=orgx-agenda-files-reload  j=orgx-capture
;;;   u=orgx-agenda-untagged-in-file  /=orgx-agenda-for-file
;;;   l → orgx-link-map (submap)
;;;
;;; Cross-map precedence: minor-mode map shadows global in org buffers.
;;; Keys only in global (unreachable via minor-mode, acceptable):
;;;   4 (org-agenda direct), j (duplicate orgx-capture alias)
;;;   c in global = orgx-capture; c in personal = orgx-minor-mode-capture-map
;;;     → in org buffers C-c o c opens capture submap; use C-c o c c for capture
;;;
;;;  W (widen) dropped: violates no-capitals rule; use C-x n w instead.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; orgx-minor-mode

(defvar-keymap orgx-minor-mode-capture-map
  :name "orgx-capture"
  :doc "Capture commands under C-c o c (orgx-minor-mode)."
  "c" #'orgx-capture
  "m" #'org-capture
  "p" #'org-capture-goto-last-stored
  "l" #'org-capture-goto-last-stored
  "t" #'org-capture-goto-target
  "r" #'org-capture-refile
  "w" #'org-capture-refile)

(defvar-keymap orgx-minor-mode-archive-map
  :name "orgx-archive"
  :doc "Archive commands under C-c o C-f (orgx-minor-mode)."
  "d" #'orgx-mark-done-and-archive
  "e" #'org-cycle-force-archived
  "t" #'org-archive-set-tag
  "s" #'org-archive-to-archive-sibling
  "a" #'orgx-archive-done-tasks-to-archive-sibling
  "f" #'orgx-archive-done-tasks-to-archive-file)

(defvar-keymap orgx-minor-mode-commands-map
  :name "orgx-personal"
  :doc "C-c o prefix in org-mode buffers (orgx-minor-mode)."
  "s"   #'org-agenda
  "a"   #'orgx-agenda-view
  "u"   #'orgx-agenda-untagged-in-file
  "h"   #'consult-org-heading
  "k"   #'org-capture
  "f"   #'orgx-agenda-files-open
  "r"   #'orgx-agenda-files-reload
  "/"   #'orgx-agenda-for-file
  "t"   #'org-set-tags-command
  "n"   #'org-narrow-to-subtree
  "p"   #'org-insert-property-drawer
  "w"   #'org-refile
  "d"   #'orgx-date-now
  "C-s" #'org-save-all-org-buffers
  "c"   orgx-minor-mode-capture-map
  "C-f" orgx-minor-mode-archive-map)

(defvar-keymap orgx-minor-mode-map
  :doc "Keymap for `orgx-minor-mode'."
  "C-c l o" #'org-link-open-from-string
  "C-c C-p" #'set-mark-command
  "M-TAB"   #'org-cycle
  "C-M-TAB" #'org-cycle-force-archived
  "C-c C-w" #'whitespace-cleanup
  "C-c o"   orgx-minor-mode-commands-map)

;; Capture template integrations for external packages.

(with-eval-after-load 'agent-shell-queue-org
  (bind-keys
   :map orgx-minor-mode-commands-map
   ("q" . agent-shell-queue-org-refile-from-heading)))

(define-minor-mode orgx-minor-mode
  "Personal org-mode keybindings and buffer setup.
Activates `orgx-minor-mode-commands-map' under C-c o, sets fill-column, and wires
the toc-org write hook."
  :lighter " ox"
  :keymap orgx-minor-mode-map
  (when orgx-minor-mode
    (orgx--set-up-buffer)))

(defun orgx-minor-mode-turn-on ()
  "Enable `orgx-minor-mode' in the current buffer."
  (orgx-minor-mode 1))

;; Hooked onto `org-mode-hook' from the `use-package orgx' :init block in
;; `tychoish-core.el' so the hook is live before this file loads.

;;; orgx-agenda-minor-mode

(defvar-keymap orgx-agenda-minor-mode-map
  :doc "Keymap for `orgx-agenda-minor-mode'."
  "C-l" #'org-agenda-open-link
  "M-c" #'org-agenda-goto-calendar
  "/"   #'orgx-agenda-for-file
  "C-e" #'orgx-migrate-subtree-to-denote)

(define-minor-mode orgx-agenda-minor-mode
  "Personal org-agenda keybindings and setup."
  :lighter " oxa"
  :keymap orgx-agenda-minor-mode-map
  (when orgx-agenda-minor-mode
    (orgx--background-revbufs)))

(defun orgx-agenda-minor-mode-turn-on ()
  "Enable `orgx-agenda-minor-mode' in the current buffer."
  (orgx-agenda-minor-mode 1))

;; Hooked onto `org-agenda-mode-hook' from the `use-package orgx' :init
;; block in `tychoish-core.el' so the hook is live before this file loads.

(provide 'orgx)
;;; orgx.el ends here
