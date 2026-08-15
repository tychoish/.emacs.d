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
  (require 'org-macs)
  (require 'org-element))

(require 'subr-x)
(require 'org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Section 1: Configuration and keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Autoloads and forward declarations

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(autoload 'org-agenda-files "org")
(autoload 'org-save-all-org-buffers "org")
(autoload 'org-store-link "ol")
(autoload 'org-insert-link "ol")
(autoload 'org-annotate-file "org-annotate-file")
(autoload 'annotated-completing-read "annotated-completing-read")
(autoload 'org-id-get-create "org-id")

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

;; `org-todo' dynamically binds these around `org-after-todo-state-change-hook';
;; declared here so `orgx-enforce-question-answered' can read them without a
;; free-variable warning.
(defvar org-state)
(defvar org-last-state)

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
  (add-hook 'org-after-todo-state-change-hook 'orgx-enforce-question-answered)

  (setq org-modules
	'(org-capture
          org-datetree
          org-annotate-file
          org-depend
          org-habit))

  (org-load-modules-maybe t)

  ;; org-faces
  (setq org-todo-keyword-faces
        '(("TODO" . warning)
          ("INPROGRESS" . warning)
          ("INCOMPLETE" . warning)
          ("SCHEDULED" . identifier)
	  ("QUESTION" . warning)
	  ("BACKLOG" . (:foreground warning :weight bold))
	  ("DONE" . (:foreground "slate gray"))
          ("PROJECT" . (:foreground "slate gray" :weight bold))
          ("ANSWERED" . (:foreground "slate gray" :weight bold))))

  ;; ;; org.el
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d!)")
          (sequence "QUESTION(q)" "|" "ANSWERED(a@)")
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
          (:endgroup . nil)
          ("question" . ?q)))

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
  :doc "keymap for org-gist commands")

(keymap-set orgx-gist-map "p" #'org-gist-export-private-gist)
(keymap-set orgx-gist-map "g" #'org-gist-export-public-gist)

;; org-agenda keybindings and configuration.

(defun orgx--denote-agenda-settings (header)
  "Return shared `org-agenda-custom-commands' SETTINGS for a denote view.
Scopes the view to `orgx-denote-files', sorts by denote signature via
`orgx-agenda-cmp-denote-signature', and shows only the file's sequence
number (via `orgx-denote-agenda-category') in place of the full denote
filename — for both the \"todo\" and \"tags\" agenda line types, since
custom commands built from `todo' and `tags-todo' render through
different `org-agenda-prefix-format' keys. HEADER overrides the agenda
header text, or leaves the default when nil."
  `((org-agenda-files (orgx-denote-files))
    (org-agenda-skip-function-global nil)
    (org-agenda-overriding-header ,header)
    (org-agenda-sorting-strategy '(user-defined-up))
    (org-agenda-cmp-user-defined #'orgx-agenda-cmp-denote-signature)
    (org-agenda-prefix-format '((todo . " %i %-8(orgx-denote-agenda-category) ")
                                (tags . " %i %-8(orgx-denote-agenda-category) ")))))


(with-eval-after-load 'org-agenda
  (setq org-agenda-skip-function-global #'orgx-skip-child-of-project-tag)
  (setq org-agenda-sticky t)

  (setq org-agenda-custom-commands
        `(("b" "Backlog" tags "+backlog|+inbox-ITEM=\"Inbox\"|TODO=BLOCKED"
           ((org-agenda-skip-function-global nil)))
          ("u" "Untagged TODOs (local)" todo ""
           ((org-agenda-skip-function #'orgx-skip-unless-untagged)
            (org-agenda-overriding-header "TODOs with no local tags")))
          ("h" "Untagged headings (local)" tags "LEVEL>=1-TODO={.+}"
           ((org-agenda-skip-function #'orgx-skip-unless-untagged)
            (org-agenda-overriding-header "Headings with no local tags")))
	  ("d" . "denote database files")
          ("da" "Denote Agenda" todo ""
	   ,(orgx--denote-agenda-settings "Denote Agenda: All"))
          ("dt" "Denote Agenda Tasks (without agent or questions)"
	   ((tags-todo "-agent-question"
                       ((org-agenda-overriding-header "Denote Agenda"))))
           ,(orgx--denote-agenda-settings "Denote Agenda: Tasks"))
          ("dn" "Denote Agenda (without agent)"
	   ((tags-todo "-agent"
                       ((org-agenda-overriding-header "Denote (non) Agent Tasks"))))
           ,(orgx--denote-agenda-settings "Denote Agenda: Non-agent tasks"))
          ("dg" "Denote Agenda (agent only)"
	   ((tags-todo "agent"
                       ((org-agenda-overriding-header "Denote Agent Tasks"))))
           ,(orgx--denote-agenda-settings "Denote Agenda: Agent tasks"))
          ("dq" "Human Questions"
           ((tags "+question|TODO=\"QUESTION\""
                  ((org-agenda-skip-function #'orgx-skip-unless-open-question)
                   (org-agenda-overriding-header "Human Questions"))))
           ,(orgx--denote-agenda-settings nil))
          ("dc" "Denote Composite Agenda"
           ((tags "+question|TODO=\"QUESTION\""
                  ((org-agenda-skip-function #'orgx-skip-unless-open-question)
                   (org-agenda-overriding-header "Human questions")))
            (tags-todo "-agent-question"
                       ((org-agenda-overriding-header "Tasks")))
            (tags-todo "agent"
                       ((org-agenda-overriding-header "Agent tasks"))))
           ,(orgx--denote-agenda-settings nil))
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

(defun bootstrap-set-notes-directory (&optional path)
  (when path
    (setq local-notes-directory (expand-file-name path)))

  (unless local-notes-directory
    (error "must have defined the `local-notes-directory'"))

  (setq org-directory (file-name-concat local-notes-directory "org"))
  (setq org-agenda-files (thread-last (list org-directory user-org-directories)
                                      (flatten-tree)
                                      (seq-map #'expand-file-name)
			              (seq-filter 'identity)
			              (seq-map #'string-trim)
			              (seq-remove #'string-empty-p)
                                      (seq-uniq)))
  (setq org-annotate-file-storage-file (file-name-concat org-directory "records.org"))
  (setq org-default-notes-file (file-name-concat org-directory "records.org"))
  (setq org-archive-location (file-name-concat org-directory "archive/%s::datetree/"))
  (setq deft-directory (file-name-concat local-notes-directory "deft"))
  (setq denote-directory (file-name-concat local-notes-directory "denote"))
  local-notes-directory)

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
;;;###autoload
(defun ad:org-agenda-redo (orig-fun &rest args)
  "Handle `org-agenda-redo' gracefully on unpopulated or empty agenda buffers."
  (let ((p (or (and (looking-at "'") (1- (point))) (point))))
    (if (and (zerop (buffer-size))
             (null (get-text-property p 'org-redo-cmd))
             (null (get-text-property p 'org-series-redo-cmd)))
        (if (fboundp 'orgx-agenda-view)
            (orgx-agenda-view)
          (user-error "Agenda buffer is unpopulated; select an agenda view first"))
      (apply orig-fun args))))

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
  "Select a capture template interactively.
Candidates are grouped primarily by target file. Templates with no file
target (e.g. function-based integration targets) fall back to their
key-prefix's root description when even that is unknown."
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
              (group (cond
                      ((not (string-empty-p target-file)) target-file)
                      ((map-elt prefix-map (substring key-char 0 1)))
                      (t "Other"))))
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
(defun orgx-skip-unless-open-question ()
  "Skip headings in agenda views that are not open human questions.
Returns point after subtree to skip when the heading is not an open question
\(e.g. answered/done items, metadata subheadings like Context/Response, or child headings inheriting :question:)."
  (let* ((hl (org-get-heading t t t t))
         (todo (org-get-todo-state))
         (local-tags (org-get-tags nil t))
         (is-answered (or (equal todo "ANSWERED")
                          (equal todo "DONE")
                          (and hl (string-prefix-p "ANSWERED " hl))))
         (is-metadata (member hl '("Context" "Response" "LOGBOOK" "PROPERTIES")))
         (is-question (or (member "question" local-tags)
                          (equal todo "QUESTION")
                          (and hl (string-prefix-p "QUESTION " hl)))))
    (if (or is-answered is-metadata (not is-question))
        (or (outline-next-heading) (point-max))
      nil)))

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
         (org-agenda-custom-commands
          `(("V" ,header (,block)
             ((org-agenda-buffer-name
               ,(format "*Org Agenda(%s:%s)*"
                        (file-name-nondirectory file) (or tag "untagged"))))))))
    (org-agenda nil "V")))

(defun orgx-agenda-for-file (file)
  "Run a combined day/week agenda and TODO list restricted to FILE.
If an org file is open, defaults to using this file. Otherwise, or with
a prefix argument, prompts for users to select from open agenda files."
  (interactive
   (list (or (and (not current-prefix-arg)
                  (buffer-file-name)
                  (car (member (expand-file-name (buffer-file-name)) (org-agenda-files))))
             (annotated-completing-read
              (seq-map (lambda (f)
                         (cons f (format "%-8s %s"
                                         (file-size-human-readable
                                          (or (file-attribute-size (file-attributes f)) 0))
                                         (abbreviate-file-name (file-name-directory f)))))
                       (org-agenda-files))
              :prompt "Agenda for file: "
              :category 'org-agenda
              :require-match t))))
  (let* ((org-agenda-files (list file))
         (org-agenda-custom-commands
          `(("V" "Agenda for file"
             ((agenda "")
              (alltodo "" ((org-agenda-overriding-header
                            ,(format "Tasks in %s" (file-name-nondirectory file))))))
             ((org-agenda-buffer-name
               ,(format "*Org Agenda(%s)*" (file-name-nondirectory file))))))))
    (org-agenda nil "V")))

(defun orgx-agenda-switch-buffer ()
  "Switch to a live sticky `*Org Agenda(...)*' buffer instead of re-running it.
Lists every buffer whose name matches that pattern via
`annotated-completing-read', annotated with whether it is currently
visible in a window, and switches to the chosen one."
  (interactive)
  (let ((buffers (seq-filter (lambda (buf) (string-match-p "\\`\\*Org Agenda" (buffer-name buf)))
                              (buffer-list))))
    (unless buffers
      (user-error "No sticky agenda buffers are open"))
    (switch-to-buffer
     (annotated-completing-read
      (seq-map (lambda (buf)
                 (cons (buffer-name buf)
                       (if (get-buffer-window buf) "visible" "not visible")))
                buffers)
      :prompt "Switch to agenda buffer: "
      :category 'buffer
      :require-match t))))

;; denote agenda integration

(defun orgx-denote-files ()
  "Return every .org file across all directories in `denote-directories'.
Computed fresh on each call so newly added or renamed notes, and any
change to `denote-directory', are always picked up — do not cache the
result."
  (thread-last
    (denote-directories)
    (seq-mapcat (lambda (dir) (directory-files-recursively dir "\\.org\\'")))))

(defconst orgx-denote-agenda-category-width 8
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

(defun orgx-agenda-cmp-denote-signature (a b)
  "Order agenda lines A and B by the denote signature of their source file.
Used as `org-agenda-cmp-user-defined' for the \"Denote Files Agenda\"
custom command so entries group by Folgezettel sequence (i.e. file name)
rather than by keyword, deadline, or file order."
  (let* ((file-a (buffer-file-name (marker-buffer (get-text-property 0 'org-marker a))))
         (file-b (buffer-file-name (marker-buffer (get-text-property 0 'org-marker b))))
         (sig-a (or (denote-retrieve-filename-signature file-a) ""))
         (sig-b (or (denote-retrieve-filename-signature file-b) "")))
    (cond ((string< sig-a sig-b) -1)
          ((string< sig-b sig-a) 1))))

;;;###autoload
(defun orgx-agenda-denote-todos ()
  "Show all TODO-keyword items (except human questions) across the denote tree.
Convenience entry point for the \"dt\" custom agenda command, which scans
`orgx-denote-files' (recursively, including denote/journal/ and
any other subdirectories) rather than the usual `org-agenda-files',
ordered by denote signature. Items tagged :question: are implementation
TODOs' human counterpart and surface in the \"dq\" agenda view instead."
  (interactive)
  (org-agenda nil "dt"))

;;;###autoload
(defun orgx-agenda-denote-questions ()
  "Show all human questions across the denote tree.
Convenience entry point for the \"dq\" custom agenda command, which scans
`orgx-denote-files' for open items tagged :question: or with the
QUESTION TODO keyword, ordered by denote signature."
  (interactive)
  (org-agenda nil "dq"))

;;;###autoload
(defun orgx-insert-question (question &optional context)
  "Insert a QUESTION-keyword heading tagged :question: for QUESTION.
Creates a new heading at point via `org-insert-heading-respect-content',
tags it :question: so it surfaces in the \"Human Questions\" agenda view
distinct from implementation TODOs, and stamps it with an `org-id' so the
entry can be linked to directly. Adds a \"Response\" child heading to hold
the answer as a first-class subtree rather than an inline field, and —
only when CONTEXT is non-empty — a sibling \"Context\" child heading
first, for background or reference material. Resolve the question by
marking the heading ANSWERED, which — like GONEAWAY — prompts for a
closing note to record the answer."
  (interactive "sQuestion: \nsContext (leave blank to skip): ")
  (org-insert-heading-respect-content)
  (insert (concat "QUESTION " question))
  (org-toggle-tag "question" 'on)
  (org-id-get-create)
  (let ((stars (make-string (1+ (org-current-level)) ?*)))
    (unless (or (null context) (string-empty-p context))
      (org-end-of-subtree)
      (insert (concat "\n" stars " Context\n" context)))
    (org-end-of-subtree)
    (insert (concat "\n" stars " Response\n"))))

(defun orgx-enforce-question-answered ()
  "Block closing a :question:-tagged heading with any DONE state but ANSWERED.
Runs on `org-after-todo-state-change-hook'; a question is only actually
answered once the Response slot is filled in, so GONEAWAY, INCOMPLETE,
and the other done keywords would let it fall out of the \"Human
Questions\" agenda view unanswered. Reverts the state change and signals
`user-error' instead."
  (when (and (member org-state org-done-keywords)
             (not (string= org-state "ANSWERED"))
             (member "question" (org-get-tags)))
    (org-todo org-last-state)
    (user-error "Headings tagged :question: can only be closed with ANSWERED, not %s" org-state)))

(defconst orgx-agenda-builtin-views
  '(("a" "Agenda (week/day)")
    ("t" "All TODOs")
    ("m" "Match tags / props / todo")
    ("s" "Search keywords"))
  "Standard org-agenda built-in views included in `orgx-agenda-view'.")

(defun orgx--agenda-view-candidate (entry width)
  "Candidate label for ENTRY, its dispatch key left-justified to WIDTH."
  (concat (string-pad (car entry) width) ": " (cadr entry)))

(defun orgx--agenda-view-settings (entry)
  "Return the general SETTINGS alist for a custom-command ENTRY, or nil.
Handles both a simple command, (key desc type match settings), and a
composite series of blocks, (key desc (cmd1 cmd2 ...) settings) — the
latter has a list, not a symbol, in the type slot."
  (when (>= (length entry) 3)
    (if (listp (nth 2 entry))
        (when (>= (length entry) 4) (nth 3 entry))
      (when (>= (length entry) 5) (nth 4 entry)))))

(defun orgx--agenda-view-annotation (entry)
  "Detailed annotation for ENTRY: command type, match, header, and file scope."
  (if (< (length entry) 3)
      "built-in"
    (let* ((type (nth 2 entry))
           (match (when (>= (length entry) 4) (nth 3 entry)))
           (match-label
            (cond
             ((and (symbolp match) (functionp match)) (symbol-name match))
             ((and (stringp match) (not (string-empty-p match))) match)))
           (settings (orgx--agenda-view-settings entry))
           (header (cadr (assq 'org-agenda-overriding-header settings)))
           (files-p (assq 'org-agenda-files settings)))
      (string-join
       (seq-filter #'identity
                   (list (unless (listp type) (symbol-name type))
                         match-label
                         (when files-p "denote files")
                         (when (and header (not (equal header (cadr entry))))
                           header)))
       "  "))))

(defun orgx--agenda-view-group (entry)
  "Group label for ENTRY: built-in, denote-scoped, or global by command type.
File scope (whether the view is restricted to `orgx-denote-files') is a
more useful split than raw command type here, since almost every custom
command in this config is `tags-todo' — grouping by type alone would
dump nearly everything into one bucket. Global commands still fall back
to a per-type label so that bucket doesn't become one dumping ground of
its own."
  (cond
   ((< (length entry) 3) "Built-in")
   ((assq 'org-agenda-files (orgx--agenda-view-settings entry)) "Denote")
   (t (format "Global: %s" (capitalize (format "%s" (nth 2 entry)))))))

;;;###autoload
(defun orgx-agenda-view ()
  "Select an org-agenda view via annotated completing read.
Includes both the standard built-in views and any entries in
`org-agenda-custom-commands'.  Each candidate is labeled with its
dispatch key (e.g. \"da: Denote Agenda ALL\") and annotated with its
command type, match string or filter function, overriding header, and
file scope.  Candidates are grouped by file scope — Built-in, Denote,
or Global (further split by command type) — via
`orgx--agenda-view-group'."
  (interactive)
  (require 'org-agenda)
  (let* ((customs (seq-filter (lambda (e)
				(and (proper-list-p e)
				     (stringp (cadr e))))
                              org-agenda-custom-commands))
         (all (append orgx-agenda-builtin-views customs))
         (key-width (apply #'max (seq-map (lambda (e) (length (car e))) all)))
         (acr-table (seq-map (lambda (e)
                                (cons (orgx--agenda-view-candidate e key-width)
                                      (cons (orgx--agenda-view-annotation e) e)))
                              all))
         (entry (annotated-completing-read
                 acr-table
                 :prompt "Agenda view: "
                 :require-match t
                 :category 'org-agenda
                 :group-name (lambda (candidate)
                               (when-let* ((e (cddr (assoc candidate acr-table))))
                                 (orgx--agenda-view-group e))))))
    (org-agenda nil (car entry))))

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

(defalias 'denote-org-refile-to-denote 'orgx-migrate-subtree-to-denote)
(defalias 'orgx-refile-to-denote 'orgx-migrate-subtree-to-denote)
(defalias 'org-refile-to-denote 'orgx-migrate-subtree-to-denote)

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
;;;   s=org-save-all-org-buffers  a=orgx-agenda-view  u=orgx-agenda-untagged-in-file
;;;   h=consult-org-heading  k=org-capture  o=orgx-agenda-files-open
;;;   r=orgx-agenda-files-reload  /=orgx-agenda-for-file
;;;   t=org-set-tags-command  n=org-narrow-to-subtree
;;;   p=org-insert-property-drawer  w=org-refile  d=orgx-date-now
;;;   q=agent-shell-queue-org-refile-from-heading (set lazily on package load)
;;;   c → orgx-minor-mode-capture-map (submap)
;;;   f → orgx-minor-mode-archive-map (submap)
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
;;;   f in global = orgx-agenda-files-open; f in personal = orgx-minor-mode-archive-map
;;;   c in global = orgx-capture; c in personal = orgx-minor-mode-capture-map
;;;     → in org buffers C-c o c opens capture submap; use C-c o c c for capture
;;;
;;;  W (widen) dropped: violates no-capitals rule; use C-x n w instead.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; orgx-minor-mode

(defvar-keymap orgx-minor-mode-capture-map
  :name "orgx-capture"
  :doc "Capture commands under C-c o c (orgx-minor-mode).")

(keymap-set orgx-minor-mode-capture-map "c" #'orgx-capture)
(keymap-set orgx-minor-mode-capture-map "m" #'org-capture)
(keymap-set orgx-minor-mode-capture-map "p" #'org-capture-goto-last-stored)
(keymap-set orgx-minor-mode-capture-map "l" #'org-capture-goto-last-stored)
(keymap-set orgx-minor-mode-capture-map "t" #'org-capture-goto-target)
(keymap-set orgx-minor-mode-capture-map "r" #'org-capture-refile)
(keymap-set orgx-minor-mode-capture-map "w" #'org-capture-refile)

(defvar-keymap orgx-minor-mode-archive-map
  :name "orgx-archive"
  :doc "Archive commands under C-c o C-f (orgx-minor-mode).")

(keymap-set orgx-minor-mode-archive-map "e" #'org-cycle-force-archived)
(keymap-set orgx-minor-mode-archive-map "t" #'org-archive-set-tag)
(keymap-set orgx-minor-mode-archive-map "s" (cons "current→sibling" #'org-archive-to-archive-sibling))
(keymap-set orgx-minor-mode-archive-map "d" (cons "done+archive" #'orgx-mark-done-and-archive))
(keymap-set orgx-minor-mode-archive-map "a" (cons "done→sibling" #'orgx-archive-done-tasks-to-archive-sibling))
(keymap-set orgx-minor-mode-archive-map "f" (cons "done→file" #'orgx-archive-done-tasks-to-archive-file))

(defvar-keymap orgx-minor-mode-commands-map
  :name "orgx-personal"
  :doc "C-c o prefix in org-mode buffers (orgx-minor-mode).")

(keymap-set orgx-minor-mode-commands-map "4" #'org-agenda)
(keymap-set orgx-minor-mode-commands-map "a" #'orgx-agenda-view)
(keymap-set orgx-minor-mode-commands-map "h" #'consult-org-heading)
(keymap-set orgx-minor-mode-commands-map "k" #'org-capture)
(keymap-set orgx-minor-mode-commands-map "o" #'orgx-agenda-files-open)
(keymap-set orgx-minor-mode-commands-map "r" (cons "reload-agenda" #'orgx-agenda-files-reload))
(keymap-set orgx-minor-mode-commands-map "/" (cons "agenda-for-file" #'orgx-agenda-for-file))
(keymap-set orgx-minor-mode-commands-map "t" #'org-set-tags-command)
(keymap-set orgx-minor-mode-commands-map "n" (cons "narrow-to-subtree" #'org-narrow-to-subtree))
(keymap-set orgx-minor-mode-commands-map "p" (cons "insert-proprty-drawer" #'org-insert-property-drawer))
(keymap-set orgx-minor-mode-commands-map "w" #'org-refile)
(keymap-set orgx-minor-mode-commands-map "d" #'orgx-date-now)
(keymap-set orgx-minor-mode-commands-map "s" #'org-save-all-org-buffers)

(keymap-set orgx-minor-mode-commands-map "u" (cons "untaged-in-file" #'orgx-agenda-untagged-in-file))
(keymap-set orgx-minor-mode-commands-map "c" (cons "capture" orgx-minor-mode-capture-map))
(keymap-set orgx-minor-mode-commands-map "f" (cons "archive" orgx-minor-mode-archive-map))

(defvar-keymap orgx-minor-mode-map
  :doc "Keymap for `orgx-minor-mode'.")

(keymap-set orgx-minor-mode-map "C-c l o" #'org-link-open-from-string)
(keymap-set orgx-minor-mode-map "C-c C-p" #'set-mark-command)
(keymap-set orgx-minor-mode-map "M-TAB" #'org-cycle)
(keymap-set orgx-minor-mode-map "C-M-TAB" #'org-cycle-force-archived)
(keymap-set orgx-minor-mode-map "C-c C-w" #'whitespace-cleanup)
(keymap-set orgx-minor-mode-map "S-<up>" #'org-priority-up)
(keymap-set orgx-minor-mode-map "S-<down>" #'org-priority-down)

(keymap-set orgx-minor-mode-map "C-c o" (cons "orgx-commands" orgx-minor-mode-commands-map))

;; Capture template integrations for external packages.

(with-eval-after-load 'agent-shell-queue-org
  (keymap-set orgx-minor-mode-commands-map "q" #'agent-shell-queue-org-refile-from-heading))

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
  :doc "Keymap for `orgx-agenda-minor-mode'.")

(keymap-set orgx-agenda-minor-mode-map "C-l" #'org-agenda-open-link)
(keymap-set orgx-agenda-minor-mode-map "M-c" #'org-agenda-goto-calendar)
(keymap-set orgx-agenda-minor-mode-map "/" #'orgx-agenda-for-file)
(keymap-set orgx-agenda-minor-mode-map "C-e" #'orgx-migrate-subtree-to-denote)
(keymap-set orgx-agenda-minor-mode-map "C-b" #'orgx-agenda-switch-buffer)
(keymap-set orgx-agenda-minor-mode-map "M-a" #'orgx-agenda-view)

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
