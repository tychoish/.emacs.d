;;  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'subr-x)
  (require 'xtdlib))

(autoload 'org-agenda-files "org")
(autoload 'org-save-all-org-buffers "org")

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
  :defer t)

(use-package ox-rst
  :ensure t
  :defer t)

(defconst tychoish/org-date-spec-datetime "<%Y-%02m-%02d %02H:%02M:%02S %Z>")
(defconst tychoish/org-date-spec-date "<%Y-%02m-%02d>")

(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'turn-on-soft-wrap) ;; from 'tychoish-common
  (add-hook 'org-agenda-mode-hook 'tychoish/background-revbufs-for-hook)
  (add-hook 'org-mode-hook 'tychoish/set-up-buffer-org-mode)

  (add-hook 'org-ctrl-c-ctrl-c-hook 'org-set-weekday-of-timestamp)
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
  (setq org-use-speed-commands #'tychoish/org-use-speed-commands)

  (seq-do (lambda (sym)
            (when (boundp sym)
              (set sym (make-sparse-keymap))))
          '(tychoish/org-mode-personal-map
            tychoish/org-mode-capture-map
            tychoish/org-mode-personal-archive-map))

  (bind-keys
   :map org-mode-map
   ("C-c l o" . org-link-open-from-string)
   ("C-c C-p" . set-mark-command)
   ("M-TAB" . org-cycle)
   ("C-M-TAB" . org-cycle-force-archived)
   ("C-c C-w" . whitespace-cleanup)
   :map org-mode-map
   :prefix "C-c o"
   :prefix-map tychoish/org-mode-personal-map ;; "C-c o"
   ("s" . org-agenda)
   ("a" . consult-org-agenda)
   ("u" . tychoish-org-agenda-untagged-in-file)
   ("h" . consult-org-heading)
   ("k" . org-capture)
   ("f" . org-agenda-files-open)
   ("t" . org-set-tags-command)
   ("n" . org-narrow-to-subtree)
   ("W" . widen)
   ("p" . org-insert-property-drawer)
   ("w" . org-refile)
   ("d" . tychoish-org-date-now)
   ("C-s" . org-save-all-org-buffers)
   ;; ("i" . org-ctags-create-tags)
   ;; ("g" . tychoish/org-gist-map)
   :map tychoish/org-mode-personal-map
   :prefix "c"
   :prefix-map tychoish/org-mode-capture-map
   ("c" . consult-org-capture)
   ("m" . org-capture)
   ("p" . org-capture-goto-last-stored)
   ("l" . org-capture-goto-last-stored)
   ("t" . org-capture-goto-target)
   ("r" . org-capture-refile)
   ("w" . org-capture-refile)
   :map tychoish/org-mode-personal-map
   :prefix "f"
   :prefix-map tychoish/org-mode-personal-archive-map
   ("d" . tychoish-org-mark-done-and-archive)
   ("e" . org-cycle-force-archived)
   ("t" . org-archive-set-tag)
   ("s" . org-archive-to-archive-sibling)
   ("a" . org-archive-done-tasks-to-archive-sibling)
   ("f" . org-archive-done-tasks-to-archive-file)))

(autoload 'org-store-link "ol")
(autoload 'org-insert-link "ol")
(autoload 'org-annotate-file "org-annotate-file")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-mode extensions and supporting packages

(autoload 'toc-org-insert-toc "toc-org")
(autoload 'org-gist-export-to-gist "ox-gist")
(autoload 'org-rst-export-to-rst "ox-rst")
(autoload 'org-rst-export-as-rst "ox-rst")
(autoload 'org-leanpub-book-export-markdown "ox-leanpub-book")

(autoload 'annotated-completing-read "annotated-completing-read")

;; Declared special so the `let'-binding in `tychoish-org-mark-done-and-archive'
;; takes effect dynamically and the byte compiler doesn't warn about an unused
;; lexical variable.
(defvar org-archive-sibling-heading)

(setq org-archive-default-command #'org-archive-to-archive-sibling)

(autoload 'org-leanpub-book-export-markua "ox-leanpub-book")
(autoload 'org-leanpub-markua-export-to-markua "ox-leanpub-markua")
(autoload 'org-leanpub-markua-export-as-markua "ox-leanpub-markua")
(autoload 'org-leanpub-markdown-export-to-markdown "ox-leanpub-markdown")
(autoload 'org-leanpub-markdown-export-as-markdown "ox-leanpub-markdown")

(defvar tychoish-org--auxiliary-packages
  '(org-contrib toc-org ox-gist ox-hugo ox-rst ox-leanpub)
  "Supporting org packages that should be installed when org-mode loads the first time.")

(defun tychoish-org--install-auxiliary-packages ()
  "Install all of the auxiliary packages."
  (thread-last tychoish-org--auxiliary-packages
	       (seq-remove #'package-installed-p)
	       (mapcar #'package-install-async)
	       (length)))

(with-eval-after-load 'ox-rst
  (setq org-rst-headline-underline-characters (list ?= ?- ?~ ?' ?^ ?`)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key bindings

(bind-keys
 :prefix "C-c o"
 :prefix-map tychoish/global-org-map
 ("a" . consult-org-agenda)
 ("c" . consult-org-capture)
 ("4" . org-agenda)
 ("k" . org-capture)
 ("f" . org-agenda-files-open)
 ("s" . org-save-all-org-buffers)
 ("r" . org-agenda-files-reload)
 ("j" . consult-org-capture)
 ("u" . tychoish-org-agenda-untagged-in-file)
 ("/" . tychoish-org-agenda-for-file)
 :map tychoish/global-org-map
 :prefix "l"
 :prefix-map tychoish/org-link-mode-map
 ("s" . org-store-link)
 ("i" . org-insert-link)
 ("a" . org-annotate-file))

(defvar-keymap tychoish/org-gist-map
  :name "org-gist"
  :doc "keymap for org-gist commands"
  "p" #'org-gist-export-private-gist
  "g" #'org-gist-export-public-gist)

(with-eval-after-load 'org-agenda
  (bind-keys
   :map org-agenda-mode-map
   ("C-l" . org-agenda-open-link)
   ("M-c" . org-agenda-goto-calendar)
   ("/" . tychoish-org-agenda-for-file)
   ("C-e" . org-migrate-subtree-to-denote)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions and helpers

(defun tychoish/set-up-buffer-org-mode ()
  (add-hook 'write-contents-functions 'tychoish--add-toc-org-op nil t)
  (setq-local fill-column 80))

;;;###autoload
(defun org-agenda-files-open ()
  "Open all agenda files if not already open."
  (interactive)
  (let* ((files (thread-last (org-agenda-files)
                             (seq-mapcat (lambda (it)
                                           (if (f-directory-p it)
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
(defun org-agenda-files-reload ()
  "Open all agenda files, and reverting to the version on disk as needed."
  (interactive)
  (thread-last (org-agenda-files-open)
               (seq-map (lambda (it)
                          (with-current-buffer it
                            (revert-buffer nil (or current-prefix-arg (not (called-interactively-p 'interactive))) t)
                            (buffer-file-name))))))

(defun tychoish/background-revbufs-for-hook ()
  "Run `revbufs' without disturbing the current window configuration."
  (save-window-excursion (revbufs)))

(defun tychoish--add-toc-org-op ()
  (when (require 'toc-org nil t)
    (save-excursion (toc-org-insert-toc))))

(defun org-gist-export-private-gist ()
  (interactive)
  (org-gist-export-to-gist nil 'open))

(defun org-gist-export-public-gist ()
  (interactive)
  (org-gist-export-to-gist 'public))

(cl-defun tychoish-org-date-now (&key short)
  (interactive)
  (format-time-string (if short
			  tychoish/org-date-spec-date
			tychoish/org-date-spec-datetime) (current-time)))

(defun tychoish-org-mark-done-and-archive ()
  "Mark the current entry done and archive it under the \"Completed\" sibling."
  (interactive)
  (require 'org-archive)
  (org-todo 'done)
  (let ((org-archive-sibling-heading "Completed"))
    (org-archive-to-archive-sibling)))

(defun org-set-weekday-of-timestamp ()
  "Re-normalize the timestamp at point.
Used to add the weekday to a bare numeric date like <2026-05-10>: the
`org-timestamp-change' call with a zero delta rewrites the timestamp in
canonical form, which has the side effect of appending the weekday."
  (interactive)
  (when (org-at-timestamp-p t)
    (org-timestamp-change 0 'year)
    t))

(defun tychoish/org-use-speed-commands ()
  (and (looking-at org-outline-regexp) (looking-back "^\\**" nil)))

(defvar tychoish-org-project-tags '("PROJECT" "EPIC")
  "Tags that suppress their children from all agenda views.
A heading carrying any of these tags acts as a project boundary: its
descendant entries are hidden from agenda while the heading itself stays
visible.  Changes take effect after the next agenda rebuild.")

(defun tychoish-org-skip-child-of-project-tag ()
  "Skip the current entry if any ancestor carries a project grouping tag.
Returns the end-of-subtree position to skip past, or nil to keep the entry.
The tagged ancestor itself is never skipped — only its descendants are.
Intended for `org-agenda-skip-function-global'."
  (save-excursion
    (let (skip)
      (while (and (not skip) (org-up-heading-safe))
        (when (seq-intersection tychoish-org-project-tags
                                (org-get-tags nil t))
          (setq skip (save-excursion (org-end-of-subtree t) (point)))))
      skip)))

(defun tychoish-org-done-state-match ()
  "Return an org-map-entries match string for all completed todo states.
Derives the set from `org-todo-keywords-1' (buffer-local when set) and
falls back to `org-done-keywords' which org populates from the keyword
sequences after the \"|\" separator."
  (let ((done-states (or (and (boundp 'org-done-keywords) org-done-keywords)
                         '("DONE"))))
    (concat "/" (mapconcat #'identity done-states "|"))))

(defun tychoish-org-archive-completed-tasks (archive-fn label)
  "Collect all completed tasks in scope and archive each with ARCHIVE-FN.
Scope is the current subtree when point is inside a heading, else the
full file.  Skips any entry whose tree already carries the :ARCHIVE: tag
\(directly or inherited).  Reports count with LABEL in the echo area."
  (let ((scope (if (org-before-first-heading-p) 'file 'tree))
        markers)
    (org-map-entries
     (fn (push (point-marker) markers))
     (tychoish-org-done-state-match)
     scope
     'archive)
    (let ((count (length markers)))
      (dolist (marker markers)
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (funcall archive-fn)
          (set-marker marker nil)))
      (message "Archived %d completed task(s)%s" count label))))

(defun org-archive-done-tasks-to-archive-sibling ()
  "Archive all completed tasks in scope to the archive sibling heading."
  (interactive)
  (tychoish-org-archive-completed-tasks #'org-archive-to-archive-sibling ""))

(defun org-archive-done-tasks-to-archive-file ()
  "Archive all completed tasks in scope to the org archive file."
  (interactive)
  (tychoish-org-archive-completed-tasks #'org-archive-subtree " to file"))

(declare-function annotated-completing-read "annotated-completing-read")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: org-capture

;;;###autoload
(defun consult-org-capture ()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-agenda: untagged filter

(defvar tychoish-org-agenda-required-tag nil
  "Dynamic binding used by `tychoish-org-skip-unless-untagged'.
When nil: skip entries that have any local tag (show only fully-untagged items).
When set to a tag string: skip entries that possess that tag (show items missing it).")

(defvar tychoish-org-agenda-include-inherited-tags nil
  "Dynamic binding used by `tychoish-org-skip-unless-untagged'.
When nil (default): only local tags are considered — headings that merely
inherit tags from ancestors are treated as untagged.
When non-nil: inherited tags are included — a heading is considered tagged
if it or any ancestor carries a tag.")

(defconst tychoish-org-datetree-heading-re
  (rx bol
      (or (seq (= 4 digit) eol)
          (seq (= 4 digit) "-" (= 2 digit) " " (+ alpha))
          (seq (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) " " (+ alpha))))
  "Regexp matching org datetree auto-generated headings (year, month, day).")

(defun tychoish-org-skip-unless-untagged ()
  "Skip agenda entries that carry tags, match datetree headings, or have
`tychoish-org-agenda-required-tag'.
When `tychoish-org-agenda-required-tag' is nil, keeps only entries with no
tags at all.  When it is a tag string, keeps only entries missing that tag.
Datetree structural headings are always skipped.
Respects `tychoish-org-agenda-include-inherited-tags': when nil, only local
tags are tested; when non-nil, inherited tags are included in the check."
  (let ((tags (if tychoish-org-agenda-include-inherited-tags
                  (org-get-tags)
                (org-get-tags nil t)))
        (heading (org-get-heading t t t t)))
    (when (or (string-match-p tychoish-org-datetree-heading-re heading)
              (if tychoish-org-agenda-required-tag
                  (member tychoish-org-agenda-required-tag tags)
                tags))
      (or (outline-next-heading) (point-max)))))

(defun tychoish-org-agenda-untagged-in-file (file &optional tag todo-only inherited)
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
                           ((org-agenda-skip-function #'tychoish-org-skip-unless-untagged)
                            (org-agenda-overriding-header ,header)))
                  `(tags "LEVEL>=1-TODO={.+}"
                         ((org-agenda-skip-function #'tychoish-org-skip-unless-untagged)
                          (org-agenda-overriding-header ,header)))))
         (tychoish-org-agenda-required-tag tag)
         (tychoish-org-agenda-include-inherited-tags inherited)
         (org-agenda-custom-commands `(("V" ,header (,block)))))
    (org-agenda nil "V")))

(defun tychoish-org-agenda-for-file (file)
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

(defconst tychoish-org-agenda-builtin-views
  '(("a" "Agenda (week/day)")
    ("t" "All TODOs")
    ("m" "Match tags / props / todo")
    ("s" "Search keywords"))
  "Standard org-agenda built-in views included in `consult-org-agenda'.")

;;;###autoload
(defun consult-org-agenda ()
  "Select an org-agenda view via annotated completing read.
Includes both the standard built-in views and any entries in
`org-agenda-custom-commands'.  Each candidate is annotated with its
key and, for custom commands, the match string or filter function
name.  Candidates are grouped by command type (built-in, tags, todo,
etc.)."
  (interactive)
  (require 'org-agenda)
  (let* ((customs (seq-filter (lambda (e) (stringp (cadr e)))
                              org-agenda-custom-commands))
         (all (append tychoish-org-agenda-builtin-views customs))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-agenda

(with-eval-after-load 'org-agenda
  (setq org-agenda-skip-function-global #'tychoish-org-skip-child-of-project-tag)

  (setq org-agenda-custom-commands
        '(("b" "Backlog" tags "+backlog|+inbox-ITEM=\"Inbox\"|TODO=BLOCKED"
           ((org-agenda-skip-function-global nil)))
          ("u" "Untagged TODOs (local)" todo ""
           ((org-agenda-skip-function #'tychoish-org-skip-unless-untagged)
            (org-agenda-overriding-header "TODOs with no local tags")))
          ("U" "Untagged headings (local)" tags "LEVEL>=1-TODO={.+}"
           ((org-agenda-skip-function #'tychoish-org-skip-unless-untagged)
            (org-agenda-overriding-header "Headings with no local tags")))
          ("i" "Untagged TODOs (incl. inherited)" todo ""
           ((org-agenda-skip-function #'tychoish-org-skip-unless-untagged)
            (org-agenda-overriding-header "TODOs with no local or inherited tags")
            (tychoish-org-agenda-include-inherited-tags t)))
          ("I" "Untagged headings (incl. inherited)" tags "LEVEL>=1-TODO={.+}"
           ((org-agenda-skip-function #'tychoish-org-skip-unless-untagged)
            (org-agenda-overriding-header "Headings with no local or inherited tags")
            (tychoish-org-agenda-include-inherited-tags t)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-capture-templates

(defvar org-capture-templates nil)

(defun tychoish-org-reset-capture-templates ()
  (setq org-capture-templates '(("t" "tasks")
                                ("j" "journal")
                                ("n" "notes")
                                ("a" "agent"))))


(defun ~title~ ()
  "Read a title string interactively during `org-capture' template expansion."
  (read-string "title => "))

(cl-defun tychoish/org-capture--add-flat-templates
    (&key kind char name path (key "") target body-fn first-sub default-subs
          (prepend t) (time-prompt-suffix nil))
  "Register a flat set of capture templates of KIND for NAME at PATH.

CHAR is the single-letter kind prefix (\"t\", \"j\", \"n\").  KEY is
the project prefix; an empty string disables the hierarchical
\"<key><suffix>\" and shortcut \"<char><key>\" entries.

TARGET is the org-capture target form.  BODY-FN is called with the
per-template anchor string and returns the template body.

FIRST-SUB is (ANCHOR . DESCRIPTION) for the \"<char><key>\" shortcut,
added only when KEY is non-empty.

DEFAULT-SUBS is a list of (SUFFIX ANCHOR DESCRIPTION); each becomes a
\"<key><suffix>\" template.

PREPEND becomes the templates' :prepend value.  When a key sequence
ends with TIME-PROMPT-SUFFIX, the template is marked :time-prompt t."
  (when (string-equal char key)
    (user-error "cannot define %s %s org-capture-templates with key `%s'" kind name char))

  (let (specs append-item)
    (unless (string-equal "" key)
      (setq append-item t)
      (add-to-list 'org-capture-templates
                   (list (concat key char)
                         (format "%s %s <%s>" name kind (file-name-nondirectory path))))
      (push (list (concat char key) (car first-sub) (cdr first-sub)) specs))
    (dolist (entry (append specs
                           (seq-map (lambda (it) (cons (concat key (car it)) (cdr it)))
                                    default-subs)))
      (let* ((key-sequence (nth 0 entry))
             (template-anchor (nth 1 entry))
             (description (nth 2 entry))
             (template (list key-sequence
                             (format "%s (%s; %s)" kind name description)
                             'entry target
                             (funcall body-fn template-anchor)
                             :prepend prepend
                             :kill-buffer t
                             :empty-lines-after 1)))
        (when (and time-prompt-suffix
                   (string-suffix-p time-prompt-suffix key-sequence))
          (setq template (append template (list :time-prompt t))))
        (add-to-list 'org-capture-templates template append-item)))))

(cl-defun tychoish/org-capture-add-journal-templates (&key name path (key ""))
  (tychoish/org-capture--add-flat-templates
   :kind "journal" :char "j" :name name :path path :key key
   :target (if (string-equal "" key)
               (list 'file+olp+datetree path)
             (list 'file+olp+datetree path "Journal"))
   :body-fn (lambda (anchor)
              (concat "* %(~title~) <%<%Y-%m-%d %H:%M>>" anchor "\n%?"))
   :first-sub (cons "" "<today>")
   :default-subs '(("jj" "" "<today>")
                   ("jp" "" "<date prompt>")
                   ("jx" "%x" "X11 buffer")
                   ("jl" "%a" "org-link")
                   ("jk" "%c" "emacs kill-ring"))
   :prepend nil
   :time-prompt-suffix "jp"))

(cl-defun tychoish/org-capture-add-task-templates (&key name path (key ""))
  (tychoish/org-capture--add-flat-templates
   :kind "tasks" :char "t" :name name :path path :key key
   :target (list 'file+headline path "Tasks")
   :body-fn (lambda (anchor) (concat "* TODO %(~title~)\n" anchor "\n%?"))
   :first-sub (cons "%i" "selection")
   :default-subs '(("tt" "%i" "selection")
                   ("tx" "%x" "X11 buffer")
                   ("tl" "%a" "org-link")
                   ("tk" "%c" "emacs kill-ring"))))

(cl-defun tychoish/org-capture-add-note-templates (&key name path (key ""))
  (tychoish/org-capture--add-flat-templates
   :kind "notes" :char "n" :name name :path path :key key
   :target (list 'file+headline path "Inbox")
   :body-fn (lambda (anchor) (concat "* %(~title~)\n" anchor "\n%?"))
   :first-sub (cons "%i" "selection")
   :default-subs '(("nn" "%i" "selection")
                   ("nx" "%x" "X11 buffer")
                   ("nl" "%a" "org-link")
                   ("nk" "%c" "emacs kill-ring"))))


;; registration helpers

;;;###autoload
(defmacro tychoish-org-add-project-file-capture-templates (&rest args)
  "Register project file capture templates once `org' is loaded.
Expands to a call to `tychoish-org--add-project-file-capture-templates',
forwarding ARGS (the same :name/:path/:key/:agenda keywords), wrapped in
`with-eval-after-load' so the templates take effect the moment `org' loads
--- including when `org' loads because the user just ran `org-capture' ---
without needing a manual, ordered setup step from a per-machine user file.
Also avoids forcing `org' to load during init merely to register templates."
  `(with-eval-after-load 'org
     (tychoish-org--add-project-file-capture-templates ,@args)))

(cl-defun tychoish-org--add-project-file-capture-templates (&key name (path nil) (key "") (agenda nil))
  "Defines a set of capture mode templates for adding notes and tasks to a file.
Called via the `tychoish-org-add-project-file-capture-templates' macro, which
defers invocation until `org' is loaded.  Call this directly only from code
that already runs after `org' is loaded."
  (unless (and (boundp 'org-capture-templates) org-capture-templates)
    (tychoish-org-reset-capture-templates))

  (when (and agenda (not (-contains-p (org-agenda-files) (f-full path))))
    (add-to-list 'org-agenda-files path))
  (when (not (equal "" key))
    (when (string-search "jnt" key)
      (error "org-capture prefix key '%s' for '%s' contains well-known prefix" key path))
    (when (string-search "xlk" key)
      (error "org-capture prefix key '%s' for '%s' contains sub-template key" key path)))

  (unless name
    (setq name "planner"))

  (setq path (if path
		 (expand-file-name path)
	       (concat (f-make-slug name) ".org")))

  (add-to-list 'org-capture-templates (list key (format "%s (project; %s)" name (file-name-nondirectory path))) t)

  (when-let* ((file-dir (file-name-directory path))
	      (_ (not (string-equal file-dir org-directory))))
    (add-to-list 'org-agenda-files path))

  (let ((org-filename (if (or (file-exists-p path)
			      (and
			       (< 1 (length (f-split path)))
			       (file-exists-p (f-dirname path))))
			  path
			(concat org-directory "/" path))))

    (tychoish/org-capture-add-note-templates
     :name name
     :key key
     :path org-filename)

    (tychoish/org-capture-add-journal-templates
     :name name
     :key key
     :path org-filename)

    (tychoish/org-capture-add-task-templates
     :name name
     :key key
     :path org-filename)))

;; org capture templates definitions
(defun tychoish-org--setup-standard-capture-templates ()
  (tychoish/org-capture-add-note-templates
   :name "notes"
   :path "records.org")

  (tychoish/org-capture-add-journal-templates
   :name "diary"
   :path "journal.org")

  (tychoish/org-capture-add-task-templates
   :name "prime"
   :path "planner.org")

  (tychoish/org-capture-add-task-templates
   :name "agent"
   :path "agent.org"
   :key "a")

  (tychoish/org-capture-add-journal-templates
   :name "agent"
   :path "agent.org"
   :key "a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hooks

(add-one-shot-hook
 :name "org-install-aux-packages"
 :hook 'org-mode-hook
 :operation #'tychoish-org--install-auxiliary-packages)

(add-one-shot-hook
 :name "org-capture [install standard templates]"
 :hook 'emacs-startup-hook
 :operation #'tychoish-org--setup-standard-capture-templates
 :idle-timer 1.0)

(defun ad:org-agenda--open-files (&rest _)
  "Pre-load all agenda files before `org-agenda'."
  (org-agenda-files-open))

(advice-add 'org-agenda :before #'ad:org-agenda--open-files)

;;;###autoload
(defun tychoish-org-jump-to-heading ()
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
      (org-show-context 'agenda))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; denote org-capture integration

(declare-function denote-org-capture "denote")
(declare-function denote-last-path "denote")

(with-eval-after-load 'denote
  (add-to-list 'org-capture-templates
               '("dn" "denote note" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

(declare-function denote-org-extract-org-subtree "denote-org")
(declare-function denote-format-link "denote")
(declare-function denote-directory-files "denote")
(declare-function org-agenda-goto "org-agenda")

(defun tychoish-org--parse-heading-date (heading)
  "Return an Emacs time value for the first org timestamp in HEADING, or nil."
  (when (string-match org-ts-regexp-both heading)
    (condition-case nil
        (org-time-string-to-time (match-string 0 heading))
      (error nil))))

(defun org-migrate-subtree-to-denote ()
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
         (heading-date (tychoish-org--parse-heading-date heading)))
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

(declare-function denote-journal-capture-entry-for-date "denote-journal-capture")
(declare-function denote-journal-capture-entry-today "denote-journal-capture")

(with-eval-after-load 'denote-journal-capture
  (add-to-list 'org-capture-templates
               '("dj" "denote journal (today)" entry
                 (file+olp denote-journal-capture-entry-today "Journal")
                 "* %(denote-journal-capture-timestamp) %^{Entry}\n%?"
                 :no-save t
                 :kill-buffer t
                 :jump-to-captured t))
  (add-to-list 'org-capture-templates
               '("dJ" "denote journal (date)" entry
                 (file+olp denote-journal-capture-entry-for-date "Journal")
                 "* %(denote-journal-capture-timestamp) %^{Entry}\n%?"
                 :no-save t
                 :kill-buffer t
                 :jump-to-captured t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; agent-shell-queue org-capture integration

(declare-function agent-shell-queue-capture-from-region "agent-shell-queue")

(with-eval-after-load 'agent-shell-queue
  (add-to-list 'org-capture-templates
               '("q" "agent queue item" plain
                 (function ignore)
                 ""
                 :immediate-finish t
                 :before-finalize (lambda ()
                                    (call-interactively
                                     #'agent-shell-queue-capture-from-context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; refile current org heading to agent-shell-queue

(declare-function agent-shell-queue-org-refile-from-heading "agent-shell-queue-org")

(with-eval-after-load 'agent-shell-queue-org
  (bind-key "C-c o q" #'agent-shell-queue-org-refile-from-heading org-mode-map))

(provide 'tychoish-org)
