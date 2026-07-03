;;  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'subr-x)
  (require 'xtdlib))

(autoload 'org-agenda-files "org")
(autoload 'org-save-all-org-buffers "org")

(use-package org-contrib
  :ensure t
  :defer t)

(defconst tychoish/org-date-spec-datetime "<%Y-%02m-%02d %02H:%02M:%02S %Z>")
(defconst tychoish/org-date-spec-date "<%Y-%02m-%02d>")

(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'turn-on-soft-wrap) ;; from 'tychoish-common
  (defun tychoish--org-enable-vfc-heading-truncation ()
    "Enable heading truncation mode when visual-fill-column is active."
    (when (bound-and-true-p visual-fill-column-mode)
      (tychoish-vfc-heading-truncation-mode 1)))
  (add-hook 'org-mode-hook #'tychoish--org-enable-vfc-heading-truncation)
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
   ("a" . org-agenda)
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
   ("f" . org-archive-done-tasks-to-archive-file))

  (with-eval-after-load 'consult
    (bind-keys
     :map tychoish/org-mode-personal-map
     ("h" . consult-org-heading)         ;; Alternative: consult-org-heading (for jump)
     ("s" . consult-org-agenda))))

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

(with-eval-after-load 'ox-leanpub
  (org-leanpub-book-setup-menu-markua))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key bindings

(bind-keys
 :map tychoish/global-org-map
 ("f" . org-agenda-files-open)
 ("r" . org-agenda-files-reload)
 ("j" . consult-org-capture)
 ("c" . consult-org-capture)
 :map tychoish/org-link-mode-map
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
   ("M-c" . org-agenda-goto-calendar)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: org-capture

;;;###autoload
(defun consult-org-capture ()
  "Select a capture template interactively."
  (interactive)
  (let ((key-table (make-hash-table :test #'equal))
	(annotation-table (make-hash-table :test #'equal)))
    (seq-do
     (lambda (template)
       (let* ((key-char (nth 0 template))
        (description (nth 1 template))
        (target-loc (cadr (nth 3 template)))
        (target-file (if (stringp target-loc) (file-name-nondirectory target-loc) ""))
        (content (nth 4 template))
        (raw (if (stringp content) (string-replace "\n" " " content) ""))
        (preview (if (> (length raw) 32) (concat (substring raw 0 29) "...") raw)))
   (setf (map-elt key-table description) key-char)
   (setf (map-elt annotation-table description)
         (format "[%s] <%s> '%s'" key-char target-file preview))))
     (seq-filter (lambda (it) (< 4 (length it))) org-capture-templates))
    (org-capture nil (map-elt key-table
            (annotated-completing-read
             annotation-table
             :prompt "org-capture => "
             :category 'org-capture
             :require-match nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-agenda

(with-eval-after-load 'org-agenda
  (setq org-agenda-skip-function-global #'tychoish-org-skip-child-of-project-tag)

  (setq org-agenda-custom-commands
        '(("b" "Backlog" tags "+backlog|+inbox-ITEM=\"Inbox\"|TODO=BLOCKED"
           ((org-agenda-skip-function-global nil)))))

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
                                ("r" "routines")
                                ("a" "agent"))))

;;;###autoload
(cl-defun tychoish/org-capture-add-routine-templates
    (&key name (key "") (path (concat (f-make-slug name) ".org")))

  (when (string-equal "r" key)
    (user-error "cannot define routine (loops) %s org-capture-templates with key `r'" name))

  (when-let* ((description (format "%s routines <%s>" name (file-name-nondirectory path)))
	      (_ (not (string-equal "" key))))
    (add-to-list 'org-capture-templates (list (concat "r" key) description))
    (add-to-list 'org-capture-templates (list (concat key "r") description)))

  (thread-last '(("1d" .  "Daily")
                 ("1w" .  "Weekly")
                 ("4w" . "Monthly")
                 ("12w" . "Quarterly")
                 ("26w" . "Half Yearly")
                 ("52w" . "Yearly"))
               (seq-mapcat
                (lambda (it)
                  (let* ((interval (car it))
                         (heading (cdr it))
                         (interval-key (downcase (substring-no-properties heading 0 1))))
                    (thread-last (list (concat key "r" interval-key)
                                       (concat "r" key interval-key))
                                 (seq-map (lambda (prefix)
                                            (cons prefix (cons interval heading))))))))
               (seq-do
                (lambda (it)
                  (let* ((prefix (car it))
                         (interval (cadr it))
                         (heading (cddr it))
                         (lower-heading (downcase heading))
                         (menu-name (format "routine (%s; %s)" name lower-heading)))
                    (add-to-list
                     'org-capture-templates
                     (list prefix menu-name
                           'entry (list 'file+olp path "Loops" heading)
                           (concat "* %(~title~)\nSCHEDULED: <%(org-read-date nil nil \"++" interval "\") ++" interval ">\n%?")
                           :prepend t
                           :kill-buffer t
                           :empty-lines-after 1)))))))

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
(cl-defun tychoish-org-add-project-file-capture-templates (&key name (path nil) (key "") (agenda nil))
  "Defines a set of capture mode templates for adding notes and tasks to a file."

  (unless (and (boundp 'org-capture-templates) org-capture-templates)
    (tychoish-org-reset-capture-templates))

  (when (and agenda (not (-contains-p (org-agenda-files) (f-full path))))
    (add-to-list 'org-agenda-files path))
  (when (not (equal "" key))
    (when (string-search "jntr" key)
      (error "org-capture prefix key '%s' for '%s' contains well-known prefix" key path))
    (when (string-search "xlk" key)
      (error "org-capture prefix key '%s' for '%s' contains sub-template key" key path)))

  (unless name
    (setq name "planner"))

  (setq path (if path
		 (f-expand path)
	       (concat (f-make-slug name) ".org")))

  (add-to-list 'org-capture-templates (list key (format "%s (project; %s)" name (file-name-nondirectory path))) t)

  (let ((org-filename (if (or (file-exists-p path)
			      (and
			       (< 1 (length (f-split path)))
			       (file-exists-p (f-dirname path))))
			  path
			(concat org-directory "/" path))))
    (tychoish/org-capture-add-routine-templates
     :name name
     :key key
     :path org-filename)

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
