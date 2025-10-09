;;  -*- lexical-binding: t -*-

(require 'f)
(require 's)
(require 'dash)

(require 'org)
(require 'org-contrib)
(require 'org-capture)
(require 'org-agenda)

(require 'tychoish-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-mode extensions and supporting packages

(use-package org-contrib
  :ensure t
  :after org)

(use-package toc-org
  :ensure t
  :commands (toc-org-insert-toc)
  :init
  (defun tychoish--add-toc-org-op ()
    (save-excursion (toc-org-insert-toc))))

(use-package ox-gist
  :ensure t
  :commands (org-gist-export-to-gist)
  :init
  (defun org-gist-export-private-gist ()
    (interactive)
    (org-gist-export-to-gist nil 'open))

  (defun org-gist-export-public-gist ()
    (interactive)
    (org-gist-export-to-gist 'public)))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package ox-rst
  :ensure t
  :after ox
  :commands (org-rst-export-to-rst org-rst-export-as-rst)
  :config
  (setq org-rst-headline-underline-characters (list ?= ?- ?~ ?' ?^ ?`)))

(use-package ox-leanpub
  :ensure t
  :after ox
  :commands (org-leanpub-book-export-markdown
	     org-leanpub-book-export-markua
	     org-leanpub-markua-export-to-markua
	     org-leanpub-markua-export-as-markua
	     org-leanpub-markdown-export-to-markdown
	     org-leanpub-markdown-export-as-markdown)
  :config
  (require 'ox-leanpub-markua)
  (org-leanpub-book-setup-menu-markua))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key bindings

;; bind keys inside of org-mode
(bind-keys :map org-mode-map
           ("C-c l o" . org-link-open-from-string)
           ("C-c C-p" . set-mark-command)
           ("M-TAB" . org-cycle)
           ("C-M-TAB" . org-cycle-force-archived)
           :map org-mode-map
           :prefix "C-c o"
           :prefix-map tychoish/org-mode-personal-map ;; "C-c o"
           ("a" . org-agenda)
           ("k" . org-capture)
	   ("f" . org-agenda-files-open)
           ("t" . org-set-tags-command)
           ("n" . org-narrow-to-subtree)
           ("w" . widen)
           ("p" . org-insert-property-drawer)
           ("w" . org-refile)
           ("d" . tychoish-org-date-now)
           ;; ("i" . org-ctags-create-tags)
	   ;; ("g" . tychoish/org-gist-map)
           :map tychoish/org-mode-personal-map
           :prefix "c"
           :prefix-map tychoish/org-mode-capture-map
           ("c" . org-capture)
           ("p" . org-capture-goto-last-stored)
           ("l" . org-capture-goto-last-stored)
           ("t" . org-capture-goto-target)
           ("r" . org-capture-refile)
           ("w" . org-capture-refile)
	   :map org-agenda-mode-map
	   ("C-l" . org-agenda-open-link)
	   ("M-c" . org-agenda-goto-calendar)
           :map tychoish/org-mode-personal-map
           :prefix "f"
           :prefix-map tychoish/org-mode-personal-archive-map
           ("d" . tychoish-org-mark-done-and-archive)
           ("e" . org-cycle-force-archived)
           ("t" . org-archive-set-tag)
           ("s" . org-archive-to-archive-sibling))

(with-eval-after-load 'helm
  (bind-keys :map tychoish/helm-center-menu-map
             :prefix "o"
             :prefix-map tychoish/org-mode-personal-helm-map
             ("b" . helm-org-in-buffer-headings)
             ("p" . helm-org-parent-headings)
             ("a" . helm-org-agenda-files-headings)))

(with-eval-after-load 'consult
  (bind-keys :map tychoish/org-mode-personal-map ;; "C-c o"
             ("h" . consult-org-heading)               ;; Alternative: consult-org-heading (for jump)
             ("s" . consult-org-agenda)))              ;; Alternative: consult-org-heading (for jump))

(with-eval-after-load 'consult-tycho
  (bind-keys :map tychoish/org-mode-capture-map
	     ("j" . consult-org-capture)
	     ("h" . consult-org-capture-target)))

(defvar-keymap tychoish/org-gist-map
  :name "org-gist"
  :doc "keymap for org-gist commands"
  "p" #'org-gist-export-private-gist
  "g" #'org-gist-export-public-gist)

(bind-key "g" 'tychoish/org-gist-map tychoish/org-mode-personal-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; configuration

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "BLOCKED(s)" "BACKLOG(b)" "INPROGRESS(p)" "|" "SKIPPED" "GONEAWAY(g@)" "INCOMPLETE(i@)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("INPROGRESS" . "orange")
        ("INCOMPLETE" . "orange")
        ("SCHEDULED" . "green")
        ("BACKLOG" . (:foreground "orange" :weight bold))
        ("PROJECT" . (:foreground "blue" :weight bold))))

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

(setq org-modules
      '(org-capture
        org-datetree
        org-annotate-file
        org-depend
        org-habit))

(setq org-CUA-compatible t)
(setq org-tags-column -70)
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
(setq org-agenda-start-on-weekday nil)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)
(setq org-fast-tag-selection-include-todo t)
(setq org-fontify-done-headline t)
(setq org-footnote-auto-label nil)
(setq org-footnote-define-inline nil)
(setq org-footnote-section nil)
(setq org-log-into-drawer t)
(setq org-outline-path-complete-in-steps nil)
(setq org-provide-todo-statistics t)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
(setq org-refile-use-outline-path 'file)
(setq org-replace-disputed-keys t)
(setq org-return-follows-link t)
(setq org-reverse-note-order t)
(setq org-startup-folded 'content)
(setq org-startup-indented nil)
(setq org-tags-exclude-from-inheritance '("project"))
(setq org-track-ordered-property-with-tag t)
(setq org-use-fast-tag-selection 'auto)
(setq org-use-fast-todo-selection 'auto)
(setq org-use-speed-commands #'tychoish/org-use-speed-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions and helpers

(defun tychoish/set-up-buffer-org-mode ()
  (add-hook 'write-contents-functions 'tychoish--add-toc-org-op nil t)
  (setq-local fill-column 80))

;;;###autoload
(defun tychoish-org-reset-capture-templates ()
  (interactive)
  (setq org-capture-templates '(("t" "tasks")
                                ("j" "journal")
                                ("n" "notes")
                                ("r" "routines")))
  (message "reset all org-capture templates"))

(defun org-agenda-files-reload ()
  "reloads all agenda files"
  (interactive)
  (setq org-agenda-files (--remove (s-suffix-p "archive.org" it) (f-glob "*.org" org-directory))))

(defun org-agenda-files-open ()
  (interactive)
  (let ((files (--mapc (find-file-noselect it t) (org-agenda-files))))
    (message "opened %d agenda files [%s]" (length files) (s-join ", " (-map #'f-filename files)))))

(defun tychoish/background-revbufs-for-hook ()
  (let ((buf (get-buffer-create "*revbufs*")))
    (with-current-buffer buf
      (revbufs)
      (bury-buffer buf))))

(defconst tychoish/org-date-spec-datetime "<%Y-%02m-%02d %02H:%02M:%02S %Z>")

(defconst tychoish/org-date-spec-date "<%Y-%02m-%02d>")

(defun tychoish-org-date-now (&optional &key short)
  (interactive)
  (format-time-string (if short
			  tychoish/org-date-spec-date
			tychoish/org-date-spec-datetime) (time-stamp)))

(defun tychoish-org-mark-done-and-archive ()
  "mark done and move to completed archive sibling"
  (interactive)
  (org-todo 'done)
  (require 'org-archive)
  (let ((org-archive-sibling-heading "Completed"))
    (ignore org-archive-sibling-heading)
    (org-archive-to-archive-sibling)))

(defun org-set-weekday-of-timestamp ()
  "Check if cursor is within a timestamp and compute weekday from numeric date"
  (interactive)
  (when (org-at-timestamp-p t)
    (org-timestamp-change 0 'year)
    t))

(defun tychoish/org-use-speed-commands ()
    (and (looking-at org-outline-regexp) (looking-back "^\**" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-agenda

(setq org-agenda-include-diary nil)
(setq org-agenda-custom-commands
      '(("b" "Backlog" tags "+backlog|+inbox-ITEM=\"Inbox\"|TODO=BLOCKED")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-capture-templates

(cl-defun tychoish/org-capture-add-routine-templates (&key name
							   (key "")
							   (path (concat (make-filename-slug name) ".org")))
  (when (string-equal "r" key)
    (user-error "cannot define routine (loops) %s org-capture-templates with key `r'" name))

  (let ((description (format "%s routines <%s>" name (f-filename path))))
    (unless (string-equal "" key)
      (add-to-list 'org-capture-templates (list (concat "r" key) description))
      (add-to-list 'org-capture-templates (list (concat key "r") description))))

  (->> '(("1d" .  "Daily")
         ("1w" .  "Weekly")
         ("4w" . "Monthly")
         ("12w" . "Quarterly")
         ("21w" . "Half Yearly")
         ("52w" . "Yearly"))
       (--flat-map (let* ((interval (car it))
			  (heading (cdr it))
			  (interval-key (downcase (substring-no-properties heading 0 1))))
		     (->> (list (concat key "r" interval-key)
				(concat "r" key interval-key))
			  (-map (lambda (prefix) (cons prefix (cons interval heading)))))))

       (--map (let* ((prefix (car it))
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
		       :empty-lines-after 1))))))

(cl-defun tychoish/org-capture-add-journal-templates (&key name path (key ""))
  (when (string-equal "j" key)
    (user-error "cannot define journal %s org-capture-templates with key `j'" name))

  (let (specs append-item
	      (capture-location (if (equal "" key)
			      (list 'file+olp+datetree path)
			    (list 'file+olp+datetree path "Journal"))))

    (unless (string-equal "" key)
      (setq append-item t)
      (add-to-list 'org-capture-templates (list (concat key "j") (format "%s journal <%s>" name  (f-filename path))))
      (push (cons (concat "j" key) (cons "" "<today>")) specs))

    (->> (-join specs
		(-l (cons (concat key "jj") (cons "" "<today>"))
		    (cons (concat key "jp") (cons "" "<date prompt>"))
		    (cons (concat key "jx") (cons "%x" "X11 buffer"))
		    (cons (concat key "jl") (cons "%a" "org-link"))
		    (cons (concat key "jk") (cons "%c" "emacs kill-ring"))))
	 (reverse)
	 (--map (let ((key-sequence (car it))
		      (template-anchor (cadr it))
		      (description (cddr it)))
		  (add-to-list
		   'org-capture-templates
		   (-l key-sequence (format "journal (%s; %s)" name description)
		       'entry capture-location
                       (concat "* %(~title~) <%<%Y-%m-%d %H:%M>>" template-anchor "\n%?")
                       :prepend nil
                       :kill-buffer t
		       :time-prompt (s-suffix-p "jp" key-sequence)
                       :empty-lines-after 1)
		   append-item))))))

(defun ~title~ () (consult-tycho--select-context-for-operation "title => "))

(cl-defun tychoish/org-capture-add-task-templates (&key name path (key ""))
  (when (string-equal "t" key)
    (user-error "cannot define task %s org-capture-templates with key `t'" name))

  (let (specs append-item)
    (unless (string-equal "" key)
      (add-to-list 'org-capture-templates (list (concat key "t") (format "%s tasks <%s>" name  (f-filename path))))
      (push (cons (concat "t" key) (cons "%i" "selection")) specs)
      (setq append-item t))

    (->> (-join specs
		(-l (cons (concat key "tt") (cons "%i" "selection"))
		    (cons (concat key "tx") (cons "%x" "X11 buffer"))
		    (cons (concat key "tl") (cons "%a" "org-link"))
		    (cons (concat key "tk") (cons "%c" "emacs kill-ring"))))
	 (-non-nil)
	 (--map (let ((key-sequence (car it))
		      (template-anchor (cadr it))
		      (description (cddr it)))
		  (add-to-list
		   'org-capture-templates
		   (-l key-sequence (format "tasks (%s; %s)" name description)
		       'entry
		       (list 'file+headline path "Tasks")
                       (concat "* %(~title~)\n" template-anchor "\n%?")
                       :prepend t
                       :kill-buffer t
                       :empty-lines-after 1)
		   append-item))))))

(cl-defun tychoish/org-capture-add-note-templates (&key name path (key ""))
  (when (string-equal "n" key)
    (user-error "cannot define notes %s org-capture-templates with key `n'" name))

  (let (specs append-item)
	(unless (string-equal "" key)
	  (setq append-item t)
	  (add-to-list 'org-capture-templates (list (concat key "t") (format "%s notes <%s>" name  (f-filename path))))
	  (push (cons (concat "n" key) (cons "%i" "selection")) specs))

	(->> (-join specs
		    (-l (cons (concat key "nn") (cons "%i" "selection"))
			(cons (concat key "nx") (cons "%x" "X11 buffer"))
			(cons (concat key "nl") (cons "%a" "org-link"))
			(cons (concat key "nk") (cons "%c" "emacs kill-ring"))))
	     (-non-nil)
	     (--map (let ((key-sequence (car it))
			  (template-anchor (cadr it))
			  (description (cddr it)))
		      (add-to-list
		       'org-capture-templates
		       (-l key-sequence (format "notes (%s; %s)" name description)
			   'entry
			   (list 'file+headline path "Inbox")
			   (concat "* %(~title~)\n" template-anchor "\n%?")
			   :prepend t
			   :kill-buffer t
			   :empty-lines-after 1)
		       append-item))))))

;; registration helpers

(cl-defun tychoish-org-add-project-file-capture-templates (&key name (path nil) (key ""))
  "Defines a set of capture mode templates for adding notes and tasks to a file."

  (when (not (equal "" key))
    (when (s-contains? key "jntr")
      (error "org-capture prefix key '%s' for '%s' contains well-known prefix" key path))
    (when (s-contains? key "xlk")
      (error "org-capture prefix key '%s' for '%s' contains sub-template key" key path))
    (when (s-contains? key "csw")
      (error "org-capture prefix key '%s' for '%s' contains, which is a reserved key" key path)))

  (unless path
    (setq path (concat (make-filename-slug name) ".org")))

  (add-to-list 'org-capture-templates (list key (format "%s (project; %s)" name (f-filename path))) t)

  (let ((org-filename (concat org-directory "/" path)))
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
     :path org-filename)

    (message "registered org-capture templates for %s" org-filename)))

;; org capture templates definitions
(defun tychoish-org-setup-standard-capture-templates ()
  (tychoish/org-capture-add-routine-templates
   :name "prime"
   :path "planner.org")

  (tychoish/org-capture-add-note-templates
   :name "scratch"
   :path "records.org")

  (tychoish/org-capture-add-journal-templates
     :name "diary"
     :path "journal.org")

  (tychoish/org-capture-add-task-templates
   :name "prime"
   :path "planner.org")

  (message "registered standard org-capture-templates"))

(provide 'tychoish-org)
