;;  -*- lexical-binding: t -*-

(autoload 'org-agenda-files "org")
(autoload 'org-save-all-org-buffers "org")

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

  (org-load-modules-maybe t))

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

(autoload 'org-leanpub-book-export-markua "ox-leanpub-book")
(autoload 'org-leanpub-markua-export-to-markua "ox-leanpub-markua")
(autoload 'org-leanpub-markua-export-as-markua "ox-leanpub-markua")
(autoload 'org-leanpub-markdown-export-to-markdown "ox-leanpub-markdown")
(autoload 'org-leanpub-markdown-export-as-markdown "ox-leanpub-markdown")

(defvar tychoish-org--auxiliary-packages
  '(org-contrib toc-org ox-gist ox-hugo ox-rst ox-leanpub)
  "Supporting org packages that should be installed when org-mode loads the first time.")

(defun tychoish-org--install-auxiliary-packages ()
  "Install all of the auxiliary packages"
  "<org.el> install aux packages"
  (->> tychoish-org--auxiliary-packages
       (-remove #'package-installed-p)
       (-map #'package-install-async)
       (length)))

(with-eval-after-load 'ox-rst
  (setq org-rst-headline-underline-characters (list ?= ?- ?~ ?' ?^ ?`)))

(with-eval-after-load 'ox-leanpub
  (org-leanpub-book-setup-menu-markua))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key bindings

(bind-keys :map tychoish/global-org-map
	   ("f" . org-agenda-files-open)
	   ("r" . org-agenda-files-reload)
	   :map tychoish/org-link-mode-map
           ("a" . org-annotate-file))

(defvar-keymap tychoish/org-gist-map
  :name "org-gist"
  :doc "keymap for org-gist commands"
  "p" #'org-gist-export-private-gist
  "g" #'org-gist-export-public-gist)

(with-eval-after-load 'org
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
             ("C-s" . org-save-all-org-buffers)
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
             :map tychoish/org-mode-personal-map
             :prefix "f"
             :prefix-map tychoish/org-mode-personal-archive-map
             ("d" . tychoish-org-mark-done-and-archive)
             ("e" . org-cycle-force-archived)
             ("t" . org-archive-set-tag)
             ("s" . org-archive-to-archive-sibling))

  (with-eval-after-load 'consult
    (bind-keys :map tychoish/org-mode-personal-map
               ("h" . consult-org-heading)         ;; Alternative: consult-org-heading (for jump)
               ("s" . consult-org-agenda))))        ;; Alternative: consult-org-heading (for jump))


(with-eval-after-load 'org-agenda
  (bind-keys :map org-agenda-mode-map
	     ("C-l" . org-agenda-open-link)
	     ("M-c" . org-agenda-goto-calendar)))

(with-eval-after-load 'consult-tycho
  (bind-keys :map tychoish/org-mode-capture-map
	     ("j" . consult-org-capture)
	     ("h" . consult-org-capture-target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; configuration

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

;; org.el
(setq org-CUA-compatible t)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions and helpers

(defun tychoish/set-up-buffer-org-mode ()
  (add-hook 'write-contents-functions 'tychoish--add-toc-org-op nil t)
  (setq-local fill-column 80))

;;;###autoload
(defun org-agenda-files-open ()
  "Open all agenda files if not already open."
  (interactive)
  (let* ((files (->> (org-agenda-files)
		     (--flat-map (if (f-directory-p it)
				     (f-glob "*.org" org-directory)
				   (cons it nil)))
		     (--remove (s-suffix-p "archive.org" it))))
	 (buffers (->> files
		       (--map (or (get-file-buffer it)
				  (find-file-noselect it t))))))
    (message "opened %d agenda files [%s]" (length files) (s-join ", " files))
    buffers))

;;;###autoload
(defun org-agenda-files-reload ()
  "Open all agenda files, and reverting to the version on disk as needed."
  (interactive)
  (->> (org-agenda-files-open)
       (--map (with-current-buffer it
		(revert-buffer nil (or current-prefix-arg (not (called-interactively-p 'interactive))) t)
		(buffer-file-name)))))

(defun tychoish/background-revbufs-for-hook ()
  (let ((buf (get-buffer-create "*revbufs*")))
    (with-current-buffer buf
      (revbufs)
      (bury-buffer buf))))

(defun tychoish--add-toc-org-op ()
  (save-excursion (toc-org-insert-toc)))

(defun org-gist-export-private-gist ()
  (interactive)
  (org-gist-export-to-gist nil 'open))

(defun org-gist-export-public-gist ()
  (interactive)
  (org-gist-export-to-gist 'public))

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

(setq org-agenda-custom-commands
      '(("b" "Backlog" tags "+backlog|+inbox-ITEM=\"Inbox\"|TODO=BLOCKED")))

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
(setq org-agenda-start-on-weekday nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-capture-templates

(defvar org-capture-templates nil)

(defun tychoish-org-reset-capture-templates ()
  (unless (boundp 'org-capture-templates)
    (defvar org-capture-templates))

  (setq org-capture-templates '(("t" "tasks")
                                ("j" "journal")
                                ("n" "notes")
                                ("r" "routines"))))

;;;###autoload
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

;;;###autoload
(cl-defun tychoish-org-add-project-file-capture-templates (&key name (path nil) (key ""))
  "Defines a set of capture mode templates for adding notes and tasks to a file."

  (unless (and (boundp 'org-capture-templates) org-capture-templates)
    (tychoish-org-reset-capture-templates))

  (when (not (equal "" key))
    (when (s-contains? key "jntr")
      (error "org-capture prefix key '%s' for '%s' contains well-known prefix" key path))
    (when (s-contains? key "xlk")
      (error "org-capture prefix key '%s' for '%s' contains sub-template key" key path)))

  (setq path (if path
		 (f-expand path)
	       (concat (make-filename-slug name) ".org")))

  (add-to-list 'org-capture-templates (list key (format "%s (project; %s)" name (f-filename path))) t)


  (let ((org-filename (if (or (f-exists-p path)
			      (and
			       (< 1 (length (f-split path)))
			       (f-exists-p (f-dirname path))))
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
(defun tychoish-org-setup-standard-capture-templates ()
  (tychoish/org-capture-add-note-templates
   :name "scratch"
   :path "records.org")

  (tychoish/org-capture-add-journal-templates
   :name "diary"
   :path "journal.org")

  (tychoish/org-capture-add-task-templates
   :name "prime"
   :path "planner.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hooks

(add-hygenic-one-shot-hook
 :name "org-install-aux-packages"
 :hook 'org-mode-hook
 :function (tychoish-org--install-auxiliary-packages))

(add-hygenic-one-shot-hook
 :name "org-capture [install standard templates]"
 :hook 'emacs-startup-hook
 :function (tychoish-org-setup-standard-capture-templates))

(provide 'tychoish-org)
