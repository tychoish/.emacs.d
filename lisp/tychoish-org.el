(require 'org)

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
    (save-excursion (toc-org-insert-toc)))

  (defun tychoish--add-toc-org-hook ()
    (add-hook 'write-contents-functions 'tychoish--add-toc-org-op nil t)))

(use-package ox-gist
  :ensure t
  :commands (org-gist-export-to-gist))

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

(defvar-keymap tychoish/org-gist-map
  :name "org-gist"
  :doc "keymap for org-gist commands"
  "p" #'org-gist-export-private-gist
  "g" #'org-gist-export-public-gist)

(defun org-gist-export-private-gist ()
  (interactive)
  (org-gist-export-to-gist))

(defun org-gist-export-public-gist ()
  (interactive)
  (org-gist-export-to-gist 'public))

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
           ("t" . org-set-tags-command)
           ("n" . org-narrow-to-subtree)
           ("w" . widen)
           ("p" . org-insert-property-drawer)
           ("w" . org-refile)
           ("d" . tychoish-org-date-now)
           ("i" . org-ctags-create-tags)
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
           :prefix "a"
           :prefix-map tychoish/org-mode-personal-archive-map
           ("d" . tychoish-org-mark-done-and-archive)
           ("e" . org-cycle-force-archived)
           ("f" . org-archive-set-tag)
           ("s" . org-archive-to-archive-sibling)
           :map tychoish/org-mode-personal-map
           :prefix "r"
           :prefix-map tychoish/org-mode-personal-bibtex-map
           ("a" . org-bibtex-check-all)
           ("c" . org-bibtex-create)
           ("e" . org-bibtex)
           ("k" . org-bibtex-export-to-kill-ring)
           ("r" . org-bibtex-create-in-current-entry)
           ("s" . org-bibtex-search)
           ("v" . org-bibtex-check))

(with-eval-after-load 'org-agenda
  (bind-keys :map org-agenda-mode-map
	     ("C-l" . org-agenda-open-link)
	     ("M-c" . org-agenda-goto-calendar)))

(with-eval-after-load 'helm
  (bind-keys :map tychoish/helm-center-menu-map
             :prefix "o"
             :prefix-map tychoish/org-mode-personal-helm-map
             ("b" . helm-org-in-buffer-headings)
             ("c" . helm-capture-templates)
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

(defun tychoish-org-date-now ()
  (interactive)
  (format-time-string "<%Y-%02m-%02d %02H:%02M:%02S %Z>" (time-stamp)))

(defun tychoish-org-mark-done-and-archive ()
  "mark done and move to completed archive sibling"
  (interactive)
  (org-todo 'done)
  (let ((org-archive-sibling-heading "Completed"))
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
      '(("b" "Backlog" tags "+backlog|+inbox-ITEM=\"Inbox\"|TODO=BLOCKED")
        ("c" "SuperView"
         ((agenda "" ((org-agenda-overriding-header "Schedule:")
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "Tasks:")
                       (org-super-agenda-groups
                        '((:log t)
                          (:name "To refile"
                                 :file-path (concat tychoish-org-fn-main "/refile"))
                          (:name "Today's tasks"
                                 :file-path "journal/")
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Scheduled Soon"
                                 :scheduled future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:discard (:not (:todo "TODO")))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org capture templates definitions

(defun tychoish-org-setup-standard-capture-templates ()
  "Defines a set of capture templates for a records.org, journal.org, and planner.org"
  (interactive)
  ;;
  ;; loops/routines
  ;;
  (tychoish/org-add-routines
   :name "prime"
   :path "planner.org")

  ;;
  ;; notes
  ;;
  (add-to-list 'org-capture-templates
               '("nx" "notes (prime; X buffer)"
                 entry (file+headline "records.org" "Inbox")
                 "* %^{Title}\n%x\n%?"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("nk" "notes (prime; kill buffer)"
                 entry (file+headline "records.org" "Inbox")
                 "* %^{Title}\n%^C\n%?"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("nl" "notes (prime; org-link)"
                 entry (file+headline "records.org" "Inbox")
                 "* %^{Title}\n%A\n%?"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("nn" "notes (prime; selection)"
                 entry (file+headline "records.org" "Inbox")
                 "* %^{Title}\n%?\n%i"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))

  ;;
  ;; journal
  ;;
  (add-to-list 'org-capture-templates
               '("jx" "journal (prime; X11 buffer)"
                 entry (file+olp+datetree "journal.org")
                 "* %^{Title} <%<%Y-%m-%d %H:%M>>\n%x\n\n%?"
                 :prepend nil
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("jk" "journal (prime; kill buffer)"
                 entry (file+olp+datetree "journal.org")
                 "* %^{Title} <%<%Y-%m-%d %H:%M>>\n%^C\n\n%?"
                 :prepend nil
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("jl" "journal (prime; org-link)"
                 entry (file+olp+datetree "journal.org")
                 "* %^{Title} <%<%Y-%m-%d %H:%M>>\n%A\n\n%?"
                 :prepend nil
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("jj" "journal (prime; selection)"
                 entry (file+olp+datetree "journal.org")
                 "* %^{Title} <%<%Y-%m-%d %H:%M>>\n%?\n%i"
                 :prepend nil
                 :kill-buffer t
                 :empty-lines-after 1))

  ;;
  ;; tasks
  ;;
  (add-to-list 'org-capture-templates
               '("tx" "tasks (prime; X buffer)"
                 entry (file+headline "planner.org" "Tasks")
                 "* TODO %^{Task}\n%x\n"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("tk" "tasks (prime; kill buffer)"
                 entry (file+headline "planner.org" "Tasks")
                 "* TODO %^{Task}\n%^C\n"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("tl" "tasks (prime; org-link)"
                 entry (file+headline "planner.org" "Tasks")
                 "* TODO %^{Task}\n%A\n"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("tt" "tasks (prime; selection)"
                 entry (file+headline "planner.org" "Tasks")
                 "* TODO %^{Task}\n%i\n"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))

  (message "registered standard tychoish org-capture templates"))

(cl-defun tychoish/org-add-routines (&key name
					  (key "")
					  (path (concat (make-filename-slug name) ".org")))

  (if (string-equal "" key)
      (add-to-list 'org-capture-templates '("r" "routines"))
    (add-to-list 'org-capture-templates `(,(concat "r" key) ,(concat name " routines")))
    (add-to-list 'org-capture-templates `(,(concat key "r") ,(concat name " routines"))))

  (->> '(("1d" .  "Daily")
         ("1w" .  "Weekly")
         ("4w" . "Monthly")
         ("12w" . "Quarterly")
         ("21w" . "Half Yearly")
         ("52w" . "Yearly"))

       (--flat-map (let* ((interval (car it))
			  (heading (cdr it))
			  (interval-prefix (downcase (substring heading 0 1))))
		     (->> (list (format "%s%s%s" key "r" interval-prefix)
				(format "%s%s%s" "r" key interval-prefix))
			  (-distinct)
			  (-map (lambda (prefix) (cons prefix (cons interval heading)))))))

       (--map (let* ((prefix (car it))
		     (interval (cadr it))
		     (heading (cddr it))
		     (lower-heading (downcase heading))
		     (menu-name (s-join " " (list name lower-heading "routine"))))

		(add-to-list 'org-capture-templates
			     `(,prefix ,menu-name
			       entry (file+olp ,path "Loops" ,heading)
			       ,(concat "\n* %^{Title}\nSCHEDULED: <%(org-read-date nil nil \"++" interval "\") ++" interval ">\n%?")
			       :prepend t
			       :kill-buffer t
			       :empty-lines-after 1))))))

(cl-defun tychoish-org-add-project-file-capture-templates (&key name
								(key "")
							        (path (concat (make-filename-slug name) ".org")))
  "Defines a set of capture mode templates for adding notes and tasks to a file."
  (interactive)

  (when (s-contains? key "jntr")
    (error "org-capture prefix key '%s' for '%s' contains well-known prefix" key path))
  (when (s-contains? key "xlk")
    (error "org-capture prefix key '%s' for '%s' contains sub-template key" key path))
  (when (s-contains? key "csw")
    (error "org-capture prefix key '%s' for '%s' contains, which is a reserved key" key path))

  (let ((org-filename (concat org-directory "/" path)))

    (unless (string-equal "" key)
      (add-to-list 'org-capture-templates `(,key ,name)))

    ;; journal, for date related content
    (add-to-list 'org-capture-templates `(,(concat key "j") ,(concat name " journal")))
    (add-to-list 'org-capture-templates `(,(concat key "n") ,(concat name " notes")))
    (add-to-list 'org-capture-templates `(,(concat key "t") ,(concat name " tasks")))

    (tychoish/org-add-routines
     :name name
     :key key
     :path org-filename)

    (dolist (prefix (list (concat key "jj")
                          (concat "j" key)))
      (add-to-list 'org-capture-templates
                   `(,prefix ,(concat name " journal")
                     entry (file+olp+datetree ,org-filename "Journal")
                     "* %^{Title} <%<%Y-%m-%d %H:%M>>\n"
                     :prepend nil
                     :kill-buffer t
                     :empty-lines-after 1)))

    (dolist (prefix (list (concat key "nn")
                          (concat "n" key)))
      (add-to-list 'org-capture-templates
                   `(,prefix ,(concat name " notes")
                     entry (file+headline ,org-filename "Inbox")
                     "* %^{Title}\n%?\n%i\n"
                     :prepend t
                     :kill-buffer t
                     :empty-lines-after 1)))

    (dolist (prefix (list (concat "t" key)
                          (concat key "tt")))
      (add-to-list 'org-capture-templates
                   `(,prefix ,(concat name " tasks")
                             entry (file+headline ,org-filename "Tasks")
                             "* TODO %^{Task}\n%i\n"
                             :prepend t
                             :kill-buffer t
                             :empty-lines-after 0)))

    (add-to-list 'org-capture-templates
                 `(,(concat key "jl") ,(concat name " journal (org-link)")
                   entry (file+olp+datetree ,org-filename "Journal")
                   "* %^{Title} <%<%Y-%m-%d %H:%M>>\n%a\n"
                   :prepend nil
                   :kill-buffer t
                   :empty-lines-after 1))
    (add-to-list 'org-capture-templates
                 `(,(concat key "jx") ,(concat name " journal (X buffer)")
                   entry (file+olp+datetree ,org-filename "Journal")
                   "* %^{Title} <%<%Y-%m-%d %H:%M>>\n%x\n"
                   :prepend nil
                   :kill-buffer t
                   :empty-lines-after 1))
    (add-to-list 'org-capture-templates
                 `(,(concat key "jx") ,(concat name " journal (kill-buffer)")
                   entry (file+olp+datetree ,org-filename "Journal")
                   "* %^{Title} <%<%Y-%m-%d %H:%M>>\n%c\n"
                   :prepend nil
                   :kill-buffer t
                   :empty-lines-after 1))

    (add-to-list 'org-capture-templates
                 `(,(concat key "nl") ,(concat name " notes (org-link)")
                   entry (file+headline ,org-filename "Inbox")
                   "* %^{Title}\n%?\n%a"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 1))
    (add-to-list 'org-capture-templates
                 `(,(concat key "nx") ,(concat name " notes (X buffer)")
                   entry (file+headline ,org-filename "Inbox")
                   "* %^{Title}\n%?\n%x"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 1))
    (add-to-list 'org-capture-templates
                 `(,(concat key "nk") ,(concat name " notes (kill buffer)")
                   entry (file+headline ,org-filename "Inbox")
                   "* %^{Title}\n%?\n%c"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 1))

    ;; extra tasks
    (add-to-list 'org-capture-templates
                 `(,(concat key "tl") ,(concat name " tasks (org-link)")
                   entry (file+headline ,org-filename "Tasks")
                   "* TODO %^{Task}\n%a\n"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 0))
    (add-to-list 'org-capture-templates
                 `(,(concat key "tx") ,(concat name " tasks (X buffer)")
                   entry (file+headline ,org-filename "Tasks")
                   "* TODO %^{Task}\n%x\n"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 0))
    (add-to-list 'org-capture-templates
                 `(,(concat key "tk") ,(concat name " tasks (kill buffer)")
                   entry (file+headline ,org-filename "Tasks")
                   "* TODO %^{Task}\n%c\n"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 0))
    (message (concat "registered org-capture templates for " org-filename))))


(provide 'tychoish-org)
