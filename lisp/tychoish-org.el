(use-package org-contrib
  :ensure t
  :after org)

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

(defun tychoish-org-setup-standard-capture-templates ()
  "Defines a set of capture templates for a records.org, journal.org, and planner.org"
  (interactive)
  ;;
  ;; loops/routines
  ;;
  (add-to-list 'org-capture-templates
               '("rd" "daily loop (prime)"
                 entry (file+olp "planner.org" "Loops" "Daily")
                 "* TODO %^{Title}\nSCHEDULED: <%<%Y-%m-%d %H:%M +1d>>\n%?\n%i"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("rw" "weekly loop (prime)"
                 entry (file+headline "planner.org" "Loops" "Weekly")
                 "* TODO %^{Title}\nSCHEDULED: <%<%Y-%m-%d +1w>\n%?\n%i"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("rm" "monthly loop (prime)"
                 entry (file+headline "planner.org" "Loops" "Monthly")
                 "* TODO %^{Title}\nSCHEDULED: <%<%Y-%m-%d +1m>\n%?\n%i"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("rq" "quarterly loop (prime)"
                 entry (file+headline "planner.org" "Loops" "Quarterly")
                 "* TODO %^{Title}\nSCHEDULED: <%<%Y-%m-%d +1q>\n%?\n%i"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("ry" "yearly loop (prime)"
                 entry (file+headline "planner.org" "Loops" "Yearly")
                 "* TODO %^{Title}\nSCHEDULED: <%<%Y-%m-%d +1y>\n%?\n%i"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))

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
                 "* TODO %^{Task}\n%x\n%?"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("tk" "tasks (prime; kill buffer)"
                 entry (file+headline "planner.org" "Tasks")
                 "* TODO %^{Task}\n%^C\n%?"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("tl" "tasks (prime; org-link)"
                 entry (file+headline "planner.org" "Tasks")
                 "* TODO %^{Task}\n%A\n%?"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))
  (add-to-list 'org-capture-templates
               '("tt" "tasks (prime; selection)"
                 entry (file+headline "planner.org" "Tasks")
                 "* TODO %^{Task}\n%?\n%i"
                 :prepend t
                 :kill-buffer t
                 :empty-lines-after 1))

  (message "registered standard tychoish org-capture templates"))

(defun tychoish-org--add-routines (prefix-key name orgfile-path)
  (add-to-list 'org-capture-templates `(,(concat "r" prefix-key) ,name))
  (add-to-list 'org-capture-templates `(,(concat prefix-key "r") "loops"))

  (dolist (ival-pair '(("1d" . "Daily")
                       ("1w" . "Weekly")
                       ("4w" . "Monthly")
                       ("12w" . "Quarterly")
                       ("21w" . "Half Yearly")
                       ("52w" . "Yearly")))
    (let* ((interval (car ival-pair))
           (heading (cdr ival-pair))
           (interval-prefix (downcase (substring heading 0 1)))
           (lower-heading (downcase heading))
           (menu-name (s-join " " (list name lower-heading "routine")))
           (template (concat "* %^{Title}\nSCHEDULED: <%(org-read-date nil nil \"++" interval "\") ++" interval ">\n%?")))

      (dolist (prefix (list (concat prefix-key "r" interval-prefix)
                            (concat "r" prefix-key interval-prefix)))
        (add-to-list 'org-capture-templates
                     `(,prefix ,(concat name " " lower-heading " routine")
                       entry (file+olp ,orgfile-path "Loops" ,heading)
                       ,template
                       :prepend t
                       :kill-buffer t
                       :empty-lines-after 1))))))

(defun tychoish-org-add-project-file-capture-templates (name &rest args)
  "Defines a set of capture mode templates for adding notes and tasks to a file."
  (interactive)

  (let ((org-filename (concat org-directory "/" (make-filename-slug name) ".org"))
        (prefix-key (or (plist-get args :prefix) "")))

    (when (s-contains? prefix-key "jntr")
      (error "org-capture prefix key '%s' for '%s' contains well-known prefix" prefix-key org-filename))
    (when (s-contains? prefix-key "xlk")
      (error "org-capture prefix key '%s' for '%s' contains sub-template key" prefix-key org-filename))
    (when (s-contains? prefix-key "csw")
      (error "org-capture prefix key '%s' for '%s' contains, which is a reserved key" prefix-key org-filename))

    (when prefix-key
      (add-to-list 'org-capture-templates `(,prefix-key ,name))
      (tychoish-org--add-routines prefix-key name org-filename))

    ;; journal, for date related content
    (add-to-list 'org-capture-templates `(,(concat prefix-key "j") ,(concat name " journal")))
    (add-to-list 'org-capture-templates `(,(concat prefix-key "n") ,(concat name " notes")))
    (add-to-list 'org-capture-templates `(,(concat prefix-key "t") ,(concat name " tasks")))

    (dolist (prefix (list (concat prefix-key "jj")
                          (concat "j" prefix-key)))
      (add-to-list 'org-capture-templates
                   `(,prefix ,(concat name " journal")
                     entry (file+olp+datetree ,org-filename "Journal")
                     "* %^{Title} <%<%Y-%m-%d %H:%M>>\n%?"
                     :prepend nil
                     :kill-buffer t
                     :empty-lines-after 1)))

    (add-to-list 'org-capture-templates
                 `(,(concat prefix-key "jl") ,(concat name " journal (org-link)")
                   entry (file+olp+datetree ,org-filename "Journal")
                   "* %^{Title} <%<%Y-%m-%d %H:%M>>\n%a\n%?"
                   :prepend nil
                   :kill-buffer t
                   :empty-lines-after 1))
    (add-to-list 'org-capture-templates
                 `(,(concat prefix-key "jx") ,(concat name " journal (kill-buffer)")
                   entry (file+olp+datetree ,org-filename "Journal")
                   "* %^{Title} <%<%Y-%m-%d %H:%M>>\n%x\n%?"
                   :prepend nil
                   :kill-buffer t
                   :empty-lines-after 1))

    (dolist (prefix (list (concat prefix-key "nn")
                          (concat "n" prefix-key)))
      (add-to-list 'org-capture-templates
                   `(,prefix ,(concat name " notes")
                     entry (file+headline ,org-filename "Inbox")
                     "* %^{Title}\n%?\n%i"
                     :prepend t
                     :kill-buffer t
                     :empty-lines-after 1)))

    (add-to-list 'org-capture-templates
                 `(,(concat prefix-key "nl") ,(concat name " notes (org-link)")
                   entry (file+headline ,org-filename "Inbox")
                   "* %^{Title}\n%?\n%a"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 1))
    (add-to-list 'org-capture-templates
                 `(,(concat prefix-key "nx") ,(concat name " notes (X buffer)")
                   entry (file+headline ,org-filename "Inbox")
                   "* %^{Title}\n%?\n%x"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 1))
    (add-to-list 'org-capture-templates
                 `(,(concat prefix-key "nk") ,(concat name " notes (kill buffer)")
                   entry (file+headline ,org-filename "Inbox")
                   "* %^{Title}\n%?\n%c"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 1))

    (dolist (prefix (list (concat "t" prefix-key)
                          (concat prefix-key "tt")))
      (add-to-list 'org-capture-templates
                   `(,prefix ,(concat name " basic tasks")
                             entry (file+headline ,org-filename "Tasks")
                             "* TODO %^{Task}\n%i\n%?"
                             :prepend t
                             :kill-buffer t
                             :empty-lines-after 0)))
    ;; extra tasks
    (add-to-list 'org-capture-templates
                 `(,(concat prefix-key "tl") ,(concat name " tasks (org-link)")
                   entry (file+headline ,org-filename "Tasks")
                   "* TODO %^{Task}\n%a\n%?"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 0))
    (add-to-list 'org-capture-templates
                 `(,(concat prefix-key "tl") ,(concat name " tasks (X buffer)")
                   entry (file+headline ,org-filename "Tasks")
                   "* TODO %^{Task}\n%x\n%?"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 0))
    (add-to-list 'org-capture-templates
                 `(,(concat prefix-key "tk") ,(concat name " tasks (kill buffer)")
                   entry (file+headline ,org-filename "Tasks")
                   "* TODO %^{Task}\n%c\n%?"
                   :prepend t
                   :kill-buffer t
                   :empty-lines-after 0))
    (message (concat "registered org-capture templates for " org-filename))))


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
  (setq org-agenda-files (--remove (s-matches? "archive.org$" it) (f-glob "*.org" org-directory))))

(provide 'tychoish-org)
