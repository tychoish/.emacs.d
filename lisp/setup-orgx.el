;;; setup-orgx.el --- Personal Org mode configuration and setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal configuration, hooks, theme faces, agenda commands, and capture
;; setup for Org mode, built on `orgx' and `orgx-capture'.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'modus-themes)

;; Fallback to local checkout if orgx is not yet installed in ELPA
(unless (locate-library "orgx")
  (add-to-list 'load-path (expand-file-name "~/src/orgx/lisp")))

(defvar org-rst-headline-underline-characters)
(defvar org-archive-default-command)
(defvar org-agenda-files)

(autoload 'orgx-skip-child-of-project-tag "orgx")
(autoload 'orgx-skip-unless-untagged "orgx")
(autoload 'orgx-mode-turn-on "orgx" nil t)
(autoload 'orgx-mode "orgx" nil t)
(autoload 'orgx-capture-setup-standard-templates "orgx-capture" nil t)
(autoload 'orgx-setup-standard-capture-templates "orgx" nil t)
(autoload 'orgx-denote-setup-agenda-commands "orgx-denote" nil t)

;; Supporting export/format packages and commands

(autoload 'org-gist-export-to-gist "ox-gist" nil t)
(autoload 'toc-org-insert-toc "toc-org" nil t)
(autoload 'org-leanpub-book-export-markdown "ox-leanpub" nil t)
(autoload 'org-rst-export-to-rst "ox-rst" nil t)
(autoload 'org-rst-export-as-rst "ox-rst" nil t)

(setq org-rst-headline-underline-characters (list ?= ?- ?~ ?' ?^ ?`))

;; gist export (ox-gist integration)

(defun org-gist-export-private-gist ()
  (interactive)
  (org-gist-export-to-gist nil 'open))

(defun org-gist-export-public-gist ()
  (interactive)
  (org-gist-export-to-gist 'public))

(defvar-keymap orgx-gist-map
  :name "org-gist"
  :doc "Keymap for org-gist commands.")

(keymap-set orgx-gist-map "p" #'org-gist-export-private-gist)
(keymap-set orgx-gist-map "g" #'org-gist-export-public-gist)

;; Org-mode hooks and minor mode activation

(add-hook 'org-mode-hook #'orgx-mode-turn-on)

(setq org-modules
      '(org-capture
        org-datetree
        org-annotate-file
        org-depend
        org-habit))

(org-load-modules-maybe t)

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages '((python . t))))

;; org-faces
(setq org-todo-keyword-faces
      `(("TODO" . (:foreground ,(modus-themes-get-color-value 'red) :weight bold))
        ("DONE" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("QUESTION" . (:foreground ,(modus-themes-get-color-value 'cyan) :weight bold))
        ("ANSWERED" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("CONSIDER" . (:foreground ,(modus-themes-get-color-value 'yellow) :weight bold))
        ("MUST" . (:foreground ,(modus-themes-get-color-value 'yellow-warmer) :weight bold))
        ("ACKNOWLEDGED" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("IGNORED" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("REJECTED" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("BACKLOG" . (:foreground ,(modus-themes-get-color-value 'indigo) :weight bold))
        ("INPROGRESS" . (:foreground ,(modus-themes-get-color-value 'blue) :weight bold))
        ("SCHEDULED" . (:foreground ,(modus-themes-get-color-value 'cyan-warmer) :weight bold))
        ("GONEAWAY" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("INCOMPLETE" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("BLOCKED" . (:foreground ,(modus-themes-get-color-value 'rust) :weight bold))
        ("PAUSED" . (:foreground ,(modus-themes-get-color-value 'magenta) :weight bold))
        ("SKIPPED" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("CANCELED" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("DISMISSED" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("ABANDONED" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
        ("PROJECT" . (:foreground ,(modus-themes-get-color-value 'blue-warmer) :weight bold))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "QUESTION(q)" "|" "ANSWERED(a@)")
        (sequence "INPROGRESS" "|" "COMPLETE")
        (sequence "CONSIDER(c)" "MUST(m)" "|" "ACKNOWLEDGED(k@)" "IGNORED(u)" "REJECTED(r)" "DISMISSED(p)")
        (sequence "BACKLOG(b)" "SCHEDULED(h)" "|" "GONEAWAY(g@)" "INCOMPLETE(i@)")
        (sequence "BLOCKED(o)" "PAUSED(h)" "|" "SKIPPED(k@)" "CANCELED(x@)" "INCOMPLETE(i@)" "ABANDONED(@l)")))

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
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
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

;; Org-agenda custom commands and views

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
        ("i" . "including inherited")
        ("iu" "Untagged TODOs (incl. inherited)" todo ""
         ((org-agenda-skip-function #'orgx-skip-unless-untagged)
          (org-agenda-overriding-header "TODOs with no local or inherited tags")
          (orgx-agenda-include-inherited-tags t)))
        ("ih" "Untagged headings (incl. inherited)" tags "LEVEL>=1-TODO={.+}"
         ((org-agenda-skip-function #'orgx-skip-unless-untagged)
          (org-agenda-overriding-header "Headings with no local or inherited tags")
          (orgx-agenda-include-inherited-tags t)))))

;; Wire Denote-focused agenda views
(when (locate-library "orgx-denote")
  (require 'orgx-denote)
  (orgx-denote-setup-agenda-commands))

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

(setq org-archive-default-command #'org-archive-to-archive-sibling)

;; Initialize standard capture templates
(add-hook 'org-capture-mode-hook #'orgx-capture-setup-standard-templates)

(provide 'setup-orgx)
;;; setup-orgx.el ends here
