;;; setup-orgx.el --- Personal Org mode configuration and setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal configuration, hooks, theme faces, agenda commands, and capture
;; setup for Org mode, built on `orgx' and `orgx-capture'.

;;; Code:

(require 'orgx)
(require 'orgx-capture)

;; Supporting export/format packages

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

(use-package ob-mermaid
  :ensure t
  :defer t)

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

;; Org-mode hooks, faces, and variables

(with-eval-after-load 'org
  (add-hook 'org-ctrl-c-ctrl-c-hook #'orgx-set-weekday-of-timestamp)
  (add-hook 'org-shiftup-final-hook #'windmove-up)
  (add-hook 'org-shiftleft-final-hook #'windmove-left)
  (add-hook 'org-shiftdown-final-hook #'windmove-down)
  (add-hook 'org-shiftright-final-hook #'windmove-right)
  (add-hook 'org-after-todo-state-change-hook #'orgx-enforce-question-answered)

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
  (require 'modus-themes)
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
  (setq org-return-follows-link t)
  (setq org-use-speed-commands #'orgx--use-speed-commands))

;; Org-agenda custom commands and views

(with-eval-after-load 'org-agenda
  (setq org-agenda-skip-function-global #'orgx-skip-child-of-project-tag)
  (setq org-agenda-sticky t)

  (setq org-agenda-custom-commands
        `(("b" "Backlog" tags "+backlog|+inbox-ITEM="Inbox"|TODO=BLOCKED"
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
	   ((tags-todo "-agent-question-TODO="QUESTION""
                       ((org-agenda-overriding-header "Denote Agenda"))))
           ,(orgx--denote-agenda-settings "Denote Agenda: Tasks"))
          ("dn" "Denote Agenda (without agent)"
	   ((tags-todo "-agent"
                       ((org-agenda-overriding-header "Denote (non) Agent Tasks"))))
           ,(orgx--denote-agenda-settings "Denote Agenda: Non-agent tasks"))
          ("dg" "Denote Agenda (agent only)"
	   ((tags-todo "+agent-question-TODO="QUESTION""
                       ((org-agenda-overriding-header "Denote Agent Tasks"))))
           ,(orgx--denote-agenda-settings "Denote Agenda: Agent tasks"))
          ("dq" "Human Questions"
           ((tags "+question|TODO="QUESTION""
                  ((org-agenda-skip-function #'orgx-skip-unless-open-question)
                   (org-agenda-overriding-header "Human Questions"))))
           ,(orgx--denote-agenda-settings nil))
          ("dc" "Denote Composite Agenda"
           ((tags "+question|TODO="QUESTION""
                  ((org-agenda-skip-function #'orgx-skip-unless-open-question)
                   (org-agenda-overriding-header "Human questions")))
            (tags-todo "TODO="CONSIDER"|TODO="MUST""
                       ((org-agenda-overriding-header "Pending Considerations")))
            (tags-todo "TODO="INPROGRESS"|TODO="PAUSED"|TODO="SCHEDULED"|TODO="BLOCKED""
                       ((org-agenda-overriding-header "Active Tasks")))
            (tags-todo "-agent-question-TODO="QUESTION""
                       ((org-agenda-overriding-header "Tasks")))
            (tags-todo "+agent-question-TODO="QUESTION"-TODO="CONSIDER"-TODO="MUST"-TODO="INPROGRESS"-TODO="PAUSED"-TODO="SCHEDULED"-TODO="BLOCKED""
                       ((org-agenda-overriding-header "Agent tasks"))))
           ,(orgx--denote-agenda-settings nil '(orgx-denote-composite-files)))
          ("dx" "Completed Plans"
           ((tags "+question|TODO="QUESTION""
                  ((org-agenda-skip-function #'orgx-skip-unless-open-question)
                   (org-agenda-overriding-header "Open Questions in Completed Plans")))
            (tags-todo "TODO="INPROGRESS"|TODO="PAUSED"|TODO="SCHEDULED"|TODO="BLOCKED""
                       ((org-agenda-overriding-header "Active/Incomplete Tasks in Completed Plans")))
            (tags-todo "-agent-question-TODO="QUESTION""
                       ((org-agenda-overriding-header "Tasks in Completed Plans")))
            (tags-todo "+agent-question-TODO="QUESTION""
                       ((org-agenda-overriding-header "Agent Tasks in Completed Plans")))
            (tags "TODO="DONE"|TODO="ANSWERED"|TODO="COMPLETE"|TODO="CANCELED"|TODO="ABANDONED""
                  ((org-agenda-overriding-header "Completed Tasks in Completed Plans"))))
           ,(orgx--denote-agenda-settings "Completed Plans" '(orgx-denote-completed-plan-files)))
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

;; Initialize standard capture templates
(with-eval-after-load 'orgx-capture
  (orgx--setup-standard-capture-templates))


;; Auxiliary package installation for this machine

(defvar orgx--auxiliary-packages
  '(org-contrib toc-org ox-gist ox-hugo ox-rst ox-leanpub ob-mermaid)
  "Supporting org packages that should be installed when org-mode loads.")

(defun orgx--install-auxiliary-packages ()
  "Install all auxiliary packages."
  (thread-last orgx--auxiliary-packages
	       (seq-remove #'package-installed-p)
	       (mapcar #'package-install-async)
	       (length)))

(provide 'setup-orgx)
;;; setup-orgx.el ends here
