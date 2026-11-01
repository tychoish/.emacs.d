;;; setup-orgx.el --- Personal Org mode configuration and setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal configuration, hooks, theme faces, agenda commands, and capture
;; setup for Org mode, built on `orgx' and `orgx-capture'.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'orgx)
(require 'orgx-denote)
(require 'modus-themes)

(defvar org-agenda-files)

;; Org modules and Babel

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

;; GTD Workflow: Keywords, Faces, and Tags

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "QUESTION(q)" "|" "ANSWERED(a@)")
        (sequence "INPROGRESS" "|" "COMPLETE")
        (sequence "CONSIDER(c)" "MUST(m)" "|" "ACKNOWLEDGED(k@)" "IGNORED(u)" "REJECTED(r)" "DISMISSED(p)")
        (sequence "BACKLOG(b)" "SCHEDULED(h)" "|" "GONEAWAY(g@)" "INCOMPLETE(i@)")
        (sequence "BLOCKED(o)" "PAUSED(h)" "|" "SKIPPED(k@)" "CANCELED(x@)" "INCOMPLETE(i@)" "ABANDONED(@l)")))

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
        ("DISMISSED" . (:foreground ,(modus-themes-get-color-value 'fg-dim)))
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

;; Refile targets (file scope)

(setq org-refile-targets '((org-agenda-files :maxlevel . 4)))

;; Agenda custom commands and views

(setq org-agenda-sticky t)
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

(setq org-agenda-skip-function-global #'orgx-skip-child-of-project-tag)

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

;; Initialize orgx defaults, templates, and agenda integration

(orgx-setup)

;; Hooks and minor mode activation

(add-hook 'org-mode-hook #'orgx-mode-turn-on)
(add-hook 'org-agenda-mode-hook #'orgx-agenda-minor-mode-turn-on)
(add-hook 'org-capture-mode-hook #'orgx-mode-turn-on)

(provide 'setup-orgx)
;;; setup-orgx.el ends here
