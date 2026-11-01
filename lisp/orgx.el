;;; orgx.el --- Personal org-mode extensions and navigation -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, convenience

;;; Commentary:
;; Core Org mode extensions:
;; - Date formatting and timestamp utilities
;; - Agenda file management, reload, and scoping commands
;; - Custom speed commands and headline navigation
;; - Archive management and completed task refiling
;; - Minor mode (`orgx-minor-mode') with unified keymap under `C-c o'
;; - Global keymap (`orgx-global-map')

;;; Code:

(eval-when-compile
  (require 'org-macs)
  (require 'org-element))

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'outline)
(require 'org)
(require 'org-fold)
(require 'org-capture)
(require 'org-refile)
(require 'org-agenda)
(require 'org-archive)
(require 'ol)

(autoload 'whitespace-cleanup "whitespace")
(autoload 'org-annotate-file "org-annotate-file")
(autoload 'consult-org-heading "consult")
(autoload 'agent-shell-queue-org-refile-from-heading "agent-shell-queue-org" nil t)

;; Autoload capture workflows from orgx-capture and orgx-capture-shell on demand
(autoload 'orgx-capture "orgx-capture" "Entrypoint for orgx capture workflows." t)
(autoload 'orgx-reset-capture-templates "orgx-capture" "Reset org-capture-templates.")
(autoload 'orgx-capture-shell "orgx-capture-shell" "Launch the orgx capture shell." t)
(autoload 'orgx-capture-shell-toggle "orgx-capture-shell" "Toggle the orgx capture shell popup window." t)
(autoload 'orgx-capture-shell-transient "orgx-capture-shell" "Summon transient modal interface for orgx capture shell." t)
(autoload 'orgx-migrate-subtree-to-denote "orgx-denote" "Extract Org subtree to Denote note." t)

(defvar org-link-parameters)
(defvar org-stored-links)
(defvar orgx-minor-mode)
(defvar org-state)
(defvar org-last-state)
(defvar orgx-agenda-minor-mode)

(defgroup orgx nil
  "Org-mode personal extensions."
  :group 'org)

(defcustom orgx-project-tags '("project" "task" "epic" "plan" "scope" "tracker")
  "Tags that identify projects for skip functions."
  :type '(repeat string)
  :group 'orgx)

;;; Date formatting

(defconst orgx-date-spec-datetime "<%Y-%02m-%02d %02H:%02M:%02S %Z>")
(defconst orgx-date-spec-date "<%Y-%02m-%02d>")

(cl-defun orgx-date-now (&key short)
  "Format current time into an Org active timestamp.
When SHORT is non-nil, format as date-only."
  (interactive)
  (format-time-string (if short
                          orgx-date-spec-date
                        orgx-date-spec-datetime)
                      (current-time)))

;;; TOC priority preservation (for toc-org)

(defvar orgx--toc-priorities nil)

(defun orgx--toc-collect-priorities ()
  "Collect priority markers from all headings in the current buffer."
  (let (alist)
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (when-let* ((priority (nth 3 (org-heading-components)))
                     (title (org-get-heading t t t t)))
           (push (cons title priority) alist)))))
    (nreverse alist)))

(defun orgx--toc-bind-priorities (fn &rest args)
  "Around advice for `toc-org-insert-toc': collect buffer priorities."
  (let ((orgx--toc-priorities (orgx--toc-collect-priorities)))
    (apply fn args)))

(defun orgx--toc-hrefify-priority (fn str &optional hash)
  "Around advice for `toc-org-hrefify-gh': prepend heading priority."
  (if-let* ((cell (assoc str orgx--toc-priorities)))
      (progn
        (setq orgx--toc-priorities (delq cell orgx--toc-priorities))
        (funcall fn (format "[#%c] %s" (cdr cell) str) hash))
    (funcall fn str hash)))

(advice-add 'toc-org-insert-toc :around #'orgx--toc-bind-priorities)
(advice-add 'toc-org-hrefify-gh :around #'orgx--toc-hrefify-priority)

(declare-function revbufs "revbufs")

(defun orgx--background-revbufs ()
  "Run `revbufs' without disturbing current window layout."
  (when (fboundp 'revbufs)
    (save-window-excursion (revbufs))))

(defun orgx--use-speed-commands ()
  "Speed command predicate: cursor at beginning of outline heading."
  (and (looking-at org-outline-regexp) (looking-back "^\\**" nil)))

;;; Agenda file management

;;;###autoload
(defun orgx-agenda-files-open ()
  "Open all files listed in `org-agenda-files'."
  (interactive)
  (when (boundp 'org-agenda-files)
    (seq-do (lambda (f)
              (when (file-exists-p f)
                (find-file-noselect f)))
            (org-agenda-files))))

;;;###autoload
(defun orgx-agenda-files-reload ()
  "Revert all buffers visiting `org-agenda-files' to match disk."
  (interactive)
  (when (boundp 'org-agenda-files)
    (seq-do (lambda (f)
              (when-let* ((buf (get-file-buffer f)))
                (with-current-buffer buf
                  (revert-buffer :ignore-auto :noconfirm))))
            (org-agenda-files))))

(defun ad:org-agenda--open-files (&rest _)
  "Advice to background-revert buffers when opening agenda files."
  (orgx--background-revbufs))

(defun ad:org-agenda-redo (orig-fun &rest args)
  "Advice to background-revert buffers before re-running agenda view."
  (orgx--background-revbufs)
  (apply orig-fun args))

(advice-add 'org-agenda-redo :around #'ad:org-agenda-redo)

;;; Task status and archiving

;;;###autoload
(defun orgx-mark-done-and-archive ()
  "Mark current heading DONE and execute `org-archive-default-command'."
  (interactive)
  (org-todo "DONE")
  (call-interactively org-archive-default-command))

(defun orgx-set-weekday-of-timestamp ()
  "Add day of the week to an active timestamp at point."
  (interactive)
  (let ((time (org-time-string-to-time (thing-at-point 'line t))))
    (when time
      (delete-region (line-beginning-position) (line-end-position))
      (insert (format-time-string "<%Y-%m-%d %a>" time)))))

(defun orgx-skip-child-of-project-tag ()
  "Skip children of headings tagged with any element of `orgx-project-tags'."
  (save-restriction
    (widen)
    (let (skip)
      (while (and (not skip) (org-up-heading-safe))
        (when (seq-intersection orgx-project-tags (org-get-tags nil t))
          (setq skip (save-excursion (org-end-of-subtree t) (point)))))
      skip)))

(defun orgx-done-state-match ()
  "Return an `org-map-entries' match string for all completed todo states."
  (let ((done-states (or (and (boundp 'org-done-keywords) org-done-keywords)
                         '("DONE"))))
    (concat "/" (string-join done-states "|"))))

(defun orgx-archive-completed-tasks (archive-fn label)
  "Archive completed tasks using ARCHIVE-FN, logging progress with LABEL."
  (let ((count 0)
        (matcher (orgx-done-state-match)))
    (org-map-entries
     (lambda ()
       (setq org-map-continue-from (point))
       (funcall archive-fn)
       (setq count (1+ count)))
     matcher
     nil)
    (message "Archived %d tasks to %s." count label)))

;;;###autoload
(defun orgx-archive-done-tasks-to-archive-sibling ()
  "Archive all DONE tasks in the current buffer to their archive sibling."
  (interactive)
  (orgx-archive-completed-tasks #'org-archive-to-archive-sibling "archive sibling"))

;;;###autoload
(defun orgx-archive-done-tasks-to-archive-file ()
  "Archive all DONE tasks in the current buffer to its default archive file."
  (interactive)
  (orgx-archive-completed-tasks #'org-archive-subtree "archive file"))

;;; Skip functions

(defun orgx-skip-unless-untagged ()
  "Skip headings that have tags (including inherited tags)."
  (when (org-get-tags)
    (or (outline-next-heading) (point-max))))

(defun orgx-skip-unless-open-question ()
  "Skip headings in agenda views that are not open human questions."
  (if (and (fboundp 'orgx-denote-file-completed-p)
           (bound-and-true-p orgx-denote-agenda-suppress-completed)
           (orgx-denote-file-completed-p (buffer-file-name)))
      (point-max)
    (let* ((hl (org-get-heading t t t t))
           (todo (org-get-todo-state))
           (local-tags (org-get-tags nil t))
           (is-answered (or (equal todo "ANSWERED")
                            (equal todo "DONE")
                            (and hl (string-prefix-p "ANSWERED " hl))))
           (is-metadata (member hl '("Context" "Response" "LOGBOOK" "PROPERTIES")))
           (is-sub-under-question (and (not (member "question" local-tags))
                                       (org-up-heading-safe)
                                       (member "question" (org-get-tags nil t)))))
      (if (or is-answered is-metadata is-sub-under-question)
          (save-excursion (org-end-of-subtree t) (point))
        nil))))

;;; Agenda view scoping

;;;###autoload
(defun orgx-agenda-untagged-in-file (file &optional tag todo-only inherited)
  "Show untagged tasks in FILE.
When optional TAG is supplied, search for that tag instead."
  (interactive "fFile: ")
  (let ((org-agenda-files (list file)))
    (if tag
        (org-tags-view todo-only (format "%s%s" (if inherited "" "+") tag))
      (let ((org-agenda-skip-function #'orgx-skip-unless-untagged))
        (org-tags-view t "+TODO=\"TODO\"|+TODO=\"INPROGRESS\"")))))

;;;###autoload
(defun orgx-agenda-for-file (file)
  "Show full todo agenda restricted to FILE."
  (interactive "fFile: ")
  (let ((org-agenda-files (list file)))
    (org-todo-list)))

;;; Human question workflow

;;;###autoload
(defun orgx-insert-question (question &optional context)
  "Insert an actionable human QUESTION heading with optional CONTEXT."
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
  "Block closing a :question:-tagged heading with any DONE state but ANSWERED."
  (when (and (member org-state org-done-keywords)
             (not (string= org-state "ANSWERED"))
             (member "question" (org-get-tags)))
    (org-todo org-last-state)
    (user-error "Headings tagged :question: can only be closed with ANSWERED, not %s" org-state)))

;;; Agenda view dispatcher

(defconst orgx-agenda-builtin-views
  '(("a" "Agenda (week/day)")
    ("t" "All TODOs")
    ("m" "Match tags / props / todo")
    ("s" "Search keywords"))
  "Standard built-in views for `orgx-agenda-view'.")

(defun orgx--agenda-view-candidate (entry width)
  "Format label for ENTRY with key left-padded to WIDTH."
  (concat (string-pad (car entry) width) ": " (cadr entry)))

(defun orgx--agenda-view-settings (entry)
  "Return SETTINGS alist for a custom-command ENTRY."
  (when (>= (length entry) 3)
    (if (listp (nth 2 entry))
        (when (>= (length entry) 4) (nth 3 entry))
      (when (>= (length entry) 5) (nth 4 entry)))))

(defun orgx--agenda-view-annotation (entry)
  "Return annotation description for ENTRY."
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
                         (when files-p "custom files")
                         (when (and header (not (equal header (cadr entry))))
                           header)))
       "  "))))

(defun orgx--agenda-view-group (entry)
  "Group label for ENTRY."
  (cond
   ((< (length entry) 3) "Built-in")
   ((assq 'org-agenda-files (orgx--agenda-view-settings entry)) "File-scoped")
   (t (format "Global: %s" (capitalize (format "%s" (nth 2 entry)))))))

;;;###autoload
(defun orgx-agenda-view ()
  "Select and dispatch an `org-agenda' view via completing-read."
  (interactive)
  (let* ((custom (or (and (boundp 'org-agenda-custom-commands) org-agenda-custom-commands) nil))
         (entries (append orgx-agenda-builtin-views custom))
         (max-key-len (apply #'max (mapcar (lambda (e) (length (car e))) entries)))
         (table (make-hash-table :test 'equal))
         candidates)
    (dolist (entry entries)
      (let ((cand (orgx--agenda-view-candidate entry max-key-len)))
        (puthash cand entry table)
        (push cand candidates)))
    (setq candidates (nreverse candidates))
    (let* ((collection
            (lambda (str pred action)
              (if (eq action 'metadata)
                  `(metadata
                    (category . org-agenda-view)
                    (annotation-function
                     . ,(lambda (c)
                          (when-let* ((entry (gethash c table)))
                            (concat "   " (orgx--agenda-view-annotation entry)))))
                    (group-function
                     . ,(lambda (c transform)
                          (if transform c
                            (when-let* ((entry (gethash c table)))
                              (orgx--agenda-view-group entry))))))
                (complete-with-action action candidates str pred))))
           (choice (completing-read "Agenda view: " collection nil t)))
      (when-let* ((entry (gethash choice table)))
        (org-agenda nil (car entry))))))

;;; Heading search and navigation

;;;###autoload
(defun orgx-agenda-switch-buffer ()
  "Switch to an agenda buffer or other Org file using completing-read."
  (interactive)
  (let* ((agenda-bufs (seq-filter (lambda (b)
                                    (with-current-buffer b
                                      (derived-mode-p 'org-mode 'org-agenda-mode)))
                                  (buffer-list)))
         (names (mapcar #'buffer-name agenda-bufs)))
    (if names
        (switch-to-buffer (completing-read "Org buffer: " names nil t))
      (message "No active Org or Agenda buffers."))))

(defun orgx--heading-context (point)
  "Return path of ancestor headings leading to POINT."
  (save-excursion
    (goto-char point)
    (let (path)
      (while (org-up-heading-safe)
        (push (org-get-heading t t t t) path))
      path)))

(defun orgx--format-heading-candidate (heading path file max-file-len max-path-len)
  "Format candidate line aligning FILE, PATH, and HEADING."
  (let ((file-part (string-pad (file-name-sans-extension file) max-file-len))
        (path-part (string-pad (if path (string-join path " > ") "") max-path-len)))
    (format "%s  %s  %s" file-part path-part heading)))

;;;###autoload
(defun orgx-find-heading ()
  "Jump to any heading across all `org-agenda-files' using completing-read."
  (interactive)
  (let (targets max-file-len max-path-len)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (let* ((heading (org-get-heading t t t t))
                    (path (orgx--heading-context (point)))
                    (pos (point))
                    (fname (file-name-nondirectory file)))
               (push (list fname path heading pos file) targets)
               (setq max-file-len (max (or max-file-len 0) (length (file-name-sans-extension fname))))
               (when path
                 (setq max-path-len (max (or max-path-len 0) (length (string-join path " > ")))))))))))
    (setq targets (nreverse targets))
    (let* ((candidates
            (mapcar (lambda (tgt)
                      (cons (orgx--format-heading-candidate (nth 2 tgt) (nth 1 tgt) (nth 0 tgt)
                                                            (or max-file-len 0) (or max-path-len 0))
                            tgt))
                    targets))
           (choice (completing-read "Heading: " (mapcar #'car candidates) nil t))
           (selected (cdr (assoc choice candidates))))
      (when selected
        (find-file (nth 4 selected))
        (goto-char (nth 3 selected))
        (org-fold-show-context 'agenda)))))

;;; Link handling

(defun orgx-link-github-line-support (orig-fn path in-emacs)
  "Support GitHub-style line fragments (#L123 or #L123-L145) in file links."
  (if (string-match "\\`\\(.*?\\)#L\\([0-9]+\\)\\(?:-L[0-9]+\\)?\\(?:\\(::.*\\)\\)?\\'" path)
      (let* ((real-path (match-string 1 path))
             (line (string-to-number (match-string 2 path)))
             (extra-opt (match-string 3 path))
             (new-path (if extra-opt (concat real-path extra-opt)
                         (format "%s::%d" real-path line))))
        (funcall orig-fn new-path in-emacs))
    (funcall orig-fn path in-emacs)))

(defun orgx-open-custom-id-slug-fallback (path)
  "Open custom ID link fallback."
  (when (string-prefix-p "#" path)
    (let ((custom-id (substring path 1)))
      (org-link-search (concat "#" custom-id))
      (org-fold-show-context)
      t)))

(advice-add 'org-link-open-as-file :around #'orgx-link-github-line-support)
(add-hook 'org-open-link-functions #'orgx-open-custom-id-slug-fallback)

;;; Keymaps and minor modes

(defvar-keymap orgx-minor-mode-capture-map
  :name "orgx-capture"
  :doc "Capture commands under C-c o c (orgx-minor-mode).")

(keymap-set orgx-minor-mode-capture-map "c" #'orgx-capture)
(keymap-set orgx-minor-mode-capture-map "m" #'org-capture)
(keymap-set orgx-minor-mode-capture-map "p" #'org-capture-goto-last-stored)
(keymap-set orgx-minor-mode-capture-map "l" #'org-capture-goto-last-stored)
(keymap-set orgx-minor-mode-capture-map "t" #'orgx-capture-shell-toggle)
(keymap-set orgx-minor-mode-capture-map "r" #'org-capture-refile)
(keymap-set orgx-minor-mode-capture-map "w" #'org-capture-refile)
(keymap-set orgx-minor-mode-capture-map "s" #'orgx-capture-shell)
(keymap-set orgx-minor-mode-capture-map "C-t" #'orgx-capture-shell-transient)

(defvar-keymap orgx-minor-mode-archive-map
  :name "orgx-archive"
  :doc "Archive commands under C-c o C-f (orgx-minor-mode).")

(keymap-set orgx-minor-mode-archive-map "e" #'org-cycle-force-archived)
(keymap-set orgx-minor-mode-archive-map "t" #'org-archive-set-tag)
(keymap-set orgx-minor-mode-archive-map "s" (cons "current→sibling" #'org-archive-to-archive-sibling))
(keymap-set orgx-minor-mode-archive-map "d" (cons "done+archive" #'orgx-mark-done-and-archive))

(defvar-keymap orgx-minor-mode-commands-map
  :name "orgx-commands"
  :doc "Command keymap for `orgx-minor-mode' mounted under C-c o.")

(keymap-set orgx-minor-mode-commands-map "a" #'orgx-agenda-view)
(keymap-set orgx-minor-mode-commands-map "d" #'orgx-date-now)
(keymap-set orgx-minor-mode-commands-map "h" #'consult-org-heading)
(keymap-set orgx-minor-mode-commands-map "k" #'org-capture)
(keymap-set orgx-minor-mode-commands-map "n" #'org-narrow-to-subtree)
(keymap-set orgx-minor-mode-commands-map "o" #'orgx-agenda-files-open)
(keymap-set orgx-minor-mode-commands-map "p" #'org-insert-property-drawer)
(keymap-set orgx-minor-mode-commands-map "q" #'agent-shell-queue-org-refile-from-heading)
(keymap-set orgx-minor-mode-commands-map "r" #'orgx-agenda-files-reload)
(keymap-set orgx-minor-mode-commands-map "s" #'org-save-all-org-buffers)
(keymap-set orgx-minor-mode-commands-map "C-s" #'orgx-capture-shell-toggle)
(keymap-set orgx-minor-mode-commands-map "C-t" #'orgx-capture-shell-transient)
(keymap-set orgx-minor-mode-commands-map "t" #'org-set-tags-command)
(keymap-set orgx-minor-mode-commands-map "u" #'orgx-agenda-untagged-in-file)
(keymap-set orgx-minor-mode-commands-map "w" #'org-refile)
(keymap-set orgx-minor-mode-commands-map "/" #'orgx-agenda-for-file)
(keymap-set orgx-minor-mode-commands-map "c" orgx-minor-mode-capture-map)
(keymap-set orgx-minor-mode-commands-map "C-f" orgx-minor-mode-archive-map)

(defvar-keymap orgx-minor-mode-map
  :doc "Buffer-local minor mode keymap activating commands under C-c o.")

(keymap-set orgx-minor-mode-map "C-c o" orgx-minor-mode-commands-map)

(defun orgx--set-up-buffer ()
  "Configure buffer settings for `orgx-minor-mode'."
  (setq-local fill-column 72)
  (when (fboundp 'toc-org-mode)
    (add-hook 'before-save-hook #'whitespace-cleanup nil t)))

;;;###autoload
(define-minor-mode orgx-minor-mode
  "Personal org-mode keybindings and buffer setup.
Activates `orgx-minor-mode-commands-map' under C-c o."
  :lighter " ox"
  :keymap orgx-minor-mode-map
  (when orgx-minor-mode
    (orgx--set-up-buffer)))

;;;###autoload
(defun orgx-minor-mode-turn-on ()
  "Enable `orgx-minor-mode' in the current buffer."
  (orgx-minor-mode 1))

;;; orgx-agenda-minor-mode

(defvar-keymap orgx-agenda-minor-mode-map
  :doc "Keymap for `orgx-agenda-minor-mode'.")

(keymap-set orgx-agenda-minor-mode-map "C-l" #'org-agenda-open-link)
(keymap-set orgx-agenda-minor-mode-map "M-c" #'org-agenda-goto-calendar)
(keymap-set orgx-agenda-minor-mode-map "/" #'orgx-agenda-for-file)
(keymap-set orgx-agenda-minor-mode-map "C-e" #'orgx-migrate-subtree-to-denote)
(keymap-set orgx-agenda-minor-mode-map "C-b" #'orgx-agenda-switch-buffer)
(keymap-set orgx-agenda-minor-mode-map "M-a" #'orgx-agenda-view)
(keymap-set orgx-agenda-minor-mode-map "r" #'org-agenda-refile)

;;;###autoload
(define-minor-mode orgx-agenda-minor-mode
  "Personal org-agenda keybindings and setup."
  :lighter " oxa"
  :keymap orgx-agenda-minor-mode-map
  (when orgx-agenda-minor-mode
    (orgx--background-revbufs)))

;;;###autoload
(defun orgx-agenda-minor-mode-turn-on ()
  "Enable `orgx-agenda-minor-mode' in the current buffer."
  (orgx-agenda-minor-mode 1))

(provide 'orgx)
;;; orgx.el ends here
