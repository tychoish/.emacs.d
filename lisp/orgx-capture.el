;;; orgx-capture.el --- org-capture template configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Capture template registration for `orgx'.  Defines the flat
;; note/journal/task template families, the project-file registration
;; helper, the standard template set installed at startup, and the
;; integration templates for denote and agent-shell-queue.
;; The interactive `orgx-capture' selector itself lives in `orgx.el'.

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'xtdlib))

(declare-function denote-org-capture "denote")
(declare-function denote-last-path "denote")
(declare-function denote-journal-capture-entry-for-date "denote-journal-capture")
(declare-function denote-journal-capture-entry-today "denote-journal-capture")
(declare-function agent-shell-queue-capture-from-context "agent-shell-queue")

(defvar org-capture-templates nil)

;; Capture template integrations for external packages.

(with-eval-after-load 'denote
  (add-to-list 'org-capture-templates
               '("dn" "denote note" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

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

(with-eval-after-load 'agent-shell-queue
  (add-to-list 'org-capture-templates
               '("q" "agent queue item" plain
                 (function ignore)
                 ""
                 :immediate-finish t
                 :before-finalize (lambda ()
                                    (call-interactively
                                     #'agent-shell-queue-capture-from-context)))))

;; org-capture-templates

(defun orgx-reset-capture-templates ()
  (setq org-capture-templates '(("t" "tasks")
                                ("j" "journal")
                                ("n" "notes")
                                ("a" "agent"))))

(defun ~title~ ()
  "Read a title string interactively during `org-capture' template expansion."
  (read-string "title => "))

(cl-defun orgx--capture-add-flat-templates
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

(cl-defun orgx-capture-add-journal-templates (&key name path (key ""))
  (orgx--capture-add-flat-templates
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

(cl-defun orgx-capture-add-task-templates (&key name path (key ""))
  (orgx--capture-add-flat-templates
   :kind "tasks" :char "t" :name name :path path :key key
   :target (list 'file+headline path "Tasks")
   :body-fn (lambda (anchor) (concat "* TODO %(~title~)\n" anchor "\n%?"))
   :first-sub (cons "%i" "selection")
   :default-subs '(("tt" "%i" "selection")
                   ("tx" "%x" "X11 buffer")
                   ("tl" "%a" "org-link")
                   ("tk" "%c" "emacs kill-ring"))))

(cl-defun orgx-capture-add-note-templates (&key name path (key ""))
  (orgx--capture-add-flat-templates
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
(defmacro orgx-add-project-file-capture-templates (&rest args)
  "Register project file capture templates once `org' is loaded.
Expands to a call to `orgx--add-project-file-capture-templates',
forwarding ARGS (the same :name/:path/:key/:agenda keywords), wrapped in
`with-eval-after-load' so the templates take effect the moment `org' loads
--- including when `org' loads because the user just ran `org-capture' ---
without needing a manual, ordered setup step from a per-machine user file.
Also avoids forcing `org' to load during init merely to register templates."
  `(with-eval-after-load 'org
     (orgx--add-project-file-capture-templates ,@args)))

(cl-defun orgx--add-project-file-capture-templates (&key name (path nil) (key "") (agenda nil))
  "Defines a set of capture mode templates for adding notes and tasks to a file.
Called via the `orgx-add-project-file-capture-templates' macro, which
defers invocation until `org' is loaded.  Call this directly only from code
that already runs after `org' is loaded."
  (unless (and (boundp 'org-capture-templates) org-capture-templates)
    (orgx-reset-capture-templates))

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

    (orgx-capture-add-note-templates
     :name name
     :key key
     :path org-filename)

    (orgx-capture-add-journal-templates
     :name name
     :key key
     :path org-filename)

    (orgx-capture-add-task-templates
     :name name
     :key key
     :path org-filename)))

;; org capture templates definitions
(defun orgx--setup-standard-capture-templates ()
  (orgx-capture-add-note-templates
   :name "notes"
   :path "records.org")

  (orgx-capture-add-journal-templates
   :name "diary"
   :path "journal.org")

  (orgx-capture-add-task-templates
   :name "prime"
   :path "planner.org")

  (orgx-capture-add-task-templates
   :name "agentic"
   :path "agentic.org"
   :key "a")

  (orgx-capture-add-journal-templates
   :name "agentic"
   :path "agentic.org"
   :key "a"))

(provide 'orgx-capture)
;;; orgx-capture.el ends here
