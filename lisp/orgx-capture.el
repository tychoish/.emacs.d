;;; orgx-capture.el --- org-capture template DSL and registration -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, capture, convenience

;;; Commentary:
;; Capture template registration and builder DSL for `orgx'.  Defines flat
;; note, journal, and task template families, project-file registration
;; macros, customizable standard template sets, and soft-loaded hooks for
;; Denote and agent-shell-queue.
;;
;; The interactive `orgx-capture' command lives in `orgx.el'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(eval-when-compile
  (require 'org))

(declare-function denote-org-capture "denote")
(declare-function denote-last-path "denote")
(declare-function denote-journal-capture-entry-for-date "denote-journal-capture")
(declare-function denote-journal-capture-entry-today "denote-journal-capture")
(declare-function denote-journal-capture-timestamp "denote-journal-capture")
(declare-function agent-shell-queue-capture-from-context "agent-shell-queue")
(declare-function annotated-completing-read "annotated-completing-read")
(declare-function org-agenda-files "org")

(defvar org-capture-templates nil)
(defvar org-capture-plist nil)
(defvar org-stored-links nil)
(defvar org-directory)

(defgroup orgx-capture nil
  "Capture template DSL and registration for `orgx'."
  :group 'org-capture
  :prefix "orgx-capture-")

(defcustom orgx-capture-default-notes-file "records.org"
  "Default file name for general note capture templates."
  :type 'string
  :group 'orgx-capture)

(defcustom orgx-capture-default-journal-file "journal.org"
  "Default file name for general journal capture templates."
  :type 'string
  :group 'orgx-capture)

(defcustom orgx-capture-default-tasks-file "planner.org"
  "Default file name for primary task capture templates."
  :type 'string
  :group 'orgx-capture)

(defcustom orgx-capture-default-agent-file "agentic.org"
  "Default file name for agent task and journal capture templates."
  :type 'string
  :group 'orgx-capture)

;;; External package integrations

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

;;; Reset and helper functions

;;;###autoload
(defun orgx-reset-capture-templates ()
  "Reset `org-capture-templates' to the standard base categories."
  (interactive)
  (setq org-capture-templates '(("t" "tasks")
                                ("j" "journal")
                                ("n" "notes")
                                ("a" "agent"))))

(defun ~title~ ()
  "Read a title string interactively during `org-capture' template expansion."
  (read-string "title => "))

(defun orgx--capture-prompt-link ()
  "Interactively select or create an Org link.
If `org-stored-links' is non-nil, offers candidates via ACR/completing-read.
Otherwise prompts for a link target and optional description."
  (let* ((candidates (seq-map (lambda (item)
                                (let ((link (car item))
                                      (desc (cadr item)))
                                  (cons (if (and desc (not (string-empty-p desc)))
                                            (format "%s (%s)" desc link)
                                          link)
                                        item)))
                              (bound-and-true-p org-stored-links)))
         (choice (cond
                  ((and candidates (fboundp 'annotated-completing-read))
                   (annotated-completing-read
                    candidates
                    :prompt "Select stored org-link (or type new): "
                    :require-match nil))
                  (candidates
                   (completing-read "Select stored org-link (or type new): "
                                    candidates nil nil))
                  (t
                   (read-string "org-link (URL/file/path) => ")))))
    (if-let* ((found (assoc choice candidates)))
        (let ((link (cadr found))
              (desc (caddr found)))
          (if (and desc (not (string-empty-p desc)))
              (format "[[%s][%s]]" link desc)
            (format "[[%s]]" link)))
      (if (string-empty-p choice)
          ""
        (let ((desc (read-string "link description (optional) => ")))
          (if (not (string-empty-p desc))
              (format "[[%s][%s]]" choice desc)
            (format "[[%s]]" choice)))))))

(defun orgx--capture-get-link ()
  "Return an Org link string `[[link][description]]` for capture expansion.
First checks if an annotation link was captured or stored in `org-stored-links'.
If unavailable, interactively prompts the user to select or create a link."
  (let ((annotation (when (boundp 'org-capture-plist)
                      (plist-get org-capture-plist :annotation))))
    (cond
     ((and (stringp annotation) (not (string-empty-p annotation)))
      annotation)
     ((bound-and-true-p org-stored-links)
      (let ((latest (car org-stored-links)))
        (if (cadr latest)
            (format "[[%s][%s]]" (car latest) (cadr latest))
          (format "[[%s]]" (car latest)))))
     (t
      (orgx--capture-prompt-link)))))

;;; Flat template generator engine

(cl-defun orgx--capture-add-flat-templates
    (&key kind char name path (key "") target body-fn first-sub default-subs
          (prepend t) (time-prompt-suffix nil))
  "Register a flat set of capture templates of KIND for NAME at PATH.

CHAR is the single-letter kind prefix (\"t\", \"j\", \"n\").  KEY is
the project prefix; an empty string disables the hierarchical
\"<key><suffix>\" and shortcut \"<char><key>\" entries.

TARGET is the org-capture target form.  BODY-FN is called with
two arguments: the per-template body anchor string and a boolean flag
indicating whether the link is placed directly on the heading line.
It returns the template body string.

FIRST-SUB is (ANCHOR . DESCRIPTION) for the \"<char><key>\" shortcut,
added only when KEY is non-empty.

DEFAULT-SUBS is a list of (SUFFIX ANCHOR DESCRIPTION &rest EXTRA-PROPS);
each becomes a \"<key><suffix>\" template.

PREPEND becomes the templates' :prepend value.  When a key sequence
ends with TIME-PROMPT-SUFFIX, the template is marked :time-prompt t."
  (when (string-equal char key)
    (user-error "Cannot define %s %s org-capture-templates with key `%s'" kind name char))

  (let (specs append-item)
    (unless (string-equal "" key)
      (setq append-item t)
      (add-to-list 'org-capture-templates
                   (list (concat key char)
                         (format "%s %s <%s>" name kind (file-name-nondirectory path))))
      (push (list (concat char key) (car first-sub) (cdr first-sub)) specs)
      (dolist (sub default-subs)
        (let ((suffix (car sub)))
          (when (and (string-prefix-p char suffix)
                     (or (string-suffix-p "q" suffix)
                         (string-suffix-p "b" suffix)
                         (string-suffix-p "h" suffix)))
            (let ((shortcut-key (concat (substring suffix 0 (length char)) key (substring suffix (length char)))))
              (push (cons shortcut-key (cdr sub)) specs))))))
    (dolist (entry (append (nreverse specs)
                           (seq-map (lambda (it) (cons (concat key (car it)) (cdr it)))
                                    default-subs)))
      (let* ((key-sequence (nth 0 entry))
             (template-anchor (nth 1 entry))
             (description (nth 2 entry))
             (extra-props (nthcdr 3 entry))
             (heading-anchor-p (plist-get extra-props :heading-anchor))
             (template (append (list key-sequence
                                     (format "%s (%s; %s)" kind name description)
                                     'entry target
                                     (funcall body-fn template-anchor heading-anchor-p)
                                     :prepend prepend
                                     :kill-buffer t
                                     :empty-lines 0)
                               extra-props)))
        (when (and time-prompt-suffix
                   (string-suffix-p time-prompt-suffix key-sequence))
          (setq template (append template (list :time-prompt t))))
        (add-to-list 'org-capture-templates template append-item)))))

;;; Public template builder entrypoints

;;;###autoload
(cl-defun orgx-capture-add-journal-templates (&key name path (key ""))
  "Register a full set of journal capture templates for NAME at PATH."
  (orgx--capture-add-flat-templates
   :kind "journal" :char "j" :name name :path path :key key
   :target (if (string-equal "" key)
               (list 'file+olp+datetree path)
             (list 'file+olp+datetree path "Journal"))
   :body-fn (lambda (anchor &optional heading-p)
              (cond
               (heading-p
                (concat "* " anchor " <%<%Y-%m-%d %H:%M>>%?"))
               ((string-empty-p anchor)
                "* %(~title~) <%<%Y-%m-%d %H:%M>>%?")
               (t
                (concat "* %(~title~) <%<%Y-%m-%d %H:%M>>\n" anchor "%?"))))
   :first-sub (cons "" "<today>")
   :default-subs '(("jj" "" "<today>")
                   ("jp" "" "<date prompt>")
                   ("jx" "%x" "X11 buffer")
                   ("jlb" "%(orgx--capture-get-link)" "org-link (body)")
                   ("jlh" "%(orgx--capture-get-link)" "org-link (heading)" :heading-anchor t)
                   ("jlq" "%(orgx--capture-prompt-link)" "quick org-link (heading)" :heading-anchor t :immediate-finish t)
                   ("jk" "%c" "emacs kill-ring"))
   :prepend nil
   :time-prompt-suffix "jp"))

;;;###autoload
(cl-defun orgx-capture-add-task-templates (&key name path (key ""))
  "Register a full set of task capture templates for NAME at PATH."
  (orgx--capture-add-flat-templates
   :kind "tasks" :char "t" :name name :path path :key key
   :target (list 'file+headline path "Tasks")
   :body-fn (lambda (anchor &optional heading-p)
              (cond
               (heading-p
                (concat "* TODO " anchor "%?"))
               ((string-empty-p anchor)
                "* TODO %(~title~)%?")
               (t
                (concat "* TODO %(~title~)\n" anchor "%?"))))
   :first-sub (cons "%i" "selection")
   :default-subs '(("tt" "%i" "selection")
                   ("tq" "" "quick task" :immediate-finish t)
                   ("tx" "%x" "X11 buffer")
                   ("tlb" "%(orgx--capture-get-link)" "org-link (body)")
                   ("tlh" "%(orgx--capture-get-link)" "org-link (heading)" :heading-anchor t)
                   ("tlq" "%(orgx--capture-prompt-link)" "quick org-link task" :heading-anchor t :immediate-finish t)
                   ("tk" "%c" "emacs kill-ring"))))

;;;###autoload
(cl-defun orgx-capture-add-note-templates (&key name path (key ""))
  "Register a full set of note capture templates for NAME at PATH."
  (orgx--capture-add-flat-templates
   :kind "notes" :char "n" :name name :path path :key key
   :target (list 'file+headline path "Inbox")
   :body-fn (lambda (anchor &optional heading-p)
              (cond
               (heading-p
                (concat "* " anchor "%?"))
               ((string-empty-p anchor)
                "* %(~title~)%?")
               (t
                (concat "* %(~title~)\n" anchor "%?"))))
   :first-sub (cons "%i" "selection")
   :default-subs '(("nn" "%i" "selection")
                   ("nx" "%x" "X11 buffer")
                   ("nlb" "%(orgx--capture-get-link)" "org-link (body)")
                   ("nlh" "%(orgx--capture-get-link)" "org-link (heading)" :heading-anchor t)
                   ("nlq" "%(orgx--capture-prompt-link)" "quick org-link (heading)" :heading-anchor t :immediate-finish t)
                   ("nk" "%c" "emacs kill-ring"))))

;;; Registration helpers

;;;###autoload
(defmacro orgx-add-project-file-capture-templates (&rest args)
  "Register project file capture templates once `org' is loaded.
Expands to a call to `orgx--add-project-file-capture-templates',
forwarding ARGS (the same :name/:path/:key/:agenda keywords), wrapped in
`with-eval-after-load' so the templates take effect the moment `org' loads
without needing a manual, ordered setup step from a per-machine user file."
  `(with-eval-after-load 'org
     (orgx--add-project-file-capture-templates ,@args)))

(cl-defun orgx--add-project-file-capture-templates (&key name (path nil) (key "") (agenda nil))
  "Define a set of capture templates for adding notes and tasks to PATH."
  (unless (and (boundp 'org-capture-templates) org-capture-templates)
    (orgx-reset-capture-templates))

  (when (and agenda (boundp 'org-agenda-files) (not (member (expand-file-name path) (org-agenda-files))))
    (add-to-list 'org-agenda-files (expand-file-name path)))
  (when (not (equal "" key))
    (when (string-search "jnt" key)
      (error "org-capture prefix key '%s' for '%s' contains well-known prefix" key path))
    (when (string-search "xlkq" key)
      (error "org-capture prefix key '%s' for '%s' contains sub-template key" key path)))

  (unless name
    (setq name "planner"))

  (setq path (if path
                 (expand-file-name path)
               (concat (subst-char-in-string ?\s ?- (downcase name)) ".org")))

  (add-to-list 'org-capture-templates
               (list key (format "%s (project; %s)" name (file-name-nondirectory path)))
               t)

  (when-let* ((file-dir (file-name-directory path))
              (_ (boundp 'org-directory))
              (_ (not (string-equal file-dir (expand-file-name org-directory)))))
    (when (boundp 'org-agenda-files)
      (add-to-list 'org-agenda-files path)))

  (let ((org-filename (if (file-exists-p path)
                          path
                        (if (boundp 'org-directory)
                            (file-name-concat org-directory path)
                          path))))
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

;;;###autoload
(defun orgx--setup-standard-capture-templates ()
  "Install standard orgx capture templates using customizable default files."
  (interactive)
  (orgx-capture-add-note-templates
   :name "notes"
   :path orgx-capture-default-notes-file)

  (orgx-capture-add-journal-templates
   :name "diary"
   :path orgx-capture-default-journal-file)

  (orgx-capture-add-task-templates
   :name "prime"
   :path orgx-capture-default-tasks-file)

  (orgx-capture-add-task-templates
   :name "agentic"
   :path orgx-capture-default-agent-file
   :key "a")

  (orgx-capture-add-journal-templates
   :name "agentic"
   :path orgx-capture-default-agent-file
   :key "a"))

(provide 'orgx-capture)
;;; orgx-capture.el ends here
