;;; orgx-capture-shell.el --- Non-modal capture shell and REPL for Org mode -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, capture, convenience, comm

;;; Commentary:
;; A non-modal comint REPL and capture shell for Org mode.
;; Provides continuous, rapid logging into Org headings without interrupting
;; window configurations or entering modal capture buffers.
;;
;; Features:
;; - Comint-based REPL with customizable prompt and completion
;; - Cascading target resolution via `orgx-capture-shell-target-directory-alist'
;; - Dedicated bottom popup window (15 lines) with toggle command
;; - Slash commands (/target, /template, /denote, /undo, /recent, /help)
;; - Undo stack for rolling back captured entries

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'transient)

(declare-function annotated-completing-read "annotated-completing-read")
(declare-function denote-directory-files "denote")
(declare-function denote-retrieve-filename-title "denote")

(defgroup orgx-capture-shell nil
  "Continuous capture shell and REPL for Org mode."
  :group 'orgx
  :prefix "orgx-capture-shell-")

(defcustom orgx-capture-shell-buffer-name "*orgx-capture-shell*"
  "Name of the orgx capture shell buffer."
  :type 'string
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-default-file "agentic.org"
  "Default target Org file when no directory-specific target matches."
  :type 'string
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-default-headline "Tasks"
  "Default target headline within the target file."
  :type 'string
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-target-directory-alist nil
  "Alist mapping directory path patterns to default (FILE . HEADLINE) targets.
Each entry is of the form (DIRECTORY-PATTERN . (FILE . HEADLINE)) or
\(DIRECTORY-PATTERN . FILE).
When capturing, if `default-directory' matches DIRECTORY-PATTERN,
that file and headline are used as the default target."
  :type '(alist :key-type (choice string regexp)
                :value-type (choice (cons string string) string))
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-popup-height 15
  "Height in lines for the bottom popup window."
  :type 'integer
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-window-rule t
  "When non-nil, automatically register display-buffer rule for bottom popup."
  :type 'boolean
  :group 'orgx-capture-shell)

;;; State variables (buffer-local)

(defvar-local orgx-capture-shell-current-file nil
  "Current target Org file for this shell buffer.")

(defvar-local orgx-capture-shell-current-headline nil
  "Current target headline within `orgx-capture-shell-current-file'.")

(defvar-local orgx-capture-shell-current-template 'task
  "Current active template type: \\='task, \\='note, \\='journal, or \\='plain.")

(defvar-local orgx-capture-shell-undo-ring nil
  "List of recently captured items: ((FILE POSITION TEXT)...).")

;;; Target resolution cascade

(defun orgx-capture-shell--resolve-target (&optional dir)
  "Resolve the default (FILE . HEADLINE) target for DIR or `default-directory'."
  (let* ((target-dir (file-name-as-directory (expand-file-name (or dir default-directory))))
         (matched nil))
    (dolist (entry orgx-capture-shell-target-directory-alist)
      (when (and (not matched)
                 (let ((pat (car entry)))
                   (if (string-prefix-p "^" pat)
                       (string-match-p pat target-dir)
                     (string-prefix-p (expand-file-name pat) target-dir))))
        (setq matched (cdr entry))))
    (cond
     ((consp matched)
      (cons (expand-file-name (car matched)) (cdr matched)))
     ((stringp matched)
      (cons (expand-file-name matched) orgx-capture-shell-default-headline))
     (t
      (let ((base-file (if (boundp 'org-directory)
                           (expand-file-name orgx-capture-shell-default-file org-directory)
                         (expand-file-name orgx-capture-shell-default-file))))
        (cons base-file orgx-capture-shell-default-headline))))))

;;; Popup window management

(defun orgx-capture-shell-register-window-rule ()
  "Register the 15-line bottom popup rule in `display-buffer-alist'."
  (interactive)
  (let ((rule `(,(regexp-quote orgx-capture-shell-buffer-name)
                (display-buffer-reuse-window display-buffer-at-bottom)
                (window-height . ,orgx-capture-shell-popup-height)
                (reusable-frames . visible))))
    (unless (assoc (car rule) display-buffer-alist)
      (add-to-list 'display-buffer-alist rule))))

(when orgx-capture-shell-window-rule
  (orgx-capture-shell-register-window-rule))

;;; Core capture engine

(defun orgx-capture-shell--format-entry (text template)
  "Format capture TEXT according to TEMPLATE (\\='task, \\='note, \\='journal, \\='plain)."
  (let ((trimmed (string-trim text)))
    (if (string-prefix-p "*" trimmed)
        trimmed
      (pcase template
        ('task
         (format "* TODO %s" trimmed))
        ('journal
         (format "* %s <%s>"
                 trimmed
                 (format-time-string "%Y-%m-%d %H:%M")))
        ('note
         (format "* %s" trimmed))
        (_
         (format "* %s" trimmed))))))

(defun orgx-capture-shell--insert-entry (file headline entry-text)
  "Insert ENTRY-TEXT under HEADLINE in FILE.
Returns a record (FILE MARKER-POS ENTRY-TEXT) for the undo ring."
  (let ((buf (or (get-file-buffer file)
                 (find-file-noselect file t))))
    (with-current-buffer buf
      (save-excursion
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((found nil))
           (if (and headline (not (string-empty-p headline)))
               (progn
                 (while (and (not found) (re-search-forward org-heading-regexp nil t))
                   (when (string-equal (org-get-heading t t t t) headline)
                     (setq found (point))))
                 (if found
                     (progn
                       (goto-char found)
                       (org-end-of-subtree t t)
                       (unless (bolp) (insert "\n"))
                       (let ((pos (point)))
                         (insert entry-text "\n")
                         (save-buffer)
                         (list file pos entry-text)))
                   ;; Headline not found: create it at top level or end of file
                   (goto-char (point-max))
                   (unless (bolp) (insert "\n"))
                   (insert "* " headline "\n")
                   (let ((pos (point)))
                     (insert entry-text "\n")
                     (save-buffer)
                     (list file pos entry-text))))
             ;; No headline: append at end of file
             (goto-char (point-max))
             (unless (bolp) (insert "\n"))
             (let ((pos (point)))
               (insert entry-text "\n")
               (save-buffer)
               (list file pos entry-text)))))))))

(defun orgx-capture-shell--undo-last ()
  "Undo the most recently captured entry from `orgx-capture-shell-undo-ring'."
  (interactive)
  (if-let* ((rec (pop orgx-capture-shell-undo-ring)))
      (let* ((file (nth 0 rec))
             (pos (nth 1 rec))
             (text (nth 2 rec))
             (buf (or (get-file-buffer file)
                      (find-file-noselect file t))))
        (with-current-buffer buf
          (save-excursion
            (org-with-wide-buffer
             (goto-char (min pos (point-max)))
             ;; Search forward/backward around pos for text
             (let ((len (+ (length text) 1)))
               (if (search-forward text (+ (point) (* len 2)) t)
                   (delete-region (match-beginning 0) (min (point-max) (+ (match-end 0) 1)))
                 (goto-char (max (point-min) (- pos (* len 2))))
                 (when (search-forward text (+ pos len) t)
                   (delete-region (match-beginning 0) (min (point-max) (+ (match-end 0) 1))))))
             (save-buffer))))
        (format "Undid capture: \"%s\" from %s" (string-trim text) (file-name-nondirectory file)))
    "Undo ring is empty."))

;;; Slash command dispatcher

(defun orgx-capture-shell--prompt-string ()
  "Return the prompt string reflecting current target."
  (let ((file-name (if orgx-capture-shell-current-file
                       (file-name-nondirectory orgx-capture-shell-current-file)
                     "none"))
        (hl (or orgx-capture-shell-current-headline "")))
    (format "orgx-capture [%s%s]> "
            file-name
            (if (string-empty-p hl) "" (format ":%s" hl)))))

(defun orgx-capture-shell--execute-slash-command (cmd-line)
  "Execute a slash command CMD-LINE (e.g. \"/target foo.org:Tasks\")."
  (let* ((parts (split-string (string-remove-prefix "/" cmd-line) "[ \t]+" t))
         (verb (downcase (car parts)))
         (args (string-join (cdr parts) " ")))
    (pcase verb
      ((or "target" "t")
       (if (string-empty-p args)
           (format "Current target: %s [%s]"
                   (or orgx-capture-shell-current-file "none")
                   (or orgx-capture-shell-current-headline "none"))
         (let* ((split (split-string args ":" t))
                (f (car split))
                (h (cadr split)))
           (setq orgx-capture-shell-current-file (expand-file-name f))
           (when h (setq orgx-capture-shell-current-headline h))
           (format "Target set to %s [%s]"
                   (file-name-nondirectory orgx-capture-shell-current-file)
                   (or orgx-capture-shell-current-headline "none")))))
      ((or "template" "tpl")
       (if (string-empty-p args)
           (format "Current template: %s" orgx-capture-shell-current-template)
         (let ((sym (intern (downcase args))))
           (if (memq sym '(task note journal plain))
               (progn
                 (setq orgx-capture-shell-current-template sym)
                 (format "Template switched to: %s" sym))
             (format "Unknown template: %s (choose task, note, journal, plain)" args)))))
      ("denote"
       (if (require 'denote nil t)
           (let* ((files (denote-directory-files))
                  (choice (completing-read "Select denote note: "
                                           (mapcar #'file-name-nondirectory files)
                                           nil t)))
             (when-let* ((full (seq-find (lambda (f) (string-equal (file-name-nondirectory f) choice)) files)))
               (setq orgx-capture-shell-current-file full)
               (setq orgx-capture-shell-current-headline nil)
               (format "Target set to denote note: %s" choice)))
         "Denote is not available."))
      ("undo"
       (orgx-capture-shell--undo-last))
      ((or "recent" "list")
       (if orgx-capture-shell-undo-ring
           (mapconcat (lambda (rec)
                        (format "- [%s] %s"
                                (file-name-nondirectory (nth 0 rec))
                                (string-trim (nth 2 rec))))
                      orgx-capture-shell-undo-ring "\n")
         "No recent captures."))
      ("clear"
       (comint-clear-buffer)
       "")
      ((or "help" "h" "?")
       (mapconcat #'identity
                  '("Available slash commands:"
                    "  /target [file[:headline]]  - Switch capture target file and headline"
                    "  /template [task|note|journal|plain] - Switch capture formatting template"
                    "  /denote                   - Pick a denote note as capture target"
                    "  /undo                     - Undo the most recently captured item"
                    "  /recent, /list            - Display recently captured items"
                    "  /clear                    - Clear the shell buffer"
                    "  /help                     - Display this help message")
                  "\n"))
      (_
       (format "Unknown command: /%s (type /help for available commands)" verb)))))

;;; Comint input filter and handler

(defun orgx-capture-shell-input-sender (proc input)
  "Send and process INPUT from the comint prompt via PROC."
  (let ((trimmed (string-trim input)))
    (unless (string-empty-p trimmed)
      (if (string-prefix-p "/" trimmed)
          (let ((msg (orgx-capture-shell--execute-slash-command trimmed)))
            (unless (string-empty-p msg)
              (comint-output-filter proc (concat msg "\n"))))
        ;; Regular capture
        (unless orgx-capture-shell-current-file
          (let ((res (orgx-capture-shell--resolve-target)))
            (setq orgx-capture-shell-current-file (car res))
            (setq orgx-capture-shell-current-headline (cdr res))))
        (let* ((formatted (orgx-capture-shell--format-entry
                           trimmed
                           orgx-capture-shell-current-template))
               (rec (orgx-capture-shell--insert-entry
                     orgx-capture-shell-current-file
                     orgx-capture-shell-current-headline
                     formatted)))
          (push rec orgx-capture-shell-undo-ring)
          (comint-output-filter
           proc
           (format "Captured -> [%s%s] %s\n"
                   (file-name-nondirectory orgx-capture-shell-current-file)
                   (if orgx-capture-shell-current-headline
                       (format ":%s" orgx-capture-shell-current-headline)
                     "")
                   formatted))))))
  ;; Update prompt with current target
  (setq-local comint-prompt-regexp (rx bol "orgx-capture " (* nonl) "> "))
  (comint-output-filter proc (orgx-capture-shell--prompt-string)))

;;; Completion at point

(defconst orgx-capture-shell-commands
  '("/target" "/template" "/denote" "/undo" "/recent" "/list" "/clear" "/help")
  "List of recognized slash commands in orgx-capture-shell.")

(defun orgx-capture-shell-completion-at-point ()
  "Completion at point function for orgx-capture-shell slash commands."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let ((word (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (when (string-prefix-p "/" word)
          (list (car bounds)
                (cdr bounds)
                orgx-capture-shell-commands
                :exclusive 'no))))))

;;; Major Mode Definition

(defvar-keymap orgx-capture-shell-mode-map
  :doc "Keymap for `orgx-capture-shell-mode'."
  :parent comint-mode-map
  "C-c C-u" #'orgx-capture-shell--undo-last
  "C-c C-k" #'comint-kill-input)

(define-derived-mode orgx-capture-shell-mode comint-mode "orgx-shell"
  "Major mode for the interactive `orgx-capture-shell' REPL.
\\{orgx-capture-shell-mode-map}"
  (setq comint-prompt-regexp (rx bol "orgx-capture " (* nonl) "> "))
  (setq comint-use-prompt-regexp t)
  (setq comint-input-sender #'orgx-capture-shell-input-sender)
  (setq-local fill-column 80)
  (add-hook 'completion-at-point-functions #'orgx-capture-shell-completion-at-point nil t)

  ;; Resolve initial target
  (let ((target (orgx-capture-shell--resolve-target)))
    (setq orgx-capture-shell-current-file (car target))
    (setq orgx-capture-shell-current-headline (cdr target))))

;;; Interactive Entrypoints

;;;###autoload
(defun orgx-capture-shell ()
  "Start or switch to the `orgx-capture-shell' REPL."
  (interactive)
  (let ((buf (get-buffer-create orgx-capture-shell-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'orgx-capture-shell-mode)
        ;; Start a dummy comint process for interactive typing and prompt
        (unless (get-buffer-process buf)
          (start-process "orgx-capture-shell" buf "cat"))
        (orgx-capture-shell-mode)
        (goto-char (point-max))
        (comint-output-filter
         (get-buffer-process buf)
         (concat "Orgx Capture Shell (type /help for commands)\n"
                 (orgx-capture-shell--prompt-string)))))
    (pop-to-buffer buf)
    buf))

;;;###autoload
(defun orgx-capture-shell-toggle ()
  "Toggle visibility of the `orgx-capture-shell' popup window.
If the shell window is visible and active, hide it and return to the previous
window.  If it is hidden or not active, display it in the bottom popup window."
  (interactive)
  (let* ((buf (get-buffer orgx-capture-shell-buffer-name))
         (win (and buf (get-buffer-window buf))))
    (if (and win (eq win (selected-window)))
        (delete-window win)
      (orgx-capture-shell))))


;;; Interactive Modality Commands & Transient Menu

(defun orgx-capture-shell-set-target-file (file &optional headline)
  "Set the capture target to FILE and optional HEADLINE."
  (interactive "fOrg file: \nsHeadline (optional): ")
  (setq orgx-capture-shell-current-file (expand-file-name file))
  (setq orgx-capture-shell-current-headline (and (not (string-empty-p headline)) headline))
  (message "Target set to %s%s"
           (file-name-nondirectory orgx-capture-shell-current-file)
           (if orgx-capture-shell-current-headline
               (format ":%s" orgx-capture-shell-current-headline)
             "")))

(defun orgx-capture-shell-set-template (template)
  "Set the active capture TEMPLATE."
  (interactive
   (list (intern (completing-read "Template: " '("task" "note" "journal" "plain") nil t))))
  (setq orgx-capture-shell-current-template template)
  (message "Template set to %s" template))

(defun orgx-capture-shell-set-denote-target ()
  "Prompt for a denote note and set it as capture target."
  (interactive)
  (if (require 'denote nil t)
      (let* ((files (denote-directory-files))
             (choice (completing-read "Select denote note: "
                                      (mapcar #'file-name-nondirectory files)
                                      nil t)))
        (when-let* ((full (seq-find (lambda (f) (string-equal (file-name-nondirectory f) choice)) files)))
          (setq orgx-capture-shell-current-file full)
          (setq orgx-capture-shell-current-headline nil)
          (message "Target set to denote note: %s" choice)))
    (user-error "Denote is not available")))

;;;###autoload (autoload 'orgx-capture-shell-transient "orgx-capture-shell" nil t)
(transient-define-prefix orgx-capture-shell-transient ()
  "Transient menu for orgx capture shell operations."
  ["Orgx Capture Shell"
   ["Actions"
    ("s" "Launch / Focus Shell" orgx-capture-shell)
    ("t" "Toggle Popup Window" orgx-capture-shell-toggle)
    ("u" "Undo Last Capture" (lambda () (interactive) (message "%s" (orgx-capture-shell--undo-last))))
    ("o" "Visit Current Target" (lambda () (interactive) (when orgx-capture-shell-current-file (find-file orgx-capture-shell-current-file))))]
   ["Switch Modality"
    ("f" "Set Target File" orgx-capture-shell-set-target-file)
    ("m" "Set Template" orgx-capture-shell-set-template)
    ("d" "Set Denote Note" orgx-capture-shell-set-denote-target)]])

(provide 'orgx-capture-shell)
;;; orgx-capture-shell.el ends here
