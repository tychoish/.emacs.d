;;; test-orgx.el --- ERT tests for orgx.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^orgx/")

(require 'ert)
(require 'org)
(require 'orgx)
;; `orgx-minor-mode-turn-on' is hooked onto `org-mode-hook' in tychoish-core's
;; `use-package orgx' `:init' block, not in orgx.el itself.
(require 'tychoish-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; orgx--parse-heading-date

(ert-deftest orgx/parse-heading-date-inactive ()
  "Returns time for an inactive timestamp in the heading."
  (let ((result (orgx--parse-heading-date "[2024-03-15 Fri] My note")))
    (should result)
    (let ((decoded (decode-time result)))
      (should (= 2024 (nth 5 decoded)))
      (should (= 3 (nth 4 decoded)))
      (should (= 15 (nth 3 decoded))))))

(ert-deftest orgx/parse-heading-date-active ()
  "Returns time for an active timestamp in the heading."
  (let ((result (orgx--parse-heading-date "<2023-11-01 Wed 09:30> Meeting notes")))
    (should result)
    (let ((decoded (decode-time result)))
      (should (= 2023 (nth 5 decoded)))
      (should (= 11 (nth 4 decoded)))
      (should (= 1 (nth 3 decoded))))))

(ert-deftest orgx/parse-heading-date-no-timestamp ()
  "Returns nil when the heading contains no timestamp."
  (should-not (orgx--parse-heading-date "Plain heading with no date")))

(ert-deftest orgx/parse-heading-date-empty ()
  "Returns nil for an empty heading string."
  (should-not (orgx--parse-heading-date "")))

(ert-deftest orgx/parse-heading-date-date-only ()
  "Returns time for a date-only inactive timestamp."
  (let ((result (orgx--parse-heading-date "[2025-06-30 Mon]")))
    (should result)
    (let ((decoded (decode-time result)))
      (should (= 2025 (nth 5 decoded)))
      (should (= 6 (nth 4 decoded)))
      (should (= 30 (nth 3 decoded))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; orgx minor-mode keymap structure

(ert-deftest orgx/minor-mode-map-is-keymap ()
  "orgx-minor-mode-map is a real keymap."
  (should (keymapp orgx-minor-mode-map)))

(ert-deftest orgx/minor-mode-map-C-c-o-is-personal-map ()
  "C-c o in orgx-minor-mode-map leads to orgx-minor-mode-commands-map."
  (should (eq orgx-minor-mode-commands-map
              (keymap-lookup orgx-minor-mode-map "C-c o"))))

(ert-deftest orgx/personal-map-f-is-archive-submap ()
  "f in orgx-minor-mode-commands-map leads to the archive submap."
  (should (eq orgx-minor-mode-archive-map
              (keymap-lookup orgx-minor-mode-commands-map "f"))))

(ert-deftest orgx/personal-map-c-is-capture-submap ()
  "c in orgx-minor-mode-commands-map leads to the capture submap."
  (should (eq orgx-minor-mode-capture-map
              (keymap-lookup orgx-minor-mode-commands-map "c"))))

(ert-deftest orgx/personal-map-no-capitals ()
  "No plain capital-letter keys in orgx-minor-mode-commands-map (W was dropped)."
  (should-not (keymap-lookup orgx-minor-mode-commands-map "W")))

(ert-deftest orgx/personal-map-has-reload-and-for-file ()
  "r (reload) and / (for-file) are in orgx-minor-mode-commands-map."
  (should (commandp (keymap-lookup orgx-minor-mode-commands-map "r")))
  (should (commandp (keymap-lookup orgx-minor-mode-commands-map "/"))))

(ert-deftest orgx/agenda-minor-mode-map-is-keymap ()
  "orgx-agenda-minor-mode-map is a real keymap."
  (should (keymapp orgx-agenda-minor-mode-map)))

(ert-deftest orgx/agenda-minor-mode-map-has-bindings ()
  "Agenda minor-mode map binds /, C-l, C-e."
  (should (commandp (keymap-lookup orgx-agenda-minor-mode-map "/")))
  (should (commandp (keymap-lookup orgx-agenda-minor-mode-map "C-l")))
  (should (commandp (keymap-lookup orgx-agenda-minor-mode-map "C-e"))))

(ert-deftest orgx/turn-on-fns-on-hooks ()
  "Named turn-on functions are registered on the mode hooks."
  (should (memq #'orgx-minor-mode-turn-on org-mode-hook))
  (should (memq #'orgx-agenda-minor-mode-turn-on org-agenda-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; orgx-denote-agenda-category

(unless 'failing-tests-are-fixed
  ;; These doens't work because something with the tempbuffers hangs
  ;; in the non-interactive case
  (ert-deftest orgx/denote-agenda-category-sequence-only ()
    "Shows only the denote sequence signature, never the title or file name."
    (let* ((file (make-temp-file "20240101T100000==3d2b--test-title__tag" nil ".org")))
      (unwind-protect
          (with-temp-buffer
            (setq buffer-file-name file)
            (insert "#+TITLE: A Much Longer Title That Would Overflow\n")
            (should (equal "3d2b" (orgx-denote-agenda-category))))
	(delete-file file))))

  (ert-deftest orgx/denote-agenda-category-empty-without-sequence ()
    "Returns an empty string when the file has no denote sequence signature."
    (let* ((file (make-temp-file "20240101T100000--test-title__tag" nil ".org")))
      (unwind-protect
          (with-temp-buffer
            (setq buffer-file-name file)
            (insert "#+TITLE: Some Title\n")
            (should (equal "" (orgx-denote-agenda-category))))
	(delete-file file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; orgx-enforce-question-answered

(ert-deftest orgx/enforce-question-answered-blocks-done ()
  "A :question: heading refuses to close as DONE."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Sample question :question:\n")
    (goto-char (point-min))
    (org-back-to-heading)
    (should-error (org-todo "DONE") :type 'user-error)
    (should (equal "TODO" (org-get-todo-state)))))

(ert-deftest orgx/enforce-question-answered-allows-answered ()
  "A :question: heading may close as ANSWERED."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Sample question :question:\n")
    (goto-char (point-min))
    (org-back-to-heading)
    (org-todo "ANSWERED")
    (should (equal "ANSWERED" (org-get-todo-state)))))

(ert-deftest orgx/enforce-question-answered-ignores-untagged-headings ()
  "A heading without :question: closes normally with DONE."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Ordinary task\n")
    (goto-char (point-min))
    (org-back-to-heading)
    (org-todo "DONE")
    (should (equal "DONE" (org-get-todo-state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; orgx--agenda-view-candidate

(ert-deftest orgx/agenda-view-candidate-pads-key-to-width ()
  "Dispatch key is left-justified to the given width before the colon."
  (should (equal "da: Denote Agenda ALL"
                 (orgx--agenda-view-candidate '("da" "Denote Agenda ALL") 2)))
  (should (equal "a : Agenda (week/day)"
                 (orgx--agenda-view-candidate '("a" "Agenda (week/day)") 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; orgx--denote-agenda-settings

(ert-deftest orgx/denote-agenda-settings-sets-header ()
  "HEADER is threaded through to org-agenda-overriding-header."
  (let ((settings (orgx--denote-agenda-settings "My Header")))
    (should (equal "My Header" (cadr (assq 'org-agenda-overriding-header settings))))))

(ert-deftest orgx/denote-agenda-settings-covers-todo-and-tags-prefix-formats ()
  "Prefix format is set for both the todo and tags agenda line types."
  (let* ((settings (orgx--denote-agenda-settings "Header"))
         (prefix-form (cadr (assq 'org-agenda-prefix-format settings)))
         (prefix-alist (eval prefix-form)))
    (should (assq 'todo prefix-alist))
    (should (assq 'tags prefix-alist))))
(ert-deftest orgx/ad-org-agenda-redo-unpopulated-buffer ()
  "ad:org-agenda-redo handles unpopulated agenda buffers gracefully."
  (with-current-buffer (get-buffer-create "*Org Agenda(test-unpopulated)*")
    (org-agenda-mode)
    (should (zerop (buffer-size)))
    (should-not (get-text-property (point-min) 'org-redo-cmd))
    ;; In an unpopulated buffer, ad:org-agenda-redo should avoid args-out-of-range: 0, 0
    ;; and attempt to prompt via orgx-agenda-view (or user-error if unbound).
    (let ((prompted nil))
      (cl-letf (((symbol-function 'orgx-agenda-view) (lambda () (setq prompted t))))
        (ad:org-agenda-redo #'ignore)
        (should prompted)))
    (kill-buffer)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-agenda custom command "dq"

(ert-deftest orgx/denote-agenda-dq-custom-command-structure ()
  "The 'dq' custom agenda command uses tags type with open question skip function."
  (require 'org-agenda)
  (let ((dq-cmd (assoc "dq" org-agenda-custom-commands)))
    (should dq-cmd)
    (should (equal "Human Questions" (nth 1 dq-cmd)))
    (let ((block (car (nth 2 dq-cmd))))
      (should (eq 'tags (nth 0 block)))
      (should (equal "+question|TODO=\"QUESTION\"" (nth 1 block))))))

(ert-deftest orgx/skip-unless-open-question-filters-open-questions ()
  "orgx-skip-unless-open-question retains open questions and skips answered, done, or inherited subheadings."
  (with-temp-buffer
    (setq-local org-todo-keywords '((sequence "TODO" "QUESTION" "INPROGRESS" "|" "ANSWERED" "DONE")))
    (org-mode)
    (org-set-regexps-and-options)
    (insert "
* Open Question A :question:
* QUESTION Open Question B
* QUESTION Open Question C :question:
* INPROGRESS Open Question D :question:
*** Context                                       :question:
*** Response                                      :question:
* ANSWERED Closed Question E :question:
* DONE Closed Question F :question:
* TODO Task G :agent:
")
    (let (results)
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\)\\s-+" nil t)
        (let ((skip (orgx-skip-unless-open-question)))
          (if skip
              (goto-char skip)
            (push (org-get-heading t t t t) results))))
      (setq results (mapcar #'substring-no-properties (nreverse results)))
      (should (equal '("Open Question A" "Open Question B" "Open Question C" "Open Question D")
                     results)))))
(ert-deftest orgx/denote-questions-command-exists ()
  "orgx-agenda-denote-questions is an interactive command targeting agenda key dq."
  (should (commandp #'orgx-agenda-denote-questions)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quick task capture templates (tq / :immediate-finish t)

(ert-deftest orgx/quick-task-templates-created ()
  "orgx-capture-add-task-templates adds tq template with :immediate-finish t."
  (let ((org-capture-templates nil))
    (orgx-capture-add-task-templates :name "test" :path "/tmp/test.org")
    (let ((tq-entry (assoc "tq" org-capture-templates)))
      (should tq-entry)
      (should (member :immediate-finish tq-entry))
      (should (eq t (cadr (member :immediate-finish tq-entry))))
      (should (string-match-p "%(~title~)" (nth 4 tq-entry))))))

(ert-deftest orgx/quick-task-templates-with-key ()
  "orgx-capture-add-task-templates with key generates <key>tq and t<key>q entries with :immediate-finish t."
  (let ((org-capture-templates nil))
    (orgx-capture-add-task-templates :name "test" :path "/tmp/test.org" :key "p")
    (let ((ptq-entry (assoc "ptq" org-capture-templates))
          (tpq-entry (assoc "tpq" org-capture-templates)))
      (should ptq-entry)
      (should tpq-entry)
      (should (member :immediate-finish ptq-entry))
      (should (member :immediate-finish tpq-entry)))))
(ert-deftest orgx/org-link-templates-use-get-link ()
  "Task, journal, and note org-link templates use orgx--capture-get-link."
  (let ((org-capture-templates nil))
    (orgx-capture-add-task-templates :name "test" :path "/tmp/test.org")
    (orgx-capture-add-journal-templates :name "test" :path "/tmp/test.org")
    (orgx-capture-add-note-templates :name "test" :path "/tmp/test.org")
    (let ((tl-entry (assoc "tl" org-capture-templates))
          (jl-entry (assoc "jl" org-capture-templates))
          (nl-entry (assoc "nl" org-capture-templates)))
      (should tl-entry)
      (should jl-entry)
      (should nl-entry)
      (should (string-match-p "%(orgx--capture-get-link)" (nth 4 tl-entry)))
      (should (string-match-p "%(orgx--capture-get-link)" (nth 4 jl-entry)))
      (should (string-match-p "%(orgx--capture-get-link)" (nth 4 nl-entry))))))

(ert-deftest orgx/quick-org-link-task-templates-created ()
  "orgx-capture-add-task-templates adds tlq, <key>tlq, and t<key>lq templates with :immediate-finish t."
  (let ((org-capture-templates nil))
    (orgx-capture-add-task-templates :name "test" :path "/tmp/test.org" :key "p")
    (let ((tlq-entry (assoc "tlq" org-capture-templates))
          (ptlq-entry (assoc "ptlq" org-capture-templates))
          (tplq-entry (assoc "tplq" org-capture-templates)))
      (should ptlq-entry)
      (should tplq-entry)
      (should (member :immediate-finish ptlq-entry))
      (should (member :immediate-finish tplq-entry))
      (should (string-match-p "%(orgx--capture-prompt-link)" (nth 4 ptlq-entry))))))

(ert-deftest orgx/capture-get-link-helper ()
  "orgx--capture-get-link returns annotation when non-empty, recent stored link when annotation is empty."
  (let ((org-capture-plist '(:annotation "[[file:/foo.el][foo.el]]"))
        (org-stored-links '(("http://example.com" "Example"))))
    (should (equal "[[file:/foo.el][foo.el]]" (orgx--capture-get-link))))
  (let ((org-capture-plist '(:annotation ""))
        (org-stored-links '(("http://example.com" "Example"))))
    (should (equal "[[http://example.com][Example]]" (orgx--capture-get-link)))))
(provide 'test-orgx)
;;; test-orgx.el ends here
