;;; test-orgx.el --- ERT tests for orgx.el -*- lexical-binding: t -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^orgx/")

(require 'ert)
(require 'org)
(require 'orgx)

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

(ert-deftest orgx/personal-map-f-is-command ()
  "f in orgx-minor-mode-commands-map is a command (not shadowed by archive prefix)."
  (should (commandp (keymap-lookup orgx-minor-mode-commands-map "f"))))

(ert-deftest orgx/personal-map-C-f-is-archive-submap ()
  "C-f in orgx-minor-mode-commands-map leads to the archive submap."
  (should (eq orgx-minor-mode-archive-map
              (keymap-lookup orgx-minor-mode-commands-map "C-f"))))

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
      (delete-file file))))

(provide 'test-orgx)
;;; test-orgx.el ends here
