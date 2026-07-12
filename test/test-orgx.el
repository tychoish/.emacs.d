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

(provide 'test-orgx)
;;; test-orgx.el ends here
