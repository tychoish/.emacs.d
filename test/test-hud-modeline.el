;;; test-hud-modeline.el --- ERT tests for hud-modeline.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for hud-modeline segment rendering, delight interaction, alignment,
;; and the segment management API.
;;
;; Run in a live session:  M-x ert RET "hud-modeline--" RET
;; Run in batch:
;;   emacs -batch -l test/test-helper.el -l lisp/hud-modeline.el \
;;         -l test/test-hud-modeline.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'hud-modeline)

;; Declare as defvar so let-bindings produce dynamic (special) bindings.
;; format-mode-line reads symbol values dynamically; lexical let-bindings
;; (the default under lexical-binding: t) are invisible to it.
(defvar hud-modeline--test-inhibit nil
  "Test-only sentinel for format-mode-line conditional tests.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-mode-line / delight interaction
;;
;; format-mode-line handles 2-element (COND THEN) mode-line conditionals
;; but silently drops the ELSE branch of 3-element (COND THEN ELSE) forms.
;; Delight stores major-mode names as (INHIBIT-VAR ORIGINAL LIGHTER).
;; The C mode-line renderer handles ELSE; format-mode-line (Lisp) does not.
;;
;; Note: format-mode-line returns "" for all constructs in batch mode
;; (no display server).  Tests only assert empty-string outcomes here;
;; positive-rendering assertions live in the hud-modeline--major-mode
;; tests below, which bypass format-mode-line.

(ert-deftest hud-modeline--format-mode-line-cond-nil-returns-empty ()
  "format-mode-line (COND THEN): returns empty string when COND is nil."
  (let ((hud-modeline--test-inhibit nil))
    (should (equal "" (format-mode-line '(hud-modeline--test-inhibit "shown"))))))

(ert-deftest hud-modeline--format-mode-line-3-element-else-dropped ()
  "format-mode-line (COND THEN ELSE): ELSE is silently dropped when COND is nil.
Root cause: delight stores (INHIBIT-VAR ORIGINAL LIGHTER) in mode-name.
With INHIBIT-VAR nil the C renderer shows LIGHTER, but format-mode-line
returns empty string instead.
This behavior only manifests in batch mode without delight: delight's advice
around format-mode-line correctly returns the lighter in interactive sessions."
  (skip-unless (and noninteractive (not (featurep 'delight))))
  (let ((hud-modeline--test-inhibit nil))
    (should (equal "" (format-mode-line '(hud-modeline--test-inhibit "original" "lighter"))))))

(ert-deftest hud-modeline--delight-form-via-format-mode-line-returns-empty ()
  "format-mode-line of a delight 3-element mode-name returns empty string.
Confirms the root cause: format-mode-line cannot render the lighter without
delight's advice active.  Skipped when delight is loaded because delight's
advice on format-mode-line causes it to render the lighter instead."
  (skip-unless (and noninteractive (not (featurep 'delight))))
  (with-temp-buffer
    (setq-local mode-name '(delight-mode-name-inhibit "Fundamental" "fun"))
    (should (equal "" (format-mode-line mode-name nil nil (current-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-modeline--major-mode

(ert-deftest hud-modeline--major-mode-plain-string ()
  "`hud-modeline--major-mode' returns a plain mode-name string unchanged."
  (with-temp-buffer
    (setq-local mode-name "MyMode")
    (should (equal "MyMode" (hud-modeline--major-mode)))))

(ert-deftest hud-modeline--major-mode-truncates-long-name ()
  "`hud-modeline--major-mode' truncates names longer than `hud-modeline-mode-max-length'."
  (with-temp-buffer
    (setq-local mode-name "VeryLongModeName")
    (let ((hud-modeline-mode-max-length 8))
      (let ((result (hud-modeline--major-mode)))
        (should (equal 8 (length result)))
        (should (string-suffix-p "…" result))))))

(ert-deftest hud-modeline--major-mode-delight-form-returns-lighter ()
  "`hud-modeline--major-mode' returns LIGHTER from a delight 3-element form."
  (with-temp-buffer
    (setq-local mode-name '(delight-mode-name-inhibit "Fundamental" "fun"))
    (should (equal "fun" (hud-modeline--major-mode)))))

(ert-deftest hud-modeline--major-mode-delight-inhibited-returns-original ()
  "When the delight inhibit variable is non-nil, the original name is shown."
  (with-temp-buffer
    (setq-local mode-name '(hud-modeline--test-inhibit "Original" "lighter"))
    (let ((hud-modeline--test-inhibit t))
      (should (equal "Original" (hud-modeline--major-mode))))))

(ert-deftest hud-modeline--major-mode-delight-uninhibited-returns-lighter ()
  "When the delight inhibit variable is nil, the lighter is shown."
  (with-temp-buffer
    (setq-local mode-name '(hud-modeline--test-inhibit "Original" "lighter"))
    (let ((hud-modeline--test-inhibit nil))
      (should (equal "lighter" (hud-modeline--major-mode))))))

(ert-deftest hud-modeline--major-mode-name-captured-before-inhibit-binds ()
  "Mode name is captured correctly before the inhibit variable is bound.
This documents and tests the call-site ordering: hud-modeline--mode-segment
calls hud-modeline--major-mode first, then passes the result into
format-mode-line.  Once captured, the string is unaffected by subsequent
inhibit bindings."
  (with-temp-buffer
    (setq-local mode-name '(hud-modeline--test-inhibit "Original" "lighter"))
    (let ((hud-modeline--test-inhibit nil))
      (let ((captured (hud-modeline--major-mode)))
        (let ((hud-modeline--test-inhibit t))
          (should (equal "lighter" captured)))))))

(ert-deftest hud-modeline--major-mode-delight-symbol-void-returns-lighter ()
  "hud-modeline--major-mode returns lighter when delight-mode-name-inhibit is void.
delight calls (makunbound 'delight-mode-name-inhibit) at load time so the
variable is void between mode-line renders — this is the primary runtime path.
Regression: if this fails, hud-modeline--major-mode is likely being called
inside format-mode-line where delight's advice binds the variable to non-nil."
  (with-temp-buffer
    (setq-local mode-name '(delight-mode-name-inhibit "Fundamental" "fun"))
    (should (equal "fun" (hud-modeline--major-mode)))))

(ert-deftest hud-modeline--major-mode-non-string-lighter-passthrough ()
  "hud-modeline--major-mode passes non-string lighters through unchanged."
  (with-temp-buffer
    (setq-local mode-name '(delight-mode-name-inhibit "Emacs-Lisp"
                                                      ("el" (lexical-binding ":l" ":d"))))
    (should (equal '("el" (lexical-binding ":l" ":d"))
                   (hud-modeline--major-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-modeline--render-segments

(ert-deftest hud-modeline--render-segments-concatenates-results ()
  "render-segments joins results of all non-nil segment functions with a space."
  (let ((segs '((a . hud-modeline--test-seg-foo)
                (b . hud-modeline--test-seg-bar))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "foo"))
              ((symbol-function 'hud-modeline--test-seg-bar) (lambda () "bar")))
      (should (equal "foo bar" (hud-modeline--render-segments segs))))))

(ert-deftest hud-modeline--render-segments-skips-nil-disabled ()
  "render-segments skips segments with nil function (disabled)."
  (let ((segs '((a . hud-modeline--test-seg-foo)
                (b . nil)
                (c . hud-modeline--test-seg-bar))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "foo"))
              ((symbol-function 'hud-modeline--test-seg-bar) (lambda () "bar")))
      (should (equal "foo bar" (hud-modeline--render-segments segs))))))

(ert-deftest hud-modeline--render-segments-skips-nil-return ()
  "render-segments skips segments whose function returns nil."
  (let ((segs '((a . hud-modeline--test-seg-foo)
                (b . hud-modeline--test-seg-nil)
                (c . hud-modeline--test-seg-bar))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "foo"))
              ((symbol-function 'hud-modeline--test-seg-nil) (lambda () nil))
              ((symbol-function 'hud-modeline--test-seg-bar) (lambda () "bar")))
      (should (equal "foo bar" (hud-modeline--render-segments segs))))))

(ert-deftest hud-modeline--render-segments-empty-alist ()
  "render-segments returns empty string for an empty alist."
  (should (equal "" (hud-modeline--render-segments '()))))

(ert-deftest hud-modeline--render-segments-all-nil ()
  "render-segments returns empty string when all segments return nil."
  (let ((segs '((a . hud-modeline--test-seg-nil))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-nil) (lambda () nil)))
      (should (equal "" (hud-modeline--render-segments segs))))))

(ert-deftest hud-modeline--render-segments-keys-ignored ()
  "render-segments keys are symbols used for lookup; they do not appear in output."
  (let ((segs '((my-custom-segment . hud-modeline--test-seg-foo))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "content")))
      (let ((result (hud-modeline--render-segments segs)))
        (should (equal "content" result))
        (should-not (string-match-p "my-custom-segment" result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Segment management API

(ert-deftest hud-modeline--override-segment-replaces-function ()
  "`hud-modeline-override-segment' replaces a segment's function."
  (let ((hud-modeline-left-segments
         (copy-alist hud-modeline--default-left-segments)))
    (hud-modeline-override-segment 'hud-modeline-left-segments
                                   'state
                                   #'hud-modeline--separator)
    (should (eq #'hud-modeline--separator
                (map-elt hud-modeline-left-segments 'state)))))

(ert-deftest hud-modeline--disable-segment-sets-nil ()
  "`hud-modeline-disable-segment' sets a segment's function to nil."
  (let ((hud-modeline-left-segments
         (copy-alist hud-modeline--default-left-segments)))
    (hud-modeline-disable-segment 'hud-modeline-left-segments 'state)
    (should (null (map-elt hud-modeline-left-segments 'state)))))

(ert-deftest hud-modeline--reset-segment-restores-default ()
  "`hud-modeline-reset-segment' restores a segment to its default function."
  (let ((hud-modeline-left-segments
         (copy-alist hud-modeline--default-left-segments)))
    (hud-modeline-disable-segment 'hud-modeline-left-segments 'state)
    (hud-modeline-reset-segment 'hud-modeline-left-segments 'state)
    (should (eq #'hud-modeline--state-segment
                (map-elt hud-modeline-left-segments 'state)))))

(ert-deftest hud-modeline--reset-all-segments-restores-all ()
  "`hud-modeline-reset-all-segments' restores every segment to its default."
  (let ((hud-modeline-left-segments
         (copy-alist hud-modeline--default-left-segments)))
    (hud-modeline-disable-segment 'hud-modeline-left-segments 'state)
    (hud-modeline-disable-segment 'hud-modeline-left-segments 'buffer-name)
    (hud-modeline-reset-all-segments 'hud-modeline-left-segments)
    (should (equal hud-modeline-left-segments
                   hud-modeline--default-left-segments))))

(ert-deftest hud-modeline--disabled-segment-absent-from-output ()
  "A disabled segment contributes nothing to the rendered mode line."
  (let ((hud-modeline-left-segments
         (list (cons 'a #'hud-modeline--test-seg-foo)
               (cons 'b nil)
               (cons 'c #'hud-modeline--test-seg-bar))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "foo"))
              ((symbol-function 'hud-modeline--test-seg-bar) (lambda () "bar")))
      (should (equal "foo bar"
                     (hud-modeline--render-segments hud-modeline-left-segments))))))

(ert-deftest hud-modeline--override-segment-affects-rendering ()
  "An overridden segment's new function is used during rendering."
  (let ((hud-modeline-left-segments
         (copy-alist hud-modeline--default-left-segments)))
    (hud-modeline-override-segment 'hud-modeline-left-segments
                                   'separator
                                   #'hud-modeline--test-custom-sep)
    (cl-letf (((symbol-function 'hud-modeline--test-custom-sep)
               (lambda () "---")))
      (let ((result (hud-modeline--render-segments hud-modeline-left-segments)))
        (should (string-match-p "---" result))))))

(ert-deftest hud-modeline--default-left-segments-has-required-keys ()
  "Default left segments alist contains all expected keys."
  (seq-do (lambda (key)
            (should (assq key hud-modeline--default-left-segments)))
          '(state debug buffer-name separator modes)))

(ert-deftest hud-modeline--default-right-segments-has-required-keys ()
  "Default right segments alist contains the core misc key."
  (should (assq 'misc hud-modeline--default-right-segments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-modeline--pad

(ert-deftest hud-modeline--pad-adds-leading-space ()
  "hud-modeline--pad adds a leading space when none is present."
  (should (equal " foo" (hud-modeline--pad "foo" t))))

(ert-deftest hud-modeline--pad-no-double-leading-space ()
  "hud-modeline--pad does not add a second leading space."
  (should (equal " foo" (hud-modeline--pad " foo" t))))

(ert-deftest hud-modeline--pad-adds-trailing-space ()
  "hud-modeline--pad adds a trailing space when none is present."
  (should (equal "foo " (hud-modeline--pad "foo" nil))))

(ert-deftest hud-modeline--pad-no-double-trailing-space ()
  "hud-modeline--pad does not add a second trailing space."
  (should (equal "foo " (hud-modeline--pad "foo " nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-modeline--render alignment
;;
;; Stub window-body-width and the left/right renderers via advice so
;; tests run in batch mode.  Named stub functions are required (project
;; rule: no lambdas in advice/hooks).

(defvar hud-modeline--test-stub-lhs nil "Left content for render stubs.")
(defvar hud-modeline--test-stub-rhs nil "Right content for render stubs.")
(defvar hud-modeline--test-stub-width nil "Window width for render stubs.")

(defun hud-modeline--test-left-stub () hud-modeline--test-stub-lhs)
(defun hud-modeline--test-right-stub () hud-modeline--test-stub-rhs)
(defun hud-modeline--test-width-stub () hud-modeline--test-stub-width)

(defmacro hud-modeline-test--with-render-stubs (lhs rhs width &rest body)
  "Run BODY with hud-modeline--left, --right, and window-body-width stubbed."
  (declare (indent 3))
  `(let ((hud-modeline--test-stub-lhs ,lhs)
         (hud-modeline--test-stub-rhs ,rhs)
         (hud-modeline--test-stub-width ,width))
     (advice-add 'hud-modeline--left :override #'hud-modeline--test-left-stub)
     (advice-add 'hud-modeline--right :override #'hud-modeline--test-right-stub)
     (advice-add 'window-body-width :override #'hud-modeline--test-width-stub)
     (unwind-protect
         (progn ,@body)
       (advice-remove 'hud-modeline--left #'hud-modeline--test-left-stub)
       (advice-remove 'hud-modeline--right #'hud-modeline--test-right-stub)
       (advice-remove 'window-body-width #'hud-modeline--test-width-stub))))

(ert-deftest hud-modeline--render-uses-display-align-to ()
  "Rendered mode line uses a display align-to property for right-alignment.
The fill space between lhs and rhs carries a (space :align-to (- right N))
display property so the rhs stays flush to the window edge regardless of
content prepended to mode-line-format by packages such as anzu or popper."
  (hud-modeline-test--with-render-stubs "lhs" "rhs" 40
    (let* ((result (hud-modeline--render))
           (fill-start (+ (length " lhs") 0))
           (disp (get-text-property fill-start 'display result)))
      (should disp)
      (should (eq 'space (car (car disp))))
      (should (plist-get (cdr (car disp)) :align-to)))))

(ert-deftest hud-modeline--render-lhs-at-left-edge ()
  "Rendered mode line begins with the padded left content."
  (hud-modeline-test--with-render-stubs "lhs" "rhs" 40
    (should (string-prefix-p " lhs" (hud-modeline--render)))))

(ert-deftest hud-modeline--render-rhs-at-right-edge ()
  "Rendered mode line ends with the padded right content."
  (hud-modeline-test--with-render-stubs "lhs" "rhs" 40
    (should (string-suffix-p "rhs " (hud-modeline--render)))))

(ert-deftest hud-modeline--render-lhs-rhs-separated-by-fill ()
  "Left and right content are separated by a fill space with display alignment."
  (hud-modeline-test--with-render-stubs "lhs" "rhs" 40
    (let* ((result (hud-modeline--render))
           ;; fill space is the single space between padded lhs and rhs
           (fill-pos (length " lhs")))
      (should (string-match-p " lhs rhs " result))
      (should (get-text-property fill-pos 'display result)))))

(ert-deftest hud-modeline--render-fill-minimum-one-when-content-exceeds-width ()
  "Fill contains at least 1 space character even when lhs+rhs exceeds window width.
With display align-to, the fill is always exactly 1 propertized space character."
  (hud-modeline-test--with-render-stubs
      "this-left-side-is-very-long"
      "this-right-side-is-also-quite-long"
      20
    (let* ((result (hud-modeline--render))
           (lhs-len (length " this-left-side-is-very-long")))
      (should (> (length result) lhs-len))
      (should (equal " " (substring result lhs-len (1+ lhs-len)))))))

(ert-deftest hud-modeline--render-align-to-uses-rhs-cols ()
  "The align-to target changes when rhs width changes."
  (hud-modeline-test--with-render-stubs "lhs" "short" 80
    (let* ((r1 (hud-modeline--render))
           (d1 (get-text-property (+ (length " lhs") 0) 'display r1))
           (align1 (plist-get (cdr (car d1)) :align-to)))
      (hud-modeline-test--with-render-stubs "lhs" "much-longer-rhs" 80
        (let* ((r2 (hud-modeline--render))
               (d2 (get-text-property (+ (length " lhs") 0) 'display r2))
               (align2 (plist-get (cdr (car d2)) :align-to)))
          (should (not (equal align1 align2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-modeline-format structure
;;
;; hud-modeline-format must be a list-of-constructs (not a bare construct)
;; so that packages like anzu can safely prepend to it via `cons' without
;; tearing apart the (:eval ...) element.  When anzu does
;;   (setq mode-line-format (cons anzu-fmt mode-line-format))
;; it must get (anzu-fmt (:eval (hud-modeline--render))), not
;; (anzu-fmt :eval (hud-modeline--render)).

(ert-deftest hud-modeline--format-is-list-of-constructs ()
  "hud-modeline-format is a list whose single element is an :eval construct.
A bare (:eval ...) at the top level would be torn apart by cons-based
prependers such as anzu, producing a stray :eval keyword in mode-line-format
that Emacs renders as *invalid*."
  (should (listp hud-modeline-format))
  (should (= 1 (length hud-modeline-format)))
  (should (equal (car hud-modeline-format) '(:eval (hud-modeline--render)))))

(provide 'test-hud-modeline)
;;; test-hud-modeline.el ends here
