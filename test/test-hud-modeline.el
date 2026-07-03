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
;;
;; Alist values are now hud-modeline-segment structs.  Non-struct values
;; are filtered out by `hud-modeline-segment-p'; structs with enabled=nil
;; are filtered before the fn is called.

(ert-deftest hud-modeline--render-segments-concatenates-results ()
  "render-segments joins results of all enabled segment functions with a space."
  (let ((segs (list (cons 'a (hud-modeline--make-segment :fn #'hud-modeline--test-seg-foo))
                    (cons 'b (hud-modeline--make-segment :fn #'hud-modeline--test-seg-bar)))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "foo"))
              ((symbol-function 'hud-modeline--test-seg-bar) (lambda () "bar")))
      (should (equal "foo bar" (hud-modeline--render-segments segs))))))

(ert-deftest hud-modeline--render-segments-skips-disabled ()
  "render-segments skips segments with enabled=nil."
  (let ((segs (list (cons 'a (hud-modeline--make-segment :fn #'hud-modeline--test-seg-foo))
                    (cons 'b (hud-modeline--make-segment :fn #'hud-modeline--test-seg-bar
                                                         :enabled nil))
                    (cons 'c (hud-modeline--make-segment :fn #'hud-modeline--test-seg-bar)))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "foo"))
              ((symbol-function 'hud-modeline--test-seg-bar) (lambda () "bar")))
      (should (equal "foo bar" (hud-modeline--render-segments segs))))))

(ert-deftest hud-modeline--render-segments-skips-nil-return ()
  "render-segments skips segments whose function returns nil."
  (let ((segs (list (cons 'a (hud-modeline--make-segment :fn #'hud-modeline--test-seg-foo))
                    (cons 'b (hud-modeline--make-segment :fn #'hud-modeline--test-seg-nil))
                    (cons 'c (hud-modeline--make-segment :fn #'hud-modeline--test-seg-bar)))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "foo"))
              ((symbol-function 'hud-modeline--test-seg-nil) (lambda () nil))
              ((symbol-function 'hud-modeline--test-seg-bar) (lambda () "bar")))
      (should (equal "foo bar" (hud-modeline--render-segments segs))))))

(ert-deftest hud-modeline--render-segments-empty-alist ()
  "render-segments returns empty string for an empty alist."
  (should (equal "" (hud-modeline--render-segments '()))))

(ert-deftest hud-modeline--render-segments-all-nil ()
  "render-segments returns empty string when all segment functions return nil."
  (let ((segs (list (cons 'a (hud-modeline--make-segment :fn #'hud-modeline--test-seg-nil)))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-nil) (lambda () nil)))
      (should (equal "" (hud-modeline--render-segments segs))))))

(ert-deftest hud-modeline--render-segments-keys-ignored ()
  "render-segments keys are symbols for lookup only; they do not appear in output."
  (let ((segs (list (cons 'my-custom-segment
                          (hud-modeline--make-segment :fn #'hud-modeline--test-seg-foo)))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "content")))
      (let ((result (hud-modeline--render-segments segs)))
        (should (equal "content" result))
        (should-not (string-match-p "my-custom-segment" result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Segment management API

(defun hud-modeline--test-seg-alist (&rest pairs)
  "Build a segment alist from PAIRS of (KEY FN &optional ENABLED).
ENABLED defaults to t when omitted."
  (seq-map (lambda (pair)
             (cons (car pair)
                   (hud-modeline--make-segment
                    :fn (cadr pair)
                    :enabled (if (cddr pair) (caddr pair) t))))
           pairs))

(ert-deftest hud-modeline--override-segment-replaces-function ()
  "`hud-modeline-override-segment' updates the fn field of the segment struct."
  (let ((hud-modeline-left-segments
         (hud-modeline--test-seg-alist '(state hud-modeline--state-segment))))
    (hud-modeline-override-segment 'hud-modeline-left-segments 'state #'hud-modeline--pad)
    (should (eq #'hud-modeline--pad
                (hud-modeline-segment-fn (map-elt hud-modeline-left-segments 'state))))))

(ert-deftest hud-modeline--disable-segment-sets-enabled-nil ()
  "`hud-modeline-disable-segment' sets the segment's enabled field to nil."
  (let ((hud-modeline-left-segments
         (hud-modeline--test-seg-alist '(state hud-modeline--state-segment))))
    (hud-modeline-disable-segment 'hud-modeline-left-segments 'state)
    (should (null (hud-modeline-segment-enabled
                   (map-elt hud-modeline-left-segments 'state))))))

(ert-deftest hud-modeline--enable-segment-restores-enabled ()
  "`hud-modeline-enable-segment' sets the segment's enabled field back to non-nil."
  (let ((hud-modeline-left-segments
         (hud-modeline--test-seg-alist '(state hud-modeline--state-segment))))
    (hud-modeline-disable-segment 'hud-modeline-left-segments 'state)
    (hud-modeline-enable-segment  'hud-modeline-left-segments 'state)
    (should (hud-modeline-segment-enabled
             (map-elt hud-modeline-left-segments 'state)))))

(ert-deftest hud-modeline--disabled-segment-absent-from-output ()
  "A segment with enabled=nil contributes nothing to the rendered mode line."
  (let ((hud-modeline-left-segments
         (list (cons 'a (hud-modeline--make-segment :fn #'hud-modeline--test-seg-foo))
               (cons 'b (hud-modeline--make-segment :fn #'hud-modeline--test-seg-bar
                                                    :enabled nil))
               (cons 'c (hud-modeline--make-segment :fn #'hud-modeline--test-seg-bar)))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "foo"))
              ((symbol-function 'hud-modeline--test-seg-bar) (lambda () "bar")))
      (should (equal "foo bar"
                     (hud-modeline--render-segments hud-modeline-left-segments))))))

(ert-deftest hud-modeline--override-segment-affects-rendering ()
  "An overridden segment's new function is used during rendering."
  (let ((hud-modeline-left-segments
         (hud-modeline--test-seg-alist '(foo hud-modeline--test-seg-foo)
                                       '(sep hud-modeline--test-seg-bar))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "foo"))
              ((symbol-function 'hud-modeline--test-seg-bar) (lambda () "old")))
      (hud-modeline-override-segment 'hud-modeline-left-segments 'sep
                                     #'hud-modeline--test-custom-sep)
      (cl-letf (((symbol-function 'hud-modeline--test-custom-sep) (lambda () "---")))
        (should (string-match-p "---"
                                (hud-modeline--render-segments hud-modeline-left-segments)))))))

(ert-deftest hud-modeline--left-segments-has-required-keys ()
  "Live left segments alist contains all expected keys."
  (seq-do (lambda (key)
            (should (assq key hud-modeline-left-segments)))
          '(state icon buffer-name separator modes)))

(ert-deftest hud-modeline--right-segments-has-required-keys ()
  "Live right segments alist contains the misc key."
  (should (assq 'misc hud-modeline-right-segments)))

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
;; hud-modeline--buffer-name

(ert-deftest hud-modeline--buffer-name-renamed-shows-full-name ()
  "When the buffer has been explicitly renamed, show the full buffer name.
Regression: hud-modeline was showing the project-relative file path for
renamed buffers (e.g. those renamed by denote-rename-buffer-mode) because
it preferred file-relative-name whenever projectile was active."
  (with-temp-buffer
    (setq-local buffer-file-name
                "/home/user/notes/20260702T123456--my-title__tag.md")
    (rename-buffer "my-title <tag>" t)
    (cl-letf (((symbol-function 'projectile-project-root)
               (lambda () "/home/user/notes/"))
              ((symbol-function 'projectile-mode) (lambda () t)))
      (let* ((result (hud-modeline--buffer-name-segment))
             (text (substring-no-properties result)))
        (should (equal "my-title <tag>" text))))))

(ert-deftest hud-modeline--buffer-name-unrenamed-uses-relative-path ()
  "When buffer name matches file basename and projectile provides a root,
show the project-relative file path."
  (with-temp-buffer
    (setq-local buffer-file-name "/home/user/proj/src/foo.el")
    (rename-buffer "foo.el" t)
    (cl-letf (((symbol-function 'projectile-project-root)
               (lambda () "/home/user/proj/"))
              ((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'projectile-project-root)
                     t
                   (funcall #'fboundp sym)))))
      (let* ((result (hud-modeline--buffer-name-segment))
             (text (substring-no-properties result)))
        (should (equal "src/foo.el" text))))))

(ert-deftest hud-modeline--buffer-name-no-file-uses-buffer-name ()
  "Scratch and other non-file buffers use buffer-name directly."
  (with-temp-buffer
    (let* ((result (hud-modeline--buffer-name-segment))
           (text (substring-no-properties result)))
      (should (equal (buffer-name) text)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-modeline--left / hud-modeline--right padding

(ert-deftest hud-modeline--left-has-leading-space ()
  "`hud-modeline--left' output begins with a space."
  (let ((hud-modeline-left-segments
         (list (cons 'a (hud-modeline--make-segment :fn #'hud-modeline--test-seg-foo)))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "content")))
      (should (string-prefix-p " " (hud-modeline--left))))))

(ert-deftest hud-modeline--right-has-trailing-space ()
  "`hud-modeline--right' output ends with a space."
  (let ((hud-modeline-right-segments
         (list (cons 'a (hud-modeline--make-segment :fn #'hud-modeline--test-seg-foo)))))
    (cl-letf (((symbol-function 'hud-modeline--test-seg-foo) (lambda () "content")))
      (should (string-suffix-p " " (hud-modeline--right))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-modeline-format structure
;;
;; hud-modeline-format must be a list-of-constructs so that packages like
;; anzu that prepend to `mode-line-format' via `cons' get a proper list.
;; Right-alignment is delegated to `mode-line-format-right-align'.

(ert-deftest hud-modeline--format-is-list-with-right-align ()
  "hud-modeline-format is a list containing mode-line-format-right-align.
The symbol must appear between the left and right :eval constructs so that
Emacs right-aligns the right side without any manual width calculation."
  (should (listp hud-modeline-format))
  (should (memq 'mode-line-format-right-align hud-modeline-format))
  (should (equal (car hud-modeline-format) '(:eval (hud-modeline--left))))
  (should (equal (car (last hud-modeline-format)) '(:eval (hud-modeline--right)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-modeline--icon error handling

(ert-deftest hud-modeline--icon-returns-nil-on-nerd-icons-error ()
  "`hud-modeline--icon' returns nil when `nerd-icons-icon-for-buffer' signals an error.
Regression: a missing icon name (e.g. nf-cod-list-ordered) previously propagated
an unhandled error through the mode-line :eval form, silencing the entire modeline."
  (let ((hud-modeline-icons t)
        (hud-modeline--icon-cache nil))
    (cl-letf (((symbol-function 'nerd-icons-icon-for-buffer)
               (lambda () (error "Unable to find icon with name 'nf-cod-list-ordered' in icon set 'codicon'"))))
      (should (null (hud-modeline--icon-segment))))))

(ert-deftest hud-modeline--icon-caches-none-sentinel-on-error ()
  "`hud-modeline--icon' caches `none' sentinel after an error so subsequent calls skip the lookup."
  (let ((hud-modeline-icons t)
        (hud-modeline--icon-cache nil))
    (cl-letf (((symbol-function 'nerd-icons-icon-for-buffer)
               (lambda () (error "icon lookup failed"))))
      (hud-modeline--icon-segment)
      (should (eq 'none hud-modeline--icon-cache)))))

(provide 'test-hud-modeline)
;;; test-hud-modeline.el ends here
