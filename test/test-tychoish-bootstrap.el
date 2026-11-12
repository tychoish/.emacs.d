;;; test-bootstrap.el --- ERT tests for bootstrap.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^bootstrap/")

(require 'ert)
(require 'test-helper)
(require 'sprite)
(require 'overmark)
(require 'cl-lib)
(require 'bootstrap)
(require 'eglox-project)
(require 'setup-core)

(defvar denote-directory)
(defvar deft-directory)

(defconst bootstrap-cache--buffer-name " bootstrap-cache-buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite-resolve-instance-id

(ert-deftest bootstrap/resolve-instance-id-daemon-t ()
  "Returns \"primary\" when daemonp returns t."
  (with-current-buffer (get-buffer-create bootstrap-cache--buffer-name)
    (setq bootstrap-cache--resolved-instance-id nil))
  (cl-letf (((symbol-function 'daemonp) (lambda () t)))
    (should (equal "primary" (sprite-resolve-instance-id)))))

(ert-deftest bootstrap/resolve-instance-id-named-daemon ()
  "Returns daemon name when daemonp returns a string."
  (skip-unless (not noninteractive))
  (with-current-buffer (get-buffer-create bootstrap-cache--buffer-name)
    (setq bootstrap-cache--resolved-instance-id nil))
  (cl-letf (((symbol-function 'daemonp) (lambda () "work"))
            ((symbol-function 'setenv) #'ignore))
    (should (equal "work" (sprite-resolve-instance-id)))))

(ert-deftest bootstrap/resolve-instance-id-falls-back-to-solo ()
  "Returns \"solo\" as final fallback when no daemon or known ID."
  (with-current-buffer (get-buffer-create bootstrap-cache--buffer-name)
    (setq bootstrap-cache--resolved-instance-id nil))
  (cl-letf (((symbol-function 'daemonp) (lambda () nil)))
    (let ((sprite-instance-id nil))
      (should (equal "solo" (sprite-resolve-instance-id))))))

(ert-deftest bootstrap/resolve-instance-id-uses-cache ()
  "sprite-instance-name returns sprite-instance-id without recomputing."
  (let ((sprite-instance-id "cached-value"))
    (should (equal "cached-value" (sprite-instance-name)))))

(ert-deftest bootstrap/resolve-instance-id-returns-string ()
  "Return value is always a string."
  (with-current-buffer (get-buffer-create bootstrap-cache--buffer-name)
    (setq bootstrap-cache--resolved-instance-id nil))
  (cl-letf (((symbol-function 'daemonp) (lambda () nil)))
    (let ((sprite-instance-id nil))
      (should (stringp (sprite-resolve-instance-id))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite-state-file-prefix

(ert-deftest bootstrap/config-file-prefix-basic ()
  "Joins host, instance, and name with hyphens."
  (cl-letf (((symbol-function 'sprite-conf-host-and-instance)
             (lambda () (list "myhost" "solo")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (equal "myhost-solo-eshell"
                     (sprite-state-file-prefix "eshell"))))))

(ert-deftest bootstrap/config-file-prefix-root-includes-login ()
  "Prefix includes login name when running as root."
  (cl-letf (((symbol-function 'sprite-conf-host-and-instance)
             (lambda () (list "myhost" "solo")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "root"))
      (should (string-match-p "root" (sprite-state-file-prefix "bookmarks"))))))

(ert-deftest bootstrap/config-file-prefix-returns-string ()
  "Return value is always a string."
  (cl-letf (((symbol-function 'sprite-conf-host-and-instance)
             (lambda () (list "host" "inst")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (stringp (sprite-state-file-prefix "test"))))))

(ert-deftest bootstrap/config-file-prefix-no-empty-segments ()
  "Prefix does not produce leading, trailing, or consecutive hyphens."
  (cl-letf (((symbol-function 'sprite-conf-host-and-instance)
             (lambda () (list "host" "daemon")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (let ((result (sprite-state-file-prefix "bookmarks")))
        (should-not (string-match-p "^-\\|--\\|-$" result))))))

(ert-deftest bootstrap/config-file-prefix-contains-name ()
  "The NAME argument appears in the result."
  (cl-letf (((symbol-function 'sprite-conf-host-and-instance)
             (lambda () (list "host" "inst")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (string-match-p "recentf" (sprite-state-file-prefix "recentf"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite-state-path

(ert-deftest bootstrap/conf-state-path-returns-string ()
  "Returns a string path."
  (cl-letf (((symbol-function 'sprite-conf-host-and-instance)
             (lambda () (list "host" "inst")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (stringp (sprite-state-path "bookmarks"))))))

(ert-deftest bootstrap/conf-state-path-contains-state-dir ()
  "Result path contains the state directory name."
  (cl-letf (((symbol-function 'sprite-conf-host-and-instance)
             (lambda () (list "host" "inst")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (string-match-p sprite--conf-state-directory
                              (sprite-state-path "bookmarks"))))))

(ert-deftest bootstrap/conf-state-path-rooted-in-emacs-dir ()
  "Result path is under `user-emacs-directory'."
  (cl-letf (((symbol-function 'sprite-conf-host-and-instance)
             (lambda () (list "host" "inst")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (string-prefix-p (expand-file-name user-emacs-directory)
                               (expand-file-name (sprite-state-path "bookmarks")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer-derived-mode-p

(ert-deftest bootstrap/buffer-derived-mode-p-exact-match ()
  "Returns t when buffer uses the exact mode."
  (with-temp-buffer
    (text-mode)
    (should (buffer-derived-mode-p (current-buffer) 'text-mode))))

(ert-deftest bootstrap/buffer-derived-mode-p-parent-match ()
  "Returns t when mode is a parent of the buffer's major-mode."
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (buffer-derived-mode-p (current-buffer) 'prog-mode))))

(ert-deftest bootstrap/buffer-derived-mode-p-no-match ()
  "Returns nil when mode does not match."
  (with-temp-buffer
    (text-mode)
    (should-not (buffer-derived-mode-p (current-buffer) 'emacs-lisp-mode))))

(ert-deftest bootstrap/buffer-derived-mode-p-accepts-name-string ()
  "Buffer argument can be a buffer-name string."
  (let ((buf (get-buffer-create " *bootstrap-mode-name-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (emacs-lisp-mode))
          (should (buffer-derived-mode-p " *bootstrap-mode-name-test*" 'emacs-lisp-mode)))
      (kill-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer-line-count

(ert-deftest bootstrap/buffer-line-count-single-line ()
  "A buffer with one line of text reports 1."
  (with-temp-buffer
    (insert "hello")
    (should (= 1 (buffer-line-count (current-buffer))))))

(ert-deftest bootstrap/buffer-line-count-multiple-lines ()
  "Reports the correct count for multi-line content."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (should (= 3 (buffer-line-count (current-buffer))))))

(ert-deftest bootstrap/buffer-line-count-returns-integer ()
  "Return value is an integer."
  (with-temp-buffer
    (insert "a\nb\nc")
    (should (integerp (buffer-line-count (current-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer-directory

(ert-deftest bootstrap/buffer-directory-non-buffer-returns-nil ()
  "Returns nil for a non-buffer argument."
  (should-not (buffer-directory "not-a-buffer"))
  (should-not (buffer-directory 42))
  (should-not (buffer-directory nil)))

(ert-deftest bootstrap/buffer-directory-no-file-returns-nil ()
  "Returns nil for a scratch buffer not visiting a file."
  (with-temp-buffer
    (should-not (buffer-directory (current-buffer)))))

(ert-deftest bootstrap/buffer-directory-file-buffer ()
  "Returns the directory portion of a file-visiting buffer's path."
  (let ((tmp (make-temp-file "ert-bootstrap-bufdir-")))
    (unwind-protect
        (let ((buf (find-file-noselect tmp)))
          (unwind-protect
              (should (equal (file-name-directory tmp)
                             (buffer-directory buf)))
            (kill-buffer buf)))
      (delete-file tmp))))

(ert-deftest bootstrap/buffer-directory-returns-string-or-nil ()
  "Return value is always a string or nil."
  (with-temp-buffer
    (let ((result (buffer-directory (current-buffer))))
      (should (or (null result) (stringp result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffers-matching-mode

(ert-deftest bootstrap/buffers-matching-mode-finds-match ()
  "Returns a list including buffers with the given mode."
  (let ((buf (get-buffer-create " *bootstrap-mode-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (text-mode))
          (should (member buf (buffers-matching-mode 'text-mode))))
      (kill-buffer buf))))

(ert-deftest bootstrap/buffers-matching-mode-returns-list ()
  "Return value is always a list."
  (should (listp (buffers-matching-mode 'text-mode))))

(ert-deftest bootstrap/buffers-matching-mode-no-false-positives ()
  "Does not include buffers in a different mode."
  (let ((b1 (get-buffer-create " *bootstrap-mode-a*"))
        (b2 (get-buffer-create " *bootstrap-mode-b*")))
    (unwind-protect
        (progn
          (with-current-buffer b1 (emacs-lisp-mode))
          (with-current-buffer b2 (text-mode))
          (let ((results (buffers-matching-mode 'emacs-lisp-mode)))
            (should (member b1 results))
            (should-not (member b2 results))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest bootstrap/buffers-matching-mode-empty-for-unknown ()
  "Returns empty list when no buffers use a synthetic unknown mode."
  (should (null (buffers-matching-mode 'bootstrap-ert-fake-mode-xyz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffers-matching-path

(ert-deftest bootstrap/buffers-matching-path-finds-match ()
  "Finds buffers whose file name matches the regexp."
  (let ((tmp (make-temp-file "ert-bootstrap-path-")))
    (unwind-protect
        (let ((buf (find-file-noselect tmp)))
          (unwind-protect
              (should (member buf (buffers-matching-path (regexp-quote tmp))))
            (kill-buffer buf)))
      (delete-file tmp))))

(ert-deftest bootstrap/buffers-matching-path-no-match ()
  "Returns nil when no buffer path matches."
  (should (null (buffers-matching-path "/__no_such_ert_path_xyz__/"))))

(ert-deftest bootstrap/buffers-matching-path-returns-list ()
  "Return value is always a list."
  (should (listp (buffers-matching-path ".*"))))

(ert-deftest bootstrap/buffers-matching-path-excludes-scratch ()
  "Scratch buffers (no file name) are not included."
  (with-temp-buffer
    (should-not (member (current-buffer) (buffers-matching-path ".*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clean-kill-ring-filter-catch-p

(ert-deftest bootstrap/clean-kill-ring-filter-catches-empty ()
  "Empty string is caught."
  (should (clean-kill-ring-filter-catch-p "")))

(ert-deftest bootstrap/clean-kill-ring-filter-catches-whitespace ()
  "Whitespace-only string is caught."
  (should (clean-kill-ring-filter-catch-p "   ")))

(ert-deftest bootstrap/clean-kill-ring-filter-catches-newlines ()
  "Newline-only string is caught."
  (should (clean-kill-ring-filter-catch-p "\n\n")))

(ert-deftest bootstrap/clean-kill-ring-filter-passes-content ()
  "Non-blank string is not caught."
  (should-not (clean-kill-ring-filter-catch-p "hello world")))

(ert-deftest bootstrap/clean-kill-ring-filter-passes-padded-content ()
  "String with surrounding whitespace but real content is not caught."
  (should-not (clean-kill-ring-filter-catch-p "  hello  ")))

(ert-deftest bootstrap/clean-kill-ring-filter-custom-filter ()
  "Custom filters in `clean-kill-ring-filters' are applied."
  (let ((clean-kill-ring-filters (list (lambda (s) (string-prefix-p "BAD:" s)))))
    (should (clean-kill-ring-filter-catch-p "BAD:payload"))
    (should-not (clean-kill-ring-filter-catch-p "GOOD:payload"))))

(ert-deftest bootstrap/clean-kill-ring-filter-returns-t-or-nil ()
  "Return value is exactly t or nil, not just truthy/falsy."
  (should (eq t (clean-kill-ring-filter-catch-p "")))
  (should (eq nil (clean-kill-ring-filter-catch-p "text"))))

(ert-deftest bootstrap/clean-kill-ring-filter-strips-properties ()
  "Text properties on the string do not affect matching."
  (let ((s (copy-sequence "hello")))
    (put-text-property 0 5 'face 'bold s)
    (should-not (clean-kill-ring-filter-catch-p s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clean-kill-ring-clean

(ert-deftest bootstrap/clean-kill-ring-removes-blanks ()
  "Blank strings are removed from `kill-ring'."
  (let ((kill-ring (list "hello" "" "world" "  "))
        (clean-kill-ring-prevent-duplicates nil))
    (clean-kill-ring-clean nil)
    (should (equal '("hello" "world") kill-ring))))

(ert-deftest bootstrap/clean-kill-ring-empty-input ()
  "Works on an empty `kill-ring' without error."
  (let ((kill-ring nil))
    (clean-kill-ring-clean)
    (should (null kill-ring))))

(ert-deftest bootstrap/clean-kill-ring-all-blank ()
  "When all entries are blank, `kill-ring' becomes nil."
  (let ((kill-ring (list "" "  " "\n"))
        (clean-kill-ring-prevent-duplicates nil))
    (clean-kill-ring-clean nil)
    (should (null kill-ring))))

(ert-deftest bootstrap/clean-kill-ring-dedup-with-arg ()
  "Duplicate entries are removed when REMOVE-DUPS is non-nil."
  (let ((kill-ring (list "a" "b" "a" "b" "c")))
    (clean-kill-ring-clean t)
    (should (= (length kill-ring)
               (length (delete-dups (copy-sequence kill-ring)))))))

(ert-deftest bootstrap/clean-kill-ring-dedup-from-variable ()
  "Deduplication occurs when `clean-kill-ring-prevent-duplicates' is t."
  (let ((kill-ring (list "x" "y" "x"))
        (clean-kill-ring-prevent-duplicates t))
    (clean-kill-ring-clean)
    (should (= 2 (length kill-ring)))))

(ert-deftest bootstrap/clean-kill-ring-no-dedup-when-disabled ()
  "Duplicates are kept when both dedup flags are nil."
  (let ((kill-ring (list "x" "y" "x"))
        (clean-kill-ring-prevent-duplicates nil))
    (clean-kill-ring-clean nil)
    (should (= 3 (length kill-ring)))))

(ert-deftest bootstrap/clean-kill-ring-preserves-order ()
  "Relative order of surviving entries is preserved."
  (let ((kill-ring (list "first" "second" "third"))
        (clean-kill-ring-prevent-duplicates nil))
    (clean-kill-ring-clean nil)
    (should (equal '("first" "second" "third") kill-ring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uniquify-region-lines

(ert-deftest bootstrap/uniquify-region-collapses-adjacent-dups ()
  "Two adjacent identical lines are collapsed to one."
  (with-temp-buffer
    (insert "alpha\nalpha\nbeta\n")
    (uniquify-region-lines (point-min) (point-max))
    (should (equal "alpha\nbeta\n" (buffer-string)))))

(ert-deftest bootstrap/uniquify-region-collapses-many-dups ()
  "Three or more identical adjacent lines collapse to one."
  (with-temp-buffer
    (insert "dup\ndup\ndup\n")
    (uniquify-region-lines (point-min) (point-max))
    (should (equal "dup\n" (buffer-string)))))

(ert-deftest bootstrap/uniquify-region-keeps-non-adjacent-dups ()
  "Non-adjacent duplicates are preserved."
  (with-temp-buffer
    (insert "a\nb\na\n")
    (uniquify-region-lines (point-min) (point-max))
    (should (equal "a\nb\na\n" (buffer-string)))))

(ert-deftest bootstrap/uniquify-region-no-change-when-unique ()
  "Buffer content is unchanged when all lines are unique."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (uniquify-region-lines (point-min) (point-max))
    (should (equal "a\nb\nc\n" (buffer-string)))))

(ert-deftest bootstrap/uniquify-region-empty-buffer-no-error ()
  "Works on an empty buffer without signaling an error."
  (with-temp-buffer
    (should-not (uniquify-region-lines (point-min) (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uniquify-buffer-lines

(ert-deftest bootstrap/uniquify-buffer-deduplicates ()
  "Removes adjacent duplicate lines from the whole buffer."
  (with-temp-buffer
    (insert "x\nx\ny\n")
    (uniquify-buffer-lines)
    (should (equal "x\ny\n" (buffer-string)))))

(ert-deftest bootstrap/uniquify-buffer-no-change-when-unique ()
  "No-op when all lines are already unique."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (uniquify-buffer-lines)
    (should (equal "a\nb\nc\n" (buffer-string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; overmark-font-lock-width-keyword

(ert-deftest bootstrap/overmark-font-lock-width-keyword-returns-list ()
  "Returns a non-nil list."
  (should (listp (overmark-font-lock-width-keyword 80)))
  (should (overmark-font-lock-width-keyword 80)))

(ert-deftest bootstrap/overmark-font-lock-width-keyword-regexp-matches-long ()
  "The keyword regexp matches a line longer than the specified width."
  (let* ((kw (overmark-font-lock-width-keyword 80))
         (re (caar kw))
         (long-line (make-string 85 ?a)))
    (should (string-match re long-line))))

(ert-deftest bootstrap/overmark-font-lock-width-keyword-regexp-no-match-short ()
  "The keyword regexp does not match a line at or under the width."
  (let* ((kw (overmark-font-lock-width-keyword 80))
         (re (caar kw))
         (short-line (make-string 79 ?a)))
    (should-not (string-match re short-line))))

(ert-deftest bootstrap/overmark-font-lock-width-keyword-uses-warning-face ()
  "The keyword specifies `font-lock-warning-face'."
  (let ((result (overmark-font-lock-width-keyword 80)))
    (should (eq 'font-lock-warning-face (nth 1 (cadar result))))))

(ert-deftest bootstrap/overmark-font-lock-width-keyword-respects-width ()
  "Different widths produce different regexps."
  (let ((kw-60 (caar (overmark-font-lock-width-keyword 60)))
        (kw-120 (caar (overmark-font-lock-width-keyword 120))))
    (should-not (equal kw-60 kw-120))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; overmark-font-lock-show-tabs

(ert-deftest bootstrap/overmark-font-lock-show-tabs-returns-list ()
  "Returns a non-nil list."
  (should (listp (overmark-font-lock-show-tabs)))
  (should (overmark-font-lock-show-tabs)))

(ert-deftest bootstrap/overmark-font-lock-show-tabs-matches-tab ()
  "The pattern in the result matches a literal tab character."
  (let* ((result (overmark-font-lock-show-tabs))
         (pattern (caar result)))
    (should (string-match pattern "\t"))))

(ert-deftest bootstrap/overmark-font-lock-show-tabs-no-match-space ()
  "The tab pattern does not match a space."
  (let* ((result (overmark-font-lock-show-tabs))
         (pattern (caar result)))
    (should-not (string-match pattern " "))))

(ert-deftest bootstrap/overmark-font-lock-show-tabs-pure ()
  "Returns an equal value on every call (no side effects)."
  (should (equal (overmark-font-lock-show-tabs) (overmark-font-lock-show-tabs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set-tab-width

(ert-deftest bootstrap/set-tab-width-sets-locally ()
  "Sets `tab-width' buffer-locally."
  (with-temp-buffer
    (set-tab-width 4)
    (should (= 4 tab-width))
    (should (local-variable-p 'tab-width))))

(ert-deftest bootstrap/set-tab-width-various-values ()
  "Accepts different numeric values."
  (with-temp-buffer (set-tab-width 2) (should (= 2 tab-width)))
  (with-temp-buffer (set-tab-width 8) (should (= 8 tab-width))))

(ert-deftest bootstrap/set-tab-width-does-not-affect-other-buffers ()
  "Does not change `tab-width' in other buffers."
  (let ((other (get-buffer-create " *bootstrap-tab-test*")))
    (unwind-protect
        (with-current-buffer other
          (let ((before (if (local-variable-p 'tab-width)
                            tab-width
                          (default-value 'tab-width))))
            (with-temp-buffer (set-tab-width 2))
            (should (equal before (if (local-variable-p 'tab-width)
                                      tab-width
                                    (default-value 'tab-width))))))
      (kill-buffer other))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toggle-word-wrap

(ert-deftest bootstrap/toggle-word-wrap-rejects-non-nil-arg ()
  "Signals `user-error' when called with a non-nil argument."
  (should-error (toggle-word-wrap 'something) :type 'user-error)
  (should-error (toggle-word-wrap t) :type 'user-error)
  (should-error (toggle-word-wrap 1) :type 'user-error))

(ert-deftest bootstrap/toggle-word-wrap-nil-ok ()
  "Calling with nil does not signal an error."
  (with-temp-buffer
    (cl-letf (((symbol-function 'turn-on-soft-wrap) #'ignore)
              ((symbol-function 'turn-on-hard-wrap) #'ignore))
      (should-not (toggle-word-wrap nil)))))

(ert-deftest bootstrap/toggle-word-wrap-no-arg-ok ()
  "Calling with no argument does not signal an error."
  (with-temp-buffer
    (cl-letf (((symbol-function 'turn-on-soft-wrap) #'ignore)
              ((symbol-function 'turn-on-hard-wrap) #'ignore))
      (should-not (toggle-word-wrap)))))

(ert-deftest bootstrap/toggle-word-wrap-delegates-correctly ()
  "Calls turn-on-soft-wrap when hard-wrapping, turn-on-hard-wrap when not."
  (with-temp-buffer
    (let ((calls nil))
      (cl-letf (((symbol-function 'turn-on-soft-wrap)
                 (lambda () (push 'soft calls)))
                ((symbol-function 'turn-on-hard-wrap)
                 (lambda () (push 'hard calls))))
        ;; With auto-fill-function set, should go to soft-wrap
        (setq-local auto-fill-function #'ignore)
        (toggle-word-wrap)
        (should (equal '(soft) calls))
        ;; With no auto-fill-function, should go to hard-wrap
        (setq calls nil)
        (setq-local auto-fill-function nil)
        (toggle-word-wrap)
        (should (equal '(hard) calls))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unfill-region

(ert-deftest bootstrap/unfill-region-joins-plain-lines ()
  "Joins soft-wrapped lines that start with a normal character."
  (with-temp-buffer
    (insert "This is a long\nsentence here.\n")
    (unfill-region (point-min) (point-max))
    (should (equal "This is a long sentence here.\n" (buffer-string)))))

(ert-deftest bootstrap/unfill-region-preserves-blank-lines ()
  "Blank lines (paragraph separators) are not joined."
  (with-temp-buffer
    (insert "paragraph one\n\nparagraph two\n")
    (unfill-region (point-min) (point-max))
    (should (string-match-p "\n\n" (buffer-string)))))

(ert-deftest bootstrap/unfill-region-preserves-indented-lines ()
  "Lines starting with whitespace (code/quote) are not joined."
  (with-temp-buffer
    (insert "normal text\n  indented line\n")
    (unfill-region (point-min) (point-max))
    (should (string-match-p "\n  indented" (buffer-string)))))

(ert-deftest bootstrap/unfill-region-preserves-list-items ()
  "Lines starting with * are not joined."
  (with-temp-buffer
    (insert "some text\n* list item\n")
    (unfill-region (point-min) (point-max))
    (should (string-match-p "\n\\* list item" (buffer-string)))))

(ert-deftest bootstrap/unfill-region-empty-no-error ()
  "Works on an empty buffer without error."
  (with-temp-buffer
    (should-not (unfill-region (point-min) (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ensure-frame-font

(ert-deftest bootstrap/ensure-font-sets-when-absent ()
  "Sets font when no font entry exists in `default-frame-alist'."
  (let ((default-frame-alist nil))
    (cl-letf (((symbol-function 'set-frame-font) #'ignore))
      (ensure-frame-font "Mono" 12)
      (should (assoc 'font default-frame-alist)))))

(ert-deftest bootstrap/ensure-font-noop-when-present ()
  "Does nothing when a font entry already exists."
  (let ((default-frame-alist (list (cons 'font "Existing-10"))))
    (cl-letf (((symbol-function 'set-frame-font) #'ignore))
      (ensure-frame-font "Other" 14)
      (should (equal "Existing-10" (cdr (assoc 'font default-frame-alist)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; disable-all-themes / bootstrap-default-theme

(ert-deftest bootstrap/default-theme-enables-modus-operandi ()
  "Calls `load-theme' and `enable-theme' for modus-operandi."
  (let (loaded enabled)
    (cl-letf (((symbol-function 'load-theme)
               (lambda (theme &rest _) (push theme loaded) t))
              ((symbol-function 'enable-theme)
               (lambda (theme) (push theme enabled))))
      (bootstrap-default-theme)
      (should (equal '(modus-operandi) loaded))
      (should (equal '(modus-operandi) enabled)))))

(ert-deftest bootstrap/disable-all-themes-calls-disable-for-each ()
  "Calls `disable-theme' for every entry in `custom-enabled-themes'."
  (let ((custom-enabled-themes '(theme-a theme-b theme-c))
        (disabled nil))
    (cl-letf (((symbol-function 'disable-theme)
               (lambda (theme) (push theme disabled))))
      (disable-all-themes)
      (should (equal (sort disabled #'string<)
                     (sort '(theme-a theme-b theme-c) #'string<))))))

(ert-deftest bootstrap/disable-all-themes-empty-list-no-error ()
  "Works without error when no themes are enabled."
  (let ((custom-enabled-themes nil))
    (cl-letf (((symbol-function 'disable-theme) #'ignore))
      (should-not (disable-all-themes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite state startup

(ert-deftest bootstrap/programming-startup-configures-state-first ()
  "Project consumers see instance paths before Eglox starts."
  (should (memq 'sprite-state-configure sprite-startup-operations))
  (should (memq 'eglox-global-mode sprite-startup-operations))
  (should (< (seq-position sprite-startup-operations 'sprite-state-configure)
             (seq-position sprite-startup-operations 'eglox-global-mode))))

(ert-deftest bootstrap/sprite-state-startup-order ()
  "State mode starts after configuration and defers desktop restoration."
  (should (memq 'sprite-state-global-mode sprite-startup-operations))
  (should (memq 'sprite-state-start-desktop sprite-startup-operations))
  (should (< (seq-position sprite-startup-operations 'sprite-state-global-mode)
             (seq-position sprite-startup-operations 'sprite-state-start-desktop)))
  (should sprite-state-savehist-enabled)
  (should sprite-state-recentf-enabled)
  (should sprite-state-desktop-deferred))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bootstrap-set-up-ssh-agent

(ert-deftest bootstrap/ssh-agent-returns-existing-env ()
  "Returns the current SSH_AUTH_SOCK value when already set."
  (cl-letf (((symbol-function 'getenv)
             (lambda (var)
               (when (equal var "SSH_AUTH_SOCK")
                 "/run/user/1000/ssh-agent.socket"))))
    (should (equal "/run/user/1000/ssh-agent.socket"
                   (bootstrap-set-up-ssh-agent)))))

(ert-deftest bootstrap/ssh-agent-returns-nil-when-no-socket ()
  "Returns nil when env is unset and no socket candidates exist."
  (cl-letf (((symbol-function 'getenv) (lambda (_) nil))
            ((symbol-function 'directory-files) (lambda (&rest _) nil))
            ((symbol-function 'file-writable-p) (lambda (_) nil)))
    (should-not (bootstrap-set-up-ssh-agent))))

(ert-deftest bootstrap/ssh-agent-sets-env-from-candidate ()
  "Sets and returns SSH_AUTH_SOCK from first socket candidate."
  (let ((socket "/tmp/ssh-abc/agent.123")
        assigned)
    (cl-letf (((symbol-function 'getenv) (lambda (_) nil))
              ((symbol-function 'directory-files) (lambda (&rest _) (list socket)))
              ((symbol-function 'file-writable-p) (lambda (path) (equal path socket)))
              ((symbol-function 'setenv)
               (lambda (var val) (setq assigned (cons var val)) val)))
      (should (equal socket (bootstrap-set-up-ssh-agent)))
      (should (equal assigned (cons "SSH_AUTH_SOCK" socket))))))

(ert-deftest bootstrap/ssh-agent-return-type ()
  "Return value is always a string or nil."
  (cl-letf (((symbol-function 'getenv) (lambda (_) nil))
            ((symbol-function 'directory-files) (lambda (&rest _) nil))
            ((symbol-function 'file-writable-p) (lambda (_) nil)))
    (let ((result (bootstrap-set-up-ssh-agent)))
      (should (or (null result) (stringp result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest bootstrap/show-whitespace-mode-starts-early ()
  "The global mode is included in the early startup operations."
  (should (memq 'overmark-global-mode sprite-startup-operations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kill-buffers-visiting-missing-files

(ert-deftest bootstrap/kill-missing-file-buffers-finds-missing ()
  "Kills and returns path for a buffer whose file no longer exists."
  (let ((tmp (make-temp-file "ert-kill-missing-")))
    (unwind-protect
        (let ((buf (find-file-noselect tmp)))
          (unwind-protect
              (progn
                (delete-file tmp)
                (cl-letf (((symbol-function 'kill-buffer-ask)
                           (lambda (b) (kill-buffer b) t)))
                  (should (member tmp (kill-buffers-visiting-missing-files)))))
            (when (buffer-live-p buf) (kill-buffer buf))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest bootstrap/kill-missing-file-buffers-skips-existing ()
  "Does not include buffers whose file still exists."
  (let ((tmp (make-temp-file "ert-kill-existing-")))
    (unwind-protect
        (let ((buf (find-file-noselect tmp)))
          (unwind-protect
              (cl-letf (((symbol-function 'kill-buffer-ask)
                         (lambda (_) (error "should not be called"))))
                (should-not (member tmp (kill-buffers-visiting-missing-files))))
            (kill-buffer buf)))
      (delete-file tmp))))

(ert-deftest bootstrap/kill-missing-file-buffers-skips-scratch ()
  "Does not prompt for scratch buffers that have no file name."
  (with-temp-buffer
    (cl-letf (((symbol-function 'kill-buffer-ask)
               (lambda (_) (error "should not be called"))))
      (kill-buffers-visiting-missing-files))))

(ert-deftest bootstrap/kill-missing-file-buffers-respects-no ()
  "Excludes buffers from the return value when kill-buffer-ask returns nil."
  (let ((tmp (make-temp-file "ert-kill-no-")))
    (unwind-protect
        (let ((buf (find-file-noselect tmp)))
          (unwind-protect
              (progn
                (delete-file tmp)
                (cl-letf (((symbol-function 'kill-buffer-ask) (lambda (_) nil)))
                  (should-not (kill-buffers-visiting-missing-files))))
            (when (buffer-live-p buf) (kill-buffer buf))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest bootstrap/kill-missing-file-buffers-empty ()
  "Returns nil when there are no missing-file buffers."
  (cl-letf (((symbol-function 'buffer-list) (lambda (&optional _frame) nil)))
    (should-not (kill-buffers-visiting-missing-files))))

(ert-deftest bootstrap/kill-missing-file-buffers-returns-list ()
  "Return value is always a list."
  (cl-letf (((symbol-function 'buffer-list) (lambda (&optional _frame) nil)))
    (should (listp (kill-buffers-visiting-missing-files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delight mode lighters

(defmacro bootstrap-test/with-delight-applied (&rest body)
  "Run BODY after ensuring delight lighters are applied, restoring previous mode state."
  (declare (indent 0))
  `(let ((was-on (bound-and-true-p hud-modeline-mode)))
     (unwind-protect
         (progn
           (require 'hud-modeline)
           (unless was-on (hud-modeline-mode 1))
           ,@body)
       (unless was-on
         (when (bound-and-true-p hud-modeline-mode)
           (hud-modeline-mode -1))))))

(ert-deftest bootstrap/delight-fundamental-mode-name ()
  "fundamental-mode sets mode-name lighter to \"fun\" after delight runs.
delight stores (INHIBIT-VAR ORIGINAL LIGHTER) in mode-name; the C mode-line
renderer shows LIGHTER when INHIBIT-VAR is nil."
  (bootstrap-test/with-delight-applied
    (with-temp-buffer
      (fundamental-mode)
      (should (listp mode-name))
      (should (equal "fun" (nth 2 mode-name))))))

(ert-deftest bootstrap/delight-sh-mode-name ()
  "sh-mode sets mode-name lighter to \"sh\" after delight runs."
  (bootstrap-test/with-delight-applied
    (with-temp-buffer
      (sh-mode)
      (should (listp mode-name))
      (should (equal "sh" (nth 2 mode-name))))))

(ert-deftest bootstrap/delight-org-mode-name ()
  "org-mode sets mode-name lighter to \"org\" after delight runs."
  (bootstrap-test/with-delight-applied
    (with-temp-buffer
      (org-mode)
      (should (listp mode-name))
      (should (equal "org" (nth 2 mode-name))))))

(ert-deftest bootstrap/delight-visual-line-mode-lighter ()
  "visual-line-mode lighter in minor-mode-alist is \" wr\" after delight runs."
  (bootstrap-test/with-delight-applied
    (should (equal " wr"
                   (cadr (assq 'visual-line-mode minor-mode-alist))))))

(ert-deftest bootstrap/delight-auto-revert-mode-hidden ()
  "auto-revert-mode lighter in minor-mode-alist is nil after delight runs."
  (bootstrap-test/with-delight-applied
    (require 'autorevert)
    (should (null (cadr (assq 'auto-revert-mode minor-mode-alist))))))

(ert-deftest bootstrap/delight-eldoc-mode-hidden ()
  "eldoc-mode lighter in minor-mode-alist is nil after delight runs."
  (bootstrap-test/with-delight-applied
    (require 'eldoc)
    (should (null (cadr (assq 'eldoc-mode minor-mode-alist))))))

(ert-deftest bootstrap/with-temporary-package-require-calls-install-when-missing ()
  "Test that `with-temporary-package-require' installs the package if not present."
  (let (installed-called require-called)
    (cl-letf (((symbol-function 'package-installed-p) (lambda (_pkg) nil))
              ((symbol-function 'package-install) (lambda (pkg) (setq installed-called pkg)))
              ((symbol-function 'package-initialize) #'ignore)
              ((symbol-function 'require) (lambda (feature &rest _) (setq require-called feature))))
      (with-temporary-package-require 'package-lint
        :path "/tmp/fake-lint-path"
        (should (eq 'package-lint installed-called))
        (should (eq 'package-lint require-called))))))

(ert-deftest bootstrap/with-temporary-package-require-skips-install-when-present ()
  "Test that `with-temporary-package-require' skips installation if already present."
  (let (installed-called require-called)
    (cl-letf (((symbol-function 'package-installed-p) (lambda (_pkg) t))
              ((symbol-function 'package-install) (lambda (pkg) (setq installed-called pkg)))
              ((symbol-function 'package-initialize) #'ignore)
              ((symbol-function 'require) (lambda (feature &rest _) (setq require-called feature))))
      (with-temporary-package-require 'package-lint
        :path "/tmp/fake-lint-path2"
        (should-not installed-called)
        (should (eq 'package-lint require-called))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cmdr-transient-insert-suffix-once

(ert-deftest bootstrap/transient-insert-suffix-once-idempotent ()
  "cmdr-transient-insert-suffix-once inserts only once and handles missing layouts."
  (require 'transient)
  ;; 1. No-op without error when prefix has no layout
  (should (null (cmdr-transient-insert-suffix-once
                 'bootstrap--test-nonexistent-menu
                 '(-1 0)
                 '("x" "test command" ignore))))
  ;; 2. Inserts suffix into a defined prefix
  (transient-define-prefix bootstrap--test-ert-menu ()
    ["Test"
     ("a" "Action A" ignore)])
  (should (cmdr-transient-insert-suffix-once
           'bootstrap--test-ert-menu
           '(-1 0)
           '("x" "test command" ignore)))
  (should (ignore-errors (transient-get-suffix 'bootstrap--test-ert-menu "x")))
  ;; 3. Second call is idempotent (no-ops and does not duplicate or error)
  (should (null (cmdr-transient-insert-suffix-once
                 'bootstrap--test-ert-menu
                 '(-1 0)
                 '("x" "test command" ignore))))
  (should (ignore-errors (transient-get-suffix 'bootstrap--test-ert-menu "x"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite-run-startup-operations (generator iterator)

(ert-deftest bootstrap/chex-mode-available-to-startup-hook ()
  "The late startup hook can invoke Chex before it has been loaded."
  (should (memq 'chex-global-mode sprite-startup-operations))
  (should (fboundp 'chex-global-mode)))

(ert-deftest bootstrap/run-enable-modes-hook-noninteractive ()
  "Executes all hook functions in order and calls on-done when noninteractive."
  (let* ((order nil)
         (done nil)
         (noninteractive t)
         (test-hook (list (lambda (arg) (push (cons 'f1 arg) order))
                          (lambda () (push 'f2 order))
                          (lambda (arg) (push (cons 'f3 arg) order)))))
    (sprite-run-startup-operations test-hook :on-done (lambda () (setq done t)))
    (should (equal '((f1 . 1) f2 (f3 . 1)) (nreverse order)))
    (should (eq t done))))

(ert-deftest bootstrap/startup-operation-uses-actual-arity ()
  "Enable optional-argument modes and call zero-argument operations without arguments."
  (let ((calls nil))
    (cl-letf (((symbol-function 'help-function-arglist)
               (lambda (&rest _) '(&rest args))))
      (sprite--execute-startup-operation (lambda () (push 'zero calls)))
      (sprite--execute-startup-operation (lambda (&optional arg) (push arg calls)))
      (should (equal (reverse calls) '(zero 1))))))

(defvar bootstrap--test-autoload-result nil)

(ert-deftest bootstrap/startup-operation-resolves-autoload-arity ()
  "Resolve unloaded autoloads before choosing the operation argument."
  (let ((file (make-temp-file "bootstrap-autoload-" nil ".el"))
        (bootstrap--test-autoload-result nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(defun bootstrap--test-autoload-zero () "
                    "(setq bootstrap--test-autoload-result 'zero))\n"
                    "(defun bootstrap--test-autoload-mode (&optional arg) "
                    "(setq bootstrap--test-autoload-result arg))\n"))
          (autoload 'bootstrap--test-autoload-zero file)
          (autoload 'bootstrap--test-autoload-mode file)
          (sprite--execute-startup-operation 'bootstrap--test-autoload-zero)
          (should (eq bootstrap--test-autoload-result 'zero))
          (sprite--execute-startup-operation 'bootstrap--test-autoload-mode)
          (should (eq bootstrap--test-autoload-result 1)))
      (fmakunbound 'bootstrap--test-autoload-zero)
      (fmakunbound 'bootstrap--test-autoload-mode)
      (delete-file file))))

(ert-deftest bootstrap/run-enable-modes-hook-catches-errors ()
  "Errors in individual hook functions are caught and iteration continues."
  (let* ((order nil)
         (done nil)
         (noninteractive t)
         (test-hook (list (lambda (arg) (push 'f1 order))
                          (lambda () (error "simulated mode failure"))
                          (lambda (arg) (push 'f3 order)))))
    (sprite-run-startup-operations test-hook :on-done (lambda () (setq done t)))
    (should (equal '(f1 f3) (nreverse order)))
    (should (eq t done))))

(ert-deftest bootstrap/startup-mcp-operation-passes-no-mode-argument ()
  "The MCP initializer is a startup operation and receives no mode argument."
  (let ((called nil)
        (noninteractive t))
    (should (memq 'mcpkit-init-services sprite-startup-operations))
    (cl-letf (((symbol-function 'mcpkit-init-services)
               (lambda () (setq called t))))
      (sprite-run-startup-operations '(mcpkit-init-services))
      (should called))))

(ert-deftest bootstrap/post-late-startup-runs-package-operations-in-order ()
  "Maintenance runs package operations in order from an idle timer."
  (require 'setup-packages)
  (require 'elpaish)
  (let ((order nil)
        (timers nil)
        (noninteractive nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_secs _repeat fn &rest _)
                 (push fn timers)
                 fn))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'elpaish-install-packages)
               (lambda (&rest args)
                 (should (plist-get args :async))
                 (push 'install order)))
              ((symbol-function 'bootstrap-package-quickstart)
               (lambda () (push 'refresh order)))
              ((symbol-function 'builder-emacs-conf-native-compile-all)
               (lambda () (push 'compile order))))
      (bootstrap-schedule-maintenance)
      (should (consp timers))
      (should-not order)
      (while timers
        (let ((step (pop timers)))
          (funcall step)))
      (should (equal (reverse order) '(install refresh compile))))))

(ert-deftest bootstrap/frame-alpha-preserves-configured-default ()
  "Apply an existing default opacity to a frame without an explicit value."
  (let ((default-frame-alist '((alpha . 87)))
        (applied nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'frame-parameter) (lambda (&rest _) nil))
              ((symbol-function 'set-frame-parameter)
               (lambda (_frame parameter value) (setq applied (cons parameter value)))))
      (ensure-frame-opacity)
      (should (equal applied '(alpha . 87)))
      (should (equal default-frame-alist '((alpha . 87)))))))

(ert-deftest bootstrap/run-enable-modes-hook-cancel ()
  "Cancellation function halts pending execution in interactive mode."
  (let* ((order nil)
         (noninteractive nil)
         (test-hook (list (lambda (arg) (push 'f1 order))
                          (lambda (arg) (push 'f2 order))))
         (cancel-fn (sprite-run-startup-operations test-hook :delay 10.0)))
    (funcall cancel-fn)
    (should (null order))))

(ert-deftest bootstrap/with-gc-suppressed-schedules-idle-gc ()
  "with-gc-suppressed schedules an idle GC timer instead of blocking."
  (load-file (expand-file-name "early-init.el" user-emacs-directory))
  (let ((noninteractive nil))
    (when (timerp bootstrap--gc-idle-timer)
      (cancel-timer bootstrap--gc-idle-timer)
      (setq bootstrap--gc-idle-timer nil))
    (unwind-protect
        (progn
          (with-gc-suppressed (+ 1 1))
          (should (timerp bootstrap--gc-idle-timer)))
      (when (timerp bootstrap--gc-idle-timer)
        (cancel-timer bootstrap--gc-idle-timer)
        (setq bootstrap--gc-idle-timer nil)))))

(ert-deftest bootstrap/set-notes-directory-sets-paths-without-requiring-org ()
  "bootstrap-set-notes-directory sets all note directory variables without requiring org."
  (let ((local-notes-directory nil)
        (user-org-directories nil)
        (org-directory nil)
        (org-agenda-files nil)
        (deft-directory nil)
        (denote-directory nil))
    (bootstrap-set-notes-directory "/tmp/test-notes")
    (should (equal local-notes-directory (expand-file-name "/tmp/test-notes")))
    (should (equal org-directory (expand-file-name "/tmp/test-notes/org")))
    (should (equal deft-directory (expand-file-name "/tmp/test-notes/deft")))
    (should (equal denote-directory (expand-file-name "/tmp/test-notes/denote")))))


(ert-deftest bootstrap/optimize-quickstart-transforms-correctly ()
  "Test that elpaish-install-optimize-quickstart-file streamlines quickstart contents."
  (let ((tmp-file (make-temp-file "test-quickstart-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp-file
            (insert ";;; Quickstart\n\n")
            (insert "(let* ((load-file-name \"/tmp/dummy/pkg-autoloads.el\")(load-true-file-name load-file-name))\n")
            (insert "  (add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))\n")
            (insert "  (register-definition-prefixes \"pkg\" '(\"pkg-\" \"pkg/\"))\n")
            (insert "  (autoload 'pkg-fn \"pkg\" \"This is a docstring\" t)\n")
            (insert "  (provide 'pkg-autoloads)\n)\n\x0c\n"))
          (elpaish-install-optimize-quickstart-file tmp-file)
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "(setq load-path" content))
              (should (string-match-p "(append '(\"/tmp/dummy\")" content))
              (should-not (string-match-p "register-definition-prefixes" content))
              (should-not (string-match-p "This is a docstring" content))
              (should-not (string-match-p "provide 'pkg-autoloads" content))
              (should-not (string-match-p "let\*" content))
              (should-not (string-match-p "\x0c" content))
              (should (string-match-p "(autoload 'pkg-fn \"pkg\" nil t)" content)))))
      (when (file-exists-p tmp-file)
        (delete-file tmp-file)))))

(defvar warning-inhibit-types nil)

(ert-deftest bootstrap/quickstart-compilation-is-silent-on-errors ()
  "Compiler errors during quickstart refresh do not display or log warnings."
  (require 'bytecomp)
  (require 'warnings)
  (let ((file (make-temp-file "quickstart-error-" nil ".el"))
        (byte-compile-log-buffer " *quickstart-test-compile-log*")
        (warning-inhibit-types nil)
        (warning-suppress-types nil)
        (warning-suppress-log-types nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert ";;; -*- lexical-binding: t; -*-\n(\n"))
          (elpaish-install--package-quickstart-refresh-optimize
           (lambda () (byte-compile-file file)))
          (when-let* ((buffer (get-buffer byte-compile-log-buffer)))
            (with-current-buffer buffer
              (should-not (string-match-p "Error:" (buffer-string))))))
      (when-let* ((buffer (get-buffer byte-compile-log-buffer)))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest bootstrap/package-quickstart-stale-p-detects-missing-elc ()
  "Test that stale-p returns non-nil when the .elc file is missing."
  (require 'elpaish-install)
  (let* ((tmp-el (make-temp-file "test-qs-" nil ".el"))
         (package-quickstart-file tmp-el))
    (unwind-protect
        (should (elpaish-install-package-quickstart-stale-p))
      (when (file-exists-p tmp-el) (delete-file tmp-el)))))

(ert-deftest bootstrap/startup-refreshes-quickstart-only-when-stale ()
  "Startup checks freshness before refreshing the quickstart."
  (require 'package)
  (let ((stale nil)
        (refreshes 0))
    (cl-letf (((symbol-function 'elpaish-install-refresh-package-quickstart)
               (lambda (&optional force)
                 (when (or force stale)
                   (setq refreshes (1+ refreshes))))))
      (bootstrap-package-quickstart)
      (should (= refreshes 0))
      (setq stale t)
      (bootstrap-package-quickstart)
      (should (= refreshes 1)))))

(ert-deftest bootstrap/set-frame-font-interactive-uses-annotated-completing-read ()
  "Interactively calling `set-frame-font' uses `annotated-completing-read-font-name'."
  (let ((acr-called nil)
        (orig-called nil))
    (cl-letf (((symbol-function 'annotated-completing-read-font-name)
               (lambda (&rest _) (setq acr-called t) "Monospace"))
              ((symbol-function 'set-frame-font)
               (lambda (font &rest _) (setq orig-called font))))
      (advice-add 'set-frame-font :around #'cmdr-set-frame-font-interactive)
      (unwind-protect
          (progn
            (call-interactively #'set-frame-font)
            (should acr-called)
            (should (equal "Monospace" orig-called)))
        (advice-remove 'set-frame-font #'cmdr-set-frame-font-interactive)))))

(defvar bootstrap--test-shared-setting nil)

(ert-deftest bootstrap/cross-package-settings-defcustom-lists ()
  "Each package exposes a `*-settings' defcustom alist and restores prior values."
  (require 'eglox)
  (require 'eglox-project)
  (require 'minaid)
  (require 'hud-modeline)
  (require 'sprite-state)
  (should (listp eglox-settings))
  (should (listp eglox-project-settings))
  (should (listp minaid-settings))
  (should (listp hud-modeline-settings))
  (should (listp sprite-state-settings))
  (set-default 'bootstrap--test-shared-setting 'baseline)
  (let ((eglox--saved-settings nil)
        (eglox-settings '((bootstrap--test-shared-setting . updated))))
    (unwind-protect
        (progn
          (eglox--apply-settings)
          (should (eq (default-value 'bootstrap--test-shared-setting) 'updated))
          (eglox--restore-settings)
          (should (eq (default-value 'bootstrap--test-shared-setting) 'baseline)))
      (eglox--restore-settings)
      (set-default 'bootstrap--test-shared-setting nil))))

(ert-deftest bootstrap/cross-package-lifecycle-and-disabled-eglot-minaid-flows ()
  "Exercise recentf, central project cache, and HUD frame title with Eglot and Minaid disabled."
  (require 'sprite-state)
  (require 'eglox-project)
  (require 'eglox)
  (require 'minaid)
  (require 'hud-modeline)
  (let* ((tmp-root (file-name-as-directory (make-temp-file "cross-pkg-test-" t)))
         (cache-dir (file-name-as-directory (expand-file-name "cache" tmp-root)))
         (proj-dir (file-name-as-directory (expand-file-name "proj" tmp-root)))
         (proj-file (expand-file-name "main.py" proj-dir))
         (eglox-project-cache-directory cache-dir)
         (eglox-lsp-enabled nil)
         (sprite-state-desktop-enabled nil)
         (sprite-state-savehist-enabled nil)
         (sprite-state-recentf-enabled t)
         (sprite-state-locks-enabled nil)
         (sprite-state-auto-save-enabled nil)
         (was-state (bound-and-true-p sprite-state-global-mode))
         (was-eglox (bound-and-true-p eglox-global-mode))
         (was-proj (bound-and-true-p eglox-project-mode))
         (was-minaid (bound-and-true-p minaid-mode))
         (was-hud (bound-and-true-p hud-modeline-mode)))
    (unwind-protect
        (progn
          (make-directory cache-dir t)
          (make-directory proj-dir t)
          (write-region "" nil (expand-file-name ".projectile" proj-dir) nil 'silent)
          (write-region "x = 1\n" nil proj-file nil 'silent)
          (when was-minaid (minaid-mode -1))
          (when was-eglox (eglox-global-mode -1))
          (when was-proj (eglox-project-mode -1))
          (when was-state (sprite-state-global-mode -1))
          (when was-hud (hud-modeline-mode -1))
          ;; Enable state, eglox (with LSP disabled), and HUD while Minaid remains off:
          (sprite-state-global-mode 1)
          (eglox-global-mode 1)
          (hud-modeline-mode 1)
          ;; Repeat enablement to verify no duplicate advice/hooks:
          (sprite-state-global-mode 1)
          (eglox-global-mode 1)
          (hud-modeline-mode 1)
          (should-not (bound-and-true-p minaid-mode))
          (should (bound-and-true-p recentf-mode))
          (should (bound-and-true-p eglox-project-mode))
          (should (advice-member-p #'eglox-project--cache-file 'projectile-project-cache-file))
          ;; Exercise file visit -> recentf + central projectile cache:
          (let ((buf (find-file-noselect proj-file)))
            (unwind-protect
                (with-current-buffer buf
                  (recentf-add-file proj-file)
                  (should (member (expand-file-name proj-file)
                                  (seq-map #'expand-file-name recentf-list)))
                  (let ((cfile (projectile-project-cache-file proj-dir)))
                    (should (string-prefix-p cache-dir cfile))
                    (projectile-serialize '("main.py") cfile)
                    (should (file-exists-p cfile))
                    (should-not (file-exists-p (expand-file-name ".projectile-cache.eld" proj-dir)))))
              (kill-buffer buf)))
          ;; Exercise HUD frame title with Sprite identity:
          (let ((sprite-instance-id "test-inst"))
            (should (string-match-p "test-inst" (hud-modeline--frame-title))))
          ;; Teardown all modes cleanly:
          (hud-modeline-mode -1)
          (eglox-global-mode -1)
          (sprite-state-global-mode -1)
          (should-not (advice-member-p #'eglox-project--cache-file 'projectile-project-cache-file)))
      (when was-state (sprite-state-global-mode 1))
      (when was-proj (eglox-project-mode 1))
      (when was-eglox (eglox-global-mode 1))
      (when was-minaid (minaid-mode 1))
      (when was-hud (hud-modeline-mode 1))
      (delete-directory tmp-root t))))

(provide (quote test-bootstrap))
;;; test-bootstrap.el ends here
