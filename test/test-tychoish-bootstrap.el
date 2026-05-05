;;; test-tychoish-bootstrap.el --- ERT tests for tychoish-bootstrap.el -*- lexical-binding: t -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^bootstrap/")

(require 'ert)
(require 'cl-lib)
(require 'tychoish-bootstrap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gui-p

(ert-deftest bootstrap/gui-p-daemon ()
  "gui-p returns t when daemonp is non-nil."
  (cl-letf (((symbol-function 'daemonp) (lambda () t))
            ((symbol-function 'window-system) (lambda () nil)))
    (should (eq t (gui-p)))))

(ert-deftest bootstrap/gui-p-window-system ()
  "gui-p returns t when window-system is non-nil."
  (cl-letf (((symbol-function 'daemonp) (lambda () nil))
            ((symbol-function 'window-system) (lambda () 'x)))
    (should (eq t (gui-p)))))

(ert-deftest bootstrap/gui-p-terminal ()
  "gui-p returns nil in a pure terminal session."
  (cl-letf (((symbol-function 'daemonp) (lambda () nil))
            ((symbol-function 'window-system) (lambda () nil)))
    (should-not (gui-p))))

(ert-deftest bootstrap/gui-p-named-daemon ()
  "gui-p returns t when daemonp returns a name string."
  (cl-letf (((symbol-function 'daemonp) (lambda () "work"))
            ((symbol-function 'window-system) (lambda () nil)))
    (should (gui-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tychoish/resolve-instance-id

(ert-deftest bootstrap/resolve-instance-id-daemon-t ()
  "Returns \"primary\" when daemonp returns t."
  (with-current-buffer (get-buffer-create tychoish-cache--buffer-name)
    (setq tychoish-cache--resolved-instance-id nil))
  (cl-letf (((symbol-function 'daemonp) (lambda () t)))
    (should (equal "primary" (tychoish/resolve-instance-id)))))

(ert-deftest bootstrap/resolve-instance-id-named-daemon ()
  "Returns daemon name when daemonp returns a string."
  (with-current-buffer (get-buffer-create tychoish-cache--buffer-name)
    (setq tychoish-cache--resolved-instance-id nil))
  (cl-letf (((symbol-function 'daemonp) (lambda () "work")))
    (should (equal "work" (tychoish/resolve-instance-id)))))

(ert-deftest bootstrap/resolve-instance-id-falls-back-to-solo ()
  "Returns \"solo\" as final fallback when no daemon or known ID."
  (with-current-buffer (get-buffer-create tychoish-cache--buffer-name)
    (setq tychoish-cache--resolved-instance-id nil))
  (cl-letf (((symbol-function 'daemonp) (lambda () nil)))
    (let ((cli/instance-id nil)
          (tychoish/emacs-instance-id nil))
      (should (equal "solo" (tychoish/resolve-instance-id))))))

(ert-deftest bootstrap/resolve-instance-id-uses-cache ()
  "Returns the cached value without recomputing."
  (with-current-buffer (get-buffer-create tychoish-cache--buffer-name)
    (setq tychoish-cache--resolved-instance-id "cached-value"))
  (should (equal "cached-value" (tychoish/resolve-instance-id))))

(ert-deftest bootstrap/resolve-instance-id-returns-string ()
  "Return value is always a string."
  (with-current-buffer (get-buffer-create tychoish-cache--buffer-name)
    (setq tychoish-cache--resolved-instance-id nil))
  (cl-letf (((symbol-function 'daemonp) (lambda () nil)))
    (let ((cli/instance-id nil) (tychoish/emacs-instance-id nil))
      (should (stringp (tychoish/resolve-instance-id))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tychoish-get-config-file-prefix

(ert-deftest bootstrap/config-file-prefix-basic ()
  "Joins host, instance, and name with hyphens."
  (cl-letf (((symbol-function 'tychoish/conf-emacs-host-and-instance)
             (lambda () (list "myhost" "solo")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (equal "myhost-solo-eshell"
                     (tychoish-get-config-file-prefix "eshell"))))))

(ert-deftest bootstrap/config-file-prefix-root-includes-login ()
  "Prefix includes login name when running as root."
  (cl-letf (((symbol-function 'tychoish/conf-emacs-host-and-instance)
             (lambda () (list "myhost" "solo")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "root"))
      (should (string-match-p "root" (tychoish-get-config-file-prefix "bookmarks"))))))

(ert-deftest bootstrap/config-file-prefix-returns-string ()
  "Return value is always a string."
  (cl-letf (((symbol-function 'tychoish/conf-emacs-host-and-instance)
             (lambda () (list "host" "inst")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (stringp (tychoish-get-config-file-prefix "test"))))))

(ert-deftest bootstrap/config-file-prefix-no-empty-segments ()
  "Prefix does not produce leading, trailing, or consecutive hyphens."
  (cl-letf (((symbol-function 'tychoish/conf-emacs-host-and-instance)
             (lambda () (list "host" "daemon")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (let ((result (tychoish-get-config-file-prefix "bookmarks")))
        (should-not (string-match-p "^-\\|--\\|-$" result))))))

(ert-deftest bootstrap/config-file-prefix-contains-name ()
  "The NAME argument appears in the result."
  (cl-letf (((symbol-function 'tychoish/conf-emacs-host-and-instance)
             (lambda () (list "host" "inst")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (string-match-p "recentf" (tychoish-get-config-file-prefix "recentf"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tychoish/conf-state-path

(ert-deftest bootstrap/conf-state-path-returns-string ()
  "Returns a string path."
  (cl-letf (((symbol-function 'tychoish/conf-emacs-host-and-instance)
             (lambda () (list "host" "inst")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (stringp (tychoish/conf-state-path "bookmarks"))))))

(ert-deftest bootstrap/conf-state-path-contains-state-dir ()
  "Result path contains the state directory name."
  (cl-letf (((symbol-function 'tychoish/conf-emacs-host-and-instance)
             (lambda () (list "host" "inst")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (string-match-p tychoish/conf-state-directory-name
                              (tychoish/conf-state-path "bookmarks"))))))

(ert-deftest bootstrap/conf-state-path-rooted-in-emacs-dir ()
  "Result path is under `user-emacs-directory'."
  (cl-letf (((symbol-function 'tychoish/conf-emacs-host-and-instance)
             (lambda () (list "host" "inst")))
            ((symbol-function 'f-symlink-p) (lambda (_) nil)))
    (let ((user-login-name "user"))
      (should (string-prefix-p user-emacs-directory
                               (tychoish/conf-state-path "bookmarks"))))))

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
;;; font-lock-width-keyword

(ert-deftest bootstrap/font-lock-width-keyword-returns-list ()
  "Returns a non-nil list."
  (should (listp (font-lock-width-keyword 80)))
  (should (font-lock-width-keyword 80)))

(ert-deftest bootstrap/font-lock-width-keyword-regexp-matches-long ()
  "The keyword regexp matches a line longer than the specified width."
  (let* ((kw (font-lock-width-keyword 80))
         (re (caar kw))
         (long-line (make-string 85 ?a)))
    (should (string-match re long-line))))

(ert-deftest bootstrap/font-lock-width-keyword-regexp-no-match-short ()
  "The keyword regexp does not match a line at or under the width."
  (let* ((kw (font-lock-width-keyword 80))
         (re (caar kw))
         (short-line (make-string 79 ?a)))
    (should-not (string-match re short-line))))

(ert-deftest bootstrap/font-lock-width-keyword-uses-warning-face ()
  "The keyword specifies `font-lock-warning-face'."
  (let ((result (font-lock-width-keyword 80)))
    (should (eq 'font-lock-warning-face (nth 1 (cadar result))))))

(ert-deftest bootstrap/font-lock-width-keyword-respects-width ()
  "Different widths produce different regexps."
  (let ((kw-60 (caar (font-lock-width-keyword 60)))
        (kw-120 (caar (font-lock-width-keyword 120))))
    (should-not (equal kw-60 kw-120))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-lock-show-tabs

(ert-deftest bootstrap/font-lock-show-tabs-returns-list ()
  "Returns a non-nil list."
  (should (listp (font-lock-show-tabs)))
  (should (font-lock-show-tabs)))

(ert-deftest bootstrap/font-lock-show-tabs-matches-tab ()
  "The pattern in the result matches a literal tab character."
  (let* ((result (font-lock-show-tabs))
         (pattern (caar result)))
    (should (string-match pattern "\t"))))

(ert-deftest bootstrap/font-lock-show-tabs-no-match-space ()
  "The tab pattern does not match a space."
  (let* ((result (font-lock-show-tabs))
         (pattern (caar result)))
    (should-not (string-match pattern " "))))

(ert-deftest bootstrap/font-lock-show-tabs-pure ()
  "Returns an equal value on every call (no side effects)."
  (should (equal (font-lock-show-tabs) (font-lock-show-tabs))))

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
;;; tychoish/set-tab-width (macro)

(ert-deftest bootstrap/set-tab-width-macro-defines-function ()
  "Macro defines a function with the expected name."
  (tychoish/set-tab-width 4)
  (should (fboundp 'tychoish/set-local-tab-width-4)))

(ert-deftest bootstrap/set-tab-width-macro-generated-fn-works ()
  "Generated function sets `tab-width' buffer-locally."
  (tychoish/set-tab-width 6)
  (with-temp-buffer
    (tychoish/set-local-tab-width-6)
    (should (= 6 tab-width))))

(ert-deftest bootstrap/set-tab-width-macro-rejects-non-integer ()
  "Macro signals `wrong-type-argument' for a non-integer argument."
  (should-error (eval '(tychoish/set-tab-width "four") t)
                :type 'wrong-type-argument))

(ert-deftest bootstrap/set-tab-width-macro-rejects-float ()
  "Macro signals `wrong-type-argument' for a float argument."
  (should-error (eval '(tychoish/set-tab-width 4.0) t)
                :type 'wrong-type-argument))

(ert-deftest bootstrap/set-tab-width-macro-large-warns ()
  "Macro calls `warn' for a tab width >= 32."
  (let ((warned nil))
    (cl-letf (((symbol-function 'warn) (lambda (&rest _) (setq warned t))))
      (eval '(tychoish/set-tab-width 32) t))
    (should warned)))

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
;;; tychoish-setup-font

(ert-deftest bootstrap/setup-font-adds-entry ()
  "Adds a font entry to `default-frame-alist' when absent."
  (let ((default-frame-alist nil))
    (tychoish-setup-font "Monospace" 12)
    (should (assoc 'font default-frame-alist))))

(ert-deftest bootstrap/setup-font-name-size-format ()
  "Font value is \"NAME-SIZE\"."
  (let ((default-frame-alist nil))
    (tychoish-setup-font "Monospace" 12)
    (should (equal "Monospace-12" (cdr (assoc 'font default-frame-alist))))))

(ert-deftest bootstrap/setup-font-updates-existing-in-place ()
  "Updates an existing font entry without creating a duplicate."
  (let ((default-frame-alist (list (cons 'font "OldFont-10"))))
    (tychoish-setup-font "NewFont" 14)
    (should (= 1 (length (--filter (eq (car it) 'font) default-frame-alist))))
    (should (equal "NewFont-14" (cdr (assoc 'font default-frame-alist))))))

(ert-deftest bootstrap/setup-font-returns-alist-entry ()
  "Returns a cons cell of the form (font . string)."
  (let ((default-frame-alist nil))
    (let ((result (tychoish-setup-font "Mono" 11)))
      (should (consp result))
      (should (eq 'font (car result)))
      (should (stringp (cdr result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tychoish/ensure-font

(ert-deftest bootstrap/ensure-font-sets-when-absent ()
  "Sets font when no font entry exists in `default-frame-alist'."
  (let ((default-frame-alist nil))
    (tychoish/ensure-font "Mono" 12)
    (should (assoc 'font default-frame-alist))))

(ert-deftest bootstrap/ensure-font-noop-when-present ()
  "Does nothing when a font entry already exists."
  (let ((default-frame-alist (list (cons 'font "Existing-10"))))
    (tychoish/ensure-font "Other" 14)
    (should (equal "Existing-10" (cdr (assoc 'font default-frame-alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; disable-all-themes / tychoish/ensure-*-theme

(ert-deftest bootstrap/ensure-light-theme-noop-when-active ()
  "Does not call load function when a theme is already enabled."
  (let ((custom-enabled-themes '(modus-operandi)))
    (cl-letf (((symbol-function 'tychoish-load-light-theme)
               (lambda () (error "should not be called"))))
      (should-not (tychoish/ensure-light-theme)))))

(ert-deftest bootstrap/ensure-light-theme-loads-when-empty ()
  "Calls `tychoish-load-light-theme' when no theme is active."
  (let ((custom-enabled-themes nil)
        (called nil))
    (cl-letf (((symbol-function 'tychoish-load-light-theme)
               (lambda () (setq called t))))
      (tychoish/ensure-light-theme)
      (should called))))

(ert-deftest bootstrap/ensure-dark-theme-noop-when-active ()
  "Does not call load function when a theme is already enabled."
  (let ((custom-enabled-themes '(modus-vivendi)))
    (cl-letf (((symbol-function 'tychoish-load-dark-theme)
               (lambda () (error "should not be called"))))
      (should-not (tychoish/ensure-dark-theme)))))

(ert-deftest bootstrap/ensure-dark-theme-loads-when-empty ()
  "Calls `tychoish-load-dark-theme' when no theme is active."
  (let ((custom-enabled-themes nil)
        (called nil))
    (cl-letf (((symbol-function 'tychoish-load-dark-theme)
               (lambda () (setq called t))))
      (tychoish/ensure-dark-theme)
      (should called))))

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
;;; project-find-go-module

(ert-deftest bootstrap/project-find-go-module-finds-go-mod ()
  "Returns a cons with head `go-module' when go.mod is present."
  (let ((dir (make-temp-file "ert-go-mod-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "go.mod" dir))
          (let ((result (project-find-go-module dir)))
            (should result)
            (should (eq 'go-module (car result)))
            (should (stringp (cdr result)))))
      (delete-directory dir t))))

(ert-deftest bootstrap/project-find-go-module-finds-go-work ()
  "Returns a match when go.work is present."
  (let ((dir (make-temp-file "ert-go-work-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "go.work" dir))
          (should (project-find-go-module dir)))
      (delete-directory dir t))))

(ert-deftest bootstrap/project-find-go-module-returns-nil-non-go ()
  "Returns nil in a directory with no Go module files."
  (let ((dir (make-temp-file "ert-not-go-" t)))
    (unwind-protect
        (should-not (project-find-go-module dir))
      (delete-directory dir t))))

(ert-deftest bootstrap/project-find-go-module-return-type ()
  "Return value is a cons cell (go-module . path) or nil."
  (let ((dir (make-temp-file "ert-go-type-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "go.mod" dir))
          (let ((result (project-find-go-module dir)))
            (should (consp result))
            (should (eq 'go-module (car result)))))
      (delete-directory dir t))))

(ert-deftest bootstrap/project-find-go-module-finds-in-parent ()
  "Finds go.mod in a parent directory."
  (let ((dir (make-temp-file "ert-go-parent-" t)))
    (unwind-protect
        (let ((subdir (expand-file-name "pkg/sub" dir)))
          (make-directory subdir t)
          (write-region "" nil (expand-file-name "go.mod" dir))
          (let ((result (project-find-go-module subdir)))
            (should result)
            (should (eq 'go-module (car result)))))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; project-find-cmake-project

(ert-deftest bootstrap/project-find-cmake-finds-cmake ()
  "Returns a cons with head `cmake-root' when CMakeLists.txt is present."
  (let ((dir (make-temp-file "ert-cmake-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "CMakeLists.txt" dir))
          (let ((result (project-find-cmake-project dir)))
            (should result)
            (should (eq 'cmake-root (car result)))))
      (delete-directory dir t))))

(ert-deftest bootstrap/project-find-cmake-returns-nil-non-cmake ()
  "Returns nil in a non-CMake directory."
  (let ((dir (make-temp-file "ert-not-cmake-" t)))
    (unwind-protect
        (should-not (project-find-cmake-project dir))
      (delete-directory dir t))))

(ert-deftest bootstrap/project-find-cmake-return-type ()
  "Return value is a cons cell (cmake-root . path) or nil."
  (let ((dir (make-temp-file "ert-cmake-type-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "CMakeLists.txt" dir))
          (let ((result (project-find-cmake-project dir)))
            (should (consp result))
            (should (eq 'cmake-root (car result)))))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; should-read-abbrev-file-p

(ert-deftest bootstrap/should-read-abbrev-not-in-cache ()
  "Returns t when path is absent from cache."
  (let ((tychoish/abbrev-files-cache (ht-create)))
    (should (should-read-abbrev-file-p "/some/path.el"))))

(ert-deftest bootstrap/should-read-abbrev-fresh-cache-entry ()
  "Returns nil when cache entry is newer than the file."
  (let ((tmp (make-temp-file "ert-abbrev-")))
    (unwind-protect
        (let ((tychoish/abbrev-files-cache (ht-create)))
          (ht-set tychoish/abbrev-files-cache tmp
                  (time-add (current-time) (seconds-to-time 3600)))
          (should-not (should-read-abbrev-file-p tmp)))
      (delete-file tmp))))

(ert-deftest bootstrap/should-read-abbrev-stale-cache-entry ()
  "Returns t when the file is newer than the cache entry."
  (let ((tmp (make-temp-file "ert-abbrev-stale-")))
    (unwind-protect
        (let ((tychoish/abbrev-files-cache (ht-create)))
          (ht-set tychoish/abbrev-files-cache tmp
                  (time-subtract (current-time) (seconds-to-time 3600)))
          (should (should-read-abbrev-file-p tmp)))
      (delete-file tmp))))

(ert-deftest bootstrap/should-read-abbrev-returns-boolean ()
  "Return value is t or nil."
  (let ((tychoish/abbrev-files-cache (ht-create)))
    (let ((result (should-read-abbrev-file-p "/nope")))
      (should (or (eq result t) (eq result nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tychoish/desktop-save

(ert-deftest bootstrap/desktop-save-skips-solo-instance ()
  "Does not call `desktop-save' when instance-id is \"solo\"."
  (let ((tychoish/emacs-instance-id "solo")
        (called nil))
    (cl-letf (((symbol-function 'desktop-save) (lambda (&rest _) (setq called t))))
      (tychoish/desktop-save)
      (should-not called))))

(ert-deftest bootstrap/desktop-save-runs-for-named-instance ()
  "Calls `desktop-save' for a named instance when time threshold is exceeded."
  (let ((tychoish/emacs-instance-id "work")
        (desktop/last-save-time (time-subtract (current-time) (seconds-to-time 300)))
        (desktop-dirname "/tmp")
        (called nil))
    (cl-letf (((symbol-function 'desktop-save) (lambda (&rest _) (setq called t)))
              ((symbol-function 'random) (lambda (_) 0)))
      (tychoish/desktop-save)
      (should called))))

(ert-deftest bootstrap/desktop-save-updates-last-save-time ()
  "Updates `desktop/last-save-time' after saving."
  (let ((tychoish/emacs-instance-id "work")
        (desktop/last-save-time (time-subtract (current-time) (seconds-to-time 300)))
        (desktop-dirname "/tmp"))
    (cl-letf (((symbol-function 'desktop-save) #'ignore)
              ((symbol-function 'random) (lambda (_) 0)))
      (let ((before desktop/last-save-time))
        (tychoish/desktop-save)
        (should (time-less-p before desktop/last-save-time))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tychoish/set-up-ssh-agent

(ert-deftest bootstrap/ssh-agent-returns-existing-env ()
  "Returns the current SSH_AUTH_SOCK value when already set."
  (cl-letf (((symbol-function 'getenv)
             (lambda (var)
               (when (equal var "SSH_AUTH_SOCK")
                 "/run/user/1000/ssh-agent.socket"))))
    (should (equal "/run/user/1000/ssh-agent.socket"
                   (tychoish/set-up-ssh-agent)))))

(ert-deftest bootstrap/ssh-agent-returns-nil-when-no-socket ()
  "Returns nil when env is unset and no socket candidates exist."
  (cl-letf (((symbol-function 'getenv) (lambda (_) nil))
            ((symbol-function 'find-ssh-agent-socket-candidates) (lambda () nil)))
    (should-not (tychoish/set-up-ssh-agent))))

(ert-deftest bootstrap/ssh-agent-sets-env-from-candidate ()
  "Sets and returns SSH_AUTH_SOCK from first socket candidate."
  (let ((socket "/tmp/ssh-abc/agent.123"))
    (cl-letf (((symbol-function 'getenv) (lambda (_) nil))
              ((symbol-function 'find-ssh-agent-socket-candidates)
               (lambda () (list socket)))
              ((symbol-function 'setenv)
               (lambda (_var val) val)))
      (should (equal socket (tychoish/set-up-ssh-agent))))))

(ert-deftest bootstrap/ssh-agent-return-type ()
  "Return value is always a string or nil."
  (cl-letf (((symbol-function 'getenv) (lambda (_) nil))
            ((symbol-function 'find-ssh-agent-socket-candidates) (lambda () nil)))
    (let ((result (tychoish/set-up-ssh-agent)))
      (should (or (null result) (stringp result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tychoish/set-up-show-whitespace

(ert-deftest bootstrap/show-whitespace-enables ()
  "Sets `show-trailing-whitespace' to t in the current buffer."
  (with-temp-buffer
    (setq-local show-trailing-whitespace nil)
    (tychoish/set-up-show-whitespace)
    (should show-trailing-whitespace)))

(ert-deftest bootstrap/show-whitespace-is-buffer-local ()
  "Setting does not affect other buffers."
  (let ((other (get-buffer-create " *bootstrap-wspc-test*")))
    (unwind-protect
        (progn
          (with-current-buffer other
            (setq-local show-trailing-whitespace nil))
          (with-temp-buffer
            (tychoish/set-up-show-whitespace))
          (should-not (buffer-local-value 'show-trailing-whitespace other)))
      (kill-buffer other))))

(ert-deftest bootstrap/show-whitespace-returns-t ()
  "Return value is t."
  (with-temp-buffer
    (should (eq t (tychoish/set-up-show-whitespace)))))

(provide 'test-tychoish-bootstrap)
;;; test-tychoish-bootstrap.el ends here
