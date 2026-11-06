;;; test-bootstrap.el --- ERT tests for bootstrap.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^bootstrap/")

(require 'ert)
(require 'cl-lib)
(require 'bootstrap)
(require 'setup-defaults)
(require 'setup-package-defaults)
(require 'setup-project)
(require 'setup-hooks)

(require 'setup-core)

(defconst bootstrap-cache--buffer-name " bootstrap-cache-buffer")


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
;;; hud-set-tab-width (macro)

(ert-deftest bootstrap/set-tab-width-macro-defines-function ()
  "Macro defines a function with the expected name."
  (hud-set-tab-width 4)
  (should (fboundp 'hud-set-local-tab-width-4)))

(ert-deftest bootstrap/set-tab-width-macro-generated-fn-works ()
  "Generated function sets `tab-width' buffer-locally."
  (hud-set-tab-width 6)
  (with-temp-buffer
    (hud-set-local-tab-width-6)
    (should (= 6 tab-width))))

(ert-deftest bootstrap/set-tab-width-macro-rejects-non-integer ()
  "Macro signals `wrong-type-argument' for a non-integer argument."
  (should-error (eval '(hud-set-tab-width "four") t)
                :type 'wrong-type-argument))

(ert-deftest bootstrap/set-tab-width-macro-rejects-float ()
  "Macro signals `wrong-type-argument' for a float argument."
  (should-error (eval '(hud-set-tab-width 4.0) t)
                :type 'wrong-type-argument))

(ert-deftest bootstrap/set-tab-width-macro-large-warns ()
  "Macro calls `warn' for a tab width >= 32."
  (let ((warned nil))
    (cl-letf (((symbol-function 'warn) (lambda (&rest _) (setq warned t))))
      (eval '(hud-set-tab-width 32) t))
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
;;; bootstrap-setup-font

(ert-deftest bootstrap/setup-font-adds-entry ()
  "Adds a font entry to `default-frame-alist' when absent."
  (let ((default-frame-alist nil))
    (cl-letf (((symbol-function 'set-frame-font) #'ignore))
      (bootstrap-setup-font "Monospace" 12)
      (should (assoc 'font default-frame-alist)))))

(ert-deftest bootstrap/setup-font-name-size-format ()
  "Font value is \"NAME-SIZE\"."
  (let ((default-frame-alist nil))
    (cl-letf (((symbol-function 'set-frame-font) #'ignore))
      (bootstrap-setup-font "Monospace" 12)
      (should (equal "Monospace-12" (cdr (assoc 'font default-frame-alist)))))))

(ert-deftest bootstrap/setup-font-updates-existing-in-place ()
  "Updates an existing font entry without creating a duplicate."
  (let ((default-frame-alist (list (cons 'font "OldFont-10"))))
    (cl-letf (((symbol-function 'set-frame-font) #'ignore))
      (bootstrap-setup-font "NewFont" 14)
      (should (= 1 (length (seq-filter (lambda (it) (eq (car it) 'font)) default-frame-alist))))
      (should (equal "NewFont-14" (cdr (assoc 'font default-frame-alist)))))))

(ert-deftest bootstrap/setup-font-returns-alist-entry ()
  "Returns a cons cell of the form (font . string)."
  (let ((default-frame-alist nil))
    (cl-letf (((symbol-function 'set-frame-font) #'ignore))
      (let ((result (bootstrap-setup-font "Mono" 11)))
        (should (consp result))
        (should (eq 'font (car result)))
        (should (stringp (cdr result)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bootstrap-ensure-font

(ert-deftest bootstrap/ensure-font-sets-when-absent ()
  "Sets font when no font entry exists in `default-frame-alist'."
  (let ((default-frame-alist nil))
    (cl-letf (((symbol-function 'set-frame-font) #'ignore))
      (bootstrap-ensure-font "Mono" 12)
      (should (assoc 'font default-frame-alist)))))

(ert-deftest bootstrap/ensure-font-noop-when-present ()
  "Does nothing when a font entry already exists."
  (let ((default-frame-alist (list (cons 'font "Existing-10"))))
    (cl-letf (((symbol-function 'set-frame-font) #'ignore))
      (bootstrap-ensure-font "Other" 14)
      (should (equal "Existing-10" (cdr (assoc 'font default-frame-alist)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; disable-all-themes / bootstrap-ensure-*-theme

(ert-deftest bootstrap/ensure-light-theme-noop-when-active ()
  "Does not call load function when a theme is already enabled."
  (let ((custom-enabled-themes '(modus-operandi)))
    (cl-letf (((symbol-function 'bootstrap-load-light-theme)
               (lambda () (error "should not be called"))))
      (should-not (bootstrap-ensure-light-theme)))))

(ert-deftest bootstrap/ensure-light-theme-loads-when-empty ()
  "Calls `bootstrap-load-light-theme' when no theme is active."
  (let ((custom-enabled-themes nil)
        (called nil))
    (cl-letf (((symbol-function 'bootstrap-load-light-theme)
               (lambda () (setq called t))))
      (bootstrap-ensure-light-theme)
      (should called))))

(ert-deftest bootstrap/ensure-dark-theme-noop-when-active ()
  "Does not call load function when a theme is already enabled."
  (let ((custom-enabled-themes '(modus-vivendi)))
    (cl-letf (((symbol-function 'bootstrap-load-dark-theme)
               (lambda () (error "should not be called"))))
      (should-not (bootstrap-ensure-dark-theme)))))

(ert-deftest bootstrap/ensure-dark-theme-loads-when-empty ()
  "Calls `bootstrap-load-dark-theme' when no theme is active."
  (let ((custom-enabled-themes nil)
        (called nil))
    (cl-letf (((symbol-function 'bootstrap-load-dark-theme)
               (lambda () (setq called t))))
      (bootstrap-ensure-dark-theme)
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
  (let ((bootstrap-abbrev-files-cache (make-hash-table :test #'equal)))
    (should (should-read-abbrev-file-p "/some/path.el"))))

(ert-deftest bootstrap/should-read-abbrev-fresh-cache-entry ()
  "Returns nil when cache entry is newer than the file."
  (let ((tmp (make-temp-file "ert-abbrev-")))
    (unwind-protect
        (let ((bootstrap-abbrev-files-cache (make-hash-table :test #'equal)))
          (setf (map-elt bootstrap-abbrev-files-cache tmp)
                  (time-add (current-time) (seconds-to-time 3600)))
          (should-not (should-read-abbrev-file-p tmp)))
      (delete-file tmp))))

(ert-deftest bootstrap/should-read-abbrev-stale-cache-entry ()
  "Returns t when the file is newer than the cache entry."
  (let ((tmp (make-temp-file "ert-abbrev-stale-")))
    (unwind-protect
        (let ((bootstrap-abbrev-files-cache (make-hash-table :test #'equal)))
          (setf (map-elt bootstrap-abbrev-files-cache tmp)
                  (time-subtract (current-time) (seconds-to-time 3600)))
          (should (should-read-abbrev-file-p tmp)))
      (delete-file tmp))))

(ert-deftest bootstrap/should-read-abbrev-returns-boolean ()
  "Return value is t or nil."
  (let ((bootstrap-abbrev-files-cache (make-hash-table :test #'equal)))
    (let ((result (should-read-abbrev-file-p "/nope")))
      (should (or (eq result t) (eq result nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bootstrap-desktop-save

(ert-deftest bootstrap/desktop-save-skips-solo-instance ()
  "Does not call `desktop-save' when instance-id is \"solo\"."
  (let ((sprite-instance-id "solo")
        (called nil))
    (cl-letf (((symbol-function 'desktop-save) (lambda (&rest _) (setq called t))))
      (bootstrap-desktop-save)
      (should-not called))))

(ert-deftest bootstrap/desktop-save-runs-for-named-instance ()
  "Calls `desktop-save' for a named instance when time threshold is exceeded."
  (let ((sprite-instance-id "work")
        (desktop/last-save-time (time-subtract (current-time) (seconds-to-time 300)))
        (desktop-dirname "/tmp")
        (called nil))
    (cl-letf (((symbol-function 'desktop-save) (lambda (&rest _) (setq called t)))
              ((symbol-function 'random) (lambda (_) 0)))
      (bootstrap-desktop-save)
      (should called))))

(ert-deftest bootstrap/desktop-save-updates-last-save-time ()
  "Updates `desktop/last-save-time' after saving."
  (let ((sprite-instance-id "work")
        (desktop/last-save-time (time-subtract (current-time) (seconds-to-time 300)))
        (desktop-dirname "/tmp"))
    (cl-letf (((symbol-function 'desktop-save) #'ignore)
              ((symbol-function 'random) (lambda (_) 0)))
      (let ((before desktop/last-save-time))
        (bootstrap-desktop-save)
        (should (time-less-p before desktop/last-save-time))))))

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
            ((symbol-function 'find-ssh-agent-socket-candidates) (lambda () nil)))
    (should-not (bootstrap-set-up-ssh-agent))))

(ert-deftest bootstrap/ssh-agent-sets-env-from-candidate ()
  "Sets and returns SSH_AUTH_SOCK from first socket candidate."
  (let ((socket "/tmp/ssh-abc/agent.123"))
    (cl-letf (((symbol-function 'getenv) (lambda (_) nil))
              ((symbol-function 'find-ssh-agent-socket-candidates)
               (lambda () (list socket)))
              ((symbol-function 'setenv)
               (lambda (_var val) val)))
      (should (equal socket (bootstrap-set-up-ssh-agent))))))

(ert-deftest bootstrap/ssh-agent-return-type ()
  "Return value is always a string or nil."
  (cl-letf (((symbol-function 'getenv) (lambda (_) nil))
            ((symbol-function 'find-ssh-agent-socket-candidates) (lambda () nil)))
    (let ((result (bootstrap-set-up-ssh-agent)))
      (should (or (null result) (stringp result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bootstrap-trailing-whitespace-mode

(ert-deftest bootstrap/show-whitespace-global-mode ()
  "Enable existing and future programming/text buffers, and disable them."
  (let ((prog-mode-hook nil)
        (text-mode-hook nil)
        (bootstrap-trailing-whitespace-mode nil)
        (code (generate-new-buffer " *bootstrap-whitespace-code*"))
        (text (generate-new-buffer " *bootstrap-whitespace-text*"))
        (other (generate-new-buffer " *bootstrap-whitespace-other*")))
    (unwind-protect
        (progn
          (with-current-buffer code (prog-mode))
          (with-current-buffer other (fundamental-mode))
          (cl-letf (((symbol-function 'buffer-list) (lambda (&rest _) (list code text other))))
            (bootstrap-trailing-whitespace-mode 1)
            (should (buffer-local-value 'show-trailing-whitespace code))
            (should-not (buffer-local-value 'show-trailing-whitespace other))
            (with-current-buffer text (text-mode))
            (should (buffer-local-value 'show-trailing-whitespace text))
            (bootstrap-trailing-whitespace-mode -1)
            (should-not (buffer-local-value 'show-trailing-whitespace code))
            (should-not (buffer-local-value 'show-trailing-whitespace text))
            (should-not (memq #'bootstrap--set-trailing-whitespace prog-mode-hook))
            (should-not (memq #'bootstrap--set-trailing-whitespace text-mode-hook))))
      (kill-buffer code)
      (kill-buffer text)
      (kill-buffer other))))

(ert-deftest bootstrap/show-whitespace-mode-starts-early ()
  "The global mode is included in the early startup operations."
  (should (memq 'bootstrap-trailing-whitespace-mode bootstrap-early-startup-operations)))

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
  (cl-letf (((symbol-function 'buffer-list) (lambda () nil)))
    (should-not (kill-buffers-visiting-missing-files))))

(ert-deftest bootstrap/kill-missing-file-buffers-returns-list ()
  "Return value is always a list."
  (cl-letf (((symbol-function 'buffer-list) (lambda () nil)))
    (should (listp (kill-buffers-visiting-missing-files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delight mode lighters

(defmacro bootstrap-test/with-delight-applied (&rest body)
  "Run BODY after ensuring delight lighters are applied."
  (declare (indent 0))
  `(progn
     (require 'delight)
     ;; don't need to set up lighters here because they're loaded from use-package
     ,@body))

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

(ert-deftest bootstrap/ensure-lazy-keyword-is-registered ()
  "Test that :ensure-lazy is registered as a keyword in use-package."
  (should (member :ensure-lazy use-package-keywords))
  (should (fboundp 'use-package-handler/:ensure-lazy)))

(ert-deftest bootstrap/ensure-lazy-handler-expansion-no-commands ()
  "Test that `:ensure-lazy' handler expands to immediate require/install when no commands are given."
  (let* ((expanded (use-package-handler/:ensure-lazy
                    'my-test-package :ensure-lazy t nil nil)))
    ;; Without commands, it should expand to installing/requiring immediately
    (should (string-match-p "package-install" (format "%S" expanded)))
    (should (string-match-p "require" (format "%S" expanded)))))

(ert-deftest bootstrap/ensure-lazy-handler-expansion-with-commands ()
  "Test that `:ensure-lazy' handler expands to autoload wrappers when commands are given."
  (let* ((state (list :commands '(my-test-cmd-1 my-test-cmd-2)))
         (expanded (use-package-handler/:ensure-lazy
                    'my-test-package :ensure-lazy t nil state)))
    ;; With commands, it should define wrapper functions for the commands
    (should (string-match-p "defun my-test-cmd-1" (format "%S" expanded)))
    (should (string-match-p "defun my-test-cmd-2" (format "%S" expanded)))
    (should (string-match-p "package-install" (format "%S" expanded)))))

(ert-deftest bootstrap/ensure-handler-skips-when-lazy-present ()
  "Test that standard `:ensure' handler does nothing when `:ensure-lazy' is present."
  (let (orig-called)
    (cl-letf (((symbol-function 'use-package-process-keywords)
               (lambda (_name rest _state) (list 'skipped rest))))
      (let ((result (ad:use-package-handler/:ensure-skip-lazy
                     (lambda (&rest _) (setq orig-called t))
                     'my-test-package :ensure t '(:ensure-lazy t) '(:ensure-lazy t))))
        (should-not orig-called)
        (should (equal '(skipped (:ensure-lazy t)) result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hud-transient-insert-suffix-once

(ert-deftest bootstrap/transient-insert-suffix-once-idempotent ()
  "hud-transient-insert-suffix-once inserts only once and handles missing layouts."
  (require 'transient)
  ;; 1. No-op without error when prefix has no layout
  (should (null (hud-transient-insert-suffix-once
                 'bootstrap--test-nonexistent-menu
                 '(-1 0)
                 '("x" "test command" ignore))))
  ;; 2. Inserts suffix into a defined prefix
  (transient-define-prefix bootstrap--test-ert-menu ()
    ["Test"
     ("a" "Action A" ignore)])
  (should (hud-transient-insert-suffix-once
           'bootstrap--test-ert-menu
           '(-1 0)
           '("x" "test command" ignore)))
  (should (ignore-errors (transient-get-suffix 'bootstrap--test-ert-menu "x")))
  ;; 3. Second call is idempotent (no-ops and does not duplicate or error)
  (should (null (hud-transient-insert-suffix-once
                 'bootstrap--test-ert-menu
                 '(-1 0)
                 '("x" "test command" ignore))))
  (should (ignore-errors (transient-get-suffix 'bootstrap--test-ert-menu "x"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bootstrap-run-startup-operations (generator iterator)

(ert-deftest bootstrap/chex-mode-available-to-startup-hook ()
  "The late startup hook can invoke Chex before it has been loaded."
  (should (memq 'chex-global-mode bootstrap-late-startup-operations))
  (should (fboundp 'chex-global-mode)))

(ert-deftest bootstrap/run-enable-modes-hook-noninteractive ()
  "Executes all hook functions in order and calls on-done when noninteractive."
  (let* ((order nil)
         (done nil)
         (noninteractive t)
         (test-hook (list (lambda (arg) (push (cons 'f1 arg) order))
                          (lambda () (push 'f2 order))
                          (lambda (arg) (push (cons 'f3 arg) order)))))
    (bootstrap-run-startup-operations test-hook :on-done (lambda () (setq done t)))
    (should (equal '((f1 . 1) f2 (f3 . 1)) (nreverse order)))
    (should (eq t done))))

(ert-deftest bootstrap/startup-operation-uses-actual-arity ()
  "Enable optional-argument modes and call zero-argument operations without arguments."
  (let ((calls nil))
    (cl-letf (((symbol-function 'help-function-arglist)
               (lambda (&rest _) '(&rest args))))
      (bootstrap--execute-startup-operation (lambda () (push 'zero calls)))
      (bootstrap--execute-startup-operation (lambda (&optional arg) (push arg calls)))
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
          (bootstrap--execute-startup-operation 'bootstrap--test-autoload-zero)
          (should (eq bootstrap--test-autoload-result 'zero))
          (bootstrap--execute-startup-operation 'bootstrap--test-autoload-mode)
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
    (bootstrap-run-startup-operations test-hook :on-done (lambda () (setq done t)))
    (should (equal '(f1 f3) (nreverse order)))
    (should (eq t done))))

(ert-deftest bootstrap/startup-mcp-operation-passes-no-mode-argument ()
  "The MCP initializer is a startup operation and receives no mode argument."
  (let ((called nil)
        (noninteractive t))
    (should (memq 'mcpkit-init-services bootstrap-late-startup-operations))
    (cl-letf (((symbol-function 'mcpkit-init-services)
               (lambda () (setq called t))))
      (bootstrap-run-startup-operations '(mcpkit-init-services))
      (should called))))

(ert-deftest bootstrap/require-package-defines-startup-operation ()
  "Bare and quoted feature symbols define a zero-argument requiring function."
  (should (eq 'bootstrap-require-pkg-subr-x
              (bootstrap-require-package subr-x)))
  (should (eq 'bootstrap-require-pkg-seq
              (bootstrap-require-package 'seq)))
  (should (eq 0 (car (func-arity 'bootstrap-require-pkg-seq))))
  (should (null (bootstrap-require-pkg-seq)))
  (should (memq 'bootstrap-require-pkg-setup-project
                bootstrap-late-startup-operations))
  (should (fboundp 'bootstrap-require-pkg-setup-project)))

(ert-deftest bootstrap/require-package-validates-feature-provided ()
  "The defined function errors when requiring does not provide the feature."
  (bootstrap-require-package bootstrap-test-never-provided)
  (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
    (should-error (bootstrap-require-pkg-bootstrap-test-never-provided))))

(ert-deftest bootstrap/require-package-rejects-non-symbols ()
  "Non-symbol arguments fail at macro-expansion time."
  (should-error (macroexpand '(bootstrap-require-package "seq")))
  (should-error (macroexpand '(bootstrap-require-package nil))))

(ert-deftest bootstrap/post-late-startup-runs-package-operations-in-order ()
  "Start installation asynchronously and continue with package maintenance."
  (let ((order nil)
        (noninteractive t))
    (cl-letf (((symbol-function 'bootstrap-require-pkg-setup-packages)
               (lambda () (push 'require order)))
              ((symbol-function 'elpaish-install-packages)
               (lambda (&rest args)
                 (should (plist-get args :async))
                 (push 'install order)))
              ((symbol-function 'bootstrap-check-package-quickstart)
               (lambda () (push 'refresh order)))
              ((symbol-function 'builder-emacs-conf-native-compile-all)
               (lambda () (push 'compile order))))
      (bootstrap-run-startup-operations bootstrap-post-late-startup-operations)
      (should (equal (reverse order) '(require install refresh compile))))))

(ert-deftest bootstrap/frame-alpha-preserves-configured-default ()
  "Apply an existing default opacity to a frame without an explicit value."
  (let ((default-frame-alist '((alpha . 87)))
        (applied nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'frame-parameter) (lambda (&rest _) nil))
              ((symbol-function 'set-frame-parameter)
               (lambda (_frame parameter value) (setq applied (cons parameter value)))))
      (bootstrap-ensure-frame-alpha)
      (should (equal applied '(alpha . 87)))
      (should (equal default-frame-alist '((alpha . 87)))))))

(ert-deftest bootstrap/run-enable-modes-hook-cancel ()
  "Cancellation function halts pending execution in interactive mode."
  (let* ((order nil)
         (noninteractive nil)
         (test-hook (list (lambda (arg) (push 'f1 order))
                          (lambda (arg) (push 'f2 order))))
         (cancel-fn (bootstrap-run-startup-operations test-hook :delay 10.0)))
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
  "Test that bootstrap-optimize-quickstart-file streamlines quickstart contents."
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
          (bootstrap-optimize-quickstart-file tmp-file)
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

(ert-deftest bootstrap/quickstart-compilation-is-silent-on-errors ()
  "Compiler errors during quickstart refresh do not display or log warnings."
  (require 'bytecomp)
  (let ((file (make-temp-file "quickstart-error-" nil ".el"))
        (byte-compile-log-buffer " *quickstart-test-compile-log*")
        (warning-inhibit-types nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert ";;; -*- lexical-binding: t; -*-\n(\n"))
          (ad:package-quickstart-refresh--optimize
           (lambda () (byte-compile-file file)))
          (when-let* ((buffer (get-buffer byte-compile-log-buffer)))
            (with-current-buffer buffer
              (should-not (string-match-p "Error:" (buffer-string))))))
      (when-let* ((buffer (get-buffer byte-compile-log-buffer)))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest bootstrap/package-quickstart-stale-p-detects-missing-elc ()
  "Test that stale-p returns non-nil when the .elc file is missing."
  (let* ((tmp-el (make-temp-file "test-qs-" nil ".el"))
         (package-quickstart-file tmp-el))
    (unwind-protect
        (should (bootstrap-package-quickstart-stale-p))
      (when (file-exists-p tmp-el) (delete-file tmp-el)))))

(ert-deftest bootstrap/startup-refreshes-quickstart-only-when-stale ()
  "Startup checks freshness before refreshing the quickstart."
  (let ((stale nil)
        (refreshes 0))
    (cl-letf (((symbol-function 'bootstrap-package-quickstart-stale-p)
               (lambda () stale))
              ((symbol-function 'bootstrap-sync-package-load-paths) #'ignore)
              ((symbol-function 'package-quickstart-refresh)
               (lambda () (setq refreshes (1+ refreshes)))))
      (should (memq 'bootstrap-check-package-quickstart
                    bootstrap-post-late-startup-operations))
      (bootstrap--execute-startup-operation 'bootstrap-check-package-quickstart)
      (should (= refreshes 0))
      (setq stale t)
      (bootstrap--execute-startup-operation 'bootstrap-check-package-quickstart)
      (should (= refreshes 1)))))

(ert-deftest bootstrap/elpaish-refreshes-quickstart-after-changes ()
  "Only successful package changes schedule a quickstart refresh."
  (let ((refreshes 0))
    (cl-letf (((symbol-function 'ad:refresh-package-quickstart)
               (lambda (&rest _) (setq refreshes (1+ refreshes)))))
      (bootstrap-refresh-quickstart-after-elpaish '((:status :failed) (:status :up-to-date)) 'install)
      (should (= refreshes 0))
      (bootstrap-refresh-quickstart-after-elpaish '((:status :installed)) 'install)
      (bootstrap-refresh-quickstart-after-elpaish '((:status :upgraded)) 'upgrade)
      (should (= refreshes 2)))))

(provide (quote test-bootstrap))
;;; test-bootstrap.el ends here
