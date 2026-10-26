;;; test-grepx.el --- ERT tests for grepx.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'test-helper)
(require 'cl-lib)
(require 'deadgrep)
(require 'consult nil t)
(require 'hud-mode)
(require 'grepx)
(require 'minaid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode Lifecycle Tests

(ert-deftest grepx-test/lifecycle-toggle ()
  "Test that grepx-mode enables and disables cleanly, managing advice."
  ;; Initially disable to test clean transition
  (grepx-mode -1)
  (should-not grepx-mode)

  ;; Enable
  (grepx-mode 1)
  (should grepx-mode)
  (should (advice-member-p #'grepx--heading-wrapper 'deadgrep--write-heading))
  (should (advice-member-p #'grepx--next-error-wrapper 'deadgrep-next-error))
  (should (advice-member-p #'grepx--consult-grep-wrapper 'consult--grep))

  ;; Disable
  (grepx-mode -1)
  (should-not grepx-mode)
  (should-not (advice-member-p #'grepx--heading-wrapper 'deadgrep--write-heading))
  (should-not (advice-member-p #'grepx--next-error-wrapper 'deadgrep-next-error))
  (should-not (advice-member-p #'grepx--consult-grep-wrapper 'consult--grep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compact Header & Toggle Tests

(ert-deftest grepx-test/compact-header-rendering ()
  "Test that grepx--write-compact-heading writes the 1-line compact header."
  (with-temp-buffer
    (let ((deadgrep--search-term "test-query")
          (deadgrep--search-type 'regexp)
          (deadgrep--search-case 'smart)
          (deadgrep--file-type 'all)
          (default-directory "/tmp/"))
      (grepx--write-compact-heading)
      (let ((content (buffer-string)))
        (should (string-match-p "\\[deadgrep\\]" content))
        (should (string-match-p "\"test-query\"" content))
        (should (string-match-p "\\[regexp, smart, files: all\\]" content))
        (should (string-match-p "\\[\\?\\]" content))
        (should (get-text-property (point-min) 'grepx-heading))
        (should (get-text-property (point-min) 'read-only))))))

(ert-deftest grepx-test/toggle-header ()
  "Test toggling between compact and standard button header."
  (with-temp-buffer
    (deadgrep-mode)
    (let ((deadgrep--search-term "sample")
          (deadgrep--search-type 'string)
          (deadgrep--search-case 'smart)
          (deadgrep--file-type 'all)
          (grepx-compact-header t))
      ;; Render compact initially
      (grepx--write-compact-heading)
      (should (string-match-p "\\[deadgrep\\]" (buffer-string)))

      ;; Append fake search result to test that results are preserved
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "foo.el:10: sample match\n"))

      ;; Toggle header
      (grepx-toggle-header)
      (should-not grepx-compact-header)
      (should (string-match-p "foo.el:10: sample match" (buffer-string)))

      ;; Toggle back
      (grepx-toggle-header)
      (should grepx-compact-header)
      (should (string-match-p "\\[deadgrep\\]" (buffer-string)))
      (should (string-match-p "foo.el:10: sample match" (buffer-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window Reuse & next-error Pinning Tests

(ert-deftest grepx-test/target-source-window-reuse ()
  "Test that grepx--target-source-window never returns search-win when others exist."
  (let ((search-win (selected-window)))
    (let ((target-win (grepx--target-source-window search-win)))
      (should (window-live-p target-win))
      (should-not (eq target-win search-win))
      (when (and (window-live-p target-win) (not (eq target-win search-win)))
        (delete-window target-win)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymap Ownership Tests

(ert-deftest grepx-test/keymaps-owned-by-grepx ()
  "Verify grepx-map, grepx-rg-map, grepx-project-map, and grepx-search-map."
  (grepx-mode 1)
  (should (boundp 'grepx-map))
  (should (boundp 'grepx-rg-map))
  (should (boundp 'grepx-project-map))
  (should (boundp 'grepx-search-map))

  ;; grepx-map bindings
  (should (eq (keymap-lookup grepx-map "o") #'occur))
  (should (eq (keymap-lookup grepx-map "g") #'grepx-grep))
  (should (eq (keymap-lookup grepx-map "d") #'grepx-find))
  (should (eq (keymap-lookup grepx-map "c") #'grepx-compile))
  (should (eq (keymap-lookup grepx-map "m") #'grepx-menu))
  (should (eq (keymap-lookup grepx-map "s") grepx-search-map))
  (should (eq (keymap-lookup grepx-map "a") #'grepx-ag-pwd))
  (should (eq (keymap-lookup grepx-map "u") #'grepx-ugrep-pwd))

  ;; grepx-search-map bindings
  (should (eq (keymap-lookup grepx-search-map "l") #'consult-line))
  (should (eq (keymap-lookup grepx-search-map "m") #'consult-line-multi))
  (should (eq (keymap-lookup grepx-search-map "k") #'consult-keep-lines))

  ;; grepx-rg-map bindings
  (should (eq (keymap-lookup grepx-rg-map "d") #'grepx-find))
  (should (eq (keymap-lookup grepx-rg-map "c") #'grepx-directory))
  (should (eq (keymap-lookup grepx-rg-map "p") #'grepx-project))
  (should (eq (keymap-lookup grepx-rg-map "m") #'grepx-merge-conflicts))
  (should (eq (keymap-lookup grepx-rg-map "s") #'grepx-live-pwd))
  (should (eq (keymap-lookup grepx-rg-map "r") #'grepx-live-project))
  (should (eq (keymap-lookup grepx-rg-map "g") #'grepx-live))
  (should (eq (keymap-lookup grepx-rg-map "k") #'grepx-compile))

  ;; grepx-project-map bindings
  (should (eq (keymap-lookup grepx-project-map "p") #'grepx-project))
  (should (eq (keymap-lookup grepx-project-map "r") #'grepx-live-project))
  (should (eq (keymap-lookup grepx-project-map "a") #'grepx-ag-project))
  (should (eq (keymap-lookup grepx-project-map "g") #'grepx-git-grep))
  (should (eq (keymap-lookup grepx-project-map "u") #'grepx-ugrep-project))
  (should (eq (keymap-lookup grepx-project-map "f") #'find-grep))
  (should (eq (keymap-lookup grepx-project-map "c") #'grepx-compile))

  ;; grepx-mode-map bindings
  (should (eq (keymap-lookup grepx-mode-map "C-c g") grepx-map))

  ;; deadgrep-mode-map bindings
  (should (eq (keymap-lookup deadgrep-mode-map "?") #'grepx-toggle-header))
  (should (eq (keymap-lookup deadgrep-mode-map "w") #'deadgrep-edit-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACR Menu Tests

(ert-deftest grepx-test/acr-menu ()
  "Test grepx-menu definition, command table, and dispatch."
  (should (commandp 'grepx-menu))
  (should (consp grepx-menu--commands))
  ;; Check that each entry is an ACR triple: (CANDIDATE ANNOTATION . TARGET)
  (dolist (entry grepx-menu--commands)
    (should (stringp (car entry)))
    (should (stringp (cadr entry)))
    (should (symbolp (cddr entry))))
  ;; Check that key commands are listed in grepx-menu--commands
  (let ((cmds (mapcar #'cddr grepx-menu--commands)))
    (should (memq 'grepx-find cmds))
    (should (memq 'grepx-directory cmds))
    (should (memq 'grepx-project cmds))
    (should (memq 'grepx-compile cmds))
    (should (memq 'grepx-live cmds))
    (should (memq 'grepx-live-pwd cmds))
    (should (memq 'grepx-live-project cmds))
    (should (memq 'grepx-consult-ag cmds))
    (should (memq 'grepx-consult-ugrep cmds))
    (should (memq 'grepx--run-acr-wizard cmds))
    (should (memq 'grepx-grep cmds))
    (should (memq 'find-grep cmds))
    (should (memq 'deadgrep cmds))
    (should (memq 'projectile-grep cmds))
    (should (memq 'projectile-ag cmds))
    (should (memq 'projectile-ripgrep cmds)))
  ;; Verify ACR directly accepts grepx-menu--commands, generates annotations, and resolves TARGET
  (let* ((map (annotated-completing-read--to-map grepx-menu--commands))
         (collection (annotated-completing-read--build-collection
                      map nil nil nil nil))
         (ann-fn (cdr (assq 'annotation-function (funcall collection "" nil 'metadata))))
         (sample-ann (funcall ann-fn "Search (pwd)"))
         (target (annotated-completing-read--resolve-target map "Search (pwd)")))
    (should (stringp sample-ann))
    (should (string-match-p "Persistent search buffer in default-directory" sample-ann))
    (should (eq target 'grepx-find))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project Root Signature Compatibility (consult-project-function)

(ert-deftest grepx-test/project-root-signature ()
  "Verify project-root helpers accept optional may-prompt argument without error."
  ;; Calling with 1 argument (may-prompt) must not throw wrong-number-of-arguments
  (should (stringp (grepx--project-root nil)))
  (should (stringp (grepx--project-root t)))
  (should (stringp (minaid--project-root nil)))
  (should (stringp (minaid--project-root t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backend Registry Tests

(ert-deftest grepx-test/backend-registry-registration ()
  "Test backend registration, priority lookup, and preference overrides."
  (let ((grepx-backends grepx-backends)
        (grepx-preferred-persistent-backend nil)
        (grepx-preferred-live-backend nil))
    ;; Verify default backends exist
    (should (grepx-find-backend :persistent))
    (should (grepx-find-backend :live))

    ;; Register custom high-priority persistent backend
    (grepx-register-backend 'test-custom-persistent
      :type :persistent
      :priority 999
      :available-p (lambda () t)
      :run-fn (lambda (_dir _query) 'custom-persistent-ran)
      :description "Test custom persistent backend")

    (let ((found (grepx-find-backend :persistent)))
      (should (eq (plist-get found :name) 'test-custom-persistent))
      (should (eq (funcall (plist-get found :run-fn) nil nil) 'custom-persistent-ran)))

    ;; When unavailable, falls back to next available backend
    (grepx-register-backend 'test-unavailable-persistent
      :type :persistent
      :priority 1000
      :available-p (lambda () nil)
      :run-fn (lambda (_dir _query) (error "Should not be called"))
      :description "Unavailable backend")

    (let ((found (grepx-find-backend :persistent)))
      (should (eq (plist-get found :name) 'test-custom-persistent)))

    ;; Explicit preference overrides priority
    (let ((grepx-preferred-persistent-backend 'compile-rg))
      (let ((found (grepx-find-backend :persistent)))
        (should (eq (plist-get found :name) 'compile-rg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fallback Ladder and Consult/Deadgrep Decoupling Tests

(ert-deftest grepx-test/consult-free-and-deadgrep-free-fallbacks ()
  "Verify search commands dispatch gracefully when deadgrep and consult are unavailable."
  (let* ((grepx-backends nil)
         (executed nil))
    ;; Register only fallback backends
    (grepx-register-backend 'compile-rg
      :type :persistent
      :priority 80
      :available-p (lambda () t)
      :run-fn (lambda (dir term) (setq executed (list :compile-rg dir term)))
      :description "Compile rg fallback")
    (grepx-register-backend 'acr-wizard
      :type :live
      :priority 50
      :available-p (lambda () t)
      :run-fn (lambda (dir initial) (setq executed (list :acr-wizard dir initial)))
      :description "ACR wizard fallback")

    ;; grepx-find dispatches through fallback without error
    (grepx-find "my-search" "/tmp/test-dir")
    (should (equal executed (list :compile-rg "/tmp/test-dir" "my-search")))

    ;; grepx-live dispatches to ACR wizard fallback without error
    (setq executed nil)
    (grepx-live "/tmp/live-dir" "live-query")
    (should (equal executed (list :acr-wizard "/tmp/live-dir" "live-query")))))

(ert-deftest grepx-test/ag-and-ugrep-builders ()
  "Test Consult builders for Ag and Ugrep when Consult is present."
  (when (fboundp 'consult--grep-make-builder)
    (let ((ag-builder (grepx--ag-make-builder '("/tmp")))
          (ugrep-builder (grepx--ugrep-make-builder '("/tmp"))))
      (should (functionp ag-builder))
      (should (functionp ugrep-builder))
      ;; Builders return command line lists
      (let ((ag-cmd (car (funcall ag-builder "search-term")))
            (ugrep-cmd (car (funcall ugrep-builder "search-term"))))
        (should (member "ag" ag-cmd))
        (should (member "--vimgrep" ag-cmd))
        (should (member "ugrep" ugrep-cmd))
        (should (member "--vimgrep" ugrep-cmd))))))

(ert-deftest grepx-test/upstream-consult-tools-dispatch ()
  "Test grepx-grep and grepx-git-grep dispatching with and without consult."
  (let ((consult-ran nil)
        (vc-ran nil))
    ;; When consult-git-grep is bound
    (cl-letf (((symbol-function 'consult-git-grep)
               (lambda (&optional dir initial)
                 (setq consult-ran (list dir initial)))))
      (grepx-git-grep "/tmp/git-repo" "needle")
      (should (equal consult-ran (list "/tmp/git-repo" "needle"))))

    ;; When consult-git-grep is unbound, falls back to vc-git-grep
    (setq consult-ran nil)
    (cl-letf (((symbol-function 'consult-git-grep) nil)
              ((symbol-function 'vc-git-grep)
               (lambda () (interactive) (setq vc-ran t))))
      (grepx-git-grep "/tmp/git-repo")
      (should vc-ran))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command Definitions & No Compatibility Shims

(ert-deftest grepx-test/command-definitions ()
  "Verify all grepx commands are valid interactive commands."
  (should (commandp 'grepx-find))
  (should (commandp 'grepx-directory))
  (should (commandp 'grepx-project))
  (should (commandp 'grepx-compile))
  (should (commandp 'grepx-merge-conflicts))
  (should (commandp 'grepx-live))
  (should (commandp 'grepx-live-pwd))
  (should (commandp 'grepx-live-project))
  (should (commandp 'grepx-live-up-directory))
  (should (commandp 'grepx-live-to-buffer))
  (should (commandp 'grepx-toggle-header))
  (should (commandp 'grepx-menu))
  (should (commandp 'grepx-ag-pwd))
  (should (commandp 'grepx-ag-project))
  (should (commandp 'grepx-ugrep-pwd))
  (should (commandp 'grepx-ugrep-project))
  (should (commandp 'grepx-git-grep))
  (should (commandp 'grepx-grep))
  (should (commandp 'grepx-consult-ag))
  (should (commandp 'grepx-consult-ugrep))
  (should (commandp 'grepx--run-acr-wizard)))

(ert-deftest grepx-test/no-compatibility-shims ()
  "Verify obsolete backward-compatibility shims are removed."
  (should-not (fboundp 'find-ripgrep))
  (should-not (fboundp 'find-ripgrep-compile))
  (should-not (fboundp 'find-ripgrep-project))
  (should-not (fboundp 'find-merge-conflicts))
  (should-not (fboundp 'consult-rg))
  (should-not (fboundp 'consult-rg-project))
  (should-not (fboundp 'consult-rg-pwd))
  (should-not (fboundp 'consult-rg-pwd-wizard))
  (should-not (fboundp 'consult-rg-project-wizard)))

(provide 'test-grepx)
;;; test-grepx.el ends here

(ert-deftest grepx-test/isearch-bindings ()
  "Test that grepx binds M-s r and M-s g in the built-in isearch-mode-map."
  (should (eq (lookup-key isearch-mode-map (kbd "M-s r")) #'grepx-live))
  (should (eq (lookup-key isearch-mode-map (kbd "M-s g")) #'grepx-project)))
