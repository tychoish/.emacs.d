;;; test-egloxtra.el --- ERT tests for eglox.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^eglox/")

(require 'ert)
(require 'test-helper)
(require 'cl-lib)
(require 'eglot)
(require 'eglox)
(require 'projectile nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Helpers

(defmacro eglox-test--with-fake-eglot-server (var &rest body)
  "Bind VAR to a minimal live `eglot-lsp-server' instance for BODY.
`eglot-lsp-server' initialization requires a real process (it calls
`process-put' on its `:process' slot) and a real project (some capability
helpers call `project-root' on it), so this spawns an inert process and a
transient project and cleans both up afterward."
  (declare (indent 1))
  `(let* ((eglox-test--proc (start-process "eglox-test" nil "cat"))
          (,var (make-instance 'eglot-lsp-server :name "test"
                                :process eglox-test--proc)))
     (setf (eglot--project ,var) (cons 'transient temporary-file-directory))
     (unwind-protect
         (progn ,@body)
       (delete-process eglox-test--proc))))

(defun eglox-test--flatten-workspace-configuration (val)
  "Reproduce `eglot--workspace-configuration-plist's flattening of VAL."
  (or (and (consp (car val))
           (cl-loop for (section . v) in val
                    collect (if (keywordp section) section (intern (format ":%s" section)))
                    collect v))
      val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hooks & Tempel Integration

(ert-deftest eglox/managed-mode-hook-functions-bound ()
  "Ensure all functions added to `eglot-managed-mode-hook' are bound."
  (dolist (fn eglot-managed-mode-hook)
    (should (fboundp fn))))

(ert-deftest eglox/tempel-mode-not-on-hook ()
  "Ensure `eglot-tempel-mode' is not on `eglot-managed-mode-hook'."
  (should-not (memq 'eglot-tempel-mode eglot-managed-mode-hook))
  (should-not (memq 'tychoish/eglot-tempel-enable eglot-managed-mode-hook)))

(ert-deftest eglox/ensure-hook-cycles-flycheck-and-enables-tempel ()
  "Test `eglox-ensure-hook' enables `eglot-tempel-mode' and cycles flycheck."
  (let ((tempel-calls 0)
        (flycheck-history nil)
        (eglot-tempel-mode nil))
    (cl-letf (((symbol-function 'eglot-tempel-mode)
               (lambda (arg) (cl-incf tempel-calls) (setq eglot-tempel-mode (= arg 1))))
              ((symbol-function 'flycheck-eglot-mode)
               (lambda (arg) (push arg flycheck-history))))
      (eglox-ensure-hook)
      (should (= tempel-calls 1))
      (should (equal flycheck-history '(1 -1)))
      ;; When tempel mode is already enabled, should not enable again
      (setq tempel-calls 0)
      (setq flycheck-history nil)
      (eglox-ensure-hook)
      (should (= tempel-calls 0))
      (should (equal flycheck-history '(1 -1))))))

(ert-deftest eglox/completion-at-point-guard-no-server ()
  "Test `ad:eglot-completion-at-point-guard-no-server' guards without live server."
  (cl-letf (((symbol-function 'eglot-current-server) (lambda () nil)))
    (should-not (ad:eglot-completion-at-point-guard-no-server
                 (lambda () (error "Should not be called")))))
  (cl-letf (((symbol-function 'eglot-current-server) (lambda () 'fake-server)))
    (should (equal 'success
                   (ad:eglot-completion-at-point-guard-no-server
                    (lambda () 'success))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projectile Cache Warming

(ert-deftest eglox/projectile-cache-warm-p ()
  "Test `eglox-projectile-cache-warm-p' checking memory and persistent disk cache."
  (let ((projectile-enable-caching t)
        (projectile-projects-cache (make-hash-table :test #'equal)))
    (puthash "/tmp/warm-project" '("file1" "file2") projectile-projects-cache)
    (should (eglox-projectile-cache-warm-p "/tmp/warm-project"))
    (should-not (eglox-projectile-cache-warm-p "/tmp/cold-project"))

    ;; Persistent caching fallback
    (let ((projectile-enable-caching 'persistent))
      (cl-letf (((symbol-function 'projectile-load-project-cache)
                 (lambda (root) (when (string= root "/tmp/on-disk-project") t))))
        (should (eglox-projectile-cache-warm-p "/tmp/on-disk-project"))
        (should-not (eglox-projectile-cache-warm-p "/tmp/other-project"))))

    ;; Disabled caching
    (let ((projectile-enable-caching nil))
      (should-not (eglox-projectile-cache-warm-p "/tmp/warm-project")))))

(ert-deftest eglox/projectile-warm-cache-enqueue ()
  "Test `eglox-projectile-warm-cache-enqueue' adds cold roots and creates timer."
  (let ((eglox-projectile-warm-cache-queue nil)
        (eglox-projectile-warm-cache-idle-timer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'eglox-projectile-cache-warm-p)
                   (lambda (root) (string= root "/tmp/warm"))))
          (eglox-projectile-warm-cache-enqueue "/tmp/cold-1")
          (should (equal eglox-projectile-warm-cache-queue '("/tmp/cold-1")))
          (should (timerp eglox-projectile-warm-cache-idle-timer))

          ;; Warm roots are ignored
          (eglox-projectile-warm-cache-enqueue "/tmp/warm")
          (should (equal eglox-projectile-warm-cache-queue '("/tmp/cold-1")))

          ;; Already-queued roots are ignored
          (eglox-projectile-warm-cache-enqueue "/tmp/cold-1")
          (should (equal eglox-projectile-warm-cache-queue '("/tmp/cold-1")))

          ;; Multiple cold roots
          (eglox-projectile-warm-cache-enqueue "/tmp/cold-2")
          (should (equal eglox-projectile-warm-cache-queue '("/tmp/cold-1" "/tmp/cold-2"))))
      (when (timerp eglox-projectile-warm-cache-idle-timer)
        (cancel-timer eglox-projectile-warm-cache-idle-timer)))))

(ert-deftest eglox/projectile-warm-cache-process-queue ()
  "Test `eglox-projectile-warm-cache-process-queue' processes batches and cancels timer."
  (let ((eglox-projectile-warm-cache-queue '("/tmp/p1" "/tmp/p2" "/tmp/p3" "/tmp/p4"))
        (eglox-projectile-warm-cache-idle-timer (run-with-idle-timer 100 t #'ignore))
        (indexed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'eglox-projectile-cache-warm-p)
                   (lambda (root) (string= root "/tmp/p2")))
                  ((symbol-function 'projectile-index-project-async)
                   (lambda (root) (push root indexed))))
          ;; First tick: chunk size is 3 (/tmp/p1, /tmp/p2, /tmp/p3)
          ;; /tmp/p2 is warm so it's not indexed
          (eglox-projectile-warm-cache-process-queue)
          (should (equal eglox-projectile-warm-cache-queue '("/tmp/p4")))
          (should (equal (sort indexed #'string<) '("/tmp/p1" "/tmp/p3")))
          (should eglox-projectile-warm-cache-idle-timer)

          ;; Second tick: /tmp/p4 is popped
          (eglox-projectile-warm-cache-process-queue)
          (should (null eglox-projectile-warm-cache-queue))
          (should (equal (sort indexed #'string<) '("/tmp/p1" "/tmp/p3" "/tmp/p4")))

          ;; Third tick: queue is empty, cancels timer
          (eglox-projectile-warm-cache-process-queue)
          (should-not eglox-projectile-warm-cache-idle-timer))
      (when (timerp eglox-projectile-warm-cache-idle-timer)
        (cancel-timer eglox-projectile-warm-cache-idle-timer)))))

(ert-deftest eglox/projectile-warm-cache-for-buffer-and-change ()
  "Test buffer and project change triggers for Projectile cache warming."
  (let ((enqueued nil))
    (cl-letf (((symbol-function 'eglox-projectile-warm-cache-enqueue)
               (lambda (root) (push root enqueued)))
              ((symbol-function 'projectile-project-root)
               (lambda () "/tmp/current-project")))
      (let ((projectile-mode t))
        (eglox-projectile-warm-cache-for-buffer)
        (should (equal enqueued '("/tmp/current-project")))

        (eglox-projectile-warm-cache-on-project-change "/tmp/new" "/tmp/old")
        (should (equal enqueued '("/tmp/current-project" "/tmp/current-project"))))

      (let ((projectile-mode nil))
        (setq enqueued nil)
        (eglox-projectile-warm-cache-for-buffer)
        (should-not enqueued)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save Hooks & Actions

(ert-deftest eglox/format-for-hook ()
  "Test `eglox-format-for-hook' runs format only when managed."
  (let ((formatted nil))
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil))
              ((symbol-function 'eglot-format-buffer) (lambda () (setq formatted t))))
      (eglox-format-for-hook)
      (should-not formatted))
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
              ((symbol-function 'eglox-server-live-p) (lambda () t))
              ((symbol-function 'eglot-format-buffer) (lambda () (setq formatted t))))
      (eglox-format-for-hook)
      (should formatted))))

(ert-deftest eglox/organize-imports-for-hook ()
  "Test `eglox-organize-imports-for-hook' runs organize imports across whole buffer."
  (with-temp-buffer
    (insert "line 1\nline 2\n")
    (let ((called-with nil))
      ;; Unmanaged
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil))
                ((symbol-function 'eglot-code-action-organize-imports)
                 (lambda (beg end) (setq called-with (list beg end)))))
        (eglox-organize-imports-for-hook)
        (should-not called-with))
      ;; Managed
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                ((symbol-function 'eglox-server-live-p) (lambda () t))
                ((symbol-function 'eglot-code-action-organize-imports)
                 (lambda (beg end) (setq called-with (list beg end)))))
        (eglox-organize-imports-for-hook)
        (should (equal called-with (list (point-min) (point-max))))
        ;; Demoted error tolerance
        (cl-letf (((symbol-function 'eglot-code-action-organize-imports)
                   (lambda (_beg _end) (error "LSP error"))))
          (should-not (condition-case _err
                          (progn (eglox-organize-imports-for-hook) nil)
                        (error t))))))))

(ert-deftest eglox/code-action-inline ()
  "Test `eglox-code-action-inline' requests source.inline action."
  (let ((called-action nil))
    ;; Unmanaged
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil))
              ((symbol-function 'eglot-code-actions)
               (lambda (_beg _end action _async) (setq called-action action))))
      (eglox-code-action-inline)
      (should-not called-action))
    ;; Managed
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
              ((symbol-function 'eglot-code-actions)
               (lambda (_beg _end action _async) (setq called-action action))))
      (eglox-code-action-inline)
      (should (equal called-action "source.inline"))
      ;; Demoted error tolerance
      (cl-letf (((symbol-function 'eglot-code-actions)
                 (lambda (&rest _) (error "Action error"))))
        (should-not (condition-case _err
                        (progn (eglox-code-action-inline) nil)
                      (error t)))))))

(ert-deftest eglox/update-workspace ()
  "Test `eglox-update-workspace' signals configuration change to server."
  (let ((signaled-server nil))
    (cl-letf (((symbol-function 'eglot--current-server-or-lose) (lambda () 'fake-server))
              ((symbol-function 'eglot-signal-didChangeConfiguration)
               (lambda (srv) (setq signaled-server srv))))
      (eglox-update-workspace)
      (should (eq signaled-server 'fake-server)))))

(ert-deftest eglox/before-save-hook-installs-local-hooks ()
  "Test `eglox-before-save-hook' installs buffer-local format and import hooks."
  (with-temp-buffer
    (eglox-before-save-hook)
    (should (memq #'eglox-format-for-hook before-save-hook))
    (should (memq #'eglox-organize-imports-for-hook before-save-hook))
    (should (local-variable-p 'before-save-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server Pruning & Lifecycle

(ert-deftest eglox/prune-dead-servers ()
  "Test `eglox-prune-dead-servers' drops dead server structs."
  (eglox-test--with-fake-eglot-server live-srv
    (let* ((dead-proc (start-process "dead" nil "cat"))
           (dead-srv (make-instance 'eglot-lsp-server :name "dead" :process dead-proc))
           (project 'test-prune-dead)
           (eglot--servers-by-project (make-hash-table :test #'equal)))
      (delete-process dead-proc)
      (puthash project (list live-srv dead-srv) eglot--servers-by-project)
      (eglox-prune-dead-servers)
      (should (equal (list live-srv) (gethash project eglot--servers-by-project))))))

(ert-deftest eglox/reconnect-orphaned-buffers ()
  "Test `eglox-reconnect-orphaned-buffers' reconnects orphaned buffers."
  (let* ((dead-proc (start-process "dead" nil "cat"))
         (dead-srv (make-instance 'eglot-lsp-server :name "dead" :process dead-proc))
         (test-buf (generate-new-buffer "eglox-orphan-test"))
         (ensured nil)
         (managed-off-called nil))
    (delete-process dead-proc)
    (with-current-buffer test-buf
      (setq-local eglot--managed-mode t)
      (setq-local eglot--cached-server dead-srv)
      (setq-local buffer-file-name "/tmp/eglox-orphan-test.py"))
    (unwind-protect
        (cl-letf (((symbol-function 'eglot--managed-mode-off)
                   (lambda () (setq managed-off-called t)))
                  ((symbol-function 'eglot-ensure)
                   (lambda () (setq ensured t))))
          (eglox-reconnect-orphaned-buffers)
          (with-current-buffer test-buf
            (should-not eglot--cached-server)
            (should managed-off-called)
            (should ensured)))
      (kill-buffer test-buf))))

(ert-deftest eglox/cleanup-stale-connections ()
  "Test `eglox-cleanup-stale-connections' runs all three pruning steps."
  (let ((dead-called nil)
        (dup-called nil)
        (reconnect-called nil))
    (cl-letf (((symbol-function 'eglox-prune-dead-servers) (lambda () (setq dead-called t)))
              ((symbol-function 'eglox-prune-duplicate-servers) (lambda () (setq dup-called t)))
              ((symbol-function 'eglox-reconnect-orphaned-buffers) (lambda () (setq reconnect-called t))))
      (eglox-cleanup-stale-connections)
      (should dead-called)
      (should dup-called)
      (should reconnect-called))))

(ert-deftest eglox/cleanup-stale-connections-safe-when-no-servers ()
  "Test `eglox-cleanup-stale-connections' runs cleanly with an empty servers table."
  (let ((eglot--servers-by-project (make-hash-table :test #'equal)))
    (eglox-cleanup-stale-connections)))

(ert-deftest eglox/global-mode-defers-when-eglot-uninitialized ()
  "Test `eglox-global-mode' does not start timer or install advice when Eglot is uninitialized."
  (let ((eglox--eglot-initialized nil))
    (eglox-global-mode -1)
    (eglox-global-mode 1)
    (should eglox-global-mode)
    (should (null eglox-cleanup-stale-connections-timer))
    (should-not (advice-member-p #'ad:eglot-completion-at-point-guard-no-server 'eglot-completion-at-point))
    (eglox-global-mode -1)))

(ert-deftest eglox/mark-and-unmark-connecting ()
  "Test `eglox-mark-connecting' and `eglox-unmark-connecting'."
  (eglox-test--with-fake-eglot-server srv
    (let ((eglox-connecting-projects nil))
      (setf (eglot--project srv) 'test-proj)
      (eglox-mark-connecting srv)
      (should (equal eglox-connecting-projects '(test-proj)))
      ;; Duplicate mark is no-op
      (eglox-mark-connecting srv)
      (should (equal eglox-connecting-projects '(test-proj)))
      (eglox-unmark-connecting srv)
      (should-not eglox-connecting-projects))))

(ert-deftest eglox/connect-dedupe ()
  "Test `ad:eglot--connect-dedupe' deduplication logic."
  (eglox-test--with-fake-eglot-server live-srv
    (let ((eglot--servers-by-project (make-hash-table :test #'equal))
          (eglox-connecting-projects nil)
          (orig-called nil))
      (puthash 'proj1 (list live-srv) eglot--servers-by-project)
      ;; 1. Live server exists: returns existing server without calling orig-fn
      (should (eq live-srv
                  (ad:eglot--connect-dedupe
                   (lambda (&rest _) (setq orig-called t))
                   nil 'proj1 nil nil nil)))
      (should-not orig-called)

      ;; 2. In-flight connecting project: returns nil without calling orig-fn
      (push 'proj2 eglox-connecting-projects)
      (setq orig-called nil)
      (should-not (ad:eglot--connect-dedupe
                   (lambda (&rest _) (setq orig-called t))
                   nil 'proj2 nil nil nil))
      (should-not orig-called)

      ;; 3. New project: calls orig-fn
      (setq orig-called nil)
      (should (eq 'orig-ok
                  (ad:eglot--connect-dedupe
                   (lambda (&rest _) (setq orig-called t) 'orig-ok)
                   nil 'proj3 nil nil nil)))
      (should orig-called))))

(ert-deftest eglox/on-shutdown-cleanup-stale ()
  "Test `ad:eglot--on-shutdown-cleanup-stale' runs cleanup on crashes only."
  (eglox-test--with-fake-eglot-server srv
    (let ((unmarked nil)
          (orig-ran nil)
          (cleaned nil))
      (cl-letf (((symbol-function 'eglox-unmark-connecting)
                 (lambda (s) (when (eq s srv) (setq unmarked t))))
                ((symbol-function 'eglox-cleanup-stale-connections)
                 (lambda () (setq cleaned t))))
        ;; Shutdown requested: do NOT run cleanup
        (setf (eglot--shutdown-requested srv) t)
        (ad:eglot--on-shutdown-cleanup-stale
         (lambda (s) (setq orig-ran t))
         srv)
        (should unmarked)
        (should orig-ran)
        (should-not cleaned)

        ;; Crashed (shutdown-requested nil): DO run cleanup
        (setf (eglot--shutdown-requested srv) nil)
        (setq unmarked nil orig-ran nil cleaned nil)
        (ad:eglot--on-shutdown-cleanup-stale
         (lambda (s) (setq orig-ran t))
         srv)
        (should unmarked)
        (should orig-ran)
        (should cleaned)))))

(ert-deftest eglox/mode-hooks-installed ()
  "Verify `eglox-ensure' or `eglot-ensure' is installed on required mode hooks."
  (dolist (hook '(js-mode-hook
                  js-ts-mode-hook
                  typescript-ts-mode-hook
                  go-ts-mode-hook
                  go-mod-ts-mode-hook
                  yaml-mode-hook
                  rustic-mode-hook
                  c++-ts-mode-hook
                  c-ts-mode-hook
                  c-or-c++-mode-hook
                  python-mode-hook
                  python-ts-mode-hook
                  go-mode-hook
                  go-mod-mode-hook
                  bash-ts-mode-hook
                  bash-mode-hook
                  sh-mode-hook))
    (should (or (memq #'eglox-ensure (symbol-value hook))
                (memq #'eglot-ensure (symbol-value hook))))))

(ert-deftest eglox/deferred-keymap-resolves ()
  "Test deferred keymap lookup and resolution."
  (let ((map (make-sparse-keymap)))
    (hud-bind-deferred-keymap map "l" 'eglox 'eglox-global-map "eglot")
    (let ((cmd (keymap-lookup map "l")))
      (funcall cmd)
      (should (eq (keymap-lookup map "l") eglox-global-map)))))

(ert-deftest eglox/global-map-bindings ()
  "Test `eglox-global-map' contains expected bindings."
  (should (keymapp eglox-global-map))
  (should (eq (keymap-lookup eglox-global-map "s") #'eglot))
  (should (eq (keymap-lookup eglox-global-map "r") #'eglot-reconnect))
  (should (eq (keymap-lookup eglox-global-map "k") #'eglot-shutdown))
  (should (eq (keymap-lookup eglox-global-map "l") #'eglot-list-connections))
  (should (eq (keymap-lookup eglox-global-map "c") #'execute-extended-eglot-command))
  (should (eq (keymap-lookup eglox-global-map "g") #'eglot-forget-pending-continuations)))

(ert-deftest eglox/mode-map-bindings ()
  "Test `eglox-mode-map' contains expected bindings."
  (should (keymapp eglox-mode-map))
  (should (eq (keymap-lookup eglox-mode-map "e") #'execute-extended-eglot-command))
  (should (eq (keymap-lookup eglox-mode-map "s") #'consult-eglot-symbols))
  (should (eq (keymap-lookup eglox-mode-map "r") #'eglot-rename))
  (should (eq (keymap-lookup eglox-mode-map "a") #'eglot-code-actions))
  (should (eq (keymap-lookup eglox-mode-map "i") #'eglox-code-action-inline))
  (should (eq (keymap-lookup eglox-mode-map "f") #'eglot-format-buffer))
  (should (eq (keymap-lookup eglox-mode-map "o") #'eglox-organize-imports-for-hook))
  (should (eq (keymap-lookup eglox-mode-map "q") #'eglot-code-action-quickfix))
  (should (eq (keymap-lookup eglox-mode-map "w") #'eglot-code-action-rewrite))
  (should (eq (keymap-lookup eglox-mode-map "t") #'eglot-test-at-point))
  (should (eq (keymap-lookup eglox-mode-map "u") #'eglox-update-workspace))
  (should (keymapp (keymap-lookup eglox-minor-mode-map "C-c l"))))

(ert-deftest eglox/minor-mode-lifecycle ()
  "Test `eglox-mode' toggling properly installs and uninstalls hooks."
  (with-temp-buffer
    (let ((eglot-tempel-mode nil)
          (flycheck-mode-cycles 0))
      (cl-letf (((symbol-function 'eglot-tempel-mode)
                 (lambda (arg) (setq eglot-tempel-mode (> arg 0))))
                ((symbol-function 'flycheck-eglot-mode)
                 (lambda (_arg) (cl-incf flycheck-mode-cycles))))
        (eglox-mode 1)
        (should eglox-mode)
        (should (memq #'eglox-format-for-hook before-save-hook))
        (should (memq #'eglox-organize-imports-for-hook before-save-hook))
        (should eglot-tempel-mode)
        (should (= flycheck-mode-cycles 2))
        (eglox-mode -1)
        (should-not eglox-mode)
        (should-not (memq #'eglox-format-for-hook before-save-hook))
        (should-not (memq #'eglox-organize-imports-for-hook before-save-hook))))))

(ert-deftest eglox/managed-mode-sync ()
  "Test `eglox--managed-mode-sync' activates `eglox-mode' based on `eglot--managed-mode'."
  (with-temp-buffer
    (cl-letf (((symbol-function 'eglox-ensure-hook) #'ignore)
              ((symbol-function 'eglox-before-save-hook) #'ignore))
      (let ((eglot--managed-mode t))
        (eglox--managed-mode-sync)
        (should eglox-mode))
      (let ((eglot--managed-mode nil))
        (eglox--managed-mode-sync)
        (should-not eglox-mode)))))

(ert-deftest eglox/global-mode-toggle ()
  "Test enabling and disabling `eglox-global-mode'."
  (eglox-global-mode -1)
  (should-not eglox-global-mode)
  (should-not (advice-member-p #'ad:eglot-completion-at-point-guard-no-server 'eglot-completion-at-point))
  (should-not (advice-member-p #'ad:eglot--connect-dedupe 'eglot--connect))
  (should-not (advice-member-p #'ad:eglot--on-shutdown-cleanup-stale 'eglot--on-shutdown))
  (should-not (memq #'eglox--managed-mode-sync eglot-managed-mode-hook))
  (should (null eglox-cleanup-stale-connections-timer))
  (eglox-global-mode 1)
  (should eglox-global-mode)
  (should (advice-member-p #'ad:eglot-completion-at-point-guard-no-server 'eglot-completion-at-point))
  (should (advice-member-p #'ad:eglot--connect-dedupe 'eglot--connect))
  (should (advice-member-p #'ad:eglot--on-shutdown-cleanup-stale 'eglot--on-shutdown))
  (should (memq #'eglox--managed-mode-sync eglot-managed-mode-hook))
  (should (timerp eglox-cleanup-stale-connections-timer)))

(ert-deftest eglox/prune-duplicate-servers-marks-shutdown-requested ()
  "Regression test: pruning duplicates must mark `eglot--shutdown-requested'.
Otherwise `eglot--on-shutdown' auto-reconnects, creating an infinite reconnect loop."
  (eglox-test--with-fake-eglot-server keep
    (eglox-test--with-fake-eglot-server dup
      (let* ((project 'test-project)
             (eglot--servers-by-project (make-hash-table :test #'equal))
             (shutdown-calls nil))
        (puthash project (list keep dup) eglot--servers-by-project)
        (cl-letf (((symbol-function 'jsonrpc-shutdown)
                   (lambda (server &optional _cleanup) (push server shutdown-calls))))
          (eglox-prune-duplicate-servers))
        (should (equal (list dup) shutdown-calls))
        (should (eglot--shutdown-requested dup))
        (should-not (eglot--shutdown-requested keep))
        (should (equal (list keep) (gethash project eglot--servers-by-project)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client Capabilities

(ert-deftest eglox/declines-didChangeWatchedFiles-dynamic-registration ()
  "Regression test: never advertise `workspace/didChangeWatchedFiles' dynamic registration."
  (eglox--init-eglot)
  (eglox-test--with-fake-eglot-server server
    (let ((caps (eglot-client-capabilities server)))
      (should (equal '(:dynamicRegistration :json-false)
                     (plist-get (plist-get caps :workspace) :didChangeWatchedFiles))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Throttled Buffer Activation

(ert-deftest eglox/activate-editing-mode-throttled ()
  "Regression test: rate limit buffer activation to burst limit."
  (let* ((eglox-activate-recent nil)
         (eglox-activate-queue nil)
         (eglox-activate-timer nil)
         (call-count 0)
         (total (* 2 eglox-activate-burst-limit))
         (buffers (mapcar (lambda (n)
                            (let ((buf (generate-new-buffer (format "eglox-throttle-test-%d" n))))
                              (with-current-buffer buf
                                (insert "x = 1\n")
                                (set-buffer-modified-p nil)
                                (setq buffer-file-name (format "/tmp/eglox-throttle-test-%d.py" n))
                                (setq major-mode 'python-mode))
                              buf))
                          (number-sequence 1 total))))
    (unwind-protect
        (progn
          (seq-do (lambda (buf)
                    (with-current-buffer buf
                      (ad:eglot--maybe-activate-editing-mode-throttle
                       (lambda () (cl-incf call-count)))))
                  buffers)
          (should (= call-count eglox-activate-burst-limit))
          (should (= (- total eglox-activate-burst-limit)
                     (length eglox-activate-queue)))
          (should eglox-activate-timer))
      (when (timerp eglox-activate-timer)
        (cancel-timer eglox-activate-timer))
      (seq-do (lambda (buf)
                (when (buffer-live-p buf)
                  (with-current-buffer buf (set-buffer-modified-p nil))
                  (kill-buffer buf)))
              buffers))))

(ert-deftest eglox/activate-queue-prunes-empty-and-dead-buffers ()
  "Empty, dead, and managed buffers are dropped from the activation queue."
  (let* ((eglox-activate-queue nil)
         (eglox-activate-timer nil)
         (eglox-activate-last-log 0.0)
         (full (generate-new-buffer "eglox-prune-full"))
         (empty (generate-new-buffer "eglox-prune-empty"))
         (dead (generate-new-buffer "eglox-prune-dead")))
    (unwind-protect
        (progn
          (with-current-buffer full (insert "x"))
          (with-current-buffer dead (insert "x"))
          (setq eglox-activate-queue (list full empty dead))
          (kill-buffer dead)
          (should (= 2 (eglox-activate-prune-queue)))
          (should (equal (list full) eglox-activate-queue))
          (eglox-activate-enqueue empty)
          (should (equal (list full) eglox-activate-queue)))
      (when (timerp eglox-activate-timer)
        (cancel-timer eglox-activate-timer))
      (seq-do #'kill-buffer (seq-filter #'buffer-live-p (list full empty dead))))))

(ert-deftest eglox/activate-queue-size-prunes-and-counts ()
  "`eglox-activate-queue-size' counts only pending buffers."
  (let* ((eglox-activate-queue nil)
         (eglox-activate-timer nil)
         (full (generate-new-buffer "eglox-size-full"))
         (empty (generate-new-buffer "eglox-size-empty")))
    (unwind-protect
        (progn
          (with-current-buffer full (insert "x"))
          (setq eglox-activate-queue (list full empty))
          (should (= 1 (eglox-activate-queue-size)))
          (should (string-match-p "1 buffer" (eglox-activate-queue-report)))
          (should (string-match-p "idle" (eglox-activate-queue-report))))
      (seq-do #'kill-buffer (list full empty)))))

(ert-deftest eglox/activate-editing-mode-unmanaged-bypasses-throttle ()
  "Regression test: buffers without a file or configured server bypass throttle."
  (let* ((eglox-activate-recent nil)
         (eglox-activate-queue nil)
         (eglox-activate-timer nil)
         (call-count 0)
         (total (* 2 eglox-activate-burst-limit))
         (buffers (mapcar (lambda (n) (generate-new-buffer (format "eglox-unmanaged-test-%d" n)))
                          (number-sequence 1 total))))
    (unwind-protect
        (progn
          (seq-do (lambda (buf)
                    (with-current-buffer buf
                      (ad:eglot--maybe-activate-editing-mode-throttle
                       (lambda () (cl-incf call-count)))))
                  buffers)
          (should (= call-count total))
          (should (null eglox-activate-queue))
          (should-not eglox-activate-timer))
      (seq-do #'kill-buffer buffers))))

(ert-deftest eglox/activate-queue-drains-in-batches ()
  "Regression test: queued buffers drain in batches."
  (let* ((eglox-activate-recent nil)
         (eglox-activate-queue nil)
         (eglox-activate-timer nil)
         (activated nil)
         (total (* 2 eglox-activate-burst-limit))
         (buffers (mapcar (lambda (n)
                            (with-current-buffer (generate-new-buffer (format "eglox-drain-test-%d" n))
                              (insert "x")
                              (current-buffer)))
                          (number-sequence 1 total))))
    (unwind-protect
        (cl-letf (((symbol-function 'eglot--maybe-activate-editing-mode)
                   (lambda () (push (current-buffer) activated))))
          (setq eglox-activate-queue (copy-sequence buffers))
          (eglox-activate-process-queue)
          (should (= eglox-activate-burst-limit (length activated)))
          (should (= (- total eglox-activate-burst-limit)
                     (length eglox-activate-queue)))
          (eglox-activate-process-queue)
          (should (= total (length activated)))
          (should (null eglox-activate-queue)))
      (when (timerp eglox-activate-timer)
        (cancel-timer eglox-activate-timer))
      (seq-do #'kill-buffer buffers))))

(ert-deftest eglox/activate-prioritizes-visible-buffer ()
  "Regression test: visible buffers in queue are activated immediately."
  (let* ((eglox-activate-recent nil)
         (buf (generate-new-buffer "eglox-visible-test"))
         (other (generate-new-buffer "eglox-visible-test-other"))
         (eglox-activate-queue (list buf other))
         (activated nil))
    (unwind-protect
        (cl-letf (((symbol-function 'eglot--maybe-activate-editing-mode)
                   (lambda () (push (current-buffer) activated))))
          (eglox-activate-prioritize-visible (selected-window))
          (should-not activated)
          (with-current-buffer buf
            (eglox-activate-prioritize-visible
             (progn (set-window-buffer (selected-window) buf) (selected-window))))
          (should (equal (list buf) activated))
          (should-not (memq buf eglox-activate-queue))
          (should (memq other eglox-activate-queue))
          ;; Frame object
          (setq eglox-activate-queue (list other))
          (set-window-buffer (selected-window) other)
          (eglox-activate-prioritize-visible (selected-frame))
          (should (equal (list other buf) activated))
          (should-not (memq other eglox-activate-queue)))
      (seq-do #'kill-buffer (list buf other)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Workspace Configuration

(ert-deftest eglox/default-server-configuration-encodes-cleanly ()
  "Regression test: every section in `eglox-default-server-configuration' encodes to JSON."
  (should (stringp
           (jsonrpc--json-encode
            (eglox-test--flatten-workspace-configuration
             eglox-default-server-configuration)))))

(ert-deftest eglox/default-server-configuration-nesting ()
  "Regression test: pylsp's `:plugins' and yaml's `:format' must be nested plists."
  (let* ((flat (eglox-test--flatten-workspace-configuration
                eglox-default-server-configuration))
         (pylsp (plist-get flat :pylsp))
         (yaml (plist-get flat :yaml)))
    (when pylsp
      (should (listp (plist-get pylsp :plugins)))
      (should-not (keywordp (plist-get pylsp :plugins))))
    (when yaml
      (should (listp (plist-get yaml :format)))
      (should-not (keywordp (plist-get yaml :format))))))

(ert-deftest eglox/save-hooks-reconnect-when-server-dead ()
  "Save hooks skip dead servers and trigger stale connection cleanup."
  (let ((cleanups 0)
        (acted nil))
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
              ((symbol-function 'eglox-server-live-p) (lambda () nil))
              ((symbol-function 'eglox-cleanup-stale-connections)
               (lambda () (cl-incf cleanups)))
              ((symbol-function 'eglot-format-buffer) (lambda () (setq acted t)))
              ((symbol-function 'eglot-code-action-organize-imports)
               (lambda (&rest _) (setq acted t))))
      (eglox-format-for-hook)
      (eglox-organize-imports-for-hook)
      (should (= 2 cleanups))
      (should-not acted))))

(ert-deftest eglox/ensure-unmanaged-buffers-enqueues-file-buffers ()
  "Unmanaged file buffers in eglox modes are queued; others are skipped."
  (let* ((eglox-activate-queue nil)
         (eglox-activate-timer nil)
         (py (generate-new-buffer "eglox-unmanaged-py"))
         (txt (generate-new-buffer "eglox-unmanaged-txt")))
    (unwind-protect
        (cl-letf (((symbol-function 'eglot-ensure) #'ignore))
          (with-current-buffer py
            (insert "x = 1\n")
            (set-buffer-modified-p nil)
            (setq buffer-file-name "/tmp/eglox-unmanaged.py")
            (setq major-mode 'python-mode))
          (with-current-buffer txt
            (insert "x")
            (set-buffer-modified-p nil)
            (setq buffer-file-name "/tmp/eglox-unmanaged.txt"))
          (should (= 1 (eglox-ensure-unmanaged-buffers)))
          (should (memq py eglox-activate-queue))
          (should-not (memq txt eglox-activate-queue)))
      (when (timerp eglox-activate-timer)
        (cancel-timer eglox-activate-timer))
      (seq-do (lambda (buf)
                (when (buffer-live-p buf)
                  (with-current-buffer buf (set-buffer-modified-p nil))
                  (kill-buffer buf)))
              (list py txt)))))

(ert-deftest eglox/activate-flush-queue-bypasses-throttle ()
  "Flushing activates COUNT buffers at once, beyond the burst limit."
  (let* ((eglox-activate-queue nil)
         (eglox-activate-recent nil)
         (eglox-activate-timer nil)
         (activated 0)
         (buffers (mapcar (lambda (n)
                            (with-current-buffer (generate-new-buffer (format "eglox-flush-%d" n))
                              (insert "x")
                              (current-buffer)))
                          (number-sequence 1 (* 3 eglox-activate-burst-limit)))))
    (unwind-protect
        (cl-letf (((symbol-function 'eglot--maybe-activate-editing-mode)
                   (lambda () (cl-incf activated))))
          (setq eglox-activate-queue (copy-sequence buffers))
          (should (= 1 (eglox-activate-flush-queue (1- (length buffers)))))
          (should (= (1- (length buffers)) activated)))
      (seq-do #'kill-buffer buffers))))

(provide 'test-egloxtra)
;;; test-egloxtra.el ends here

