;;; test-agent-shell-extras.el --- ERT tests for agent-shell-extras -*- lexical-binding: t -*-

;; Run inside a live Emacs session with the full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^agent-shell-extras/")
;;
;; Batch run:
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-helper.el \
;;     -l ~/.emacs.d/test/test-agent-shell-extras.el \
;;     --eval '(ert-run-tests-batch-and-exit "agent-shell-extras/")'

(require 'ert)
(require 'test-helper)
(require 'agent-shell-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transient menu key integrity

(ert-deftest agent-shell-extras/global-menu-no-key-prefix-conflicts ()
  "No key in agent-shell-global-menu is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-global-menu))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-extras/global-menu-no-duplicate-keys ()
  "No key appears more than once in agent-shell-global-menu."
  (let* ((keys (transient-test/collect-keys 'agent-shell-global-menu))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

(ert-deftest agent-shell-extras/session-menu-no-key-prefix-conflicts ()
  "No key in agent-shell-session-menu is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-session-menu))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-extras/session-menu-no-duplicate-keys ()
  "No key appears more than once in agent-shell-session-menu."
  (let* ((keys (transient-test/collect-keys 'agent-shell-session-menu))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

(provide 'test-agent-shell-extras)
;;; test-agent-shell-extras.el ends here
