;;; test-sprite-mode.el --- ERT tests for sprite-mode keymap layering -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; `sprite-mode' lives in the sprite package (sprite-daemon.el).  These tests
;; cover only its prefix-binding helpers against a scratch parent keymap, so no
;; registry/savehist side effects occur.

;;; Code:

(require 'ert)
(require 'sprite-daemon)

(defvar sprite-mode-test--parent nil)

(defmacro sprite-mode-test--with-parent (prefix &rest body)
  (declare (indent 1))
  `(let ((sprite-mode-test--parent (make-sparse-keymap))
         (sprite-mode-map-prefix (cons ,prefix 'sprite-mode-test--parent)))
     (keymap-set sprite-mode-test--parent "s" #'ignore)
     (keymap-set sprite-mode-test--parent "q" #'car)
     ,@body))

(ert-deftest sprite-mode-test/bind-attaches-only-own-map ()
  (sprite-mode-test--with-parent "z"
    (let ((global-before (copy-tree global-map)))
      (sprite-mode--bind-map-prefix)
      (should (eq #'sprite-create (keymap-lookup sprite-mode-test--parent "z c")))
      (should (eq #'sprite-list (keymap-lookup sprite-mode-test--parent "z l")))
      (should (eq #'sprite-open-frame (keymap-lookup sprite-mode-test--parent "z o")))
      (should (eq #'sprite-get-or-create-next (keymap-lookup sprite-mode-test--parent "z n")))
      (should (eq #'car (keymap-lookup sprite-mode-test--parent "q")))
      (should (equal global-before global-map)))))

(ert-deftest sprite-mode-test/unbind-reveals-prior-bindings ()
  (sprite-mode-test--with-parent "z"
    (sprite-mode--bind-map-prefix)
    (sprite-mode--unbind-map-prefix)
    (should-not (keymap-lookup sprite-mode-test--parent "z"))
    (should (eq #'ignore (keymap-lookup sprite-mode-test--parent "s")))
    (should (eq #'car (keymap-lookup sprite-mode-test--parent "q")))))

(ert-deftest sprite-mode-test/non-colliding-prefix-keeps-existing-key ()
  (sprite-mode-test--with-parent "z"
    (sprite-mode--bind-map-prefix)
    (should (eq #'ignore (keymap-lookup sprite-mode-test--parent "s")))))

(ert-deftest sprite-mode-test/colliding-prefix-shadows-existing-key ()
  "Documents the current config bug: prefix \"s\" overwrites core \"s\"."
  (sprite-mode-test--with-parent "s"
    (sprite-mode--bind-map-prefix)
    (should-not (eq #'ignore (keymap-lookup sprite-mode-test--parent "s")))))

(ert-deftest sprite-mode-test/nil-prefix-binds-nothing ()
  (sprite-mode-test--with-parent "z"
    (let ((sprite-mode-map-prefix nil))
      (sprite-mode--bind-map-prefix)
      (should-not (keymap-lookup sprite-mode-test--parent "z")))))

(provide 'test-sprite-mode)
;;; test-sprite-mode.el ends here
