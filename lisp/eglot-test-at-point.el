;;; eglot-test-at-point.el --- Test discovery for eglot -*- lexical-binding: t -*-

;; Author: sam kleinman
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, languages, eglot, testing
;; URL: https://github.com/tychoish/eglot-test-at-point

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides code-lens and document-symbol based test discovery via Eglot.
;; Language-specific setup is handled by `eglot-test-at-point-setup-go',
;; `eglot-test-at-point-setup-rust', and `eglot-test-at-point-setup-python'.
;; The primary user-facing commands are `eglot-test-at-point', which runs
;; the test nearest point, and `eglot-test-at-point-select', which offers
;; an annotated completion menu of all discovered tests in the buffer.
;; Per-language behaviour is controlled via buffer-local variables such as
;; `eglot-test-at-point-command', `eglot-test-at-point-name-fn', and
;; `eglot-test-at-point-run-command-fn'.

(require 'eglot)
(require 'seq)

(require 'annotated-completing-read)
(require 'xtdlib)

(defvar-local eglot-test-at-point-command nil
  "LSP command name identifying test-running code lenses in the current buffer.
Set buffer-locally per language, e.g. \"gopls.run_tests\" for Go.")

(defvar-local eglot-test-at-point-name-fn nil
  "Function (ARGUMENTS) => test-name-string called on a matching code lens.
ARGUMENTS is the raw plist/vector from the LSP command.  Return nil to skip.")

(defvar-local eglot-test-at-point-run-command-fn nil
  "Function (TEST-NAME) => shell-command-string to run that specific test.
Set buffer-locally per language.")

(defvar-local eglot-test-at-point-list-fn nil
  "Function () => alist of (NAME . ANNOTATION) for all tests in the current buffer.
Used by `eglot-test-at-point-select' to populate the selection menu.")

(defvar-local eglot-test-at-point-get-name-fn nil
  "Optional function () => test-name-string, bypassing code lens discovery.
When set, `eglot-test-at-point-name' calls this instead of the
`textDocument/codeLens' path.  Use for languages whose LSP server supports
`textDocument/documentSymbol' but not test-running code lenses.")

(defun eglot-test-at-point-setup-go ()
  "Configure test codelens vars for Go (gopls `test' lens)."
  (setq-local
   eglot-test-at-point-command "gopls.run_tests"
   eglot-test-at-point-list-fn #'eglot-test-at-point--go-names-in-buffer
   eglot-test-at-point-run-command-fn (lambda (name) (format "go test -v -run=^%s$ ." name))
   eglot-test-at-point-name-fn (lambda (args)
				    (let* ((run-args (aref args 0))
					   (tests (plist-get run-args :Tests))
					   (benchmarks (plist-get run-args :Benchmarks)))
				      (cond
				       ((and tests (> (length tests) 0)) (aref tests 0))
				       ((and benchmarks (> (length benchmarks) 0)) (aref benchmarks 0)))))))

(defun eglot-test-at-point-setup-rust ()
  "Configure test codelens vars for Rust (rust-analyzer `runSingle' lens)."
  (setq-local
   eglot-test-at-point-command "rust-analyzer.runSingle"
   eglot-test-at-point-run-command-fn (lambda (name) (format "cargo test %s -- --nocapture" name))
   eglot-test-at-point-name-fn (lambda (args)
				   (when-let* ((runnable (aref args 0))
					       (label (plist-get runnable :label))
					       (kind (plist-get runnable :kind))
					       ((member kind '("test" "bench"))))
				     (string-remove-prefix (concat kind " ") label)))))

(defun eglot-test-at-point-setup-python ()
  "Configure test discovery for Python via pylsp (+ python-lsp-ruff, pylsp-rope).
pylsp supports `textDocument/documentSymbol' but not test code lenses, so
`eglot-test-at-point-get-name-fn' is used instead of the codeLens path.
Discovers top-level `test_*' functions (kind 12) and `test_*' methods (kind 6)
inside `Test*' classes (kind 5).  Runs tests with `python -m pytest'."
  (setq-local
   eglot-test-at-point-command nil
   eglot-test-at-point-name-fn nil
   eglot-test-at-point-run-command-fn (lambda (name) (format "python -m pytest -v %s::%s" (buffer-file-name) name))
   eglot-test-at-point-get-name-fn #'eglot-test-at-point--python-name-at-point))

(defun eglot-test-at-point--python-name-at-point ()
  (when-let* ((server (eglot-current-server))
	      (_ (eglot-server-capable :documentSymbolProvider))
	      (pos (point))
	      (symbols (eglot--request server
				       :textDocument/documentSymbol
				       `(:textDocument ,(eglot--TextDocumentIdentifier))
				       :cancel-on-input non-essential)))
    (or
     ;; top-level test_* functions
     (when-let* ((sym (seq-find
                       (lambda (sym)
                         (let ((name (plist-get sym :name))
                               (kind (plist-get sym :kind))
                               (range (plist-get sym :range)))
                           (and (eql kind 12)
                                (string-prefix-p "test_" name)
                                range
                                (let ((reg (eglot-range-region range)))
                                  (and (<= (car reg) pos) (< pos (cdr reg)))))))
                       symbols)))
       (plist-get sym :name))
     ;; test_* methods inside Test* classes
     (seq-some
      (lambda (sym)
        (when-let* ((class-name (plist-get sym :name))
                    (kind (plist-get sym :kind))
                    (children (plist-get sym :children))
                    ((eql kind 5))
                    ((string-prefix-p "Test" class-name)))
          (seq-some
           (lambda (method)
             (when-let* ((mname (plist-get method :name))
                         (mkind (plist-get method :kind))
                         (mrange (plist-get method :range))
                         ((eql mkind 6))
                         ((string-prefix-p "test_" mname))
                         (reg (eglot-range-region mrange))
                         ((<= (car reg) pos))
                         ((< pos (cdr reg))))
               (format "%s::%s" class-name mname)))
           children)))
      symbols))))

(add-hook 'go-mode-hook #'eglot-test-at-point-setup-go)
(add-hook 'go-ts-mode-hook #'eglot-test-at-point-setup-go)
(add-hook 'rustic-mode-hook #'eglot-test-at-point-setup-rust)
(add-hook 'rust-mode-hook #'eglot-test-at-point-setup-rust)
(add-hook 'python-ts-mode-hook #'eglot-test-at-point-setup-python)
(add-hook 'python-mode-hook #'eglot-test-at-point-setup-python)

(defun eglot-test-at-point--go-names-in-buffer ()
  "Return list of (NAME . TYPE) for every test/benchmark in the current Go buffer.
TYPE is `test' or `benchmark'.  Uses gopls `test' code lenses."
  (when-let* ((server (eglot-current-server))
              (_ (eglot-server-capable :codeLensProvider))
              (lenses (eglot--request server
                                      :textDocument/codeLens
                                      `(:textDocument ,(eglot--TextDocumentIdentifier))
                                      :cancel-on-input non-essential)))
    (thread-last lenses
      (seq-filter (lambda (lens)
                    (when-let* ((cmd (plist-get lens :command)))
                      (equal (plist-get cmd :command) "gopls.run_tests"))))
      (seq-mapcat (lambda (lens)
                    (let* ((cmd (plist-get lens :command))
                           (raw-args (plist-get cmd :arguments))
                           (run-args (when (and raw-args (> (length raw-args) 0))
                                       (aref raw-args 0))))
                      (append
                       (seq-map (lambda (name) (cons name 'test))
                                (or (plist-get run-args :Tests) []))
                       (seq-map (lambda (name) (cons name 'benchmark))
                                (or (plist-get run-args :Benchmarks) [])))))))))

(defun eglot-test-at-point-name ()
  "Return the test name at point via LSP, or nil.
If `eglot-test-at-point-get-name-fn' is set, calls it directly (for languages
whose server lacks test code lenses).  Otherwise uses `textDocument/codeLens'
with `eglot-test-at-point-command' and `eglot-test-at-point-name-fn'."
  (if eglot-test-at-point-get-name-fn
      (funcall eglot-test-at-point-get-name-fn)
    (when-let* ((cmd-name eglot-test-at-point-command)
                (name-fn eglot-test-at-point-name-fn)
                (server (eglot-current-server))
                (_ (eglot-server-capable :codeLensProvider))
                (pos (point))
                (lenses (eglot--request server
                                          :textDocument/codeLens
                                          `(:textDocument ,(eglot--TextDocumentIdentifier))
                                          :cancel-on-input non-essential)))
      (when-let* ((best (thread-last lenses
                           (seq-filter (lambda (lens)
                                         (let* ((cmd (plist-get lens :command))
                                                (range (plist-get lens :range)))
                                           (and cmd range
                                                (equal (plist-get cmd :command) cmd-name)
                                                (<= (car (eglot-range-region range)) pos)))))
                           last
                           car))
                   (best-args (plist-get (plist-get best :command) :arguments)))
        (funcall name-fn best-args)))))

;;;###autoload
(defun eglot-test-at-point ()
  "Run the test at point in a dedicated compilation buffer.
Buffer is named *<project>-test-<directory>*.  Requires
`eglot-test-at-point-run-command-fn' and either `eglot-test-at-point-get-name-fn'
(direct discovery) or the `eglot-test-at-point-*' trio (code lens path)
to be set buffer-locally."
  (interactive)
  (unless eglot-test-at-point-run-command-fn
    (user-error "no test runner configured for %s" major-mode))
  (let* ((test-name (or (eglot-test-at-point-name)
                        (user-error "no test found at point")))
         (directory (file-name-directory (buffer-file-name)))
         (pkg-name (file-name-directory directory))
         (buf-name (format "*%s-test-%s*" (approximate-project-name) pkg-name))
         (command (funcall eglot-test-at-point-run-command-fn test-name))
         (default-directory directory))
    (compilation-start command 'compilation-mode (compile-buffer-name buf-name))))

;;;###autoload
(defun eglot-test-at-point-select ()
  "Select a test from the current buffer and run it in a compilation buffer.
Uses `annotated-completing-read' for completion with type annotations.
Buffer is named *<project>-test-<directory>*.  Requires
`eglot-test-at-point-list-fn' and `eglot-test-at-point-run-command-fn'
to be set buffer-locally."
  (interactive)
  (unless eglot-test-at-point-run-command-fn
    (user-error "no test runner configured for %s" major-mode))
  (unless eglot-test-at-point-list-fn
    (user-error "no test listing function configured for %s" major-mode))
  (let* ((table (or (funcall eglot-test-at-point-list-fn)
                     (user-error "no tests found in buffer")))
         (test-name (annotated-completing-read table :prompt "Test: " :require-match t))
         (directory (file-name-directory (buffer-file-name)))
         (pkg-name (file-name-directory directory))
         (buf-name (format "*%s-test-%s*" (approximate-project-name) pkg-name))
         (command (funcall eglot-test-at-point-run-command-fn test-name))
         (default-directory directory))
    (compilation-start command 'compilation-mode (compile-buffer-name buf-name))))

(provide 'eglot-test-at-point)
;;; eglot-test-at-point.el ends here"
