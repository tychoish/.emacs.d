;;; annotated-completing-read.el --- completing-read with aligned annotations -*- lexical-binding: t -*-

;; Author: sam kleinman
;; Maintainer: tychoish

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

;; Provides `annotated-completing-read', a wrapper around `completing-read'
;; that accepts a hash table of candidates to annotations and surfaces them
;; as aligned completion metadata understood by vertico, marginalia, and embark.
;;
;; Also provides `completing-read-context-from-point', a context-aware
;; selection interface that populates candidates from thing-at-point, the
;; active region, the current line, and the kill ring.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)
(require 'xlib)

(defvar annotated-completing-read-history (ht-create)
  "Hash table mapping command symbols to per-command minibuffer history lists.
Keys are symbols — typically `this-command' at call time — and values are
the standard Emacs history lists accumulated by `completing-read'.")

(cl-defun annotated-completing-read (table &key (prompt "=> ") require-match category history group-name group-display initial-input)
  "Read a candidate from TABLE with aligned per-candidate annotations.
TABLE is any Emacs hash table (`ht' or plain `make-hash-table') mapping
candidate strings to annotation strings.  Column alignment is computed
automatically; callers need not pad candidates or annotations.

PROMPT is the minibuffer prompt (default \"=> \"); a trailing space is appended
automatically if absent.

REQUIRE-MATCH, when non-nil, forces the user to select an existing candidate.
When nil (the default), arbitrary input not present in TABLE is accepted and
returned verbatim.

CATEGORY is an optional completion-category symbol surfaced as table metadata.
Completion UIs (vertico, embark, marginalia) use it to select annotations,
keybindings, and actions.  Common values:

  `file'              – file-name actions, path display via marginalia
  `buffer'            – buffer switching and embark buffer actions
  `command'           – M-x style command dispatch
  `symbol'            – Lisp symbol lookup and eldoc integration
  `bookmark'          – bookmark-jump actions
  `consult-grep'      – consult grep result actions (jump to line, etc.)
  `consult-mu'        – consult-mu mail account entries

HISTORY is a symbol key into `annotated-completing-read-history' (a hash table
of per-command history lists).  Defaults to `this-command' captured at call
time, giving each command its own isolated history automatically.  Pass an
explicit symbol to share history across several call sites.

GROUP-NAME is a function (CANDIDATE) => group-name-string that returns which
group a candidate belongs to, or a plain string for a single constant group.
When nil, no grouping metadata is emitted.

GROUP-DISPLAY is an optional function (CANDIDATE) => display-string that
controls how a candidate is rendered within its group.  Defaults to identity
(candidate displayed verbatim).  Only meaningful when GROUP-NAME is set.

Together GROUP-NAME and GROUP-DISPLAY are assembled into the `group-function'
completion metadata entry expected by vertico and other UIs.

INITIAL-INPUT is an optional string pre-filled into the minibuffer.

Signals `user-error' if TABLE is not a hash table."
  (unless (hash-table-p table)
    (user-error "TABLE must be a hash table mapping candidates to annotations"))
  (let* ((prompt      (if (string-suffix-p " " prompt) prompt (concat prompt " ")))
         (hist-key    (or history this-command 'annotated-completing-read))
         (longest     (length-of-longest-item (ht-keys table)))
         (annotate-fn (lambda (candidate)
                        (concat (prefix-padding-for-annotation candidate longest)
                                (ht-get table candidate))))
         (name-fn     (cond ((functionp group-name) group-name)
                            (group-name (lambda (_candidate) group-name))))
         (display-fn  (or group-display #'identity))
         (group-fn    (when name-fn
                        (lambda (candidate transform)
                          (if transform
                              (funcall display-fn candidate)
                            (funcall name-fn candidate)))))
         (collection  (lambda (str pred action)
                        (if (eq action 'metadata)
                            `(metadata
                              (annotation-function . ,annotate-fn)
                              ,@(when category `((category . ,category)))
                              ,@(when group-fn `((group-function . ,group-fn))))
                          (complete-with-action action (ht-keys table) str pred)))))
    (let ((hist-sym (make-symbol "history-cell")))
      (set hist-sym (ht-get annotated-completing-read-history hist-key))
      (prog1
          (completing-read prompt collection nil require-match initial-input hist-sym)
        (ht-set! annotated-completing-read-history hist-key
                 (symbol-value hist-sym))))))

(defun completing-read--context-candidates (&optional seed)
  "Build an annotated hash table of candidates from the current context.
SEED is a string or list of strings to include as explicit candidates."
  (let ((table (ht-create)))

    (->> (cond ((listp seed) seed)
               ((stringp seed) (list seed)))
         (-keep #'s-trimmed-or-nil)
         (--filter (< (length it) 128))
         (--mapc (ht-set table it "seed")))

    (->> (-concat (--map (cons 'text-mode it) '(word email url sentence))
                  (--map (cons 'prog-mode it) '(symbol word sexp defun)))
         (--keep (when (derived-mode-p (car it))
                   (when-let* ((val (thing-at-point (cdr it)))
                               (val (s-trimmed-or-nil val))
                               (val (substring-no-properties val))
                               (_ (< (length val) 64)))
                     (cons val (format "%s at point" (cdr it))))))
         (--mapc (ht-set table (car it) (cdr it))))

    (when (use-region-p)
      (when-let* ((sel (buffer-substring-no-properties (region-beginning) (region-end)))
                  (sel (s-trimmed-or-nil sel))
                  (_ (< (length sel) 128)))
        (ht-set table sel (format "region · %s" (buffer-name)))))

    (when-let* ((line (thing-at-point 'line))
                (line (s-trimmed-or-nil (substring-no-properties line)))
                (_ (< (length line) 128)))
      (ht-set table line (format "line · %s" (buffer-name))))

    (let ((idx 0))
      (->> kill-ring
           (-map #'substring-no-properties)
           (-keep #'s-trimmed-or-nil)
           (--filter (< (length it) 128))
           (-take 10)
           (--mapc (ht-set table it (format "kill-ring [%d]" (cl-incf idx))))))

    table))

(defun completing-read-context-from-point (&optional prompt seed &key history)
  "Select a string from context-aware candidates with PROMPT.
Candidates are drawn from thing-at-point, the active region, the current
line, the kill ring, and any explicit SEED strings.  SEED may be a string
or a list of strings.

HISTORY is a symbol passed to `annotated-completing-read' to scope the
per-command history; defaults to `this-command', giving each calling
command its own isolated history."
  (let* ((cmd (or history this-command 'completing-read-context-from-point))
         (candidates (completing-read--context-candidates seed)))
    (if (> (ht-size candidates) 0)
        (annotated-completing-read
         candidates
         :require-match nil
         :prompt (or prompt "context: ")
         :history cmd)
      "")))

(provide 'annotated-completing-read)
;;; annotated-completing-read.el ends here
