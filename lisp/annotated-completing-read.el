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

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'xlib)

(defvar annotated-completing-read-history (ht-create)
  "Hash table mapping command symbols to per-command minibuffer history lists.
Keys are symbols — typically `this-command' at call time — and values are
the standard Emacs history lists accumulated by `completing-read'.")

(defvar annotated-completing-read--history-cell nil
  "Temporary per-call history list threaded through `completing-read'.
`completing-read' mutates a named special variable; this one is pre-loaded
from `annotated-completing-read-history' and written back after each call.")

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
    (let ((annotated-completing-read--history-cell
           (ht-get annotated-completing-read-history hist-key)))
      (prog1
          (completing-read prompt collection nil require-match initial-input
                           'annotated-completing-read--history-cell)
        (ht-set! annotated-completing-read-history hist-key
                 annotated-completing-read--history-cell)))))

(provide 'annotated-completing-read)
;;; annotated-completing-read.el ends here
