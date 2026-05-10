;;; annotated-completing-read.el --- Completing-read with aligned annotations -*- lexical-binding: t -*-

;; Author: sam kleinman
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (dash "2.19") (ht "2.3") (s "1.12") (f "0.20"))
;; Keywords: convenience, matching
;; URL: https://github.com/tychoish/dot-emacs

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
;; Also provides `annotated-completing-read-context-from-point', a context-aware
;; selection interface that populates candidates from thing-at-point, the
;; active region, the current line, and the kill ring.

;;; Code:

(require 'cl-lib)
(require 'xlib)
(require 'dash)
(require 'ht)
(require 's)
(require 'f)

(defun annotated-completing-read--length-of-longest (items)
  (apply #'max 0 (mapcar #'length items)))

(defun annotated-completing-read--prefix-padding (key longest)
  (make-string (abs (+ 4 (- longest (length key)))) ?\s))

(defvar annotated-completing-read-history (ht-create)
  "Hash table mapping command symbols to per-command minibuffer history lists.
Keys are symbols — typically `this-command' at call time — and values are
the standard Emacs history lists accumulated by `completing-read'.")

;;;###autoload
(cl-defun annotated-completing-read
    (table &key (prompt "=> ") require-match category history group-name group-display initial-input sort-fn)
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

SORT-FN is an optional function (LIST-OF-STRINGS) => LIST-OF-STRINGS that
reorders candidates before display.  Surfaced as `display-sort-function' in
completion metadata, so vertico and other UIs apply it before rendering.

Signals `user-error' if TABLE is not a hash table."
  (unless (hash-table-p table)
    (user-error "TABLE must be a hash table mapping candidates to annotations"))
  (let* ((prompt (if (string-suffix-p " " prompt) prompt (concat prompt " ")))
         (hist-key (or history this-command 'annotated-completing-read))
         (longest (annotated-completing-read--length-of-longest (ht-keys table)))
         (annotate-fn (lambda (candidate)
                        (concat (annotated-completing-read--prefix-padding candidate longest)
                                (ht-get table candidate))))
         (name-fn (cond ((functionp group-name) group-name)
                        (group-name (lambda (_candidate) group-name))))
         (display-fn (or group-display #'identity))
         (group-fn (when name-fn
                     (lambda (candidate transform)
                       (if transform
                           (funcall display-fn candidate)
                         (funcall name-fn candidate)))))
         (collection (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotate-fn)
                             ,@(when category `((category . ,category)))
                             ,@(when group-fn `((group-function . ,group-fn)))
                             ,@(when sort-fn `((display-sort-function . ,sort-fn))))
                         (complete-with-action action (ht-keys table) str pred))))
	 (hist-sym (make-symbol "history-cell")))
    (set hist-sym (ht-get annotated-completing-read-history hist-key))
    (prog1
	(completing-read prompt collection nil require-match initial-input hist-sym)
      (ht-set! annotated-completing-read-history hist-key (symbol-value hist-sym)))))

(defun annotated-completing-read--context-candidates (&optional seed)
  "Build an annotated hash table of candidates from the current context.
SEED is a string or list of strings to include as explicit candidates."
  (let ((table (ht-create))
	(idx 0))

    (->> (cond ((listp seed) seed)
               ((stringp seed) (list seed)))
         (-keep #'s-trimmed-or-nil)
         (--filter (< (length it) 128))
         (--mapc (ht-set table it "seed")))

    (->> (-concat (--map (cons 'text-mode it) '(word email url sentence))
                  (--map (cons 'prog-mode it) '(symbol word sexp defun)))
         (--keep (when-let* ((_ (derived-mode-p (car it)))
			     (val (thing-at-point (cdr it)))
                             (val (s-trimmed-or-nil val))
                             (val (substring-no-properties val))
                             (_ (< (length val) 64)))
                   (cons val (format "%s at point" (cdr it)))))
         (--mapc (ht-set table (car it) (cdr it))))

    (when-let* ((_ (use-region-p))
		(sel (buffer-substring-no-properties (region-beginning) (region-end)))
                (sel (s-trimmed-or-nil sel))
                (_ (< (length sel) 128)))
      (ht-set table sel (format "region · %s" (buffer-name))))

    (when-let* ((line (thing-at-point 'line))
                (line (s-trimmed-or-nil (substring-no-properties line)))
                (_ (< (length line) 128)))
      (ht-set table line (format "line · %s" (buffer-name))))

    (->> kill-ring
         (-map #'substring-no-properties)
         (-keep #'s-trimmed-or-nil)
         (--filter (< (length it) 128))
         (-take 10)
         (--mapc (ht-set table it (format "kill-ring [%d]" (cl-incf idx)))))

    table))

;;;###autoload
(defun annotated-completing-read-context-from-point (&optional prompt seed &key history)
  "Select a string from context-aware candidates with PROMPT.
Candidates are drawn from thing-at-point, the active region, the current
line, the kill ring, and any explicit SEED strings.  SEED may be a string
or a list of strings.

HISTORY is a symbol passed to `annotated-completing-read' to scope the
per-command history; defaults to `this-command', giving each calling
command its own isolated history."
  (let* ((cmd (or history this-command 'annotated-completing-read-context-from-point))
         (candidates (annotated-completing-read--context-candidates seed)))
    (if (> (ht-size candidates) 0)
        (annotated-completing-read
         candidates
         :require-match nil
         :prompt (or prompt "context: ")
         :history cmd)
      "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; directory selection

(defun annotated-completing-read--directory-clean (dirs)
  "Normalise DIRS: expand relative paths, drop nil/blank, deduplicate."
  (->> dirs
       (-keep #'s-trimmed-or-nil)
       (--map (or (when (f-absolute-p it) it)
                  (expand-file-name it)))
       (f-distinct)))

(defun annotated-completing-read--directory-parents (&optional start stop)
  "Return intermediate directory paths walking up from START to STOP."
  (let* ((start (or start default-directory))
         (stop (or stop "~/"))
         (stop-path (expand-file-name (string-trim stop)))
         (current (expand-file-name (string-trim start)))
         (output (list stop-path current)))
    (while (and
            current
            (or (not (string= current stop-path))
                (not (string-prefix-p stop-path current))))
      (setq current (file-name-parent-directory current))
      (push current output))
    (->> output
         (f-filter-directories '(cannonicalize unique)))))

(defun annotated-completing-read--directory-default-candidates ()
  "Assemble context-aware directory candidates from project, buffers, and point."
  (let* ((proj-root (approximate-project-root))
         (home (expand-file-name "~/"))
         (proj-bufs (->> (approximate-project-buffers)
                         (--keep (when (bufferp it)
                                   (with-current-buffer it
                                     (when-let* ((f (buffer-file-name it)))
                                       (cond ((f-directory-p f) f)
                                             ((f-file-p f) (f-dirname f))
                                             (t default-directory)))))))))
    (--> (append
          (annotated-completing-read--directory-parents default-directory proj-root)
          proj-bufs
          (list (thing-at-point 'filename)
                (thing-at-point 'existing-filename)
                default-directory
                user-emacs-directory
                home))
         (if (or (and (length< it 16)
                      (not (f-equal-p home proj-root)))
                 current-prefix-arg)
             (-join (f-directories proj-root) it)
           it)
         (f-filter-directories '(cannonicalize unique) it))))

(defun annotated-completing-read--directory-entry-counts (dir)
  "Return a brief annotation with subdirectory and file counts for DIR."
  (if (file-accessible-directory-p dir)
      (let* ((entries (directory-files dir t "\\`[^.]"))
             (n-dirs  (cl-count-if #'file-directory-p entries))
             (n-files (- (length entries) n-dirs)))
	(format "%d dirs, %d files" n-dirs n-files))
    ""))

;;;###autoload
(cl-defun annotated-completing-read-directory (&optional &key candidates prompt require-match)
  "Select a directory with annotated completion.
CANDIDATES is an explicit list of directory paths; if nil, a context-aware
list is computed from the project root, open buffers, and `thing-at-point'.
PROMPT defaults to \"directory: \".  REQUIRE-MATCH is passed through to
`annotated-completing-read'.

With 8 or fewer candidates the annotation shows the directory's relationship
to the current directory (\"parent\", \"project root\", etc.).  With more
than 8 candidates candidates are grouped by that relationship label and the
annotation shows entry counts instead."
  (let* ((dirs (or (annotated-completing-read--directory-clean candidates)
                   (annotated-completing-read--directory-default-candidates)))
         (project-root (approximate-project-root))
         (relationship (ht-create)))
    (--each dirs
      (ht-set relationship it
              (cond
               ((f-equal-p it default-directory) "current directory")
               ((f-equal-p it project-root) "project root")
               ((f-ancestor-of-p it default-directory) "parent")
               ((f-ancestor-of-p default-directory it) "child")
               ((f-equal-p (f-parent it) (f-parent default-directory)) "sibling")
               (t ""))))
    (if (> (ht-size relationship) 8)
        (let ((counts (ht-create)))
          (--each dirs (ht-set counts it (annotated-completing-read--directory-entry-counts it)))
          (annotated-completing-read counts
           :prompt (or prompt "directory: ")
           :require-match require-match
           :group-name (lambda (c)
                         (let ((r (ht-get relationship c "")))
                           (if (string-empty-p r) "other" r)))))
      (annotated-completing-read relationship
       :prompt (or prompt "directory: ")
       :require-match require-match))))

(provide 'annotated-completing-read)
;;; annotated-completing-read.el ends here
