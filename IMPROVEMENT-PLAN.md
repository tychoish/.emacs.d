# Emacs Configuration Improvement Plan

## Priority 1: Correctness Bugs

These are silent bugs that cause wrong behavior at runtime.

### DONE 1.1 Instance-ID cache variable name typo (`tychoish-common.el:26`)
`tychoish/resolve-instance-id` declares `tychoish-cache--resolved-instance-id` with `defvar-local` (double-hyphen) but sets `tychoish-cache/resolved-instance-id` (slash). The cache never populates; the function re-runs every call.
```elisp
;; Fix: match the name
(setq tychoish-cache--resolved-instance-id ...)
```

### DONE 1.2 Desktop save timer variable typo (`tychoish-bootstrap.el:413`)
`tychoish/desktop-save` saves the current time into `desktop/time-since-last-save`, but the guard comparison reads `desktop/last-save-time`. The comparison always sees `nil`, so the 150-second rate-limit never triggers.
```elisp
;; Fix:
(setq desktop/last-save-time (current-time))
```

### DONE 1.3 `buffers-matching-mode` kills buffers unexpectedly (`tychoish-common.el:496-499`)
The function is named "matching" but **kills** the buffers as a side effect. Callers expecting a list of buffers get the return value of `mapc` (also a list, but the buffers have been killed). The public `kill-buffers-matching-mode` at line 576 then calls this and calls `mapc #'kill-buffer` again on the already-killed result.
```elisp
;; Fix: separate selection from killing
(defun buffers-matching-mode (mode)
  (->> (buffer-list)
       (--select (with-current-buffer it (eq major-mode mode)))))
```

### DONE 1.4 `(length length)` typo (`tychoish-common.el:556`)
`kill-buffers-matching-path` reports `(length length)` instead of `(length killed)`.

### DONE 1.5 `s-normalize-symbol-name` ignores sanitized value (`xlib.el:126`)
The `sanatized` binding is computed but then `name` (the original) is passed to `s-replace-all` instead:
```elisp
;; Fix:
(canonicalized (s-replace-all ... sanatized))
```

### DONE 1.6 `use-jargon` undefined variable in `s-define-join-string-function` (`xlib.el:93-94`)
Two `cond` arms reference `use-jargon` but the keyword argument is `use-jargon-names`. Would cause a void-variable error if `"__"` or `"--"` were ever passed.
```elisp
;; Fix:
((equal char "__") (if use-jargon-names "dunder" "double-underscore"))
((equal char "--") (if use-jargon-names "ddash" "double-dash"))
```

### DONE 1.7 `-distinct-paths` syntax error (`xlib.el:211-214`)
Missing a closing paren — `(-distinct cell)` is parsed as a second variable binding in the `let` form, not as the body:
```elisp
;; Current (broken):
(let ((-compare-fn #'f-equal-p)
  (-distinct cell))))   ; <-- -distinct cell is a binding, not the body

;; Fix:
(let ((-compare-fn #'f-equal-p))
  (-distinct cell))
```

### DONE 1.8 `f-recursive-directories-containing` ignores its argument (`xlib.el:442-444`)
The `filename` parameter is shadowed by the lambda parameter, which always hardcodes `"go.mod"`.
```elisp
;; Fix:
(defun f-recursive-directories-containing (filename &optional path)
  (->> (f-entries path (lambda (f) (f-filename-is-p f filename)) t)
       (-map #'f-dirname)))
```

### DONE 1.9 `auto-mode-alist` regexes end with `'` not `\\'` (`tychoish-bootstrap.el:879-901`)
Every pattern like `"\\.tex'"` has a literal apostrophe at the end instead of an end-of-string anchor. These patterns match files containing `.tex'` (with an apostrophe), not `.tex` files.
```elisp
;; Fix all patterns:
("\\.tex\\'" . LaTeX-mode)
("\\.el\\'" . emacs-lisp-mode)
;; etc.
```

### DONE 1.10 Duplicate `setq` calls (`tychoish-bootstrap.el:302,307` and `tychoish-bootstrap.el:392,395`)
`read-file-name-completion-ignore-case` is set twice (lines 302 and 307). `project-list-file` is set twice inside `tychoish/set-up-emacs-instance-persistence` (lines 392 and 395).

---

## Priority 2: Performance

### 2.1 `run-hooks` advice active by default (`tychoish-bootstrap.el:506-507`)
```elisp
(advice-add 'run-hooks :around 'with-hook-timing)
(advice-add 'run-hooks-with-args :around 'with-hook-timing)
```
This wraps **every** hook invocation in the entire session with `->>` threading and `format`. Even when `slow-op-reporting` is nil the advice infrastructure itself has overhead. This adds latency to cursor movement, font-lock, idle timers, and everything else that uses hooks.

**Fix**: Gate the advice-add behind `slow-op-reporting` or `init-file-debug`, or remove it entirely and use `profiler-start`/`profiler-report` for ad-hoc profiling.

### DONE 2.2 Desktop save on `after-save-hook` (`tychoish-bootstrap.el:442`)
Saving the desktop on every file save (even with 40% probability) triggers disk I/O during interactive editing. The random gate is also asymmetric — average saves per edit is 0.4×, but variance is high.

**Fix**: Use an idle timer instead:
```elisp
(run-with-idle-timer 120 t #'tychoish/desktop-save)
```

### DONE 2.3 `(require 'desktop)` called after `(desktop-read)` (`tychoish-bootstrap.el:433-438`)
Desktop is loaded to call `desktop-read`, but `(require 'desktop)` appears five lines later. The require is presumably satisfied by autoloads, but the ordering is confusing and could break if autoloads change.

**Fix**: `(require 'desktop)` before using any desktop functions.

### DONE 2.4 `use-package-compute-statistics` always enabled (`tychoish-core.el:14`)
This records timing for every `use-package` form and is useful for profiling but adds non-trivial overhead in production. Should be disabled by default and enabled via the `--with-slow-op-timing` flag or `init-file-debug`.

### DONE 2.5 `with-gc-suppressed` discards body return value (`early-init.el:1-6`)
The outer `progn` returns the result of `garbage-collect`, not the body. For init.el's top-level usage this doesn't matter, but it's a subtle trap for other callers.
```elisp
;; Fix:
(defmacro with-gc-suppressed (&rest body)
  `(let ((gc-cons-threshold 800000000000000)
         (gc-cons-percentage 1.0))
     (prog1 (progn ,@body)
       (garbage-collect))))
```

---

## Priority 3: Clarity and API

### 3.1 `--in-place` has dead code (`xlib.el:325`)
The macro body contains two backtick forms. Only the second (the inline `let` loop) is returned; the first `(-in-place ...)` call is dead code that will never execute.

### 3.2 `s-contains-whitespace-p` inverted docstring (`xlib.el:139-142`)
The docstring says "Return t when VALUE is a string with **non-whitespace** content" but the body returns `t` when the string IS all whitespace (empty after trim). The name also says "contains whitespace" which should return `t` if there IS whitespace, not if it's all whitespace.

### 3.3 `add-one-shot-hook` `:function` vs `:form` inconsistency
Several call sites pass a call-form as `:function` (e.g., `tychoish-bootstrap.el:583`), relying on the fallthrough `((listp function) function)` path. The `:form` keyword exists precisely for this. These should use `:form` for clarity.

### 3.4 `approximate-project-*` confusing load-as-side-effect pattern (`xlib.el:769-797`)
All three `approximate-project-*` functions use:
```elisp
(or (when (and (package-installed-p 'projectile) (not (featurep 'projectile)))
      (require 'projectile) nil)  ; <-- always returns nil
    ...)
```
This loads projectile as a side effect but always returns nil so the next clause runs. This is hard to read. Use `(progn (require 'projectile) (projectile-project-root))` directly in the appropriate branch.

### 3.5 `tychoish/conf-state-path` vs `tychoish-get-config-file-path` naming inconsistency
Two functions exist with similar purposes but inconsistent naming (slash vs hyphen prefix). `tychoish/conf-state-path` is the more recent one and should be preferred; `tychoish-get-config-file-path` appears to be superseded.

### 3.6 `setq-when-nil` expansion-time `boundp` check (`xlib.el:726-728`)
The check `(unless (boundp variable))` runs at macro expansion time, making it impossible to use the macro on a variable that will be defined later. The check should either move to runtime or be removed.

---

## Priority 4: Organization

### 4.1 Split `tychoish-core.el` (34,764+ lines)
This file is the largest single source of startup opacity. Suggested splits by functional domain:
- `tychoish-completion.el` — vertico, marginalia, consult, orderless, embark, corfu
- `tychoish-editing.el` — multiple-cursors, expand-region, avy, surround
- `tychoish-vcs.el` — magit, git-timemachine, forge, diff-hl
- `tychoish-lsp.el` — eglot, flycheck/flymake, tree-sitter configs
- `tychoish-langs.el` — go-mode, rust-mode, python, etc.
- `tychoish-ai.el` — gptel, aidermacs, copilot configs
- `tychoish-ui.el` — themes, modeline, icons (currently split across bootstrap and core)

Each split file should have a standard header and `provide` form. The main `tychoish-core.el` becomes a thin loader or is removed in favor of direct `require` calls in `init.el`.

### 4.2 Add standard file header to `tychoish-common.el`
The file starts with a bare `; -*- lexical-binding: t; -*-` comment (lowercase semicolons) and lacks the standard `;;; tychoish-common.el --- description` header, `;;; Commentary:`, and `;;; Code:` separators.

### 4.3 Consolidate `auto-mode-alist` registrations
The systemd unit patterns (service, timer, target, mount, automount, slice, socket, path, conf) could use a loop:
```elisp
(dolist (ext '("service" "timer" "target" "mount" "automount" "slice" "socket" "path" "conf"))
  (add-to-list 'auto-mode-alist (cons (format "\\.%s\\'" ext) 'conf-unix-mode)))
```

### DONE 4.4 Collapse duplicate keybinding/keymap patterns
`tychoish-bootstrap.el` has a mix of `defvar-keymap` + `define-prefix-command` used for the robot map, and `bind-keys :prefix` used for all other maps. Pick one pattern and use it consistently.

**Decision**: use `bind-keys` in all cases. 

---

## Deferred / Low Priority

- `s-define-join-string-function` generates duplicate functions for hyphen/snake/space (called twice each, with and without `:use-jargon-names`). The jargon variants are defined but not used anywhere obvious.
- `--in-place` could be simplified to just call `-in-place` (remove the inline reimplementation).
- `with-toggle-once` macro in xlib.el is unused — confirm and remove if so.
- The `eglot-test-at-point.el` comment (line 192) references `tychoish/test-name-at-point` which should be `eglot-test-name-at-point`.
- Consider removing the commented-out keybindings in `tychoish-bootstrap.el:61-64` and `tychoish-bootstrap.el:113-114` — they add noise without adding value.
