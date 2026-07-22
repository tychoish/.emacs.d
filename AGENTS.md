# Agent Guidelines: Emacs Configuration

This file documents code style, organizational conventions, and development
principles for this Emacs configuration. Follow these guidelines when making
any changes.

Emacs Lisp-specific coding rules (naming conventions, list operations, macros, cl-lib
restrictions) live in `.claude/rules/lisp.md`.

### Hash Table Access

Never use `gethash`, `puthash`, or any `ht.el` function. Use `map.el` accessors instead —
they work uniformly across hash tables, alists, and plists.

| Avoid                             | Use instead                       |
|-----------------------------------|-----------------------------------|
| `(gethash key table)`             | `(map-elt table key)`             |
| `(puthash key val table)`         | `(setf (map-elt table key) val)`  |
| `(ht-get table key)`              | `(map-elt table key)`             |
| `(ht-set table key val)`          | `(setf (map-elt table key) val)`  |
| `(ht-create #'equal)`             | `(make-hash-table :test #'equal)` |
| `(ht-each (lambda (k v) ...) t)`  | `(map-do (lambda (k v) ...) t)`   |

Use `map-into` to build a hash table from a list of cons cells without an explicit loop:

```elisp
(map-into (seq-map (lambda (x) (cons (key-fn x) x)) items)
          '(hash-table :test equal))
```

---

## File Organization

### Module structure

The configuration is split into:

| File                                | Purpose                                                                                       |
|-------------------------------------|-----------------------------------------------------------------------------------------------|
| `early-init.el`                     | GC suppression, native-comp settings only. No `require` calls.                                |
| `init.el`                           | Entry point. Sets global variables, loads modules sequentially.                               |
| `lisp/tychoish-common.el`           | Instance ID, display utils, buffer/file helpers. No UI config.                                |
| `lisp/bootstrap.el`                 | Keybindings, global `setq` settings, hooks, startup functions. Loaded before `tychoish-core`. |
| `lisp/tychoish-core.el`             | All `use-package` forms. Organized by functional area with section headers.                   |
| `lisp/tychoish-mail.el`             | Mu4e and mail account configuration.                                                          |
| `lisp/orgx.el`                      | Org-mode, org-roam, capture templates.                                                        |
| `lisp/xtdlib.el`                    | Pure utility library: extensions, macros, no Emacs UI deps.                                   |
| `lisp/builder.el`                   | Compilation buffer system.                                                                    |
| `lisp/annotated-completing-read.el` | annotated-completing-read (ACR) completion utility.                                           |
| `elpa/agent-shell-menu/`            | ACR-based menus, transient prefixes, buffer/permission/command/collapse UI (own git repo)     |
| `lisp/eglot-test-at-point.el`       | Eglot test runner helper.                                                                     |
| `user/*.el`                         | Per-machine overrides. Loaded last; not committed.                                            |

### Every `.el` file must have

```elisp
;;; filename.el --- one-line description -*- lexical-binding: t; -*-

;;; Commentary:
;; Brief description of purpose.

;;; Code:

;; ... content ...

(provide 'filename)
;;; filename.el ends here
```

Lexical binding is mandatory on every file.

---

## Loading and Startup

### Defer everything possible

- Use `use-package` with `:defer t` or `:commands` / `:hook` / `:bind` (which imply deferral) for packages that don't need to load at startup. Always specify `:commands` last.
- Use `with-eval-after-load` (not `eval-after-load`) for configuration that depends on a package being loaded but doesn't need to trigger loading.
- Use `declare-function` to suppress byte-compiler warnings for functions from deferred packages.

### Load order

```
early-init.el
  └─ init.el (inside with-gc-suppressed)
       ├─ xtdlib (eval-when-compile + load-path setup)
       ├─ bootstrap (keybindings, settings, hooks)
       ├─ tychoish-core (use-package forms)
       ├─ tychoish-mail
       ├─ orgx
       └─ user/*.el
```

Do not add new top-level `require` calls to `init.el` outside this sequence without a strong reason.

### Hooks over direct calls

Prefer `add-hook` / `add-one-shot-hook` over calling initialization functions directly at load time. This keeps startup linear and makes dependencies explicit.

### `add-one-shot-hook` usage

Use the correct keyword for the body:

- `:function symbol` — pass a function symbol (no parens): `:function #'my-fn`
- `:form (expr)` — pass an arbitrary expression: `:form (progn (foo) (bar))`
- Never pass a call-expression as `:function` — use `:form` instead

The `after-first-frame-created` pseudo-hook resolves to `server-after-make-frame-hook` for daemon instances and `window-setup-hook` otherwise. Use it for anything that needs a live frame.

---

## Settings and Configuration

### `setq` placement

- Global defaults and UI settings belong in `bootstrap.el`, in clearly labeled sections.
- Package-specific settings belong in the `:init` or `:config` block of the corresponding `use-package` form.
- Per-buffer settings use `setq-local` inside mode hooks.
- Do not set the same variable twice. Check before adding new `setq` calls.

### State file paths

Always use `sprite-state-path` to build paths under the state directory. Never hardcode `~/.emacs.d/` paths directly in configuration. This function incorporates the hostname and instance name to keep multi-instance state isolated.

### `custom-file`

Do not add variables to `custom.el` by hand. `custom-file` is set to a state-path so Emacs can write there; configuration should use explicit `setq` in the appropriate source file instead.

---

## Performance

### Profiling first

Before adding any lazy-loading or deferral complexity, measure with `M-x profiler-start` / `M-x profiler-report` or the `--with-slow-op-timing` CLI flag. Do not optimize speculatively.

### `with-slow-op-timer`

Wrap `require` calls and other potentially slow operations with `with-slow-op-timer`. The timer only logs when `slow-op-reporting` is non-nil (set by `--with-slow-op-timing`), so it has no production overhead.

### Avoid advice on hot paths by default

`advice-add` on `run-hooks`, `run-hooks-with-args`, or other frequently-called Emacs internals should only be active during active debugging sessions, not in normal operation. Gate them behind the slow-op CLI flag.

### Idle timers over hooks for non-urgent I/O

Disk writes (desktop save, abbrev save, etc.) belong in `run-with-idle-timer`, not in `after-save-hook`. The idle timer fires only when Emacs is idle, not mid-keypress.

---

## Error Handling

- Use `user-error` (not `error`) for conditions caused by incorrect user input or configuration — these display without a backtrace.
- Use `error` for internal invariant violations.
- Validate inputs at public function boundaries; trust internal calls.

---

## Testing

Tests live in `test/`. Load the test file with `M-x load-file` or run with:

```sh
emacs -batch -l test/tychoish-test.el
```

When adding a new utility to `xtdlib.el` or a DSL macro, add at minimum a smoke test that exercises the generated output. The `eglot-test-at-point.el` module has examples of test runner integration.

---

## Transient Menus

Every key sequence within a `transient-define-prefix` must be unique across all groups in that prefix. Transient does not enforce this at compile time or load time — duplicate keys silently shadow each other, so the collision only becomes apparent at runtime. Always audit for duplicates before committing changes to a transient menu.

---

## Commit and Change Hygiene

- Keep changes to a single concern per commit. Mixing a bug fix with a refactor makes bisecting harder.
- After changing `bootstrap.el` or `xtdlib.el`, byte-compile with `M-x byte-compile-all-user-emacs-files` (bound by default) to catch warnings.
- Removing a keybinding: check all other files for references to the bound command before removing its map entry.
- The `user/` directory is gitignored. Machine-specific overrides go there, not in the committed files.

---

## Agent development workflow

### Always use emacsclient

Use `emacsclient` for all Emacs operations — byte-compilation, `check-parens`, ERT tests, and arbitrary elisp evaluation.

Only invoke emacs directly as a smoke test for the entire config **OR** to ensure that the current session does not have stale state.

### Byte-compilation hygiene

After byte-compiling a file as a correctness check, delete the `.elc` file — in `lisp/`,
`user/`, and `test/` directories remove any/all `.elc` files. Byte-compilation is used
only to catch errors, not to produce artifacts.

Use `builder-emacs-conf-byte-compile-and-delete-artifact` (in `builder.el`) instead of calling
`byte-compile-file` directly — it deletes the `.elc` for you, and it still validates files
that declare `no-byte-compile: t` (e.g. `bootstrap.el`, `tychoish-core.el`), which
`byte-compile-file` would otherwise silently skip:

```sh
emacsclient --eval '(builder-emacs-conf-byte-compile-and-delete-artifact "lisp/foo.el")'
```

Returns `t` on a clean compile; check `*Compile-Log*` for warnings/errors otherwise.

### Reload after changes

After verifying an elisp file compiles cleanly, reload it with
`emacsclient --eval '(load-file "/path/to/file.el")'`. Changes don't take effect in the
running Emacs until the file is reloaded.

After reloading `lisp/bootstrap.el` ALWAYS reload `lisp/tychoish-core.el`.

### Testing

All fixes to bugs must include a regression test that would have caught the
bug. Where possible, write the reproduction test before making the fix so the
test fails first, then passes after the fix.

Features and new functionality are not complete until tests have been written.
A task is not done when the code compiles and appears to work — it is done when
the relevant test suite passes with coverage for the new or fixed behaviour.

### Code Style

Prefer standard library functions over reimplementing basic functionality.

Using functionality from `xtdlib` is generally acceptable, though some packages
(annotated-completing-read, agent-shell-queue) should not pick up this dependency without
consultation. See `lisp.md` for cl-lib restrictions, list iteration, and naming rules.

