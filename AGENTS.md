# Agent Guidelines: Emacs Configuration

Code style, organization, and development conventions for this config. Elisp-specific
coding rules (naming, list ops, macros, cl-lib restrictions) live in `.claude/rules/lisp.md`.

### Hash Table Access

Never use `gethash`, `puthash`, or `ht.el`. Use `map.el` — it works uniformly across
hash tables, alists, and plists.

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

- Defer everything possible: `use-package` with `:defer t`/`:commands`/`:hook`/`:bind`
  (`:commands` last). Use `with-eval-after-load` (not `eval-after-load`) for config that
  needs a package loaded but shouldn't trigger loading it. `declare-function` for deferred
  packages' functions.
- Load order: `early-init.el` → `init.el` (`with-gc-suppressed`) → `xtdlib` → `bootstrap` →
  `tychoish-core` → `tychoish-mail` → `orgx` → `user/*.el`. Don't add top-level `require`
  calls to `init.el` outside this sequence without a strong reason.
- Prefer `add-hook`/`add-one-shot-hook` over calling init functions directly at load time.
- `add-one-shot-hook`: `:function #'sym` (no parens) or `:form (progn ...)` for an
  arbitrary expression — never a call-expression as `:function`.
- `after-first-frame-created` resolves to `server-after-make-frame-hook` (daemon) or
  `window-setup-hook` (otherwise); use it for anything needing a live frame.

---

## Settings and Configuration

- `setq` placement: global/UI settings in `bootstrap.el`; package settings in that
  package's `use-package` `:init`/`:config`; per-buffer via `setq-local` in mode hooks.
  Never set the same variable twice.
- Use `sprite-state-path` for state-dir paths (incorporates hostname/instance name) —
  never hardcode `~/.emacs.d/` paths.
- Don't hand-edit `custom.el`; `custom-file` is a state-path Emacs writes to. Use explicit
  `setq` in the right source file instead.

---

## Performance

- Profile first (`M-x profiler-start`/`-report` or `--with-slow-op-timing`) before adding
  deferral complexity. Don't optimize speculatively.
- Wrap `require` and slow ops in `with-slow-op-timer` (no-op unless `slow-op-reporting`,
  set by `--with-slow-op-timing`).
- Don't `advice-add` on hot paths (`run-hooks`, etc.) outside active debugging; gate behind
  the slow-op flag.
- Non-urgent disk writes (desktop/abbrev save) go on `run-with-idle-timer`, not
  `after-save-hook` — idle timers don't fire mid-keypress.

---

## Error Handling

- `user-error` for bad user input/config (no backtrace); `error` for internal invariant
  violations. Validate at public function boundaries; trust internal calls.

---

## Testing

Tests live in `test/`: `M-x load-file` or `emacs -batch -l test/tychoish-test.el`. New
`xtdlib.el` utilities or DSL macros need at least a smoke test. See
`eglot-test-at-point.el` for test-runner integration examples.

---

## Transient Menus

Every key in a `transient-define-prefix` must be unique across all its groups — Transient
doesn't enforce this, so collisions silently shadow and only surface at runtime. Audit
before committing.

---

## Commit and Change Hygiene

- One concern per commit.
- After changing `bootstrap.el`/`xtdlib.el`, byte-compile with
  `M-x byte-compile-all-user-emacs-files` to catch warnings.
- Removing a keybinding: check other files for references first.
- `user/` is gitignored — machine-specific overrides go there, not in committed files.

---

## Agent development workflow

### Always use emacsclient

Use `emacsclient` for all Emacs operations — byte-compilation, `check-parens`, ERT tests,
arbitrary elisp. Invoke `emacs` directly only to smoke-test the whole config or rule out
stale session state.

### Byte-compilation hygiene

Byte-compilation is for catching errors, not producing artifacts — always delete the
resulting `.elc`. Use `builder.el`'s checks rather than calling `byte-compile-file`/`load`
directly; see their docstrings for details. Two independent isolation transports exist —
pick by what you're ruling out, and don't layer one on the other:

- **Subprocess** (`builder-emacs-conf-byte-compile-and-delete-artifact`,
  `-load-check`, `-elisp-package-test-isolated`, plus non-blocking
  `-async-byte-compile-check`): a throwaway `emacs --batch` with only `lisp/` on
  `load-path`. Catches a missing `require` this session would mask by already having it
  loaded. Returns `t`/`nil`; log lands in `*Compile-Log*`, `*builder-load-check-log*`, or
  `*builder-test-check-log*`.
- **Sprite** (`builder-emacs-conf-sprite-byte-compile-check`, `-sprite-load-check`,
  `-sprite-test-check`): runs in a persistent, already-running daemon instead — same
  loaded state as this session, so it won't catch a missing `require`, but will catch this
  session's own stale advice/redefinitions/buffer-local state. Returns a
  `sprite-direct-promise`; reports via `alert` into the same log buffers.

```sh
emacsclient --eval '(builder-emacs-conf-byte-compile-and-delete-artifact "lisp/foo.el")'
```

Check the log even when this returns `t` — a missing `require` is a warning
("is not known to be defined"), not necessarily a failed compile.

### Reload after changes

After verifying a file compiles cleanly, reload with
`emacsclient --eval '(load-file "/path/to/file.el")'` — changes don't take effect until
reloaded. Reloading `lisp/bootstrap.el` ALWAYS requires reloading `lisp/tychoish-core.el` too.

### Testing

Bug fixes need a regression test (write it first, so it fails before the fix and passes
after, where possible). New functionality isn't done until the relevant test suite passes
with coverage — not just when the code compiles and appears to work.

### Code Style

Prefer standard library functions over reimplementing basic functionality. `xtdlib` is
generally fine to use, except in annotated-completing-read/agent-shell-queue without
consultation. See `lisp.md` for cl-lib, list iteration, and naming rules.

