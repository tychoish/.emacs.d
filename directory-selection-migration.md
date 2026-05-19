# Plan: Migrate Directory Selection to `annotated-completing-read`

## Summary

Move the four directory-selection functions out of `builder.el` and into
`annotated-completing-read.el` as a general-purpose `completing-read-directory`
facility. The result is reusable by any package without pulling in `builder.el`.
Because `approximate-project-root` and `approximate-project-buffers` now live in
`xlib.el` — which `annotated-completing-read.el` already requires — the
project-context functions can be called directly with no indirection or hook
variables needed.

---

## Current state

### Functions in `builder.el`

| Symbol                                  | Visibility                   | Role                                                                                                      |
|-----------------------------------------|------------------------------|-----------------------------------------------------------------------------------------------------------|
| `builder--select-directory`             | internal (but widely called) | main entry point; wraps `annotated-completing-read`                                                       |
| `builder--clean-directory-options`      | internal                     | normalises an input list: expand relative paths, remove nil/blank/duplicates                              |
| `builder--directory-parents`            | internal                     | walks up the tree from `start` to `stop`, returning all intermediate paths                                |
| `builder--directory-default-candidates` | internal                     | assembles the full "smart" candidate list from project context, open buffers, kill ring, `thing-at-point` |

### Call sites outside `builder.el`

- `consult-tycho.el` — 1 call to `builder--select-directory` (in `tychoish-create-note-file`)
- `tychoish-common.el` — 1 call (in `kill-buffers-in-directory`), plus a `declare-function`
- `tychoish-core.el` — 5 calls (consult-rg variants, `tychoish-rg-compile`, `builder` use-package block)

### Dependencies of the directory functions

- **`f.el` / `xlib.el`**: `f-equal-p`, `f-ancestor-p`, `f-parent`, `f-directories`, `f-filter-directories`, `f-distinct` — `annotated-completing-read.el` already requires `xlib`
- **`xlib.el`**: `approximate-project-root`, `approximate-project-buffers` — both now live in `xlib.el`, which `annotated-completing-read.el` already requires; no indirection needed
- **`dash`/`s`**: already required in `annotated-completing-read.el`
- **Emacs built-ins**: `default-directory`, `user-emacs-directory`, `thing-at-point`

---

## Proposed design

### New public symbol

```elisp
(cl-defun completing-read-directory
    (&optional &key candidates prompt require-match)
  ...)
```

Replaces `builder--select-directory`.  Keyword arguments:

| Key              | Default         | Meaning                                                                       |
|------------------|-----------------|-------------------------------------------------------------------------------|
| `:candidates`    | `nil`           | Explicit list of directory paths; if nil, the default candidates are computed |
| `:prompt`        | `"directory: "` | Minibuffer prompt                                                             |
| `:require-match` | `nil`           | Passed through to `annotated-completing-read`                                 |

### Internal helpers (new in `annotated-completing-read.el`)

```elisp
(defun completing-read--directory-clean (dirs)
  "Normalise DIRS: expand relative paths, drop nil/blank, deduplicate.")

(defun completing-read--directory-parents (&optional start stop)
  "Return intermediate directory paths walking up from START to STOP.")

(defun completing-read--directory-default-candidates ()
  "Assemble the full context-aware candidate list.")
```

`--directory-default-candidates` calls `approximate-project-root` and
`approximate-project-buffers` directly (both in `xlib.el`).  For the home
directory boundary, use `(expand-file-name "~/")` — there is no
`user-home-directory` variable in standard Emacs or in this codebase.

### Annotation labels

Keep the existing set — they read well and tests already cover them:

| Condition                                              | Label                 |
|--------------------------------------------------------|-----------------------|
| `f-equal-p it default-directory`                       | `"current directory"` |
| `f-equal-p it project-root`                            | `"project root"`      |
| `f-ancestor-p it default-directory`                    | `"parent"`            |
| `f-ancestor-p default-directory it`                    | `"child"`             |
| `f-equal-p (f-parent it) (f-parent default-directory)` | `"sibling"`           |
| anything else                                          | `""`                  |

Consider adding grouping via `annotated-completing-read`'s `:group-name` so
the prompt organises candidates visually into "project", "parents", "other"
buckets — low-priority, do it if the candidate list grows long in practice.

---

## Migration steps

### 1. Add internal helpers to `annotated-completing-read.el`

Port `builder--clean-directory-options` → `completing-read--directory-clean`.
Port `builder--directory-parents` → `completing-read--directory-parents`.
Port `builder--directory-default-candidates` → `completing-read--directory-default-candidates`,
calling `approximate-project-root` and `approximate-project-buffers` directly.
Replace any reference to `user-home-directory` with `(expand-file-name "~/")`.

### 2. Add `completing-read-directory` to `annotated-completing-read.el`

Port `builder--select-directory`, rename, make keyword args explicit and
documented.  Add `;;;###autoload`.

### 3. Update call sites

| File                 | Change                                                                                                                                                                                                                                                                                                                                                                             |
|----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `builder.el`         | Replace `builder--select-directory` with `completing-read-directory` in `builder-change-directory`; remove `builder--clean-directory-options`, `builder--directory-parents`, `builder--directory-default-candidates` (now internal to `annotated-completing-read`). Keep internal calls that build the `builder-change-directory` candidate list wired to the new public function. |
| `consult-tycho.el`   | `builder--select-directory` → `completing-read-directory`                                                                                                                                                                                                                                                                                                                          |
| `tychoish-common.el` | same; remove `declare-function`                                                                                                                                                                                                                                                                                                                                                    |
| `tychoish-core.el`   | same (5 call sites)                                                                                                                                                                                                                                                                                                                                                                |

### 4. Tests

Add a `completing-read--directory-*` section to `test-annotated-completing-read.el`:

- `completing-read--directory-clean`: nil/blank/relative path normalisation, deduplication
- `completing-read--directory-parents`: start == stop, start is child of stop, stop unreachable
- `completing-read-directory`: annotation labels (using `cl-letf` to mock `f-equal-p`/`f-ancestor-p`), prompt forwarding, `:candidates` override

Port the four existing `builder-test/select-directory-*` tests from
`test-builder.el` to `test-annotated-completing-read.el` under the new names,
then remove them from `test-builder.el`.

### 5. Clean up `builder.el`

Remove the four functions and the `directory selection` comment block.  Keep
`builder-change-directory` but rewire it to call `completing-read-directory`
directly with an explicit `:candidates` list.

---

## What stays in `builder.el`

`builder-change-directory` is a `compilation-mode`-specific command; it stays in
`builder.el` but becomes a thin wrapper over `completing-read-directory`.

The candidate-list construction in `builder--get-candidates` (line ~409) that
calls `builder--directory-parents` can be updated to call
`completing-read--directory-parents` directly, or the relevant logic can be
inlined at that call site since it is one line.

---

## Open questions

- **`f-filter-directories` and `f-distinct`** are defined in `xlib.el`, which
  `annotated-completing-read` already requires.  Confirm nothing in the ported
  functions needs a symbol that lives somewhere else before starting.

- **Grouping**: the current annotation labels are sufficient for now, but if the
  default candidate list grows (e.g. recent directories from `recentf`), adding
  `:group-name` grouping to `completing-read-directory` will improve readability.
  Leave the hook open by accepting an optional `:group-fn` keyword from the start.
