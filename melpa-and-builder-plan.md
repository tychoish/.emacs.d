# MELPA Release & builder.el Improvement Plan

## Part 1: MELPA Release for `xlib` and `annotated-completing-read`

### Dependency ordering

`annotated-completing-read` depends on `xlib`, so `xlib` must land on MELPA first.

---

### xlib — tasks before submission

**1. DONE Package headers** (`xlib.el:1–8`)**
- Add missing `Keywords:` header (e.g. `extensions utility`)
- Add `URL:` header pointing to a dedicated repo
- Add GPL license block

**2. DONE Remove init-specific code** (`xlib.el:44–49`)

`cli/time-reporting` and `(add-to-list 'command-line-functions ...)` are
init-process hooks. They should be moved out of the library or guarded with a
`defcustom`.

**3. DONE Reduce questionable dependencies**

- `anaphora`: used only once (`aif` at `add-one-shot-hook:597`). Replace with a
  `when-let*` form and drop the dependency.
- `bind-key`: used in `create-toggle-functions` and
  `make-read-extended-command-for-prefix`. `bind-key` is on ELPA (bundled with
  `use-package`), so it is fine to keep — but consider making it optional via
  `declare-function` if callers do not always need it.
- DONE `cl-lib "1.0"`: `cl-lib` is bundled with Emacs since 24.3; the version pin is
  odd when the minimum Emacs is 28.1. Remove the explicit `cl-lib` entry from
  `Package-Requires`.

**4. New repo**

- Create `tychoish/xlib.el` on GitHub as a proper single-package repository.
- Add `xlib.el` and its test file `test-xlib.el`.
- Add `README.md` and a CI workflow running `make test` via `eldev` or `cask`.

**5. MELPA recipe**

```elisp
(xlib :fetcher github :repo "tychoish/xlib.el")
```

**6. Verification**

- Run `package-lint` and fix any warnings.
- Run `checkdoc` on all exported symbols.
- Confirm byte-compilation is clean with no warnings.

---

### annotated-completing-read — tasks before submission

**1. DONE Fix the `URL:` header** (`annotated-completing-read.el:8`)

Currently points to the monorepo (`tychoish/dot-emacs`). Change to a dedicated
repo once extracted.

**2. New repo**

- Create `tychoish/annotated-completing-read.el`.
- Copy over `annotated-completing-read.el` and
  `test/test-annotated-completing-read.el`.
- Add CI.

**3. DONE Add `xlib` to `Package-Requires`** (line 6)

`xlib` is already `require`d but not declared. Once xlib is on MELPA:

```
(xlib "0.1.0")
```

**4. DONE Clarify the `f` dependency**

All `f-*` calls in `annotated-completing-read.el` fall into two groups:

- Functions defined *in xlib* (`f-filter-directories`, `f-distinct`) — covered
  by the xlib dependency.
- Standard f.el primitives (`f-absolute-p`, `f-equal-p`, `f-directory-p`,
  `f-dirname`, `f-parent`, `f-ancestor-of-p`) — used directly in the file.

Verify whether `f` itself still needs its own `Package-Requires` entry or
whether the transitive dependency through xlib is sufficient. If `f` primitives
are called directly, keep `(f "0.20")` in the header.

**5. MELPA recipe**

```elisp
(annotated-completing-read :fetcher github :repo "tychoish/annotated-completing-read.el")
```

**6. Verification** — same `package-lint` / `checkdoc` / byte-compile gate as xlib.

---

## Part 2: builder.el Usability & Implementation Improvements

### DONE 1 — Fix the UX flow (confusing selections)

**Problem:** The current flow is buffer-first, command-second. The user must
pick (or name) an output buffer before knowing what they want to run. The
`y-or-n-p "recompile?"` prompt for existing buffers is a jarring interruption
mid-flow.

**Fix:** Invert the steps.

1. Select the command to run (from candidates).
2. After the command is confirmed, present a buffer selection — pre-selecting
   the buffer that last ran the same command, if one exists.
3. Replace `y-or-n-p "recompile?"` with: if the selected buffer already ran
   this command, default to recompile; allow overriding with prefix arg.

This change is entirely in `builder-compile-project` (`builder.el:122–183`).

---

### DONE 2 — Sort order

**Problem:** Candidates come from a hash table (`ht-create`), so presentation
order is arbitrary. The most-relevant candidates (current file, recently used)
are buried unpredictably.

**Fix:** Add a `:priority` slot to `builder-candidate` (integer, lower =
higher priority).

| Tier | Source                                                     |
|------|------------------------------------------------------------|
| 0    | Current file (vale, gofumpt, file-specific tests)          |
| 1    | Commands already running in compilation buffers (rerun)    |
| 2    | Project-level commands (go test, cargo test, make targets) |
| 3    | Shell / minibuffer history                                 |

Then add a sort function to the annotated-completing-read to order
results based on these factors (and also shorter keys first.)

---

### DONE 3 — Reduce noisy candidates

**Problem:** `minibuffer-shell-commands` (`builder.el:428–441`) adds the entire
`shell-command-history` and minibuffer history, producing dozens of irrelevant
entries.

**Fix:**
- Cap at the 20 most-recent unique entries from `shell-command-history`.
- Exclude entries already generated by other candidate functions (deduplicate
  against the table before inserting).

**Problem:** `project-compilation-buffer-commands` (`builder.el:443–461`)
generates 3 directory variants per command (project root, buffer dir, default
dir), multiplying the list 3×.

**Fix:** Only emit a variant when its directory differs meaningfully from the
others — skip variants where two of the three resolved paths are the same.

---

### DONE 4 — Performance

**Problem:** `builder--get-candidates` runs shell commands synchronously —
`make --dry-run`, `just --summary`, `go list` — blocking the UI for potentially
hundreds of milliseconds.

**Fix (short-term):**

- Move shell-command candidate generators behind `condition-case` with
  `call-process` and an explicit timeout rather than `shell-command-to-string`.
- Cap make target discovery at 50 targets per directory.

**Fix (medium-term):**

Make the cache project-scoped, not buffer-scoped. Store candidates in a
project-keyed hash table so the first buffer in a project pays the cost and
subsequent buffers reuse results. Invalidate on file-save in the project root.

The relevant variables are `builder--cached-candidates` (`builder.el:340`) and
`builder--cache-bypass` (`builder.el:342`).

---

### 5 — Simplify the extension point

**Problem:** `builder-register-candidates` is a macro that generates named
functions and optionally registers them on mode hooks. The macro interface is
non-obvious — callers must know the 4-argument protocol
`(project-root-directory project-name directories operation-table)` and
understand that the `:pipeline` value is an expression, not a function body.

**Fix:** Keep the macro but add a plain-function alternative:

```elisp
(defun builder-add-candidates (fn)
  "Register FN as a candidate generator.
FN is called with (ROOT NAME DIRS TABLE) where TABLE is a `ht' of
`builder-candidate' objects.  FN should call `builder--add-candidates'
to populate TABLE and return t."
  (add-hook 'builder-candidate-functions fn))
```

This lets users write a plain `defun` and register it without understanding the
macro. The macro becomes a convenience wrapper over `builder-add-candidates`,
not the primary API.

Also: document the `directories` argument clearly — it is the set of
directories derived from open project buffers and is what drives
language-specific discovery.

---

### 6 — Intuitive package extension (self-contained language files)

**Problem:** All language-specific candidate generators live in `builder.el`
itself. There is no clear convention for adding a new language from outside the
file, and the file becomes unwieldy as more languages are added.

**Fix:** Introduce a naming convention and autoload pattern. Move each language
block to a separate file that is autoloaded when the relevant major mode
activates:

| File                | Content                          | Loaded via                                                |
|---------------------|----------------------------------|-----------------------------------------------------------|
| `builder-go.el`     | Go candidates                    | `go-ts-mode-hook`, `go-mode-hook`                         |
| `builder-rust.el`   | Rust / Cargo candidates          | `rust-mode-hook`, `rust-ts-mode-hook`, `rustic-mode-hook` |
| `builder-python.el` | Python / ruff / black candidates | `python-mode-hook`, `python-ts-mode-hook`                 |
| `builder-elisp.el`  | Emacs Lisp / package-lint / elsa | `emacs-lisp-mode-hook`                                    |
| `builder-make.el`   | Make and Just targets            | always loaded                                             |

`builder.el` itself keeps only: the `builder-candidate` struct,
`builder-compile-project`, the cache machinery, and
`builder-register-candidates` / `builder-add-candidates`.

Each language file uses `builder-register-candidates` with `:hooks` so it only
activates when the user opens a file of that type. Cold startup cost is near
zero and the file is the obvious place to look when adding a new language.

---

## Summary of changes by file

| File                           | Change                                                                                            |
|--------------------------------|---------------------------------------------------------------------------------------------------|
| `xlib.el`                      | Add headers, remove init code, drop `anaphora`, new repo                                          |
| `annotated-completing-read.el` | Fix URL, add `xlib` to `Package-Requires`, new repo                                               |
| `builder.el`                   | Invert UX flow, sort candidates, cap noise, project-scoped cache, add `builder-add-candidates` fn |
| `builder-go.el` *(new)*        | Extract Go candidate generators                                                                   |
| `builder-rust.el` *(new)*      | Extract Rust candidate generators                                                                 |
| `builder-python.el` *(new)*    | Extract Python candidate generators                                                               |
| `builder-elisp.el` *(new)*     | Extract Emacs Lisp candidate generators                                                           |
| `builder-make.el` *(new)*      | Extract make / just candidate generators                                                          |
