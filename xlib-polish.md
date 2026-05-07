0# xlib.el Polish Plan

xlib.el is ~850 lines organized into 8 sections: timing/debug, string utilities
(s.el extensions), list utilities (dash.el extensions), hash table utilities
(ht.el extensions), file path utilities (f.el extensions), option flags, utility
macros (hooks/timers/context), and project context helpers.

The existing test suite (test-xlib.el, ~776 lines, ~112 tests) gives solid
coverage of string and list primitives but has notable gaps in macros, generated
functions, and filesystem utilities.

---

## 1. Bugs

### 1a. `s-or-char-equal`: second `cl-check-type` checks `char` instead of `value` (line 83)

```elisp
;; current
(cl-check-type char character "char must be character type")
(cl-check-type char (or string character) "value must be or string")  ;; BUG: char twice

;; fix
(cl-check-type char  character           "char must be character type")
(cl-check-type value (or string character) "value must be a string or character")
```

`value` is never type-checked; wrong-type input passes through silently and may
fail later with a confusing error.

### 1b. `s-define-join-string-function`: `?>` maps to `"lt"` instead of `"gt"` (line 100)

```elisp
((s-or-char-equal ?> char) "lt")  ;; wrong — > is greater-than, not less-than
```

This would generate `s-join-with-lt` for a `>` separator, which is backwards.
Fix: change `"lt"` to `"gt"`, or remove the clause if `>` as a separator is
never actually used. Also note line 105 is an exact duplicate of line 104
(`"<="` → `"lte"` appears twice).

### 1c. `s-number-word`: typo for 18 (line 189)

```elisp
((eql num 18) "eightteen")  ;; should be "eighteen"
```

The existing test at line 192 checks the range 10–20 but only validates the
return type, not the spelling, so this slips through.

### 1d. `merge-predicate-functions`: quoting bug makes `head` a function call (line 784)

```elisp
(defmacro merge-predicate-functions (&rest preds)
  `(lambda (value)
     (let ((head ,preds)          ;; BUG: ,preds expands to (p1 p2 ...) — a call!
           (predicate ,(car preds))
           (val t))
       ...)))
```

`,preds` in a quasiquote is not a list literal; it expands as an expression.
If `preds` is `(pred-a pred-b)`, `head` becomes the result of calling `pred-a`
with `pred-b` as an argument. Fix with `'(,@preds)` or by building the lambda
differently:

```elisp
(defmacro merge-predicate-functions (&rest preds)
  `(lambda (value)
     (and ,@(mapcar (lambda (p) `(funcall #',p value)) preds))))
```

This also makes the short-circuit behavior explicit. If the macro is genuinely
unused, consider removing it rather than fixing it.

---

## 2. Simplifications

### 2a. `-sparse-append`: 6 lines for a one-liner (lines 271–275)

```elisp
;; current (6 lines + docstring)
(defun -sparse-append (&rest items)
  "Append all items, filtering out nil values."
  (-non-nil (apply #'append items)))
```

Reduce to a single-line body — it's already correct and readable that way.

### 2b. `s-default`: redundant first `cond` clause (lines 161–162)

```elisp
;; current
(cond
 ((string-equal default input) default)  ;; returns default — but so does `(t input)` when equal
 ((eq input nil) default)
 ((string-equal input "") default)
 (t input))

;; simplified — nil check must stay first (string-equal signals on nil otherwise)
(cond
 ((or (null input) (string-equal input "")) default)
 (t input))
```

The `(string-equal default input)` clause returns `default` when they're equal,
but since `input` equals `default` in that case, falling through to `(t input)`
would return the same value. More importantly, the nil check (line 163) should
come *before* any `string-equal` call, because `string-equal` coerces symbols
(including `nil` → `"nil"`), which silently accepts nil rather than treating it
as empty. The rewrite above makes the intent unambiguous.

### 2c. `s-define-join-string-function` name-resolution block is overly long (lines 92–119)

The cond block for computing the join-separator name is ~28 lines for what is
essentially a lookup table. Consider extracting it into a constant alist:

```elisp
(defconst xlib--join-char-names
  `((?- "hyphen" "kebab") (?_ "underscore" "snake") (?\s "space" "spc")
    (?. "period" "dot") (?= "equal" "equal") (?+ "plus" "plus")
    (?| "pipe" "pipe") (?> "gt" "gt"))
  "Alist mapping join characters to (canonical-name jargon-name).")
```

This keeps the macro shorter and makes adding new separators trivial.

### 2d. `compile-buffer-name`: returns a backquoted form, not a lambda (line 779–780)

```elisp
;; current — returns a quoted list, not a callable
(defun compile-buffer-name (name)
  `(lambda (&optional _) ,name))

;; should return a proper function
(defun compile-buffer-name (name)
  (lambda (&optional _) name))
```

The tests pass only because `funcall` on a list form happens to work in some
Emacs versions, but this is not reliable. The function should close over `name`
as a proper lexical lambda.

---

## 3. Naming and Clarity

### 3a. `s-contains-whitespace-p` means the opposite of its name (line 152)

The docstring says "consisting entirely of whitespace" — the function returns t
only when the string is *blank* (empty or all-whitespace). A caller reading the
name would expect it to return t if whitespace is *present anywhere*.

Rename to `s-blank-p` (matching the s.el convention) and add a `defalias` for
`s-contains-whitespace-p`. This is safe because no external .

### 3b. `package-avalible-p` has a persistent typo (line 816)

The spelling is "available", not "avalible". Because this name appears throughout
the codebase, the safest fix is:

```elisp
(defun package-available-p (feature) ...)
(define-obsolete-function-alias 'package-avalible-p 'package-available-p "2025-01")
```

Update all call sites in the repo after adding the alias. (Do not
create a new alias, simply rename the function at all call sites.)

### 3c. Missing docstrings on commonly-used functions

The following functions have no docstring at all and would benefit from one:
- `-filter-s-trim` (line 74)
- `-distinct-paths` (line 224)
- `-unwind` (line 235) — has one but it just says "flatten one level"
- `-in-place` (line 288)
- `f-distinct` (line 435)
- `f-recursive-directories-containing` (line 463)
- `compile-buffer-name` (line 779)
- `merge-predicate-functions` (line 782)
- `mode-buffers-for-project` (line 805)
- `mode-buffers` (line 809)

`add-one-shot-hook` (line 576) is the most complex thing in the file and has no
docstring explaining the `:hook`/`:idle-timer`/`:persist`/`:count` interaction.

Docstrings should be clear, and concise, and indicate how functions or
symbols should be used. Docstrings should be more than a summary
of the implementation.

---

## 4. Test Coverage Gaps

Tests are needed roughly in this priority order.

### Priority 1: Behavior that recently broke or is likely to break

**`add-one-shot-hook` edge cases** — only 4 tests exist; missing:

- `:hook after-first-frame-created` (unquoted) routes to `window-setup-hook`
  in non-daemon sessions and `server-after-make-frame-hook` in daemon sessions.
  This is the sentinel that recently caused font-setup bugs; it needs explicit
  tests for both branches.
- `:hook 'after-first-frame-created` (quoted) now also routes correctly after
  the xlib.el fix; add a test confirming it.
- `:idle-timer N` — the timer branch (lines 651–657) is completely untested.
  Verify: the hook function is registered, the idle timer fires, the hook
  removes itself after firing, the counter increments inside the timer not at
  hook-registration time.
- `:count N` > 1 — hook fires N times then removes itself.
- Multiple hook names in a list: `(:hook '(hook-a hook-b))`.
- `:persist t` with `:count` — confirmed by existing test but should be
  combined with `:idle-timer`.

### Priority 2: Macros that generate code with no test coverage

**`create-toggle-functions`** (line 675) — generates three functions
(`turn-on-X`, `turn-off-X`, `toggle-X`). Tests needed:
- Each generated function exists and is callable.
- `turn-on` sets variable to t, `turn-off` sets to nil, `toggle` flips.
- `:local` variant uses `setq-local` (verify buffer-local behavior).
- `:keymap` + `:key` binds the toggle.

**`make-read-extended-command-for-prefix`** (line 695) — generates a completion
predicate and optionally a keybinding. Tests needed:
- Generated predicate returns t for commands whose names start with prefix.
- Generated predicate returns nil for others.
- Optional keybinding in `:bind-key` is registered.

**`setq-when-nil`** (line 769):
- Sets variable when it is nil.
- Does not overwrite a non-nil variable.
- Works with `setq-local`.

**`with-temp-keymap`** (line 762):
- Body can bind keys into the keymap.
- Returns the keymap.
- Keymap is fresh on each call.

**`make-run-hooks-function-for`** (line 668):
- Generated function calls the mode hook.
- Noop when hook variable is unset.

### Priority 3: List and hash-table functions with no tests

**`-distinct-paths`** (line 224): deduplicates path strings using `f-equal-p`.
- Identical strings → one entry.
- Paths that differ only in trailing slash → deduplicated.
- Empty list → empty list.

**`-distinct-by-alist-key`** (line 228):
- Removes entries with duplicate values for the specified key.
- Preserves first occurrence.

**`-strings`** (line 256) — complex options:
- Plain list of strings → unchanged.
- `:filter` option removes non-strings.
- `:stringify` option coerces non-strings.
- Both options together.
- nil input.

**`--in-place`** anaphoric macro (line 345):
- `it` is bound to each list element.
- Destructive in-place modification.
- Returns element count.

**`--map-uniq`** anaphoric macro (line 355):
- `it` is bound; results deduplicated.

**`define-ht-named-table`** (line 391):
- Defines a hash table variable.
- Generates `get`, `set`, `contains-p` functions.
- Functions operate on the correct table.

### Priority 4: Filesystem functions (require temp-file helpers)

**`f-mtime` / `f-atime`** (lines 408–411): need a temp file; verify return is a
time value, not nil.

**`f-distinct`** (line 435): duplicate paths removed; relative vs. absolute paths.

**`f-files-in-directory`** (line 455): returns files (not dirs); handles list
of directories; handles nonexistent path gracefully.

**`f-recursive-directories-containing`** (line 463): finds ancestor directories
containing a filename; stops at filesystem root; handles missing file.

**`f-visually-compress-to-N` generated functions** (lines 530–539): the macro
generates `f-visually-compress-to-1` through `f-visually-compress-to-10`; only
the underlying `f-visually-compress-path` is tested. Add one test per boundary
(1-char, 5-char, 10-char limit).

### Priority 5: Remaining string gaps

**`s-normalize-symbol-name`** (line 137):
- Spaces and punctuation → hyphens.
- Consecutive separators collapse.
- Already normalized strings unchanged.

**`s-number-word` spelling** — existing tests check range but not spelling.
Add exact-equality checks for each of 1–20 (catches the "eightteen" bug
and any future regressions).

---

## 5. Work Summary

| Category                       | Items                   | Effort |
|--------------------------------|-------------------------|--------|
| Bug fixes                      | 4 (§1a–1d)              | ~1h    |
| Simplifications                | 4 (§2a–2d)              | ~1h    |
| Naming/clarity                 | 3 + docstrings (§3a–3c) | ~1h    |
| Tests: Priority 1 (hooks)      | ~10 tests               | ~1h    |
| Tests: Priority 2 (macros)     | ~20 tests               | ~2h    |
| Tests: Priority 3 (list/ht)    | ~15 tests               | ~1h    |
| Tests: Priority 4 (filesystem) | ~12 tests               | ~1.5h  |
| Tests: Priority 5 (strings)    | ~5 tests                | ~30min |

Recommended order: bugs first (they may already be causing silent misbehavior),
then the `add-one-shot-hook` idle-timer tests (directly relevant to the
font/frame startup issue), then remaining test gaps, then the naming/clarity
work last since it may require touching call sites.
