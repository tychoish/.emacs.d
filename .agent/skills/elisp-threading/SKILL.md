---
name: elisp-threading
description: |
  Refactor nested seq-* / mapcar calls in Emacs Lisp into thread-last pipelines.
  Apply whenever two or more seq operations are chained or nested on the same collection.
triggers:
  - "flatten nested seq"
  - "thread-last"
  - "threading macro"
  - "seq-map seq-filter"
  - "nested mapcar"
  - "refactor elisp"
  - "simplify seq"
applies-to: "**/*.el"
---

# Elisp Threading: Flatten Nested `seq-*` Calls with `thread-last`

## The Rule

When two or more `seq-*` (or `mapcar`) operations are applied to the same
collection in sequence, replace the nesting with a `thread-last` pipeline.
Each stage becomes one indented line. Single operations are left as-is.

```elisp
;; Bad — nested, hard to read
(seq-map #'process (seq-filter #'keep-p items))

;; Good — pipeline, reads top to bottom
(thread-last items
  (seq-filter #'keep-p)
  (seq-map #'process))
```

`thread-last` threads the value as the **last** argument. All standard
`seq-*` functions take the sequence last, so they compose naturally.

---

## Pattern Recognition

Spot these as threading candidates:

| Nested form                  | `thread-last` stage               |
|------------------------------|-----------------------------------|
| `(seq-filter PRED SEQ)`      | `(seq-filter PRED)`               |
| `(seq-remove PRED SEQ)`      | `(seq-remove PRED)`               |
| `(seq-map FN SEQ)`           | `(seq-map FN)`                    |
| `(seq-keep FN SEQ)`          | `(seq-keep FN)`                   |
| `(seq-mapcat FN SEQ)`        | `(seq-mapcat FN)`                 |
| `(seq-sort PRED SEQ)`        | `(seq-sort PRED)`                 |
| `(seq-sort-by KEY PRED SEQ)` | `(seq-sort-by KEY PRED)`          |
| `(seq-group-by FN SEQ)`      | `(seq-group-by FN)`               |
| `(seq-uniq SEQ)`             | `seq-uniq`                        |
| `(seq-take N SEQ)`           | `(seq-take N)`                    |
| `(seq-drop N SEQ)`           | `(seq-drop N)`                    |
| `(seq-take-while PRED SEQ)`  | `(seq-take-while PRED)`           |
| `(seq-drop-while PRED SEQ)`  | `(seq-drop-while PRED)`           |
| `(nreverse SEQ)`             | `nreverse`                        |
| `(mapcar FN SEQ)`            | replace with `(seq-map FN)` stage |

---

## Migrating from dash.el

Prefer `seq.el` over `dash.el` in this codebase. When you encounter `-`-prefixed
dash functions, replace them with their `seq-*` equivalents before threading.

| dash.el form | seq.el equivalent |
|---|---|
| `(-map FN LIST)` | `(seq-map FN SEQ)` |
| `(-filter PRED LIST)` / `(-select PRED LIST)` | `(seq-filter PRED SEQ)` |
| `(-remove PRED LIST)` / `(-reject PRED LIST)` | `(seq-remove PRED SEQ)` |
| `(-keep FN LIST)` | `(seq-keep FN SEQ)` |
| `(-mapcat FN LIST)` | `(seq-mapcat FN SEQ)` |
| `(-sort PRED LIST)` | `(seq-sort PRED SEQ)` |
| `(-uniq LIST)` | `(seq-uniq SEQ)` |
| `(-take N LIST)` | `(seq-take N SEQ)` |
| `(-drop N LIST)` | `(seq-drop N SEQ)` |
| `(-take-while PRED LIST)` | `(seq-take-while PRED SEQ)` |
| `(-drop-while PRED LIST)` | `(seq-drop-while PRED SEQ)` |
| `(-group-by FN LIST)` | `(seq-group-by FN SEQ)` |
| `(-find PRED LIST)` / `(-first PRED LIST)` | `(seq-find PRED SEQ)` |
| `(-some PRED LIST)` / `(-any PRED LIST)` | `(seq-some PRED SEQ)` |
| `(-every-p PRED LIST)` / `(-all-p PRED LIST)` | `(seq-every-p PRED SEQ)` |
| `(-count PRED LIST)` | `(seq-count PRED SEQ)` |
| `(-reduce-from FN INIT LIST)` | `(seq-reduce FN SEQ INIT)` |
| `(-contains? LIST ITEM)` | `(seq-contains-p SEQ ITEM)` |
| `(-concat LIST1 LIST2)` | `(seq-concatenate 'list SEQ1 SEQ2)` |
| `(--map FORM LIST)` | `(seq-map (lambda (it) FORM) SEQ)` |
| `(--filter FORM LIST)` | `(seq-filter (lambda (it) FORM) SEQ)` |
| `(--remove FORM LIST)` | `(seq-remove (lambda (it) FORM) SEQ)` |

The anaphoric `--map` / `--filter` forms use the implicit variable `it`. When
converting, name the lambda argument explicitly (e.g. `item`, `x`, or a
domain-appropriate name) rather than keeping `it`.

---

## Migrating from Legacy and cl-lib Functions

Replace these with `seq-*` equivalents before or while threading:

| Legacy form | seq.el equivalent |
|---|---|
| `(mapcar FN LIST)` | `(seq-map FN SEQ)` |
| `(mapc FN LIST)` | `(seq-do FN SEQ)` |
| `(mapcan FN LIST)` | `(seq-mapcat FN SEQ)` |
| `(cl-remove-if PRED LIST)` | `(seq-remove PRED SEQ)` |
| `(cl-remove-if-not PRED LIST)` | `(seq-filter PRED SEQ)` |
| `(cl-find-if PRED LIST)` | `(seq-find PRED SEQ)` |
| `(cl-some PRED LIST)` | `(seq-some PRED SEQ)` |
| `(cl-every PRED LIST)` | `(seq-every-p PRED SEQ)` |
| `(cl-count-if PRED LIST)` | `(seq-count PRED SEQ)` |
| `(cl-reduce FN LIST :initial-value INIT)` | `(seq-reduce FN SEQ INIT)` |
| `(cl-mapcan FN LIST)` | `(seq-mapcat FN SEQ)` |
| `(delq nil (mapcar FN LIST))` | `(seq-keep FN SEQ)` |
| `(remove nil (mapcar FN LIST))` | `(seq-keep FN SEQ)` |

`seq-mapcat` flattens one level: `(seq-mapcat FN SEQ)` is equivalent to
`(apply #'append (seq-map FN SEQ))`. Replace the `apply`+`append`+`seq-map`
triple with a single `seq-mapcat` stage.

---

## Examples

### Filter then map

```elisp
;; Before
(seq-map #'format-entry
         (seq-filter #'entry-visible-p entries))

;; After
(thread-last entries
  (seq-filter #'entry-visible-p)
  (seq-map #'format-entry))
```

### Remove, filter, map

```elisp
;; Before
(seq-map #'summarize
         (seq-filter #'recent-p
                     (seq-remove #'archived-p items)))

;; After
(thread-last items
  (seq-remove #'archived-p)
  (seq-filter #'recent-p)
  (seq-map #'summarize))
```

### Intermediate `let` binding holds a collection

```elisp
;; Before
(let ((active (seq-filter #'process-running-p processes)))
  (seq-map #'process-name active))

;; After — eliminate the binding entirely
(thread-last processes
  (seq-filter #'process-running-p)
  (seq-map #'process-name))
```

Only eliminate the binding if it is used exactly once. If it is used in
multiple places, keep the `let`.

### Replace single-arg lambda with `#'`

When a `seq-map` or `seq-filter` stage wraps a lambda that only calls one
function, replace the lambda with a direct function reference:

```elisp
;; Before
(thread-last items
  (seq-filter (lambda (x) (string-prefix-p "foo" x)))
  (seq-map (lambda (x) (upcase x))))

;; After — #' reference where possible
(thread-last items
  (seq-filter (lambda (x) (string-prefix-p "foo" x)))  ; needs closure arg — keep lambda
  (seq-map #'upcase))                                    ; single-arg passthrough — use #'
```

### `mapcar` → `seq-map` in a pipeline

`mapcar` takes the list last, so it slots into `thread-last` as `seq-map` does.
Always convert `mapcar` to `seq-map` when building a pipeline:

```elisp
;; Before
(mapcar #'car (seq-filter #'consp pairs))

;; After
(thread-last pairs
  (seq-filter #'consp)
  (seq-map #'car))
```

---

## When NOT to Use `thread-last`

- **Single operation** — `(seq-filter #'p xs)` alone needs no threading.
- **Wrong argument position** — if a function takes the collection as the
  *first* argument (e.g., `string-join`), use `thread-first` or an explicit
  lambda stage instead of forcing `thread-last`.
- **Intermediate value reused** — if a `let` binding is referenced more than
  once, keep it; threading would recompute.
- **Buffer/point traversal** — `while` loops using `forward-line`, `eobp`,
  etc. have no `seq-*` equivalent; leave them as-is.

---

## How to Apply During Code Review or Editing

1. **Scan** for `seq-map`, `seq-filter`, `seq-remove`, `seq-keep`, `mapcar`
   calls whose argument is another `seq-*` call (nested parens, not a
   variable).
2. **Identify the root collection** — the innermost value that isn't a `seq-*`
   call.
3. **Reverse the nesting order** into a `thread-last` pipeline: outermost
   operation becomes the last stage.
4. **Drop intermediate `let` bindings** that held a single-use collection.
5. **Simplify single-arg lambdas** to `#'` references where the lambda body is
   just a direct call with the bound variable as sole argument.
6. **Verify argument position** for every stage: each partial form must accept
   the threaded value as its last argument.
