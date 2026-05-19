# Elisp Coding Rules

Rules for writing and editing Emacs Lisp in this configuration.

---

## cl-lib usage

ONLY use `cl-defmacro`, `cl-defun`, `cl-defstruct`, `cl-defgeneric`, and `cl-defmethod`
from cl-lib. Do **not** use `cl-flet`, `cl-labels`, `cl-loop`, `cl-some`, `cl-every`,
`cl-find-if`, `cl-remove-if`, `cl-remove-if-not`, `cl-reduce`, or any other CL iteration
or collection function.

---

## List Operations and Iteration

Use dash.el equivalents instead of cl-lib or raw `mapcar`:

| Avoid | Use instead |
|-------|-------------|
| `(mapcar (lambda ...) list)` | `(-map (lambda ...) list)` or `(--map EXPR list)` |
| `(cl-remove-if PRED list)` | `(-remove PRED list)` or `(--remove EXPR list)` |
| `(cl-remove-if-not PRED list)` | `(-filter PRED list)` or `(--filter EXPR list)` |
| `(cl-find-if PRED list)` | `(-first PRED list)` or `(--first EXPR list)` |
| `(cl-some PRED list)` | `(-any? PRED list)` or `(--any? EXPR list)` |
| `(cl-every PRED list)` | `(-all? PRED list)` or `(--all? EXPR list)` |
| `(cl-reduce (lambda (acc x) (+ acc (f x))) list :initial-value 0)` | `(--sum (f it) list)` |
| `(dolist (x list) BODY)` | `(-each list (lambda (x) BODY))` or `(--each list BODY)` |

Use anaphoric variants (`--map`, `--filter`, `--each`, `--sum`, etc.) when the body is a
simple expression referring to the element as `it`. Use the lambda form when the body is
multi-line or captures outer variables by name.

### Threading macros over nesting

Use `->` and `->>` from dash.el to express a series of transformations on the same value
instead of nesting function calls:

```elisp
;; Avoid:
(mapcar #'f (cl-remove-if #'pred (mapcar #'g list)))

;; Prefer:
(->> list (-map #'g) (-remove #'pred) (-map #'f))
```

---

## Let bindings

Avoid `let` bindings for variables used only once — inline the expression at the use
site instead. Single-use bindings add visual indirection without clarity benefit.
Exceptions: the expression is complex enough that naming it aids readability, or it
appears in a `when-let*`/`if-let*` chain where the binding is also the nil-guard.

```elisp
;; Avoid:
(let ((result (process-item item)))
  (insert result))

;; Prefer:
(insert (process-item item))
```

---

## Conditionals and Bindings

Use `when-let*` and `if-let*` to flatten binding+condition chains. Avoid
`(let ((x (f))) (when x ...))` nesting:

```elisp
;; Avoid:
(let ((pair (assoc id alist)))
  (when pair
    (let ((item (cdr pair)))
      (when (eq (item-status item) 'active)
        ...))))

;; Prefer:
(when-let* ((pair (assoc id alist))
             (item (cdr pair))
             ((eq (item-status item) 'active)))
  ...)
```

Use `if-let*` when there is an else branch, `when-let*` when there is none.

Never use the deprecated `if-let` or `when-let` (without the `*`); always use `if-let*`
and `when-let*`.

---

## Deprecated functions

Always use current, non-deprecated standard library functions. When editing a file,
replace any deprecated calls encountered — do not leave them in place. Common
substitutions:

| Deprecated | Use instead |
|------------|-------------|
| `when-let` | `when-let*` |
| `if-let` | `if-let*` |

---

## Hooks

NEVER add lambdas to hooks — always use named functions.

---

## Lambdas

Never write a lambda that only calls one function and could be replaced by a function
reference. Pass the function directly with `#'`:

```elisp
;; Avoid:
(-map (lambda (x) (upcase x)) list)
(add-hook 'some-hook (lambda () (my-fn)))

;; Prefer:
(-map #'upcase list)
(add-hook 'some-hook #'my-fn)
```

---

## Conditionals

### Use `when`/`unless` for single-branch conditions

Never use `(if COND BODY)` with no else branch. Use `when` or `unless` instead:

```elisp
;; Avoid:
(if condition
  (do-something))

;; Prefer:
(when condition
  (do-something))
```

### Use `cond` over nested `if`

Avoid nesting `if` forms. Use `cond` when there are two or more branches:

```elisp
;; Avoid:
(if (eq x 'a)
  (handle-a)
  (if (eq x 'b)
    (handle-b)
    (handle-default)))

;; Prefer:
(cond
  ((eq x 'a) (handle-a))
  ((eq x 'b) (handle-b))
  (t (handle-default)))
```

### Always break `if` across at least three lines

Never write a one-line `if`. The condition, then-branch, and else-branch each get their
own line:

```elisp
;; Avoid:
(if condition (then-expr) (else-expr))

;; Prefer:
(if condition
  (then-expr)
  (else-expr))
```

---

## Naming Conventions

### Function and variable prefixes

| Prefix                 | Meaning                                                                 |
|------------------------|-------------------------------------------------------------------------|
| `tychoish/`            | Public interactive commands and configuration functions                 |
| `tychoish--`           | Private implementation details; do not call from other files            |
| `tychoish-`            | Public non-interactive functions (utilities, accessors)                 |
| `ad:`                  | Advice functions (`:around`, `:before`, `:after` targets)               |
| `cli/`                 | Command-line argument handlers registered with `command-line-functions` |
| `f-`, `s-`, `-`, `ht-` | Extensions to the f/s/dash/ht libraries in `xlib.el`                    |

Never define `tychoish/` functions in `xlib.el`. `xlib.el` is a pure utility library with no dependency on the config's domain logic.

### Predicates

End predicate functions with `-p`: `gui-p`, `should-read-abbrev-file-p`.

### Toggle generators

Use `create-toggle-functions` from `xlib.el` to generate `turn-on-X`, `turn-off-X`, and
`toggle-X` triads. Do not write these by hand.

### Single-use internal functions

Never write a private (`--`) function that is called from exactly one place. Inline the
body at the call site instead. Exceptions:
- Functions passed as values: timer callbacks (`run-with-idle-timer`, `run-with-timer`),
  advice (`advice-add`), subscription/hook registrations, dispatch table entries.
- Functions covered by direct ERT tests.

---

## Macros

### Write macros only for genuine repetition

A macro is appropriate when the same structural pattern appears three or more times and
cannot be expressed as a higher-order function. Prefer functions with `funcall` over
macros when the body is just a value transformation.

### Macro hygiene

- Use `cl-defmacro` when the macro has keyword arguments.
- Use `declare (indent N)` for macros with a body argument so indentation works correctly.
- Prefer `gensym` / `make-symbol` over manual name mangling to avoid variable capture.
- Never call functions with observable side effects inside a macro's expansion-time code
  (the non-backquoted parts). Side effects at expansion time run during `load`, not when
  the macro form is evaluated at runtime.

### `with-*` macros

Macros that temporarily modify a dynamic variable must restore it with `let`, not `setq`.
Always use `prog1` or `unwind-protect` to return the body's value:

```elisp
;; Correct:
(defmacro with-foo (value &rest body)
  `(let ((foo ,value))
     ,@body))

;; Wrong (leaks):
(defmacro with-foo (value &rest body)
  `(progn (setq foo ,value) ,@body (setq foo nil)))
```
