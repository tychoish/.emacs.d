---
scope:
  paths:
    - lisp/
    - test/
  repositories:
    - xtdlib
    - annotated-completing-read
    - agent-shell-menu
    - agent-shell-queue
---

# Emacs Lisp Coding Rules

Rules for writing and editing Emacs Lisp in this configuration.

## Style

### Elisp formatting: no alignment padding

Never use extra spaces to align let-binding RHS values or any other
elements. Write `(table (make-hash-table :test #'equal))` not `(table
(make-hash-table :test #'equal))`. One space after the symbol name,
always. Applies to `let`/`let*`, `plist`, `alist`, and all
multi-element forms.

## Form

### cl-lib usage

ONLY use `cl-defmacro`, `cl-defun`, `cl-defstruct`, `cl-defgeneric`, and `cl-defmethod`
from cl-lib. Do **not** use `cl-flet`, `cl-labels`, `cl-loop`, `cl-some`, `cl-every`,
`cl-find-if`, `cl-remove-if`, `cl-remove-if-not`, `cl-reduce`, or any other CL iteration
or collection function.

### List Operations and Iteration

Use `seq.el` equivalents instead of cl-lib or raw `mapcar`:

| Avoid                                    | Use instead                        |
|------------------------------------------|------------------------------------|
| `(mapcar (lambda ...) list)`             | `(seq-map (lambda ...) list)`      |
| `(cl-remove-if PRED list)`               | `(seq-remove PRED list)`           |
| `(cl-remove-if-not PRED list)`           | `(seq-filter PRED list)`           |
| `(cl-find-if PRED list)`                 | `(seq-find PRED list)`             |
| `(cl-some PRED list)`                    | `(seq-some PRED list)`             |
| `(cl-every PRED list)`                   | `(seq-every-p PRED list)`          |
| `(cl-reduce f list :initial-value init)` | `(seq-reduce f list init)`         |
| `(dolist (it list) BODY)`                | `(seq-do (lambda (it) BODY) list)` |
| `(dolist (it list) (push (f it) acc))`   | `(thread-last list (seq-map #'f))` |

#### Prefer `seq-do` over `dolist`

Never use `dolist` for side-effectful iteration over a list. Use `seq-do` instead.
When a `dolist` accumulates results via `push`, replace the whole pattern with
`thread-last` + `seq-map`/`seq-filter` instead.

The `while` form is acceptable only for buffer-traversal loops that use point-based
operations (`forward-line`, `eobp`, etc.) — these have no `seq.el` equivalent.

#### Threading macros over nesting

Use `thread-last` from `subr-x` to express a series of transformations on the same value
instead of nesting function calls.

### Let bindings

Avoid `let` bindings for variables used only once — inline the expression at the use
site instead. Single-use bindings add visual indirection without clarity benefit.
Exceptions: the expression is complex enough that naming it aids readability, or it
appears in a `when-let*`/`if-let*` chain where the binding is also the nil-guard.

### Conditionals and Bindings

Use `when-let*` and `if-let*` to flatten binding+condition chains. Avoid
`(let ((x (f))) (when x ...))` nesting.

Use `if-let*` when there is an else branch, `when-let*` when there is none.

Never use the deprecated `if-let` or `when-let` (without the `*`); always use `if-let*`
and `when-let*`.

### Deprecated functions

Always use current, non-deprecated standard library functions. When editing a file,
replace any deprecated calls encountered — do not leave them in place. Common
substitutions:

| Deprecated | Use instead |
|------------|-------------|
| `when-let` | `when-let*` |
| `if-let`   | `if-let*`   |

### Hooks

NEVER add lambdas to hooks — always use named functions.

### Lambdas

Never write a lambda that only calls one function and could be replaced by a function
reference. Pass the function directly with `#'`.

### Conditionals

#### Use `when`/`unless` for single-branch conditions

Never use `(if COND BODY)` with no else branch. Use `when` or `unless` instead.

#### Use `cond` over nested `if`

Avoid nesting `if` forms. Use `cond` when there are two or more branches.

#### Always break `if` across at least three lines

Never write a one-line `if`. The condition, then-branch, and else-branch each get their
own line.

### Naming Conventions

#### Function and variable prefixes

| Prefix                 | Meaning                                                                 |
|------------------------|-------------------------------------------------------------------------|
| `tychoish--`           | Private implementation details; do not call from other files            |
| `tychoish-`            | Public non-interactive functions (utilities, accessors)                 |
| `ad:`                  | Advice functions (`:around`, `:before`, `:after` targets)               |
| `cli/`                 | Command-line argument handlers registered with `command-line-functions` |

Never define `tychoish/` functions in `xlib.el`. `xlib.el` is a pure utility library with no dependency on the config's domain logic.

#### Predicates

End predicate functions with `-p`: `gui-p`, `should-read-abbrev-file-p`.

#### Toggle generators

Use `create-toggle-functions` from `xlib.el` to generate `turn-on-X`, `turn-off-X`, and
`toggle-X` triads. Do not write these by hand.

#### Single-use internal functions

Never write a private (`--`) function that is called from exactly one place. Inline the
body at the call site instead. Exceptions:
- Functions passed as values: timer callbacks (`run-with-idle-timer`, `run-with-timer`),
  advice (`advice-add`), subscription/hook registrations, dispatch table entries.
- Functions covered by direct ERT tests.


### Macros

#### Write macros only for genuine repetition

A macro is appropriate when the same structural pattern appears three or more times and
cannot be expressed as a higher-order function. Prefer functions with `funcall` over
macros when the body is just a value transformation.

#### Macro hygiene

- Use `cl-defmacro` when the macro has keyword arguments.
- Use `declare (indent N)` for macros with a body argument so indentation works correctly.
- Prefer `gensym` / `make-symbol` over manual name mangling to avoid variable capture.
- Never call functions with observable side effects inside a macro's expansion-time code
  (the non-backquoted parts). Side effects at expansion time run during `load`, not when
  the macro form is evaluated at runtime.

#### `with-*` macros

Macros that temporarily modify a dynamic variable must restore it with `let`, not `setq`.
Always use `prog1` or `unwind-protect` to return the body's value.
