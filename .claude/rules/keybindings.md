# Keybinding Conventions

## Single-key rule

Every binding in a keymap must be a **single keypress** — one character, optionally with a
modifier. Never bind multi-character sequences (`"O l"`, `"R d"`) directly in a map.
If a group of related commands needs a shared prefix, create an explicit named sub-map with
`bind-keys :prefix ... :prefix-map`.

## Case and modifier rules

1. **Lowercase letters first.** Bind commands to lowercase single letters.  A capital letter
   requires Shift (two physical keys) and should be avoided.
2. **`C-key` for secondary variants.** When the natural lowercase letter is already taken by a
   more common command, bind the less-common variant to `C-key` in the same map.
3. **No modifier re-introduction after dropping.** If a sub-map is reached via a
   `C-key` or `M-key` prefix, the keys *inside* that sub-map must not require `C-` or `M-`
   themselves (except as noted in rule 2 for rare variants).  The modifier signals entry into
   a level; inside that level keys are plain.
4. **Avoid `M-key` except for truly global, mode-agnostic bindings.** Prefer `C-key` over
   `M-key` for sub-map prefixes.

## Sub-map prefix key selection

- If the natural mnemonic letter is free in the parent map, use it as the prefix key.
- If the natural mnemonic letter is already bound to a command, use `C-letter` as the prefix
  key.  Example: `"o"` bound to `denote-open-or-create` → org sub-map gets prefix `"C-o"`.
- Never use a prefix key that is already bound to a command in the same map; the command
  binding is silently lost.

## Non-alpha single keys

Non-alphabetic single keys (`/`, `;`, `.`) are acceptable for commands that have no obvious
mnemonic letter or that supplement an existing alphabetic binding in the same conceptual slot.

## Checklist before committing a keymap change

- [ ] No key appears more than once in the map (including as both a command and a prefix).
- [ ] All sequences are single-keypress (no bare `"X y"` style bindings).
- [ ] No capital letters used as binding keys.
- [ ] Any `C-key` sub-map prefix has a corresponding `defvar-keymap` or `:prefix-map`.
