# Keybinding Conventions

## Keys

- Every binding is a single keypress: one character, optionally with one modifier.
- Use lowercase letters.
- Use non-alphabetic keys (`/`, `;`, `.`) freely when no mnemonic letter fits.
- Once a modifier prefix (`C-c`, `C-x`, etc.) reaches a map, every key below that point —
  in that map and in any nested sub-map — is plain, with no modifier.
- Reserve `M-key` for global, mode-agnostic bindings that no modifier prefix already guards.

## Multi-command groups

Bind a group of related commands as a named sub-map:

1. Define it with `defvar-keymap` and a `:doc` string naming its parent prefix.
2. Attach it in the parent map with `keymap-set parent-map "key" (cons "name" child-map)`.
3. Bind its own keys with `keymap-set`, plain, no modifier.

## Choosing a key

- Prefer the mnemonic letter (first letter of the command/concept).
- If taken, use a different mnemonic letter (e.g. a synonym's first letter).
- If a plain command needs to grow into a family of related commands, turn its key into a
  sub-map prefix and move the original command inside it.
- Never bind over a key already used in the same map.

## Checklist

- [ ] Every key in the map is a single, unmodified keypress (except the map's own prefix).
- [ ] No key is bound twice, as command or prefix.
- [ ] No capital letters.
- [ ] Every sub-map prefix has a matching `defvar-keymap`.
