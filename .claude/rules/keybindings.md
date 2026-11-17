# Keybinding Conventions

## Keys

- Every binding is a single keypress: one character, optionally with one modifier.
- Use lowercase letters.
- Use non-alphabetic keys (`/`, `;`, `.`) freely when no mnemonic letter fits.
- Modifiers may appear only at the start of a key sequence, before the first plain key:
  `C-c C-t r` is fine; `C-c t C-r` is not. Once a plain key has been pressed, every key
  after it — in that map and in any nested sub-map — is plain, with no modifier. A map
  mounted on a plain-key prefix (e.g. `C-c o`) therefore holds only plain keys.
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

- [ ] No modifier follows a plain key in any sequence (`C-c C-t r` ok, `C-c t C-r` not).
- [ ] No key is bound twice, as command or prefix.
- [ ] No capital letters.
- [ ] Every sub-map prefix has a matching `defvar-keymap`.
