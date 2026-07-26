# Changelog

## 1.0.0 — 2026-07-26

Full rewrite of the internals. The documented user API
(`auto-modal-bind-key`, `auto-modal-unbind-key`,
`auto-modal-unbind-with-predicate`, `auto-modal-enable-insert`, the cursor
options, `auto-modal-mode` / `global-auto-modal-mode`) is unchanged.

### Fixed

- **The package now byte-compiles and loads in `emacs -Q`.** Previously
  byte-compilation aborted (`auto-modal-key-bind` was a macro calling
  `auto-modal-has-key-p` at expansion time, before it was defined) and
  loading failed with `void-function cl-defstruct` because no `require`
  forms were present.
- **Key dispatch no longer errors in buffers without a local keymap**
  (`lookup-key` was called on `nil`).
- **Turning `auto-modal-mode` off now really removes its hooks.**
  `add-hook`/`remove-hook` were called with mismatched positional flags
  (`t` meant *append* on add but *local* on remove), so global hooks
  leaked forever; theme advice was also removed while other buffers still
  used the mode. Hooks are now installed with the first buffer that
  enables the mode and removed with the last.
- **`override-p` is honored per binding**, not per key. Previously the
  first registered binding of a key determined the override behavior of
  every other binding of the same key.
- **Invalid cursor-color values signal the intended error.** The old
  `cond` used a pcase-style `_` wildcard, which is evaluated as a (void)
  variable in `cond`.
- **A failed `auto-modal-bind-key` no longer leaves the key bound** with
  no data attached (validation now runs before any mutation).
- **The dispatch log is trimmed reliably** (the old code only trimmed when
  the line count was *exactly* the maximum).
- **Bindings to lazily-loaded commands work.** Registering a not yet
  defined function (e.g. `avy-goto-line` before avy loads) no longer
  signals during setup; dispatching to a still-undefined function gives a
  clear error instead. Undefined predicate symbols are treated as
  unsatisfied rather than erroring.
- **Key names are canonicalized at registration** (via
  `key-description`), so alternative kbd spellings of the same key —
  `"C-i"`/`"TAB"`, `"C-m"`/`"RET"` — resolve to the same binding, and an
  invalid key string is rejected up front instead of corrupting state.
- **A predicate that signals an error is contained**: it is treated as
  unsatisfied (reported once) instead of knocking auto-modal's function
  off the global `post-command-hook` and silently disabling the package
  everywhere.
- **`auto-modal-unbind-key` actually removes bindings.** The old version
  always signaled an error before doing anything (it validated a variable
  that had not been extracted yet).
- **Killing a buffer with the mode enabled cleans up**: the global hooks
  are removed together with the last live auto-modal buffer.
- **Theme switching no longer captures auto-modal's own cursor color as
  the theme default** when the new theme does not set a cursor color.
- **`auto-modal-next-line` / `auto-modal-previous-line` (example config)
  no longer recurse to the stack limit** at buffer boundaries (e.g.
  previous-line below leading empty lines at the beginning of the
  buffer).
- **Entries of `auto-modal-vi-keybinds` with arguments work**
  (`("z" forward-line 2)`); previously the argument was misparsed as the
  override flag.
- **Enabling the mode no longer restyles unrelated buffers.** The old
  window-configuration hook applied the *insert* cursor to every window's
  buffer, including buffers that never enabled the mode; the frame cursor
  color is now restored when a non-auto-modal buffer is selected.
- The one-shot insert escape (`auto-modal-enable-insert`) is consumed at
  the start of the next command instead of in the same command's
  `post-command-hook`, which makes it robust against state re-computation.

### Changed

- `suppress-key-mode` → `auto-modal-control-mode` (internal, managed
  automatically; the keymap is `auto-modal-control-mode-map`).
- All keys dispatch through a single named command,
  `auto-modal-dispatch`, instead of one generated closure per key; the
  `bind-key` (use-package) dependency is gone.
- The modal state is computed once per command from a single
  `post-command-hook` function (the old code evaluated every predicate up
  to four times per command); major mode inheritance distances are cached.
- Theme changes are tracked with `enable-theme-functions` /
  `disable-theme-functions` (Emacs 29) instead of advising `load-theme`
  and `counsel-load-theme`.
- `global-auto-modal-mode` is a `define-globalized-minor-mode` and skips
  minibuffers and internal buffers.
- `auto-modal-is-triggerp` → `auto-modal-triggered-p`;
  `auto-modal-vi-mode-toogle` → `auto-modal-vi-mode-toggle` (obsolete
  aliases kept for both).
- `sexp-*` functions in the example configuration are namespaced as
  `auto-modal-sexp-*`.
- `auto-modal-enable-insert` is a proper interactive command.
- In control state, a key whose predicates are all unsatisfied now falls
  back to the full `key-binding` lookup (honoring other minor modes and
  command remapping) instead of only local map then global map; the
  example config registers the region bindings after the bol bindings so
  a region operation wins when both predicates hold; the dispatch log
  appends entries (newest last) in `%S` format.
- `auto-modal-help-key`, `auto-modal-enable-log`,
  `auto-modal-enable-keyhint` are `defcustom`s; every option has `:type`
  and belongs to the new `auto-modal` customization group.
- `readme.md` / `readme_zh.md` → `README.md` / `README_zh.md`, both fully
  rewritten with an API reference, an example-configuration manual and a
  migration guide.

### Added

- `auto-modal-unbind` — remove bindings by key, optionally filtered by
  mode and predicate, without repeating the full registration arguments.
- `auto-modal-list-keybinds` — a tabulated listing of every registered
  binding.
- An ERT test suite (`auto-modal-tests.el`, 44 tests), a `Makefile`
  (`compile` / `checkdoc` / `test`), and GitHub Actions CI on Emacs 29.4
  and 30.1.

### Removed

The following undocumented internals no longer exist: `major-mode-chain`,
`major-mode-derived-p`, `background-mode-change` (function, hook, setup,
unset), `auto-modal-functions-data`, `auto-modal-trigger-functions`,
`auto-modal-key-command`,
`auto-modal-has-key-p`, `auto-modal-key-override-status`,
`auto-modal-original-command`, `auto-modal-switch-to-insert`,
`auto-modal-switch-to-control`, `auto-modal-bind-keyhint`,
`auto-modal-unbind-keyhint`, `auto-modal-log-num`,
`auto-modal-pre-is-control-p`, `auto-modal-set-cursor-when-idle` (the
example config no longer adds a global `post-command-hook` at load time).

## 0.0.1

Initial version.
