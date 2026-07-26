# auto-modal

[![CI](https://github.com/Kinneyzhang/auto-modal/actions/workflows/test.yml/badge.svg)](https://github.com/Kinneyzhang/auto-modal/actions/workflows/test.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

*Modal editing without mode switching: single-letter commands that appear
exactly where you would never type text.*

[中文文档](README_zh.md)

---

In Emacs, frequently used commands are usually bound to key sequences with
prefix keys such as `C-c` and `C-x`. That is not as fast as the
single-letter keys of modal editing in the vi tradition, which is why many
people use `evil-mode` or `meow`. But modal editing has a cost of its own:
you constantly have to *switch modes by hand*.

**auto-modal** takes a third path: the editing state is *derived from where
point is*. You register keybindings consisting of a key, a major mode, a
predicate function and a command. Whenever point satisfies a predicate —
say, at the beginning of a non-empty line, or right next to a parenthesis
in Lisp code — the buffer automatically enters **control state** and your
single-letter keys run commands. Everywhere else the buffer stays in
**insert state** and behaves exactly like ordinary Emacs. The cursor shape
and color follow the state, so you always know where you are. You never
switch modes; you just move point, and the mode follows.

## Table of contents

- [Requirements](#requirements)
- [Installation](#installation)
- [Quick start](#quick-start)
- [Concepts](#concepts)
  - [Control state and insert state](#control-state-and-insert-state)
  - [Trigger predicates](#trigger-predicates)
  - [Major mode inheritance](#major-mode-inheritance)
  - [Key resolution and precedence](#key-resolution-and-precedence)
  - [Typing at a trigger position](#typing-at-a-trigger-position)
- [API reference](#api-reference)
  - [Registering keybindings](#registering-keybindings)
  - [Removing keybindings](#removing-keybindings)
  - [Minor modes and commands](#minor-modes-and-commands)
  - [Customization options](#customization-options)
  - [Hooks](#hooks)
- [The help key](#the-help-key)
- [Logging](#logging)
- [The example configuration](#the-example-configuration)
  - [Beginning-of-line navigation](#beginning-of-line-navigation)
  - [Region bindings](#region-bindings)
  - [S-expression navigation](#s-expression-navigation)
  - [A minimal vi emulation](#a-minimal-vi-emulation)
- [Writing your own predicates](#writing-your-own-predicates)
- [Performance notes](#performance-notes)
- [Migrating from 0.x](#migrating-from-0x)
- [Development](#development)
- [License](#license)

## Requirements

- Emacs **29.1** or later.
- No external packages required. (Two bindings in the optional example
  configuration reference `avy` commands; without avy they register fine
  and signal a clear "not defined" error if pressed.)

## Installation

Install auto-modal directly from the repository.

**With `use-package` and `:vc` (Emacs 30+):**

```emacs-lisp
(use-package auto-modal
  :vc (:url "https://github.com/Kinneyzhang/auto-modal" :rev :newest))
```

**With `straight.el`:**

```emacs-lisp
(straight-use-package
 '(auto-modal :type git :host github :repo "Kinneyzhang/auto-modal"))
```

**Manually:**

```emacs-lisp
;; git clone https://github.com/Kinneyzhang/auto-modal ~/path/to/auto-modal
(add-to-list 'load-path "~/path/to/auto-modal")
(require 'auto-modal)
```

## Quick start

```emacs-lisp
(require 'auto-modal)

;; A predicate: point at the beginning of a non-empty line.
(defun my/bolp ()
  (and (bolp) (not (looking-at-p "^$"))))

;; When the predicate holds, "j" and "k" move between lines
;; and "SPC" lets you type at that position.
(auto-modal-bind-key "j" 'global 'my/bolp 'next-line)
(auto-modal-bind-key "k" 'global 'my/bolp 'previous-line)
(auto-modal-bind-key "SPC" 'global 'my/bolp 'auto-modal-enable-insert)

;; Turn it on everywhere.
(global-auto-modal-mode 1)
```

Now move point to the beginning of any non-empty line: the cursor becomes a
box — control state. Press `j`/`k` to move by lines, `?` to see every key
that currently does something. Move point anywhere else (or press `SPC` and
start typing) and the buffer is plain Emacs again.

Prefer a ready-made setup? Load the bundled example configuration and skip
ahead to [its manual](#the-example-configuration):

```emacs-lisp
(require 'auto-modal-config)
(global-auto-modal-mode 1)
```

## Concepts

### Control state and insert state

With `auto-modal-mode` enabled, a buffer is always in one of two states:

| State | When | Keys | Default cursor |
|---|---|---|---|
| **Control** | point satisfies at least one registered predicate | registered keys run commands; other self-inserting keys are suppressed | `box` |
| **Insert** | everywhere else | ordinary Emacs | `bar` |

The state is re-computed after every command. There is nothing to toggle by
hand (although you can: see
[Typing at a trigger position](#typing-at-a-trigger-position)).

Non-letter keys (`RET`, `C-…`, `M-…`, function keys …) are never touched:
in control state only *self-inserting* keys are suppressed, and only the
keys you registered run auto-modal commands.

### Trigger predicates

A predicate is a function of no arguments, called with point at the
position to test. It should be cheap and side-effect free — predicates run
from `post-command-hook`. The predicate `t` means "always" and turns
auto-modal into classic hand-switched modal editing (see
[the vi emulation](#a-minimal-vi-emulation)).

Good trigger positions are places where you would rarely type text:

- the beginning of a non-empty line (`(and (bolp) (not (looking-at-p "^$")))`),
- next to a parenthesis in Lisp code,
- on an active region (`use-region-p`),
- on an org heading star, a Markdown `#`, a dired line…

### Major mode inheritance

Each binding names a major mode. It applies in that mode *and every mode
derived from it*. The special symbol `global` (an alias for
`fundamental-mode`) applies everywhere.

```emacs-lisp
(auto-modal-bind-key "j" 'global           'my/bolp 'next-line)
(auto-modal-bind-key "j" 'prog-mode       'my/bolp 'my/next-defun)
(auto-modal-bind-key "j" 'emacs-lisp-mode 'my/bolp 'my/next-sexp)
```

With all three registered, pressing `j` at the beginning of a line runs
`my/next-sexp` in `emacs-lisp-mode` buffers (closest mode wins),
`my/next-defun` in every other programming mode, and `next-line` everywhere
else.

### Key resolution and precedence

When you press a registered key in control state, auto-modal picks the
binding to run:

1. Among all bindings for that key whose mode applies to the buffer **and**
   whose predicate is satisfied, the one whose mode is *closest* to the
   current major mode wins. On a tie, the most recently added one wins.
2. If the winning binding was registered without `override-p` and the major
   mode's own keymap binds the key, the major mode's command runs instead.
   This keeps special modes like dired or magit intact: their letter keys
   keep doing what they always did. Pass a non-nil fifth argument
   (`override-p`) to `auto-modal-bind-key` to win over the major mode.
3. If **no** binding's predicate is satisfied (the buffer is in control
   state because of some *other* key's predicate), the key does whatever it
   would do without auto-modal — usually self-insert.

### Typing at a trigger position

Sometimes you do need to type text exactly at a trigger position. The
command `auto-modal-enable-insert` switches to insert state for one
command: bind it to a key (`SPC` works well), press it, and type away. The
escape hatch is consumed at your next keystroke; as soon as point stops
satisfying the predicates the buffer returns to automatic switching — in
practice you type, move on, and never think about it.

## API reference

### Registering keybindings

```emacs-lisp
(auto-modal-bind-key KEY-NAME MODE PREDICATE FUNCTION-ARGS &optional OVERRIDE-P)
```

| Argument | Meaning |
|---|---|
| `KEY-NAME` | a key string in `kbd` format: `"j"`, `"SPC"`, `"<"` … |
| `MODE` | a major mode symbol, or `global` for every buffer |
| `PREDICATE` | a nullary function, or `t` for "always" |
| `FUNCTION-ARGS` | a function — called interactively when it is a command — or a list `(FUNCTION ARGS...)`, applied as `(apply FUNCTION ARGS)` |
| `OVERRIDE-P` | non-nil: win over the major mode's own binding of the key |

```emacs-lisp
(auto-modal-bind-key "o" 'global 'my/bolp '(other-window 1))   ; with arguments
(auto-modal-bind-key "j" 'dired-mode t 'dired-next-line t)     ; override dired's own "j"
```

`PREDICATE` and `FUNCTION` may be symbols that are not defined yet, so you
can bind commands of packages that load lazily; an unsatisfied (undefined)
predicate simply never triggers, and dispatching to an undefined function
signals a clear error.

The key named by `auto-modal-help-key` (`?` by default) is reserved and
cannot be registered.

### Removing keybindings

| Function | Behavior |
|---|---|
| `(auto-modal-unbind-key KEY MODE PRED FUNCTION-ARGS &optional OVERRIDE-P)` | removes the binding registered with *exactly* these arguments |
| `(auto-modal-unbind KEY &optional MODE PREDICATE)` | removes every binding of `KEY`, optionally restricted to a mode and/or predicate; also a command |
| `(auto-modal-unbind-with-predicate PREDICATE)` | removes every binding using `PREDICATE` |

### Minor modes and commands

| Symbol | Kind | Description |
|---|---|---|
| `auto-modal-mode` | minor mode | automatic modal switching in the current buffer |
| `global-auto-modal-mode` | global minor mode | enables `auto-modal-mode` in every buffer (except minibuffers and internal buffers) |
| `auto-modal-list-keybinds` | command | list all registered bindings in a table |
| `auto-modal-keyhint-show` | command | show the keys applicable at point in the echo area |
| `auto-modal-enable-insert` | command | one-shot switch to insert state (bind it to a key) |
| `auto-modal-triggered-p` | function | non-nil when point satisfies some predicate |
| `auto-modal-set-cursor` | command | recompute state and cursor of the current buffer |
| `auto-modal-all-keys` | function | all registered key names |
| `auto-modal-data` | variable | the list of `auto-modal-keybind` structs |

### Customization options

`M-x customize-group RET auto-modal RET`, or set from Lisp:

| Option | Default | Meaning |
|---|---|---|
| `auto-modal-control-cursor-type` | `box` | `cursor-type` in control state; `default` = your default cursor |
| `auto-modal-insert-cursor-type` | `bar` | `cursor-type` in insert state |
| `auto-modal-control-cursor-color` | `nil` | cursor color in control state, see below |
| `auto-modal-insert-cursor-color` | `nil` | cursor color in insert state |
| `auto-modal-help-key` | `"?"` | reserved key showing the keyhint |
| `auto-modal-enable-keyhint` | `nil` | show the keyhint automatically on entering control state |
| `auto-modal-enable-log` | `nil` | log dispatched keys to `*Auto-modal-log*` |
| `auto-modal-log-max-number` | `40` | log entries kept |

The two color options accept four forms:

```emacs-lisp
nil                       ; the theme's default cursor color
'error                    ; a face: use its foreground color
"#ff6c6b"                 ; a color string
'("#005f87" . "#87d7ff")  ; (LIGHT . DARK), picked by background
```

The `(LIGHT . DARK)` cons is re-evaluated when the theme changes, so one
setting works for both light and dark themes.

### Hooks

| Hook | Run |
|---|---|
| `auto-modal-turn-on-hook` | after `auto-modal-mode` is enabled in a buffer |
| `auto-modal-turn-off-hook` | after it is disabled |

## The help key

The whole point of auto-modal is that active keys depend on where point is
— so discoverability matters. In control state, press `?`
(`auto-modal-help-key`) to see every key applicable at point:

```
SPC → auto-modal-enable-insert  j → auto-modal-next-line  k → auto-modal-previous-line  …
```

Set `auto-modal-enable-keyhint` to `t` to show this automatically every
time a buffer enters control state.

## Logging

Set `auto-modal-enable-log` to `t` and every dispatched key with its
command is recorded in the `*Auto-modal-log*` buffer (trimmed to
`auto-modal-log-max-number` entries). Useful to understand which binding
won when several could apply.

## The example configuration

`auto-modal-config.el` ships a complete working setup — the author's own.
Load it as a starting point, or copy the parts you like:

```emacs-lisp
(require 'auto-modal-config)
(global-auto-modal-mode 1)
```

### Beginning-of-line navigation

Predicate: `auto-modal-bolp` — point at the beginning of a non-empty line.

| Key | Command |
|---|---|
| `j` / `k` | next / previous non-empty line (`auto-modal-next-line` / `auto-modal-previous-line`) |
| `l` | `avy-goto-line` |
| `c` | `avy-goto-char-timer` |
| `o` | other window |
| `v` | `set-mark-command` |
| `<` / `>` | backward / forward page |
| `z` | `read-only-mode` |
| `SPC` | type here (one-shot insert) |

### Region bindings

Predicate: `use-region-p` — any active region, anywhere in the line.

| Key | Command |
|---|---|
| `u` | `upcase-dwim` |
| `d` | `downcase-dwim` |
| `c` | `kill-ring-save` |

Note how `c` is bound twice — once for regions, once for beginnings of
lines. Predicates disambiguate: whichever one is satisfied wins. When
both are satisfied at once (region active with point at bol), the
region binding wins because it is registered later — ties go to the
most recently added binding.

### S-expression navigation

Predicate: `auto-modal-sexp-around-paren-p` — point next to a paren in
`emacs-lisp-mode` (parens inside strings and comments don't count).

| Key | Command | Moves |
|---|---|---|
| `f` / `b` | `auto-modal-sexp-forward` / `-backward` | next/previous paren of the same side |
| `j` / `k` | `auto-modal-sexp-down` / `-up` | next/previous paren at the same depth |
| `i` / `o` | `auto-modal-sexp-into` / `-outside` | one level deeper / out to the enclosing expression |
| `s` | `auto-modal-sexp-balance` | the balancing paren |
| `n` | `auto-modal-sexp-newline-paren` | insert `()` on a new line, point inside |
| `;` | `auto-modal-sexp-comment` | comment the expression out |
| `SPC` | type here (one-shot insert) | |

### A minimal vi emulation

With the predicate `t`, auto-modal degenerates into classic modal editing —
here is vi in thirty lines. `auto-modal-vi-mode` gives you `h/j/k/l/w/b`
in every buffer that has `auto-modal-mode` enabled (enable
`global-auto-modal-mode` to get them everywhere), `i` to insert,
`<escape>` to toggle back:

```emacs-lisp
(auto-modal-vi-mode 1)
```

The key list lives in `auto-modal-vi-keybinds`; extend it before enabling
the mode. An entry may carry arguments: `("z" forward-line 2)`. Note that
on terminal frames `ESC` acts as a prefix key, so bind a different toggle
key there.

## Writing your own predicates

A predicate runs after every command, so keep it cheap and read-only.
Some ideas:

```emacs-lisp
;; On the stars of an org heading.
(defun my/org-headp ()
  (and (org-at-heading-p) (< (point) (+ (line-beginning-position)
                                        (org-current-level)))))
(auto-modal-bind-key "n" 'org-mode 'my/org-headp 'org-next-visible-heading)
(auto-modal-bind-key "p" 'org-mode 'my/org-headp 'org-previous-visible-heading)

;; At the end of a line, one key to jump to end of buffer.
(auto-modal-bind-key "e" 'global 'eolp 'end-of-buffer)

;; In an empty line of a programming buffer, quick comment.
(defun my/empty-linep () (looking-at-p "^$"))
(auto-modal-bind-key ";" 'prog-mode 'my/empty-linep 'comment-dwim)
```

Rules of thumb:

- Prefer *positions where typing is rare*; that is what makes the automatic
  switching invisible.
- Return non-nil fast; you can rely on `syntax-ppss` for syntax context.
- The same key with different predicates is fine — see `c` above.

## Performance notes

The implementation was written to keep the per-keystroke overhead small:

- Trigger state is computed **once per command** from a single
  `post-command-hook` function, short-circuiting at the first satisfied
  predicate.
- Major mode inheritance distances are cached in a hash table.
- The global hooks are installed while at least one buffer has
  `auto-modal-mode` enabled, and removed with the last one. In buffers
  without the mode the hook exits after one buffer-local check.

The practical cost is your predicates. If one of them is expensive, scope
it to the narrowest major mode possible so other buffers never run it.

## Migrating from 0.x

Version 1.0.0 is a full rewrite of the internals; the documented API is
unchanged, but some internal symbols are gone or renamed. Configurations
that only used `auto-modal-bind-key` / `auto-modal-unbind-key` /
`auto-modal-unbind-with-predicate` and the cursor options work unchanged.

| Old | New |
|---|---|
| `suppress-key-mode`, `suppress-key-mode-map` | `auto-modal-control-mode`, `auto-modal-control-mode-map` (managed automatically) |
| `auto-modal-is-triggerp` | `auto-modal-triggered-p` (obsolete alias kept) |
| `auto-modal-mode-turn-on` | internal (obsolete alias kept) |
| `sexp-forward`, `sexp-down`, … (config) | `auto-modal-sexp-forward`, `auto-modal-sexp-down`, … |
| `auto-modal-vi-mode-toogle` (config) | `auto-modal-vi-mode-toggle` (obsolete alias kept) |
| `major-mode-chain`, `major-mode-derived-p`, `background-mode-change-*`, `auto-modal-functions-data`, `auto-modal-trigger-functions`, `auto-modal-key-command`, `auto-modal-original-command`, `auto-modal-switch-to-insert/control` | removed (internal) |
| depends on `bind-key` | no external dependencies |

See [CHANGELOG.md](CHANGELOG.md) for the complete list, including the bug
fixes that motivated the rewrite.

## Development

```sh
make compile   # byte-compile with warnings as errors
make checkdoc  # docstring lint
make test      # run the ERT suite
make all       # everything
```

CI runs the same three steps on Emacs 29.4 and 30.1. Bug reports and pull
requests are welcome — please keep `make all` green.

## License

GPL-3.0-or-later. See the license notices in the source files.
