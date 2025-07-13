# Auto-Modal Advanced Usage Guide

This guide provides comprehensive information on using auto-modal effectively.

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Core Concepts](#core-concepts)
4. [Configuration](#configuration)
5. [Presets](#presets)
6. [Advanced Usage](#advanced-usage)
7. [Troubleshooting](#troubleshooting)
8. [API Reference](#api-reference)

## Installation

### MELPA (Recommended)

```elisp
(use-package auto-modal
  :ensure t
  :config
  (auto-modal-mode 1))
```

### Manual Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/Kinneyzhang/auto-modal.git
   ```

2. Add to your load path:
   ```elisp
   (add-to-list 'load-path "/path/to/auto-modal")
   (require 'auto-modal)
   (auto-modal-mode 1)
   ```

## Quick Start

### Using Presets

The easiest way to get started is with presets:

```elisp
(require 'auto-modal-presets)

;; Choose a preset based on your needs
(auto-modal-setup-basic)        ; Basic navigation
(auto-modal-setup-programming)  ; Programming-focused
(auto-modal-setup-lisp)         ; Lisp editing
(auto-modal-setup-text)         ; Text editing
(auto-modal-setup-org)          ; Org-mode
```

### Basic Usage

1. Enable auto-modal: `M-x auto-modal-mode`
2. Move cursor to the beginning of a line
3. Use single-letter commands (j/k for line navigation, etc.)
4. Press Space to enable insert mode when needed

## Core Concepts

### Automatic Mode Switching

Auto-modal automatically switches between two modes:

- **Control Mode**: Single-letter commands are active
- **Insert Mode**: Normal Emacs editing

The switching is based on **predicates** - functions that determine when to activate control mode.

### Predicates

Common predicates:

- `auto-modal-bolp`: Beginning of line (non-empty)
- `use-region-p`: When text is selected
- `auto-modal-sexp-around-paren-p`: Around parentheses in Lisp

### Keybindings

Keybindings are context-aware and can be different for different major modes:

```elisp
;; Global binding
(auto-modal-bind-key "j" 'global 'auto-modal-bolp 'next-line)

;; Mode-specific binding
(auto-modal-bind-key "j" 'org-mode 'auto-modal-bolp 'outline-next-visible-heading)
```

## Configuration

### Cursor Appearance

Customize cursor appearance for different modes:

```elisp
(setq auto-modal-control-cursor-type 'box)
(setq auto-modal-insert-cursor-type 'bar)
(setq auto-modal-control-cursor-color '("red" . "orange"))
(setq auto-modal-insert-cursor-color '("blue" . "cyan"))
```

### Logging and Debugging

Enable logging for troubleshooting:

```elisp
(setq auto-modal-enable-log t)
(setq auto-modal-enable-keyhint t)
```

View logs: `M-x switch-to-buffer RET *Auto-modal-log*`

### Custom Predicates

Create custom predicates for specific use cases:

```elisp
(defun my-custom-predicate ()
  "Return t when cursor is at end of line."
  (eolp))

(auto-modal-bind-key "h" 'global 'my-custom-predicate 'backward-char)
```

## Presets

### Available Presets

1. **Basic**: Essential navigation and editing
2. **Programming**: Code-focused features
3. **Lisp**: S-expression editing
4. **Text**: Text editing and formatting
5. **Org**: Org-mode specific features

### Interactive Setup

```elisp
M-x auto-modal-setup-presets
```

### Preset Customization

You can extend presets with additional bindings:

```elisp
;; Start with basic preset
(auto-modal-setup-basic)

;; Add custom bindings
(auto-modal-bind-key "x" 'global 'auto-modal-bolp 'execute-extended-command)
```

## Advanced Usage

### Mode-Specific Configurations

Different major modes can have different behaviors:

```elisp
;; Python-specific
(auto-modal-bind-key "r" 'python-mode 'auto-modal-bolp 'python-shell-send-region)

;; JavaScript-specific
(auto-modal-bind-key "r" 'js-mode 'auto-modal-bolp 'nodejs-repl-send-region)
```

### Multiple Predicates

Combine multiple predicates for complex behaviors:

```elisp
(defun my-complex-predicate ()
  "Complex predicate combining multiple conditions."
  (and (auto-modal-bolp)
       (not (looking-at "^#"))))  ; Not on comment lines

(auto-modal-bind-key "j" 'global 'my-complex-predicate 'my-smart-navigation)
```

### Hooks and Customization

Use hooks to customize behavior:

```elisp
(add-hook 'auto-modal-turn-on-hook
          (lambda ()
            (message "Auto-modal activated")))

(add-hook 'auto-modal-turn-off-hook
          (lambda ()
            (message "Auto-modal deactivated")))
```

### Integration with Other Packages

#### Avy Integration

```elisp
(auto-modal-bind-key "l" 'global 'auto-modal-bolp 'avy-goto-line)
(auto-modal-bind-key "c" 'global 'auto-modal-bolp 'avy-goto-char-timer)
```

#### Ivy/Counsel Integration

```elisp
(auto-modal-bind-key "f" 'global 'auto-modal-bolp 'counsel-find-file)
(auto-modal-bind-key "b" 'global 'auto-modal-bolp 'counsel-switch-buffer)
```

## Troubleshooting

### Common Issues

1. **Keys not working**: Check if auto-modal-mode is enabled
2. **Wrong commands**: Verify major mode and predicates
3. **Cursor issues**: Check cursor configuration

### Debug Commands

```elisp
;; Check current bindings
(auto-modal-trigger-functions)

;; Check if predicate is active
(auto-modal-is-triggerp)

;; View all data
auto-modal-data
```

### Performance Issues

If you experience performance issues:

```elisp
;; Disable logging
(setq auto-modal-enable-log nil)

;; Disable keyhint
(setq auto-modal-enable-keyhint nil)
```

## API Reference

### Core Functions

- `auto-modal-bind-key`: Bind a key to a command
- `auto-modal-unbind-key`: Remove a key binding
- `auto-modal-enable-insert`: Switch to insert mode
- `auto-modal-is-triggerp`: Check if control mode should be active

### Predicates

- `auto-modal-bolp`: Beginning of line
- `auto-modal-eolp`: End of line
- `auto-modal-empty-line-p`: Empty line
- `auto-modal-sexp-around-paren-p`: Around parentheses

### Cursor Functions

- `auto-modal-set-cursor`: Set cursor for current mode
- `auto-modal-set-control-cursor`: Set control mode cursor
- `auto-modal-set-insert-cursor`: Set insert mode cursor

### Utility Functions

- `auto-modal-trigger-functions`: Get active functions
- `auto-modal-all-keys`: Get all bound keys
- `auto-modal-keyhint-show`: Show available keys

## Examples

### Complete Configuration Example

```elisp
(use-package auto-modal
  :ensure t
  :config
  ;; Enable auto-modal globally
  (auto-modal-mode 1)
  
  ;; Cursor configuration
  (setq auto-modal-control-cursor-type 'box)
  (setq auto-modal-insert-cursor-type 'bar)
  (setq auto-modal-control-cursor-color "red")
  (setq auto-modal-insert-cursor-color "blue")
  
  ;; Enable debugging
  (setq auto-modal-enable-log t)
  (setq auto-modal-enable-keyhint t)
  
  ;; Load presets
  (require 'auto-modal-presets)
  (auto-modal-setup-programming)
  
  ;; Custom bindings
  (auto-modal-bind-key "x" 'global 'auto-modal-bolp 'execute-extended-command)
  (auto-modal-bind-key "g" 'global 'auto-modal-bolp 'goto-line)
  
  ;; Mode-specific bindings
  (auto-modal-bind-key "t" 'org-mode 'auto-modal-bolp 'org-todo)
  (auto-modal-bind-key "r" 'python-mode 'auto-modal-bolp 'python-shell-send-region))
```

### Custom Predicate Example

```elisp
(defun my-code-predicate ()
  "Active in programming modes at beginning of line."
  (and (derived-mode-p 'prog-mode)
       (auto-modal-bolp)))

(auto-modal-bind-key "c" 'global 'my-code-predicate 'compile)
```

This guide covers the essential aspects of using auto-modal effectively. For more advanced usage and the latest features, refer to the source code and examples in the repository.