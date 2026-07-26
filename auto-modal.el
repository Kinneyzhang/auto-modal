;;; auto-modal.el --- Automatic modal switching driven by point-position predicates  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Kinney Zhang

;; Version: 1.0.0
;; Keywords: convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/auto-modal
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; auto-modal implements "automatic modal switching": instead of
;; toggling between a command state and an editing state by hand (as
;; in vi, evil or meow), the state is derived from where point is.
;;
;; Each keybinding registered with `auto-modal-bind-key' associates a
;; KEY with a MAJOR-MODE, a PREDICATE and a FUNCTION.  Whenever point
;; satisfies at least one predicate applicable to the current buffer,
;; auto-modal enters "control" state: the registered single-letter
;; keys run their functions instead of inserting text.  Everywhere
;; else the buffer behaves exactly as usual ("insert" state).  The
;; cursor type and color follow the state, so you always know which
;; one you are in.
;;
;; Minimal example:
;;
;;   (require 'auto-modal)
;;   (defun my/bolp () (and (bolp) (not (looking-at-p "^$"))))
;;   (auto-modal-bind-key "j" 'global 'my/bolp 'next-line)
;;   (auto-modal-bind-key "k" 'global 'my/bolp 'previous-line)
;;   (global-auto-modal-mode 1)
;;
;; Now "j" and "k" move by line whenever point is at the beginning of
;; a non-empty line, and self-insert normally anywhere else.
;;
;; Bindings are inherited along the major mode hierarchy: a binding
;; for `prog-mode' applies in every programming mode, and a binding
;; for the same key in a child mode shadows the parent's.  The
;; special mode symbol `global' applies to every buffer.
;;
;; See README.md for the full manual, and auto-modal-config.el for a
;; ready-to-use example configuration.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

;;;; Customization

(defgroup auto-modal nil
  "Automatic modal switching driven by point-position predicates."
  :group 'convenience
  :prefix "auto-modal-")

(defcustom auto-modal-control-cursor-type 'box
  "Cursor type used while auto-modal is in control state.
The symbol `default' means: use the default value of `cursor-type'.
Any other value is used directly as the buffer-local `cursor-type'."
  :type '(choice (const :tag "User default" default)
                 (sexp :tag "Cursor type"))
  :group 'auto-modal)

(defcustom auto-modal-insert-cursor-type 'bar
  "Cursor type used while auto-modal is in insert state.
The symbol `default' means: use the default value of `cursor-type'.
Any other value is used directly as the buffer-local `cursor-type'."
  :type '(choice (const :tag "User default" default)
                 (sexp :tag "Cursor type"))
  :group 'auto-modal)

(defcustom auto-modal-control-cursor-color nil
  "Cursor color used while auto-modal is in control state.
The value can be:
- nil: use the current theme's default cursor color.
- a face symbol: use that face's foreground color.
- a color string: use it directly.
- a cons cell (LIGHT . DARK): use LIGHT on light backgrounds and
  DARK on dark backgrounds."
  :type '(choice (const :tag "Theme default" nil)
                 (color :tag "Color")
                 (face :tag "Face foreground")
                 (cons :tag "Light/dark pair"
                       (color :tag "Light theme color")
                       (color :tag "Dark theme color")))
  :group 'auto-modal)

(defcustom auto-modal-insert-cursor-color nil
  "Cursor color used while auto-modal is in insert state.
The value can be:
- nil: use the current theme's default cursor color.
- a face symbol: use that face's foreground color.
- a color string: use it directly.
- a cons cell (LIGHT . DARK): use LIGHT on light backgrounds and
  DARK on dark backgrounds."
  :type '(choice (const :tag "Theme default" nil)
                 (color :tag "Color")
                 (face :tag "Face foreground")
                 (cons :tag "Light/dark pair"
                       (color :tag "Light theme color")
                       (color :tag "Dark theme color")))
  :group 'auto-modal)

(defcustom auto-modal-enable-log nil
  "Non-nil means record every dispatched key and command.
The log is kept in the buffer named by `auto-modal-log-buffer-name'."
  :type 'boolean
  :group 'auto-modal)

(defcustom auto-modal-log-max-number 40
  "Maximum number of entries kept in the auto-modal log buffer."
  :type 'natnum
  :group 'auto-modal)

(defcustom auto-modal-enable-keyhint nil
  "Non-nil means show the applicable keys when entering control state.
The hint is displayed in the echo area."
  :type 'boolean
  :group 'auto-modal)

(defcustom auto-modal-help-key "?"
  "Key reserved to show the applicable keybindings in control state.
This key cannot be registered with `auto-modal-bind-key'.  Set it
before enabling `auto-modal-mode'."
  :type 'string
  :group 'auto-modal)

;;;; Keybinding data

(cl-defstruct auto-modal-keybind
  "A single auto-modal keybinding.
KEY-NAME is a key string in `kbd' format, e.g. \"j\" or \"SPC\".
MODE is a major mode symbol; `fundamental-mode' applies everywhere.
PREDICATE is a function of no arguments, or t to always trigger.
FUNCTION is the function to run, with ARGS as its arguments.
OVERRIDE-P non-nil means the binding takes precedence over the
major mode's own local binding of the key."
  key-name mode predicate function args override-p)

(defvar auto-modal-data nil
  "The list of registered `auto-modal-keybind' structures.
Most recently added bindings come first.  Modify it with
`auto-modal-bind-key' and `auto-modal-unbind-key'.")

(defvar auto-modal-turn-on-hook nil
  "Hook run after `auto-modal-mode' is enabled in a buffer.")

(defvar auto-modal-turn-off-hook nil
  "Hook run after `auto-modal-mode' is disabled in a buffer.")

(defvar-local auto-modal-enable-insert-p nil
  "Non-nil means stay in insert state despite a satisfied predicate.
This is the one-shot escape hatch set by `auto-modal-enable-insert':
it lets you type at a trigger position.  It is consumed at the
beginning of the next command.")

(defvar-local auto-modal--last-state nil
  "The modal state (`control' or `insert') last applied to this buffer.")

;;;; Major mode hierarchy

(defvar auto-modal--mode-distance-cache (make-hash-table :test #'equal)
  "Cache mapping (MODE . ANCESTOR) to an inheritance distance or nil.")

(defun auto-modal--mode-parents (mode)
  "Return the parent chain of MODE, starting with MODE itself."
  (if (fboundp 'derived-mode-all-parents)
      (derived-mode-all-parents mode)
    (let ((chain (list mode))
          (parent mode))
      (while (setq parent (get parent 'derived-mode-parent))
        (setq chain (nconc chain (list parent))))
      chain)))

(defun auto-modal--mode-distance (ancestor &optional mode)
  "Return the inheritance distance from MODE up to ANCESTOR, or nil.
MODE defaults to the current buffer's major mode.  The distance is
0 when MODE is ANCESTOR, 1 for its direct parent, and so on.
`fundamental-mode' counts as the most distant ancestor of every
mode.  Return nil when MODE does not derive from ANCESTOR."
  (let* ((mode (or mode major-mode))
         (key (cons mode ancestor))
         (cached (gethash key auto-modal--mode-distance-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (puthash key
               (let ((chain (auto-modal--mode-parents mode)))
                 (or (seq-position chain ancestor #'eq)
                     (and (eq ancestor 'fundamental-mode)
                          (length chain))))
               auto-modal--mode-distance-cache))))

;;;; Control state keymap

(defvar auto-modal-control-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    map)
  "Keymap active while auto-modal is in control state.
All self-inserting keys are suppressed; keys registered with
`auto-modal-bind-key' are bound to `auto-modal-dispatch'.")

(define-minor-mode auto-modal-control-mode
  "Internal minor mode holding auto-modal's control-state keymap.
It is enabled and disabled automatically by `auto-modal-mode';
never toggle it manually."
  :interactive nil
  :keymap auto-modal-control-mode-map
  :group 'auto-modal)

;;;; Cursor handling

(defvar auto-modal--default-cursor-color nil
  "The default cursor color of the current theme, captured lazily.")

(defvar auto-modal--applied-color nil
  "The non-default cursor color auto-modal last applied to the frame.
Nil while the frame shows the default color.")

(defun auto-modal--default-cursor-color ()
  "Return the current theme's default cursor color, capturing it lazily."
  (or auto-modal--default-cursor-color
      (setq auto-modal--default-cursor-color
            (frame-parameter nil 'cursor-color))))

(defun auto-modal--resolve-cursor-type (type)
  "Return the effective cursor type for TYPE.
The symbol `default' resolves to the default value of `cursor-type';
anything else is returned unchanged."
  (if (eq type 'default)
      (default-value 'cursor-type)
    type))

(defun auto-modal--resolve-cursor-color (color)
  "Return the effective cursor color string for COLOR, or nil.
See `auto-modal-control-cursor-color' for the accepted forms."
  (cond
   ((null color) (auto-modal--default-cursor-color))
   ((facep color)
    (let ((foreground (face-attribute color :foreground nil t)))
      (if (stringp foreground)
          foreground
        (auto-modal--default-cursor-color))))
   ((stringp color) color)
   ((consp color)
    (if (eq (frame-parameter nil 'background-mode) 'dark)
        (cdr color)
      (car color)))
   (t (error "Invalid auto-modal cursor color: %S" color))))

(defun auto-modal--set-frame-cursor-color (color)
  "Set the selected frame's cursor color to COLOR unless it already is."
  (when (stringp color)
    ;; Capture the theme default before changing anything.
    (auto-modal--default-cursor-color)
    (unless (equal color (frame-parameter nil 'cursor-color))
      (set-cursor-color color))
    (setq auto-modal--applied-color
          (and (not (equal color (auto-modal--default-cursor-color)))
               color))))

(defun auto-modal--apply-cursor (state)
  "Apply the cursor type and color for STATE in the current buffer.
STATE is `control', `insert', or `default' (the user's own cursor)."
  (pcase state
    ('control
     (setq-local cursor-type (auto-modal--resolve-cursor-type
                              auto-modal-control-cursor-type))
     (auto-modal--set-frame-cursor-color
      (auto-modal--resolve-cursor-color auto-modal-control-cursor-color)))
    ('insert
     (setq-local cursor-type (auto-modal--resolve-cursor-type
                              auto-modal-insert-cursor-type))
     (auto-modal--set-frame-cursor-color
      (auto-modal--resolve-cursor-color auto-modal-insert-cursor-color)))
    (_
     (kill-local-variable 'cursor-type)
     (auto-modal--set-frame-cursor-color
      (auto-modal--default-cursor-color)))))

;;;; Trigger computation

(defvar auto-modal--broken-predicates nil
  "Predicates that signaled an error, warned about once already.")

(defun auto-modal--predicate-true-p (predicate)
  "Return non-nil if PREDICATE is satisfied at point.
PREDICATE is t (always satisfied) or a function of no arguments.
A symbol that is not (yet) defined as a function is treated as an
unsatisfied predicate.  A predicate that signals an error is
treated as unsatisfied (and reported once), so a broken predicate
cannot take down the `post-command-hook' driving auto-modal."
  (cond ((eq predicate t) t)
        ((functionp predicate)
         (condition-case err
             (funcall predicate)
           (error
            (unless (memq predicate auto-modal--broken-predicates)
              (push predicate auto-modal--broken-predicates)
              (message "Auto-modal predicate %S signaled %S; treating as unsatisfied"
                       predicate err))
            nil)))))

(defun auto-modal-triggered-p ()
  "Return non-nil if any applicable predicate is satisfied at point."
  (cl-loop for keybind in auto-modal-data
           thereis (and (auto-modal--mode-distance
                         (auto-modal-keybind-mode keybind))
                        (auto-modal--predicate-true-p
                         (auto-modal-keybind-predicate keybind)))))

(define-obsolete-function-alias 'auto-modal-is-triggerp
  #'auto-modal-triggered-p "1.0.0")

(defun auto-modal--active-keybind (key)
  "Return the keybind that KEY should run at point, or nil.
Among all bindings for KEY whose mode applies to the current
buffer and whose predicate is satisfied, return the one whose mode
is closest to the current major mode; on a tie, the most recently
added one wins."
  (let (best (best-distance most-positive-fixnum))
    (dolist (keybind auto-modal-data)
      (when (equal key (auto-modal-keybind-key-name keybind))
        (let ((distance (auto-modal--mode-distance
                         (auto-modal-keybind-mode keybind))))
          (when (and distance
                     (< distance best-distance)
                     (auto-modal--predicate-true-p
                      (auto-modal-keybind-predicate keybind)))
            (setq best keybind
                  best-distance distance)))))
    best))

;;;; State machine

;; Defined later by `define-minor-mode'.
(defvar auto-modal-mode)

(defun auto-modal--desired-state ()
  "Compute the modal state the current buffer should be in."
  (cond ((minibufferp) 'insert)
        ((and (not auto-modal-enable-insert-p)
              (auto-modal-triggered-p))
         'control)
        (t 'insert)))

(defun auto-modal--update ()
  "Recompute and apply the modal state of the current buffer."
  (let ((state (auto-modal--desired-state)))
    (if (eq state 'control)
        (unless auto-modal-control-mode (auto-modal-control-mode 1))
      (when auto-modal-control-mode (auto-modal-control-mode -1)))
    (auto-modal--apply-cursor state)
    (when (and auto-modal-enable-keyhint
               (eq state 'control)
               (not (eq auto-modal--last-state 'control)))
      (auto-modal-keyhint-show))
    (setq auto-modal--last-state state)))

(defun auto-modal-enable-insert ()
  "Switch to insert state for one command, even at a trigger position.
Bind this in control state (e.g. to \"SPC\") to start typing at a
position that would otherwise stay in control state."
  (interactive)
  (setq auto-modal-enable-insert-p t))

(defun auto-modal-set-cursor ()
  "Recompute the modal state and cursor of the current buffer."
  (interactive)
  (if auto-modal-mode
      (auto-modal--update)
    (auto-modal--apply-cursor 'default)))

(defun auto-modal-set-cursor-all-wins ()
  "Refresh the modal state of every buffer shown on the selected frame."
  (interactive)
  (dolist (window (window-list))
    (with-current-buffer (window-buffer window)
      (auto-modal-set-cursor))))

;;;; Command loop integration

(defun auto-modal--pre-command ()
  "Consume the one-shot insert escape at the start of the next command.
The command that sets `auto-modal-enable-insert-p' cannot consume
it here, because `pre-command-hook' runs before the command body.
Installed globally while at least one buffer has `auto-modal-mode'
enabled."
  (when (and auto-modal-mode auto-modal-enable-insert-p)
    (setq auto-modal-enable-insert-p nil)))

(defun auto-modal--post-command ()
  "Update the modal state after every command.
Installed globally on `post-command-hook' while at least one buffer
has `auto-modal-mode' enabled."
  (if auto-modal-mode
      (auto-modal--update)
    ;; Selected buffer does not use auto-modal: make sure the frame
    ;; cursor color is back to the default.
    (when auto-modal--applied-color
      (auto-modal--set-frame-cursor-color
       (auto-modal--default-cursor-color)))))

(defun auto-modal--after-theme-change (_theme)
  "Refresh the cached default cursor color after a theme change."
  ;; If the frame still shows a color auto-modal applied (the theme did
  ;; not set its own cursor color), restore the previous default first,
  ;; so our own color is not captured as the theme's.
  (when (and auto-modal--applied-color
             auto-modal--default-cursor-color
             (equal auto-modal--applied-color
                    (frame-parameter nil 'cursor-color)))
    (set-cursor-color auto-modal--default-cursor-color))
  (setq auto-modal--applied-color nil)
  (setq auto-modal--default-cursor-color
        (frame-parameter nil 'cursor-color))
  (auto-modal-set-cursor-all-wins))

;;;; Key dispatch

(defun auto-modal--local-command (key)
  "Return the command bound to KEY in the buffer's local keymap, if any.
Command remapping is resolved the way the command loop would,
ignoring auto-modal's own keymap."
  (when-let* ((map (current-local-map))
              (command (lookup-key map (kbd key))))
    (when (commandp command)
      (let ((auto-modal-control-mode nil))
        (or (command-remapping command) command)))))

(defun auto-modal--fallback-command (key)
  "Return the command KEY would run if auto-modal were not in control state."
  (let* ((auto-modal-control-mode nil)
         (command (key-binding (kbd key))))
    (and (commandp command) command)))

(defun auto-modal--run-keybind (key keybind)
  "Run KEYBIND's function, logging KEY when logging is enabled."
  (let ((function (auto-modal-keybind-function keybind))
        (args (auto-modal-keybind-args keybind)))
    (unless (functionp function)
      (user-error "Auto-modal binding `%S' is not defined (is its package loaded?)"
                  function))
    (prog1 (cond (args (apply function args))
                 ((commandp function) (call-interactively function))
                 (t (funcall function)))
      (when auto-modal-enable-log
        (auto-modal--record-log key (if args (cons function args) function))))))

(defun auto-modal-dispatch ()
  "Dispatch the key that invoked this command to its auto-modal binding.
The precedence is:
1. When the active binding does not have override-p set and the
   major mode's local keymap binds the key, run the local command.
2. Otherwise run the active binding's function.
3. When no binding's predicate is satisfied, run whatever command
   the key would run without auto-modal (usually `self-insert-command')."
  (interactive)
  (let* ((key (key-description (this-single-command-keys)))
         (keybind (auto-modal--active-keybind key))
         (local-command (and keybind
                             (not (auto-modal-keybind-override-p keybind))
                             (auto-modal--local-command key))))
    (cond
     (local-command (call-interactively local-command))
     (keybind (auto-modal--run-keybind key keybind))
     (t (if-let* ((command (auto-modal--fallback-command key)))
            (call-interactively command)
          (message "Auto-modal: %s is undefined here" key))))))

;; `auto-modal-dispatch' only makes sense when invoked from a key.
(put 'auto-modal-dispatch 'completion-predicate #'ignore)

;;;; Keyhint

(defun auto-modal-keyhint-show ()
  "Show the auto-modal keys applicable at point in the echo area."
  (interactive)
  (let (hints)
    (dolist (key (auto-modal-all-keys))
      (when-let* ((keybind (auto-modal--active-keybind key)))
        (push (format "%s → %S"
                      (propertize key 'face 'bold)
                      (auto-modal-keybind-function keybind))
              hints)))
    (when hints
      (if noninteractive
          (message "%s" (string-join (nreverse hints) "  "))
        (let ((minibuffer-message-timeout nil))
          (minibuffer-message (string-join (nreverse hints) "  ")))))))

;;;; Logging

(defvar auto-modal-log-buffer-name "*Auto-modal-log*"
  "Name of the buffer where auto-modal records dispatched keys.")

(defun auto-modal--record-log (key function-args)
  "Append KEY and FUNCTION-ARGS to the log buffer, trimming old entries."
  (with-current-buffer (get-buffer-create auto-modal-log-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "%s %S\n" key function-args))
      (goto-char (point-min))
      (let ((excess (- (count-lines (point-min) (point-max))
                       auto-modal-log-max-number)))
        (when (> excess 0)
          (forward-line excess)
          (delete-region (point-min) (point))))
      (setq buffer-read-only t))))

;;;; Binding and unbinding

(defun auto-modal--canonicalize-key (key-name)
  "Return KEY-NAME in canonical `key-description' form.
Signal an error when KEY-NAME is not a valid `kbd' string.
Canonicalizing at the API boundary guarantees that the stored key
name equals what `auto-modal-dispatch' computes from the pressed
key, so alternative spellings of the same key stay equivalent."
  (unless (and (stringp key-name) (not (string-empty-p key-name)))
    (error "Key name %S must be a string in `kbd' format" key-name))
  (condition-case nil
      (key-description (kbd key-name))
    (error (error "Key name %S is not valid `kbd' syntax" key-name))))

(defun auto-modal--validate (key-name mode predicate function)
  "Validate the KEY-NAME, MODE, PREDICATE and FUNCTION of a binding.
Signal an error when one of them is malformed.  PREDICATE and
FUNCTION may be symbols that are not defined yet, to allow binding
commands of packages that are loaded lazily."
  (auto-modal--canonicalize-key key-name)
  (unless (and mode (symbolp mode))
    (error "%S is not a major mode symbol" mode))
  (unless (or (eq predicate t)
              (functionp predicate)
              (and predicate (symbolp predicate)))
    (error "%S is not a valid auto-modal predicate" predicate))
  (unless (or (functionp function)
              (and function (symbolp function)))
    (error "%S is not a valid auto-modal function" function)))

(defun auto-modal--normalize-function-args (function-args)
  "Split FUNCTION-ARGS into a cons cell (FUNCTION . ARGS).
FUNCTION-ARGS is either a function, or a list whose head is a
function and whose tail is its arguments."
  (if (and (consp function-args)
           (not (functionp function-args)))
      (cons (car function-args) (cdr function-args))
    (cons function-args nil)))

(defun auto-modal-all-keys ()
  "Return all key names present in `auto-modal-data'."
  (delete-dups (mapcar #'auto-modal-keybind-key-name auto-modal-data)))

(defun auto-modal--maybe-release-key (key-name)
  "Remove KEY-NAME from the control keymap when no binding remains for it."
  (unless (member key-name (auto-modal-all-keys))
    (define-key auto-modal-control-mode-map (kbd key-name) nil t)))

;;;###autoload
(defun auto-modal-bind-key (key-name mode predicate function-args
                                     &optional override-p)
  "Register an auto-modal keybinding.

KEY-NAME is a key string in `kbd' format, e.g. \"j\" or \"SPC\".
MODE is a major mode symbol; the binding applies to that mode and
all modes derived from it.  The special symbol `global' (an alias
for `fundamental-mode') applies to every buffer.
PREDICATE is a function of no arguments called with point at the
position to test, or t to always trigger.
FUNCTION-ARGS is either a function, or a list (FUNCTION . ARGS) in
which case FUNCTION is applied to ARGS.  A plain command is called
interactively.
OVERRIDE-P non-nil gives this binding precedence over the major
mode's own local binding of KEY-NAME; when nil, a local keymap
binding of the key wins (so special modes like dired keep their
own letter keys).

When several bindings for the same key apply at point, the one
whose MODE is closest to the current major mode wins; on a tie the
most recently added one wins."
  (let* ((mode (if (eq mode 'global) 'fundamental-mode mode))
         (function-and-args (auto-modal--normalize-function-args
                             function-args))
         (function (car function-and-args))
         (args (cdr function-and-args)))
    (auto-modal--validate key-name mode predicate function)
    (setq key-name (auto-modal--canonicalize-key key-name))
    (when (equal key-name (auto-modal--canonicalize-key auto-modal-help-key))
      (user-error "Key %s is reserved to show auto-modal keyhints; \
customize `auto-modal-help-key' to change it" key-name))
    (let ((keybind (make-auto-modal-keybind
                    :key-name key-name
                    :mode mode
                    :predicate predicate
                    :function function
                    :args args
                    :override-p override-p)))
      (unless (member keybind auto-modal-data)
        (push keybind auto-modal-data))
      (define-key auto-modal-control-mode-map (kbd key-name)
                  #'auto-modal-dispatch)
      keybind)))

(defun auto-modal-unbind-key (key-name mode predicate function-args
                                       &optional override-p)
  "Remove the binding registered with exactly the same arguments.
KEY-NAME, MODE, PREDICATE, FUNCTION-ARGS and OVERRIDE-P must equal
the arguments originally passed to `auto-modal-bind-key'.  For a
more forgiving variant, see `auto-modal-unbind'."
  (let* ((mode (if (eq mode 'global) 'fundamental-mode mode))
         (key-name (auto-modal--canonicalize-key key-name))
         (function-and-args (auto-modal--normalize-function-args
                             function-args))
         (function (car function-and-args))
         (args (cdr function-and-args)))
    (setq auto-modal-data
          (remove (make-auto-modal-keybind
                   :key-name key-name
                   :mode mode
                   :predicate predicate
                   :function function
                   :args args
                   :override-p override-p)
                  auto-modal-data))
    (auto-modal--maybe-release-key key-name)))

(defun auto-modal-unbind (key-name &optional mode predicate)
  "Remove every binding of KEY-NAME, optionally filtered.
When MODE is non-nil, only remove bindings for that mode (`global'
is an alias for `fundamental-mode').  When PREDICATE is non-nil,
only remove bindings with that predicate."
  (interactive
   (list (completing-read "Unbind auto-modal key: " (auto-modal-all-keys))))
  (let ((mode (if (eq mode 'global) 'fundamental-mode mode))
        (key-name (auto-modal--canonicalize-key key-name)))
    (setq auto-modal-data
          (seq-remove
           (lambda (keybind)
             (and (equal key-name (auto-modal-keybind-key-name keybind))
                  (or (null mode)
                      (eq mode (auto-modal-keybind-mode keybind)))
                  (or (null predicate)
                      (eq predicate (auto-modal-keybind-predicate keybind)))))
           auto-modal-data))
    (auto-modal--maybe-release-key key-name)))

(defun auto-modal-unbind-with-predicate (predicate)
  "Remove all keybindings whose predicate is PREDICATE."
  (let ((keys (mapcar #'auto-modal-keybind-key-name
                      (seq-filter
                       (lambda (keybind)
                         (eq predicate
                             (auto-modal-keybind-predicate keybind)))
                       auto-modal-data))))
    (setq auto-modal-data
          (seq-remove (lambda (keybind)
                        (eq predicate
                            (auto-modal-keybind-predicate keybind)))
                      auto-modal-data))
    (mapc #'auto-modal--maybe-release-key (delete-dups keys))))

(defun auto-modal--bind-all-keys ()
  "Synchronize the control keymap with `auto-modal-data'."
  (define-key auto-modal-control-mode-map (kbd auto-modal-help-key)
              #'auto-modal-keyhint-show)
  (dolist (key-name (auto-modal-all-keys))
    (define-key auto-modal-control-mode-map (kbd key-name)
                #'auto-modal-dispatch)))

;;;; Listing keybindings

(define-derived-mode auto-modal-keybinds-mode tabulated-list-mode
  "Auto-Modal-Keybinds"
  "Major mode for listing auto-modal keybindings."
  (setq tabulated-list-format
        [("Key" 8 t)
         ("Mode" 22 t)
         ("Predicate" 28 t)
         ("Function" 32 t)
         ("Override" 8 nil)])
  (tabulated-list-init-header))

(defun auto-modal-list-keybinds ()
  "Display all registered auto-modal keybindings in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Auto-modal-keybinds*")
    (auto-modal-keybinds-mode)
    (setq tabulated-list-entries
          (let ((index 0))
            (mapcar
             (lambda (keybind)
               (list (cl-incf index)
                     (vector
                      (auto-modal-keybind-key-name keybind)
                      (symbol-name (auto-modal-keybind-mode keybind))
                      (format "%S" (auto-modal-keybind-predicate keybind))
                      (format "%S" (if (auto-modal-keybind-args keybind)
                                       (cons (auto-modal-keybind-function keybind)
                                             (auto-modal-keybind-args keybind))
                                     (auto-modal-keybind-function keybind)))
                      (if (auto-modal-keybind-override-p keybind) "yes" ""))))
             (reverse auto-modal-data))))
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

;;;; Minor modes

(defun auto-modal--any-buffer-active-p ()
  "Return non-nil when some live buffer has `auto-modal-mode' enabled."
  (cl-loop for buffer in (buffer-list)
           thereis (buffer-local-value 'auto-modal-mode buffer)))

(defun auto-modal--global-setup ()
  "Install the global hooks auto-modal needs.  Idempotent."
  (auto-modal--default-cursor-color)
  (add-hook 'pre-command-hook #'auto-modal--pre-command)
  (add-hook 'post-command-hook #'auto-modal--post-command)
  (add-hook 'enable-theme-functions #'auto-modal--after-theme-change)
  (add-hook 'disable-theme-functions #'auto-modal--after-theme-change))

(defun auto-modal--global-teardown ()
  "Remove the global hooks once auto-modal is off in every buffer."
  (unless (auto-modal--any-buffer-active-p)
    (remove-hook 'pre-command-hook #'auto-modal--pre-command)
    (remove-hook 'post-command-hook #'auto-modal--post-command)
    (remove-hook 'enable-theme-functions #'auto-modal--after-theme-change)
    (remove-hook 'disable-theme-functions #'auto-modal--after-theme-change)))

;;;###autoload
(define-minor-mode auto-modal-mode
  "Toggle automatic modal switching in the current buffer.

When enabled, the buffer is in \"control\" state whenever point
satisfies one of the predicates registered with
`auto-modal-bind-key' for the current major mode: the registered
keys then run commands instead of inserting text.  Anywhere else
the buffer is in \"insert\" state and behaves as usual.

The cursor type and color follow the state; see
`auto-modal-control-cursor-type' and friends.  Press the key in
`auto-modal-help-key' (\"?\" by default) in control state to see
the applicable keys."
  :lighter " AutoModal"
  :group 'auto-modal
  (if auto-modal-mode
      (progn
        (auto-modal--global-setup)
        (auto-modal--bind-all-keys)
        (add-hook 'kill-buffer-hook #'auto-modal--on-kill-buffer nil t)
        (auto-modal--update)
        (run-hooks 'auto-modal-turn-on-hook))
    (auto-modal-control-mode -1)
    (auto-modal--apply-cursor 'default)
    (setq auto-modal--last-state nil)
    (setq auto-modal-enable-insert-p nil)
    (remove-hook 'kill-buffer-hook #'auto-modal--on-kill-buffer t)
    (auto-modal--global-teardown)
    (run-hooks 'auto-modal-turn-off-hook)))

(defun auto-modal--on-kill-buffer ()
  "Disable `auto-modal-mode' before its buffer is killed.
This makes sure the global hooks are removed together with the
last live auto-modal buffer."
  (when auto-modal-mode
    (auto-modal-mode -1)))

(defun auto-modal--turn-on ()
  "Enable `auto-modal-mode' except in minibuffers and internal buffers."
  (unless (or (minibufferp)
              (string-prefix-p " " (buffer-name)))
    (auto-modal-mode 1)))

(define-obsolete-function-alias 'auto-modal-mode-turn-on
  #'auto-modal--turn-on "1.0.0")

;;;###autoload
(define-globalized-minor-mode global-auto-modal-mode
  auto-modal-mode auto-modal--turn-on
  :group 'auto-modal)

(provide 'auto-modal)
;;; auto-modal.el ends here
