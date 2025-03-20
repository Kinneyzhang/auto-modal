;;; auto-modal.el --- Automatically switch to a VI-like control mode based on different conditions.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang
;;
;; Version: 0.0.1
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Automatically switch to a VI-like control mode based on different conditions.

;;; Code:

(defun major-mode-chain (mode)
  "A list of major modes which MODE is derived from."
  (let* ((chain (list mode))
         (parent-mode mode))
    (while (setq parent-mode (get parent-mode 'derived-mode-parent))
      (push parent-mode chain))
    (push 'fundamental-mode chain)))

(defun major-mode-derived-p (derived-mode &optional current-mode)
  "Determine whether CURRENT-MODE is derived from DERIVED-MODE.
If CURRENT-MODE is nil, defaults to major mode of current buffer.
It returns which generation of the parent major mode of current
major mode."
  (when-let* ((modes (major-mode-chain (or current-mode major-mode)))
              (parents (member derived-mode modes)))
    (length parents)))

(defvar background-mode-change-hook nil
  "Normal hook that is run after the background of theme changed.")

(defun background-mode-change (original-func &rest args)
  "Advice functon when load a theme."
  (let ((before-background-mode (frame-parameter nil 'background-mode))
        after-background-mode)
    (apply original-func args)
    ;; set `auto-modal-default-cursor-color' of current new theme.
    (setq auto-modal-default-cursor-color
          (frame-parameter nil 'cursor-color))
    (setq after-background-mode (frame-parameter nil 'background-mode))
    (unless (eq before-background-mode after-background-mode)
      (run-hooks 'background-mode-change-hook))))

(defun background-mode-change-setup ()
  "Add advice when load a theme and run `background-mode-change-hook'.
in the advice function after the background-mode changed."
  (when (package-installed-p 'counsel)
    (advice-add #'counsel-load-theme :around #'background-mode-change))
  (advice-add #'load-theme :around #'background-mode-change))

(defun background-mode-change-unset ()
  "Remove advice when load a theme and run `background-mode-change-hook'.
in the advice function after the background-mode changed."
  "When load a theme, run `background-mode-change-hook'."
  (when (package-installed-p 'counsel)
    (advice-remove #'counsel-load-theme #'background-mode-change))
  (advice-remove #'load-theme #'background-mode-change))



(defvar suppress-key-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap of `suppress-key-mode'，which is used to override
 all normally self-inserting keys to be undefined.")

;;;###autoload
(define-minor-mode suppress-key-mode
  "Make `suppress-key-mode-map' override all normally
 self-inserting keys to be undefined."
  :keymap suppress-key-mode-map
  :global t
  :interactive nil
  (when suppress-key-mode
    (suppress-keymap suppress-key-mode-map t)))



(defun auto-modal-default-cursor-type ()
  "Default cursor type of user's setting, it's
the value of `setq-default'."
  (default-value 'cursor-type))

(defconst auto-modal-default-cursor-color (frame-parameter nil 'cursor-color)
  "Default cursor color of current theme.
Every time load a theme, reset it by
`(frame-parameter nil 'cursor-color)'")

(defcustom auto-modal-control-cursor-type 'box
  "Type of cursor when in auto-modal control mode.
When the value is 'default, use the default cursor type
set by `setq-default'. Otherwise the cursor type is the
same in `cursor-type'.")

(defcustom auto-modal-insert-cursor-type 'bar
  "Type of cursor when in auto-modal insert mode.
When the value is 'default, use the default cursor type
set by `setq-default'.")

(defcustom auto-modal-control-cursor-color nil
  "Color of cursor when in auto-modal control mode.

When the value is nil, use `auto-modal-default-cursor-type' by default.
When the value is a face, use the foreground color of that face.
When the value is a string, directly use the string as color.
When the value is a cons cell, use the car of it in light theme and
the cdr of it in dark theme.")

(defcustom auto-modal-insert-cursor-color nil
  "Color of cursor when in auto-modal insert mode.

When the value is nil, use `auto-modal-default-cursor-type' by default.
When the value is a face, use the foreground color of that face.
When the value is a string, directly use the string as color.
When the value is a cons cell, use the car of it in light theme and
the cdr of it in dark theme.")

(defcustom auto-modal-log-max-number 40
  "Max number of auto-modal key command log.")

(defvar auto-modal-log-num 0
  "Current number of auto-modal log.")

(defvar-local auto-modal-enable-insert-p nil
  "A buffer local variable to enable to insert when in control mode.")

(defvar auto-modal-data nil
  "A list of `auto-modal-keybind' data.")

(defvar auto-modal-turn-on-hook nil
  "Hook run when `auto-modal-mode' is turned on.")

(defvar auto-modal-turn-off-hook nil
  "Hook run when `auto-modal-mode' is turned off.")

(defvar auto-modal-enable-log nil
  "Whether to enable record key and command in message.")

(defvar auto-modal-enable-keyhint nil
  "Whether to enable show keyhint in minibuffer.")

(defvar auto-modal-help-key "?"
  "Default key name to prompt keybindings in echo area.")

(cl-defstruct auto-modal-keybind
  "Core structure of auto-modal keybinding."
  key-name mode predicate function args)

(defun auto-modal-current-cursor-type ()
  "Get current cursor type."
  cursor-type)

(defun auto-modal-current-cursor-color ()
  "Get current cursor color."
  (frame-parameter nil 'cursor-color))

(defun auto-modal-should-cursor-type (type)
  "Get cursor type should be, if TYPE is 'default,
use `auto-modal-default-cursor-type', otherwise use TYPE."
  (if (eq 'default type)
      (auto-modal-default-cursor-type)
    
    type))

(defun auto-modal-should-cursor-color (color)
  "Get cursor color it's currently should be.

When COLOR is nil, use `auto-modal-default-cursor-type' by default.
When COLOR is a face, use the foreground color of that face.
When COLOR is a string, directly use the string as color.
When COLOR is a cons cell, use the car of it in light theme and
the cdr of it in dark theme."
  (cond
   ((null color) auto-modal-default-cursor-color)
   ((facep color) (face-attribute color :foreground))
   ((stringp color) color)
   ((consp color)
    (pcase (frame-parameter nil 'background-mode)
      ('light (car color))
      ('dark (cdr color))))
   (_ (error "Invalid format of auto-modal cursor color: %S" color))))

(defun auto-modal-set-control-cursor ()
  "Set the type and color of cursor when in control mode."
  ;; set cursor color
  (let ((should-color (auto-modal-should-cursor-color
                       auto-modal-control-cursor-color))
        (current-color (auto-modal-current-cursor-color)))
    (unless (eq current-color should-color)
      (set-cursor-color should-color)))
  ;; set cursor type
  (let ((should-type (auto-modal-should-cursor-type
                      auto-modal-control-cursor-type))
        (current-type (auto-modal-current-cursor-type)))
    (unless (eq current-type should-type)
      (setq-local cursor-type should-type))))

(defun auto-modal-set-insert-cursor ()
  "Set the type and color of cursor when in insert mode."
  ;; set cursor color
  (let ((should-color (auto-modal-should-cursor-color
                       auto-modal-insert-cursor-color))
        (current-color (auto-modal-current-cursor-color)))
    (unless (eq current-color should-color)
      (set-cursor-color should-color)))
  ;; set cursor type
  (let ((should-type (auto-modal-should-cursor-type
                      auto-modal-insert-cursor-type))
        (current-type (auto-modal-current-cursor-type)))
    (unless (eq current-type should-type)
      (setq-local cursor-type should-type))))

(defun auto-modal-switch-to-insert ()
  "Switch to auto-modal insert mode."
  (interactive)
  (auto-modal-set-insert-cursor)
  (when suppress-key-mode
    (suppress-key-mode -1)))

(defun auto-modal-switch-to-control ()
  "Switch to auto-modal control mode."
  (interactive)
  (auto-modal-set-control-cursor)
  (when (not suppress-key-mode)
    (suppress-key-mode 1)))

(defun auto-modal-enable-insert ()
  "Enable insert when in auto-modal control mode."
  (setq auto-modal-enable-insert-p t))

(defun auto-modal-functions-data (&optional mode)
  "A list of predicate, function, arg and key-name of MODE."
  (mapcar (lambda (data)
            (append
             (list (auto-modal-keybind-key-name data))
             (cons (auto-modal-keybind-predicate data)
                   (append (list (auto-modal-keybind-function data))
                           (auto-modal-keybind-args data)))))
          (seq-filter (lambda (cl-x)
                        (major-mode-derived-p
                         (auto-modal-keybind-mode cl-x)
                         (or mode major-mode)))
                      auto-modal-data)))

(defun auto-modal-trigger-functions (&optional mode)
  "Return all functions that could be triggered."
  (mapcar (lambda (data)
            (cons (car data) (cddr data)))
          (seq-filter (lambda (data)
                        (or (eq t (cadr data))
                            (funcall (cadr data))))
                      (auto-modal-functions-data mode))))

(defun auto-modal-keyhint-show ()
  "Show keyhint message in echo area."
  (interactive)
  (let ((minibuffer-message-timeout nil))
    (minibuffer-message
     (mapconcat (lambda (data)
                  (format "%s → %S "
                          (propertize (car data) 'face 'bold)
                          (cadr data)))
                (auto-modal-trigger-functions)
                " "))))

(defun auto-modal-is-triggerp ()
  "Determine whether the conditions for triggering the modal
switch are met by executing all assertion functions for the
current major mode. If any of them return true, the trigger
condition is satisfied."
  (seq-some (lambda (bool)
              (not (null bool)))
            (mapcar (lambda (func)
                      (or (eq t func) (funcall func)))
                    (mapcar #'cadr (auto-modal-functions-data
                                    major-mode)))))

(defun auto-modal-set-cursor ()
  "Set cursor independently."
  (if (and auto-modal-mode
           (auto-modal-is-triggerp))
      (auto-modal-set-control-cursor)
    (auto-modal-set-insert-cursor)))

(defun auto-modal-set-cursor-all-wins ()
  "Set cursor type correctly in all windows when
turning `auto-modal-mode' on and off."
  (interactive)
  (dolist (win (window-list))
    (with-current-buffer (window-buffer win)
      (auto-modal-set-cursor))))

(defvar auto-modal-pre-is-control-p nil
  "Whether pre command is in control mode.")

(defun auto-modal-pre-command-function ()
  (if (auto-modal-is-triggerp)
      (setq auto-modal-pre-is-control-p t)
    (setq auto-modal-pre-is-control-p nil)))

(defun auto-modal-post-command-function ()
  "Automatically switch modal after executing each command."
  (when (and auto-modal-mode
             (bufferp (current-buffer)))
    (if (minibufferp)
        ;; Always enable inserting when in minibuffer.
        (auto-modal-switch-to-insert)
      (when (and (auto-modal-is-triggerp)
                 (not auto-modal-enable-insert-p))
        (auto-modal-switch-to-control))
      (when (or (not (auto-modal-is-triggerp))
                auto-modal-enable-insert-p)
        (auto-modal-switch-to-insert)
        (setq auto-modal-enable-insert-p nil)))
    (when auto-modal-enable-keyhint
      (unless (eq auto-modal-pre-is-control-p
                  (auto-modal-is-triggerp))
        (auto-modal-keyhint-show)))))

(defun auto-modal-key-command (key-name)
  "Return command according to KEY-NAME and current major mode."
  (when-let* ((keybinds
               (seq-filter
                (lambda (keybind)
                  (and (string= key-name
                                (auto-modal-keybind-key-name keybind))
                       (or (eq t (auto-modal-keybind-predicate keybind))
                           (funcall (auto-modal-keybind-predicate keybind)))))
                auto-modal-data))
              (mode-levels
               (seq-map (lambda (el)
                          (major-mode-derived-p
                           (auto-modal-keybind-mode el)))
                        keybinds))
              (mode-levels-without-nil (remove nil mode-levels))
              (min-level (seq-min mode-levels-without-nil))
              (i (seq-position mode-levels min-level))
              ;; 取 major-mode 的继承关系离自己最近的
              (keybind (nth i keybinds))
              (func (auto-modal-keybind-function keybind)))
    (if-let ((args (auto-modal-keybind-args keybind)))
        `(,func ,@args)
      (list func))))

(defun auto-modal-has-key-p (key-name)
  "Determine whether key-name is already exist in `auto-modal-data'."
  (member key-name (mapcar #'auto-modal-keybind-key-name auto-modal-data)))

(defun auto-modal--validate (mode predicate function)
  "Validate MODE, PREDICATE and FUNCTION."
  ;; (unless (fboundp mode)
  ;;   (error "%S is not a major mode!" mode))
  (unless (functionp function)
    (error "%S is not a function!" function))
  (unless (or (functionp predicate) (eq predicate t))
    (error "%S is not a predicate function!" predicate)))

(defun auto-modal-record-log (key command)
  (with-current-buffer (get-buffer-create "*Auto-modal-log*")
    (goto-char (point-min))
    (let ((inhibit-read-only 1))
      (save-excursion
        (goto-char (point-max))
        (setq auto-modal-log-num (line-number-at-pos))
        (when (= auto-modal-log-num auto-modal-log-max-number)
          (delete-line) (delete-char -1)))
      (if (looking-at "^$")
          (insert (format "%s %s" key command))
        (add-text-properties (line-beginning-position)
                             (line-end-position)
                             '(face shadow))
        (insert (format "%s %s\n" key command)))
      (read-only-mode 1))))

(defun auto-modal-original-command (key)
  (or (lookup-key (current-local-map) (kbd key))
      (lookup-key global-map (kbd key))))

(defmacro auto-modal-key-bind (key-name)
  "Bind key KEY-NAME to `suppress-key-mode-map' if KEY-NAME
is not in `auto-modal-data'."
  (unless (auto-modal-has-key-p key-name)
    `(bind-key
      ,key-name
      (lambda ()
        (interactive)
        (if-let ((command (lookup-key (current-local-map) (kbd ,key-name))))
            (call-interactively command)
          (if-let ((func-args (auto-modal-key-command ,key-name)))
              (progn
                (if (= (length func-args) 1)
                    (if (commandp (car func-args))
                        (call-interactively (car func-args))
                      (apply func-args))
                  (apply func-args))
                (when auto-modal-enable-log
                  (auto-modal-record-log ,key-name func-args)))
            (if-let* ((command (auto-modal-original-command ,key-name))
                      (_ (commandp command)))
                (call-interactively command)
              (message "auto-modal-log: %s is undifined" ,key-name)))))
      'suppress-key-mode-map)))

(defmacro auto-modal-key-unbind (key-name)
  "Unbind key KEY-NAME from `suppress-key-mode-map' if KEY-NAME
is not in `auto-modal-data'."
  (unless (auto-modal-has-key-p key-name)
    `(unbind-key ,key-name 'suppress-key-mode-map)))

(defun auto-modal-bind-key (key-name mode predicate function &rest args)
  "Add one auto-modal keybind to `auto-modal-data'."
  ;; if there is no current key in data before adding, bind it.
  (if (string= key-name auto-modal-help-key)
      (error "%s is binded to `auto-modal-keyhint-show' by default,\
you should not bind it to other functions!"
             auto-modal-help-key)
    (when (and auto-modal-mode
               (not (member key-name (auto-modal-all-keys))))
      ;; when `auto-modal-mode' is on，bind key at realtime.
      (auto-modal-key-bind key-name))
    (let ((mode (if (eq 'global mode) 'fundamental-mode mode)))
      (auto-modal--validate mode predicate function)
      (add-to-list 'auto-modal-data
                   (make-auto-modal-keybind :key-name key-name
                                            :mode mode
                                            :predicate predicate
                                            :function function
                                            :args args)))))

(defun auto-modal-unbind-key (key-name mode predicate function &rest args)
  "Remove one auto-modal keybind from `auto-modal-data'."
  (let ((mode (if (eq 'global mode) 'fundamental-mode mode)))
    (auto-modal--validate mode predicate function)
    (setq auto-modal-data
          (remove (make-auto-modal-keybind :key-name key-name
                                           :mode mode
                                           :predicate predicate
                                           :function function
                                           :args args)
                  auto-modal-data))
    ;; if there is no current key in data after removing, unbind it.
    (when (and auto-modal-mode
               (not (member key-name (auto-modal-all-keys))))
      ;; when `auto-modal-mode' is on，unbind key at realtime.
      (auto-modal-key-unbind key-name))))

(defun auto-modal-unbind-with-predicate (predicate)
  "Remove all keybinds with predicate PREDICATE."
  (setq auto-modal-data
        (seq-remove (lambda (data)
                      (eq (auto-modal-keybind-predicate data)
                          predicate))
                    auto-modal-data)))

(defun auto-modal-all-keys ()
  "Return all keys in `auto-modal-data'."
  (delete-dups (seq-map (lambda (keybind)
                          (auto-modal-keybind-key-name keybind))
                        auto-modal-data)))

(defun auto-modal-bind-keyhint ()
  "Bind keyhint function to `auto-modal-help-key'."
  (bind-key auto-modal-help-key
            (lambda ()
              (interactive)
              (when auto-modal-enable-log
                (auto-modal-record-log auto-modal-help-key
                                       (list 'auto-modal-keyhint-show)))
              (when (auto-modal-is-triggerp)
                (auto-modal-keyhint-show)))
            'suppress-key-mode-map))

(defun auto-modal-unbind-keyhint ()
  "Unbind keyhint function to `auto-modal-help-key'."
  (unbind-key auto-modal-help-key 'suppress-key-mode-map))

(defun auto-modal-bind-all-keys ()
  "Bind all keys in `auto-modal-data' to `suppress-key-mode-map'
when `auto-modal-mode' turns on."
  (auto-modal-bind-keyhint)
  (dolist (key-name (auto-modal-all-keys))
    (auto-modal-key-bind key-name)))

(defun auto-modal-unbind-all-keys ()
  "Unbind all keys in `auto-modal-data' to `suppress-key-mode-map'
when `auto-modal-mode' turns off."
  (auto-modal-unbind-keyhint)
  (dolist (key-name (auto-modal-all-keys))
    (auto-modal-key-unbind key-name)))

;;;###autoload
(define-minor-mode auto-modal-mode
  "Minor mode for switching modal automatically."
  :global t
  (auto-modal-set-cursor-all-wins)
  ;; FIXME: cannot work properbly after emacs startup
  (if auto-modal-mode
      (progn
        (auto-modal-bind-all-keys)
        (add-hook 'pre-command-hook 'auto-modal-pre-command-function)
        (add-hook 'post-command-hook 'auto-modal-post-command-function)
        (add-hook 'window-configuration-change-hook
                  'auto-modal-set-cursor-all-wins)
        (run-hooks 'auto-modal-turn-on-hook)
        (background-mode-change-setup))
    (setq-local cursor-type (auto-modal-default-cursor-type))
    (set-cursor-color auto-modal-default-cursor-color)
    (suppress-key-mode -1)
    (auto-modal-unbind-all-keys)
    (remove-hook 'pre-command-hook 'auto-modal-pre-command-function)
    (remove-hook 'post-command-hook 'auto-modal-post-command-function)
    (remove-hook 'window-configuration-change-hook
                 'auto-modal-set-cursor-all-wins)
    (run-hooks 'auto-modal-turn-off-hook)
    (background-mode-change-unset)))

(provide 'auto-modal)
