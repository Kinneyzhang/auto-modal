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



(defcustom auto-modal-control-cursor-type 'box
  "Type of cursor when in auto-modal control mode.")

(defcustom auto-modal-insert-cursor-type 'bar
  "Type of cursor when in auto-modal insert mode.")

(defvar-local auto-modal-enable-insert-p nil
  "A buffer local variable to enable to insert when in control mode.")

(defvar auto-modal-data nil
  "A list of `auto-modal-keybind' data.")

(defvar auto-modal-turn-on-hook nil
  "Hook run when `auto-modal-mode' is turned on.")

(defvar auto-modal-turn-off-hook nil
  "Hook run when `auto-modal-mode' is turned off.")

(cl-defstruct auto-modal-keybind
  "Core structure of auto-modal keybinding."
  key-name mode predicate function args)

(defun auto-modal-switch-to-insert ()
  "Switch to auto-modal insert mode."
  (interactive)
  (when (eq cursor-type auto-modal-control-cursor-type)
    (setq-local cursor-type auto-modal-insert-cursor-type))
  (when suppress-key-mode
    (suppress-key-mode -1)))

(defun auto-modal-enable-insert ()
  "Enable insert when in auto-modal control mode."
  (setq auto-modal-enable-insert-p t))

(defun auto-modal-switch-to-control ()
  "Switch to auto-modal control mode."
  (interactive)
  (when (eq cursor-type auto-modal-insert-cursor-type)
    (setq-local cursor-type auto-modal-control-cursor-type))
  (when (not suppress-key-mode)
    (suppress-key-mode 1)))

(defun auto-modal-predicate-functions (&optional mode)
  "Return all predicate functions that trigger modal switch
for major mode MODE. If MODE is nil, default to major mode
of current buffer."
  (mapcar #'auto-modal-keybind-predicate
          (seq-filter (lambda (cl-x)
                        (major-mode-derived-p
                         (auto-modal-keybind-mode cl-x)
                         (or mode major-mode)))
                      auto-modal-data)))

(defun auto-modal-is-triggerp ()
  "Determine whether the conditions for triggering the modal
switch are met by executing all assertion functions for the
current major mode. If any of them return true, the trigger
condition is satisfied."
  (seq-some (lambda (bool)
              (not (null bool)))
            (mapcar (lambda (func)
                      (or (eq t func) (funcall func)))
                    (auto-modal-predicate-functions major-mode))))

(defun auto-modal-set-cursor ()
  (if (and auto-modal-mode
           (auto-modal-is-triggerp))
      (setq-local cursor-type auto-modal-control-cursor-type)
    (setq-local cursor-type auto-modal-insert-cursor-type)))

(defun auto-modal-set-cursor-when-idle ()
  "Set cursor type correctly in current buffer
after idle time."
  (interactive)
  (run-with-idle-timer 0.1 nil 'auto-modal-set-cursor))

(defun auto-modal-set-cursor-all-wins ()
  "Set cursor type correctly in all windows when
turning `auto-modal-mode' on and off."
  (interactive)
  (dolist (win (window-list))
    (with-current-buffer (window-buffer win)
      (auto-modal-set-cursor))))

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
        (setq auto-modal-enable-insert-p nil)))))

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
  (unless (fboundp mode)
    (error "%S is not a major mode!" mode))
  (unless (functionp function)
    (error "%S is not a function!" function))
  (unless (or (functionp predicate) (eq predicate t))
    (error "%S is not a predicate function!" predicate)))

(defmacro auto-modal-key-bind (key-name)
  "Bind key KEY-NAME to `suppress-key-mode-map' if KEY-NAME
is not in `auto-modal-data'."
  (unless (auto-modal-has-key-p key-name)
    `(bind-key ,key-name
               (lambda ()
                 (interactive)
                 (if-let ((func-args (auto-modal-key-command ,key-name)))
                     (progn
                       (if (commandp (car func-args))
                           (call-interactively (car func-args))
                         (apply func-args)))
                   (user-error "%s is undifined" ,key-name)))
               'suppress-key-mode-map)))

(defmacro auto-modal-key-unbind (key-name)
  "Unbind key KEY-NAME from `suppress-key-mode-map' if KEY-NAME
is not in `auto-modal-data'."
  (unless (auto-modal-has-key-p key-name)
    `(unbind-key ,key-name 'suppress-key-mode-map)))

(defun auto-modal-bind-key (key-name mode predicate function &rest args)
  "Add one auto-modal keybind to `auto-modal-data'."
  ;; if there is no current key in data before adding, bind it.
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
                                          :args args))))

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

(defun auto-modal-bind-all-keys ()
  "Bind all keys in `auto-modal-data' to `suppress-key-mode-map'
when `auto-modal-mode' turns on."
  (dolist (key-name (auto-modal-all-keys))
    (auto-modal-key-bind key-name)))

(defun auto-modal-unbind-all-keys ()
  "Unbind all keys in `auto-modal-data' to `suppress-key-mode-map'
when `auto-modal-mode' turns off."
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
        (add-hook 'post-command-hook 'auto-modal-post-command-function)
        ;; delay update cursor type in some cases
        (add-hook 'post-command-hook 'auto-modal-set-cursor-when-idle)
        (add-hook 'window-configuration-change-hook 'auto-modal-set-cursor-all-wins)
        (run-hooks 'auto-modal-turn-on-hook))
    (suppress-key-mode -1)
    (auto-modal-unbind-all-keys)
    (remove-hook 'post-command-hook 'auto-modal-post-command-function)
    (remove-hook 'post-command-hook 'auto-modal-set-cursor-when-idle)
    (remove-hook 'window-configuration-change-hook 'auto-modal-set-cursor-all-wins)
    (run-hooks 'auto-modal-turn-off-hook)))

(provide 'auto-modal)
