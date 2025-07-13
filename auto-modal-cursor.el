;;; auto-modal-cursor.el --- Cursor management for auto-modal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; This file is part of auto-modal.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Cursor type and color management for auto-modal.

;;; Code:

(require 'auto-modal-core)

;;; Cursor Configuration

(defcustom auto-modal-control-cursor-type 'box
  "Type of cursor when in auto-modal control mode.
When the value is 'default, use the default cursor type
set by `setq-default'. Otherwise the cursor type is the
same in `cursor-type'."
  :type '(choice (const :tag "Default" default)
                 (const :tag "Box" box)
                 (const :tag "Bar" bar)
                 (const :tag "Hbar" hbar))
  :group 'auto-modal)

(defcustom auto-modal-insert-cursor-type 'bar
  "Type of cursor when in auto-modal insert mode.
When the value is 'default, use the default cursor type
set by `setq-default'."
  :type '(choice (const :tag "Default" default)
                 (const :tag "Box" box)
                 (const :tag "Bar" bar)
                 (const :tag "Hbar" hbar))
  :group 'auto-modal)

(defcustom auto-modal-control-cursor-color nil
  "Color of cursor when in auto-modal control mode.

When the value is nil, use `auto-modal-default-cursor-color' by default.
When the value is a face, use the foreground color of that face.
When the value is a string, directly use the string as color.
When the value is a cons cell, use the car of it in light theme and
the cdr of it in dark theme."
  :type '(choice (const :tag "Default" nil)
                 (face :tag "Face")
                 (string :tag "Color name")
                 (cons :tag "Light/Dark colors"
                       (string :tag "Light color")
                       (string :tag "Dark color")))
  :group 'auto-modal)

(defcustom auto-modal-insert-cursor-color nil
  "Color of cursor when in auto-modal insert mode.

When the value is nil, use `auto-modal-default-cursor-color' by default.
When the value is a face, use the foreground color of that face.
When the value is a string, directly use the string as color.
When the value is a cons cell, use the car of it in light theme and
the cdr of it in dark theme."
  :type '(choice (const :tag "Default" nil)
                 (face :tag "Face")
                 (string :tag "Color name")
                 (cons :tag "Light/Dark colors"
                       (string :tag "Light color")
                       (string :tag "Dark color")))
  :group 'auto-modal)

;;; Variables

(defvar background-mode-change-hook nil
  "Normal hook that is run after the background of theme changed.")

(defconst auto-modal-default-cursor-color (frame-parameter nil 'cursor-color)
  "Default cursor color of current theme.
Every time load a theme, reset it by
`(frame-parameter nil 'cursor-color)'")

;;; Theme Change Support

(defun background-mode-change (original-func &rest args)
  "Advice function when load a theme."
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
  "Add advice when load a theme and run `background-mode-change-hook'
in the advice function after the background-mode changed."
  (when (fboundp 'counsel-load-theme)
    (advice-add #'counsel-load-theme :around #'background-mode-change))
  (advice-add #'load-theme :around #'background-mode-change))

(defun background-mode-change-unset ()
  "Remove advice when load a theme and run `background-mode-change-hook'
in the advice function after the background-mode changed."
  (when (fboundp 'counsel-load-theme)
    (advice-remove #'counsel-load-theme #'background-mode-change))
  (advice-remove #'load-theme #'background-mode-change))

;;; Cursor Management Functions

(defun auto-modal-default-cursor-type ()
  "Default cursor type of user's setting, it's the value of `setq-default'."
  (default-value 'cursor-type))

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

When COLOR is nil, use `auto-modal-default-cursor-color' by default.
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
    (unless (equal current-color should-color)
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
    (unless (equal current-color should-color)
      (set-cursor-color should-color)))
  ;; set cursor type
  (let ((should-type (auto-modal-should-cursor-type
                      auto-modal-insert-cursor-type))
        (current-type (auto-modal-current-cursor-type)))
    (unless (eq current-type should-type)
      (setq-local cursor-type should-type))))

(defun auto-modal-set-cursor ()
  "Set cursor independently."
  (if (and (bound-and-true-p auto-modal-mode)
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

(provide 'auto-modal-cursor)
;;; auto-modal-cursor.el ends here