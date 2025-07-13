;;; auto-modal.el --- Automatically switch to a VI-like control mode based on different conditions -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang
;;
;; Version: 0.1.0
;; Keywords: convenience modal editing
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/auto-modal
;; Package-Requires: ((emacs "29.1") (bind-key "2.4"))

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

;; Auto-modal provides "automatic modal switching" for Emacs.
;; 
;; When the cursor position satisfies a specified predicate function,
;; it automatically switches to "command mode" (single-letter commands).
;; When the cursor position doesn't satisfy the predicate, it automatically
;; switches to normal mode (regular Emacs editing).
;;
;; Key features:
;; - Automatic mode switching based on cursor position
;; - Context-aware keybindings for different major modes
;; - Visual feedback with different cursor styles
;; - Flexible predicate system for trigger conditions
;; - Built-in support for common editing patterns
;; - Extensible configuration system

;;; Code:

(require 'auto-modal-core)
(require 'auto-modal-cursor)
(require 'auto-modal-keybind)

;;; Customization Group

(defgroup auto-modal nil
  "Automatically switch to a VI-like control mode based on different conditions."
  :group 'convenience
  :prefix "auto-modal-")

;;; Hooks

(defvar auto-modal-turn-on-hook nil
  "Hook run when `auto-modal-mode' is turned on.")

(defvar auto-modal-turn-off-hook nil
  "Hook run when `auto-modal-mode' is turned off.")

;;; Mode Variables

(defvar auto-modal-pre-is-control-p nil
  "Whether pre command is in control mode.")

;;; Mode Switching Functions

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

;;; Hook Functions

(defun auto-modal-pre-command-function ()
  "Record control mode status before command execution."
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

;;; Main Mode

;;;###autoload
(define-minor-mode auto-modal-mode
  "Minor mode for switching modal automatically."
  :global t
  :group 'auto-modal
  (auto-modal-set-cursor-all-wins)
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
;;; auto-modal.el ends here