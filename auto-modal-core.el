;;; auto-modal-core.el --- Core functionality for auto-modal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; This file is part of auto-modal.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Core data structures and functions for auto-modal.

;;; Code:

(require 'cl-lib)

;;; Data Structures

(cl-defstruct auto-modal-keybind
  "Core structure of auto-modal keybinding."
  key-name mode predicate function args override-p)

;;; Variables

(defvar auto-modal-data nil
  "A list of `auto-modal-keybind' data.")

(defvar auto-modal-help-key "?"
  "Default key name to prompt keybindings in echo area.")

(defvar auto-modal-enable-log nil
  "Whether to enable record key and command in message.")

(defvar auto-modal-enable-keyhint nil
  "Whether to enable show keyhint in minibuffer.")

(defvar-local auto-modal-enable-insert-p nil
  "A buffer local variable to enable to insert when in control mode.")

;;; Utility Functions

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

;;; Core Functions

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

(defun auto-modal-all-keys ()
  "Return all keys in `auto-modal-data'."
  (delete-dups (seq-map (lambda (keybind)
                          (auto-modal-keybind-key-name keybind))
                        auto-modal-data)))

(defun auto-modal-key-override-status (key)
  "Get override status for KEY."
  (auto-modal-keybind-override-p
   (seq-find (lambda (keybind)
               (string= key (auto-modal-keybind-key-name keybind)))
             auto-modal-data)))

(defun auto-modal--validate (mode predicate function)
  "Validate MODE, PREDICATE and FUNCTION."
  (unless (functionp function)
    (error "%S is not a function!" function))
  (unless (or (functionp predicate) (eq predicate t))
    (error "%S is not a predicate function!" predicate)))

(defun auto-modal-enable-insert ()
  "Enable insert when in auto-modal control mode."
  (setq auto-modal-enable-insert-p t))

(provide 'auto-modal-core)
;;; auto-modal-core.el ends here