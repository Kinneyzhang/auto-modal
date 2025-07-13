;;; auto-modal-keybind.el --- Keybinding management for auto-modal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; This file is part of auto-modal.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Keybinding management and execution for auto-modal.

;;; Code:

(require 'auto-modal-core)
(require 'auto-modal-cursor)
(require 'bind-key)

;;; Suppress Key Mode

(defvar suppress-key-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap of `suppress-key-mode', which is used to override
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

;;; Logging System

(defcustom auto-modal-log-max-number 40
  "Max number of auto-modal key command log."
  :type 'integer
  :group 'auto-modal)

(defvar auto-modal-log-num 0
  "Current number of auto-modal log.")

(defun auto-modal-record-log (key command)
  "Record KEY and COMMAND in auto-modal log buffer."
  (with-current-buffer (get-buffer-create "*Auto-modal-log*")
    (goto-char (point-min))
    (let ((inhibit-read-only t))
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

;;; Keyhint System

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
  "Unbind keyhint function from `auto-modal-help-key'."
  (unbind-key auto-modal-help-key 'suppress-key-mode-map))

;;; Key Execution

(defun auto-modal-original-command (key)
  "Get the original command for KEY."
  (or (lookup-key (current-local-map) (kbd key))
      (lookup-key global-map (kbd key))))

(defmacro auto-modal-key-bind (key-name &optional override-p)
  "Bind key KEY-NAME to `suppress-key-mode-map' if KEY-NAME
is not in `auto-modal-data'."
  (unless (auto-modal-has-key-p key-name)
    `(bind-key
      ,key-name
      (lambda ()
        (interactive)
        (if-let ((command (and (not ,override-p)
                               (lookup-key (current-local-map) (kbd ,key-name)))))
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
              (message "auto-modal-log: %s is undefined" ,key-name)))))
      'suppress-key-mode-map)))

(defmacro auto-modal-key-unbind (key-name)
  "Unbind key KEY-NAME from `suppress-key-mode-map' if KEY-NAME
is not in `auto-modal-data'."
  (unless (auto-modal-has-key-p key-name)
    `(unbind-key ,key-name 'suppress-key-mode-map)))

;;; Keybinding Management

(defun auto-modal-bind-key (key-name mode predicate function-args &optional override-p)
  "Add one auto-modal keybind to `auto-modal-data'."
  ;; if there is no current key in data before adding, bind it.
  (if (string= key-name auto-modal-help-key)
      (error "%s is bound to `auto-modal-keyhint-show' by default,\
you should not bind it to other functions!"
             auto-modal-help-key)
    (when (and (bound-and-true-p auto-modal-mode)
               (not (member key-name (auto-modal-all-keys))))
      ;; when `auto-modal-mode' is on, bind key at realtime.
      (auto-modal-key-bind key-name override-p))
    (let* ((mode (if (eq 'global mode) 'fundamental-mode mode))
           function args)
      (cond
       ((symbolp function-args)
        (setq function function-args))
       ((consp function-args)
        (setq function (car function-args))
        (setq args (cdr function-args))))
      (auto-modal--validate mode predicate function)
      (add-to-list 'auto-modal-data
                   (make-auto-modal-keybind :key-name key-name
                                            :mode mode
                                            :predicate predicate
                                            :function function
                                            :args args
                                            :override-p override-p)))))

(defun auto-modal-unbind-key (key-name mode predicate function-args &optional override-p)
  "Remove one auto-modal keybind from `auto-modal-data'."
  (let ((mode (if (eq 'global mode) 'fundamental-mode mode))
        function args)
    (cond
     ((symbolp function-args)
      (setq function function-args))
     ((consp function-args)
      (setq function (car function-args))
      (setq args (cdr function-args))))
    (auto-modal--validate mode predicate function)
    (setq auto-modal-data
          (remove (make-auto-modal-keybind :key-name key-name
                                           :mode mode
                                           :predicate predicate
                                           :function function
                                           :args args
                                           :override-p override-p)
                  auto-modal-data))
    ;; if there is no current key in data after removing, unbind it.
    (when (and (bound-and-true-p auto-modal-mode)
               (not (member key-name (auto-modal-all-keys))))
      ;; when `auto-modal-mode' is on, unbind key at realtime.
      (auto-modal-key-unbind key-name))))

(defun auto-modal-unbind-with-predicate (predicate)
  "Remove all keybinds with predicate PREDICATE."
  (setq auto-modal-data
        (seq-remove (lambda (data)
                      (eq (auto-modal-keybind-predicate data)
                          predicate))
                    auto-modal-data)))

(defun auto-modal-bind-all-keys ()
  "Bind all keys in `auto-modal-data' to `suppress-key-mode-map'
when `auto-modal-mode' turns on."
  (auto-modal-bind-keyhint)
  (dolist (key-name (auto-modal-all-keys))
    (auto-modal-key-bind
     key-name
     (auto-modal-key-override-status key-name))))

(defun auto-modal-unbind-all-keys ()
  "Unbind all keys in `auto-modal-data' from `suppress-key-mode-map'
when `auto-modal-mode' turns off."
  (auto-modal-unbind-keyhint)
  (dolist (key-name (auto-modal-all-keys))
    (auto-modal-key-unbind key-name)))

(provide 'auto-modal-keybind)
;;; auto-modal-keybind.el ends here