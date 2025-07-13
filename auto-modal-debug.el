;;; auto-modal-debug.el --- Debugging utilities for auto-modal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; This file is part of auto-modal.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Debugging and diagnostic utilities for auto-modal.

;;; Code:

(require 'auto-modal-core)

;;; Debug Variables

(defvar auto-modal-debug-mode nil
  "Whether debug mode is enabled.")

(defvar auto-modal--debug-buffer "*Auto-modal Debug*"
  "Buffer name for debug output.")

(defvar auto-modal--debug-history nil
  "History of debug events.")

(defvar auto-modal--debug-max-history 100
  "Maximum number of debug events to keep.")

;;; Debug Functions

(defun auto-modal--debug-message (format-string &rest args)
  "Debug message with FORMAT-STRING and ARGS."
  (when auto-modal-debug-mode
    (let ((message (apply #'format format-string args))
          (timestamp (format-time-string "%H:%M:%S.%3N")))
      (push (list timestamp message) auto-modal--debug-history)
      (when (> (length auto-modal--debug-history) auto-modal--debug-max-history)
        (setq auto-modal--debug-history
              (butlast auto-modal--debug-history 
                       (- (length auto-modal--debug-history)
                          auto-modal--debug-max-history))))
      (with-current-buffer (get-buffer-create auto-modal--debug-buffer)
        (goto-char (point-max))
        (insert (format "[%s] %s\n" timestamp message))
        (when (get-buffer-window (current-buffer))
          (with-selected-window (get-buffer-window (current-buffer))
            (goto-char (point-max))))))))

(defun auto-modal-debug-toggle ()
  "Toggle debug mode."
  (interactive)
  (setq auto-modal-debug-mode (not auto-modal-debug-mode))
  (if auto-modal-debug-mode
      (progn
        (auto-modal--debug-message "Debug mode enabled")
        (message "Auto-modal debug mode enabled"))
    (message "Auto-modal debug mode disabled")))

(defun auto-modal-debug-clear ()
  "Clear debug history."
  (interactive)
  (setq auto-modal--debug-history nil)
  (when (get-buffer auto-modal--debug-buffer)
    (with-current-buffer auto-modal--debug-buffer
      (erase-buffer))))

(defun auto-modal-debug-show ()
  "Show debug buffer."
  (interactive)
  (let ((buffer (get-buffer-create auto-modal--debug-buffer)))
    (with-current-buffer buffer
      (when (= (buffer-size) 0)
        (insert "Auto-modal Debug Log\n")
        (insert "===================\n\n")
        (dolist (entry (reverse auto-modal--debug-history))
          (insert (format "[%s] %s\n" (car entry) (cadr entry)))))
      (goto-char (point-max)))
    (display-buffer buffer)))

;;; Diagnostic Functions

(defun auto-modal-diagnose ()
  "Diagnose auto-modal configuration and state."
  (interactive)
  (with-current-buffer (get-buffer-create "*Auto-modal Diagnostics*")
    (erase-buffer)
    (insert "Auto-modal Diagnostics\n")
    (insert "======================\n\n")
    
    ;; Mode status
    (insert "Mode Status:\n")
    (insert (format "  auto-modal-mode: %s\n" (if auto-modal-mode "enabled" "disabled")))
    (insert (format "  Current buffer: %s\n" (buffer-name)))
    (insert (format "  Major mode: %s\n" major-mode))
    (insert (format "  Point: %d\n" (point)))
    (insert (format "  Line: %d\n" (line-number-at-pos)))
    (insert (format "  Column: %d\n" (current-column)))
    (insert "\n")
    
    ;; Trigger status
    (insert "Trigger Status:\n")
    (insert (format "  Trigger active: %s\n" (if (auto-modal-is-triggerp) "yes" "no")))
    (insert (format "  Suppress mode: %s\n" (if suppress-key-mode "enabled" "disabled")))
    (insert (format "  Insert enabled: %s\n" (if auto-modal-enable-insert-p "yes" "no")))
    (insert "\n")
    
    ;; Cursor status
    (insert "Cursor Status:\n")
    (insert (format "  Type: %s\n" (auto-modal-current-cursor-type)))
    (insert (format "  Color: %s\n" (auto-modal-current-cursor-color)))
    (insert (format "  Control type: %s\n" auto-modal-control-cursor-type))
    (insert (format "  Insert type: %s\n" auto-modal-insert-cursor-type))
    (insert "\n")
    
    ;; Active predicates
    (insert "Active Predicates:\n")
    (let ((functions-data (auto-modal-functions-data)))
      (if functions-data
          (dolist (data functions-data)
            (let ((key (car data))
                  (predicate (cadr data))
                  (active (if (eq (cadr data) t) t (funcall (cadr data)))))
              (insert (format "  %s: %s -> %s\n" key predicate (if active "ACTIVE" "inactive")))))
        (insert "  No predicates active\n")))
    (insert "\n")
    
    ;; Available commands
    (insert "Available Commands:\n")
    (let ((trigger-funcs (auto-modal-trigger-functions)))
      (if trigger-funcs
          (dolist (func trigger-funcs)
            (insert (format "  %s -> %s\n" (car func) (cadr func))))
        (insert "  No commands available\n")))
    (insert "\n")
    
    ;; All bindings
    (insert "All Bindings:\n")
    (if auto-modal-data
        (dolist (binding auto-modal-data)
          (insert (format "  %s [%s] %s -> %s\n"
                          (auto-modal-keybind-key-name binding)
                          (auto-modal-keybind-mode binding)
                          (auto-modal-keybind-predicate binding)
                          (auto-modal-keybind-function binding))))
      (insert "  No bindings configured\n"))
    
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun auto-modal-test-predicate (predicate)
  "Test a PREDICATE function interactively."
  (interactive "aPredicate function: ")
  (let ((result (if (functionp predicate)
                    (funcall predicate)
                  (error "Not a function: %s" predicate))))
    (message "Predicate %s returned: %s" predicate result)
    result))

(defun auto-modal-test-all-predicates ()
  "Test all predicates in current context."
  (interactive)
  (let ((functions-data (auto-modal-functions-data)))
    (if functions-data
        (dolist (data functions-data)
          (let ((key (car data))
                (predicate (cadr data)))
            (condition-case err
                (let ((result (if (eq predicate t) t (funcall predicate))))
                  (message "Key %s predicate %s: %s" key predicate result))
              (error (message "Key %s predicate %s ERROR: %s" key predicate err)))))
      (message "No predicates to test"))))

;;; Performance Debugging

(defun auto-modal-debug-performance ()
  "Show performance debugging information."
  (interactive)
  (with-current-buffer (get-buffer-create "*Auto-modal Performance Debug*")
    (erase-buffer)
    (insert "Auto-modal Performance Debug\n")
    (insert "============================\n\n")
    
    ;; Hook timings
    (insert "Hook Performance:\n")
    (insert (format "  pre-command-hook: %s\n" 
                    (if (member 'auto-modal-pre-command-function pre-command-hook)
                        "installed" "not installed")))
    (insert (format "  post-command-hook: %s\n"
                    (if (member 'auto-modal-post-command-function post-command-hook)
                        "installed" "not installed")))
    (insert "\n")
    
    ;; Cache statistics
    (when (boundp 'auto-modal--predicate-cache)
      (insert "Cache Statistics:\n")
      (insert (format "  Cache size: %d entries\n" 
                      (hash-table-count auto-modal--predicate-cache)))
      (insert (format "  Cache timeout: %f seconds\n" 
                      (if (boundp 'auto-modal--cache-timeout) 
                          auto-modal--cache-timeout 0.1)))
      (insert "\n"))
    
    ;; Memory usage
    (insert "Memory Usage:\n")
    (insert (format "  Auto-modal data: %d bindings\n" (length auto-modal-data)))
    (insert (format "  Garbage collections: %d\n" gcs-done))
    (insert "\n")
    
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; Interactive Debugging

(defun auto-modal-debug-step ()
  "Step through auto-modal execution."
  (interactive)
  (auto-modal--debug-message "=== DEBUG STEP ===")
  (auto-modal--debug-message "Point: %d, Line: %d, Column: %d" 
                             (point) (line-number-at-pos) (current-column))
  (auto-modal--debug-message "Major mode: %s" major-mode)
  (auto-modal--debug-message "Trigger active: %s" (auto-modal-is-triggerp))
  (auto-modal--debug-message "Available functions: %s" (auto-modal-trigger-functions))
  (auto-modal-debug-show))

(defun auto-modal-debug-key (key)
  "Debug what happens when KEY is pressed."
  (interactive "kKey to debug: ")
  (let ((key-name (key-description key)))
    (auto-modal--debug-message "=== KEY DEBUG: %s ===" key-name)
    (auto-modal--debug-message "Key command: %s" (auto-modal-key-command key-name))
    (auto-modal--debug-message "Has key: %s" (auto-modal-has-key-p key-name))
    (auto-modal--debug-message "Override status: %s" (auto-modal-key-override-status key-name))
    (auto-modal-debug-show)))

;;; Validation

(defun auto-modal-validate-configuration ()
  "Validate auto-modal configuration."
  (interactive)
  (let ((errors nil)
        (warnings nil))
    
    ;; Check for common issues
    (unless auto-modal-mode
      (push "auto-modal-mode is not enabled" errors))
    
    (unless auto-modal-data
      (push "No key bindings configured" warnings))
    
    (dolist (binding auto-modal-data)
      (let ((key (auto-modal-keybind-key-name binding))
            (func (auto-modal-keybind-function binding))
            (pred (auto-modal-keybind-predicate binding)))
        
        ;; Check function exists
        (unless (or (functionp func) (commandp func))
          (push (format "Function %s for key %s is not defined" func key) errors))
        
        ;; Check predicate
        (unless (or (eq pred t) (functionp pred))
          (push (format "Predicate %s for key %s is not a function" pred key) errors))))
    
    ;; Report results
    (let ((message-parts nil))
      (when errors
        (push (format "ERRORS: %s" (mapconcat 'identity errors "; ")) message-parts))
      (when warnings
        (push (format "WARNINGS: %s" (mapconcat 'identity warnings "; ")) message-parts))
      (unless (or errors warnings)
        (push "Configuration is valid" message-parts))
      
      (message "Auto-modal validation: %s" (mapconcat 'identity message-parts " | ")))))

;;; Instrumentation

(defmacro auto-modal-debug-instrument (function)
  "Instrument FUNCTION with debug logging."
  `(advice-add ',function :around
               (lambda (orig-func &rest args)
                 (auto-modal--debug-message "Calling %s with args: %s" ',function args)
                 (let ((result (apply orig-func args)))
                   (auto-modal--debug-message "Function %s returned: %s" ',function result)
                   result))))

(defun auto-modal-debug-instrument-all ()
  "Instrument all auto-modal functions for debugging."
  (interactive)
  (auto-modal-debug-instrument auto-modal-is-triggerp)
  (auto-modal-debug-instrument auto-modal-key-command)
  (auto-modal-debug-instrument auto-modal-switch-to-control)
  (auto-modal-debug-instrument auto-modal-switch-to-insert)
  (message "Auto-modal functions instrumented for debugging"))

(defun auto-modal-debug-uninstrument-all ()
  "Remove instrumentation from all auto-modal functions."
  (interactive)
  (advice-remove 'auto-modal-is-triggerp)
  (advice-remove 'auto-modal-key-command)
  (advice-remove 'auto-modal-switch-to-control)
  (advice-remove 'auto-modal-switch-to-insert)
  (message "Auto-modal debugging instrumentation removed"))

;;; Keybindings

(defvar auto-modal-debug-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'auto-modal-debug-toggle)
    (define-key map (kbd "c") 'auto-modal-debug-clear)
    (define-key map (kbd "s") 'auto-modal-debug-show)
    (define-key map (kbd "d") 'auto-modal-diagnose)
    (define-key map (kbd "p") 'auto-modal-debug-performance)
    (define-key map (kbd "v") 'auto-modal-validate-configuration)
    (define-key map (kbd "S") 'auto-modal-debug-step)
    (define-key map (kbd "k") 'auto-modal-debug-key)
    (define-key map (kbd "i") 'auto-modal-debug-instrument-all)
    (define-key map (kbd "u") 'auto-modal-debug-uninstrument-all)
    map)
  "Keymap for auto-modal debug commands.")

(provide 'auto-modal-debug)
;;; auto-modal-debug.el ends here