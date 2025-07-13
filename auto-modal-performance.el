;;; auto-modal-performance.el --- Performance optimizations for auto-modal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; This file is part of auto-modal.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Performance optimizations and caching for auto-modal.

;;; Code:

(require 'auto-modal-core)

;;; Caching System

(defvar auto-modal--predicate-cache (make-hash-table :test 'equal)
  "Cache for predicate results to improve performance.")

(defvar auto-modal--cache-timeout 0.1
  "Timeout for predicate cache in seconds.")

(defvar auto-modal--last-cache-clear 0
  "Time when cache was last cleared.")

(defun auto-modal--clear-cache ()
  "Clear predicate cache if timeout exceeded."
  (let ((current-time (float-time)))
    (when (> (- current-time auto-modal--last-cache-clear) auto-modal--cache-timeout)
      (clrhash auto-modal--predicate-cache)
      (setq auto-modal--last-cache-clear current-time))))

(defun auto-modal--cached-predicate-p (predicate)
  "Check if PREDICATE result is cached and still valid."
  (auto-modal--clear-cache)
  (gethash (list predicate (point) (buffer-chars-modified-tick)) 
           auto-modal--predicate-cache))

(defun auto-modal--cache-predicate (predicate result)
  "Cache PREDICATE RESULT for current position."
  (puthash (list predicate (point) (buffer-chars-modified-tick))
           result auto-modal--predicate-cache))

;;; Optimized Predicate Evaluation

(defun auto-modal-is-triggerp-cached ()
  "Optimized version of `auto-modal-is-triggerp' with caching."
  (let ((cache-key (list 'trigger-check (point) (buffer-chars-modified-tick) major-mode)))
    (or (gethash cache-key auto-modal--predicate-cache)
        (let ((result (seq-some (lambda (bool)
                                  (not (null bool)))
                                (mapcar (lambda (func)
                                          (if (eq t func)
                                              t
                                            (let ((cached (auto-modal--cached-predicate-p func)))
                                              (if cached
                                                  cached
                                                (let ((result (funcall func)))
                                                  (auto-modal--cache-predicate func result)
                                                  result)))))
                                        (mapcar #'cadr (auto-modal-functions-data major-mode))))))
          (puthash cache-key result auto-modal--predicate-cache)
          result))))

;;; Batch Operations

(defvar auto-modal--batch-operations nil
  "Queue for batch operations.")

(defun auto-modal--add-to-batch (operation)
  "Add OPERATION to batch queue."
  (push operation auto-modal--batch-operations))

(defun auto-modal--process-batch ()
  "Process all queued batch operations."
  (when auto-modal--batch-operations
    (dolist (operation (reverse auto-modal--batch-operations))
      (funcall operation))
    (setq auto-modal--batch-operations nil)))

;;; Lazy Loading

(defvar auto-modal--lazy-bindings nil
  "Queue for lazy key bindings.")

(defun auto-modal-lazy-bind-key (key-name mode predicate function-args &optional override-p)
  "Lazy version of `auto-modal-bind-key' that defers binding until needed."
  (push (lambda ()
          (auto-modal-bind-key key-name mode predicate function-args override-p))
        auto-modal--lazy-bindings))

(defun auto-modal--process-lazy-bindings ()
  "Process all lazy key bindings."
  (when auto-modal--lazy-bindings
    (dolist (binding (reverse auto-modal--lazy-bindings))
      (funcall binding))
    (setq auto-modal--lazy-bindings nil)))

;;; Optimized Hook Functions

(defvar auto-modal--hook-timer nil
  "Timer for batching hook operations.")

(defun auto-modal-post-command-function-optimized ()
  "Optimized version of post-command hook with batching."
  (when auto-modal--hook-timer
    (cancel-timer auto-modal--hook-timer))
  (setq auto-modal--hook-timer
        (run-with-idle-timer 0.01 nil
                             (lambda ()
                               (when (and auto-modal-mode
                                          (bufferp (current-buffer)))
                                 (if (minibufferp)
                                     (auto-modal-switch-to-insert)
                                   (let ((should-control (auto-modal-is-triggerp-cached)))
                                     (when (and should-control
                                                (not auto-modal-enable-insert-p))
                                       (auto-modal-switch-to-control))
                                     (when (or (not should-control)
                                               auto-modal-enable-insert-p)
                                       (auto-modal-switch-to-insert)
                                       (setq auto-modal-enable-insert-p nil))))
                                 (auto-modal--process-batch))))))

;;; Memory Management

(defun auto-modal--gc-collect ()
  "Collect garbage when appropriate."
  (when (> (- (float-time) auto-modal--last-cache-clear) 10)
    (garbage-collect)))

;;; Performance Monitoring

(defvar auto-modal--performance-stats (make-hash-table :test 'equal)
  "Performance statistics.")

(defun auto-modal--record-performance (operation start-time)
  "Record performance for OPERATION from START-TIME."
  (let ((elapsed (- (float-time) start-time)))
    (puthash operation
             (cons elapsed (gethash operation auto-modal--performance-stats))
             auto-modal--performance-stats)))

(defmacro auto-modal--with-performance (operation &rest body)
  "Execute BODY and record performance for OPERATION."
  `(let ((start-time (float-time)))
     (prog1 (progn ,@body)
       (auto-modal--record-performance ,operation start-time))))

(defun auto-modal-performance-report ()
  "Display performance report."
  (interactive)
  (with-current-buffer (get-buffer-create "*Auto-modal Performance*")
    (erase-buffer)
    (insert "Auto-modal Performance Report\n")
    (insert "==============================\n\n")
    (maphash (lambda (operation times)
               (when times
                 (let ((avg (/ (apply #'+ times) (length times)))
                       (max-time (apply #'max times))
                       (min-time (apply #'min times)))
                   (insert (format "Operation: %s\n" operation))
                   (insert (format "  Average: %.4f seconds\n" avg))
                   (insert (format "  Maximum: %.4f seconds\n" max-time))
                   (insert (format "  Minimum: %.4f seconds\n" min-time))
                   (insert (format "  Count: %d\n\n" (length times))))))
             auto-modal--performance-stats)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; Optimization Settings

(defcustom auto-modal-use-performance-optimizations t
  "Whether to use performance optimizations."
  :type 'boolean
  :group 'auto-modal)

(defcustom auto-modal-cache-timeout 0.1
  "Timeout for predicate cache in seconds."
  :type 'number
  :group 'auto-modal)

(defcustom auto-modal-batch-operations t
  "Whether to batch operations for better performance."
  :type 'boolean
  :group 'auto-modal)

;;; Initialization

(defun auto-modal-performance-init ()
  "Initialize performance optimizations."
  (when auto-modal-use-performance-optimizations
    (setq auto-modal--cache-timeout auto-modal-cache-timeout)
    
    ;; Replace standard functions with optimized versions
    (advice-add 'auto-modal-is-triggerp :override #'auto-modal-is-triggerp-cached)
    (advice-add 'auto-modal-post-command-function :override #'auto-modal-post-command-function-optimized)
    
    ;; Process lazy bindings
    (auto-modal--process-lazy-bindings)
    
    ;; Setup garbage collection
    (run-with-idle-timer 30 t #'auto-modal--gc-collect)))

(defun auto-modal-performance-cleanup ()
  "Clean up performance optimizations."
  (advice-remove 'auto-modal-is-triggerp #'auto-modal-is-triggerp-cached)
  (advice-remove 'auto-modal-post-command-function #'auto-modal-post-command-function-optimized)
  
  (when auto-modal--hook-timer
    (cancel-timer auto-modal--hook-timer)
    (setq auto-modal--hook-timer nil))
  
  (clrhash auto-modal--predicate-cache)
  (clrhash auto-modal--performance-stats))

;;; Hooks

(add-hook 'auto-modal-turn-on-hook #'auto-modal-performance-init)
(add-hook 'auto-modal-turn-off-hook #'auto-modal-performance-cleanup)

(provide 'auto-modal-performance)
;;; auto-modal-performance.el ends here