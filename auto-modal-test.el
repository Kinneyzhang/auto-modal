;;; auto-modal-test.el --- Tests for auto-modal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; This file is part of auto-modal.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Test suite for auto-modal package.

;;; Code:

(require 'ert)
(require 'auto-modal-core)
(require 'auto-modal-cursor)
(require 'auto-modal-keybind)

;;; Test Utilities

(defun auto-modal-test-with-temp-buffer (content &rest body)
  "Execute BODY in a temporary buffer with CONTENT."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (emacs-lisp-mode)
    (eval `(progn ,@body))))

(defmacro auto-modal-test-with-mode (mode &rest body)
  "Execute BODY with MODE enabled."
  `(let ((auto-modal-data nil))
     (unwind-protect
         (progn
           (funcall ,mode)
           ,@body)
       (when (fboundp ,mode)
         (funcall ,mode -1)))))

;;; Core Tests

(ert-deftest auto-modal-test-major-mode-chain ()
  "Test major mode chain functionality."
  (should (member 'fundamental-mode (major-mode-chain 'emacs-lisp-mode)))
  (should (member 'prog-mode (major-mode-chain 'emacs-lisp-mode)))
  (should (member 'lisp-mode (major-mode-chain 'emacs-lisp-mode))))

(ert-deftest auto-modal-test-major-mode-derived-p ()
  "Test major mode derivation checking."
  (should (major-mode-derived-p 'fundamental-mode 'emacs-lisp-mode))
  (should (major-mode-derived-p 'prog-mode 'emacs-lisp-mode))
  (should-not (major-mode-derived-p 'text-mode 'emacs-lisp-mode)))

(ert-deftest auto-modal-test-keybind-structure ()
  "Test keybind structure creation and access."
  (let ((keybind (make-auto-modal-keybind
                  :key-name "j"
                  :mode 'emacs-lisp-mode
                  :predicate (lambda () t)
                  :function 'next-line
                  :args nil
                  :override-p nil)))
    (should (string= "j" (auto-modal-keybind-key-name keybind)))
    (should (eq 'emacs-lisp-mode (auto-modal-keybind-mode keybind)))
    (should (eq 'next-line (auto-modal-keybind-function keybind)))
    (should (null (auto-modal-keybind-args keybind)))
    (should (null (auto-modal-keybind-override-p keybind)))))

(ert-deftest auto-modal-test-bind-key ()
  "Test key binding functionality."
  (let ((auto-modal-data nil))
    (auto-modal-bind-key "j" 'emacs-lisp-mode (lambda () t) 'next-line)
    (should (= 1 (length auto-modal-data)))
    (should (auto-modal-has-key-p "j"))
    (should (member "j" (auto-modal-all-keys)))))

(ert-deftest auto-modal-test-unbind-key ()
  "Test key unbinding functionality."
  (let ((auto-modal-data nil))
    (auto-modal-bind-key "j" 'emacs-lisp-mode (lambda () t) 'next-line)
    (auto-modal-unbind-key "j" 'emacs-lisp-mode (lambda () t) 'next-line)
    (should (= 0 (length auto-modal-data)))
    (should-not (auto-modal-has-key-p "j"))))

(ert-deftest auto-modal-test-key-command ()
  "Test key command resolution."
  (let ((auto-modal-data nil))
    (auto-modal-bind-key "j" 'emacs-lisp-mode (lambda () t) 'next-line)
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((command (auto-modal-key-command "j")))
        (should (eq 'next-line (car command)))))))

(ert-deftest auto-modal-test-functions-data ()
  "Test functions data retrieval."
  (let ((auto-modal-data nil))
    (auto-modal-bind-key "j" 'emacs-lisp-mode (lambda () t) 'next-line)
    (auto-modal-bind-key "k" 'fundamental-mode (lambda () t) 'previous-line)
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((data (auto-modal-functions-data)))
        (should (= 2 (length data)))  ; Should include both bindings due to inheritance
        (should (member "j" (mapcar #'car data)))
        (should (member "k" (mapcar #'car data)))))))

;;; Cursor Tests

(ert-deftest auto-modal-test-cursor-type-validation ()
  "Test cursor type validation."
  (should (eq 'box (auto-modal-should-cursor-type 'box)))
  (should (eq 'bar (auto-modal-should-cursor-type 'bar)))
  (should (eq (auto-modal-default-cursor-type) 
              (auto-modal-should-cursor-type 'default))))

(ert-deftest auto-modal-test-cursor-color-validation ()
  "Test cursor color validation."
  (should (string= auto-modal-default-cursor-color
                   (auto-modal-should-cursor-color nil)))
  (should (string= "red" (auto-modal-should-cursor-color "red"))))

;;; Predicate Tests

(ert-deftest auto-modal-test-trigger-detection ()
  "Test trigger condition detection."
  (let ((auto-modal-data nil))
    (auto-modal-bind-key "j" 'fundamental-mode (lambda () t) 'next-line)
    (with-temp-buffer
      (should (auto-modal-is-triggerp)))
    
    (setq auto-modal-data nil)
    (auto-modal-bind-key "j" 'fundamental-mode (lambda () nil) 'next-line)
    (with-temp-buffer
      (should-not (auto-modal-is-triggerp)))))

(ert-deftest auto-modal-test-trigger-functions ()
  "Test trigger functions retrieval."
  (let ((auto-modal-data nil))
    (auto-modal-bind-key "j" 'fundamental-mode (lambda () t) 'next-line)
    (auto-modal-bind-key "k" 'fundamental-mode (lambda () nil) 'previous-line)
    (with-temp-buffer
      (let ((funcs (auto-modal-trigger-functions)))
        (should (= 1 (length funcs)))
        (should (string= "j" (caar funcs)))))))

;;; Integration Tests

(ert-deftest auto-modal-test-mode-specific-bindings ()
  "Test mode-specific binding behavior."
  (let ((auto-modal-data nil))
    (auto-modal-bind-key "j" 'emacs-lisp-mode (lambda () t) 'next-line)
    (auto-modal-bind-key "j" 'fundamental-mode (lambda () t) 'previous-line)
    
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((command (auto-modal-key-command "j")))
        (should (eq 'next-line (car command)))))
    
    (with-temp-buffer
      (fundamental-mode)
      (let ((command (auto-modal-key-command "j")))
        (should (eq 'previous-line (car command)))))))

(ert-deftest auto-modal-test-predicate-with-args ()
  "Test functions with arguments."
  (let ((auto-modal-data nil))
    (auto-modal-bind-key "j" 'fundamental-mode (lambda () t) '(forward-line 2))
    (with-temp-buffer
      (let ((command (auto-modal-key-command "j")))
        (should (eq 'forward-line (car command)))
        (should (= 2 (cadr command)))))))

(ert-deftest auto-modal-test-help-key-protection ()
  "Test that help key cannot be bound."
  (let ((auto-modal-data nil))
    (should-error (auto-modal-bind-key "?" 'fundamental-mode (lambda () t) 'next-line))))

;;; Error Handling Tests

(ert-deftest auto-modal-test-validation-errors ()
  "Test validation error handling."
  (should-error (auto-modal-bind-key "j" 'fundamental-mode 'not-a-function 'next-line))
  (should-error (auto-modal-bind-key "j" 'fundamental-mode (lambda () t) 'not-a-function)))

(ert-deftest auto-modal-test-unbind-with-predicate ()
  "Test unbinding with predicate."
  (let ((auto-modal-data nil)
        (test-pred (lambda () t)))
    (auto-modal-bind-key "j" 'fundamental-mode test-pred 'next-line)
    (auto-modal-bind-key "k" 'fundamental-mode test-pred 'previous-line)
    (auto-modal-bind-key "l" 'fundamental-mode (lambda () nil) 'forward-char)
    
    (should (= 3 (length auto-modal-data)))
    (auto-modal-unbind-with-predicate test-pred)
    (should (= 1 (length auto-modal-data)))))

(provide 'auto-modal-test)
;;; auto-modal-test.el ends here