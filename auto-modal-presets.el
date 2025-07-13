;;; auto-modal-presets.el --- Pre-configured presets for auto-modal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; This file is part of auto-modal.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Pre-configured presets for common editing patterns in auto-modal.

;;; Code:

(require 'auto-modal-core)
(require 'auto-modal-keybind)

;;; Common Predicates

(defun auto-modal-bolp ()
  "Return t if cursor is at the beginning of a non-empty line."
  (and (bolp) (not (looking-at "^$"))))

(defun auto-modal-eolp ()
  "Return t if cursor is at the end of a non-empty line."
  (and (eolp) (not (looking-at "^$"))))

(defun auto-modal-empty-line-p ()
  "Return t if cursor is on an empty line."
  (looking-at "^$"))

(defun auto-modal-whitespace-line-p ()
  "Return t if cursor is on a line with only whitespace."
  (looking-at "^[[:space:]]*$"))

;;; Common Navigation Functions

(defun auto-modal-next-line ()
  "Move to the next non-empty line."
  (interactive)
  (unless (save-excursion (forward-line 1)
                          (= (point) (point-max)))
    (forward-line 1))
  (goto-char (line-beginning-position))
  (while (and (not (= (point) (point-max)))
              (looking-at "^$"))
    (auto-modal-next-line)))

(defun auto-modal-previous-line ()
  "Move to the previous non-empty line."
  (interactive)
  (forward-line -1)
  (goto-char (line-beginning-position))
  (while (and (not (= (point) (point-min)))
              (looking-at "^$"))
    (auto-modal-previous-line)))

(defun auto-modal-next-paragraph ()
  "Move to the next paragraph."
  (interactive)
  (forward-paragraph)
  (skip-chars-forward " \t\n"))

(defun auto-modal-previous-paragraph ()
  "Move to the previous paragraph."
  (interactive)
  (backward-paragraph)
  (skip-chars-backward " \t\n"))

;;; S-expression Support

(defun auto-modal-sexp-left-paren-p ()
  "Return t if cursor is before a left parenthesis in an s-expression."
  (and-let* ((char (char-after))
             ((char-equal char ?\())
             (state (syntax-ppss))
             ((not (nth 3 state)))
             ((not (nth 4 state))))
    (nth 0 state)))

(defun auto-modal-sexp-right-paren-p ()
  "Return t if cursor is after a right parenthesis in an s-expression."
  (and-let* ((char (char-before))
             ((char-equal char ?\)))
             (state (save-excursion
                      (syntax-ppss (1- (point)))))
             ((not (nth 3 state)))
             ((not (nth 4 state))))
    (1- (nth 0 state))))

(defun auto-modal-sexp-around-paren-p ()
  "Return t if cursor is around parentheses in an s-expression."
  (or (auto-modal-sexp-left-paren-p) (auto-modal-sexp-right-paren-p)))

;;; Preset Configurations

;;;###autoload
(defun auto-modal-setup-basic ()
  "Setup basic auto-modal configuration."
  (interactive)
  ;; Basic line navigation
  (auto-modal-bind-key "j" 'global 'auto-modal-bolp 'auto-modal-next-line)
  (auto-modal-bind-key "k" 'global 'auto-modal-bolp 'auto-modal-previous-line)
  (auto-modal-bind-key "SPC" 'global 'auto-modal-bolp 'auto-modal-enable-insert)
  
  ;; Window management
  (auto-modal-bind-key "o" 'global 'auto-modal-bolp '(other-window 1))
  (auto-modal-bind-key "O" 'global 'auto-modal-bolp '(other-window -1))
  
  ;; Buffer management
  (auto-modal-bind-key "b" 'global 'auto-modal-bolp 'switch-to-buffer)
  (auto-modal-bind-key "f" 'global 'auto-modal-bolp 'find-file)
  
  ;; Page navigation
  (auto-modal-bind-key "<" 'global 'auto-modal-bolp 'backward-page)
  (auto-modal-bind-key ">" 'global 'auto-modal-bolp 'forward-page)
  
  ;; Paragraph navigation
  (auto-modal-bind-key "{" 'global 'auto-modal-bolp 'auto-modal-previous-paragraph)
  (auto-modal-bind-key "}" 'global 'auto-modal-bolp 'auto-modal-next-paragraph)
  
  ;; Mark and region
  (auto-modal-bind-key "v" 'global 'auto-modal-bolp 'set-mark-command)
  (auto-modal-bind-key "u" 'global 'use-region-p 'upcase-dwim)
  (auto-modal-bind-key "d" 'global 'use-region-p 'downcase-dwim)
  (auto-modal-bind-key "c" 'global 'use-region-p 'kill-ring-save)
  
  ;; Toggle read-only
  (auto-modal-bind-key "r" 'global 'auto-modal-bolp 'read-only-mode)
  
  (message "Auto-modal basic setup complete"))

;;;###autoload
(defun auto-modal-setup-programming ()
  "Setup programming-focused auto-modal configuration."
  (interactive)
  ;; Include basic setup
  (auto-modal-setup-basic)
  
  ;; Jump to definitions and references
  (auto-modal-bind-key "." 'prog-mode 'auto-modal-bolp 'xref-find-definitions)
  (auto-modal-bind-key "," 'prog-mode 'auto-modal-bolp 'xref-pop-marker-stack)
  (auto-modal-bind-key "/" 'prog-mode 'auto-modal-bolp 'xref-find-references)
  
  ;; Compilation
  (auto-modal-bind-key "m" 'prog-mode 'auto-modal-bolp 'compile)
  (auto-modal-bind-key "n" 'prog-mode 'auto-modal-bolp 'next-error)
  (auto-modal-bind-key "p" 'prog-mode 'auto-modal-bolp 'previous-error)
  
  ;; Code navigation
  (auto-modal-bind-key "a" 'prog-mode 'auto-modal-bolp 'beginning-of-defun)
  (auto-modal-bind-key "e" 'prog-mode 'auto-modal-bolp 'end-of-defun)
  
  ;; Comments
  (auto-modal-bind-key ";" 'prog-mode 'auto-modal-bolp 'comment-line)
  
  (message "Auto-modal programming setup complete"))

;;;###autoload
(defun auto-modal-setup-lisp ()
  "Setup Lisp-focused auto-modal configuration."
  (interactive)
  ;; Include programming setup
  (auto-modal-setup-programming)
  
  ;; S-expression navigation
  (auto-modal-bind-key "f" 'lisp-mode 'auto-modal-sexp-around-paren-p 'forward-sexp)
  (auto-modal-bind-key "b" 'lisp-mode 'auto-modal-sexp-around-paren-p 'backward-sexp)
  (auto-modal-bind-key "u" 'lisp-mode 'auto-modal-sexp-around-paren-p 'backward-up-list)
  (auto-modal-bind-key "d" 'lisp-mode 'auto-modal-sexp-around-paren-p 'down-list)
  (auto-modal-bind-key "n" 'lisp-mode 'auto-modal-sexp-around-paren-p 'forward-list)
  (auto-modal-bind-key "p" 'lisp-mode 'auto-modal-sexp-around-paren-p 'backward-list)
  
  ;; S-expression editing
  (auto-modal-bind-key "k" 'lisp-mode 'auto-modal-sexp-around-paren-p 'kill-sexp)
  (auto-modal-bind-key "t" 'lisp-mode 'auto-modal-sexp-around-paren-p 'transpose-sexps)
  (auto-modal-bind-key "w" 'lisp-mode 'auto-modal-sexp-around-paren-p 'copy-sexp)
  
  ;; Evaluation
  (auto-modal-bind-key "x" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p 'eval-last-sexp)
  (auto-modal-bind-key "X" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p 'eval-defun)
  
  ;; Insert space for editing
  (auto-modal-bind-key "SPC" 'lisp-mode 'auto-modal-sexp-around-paren-p 'auto-modal-enable-insert)
  
  (message "Auto-modal Lisp setup complete"))

;;;###autoload
(defun auto-modal-setup-text ()
  "Setup text-editing focused auto-modal configuration."
  (interactive)
  ;; Include basic setup
  (auto-modal-setup-basic)
  
  ;; Text navigation
  (auto-modal-bind-key "w" 'text-mode 'auto-modal-bolp 'forward-word)
  (auto-modal-bind-key "W" 'text-mode 'auto-modal-bolp 'backward-word)
  (auto-modal-bind-key "s" 'text-mode 'auto-modal-bolp 'forward-sentence)
  (auto-modal-bind-key "S" 'text-mode 'auto-modal-bolp 'backward-sentence)
  
  ;; Spelling
  (auto-modal-bind-key "=" 'text-mode 'auto-modal-bolp 'ispell-word)
  (auto-modal-bind-key "+" 'text-mode 'auto-modal-bolp 'ispell-buffer)
  
  ;; Fill and formatting
  (auto-modal-bind-key "q" 'text-mode 'auto-modal-bolp 'fill-paragraph)
  (auto-modal-bind-key "Q" 'text-mode 'auto-modal-bolp 'fill-region)
  
  (message "Auto-modal text setup complete"))

;;;###autoload
(defun auto-modal-setup-org ()
  "Setup org-mode focused auto-modal configuration."
  (interactive)
  ;; Include text setup
  (auto-modal-setup-text)
  
  ;; Org navigation
  (auto-modal-bind-key "h" 'org-mode 'auto-modal-bolp 'outline-up-heading)
  (auto-modal-bind-key "j" 'org-mode 'auto-modal-bolp 'outline-next-visible-heading)
  (auto-modal-bind-key "k" 'org-mode 'auto-modal-bolp 'outline-previous-visible-heading)
  (auto-modal-bind-key "l" 'org-mode 'auto-modal-bolp 'outline-next-heading)
  
  ;; Org structure
  (auto-modal-bind-key "t" 'org-mode 'auto-modal-bolp 'org-todo)
  (auto-modal-bind-key "T" 'org-mode 'auto-modal-bolp 'org-show-todo-tree)
  (auto-modal-bind-key "d" 'org-mode 'auto-modal-bolp 'org-deadline)
  (auto-modal-bind-key "s" 'org-mode 'auto-modal-bolp 'org-schedule)
  
  ;; Org editing
  (auto-modal-bind-key "i" 'org-mode 'auto-modal-bolp 'org-insert-heading)
  (auto-modal-bind-key "I" 'org-mode 'auto-modal-bolp 'org-insert-subheading)
  (auto-modal-bind-key "r" 'org-mode 'auto-modal-bolp 'org-refile)
  (auto-modal-bind-key "a" 'org-mode 'auto-modal-bolp 'org-archive-subtree)
  
  ;; Org visibility
  (auto-modal-bind-key "TAB" 'org-mode 'auto-modal-bolp 'org-cycle)
  (auto-modal-bind-key "S-TAB" 'org-mode 'auto-modal-bolp 'org-global-cycle)
  
  (message "Auto-modal org setup complete"))

;;; Preset Selection

;;;###autoload
(defun auto-modal-setup-presets ()
  "Interactive setup for auto-modal presets."
  (interactive)
  (let ((choice (completing-read "Choose preset: "
                                 '("basic" "programming" "lisp" "text" "org")
                                 nil t)))
    (cond
     ((string= choice "basic") (auto-modal-setup-basic))
     ((string= choice "programming") (auto-modal-setup-programming))
     ((string= choice "lisp") (auto-modal-setup-lisp))
     ((string= choice "text") (auto-modal-setup-text))
     ((string= choice "org") (auto-modal-setup-org))
     (t (message "Invalid preset choice")))))

(provide 'auto-modal-presets)
;;; auto-modal-presets.el ends here