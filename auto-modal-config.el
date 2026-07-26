;;; auto-modal-config.el --- Example configuration for auto-modal  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Kinney Zhang

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A ready-to-use example configuration for auto-modal.  Loading this
;; file registers three groups of keybindings:
;;
;; - Beginning-of-line bindings: when point is at the beginning of a
;;   non-empty line, "j"/"k" navigate between lines, "SPC" switches
;;   to insert state, and a few other keys run handy commands.
;; - Region bindings: when the region is active, "u", "d" and "c"
;;   operate on it.
;; - S-expression bindings (emacs-lisp-mode): when point is next to a
;;   parenthesis, "f"/"b"/"j"/"k"/"i"/"o" navigate the paren
;;   structure, "s" jumps to the balanced paren, "n" inserts a new
;;   paren line and ";" comments the expression out.
;;
;; It also provides `auto-modal-vi-mode', a minimal vi emulation
;; built on auto-modal with an always-true predicate.
;;
;; Treat this file as a starting point: copy what you like into your
;; own configuration and adapt it.

;;; Code:

(require 'auto-modal)

;;;; Beginning-of-line bindings

(defun auto-modal-bolp ()
  "Return non-nil when point is at the beginning of a non-empty line."
  (and (bolp) (not (looking-at-p "^$"))))

(defun auto-modal-next-line ()
  "Move point to the beginning of the next non-empty line.
Stay put when only empty lines (or nothing) follow."
  (interactive)
  (let ((target (save-excursion
                  (catch 'stop
                    (while t
                      (forward-line 1)
                      (cond ((eobp) (throw 'stop nil))
                            ((not (looking-at-p "^$"))
                             (throw 'stop (point)))))))))
    (when target (goto-char target))))

(defun auto-modal-previous-line ()
  "Move point to the beginning of the previous non-empty line.
Stay put when only empty lines (or nothing) precede."
  (interactive)
  (let ((target (save-excursion
                  (catch 'stop
                    (while t
                      (when (bobp) (throw 'stop nil))
                      (forward-line -1)
                      (unless (looking-at-p "^$")
                        (throw 'stop (point))))))))
    (when target (goto-char target))))

(auto-modal-bind-key "j" 'global 'auto-modal-bolp 'auto-modal-next-line)
(auto-modal-bind-key "k" 'global 'auto-modal-bolp 'auto-modal-previous-line)
(auto-modal-bind-key "l" 'global 'auto-modal-bolp 'avy-goto-line)
(auto-modal-bind-key "c" 'global 'auto-modal-bolp 'avy-goto-char-timer)
(auto-modal-bind-key "o" 'global 'auto-modal-bolp '(other-window 1))
(auto-modal-bind-key "SPC" 'global 'auto-modal-bolp 'auto-modal-enable-insert)
(auto-modal-bind-key "<" 'global 'auto-modal-bolp 'backward-page)
(auto-modal-bind-key ">" 'global 'auto-modal-bolp 'forward-page)
(auto-modal-bind-key "v" 'global 'auto-modal-bolp 'set-mark-command)
(auto-modal-bind-key "z" 'global 'auto-modal-bolp 'read-only-mode)

;;;; Region bindings

;; Registered after the beginning-of-line bindings so that when both
;; predicates hold (region active, point at bol) the region binding
;; wins the tie as the most recently added.

(auto-modal-bind-key "u" 'global 'use-region-p 'upcase-dwim)
(auto-modal-bind-key "d" 'global 'use-region-p 'downcase-dwim)
(auto-modal-bind-key "c" 'global 'use-region-p 'kill-ring-save)

;;;; A minimal vi emulation

(defvar auto-modal-vi-keybinds
  '(("i" auto-modal-vi-insert-mode)
    ("j" next-line)
    ("k" previous-line)
    ("h" backward-char)
    ("l" forward-char)
    ("w" forward-word)
    ("b" backward-word))
  "Keybindings installed by `auto-modal-vi-normal-mode'.
Each element is a list (KEY FUNCTION ARGS...).")

(defvar auto-modal-vi-insert-flag nil
  "Non-nil means `auto-modal-vi-mode' is in vi insert state.")

(defun auto-modal-vi-pred ()
  "Predicate of vi emulation bindings: always true."
  t)

(defun auto-modal-vi-normal-mode ()
  "Enter the vi normal state of `auto-modal-vi-mode'."
  (setq auto-modal-vi-insert-flag nil)
  (dolist (keybind auto-modal-vi-keybinds)
    ;; (cdr keybind) is (FUNCTION ARGS...) — exactly the list form
    ;; auto-modal-bind-key accepts as FUNCTION-ARGS.
    (auto-modal-bind-key (car keybind) 'global 'auto-modal-vi-pred
                         (cdr keybind))))

(defun auto-modal-vi-insert-mode ()
  "Enter the vi insert state of `auto-modal-vi-mode'."
  (setq auto-modal-vi-insert-flag t)
  (auto-modal-unbind-with-predicate 'auto-modal-vi-pred))

(defun auto-modal-vi-mode-toggle ()
  "Toggle between the vi normal and insert states."
  (interactive)
  (if auto-modal-vi-insert-flag
      (auto-modal-vi-normal-mode)
    (auto-modal-vi-insert-mode)))

(define-obsolete-function-alias 'auto-modal-vi-mode-toogle
  #'auto-modal-vi-mode-toggle "1.0.0")

(defvar auto-modal-vi-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<escape>") #'auto-modal-vi-mode-toggle)
    map)
  "Keymap of `auto-modal-vi-mode'.")

;;;###autoload
(define-minor-mode auto-modal-vi-mode
  "A minimal vi emulation built on auto-modal.
In vi normal state the keys in `auto-modal-vi-keybinds' are active
in every buffer that has `auto-modal-mode' enabled (enable
`global-auto-modal-mode' to get them everywhere); press \"i\" to
enter vi insert state and <escape> to toggle back.  On terminal
frames ESC is a prefix key, so bind another toggle key there."
  :global t
  :keymap auto-modal-vi-keymap
  :group 'auto-modal
  (if auto-modal-vi-mode
      (progn
        (unless auto-modal-mode (auto-modal-mode 1))
        (auto-modal-vi-normal-mode))
    (auto-modal-vi-insert-mode)
    (setq auto-modal-vi-insert-flag nil)))

;;;; S-expression navigation (emacs-lisp-mode)

(defun auto-modal-sexp-left-paren-p ()
  "Return the paren depth when the char after point is a left paren.
Return nil inside strings and comments, or when the char after
point is not an opening parenthesis."
  (and-let* ((char (char-after))
             ((char-equal char ?\())
             (state (syntax-ppss))
             ((not (nth 3 state)))
             ((not (nth 4 state))))
    (nth 0 state)))

(defun auto-modal-sexp-right-paren-p ()
  "Return the paren depth when the char before point is a right paren.
Return nil inside strings and comments, or when the char before
point is not a closing parenthesis."
  (and-let* ((char (char-before))
             ((char-equal char ?\)))
             (state (save-excursion
                      (syntax-ppss (1- (point)))))
             ((not (nth 3 state)))
             ((not (nth 4 state))))
    (1- (nth 0 state))))

(defun auto-modal-sexp-around-paren-p ()
  "Return non-nil when point is right next to a paren of an S-expression."
  (or (auto-modal-sexp-left-paren-p)
      (auto-modal-sexp-right-paren-p)))

(defun auto-modal-sexp--side ()
  "Return a cons (PREDICATE . PAREN-STRING) for the paren at point."
  (cond ((auto-modal-sexp-left-paren-p)
         (cons 'auto-modal-sexp-left-paren-p "("))
        ((auto-modal-sexp-right-paren-p)
         (cons 'auto-modal-sexp-right-paren-p ")"))))

(defun auto-modal-sexp--forward (&optional backwardp)
  "Move to the next paren of the same side, searching forward.
With BACKWARDP non-nil, search backward instead."
  (let* ((search-func (if backwardp
                          're-search-backward
                        're-search-forward))
         (side (auto-modal-sexp--side))
         (predicate (car side))
         (paren (cdr side))
         (origin (point)))
    (goto-char
     (save-excursion
       (catch 'return
         (while (funcall search-func paren nil t)
           (when-let* ((found (if (eq predicate 'auto-modal-sexp-left-paren-p)
                                  (match-beginning 0)
                                (match-end 0)))
                       ((not (= found origin)))
                       ((save-excursion
                          (goto-char found)
                          (funcall predicate))))
             (throw 'return found)))
         origin)))))

(defun auto-modal-sexp-forward ()
  "Move forward to the next paren of the same side."
  (interactive)
  (auto-modal-sexp--forward))

(defun auto-modal-sexp-backward ()
  "Move backward to the previous paren of the same side."
  (interactive)
  (auto-modal-sexp--forward t))

(defun auto-modal-sexp-balance ()
  "Jump to the paren balancing the one at point."
  (interactive)
  (if (auto-modal-sexp-left-paren-p)
      (forward-sexp)
    (backward-sexp)))

(defun auto-modal-sexp--down (&optional backwardp)
  "Move to the next paren of the same side and depth.
With BACKWARDP non-nil, search backward instead.  Stop before
leaving the enclosing expression."
  (let* ((search-func (if backwardp
                          're-search-backward
                        're-search-forward))
         (side (auto-modal-sexp--side))
         (predicate (car side))
         (paren (cdr side))
         (origin (point))
         (origin-depth (funcall predicate)))
    (goto-char
     (save-excursion
       (catch 'return
         (while (funcall search-func paren nil t)
           (when-let* ((found (if (eq predicate 'auto-modal-sexp-left-paren-p)
                                  (match-beginning 0)
                                (match-end 0)))
                       (depth (save-excursion
                                (goto-char found)
                                (funcall predicate))))
             (if (< depth origin-depth)
                 (throw 'return origin)
               (when (and (not (= found origin))
                          (= depth origin-depth))
                 (throw 'return found)))))
         origin)))))

(defun auto-modal-sexp-down ()
  "Move forward to the next paren at the same depth."
  (interactive)
  (auto-modal-sexp--down))

(defun auto-modal-sexp-up ()
  "Move backward to the previous paren at the same depth."
  (interactive)
  (auto-modal-sexp--down t))

(defun auto-modal-sexp--into (&optional backwardp)
  "Move to the next deeper paren of the same side.
With BACKWARDP non-nil, move to the enclosing (shallower) paren
instead."
  (let* ((search-func (if backwardp
                          're-search-backward
                        're-search-forward))
         (side (auto-modal-sexp--side))
         (predicate (car side))
         (paren (cdr side))
         (origin (point))
         (origin-depth (funcall predicate)))
    (goto-char
     (save-excursion
       (catch 'return
         (while (funcall search-func paren nil t)
           (when-let* ((found (if (eq predicate 'auto-modal-sexp-left-paren-p)
                                  (match-beginning 0)
                                (match-end 0)))
                       (depth (save-excursion
                                (goto-char found)
                                (funcall predicate))))
             (if backwardp
                 (when (and (not (= found origin))
                            (< depth origin-depth))
                   (throw 'return found))
               (if (< depth origin-depth)
                   (throw 'return origin)
                 (when (and (not (= found origin))
                            (> depth origin-depth))
                   (throw 'return found))))))
         origin)))))

(defun auto-modal-sexp-into ()
  "Move forward into the next deeper expression."
  (interactive)
  (auto-modal-sexp--into))

(defun auto-modal-sexp-outside ()
  "Move backward out to the enclosing expression."
  (interactive)
  (auto-modal-sexp--into t))

(defun auto-modal-sexp-newline-paren ()
  "Insert a new pair of parens on a fresh line and place point inside."
  (interactive)
  (if (auto-modal-sexp-left-paren-p)
      (progn
        (insert "()")
        (backward-char 1)
        (save-excursion
          (forward-char 1)
          (newline-and-indent)))
    (newline-and-indent)
    (insert "()")
    (backward-char 1)))

(defun auto-modal-sexp-comment ()
  "Comment out the expression delimited by the paren at point."
  (interactive)
  (let ((start (point))
        end)
    (auto-modal-sexp-balance)
    (setq end (point))
    (if (< start end)
        (comment-region start end)
      (comment-region end start))))

(auto-modal-bind-key "f" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p
                     'auto-modal-sexp-forward)
(auto-modal-bind-key "b" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p
                     'auto-modal-sexp-backward)
(auto-modal-bind-key "j" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p
                     'auto-modal-sexp-down)
(auto-modal-bind-key "k" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p
                     'auto-modal-sexp-up)
(auto-modal-bind-key "i" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p
                     'auto-modal-sexp-into)
(auto-modal-bind-key "o" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p
                     'auto-modal-sexp-outside)
(auto-modal-bind-key "s" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p
                     'auto-modal-sexp-balance)
(auto-modal-bind-key "n" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p
                     'auto-modal-sexp-newline-paren)
(auto-modal-bind-key ";" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p
                     'auto-modal-sexp-comment)
(auto-modal-bind-key "SPC" 'emacs-lisp-mode 'auto-modal-sexp-around-paren-p
                     'auto-modal-enable-insert)

(provide 'auto-modal-config)
;;; auto-modal-config.el ends here
