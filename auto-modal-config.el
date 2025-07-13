(require 'auto-modal)
(require 'auto-modal-presets)

;;; Enhanced Configuration Example

;; Basic setup with programming focus
(auto-modal-setup-programming)

;; Additional customizations
(setq auto-modal-enable-log t)
(setq auto-modal-enable-keyhint t)

;; Cursor customization
(setq auto-modal-control-cursor-type 'box)
(setq auto-modal-insert-cursor-type 'bar)
(setq auto-modal-control-cursor-color '("red" . "orange"))
(setq auto-modal-insert-cursor-color '("blue" . "cyan"))

;;; Enhanced Navigation Functions

(defun auto-modal-smart-next-line ()
  "Smart next line navigation that skips comments and empty lines."
  (interactive)
  (forward-line 1)
  (while (and (not (eobp))
              (or (looking-at "^[[:space:]]*$")
                  (looking-at "^[[:space:]]*[#;]")))
    (forward-line 1))
  (back-to-indentation))

(defun auto-modal-smart-previous-line ()
  "Smart previous line navigation that skips comments and empty lines."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp))
              (or (looking-at "^[[:space:]]*$")
                  (looking-at "^[[:space:]]*[#;]")))
    (forward-line -1))
  (back-to-indentation))

;; Enhanced navigation bindings
(auto-modal-bind-key "J" 'global 'auto-modal-bolp 'auto-modal-smart-next-line)
(auto-modal-bind-key "K" 'global 'auto-modal-bolp 'auto-modal-smart-previous-line)

;;; Project Management Integration

(when (fboundp 'project-find-file)
  (auto-modal-bind-key "F" 'global 'auto-modal-bolp 'project-find-file))

(when (fboundp 'project-switch-project)
  (auto-modal-bind-key "P" 'global 'auto-modal-bolp 'project-switch-project))

;;; Git Integration

(when (fboundp 'magit-status)
  (auto-modal-bind-key "g" 'global 'auto-modal-bolp 'magit-status))

;;; Enhanced Selection Operations

(defun auto-modal-enhance-region-setup ()
  "Enhanced region operations with idle cursor update."
  (when (use-region-p)
    (run-with-idle-timer 0.1 nil 'auto-modal-set-cursor)))

(add-hook 'post-command-hook 'auto-modal-enhance-region-setup)

;; Enhanced selection bindings
(auto-modal-bind-key "x" 'global 'use-region-p 'kill-region)
(auto-modal-bind-key "y" 'global 'use-region-p 'kill-ring-save)
(auto-modal-bind-key "p" 'global 'use-region-p 'yank)

;;; vi-mode

(defvar auto-modal-vi-keybinds
  '(("i" auto-modal-vi-insert-mode)
    ("j" next-line)
    ("k" previous-line)
    ("h" backward-char)
    ("l" forward-char)
    ("w" forward-word)
    ("b" backward-word)))

(defvar auto-modal-vi-insert-flag nil
  "When `auto-modal-vi-insert-flag' is nil,
it's in vi normal mode. Otherwise, it's in
vi insert mode.")

(defun auto-modal-vi-pred () t)

(defun auto-modal-vi-normal-mode ()
  (setq auto-modal-vi-insert-flag nil)
  (dolist (keybind auto-modal-vi-keybinds)
    (apply 'auto-modal-bind-key
           (car keybind) 'global 'auto-modal-vi-pred (cdr keybind))))

(defun auto-modal-vi-insert-mode ()
  (setq auto-modal-vi-insert-flag t)
  (auto-modal-unbind-with-predicate 'auto-modal-vi-pred))

(defun auto-modal-vi-mode-toogle ()
  (interactive)
  (if auto-modal-vi-insert-flag
      (auto-modal-vi-normal-mode)
    (auto-modal-vi-insert-mode)))

(defvar auto-modal-vi-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<escape>") 'auto-modal-vi-mode-toogle)
    map))

;;;###autoload
(define-minor-mode auto-modal-vi-mode
  "Auto-modal vi mode"
  :global t
  :keymap auto-modal-vi-keymap
  (unless auto-modal-mode (auto-modal-mode 1))
  (if auto-modal-vi-mode
      (auto-modal-vi-normal-mode)
    (auto-modal-vi-insert-mode)))

;;; sexp-mode

(defun sexp-left-paren-p ()
  "Judge if the char after cursor is
a left parenthesis of S expression."
  (and-let* ((char (char-after))
             ((char-equal char ?\())
             (state (syntax-ppss))
             ((not (nth 3 state)))
             ((not (nth 4 state))))
    (nth 0 state)))

(defun sexp-right-paren-p ()
  "Judge if the char before cursor is
a right parenthesis of S expression."
  (and-let* ((char (char-before))
             ((char-equal char ?\)))
             (state (save-excursion
                      (syntax-ppss (1- (point)))))
             ((not (nth 3 state)))
             ((not (nth 4 state))))
    (1- (nth 0 state))))

(defun sexp-around-paren-p ()
  (or (sexp-left-paren-p) (sexp-right-paren-p)))

(defun sexp--left-or-right ()
  (cond ((sexp-left-paren-p) (cons 'sexp-left-paren-p "("))
        ((sexp-right-paren-p) (cons 'sexp-right-paren-p ")"))))

(defun sexp--forward (&optional backwardp)
  (let* ((search-func (if backwardp
                          're-search-backward
                        're-search-forward))
         (left-or-right (sexp--left-or-right))
         (func (car left-or-right))
         (char (cdr left-or-right))
         (pos (point)))
    (goto-char
     (save-excursion
       (catch 'return
         (while (funcall search-func char nil t)
           (when-let* ((lr-pos (if (eq func 'sexp-left-paren-p)
                                   (match-beginning 0)
                                 (match-end 0)))
                       ((not (= lr-pos pos)))
                       ((save-excursion
                          (goto-char lr-pos)
                          (funcall func))))
             (throw 'return lr-pos)))
         pos)))))

(defun sexp-forward ()
  (sexp--forward))

(defun sexp-backward ()
  (sexp--forward t))

(defun sexp-balance ()
  (if (sexp-left-paren-p)
      (forward-sexp)
    (backward-sexp)))

(defun sexp--down (&optional backwardp)
  (let* ((search-func (if backwardp
                          're-search-backward
                        're-search-forward))
         (left-or-right (sexp--left-or-right))
         (func (car left-or-right))
         (char (cdr left-or-right))
         (curr-pos (point))
         (curr-depth (funcall func)))
    (goto-char
     (save-excursion
       (catch 'return
         (while (funcall search-func char nil t)
           (when-let* ((lr-pos (if (eq func 'sexp-left-paren-p)
                                   (match-beginning 0)
                                 (match-end 0)))
                       (depth (save-excursion
                                (goto-char lr-pos)
                                (funcall func))))
             (if (< depth curr-depth)
                 (throw 'return curr-pos)
               (when (and (not (= lr-pos curr-pos))
                          (= depth curr-depth))
                 (throw 'return lr-pos)))))
         curr-pos)))))

(defun sexp-down ()
  (sexp--down))

(defun sexp-up ()
  (sexp--down t))

(defun sexp--into (&optional backwardp)
  (let* ((search-func (if backwardp
                          're-search-backward
                        're-search-forward))
         (left-or-right (sexp--left-or-right))
         (func (car left-or-right))
         (char (cdr left-or-right))
         (curr-pos (point))
         (curr-depth (funcall func)))
    (goto-char
     (save-excursion
       (catch 'return
         (while (funcall search-func char nil t)
           (when-let* ((lr-pos (if (eq func 'sexp-left-paren-p)
                                   (match-beginning 0)
                                 (match-end 0)))
                       (depth (save-excursion
                                (goto-char lr-pos)
                                (funcall func))))
             (if backwardp
                 (when (and (not (= lr-pos curr-pos))
                            (< depth curr-depth))
                   (throw 'return lr-pos))
               (if (< depth curr-depth)
                   (throw 'return curr-pos)
                 (when (and (not (= lr-pos curr-pos))
                            (> depth curr-depth))
                   (throw 'return lr-pos))))))
         curr-pos)))))

(defun sexp-into ()
  (sexp--into))

(defun sexp-outside ()
  (sexp--into t))

(defun sexp-newline-paren ()
  (if (sexp-left-paren-p)
      (progn
        (insert "()")
        (backward-char 1)
        (save-excursion
          (forward-char 1)
          (newline-and-indent)))
    (newline-and-indent)
    (insert "()")
    (backward-char 1)))

(defun sexp-comment ()
  (let ((p1 (point))
        p2)
    (sexp-balance)
    (setq p2 (point))
    (if (< p1 p2)
        (comment-region p1 p2)
      (comment-region p2 p1))))

(auto-modal-bind-key "f" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-forward)
(auto-modal-bind-key "b" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-backward)
(auto-modal-bind-key "j" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-down)
(auto-modal-bind-key "k" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-up)
(auto-modal-bind-key "i" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-into)
(auto-modal-bind-key "o" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-outside)
(auto-modal-bind-key "s" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-balance)
(auto-modal-bind-key "n" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-newline-paren)
(auto-modal-bind-key ";" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-comment)
(auto-modal-bind-key "SPC" 'emacs-lisp-mode 'sexp-around-paren-p 'auto-modal-enable-insert)

(provide 'auto-modal-config)
