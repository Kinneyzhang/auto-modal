(require 'auto-modal)

;;; selection

(defun auto-modal-set-cursor-when-idle ()
  "Set cursor type correctly in current buffer
after idle time. It's useful when `use-region-p'
is the predicate function."
  (interactive)
  (when (use-region-p)
    (run-with-idle-timer 0.1 nil 'auto-modal-set-cursor)))

;; delay update cursor-type when use-region-p
(add-hook 'post-command-hook 'auto-modal-set-cursor-when-idle)

(auto-modal-bind-key "u" 'global 'use-region-p 'upcase-dwim)
(auto-modal-bind-key "d" 'global 'use-region-p 'downcase-dwim)
(auto-modal-bind-key "c" 'global 'use-region-p 'kill-ring-save)

;;; bol

(defun auto-modal-bolp ()
  (and (bolp) (not (looking-at "^$"))))

(defun auto-modal-next-line ()
  (interactive)
  (forward-line 1)
  (goto-char (line-beginning-position))
  (while (and (not (= (point) (point-max)))
              (looking-at "^$"))
    (auto-modal-next-line)))

(defun auto-modal-previous-line ()
  (interactive)
  (forward-line -1)
  (goto-char (line-beginning-position))
  (while (and (not (= (point) (point-max)))
              (looking-at "^$"))
    (auto-modal-previous-line)))

(auto-modal-bind-key "l" 'global 'auto-modal-bolp 'avy-goto-line)
(auto-modal-bind-key "c" 'global 'auto-modal-bolp 'avy-goto-char-timer)
(auto-modal-bind-key "j" 'global 'auto-modal-bolp 'auto-modal-next-line)
(auto-modal-bind-key "o" 'global 'auto-modal-bolp '(other-window 1))
(auto-modal-bind-key "k" 'global 'auto-modal-bolp 'auto-modal-previous-line)
(auto-modal-bind-key "SPC" 'global 'auto-modal-bolp 'auto-modal-enable-insert)
(auto-modal-bind-key "<" 'global 'auto-modal-bolp 'backward-page)
(auto-modal-bind-key ">" 'global 'auto-modal-bolp 'forward-page)
(auto-modal-bind-key "v" 'global 'auto-modal-bolp 'set-mark-command)
(auto-modal-bind-key "z" 'global 'auto-modal-bolp 'read-only-mode)

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
