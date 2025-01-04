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
(auto-modal-bind-key "o" 'global 'auto-modal-bolp 'other-window 1)
(auto-modal-bind-key "k" 'global 'auto-modal-bolp 'auto-modal-previous-line)
(auto-modal-bind-key "SPC" 'global 'auto-modal-bolp 'auto-modal-enable-insert)
(auto-modal-bind-key "f" 'global 'auto-modal-bolp 'counsel-find-file)
(auto-modal-bind-key "<" 'global 'auto-modal-bolp 'backward-page)
(auto-modal-bind-key ">" 'global 'auto-modal-bolp 'forward-page)

;;; vi-mode

(defvar auto-modal-vi-keybinds
  '(("i" auto-modal-vi-insert-mode)
    ("j" next-line)
    ("k" previous-line)
    ("z" forward-line 2)
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
  (let ((state (syntax-ppss)))
    (and (char-equal (char-after) ?\()
         ;; not in string
         (not (nth 3 state))
         ;; not in comment
         (not (nth 4 state)))))

(defun sexp-right-paren-p ()
  (let ((state (syntax-ppss)))
    (and (char-equal (char-after) ?\))
         ;; not in string
         (not (nth 3 state))
         ;; not in comment
         (not (nth 4 state)))))

(defun sexp-next ()
  (let ((depth (nth 0 (syntax-ppss))))
    (forward-char 1)
    (catch 'return
      (while t
        (let ((state (syntax-ppss)))
          (if (and (eq depth (nth 0 state))
                   (not (nth 3 state)) ; 不在字符串中
                   (not (nth 4 state)) ; 不在注释中
                   (char-equal (char-after) ?\())
              (throw 'return (point))
            (forward-char 1)))))))


(provide 'auto-modal-config)
