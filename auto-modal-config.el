(require 'auto-modal)

;;; selection

(defun auto-modal-set-cursor-when-idle ()
  "Set cursor type correctly in current buffer
after idle time. It's useful when `use-region-p'
is the predicate function."
  (interactive)
  (run-with-idle-timer 0.1 nil 'auto-modal-set-cursor))

;; delay update cursor type when use-region-p
;; (add-hook 'post-command-hook 'auto-modal-set-cursor-when-idle)
;; (remove-hook 'post-command-hook 'auto-modal-set-cursor-when-idle)

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

;; (defun auto-modal-before-parensp ()
;;   (looking-at "("))

;; (defun auto-modal-next-function ()
;;   (when (save-excursion
;;           (re-search-forward "^(defun.+" nil t))
;;     (goto-char (match-beginning 0))))

;; (defun sexp-into ()
;;   "Go to the inner sexp."
;;   (when (re-search-forward "(" nil t)
;;     ))

;; (defun sexp-out ()
;;   "Go outside of current sexp.")

;; (defun sexp-next ()
;;   "Go to the next sexp in the same level."
;;   )

;; (defun sexp-previous ()
;;   "Go to the previous sexp in the same level.")

;; (defun sexp-end ()
;;   "Go to the end of a sexp.")

(provide 'auto-modal-config)
