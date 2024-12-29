(defvar auto-modal-vi-flag nil)

(defvar auto-modal-vi-keybinds
  '(("i" auto-modal-vi-insert-mode)
    ("j" next-line)
    ("k" previous-line)
    ("h" backward-char)
    ("l" forward-char)
    ("w" forward-word)
    ("b" backward-word)))

(defun auto-modal-vi-pred ()
  "Enable vi normal mode at every place."
  t)

(defun auto-modal-vi-normal-mode ()
  (setq auto-modal-vi-flag t)
  (dolist (keybind auto-modal-vi-keybinds)
    (apply 'auto-modal-bind-key
           (car keybind) 'global 'auto-modal-vi-pred (cdr keybind))))

(defun auto-modal-vi-insert-mode ()
  (setq auto-modal-vi-flag nil)
  ;; clear all keybinds with 'auto-modal-vi-pred predicate
  (auto-modal-unbind-with-predicate 'auto-modal-vi-pred))

(defun auto-modal-vi-mode-toogle ()
  (interactive)
  (if auto-modal-vi-flag
      (progn
        (setq auto-modal-vi-flag nil)
        (auto-modal-vi-insert-mode))
    (setq auto-modal-vi-flag t)
    (auto-modal-vi-normal-mode)))

;;;###autoload
(define-minor-mode auto-modal-vi-mode
  "Auto-modal vi mode"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (bind-key (kbd "<escape>") 'auto-modal-vi-mode-toogle map)
            map)
  (if auto-modal-mode
      (if auto-modal-vi-mode
          (auto-modal-vi-normal-mode)
        (auto-modal-vi-insert-mode))
    (auto-modal-vi-insert-mode)
    (setq auto-modal-vi-mode nil)
    (error "auto-modal-mode is not turned on!")))

(provide 'auto-modal-vi-mode)
