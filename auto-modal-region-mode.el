(defvar auto-modal-region-keybinds
  '(("u" upcase-dwim)
    ("d" downcase-dwim)
    ("c" kill-ring-save)))

(defun auto-modal-use-region ()
  (dolist (keybind auto-modal-region-keybinds)
    (apply 'auto-modal-bind-key
           (car keybind) 'global 'use-region-p (cdr keybind))))

(defun auto-modal-cancel-region ()
  (auto-modal-unbind-with-predicate 'use-region-p))

(defun auto-modal-set-buffer-cursor ()
  "Set cursor type correctly in all windows when
turning `auto-modal-mode' on and off."
  (interactive)
  (run-with-idle-timer
   0.1 nil
   (lambda ()
     (if (and auto-modal-mode
              (auto-modal-is-triggerp))
         (setq-local cursor-type auto-modal-control-cursor-type)
       (setq-local cursor-type auto-modal-insert-cursor-type)))))

;;;###autoload
(define-minor-mode auto-modal-region-mode
  "Auto-modal region mode"
  :global t
  (if auto-modal-region-mode
      (if auto-modal-mode
          (progn
            (add-hook 'post-command-hook 'auto-modal-set-buffer-cursor)
            (auto-modal-use-region))
        (remove-hook 'post-command-hook 'auto-modal-set-buffer-cursor)
        (auto-modal-cancel-region)
        (setq auto-modal-region-mode nil)
        (error "auto-modal-mode is not turned on!"))
    (remove-hook 'post-command-hook 'auto-modal-set-buffer-cursor)
    (auto-modal-cancel-region)))

(provide 'auto-modal-region-mode)
