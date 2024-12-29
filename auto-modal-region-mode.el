(defvar auto-modal-region-keybinds
  '(("u" upcase-dwim 1)
    ("d" downcase-dwim 1)))

(defun auto-modal-use-region ()
  (dolist (keybind auto-modal-region-keybinds)
    (apply 'auto-modal-bind-key
           (car keybind) 'global 'use-region-p (cdr keybind))))

(defun auto-modal-cancel-region ()
  (auto-modal-unbind-with-predicate 'use-region-p))

;;;###autoload
(define-minor-mode auto-modal-region-mode
  "Auto-modal region mode"
  :global t
  (if auto-modal-region-mode
      (if auto-modal-mode
          (auto-modal-use-region)
        (auto-modal-cancel-region)
        (setq auto-modal-region-mode nil)
        (error "auto-modal-mode is not turned on!"))
    (auto-modal-cancel-region)))

(provide 'auto-modal-region-mode)
