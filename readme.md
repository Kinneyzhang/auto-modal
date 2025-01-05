In emacs, for commands that need to be used frequently, we tend to bind them to shortcut keys, which means that pressing a few key combinations completes the invocation of a complex function. The ability to bind arbitrary commands to shortcuts is one of the important reasons why emacs is considered flexible and efficient. The feature of shortcut keys in emacs is that you need to use prefix keys, such as `C-c` and `C-x`, which is not as “fast” as using single-letter keys in modal editing like vi. Users who are used to the modal switching method of vi may use solutions such as `evil-mode` and `meow` in emacs to continue this usage habit. I personally don't use “modal editing”, but I also find that single-letter keys are indeed efficient, so I thought about a solution that can use single-letter shortcuts but does not require active switching of modes. This led to the auto-modal package.

# What does auto-modal do

Auto-modal is called “automatic modal switching” as the name suggests. When the cursor position satisfies a specified assertion function, it automatically switches to “command mode” (here, command mode refers to the named execution that can be triggered by multiple predefined single-letter keys); when the cursor position does not satisfy the specified assertion function, it automatically switches to normal mode (here, normal mode refers to the state of using emacs normally). In these two switching processes, any other key bindings will not be affected.

In normal editing in emacs, letter (number, punctuation) keys are bound to `self-insert-command` by default, which inserts the typed character at the cursor position. Obviously, switching to command mode is inappropriate in this editing situation. So we need to find some rare cases in editing, in which we rarely type characters, and use them as trigger conditions for automatic mode switching. For example, at the beginning of a line with existing text or to the left of the left parenthesis in lisp-like languages. When we need to trigger a command with the letter key, we can simply achieve this state at the cost of moving the cursor in our daily work, execute the command, and then move the cursor to continue editing... Throughout the whole process, we don't need to care about which mode we are currently in, because we can define different cursor styles for the two different states, and the cursor style will also automatically switch when the mode automatically switches.

# More refined automatic switching

The auto-modal function would be very limited if we could only define one or a few conditions for automatically triggering mode switching for all buffers. Because of the different text structures, the same trigger condition is not suitable for all situations; and if all possible trigger positions are bound, it will become redundant in situations where switching is not required. Auto-modal supports binding the same key to different trigger conditions in different major modes.

For example:

```emacs-lisp
(auto-modal-bind-key “j” 'org-mode 'auto-modal-bolp 'dired-jump)
(auto-modal-bind-key “j” 'emacs-lisp-mode 'auto-modal-before-parensp 'auto-modal-next-parens)
(auto-modal-bind-key “j” 'emacs-lisp-mode 'auto-modal-bolp 'auto-modal-next-function)
(auto-modal-bind-key “j” 'fundamental-mode 'auto-modal-bolp 'auto-modal-next-line)
```

- First binding: in org-mode, when the auto-modal-bolp assertion is satisfied, pressing “j” jumps to the current dired directory
- Second binding: in emacs-lisp-mode, when the auto-modal-before-parensp assertion is satisfied, pressing “j” jumps to the beginning of the next parenthesis
- Third binding: in emacs-lisp-mode, when the auto-modal-bolp assertion is satisfied, pressing “j” jumps to the beginning of the next function
- Fourth binding: in all major modes, when the auto-modal-bolp assertion is satisfied, pressing “j” jumps to the next line

(The functions in the above examples are used only as examples for understanding and do not provide actual functions.)

It is worth noting that key bindings can be inherited with major mode inheritance, and when a child major mode specifies a function for the same key and trigger condition, it will override the binding of the parent major mode. The third binding above overrides the behavior of the fourth binding. If there is no other binding, in all non-emacs-lisp-modes, all keys “j” that satisfy the auto-modal-bolp assertion will trigger a jump to the next line.

What to do when you need to enter characters in command mode?

Although input at the trigger position is considered a rare case, there may be scenarios where input is required, and in this case, active switching to insert mode is required. Use the built-in command `auto-modal-enable-insert` to actively switch to insert mode, which you can bind to a letter key.

# My configuration
Auto-modal is a highly customizable modal auto-switching system. Users can personalize the configuration according to their own needs or discover more interesting usage. If you are not sure how to use it, here is my current personal configuration for your reference.

## use-region-p
When region is selected, set some letter keys to operate on the selected text or execute other commands

```emacs-lisp
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
```

## bolp
I like to use the position of the cursor at the beginning of a line (excluding the beginning of an empty line) as the trigger condition for automatically switching between modes. In addition to being the case for entering characters, moving the cursor to the beginning is also a very frequent operation in daily editing.

```emacs-lisp
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

(defun auto-modal-enable-insert ()
  (setq auto-modal-enable-insert-p t))

(auto-modal-bind-key "l" 'global 'auto-modal-bolp 'avy-goto-line)
(auto-modal-bind-key "c" 'global 'auto-modal-bolp 'avy-goto-char-timer)
(auto-modal-bind-key "j" 'global 'auto-modal-bolp 'auto-modal-next-line)
(auto-modal-bind-key "o" 'global 'auto-modal-bolp 'other-window 1)
(auto-modal-bind-key "k" 'global 'auto-modal-bolp 'auto-modal-previous-line)
(auto-modal-bind-key "SPC" 'global 'auto-modal-bolp 'auto-modal-enable-insert)
(auto-modal-bind-key "f" 'global 'auto-modal-bolp 'counsel-find-file)
(auto-modal-bind-key "<" 'global 'auto-modal-bolp 'backward-page)
(auto-modal-bind-key ">" 'global 'auto-modal-bolp 'forward-page)
```

## vi-mode
If the trigger condition is set to a function that always returns t, auto-modal degenerates into active mode switching for vi. The following is a simple vi-mode implementation using the auto-modal configuration.

```emacs-lisp
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
```

## Sexp
```emacs-lisp
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

(auto-modal-bind-key "f" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-forward)
(auto-modal-bind-key "b" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-backward)
(auto-modal-bind-key "j" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-down)
(auto-modal-bind-key "k" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-up)
(auto-modal-bind-key "i" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-into)
(auto-modal-bind-key "o" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-outside)
(auto-modal-bind-key "s" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-balance)
(auto-modal-bind-key "n" 'emacs-lisp-mode 'sexp-around-paren-p 'sexp-newline-paren)
(auto-modal-bind-key "SPC" 'emacs-lisp-mode 'sexp-around-paren-p 'auto-modal-enable-insert)
```

## Other
There are also some other useful and interesting usages, so feel free to explore.
