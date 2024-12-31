在 emacs 中，对于需要频繁使用的命令，我们倾向于将其绑定到快捷键，即按下几个组合按键便完成了一个复杂功能的调用。可以将任意的命令绑定到快捷键是 emacs 被认为灵活、高效的重要原因之一。emacs 中的快捷键的特点是需要使用前缀键，即 `C-c`、`C-x` 等，但这比起 vi 这种模态编辑中使用单字母按键，又显的不那么的“快捷”了。习惯了 vi 的模态切换方式的用户可能会在 emacs 中使用诸如 `evil-mode`、`meow` 等方案来延续这种使用习惯。我本人是不太喜欢“模态编辑”的，但又觉得单字母按键确实高效，于是便思考一种即能够使用上单字母快捷键，但又无需关心模态切换的方案，于是有了 auto-modal 这个 package。

# Auto-modal 做了什么

Auto-modal 顾名思义被称为“自动模态切换”。当光标所在位置满足一个指定的断言函数时，自动切换到“命令模式”（这里所说的命令模式指的是可以通过预先定义的多个单字母按键触发命名的执行）；当光标所在位置不满足指定的断言函数时，自动切换到普通模式（这里说的普通模式就是正常使用 emacs 的状态）。在这两种切换的过程中，任何其他的按键绑定都不会受到影响。

在 emacs 的正常编辑中，字母(数字，标点)按键默认被绑定到 `self-insert-command`，即在光标位置插入键入的字符。显然，在这种编辑情况下，切换到命令模态是不合适的。于是我们需要寻找一些在编辑中的 rare cases，在这些情况中，我们鲜少输入字符，并将其作为自动切换模态的触发情况。比如，在已有文本的一行的开头 或 lisp系语言左括号的左边 等位置。当我们需要触发字母按键的命令时，只需以日常移动光标的代价来达到这种状态，执行完命令，然后再移动光标，继续编辑... 整个过程，我们无需关心当前处于哪种模态，因为可以定义这两种不同状态下显示不同的光标样式，在模态自动切换时，光标样式也会自动切换。

# 更精细化的自动切换

如果对于所有 buffer 我们只能定义一种或几种自动触发模态切换的条件，那么 auto-modal 的功能将会变得很局限。因为不同的文本结构，同一种触发条件并不适合所有情况；而如果绑定所有可能的触发位置，对于无需切换的情况，又会变得冗余。Auto-modal 支持将同一个按键绑定到不同的 major mode 下的不同的触发条件中。

举个例子：

```
(auto-modal-bind-key "j" 'org-mode 'auto-modal-bolp 'dired-jump)
(auto-modal-bind-key "j" 'emacs-lisp-mode 'auto-modal-before-parensp 'auto-modal-next-parens)
(auto-modal-bind-key "j" 'emacs-lisp-mode 'auto-modal-bolp 'auto-modal-next-function)
(auto-modal-bind-key "j" 'fundamental-mode 'auto-modal-bolp 'auto-modal-next-line)
```

第一个绑定：在 org-mode 中，当满足 auto-modal-bolp 断言时，按 "j" 跳转到当前的 dired 目录
第二个绑定：在 emacs-lisp-mode 中，当满足 auto-modal-before-parensp 断言时，按 "j" 跳转到下一个括号的开头
第三个绑定：在 emacs-lisp-mode 中，当满足 auto-modal-bolp 断言时，按 "j" 跳转到下一个函数
第四个绑定：在所有的 major mode 中，当满足 auto-modal-bolp 断言时，按 "j" 跳转到下一行

值得注意的是，按键绑定可以随着 major mode 的继承而继承，而子 major mode 指定了同一按键、同一触发条件的函数时，会覆盖父 major mode 的绑定。上面的第三个绑定就覆盖了第四个的行为，如果没有其他的绑定，在所有非 emacs-lisp-mode 中，满足 auto-modal-bolp 断言的所有的按键 "j"，都会触发跳转到下一行。

# 触发条件下需要输入字符如果处理
虽然在触发位置输入字符被认为是 rare case，但也会存在需要输入的场景，此时就需要主动切换了插入模态了。使用内置命令 `auto-modal-enable-insert` 主动切换到插入模式，你可以将它绑定到一个单字母，比如我绑定到了空格键。

# 我的配置
auto-modal 是一个高度可定制化的模态自动切换系统，用户可能根据自己的需求，进行个性化的配置或发现更多有趣的用法。如果你还不清楚该如何使用，下面是目前我个人的配置，供大家参考。

## use-region-p
```
(auto-modal-bind-key "u" 'global 'use-region-p 'upcase-dwim)
(auto-modal-bind-key "d" 'global 'use-region-p 'downcase-dwim)
(auto-modal-bind-key "c" 'global 'use-region-p 'kill-ring-save)
```
## bolp
```
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
```
(defvar auto-modal-vi-keybinds
  '(("i" auto-modal-vi-insert-mode)
    ("j" next-line)
    ("k" previous-line)
    ("h" backward-char)
    ("l" forward-char)
    ("w" forward-word)
    ("b" backward-word)))

(defun auto-modal-vi-pred () t)

(defun auto-modal-vi-normal-mode ()
  (dolist (keybind auto-modal-vi-keybinds)
    (apply 'auto-modal-bind-key
           (car keybind) 'global 'auto-modal-vi-pred (cdr keybind))))

(defun auto-modal-vi-insert-mode ()
  (auto-modal-unbind-with-predicate 'auto-modal-vi-pred))

(defun auto-modal-vi-mode-toogle ()
  (interactive)
  (if auto-modal-vi-mode
      (auto-modal-vi-insert-mode)
    (auto-modal-vi-normal-mode)))

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
