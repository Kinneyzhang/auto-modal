# auto-modal

[![CI](https://github.com/Kinneyzhang/auto-modal/actions/workflows/test.yml/badge.svg)](https://github.com/Kinneyzhang/auto-modal/actions/workflows/test.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

*无需切换模式的模态编辑：让单字母命令只出现在你绝不会输入文字的位置。*

[English documentation](README.md)

---

在 Emacs 中，常用命令通常绑定在带前缀键（`C-c`、`C-x` 等）的按键序列上，
这比起 vi 系模态编辑的单字母按键要慢一些，因此很多人使用 `evil-mode` 或
`meow`。但模态编辑也有自己的代价：你必须**手动切换模式**。

**auto-modal** 走了第三条路：编辑状态由**光标位置**自动推导。你注册的每
个键绑定由按键、major mode、断言函数和命令四部分组成。只要光标位置满足
某个断言——比如位于非空行的行首、或者紧邻 Lisp 代码的括号——buffer 就自动
进入**控制态（control）**，单字母按键执行命令；其他任何位置 buffer 都处
于**插入态（insert）**，与普通 Emacs 完全一致。光标形状和颜色随状态变化，
你永远知道自己在哪个状态。你从不切换模式；你只是移动光标，模式跟着走。

## 目录

- [环境要求](#环境要求)
- [安装](#安装)
- [快速上手](#快速上手)
- [核心概念](#核心概念)
  - [控制态与插入态](#控制态与插入态)
  - [触发断言](#触发断言)
  - [Major mode 继承](#major-mode-继承)
  - [按键解析与优先级](#按键解析与优先级)
  - [在触发位置输入文字](#在触发位置输入文字)
- [API 参考](#api-参考)
  - [注册键绑定](#注册键绑定)
  - [移除键绑定](#移除键绑定)
  - [Minor mode 与命令](#minor-mode-与命令)
  - [自定义选项](#自定义选项)
  - [钩子](#钩子)
- [帮助键](#帮助键)
- [日志](#日志)
- [示例配置](#示例配置)
  - [行首导航](#行首导航)
  - [选区绑定](#选区绑定)
  - [S 表达式导航](#s-表达式导航)
  - [极简 vi 模拟](#极简-vi-模拟)
- [编写自己的断言](#编写自己的断言)
- [性能说明](#性能说明)
- [从 0.x 迁移](#从-0x-迁移)
- [开发](#开发)
- [许可证](#许可证)

## 环境要求

- Emacs **29.1** 或更高版本。
- 无任何外部依赖。（可选的示例配置中有两个绑定引用了 `avy` 的命令；未安
  装 avy 时注册照常成功，按下时会给出清晰的"未定义"报错。）

## 安装

直接从仓库安装即可。

**使用 `use-package` 的 `:vc`（Emacs 30+）：**

```emacs-lisp
(use-package auto-modal
  :vc (:url "https://github.com/Kinneyzhang/auto-modal" :rev :newest))
```

**使用 `straight.el`：**

```emacs-lisp
(straight-use-package
 '(auto-modal :type git :host github :repo "Kinneyzhang/auto-modal"))
```

**手动安装：**

```emacs-lisp
;; git clone https://github.com/Kinneyzhang/auto-modal ~/path/to/auto-modal
(add-to-list 'load-path "~/path/to/auto-modal")
(require 'auto-modal)
```

## 快速上手

```emacs-lisp
(require 'auto-modal)

;; 断言：光标位于非空行的行首。
(defun my/bolp ()
  (and (bolp) (not (looking-at-p "^$"))))

;; 断言满足时，"j"/"k" 上下移动，"SPC" 在此位置开始输入。
(auto-modal-bind-key "j" 'global 'my/bolp 'next-line)
(auto-modal-bind-key "k" 'global 'my/bolp 'previous-line)
(auto-modal-bind-key "SPC" 'global 'my/bolp 'auto-modal-enable-insert)

;; 全局启用。
(global-auto-modal-mode 1)
```

现在把光标移到任意非空行的行首：光标变成方块——控制态。按 `j`/`k` 按行移
动，按 `?` 查看当前位置所有可用的按键。把光标移到其他任何地方（或按
`SPC` 后开始打字），buffer 就恢复为普通 Emacs。

想直接用一套现成配置？加载自带的示例配置，然后参阅
[它的手册](#示例配置)：

```emacs-lisp
(require 'auto-modal-config)
(global-auto-modal-mode 1)
```

## 核心概念

### 控制态与插入态

启用 `auto-modal-mode` 后，buffer 总是处于两种状态之一：

| 状态 | 时机 | 按键行为 | 默认光标 |
|---|---|---|---|
| **控制态** | 光标满足至少一个已注册断言 | 注册键执行命令，其余自插入键被屏蔽 | `box` |
| **插入态** | 其他任何位置 | 普通 Emacs | `bar` |

状态在每个命令之后重新计算，无需手动切换（当然也可以：见
[在触发位置输入文字](#在触发位置输入文字)）。

非字母按键（`RET`、`C-…`、`M-…`、功能键……）完全不受影响：控制态只屏蔽
**自插入**按键，且只有你注册过的按键才会执行 auto-modal 命令。

### 触发断言

断言是无参函数，调用时光标位于待检测位置。它应当廉价且无副作用——断言在
`post-command-hook` 中运行。断言 `t` 表示"总是触发"，此时 auto-modal 退
化为经典的手动切换模态编辑（见[vi 模拟](#极简-vi-模拟)）。

好的触发位置是那些你几乎不会在此输入文字的地方：

- 非空行的行首（`(and (bolp) (not (looking-at-p "^$")))`）
- Lisp 代码中紧邻括号处
- 激活的选区上（`use-region-p`）
- org 标题的星号上、Markdown 的 `#` 上、dired 的行上……

### Major mode 继承

每个绑定指定一个 major mode，作用于该 mode **及其所有派生 mode**。特殊
符号 `global`（`fundamental-mode` 的别名）作用于所有 buffer。

```emacs-lisp
(auto-modal-bind-key "j" 'global           'my/bolp 'next-line)
(auto-modal-bind-key "j" 'prog-mode       'my/bolp 'my/next-defun)
(auto-modal-bind-key "j" 'emacs-lisp-mode 'my/bolp 'my/next-sexp)
```

三条都注册后，在行首按 `j`：`emacs-lisp-mode` 中执行 `my/next-sexp`
（最近的 mode 获胜），其他编程 mode 中执行 `my/next-defun`，其余场合执
行 `next-line`。

### 按键解析与优先级

在控制态按下一个已注册按键时，auto-modal 按以下规则选择要执行的绑定：

1. 在该键的所有"mode 适用于当前 buffer **且**断言满足"的绑定中，mode 离
   当前 major mode **最近**的获胜；距离相同时，最近注册的获胜。
2. 若获胜绑定注册时未设置 `override-p`，且 major mode 自己的 keymap 绑
   定了该键，则执行 major mode 的命令。这保证了 dired、magit 等特殊
   mode 的字母键照常工作。给 `auto-modal-bind-key` 传入非 nil 的第五个
   参数（`override-p`）可压过 major mode。
3. 若该键**没有**任何绑定的断言满足（buffer 因**其他**按键的断言而处于
   控制态），则该键执行没有 auto-modal 时的原有行为——通常是自插入。

### 在触发位置输入文字

有时你恰恰需要在触发位置输入文字。命令 `auto-modal-enable-insert` 会一
次性切换到插入态：把它绑到一个键上（`SPC` 很合适），按下后即可打字。该
逃生阀在你的下一次按键时被消费；一旦光标不再满足断言，buffer 就回到自动
切换——实际使用中你打完字、移开光标，完全无感。

## API 参考

### 注册键绑定

```emacs-lisp
(auto-modal-bind-key KEY-NAME MODE PREDICATE FUNCTION-ARGS &optional OVERRIDE-P)
```

| 参数 | 含义 |
|---|---|
| `KEY-NAME` | `kbd` 格式的按键字符串：`"j"`、`"SPC"`、`"<"` …… |
| `MODE` | major mode 符号，或 `global` 表示所有 buffer |
| `PREDICATE` | 无参函数，或 `t` 表示"总是" |
| `FUNCTION-ARGS` | 一个函数（若为命令则交互式调用），或列表 `(FUNCTION ARGS...)`，以 `(apply FUNCTION ARGS)` 执行 |
| `OVERRIDE-P` | 非 nil：压过 major mode 自己的键绑定 |

```emacs-lisp
(auto-modal-bind-key "o" 'global 'my/bolp '(other-window 1))   ; 带参数
(auto-modal-bind-key "j" 'dired-mode t 'dired-next-line t)     ; 覆盖 dired 自己的 "j"
```

`PREDICATE` 和 `FUNCTION` 可以是尚未定义的符号，因此可以绑定懒加载包的
命令；未定义的断言不会触发，而分发到未定义的函数会给出清晰的报错。

`auto-modal-help-key`（默认 `?`）指定的按键是保留键，不可注册。

### 移除键绑定

| 函数 | 行为 |
|---|---|
| `(auto-modal-unbind-key KEY MODE PRED FUNCTION-ARGS &optional OVERRIDE-P)` | 移除以**完全相同参数**注册的绑定 |
| `(auto-modal-unbind KEY &optional MODE PREDICATE)` | 移除 `KEY` 的所有绑定，可按 mode 和/或断言过滤；也是交互命令 |
| `(auto-modal-unbind-with-predicate PREDICATE)` | 移除使用 `PREDICATE` 的所有绑定 |

### Minor mode 与命令

| 符号 | 类型 | 说明 |
|---|---|---|
| `auto-modal-mode` | minor mode | 当前 buffer 的自动模态切换 |
| `global-auto-modal-mode` | 全局 minor mode | 在所有 buffer 启用（minibuffer 和内部 buffer 除外） |
| `auto-modal-list-keybinds` | 命令 | 以表格列出全部已注册绑定 |
| `auto-modal-keyhint-show` | 命令 | 在回显区显示当前位置可用的按键 |
| `auto-modal-enable-insert` | 命令 | 一次性切换到插入态（绑到按键上用） |
| `auto-modal-triggered-p` | 函数 | 光标满足某断言时返回非 nil |
| `auto-modal-set-cursor` | 命令 | 重新计算当前 buffer 的状态与光标 |
| `auto-modal-all-keys` | 函数 | 全部已注册按键名 |
| `auto-modal-data` | 变量 | `auto-modal-keybind` 结构体列表 |

### 自定义选项

`M-x customize-group RET auto-modal RET`，或用 Lisp 设置：

| 选项 | 默认值 | 含义 |
|---|---|---|
| `auto-modal-control-cursor-type` | `box` | 控制态的 `cursor-type`；`default` 表示用户默认光标 |
| `auto-modal-insert-cursor-type` | `bar` | 插入态的 `cursor-type` |
| `auto-modal-control-cursor-color` | `nil` | 控制态光标颜色，见下 |
| `auto-modal-insert-cursor-color` | `nil` | 插入态光标颜色 |
| `auto-modal-help-key` | `"?"` | 显示按键提示的保留键 |
| `auto-modal-enable-keyhint` | `nil` | 进入控制态时自动显示按键提示 |
| `auto-modal-enable-log` | `nil` | 将分发的按键记录到 `*Auto-modal-log*` |
| `auto-modal-log-max-number` | `40` | 日志保留条数 |

两个颜色选项接受四种形式：

```emacs-lisp
nil                       ; 主题的默认光标颜色
'error                    ; face：取其前景色
"#ff6c6b"                 ; 颜色字符串
'("#005f87" . "#87d7ff")  ; (亮色主题 . 暗色主题)，按背景自动选择
```

`(LIGHT . DARK)` 形式在主题切换时会重新求值，一份配置同时适配明暗主题。

### 钩子

| 钩子 | 运行时机 |
|---|---|
| `auto-modal-turn-on-hook` | buffer 中启用 `auto-modal-mode` 之后 |
| `auto-modal-turn-off-hook` | 禁用之后 |

## 帮助键

auto-modal 的精髓在于可用按键随光标位置变化——因此可发现性很重要。控制态
下按 `?`（`auto-modal-help-key`）即可查看当前位置的全部可用按键：

```
SPC → auto-modal-enable-insert  j → auto-modal-next-line  k → auto-modal-previous-line  …
```

把 `auto-modal-enable-keyhint` 设为 `t`，每次进入控制态都会自动显示。

## 日志

把 `auto-modal-enable-log` 设为 `t`，每次按键分发及其命令都会记录到
`*Auto-modal-log*` buffer（按 `auto-modal-log-max-number` 裁剪）。当多
个绑定都可能生效时，用它了解到底哪个绑定获胜了。

## 示例配置

`auto-modal-config.el` 自带一套完整可用的配置——作者本人的日常配置。可以
整体加载作为起点，也可以只抄走喜欢的部分：

```emacs-lisp
(require 'auto-modal-config)
(global-auto-modal-mode 1)
```

### 行首导航

断言：`auto-modal-bolp` ——光标位于非空行行首。

| 按键 | 命令 |
|---|---|
| `j` / `k` | 下一 / 上一非空行（`auto-modal-next-line` / `auto-modal-previous-line`） |
| `l` | `avy-goto-line` |
| `c` | `avy-goto-char-timer` |
| `o` | 切换到其他窗口 |
| `v` | `set-mark-command` |
| `<` / `>` | 向前 / 向后翻页 |
| `z` | `read-only-mode` |
| `SPC` | 在此输入（一次性插入态） |

### 选区绑定

断言：`use-region-p` ——存在激活选区，行内任意位置。

| 按键 | 命令 |
|---|---|
| `u` | `upcase-dwim` |
| `d` | `downcase-dwim` |
| `c` | `kill-ring-save` |

注意 `c` 被绑定了两次——一次给选区，一次给行首。断言负责消歧：哪个满足哪
个生效。两者同时满足时（选区激活且光标在行首），选区绑定获胜，因为它注
册得更晚——平局归最近注册的绑定。

### S 表达式导航

断言：`auto-modal-sexp-around-paren-p` ——`emacs-lisp-mode` 中光标紧邻括
号（字符串和注释中的括号不算）。

| 按键 | 命令 | 移动到 |
|---|---|---|
| `f` / `b` | `auto-modal-sexp-forward` / `-backward` | 下一 / 上一个同侧括号 |
| `j` / `k` | `auto-modal-sexp-down` / `-up` | 下一 / 上一个同深度括号 |
| `i` / `o` | `auto-modal-sexp-into` / `-outside` | 深一层 / 跳出到外层表达式 |
| `s` | `auto-modal-sexp-balance` | 配对的括号 |
| `n` | `auto-modal-sexp-newline-paren` | 新行插入 `()`，光标居中 |
| `;` | `auto-modal-sexp-comment` | 注释掉该表达式 |
| `SPC` | 在此输入（一次性插入态） | |

### 极简 vi 模拟

把断言设为 `t`，auto-modal 就退化为经典的手动模态编辑——这里是三十行实现
的 vi。`auto-modal-vi-mode` 在所有启用了 `auto-modal-mode` 的 buffer 中
提供 `h/j/k/l/w/b`（配合 `global-auto-modal-mode` 即全局可用），`i` 进
入插入，`<escape>` 切换回来：

```emacs-lisp
(auto-modal-vi-mode 1)
```

按键表在 `auto-modal-vi-keybinds` 中，启用前可自行扩展。条目可以带参
数：`("z" forward-line 2)`。注意终端 frame 中 `ESC` 是前缀键，需另绑一
个切换键。

## 编写自己的断言

断言在每个命令后运行，务必廉价且只读。一些思路：

```emacs-lisp
;; 在 org 标题的星号上。
(defun my/org-headp ()
  (and (org-at-heading-p) (< (point) (+ (line-beginning-position)
                                        (org-current-level)))))
(auto-modal-bind-key "n" 'org-mode 'my/org-headp 'org-next-visible-heading)
(auto-modal-bind-key "p" 'org-mode 'my/org-headp 'org-previous-visible-heading)

;; 行尾一键跳到 buffer 末尾。
(auto-modal-bind-key "e" 'global 'eolp 'end-of-buffer)

;; 编程 buffer 的空行上快速注释。
(defun my/empty-linep () (looking-at-p "^$"))
(auto-modal-bind-key ";" 'prog-mode 'my/empty-linep 'comment-dwim)
```

经验法则：

- 优先选择**几乎不会输入文字的位置**，这是自动切换"无感"的关键。
- 尽快返回非 nil；语法上下文可以依赖 `syntax-ppss`。
- 同一个键配不同断言完全没问题——见上文的 `c`。

## 性能说明

实现上尽量压低了每次按键的开销：

- 触发状态由单个 `post-command-hook` 函数**每命令只计算一次**，遇到第一
  个满足的断言即短路返回。
- Major mode 继承距离使用哈希表缓存。
- 全局钩子只在至少一个 buffer 启用 `auto-modal-mode` 时安装，最后一个禁
  用时移除。未启用的 buffer 中钩子做一次 buffer-local 检查即返回。

实际的开销大头是你的断言。若某个断言较昂贵，请把它的 mode 范围收窄，让
无关 buffer 永远不运行它。

## 从 0.x 迁移

1.0.0 是一次内部完全重写；对外文档化的 API 不变，但部分内部符号被移除或
改名。只使用 `auto-modal-bind-key` / `auto-modal-unbind-key` /
`auto-modal-unbind-with-predicate` 和光标选项的配置无需任何改动。

| 旧 | 新 |
|---|---|
| `suppress-key-mode`、`suppress-key-mode-map` | `auto-modal-control-mode`、`auto-modal-control-mode-map`（自动管理） |
| `auto-modal-is-triggerp` | `auto-modal-triggered-p`（保留过时别名） |
| `auto-modal-mode-turn-on` | 内部化（保留过时别名） |
| `sexp-forward`、`sexp-down` 等（config） | `auto-modal-sexp-forward`、`auto-modal-sexp-down` 等 |
| `auto-modal-vi-mode-toogle`（config） | `auto-modal-vi-mode-toggle`（保留过时别名） |
| `major-mode-chain`、`major-mode-derived-p`、`background-mode-change-*`、`auto-modal-functions-data`、`auto-modal-trigger-functions`、`auto-modal-key-command`、`auto-modal-original-command`、`auto-modal-switch-to-insert/control` | 移除（均为内部实现） |
| 依赖 `bind-key` | 零外部依赖 |

完整清单（包括促成这次重写的所有 bug 修复）见
[CHANGELOG.md](CHANGELOG.md)。

## 开发

```sh
make compile   # 字节编译（警告视为错误）
make checkdoc  # 文档字符串检查
make test      # 运行 ERT 测试套件
make all       # 以上全部
```

CI 在 Emacs 29.4 和 30.1 上运行同样的三个步骤。欢迎提 issue 和 PR——请保
持 `make all` 通过。

## 许可证

GPL-3.0-or-later，详见源文件中的许可声明。
