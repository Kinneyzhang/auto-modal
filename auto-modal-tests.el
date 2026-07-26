;;; auto-modal-tests.el --- Tests for auto-modal  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Kinney Zhang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for auto-modal.  Run them in batch with:
;;
;;   make test
;;
;; or interactively with M-x ert.

;;; Code:

(require 'ert)
(require 'auto-modal)

(defmacro auto-modal-tests--with-clean-state (&rest body)
  "Run BODY with empty auto-modal data and a pristine control keymap.
The keymap object registered in `minor-mode-map-alist' is mutated
in place (variable rebinding would not affect the active keymap)
and restored afterwards."
  (declare (indent 0))
  `(let ((auto-modal-data nil)
         (auto-modal-enable-log nil)
         (auto-modal-enable-keyhint nil)
         (saved-map-entries (cdr (copy-keymap auto-modal-control-mode-map))))
     (unwind-protect
         (progn
           (setcdr auto-modal-control-mode-map
                   (cdr (let ((map (make-sparse-keymap)))
                          (suppress-keymap map t)
                          map)))
           ,@body)
       (setcdr auto-modal-control-mode-map saved-map-entries))))

(defmacro auto-modal-tests--with-buffer (mode &rest body)
  "Run BODY in a temp buffer put into major mode MODE.
Any `auto-modal-mode' enabled inside BODY is disabled afterwards,
so tests do not leak the global hooks into the session."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (funcall ,mode))
     (unwind-protect
         (progn ,@body)
       (when auto-modal-mode (auto-modal-mode -1)))))

(defmacro auto-modal-tests--type (keys)
  "Feed KEYS (a `kbd' string) to the command loop in the current buffer.
`execute-kbd-macro' runs commands in the selected window's buffer,
so the current buffer is displayed there first."
  `(progn
     (set-window-buffer (selected-window) (current-buffer))
     (execute-kbd-macro (kbd ,keys))))

;;;; Mode hierarchy

(ert-deftest auto-modal-tests-mode-distance-self ()
  (should (= 0 (auto-modal--mode-distance 'emacs-lisp-mode 'emacs-lisp-mode))))

(ert-deftest auto-modal-tests-mode-distance-parent ()
  ;; emacs-lisp-mode derives from lisp-data-mode which derives from
  ;; prog-mode.
  (should (= 1 (auto-modal--mode-distance 'lisp-data-mode 'emacs-lisp-mode)))
  (should (= 2 (auto-modal--mode-distance 'prog-mode 'emacs-lisp-mode))))

(ert-deftest auto-modal-tests-mode-distance-unrelated ()
  (should-not (auto-modal--mode-distance 'org-mode 'emacs-lisp-mode)))

(ert-deftest auto-modal-tests-mode-distance-fundamental-is-universal ()
  (let ((distance (auto-modal--mode-distance 'fundamental-mode
                                             'emacs-lisp-mode)))
    (should (numberp distance))
    ;; More distant than any real ancestor.
    (should (> distance
               (auto-modal--mode-distance 'prog-mode 'emacs-lisp-mode)))))

;;;; Binding and unbinding

(ert-deftest auto-modal-tests-bind-key-registers-data ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'next-line)
    (should (= 1 (length auto-modal-data)))
    (let ((keybind (car auto-modal-data)))
      (should (equal "j" (auto-modal-keybind-key-name keybind)))
      ;; 'global is normalized to fundamental-mode.
      (should (eq 'fundamental-mode (auto-modal-keybind-mode keybind)))
      (should (eq 'next-line (auto-modal-keybind-function keybind)))
      (should-not (auto-modal-keybind-args keybind)))
    ;; The key is bound in the control keymap.
    (should (eq 'auto-modal-dispatch
                (lookup-key auto-modal-control-mode-map (kbd "j"))))))

(ert-deftest auto-modal-tests-bind-key-with-args ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "o" 'global t '(other-window 1))
    (let ((keybind (car auto-modal-data)))
      (should (eq 'other-window (auto-modal-keybind-function keybind)))
      (should (equal '(1) (auto-modal-keybind-args keybind))))))

(ert-deftest auto-modal-tests-bind-key-deduplicates ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'next-line)
    (auto-modal-bind-key "j" 'global t 'next-line)
    (should (= 1 (length auto-modal-data)))))

(ert-deftest auto-modal-tests-bind-key-validates ()
  (auto-modal-tests--with-clean-state
    (should-error (auto-modal-bind-key 42 'global t 'next-line))
    (should-error (auto-modal-bind-key "j" "org-mode" t 'next-line))
    (should-error (auto-modal-bind-key "j" 'global nil 'next-line))
    (should-error (auto-modal-bind-key "j" 'global t nil))
    ;; Nothing was registered or bound by the failed attempts.
    (should-not auto-modal-data)
    (should-not (lookup-key auto-modal-control-mode-map (kbd "j")))))

(ert-deftest auto-modal-tests-bind-key-accepts-undefined-symbols ()
  ;; Symbols of lazily-loaded packages are accepted.
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "l" 'global t 'auto-modal-tests--undefined-fn)
    (should (= 1 (length auto-modal-data)))))

(ert-deftest auto-modal-tests-help-key-is-reserved ()
  (auto-modal-tests--with-clean-state
    (should-error (auto-modal-bind-key auto-modal-help-key 'global t 'ignore)
                  :type 'user-error)))

(ert-deftest auto-modal-tests-unbind-key-exact ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'next-line)
    (auto-modal-unbind-key "j" 'global t 'next-line)
    (should-not auto-modal-data)
    (should-not (lookup-key auto-modal-control-mode-map (kbd "j")))))

(ert-deftest auto-modal-tests-unbind-key-keeps-other-bindings ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'next-line)
    (auto-modal-bind-key "j" 'org-mode t 'org-next-visible-heading)
    (auto-modal-unbind-key "j" 'global t 'next-line)
    (should (= 1 (length auto-modal-data)))
    ;; Still bound: another binding uses the key.
    (should (eq 'auto-modal-dispatch
                (lookup-key auto-modal-control-mode-map (kbd "j"))))))

(ert-deftest auto-modal-tests-unbind-filters ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'next-line)
    (auto-modal-bind-key "j" 'org-mode t 'org-next-visible-heading)
    (auto-modal-unbind "j" 'org-mode)
    (should (= 1 (length auto-modal-data)))
    (should (eq 'fundamental-mode
                (auto-modal-keybind-mode (car auto-modal-data))))
    (auto-modal-unbind "j")
    (should-not auto-modal-data)))

(ert-deftest auto-modal-tests-unbind-with-predicate ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global 'bolp 'next-line)
    (auto-modal-bind-key "k" 'global 'bolp 'previous-line)
    (auto-modal-bind-key "u" 'global 'use-region-p 'upcase-dwim)
    (auto-modal-unbind-with-predicate 'bolp)
    (should (= 1 (length auto-modal-data)))
    (should-not (lookup-key auto-modal-control-mode-map (kbd "j")))
    (should-not (lookup-key auto-modal-control-mode-map (kbd "k")))
    (should (eq 'auto-modal-dispatch
                (lookup-key auto-modal-control-mode-map (kbd "u"))))))

(ert-deftest auto-modal-tests-all-keys ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'next-line)
    (auto-modal-bind-key "j" 'org-mode t 'org-next-visible-heading)
    (auto-modal-bind-key "k" 'global t 'previous-line)
    (should (equal '("k" "j") (auto-modal-all-keys)))))

(ert-deftest auto-modal-tests-bind-key-canonicalizes-key-names ()
  ;; "C-i" and "TAB" are the same key; the stored name must be the
  ;; canonical one so dispatch can find it.
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "C-i" 'global t 'forward-char)
    (should (equal "TAB" (auto-modal-keybind-key-name (car auto-modal-data))))
    ;; Unbinding via either spelling works.
    (auto-modal-unbind "C-i")
    (should-not auto-modal-data)))

(ert-deftest auto-modal-tests-bind-key-rejects-invalid-kbd-syntax ()
  (auto-modal-tests--with-clean-state
    (should-error (auto-modal-bind-key "C-x-y" 'global t 'next-line))
    ;; The failed attempt must not leave partial state behind.
    (should-not auto-modal-data)))

(ert-deftest auto-modal-tests-broken-predicate-is-unsatisfied ()
  ;; A predicate that signals must not propagate (it would knock
  ;; auto-modal's function off the global post-command-hook).
  (auto-modal-tests--with-clean-state
    (let ((auto-modal--broken-predicates nil)
          (inhibit-message t))
      (auto-modal-bind-key "j" 'global
                           (lambda () (error "Boom"))
                           'next-line)
      (auto-modal-tests--with-buffer #'fundamental-mode
        (insert "hello\n")
        (goto-char (point-min))
        (should-not (auto-modal-triggered-p))))))

;;;; Trigger computation and resolution

(ert-deftest auto-modal-tests-triggered-p ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global 'bolp 'next-line)
    (auto-modal-tests--with-buffer #'fundamental-mode
      (insert "hello\nworld")
      (goto-char (point-min))
      (should (auto-modal-triggered-p))
      (forward-char 2)
      (should-not (auto-modal-triggered-p)))))

(ert-deftest auto-modal-tests-triggered-p-respects-mode ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'org-mode t 'ignore)
    (auto-modal-tests--with-buffer #'emacs-lisp-mode
      (should-not (auto-modal-triggered-p)))
    (auto-modal-tests--with-buffer #'org-mode
      (should (auto-modal-triggered-p)))))

(ert-deftest auto-modal-tests-active-keybind-closest-mode-wins ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'next-line)
    (auto-modal-bind-key "j" 'prog-mode t 'forward-sexp)
    (auto-modal-bind-key "j" 'emacs-lisp-mode t 'forward-sentence)
    (auto-modal-tests--with-buffer #'emacs-lisp-mode
      (should (eq 'forward-sentence
                  (auto-modal-keybind-function
                   (auto-modal--active-keybind "j")))))
    (auto-modal-tests--with-buffer #'python-mode
      (should (eq 'forward-sexp
                  (auto-modal-keybind-function
                   (auto-modal--active-keybind "j")))))
    (auto-modal-tests--with-buffer #'text-mode
      (should (eq 'next-line
                  (auto-modal-keybind-function
                   (auto-modal--active-keybind "j")))))))

(ert-deftest auto-modal-tests-active-keybind-skips-false-predicates ()
  ;; A closer mode whose predicate fails must not shadow a more
  ;; distant binding whose predicate succeeds.
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'next-line)
    (auto-modal-bind-key "j" 'emacs-lisp-mode 'ignore 'forward-sexp)
    (auto-modal-tests--with-buffer #'emacs-lisp-mode
      (should (eq 'next-line
                  (auto-modal-keybind-function
                   (auto-modal--active-keybind "j")))))))

(ert-deftest auto-modal-tests-active-keybind-newest-wins-on-tie ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global 'bolp 'next-line)
    (auto-modal-bind-key "j" 'global t 'forward-char)
    (auto-modal-tests--with-buffer #'fundamental-mode
      (insert "x")
      (goto-char (point-min))
      ;; Both predicates true at bol; the newest binding wins.
      (should (eq 'forward-char
                  (auto-modal-keybind-function
                   (auto-modal--active-keybind "j")))))))

;;;; Dispatch

(ert-deftest auto-modal-tests-dispatch-runs-function ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global 'bolp 'auto-modal-tests--move-eol)
    (defalias 'auto-modal-tests--move-eol
      (lambda () (interactive) (end-of-line)))
    (unwind-protect
        (auto-modal-tests--with-buffer #'fundamental-mode
          (insert "hello\n")
          (goto-char (point-min))
          (auto-modal-mode 1)
          (should auto-modal-control-mode)
          (auto-modal-tests--type "j")
          (should (= (point) (line-end-position))))
      (fmakunbound 'auto-modal-tests--move-eol))))

(ert-deftest auto-modal-tests-dispatch-applies-args ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "f" 'global 'bolp '(forward-char 3))
    (auto-modal-tests--with-buffer #'fundamental-mode
      (insert "hello\n")
      (goto-char (point-min))
      (auto-modal-mode 1)
      (auto-modal-tests--type "f")
      (should (= (point) 4)))))

(ert-deftest auto-modal-tests-dispatch-falls-back-to-self-insert ()
  ;; A key whose predicate is not satisfied falls back to the command
  ;; it would run without auto-modal.
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global 'bolp 'next-line)
    (auto-modal-bind-key "u" 'global 'use-region-p 'upcase-dwim)
    (auto-modal-tests--with-buffer #'fundamental-mode
      (insert "hello\n")
      (goto-char (point-min))
      (auto-modal-mode 1)
      (should auto-modal-control-mode)
      ;; At bol, no region: "u" self-inserts.
      (auto-modal-tests--type "u")
      (should (string-prefix-p "uhello" (buffer-string))))))

(ert-deftest auto-modal-tests-dispatch-defers-to-local-map ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'next-line)
    (auto-modal-tests--with-buffer #'fundamental-mode
      (insert "hello\n")
      (goto-char (point-min))
      ;; Give the buffer a local binding for "j".
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "j") #'end-of-line)
      (auto-modal-mode 1)
      (auto-modal-tests--type "j")
      ;; The local command ran instead of next-line.
      (should (= (point) (line-end-position))))))

(ert-deftest auto-modal-tests-dispatch-override-beats-local-map ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'forward-char t)
    (auto-modal-tests--with-buffer #'fundamental-mode
      (insert "hello\n")
      (goto-char (point-min))
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "j") #'end-of-line)
      (auto-modal-mode 1)
      (auto-modal-tests--type "j")
      ;; The auto-modal function ran, not the local command.
      (should (= (point) 2)))))

(ert-deftest auto-modal-tests-suppressed-key-does-not-insert ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global 'bolp 'ignore)
    (auto-modal-tests--with-buffer #'fundamental-mode
      (insert "hello\n")
      (goto-char (point-min))
      (auto-modal-mode 1)
      (should auto-modal-control-mode)
      ;; "x" is not bound: suppressed in control state.
      (let ((inhibit-message t))
        (ignore-errors (auto-modal-tests--type "x")))
      (should (equal "hello\n" (buffer-string))))))

(ert-deftest auto-modal-tests-override-p-is-per-binding ()
  ;; The same key with different override-p in different modes: each
  ;; binding's own flag decides, not the first-registered one.
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'end-of-line)      ; no override
    (auto-modal-bind-key "j" 'text-mode t 'forward-char t) ; override
    ;; In text-mode the overriding binding beats the local map.
    (auto-modal-tests--with-buffer #'text-mode
      (insert "hello\n")
      (goto-char (point-min))
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "j") #'end-of-buffer)
      (auto-modal-mode 1)
      (auto-modal-tests--type "j")
      (should (= (point) 2)))
    ;; In fundamental-mode the non-overriding binding defers to the
    ;; local map.
    (auto-modal-tests--with-buffer #'fundamental-mode
      (insert "hello\n")
      (goto-char (point-min))
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "j") #'end-of-buffer)
      (auto-modal-mode 1)
      (auto-modal-tests--type "j")
      (should (= (point) (point-max))))))

(ert-deftest auto-modal-tests-vi-keybind-args-are-applied ()
  ;; Entries of auto-modal-vi-keybinds may carry arguments:
  ;; ("z" forward-line 2) must apply forward-line to 2.
  (require 'auto-modal-config)
  (auto-modal-tests--with-clean-state
    (let ((auto-modal-vi-keybinds '(("z" forward-line 2))))
      (auto-modal-vi-normal-mode)
      (let ((keybind (car auto-modal-data)))
        (should (eq 'forward-line (auto-modal-keybind-function keybind)))
        (should (equal '(2) (auto-modal-keybind-args keybind)))))))

;;;; State machine

(ert-deftest auto-modal-tests-mode-toggles-control-state ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global 'bolp 'next-line)
    (auto-modal-tests--with-buffer #'fundamental-mode
      (insert "hello\nworld\n")
      (goto-char (point-min))
      (auto-modal-mode 1)
      (should auto-modal-control-mode)
      (should (eq 'control auto-modal--last-state))
      (forward-char 2)
      (auto-modal--update)
      (should-not auto-modal-control-mode)
      (should (eq 'insert auto-modal--last-state))
      (auto-modal-mode -1)
      (should-not auto-modal-control-mode))))

(ert-deftest auto-modal-tests-enable-insert-is-one-shot ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "SPC" 'global 'bolp 'auto-modal-enable-insert)
    (auto-modal-tests--with-buffer #'fundamental-mode
      (insert "hello\n")
      (goto-char (point-min))
      (auto-modal-mode 1)
      (should auto-modal-control-mode)
      (auto-modal-tests--type "SPC")
      ;; Insert state now, even though point is still at a trigger
      ;; position; the flag is consumed by the next command.
      (should-not auto-modal-control-mode)
      (should auto-modal-enable-insert-p)
      ;; Typing works, and consumes the one-shot flag.
      (auto-modal-tests--type "x")
      (should (string-prefix-p "xhello" (buffer-string)))
      (should-not auto-modal-enable-insert-p))))

(ert-deftest auto-modal-tests-global-hooks-lifecycle ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global 'bolp 'next-line)
    (let ((buffer-a (generate-new-buffer "auto-modal-test-a"))
          (buffer-b (generate-new-buffer "auto-modal-test-b")))
      (unwind-protect
          (progn
            (with-current-buffer buffer-a (auto-modal-mode 1))
            (with-current-buffer buffer-b (auto-modal-mode 1))
            (should (memq 'auto-modal--post-command post-command-hook))
            ;; Turning off in one buffer keeps the global hook.
            (with-current-buffer buffer-a (auto-modal-mode -1))
            (should (memq 'auto-modal--post-command post-command-hook))
            ;; Turning off in the last buffer removes it.
            (with-current-buffer buffer-b (auto-modal-mode -1))
            (should-not (memq 'auto-modal--post-command post-command-hook)))
        (kill-buffer buffer-a)
        (kill-buffer buffer-b)))))

(ert-deftest auto-modal-tests-minibuffer-stays-insert ()
  (auto-modal-tests--with-clean-state
    (auto-modal-bind-key "j" 'global t 'next-line)
    ;; Simulate: a minibuffer must never be in control state even
    ;; with an always-true predicate.
    (with-temp-buffer
      (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t)))
        (should (eq 'insert (auto-modal--desired-state)))))))

;;;; Cursor handling

(ert-deftest auto-modal-tests-resolve-cursor-type ()
  (let ((standard (default-value 'cursor-type)))
    (should (eq standard (auto-modal--resolve-cursor-type 'default)))
    (should (eq 'box (auto-modal--resolve-cursor-type 'box)))
    (should (equal '(bar . 2) (auto-modal--resolve-cursor-type '(bar . 2))))))

(ert-deftest auto-modal-tests-resolve-cursor-color ()
  (should (equal "#ff0000" (auto-modal--resolve-cursor-color "#ff0000")))
  (should-error (auto-modal--resolve-cursor-color 42))
  ;; Cons cell selects by background mode.
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (_frame parameter)
               (when (eq parameter 'background-mode) 'dark))))
    (should (equal "white" (auto-modal--resolve-cursor-color
                            '("black" . "white"))))))

;;;; Logging

(ert-deftest auto-modal-tests-log-records-and-trims ()
  (auto-modal-tests--with-clean-state
    (let ((auto-modal-log-max-number 3)
          (auto-modal-log-buffer-name "*auto-modal-test-log*"))
      (unwind-protect
          (progn
            (dotimes (i 5)
              (auto-modal--record-log "j" (list 'next-line i)))
            (with-current-buffer auto-modal-log-buffer-name
              (should (= 3 (count-lines (point-min) (point-max))))
              ;; The oldest entries were trimmed.
              (should (string-match-p "(next-line 4)" (buffer-string)))
              (should-not (string-match-p "(next-line 0)" (buffer-string)))))
        (when (get-buffer "*auto-modal-test-log*")
          (kill-buffer "*auto-modal-test-log*"))))))

;;;; Example configuration

(ert-deftest auto-modal-tests-config-loads ()
  (let ((auto-modal-data nil))
    (should (require 'auto-modal-config))))

(ert-deftest auto-modal-tests-config-next-previous-line ()
  (require 'auto-modal-config)
  (with-temp-buffer
    (insert "one\n\n\ntwo\nthree\n")
    ;; next: skips empty lines.
    (goto-char (point-min))
    (auto-modal-next-line)
    (should (looking-at-p "two"))
    (auto-modal-next-line)
    (should (looking-at-p "three"))
    ;; At the last non-empty line: stays put.
    (auto-modal-next-line)
    (should (looking-at-p "three"))
    ;; previous: skips empty lines back.
    (auto-modal-previous-line)
    (should (looking-at-p "two"))
    (auto-modal-previous-line)
    (should (looking-at-p "one"))
    ;; At the first line: stays put (regression: used to recurse
    ;; forever when the first line was empty).
    (auto-modal-previous-line)
    (should (looking-at-p "one"))))

(ert-deftest auto-modal-tests-config-previous-line-empty-first-line ()
  (require 'auto-modal-config)
  (with-temp-buffer
    (insert "\n\ntext\n")
    (goto-char 3)                       ; on "text"
    (should (looking-at-p "text"))
    ;; Only empty lines above: must terminate and stay put.
    (auto-modal-previous-line)
    (should (looking-at-p "text"))))

(ert-deftest auto-modal-tests-config-sexp-predicates ()
  (require 'auto-modal-config)
  (with-temp-buffer
    (delay-mode-hooks (emacs-lisp-mode))
    (insert "(foo (bar))\n\"(not code)\"\n")
    ;; Before "(foo": left paren at depth 0.
    (goto-char (point-min))
    (should (= 0 (auto-modal-sexp-left-paren-p)))
    (should-not (auto-modal-sexp-right-paren-p))
    ;; Before "(bar": left paren at depth 1.
    (goto-char 6)
    (should (= 1 (auto-modal-sexp-left-paren-p)))
    ;; After "(bar)": right paren at depth 1.
    (goto-char 11)
    (should (= 1 (auto-modal-sexp-right-paren-p)))
    ;; Inside a string: not a paren position.
    (goto-char 14)                      ; before "(not"
    (should (eq ?\( (char-after)))
    (should-not (auto-modal-sexp-left-paren-p))))

(ert-deftest auto-modal-tests-config-sexp-balance ()
  (require 'auto-modal-config)
  (with-temp-buffer
    (delay-mode-hooks (emacs-lisp-mode))
    (insert "(foo (bar) baz)")
    (goto-char (point-min))
    (auto-modal-sexp-balance)
    (should (= (point) (point-max)))
    (auto-modal-sexp-balance)
    (should (= (point) (point-min)))))

(ert-deftest auto-modal-tests-config-vi-mode-toggle ()
  (require 'auto-modal-config)
  (auto-modal-tests--with-clean-state
    (let ((auto-modal-vi-insert-flag nil))
      (auto-modal-vi-normal-mode)
      (should (auto-modal--active-keybind "h"))
      (auto-modal-vi-insert-mode)
      (should auto-modal-vi-insert-flag)
      (should-not (seq-filter
                   (lambda (keybind)
                     (eq 'auto-modal-vi-pred
                         (auto-modal-keybind-predicate keybind)))
                   auto-modal-data)))))

(provide 'auto-modal-tests)
;;; auto-modal-tests.el ends here
