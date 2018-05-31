;;; -*- lexical-binding: t -*-
;;; init-kbd-macros.el --- stuff related to custom keyboard macros

;; Got this code from https://www.emacswiki.org/emacs/KeyboardMacros
(defun eds/macro-query (arg)
  "Prompt for input using the minibuffer during kbd macro execution.
With prefix argument, allows you to select what prompt string to use.
If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
         (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                  (read-from-minibuffer prompt))))
    (unless (string= "" input) (insert input))))

(global-set-key "\C-xQ" 'eds/macro-query)

(fset 'insert-enzyme-test-case
   [?t ?e ?s ?t ?\( ?\' ?\C-f ?, ?  ?a ?s ?y ?n ?c ?  ?\( ?\) ?  ?= ?> ?\S-  ?\{ return ?\C-m ?\C-e ?\; ?\C-r ?\' ?\C-b ?\C-f ?\C-u ?\C-x ?Q ?T ?e ?s ?t ?  ?d ?e ?s ?c ?r ?i ?p ?t ?i ?o ?n ?: ? ])

(provide 'init-kbd-macros)
