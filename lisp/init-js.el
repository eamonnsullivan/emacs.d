;;; -*- lexical-binding: t -*-
;;; init-js.el --- stuff related to Javascript

(use-package nvm
  :commands (nvm-use nvm-use-for nvm--installed-versions))

(use-package rjsx-mode)

(use-package js-comint
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go))))

(use-package js2-mode
  :init
  (progn
    (add-to-list
     'auto-mode-alist
     (cons "\.js$" (defun choose-js-type-mode ()
                     (save-excursion
                       (goto-char (point-min))
                       (let ((buff (current-buffer)))
                         (if (or (search-forward "React." nil t 1)
                                 (search-forward "import React" nil t 1))
                             (rjsx-mode)
                           (js2-mode))))))))
  :config
  (setq-default js2-ignored-warnings '("msg.extra.trailing.comma"))
  (add-hook 'js2-mode-hook
            (lambda()
              (setq js2-basic-offset 2
                    js2-highlight-level 3
                    js2-bounce-indent-p t
                    indent-tabs-mode nil
                    js-indent-level 2
                    js2-strict-missing-semi-warning nil)
              (tern-mode)
              (js2-imenu-extras-mode)
              (js2-refactor-mode)
              (auto-revert-mode)
              (flycheck-mode 1))))

(use-package add-node-modules-path
   :config
   (add-hook 'js2-mode-hook 'add-node-modules-path)
   (add-hook 'rjsx-mode-hook 'add-node-modules-path))

(defun delete-tern-process()
  (interactive)
  (delete-process "Tern"))

(use-package prettier-js
  :diminish prettier-js-mode
  :config
  (setq prettier-js-args '(
                           "--arrow-parens" "always"
                           "--tab-width" "2"
                           "--use-tabs" "false"
                           "--trailing-comma" "all")))

(use-package eslint-fix
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook
               (lambda()
                 (add-hook 'after-save-hook 'eslint-fix nil t)))))

(use-package js2-refactor
  :defer t
  :commands (js2r-add-keybindings-with-prefix)
  :init
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

(require 'eds)
(require 'init-hydra)

(defvar hydra-stack nil)

(defun hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x
      (funcall x))))

(defhydra hydra-expose-js-refactoring (:color red :hint nil)
  "
^Functions/Methods^             ^Variables^                ^Debug^
^^^^^^^^^^^^^^^^^^^----------------------------------------------------------
_x_: extract function           _l_: extract to const      _d_: debug this
_m_: extract method             _i_: inline variable
_e_: expand node at point
_c_: contract node at point
_a_: args to object
    "
  ("x" (js2r-extract-function))
  ("m" (js2r-extract-method))
  ("e" (js2r-expand-node-at-point))
  ("c" (js2r-contract-node-at-point))
  ("a" (js2r-arguments-to-object))
  ("t" (js2r-toggle-arrow-function-and-expression))
  ("l" (js2r-extract-let))
  ("l" (js2r-extract-const))
  ("i" (js2r-inline-var))
  ("d" (js2r-debug-this))
  ("q" nil))

(defhydra hydra-my-javascript-macros (:color blue :hint nil)
    "
^Testing^                        ^Refactoring^
^^^^^^^^----------------------------------------------
_c_: insert a test case          _r_: refactoring menu
_t_: go to test/implementation

_q_: quit this menu
    "
    ("c" (eds/insert-enzyme-test-case t))
    ("t" (eds/toggle-test-implementation))
    ("r" (hydra-expose-js-refactoring/body))
    ("q" nil))


(eval-after-load 'js2-mode
  '(key-chord-define js2-mode-map "MM" 'hydra-my-javascript-macros/body))


(provide 'init-js)
