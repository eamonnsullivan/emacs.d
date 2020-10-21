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
     (cons "\\.js$" (defun choose-js-type-mode ()
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
              ;; js-indent-level 2
              ;; js2-basic-offset 2
              (setq js2-highlight-level 3
                    js2-bounce-indent-p t
                    indent-tabs-mode nil
                    js2-strict-missing-semi-warning nil)
              ;; (tern-mode)
              (js2-imenu-extras-mode)
              (js2-refactor-mode)
              (auto-revert-mode)
              (flycheck-mode 1))))

(use-package add-node-modules-path
   :config
   (add-hook 'js2-mode-hook 'add-node-modules-path)
   (add-hook 'rjsx-mode-hook 'add-node-modules-path))

(use-package eslint-fix
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook
               (lambda()
                 (add-hook 'after-save-hook 'eslint-fix nil t)))))

(use-package js2-refactor
  :after (js2-mode hydra)
  :hook
  (js2-mode . js2-refactor-mode)

  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")

  (require 'eds)
  (require 'init-hydra)

  ;; Hydra / js2
  (defhydra js2-refactor-hydra (:color blue :hint nil)
    "
      Javascript

  ^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
  ------------------------------------------------------------------------------------------------------------------------------
  _lp_: Localize Parameter      _ev_: Extract variable   _wi_: Wrap buffer in IIFE    _k_:  js2 kill      _lt_: log this
  _ef_: Extract function        _iv_: Inline variable    _ig_: Inject global in IIFE  _ss_: split string  _dt_: debug this
  _ip_: Introduce parameter     _rv_: Rename variable    _ee_: Expand node at point   _sl_: forward slurp
  _em_: Extract method          _vt_: Var to this        _cc_: Contract node at point _ba_: forward barf
  _ao_: Arguments to object     _sv_: Split var decl.    _uw_: unwrap
  _tf_: Toggle fun exp and decl _ag_: Add var to globals
  _ta_: Toggle fun expr and =>  _ti_: Ternary to if
"
    ("ee" js2r-expand-node-at-point)
    ("cc" js2r-contract-node-at-point)
    ("ef" js2r-extract-function)
    ("em" js2r-extract-method)
    ("tf" js2r-toggle-function-expression-and-declaration)
    ("ta" js2r-toggle-arrow-function-and-expression)
    ("ip" js2r-introduce-parameter)
    ("lp" js2r-localize-parameter)
    ("wi" js2r-wrap-buffer-in-iife)
    ("ig" js2r-inject-global-in-iife)
    ("ag" js2r-add-to-globals-annotation)
    ("ev" js2r-extract-var)
    ("iv" js2r-inline-var)
    ("rv" js2r-rename-var)
    ("vt" js2r-var-to-this)
    ("ao" js2r-arguments-to-object)
    ("ti" js2r-ternary-to-if)
    ("sv" js2r-split-var-declaration)
    ("ss" js2r-split-string)
    ("uw" js2r-unwrap)
    ("lt" js2r-log-this)
    ("dt" js2r-debug-this)
    ("sl" js2r-forward-slurp)
    ("ba" js2r-forward-barf)
    ("k" js2r-kill)
    ("q" nil)))


(defhydra hydra-my-javascript-macros (:color blue :hint nil)
    "
^Testing^                        ^Refactoring^
-------------------------------------------------------
_c_: insert a test case          _r_: refactoring menu
_t_: go to test/implementation

_q_: quit this menu
    "
    ("c" (eds/insert-enzyme-test-case t))
    ("t" (eds/toggle-test-implementation))
    ("r" (progn
           (js2-refactor-hydra/body)
           (hydra-push '(hydra-my-javascript-macros/body))))
    ("q" nil))

(eval-after-load 'js2-mode
  '(key-chord-define js2-mode-map "MM" 'hydra-my-javascript-macros/body))

(provide 'init-js)
