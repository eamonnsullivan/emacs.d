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
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
              (local-set-key (kbd "C-c l") 'js-load-file-and-go))))

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


;; my own shortcuts
(defun eds/insert-enzyme-test-case (arg)
  "Insert a skeleton test case at point. If a prefix is used, make it synchronous."
  (interactive "P")
  (move-beginning-of-line 1)
  (let ((description (read-string "Test description: "))
        (function-signature (if arg "()" "async ()")))
    (insert (format "test('%s', %s => {\n\n});" description function-signature))
    (previous-line)
    (indent-for-tab-command)))

(defun eds/insert-skeleton-test-file (export module-name)
  "Insert a basic skeleton for a test file."
  (goto-char (point-min))
  (insert "import React from 'react';\n")
  (insert "import { shallow } from 'enzyme';\n")
  (insert (format "import %s from '%s';\n\n" export module-name))
  (insert "test('', async () => {\n\n});\n"))

(defun eds/find-default-export ()
  "Try to find the default export for the current module."
  (save-excursion
    (goto-char (point-min))
    (let ((pos (re-search-forward "^export default " nil t)))
      (if pos
          (let ((default-export (string-trim (buffer-substring-no-properties pos (point-max)))))
            (if (string-match ";$" default-export)
                (substring default-export 0 -1)
              default-export))
        "{ NoDefaultExportFound }"))))

(defun eds/is-buffer-javascript-file ()
  "Are we viewing a javascript file in the current buffer."
  (and buffer-file-name (equal (downcase (file-name-extension buffer-file-name)) "js")))

(defun eds/is-buffer-javascript-test-file ()
  "Are we viewing a javascript test file in the current buffer"
  (string-match ".test.js$" buffer-file-name))

(defun eds/create-or-open-enzyme-test-file (basename test-file)
  "Open the current buffer's test file or create one if none is found."
  (if (file-exists-p test-file)
      (find-file test-file)
    (let* ((module-name (file-name-nondirectory basename))
           (default-export (eds/find-default-export)))
      (find-file test-file)
      (rjsx-mode)
      (eds/insert-skeleton-test-file default-export module-name))))

(defun eds/toggle-between-test-and-implementation ()
  "Open the current buffer's test file or create one if none is found."
  (interactive)
  (if (eds/is-buffer-javascript-file)
      (if (eds/is-buffer-javascript-test-file)
          (let* ((basename (substring buffer-file-name 0 (string-match ".test.js$" buffer-file-name)))
                 (implementation-file (concat basename ".js")))
            (find-file implementation-file))
        (let* ((basename (substring buffer-file-name 0 (string-match ".js$" buffer-file-name)))
               (test-file (concat basename ".test.js")))
          (eds/create-or-open-enzyme-test-file basename test-file)))
    (message "Only javascript files are supported at the moment.")))

(require 'init-hydra)

(defvar eds/javascript-macros
  (defhydra "hydra-my-javascript-macros" (:color blue)
    ("c" (eds/insert-enzyme-test-case nil) "Insert a test case")
    ("t" (eds/toggle-between-test-and-implementation) "Toggle between test and implementation file")
    ("q" nil "quit")))

(eval-after-load 'js2-mode
  '(key-chord-define js2-mode-map "MM" eds/javascript-macros))


(provide 'init-js)
