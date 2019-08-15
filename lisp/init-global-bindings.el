;;; -*- lexical-binding: t -*-
;;; init-global-bindings.el --- keys available in all modes

;; global key bindings
(global-set-key "\C-z" nil)
;; alternative to the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
;; backward-kill-word, restart-emacs
(require 'eds)
(global-unset-key (kbd "M-<backspace>"))
(global-set-key (kbd "M-<backspace>") 'eds/backward-kill-word)

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(defun kill-all-buffers ()
  "Kill all buffers"
  (interactive)
  (switch-to-buffer "*scratch*")
  (save-some-buffers t)
  (mapc 'kill-buffer
        (delq (current-buffer) (buffer-list))))

(defun stop-and-restart-emacs ()
  "Restarts emacs"
  (interactive)
  (eds/restart-emacs t))

(global-set-key (kbd "RET") 'newline-and-indent)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(use-package expand-region :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(set-register ?n '(file . "~/Dropbox/org/notes.org"))
(set-register ?t '(file . "~/Dropbox/org/tasks.org"))
(set-register ?p '(file . "~/Dropbox/org/personal.org"))

(defhydra hydra-goto-line (goto-map ""
                                    :pre (display-line-numbers-mode 1)
                                    :post (display-line-numbers-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))

(global-set-key (kbd "C-c m")
                (defhydra hydra-transpose (:color red)
                  "Transpose"
                  ("c" transpose-chars "characters")
                  ("w" transpose-words "words")
                  ("o" org-transpose-words "Org mode words")
                  ("l" transpose-lines "lines")
                  ("s" transpose-sentences "sentences")
                  ("e" org-transpose-elements "Org mode elements")
                  ("p" transpose-paragraphs "paragraphs")
                  ("t" org-table-transpose-table-at-point "Org mode table")
                  ("q" nil "cancel" :color blue)))

(global-set-key (kbd "C-c @")
                (defhydra hydra-hs (:idle 1.0
                                          :pre (hs-minor-mode 1)
                                          :post (hs-minor-mode -1))
                  "
Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_oggle    _n_ext line
_d_ hide block    _a_ show block              _p_revious line
_l_ hide level

_SPC_ cancel
"
                  ("s" hs-show-all)
                  ("h" hs-hide-all)
                  ("a" hs-show-block)
                  ("d" hs-hide-block)
                  ("t" hs-toggle-hiding)
                  ("l" hs-hide-level)
                  ("n" forward-line)
                  ("p" (forward-line -1))
                  ("SPC" nil)))

(provide 'init-global-bindings)
