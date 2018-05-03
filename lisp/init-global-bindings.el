;;; -*- lexical-binding: t -*-
;;; init-global-bindings.el --- keys available in all modes

;; global key bindings
(global-set-key "\C-z" nil)
;; alternative to the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
;; add keystroke for backwards-kill-word and assigns a different
;; keystroke for kill-region
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

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

(global-set-key (kbd "C-z C-d") 'kill-all-buffers)
(global-set-key (kbd "RET") 'newline-and-indent)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-z C-r") 'stop-and-restart-emacs)

(use-package expand-region :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(provide 'init-global-bindings)