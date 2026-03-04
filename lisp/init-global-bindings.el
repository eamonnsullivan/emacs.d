;;; -*- lexical-binding: t -*-
;;; init-global-bindings.el --- keys available in all modes

;; global key bindings
(global-set-key "\C-z" nil)
;; alternative to the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(require 'eds-utils)
(require 'init-utils)
(require 'init-hydra)
(require 'init-org)
(global-unset-key (kbd "M-<backspace>"))
(global-set-key (kbd "M-<backspace>") 'eds-utils/backward-kill-word)

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)


(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (switch-to-buffer "*scratch*")
  (save-some-buffers t)
  (mapc 'kill-buffer (eds-utils/filter-buffer-list (buffer-list))))

(defun stop-and-restart-emacs ()
  "Restarts EMACS."
  (interactive)
  (eds-utils/restart-emacs t))

;; (global-set-key (kbd "RET") 'newline-and-indent)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(use-package expreg
  :config
  (setopt expreg-restore-point-on-quit t)
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))


(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)

(global-set-key (kbd "C-c i b") #'copilot-chat-add-current-buffer)

(defhydra hydra-goto-line (goto-map ""
                                    :pre (display-line-numbers-mode 1)
                                    :post (display-line-numbers-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))

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

(key-chord-define-global "XX" 'calendar)
(key-chord-define-global "II" 'mu4e)


(provide 'init-global-bindings)
