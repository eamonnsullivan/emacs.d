;;; -*- lexical-binding: t -*-
;;; init-server.el -- starting/stopping the emacs server

(defun eds/kill-emacs ()
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(defun eds/restart-emacs (arg)
  "Close emacs, with a prefix arg restart it."
  (interactive "P")
  (let ((confirm-kill-emacs (unless arg 'y-or-n-p))
        (kill-emacs-query-functions
         (if arg
             (append (list
                      (lambda ()
                        (when (y-or-n-p (format "Really restart %s? "
                                                (capitalize invocation-name)))
                          (add-hook 'kill-emacs-hook
                                    (lambda ()
                                      (call-process-shell-command
                                       (format "(%s &)"
                                               (or (executable-find "emacs")
                                                   (executable-find "remacs")))))
                                    t))))
                     kill-emacs-query-functions)
           kill-emacs-query-functions)))
    (eds/kill-emacs)))

;; server
(require 'server)
(add-hook 'after-init-hook (lambda ()
                             (unless (or (daemonp) (server-running-p))
                               (progn
                                 (server-start)
                                 ;; apply my display preferences to the first frame
                                 (my-appearance-settings t))
                               (setq server-raise-frame t))))

(provide 'init-server)
