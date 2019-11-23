;;; -*- lexical-binding: t -*-
;;; init-eshell.el --- Stuff to do with (rarely used) eshell.

;; from http://www.coli.uni-saarland.de/~slemaguer/emacs/main.html
(use-package eshell
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/export "NODE_NO_READLINE=1")))

  :config
  (use-package eshell-git-prompt
    :init
    (add-hook 'eshell-load-hook
              (lambda () (eshell-git-prompt-use-theme "robbyrussell"))))

  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                        (-> ,ICON
                            (concat esh-section-delim ,FORM)
                            (with-face ,@PROPS))))))

  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))

  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))

  (esh-section esh-dir
               "\xf07c"  ;  (faicon folder)
               (abbreviate-file-name (eshell/pwd))
               '(:foreground "blue" :bold ultra-bold :underline t))

  (esh-section esh-git
               "\xe907"  ;  (git icon)
               (magit-get-current-branch)
               '(:foreground "red"))

  (esh-section esh-python
               "\xe928"  ;  (python icon)
               pyvenv-virtual-env-name)

  (esh-section esh-clock
               "\xf017"  ;  (clock icon)
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))

  ;; Below I implement a "prompt number" section
  (setq esh-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))

  (esh-section esh-num
               "\xf0c9"  ;  (list icon)
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))


  (setq esh-sep " | " ; "  "  or " | " ;; Separator between esh-sections
        esh-section-delim " " ;; Separator between an esh-section icon and form
        esh-header "\n " ;; Eshell prompt header

        ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
        ;; your login, these can be the same.
        eshell-prompt-regexp "[^└]└─> "
        eshell-prompt-string "└─> "

        eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num) ;; Choose which eshell-funcs to enable
        eshell-prompt-function 'esh-prompt-func ;; Enable the new eshell prompt
        )

  (defun eshell-mode-some-config ()
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
    (eshell/export "NODE_NO_READLINE=1"))

  (add-hook 'eshell-mode-hook
            'eshell-mode-some-config)


  ;; If I ever want my own eshell/foo commands overwrite real commands ...
  (setq eshell-prefer-lisp-functions t)

  ;; Helpers
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun clipboard/set (astring)
    "Copy a string to clipboard"
    (with-temp-buffer
      (insert astring)
      (clipboard-kill-region (point-min) (point-max))))

  (defun eshell/copy-pwd ()
    "Copy current directory to clipboard "
    (clipboard/set (eshell/pwd)))

  (defun eshell/copy-fpath (fname)
    "Copy file name with full path to clipboard "
    (let ((fpath (concat (eshell/pwd) "/" fname)))
      (clipboard/set fpath)
      (concat "Copied path: " fpath)))


  (defun eshell/emacs (&rest args)
    "Open a file (ARGS) in Emacs.  Some habits die hard."
    (if (null args)
        ;; If I just ran "emacs", I probably expect to be launching
        ;; Emacs, which is rather silly since I'm already in Emacs.
        ;; So just pretend to do what I ask.
        (bury-buffer)
      ;; We have to expand the file names or else naming a directory in an
      ;; argument causes later arguments to be looked for in that directory,
      ;; not the starting directory
      (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args)))))))

(provide 'init-eshell)
