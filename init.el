(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(describe-char-unidata-list
   (quote
    (name old-name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value iso-10646-comment)))
 '(ispell-program-name "/usr/local/bin/aspell")
 '(org-agenda-files (quote ("~/git/org/notes.org")))
 '(package-selected-packages
   (quote
    (mc-extras multiple-cursors helm-ag org org-projectile exec-path-from-shell web-mode flycheck eclimd company-emacs-eclim eclim json-mode sql-upcase groovy-mode company-anaconda anaconda-mode helm-projectile projectile eldoc-eval smartparens helm-ls-git use-package editorconfig js2-mode feature-mode package+ magit helm ensime)))
 '(scala-indent:align-forms nil)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy 2)
 '(scala-indent:indent-value-expression t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; common lisp 
(require 'cl)

;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; useful global macros
(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo)) 

;; The rest of my init file, broken up into modules
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "proxy_load.el")
(load-user-file "set_environment.el")
(load-user-file "platform.el")
(load-user-file "appearance.el")
(load-user-file "global_key_bindings.el")
(load-user-file "global_behaviour_settings.el")
(load-user-file "helm_mode.el")
(load-user-file "programming.el")
(load-user-file "abbrev_mode.el")
(load-user-file "ansi_term.el")
(load-user-file "org.el")
(load-user-file "flycheck.el")
(load-user-file "multiple-cursors.el")
