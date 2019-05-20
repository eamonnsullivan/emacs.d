(require 'package)

(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")
                    ("elpy" . "http://jorgenschaefer.github.io/packages/"))
 package-archive-priorities '(("melpa" . 1)))

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package bind-key
  :ensure t)
(use-package diminish
  :ensure t)
(setq use-package-always-ensure t)

(put 'use-package 'lisp-indent-function 1)

(use-package paradox
  :init (defalias 'upgrade-packages 'paradox-upgrade-packages)
  :config
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t)
  (setq paradox-automatically-star nil)
  (setq paradox-display-star-count nil))

(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

(use-package quelpa-use-package :ensure t)

(use-package use-package-secrets
  :custom
  (use-package-secrets-default-directory "~/.emacs.d/secrets")
  :quelpa
  (use-package-secrets :repo "a13/use-package-secrets" :fetcher github :version original))

(provide 'init-elpa)
