(require 'package)

;; mostly from https://github.com/purcell/emacs.d, but not concerning
;; myself with minimum versions and whether to refresh contents.

(defun require-package (package)
  "Install the given package."
  (if (package-installed-p package)
      t
    (if (assoc package package-archive-contents)
        (if (boundp 'package-selected-packages)
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package)))))

(defun maybe-require-package (package)
  "Try to install PACKAGE, and return non-nil if successful or
  nil on failure."
  (condition-case err
      (require-package package)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

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
(require 'bind-key)
(use-package diminish
  :ensure t)
(setq use-package-always-ensure t)


(provide 'init-elpa)
