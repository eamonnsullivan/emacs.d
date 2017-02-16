;; ensime
(use-package ensime
  :ensure t
  :config
  (setq ensime-use-helm t))

;; anaconda
(use-package anaconda-mode
  :ensure t
  :config
  ;; bug with anaconda-mode and company-mode
  (remove-hook 'anaconda-mode-response-read-fail-hook
               'anaconda-mode-show-unreadable-response))

;; smartparens-mode
(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map)
  (add-hook 'scala-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'python-mode-hook #'smartparens-mode))

;; documentation at point
(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :commands eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;; python
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'company-mode)

;; auto indent in programming modes
(add-hook 'python-mode-hook (lambda () (local-set-key (kbd "RET")
                                                      'newline-and-indent)))

;; company mode
(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda))

;; show-paren-mode
;; show a subtle blinking of the matching paren (the defaults are ugly)
;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when-available 'show-paren-mode
  (progn
    (show-paren-mode t)
    (setq show-paren-style 'parenthesis)
    (set-face-background 'show-paren-match-face "#666666")
    (set-face-attribute 'show-paren-match-face nil
                        :weight 'normal :underline nil :overline nil :slant 'normal)))

;; cc-mode customizations.
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
(add-hook 'c++-initialization-hook 'my-make-CR-do-indent)

(add-hook 'c-mode-common-hook (lambda ()
                                (c-set-style "bsd")
                                (setq c-basic-offset 4)
                                (setq indent-tabs-mode nil)
                                (font-lock-add-keywords nil
                                                        '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
                                (auto-fill-mode 1)
                                (modify-syntax-entry ?_ "w")
                                (setq c-font-lock-extra-types
                                      (list "gboolean"
                                            "gsize" "gssize"
                                            "gchar" "guchar"
                                            "gint" "gint8" "gint16" "gint32"
                                            "guint" "guint8" "guint16" "guint32"
                                            "gshort" "gushort" "glong" "gulong"
                                            "gfloat" "gdouble" "gpointer"
                                            "gconstpointer"
                                            "GList" "GSList" "GFunc" "GString"))))

;; javascript mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(use-package editorconfig
  :ensure t
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

;; java
(use-package company-emacs-eclim
  :ensure t
  :config
  (company-emacs-eclim-setup))
(use-package eclim
  :ensure t
  :commands start-eclimd
  :init
  (add-hook 'java-mode-hook 'eclim-mode)
  (add-hook 'java-mode-hook 'company-mode)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  :config
  (setq eclimd-autostart t)
  (setq eclimd-autostart-with-default-workspace t)
  (setq eclimd-executable "/Applications/Eclipse.app/Contents/Eclipse/eclimd"))


;; customize ensime's implementation/test goto configuration for the
;; BBC's slightly non-standard layout in some older projects. Going
;; from the implementation to the test file works OK, but going back
;; doesn't. I still need to fix this.
(defun bbc-goto-test--is-test-dir (dir)
  (let ((case-fold-search nil))
    (or
     (string-match-p "src/test/scala/$" dir)
     (string-match-p "../test/scala/$" dir)
     (string-match-p "../tests?/$" dir))))

(defconst bbc-test-template
  "package %TESTPACKAGE%

import org.scalatest.{FlatSpec, MustMatchers}

class %TESTCLASS% extends FlatSpec with MustMatchers {

  \"%IMPLCLASS%\" should \"have a test!\" in {
    fail(\"no test\")
  }
}
"
  "The value to insert into a new test file")

(defun bbc-impl-class-name (test-class)
  (let ((suffixes (ensime-get-goto-test-config :test-class-suffixes))
        (case-fold-search nil))
    (dolist (s suffixes)
      (when (string-match-p (concat s "$") test-class)
        (message (format "Found the class name: %s for test class %s"
                         (replace-regexp-in-string (concat s "$") "" test-class)
                         test-class))
        (return (replace-regexp-in-string (concat s "$") "" test-class))))))

(setq ensime-goto-test-config-defaults
      (list :test-class-names-fn #'ensime-goto-test--test-class-names
            :test-class-suffixes '("Spec" "Test" "Check" "Specification")
            :impl-class-name-fn #'bbc-impl-class-name
            :impl-to-test-dir-fn #'ensime-goto-test--impl-to-test-dir
            :is-test-dir-fn #'bbc-goto-test--is-test-dir
            :test-template-fn (lambda () bbc-test-template)))

;; ctags stuff
(setq path-to-ctags "/usr/local/bin/ctags")
(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (progn
      (message (format "Running ctags on directory: %s" dir-name))
      (message
       (format "Result: %s"
               (shell-command-to-string
                (format "%s -f %s/TAGS -e -R %s"
                        path-to-ctags
                        (directory-file-name dir-name)
                        (directory-file-name dir-name)))))))

;; Trying projectile
(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile-find-file)

;; feature-mode
(setq feature-step-search-path "src/test/scala/steps/**/*Steps.scala")

;; Try to find the step defining the current feature
(defun select-current-line ()
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

(defun get-selected-text (beg end)
  "message region or \"empty string\" if none highlighted"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (message "%s" (if (and beg end)
                    (buffer-substring-no-properties beg end)
                  "empty string")))

;; sql mode stuff
(when (require 'sql-upcase nil :noerror)
   (add-hook 'sql-mode-hook 'sql-upcase-mode)
   (add-hook 'sql-interactive-mode-hook 'sql-upcase-mode))

