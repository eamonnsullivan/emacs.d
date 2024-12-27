;;; -*- lexical-binding: t -*-
;;; init-cc.el --- stuff related to C and C++

(defun my-make-CR-do-indent ()
  (defvar c-mode-base-map)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
(add-hook 'c++-initialization-hook 'my-make-CR-do-indent)

(add-hook 'c-mode-common-hook (lambda ()
                                (c-set-style "bsd")
                                (defvar c-basic-offset 4)
                                (setq indent-tabs-mode nil)
                                (font-lock-add-keywords nil
                                                        '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
                                (auto-fill-mode 1)
                                (modify-syntax-entry ?_ "w")
                                (defvar c-font-lock-extra-types
                                      (list "gboolean"
                                            "gsize" "gssize"
                                            "gchar" "guchar"
                                            "gint" "gint8" "gint16" "gint32"
                                            "guint" "guint8" "guint16" "guint32"
                                            "gshort" "gushort" "glong" "gulong"
                                            "gfloat" "gdouble" "gpointer"
                                            "gconstpointer"
                                            "GList" "GSList" "GFunc" "GString"))))

(use-package zig-mode)

(provide 'init-cc)
