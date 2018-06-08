;;; -*- lexical-binding: t -*-
;;; init-java.el --- stuff related to coding in java

(add-hook 'java-mode-hook (lambda()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode nil)))

(provide 'init-java)
