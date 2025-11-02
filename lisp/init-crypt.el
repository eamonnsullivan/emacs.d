;;; -*- lexical-binding: t -*-
;;; init-crypt.el --Experimenting with some encryption

(require 'org-crypt)
(require 'epa-file)
(epa-file-enable)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "499CD6EA565D20EA6296719F2A3D60BDFE015FFD")

(provide 'init-crypt)
