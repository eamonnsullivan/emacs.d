;;; init-server.el --- Emacs server initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-03-16
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: server, convenience, tools
;; URL: https://github.com/eamonnsullivan/init-server

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for the Emacs server,
;; enabling client-server editing and workflow enhancements.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;; This programme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Licence for more details.

;; You should have received a copy of the GNU General Public Licence
;; along with this programme.  If not, see <https://www.gnu.org/licenses/>.

(require 'server)
(add-hook 'after-init-hook (lambda ()
                             (unless (or (daemonp) (server-running-p))
                               (progn
                                 (setopt server-use-tcp t)
                                 (server-start)
                                 (require 'org-protocol)
                                 ;; apply my display preferences to the first frame
                                 (my-appearance-settings t))
                               (setopt server-raise-frame t))))

(provide 'init-server)
;;; init-server.el ends here
