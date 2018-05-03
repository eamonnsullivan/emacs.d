;;; -*- lexical-binding: t -*-
;;; init-env.el -- set some env variables, for building and testing.

(setenv "SERVER_ENV" "dev")
;; for ensime-server development
(setenv "AKKA_TEST_TIMEFACTOR" "5")

(provide 'init-env)
