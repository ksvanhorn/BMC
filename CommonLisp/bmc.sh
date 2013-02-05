#!/usr/bin/env clisp

(load "~/quicklisp/setup.lisp" :verbose nil)
(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "alexandria"))
(load "bmc.lsp")
 