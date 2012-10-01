#!/usr/bin/env clisp

(load "code-to-test")
(load "../../lisp-unit/lisp-unit")
(load "Tests/testing-utilities")
(in-package :cl-user)

(defmacro runtests (name)
  (let* ((name-s (symbol-name name))
	 (pkg-path (utils:strcat "Tests/" (string-downcase name-s) "-tests"))
	 (pkg-name (intern (utils:strcat name-s "-TESTS"))))
    `(progn
       (load ,pkg-path)
       (lisp-unit:run-all-tests ,pkg-name))))

#|
(runtests adt)
(runtests utils)
(runtests expr)
(runtests model)
|#
(runtests mcimpl)
#|
(runtests simplify)
(runtests prove)
(runtests compile)
|#
