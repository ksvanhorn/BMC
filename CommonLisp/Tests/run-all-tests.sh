#!/usr/bin/env clisp

(load "code-to-test")
(load "../../lisp-unit/lisp-unit")
(use-package :lisp-unit)
(load "Tests/testing-utilities")
(in-package :cl-user)

(load "Tests/adt-tests")
(load "Tests/utils-tests")
(load "Tests/expr-tests")
(load "Tests/model-tests")
(load "Tests/compile-tests")

(lisp-unit:run-tests)