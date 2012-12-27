(defpackage :variables-tests
  (:use cl :lisp-unit :variables :testing-utilities))
(in-package :variables-tests)

(define-test variables-tests
  (assert-equal 'vars::_FOO (vars:special-var "FOO"))
  (assert-equal 'vars::|_foo| (vars:special-var "foo"))
  (assert-equal 'vars::_foo (vars:special-var 'foo))
  (assert-error 'error (vars:special-var "foo_bar"))
  (assert-error 'error (vars:special-var "x1"))

  (assert-equal 'vars::_foo_bar (vars:special-var "FOO" 'bar))
  (assert-equal 'vars::_fum_back_up12 (vars::special-var "FUM" 'back_up12))
  (assert-error 'error (vars:special-var "foo_bar" 'bar))
  (assert-error 'error (vars:special-var "x1" 'backup))

  (with-genvar-counter 33
    (assert-equal 'vars::_33foo (vars:new-var "FOO"))
    (assert-equal 34 vars::*genvar-counter*))

  (with-genvar-counter 12
    (assert-equal 'vars::_12foo (vars:new-var 'foo))
    (assert-equal 13 vars::*genvar-counter*))

  (with-genvar-counter 26
    (assert-equal 'vars::_26 (vars:new-var ""))
    (assert-equal 27 vars::*genvar-counter*))

  (with-genvar-counter 63
    (assert-equal 'vars::_63i (vars:new-var '_25i))
    (assert-equal 64 vars::*genvar-counter*))

  (with-genvar-counter 15
    (assert-equal 'vars::_15j30 (vars:new-var 'j30))
    (assert-equal 16 vars::*genvar-counter*))

  (with-genvar-counter 12
    (assert-equal 'vars::_12 (vars:new-var '_5))
    (assert-equal 13 vars::*genvar-counter*))

  (with-genvar-counter 100
    (assert-equal '(vars::|_100i| vars::|_101i|)
		  (vars:n-new-vars 2 "i"))
    (assert-equal 102 vars::*genvar-counter*))

  (with-genvar-counter 57
    (assert-equal '(vars::|_57k|) (vars:n-new-vars 1 "k"))
    (assert-equal 58 vars::*genvar-counter*))

  (with-genvar-counter 23
    (assert-equal '(vars::_23foo vars::_24foo)
		  (vars:n-new-vars 2 'foo)))

  (with-genvar-counter 4
    (assert-error 'error (vars:new-var '_foo)))
  (with-genvar-counter 3
    (assert-error 'error (vars:n-new-vars 2 '_bar)))
  (with-genvar-counter 8
    (assert-error 'error (vars:new-var '_)))
)
