(defpackage :mcimpl-tests
  (:use :cl :lisp-unit :mcimpl :model :expr :symbols :utils :testing-utilities))
(in-package :mcimpl-tests)

(define-test sexpr->parameters-tests
  (assert-equalp
    (list (make-decl :var 'foo :typ (make-vtype-scalar :stype 'realp))
	  (make-decl :var 'bar :typ (make-vtype-scalar :stype 'integerp))
	  (make-decl :var 'baz :typ (make-vtype-scalar :stype 'realp)))
    (mcimpl::sexpr->parameters
      '(:parameters (foo realp) (bar integerp) (baz realp))))

  (assert-equalp
    '()
    (mcimpl::sexpr->parameters '(:parameters)))

  (assert-error 'error
    (mcimpl::sexpr->parameters 1))
  (assert-error 'error
    (mcimpl::sexpr->parameters '()))
  (assert-error 'error
    (mcimpl::sexpr->parameters '((v realp))))
)

(define-test sexpr->derived-tests
  (assert-equalp
    '()
    (mcimpl::sexpr->derived '(:derived)))

  (assert-equalp
    `((foo . ,(expr-call '+ (expr-var 'x) (expr-var 'y)))
      (bar . ,(expr-call 'exp (expr-call '@ (expr-var 'x) (expr-const 2)))))
    (mcimpl::sexpr->derived
     '(:derived (foo (+ x y))
		(bar (exp (@ x 2))))))

  (assert-error 'error
    (mcimpl::sexpr->derived '()))
  (assert-error 'error
    (mcimpl::sexpr->derived 1))
  (assert-error 'error
    (mcimpl::sexpr->derived '(huh (foo (+ x y)))))
  (assert-error 'error
    (mcimpl::sexpr->derived '((foo (+ x y)))))
  (assert-error 'error
    (mcimpl::sexpr->derived '(:derived ())))
  (assert-error 'error
    (mcimpl::sexpr->derived '(:derived a)))
  (assert-error 'error
    (mcimpl::sexpr->derived '(:derived (foo))))
  (assert-error 'error
    (mcimpl::sexpr->derived '(:derived ((+ x y)))))
  (assert-error 'error
    (mcimpl::sexpr->derived '(:derived (foo (+ x y) (* a b)))))
)
