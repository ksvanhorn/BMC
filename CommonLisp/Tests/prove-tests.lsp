(defpackage :prove-tests
  (:use :cl :lisp-unit :testing-utilities :prove :expr :utils :symbols))
(in-package :prove-tests)

(define-test subst-expr-tests
  (assert-equalp
    (sexpr->expr 5)
    (prove::subst-expr 'x (sexpr->expr '(+ 3 z)) (sexpr->expr 5)))
  (assert-equalp
    (sexpr->expr 'true)
    (prove::subst-expr 'x (sexpr->expr '(+ 3 z)) (sexpr->expr 'true)))
  (assert-equalp
    (sexpr->expr 'false)
    (prove::subst-expr 'x (sexpr->expr '(+ 3 z)) (sexpr->expr 'false)))
  (assert-equalp
    (sexpr->expr '(+ 3 z))
    (prove::subst-expr 'x (sexpr->expr '(+ 3 z)) (sexpr->expr 'x)))
  (assert-equalp
    (sexpr->expr '(* 4 (+ 3 z)))
    (prove::subst-expr 'v (sexpr->expr '(+ 3 z)) (sexpr->expr '(* 4 v))))
  (assert-equalp
    (sexpr->expr '(* (+ 3 z) 4))
    (prove::subst-expr 'v (sexpr->expr '(+ 3 z)) (sexpr->expr '(* v 4))))

  ; let expressions
  (assert-equalp
    (sexpr->expr '(:let (x 13) (+ x (* y z))))
    (prove::subst-expr
      'v (sexpr->expr '(* y z)) (sexpr->expr '(:let (x 13) (+ x v)))))
  (assert-equalp
    (sexpr->expr '(:let (x (* y z)) (+ x y)))
    (prove::subst-expr
      'v (sexpr->expr '(* y z)) (sexpr->expr '(:let (x v) (+ x y)))))
  (assert-equalp
    (sexpr->expr '(:let (v z) (+ v y)))
    (prove::subst-expr
      'v (sexpr->expr '(* y z)) (sexpr->expr '(:let (v z) (+ v y)))))
  (assert-equalp
    (sexpr->expr '(:let (v (+ a (* y z))) (+ v y)))
    (prove::subst-expr
      'v (sexpr->expr '(* y z)) (sexpr->expr '(:let (v (+ a v)) (+ v y)))))
  (assert-equalp
    (sexpr->expr '(:let (y1 (+ a (* y z))) (+ (* y z) y1)))
    (prove::subst-expr
      'x (sexpr->expr '(* y z)) (sexpr->expr '(:let (y (+ a x)) (+ x y)))))
  (assert-equalp
    (sexpr->expr '(:let (y (+ a (* y z))) (+ v y)))
    (prove::subst-expr
      'x (sexpr->expr '(* y z)) (sexpr->expr '(:let (y (+ a x)) (+ v y)))))
  (assert-equalp
    (sexpr->expr '(:let (y (+ a (* y z))) (+ v y)))
    (prove::subst-expr
      'y (sexpr->expr '(* y z)) (sexpr->expr '(:let (y (+ a y)) (+ v y)))))
  (assert-equalp
    (sexpr->expr '(:let (y 13) (+ (qsum y (1 5) (@ z y)) y)))
    (prove::subst-expr
      'x (sexpr->expr '(qsum y (1 5) (@ z y)))
      (sexpr->expr '(:let (y 13) (+ x y)))))

  (assert-equalp
    (sexpr->expr '(qsum i (2 4) (@ y (+ i 7))))
    (prove::subst-expr
      'x (sexpr->expr 7) (sexpr->expr '(qsum i (2 4) (@ y (+ i x))))))

  ; quantifier expressions
  ;(assert-equalp
)

(define-test prove-tests
  (assert-equal
    3
    (prove::expr->prover-expr (make-expr-literal :value 3)))
  (assert-equal
    'true
    (prove::expr->prover-expr (make-expr-const :symbol 'true)))
  (assert-equal
    'false
    (prove::expr->prover-expr (make-expr-const :symbol 'false)))
  (assert-equal
    'v
    (prove::expr->prover-expr (make-expr-variable :symbol 'v)))
  (assert-equal
    '(+ x 3)
    (prove::expr->prover-expr (sexpr->expr '(+ x 3))))

  (assert-equal
     '(+ 3 (* (sqrt z) (sqrt z)))
     (prove::expr->prover-expr
       (sexpr->expr
	 '(:let (x (sqrt z)) (+ 3 (* x x))))))

#|
  (assert-equal
     '((+ 3 (* x x)) . ((= x (sqrt z))))
     (prove::expr->prover-expr
       (sexpr->expr
	 '(:let (x (sqrt z)) (+ 3 (* x (:let (x y) x)))))))
|#
)
; substitution for variable

