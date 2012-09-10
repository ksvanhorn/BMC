(defpackage :prove-tests
  (:use :cl :lisp-unit :testing-utilities :prove :expr :utils :symbols))
(in-package :prove-tests)

; Turn lambda expression into combinator form
; Recover lambda expression from combinator form

; Function to take a a theorem
; (all (?X1 ... ?Xn) (=> (P ?X1 ... ?Xn) (= (E ?X1 ... ?Xn) (E1 ?X1 ... ?Xn))))
; and turn it into a function f such that (f e)
; - matches e with the pattern (E ?X1 ... ?Xn), failing unless the match
;   succeeds and it can prove (P ?X1 ... ?Xn) true;
; - if it doesn't fail, returns a theorem e = e1 obtained as a specialization
;   of the original theorem.

#|
(defconstant +I+ prove::+I+)
(defconstant +K+ prove::+K+)
(defconstant +S+ prove::+S+)
(defconstant +B+ prove::+B+)
(defconstant +C+ prove::+C+)

(defun ap (x y) (expr-call '! x y))

(define-test reduce-SKIBC-tests
  (dolist (e (mapcar #'sexpr->expr
	       '(5 @-all v (+ x y) (:quant qsum i (m n) (@ x i))
		 (:let (x 3) (+ x x)))))
    (assert-equalp e (prove::reduce-SKIBC (ap +I+ e)))
    (assert-equalp e (prove::reduce-SKIBC (ap (ap +K+ e) (expr-var 'a))))
)

; FINISH: S, K, B, C

)

(define-test de-lambda-tests
  (assert-equalp
    (expr-const 5)
    (prove::de-lambda (expr-const 5)))
  (assert-equalp
    (expr-const '@-all)
    (prove::de-lambda (expr-const '@-all)))
  (assert-equalp
    (expr-var 'x)
    (prove::de-lambda (expr-var 'x)))
  (let ((e (sexpr->expr '(* (+ x y) (/ a b) (^ x 5)))))
    (assert-equalp e (prove::de-lambda e)))

)
|#

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

  ; lambda expressions
  (assert-equalp
    (expr-lam 'x (sexpr->expr '(+ x (* y z))))
    (prove::subst-expr
      'v (sexpr->expr '(* y z)) (expr-lam 'x (sexpr->expr '(+ x v)))))
  (assert-equalp
    (expr-lam 'y1 (sexpr->expr '(+ (* y z) y1)))
    (prove::subst-expr
      'x (sexpr->expr '(* y z)) (expr-lam 'y (sexpr->expr '(+ x y)))))
  (let ((e (expr-lam 'x (sexpr->expr '(+ x y)))))
    (assert-equalp
      e (prove::subst-expr 'x (sexpr->expr '(^ y 2)) e)))
)

(define-test expand-distr-tests
  (assert-equalp
    (sexpr->expr
      '(/ (exp (neg (^ (- x (+ mu v)) 2))) (* (sqrt (* 2 %pi)) (* 2 sigma))))
    (prove::expand-densities
      (sexpr->expr
        '(dnorm-density (+ mu v) (* 2 sigma)))))
)

#|
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
)
|#

#|
  (assert-equal
     '((+ 3 (* x x)) . ((= x (sqrt z))))
     (prove::expr->prover-expr
       (sexpr->expr
	 '(:let (x (sqrt z)) (+ 3 (* x (:let (x y) x)))))))
|#

; substitution for variable

