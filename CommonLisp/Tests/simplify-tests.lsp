(defpackage :simplify-tests
  (:use :cl :lisp-unit :simplify :expr :symbols :utils))
(in-package :simplify-tests)

(defmacro assert-expr< (x y)
  `(progn
     (assert-equal -1
		   (simplify::expr-cmp (sexpr->expr ',x) (sexpr->expr ',y)))
     (assert-equal +1
		   (simplify::expr-cmp (sexpr->expr ',y) (sexpr->expr ',x)))))

(defmacro assert-not-expr< (x)
  `(assert-equal 0 (simplify::expr-cmp (sexpr->expr ',x) (sexpr->expr ',x))))

(define-test expr<-tests
  (assert-expr< 2 3)
  (assert-expr< -1 3/5)
  (assert-not-expr< 3)

  (assert-expr< %e %pi)
  (assert-expr< %pi @-all)
  (assert-not-expr< %e)

  (assert-expr< x y)
  (assert-not-expr< w)

  (assert-expr< (+ b b) (+ a c))
  (assert-expr< (+ b c) (+ a b c))
  (assert-not-expr< (+ a c))

  (assert-expr< (* b b) (* a c))
  (assert-expr< (* b c) (* a b c))
  (assert-not-expr< (* b c))

  (assert-expr< (^ x 3) (^ y 2))
  (assert-expr< (^ v a) (^ v b))
  (assert-not-expr< (^ v a))

  (assert-expr< (fac 3) (fac 10/3))
  (assert-not-expr< (fac x))

  (assert-expr< (exp (+ 1 x)) (gamma-fct (* x y)))
  (assert-expr< (gamma-fct 3) (tanh 2))

  (assert-expr< (exp (+ 1 x)) (exp (+ 1 y)))
  (assert-expr< (max 1 y) (max 2 x))
  (assert-expr< (dnorm-density v 0 sigma) (dnorm-density v 1 sigma))
  (assert-expr< (.and a b) (.and a b c))
  (assert-not-expr< (exp a))

  (assert-expr< 2/3 %pi)
  (assert-expr< 2.7 (* 0.5 12/11))
  (assert-expr< 2.7 (* a b))
  (assert-expr< 25/37 (^ 0.1 2/7))
  (assert-expr< 25/37 (^ a b))
  (assert-expr< 811 (+ 1 2))
  (assert-expr< 811 (+ a b))
  (assert-expr< 29.4 (fac 7))
  (assert-expr< 29.4 (fac x))
  (assert-expr< 128/3 (tanh 0.1))
  (assert-expr< 128/3 (tanh x))
  (assert-expr< -3 x)
  
  (assert-expr< %e (* 0.5 12/11))
  (assert-expr< %pi (* a b))
  (assert-expr< @-all (^ 0.1 2/7))
  (assert-expr< %e (^ a b))
  (assert-expr< %pi (+ 1 2))
  (assert-expr< @-all (+ a b))
  (assert-expr< %e (fac 7))
  (assert-expr< %pi (fac x))
  (assert-expr< @-all (tanh 0.1))
  (assert-expr< %e (tanh x))
  (assert-expr< %pi x)

  (assert-expr< (* 5 (^ 2 a)) (^ 2 b))
  (assert-expr< (^ a b) (* %pi (^ c d)))
  (assert-expr< (^ a b) (* %pi (^ a b)))
  (assert-expr< (* %pi (+ a b)) (+ a c))
  (assert-expr< (* 2 %pi) (fac x))
  (assert-expr< (fac x) (* 2 (fac y)))
  (assert-expr< (* 3 (+ x y) (tanh w)) (tanh x))
  (assert-expr< (tanh x) (* (+ x y) (tanh y)))
  (assert-expr< (* 2 x) y)
  (assert-expr< a (* 2 b))
  (assert-expr< a (* 2 a))

  (assert-expr< (^ (* a (+ t v)) y) (+ u v))
  (assert-expr< (^ (+ t v) y) (+ u v))
  (assert-expr< (+ a b) (^ (+ a b) 2))
  (assert-expr< (+ a b) (^ (+ aa c) -1))
  (assert-expr< (^ (+ a b) -1) (+ a b))
  (assert-expr< (^ (* %e %pi) y) (fac a))
  (assert-expr< (^ (fac a) 2) (fac b))
  (assert-expr< (fac a) (^ (fac b) -1))
  (assert-expr< (fac a) (^ (fac a) 2))
  (assert-expr< (^ (fac a) -1) (fac a))
  (assert-expr< (^ (* %e %pi) 3) (max 0 a))
  (assert-expr< (^ (max 0 a) 3) (max 0 b))
  (assert-expr< (max 0 a) (^ (max 0 a) 2))
  (assert-expr< (max 0 a) (^ (max 1 a) -1))
  (assert-expr< (^ (max 0 a) -1) (max 0 a))
  (assert-expr< (^ (* %e %pi) 3) a)
  (assert-expr< (^ a b) b)
  (assert-expr< a (^ a 2))
  (assert-expr< a (^ b -1))
  (assert-expr< (^ a -1) a)

  (assert-expr< (+ 3 (* %e %pi)) (fac a))
  (assert-expr< (+ 3 (fac a)) (fac b))
  (assert-expr< (fac a) (+ 3 (fac b)))
  (assert-expr< (fac a) (+ 3 (fac a)))
  (assert-expr< (+ 2 (^ 2 x)) (tanh 3))
  (assert-expr< (+ 2 (tanh x)) (tanh y))
  (assert-expr< (tanh x) (+ 2 (tanh y)))
  (assert-expr< (tanh x) (+ 2 (tanh x)))
  (assert-expr< (+ (* 2 a) (^ 2 x)) b)
  (assert-expr< (+ (* 2 b) a) b)
  (assert-expr< a (+ (* 2 %e) b))
  (assert-expr< a (+ (* 2 %e) a))

  (assert-expr< (fac %e) (tanh a))
  (assert-expr< (fac (tanh a)) (tanh b))
  (assert-expr< (tanh a) (fac (tanh b)))
  (assert-expr< (tanh a) (fac (tanh a)))
  (assert-expr< (fac %e) a)
  (assert-expr< (fac a) b)
  (assert-expr< a (fac a))

  (assert-expr< f (gamma-fct x))
  (assert-expr< (gamma-fct x) h)
  (assert-expr< max (max a b))

  (assert-expr< (:lambda x (* 2 x)) (:lambda x (* 3 x)))
  (assert-expr< (:lambda x (* w x)) (:lambda y (* w x)))
  (assert-not-expr< (:lambda x (^ x 2)))
  (assert-error 'error
    (expr-cmp (sexpr->expr '(:lambda x (* 2 x)))
	      (sexpr->expr '(* 3 x))))
  (assert-error 'error
    (expr-cmp (sexpr->expr '(* 3 x))
	      (sexpr->expr '(:lambda x (* 2 x)))))
)

(defclass mock-prover ()
  ((provables :init-arg :provables :init-form '())))

(defmethod is-provable ((prover mock-prover) boolean-expr)
  (with-slots (provables) prover
     (member boolean-expr provables)))

(defmethod also-assume (prover0 boolean-exprs)
  (with-slots (provables) prover0
    (make-instance 'mock-prover :provables (append boolean-exprs provables))))

(define-test simplify-tests
;;; *** HERE ***
)
