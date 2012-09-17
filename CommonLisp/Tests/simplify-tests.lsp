(defpackage :simplify-tests
  (:use :cl :lisp-unit :simplify :expr :symbols :utils))
(in-package :simplify-tests)


; is-basic-algebraic-expression
; is-automatically-simplified-algebraic-expression

; Need to add qsum, qprod


(defmacro assert-is-asae (x)
  `(assert-true (is-asae (sexpr->expr ',x))))

(defmacro assert-not-asae (x)
  `(assert-false (is-asae (sexpr->expr ',x))))

(define-test is-asae-tests
  ; ASAE = Automatically Simplified Algebraic Expression
  (assert-is-asae 1)
  (assert-is-asae x)
  (assert-is-asae %pi)
  (assert-is-asae -3/4)
  ;(assert-is-asae (max x y))
  ;(assert-is-asae (tanh 3))

  (assert-not-asae (* 2 3))
  (assert-not-asae (* 1 x))
  (assert-not-asae (* 0 x))
  (assert-is-asae (* 3/4 x))
  (assert-is-asae (* v w))
  (assert-is-asae (* 3 y))
  (assert-is-asae (* 2 %pi v))
)

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

  (assert-expr< (* b b) (* a c))
  (assert-expr< (* b c) (* a b c))
)