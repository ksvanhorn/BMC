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

(define-test pat-match-tests
  (let ((e (expr-const 3)))
    (assert-equalp
      `(t (?x . ,e))
      (prove::pat-match e (expr-var '?x)))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e (expr-var 'x)))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e (expr-const 4)))
    (assert-equalp
      '(t . nil)
      (prove::pat-match e (expr-const 3)))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e (sexpr->expr '(^1/2 3)))))

  (let ((e (expr-var 'v)))
    (assert-equalp
      `(t (?x . ,e))
      (prove::pat-match e (expr-var '?x)))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e (expr-var 'x)))
    (assert-equalp
      '(t . nil)
      (prove::pat-match e (expr-var 'v)))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e (expr-const 4)))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e (expr-call '^1/2 (expr-const 3)))))

  (let ((e (sexpr->expr '(.and v))))
    (assert-equalp
      `(t (?y . ,e))
      (prove::pat-match e (expr-var '?y)))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e (expr-var 'v)))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e (expr-var 'x)))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e (sexpr->expr '(neg v))))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e (sexpr->expr '(.and v w))))
    (assert-equalp
      '(t . nil)
      (prove::pat-match e (sexpr->expr '(.and v))))
    (assert-equalp
      `(t (?y . ,(expr-var 'v)))
      (prove::pat-match e (sexpr->expr '(.and ?y)))))

  (let ((e (expr-lam 'v (sexpr->expr '(+ (^ v 3) (^1/2 m))))))
    (dolist (pat (mapcar #'sexpr->expr
			 '(32 v x (+ a b) (+ ?v ?x))))
      (assert-equalp '(nil . nil) (prove::pat-match e pat))))
  ; TODO: more tests?
)

(define-test pattern-creation-tests
  (assert-equalp
    (sexpr->expr '(+ b a))
    (funcall (prove::se-pattern-xform '((+ ?x ?y) . (+ ?y ?x)))
	     (sexpr->expr '(+ a b))))

  (assert-equalp
    (sexpr->expr '(* a b))
    (funcall (prove::se-pattern-xform '((+ ?x ?y) . (+ ?y ?x)))
	     (sexpr->expr '(* a b))))

  (assert-equalp
    (sexpr->expr 7)
    (funcall (prove::se-pattern-xform '((* 1 ?v) . ?v))
	     (sexpr->expr (* 1 7))))

  (let ((xform1 (prove::se-pattern-xform
		  '((* ?a (* ?b ?c)) . (* (* ?a ?b) ?c))))
	(xform2 (prove::se-pattern-xform
		  '((^1/2 ?x) . (^ ?x 1/2))))
	(xform3 (prove::se-pattern-xform
		  '((neg (neg ?x)) . ?x))))
    (assert-equalp
      (sexpr->expr '(^ (+ a b) 1/2))
      (funcall (prove::compose-xforms xform1 xform2 xform3)
	       (sexpr->expr '(^1/2 (+ a b)))))

    (assert-equalp
      (sexpr->expr '(* (* x y) z))
      (funcall (prove::compose-xforms xform3 xform1 xform2)
	       (sexpr->expr '(neg (neg (* x (* y z)))))))

    (assert-equalp
      (sexpr->expr '(* (* a b) (+ (* (* d e) f) 7)))
      (funcall (prove::recurse xform1)
	       (sexpr->expr '(* a (* b (+ (* d (* e f)) 7))))))

    (assert-equalp
      (sexpr->expr '(:let (x (^ y 1/2)) (* x (^ z 1/2))))
      (funcall (prove::recurse xform2)
	       (sexpr->expr '(:let (x (^1/2 y)) (* x (^1/2 z))))))

    (assert-equalp
      (sexpr->expr '(* (* (* a b) c) d))
      (funcall (prove::closure xform1)
	       (sexpr->expr '(* a (* b (* c d))))))
)
)

(define-test eliminate-extraneous-ops-tests
  (assert-equalp
    (sexpr->expr '(* -1 a))
    (prove::eliminate-extraneous-ops
      (sexpr->expr '(neg a))))

  (assert-equalp
    (sexpr->expr '(+ b (* -1 a)))
    (prove::eliminate-extraneous-ops
      (sexpr->expr '(- b a))))

  (assert-equalp
    (sexpr->expr '(* a (^ b -1)))
    (prove::eliminate-extraneous-ops
      (sexpr->expr '(/ a b))))

  (assert-equalp
    (sexpr->expr '(^ %e a))
    (prove::eliminate-extraneous-ops
      (sexpr->expr '(exp a))))

  (assert-equalp
    (sexpr->expr '(^ a 1/2))
    (prove::eliminate-extraneous-ops
      (sexpr->expr '(^1/2 a))))

  (assert-equalp
    (sexpr->expr '(^ b 2))
    (prove::eliminate-extraneous-ops
      (sexpr->expr '(^2 b))))

  (assert-equalp
    (sexpr->expr '(+ (^ (* b c) 1/2)
		     (^ (^ %e (+ (* c (^ (* -1 (^ %e d)) -1))
				 (* -1 b)))
			2)))
    (prove::eliminate-extraneous-ops
      (sexpr->expr
        '(+ (^1/2 (* b c))
	    (^2 (exp (- (/ c (neg (exp d))) b)))))))
)

(define-test expand-distr-tests
  (assert-equalp
    (sexpr->expr
      '(* (/ 1 (* (^1/2 (* 2 %pi)) (* 2 sigma)))
	  (exp (* -1/2 (^2 (- v (+ mu y)))))))
    (prove::expand-densities
      (sexpr->expr '(dnorm-density v (+ mu y) (* 2 sigma)))))

  (assert-equalp
    (sexpr->expr
     '(* (< 0 (@ x i)) (/ (^ beta alpha) (gamma-fct alpha))
	 (^ (@ x i) (- alpha 1)) (exp (* -1 beta (@ x i)))))
    (prove::expand-densities
      (sexpr->expr '(dgamma-density (@ x i) alpha beta))))

  (assert-equalp
    (sexpr->expr
      '(* (<= 1 y) (<= y (length p)) (@ p y)))
    (prove::expand-densities
      (sexpr->expr '(dcat-density y p))))

  (assert-equalp
    (sexpr->expr
      '(if-then-else (< (+ x a) (@ c 1))
	 (= v 1)
         (if-then-else (<= (@ c (length c)) (+ x a))
	   (= v (+ 1 (length c)))
	   (* (<= (@ c (- v 1)) (+ x a)) (< (+ x a) (@ c v))))))
    (prove::expand-densities
      (sexpr->expr '(dinterval-density v (+ x a) c))))

  (assert-equalp
    (sexpr->expr
     '(*
	(:quant qand i (1 (length p)) (< 0 (@ p i)))
	(= 1 (sum p))
	(/ (gamma-fct (sum alpha))
	   (:quant qprod i (1 (length alpha)) (gamma-fct (@ alpha i))))
	(:quant qprod i (1 (length alpha)) (^ (@ p i) (- (@ alpha i) 1)))))
    (prove::expand-densities
      (sexpr->expr '(ddirch-density p alpha))))
     
  (assert-equalp
    (sexpr->expr
     '(*
	(:quant qand i1 (1 (length i)) (< 0 (@ i i1)))
	(= 1 (sum i))
	(/ (gamma-fct (sum alpha))
	   ; the renaming of i to i1 in the next factor is unnecessary but legit
	  (:quant qprod i1 (1 (length alpha)) (gamma-fct (@ alpha i1))))
	(:quant qprod i1 (1 (length alpha)) (^ (@ i i1) (- (@ alpha i1) 1)))))
    (prove::expand-densities
      (sexpr->expr '(ddirch-density i alpha))))

  (assert-equalp
    (sexpr->expr 
      '(* (is-symm-pd Lambda)
	  (:let (k (array-length 1 V))
	    (* (^ 2 (* -1/2 nu k))
	       (^ (abs-det V) (* -1/2 nu))
	       (/ 1 (mv-gamma-fct k (* 1/2 nu)))
	       (^ (abs-det Lambda) (* 1/2 (- nu k 1)))
	       (exp (* -1/2 (trace (dot (inv V) Lambda))))))))
    (prove::expand-densities
      (sexpr->expr '(dwishart-density Lambda nu V))))

  (assert-equalp
    (sexpr->expr 
      '(* (is-symm-pd Lambda)
	  (:let (k1 (array-length 1 V))
	    (* (^ 2 (* -1/2 k k1))
	       (^ (abs-det V) (* -1/2 k))
	       (/ 1 (mv-gamma-fct k1 (* 1/2 k)))
	       (^ (abs-det Lambda) (* 1/2 (- k k1 1)))
	       (exp (* -1/2 (trace (dot (inv V) Lambda))))))))
    (prove::expand-densities
      (sexpr->expr '(dwishart-density Lambda k V))))

  (assert-equalp
   (sexpr->expr
     '(:let (k (array-length 1 Sigma))
	(* (^ (* 2 %pi) (* -1/2 k))
	   (^ (abs-det Sigma) -1/2)
	   (exp (* -1/2 (quad (inv Sigma) (@- v mu)))))))
   (prove::expand-densities
     (sexpr->expr '(dmvnorm-density v mu Sigma))))

  (assert-equalp
    (sexpr->expr
      '(* (:quant qprod j (1 n)
		  (* (/ 1 (* (^1/2 (* 2 %pi)) (^ lambda -1/2)))
		     (exp (* -1/2 (^2 (- (@ x j) m))))))
	  (* (< 0 lambda) (/ (^ beta alpha) (gamma-fct alpha))
	     (^ lambda (- alpha 1)) (exp (* -1 beta lambda)))))
    (prove::expand-densities
      (sexpr->expr
        '(* (:quant qprod j (1 n) (dnorm-density (@ x j) m (^ lambda -1/2)))
	    (dgamma-density lambda alpha beta)))))
)

(define-test expand-products-tests
  (assert-equalp
    (sexpr->expr '(* a b c))
    (prove::expand-products (sexpr->expr '(* a (* b c)))))

  (assert-equalp
    (sexpr->expr '(* a b c))
    (prove::expand-products (sexpr->expr '(* (* a b) c))))

  (assert-equalp
    (sexpr->expr '(* (^ a n) (^ b n) (^ c n)))
    (prove::expand-products
      (sexpr->expr '(^ (* a b c) n))))

  (assert-equalp
    (sexpr->expr '(* (:quant qprod k (1 n) (@ x k))
		     (:quant qprod k (1 n) (@ y k))))
    (prove::expand-products
      (sexpr->expr '(:quant qprod k (1 n) (* (@ x k) (@ y k))))))

  (assert-equalp
    (sexpr->expr
      '(:quant qprod k (1 n) (^ (@ x i) n)))
    (prove::expand-products
      (sexpr->expr '(^ (:quant qprod k (1 n ) (@ x i)) n))))

  (assert-equalp
    (sexpr->expr '(* (^ a1 (* b c)) (^ a2 (* b c))))
    (prove::expand-products
      (sexpr->expr '(^ (^ (* a1 a2) b) c))))

  (assert-equalp
    (sexpr->expr '(* (:quant qprod k (1 m) (^ (@ x i) n))
		     (:quant qprod k (1 m) (^ (@ y i) n))))
    (prove::expand-products
      (sexpr->expr '(^ (:quant qprod k (1 m) (* (@ x i) (@ y i))) n))))

  (assert-equalp
    (sexpr->expr '(* (:quant qprod i1 (lo hi) (^ (^1/2 (@ x i1)) (@ n i)))
		     (:quant qprod i1 (lo hi) (^ (@ y i1) (@ n i)))))
    (prove::expand-products
      (sexpr->expr '(^ (:quant qprod i (lo hi) (* (^1/2 (@ x i)) (@ y i)))
		       (@ n i)))))

  (assert-equalp
    (sexpr->expr '(* (^ a d) (^ b (* c d))))
    (prove::expand-products
      (sexpr->expr
        '(^ (* a (^ b c)) d))))

  (assert-equalp
    (sexpr->expr '(:let (x (+ a b)) (* (^ x -1) (^ (@ v i) -1))))
    (prove::expand-products
      (sexpr->expr
        '(:let (x (+ a b)) (^ (* x (@ v i)) -1)))))

  (assert-equalp
    (sexpr->expr '(if-then-else (< 0 x)
				(* (^ a n) (^ b n))
				(* a b c)))
    (prove::expand-products
      (sexpr->expr
        '(if-then-else (< 0 x)
		       (^ (* a b) n)
		       (* (* a b) c)))))

  (assert-equalp
    (sexpr->expr '(* (^ a b) (^ a c)))
    (prove::expand-products
      (sexpr->expr
        '(^ a (+ b c)))))

#|
  (assert-equalp
    (sexpr->expr '(+ (* a b) (* a c)))
    (prove::expand-quadratic
      (sexpr->expr '(* a (+ b c)))))
|#

  (assert-equalp
    (sexpr->expr
      '(* x (+ a b) (^ c n) (^ d n) (^ e n)
	  (:quant qprod i ((* m n) (* m k)) (@ y i))
	  (:quant qprod i ((* m n) (* m k)) (@ z i))))
    (prove::expand-products
      (sexpr->expr
        '(* x (* (+ a b) (^ (* c d e) n))
	    (:quant qprod i ((* m n) (* m k))
		    (* (@ y i) (@ z i)))))))
)

(define-test simplify-lengths-tests
  (assert-equalp
    (sexpr->expr '(+ x y))
    (prove::simplify-lengths
      (sexpr->expr '(+ x y))))
  (assert-equalp
    (sexpr->expr '(max 0 (- b (- a 1))))
    (prove::simplify-lengths
      (sexpr->expr '(array-length 1 (:quant qvec i (a b) (^2 i))))))
  (assert-equalp
    (sexpr->expr 2)
    (prove::simplify-lengths
      (sexpr->expr '(array-length 1 (vec (+ a b) (- c d))))))
  (assert-equalp
    (sexpr->expr
      '(* a (:quant qprod i (1 k)
		    (if-then-else y b (max 0 (- k (- j 1)))))))
    (prove::simplify-lengths
      (sexpr->expr
        '(* a (:quant qprod i (1 k)
		      (if-then-else y
			b
			(length (:quant qvec i (j k) (^1/2 i)))))))))
)

(define-test fold-constants-tests
  (let* ((e1 (expr-const 5))
	 (e2 (expr-var 'v))
	 (e3 (expr-call '+ e2 e1))
	 (e4 (expr-lam 'x e3))
	 (e5 (expr-call '+ e1 (expr-const '%pi))))
    (dolist (e (list e1 e2 e3 e4 e5))
      (assert-equalp e (prove::fold-constants e))))

  (assert-equalp
    (expr-const 0)
    (prove::fold-constants (sexpr->expr '(neg 0))))

  (assert-equalp
    (expr-const 7)
    (prove::fold-constants (sexpr->expr '(+ 3 4))))

  (assert-equalp
    (expr-const 12)
    (prove::fold-constants (sexpr->expr '(* 2 6))))

  (assert-equalp
    (expr-const 1/4)
    (prove::fold-constants (sexpr->expr '(^ 1/2 2))))

  (assert-equalp
    (sexpr->expr '(^ 2 -1/2))
    (prove::fold-constants (sexpr->expr '(^ 2 (* 1/2 -1)))))
)

(define-test neg-product-tests
  (assert-equalp
    (sexpr->expr '(* -2 a))
    (prove::neg-product-xform
      (sexpr->expr '(neg (* 2 a)))))

  (assert-equalp
    (sexpr->expr '(* -3/4 a b))
    (prove::neg-product-xform
      (sexpr->expr '(neg (* 3/4 a b)))))

  (assert-equalp
    (sexpr->expr '(* -5.25 a b c))
    (prove::neg-product-xform
      (sexpr->expr '(neg (* 5.25 a b c)))))

  (assert-equalp
    (sexpr->expr '(neg (* b a)))
    (prove::neg-product-xform
      (sexpr->expr '(neg (* b a)))))
)

(define-test simplify-add-zero-tests
  (assert-equalp
    (sexpr->expr '(+ a b))
    (prove::simplify-add-zero
      (sexpr->expr '(+ a b))))

  (assert-equalp
    (sexpr->expr '(+ a b))
    (prove::simplify-add-zero
      (sexpr->expr '(+ a b 0))))

  (assert-equalp
    (sexpr->expr '(+ a b))
    (prove::simplify-add-zero
      (sexpr->expr '(+ a 0 b))))

  (assert-equalp
    (sexpr->expr '(+ a b))
    (prove::simplify-add-zero
      (sexpr->expr '(+ 0 a b))))

  (assert-equalp
    (sexpr->expr 'a)
    (prove::simplify-add-zero
      (sexpr->expr '(+ 0 a))))

  (assert-equalp
    (sexpr->expr 'a)
    (prove::simplify-add-zero
      (sexpr->expr '(+ a 0))))

  (assert-equalp
    (sexpr->expr '(^ a 2))
    (prove::simplify-add-zero
      (sexpr->expr '(^ (+ a 0) 2))))

  (assert-equalp
    (sexpr->expr '(^ (+ a b) 2))
    (prove::simplify-add-zero
      (sexpr->expr '(^ (+ a 0 b) 2))))
)

(define-test simplify-mul-one-tests
  (dolist (e (cons (expr-lam 'v (expr-call '^ 3 4))
		   (mapcar #'sexpr->expr '((* a b) a 3))))
    (assert-equalp e (prove::simplify-mul-one e)))

  (assert-equalp
    (sexpr->expr '(* a b))
    (prove::simplify-mul-one
      (sexpr->expr '(* a b 1))))

  (assert-equalp
    (sexpr->expr '(* a b))
    (prove::simplify-mul-one
      (sexpr->expr '(* a 1 b))))

  (assert-equalp
    (sexpr->expr '(* a b))
    (prove::simplify-mul-one
      (sexpr->expr '(* 1 a b))))

  (assert-equalp
    (sexpr->expr 'a)
    (prove::simplify-mul-one
      (sexpr->expr '(* 1 a))))

  (assert-equalp
    (sexpr->expr 'a)
    (prove::simplify-mul-one
      (sexpr->expr '(* a 1))))
)

(define-test eliminate-let-expressions
  (assert-equalp
    (sexpr->expr '(+ x 3))
    (prove::eliminate-let-expressions
      (sexpr->expr '(:let (v 3) (+ x v)))))

  (assert-equalp
    (sexpr->expr 3)
    (prove::eliminate-let-expressions
      (sexpr->expr 3)))

  (assert-equalp
    (sexpr->expr 'x)
    (prove::eliminate-let-expressions
      (sexpr->expr 'x)))

  (assert-equalp
    (sexpr->expr '(* (+ x v) 4))
    (prove::eliminate-let-expressions
      (sexpr->expr '(* (+ x v) 4))))

  (assert-equalp
    (sexpr->expr '(+ (* (+ x 3) (+ x 3)) b (^ (* (+ x 3) (+ x 3)) 3)))
    (prove::eliminate-let-expressions
      (sexpr->expr '(:let (a (:let (b (+ x 3))
			       (* b b)))
		      (:let (c (^ a 3))
			(+ a b c))))))
)

(define-test simplify-qprod-unvarying-body-tests
  (assert-equalp
    (sexpr->expr 12)
    (prove::simplify-qprod-unvarying-body
      (sexpr->expr 12)))

  (assert-equalp
    (sexpr->expr 'x)
    (prove::simplify-qprod-unvarying-body
      (sexpr->expr 'x)))

  (assert-equalp
    (sexpr->expr '(* a (+ x y)))
    (prove::simplify-qprod-unvarying-body
      (sexpr->expr '(* a (+ x y)))))

  (assert-equalp
    (sexpr->expr '(:quant qprod i (m n) (@ x i)))
    (prove::simplify-qprod-unvarying-body
      (sexpr->expr '(:quant qprod i (m n) (@ x i)))))

  (assert-equalp
    (sexpr->expr '(^ (+ x y) (- n (- m 1))))
      (prove::simplify-qprod-unvarying-body
        (sexpr->expr '(:quant qprod j (m n) (+ x y)))))
)

(defun sexpr->exprs (se) (mapcar #'sexpr->expr se))

(define-test expand-array-lengths-tests
  (assert-equalp
    '()
    (prove::var-dims-pats '(x . ())))
  (assert-equalp
    (list (cons (sexpr->expr '(array-length 1 x))
		(sexpr->expr 1)))
    (prove::var-dims-pats (cons 'x (sexpr->exprs '(1)))))
  (assert-equalp
    (list (cons (sexpr->expr '(array-length 1 x))
		(sexpr->expr 'k)))
    (prove::var-dims-pats (cons 'x (sexpr->exprs '(k)))))
  (assert-equalp
    (list (cons (sexpr->expr '(array-length 1 x))
		(sexpr->expr 'n))
	  (cons (sexpr->expr '(array-length 2 x))
		(sexpr->expr '(* m m))))
    (prove::var-dims-pats (cons 'x (sexpr->exprs '(n (* m m))))))

  (assert-equalp
    (sexpr->expr '(+ n m k n k))
    (funcall (prove::expand-array-lengths
	       (list (cons 'x (sexpr->exprs '(n)))
		     (cons 'y (sexpr->exprs '(m k)))
		     (cons 'z (sexpr->exprs '((array-length 1 x)
					      (array-length 2 y))))))
	     (sexpr->expr
	       '(+ (array-length 1 x)
		   (array-length 1 y)
		   (array-length 2 y)
		   (array-length 1 z)
		   (array-length 2 z)))))
)

; TODO: sexpr->expr should reduce (+ x y z) to (+ (+ x y) z)?

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
     '(+ 3 (* (^1/2 z) (^1/2 z)))
     (prove::expr->prover-expr
       (sexpr->expr
	 '(:let (x (^1/2 z)) (+ 3 (* x x))))))
)
|#

#|
  (assert-equal
     '((+ 3 (* x x)) . ((= x (^1/2 z))))
     (prove::expr->prover-expr
       (sexpr->expr
	 '(:let (x (^1/2 z)) (+ 3 (* x (:let (x y) x)))))))
|#

; substitution for variable

