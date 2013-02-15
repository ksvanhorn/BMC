(defpackage :prove-tests
  (:use :cl :lisp-unit :testing-utilities :prove :expr :utils :symbols))
(in-package :prove-tests)

(define-test subst-expr-tests
  (assert-equalp
    #e5
    (prove::subst-expr 'vars::x #e(+ 3 z) #e5))

  (assert-equalp
    #etrue
    (prove::subst-expr 'vars::x #e(+ 3 z) #etrue))

  (assert-equalp
    #efalse
    (prove::subst-expr 'vars::x #e(+ 3 z) #efalse))

  (assert-equalp
    #e(+ 3 z)
    (prove::subst-expr 'vars::x #e(+ 3 z) #e x))

  (assert-equalp
    #e(* 4 (+ 3 z))
    (prove::subst-expr 'vars::v #e(+ 3 z) #e(* 4 v)))

  (assert-equalp
    #e(* (+ 3 z) 4)
    (prove::subst-expr 'vars::v #e(+ 3 z) #e(* v 4)))

  ; lambda expressions
  (with-genvar-counter 16
    (assert-equalp
     #e(:lambda _16x (+ _16x (* y z)))
     (prove::subst-expr
      'vars::v #e(* y z) #e(:lambda x (+ x v)))))

  (with-genvar-counter 82
    (assert-equalp
     #e(:lambda _82y (+ (* y z) _82y))
     (prove::subst-expr
      'vars::x #e(* y z) #e(:lambda y (+ x y)))))

  (let ((e0 #e(:lambda x (+ x y)))
	(e1 #e(:lambda x (+ x y))))
    (assert-equalp
      e1 (prove::subst-expr 'vars::x #e(^ y 2) e0)))

  (let ((e0 #e(:let (y (* y y)) (+ a y)))
	(e1 #e(:let (y (* 2 2)) (+ a y))))
    (assert-equalp
      e1 (prove::subst-expr 'vars::y #e2 e0)))

  ; Regression test
  (let ((e0 #e(:let (a 37) (+ y a)))
	(e1 #e(:let (a 37) (+ y a))))
    (assert-equalp
      e1 (prove::subst-expr 'vars::a #e(^2 foo) e0)))
)

(define-test pat-match-tests
  (assert-equalp
    `(t (vars::?x . #e3))
    (prove::pat-match #e3 #e?x))
  (assert-equalp
    '(nil . nil)
    (prove::pat-match #e3 #ex))
  (assert-equalp
    '(nil . nil)
    (prove::pat-match #e3 #e4))
  (assert-equalp
    '(t . nil)
    (prove::pat-match #e3 #e3))
  (assert-equalp
    '(nil . nil)
    (prove::pat-match #e3 #e(^1/2 3)))

  (assert-equalp
    `(t (vars::?x . #ev))
    (prove::pat-match #ev #e?x))
  (assert-equalp
    '(nil . nil)
    (prove::pat-match #ev #ex))
  (assert-equalp
    '(t . nil)
    (prove::pat-match #ev #ev))
  (assert-equalp
    '(nil . nil)
    (prove::pat-match #ev #e4))
  (assert-equalp
    '(nil . nil)
    (prove::pat-match #ev #e(^1/2 3)))

  (let ((e #e(and v)))
    (assert-equalp
      `(t (vars::?y . ,e))
      (prove::pat-match e #e?y))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e #ev))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e #ex))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e #e(neg v)))
    (assert-equalp
      '(nil . nil)
      (prove::pat-match e #e(and v w)))
    (assert-equalp
      '(t . nil)
      (prove::pat-match e #e(and v)))
    (assert-equalp
      `(t (vars::?y . #ev))
      (prove::pat-match e #e(and ?y))))

  (let ((e #e(:lambda v (+ (^ v 3) (^1/2 m)))))
    (dolist (pat '(#e32 #ev #ex #e(+ a b) #e(+ ?v ?x)))
      (assert-equalp '(nil . nil) (prove::pat-match e pat))))
  ; TODO: more tests?
)

(define-test pattern-creation-tests
  (assert-equalp
    #e(+ b a)
    (funcall (prove::se-pattern-xform '((+ ?x ?y) . (+ ?y ?x)))
	     #e(+ a b)))

  (assert-equalp
    #e(* a b)
    (funcall (prove::se-pattern-xform '((+ ?x ?y) . (+ ?y ?x)))
	     #e(* a b)))

  (assert-equalp
    #e7
    (funcall (prove::se-pattern-xform '((* 1 ?v) . ?v))
	     #e(* 1 7)))

  (let ((xform1 (prove::se-pattern-xform
		  '((* ?a (* ?b ?c)) . (* (* ?a ?b) ?c))))
	(xform2 (prove::se-pattern-xform
		  '((^1/2 ?x) . (^ ?x 1/2))))
	(xform3 (prove::se-pattern-xform
		  '((neg (neg ?x)) . ?x))))
    (assert-equalp
      #e(^ (+ a b) 1/2)
      (funcall (prove::compose-xforms xform1 xform2 xform3)
	       #e(^1/2 (+ a b))))

    (assert-equalp
      #e(* (* x y) z)
      (funcall (prove::compose-xforms xform3 xform1 xform2)
	       #e(neg (neg (* x (* y z))))))

    (assert-equalp
      #e(* (* a b) (+ (* (* d e) f) 7))
      (funcall (prove::recurse xform1)
	       #e(* a (* b (+ (* d (* e f)) 7)))))

    (assert-equalp
      #e(:let (x (^ y 1/2)) (* x (^ z 1/2)))
      (funcall (prove::recurse xform2)
	       #e(:let (x (^1/2 y)) (* x (^1/2 z)))))

    (assert-equalp
      #e(* (* (* a b) c) d)
      (funcall (prove::closure xform1)
	       #e(* a (* b (* c d))))))
)

(define-test eliminate-extraneous-ops-tests
  (assert-equalp
    #e(* -1 a)
    (prove::eliminate-extraneous-ops #e(neg a)))

  (assert-equalp
    #e(+ b (* -1 a))
    (prove::eliminate-extraneous-ops #e(- b a)))

  (assert-equalp
    #e(* a (^ b -1))
    (prove::eliminate-extraneous-ops #e(/ a b)))

  (assert-equalp
    #e(^ %e a)
    (prove::eliminate-extraneous-ops #e(exp a)))

  (assert-equalp
    #e(^ a 1/2)
    (prove::eliminate-extraneous-ops #e(^1/2 a)))

  (assert-equalp
    #e(^ b 2)
    (prove::eliminate-extraneous-ops #e(^2 b)))

  (assert-equalp
    #e(+ (^ (* b c) 1/2)
	 (^ (^ %e (+ (* c (^ (* -1 (^ %e d)) -1))
		     (* -1 b)))
	    2))
    (prove::eliminate-extraneous-ops
      #e(+ (^1/2 (* b c))
	   (^2 (exp (- (/ c (neg (exp d))) b))))))
)

(define-test expand-distr-tests
  (assert-equalp
    #e(* (/ 1 (* (^1/2 (* 2 %pi)) (* 2 sigma)))
	 (exp (* -1/2 (^2 (- v (+ mu y))))))
    (prove::expand-densities #e(dnorm-density v (+ mu y) (* 2 sigma))))

  (assert-equalp
    #e(* (< 0 (@ x i)) (/ (^ beta alpha) (gamma-fct alpha))
	 (^ (@ x i) (- alpha 1)) (exp (* -1 beta (@ x i))))
    (prove::expand-densities #e(dgamma-density (@ x i) alpha beta)))

  (assert-equalp
    #e(* (<= 1 y) (<= y (length p)) (@ p y))
    (prove::expand-densities #e(dcat-density y p)))

  (assert-equalp
    #e(if-then-else (< (+ x a) (@ c 1))
        (= v 1)
	(if-then-else (<= (@ c (length c)) (+ x a))
	  (= v (+ 1 (length c)))
	  (* (<= (@ c (- v 1)) (+ x a)) (< (+ x a) (@ c v)))))
    (prove::expand-densities #e(dinterval-density v (+ x a) c)))

  (with-genvar-counter 38
    (assert-equalp
      #e(* (:quant qand _41i (1 (length p)) (< 0 (@ p _41i)))
	   (= 1 (sum p))
	   (/ (gamma-fct (sum alpha))
	      (:quant qprod _42i (1 (length alpha))
		      (gamma-fct (@ alpha _42i))))
	   (:quant qprod _43i (1 (length alpha))
		   (^ (@ p _43i) (- (@ alpha _43i) 1))))
      (prove::expand-densities #e(ddirch-density p alpha))))
    
  (with-genvar-counter 19
    (assert-equalp
      #e(* (:quant qand _22i (1 (length i)) (< 0 (@ i _22i)))
	   (= 1 (sum i))
	   (/ (gamma-fct (sum alpha))
	      (:quant qprod _23i (1 (length alpha))
		      (gamma-fct (@ alpha _23i))))
	   (:quant qprod _24i (1 (length alpha))
		   (^ (@ i _24i) (- (@ alpha _24i) 1))))
     (prove::expand-densities #e(ddirch-density i alpha))))

  (with-genvar-counter 53
    (assert-equalp
      #e(* (is-symm-pd Lambda)
	   (:let (_55k (array-length 1 V))
		 (* (^ 2 (* -1/2 nu _55k))
		    (^ (abs-det V) (* -1/2 nu))
		    (/ 1 (mv-gamma-fct _55k (* 1/2 nu)))
		    (^ (abs-det Lambda) (* 1/2 (- nu _55k 1)))
		    (exp (* -1/2 (trace (dot (inv V) Lambda)))))))
      (prove::expand-densities #e(dwishart-density Lambda nu V))))

  (with-genvar-counter 44
    (assert-equalp
      #e(* (is-symm-pd Lambda)
	   (:let (_46k (array-length 1 V))
		 (* (^ 2 (* -1/2 k _46k))
		    (^ (abs-det V) (* -1/2 k))
		    (/ 1 (mv-gamma-fct _46k (* 1/2 k)))
		    (^ (abs-det Lambda) (* 1/2 (- k _46k 1)))
		    (exp (* -1/2 (trace (dot (inv V) Lambda)))))))
     (prove::expand-densities #e(dwishart-density Lambda k V))))

  (with-genvar-counter 26
    (assert-equalp
      #e(:let (_28k (array-length 1 Sigma))
	      (* (^ (* 2 %pi) (* -1/2 _28k))
		 (^ (abs-det Sigma) -1/2)
		 (exp (* -1/2 (quad (inv Sigma) (@- v mu))))))
     (prove::expand-densities #e(dmvnorm-density v mu Sigma))))

  (with-genvar-counter 15
    (assert-equalp
      #e(* (:quant qprod j (1 n)
		   (* (/ 1 (* (^1/2 (* 2 %pi)) (^ lambda -1/2)))
		      (exp (* -1/2 (^2 (- (@ x j) m))))))
	   (* (< 0 lambda) (/ (^ beta alpha) (gamma-fct alpha))
	      (^ lambda (- alpha 1)) (exp (* -1 beta lambda))))
      (prove::expand-densities
        #e(* (:quant qprod j (1 n) (dnorm-density (@ x j) m (^ lambda -1/2)))
	     (dgamma-density lambda alpha beta)))))
)

(define-test expand-products-tests
  (assert-equalp
    #e(* a b c)
    (prove::expand-products #e(* a (* b c))))

  (assert-equalp
    #e(* a b c)
    (prove::expand-products #e(* (* a b) c)))

  (assert-equalp
    #e(* (^ a n) (^ b n) (^ c n))
    (prove::expand-products #e(^ (* a b c) n)))

  (assert-equalp
    #e(* (:quant qprod k (1 n) (@ x k))
	 (:quant qprod k (1 n) (@ y k)))
    (prove::expand-products
      #e(:quant qprod k (1 n) (* (@ x k) (@ y k)))))

  (with-genvar-counter 42
    (assert-equalp
      #e(:quant qprod _42k (1 n) (^ (@ x _42k) n))
      (prove::expand-products #e(^ (:quant qprod k (1 n ) (@ x k)) n))))

  (assert-equalp
    #e(* (^ a1 (* b c)) (^ a2 (* b c)))
    (prove::expand-products #e(^ (^ (* a1 a2) b) c)))

  (with-genvar-counter 28
    (assert-equalp
      #e(* (:quant qprod _28k (1 m) (^ (@ x _28k) n))
	   (:quant qprod _28k (1 m) (^ (@ y _28k) n)))
      (prove::expand-products
        #e(^ (:quant qprod k (1 m) (* (@ x k) (@ y k))) n))))

  (with-genvar-counter 14
    (assert-equalp
      #e(* (:quant qprod _14i (lo hi)
		   (^ (^1/2 (@ x _14i)) (@ n i)))
	   (:quant qprod _14i (lo hi)
		   (^ (@ y _14i) (@ n i))))
      (prove::expand-products
        #e(^ (:quant qprod i (lo hi) (* (^1/2 (@ x i)) (@ y i)))
	     (@ n i)))))

  (assert-equalp
    #e(* (^ a d) (^ b (* c d)))
    (prove::expand-products #e(^ (* a (^ b c)) d)))

  (assert-equalp
    #e(:let (x (+ a b)) (* (^ x -1) (^ (@ v i) -1)))
    (prove::expand-products
      #e(:let (x (+ a b)) (^ (* x (@ v i)) -1))))

  (assert-equalp
    #e(if-then-else (< 0 x)
		    (* (^ a n) (^ b n))
		    (* a b c))
    (prove::expand-products
      #e(if-then-else (< 0 x)
		      (^ (* a b) n)
		      (* (* a b) c))))

  (assert-equalp
    #e(* (^ a b) (^ a c))
    (prove::expand-products #e(^ a (+ b c))))

#|
  (assert-equalp
    (sexpr->expr '(+ (* a b) (* a c)))
    (prove::expand-quadratic
      (sexpr->expr '(* a (+ b c)))))
|#

  (with-genvar-counter 5
    (assert-equalp
      #e(* x (+ a b) (^ c n) (^ d n) (^ e n)
	   (:quant qprod i ((* m n) (* m k)) (@ y i))
	   (:quant qprod i ((* m n) (* m k)) (@ z i)))
      (prove::expand-products
        #e(* x (* (+ a b) (^ (* c d e) n))
	     (:quant qprod i ((* m n) (* m k))
		     (* (@ y i) (@ z i)))))))
)

(define-test simplify-lengths-tests
  (assert-equalp
    #e(+ x y)
    (prove::simplify-lengths #e(+ x y)))
  (assert-equalp
    #e(max 0 (- b (- a 1)))
    (prove::simplify-lengths
      #e(array-length 1 (:quant qvec i (a b) (^2 i)))))
  (assert-equalp
    #e2
    (prove::simplify-lengths
      #e(array-length 1 (vec (+ a b) (- c d)))))
  (assert-equalp
    #e(* a (:quant qprod i (1 k)
		   (if-then-else y b (max 0 (- k (- j 1))))))
    (prove::simplify-lengths
      #e(* a (:quant qprod i (1 k)
		     (if-then-else y
		       b
		       (length (:quant qvec i (j k) (^1/2 i))))))))
)

(define-test fold-constants-tests
  (let* ((e1 #e5)
	 (e2 #ev)
	 (e3 (expr-call '+ e2 e1))
	 (e4 (expr-lam 'x e3))
	 (e5 (expr-call '+ e1 (expr-const '%pi))))
    (dolist (e (list e1 e2 e3 e4 e5))
      (assert-equalp e (prove::fold-constants e))))

  (assert-equalp
    #e0
    (prove::fold-constants #e(neg 0)))

  (assert-equalp
    #e7
    (prove::fold-constants #e(+ 3 4)))

  (assert-equalp
    #e12
    (prove::fold-constants #e(* 2 6)))

  (assert-equalp
    #e1/4
    (prove::fold-constants #e(^ 1/2 2)))

  (assert-equalp
    #e(^ 2 -1/2)
    (prove::fold-constants #e(^ 2 (* 1/2 -1))))
)

(define-test neg-product-tests
  (assert-equalp
    #e(* -2 a)
    (prove::neg-product-xform #e(neg (* 2 a))))

  (assert-equalp
    #e(* -3/4 a b)
    (prove::neg-product-xform #e(neg (* 3/4 a b))))

  (assert-equalp
    #e(* -5.25 a b c)
    (prove::neg-product-xform #e(neg (* 5.25 a b c))))

  (assert-equalp
    #e(neg (* b a))
    (prove::neg-product-xform #e(neg (* b a))))
)

(define-test simplify-add-zero-tests
  (assert-equalp
    #e(+ a b)
    (prove::simplify-add-zero #e(+ a b)))

  (assert-equalp
    #e(+ a b)
    (prove::simplify-add-zero #e(+ a b 0)))

  (assert-equalp
    #e(+ a b)
    (prove::simplify-add-zero #e(+ a 0 b)))

  (assert-equalp
    #e(+ a b)
    (prove::simplify-add-zero #e(+ 0 a b)))

  (assert-equalp
    #ea
    (prove::simplify-add-zero #e(+ 0 a)))

  (assert-equalp
    #ea
    (prove::simplify-add-zero #e(+ a 0)))

  (assert-equalp
    #e(^ a 2)
    (prove::simplify-add-zero #e(^ (+ a 0) 2)))

  (assert-equalp
    #e(^ (+ a b) 2)
    (prove::simplify-add-zero #e(^ (+ a 0 b) 2)))
)

(define-test simplify-mul-one-tests
  (dolist (e '(#e(:lambda v (^ 3 4)) #e(* a b) #ea #e3))
    (assert-equalp e (prove::simplify-mul-one e)))

  (assert-equalp
    #e(* a b)
    (prove::simplify-mul-one #e(* a b 1)))

  (assert-equalp
    #e(* a b)
    (prove::simplify-mul-one #e(* a 1 b)))

  (assert-equalp
    #e(* a b)
    (prove::simplify-mul-one #e(* 1 a b)))

  (assert-equalp
    #ea
    (prove::simplify-mul-one #e(* 1 a)))

  (assert-equalp
    #ea
    (prove::simplify-mul-one #e(* a 1)))
)

(define-test eliminate-let-expressions
  (assert-equalp
    #e(+ x 3)
    (prove::eliminate-let-expressions #e(:let (v 3) (+ x v))))

  (assert-equalp
    #e3
    (prove::eliminate-let-expressions #e3))

  (assert-equalp
    #ex
    (prove::eliminate-let-expressions #ex))

  (assert-equalp
    #e(* (+ x v) 4)
    (prove::eliminate-let-expressions #e(* (+ x v) 4)))

  (assert-equalp
    #e(+ (* (+ x 3) (+ x 3)) b (^ (* (+ x 3) (+ x 3)) 3))
    (prove::eliminate-let-expressions
      #e(:let (a (:let (b (+ x 3))
		   (* b b)))
	  (:let (c (^ a 3))
	     (+ a b c)))))
)

(define-test simplify-qprod-unvarying-body-tests
  (assert-equalp
    #e12
    (prove::simplify-qprod-unvarying-body #e12))

  (assert-equalp
    #ex
    (prove::simplify-qprod-unvarying-body #ex))

  (assert-equalp
    #e(* a (+ x y))
    (prove::simplify-qprod-unvarying-body #e(* a (+ x y))))

  (assert-equalp
    #e(:quant qprod i (m n) (@ x i))
    (prove::simplify-qprod-unvarying-body
      #e(:quant qprod i (m n) (@ x i))))

  (assert-equalp
    #e(^ (+ x y) (- n (- m 1)))
    (prove::simplify-qprod-unvarying-body
      #e(:quant qprod j (m n) (+ x y))))
)

(defun sexpr->exprs (se) (mapcar #'sexpr->expr se))

(define-test expand-array-lengths-tests
  (assert-equalp
    '()
    (prove::var-dims-pats '(vars::x . ())))
  (assert-equalp
    '((#e(array-length 1 x) . #e1))
    (prove::var-dims-pats '(vars::x . (#e1))))
  (assert-equalp
    '((#e(array-length 1 x) . #ek))
    (prove::var-dims-pats '(vars::x . (#ek))))
  (assert-equalp
    '((#e(array-length 1 x) . #en)
      (#e(array-length 2 x) . #e(* m m)))
    (prove::var-dims-pats '(vars::x . (#en #e(* m m)))))

  (assert-equalp
    #e(+ n m k n k)
    (funcall (prove::expand-array-lengths
	       '((vars::x . (#en))
		 (vars::y . (#em #ek))
		 (vars::z . (#e(array-length 1 x) #e(array-length 2 y)))))
	     #e(+ (array-length 1 x)
		  (array-length 1 y)
		  (array-length 2 y)
		  (array-length 1 z)
		  (array-length 2 z))))
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

