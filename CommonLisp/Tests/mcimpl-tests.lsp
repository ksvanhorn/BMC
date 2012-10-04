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

(define-test sexpr->updates-tests
  (assert-error 'error
    (mcimpl::sexpr->updates 12))
  (assert-error 'error
    (mcimpl::sexpr->updates foo))
  (assert-error 'error
    (mcimpl::sexpr->updates '()))
  (assert-error 'error
    (mcimpl::sexpr->updates '(foo)))
  (assert-error 'error
    (mcimpl::sexpr->updates '(:updates a)))
  (assert-error 'error
    (mcimpl::sexpr->updates '(:updates (+ x y) (~ z (dnorm a b)))))
  (assert-error 'error
    (mcimpl::sexpr->updates '(:updates foo (+ x y))))

  (assert-equalp '() (mcimpl::sexpr->updates '(:updates)))

  (assert-equalp
     `((a . ,(make-relation-stochastic
              :lhs (make-rellhs-simple :var 'x)
	      :rhs (make-distribution
		    :name 'dnorm
		    :args (list (expr-var 'm) (expr-var 'sig))))))
     (mcimpl::sexpr->updates '(:updates a (~ x (dnorm m sig)))))

  (assert-equalp
    `((foo .
       ,(make-relation-loop
	 :var 'i
	 :lo (expr-const 1)
	 :hi (expr-var 'n)
	 :body
	   (make-relation-stochastic
	     :lhs (make-rellhs-array-elt
		    :var 'y 
		    :indices (list (expr-var 'i)))
	     :rhs (make-distribution
		    :name 'dgamma
		    :args (list (expr-call '@ (expr-var 'alpha) (expr-var 'i))
				(expr-var 'beta))))))
      (bar .
       ,(make-relation-let
	  :var 'lambda
	  :val (expr-call '+ (expr-var 'lambda0)
			     (expr-call '* (expr-var 'n)
					   (expr-var 'lambda_y)))
	  :body (make-relation-stochastic
		  :lhs (make-rellhs-simple :var 'z)
		  :rhs (make-distribution
			 :name 'dnorm
			 :args (list (expr-var 'm)
				     (expr-call '^-1/2
						(expr-var 'lambda))))))))
    (mcimpl::sexpr->updates
      '(:updates
	 foo
	 (:for i (1 n) (~ (@ y i) (dgamma (@ alpha i) beta)))
	 bar
         (:let (lambda (+ lambda0 (* n lambda_y)))
           (~ z (dnorm m (^-1/2 lambda)))))))
)

(define-test sexpr->mcimpl-tests
  (assert-equalp
    (make-mcimpl
     :parameters
     (list (make-decl
	    :var 'c
	    :typ (make-vtype-scalar :stype 'realp))
	   (make-decl
	    :var 'a
	    :typ (make-vtype-scalar :stype 'integer)))
     :derived
     `((a . ,(expr-call '+ (expr-var 'x) (expr-var 'y))))
     :updates
     `((wupd .
	,(make-relation-stochastic
	  :lhs (make-rellhs-simple :var 'w)
	  :rhs (make-distribution
		:name 'dgamma
		:args (list (expr-var 'a) (expr-var 'b)))))
       (another_upd .
	,(make-relation-let
	  :var 'f
	  :val (expr-call '^2 (expr-var 'g))
	  :body
	  (make-relation-stochastic
	   :lhs (make-rellhs-simple :var 'v)
	   :rhs (make-distribution
		 :name 'dnorm
		 :args (list (expr-const 0) (expr-var 'f))))))))
    (sexpr->mcimpl
      '(:mcimpl
	 (:parameters (c realp) (a integer))
	 (:derived (a (+ x y)))
	 (:updates
	   wupd
	   (~ w (dgamma a b))
	   another_upd
	   (:let (f (^2 g))
	     (~ v (dnorm 0 f)))))))

  (assert-error 'error
    (sexpr->mcimpl 'a))
  (assert-error 'error
    (sexpr->mcimpl nil))
  (assert-error 'error
    (sexpr->mcimpl '(starts-wrong (:parameters) (:derived) (:updates))))
  (assert-error 'error
    (sexpr->mcimpl '(:mcimpl)))
  (assert-error 'error
    (sexpr->mcimpl '(:mcimpl (:parameters))))
  (assert-error 'error
    (sexpr->mcimpl '(:mcimpl (:parameters) (:derived))))
  (assert-equalp
    (make-mcimpl :parameters '() :derived '() :updates '())
    (sexpr->mcimpl '(:mcimpl (:parameters) (:derived) (:updates))))
)

(define-test subst-derived-tests
  (assert-equalp
    (sexpr->rel '(~ x (dgamma a b)))
    (mcimpl::subst-derived '() (sexpr->rel '(~ x (dgamma a b)))))

  (assert-equalp
    (sexpr->rel '(~ w (dnorm (+ a b) s)))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (x (+ a b))))
      (sexpr->rel '(~ w (dnorm x s)))))

  (assert-equalp
    (sexpr->rel '(~ x (dnorm 0 (:let (a 37) (+ (/ c d) a)))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (a (^2 foo)) (y (/ c d))))
      (sexpr->rel '(~ x (dnorm 0 (:let (a 37) (+ y a)))))))

  (assert-equalp
    (sexpr->rel '(~ x (dnorm 0 (:let (a1 37) (+ (/ a d) a1)))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (y (/ a d))))
      (sexpr->rel '(~ x (dnorm 0 (:let (a 37) (+ y a)))))))

  (assert-equalp
    (sexpr->rel '(~ (@ v (+ i (* x y))) (dgamma a b)))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (c (* x y))))
      (sexpr->rel '(~ (@ v (+ i c)) (dgamma a b)))))

  (assert-equalp
    (sexpr->rel '(~ (@ v (+ i (* x y)) (:range (- foo bar) hello) :all)
		    (dgamma a b)))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (c (* x y))
		   (lo (- foo bar))
		   (hi hello)))
      (sexpr->rel '(~ (@ v (+ i c) (:range lo hi) :all) (dgamma a b)))))

  (assert-error 'error
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (x (+ a b))))
      (sexpr->rel '(~ x (dnorm w s)))))

  (assert-error 'error
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (v (* x y))))
      (sexpr->rel '(~ (@ v (+ i c)) (dgamma a b)))))

  (assert-error 'error
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (v (* x y))))
      (sexpr->rel '(~ (@ v i :all) (dnorm m s)))))

  (assert-equalp
    (make-relation-skip)
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (a b)))
      (make-relation-skip)))

  (assert-equalp
    (sexpr->rel '(:block))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (a b)))
      (sexpr->rel '(:block))))

  (assert-equalp
    (sexpr->rel '(:block (~ x (dnorm b s)) (~ y (dnorm b tt))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (a b) (ss tt)))
      (sexpr->rel '(:block (~ x (dnorm a s)) (~ y (dnorm a ss))))))

  (assert-equalp
    (sexpr->rel '(:if (< xx y)
		      (~ w (dgamma aa b))
		      (~ z (dnorm m ss))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (x xx) (a aa) (s ss)))
      (sexpr->rel '(:if (< x y)
			(~ w (dgamma a b))
			(~ z (dnorm m s))))))

  ; Regression test
  (assert-equalp
    (sexpr->rel '(:if (< xx y) (~ w (dgamma aa b))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (x xx) (a aa) (s ss)))
      (sexpr->rel '(:if (< x y) (~ w (dgamma a b))))))

  (assert-equalp
    (sexpr->rel
      '(:for i (low high) (~ w (dgamma (@ a i) beta))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (alpha a) (lo low) (hi high)))
      (sexpr->rel
        '(:for i (lo hi) (~ w (dgamma (@ alpha i) beta))))))

  (assert-equalp
    (sexpr->rel
      '(:for i (lo hi) (~ w (dgamma (@ alpha i) beta))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (i j)))
      (sexpr->rel
        '(:for i (lo hi) (~ w (dgamma (@ alpha i) beta))))))

  (assert-equalp
    (sexpr->rel
      '(:for i1 ((@ lo i) (@ hi i j)) (~ w (dgamma (@ alpha i1) beta))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (lo (@ lo i)) (hi (@ hi i j))))
      (sexpr->rel
        '(:for i (lo hi) (~ w (dgamma (@ alpha i) beta))))))

  (assert-equalp
    (sexpr->rel
      '(:for i ((@ lo j) (@ h j)) (~ w (dgamma (@ alpha i) beta))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (i j)))
      (sexpr->rel
        '(:for i ((@ lo i) (@ h i)) (~ w (dgamma (@ alpha i) beta))))))

  (assert-equalp
    (sexpr->rel
      '(:for i1 (lo hi) (~ w (dgamma (@ alpha i1) (@ gamma i)))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (beta (@ gamma i))))
      (sexpr->rel
        '(:for i (lo hi) (~ w (dgamma (@ alpha i) beta))))))

  (assert-equalp
    (sexpr->rel
      '(:let (x (* b (+ (^2 k) d))) (~ w (dnorm (+ (dot f g) x) s))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (c (^2 k)) (m (dot f g))))
      (sexpr->rel
        '(:let (x (* b (+ c d))) (~ w (dnorm (+ m x) s))))))

  (assert-equalp
    (sexpr->rel
      '(:let (x (* b (+ (^2 k) d))) (~ w (dnorm (+ (dot f g) x) s))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (x 37) (c (^2 k)) (m (dot f g))))
      (sexpr->rel
        '(:let (x (* b (+ c d))) (~ w (dnorm (+ m x) s))))))

  (assert-equalp
    (sexpr->rel
      '(:let (x (* 37 (+ c d))) (~ w (dnorm (+ m x) s))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (x 37)))
      (sexpr->rel
        '(:let (x (* x (+ c d))) (~ w (dnorm (+ m x) s))))))

  (assert-equalp
    (sexpr->rel
      '(:let (x1 a) (~ w (dnorm (+ m x1) (* x x)))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (s (* x x))))
      (sexpr->rel
        '(:let (x a) (~ w (dnorm (+ m x) s))))))

  (assert-equalp
    (sexpr->rel
      '(:let (x1 (+ a (* 2 x))) (~ w (dnorm (+ m x1) s))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (b (* 2 x))))
      (sexpr->rel
        '(:let (x (+ a b)) (~ w (dnorm (+ m x) s))))))

  (assert-equalp
    `((fred .
       ,(sexpr->rel '(:for i (1 n)
		       (:if (= 3 (@ f i))
			 (~ (@ v i) (dnorm (sum y) (exp (@ nu 1))))))))
      (barney .
       ,(sexpr->rel '(~ w (dgamma a b))))
      (bam-bam .
       ,(sexpr->rel '(~ u (dnorm (* (exp (@ nu 1)) (sum y)) sigma)))))
    (mcimpl->substituted-updates
      (sexpr->mcimpl
        '(:mcimpl
	   (:parameters)
	   (:derived
	     (x (exp (@ nu 1)))
	     (S (sum y)))
	   (:updates
	     fred
	     (:for i (1 n)
               (:if (= 3 (@ f i))
                 (~ (@ v i) (dnorm S x))))
	     barney
	     (~ w (dgamma a b))
	     bam-bam
	     (~ u (dnorm (* x S) sigma)))))))
)


(define-test subst-derived-tests
  (assert-equalp
    (sexpr->rel '(~ x (dgamma a b)))
    (mcimpl::subst-derived '() (sexpr->rel '(~ x (dgamma a b)))))

  (assert-equalp
    (sexpr->rel '(:let (x (+ a b)) (~ w (dnorm x s))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (x (+ a b))))
      (sexpr->rel '(~ w (dnorm x s)))))

  (assert-equalp
    (sexpr->rel '(~ w (dnorm x s)))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (foo (+ a b))))
      (sexpr->rel '(~ w (dnorm x s)))))

  (assert-equalp
    (sexpr->rel
      '(:let (nu (vec alpha beta gamma))
       (:let (logb (@ nu j))
       (:let (off2 (@ nu i))
       (:let (off1 12)
       (:let (b (exp logb))
       (:let (offset (+ off1 off2))
	 (:block
	   (~ x (dnorm (+ mu offset) (* scale sigma)))
	   (~ y (dgamma (exp x) (* b (exp x))))))))))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived  (nu (vec alpha beta gamma)) ; indirectly referenced
                    (foo2 (@ mumble j))
		    (logb (@ nu j))           ; indirectly referenced
		    (off2 (@ nu i))           ; indirectly referenced
		    (off1 12)                 ; indirectly referenced
		    (foo1 (* foo2 7))
		    (b (exp logb))            ; directly referenced
                    (offset (+ off1 off2)))) ; directly referenced
      (sexpr->rel '(:block
		     (~ x (dnorm (+ mu offset) (* scale sigma)))
		     (~ y (dgamma (exp x) (* b (exp x))))))))

#|
  (assert-equalp
    (sexpr->rel '(~ x (dnorm 0 (:let (a 37) (+ (/ c d) a)))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (a (^2 foo)) (y (/ c d))))
      (sexpr->rel '(~ x (dnorm 0 (:let (a 37) (+ y a)))))))

  (assert-equalp
    (sexpr->rel '(~ x (dnorm 0 (:let (a1 37) (+ (/ a d) a1)))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (y (/ a d))))
      (sexpr->rel '(~ x (dnorm 0 (:let (a 37) (+ y a)))))))

  (assert-equalp
    (sexpr->rel '(~ (@ v (+ i (* x y))) (dgamma a b)))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (c (* x y))))
      (sexpr->rel '(~ (@ v (+ i c)) (dgamma a b)))))

  (assert-equalp
    (sexpr->rel '(~ (@ v (+ i (* x y)) (:range (- foo bar) hello) :all)
		    (dgamma a b)))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (c (* x y))
		   (lo (- foo bar))
		   (hi hello)))
      (sexpr->rel '(~ (@ v (+ i c) (:range lo hi) :all) (dgamma a b)))))

  (assert-error 'error
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (x (+ a b))))
      (sexpr->rel '(~ x (dnorm w s)))))

  (assert-error 'error
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (v (* x y))))
      (sexpr->rel '(~ (@ v (+ i c)) (dgamma a b)))))

  (assert-error 'error
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (v (* x y))))
      (sexpr->rel '(~ (@ v i :all) (dnorm m s)))))

  (assert-equalp
    (make-relation-skip)
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (a b)))
      (make-relation-skip)))

  (assert-equalp
    (sexpr->rel '(:block))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (a b)))
      (sexpr->rel '(:block))))

  (assert-equalp
    (sexpr->rel '(:block (~ x (dnorm b s)) (~ y (dnorm b tt))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (a b) (ss tt)))
      (sexpr->rel '(:block (~ x (dnorm a s)) (~ y (dnorm a ss))))))

  (assert-equalp
    (sexpr->rel '(:if (< xx y)
		      (~ w (dgamma aa b))
		      (~ z (dnorm m ss))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (x xx) (a aa) (s ss)))
      (sexpr->rel '(:if (< x y)
			(~ w (dgamma a b))
			(~ z (dnorm m s))))))

  ; Regression test
  (assert-equalp
    (sexpr->rel '(:if (< xx y) (~ w (dgamma aa b))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (x xx) (a aa) (s ss)))
      (sexpr->rel '(:if (< x y) (~ w (dgamma a b))))))

  (assert-equalp
    (sexpr->rel
      '(:for i (low high) (~ w (dgamma (@ a i) beta))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (alpha a) (lo low) (hi high)))
      (sexpr->rel
        '(:for i (lo hi) (~ w (dgamma (@ alpha i) beta))))))

  (assert-equalp
    (sexpr->rel
      '(:for i (lo hi) (~ w (dgamma (@ alpha i) beta))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (i j)))
      (sexpr->rel
        '(:for i (lo hi) (~ w (dgamma (@ alpha i) beta))))))

  (assert-equalp
    (sexpr->rel
      '(:for i1 ((@ lo i) (@ hi i j)) (~ w (dgamma (@ alpha i1) beta))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (lo (@ lo i)) (hi (@ hi i j))))
      (sexpr->rel
        '(:for i (lo hi) (~ w (dgamma (@ alpha i) beta))))))

  (assert-equalp
    (sexpr->rel
      '(:for i ((@ lo j) (@ h j)) (~ w (dgamma (@ alpha i) beta))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived '(:derived (i j)))
      (sexpr->rel
        '(:for i ((@ lo i) (@ h i)) (~ w (dgamma (@ alpha i) beta))))))

  (assert-equalp
    (sexpr->rel
      '(:for i1 (lo hi) (~ w (dgamma (@ alpha i1) (@ gamma i)))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (beta (@ gamma i))))
      (sexpr->rel
        '(:for i (lo hi) (~ w (dgamma (@ alpha i) beta))))))

  (assert-equalp
    (sexpr->rel
      '(:let (x (* b (+ (^2 k) d))) (~ w (dnorm (+ (dot f g) x) s))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (c (^2 k)) (m (dot f g))))
      (sexpr->rel
        '(:let (x (* b (+ c d))) (~ w (dnorm (+ m x) s))))))

  (assert-equalp
    (sexpr->rel
      '(:let (x (* b (+ (^2 k) d))) (~ w (dnorm (+ (dot f g) x) s))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (x 37) (c (^2 k)) (m (dot f g))))
      (sexpr->rel
        '(:let (x (* b (+ c d))) (~ w (dnorm (+ m x) s))))))

  (assert-equalp
    (sexpr->rel
      '(:let (x (* 37 (+ c d))) (~ w (dnorm (+ m x) s))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (x 37)))
      (sexpr->rel
        '(:let (x (* x (+ c d))) (~ w (dnorm (+ m x) s))))))

  (assert-equalp
    (sexpr->rel
      '(:let (x1 a) (~ w (dnorm (+ m x1) (* x x)))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (s (* x x))))
      (sexpr->rel
        '(:let (x a) (~ w (dnorm (+ m x) s))))))

  (assert-equalp
    (sexpr->rel
      '(:let (x1 (+ a (* 2 x))) (~ w (dnorm (+ m x1) s))))
    (mcimpl::subst-derived
      (mcimpl::sexpr->derived
        '(:derived (b (* 2 x))))
      (sexpr->rel
        '(:let (x (+ a b)) (~ w (dnorm (+ m x) s))))))

  (assert-equalp
    `((fred .
       ,(sexpr->rel '(:for i (1 n)
		       (:if (= 3 (@ f i))
			 (~ (@ v i) (dnorm (sum y) (exp (@ nu 1))))))))
      (barney .
       ,(sexpr->rel '(~ w (dgamma a b))))
      (bam-bam .
       ,(sexpr->rel '(~ u (dnorm (* (exp (@ nu 1)) (sum y)) sigma)))))
    (mcimpl->substituted-updates
      (sexpr->mcimpl
        '(:mcimpl
	   (:parameters)
	   (:derived
	     (x (exp (@ nu 1)))
	     (S (sum y)))
	   (:updates
	     fred
	     (:for i (1 n)
               (:if (= 3 (@ f i))
                 (~ (@ v i) (dnorm S x))))
	     barney
	     (~ w (dgamma a b))
	     bam-bam
	     (~ u (dnorm (* x S) sigma)))))))
|#
)


(define-test free-vars-in-rel-tests
  (assert-equalp
    '()
    (mcimpl::free-vars-in-rel (make-relation-skip)))
  (assert-equalp
    '()
    (mcimpl::free-vars-in-rel (sexpr->rel '(:block))))
  (assert-equalp
    '(x m s)
    (mcimpl::free-vars-in-rel
      (sexpr->rel '(~ x (dnorm m s)))))
  (assert-equalp
    '(x i m s)
    (mcimpl::free-vars-in-rel
      (sexpr->rel '(~ (@ x i) (dnorm m s)))))
  (assert-equalp
    '(x m s)
    (mcimpl::free-vars-in-rel
      (sexpr->rel '(~ (@ x :all) (dnorm m s)))))
  (assert-equalp
    '(x i lo hi m s)
    (mcimpl::free-vars-in-rel
      (sexpr->rel '(~ (@ x i (:range lo hi)) (dnorm m s)))))
  (assert-equalp
    '(x i y m foo s z a b)
    (mcimpl::free-vars-in-rel
      (sexpr->rel '(:if (= 1 (@ x i))
			(~ y (dnorm (- m foo) (^2 s)))
			(~ z (dgamma (^ a 2) (+ b 3)))))))
  (assert-equalp
    '(m n k y mu sigma)
    (mcimpl::free-vars-in-rel
      (sexpr->rel '(:for j ((+ m 1) (- n k))
		     (~ (@ y j) (dnorm (@ mu j) sigma))))))
  (assert-equalp
    '(m j n k y mu sigma)
    (mcimpl::free-vars-in-rel
      (sexpr->rel '(:for j ((+ m j) (- n k))
		     (~ (@ y j) (dnorm (@ mu j) sigma))))))
  (assert-equalp
    '(m n j y mu sigma)
    (mcimpl::free-vars-in-rel
      (sexpr->rel '(:for j ((+ m 1) (- n j))
		     (~ (@ y j) (dnorm (@ mu j) sigma))))))
  (assert-equalp
    '(a b c y m k s)
    (mcimpl::free-vars-in-rel
      (sexpr->rel '(:let (x (+ a (/ b c))) (~ y (dnorm (+ m x) (* k s)))))))
  (assert-equalp
    '(a x c y m k s)
    (mcimpl::free-vars-in-rel
      (sexpr->rel '(:let (x (+ a (/ x c))) (~ y (dnorm (+ m x) (* k s)))))))
)

; TODO:
; Check that parameters reference only args
; Check that no parameter has name clash with other parameter, arg, or var.
; Check that derived reference only args, vars, and preceding derived
; Check that no derived has name clash with other parameter, arg, var, or
; preceding derived.
; Check that no two updates have same label.
; Check that updates only reference args, vars, parameters, or deriveds.
; Check correct dimensionality for updates.
