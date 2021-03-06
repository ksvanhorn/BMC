(defpackage :mcimpl-tests
  (:use :cl :lisp-unit :mcimpl :model :expr :symbols :utils :testing-utilities))
(in-package :mcimpl-tests)

(define-test sexpr->parameters-tests
  (assert-equalp
    (list (make-decl :var 'vars::foo :typ (make-vtype-scalar :stype 'realp))
	  (make-decl :var 'vars::bar :typ (make-vtype-scalar :stype 'integerp))
	  (make-decl :var 'vars::baz :typ (make-vtype-scalar :stype 'realp)))
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
              :lhs (make-rellhs-simple :var 'vars::x)
	      :rhs (make-distribution
		    :name 'dnorm
		    :args '(#em #esig)))))
     (mcimpl::sexpr->updates '(:updates a (~ x (dnorm m sig)))))

  (assert-equalp
    `((foo .
       ,(make-relation-loop
	 :var 'vars::i
	 :lo #e1
	 :hi #en
	 :body
	   (make-relation-stochastic
	     :lhs (make-rellhs-array-elt
		    :var 'vars::y 
		    :indices '(#ei))
	     :rhs (make-distribution
		    :name 'dgamma
		    :args '(#e(@ alpha i) #ebeta)))))
      (bar .
       ,(make-relation-let
	  :var 'vars::lambda
	  :val #e(+ lambda0 (* n lambda_y))
	  :body (make-relation-stochastic
		  :lhs (make-rellhs-simple :var 'vars::z)
		  :rhs (make-distribution
			 :name 'dnorm
			 :args '(#em #e(^-1/2 lambda)))))))
    (mcimpl::sexpr->updates
      '(:updates
	 foo
	 (:for i (1 n) (~ (@ y i) (dgamma (@ alpha i) beta)))
	 bar
         (:let (lambda (+ lambda0 (* n lambda_y)))
           (~ z (dnorm m (^-1/2 lambda)))))))
)

(define-test sexpr->expectation-tests
  (assert-equalp
    `(,(sexpr->decl '(foo real)) . #efoo)
    (mcimpl::sexpr->expectation '(foo real foo)))
  (assert-equalp
     `(,(sexpr->decl '(bar (real (- n 2)))) . #e(@ bar (:range 3 n)))
     (mcimpl::sexpr->expectation '(bar (real (- n 2)) (@ bar (:range 3 n)))))
  (assert-error 'error (mcimpl::sexpr->expectation '(v integer v)))
  (assert-error 'error (mcimpl::sexpr->expectation '(u (integer k) uu)))

  ; Should not throw exception
  (mcimpl::check-expectations
    `((,(sexpr->decl '(a real)) . #eaa)
      (,(sexpr->decl '(b (real 4))) . #e(@ b (:range 2 5)))))

  (assert-error 'error
    (mcimpl::check-expectations
      `((,(sexpr->decl '(a real)) . #eaa)
	(,(sexpr->decl '(b real)) . #e(@ b 1))
	(,(sexpr->decl '(b (real 4))) . #e(@ b (:range 2 5))))))
)

(define-test sexpr->mcimpl-tests
  (let ((realp-typ (make-vtype-scalar :stype 'realp))
	(real-typ (make-vtype-scalar :stype 'real))
	(integer-typ (make-vtype-scalar :stype 'integer))
	(real-n-3-typ
	  (make-vtype-array :elem-type 'real
			    :dims '(#en #e3))))
  (assert-equalp
    (make-mcimpl
     :parameters
     (list (make-decl :var 'vars::c :typ realp-typ)
	   (make-decl :var 'vars::a :typ integer-typ))

     :acceptmons
     `((am2 ,(sexpr->decl '(i integer))
	    ,(sexpr->decl '(x real)))
       (am0)
       (am1 ,(sexpr->decl '(k integer))))

     :expectations
     (list (cons (make-decl :var 'vars::g-exp :typ real-typ) #eg)
	   (cons (make-decl :var 'vars::a-exp :typ real-n-3-typ)
		 #e(@ a :all (:range 1 3))))

     :updates
     `((wupd .
	,(make-relation-stochastic
	  :lhs (make-rellhs-simple :var 'vars::w)
	  :rhs (make-distribution :name 'dgamma :args '(#ea #eb))))
       (another_upd .
	,(make-relation-let
	  :var 'vars::f
	  :val #e(^2 g)
	  :body
	  (make-relation-stochastic
	   :lhs (make-rellhs-simple :var 'vars::v)
	   :rhs (make-distribution :name 'dnorm :args '(#e0 #ef)))))))

    (sexpr->mcimpl
      '(:mcimpl
	 (:parameters (c realp) (a integer))
	 (:acceptmons
	   (am2 (i integer) (x real))
	   (am0)
	   (am1 (k integer)))
	 (:expectations
	   (g-exp real g)
	   (a-exp (real n 3) (@ a :all (:range 1 3))))
	 (:updates
	   wupd
	   (~ w (dgamma a b))
	   another_upd
	   (:let (f (^2 g))
	     (~ v (dnorm 0 f))))))))

  (assert-error 'error
    (sexpr->mcimpl 'a))
  (assert-error 'error
    (sexpr->mcimpl nil))
  (assert-error 'error
    (sexpr->mcimpl '(starts-wrong (:parameters) (:updates))))
  (assert-error 'error
    (sexpr->mcimpl '(:mcimpl)))
  (assert-error 'error
    (sexpr->mcimpl '(:mcimpl (:parameters))))
  (assert-error 'error
    (sexpr->mcimpl '(:mcimpl (:parameters) (:updates))))
  (assert-equalp
    (make-mcimpl
      :parameters '() :acceptmons '() :expectations '() :updates '())
    (sexpr->mcimpl
      '(:mcimpl (:parameters) (:acceptmons) (:expectations) (:updates))))

  (assert-equal
    '()
    (params-names
      (sexpr->mcimpl
        '(:mcimpl (:parameters) (:acceptmons) (:expectations) (:updates)))))

  (assert-equal
    '(vars::a vars::b)
    (params-names
      (sexpr->mcimpl '(:mcimpl
		       (:parameters (a realp) (b integer))
		       (:acceptmons)
		       (:expectations)
		       (:updates)))))
)

; TODO:
; Check that parameters reference only args
; Check that no parameter has name clash with other parameter, arg, or var.
; Check that no two updates have same label.
; Check that updates only reference args, vars, or parameters.
; Check correct dimensionality for updates.
