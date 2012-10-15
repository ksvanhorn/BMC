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
    (sexpr->mcimpl '(starts-wrong (:parameters) (:updates))))
  (assert-error 'error
    (sexpr->mcimpl '(:mcimpl)))
  (assert-error 'error
    (sexpr->mcimpl '(:mcimpl (:parameters))))
  (assert-equalp
    (make-mcimpl :parameters '() :updates '())
    (sexpr->mcimpl '(:mcimpl (:parameters) (:updates))))

  (assert-equal
    '()
    (params-names
      (sexpr->mcimpl '(:mcimpl (:parameters) (:updates)))))

  (assert-equal
    '(a b)
    (params-names
      (sexpr->mcimpl '(:mcimpl
		       (:parameters (a realp) (b integer))
		       (:updates)))))
)

; TODO:
; Check that parameters reference only args
; Check that no parameter has name clash with other parameter, arg, or var.
; Check that no two updates have same label.
; Check that updates only reference args, vars, or parameters.
; Check correct dimensionality for updates.
