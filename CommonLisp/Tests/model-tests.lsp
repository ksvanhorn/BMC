(defpackage :model-tests
  (:use :cl :lisp-unit :model :expr :symbols :utils :testing-utilities))
(in-package :model-tests)

(define-test sexpr->decl-tests
  ;; sexpr->vtype
  (assert-equalp (make-vtype-scalar :stype 'integerp)
		 (sexpr->vtype 'integerp))
  (assert-equalp (make-vtype-scalar :stype 'real)
		 (sexpr->vtype 'real))
  (assert-error 'error (sexpr->vtype 'not-a-type))

  (assert-equalp (make-vtype-array
		   :elem-type 'boolean
		   :dims (list (make-expr-const :name 2)
			       (make-expr-variable :symbol 'n)))
		 (sexpr->vtype '(boolean 2 n)))
  (assert-equalp (make-vtype-array
		   :elem-type 'realp
		   :dims (list (make-expr-apply
				 :fct '+
				 :args (list (make-expr-variable :symbol 'k)
					     (make-expr-const :name 3)))))
		 (sexpr->vtype '(realp (+ k 3))))
  (assert-error 'error (sexpr->vtype '(integerp)))

  ;; sexpr->decl
  (assert-equalp (make-decl :var 'A :typ (make-vtype-scalar :stype 'realxn))
		 (sexpr->decl '(a realxn)))
  (assert-equalp (make-decl
		   :var 'f
		   :typ (make-vtype-array
			  :elem-type 'integer
			  :dims (list (make-expr-variable :symbol 'm))))
		 (sexpr->decl '(f (integer m))))
  (assert-error 'error (sexpr->decl '((@ x i) real)))
  (assert-error 'error (sexpr->decl '(true boolean)))
)

(define-test sexpr->rel-tests
  ;; distributions
  (assert-equalp (make-distribution
		   :name 'dnorm
		   :args (list (make-expr-variable :symbol 'mu)
			       (make-expr-variable :symbol 'sigma)))
		 (sexpr->distr '(dnorm mu sigma)))
  (assert-error 'error (sexpr->distr '(not-a-distribution mu sigma)))

  ;; LHS of stochastic relation
  (assert-equalp (make-rellhs-simple :var 'x)
		 (sexpr->rellhs 'x))
  (assert-equalp
    (make-rellhs-array-elt
      :var 'x
      :indices (list (make-expr-variable :symbol 'i)))
    (sexpr->rellhs '(@ x i)))
  (assert-equalp
    (make-rellhs-array-slice
      :var 'y
      :indices (list (make-array-slice-index-scalar
		       :value (make-expr-variable :symbol 'i))
		     (make-array-slice-index-range
		       :lo (make-expr-const :name 1)
		       :hi (make-expr-variable :symbol 'n))
		     (make-array-slice-index-all)))
    (sexpr->rellhs '(@ y i (:range 1 n) :all)))

  (assert-error 'error (sexpr->rellhs 3))
  (assert-error 'error (sexpr->rellhs '(x i)))

  ;; relations
  (assert-equalp
    (make-relation-let
      :var 'x
      :val (sexpr->expr '(+ y z))
      :body (sexpr->rel '(~ v (dnorm x 1))))
    (sexpr->rel '(:let (x (+ y z)) (~ v (dnorm x 1)))))

  (assert-equalp
    (make-relation-stochastic
      :lhs (sexpr->rellhs '(@ x i))
      :rhs (sexpr->distr '(dnorm (+ y z) 2)))
    (sexpr->rel '(~ (@ x i) (dnorm (+ y z) 2))))

  (assert-equalp
    (make-relation-stochastic
      :lhs (sexpr->rellhs 'y)
      :rhs (sexpr->distr '(dgamma alpha beta)))
    (sexpr->rel '(~ y (dgamma alpha beta))))

  (assert-equalp
    (make-relation-block :members '())
    (sexpr->rel '(:block)))

  (assert-equalp
    (make-relation-block
      :members (list (sexpr->rel '(~ x (dnorm (+ y 2) 1)))
		     (sexpr->rel '(~ (@ y i) (dnorm mu sigma)))))
    (sexpr->rel '(:block
		   (~ x (dnorm (+ y 2) 1))
		   (~ (@ y i) (dnorm mu sigma)))))

  (assert-error 'error (sexpr->rel '(:block (+ x y))))

  (assert-equalp
    (make-relation-if
      :condition (sexpr->expr '(.= (@ x i) 1))
      :true-branch (sexpr->rel '(~ (@ y i) (dgamma a b)))
      :false-branch (make-relation-skip))
    (sexpr->rel '(:if (.= (@ x i) 1)
		   (~ (@ y i) (dgamma a b)))))

  (assert-equalp
   (make-relation-if
     :condition (sexpr->expr '(@ good i))
     :true-branch (sexpr->rel '(~ (@ x i) (dcat q)))
     :false-branch (sexpr->rel '(~ (@ y i) (dcat p))))
   (sexpr->rel '(:if (@ good i)
		  (~ (@ x i) (dcat q))
		  (~ (@ y i) (dcat p)))))

  (assert-error 'error (sexpr->rel '(:if (x .< y))))
  (assert-error 'error (sexpr->rel '(:if (~ x (dnorm 0 1)) (~ x (dnorm m s)))))
  (assert-error 'error
    (sexpr->rel '(:if (.= (@ z i) 3) (~ x (dnorm 0 1)) (+ a b))))

  (assert-equalp
    (make-relation-loop
      :var 'k
      :lo (sexpr->expr 'm)
      :hi (sexpr->expr '(+ n 2))
      :body (sexpr->rel '(~ (@ x k) (dgamma a b))))
    (sexpr->rel '(:for k (m (+ n 2)) (~ (@ x k) (dgamma a b)))))
  (assert-error 'error
    (sexpr->rel '(:for (@ x i) (m n) (~ y (dnorm 0 3)))))
  (assert-error 'error
    (sexpr->rel '(:for j (m) (~ (@ x j) (dnorm 3 1)))))
  (assert-error 'error
    (sexpr->rel '(for j (m n p) (~ (@ x j) (dnorm 0 3)))))
  (assert-error 'error
    (sexpr->rel '(for j (m n) (* j 3))))

  (assert-equalp
    (make-relation-mh
     :lets `((m . ,(sexpr->expr '(+ n 1)))
	     (a . ,(sexpr->expr '(* x y))))
     :proposal-distribution (sexpr->rel '(~ x (dnorm m s)))
     :log-acceptance-factor (sexpr->expr '(+ x y)))
    (sexpr->rel '(:metropolis-hastings
		  :lets ((m (+ n 1))
			 (a (* x y)))
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (+ x y))))

  (assert-equalp
    (make-relation-mh
     :lets '()
     :proposal-distribution (sexpr->rel '(~ x (dnorm m s)))
     :log-acceptance-factor (sexpr->expr '(+ x y)))
    (sexpr->rel '(:metropolis-hastings
		  :lets ()
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (+ x y))))

  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets not-a-list
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (+ x y))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets (not-a-def)
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (+ x y))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets ((v))
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (+ x y))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets ((1 v))
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (+ x y))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets ((v 1 extra))
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (+ x y))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (+ x y))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets ()
		  :log-acceptance-factor (* x u v w))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets ()
		  :proposal-distribution (~ x (dnorm m s)))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (* x u v w))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets ()
		  :proposal-distribution
		  :log-acceptance-factor (* x u v w))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets ()
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor)))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets ()
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (* x u v w)
		  more)))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :wrong-keyword ()
		  :proposal-distribution (~ x (dnorm m s))
		  :log-acceptance-factor (* x u v w))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets ()
		  :wrong-keyword (~ x (dnorm m s))
		  :log-acceptance-factor (* x u v w))))
  (assert-error 'error
    (sexpr->rel '(:metropolis-hastings
		  :lets ()
		  :proposal-distribution (~ x (dnorm m s))
		  :wrong-keyword (* x u v w))))
)

(define-test rellhs->expr-tests
  (assert-equalp
    (sexpr->expr 'v)
    (rellhs->expr (sexpr->rellhs 'v)))
  (assert-equalp
    (sexpr->expr '(@ v i j))
    (rellhs->expr (sexpr->rellhs '(@ v i j))))
  (assert-equalp
    (sexpr->expr '(@ x (:range m n) i :all))
    (rellhs->expr (sexpr->rellhs '(@ x (:range m n) i :all))))
)

(define-test sexpr->model-tests
  (assert-equalp
    (make-model
      :args (list (sexpr->decl '(n integerp0))
		  (sexpr->decl '(x (real n))))
      :reqs (list (sexpr->expr '(< n 200))
		  (sexpr->expr '(:quant qand i (1 n) (<= 0 (@ x i)))))
      :vars (list (sexpr->decl '(y (real n n)))
		  (sexpr->decl '(z (integer (+ n 2)))))
      :invs (list (sexpr->expr '(is-symm-pd y)))
      :body (list (sexpr->rel '(:for i (1 n) (~ (@ y i i) (dnorm m s))))
		  (sexpr->rel '(:if (.= n 100)
				 (~ z (dmvnorm mu Sigma))
		                 (~ z (dwishart nu V))))))
    (sexpr->model '(:model
		     (:args (n integerp0)
		            (x (real n)))
		     (:reqs (< n 200)
                            (:quant qand i (1 n) (<= 0 (@ x i))))
		     (:vars (y (real n n))
                            (z (integer (+ n 2))))
		     (:invs (is-symm-pd y))
		     (:body (:for i (1 n) (~ (@ y i i) (dnorm m s)))
		            (:if (= n 100)
			      (~ z (dmvnorm mu Sigma))
		              (~ z (dwishart nu V)))))))
)

(define-test pp-decl-tests
  ;; vtype->string
  (assert-equal "INTEGER" (vtype->string (sexpr->vtype 'integer)))
  (assert-equal "REALP0" (vtype->string (sexpr->vtype 'realp0)))
  (assert-equal "BOOLEAN[2, n]"
		(vtype->string (sexpr->vtype '(boolean 2 |n|))))
  (assert-equal "REALP[k + 3]"
		(vtype->string (sexpr->vtype '(realp (+ |k| 3)))))

  ;; decl->string
  (assert-equal "a : REALXN
"
		(ppstr (pp-decl (sexpr->decl '(|a| realxn)))))
  (assert-equal "f : INTEGER[m]
"
		(ppstr (pp-decl (sexpr->decl '(|f| (integer |m|))))))
)

(define-test pp-rel-tests
  ;; distributions
  (assert-equal "DNORM(mu + d, sigma)"
		(distr->string (sexpr->distr '(dnorm (+ |mu| |d|) |sigma|))))

  ;; LHS of relation
  (assert-equal "x" (model::rellhs->string (sexpr->rellhs '|x|)))
  (assert-equal "x[i]" (model::rellhs->string (sexpr->rellhs '(@ |x| |i|))))
  (assert-equal "y[i, 1 : n, ]"
		(model::rellhs->string
		  (sexpr->rellhs '(@ |y| |i| (:range 1 |n|) :all))))
  (assert-equal "y[i + 2, m + 1 : n - 3]"
		(model::rellhs->string
		  (sexpr->rellhs
		    '(@ |y| (+ |i| 2) (:range (+ |m| 1) (- |n| 3))))))

  (assert-equal (strcat-lines
		  "let x = y + z in"
		  "    ind ~ DCAT(p)")
		(ppstr (pp-rel (sexpr->rel '(:let (|x| (+ |y| |z|))
					      (~ |ind| (dcat |p|)))))))

  (assert-equal "x[i] ~ DNORM(y + z, 1)
"
		(ppstr (pp-rel (sexpr->rel '(~ (@ |x| |i|)
					       (dnorm (+ |y| |z|) 1))))))

  (assert-equal "y ~ DGAMMA(alpha, beta)
"
		(ppstr (pp-rel (sexpr->rel '(~ |y| (dgamma |alpha| |beta|))))))

  (assert-equal "" (ppstr (pp-rel (sexpr->rel '(:block)))))

  (assert-equal (strcat-lines "    X ~ DGAMMA(Y + 2, 1)"
			      "    Y[i] ~ DNORM(MU, SIGMA)")
		(ppstr
		  (pp-rel
		    (sexpr->rel '(:block
				   (~ x (dgamma (+ y 2) 1))
				   (~ (@ y |i|) (dnorm mu sigma)))))
		  :indent-level 1))

  (assert-equal
"if (X[i] .= 1) {
    Y[i] ~ DGAMMA(a, b)
}
"
    (ppstr (pp-rel (sexpr->rel '(:if (.= (@ x |i|) 1)
				  (~ (@ y |i|) (dgamma |a| |b|)))))))

  (assert-equal
"if (good[i]) {
    x[i] ~ DCAT(q)
}
else {
    y[i] ~ DCAT(p)
}
"
    (ppstr (pp-rel (sexpr->rel '(:if (@ |good| |i|)
				  (~ (@ |x| |i|) (dcat |q|))
				  (~ (@ |y| |i|) (dcat |p|)))))))

  (assert-equal
"for (k in m : n + 2) {
    x[k] ~ DGAMMA(a, b)
}
"
    (ppstr (pp-rel (sexpr->rel '(:for |k| (|m| (+ |n| 2))
				  (~ (@ |x| |k|) (dgamma |a| |b|)))))))
)

(define-test pp-model-tests
  (assert-equal
"args {
  N : INTEGERP0
  X : REAL[N]
}
reqs {
  N < 200
  QAND(I, 1 : N, 0 <= X[I])
}
vars {
  Y : REAL[N, N]
  Z : REAL[N + 2]
}
invs {
  IS-SYMM-PD(Y)
}
model {
  for (I in 1 : N) {
    Y[I, I] ~ DNORM(M, S)
  }
  if (N .= 100) {
    Z ~ DMVNORM(MU, SIGMA)
  }
  else {
    Z ~ DWISHART(NU, V)
  }
}
"
    (ppstr
      (pp-model
        (sexpr->model
	  '(:model
	     (:args (n integerp0)
	            (x (real n)))
	     (:reqs (< n 200)
	            (:quant qand i (1 n) (<= 0 (@ x i))))
	     (:vars (y (real n n))
	            (z (real (+ n 2))))
	     (:invs (is-symm-pd y))
	     (:body (:for i (1 n)
		      (~ (@ y i i) (dnorm m s)))
	            (:if (= n 100)
		      (~ z (dmvnorm mu sigma))
		      (~ z (dwishart nu V)))))))
      :indent-amount 2))
)

(define-test model-error-check-tests
  (assert-equal
    '(c n e m k chi z)
    (model::used-before-declared
      (raw-sexpr->model
        '(:model
	  (:args (a (integer (+ c 2)))
		 (b (real n))
		 (n integerp0))
	  (:reqs (<= 3 e)
		 (= 1 (:quant qsum i ((+ m 1) (- k 1)) (@ chi i))))
	  (:vars (c realp)
		 (d (integer n (@ z 1)))
		 (e integer)
		 (i integer)
		 (m integer)
		 (k integer)
		 (x (real n n))
		 (chi (integer 5))
		 (z (integer 3)))
	  (:body)))))

  (assert-equal
    '(n m)
    (model::vars-used-in-dims
      (raw-sexpr->model
        '(:model
	   (:args)
	   (:reqs)
	   (:vars (n integer)
		  (m integer)
		  (k integer)
		  (x (real n))
		  (y (real 3 m)))))))
)

(defun sexpr->exprs (se) (mapcar #'sexpr->expr se))

(define-test decl-dims-tests
  (assert-equalp
    '(v . ())
    (decl-dims (sexpr->decl '(v integer))))
  (assert-equalp
    (cons 'x (list (expr-var 'n)))
    (decl-dims (sexpr->decl '(x (real n)))))
  (assert-equalp
    (cons 'y (sexpr->exprs '(1 (* m n))))
    (decl-dims (sexpr->decl '(y (realp 1 (* m n))))))

  (assert-equalp
    (list (cons 'x (sexpr->exprs '(5)))
	  (cons 'n '())
	  (cons 'y (sexpr->exprs '(n 2)))
	  (cons 'z '())
	  (cons 'w (sexpr->exprs '(3 n))))
    (args-vars-dims
      (raw-sexpr->model
        '(:model
	   (:args (x (integer  5))
		  (n integerp))
	   (:reqs)
	   (:vars (y (realp n 2))
		  (z boolean)
		  (w (integer 3 n)))))))
)

(define-test misc-model-tests
  (assert-equalp
    '(a b c d)
    (args-vars-names
      (raw-sexpr->model
        '(:body (:args (a integerp) (b (real 3 a)))
		(:reqs)
		(:vars (c realp0) (d (realp a (* a a))))))))
)

;; TODO: Add tests that verify error-checking (check-model)
