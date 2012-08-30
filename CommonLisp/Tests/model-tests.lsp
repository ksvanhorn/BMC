(use-package :model)
(use-package :symbols)
(use-package :testing-utilities)

(define-test sexpr->decl-tests
  ;; sexpr->vtype
  (assert-equalp (make-vtype-scalar :stype 'integerp)
		 (sexpr->vtype 'integerp))
  (assert-equalp (make-vtype-scalar :stype 'real)
		 (sexpr->vtype 'real))
  (assert-error 'error (sexpr->vtype 'not-a-type))

  (assert-equalp (make-vtype-array
		   :elem-type 'boolean
		   :dims (list (make-expr-literal :value 2)
			       (make-expr-variable :symbol 'n)))
		 (sexpr->vtype '(boolean 2 n)))
  (assert-equalp (make-vtype-array
		   :elem-type 'realp
		   :dims (list (make-expr-apply
				 :fct '+
				 :args (list (make-expr-variable :symbol 'k)
					     (make-expr-literal :value 3)))))
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

  ;; LHS of deterministic or stochastic relation
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
		       :lo (make-expr-literal :value 1)
		       :hi (make-expr-variable :symbol 'n))
		     (make-array-slice-index-all)))
    (sexpr->rellhs '(@ y i (:range 1 n) :all)))

  (assert-error 'error (sexpr->rellhs 3))
  (assert-error 'error (sexpr->rellhs '(x i)))

  ;; relations
  (assert-equalp
    (make-relation-deterministic
      :lhs (sexpr->rellhs 'x)
      :rhs (sexpr->expr '(+ y z)))
    (sexpr->rel '(<- x (+ y z))))

  (assert-equalp
    (make-relation-deterministic
      :lhs (sexpr->rellhs '(@ x i))
      :rhs (sexpr->expr '(+ y z)))
    (sexpr->rel '(<- (@ x i) (+ y z))))

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
      :members (list (sexpr->rel '(<- x (+ y 2)))
		     (sexpr->rel '(~ (@ y i) (dnorm mu sigma)))))
    (sexpr->rel '(:block
		   (<- x (+ y 2))
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
     :true-branch (sexpr->rel '(<- (@ x i) 3))
     :false-branch (sexpr->rel '(~ (@ y i) (dcat p))))
   (sexpr->rel '(:if (@ good i)
		  (<- (@ x i) 3)
		  (~ (@ y i) (dcat p)))))

  (assert-error 'error (sexpr->rel '(:if (x .< y))))
  (assert-error 'error (sexpr->rel '(:if (<- x y) (~ x (dnorm m s)))))
  (assert-error 'error
    (sexpr->rel '(:if (.= (@ x) 3) (<- x y) (+ a b))))

  (assert-equalp
    (make-relation-loop
      :var 'k
      :lo (sexpr->expr 'm)
      :hi (sexpr->expr '(+ n 2))
      :body (sexpr->rel '(~ (@ x k) (dgamma a b))))
    (sexpr->rel '(:for k (m (+ n 2)) (~ (@ x k) (dgamma a b)))))
  (assert-error 'error
    (sexpr->rel '(:for (@ x i) (m n) (<- y 3))))
  (assert-error 'error
    (sexpr->rel '(:for j (m) (<- (@ x j) 3))))
  (assert-error 'error
    (sexpr->rel '(for j (m n p) (<- (@ x j) 3))))
  (assert-error 'error
    (sexpr->rel '(for j (m n) (* j 3))))
)

(define-test sexpr->model-tests
  (assert-equalp
    (make-model
      :args (list (sexpr->decl '(n integerp0))
		  (sexpr->decl '(x (real n))))
      :reqs (list (sexpr->expr '(< n 200))
		  (sexpr->expr '(qand i (1 n) (<= 0 (@ x i)))))
      :vars (list (sexpr->decl '(y (real n n)))
		  (sexpr->decl '(z (integer (+ n 2)))))
      :body (list (sexpr->rel '(:for i (1 n) (~ (@ y i i) (dnorm m s))))
		  (sexpr->rel '(:if (.= n 100)
				 (~ z (dmvnorm mu Sigma))
		                 (~ z (dwishart nu V))))))
    (sexpr->model '(:model
		     (:args (n integerp0)
		            (x (real n)))
		     (:reqs (< n 200)
                            (qand i (1 n) (<= 0 (@ x i))))
		     (:vars (y (real n n))
                            (z (integer (+ n 2))))
		     (:body (:for i (1 n) (~ (@ y i i) (dnorm m s)))
		            (:if (= n 100)
			      (~ z (dmvnorm mu Sigma))
		              (~ z (dwishart nu V)))))))
  ;; TODO: Add tests that verify error-checking
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
  (assert-equal "x" (rellhs->string (sexpr->rellhs '|x|)))
  (assert-equal "x[i]" (rellhs->string (sexpr->rellhs '(@ |x| |i|))))
  (assert-equal "y[i, 1 : n, ]"
		(rellhs->string
		  (sexpr->rellhs '(@ |y| |i| (:range 1 |n|) :all))))
  (assert-equal "y[i + 2, m + 1 : n - 3]"
		(rellhs->string
		  (sexpr->rellhs
		    '(@ |y| (+ |i| 2) (:range (+ |m| 1) (- |n| 3))))))

  (assert-equal "x <- y + z
"
		(ppstr (pp-rel (sexpr->rel '(<- |x| (+ |y| |z|))))))

  (assert-equal "x[i] <- y + z
"
		(ppstr (pp-rel (sexpr->rel '(<- (@ |x| |i|) (+ |y| |z|))))))

  (assert-equal "y ~ DGAMMA(alpha, beta)
"
		(ppstr (pp-rel (sexpr->rel '(~ |y| (dgamma |alpha| |beta|))))))

  (assert-equal "" (ppstr (pp-rel (sexpr->rel '(:block)))))

  (assert-equal (format nil "    X <- Y + 2~%    Y[i] ~~ DNORM(MU, SIGMA)~%")
		(ppstr (pp-rel (sexpr->rel '(:block
					      (<- x (+ y 2))
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
    x[i] <- 3
}
else {
    y[i] ~ DCAT(p)
}
"
    (ppstr (pp-rel (sexpr->rel '(:if (@ |good| |i|)
				  (<- (@ |x| |i|) 3)
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
	            (qand i (1 n) (<= 0 (@ x i))))
	     (:vars (y (real n n))
	            (z (real (+ n 2))))
	     (:body (:for i (1 n)
		      (~ (@ y i i) (dnorm m s)))
	            (:if (= n 100)
		      (~ z (dmvnorm mu sigma))
		      (~ z (dwishart nu V)))))))
      :indent-amount 2))
)

; TODO: write and test a function that verifies that dimensions of a
;   variable only depend on variables preceding it.