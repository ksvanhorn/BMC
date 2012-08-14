(load "../model-utils.lsp")

(define-test extract-tests
  (let ((mdl '(:model (:args a1 a2) (:reqs r1) (:vars v1 v2 v3) (:body b1 b2))))
    (assert-equal '(a1 a2) (extract-args mdl))
    (assert-equal '(r1) (extract-reqs mdl))
    (assert-equal '(v1 v2 v3) (extract-vars mdl))
    (assert-equal '(b1 b2) (extract-body mdl))))

(define-test decl-tests
  (let ((decl '(var typ)))
    (assert-equal 'var (decl-var decl))
    (assert-equal 'typ (decl-typ decl))))

(define-test type-tests
  (let ((scalar 'some-type)
	(array1 '(typ1 n1))
	(array2 '(typ2 m1 m2)))

    (assert-equal 'scalar (type-class scalar))
    (assert-equal 'array (type-class array1))
    (assert-equal 'array (type-class array2))

    (assert-equal 'typ1 (elem-type array1))
    (assert-equal 'typ2 (elem-type array2))

    (assert-equal '(n1) (type-dims array1))
    (assert-equal '(m1 m2) (type-dims array2))))

(define-test rel-tests
  (let ((rel-deterministic '(<- vard val))
	(rel-stochastic '(~ vars distr))
	(rel-block '(:block rel1 rel2))
	(rel-if-then '(:if test1 rel))
	(rel-if-then-else '(:if test2 rel-true rel-false))
	(rel-loop '(:for i (lo hi) body)))

    (assert-equal 'deterministic (rel-class rel-deterministic))
    (assert-equal 'stochastic (rel-class rel-stochastic))
    (assert-equal 'block (rel-class rel-block))
    (assert-equal 'if-then (rel-class rel-if-then))
    (assert-equal 'if-then-else (rel-class rel-if-then-else))
    (assert-equal 'loop (rel-class rel-loop))

    (assert-equal 'vard (rel-var rel-deterministic))
    (assert-equal 'val (rel-val rel-deterministic))

    (assert-equal 'vars (rel-var rel-stochastic))
    (assert-equal 'distr (rel-distr rel-stochastic))

    (assert-equal '(rel1 rel2) (rel-block-body rel-block))

    (assert-equal 'test1 (rel-if-condition rel-if-then))
    (assert-equal 'rel (rel-true-branch rel-if-then))

    (assert-equal 'test2 (rel-if-condition rel-if-then-else))
    (assert-equal 'rel-true (rel-true-branch rel-if-then-else))
    (assert-equal 'rel-false (rel-false-branch rel-if-then-else))

    (assert-equal 'i (rel-loop-var rel-loop))
    (assert-equal '(lo hi) (rel-loop-bounds rel-loop))
    (assert-equal 'body (rel-loop-body rel-loop))
    (assert-equal 'lo (bounds-lo '(lo hi)))
    (assert-equal 'hi (bounds-hi '(lo hi)))))

(define-test expr-tests
  (let ((app-expr '(fct arg1 arg2))
        (num-expr 37)
        (var-expr 'some-variable)
        (array-app-expr-1 '(@ v i))
        (array-app-expr-2 '(@ w i1 i2)))

    (assert-equal 'funct-app (expr-class app-expr))
    (assert-equal 'literal-num (expr-class num-expr))
    (assert-equal 'variable (expr-class var-expr))
    (assert-equal 'array-app (expr-class array-app-expr-1))
    (assert-equal 'array-app (expr-class array-app-expr-2))

    (assert-equal 'fct (op app-expr))
    (assert-equal '(arg1 arg2) (args app-expr))

    (assert-equal 'v (array-op array-app-expr-1))
    (assert-equal '(i) (array-args array-app-expr-1))

    (assert-equal 'w (array-op array-app-expr-2))
    (assert-equal '(i1 i2) (array-args array-app-expr-2))))

(define-test array-expr-tests
  (let ((simple-args (array-args '(@ v i)))
	(complex-args (array-args '(@ v :all (:range lo hi) i))))
    (assert-equal 'index (index-class (first simple-args)))
    (assert-equal 'all (index-class (first complex-args)))
    (assert-equal 'range (index-class (second complex-args)))
    (assert-equal 'index (index-class (third complex-args)))
    (let ((range-arg (second complex-args)))
      (assert-equal 'lo (range-lo range-arg))
      (assert-equal 'hi (range-hi range-arg)))))

(defmacro assert-pe (s e) `(assert-equal ,s (print-expr ',e)))

(define-test print-expr-tests
    (assert-pe "QSUM(h, (1, 3), ALPHA[h])"
	       (QSUM |h| (1 3) (@ ALPHA |h|)))
    (assert-pe "is_symm_pd(Sigma)"
	       (|is_symm_pd| |Sigma|))
    (assert-pe "dnorm(MU, SIGMA)"
	       (|dnorm| MU SIGMA))
    (assert-pe "X[i, 2 : N]"
	       (@ X |i| (:range 2 N)))
    (assert-pe "MU[j]"
	       (@ MU |j|))
    (assert-pe "SIGMA[, i]"
	       (@ SIGMA :all |i|))
    (assert-pe "3 <= nv"
	       (<= 3 |nv|))
    (assert-pe "N - 1"
	       (- N 1))
    (assert-pe "QAND(k, (1, N), X[k] < X[k + 1])"
	       (QAND |k| (1 N) (< (@ X |k|) (@ X (+ |k| 1)))))
    (assert-pe "A + B * C ^ 2 * F(3) / (X + 7) + Y ^ 3 - EXP(MU)"
	       (- (+ a (/ (* b (^ c 2) (f 3)) (+ x 7)) (^ y 3)) (exp mu))))

(define-test print-decl-tests
  (assert-equal "X : REAL"
		(print-decl '(x real)))
  (assert-equal "A : INTEGER[n - 1, m]"
		(print-decl '(a (integer (- |n| 1) |m|)))))

(define-test print-rel-tests
  (assert-equal 
"  v <- foo(bar)
"
   (print-rel 2 '(<- |v| (|foo| |bar|))))

  (assert-equal 
"  V[i, j] <- EXP(W[i, j])
"
   (print-rel 2 '(<- (@ v |i| |j|) (exp (@ w |i| |j|)))))

  (assert-equal 
"  X ~ DNORM(MU, SIGMA)
"
   (print-rel 2 '(~ x (dnorm mu sigma))))

  (assert-equal 
"    X[i] ~ DGAMMA(ALPHA, BETA)
"
   (print-rel 4 '(~ (@ x |i|) (dgamma alpha beta))))

  (assert-equal 
"  if (Y[I] = 3) {
    X[I] <- Y[I] + Z
  }
"
   (print-rel 2 '(:if (= (@ y i) 3)
	       	   (<- (@ x i) (+ (@ y i) z)))))

  (assert-equal 
"  if (E) {
    X ~ DEXP(B)
  }
  else {
    V <- 12
  }
"
   (print-rel 2 '(:if e (~ x (dexp b)) (<- v 12))))

  (assert-equal 
"    X[I] <- Y[I] + Z
    V[I] ~ DNORM(MU[I], SIGMA)
    X ~ DEXP(B)
"
   (print-rel 4 '(:block
		  (<- (@ x i) (+ (@ y i) z))
		  (~ (@ v i) (dnorm (@ mu i) sigma))
		  (~ x (dexp b)))))

  (assert-equal
"  for (i in N + 2 : M - 1) {
    X[i] ~ DEXP(B[i])
  }
"
   (print-rel 2 '(:for |i| ((+ n 2) (- m 1))
		       (~ (@ x |i|) (dexp (@ b |i|)))))))

(define-test print-model-tests
  (assert-equal
"args {
  N : INTEGER
  F : REAL[N]
}
reqs {
  N >= 0
  QAND(i, (1, N), F[i] != 0)
}
vars {
  M : INTEGER
  X : REAL[N]
}
model {
  M <- N ^ 3
  for (i in 1 : N) {
    X[i] ~ DNORM(MU, SIGMA)
  }
}
"
   (print-model
    '(:model
      (:args (n integer)
	     (f (real n)))
      (:reqs (>= n 0)
	     (qand |i| (1 n) (!= (@ f |i|) 0)))
      (:vars (m integer)
	     (x (real n)))
      (:body
        (<- m (^ n 3))
	(:for |i| (1 n) (~ (@ x |i|) (dnorm mu sigma))))))))

(define-test model-case-xform-tests
  (let ((input
"(:model
  (:args (Sigma real) (y int))
  (:reqs (>= y 0))
  (:vars (v real))
  (:body (<- v (* Sigma y))))
")
	(output
"(:model
  (:args (|Sigma| |real|) (|y| |int|))
  (:reqs (>= |y| 0))
  (:vars (|v| |real|))
  (:body (<- |v| (* |Sigma| |y|))))
"))
    (assert-equal output (model-string-case-xform input))))

(define-test base-decl-tests
  (assert-equal '(x realxn) (base-decl '(x real)))
  (assert-equal '(x realxn) (base-decl '(x realx)))
  (assert-equal '(x realxn) (base-decl '(x realnn)))
  (assert-equal '(x realxn) (base-decl '(x realp)))
  (assert-equal '(x realxn) (base-decl '(x realxn)))

  (assert-equal '(v integer) (base-decl '(v integer)))
  (assert-equal '(v integer) (base-decl '(v integernn)))
  (assert-equal '(v integer) (base-decl '(v integerp)))

  (assert-equal '(y boolean) (base-decl '(y boolean)))

  (assert-equal '(x (realxn 1)) (base-decl '(x (real i))))
  (assert-equal '(v (integer 2)) (base-decl '(v (integerp nvars m))))

  (assert-equal 3 (num-dims '(realxn 3)))
  (assert-equal 1 (num-dims '(integer 1))))

(define-test base-decls-tests
  (assert-equal
   '((x realxn) (y (integer 3)))
   (args-base-decls '(:model
		      (:args (x real)
			     (y (integerp 2 4 3)))
		      (:reqs)
		      (:vars)
		      (:body))))
  (assert-equal
   '((x realxn) (y (integer 3)))
   (vars-base-decls '(:model
		      (:args)
		      (:reqs)
		      (:vars (x real)
			     (y (integerp 2 4 3)))
		      (:body)))))

(define-test misc-utils-tests
  (assert-equal '(9 16 25) (map-range 3 5 (lambda (n) (* n n))))
  (assert-equal '(i1 i3 i6) (index-vars 3 '(+ i2 (* i4 i5)))))

(define-test arg-checks-tests
  (assert-equal
   '((>= nresp 0)
     (= (array-length 1 mu) 3)
     (QAND i1 (1 3) (is-real (@ mu i1)))
     (= (array-length 1 foo) nresp)
     (= (array-length 2 foo) 2)
     (QAND i1 (1 nresp) (QAND i2 (1 2) (> (@ foo i1 i2) 0)))
     (= (array-length 1 bar) nvar)
     (> nvar 3)
     (= 1 (QSUM k (1 3) (@ mu k))))
   (args-checks '(:model
		  (:args
		   (nresp integernn)
		   (nvar integer)
		   (mu (real 3))
		   (foo (integerp nresp 2))
		   (bar (integer nvar)))
		  (:reqs
		   (> nvar 3)
		   (= 1 (QSUM k (1 3) (@ mu k))))
		  (:vars)
		  (:body)))))

#|
(defun gensym-fct ()
  (let ((ctr 0))
    (lambda (pfx)
      (incf ctr)
      (intern (concatenate 'string pfx (write-to-string ctr)))))) 

(define-test extract-pdf-tests
  (assert-equal
   '(precond-postcond-density
     (and (is-real mu) (is-realp sigma))
     (is-real x)
     (* (1/ (sqrt (* 2 %pi)))
	(1/ sigma)
	(exp (* -1/2 (1/ (sqr sigma)) (sqr (- x mu))))))
   (rel->density '(~ x (dnorm mu sigma))))

  (assert-equal
   '(let ((n1 (array-length 1 vec)))
      (precond-postcond-density
       (and (is-real y) (is-array 1 vec) (< 0 n1)
	    (qand i1 (1 n1) (is-real (@ vec i1)))
	    (qand i1 (1 (1- n1)) (< (@ vec i1) (@ vec (1+ i1)))))
       (and (is-integer x) (<= 1 x) (<= x (1+ n1))
	    (=> (< y (@ vec n1)) (< y (@ vec x)))
	    (=> (<= (@ vec 1) y) (<= (@ vec (1- x)) y)))
       1))
   (rel->density '(~ x (dinterval y vec))))

  (assert-equal
   '(let ((n1 (length m)))
      (precond-postcond-density
       (and (is-array 1 m) (qand i1 (1 n1) (is-real (@ m i1)))
	    (= n1 (array-length 1 S)) (is-symm-pd S))
       (and (is-array 1 y) (and i1 (1 n1) (is-real (@ y i1))))
       (* (^ (1/ (sqrt (* 2 %pi))) n1)
	  (1/ (sqrt (abs (det S))))
	  (exp (* -1/2 (quadmul (inv S) (vec- y m)))))))
   (rel->density '(~ y (dmvnorm m S))))

  (assert-equal
   '(precond-postcond-density
     (and (is-realp a) (is-realp b))
     (is-realp (@ lambda i))
     (* (^ b a) (1/ (gamma a)) (^ (@ lambda i) (1- a))
	(exp (neg (* b (@ lambda i))))))
   (rel->density '(~ (@ lambda i) (dgamma a b))))

  (assert-equal
   '(let ((p1 (array-length 1 V)))
      (precond-postcond-density
       (and (is-real nu) (< (1- p1) nu) (is-symm-pd V))
       (and (is-symm-pd L) (= p1 (array-length 1 L)))
       (* (^ 2 (* -1/2 nu p1))
	  (1/ (mvgamma p1 (* 1/2 nu)))
	  (^ (abs (det V)) (* -1/2 nu))
	  (^ (abs (det L)) (* 1/2 (- nu p1 1)))
	  (exp (* -1/2 (trace (matmul (inv V) L)))))))
   (rel->density '(~ L (dwishart nu V))))

  (assert-equal
   '(let ((n1 (array-length 1 p)))
      (precond-postcond-density
       (is-probability-vector p)
       (and (is-integerp y) (<= y n1))
       (@ p y)))
   (rel->density '(~ y (dcat p))))

  (assert-equal
   '(let (n1 (length alpha))
      (precond-postcond-density
       (and (is-array 1 alpha) (qand i1 (1 n1) (is-realp (@ alpha i1))))
       (and (is-probability-vector p) (= n1 (array-length 1 p)))
       (* (qprod i1 (1 n1) (gamma (@ alpha i1)))
	  (1/ (gamma (qsum i1 (1 n1) (@ alpha i1))))
	  (qprod i1 (1 n1) (^ (@ p i1) (1- (@ alpha i1)))))))
   (rel->density '(~ p (ddirch alpha))))

  (assert-equal
   '(precond-postcond-density
     (and (is-integer lo) (is-integer hi))
     (and)
     (qprod! k (lo hi) (dummy (@ x k) foo)))
   (rel->density '(:for k (lo hi) (~ (@ x k) (ddummy foo)))))

  (assert-equal
   '(*! (dummy x1 foo1) (dummy x2 foo2))
   (rel->density '(:block (~ x1 (ddummy foo1)) (~ x2 (ddummy foo2)))))

  (assert-equal
   '(when (= (@ y i) 1) (dummy (@ x i) foo))
   (rel->density '(:if (= (@ y i) 1) (~ (@ x i) (ddummy foo)))))

  (assert-equal 1 (rel->density '(<- x e)))
)
|#

; *** WHAT ABOUT SUPPORT OF DISTRIBUTIONS? ***

; Predicates
;   is-integernn
;   is-integer
;   is-integerp
;   is-real
;   is-realp
;   is-realnn
;   is-symm-pd
;   is-array <numdims> <expr> 
;   =
;   <=
;   <
;   >=
;   >
;   qand
;   is-probability-vector

; Functions
;   x - y1 - ... - yn
;   x1 + ... + xn
;   x1 * ... * xn
;   x1 *! ... *! xn
;   1-
;   1+
;   neg x
;   x / y
;   1/ x
;   x ^ y
;   exp
;   array indexing (and slicing)
;   qsum
;   qprod
;   qprod!
;   array-length <dim> <expr>
;   list (or array1?)
;   tanh
;   sqrt
;   dot
;   abs
;   det
;   gamma
;   mvgamma
;   trace
;   matmul
;   quadmul
;   vec-
;   inv   (for matrix)
;   sqr

; Constants
;   Integers
;   Rationals
;   %pi
;   %e

; Special forms
;   when <test> <value>  (equals 1 when <test> is false)
;   precond-postcond-density

; Functions:
; indp
; indnn
; ind-symm-pd
