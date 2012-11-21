(defpackage :compile-tests
  (:use :cl :lisp-unit :compile :mcimpl :model :expr :symbols
	:utils :testing-utilities))
(in-package :compile-tests)

(defun sexpr->decls (decls) (mapcar #'model:sexpr->decl decls))
(defun sexpr->exprs (se) (mapcar #'sexpr->expr se))
(defun sexpr->rels (se) (mapcar #'sexpr->rel se))

(defun sexpr->named-expr (se)
  (destructuring-bind (key . expr-se) se
    (cons key (sexpr->expr expr-se))))

(defun sexpr->named-expr-list (se) (mapcar #'sexpr->named-expr se))

(defun sort-unique (symbols)
  (remove-duplicates (sort (copy-list symbols) #'string<)))

(define-test compile-util-tests
  (assert-equal
    '(a b c) (sort-unique '(b c b b a c c a)))

  (assert-equal
    '(i4 k m n x)
    (sort-unique
      (compile::vars-in-expr-list
	 '(#e(* (/ x 4) (+ k 3) (- m n))
	   #e(:quant qand i4 (1 n) (< i4 k))
	   #e(^1/2 (^ x 6))
	   #efalse))))

  (assert-equal
    '(a c x y z)
    (sort-unique
      (compile::vars-in-expr
        #e(+ a (:let (x (* y z)) (^ x c))))))

  (assert-equal
    '(A AA ALPHA B BB FOO I II K M N UPPER-X V W X Z ZZ)
    (sort-unique
      (compile::vars-in-model
        (sexpr->model '(:model
			 (:args (x real)
				(n integer))
			 (:reqs (< n 10)
				(< 0 n))
			 (:vars (v (real n))
				(w (realp n))
				(z integer)
			        (alpha boolean))
			 (:invs (<= 0 z))
			 (:body
			   (:for i (m k)
			     (:block
			       (~ (@ v i) (dnorm a b))
			       (~ (@ w i) (dnorm 0 (exp (@ v i))))))
			   (:for ii (1 1) (:if true (~ alpha (dnorm 0 1))))
			   (:if (and (< x upper-x) (is-integer (@ v 1)))
			      (~ z (dgamma 1 (+ zz 8))))
			   (:if (< 0 foo)
			     (~ z (dnorm (+ 3 aa) 1))
			     (~ z (dgamma (* 4 bb) 1)))))))))


  (assert-equal
    '(|i1| |i2|) (compile::n-symbols-not-in 2 '(v j k)))

  (assert-equal
    '(|i1| |i3| |i6|)
    (compile::n-symbols-not-in 3 '(|i2| qand |i4| n < |i4| m + * |i5| n)))

  (assert-equal
    '(|i2| |i4|)
    (compile::n-symbols-not-in 2 '(+ x |i3| y |i1| w)))

  (assert-equalp
    #e(:quant qand i (1 (+ n 2))
	      (:quant qand j (1 (- m 3)) (is-real (@ x i j))))
    (compile::array-element-check
      #e(is-real (@ x i j))
      '(#e(+ n 2) #e(- m 3))
      '(i j)))

  (assert-equalp
    #e(:quant qand i (1 (+ n 2)) (< 0 (@ x i)))
    (compile::array-element-check
      #e(< 0 (@ x i))
      (list #e(+ n 2))
      '(i)))

  (assert-equalp
    '()
    (compile::scalar-type-checks #ea 'integer))

  (assert-equalp
    '()
    (compile::scalar-type-checks #e(+ 3 n) 'realxn))

  (assert-equalp
    '(#e(<= 0 (@ x i)))
    (compile::scalar-type-checks #e(@ x i) 'integerp0))

  (assert-equalp
    '(#e(< 0 a))
    (compile::scalar-type-checks #ea 'integerp))

  (assert-equalp
    '(#e(is-real x))
    (compile::scalar-type-checks #ex 'real))

  (assert-equalp
    '(#e(:quant qand |i1| (1 n) (< 0 (@ x |i1|))))
    (compile::array-element-checks 'x 'integerp '(#en)))

  (assert-equalp
    (list (sexpr->expr '(:quant qand |i1| (1 n)
			      (:quant qand |i2| (1 k)
				      (is-realp0 (@ v |i1| |i2|))))))
    (compile::array-element-checks 'v 'realp0 (sexpr->exprs '(n k))))

  (assert-equalp
    (list (sexpr->expr '(:quant qand |i2| (1 (+ n |i1|)) (is-real (@ y |i2|)))))
    (compile::array-element-checks 'y 'real (sexpr->exprs '((+ n |i1|)))))

  (assert-equalp
    (list (sexpr->expr '(:quant qand |i1| (1 n)
			      (:quant qand |i3| (1 k)
				      (is-realx (@ |i2| |i1| |i3|))))))
    (compile::array-element-checks '|i2| 'realx (sexpr->exprs '(n k))))

  (assert-equalp
    (list (sexpr->expr '(= (array-length 1 x) n)))
    (compile::array-length-checks 'x (sexpr->exprs '(n))))

  (assert-equalp
    (sexpr->exprs '((= (array-length 1 z) (+ n (* x y)))
		    (= (array-length 2 z) (- m 3))))
    (compile::array-length-checks 'z (sexpr->exprs '((+ n (* x y)) (- m 3)))))

  (assert-equalp
    (list (sexpr->expr '(is-realp x)))
    (compile::decl-checks (sexpr->decl '(x realp))))

  (assert-equalp
    '()
    (compile::decl-checks (sexpr->decl '(n integer))))

  (assert-equalp
    (list (sexpr->expr '(= (array-length 1 A) (+ k 1))))
    (compile::decl-checks (sexpr->decl '(A (integer (+ k 1))))))

  (assert-equalp
    (sexpr->exprs '((= (array-length 1 A) (+ k 1))
		    (:quant qand |i1| (1 (+ k 1)) (<= 0 (@ A |i1|)))))
    (compile::decl-checks (sexpr->decl '(A (integerp0 (+ k 1))))))

  (assert-equalp
    (list (sexpr->expr '(= (array-length 1 x) n)))
    (compile::decl-checks (sexpr->decl '(x (realxn n)))))

  (assert-equalp
    (sexpr->exprs '((= (array-length 1 y) m)
		    (= (array-length 2 y) (* k 3))
		    (:quant qand |i1| (1 m)
			  (:quant qand |i2| (1 (* k 3))
				  (is-real (@ y |i1| |i2|))))))
    (compile::decl-checks (sexpr->decl '(y (real m (* k 3))))))

  (assert-equalp
    (sexpr->exprs '((< 0 n)
		    (<= 0 k)
		    (= (array-length 1 v) m)
		    (:quant qand |i1| (1 m) (is-realp0 (@ v |i1|)))
		    (= (array-length 1 w) n)
		    (= (array-length 1 x) n)
		    (= (array-length 2 x) m)
		    (:quant qand |i1| (1 n)
			  (:quant qand |i2| (1 m) (is-realp (@ x |i1| |i2|))))
		    (= (array-length 1 y) m)
		    (= (array-length 2 y) n)
		    (< m n)
		    (:quant qand i (1 m) (< (@ v i) 100))
		    (is-integerp0 n)))
    (compile::args-checks
      (sexpr->model '(:model
		      (:args (n integerp)
			     (m integer)
			     (k integerp0)
			     (v (realp0 m))
			     (w (realxn n))
			     (x (realp n m))
			     (y (integer m n)))
		      (:reqs (< m n)
			     (:quant qand i (1 m) (< (@ v i) 100)))
		      (:vars (foo integer)
			     (bar (realp n)))
		      (:invs (< 12 foo))
		      (:body)))))

  (assert-equalp
    (sexpr->exprs '((is-integerp0 n)))
    (compile::args-checks
      (sexpr->model '(:model
		      (:args (n integer))
		      (:reqs)
		      (:vars (foo integer)
			     (bar (realp n))
			     (baz (real 5))
			     (bip (integer n)))
		      (:invs)
		      (:body)))))

  (assert-equalp
    (sexpr->exprs
     '((is-realp x)
       (< 0 y)
       (= (array-length 1 z) n)
       (:quant qand |i1| (1 n) (is-realp (@ z |i1|)))))
    (compile::params-checks
     (sexpr->mcimpl
      '(:mcimpl
	(:parameters (x realp) (y integerp) (z (realp n)))
	(:acceptmons)
	(:expectations)
	(:updates)))))

  (let* ((mdl
	  (raw-sexpr->model
	    '(:model
	       (:args (n integerp)
		      (m integerp0)
		      (a (realp n)))
	       (:reqs)
	       (:vars (b (real m n))
		      (c (integer n))
		      (k integerp))
	       (:invs)
	       (:body))))
	 (dim-fct (compile::model->dim-fct mdl)))
    (assert-equal 2 (funcall dim-fct 'b))
    (assert-equal 1 (funcall dim-fct 'c))
    (assert-equal 0 (funcall dim-fct 'k))
    (assert-error 'error (funcall dim-fct 'n))
    (assert-error 'error (funcall dim-fct 'm))
    (assert-error 'error (funcall dim-fct 'a))
    (assert-error 'error (funcall dim-fct 'foo)))

  (let ((mdl (sexpr->model
	      '(:model
		(:args (n integer)
		       (a real))
		(:reqs)
		(:vars (b real)
		       (c (real n)))
		(:invs)
	        (:body))))
	(impl (sexpr->mcimpl
	        '(:mcimpl
	          (:parameters (m integer)
		               (r real))
		  (:acceptmons)
		  (:expectations)
		  (:updates)))))
    (let ((is-class-var (compile::class-var-pred-from mdl impl)))
      (dolist (x '(m r n a b c))
	(assert-true (funcall is-class-var x)))
      (dolist (x '(foo bar x z))
	(assert-false (funcall is-class-var x)))))

  (assert-equalp
    (sexpr->rel '(:metropolis-hastings
		    :lets ()
		    :proposal-distribution (~ p (ddirch a))
		    :log-acceptance-ratio (dot a v)))
    (compile::remove-acceptmon
      (sexpr->rel '(:metropolis-hastings
		    :lets ()
		    :proposal-distribution (~ p (ddirch a))
		    :acceptmon (am i j)
		    :log-acceptance-ratio (dot a v)))))

  (let ((mh (sexpr->rel '(:metropolis-hastings
		    :lets ()
		    :proposal-distribution (~ p (ddirch a))
		    :log-acceptance-ratio (dot a v)))))
    (assert-equalp mh (compile::remove-acceptmon mh)))

#|
  ;; expr-dim
  (let* ((cases
	  '(((o^2 (@ x r (:range 2 nv))) .
	     ((+ 1 (- nv 2)) (+ 1 (- nv 2))))
	    (($* (- (@ x r 1) myf) (@ x r (:range 3 nv))) .
	     ((+ 1 (- nv 3))))
	    ((@ x (:range 3 nr) 3) .
	     ((+ 1 (- nr 3))))
	    ((o^2 (@- (@ x r (:range 2 nv)) mxf)) .
	     ((+ 1 (- nv 2)) (+ 1 (- nv 2))))
	    ((@ x r :all) .
             (nv))))
	 (sexpr->dims-assoc
	   (lambda (se)
	     (destructuring-bind (var-name . dims-se) se
               (cons var-name (sexpr->exprs dims-se)))))
	(var-dims
	  (mapcar sexpr->dims-assoc
	    '((x . (nr nv))
	      (nv . ())
	      (r . ())
	      (myf . ())
	      (mxf . ((- nv 1)))))))
    (dolist (x cases)
      (destructuring-bind (e . dims) x
        (assert-equalp
	  (sexpr->exprs dims)
	  (compile::expr-dim (sexpr->expr e) var-dims)))))
|#
)

(defun cexpr->string (e) (compile::expr->string e))

(define-test compile-expression-test
  (assert-equal "x" (cexpr->string (sexpr->expr '|x|)))
  (assert-equal "5" (cexpr->string (sexpr->expr '5)))
  (assert-equal "-3.25e+0" (cexpr->string (sexpr->expr -3.25)))
  (assert-equal "true" (cexpr->string (sexpr->expr 'true)))
  (assert-equal "false" (cexpr->string (sexpr->expr 'false)))

  (assert-equal "x[i - 1]"
		(cexpr->string (sexpr->expr '(@ |x| |i|))))
  (assert-equal "y[i + 2 - 1, j - 1 - 1]"
		(cexpr->string (sexpr->expr '(@ |y| (+ |i| 2) (- |j| 1)))))
  (assert-equal
    "BMC.ArraySlice(x, i - 1, BMC.Range(lo - 1, hi), BMC.FullRange)"
    (cexpr->string
     (sexpr->expr '(@ |x| |i| (:range |lo| |hi|) :all))))

  (assert-equal "Math.Sqrt(x)"
		(cexpr->string (sexpr->expr '(^1/2 |x|))))
  (assert-equal "BMC.MatrixInverse(x)"
		(cexpr->string (sexpr->expr '(inv |x|))))
  (assert-equal "Math.Exp(x / 2)"
		(cexpr->string (sexpr->expr '(exp (/ |x| 2)))))
  (assert-equal "Math.Tanh(y)"
		(cexpr->string (sexpr->expr '(tanh |y|))))
  (assert-equal "BMC.Vec(x, y, 0.0e+0)"
		(cexpr->string (sexpr->expr '(vec |x| |y| 0.0))))
  (assert-equal "BMC.Vec(x, y)"
		(cexpr->string (sexpr->expr '(vec |x| |y|))))
  (assert-equal "x - y"
		(cexpr->string (sexpr->expr '(- |x| |y|))))
  (assert-equal "-(X) * Y + -(Z)"
		(cexpr->string (sexpr->expr '(+ (* (neg x) y) (neg z)))))
  (assert-equal "BMC.QSum(M, N, (I => X[I - 1]))"
    (cexpr->string (sexpr->expr '(:quant qsum i (m n) (@ x i)))))
  (assert-equal "BMC.QSum(M, N, (I => W[I - 1] < X[I - 1]), (I => Y[I - 1]))"
    (cexpr->string (sexpr->expr '(:quant qsum i (m n) (< (@ w i) (@ x i))
					 (@ y i)))))
  (assert-equal "(x < y ? a + b : a * b)"
    (cexpr->string (sexpr->expr '(if-then-else (< |x| |y|)
					       (+ |a| |b|)
					       (* |a| |b|)))))

  (assert-equal "BMC.Let(y * y, (x => c * x))"
    (cexpr->string (sexpr->expr '(:let (|x| (* |y| |y|))
				   (* |c| |x|)))))
)

(define-test compile-ljd-tests
  (assert-equal
    "ljd += BMC.LogDensityDirichlet(p, alpha_p);
"
    (ppstr (compile::write-ljd-accum-rel "ljd"
	     (sexpr->rel '(~ |p| (ddirch |alpha_p|))))))

  (assert-equal
    "ljd += BMC.LogDensityCat(x, p);
"
    (ppstr (compile::write-ljd-accum-rel '|ljd|
	     (sexpr->rel '(~ |x| (dcat |p|))))))

  (assert-equal
    "ljd += BMC.LogDensityNorm(x[i - 1], MU, SIGMA);
"
    (ppstr (compile::write-ljd-accum-rel "ljd"
	     (sexpr->rel '(~ (@ |x| |i|) (dnorm mu sigma))))))

  (assert-equal
    "ll += BMC.LogDensityGamma(x[i - 1], A * C, B / D);
"
    (ppstr (compile::write-ljd-accum-rel "ll"
	     (sexpr->rel '(~ (@ |x| |i|) (dgamma (* a c) (/ b d)))))))

  (assert-equal
    "ljd += BMC.LogDensityMVNorm(x, MU, SIGMA);
"
    (ppstr (compile::write-ljd-accum-rel "ljd"
	     (sexpr->rel '(~ |x| (dmvnorm mu sigma))))))

  (assert-equal
    "ljd += BMC.LogDensityWishart(Lambda, nu + 3, V);
"
    (ppstr (compile::write-ljd-accum-rel "ljd"
	     (sexpr->rel '(~ |Lambda| (dwishart (+ |nu| 3) V))))))

  (assert-equal
    "lp += BMC.LogDensityInterval(v, x, c);
"
    (ppstr (compile::write-ljd-accum-rel "lp"
	      (sexpr->rel '(~ |v| (dinterval |x| |c|))))))

  (assert-equal
    (strcat-lines
      "var sigma = 1 / Math.Sqrt(lambda);"
      "lp += BMC.LogDensityNorm(X, 0, sigma);")
    (ppstr (compile::write-ljd-accum-rel "lp"
	    (sexpr->rel '(:let (|sigma| (/ 1 (^1/2 |lambda|)))
			   (~ x (dnorm 0 |sigma|))))
	    nil)))

  (assert-equal
    (strcat-lines
      "{"
      "    var sigma = 1 / Math.Sqrt(lambda);"
      "    lp += BMC.LogDensityNorm(X, 0, sigma);"
      "}")
    (ppstr (compile::write-ljd-accum-rel "lp"
	    (sexpr->rel '(:let (|sigma| (/ 1 (^1/2 |lambda|)))
			   (~ x (dnorm 0 |sigma|))))
	    t)))

  (assert-equal
    (strcat-lines "lp += BMC.LogDensityCat(Y, P);"
		  "lp += BMC.LogDensityNorm(X, 0, SIGMA[Y - 1]);")
    (ppstr (compile::write-ljd-accum-rel "lp"
	     (sexpr->rel '(:block
			    (~ y (dcat p))
			    (~ x (dnorm 0 (@ sigma y))))))))

  (assert-equal
    (strcat-lines
	    "if (X[I - 1] == 1) {"
	    "    acc += BMC.LogDensityNorm(Y[I - 1], A, B);"
            "}")
    (ppstr (compile::write-ljd-accum-rel "acc"
	     (sexpr->rel '(:if (= (@ x i) 1)
			    (~ (@ y i) (dnorm a b)))))))

  (assert-equal
    (strcat-lines
	    "if (V[I - 1] < 4) {"
	    "    acc += BMC.LogDensityNorm(Z[I - 1], M, S);"
            "}"
	    "else {"
	    "    acc += BMC.LogDensityCat(W[I - 1], P);"
	    "}")
    (ppstr (compile::write-ljd-accum-rel "acc"
	     (sexpr->rel '(:if (< (@ v i) 4)
			    (~ (@ z i) (dnorm m s))
			    (~ (@ w i) (dcat p)))))))

  (assert-equal
    (strcat-lines
      "for (int i = M - 1; i <= N + 2; ++i) {"
      "    lp += BMC.LogDensityGamma(X[i - 1], Math.Sqrt(Y[i - 1]), 1);"
      "}")
    (ppstr (compile::write-ljd-accum-rel "lp"
	     (sexpr->rel '(:for |i| ((- m 1) (+ n 2))
                            (~ (@ x |i|) (dgamma (^1/2 (@ y |i|)) 1)))))))

  (assert-equal
    ""
    (ppstr (compile::write-ljd-accum-rel "lp" (make-relation-skip))))

  ; Test that brackets placed around "let" only when necessary.
  (assert-equal
"{
    var A = 1;
    var B = 2;
    lp += BMC.LogDensityGamma(X, A, B);
}
for (int I = M; I <= N; ++I) {
    var A = 1;
    var B = 2;
    lp += BMC.LogDensityGamma(Y[I - 1], A, B);
}
if (M < N) {
    var A = 1;
    var B = 2;
    lp += BMC.LogDensityGamma(Z, A, B);
}
else {
    var A = 1;
    var B = 2;
    lp += BMC.LogDensityGamma(W, A, B);
}
"
    (ppstr (compile::write-ljd-accum-rel "lp"
	     (sexpr->rel
	       '(:block
		  (:let (a 1)
                  (:let (b 2)
                    (~ x (dgamma a b))))
		  (:for i (m n)
                    (:let (a 1)
                    (:let (b 2)
                      (~ (@ y i) (dgamma a b)))))
		  (:if (< m n)
                    (:let (a 1)
                    (:let (b 2)
                      (~ z (dgamma a b))))
		    (:let (a 1)
		    (:let (b 2)
                      (~ w (dgamma a b))))))))))
)

(define-test compile-test-acceptance-ratio-tests
  (let* ((rel 'rel-stub)
	 (is-class-var-stub (fn (v) nil))
	 (dim-fct-stub (fn (v) 0))
	 (write-body (fn (rel1 icv1 dimfct1)
		       (assert-eq rel rel1)
		       (assert-eq is-class-var-stub icv1)
		       (assert-eq dim-fct-stub dimfct1)
		       (fmt "// ..."))))
    (assert-equal
"private static void TestAcceptanceRatio_PHI(TheClass _x, double _tol)
{
    _x = _x.Copy();
    // ...
}
"
      (ppstr
       (compile::write-test-acceptance-ratio
	 "TheClass" 'phi rel is-class-var-stub dim-fct-stub write-body))))

  (flet
   ((write-log-proposal-density-test (expected &key rel xform class-vars)
      (let ((rel1 (sexpr->rel rel))
	    (is-class-var (fn (v) (member v class-vars)))
	    (var-xform (fn (v) (intern (strcat xform (symbol-name v))))))
	(assert-equal
	  expected
	  (ppstr
	    (compile::write-log-proposal-density
	      var-xform is-class-var rel1))))))

   (write-log-proposal-density-test
"_x.P = BMC.Copy(new_P);
_lpd += BMC.LogDensityDirichlet(_x.P, _x.A);
"
     :rel '(~ p (ddirch a))
     :xform "new_"
     :class-vars '(a p))

   (write-log-proposal-density-test
"_x.Y = BMC.Copy(old_Y);
_lpd += BMC.LogDensityCat(_x.Y, P);
"
     :rel '(~ y (dcat p))
     :xform "old_"
     :class-vars '(y))

   (write-log-proposal-density-test
"_x.Y[I - 1] = BMC.Copy(_new_Y[I - 1]);
_lpd += BMC.LogDensityNorm(_x.Y[I - 1], _x.MU, SIGMA);
"
     :rel '(~ (@ y i) (dnorm mu sigma))
     :xform "_new_"
     :class-vars '(y mu))

   (write-log-proposal-density-test
"_x.Z[R - 1] = BMC.Copy(_old_Z[R - 1]);
_lpd += BMC.LogDensityGamma(_x.Z[R - 1], A * _x.C, _x.B / D);
"
     :rel '(~ (@ z r) (dgamma (* a c) (/ b d)))
     :xform "_old_"
     :class-vars '(z c b))

   (write-log-proposal-density-test
"_x.V = BMC.Copy(new_V);
_lpd += BMC.LogDensityMVNorm(_x.V, MU, _x.SIGMA);
"
     :rel '(~ v (dmvnorm mu sigma))
     :xform "new_"
     :class-vars '(v sigma))

   (write-log-proposal-density-test
"_x.Lambda = BMC.Copy(old_Lambda);
_lpd += BMC.LogDensityWishart(_x.Lambda, NU + 3, W);
"
     :rel '(~ |Lambda| (dwishart (+ nu 3) W))
     :xform "old_"
     :class-vars '(|Lambda|))

   (write-log-proposal-density-test
"_x.U = BMC.Copy(new_U);
_lpd += BMC.LogDensityInterval(_x.U, _x.Y, C);
"
     :rel '(~ u (dinterval y c))
     :xform "new_"
     :class-vars '(u y))

   (write-log-proposal-density-test
"{
    var SIGMA = 1 / Math.Sqrt(_x.LAMBDA);
    _x.Y = BMC.Copy(old_Y);
    _lpd += BMC.LogDensityNorm(_x.Y, 0, SIGMA);
}
"
     :rel '(:let (sigma (/ 1 (^1/2 lambda)))
             (~ y (dnorm 0 sigma)))
     :xform "old_"
     :class-vars '(y lambda))

   (write-log-proposal-density-test
"{
    var X0 = BMC.Copy(_x.X);
    _x.X = BMC.Copy(old_X);
    _lpd += BMC.LogDensityNorm(_x.X, X0, _x.SIGMA);
}
"
     :rel '(:let (x0 x) (~ x (dnorm x0 sigma)))
     :xform "old_"
     :class-vars '(x sigma))

   (write-log-proposal-density-test
"_x.Y = BMC.Copy(new_Y);
_lpd += BMC.LogDensityCat(_x.Y, P);
_x.Z = BMC.Copy(new_Z);
_lpd += BMC.LogDensityNorm(_x.Z, 0, _x.SIGMA[_x.Y - 1]);
"
     :rel '(:block
	    (~ y (dcat p))
	    (~ z (dnorm 0 (@ sigma y))))
     :xform "new_"
     :class-vars '(y z sigma))

   (write-log-proposal-density-test
"{
    var A = BMC.Sqr(G);
    var B = BMC.Sqr(_x.H);
    _x.Y = BMC.Copy(old_Y);
    _lpd += BMC.LogDensityGamma(_x.Y, A, B);
}
_x.U = BMC.Copy(old_U);
_lpd += BMC.LogDensityNorm(_x.U, M, _x.Y);
"
     :rel '(:block
	     (:let (a (^2 g))
             (:let (b (^2 h))
               (~ y (dgamma a b))))
	     (~ u (dnorm m y)))
     :xform "old_"
     :class-vars '(y u h))
  )

  (flet
   ((write-tar-mh-test (expected &key outer-lets rel class-vars)
      (let ((outer-lets-1 (sexpr->named-expr-list outer-lets))
	    (rel1 (sexpr->rel rel))
	    (is-class-var (fn (v) (member v class-vars)))
	    (dim-fct (fn (v) nil)))
	(assert-equal
	  expected
	  (ppstr
	    (compile::write-test-acceptance-ratio-mh
	      outer-lets-1 rel1 is-class-var dim-fct))))))

   (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_P = BMC.Copy(_x.P);
BMC.DrawDirichlet(_x.P, _x.A);
double _lar = 0.0;
var _new_P = BMC.Copy(_x.P);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
_x.P = BMC.Copy(_old_P);
_lpd += BMC.LogDensityDirichlet(_x.P, _x.A);
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.P, _old_P), \"Proposal must be reversible\");
_lpd = 0.0;
_x.P = BMC.Copy(_new_P);
_lpd += BMC.LogDensityDirichlet(_x.P, _x.A);
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");
"
     :outer-lets '()
     :rel '(:metropolis-hastings
	    :lets ()
	    :proposal-distribution (~ p (ddirch a))
	    :log-acceptance-ratio 0.0)
     :class-vars '(p a))

   (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_Y = BMC.Copy(_x.Y);
_x.Y[I - 1, J - 1] = BMC.DrawNorm(_x.MU, SIGMA);
double _lar = 0.0;
var _new_Y = BMC.Copy(_x.Y);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
_x.Y[I - 1, J - 1] = BMC.Copy(_old_Y[I - 1, J - 1]);
_lpd += BMC.LogDensityNorm(_x.Y[I - 1, J - 1], _x.MU, SIGMA);
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.Y, _old_Y), \"Proposal must be reversible\");
_lpd = 0.0;
_x.Y[I - 1, J - 1] = BMC.Copy(_new_Y[I - 1, J - 1]);
_lpd += BMC.LogDensityNorm(_x.Y[I - 1, J - 1], _x.MU, SIGMA);
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");
"
     :outer-lets '()
     :rel '(:metropolis-hastings
	    :lets ()
	    :proposal-distribution (~ (@ y i j) (dnorm mu sigma))
	    :log-acceptance-ratio 0.0)
     :class-vars '(y mu))

  (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_Z = BMC.Copy(_x.Z);
var A = Math.Sqrt(_x.Z);
_x.Z = BMC.DrawNorm(_x.M, _x.S);
double _lar = 0.0;
var _new_Z = BMC.Copy(_x.Z);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
_x.Z = BMC.Copy(_old_Z);
_lpd += BMC.LogDensityNorm(_x.Z, _x.M, _x.S);
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.Z, _old_Z), \"Proposal must be reversible\");
_lpd = 0.0;
_x.Z = BMC.Copy(_new_Z);
_lpd += BMC.LogDensityNorm(_x.Z, _x.M, _x.S);
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ((a (^1/2 z)))
	   :proposal-distribution (~ z (dnorm m s))
	   :log-acceptance-ratio 0.0)
    :class-vars '(z m s))

  ;; Check that the scope of a local variable (sigma) defined in the proposal
  ;; distribution is appropriately limited, for a general M-H update.
  (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_Y = BMC.Copy(_x.Y);
var _save_Y = BMC.Copy(_x.Y);

{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.DrawNorm(0, SIGMA);
}
double _lar = BMC.Sqr(_x.Y / SIGMA);
var _new_Y = BMC.Copy(_x.Y);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.Copy(_old_Y);
    _lpd += BMC.LogDensityNorm(_x.Y, 0, SIGMA);
}
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.Y, _old_Y), \"Proposal must be reversible\");
_lpd = 0.0;
{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.Copy(_new_Y);
    _lpd += BMC.LogDensityNorm(_x.Y, 0, SIGMA);
}
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");

if (!BMC.Accept(_lar)) {
    _x.Y = _save_Y;
}
"
     :outer-lets '()
     :rel '(:metropolis-hastings
	    :lets ()
	    :proposal-distribution
	      (:let (sigma (/ alpha (^1/2 lambda)))
                (~ y (dnorm 0 sigma)))
	    :log-acceptance-ratio (^2 (/ y sigma)))
     :class-vars '(y alpha))

  ;; Check that the scope of a local variable (sigma) defined in the proposal
  ;; distribution is appropriately limited, for a Gibbs update.
  (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_Y = BMC.Copy(_x.Y);
{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.DrawNorm(0, SIGMA);
}
double _lar = 0.0;
var _new_Y = BMC.Copy(_x.Y);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.Copy(_old_Y);
    _lpd += BMC.LogDensityNorm(_x.Y, 0, SIGMA);
}
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.Y, _old_Y), \"Proposal must be reversible\");
_lpd = 0.0;
{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.Copy(_new_Y);
    _lpd += BMC.LogDensityNorm(_x.Y, 0, SIGMA);
}
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");
"
     :outer-lets '()
     :rel '(:metropolis-hastings
	    :lets ()
	    :proposal-distribution
	      (:let (sigma (/ alpha (^1/2 lambda)))
                (~ y (dnorm 0 sigma)))
	    :log-acceptance-ratio 0.0)
     :class-vars '(y alpha))

  (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_Y = BMC.Copy(_x.Y);
var _old_Z = BMC.Copy(_x.Z);
{
    var PVEC = BMC.ArrPlus(_x.FOO, BAR);
    _x.Y = BMC.DrawCat(PVEC);
}
_x.Z = BMC.DrawGamma(A, _x.B);
double _lar = 0.0;
var _new_Y = BMC.Copy(_x.Y);
var _new_Z = BMC.Copy(_x.Z);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
{
    var PVEC = BMC.ArrPlus(_x.FOO, BAR);
    _x.Y = BMC.Copy(_old_Y);
    _lpd += BMC.LogDensityCat(_x.Y, PVEC);
}
_x.Z = BMC.Copy(_old_Z);
_lpd += BMC.LogDensityGamma(_x.Z, A, _x.B);
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.Y, _old_Y), \"Proposal must be reversible\");
Assert.IsTrue(BMC.Equal(_x.Z, _old_Z), \"Proposal must be reversible\");
_lpd = 0.0;
{
    var PVEC = BMC.ArrPlus(_x.FOO, BAR);
    _x.Y = BMC.Copy(_new_Y);
    _lpd += BMC.LogDensityCat(_x.Y, PVEC);
}
_x.Z = BMC.Copy(_new_Z);
_lpd += BMC.LogDensityGamma(_x.Z, A, _x.B);
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");
"
    :outer-lets '((bar . (vec a b c)))
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution
	     (:block
	       (:let (pvec (@+ foo bar))
		 (~ y (dcat pvec)))
	       (~ z (dgamma a b)))
	   :log-acceptance-ratio 0.0)
    :class-vars '(y z foo b))

  (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_U = BMC.Copy(_x.U);
var _save_U = BMC.Copy(_x.U);

_x.U = BMC.DrawNorm(MU, _x.SIGMA);
double _lar = _x.SIGMA * (_x.U - MU);
var _new_U = BMC.Copy(_x.U);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
_x.U = BMC.Copy(_old_U);
_lpd += BMC.LogDensityNorm(_x.U, MU, _x.SIGMA);
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.U, _old_U), \"Proposal must be reversible\");
_lpd = 0.0;
_x.U = BMC.Copy(_new_U);
_lpd += BMC.LogDensityNorm(_x.U, MU, _x.SIGMA);
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");

if (!BMC.Accept(_lar)) {
    _x.U = _save_U;
}
"
    :outer-lets '()
    :rel '(:metropolis-hastings
        :lets ()
        :proposal-distribution (~ u (dnorm mu sigma))
        :log-acceptance-ratio (* sigma (- u mu)))
    :class-vars '(u sigma))

  (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_Z = BMC.Copy(_x.Z);
var _old_W = BMC.Copy(_x.W);
var F = _x.Z[I - 1];
var SIGMA2 = F * F;
var _save_Z_lbI_rb = BMC.Copy(_x.Z[I - 1]);
var _save_W = BMC.Copy(_x.W);

_x.Z[I - 1] = BMC.DrawNormTruncated(MU, SIGMA, _x.A, _x.B);
_x.W = BMC.DrawNorm(_x.Z[I - 1], 1);
double _lar = _x.Z[I - 1] + SIGMA2;
var _new_Z = BMC.Copy(_x.Z);
var _new_W = BMC.Copy(_x.W);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
_x.Z[I - 1] = BMC.Copy(_old_Z[I - 1]);
_lpd += BMC.LogDensityNormTruncated(_x.Z[I - 1], MU, SIGMA, _x.A, _x.B);
_x.W = BMC.Copy(_old_W);
_lpd += BMC.LogDensityNorm(_x.W, _x.Z[I - 1], 1);
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.Z, _old_Z), \"Proposal must be reversible\");
Assert.IsTrue(BMC.Equal(_x.W, _old_W), \"Proposal must be reversible\");
_lpd = 0.0;
_x.Z[I - 1] = BMC.Copy(_new_Z[I - 1]);
_lpd += BMC.LogDensityNormTruncated(_x.Z[I - 1], MU, SIGMA, _x.A, _x.B);
_x.W = BMC.Copy(_new_W);
_lpd += BMC.LogDensityNorm(_x.W, _x.Z[I - 1], 1);
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");

if (!BMC.Accept(_lar)) {
    _x.Z[I - 1] = _save_Z_lbI_rb;
    _x.W = _save_W;
}
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ((f (@ z i)) (sigma2 (* f f)))
	   :proposal-distribution
	     (:block
	       (~ (@ z i) (dnorm-trunc mu sigma a b))
	       (~ w (dnorm (@ z i) 1)))
	   :log-acceptance-ratio (+ (@ z i) sigma2))
    :class-vars '(z m y gamma v a b w))

  (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_Z = BMC.Copy(_x.Z);
_x.Z = BMC.DrawNorm(_x.M, A * B);
double _lar = 0.0;
var _new_Z = BMC.Copy(_x.Z);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
_x.Z = BMC.Copy(_old_Z);
_lpd += BMC.LogDensityNorm(_x.Z, _x.M, A * B);
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.Z, _old_Z), \"Proposal must be reversible\");
_lpd = 0.0;
_x.Z = BMC.Copy(_new_Z);
_lpd += BMC.LogDensityNorm(_x.Z, _x.M, A * B);
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");
"
    :outer-lets '((a . (^2 z))
		  (b . (exp y)))
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution (~ z (dnorm m (* a b)))
	   :log-acceptance-ratio 0.0)
    :class-vars '(y z m))

  (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_S = BMC.Copy(_x.S);
var _old_X = BMC.Copy(_x.X);
var S0 = _x.S[R - 1];
var _save_S_lbR_rb = BMC.Copy(_x.S[R - 1]);
var _save_X_lbR_rb = BMC.Copy(_x.X[R - 1]);

_x.S[R - 1] = BMC.DrawCat(Q);
{
    var M = _x.MU[_x.S[R - 1] - 1];
    _x.X[R - 1] = BMC.DrawNorm(M, SIGMA);
}
double _lar = _x.P[_x.S[R - 1] - 1] - _x.P[S0 - 1];
var _new_S = BMC.Copy(_x.S);
var _new_X = BMC.Copy(_x.X);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
_x.S[R - 1] = BMC.Copy(_old_S[R - 1]);
_lpd += BMC.LogDensityCat(_x.S[R - 1], Q);
{
    var M = _x.MU[_x.S[R - 1] - 1];
    _x.X[R - 1] = BMC.Copy(_old_X[R - 1]);
    _lpd += BMC.LogDensityNorm(_x.X[R - 1], M, SIGMA);
}
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.S, _old_S), \"Proposal must be reversible\");
Assert.IsTrue(BMC.Equal(_x.X, _old_X), \"Proposal must be reversible\");
_lpd = 0.0;
_x.S[R - 1] = BMC.Copy(_new_S[R - 1]);
_lpd += BMC.LogDensityCat(_x.S[R - 1], Q);
{
    var M = _x.MU[_x.S[R - 1] - 1];
    _x.X[R - 1] = BMC.Copy(_new_X[R - 1]);
    _lpd += BMC.LogDensityNorm(_x.X[R - 1], M, SIGMA);
}
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");

if (!BMC.Accept(_lar)) {
    _x.S[R - 1] = _save_S_lbR_rb;
    _x.X[R - 1] = _save_X_lbR_rb;
}
"
    :outer-lets '((Q0 . (@* A B)) (Q . ($* (^-1 (sum Q0)) Q0)))
    :rel '(:metropolis-hastings
	   :lets ((s0 (@ s r)))
	   :proposal-distribution (:block
				    (~ (@ s r) (dcat Q))
				    (:let (m (@ mu (@ s r)))
				      (~ (@ x r) (dnorm m sigma))))
	   :log-acceptance-ratio (- (@ p (@ s r)) (@ p s0)))
    :class-vars '(s x A B p mu))

  (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_Y = BMC.Copy(_x.Y);
var _old_V = BMC.Copy(_x.V);
BMC.DrawMVNorm(_x.Y, _x.MU_Y, _x.SIGMA_Y);
for (int I = 1; I <= N; ++I) {
    if (1 == Z[I - 1]) {
        var M = BMC.Dot(_x.MU_V, BMC.ArraySlice(_x.U, I - 1, BMC.FullRange));
        _x.V[I - 1] = BMC.DrawNorm(M, _x.SIGMA_V);
    }
}
double _lar = 0.0;
var _new_Y = BMC.Copy(_x.Y);
var _new_V = BMC.Copy(_x.V);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
_x.Y = BMC.Copy(_old_Y);
_lpd += BMC.LogDensityMVNorm(_x.Y, _x.MU_Y, _x.SIGMA_Y);
for (int I = 1; I <= N; ++I) {
    if (1 == Z[I - 1]) {
        var M = BMC.Dot(_x.MU_V, BMC.ArraySlice(_x.U, I - 1, BMC.FullRange));
        _x.V[I - 1] = BMC.Copy(_old_V[I - 1]);
        _lpd += BMC.LogDensityNorm(_x.V[I - 1], M, _x.SIGMA_V);
    }
}
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.Y, _old_Y), \"Proposal must be reversible\");
Assert.IsTrue(BMC.Equal(_x.V, _old_V), \"Proposal must be reversible\");
_lpd = 0.0;
_x.Y = BMC.Copy(_new_Y);
_lpd += BMC.LogDensityMVNorm(_x.Y, _x.MU_Y, _x.SIGMA_Y);
for (int I = 1; I <= N; ++I) {
    if (1 == Z[I - 1]) {
        var M = BMC.Dot(_x.MU_V, BMC.ArraySlice(_x.U, I - 1, BMC.FullRange));
        _x.V[I - 1] = BMC.Copy(_new_V[I - 1]);
        _lpd += BMC.LogDensityNorm(_x.V[I - 1], M, _x.SIGMA_V);
    }
}
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");
"
    :outer-lets '()
    :rel
    '(:metropolis-hastings
      :lets ()
      :proposal-distribution
      (:block 
        (~ y (dmvnorm mu_y sigma_y))
        (:for i (1 n)
          (:if (= 1 (@ z i))
            (:let (m (dot mu_v (@ u i :all)))
              (~ (@ v i) (dnorm m sigma_v))))))
      :log-acceptance-ratio 0.0)
    :class-vars '(y v mu_y sigma_y u mu_v sigma_v))

  (write-tar-mh-test
"double _ljd0 = _x.LogJointDensity();
var _old_Y = BMC.Copy(_x.Y);
var _old_W = BMC.Copy(_x.W);
var Y0 = BMC.Copy(_x.Y);
var _save_Y = BMC.Copy(_x.Y);
var _save_W = BMC.Copy(_x.W);

{
    var W0 = BMC.Copy(_x.W);
    BMC.DrawMVNorm(_x.Y, _x.M, SIGMAY);
    BMC.DrawMVNorm(_x.W, W0, SIGMAW);
}
double _lar = BMC.Dot(_x.Y, Y0);
var _new_Y = BMC.Copy(_x.Y);
var _new_W = BMC.Copy(_x.W);
double _ljd1 = _x.LogJointDensity();
double _lpd = 0.0;
{
    var W0 = BMC.Copy(_x.W);
    _x.Y = BMC.Copy(_old_Y);
    _lpd += BMC.LogDensityMVNorm(_x.Y, _x.M, SIGMAY);
    _x.W = BMC.Copy(_old_W);
    _lpd += BMC.LogDensityMVNorm(_x.W, W0, SIGMAW);
}
double _lpd1 = _lpd;
Assert.IsTrue(BMC.Equal(_x.Y, _old_Y), \"Proposal must be reversible\");
Assert.IsTrue(BMC.Equal(_x.W, _old_W), \"Proposal must be reversible\");
_lpd = 0.0;
{
    var W0 = BMC.Copy(_x.W);
    _x.Y = BMC.Copy(_new_Y);
    _lpd += BMC.LogDensityMVNorm(_x.Y, _x.M, SIGMAY);
    _x.W = BMC.Copy(_new_W);
    _lpd += BMC.LogDensityMVNorm(_x.W, W0, SIGMAW);
}
double _lpd0 = _lpd;
Assert.AreEqual(_lar, (_ljd1 - _ljd0) + (_lpd1 - _lpd0), _tol, \"Log acceptance ratio\");

if (!BMC.Accept(_lar)) {
    _x.Y = _save_Y;
    _x.W = _save_W;
}
"
    :outer-lets '()
    :rel
    '(:metropolis-hastings
      :lets ((y0 y))
      :proposal-distribution
      (:let (w0 w)
	(:block
	  (~ y (dmvnorm m sigmay))
	  (~ w (dmvnorm w0 sigmaw))))
      :log-acceptance-ratio (dot y y0))
    :class-vars '(y w m))
   )
)

(defun cvar2str (s) (compile::variable->string s))

(define-test compile-test-is-valid-update-tests
  ;; model-variables-assigned-in
  (assert-equalp
    '(x)
    (compile::model-variables-assigned-in
      (sexpr->rel '(~ x (dnorm mu sigma)))))
  (assert-equalp
    '(y)
    (compile::model-variables-assigned-in
      (sexpr->rel '(~ y (dnorm mu sigma)))))
  (assert-equalp
    '(z)
    (compile::model-variables-assigned-in
      (sexpr->rel '(~ (@ z i j) (dgamma a b)))))
  (assert-equalp
    '(w)
    (compile::model-variables-assigned-in
      (sexpr->rel '(~ (@ w :all (:range m n) j) (dgamma a b)))))
  (assert-equalp
    '(x y)
    (compile::model-variables-assigned-in
      (sexpr->rel '(:block
		     (~ x (dnorm 0 1))
		     (~ y (dgamma 1 1))))))
  (assert-equalp
    '(a b)
    (compile::model-variables-assigned-in
      (sexpr->rel '(:if (< (@ x i) y)
                     (~ a (dcat p))
		     (~ b (dnorm-trunc 0 1))))))
  (assert-equalp
    '(v)
    (compile::model-variables-assigned-in
      (sexpr->rel '(:for i (m n) (~ v (dwishart nu V))))))
  (assert-equalp
    '(s)
    (compile::model-variables-assigned-in
      (sexpr->rel '(:let (a 1) (~ s (dcat q))))))
  (assert-equalp
    '()
    (compile::model-variables-assigned-in (make-relation-skip)))
  (assert-equalp
    '(y)
    (compile::model-variables-assigned-in
      (sexpr->rel '(:metropolis-hastings
		    :lets ((a (+ x y)) (b 3))
		    :proposal-distribution (~ y (dnorm y0 s))
		    :log-acceptance-ratio (* (- y y0) s)))))

  ;; write-assigned-test
  (let ((dim-fct
	  (fn (var) (case var ('x0 0) ('x1 1) ('x2 2))))
	(compile::*variable->string* (compile::var2str-ext '(x0 x1 x2))))

    (assert-equal
"Assert.IsFalse(_assigned_X0, \"X0 assigned\");
_assigned_X0 = true;
"
      (ppstr (compile::write-assigned-test (sexpr->rellhs 'x0) dim-fct)))

    (assert-equal
"for (int _idx = 0; _idx < _assigned_X1.Length; ++_idx) {
    Assert.IsFalse(_assigned_X1[_idx], \"X1[{0}] assigned\", _idx);
    _assigned_X1[_idx] = true;
}
"
      (ppstr (compile::write-assigned-test (sexpr->rellhs 'x1) dim-fct)))

    (assert-equal
"for (int _idx1 = 0; _idx1 < _assigned_X2.NBRows; ++_idx1) {
    for (int _idx2 = 0; _idx2 < _assigned_X2.NBCols; ++_idx2) {
        Assert.IsFalse(_assigned_X2[_idx1, _idx2], \"X2[{0}, {1}] assigned\", _idx1, _idx2);
        _assigned_X2[_idx1, _idx2] = true;
    }
}
"
      (ppstr (compile::write-assigned-test (sexpr->rellhs 'x2) dim-fct)))

    (assert-equal
"Assert.IsFalse(_assigned_X1[K - 1], \"X1[{0}] assigned\", K - 1);
_assigned_X1[K - 1] = true;
"
      (ppstr
       (compile::write-assigned-test (sexpr->rellhs '(@ x1 k)) dim-fct)))

    (assert-equal
"Assert.IsFalse(_assigned_X2[J - 1, K - 1], \"X2[{0}, {1}] assigned\", J - 1, K - 1);
_assigned_X2[J - 1, K - 1] = true;
"
      (ppstr
       (compile::write-assigned-test (sexpr->rellhs '(@ x2 j k)) dim-fct))))

  ;; Outer part (write-test-is-valid-update)
  (assert-equal
"private static void TestIsValidUpdate_FOO(Mdl _x)
{
    _x = _x.Copy();
    // ...
}
"
    (let* ((rel 'rel-stub)
	   (is-class-var-stub (fn (v) nil))
	   (dim-fct-stub (fn (v) 0))
	   (write-body (fn (rel1 icv1 dimfct1)
			 (assert-eq rel rel1)
			 (assert-eq is-class-var-stub icv1)
			 (assert-eq dim-fct-stub dimfct1)
			 (fmt "// ..."))))
      (ppstr
        (compile::write-test-is-valid-update
	  "Mdl" 'foo rel is-class-var-stub dim-fct-stub write-body))))

  ;; Middle part (write-test-is-valid-update-body)
  (macrolet
      ((write-tivu-body-test (&key upd class-vars not-class-vars lines)
	 (let* ((v (gensym))
		(dim-fct (gensym))
		(write-mh (gensym))
		(expected (gensym))
		(MH (mapcar (fn (n)
			      `(:metropolis-hastings :lets ()
			        :proposal-distribution (~ x (dnorm 0 ,n))
				:log-acceptance-ratio %undef))
			    '(0 1 2 3 4 5)))
		(MHrels (mapcar #'sexpr->rel MH))
		(is-class-var
		 `(fn (,v) (member ,v ,class-vars))))
	   `(let* ((,expected (apply #'strcat-lines ,lines))
		   (MH ',MH)
		   (update (sexpr->rel ,upd))
		   (,dim-fct (fn (,v) 0))
		   (,write-mh
		    (fn (outer-lets mh is-cv df)
		      (assert-eq ,dim-fct df)
		      (dolist (,v ,class-vars)
			(assert-true (funcall is-cv ,v)))
		      (dolist (,v ,not-class-vars)
			(assert-false (funcall is-cv ,v)))
		      (fmt "// M-H: ~a" (position mh ',MHrels :test #'equalp))
		      (dolist (x outer-lets)
			(destructuring-bind (var . expr) x
			  (fmt "// ~a = ~a" var (cexpr->string expr)))))))
	      (assert-equal
	       ,expected
	       (ppstr
		(compile::write-test-is-valid-update-body
		  update ,is-class-var ,dim-fct ,write-mh)))))))

    (write-tivu-body-test
     :upd (elt MH 0)
     :class-vars '()
     :not-class-vars '(a b c foo)
     :lines '("// M-H: 0"))

    (write-tivu-body-test
     :upd `(:let (a (+ u v))
	     (:for r (lo (- hi 1))
	       ,(elt MH 1)))
    :class-vars '(u hi)
    :not-class-vars '(a r lo v foo)
    :lines '("var A = _x.U + V;"
	     "var _lo_R = LO;"
	     "var _hi_R = _x.HI - 1;"
	     "for (int R = _lo_R; R <= _hi_R; ++R) {"
	     "    // M-H: 1"
	     "    // A = _x.U + V"
	     "    // _lo_R = LO"
	     "    // _hi_R = _x.HI - 1"
	     "}"))

    (write-tivu-body-test
     :upd `(:let (y0 y) ,(elt MH 2))
     :class-vars '(y other)
     :not-class-vars '()
     :lines '("var Y0 = BMC.Copy(_x.Y);"
	      "// M-H: 2"
	      "// Y0 = _x.Y"))

    (write-tivu-body-test
     :upd `(:if (= 1 (@ seg r)) ,(elt MH 2))
     :class-vars '(seg)
     :not-class-vars '(r |_if_1|)
     :lines '("var _if_1 = 1 == _x.SEG[R - 1];"
	      "if (_if_1) {"
	      "    // M-H: 2"
	      "    // _if_1 = 1 == _x.SEG[R - 1]"
              "}"))

    (write-tivu-body-test
     :upd `(:if test_a (:if test_b (:if test_c ,(elt MH 1) ,(elt MH 5))
                                   ,(elt MH 4))
		       (:if test_d ,(elt MH 3)))
     :class-vars '(test_c test_a)
     :not-class-vars '(test_b test_d)
     :lines '("var _if_1 = _x.TEST_A;"
	      "if (_if_1) {"
              "    var _if_2 = TEST_B;"
              "    if (_if_2) {"
              "        var _if_3 = _x.TEST_C;"
              "        if (_if_3) {"
              "            // M-H: 1"
	      "            // _if_1 = _x.TEST_A"
              "            // _if_2 = TEST_B"
              "            // _if_3 = _x.TEST_C"
              "        }"
              "        else {"
              "            // M-H: 5"
	      "            // _if_1 = _x.TEST_A"
              "            // _if_2 = TEST_B"
              "            // _if_3 = _x.TEST_C"
              "        }"
              "    }"
              "    else {"
              "        // M-H: 4"
	      "        // _if_1 = _x.TEST_A"
              "        // _if_2 = TEST_B"
              "    }"
	      "}"
              "else {"
              "    var _if_4 = TEST_D;"
	      "    if (_if_4) {"
	      "        // M-H: 3"
	      "        // _if_1 = _x.TEST_A"
              "        // _if_4 = TEST_D"
              "    }"
              "}"))

    (write-tivu-body-test
     :upd `(:for s ((+ m 1) (- n 2))
	     (:if (< (@ y s) thresh)
	       (:let (b 28) ,(elt MH 3))
	       (:let (a 37) ,(elt MH 4))))
     :class-vars '(y m)
     :not-class-vars '(s n a b thresh |_lo_S| |_hi_S| |_if_1|)
     :lines '("var _lo_S = _x.M + 1;"
	      "var _hi_S = N - 2;"
	      "for (int S = _lo_S; S <= _hi_S; ++S) {"
	      "    var _if_1 = _x.Y[S - 1] < THRESH;"
	      "    if (_if_1) {"
	      "        var B = 28;"
	      "        // M-H: 3"
	      "        // _lo_S = _x.M + 1"
	      "        // _hi_S = N - 2"
	      "        // _if_1 = _x.Y[S - 1] < THRESH"
	      "        // B = 28"
	      "    }"
	      "    else {"
	      "        var A = 37;"
	      "        // M-H: 4"
	      "        // _lo_S = _x.M + 1"
	      "        // _hi_S = N - 2"
	      "        // _if_1 = _x.Y[S - 1] < THRESH"
	      "        // A = 37"
	      "    }"
              "}")))

  (flet ((write-tivu-body-error (upd)
           (assert-error 'error
	     (write-test-is-valid-update-body
	      (sexpr->rel upd)
	      (fn (v) nil)
	      (fn (v) 0)
	      (fn (ol rel icv df) nil)))))

    (write-tivu-body-error '(~ y (dgamma a b)))

    (write-tivu-body-error
      '(:block 
	 (~ y (dgamma a b))
	 (~ z (dnorm mu s))))

    (write-tivu-body-error
      '(:block
	 (:metropolis-hastings
	  :lets ()
	  :proposal-distribution (~ y (dgamma a b))
	  :log-acceptance-ratio 0)
	 (:metropolis-hastings
	  :lets ()
	  :proposal-distribution (~ z (dnorm mu s))
	  :log-acceptance-ratio 0))))

  ;; Inner part (write-test-is-valid-update-mh)
  (flet ((write-tivu-mh-test (expected &key outer-lets rel class-vars dims)
	   (let ((outer-lets-1 (sexpr->named-expr-list outer-lets))
		 (rel1 (sexpr->rel rel))
		 (is-class-var (fn (v) (member v class-vars)))
		 (dim-fct (fn (v) (assoc-lookup v dims))))
	     (assert-equal
	       expected
	       (ppstr
		 (compile::write-test-is-valid-update-mh
		   outer-lets-1 rel1 is-class-var dim-fct))))))

    (write-tivu-mh-test
"bool [] _assigned_p = new bool[_x.p.Length];
for (int _idx = 0; _idx < _assigned_p.Length; ++_idx) {
    Assert.IsFalse(_assigned_p[_idx], \"p[{0}] assigned\", _idx);
    _assigned_p[_idx] = true;
}
BMC.DrawDirichlet(_x.p, _x.alpha_p);
"
      :outer-lets '()
      :rel '(:metropolis-hastings
	     :lets ()
	     :proposal-distribution (~ |p| (ddirch |alpha_p|))
	     :log-acceptance-ratio 0)
      :class-vars '(|p| |alpha_p|)
      :dims '((|p| . 1)))

    (write-tivu-mh-test
"bool [] _assigned_x = new bool[_x.x.Length];
Assert.IsFalse(_assigned_x[i - 1], \"x[{0}] assigned\", i - 1);
_assigned_x[i - 1] = true;
_x.x[i - 1] = BMC.DrawCat(_x.p);
"
      :outer-lets '()
      :rel '(:metropolis-hastings
	     :lets ()
	     :proposal-distribution (~ (@ |x| |i|) (dcat |p|))
	     :log-acceptance-ratio 0)
      :class-vars '(|p| |x|)
      :dims '((|x| . 1)))

  (write-tivu-mh-test
"BMatrix _assigned_Y = new BMatrix(_x.Y.NBRows, _x.Y.NBCols);
Assert.IsFalse(_assigned_Y[i - 1, j - 1], \"Y[{0}, {1}] assigned\", i - 1, j - 1);
_assigned_Y[i - 1, j - 1] = true;
_x.Y[i - 1, j - 1] = BMC.DrawNorm(_x.MU, SIGMA);
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution (~ (@ y |i| |j|) (dnorm mu sigma))
	   :log-acceptance-ratio 0)
    :class-vars '(y mu)
    :dims '((y . 2)))

  (write-tivu-mh-test
"bool _assigned_X = false;
Assert.IsFalse(_assigned_X, \"X assigned\");
_assigned_X = true;
_x.X = BMC.DrawGamma(_x.A * C, B / _x.D);
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution (~ x (dgamma (* a c) (/ b d)))
	   :log-acceptance-ratio 0)
    :class-vars '(x a d)
    :dims '((x . 0)))

   (write-tivu-mh-test
"bool [] _assigned_V = new bool[_x.V.Length];
for (int _idx = 0; _idx < _assigned_V.Length; ++_idx) {
    Assert.IsFalse(_assigned_V[_idx], \"V[{0}] assigned\", _idx);
    _assigned_V[_idx] = true;
}
BMC.DrawMVNorm(_x.V, MU, _x.SIGMA);
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution (~ v (dmvnorm mu sigma))
	   :log-acceptance-ratio 0)
    :class-vars '(v sigma)
    :dims '((v . 1)))

  (write-tivu-mh-test
"BMatrix _assigned_Lambda = new BMatrix(_x.Lambda.NBRows, _x.Lambda.NBCols);
for (int _idx1 = 0; _idx1 < _assigned_Lambda.NBRows; ++_idx1) {
    for (int _idx2 = 0; _idx2 < _assigned_Lambda.NBCols; ++_idx2) {
        Assert.IsFalse(_assigned_Lambda[_idx1, _idx2], \"Lambda[{0}, {1}] assigned\", _idx1, _idx2);
        _assigned_Lambda[_idx1, _idx2] = true;
    }
}
BMC.DrawWishart(_x.Lambda, _x.nu + 3, V);
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution (~ |Lambda| (dwishart (+ |nu| 3) V))
	   :log-acceptance-ratio 0)
    :class-vars '(|Lambda| |nu|)
    :dims '((|Lambda| . 2)))

  (write-tivu-mh-test
"bool [] _assigned_v = new bool[_x.v.Length];
Assert.IsFalse(_assigned_v[j - 1], \"v[{0}] assigned\", j - 1);
_assigned_v[j - 1] = true;
_x.v[j - 1] = BMC.DrawInterval(_x.u, c);
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution (~ (@ |v| |j|) (dinterval |u| |c|))
	   :log-acceptance-ratio 0)
    :class-vars '(|v| |u|)
    :dims '((|v| . 1)))

  (write-tivu-mh-test
"bool _assigned_Z = false;
var A = Math.Sqrt(_x.Z);
Assert.IsFalse(_assigned_Z, \"Z assigned\");
_assigned_Z = true;
_x.Z = BMC.DrawNorm(_x.M, _x.S);
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ((a (^1/2 z)))
	   :proposal-distribution (~ z (dnorm m s))
	   :log-acceptance-ratio 0)
    :class-vars '(z m s)
    :dims '((z . 0)))

  (write-tivu-mh-test
"bool _assigned_y = false;
{
    var sigma = _x.alpha / Math.Sqrt(lambda);
    Assert.IsFalse(_assigned_y, \"y assigned\");
    _assigned_y = true;
    _x.y = BMC.DrawNorm(0, sigma);
}
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution
	     (:let (|sigma| (/ |alpha| (^1/2 |lambda|)))
	       (~ |y| (dnorm 0 |sigma|)))
	   :log-acceptance-ratio 0)
    :class-vars '(|y| |alpha|)
    :dims '((|y| . 0)))

(write-tivu-mh-test
"bool [] _assigned_Z = new bool[_x.Z.Length];
bool [] _assigned_Y = new bool[_x.Y.Length];
var Z0 = BMC.Copy(_x.Z);
{
    var Y0 = BMC.Copy(_x.Y);
    for (int _idx = 0; _idx < _assigned_Z.Length; ++_idx) {
        Assert.IsFalse(_assigned_Z[_idx], \"Z[{0}] assigned\", _idx);
        _assigned_Z[_idx] = true;
    }
    BMC.DrawMVNorm(_x.Z, Z0, SIGMAZ);
    for (int _idx = 0; _idx < _assigned_Y.Length; ++_idx) {
        Assert.IsFalse(_assigned_Y[_idx], \"Y[{0}] assigned\", _idx);
        _assigned_Y[_idx] = true;
    }
    BMC.DrawMVNorm(_x.Y, Y0, SIGMAY);
}
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ((z0 z))
	   :proposal-distribution
	     (:let (y0 y)
	       (:block
                 (~ z (dmvnorm z0 sigmaz))
		 (~ y (dmvnorm y0 sigmay))))
	   :log-acceptance-ratio 0.0)
    :class-vars '(y z)
    :dims '((y . 1) (z . 1)))

  ;; Check that the scope of a local variable (sigma) defined in the proposal
  ;; distribution does not include the computation of the acceptance ratio.
  (write-tivu-mh-test
"bool _assigned_y = false;
var _save_y = BMC.Copy(_x.y);

{
    var sigma = _x.alpha / Math.Sqrt(lambda);
    Assert.IsFalse(_assigned_y, \"y assigned\");
    _assigned_y = true;
    _x.y = BMC.DrawNorm(0, sigma);
}
double _lar = BMC.Sqr(_x.y / sigma);

if (!BMC.Accept(_lar)) {
    _x.y = _save_y;
}
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution
	     (:let (|sigma| (/ |alpha| (^1/2 |lambda|)))
	       (~ |y| (dnorm 0 |sigma|)))
	   :log-acceptance-ratio (^2 (/ |y| |sigma|)))
    :class-vars '(|y| |alpha|)
    :dims '((|y| . 0)))

  ;; Check that the scope of a local variable (sigma) defined in the proposal
  ;; distribution does not include the computation of the acceptance ratio.
  (write-tivu-mh-test
"bool _assigned_y = false;
var m = -2;
var _save_y = BMC.Copy(_x.y);

{
    var sigma = _x.alpha / Math.Sqrt(lambda);
    Assert.IsFalse(_assigned_y, \"y assigned\");
    _assigned_y = true;
    _x.y = BMC.DrawNorm(m, sigma);
}
double _lar = BMC.Sqr((_x.y - m) / sigma);
Assert.IsTrue(BMC.Equal(lambda, BMC.Sqr(_x.z)), \"lambda should not change\");

if (!BMC.Accept(_lar)) {
    _x.y = _save_y;
}
"
    :outer-lets '((|lambda| . (^2 |z|)))
    :rel '(:metropolis-hastings
	   :lets ((|m| -2))
	   :proposal-distribution
	     (:let (|sigma| (/ |alpha| (^1/2 |lambda|)))
	       (~ |y| (dnorm |m| |sigma|)))
	   :log-acceptance-ratio (^2 (/ (- |y| |m|) |sigma|)))
    :class-vars '(|y| |alpha| |z|)
    :dims '((|y| . 0)))

  (write-tivu-mh-test
"bool _assigned_Y = false;
bool _assigned_Z = false;
{
    var PVEC = BMC.ArrPlus(_x.FOO, BAR);
    Assert.IsFalse(_assigned_Y, \"Y assigned\");
    _assigned_Y = true;
    _x.Y = BMC.DrawCat(PVEC);
}
Assert.IsFalse(_assigned_Z, \"Z assigned\");
_assigned_Z = true;
_x.Z = BMC.DrawGamma(A, _x.B);
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution
	     (:block
	       (:let (pvec (@+ foo bar))
		 (~ y (dcat pvec)))
	       (~ z (dgamma a b)))
	   :log-acceptance-ratio 0)
    :class-vars '(y z foo b)
    :dims '((z . 0) (y . 0)))

  (write-tivu-mh-test
"bool _assigned_U = false;
var _save_U = BMC.Copy(_x.U);

Assert.IsFalse(_assigned_U, \"U assigned\");
_assigned_U = true;
_x.U = BMC.DrawNorm(MU, _x.SIGMA);
double _lar = _x.SIGMA * (_x.U - MU);

if (!BMC.Accept(_lar)) {
    _x.U = _save_U;
}
"
    :outer-lets '()
    :rel '(:metropolis-hastings
        :lets ()
        :proposal-distribution (~ u (dnorm mu sigma))
        :log-acceptance-ratio (* sigma (- u mu)))
    :class-vars '(u sigma)
    :dims '((u . 0)))

  (write-tivu-mh-test
"bool _assigned_Z = false;
var MU = 2 * _x.M;
var _save_Z = BMC.Copy(_x.Z);

Assert.IsFalse(_assigned_Z, \"Z assigned\");
_assigned_Z = true;
_x.Z = BMC.DrawNorm(MU, _x.SIGMA);
double _lar = _x.SIGMA * (_x.Z - MU);

if (!BMC.Accept(_lar)) {
    _x.Z = _save_Z;
}
"
   :outer-lets '()
   :rel '(:metropolis-hastings
	  :lets ((mu (* 2 m)))
	  :proposal-distribution (~ z (dnorm mu sigma))
	  :log-acceptance-ratio (* sigma (- z mu)))
   :class-vars '(z m sigma)
   :dims '((z . 0)))

  (write-tivu-mh-test
"bool [] _assigned_Z = new bool[_x.Z.Length];
bool _assigned_W = false;
var F = _x.Z[I - 1];
var SIGMA2 = F * F;
var _save_Z_lbI_rb = BMC.Copy(_x.Z[I - 1]);
var _save_W = BMC.Copy(_x.W);

Assert.IsFalse(_assigned_Z[I - 1], \"Z[{0}] assigned\", I - 1);
_assigned_Z[I - 1] = true;
_x.Z[I - 1] = BMC.DrawNormTruncated(MU, SIGMA, _x.A, _x.B);
Assert.IsFalse(_assigned_W, \"W assigned\");
_assigned_W = true;
_x.W = BMC.DrawNorm(_x.Z[I - 1], 1);
double _lar = _x.Z[I - 1] + SIGMA2;

if (!BMC.Accept(_lar)) {
    _x.Z[I - 1] = _save_Z_lbI_rb;
    _x.W = _save_W;
}
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ((f (@ z i)) (sigma2 (* f f)))
	   :proposal-distribution
	     (:block
	       (~ (@ z i) (dnorm-trunc mu sigma a b))
	       (~ w (dnorm (@ z i) 1)))
	   :log-acceptance-ratio (+ (@ z i) sigma2))
    :class-vars '(z m y gamma v a b w)
    :dims '((z . 1) (w . 0)))

  (write-tivu-mh-test
"bool _assigned_Z = false;
Assert.IsFalse(_assigned_Z, \"Z assigned\");
_assigned_Z = true;
_x.Z = BMC.DrawNorm(_x.M, A * B);
Assert.IsTrue(BMC.Equal(A, BMC.Sqr(_x.Z)), \"A should not change\");
Assert.IsTrue(BMC.Equal(B, Math.Exp(_x.Y)), \"B should not change\");
"
    :outer-lets '((a . (^2 z))
		  (b . (exp y)))
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution (~ z (dnorm m (* a b)))
	   :log-acceptance-ratio 0)
    :class-vars '(y z m)
    :dims '((z . 0)))

  (write-tivu-mh-test
"bool _assigned_Z = false;
bool _assigned_U = false;
Assert.IsFalse(_assigned_Z, \"Z assigned\");
_assigned_Z = true;
_x.Z = BMC.DrawNorm(_x.M, A * B);
Assert.IsFalse(_assigned_U, \"U assigned\");
_assigned_U = true;
_x.U = BMC.DrawNorm(0, Math.Exp(_x.Z));
Assert.IsTrue(BMC.Equal(A, BMC.Sqr(_x.Z)), \"A should not change\");
Assert.IsTrue(BMC.Equal(B, Math.Exp(_x.Y)), \"B should not change\");
"
    :outer-lets '((a . (^2 z))
		  (b . (exp y)))
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution (:block
				    (~ z (dnorm m (* a b)))
				    (~ u (dnorm 0 (exp z))))
	   :log-acceptance-ratio 0)
    :class-vars '(y z m u)
    :dims '((z . 0) (u . 0)))

  (write-tivu-mh-test
"bool [] _assigned_S = new bool[_x.S.Length];
var S0 = _x.S[R - 1];
var _save_S_lbR_rb = BMC.Copy(_x.S[R - 1]);

Assert.IsFalse(_assigned_S[R - 1], \"S[{0}] assigned\", R - 1);
_assigned_S[R - 1] = true;
_x.S[R - 1] = BMC.DrawCat(Q);
double _lar = _x.P[_x.S[R - 1] - 1] - _x.P[S0 - 1];
Assert.IsTrue(BMC.Equal(Q0, BMC.ArrTimes(_x.A, _x.B)), \"Q0 should not change\");
Assert.IsTrue(BMC.Equal(Q, BMC.ScalarTimesArr(BMC.Inv(BMC.Sum(Q0)), Q0)), \"Q should not change\");

if (!BMC.Accept(_lar)) {
    _x.S[R - 1] = _save_S_lbR_rb;
}
"
    :outer-lets '((Q0 . (@* A B)) (Q . ($* (^-1 (sum Q0)) Q0)))
    :rel '(:metropolis-hastings
	   :lets ((s0 (@ s r)))
	   :proposal-distribution (~ (@ s r) (dcat Q))
	   :log-acceptance-ratio (- (@ p (@ s r)) (@ p s0)))
    :class-vars '(s A B p)
    :dims '((S . 1)))
  )
)

(define-test compile-test-updates-tests
  (assert-equal
"using System;
using NUnit.Framework;
using Estimation;
using Estimation.Samplers;
using Common;

namespace Tests
{
    public static class TestMyModelUpdates
    {
        public static void TestAllUpdates(MyModel x, double tol)
        {
            x = x.Copy();
            x.Draw();
            TestUpdate_ALPHA(x, tol);
            TestUpdate_BETA(x, tol);
        }

        // Implement TestUpdate_ALPHA

        // Implement TestUpdate_BETA
    }
}
"
    (ppstr (compile::write-test-updates "MyModel" '(alpha beta)
	     (fn (x) (fmt "// Implement TestUpdate_~a" x)))))

  (assert-equal
"public static void TestUpdate_ALPHA(MyModel x, double tol)
{
    TestIsValidUpdate_ALPHA(x);
    TestAcceptanceRatio_ALPHA(x, tol);
}
"
    (ppstr (compile::write-test-update-main "MyModel" 'alpha)))

  (assert-equal
"public static void TestUpdate_BETA(Mdl x, double tol)
{
    TestIsValidUpdate_BETA(x);
    TestAcceptanceRatio_BETA(x, tol);
}
"
    (ppstr (compile::write-test-update-main "Mdl" 'beta)))
)

(define-test compile-tests
  (assert-equal
"using System;
using Common;

namespace Foo
{
    [Serializable]
    public class Bar
    {
        <class body>
    }
}
"
    (ppstr (compile::write-csharp-class
	     "Foo" "Bar" (fn () (fmt "<class body>")))))

  (let ((decls '((|b| boolean)
		 (|ia| integer) (|ib| integerp0) (|ic| integerp)
		 (|ra| realxn) (|rb| realx)
		 (|rc| real) (|rd| realp0) (|re| realp))))
    (assert-equal
"// Blah blah
public bool b;
public int ia;
public int ib;
public int ic;
public double ra;
public double rb;
public double rc;
public double rd;
public double re;
"
      (ppstr (compile::gen-decls "Blah blah" (sexpr->decls decls)))))

  (let ((decls '((|a| (integerp |n|))
		 (|b| (realxn |m| |n|)))))
    (assert-equal
"// Array vars
public int[] a;
public DMatrix b;
"
      (ppstr (compile::gen-decls "Array vars" (sexpr->decls decls)))))

  (assert-equal
"public abstract class AcceptanceMonitor
{
    public abstract void FOO(bool _accepted, double X);
    public abstract void BAR(bool _accepted, int N, double Y);
    public abstract void BAZ(bool _accepted);
}
"
    (ppstr (compile::write-csharp-acceptmons-class
	     `((foo ,(sexpr->decl '(x real)))
	       (bar ,(sexpr->decl '(n integer))
		    ,(sexpr->decl '(y real)))
	       (baz)))))

  (assert-equal
"public MyClassName Copy()
{
    MyClassName the_copy = new MyClassName();
    the_copy.a = BMC.Copy(this.a);
    the_copy.b = BMC.Copy(this.b);
    the_copy.c = BMC.Copy(this.c);
    return the_copy;
}
"
    (ppstr (compile::write-csharp-copy "MyClassName" '(|a| |b| |c|))))

  (assert-equal
"public DifferentClassName Copy()
{
    DifferentClassName the_copy = new DifferentClassName();
    the_copy.a = BMC.Copy(this.a);
    the_copy.b = BMC.Copy(this.b);
    return the_copy;
}
"
    (ppstr (compile::write-csharp-copy "DifferentClassName" '(|a| |b|))))

  (let ((vars '((|a| (real 2)) (|b| (real 2 2)) (|c| (realp n))
		(|d| (real n (- m 1))) (|e| real)
		(|f| (integerp (+ k 1) n)) (|g| (integerp0 k))
		(|h| (boolean n)) (|i| (boolean m k)))))
    (assert-equal
"public void AllocateModelVariables()
{
    if (a == null) a = new double[2];
    if (b == null) b = new DMatrix(2, 2);
    if (c == null) c = new double[N];
    if (d == null) d = new DMatrix(N, M - 1);
    if (f == null) f = new IMatrix(K + 1, N);
    if (g == null) g = new int[K];
    if (h == null) h = new bool[N];
    if (i == null) i = new BMatrix(M, K);
}
"
      (ppstr (compile::write-csharp-allocate-vars (sexpr->decls vars)))))

  (let ((vars '((a real)
		(b (real 2))
		(c (real m n))
		(d real))))
    (assert-equal
"public void AllocateAccumulators()
{
    _expectations = new Accumulators();
    _expectations._N = 0;
    _expectations.B = new double[2];
    _expectations.C = new DMatrix(M, N);
}
"
      (ppstr (compile::write-csharp-allocate-accums (sexpr->decls vars))))

    (assert-equal
"public void OutputExpectations(string _dirpath)
{
    double _N = (double)_expectations._N;

    _expectations.A /= _N;
    BMC.DivBy(_expectations.B, _N);
    BMC.DivBy(_expectations.C, _N);
    _expectations.D /= _N;

    BMC.StoreScalars(_dirpath,
      \"A\", _expectations.A,
      \"D\", _expectations.D);
    BMC.StoreArray(_dirpath, \"B\", _expectations.B);
    BMC.StoreArray(_dirpath, \"C\", _expectations.C);
}
"
      (ppstr (compile::write-csharp-output-expectations (sexpr->decls vars))))
)

  (let ((args '((n integer)
		(k integerp)
		(a real)
		(b boolean)
		(mu (real (+ n 1)))
		(idx (integerp 5))
		(bvec (boolean n))
		(|Sigma| (realx (- n 1) (- n 1)))
		(foo (integerp0 n k))
		(|Bar| (boolean k (* k n))))))
    (assert-equal
"public void LoadArguments(BMC.Loader loader)
{
    N = loader.LoadInteger(\"N\");
    K = loader.LoadInteger(\"K\");
    A = loader.LoadReal(\"A\");
    B = loader.LoadBoolean(\"B\");
    MU = loader.LoadRealArray(\"MU\");
    IDX = loader.LoadIntegerArray(\"IDX\");
    BVEC = loader.LoadBooleanArray(\"BVEC\");
    Sigma = loader.LoadDMatrix(\"Sigma\");
    FOO = loader.LoadIMatrix(\"FOO\");
    Bar = loader.LoadBMatrix(\"Bar\");
}
"
      (ppstr (compile::write-csharp-load-arguments (sexpr->decls args)))))

  (assert-equal
"public double LogJointDensity()
{
    double ljd1 = 0.0;
    {
        var ALPHA_P = BMC.Vec(1.5e+0, 2.0e+0, 1.0e+0);
        ljd1 += BMC.LogDensityDirichlet(P, ALPHA_P);
    }
    for (int i = 1; i <= N; ++i) {
        ljd1 += BMC.LogDensityCat(X[i - 1], P);
    }
    ljd1 += BMC.LogDensityGamma(LAMBDA, A, B);
    {
        var SIGMA = 1 / Math.Sqrt(LAMBDA);
        for (int i = 1; i <= N; ++i) {
            if (X[i - 1] == 1) {
                ljd1 += BMC.LogDensityNorm(Y[i - 1], 0, SIGMA);
            }
            else {
                ljd1 += BMC.LogDensityGamma(Y[i - 1], B, A);
            }
        }
    }
    return ljd1;
}
"
    (ppstr
      (compile::write-csharp-log-joint-density
        (model::raw-sexpr->model
	 '(:model
	   (:args (a realp)
		  (b realp)
		  (n integerp0)
		  (alpha_p (realp n)))
	   (:reqs)
	   (:vars (lambda realp)
		  (p (realp n))
		  (x (integerp n))
		  (sigma realp)
		  (y (real n)))
	   (:invs)
	   (:body
	     (:let (alpha_p (vec 1.5 2.0 1.0))
	       (~ p (ddirch alpha_p)))
	     (:for |i| (1 n)
	       (~ (@ x |i|) (dcat p)))
	     (~ lambda (dgamma a b))
	     (:let (sigma (/ 1 (^1/2 lambda)))
	       (:for |i| (1 n)
	         (:if (= (@ x |i|) 1)
		   (~ (@ y |i|) (dnorm 0 sigma))
		   (~ (@ y |i|) (dgamma b a)))))))))))

  (assert-equalp
"public void ZeroAccumulators()
{
    _expectations._N = 0;
    _expectations.ALPHA = 0.0;
    for (int _idx1 = 0; _idx1 < N; ++_idx1) {
        _expectations.BETA[_idx1] = 0.0;
    }
    for (int _idx1 = 0; _idx1 < M; ++_idx1) {
        for (int _idx2 = 0; _idx2 < K; ++_idx2) {
            _expectations.GAMMA[_idx1, _idx2] = 0.0;
        }
    }
}
"
    (ppstr
      (compile::write-csharp-zero-accum
        (sexpr->decls '((alpha real) (beta (real n)) (gamma (real m k)))))))

  (assert-equalp
"public void Accumulate()
{
    _expectations._N += 1;
    _expectations.ALPHA += A;
    BMC.Check(_expectations.DELTA.Length == DELTA.Length,
              \"_expectations.DELTA.Length == DELTA.Length\");
    for (int _idx1 = 0; _idx1 < M; ++_idx1) {
        _expectations.DELTA[_idx1] += DELTA[_idx1];
    }
    BMC.Check(_expectations.GAMMA.NBRows == G.NBRows,
              \"_expectations.GAMMA.NBRows == G.NBRows\");
    BMC.Check(_expectations.GAMMA.NBCols == G.NBCols,
              \"_expectations.GAMMA.NBCols == G.NBCols\");
    for (int _idx1 = 0; _idx1 < N; ++_idx1) {
        for (int _idx2 = 0; _idx2 < K; ++_idx2) {
            _expectations.GAMMA[_idx1, _idx2] += G[_idx1, _idx2];
        }
    }
}
"
    (ppstr
      (compile::write-csharp-accumulate
        (mcimpl::sexpr->expectations
          '(:expectations
             (alpha real a)
	     (delta (real m) delta)
	     (gamma (real n k) g))))))

  (assert-equalp
"public void Accumulate()
{
    _expectations._N += 1;
    _expectations.ALPHA += Math.Exp(A);
    {
        var _tmp = BMC.MatrixInversePD(LAMBDA);
        BMC.Check(_expectations.SIGMA.NBRows == _tmp.NBRows,
                  \"_expectations.SIGMA.NBRows == _tmp.NBRows\");
        BMC.Check(_expectations.SIGMA.NBCols == _tmp.NBCols,
                  \"_expectations.SIGMA.NBCols == _tmp.NBCols\");
        for (int _idx1 = 0; _idx1 < N; ++_idx1) {
            for (int _idx2 = 0; _idx2 < N; ++_idx2) {
                _expectations.SIGMA[_idx1, _idx2] += _tmp[_idx1, _idx2];
            }
        }
    }
    {
        var _tmp = BMC.QVec(1, M, (I => Math.Sqrt(M[I - 1])));
        BMC.Check(_expectations.DELTA.Length == _tmp.Length,
                  \"_expectations.DELTA.Length == _tmp.Length\");
        for (int _idx1 = 0; _idx1 < M; ++_idx1) {
            _expectations.DELTA[_idx1] += _tmp[_idx1];
        }
    }
}
"
    (ppstr
      (compile::write-csharp-accumulate
        (mcimpl::sexpr->expectations
          '(:expectations
             (alpha real (exp a))
	     (Sigma (real n n) (inv-pd Lambda))
	     (delta (real m) (:quant qvec i (1 m) (^1/2 (@ m i)))))))))
)

(define-test rel-draw-tests
  (assert-equal
"BMC.DrawDirichlet(p, alpha_p);
"
    (ppstr (compile::write-rel-draw
	    (sexpr->rel '(~ |p| (ddirch |alpha_p|)))
	    nil)))

  (assert-equal
"x[i - 1] = BMC.DrawCat(p);
"
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(~ (@ |x| |i|) (dcat |p|)))
	     nil)))

  (assert-equal
    "Y[i - 1, j - 1] = BMC.DrawNorm(MU, SIGMA);
"
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(~ (@ y |i| |j|) (dnorm mu sigma))))))

  (assert-equal
    "X = BMC.DrawGamma(A * C, B / D);
"
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(~ x (dgamma (* a c) (/ b d)))))))

   (assert-equal
    "BMC.DrawMVNorm(V, MU, SIGMA);
"
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(~ v (dmvnorm mu sigma))))))

  (assert-equal
    "BMC.DrawWishart(Lambda, nu + 3, V);
"
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(~ |Lambda| (dwishart (+ |nu| 3) V))))))

  (assert-equal
    "v[j - 1] = BMC.DrawInterval(x, c);
"
    (ppstr (compile::write-rel-draw
	      (sexpr->rel '(~ (@ |v| |j|) (dinterval |x| |c|))))))

  (assert-equal
    (strcat-lines
      "{"
      "    var _idx1_lo = M + 1;"
      "    var _idx1_hi = 2 * N;"
      "    var _buf = BMC.Buffer(V, BMC.Range(_idx1_lo - 1, _idx1_hi));"
      "    BMC.DrawMVNorm(_buf, M, S);"
      "    BMC.CopyInto(V, BMC.Range(_idx1_lo - 1, _idx1_hi), _buf);"
      "}")
    (ppstr (compile::write-rel-draw
	    (sexpr->rel '(~ (@ v (:range (+ m 1) (* 2 n))) (dmvnorm m s))))))

  (assert-equal
    (strcat-lines
      "{"
      "    var _idx1 = I;"
      "    var _buf = BMC.Buffer(V, _idx1 - 1, BMC.FullRange);"
      "    BMC.DrawMVNorm(_buf, M, S);"
      "    BMC.CopyInto(V, _idx1 - 1, BMC.FullRange, _buf);"
      "}")
    (ppstr (compile::write-rel-draw
	    (sexpr->rel '(~ (@ v i :all) (dmvnorm m s))))))

  (assert-equal
    (strcat-lines
      "{"
      "    var _idx2_lo = LO;"
      "    var _idx2_hi = HI;"
      "    var _buf = BMC.Buffer(X, BMC.FullRange, BMC.Range(_idx2_lo - 1, _idx2_hi));"
      "    BMC.DrawWishart(_buf, NU, W);"
      "    BMC.CopyInto(X, BMC.FullRange, BMC.Range(_idx2_lo - 1, _idx2_hi), _buf);"
      "}")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(~ (@ x :all (:range lo hi)) (dwishart nu W))))))  

  (assert-equal
    (strcat-lines
      "{"
      "    var sigma = alpha / Math.Sqrt(lambda);"
      "    x = BMC.DrawNorm(0, sigma);"
      "}")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:let (|sigma| (/ |alpha| (^1/2 |lambda|)))
			    (~ |x| (dnorm 0 |sigma|)))))))

  (assert-equal
    (strcat-lines
      "{"
      "    var sigma = alpha / Math.Sqrt(lambda);"
      "    var mu = m - offset;"
      "    x = BMC.DrawNorm(mu, sigma);"
      "}")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:let (|sigma| (/ |alpha| (^1/2 |lambda|)))
			  (:let (|mu| (- |m| |offset|))
			    (~ |x| (dnorm |mu| |sigma|))))))))

  (assert-equal
    (strcat-lines
      "var Y0 = BMC.Copy(Y);"
      "BMC.DrawMVNorm(Y, Y0, SIGMAY);")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:let (y0 y) (~ y (dmvnorm y0 sigmay))))
	     nil)))

  (assert-equal
    (strcat-lines
      "var sigma = alpha / Math.Sqrt(lambda);"
      "var mu = m - offset;"
      "x = BMC.DrawNorm(mu, sigma);")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:let (|sigma| (/ |alpha| (^1/2 |lambda|)))
			  (:let (|mu| (- |m| |offset|))
			    (~ |x| (dnorm |mu| |sigma|)))))
	     nil)))

  (assert-equal
    (strcat-lines "Y = BMC.DrawCat(PVEC);"
		  "X = BMC.DrawGamma(1, 1);")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:block (~ y (dcat pvec)) (~ x (dgamma 1 1)))))))

  (assert-equal
    (strcat-lines
	    "if (X[I - 1] == 1) {"
	    "    Z = BMC.DrawNorm(A, B);"
            "}")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:if (= (@ x i) 1) (~ z (dnorm a b)))))))
 
  (assert-equal
    (strcat-lines
	    "if (V[I - 1] < 4) {"
	    "    Z[I - 1] = BMC.DrawNorm(M, S);"
            "}"
	    "else {"
	    "    W[I - 1] = BMC.DrawCat(Q);"
	    "}")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:if (< (@ v i) 4)
			    (~ (@ z i) (dnorm m s))
			    (~ (@ w i) (dcat q)))))))

  (assert-equal
    (strcat-lines
      "if (V[I - 1] < 4) {"
      "    var M = MU - DIFF;"
      "    var S = BMC.Sqr(SS);"
      "    Z[I - 1] = BMC.DrawNorm(M, S);"
      "}"
      "else {"
      "    var Q = BMC.ArrPlus(Q1, Q2);"
      "    W[I - 1] = BMC.DrawCat(Q);"
      "}")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:if (< (@ v i) 4)
			    (:let (m (- mu diff))
			    (:let (s (^2 ss))
			      (~ (@ z i) (dnorm m s))))
			    (:let (q (@+ q1 q2))
			      (~ (@ w i) (dcat q))))))))

  (assert-equal
    (strcat-lines
      "for (int i = M - 1; i <= N + 2; ++i) {"
      "    X[i - 1] = BMC.DrawNorm(0, Math.Sqrt(Y[i - 1]));"
      "}")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:for |i| ((- m 1) (+ n 2))
                            (~ (@ x |i|) (dnorm 0 (^1/2 (@ y |i|)))))))))

  (assert-equal
    (strcat-lines
      "for (int i = M - 1; i <= N + 2; ++i) {"
      "    var j = i - 2;"
      "    var MU = FOO[j - 1];"
      "    X[i - 1] = BMC.DrawNorm(MU, Math.Sqrt(Y[i - 1]));"
      "}")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:for |i| ((- m 1) (+ n 2))
			    (:let (|j| (- |i| 2))
			    (:let (mu (@ foo |j|))
                              (~ (@ x |i|) (dnorm mu (^1/2 (@ y |i|)))))))))))

  (flet ((lhs-name-se (se pfx) (compile::lhs-name (sexpr->rellhs se) pfx)))
    (assert-equal "_foo_x" (lhs-name-se '|x| "_foo_"))
    (assert-equal "_old_foo__bar" (lhs-name-se '|foo_bar| "_old_"))
    (assert-equal "_new_Y_lbI_rb" (lhs-name-se '(@ y i) "_new_"))
    (assert-equal "_u_z_lbi_cm_sp_rb" (lhs-name-se '(@ |z| |i| :all) "_u_")))

  (assert-equal
   (strcat-lines
    "var _save_X = BMC.Copy(X);")
   (ppstr
    (compile::write-mh-saves
     (sexpr->rel '(~ x (dnorm m s))))))

  (assert-equal
   (strcat-lines
    "X = _save_X;")
   (ppstr
    (compile::write-mh-restores
     (sexpr->rel '(~ x (dnorm m s))))))

  (assert-equal
   (strcat-lines
    "var _save_Y_lbJ_rb = BMC.Copy(Y[J - 1]);"
    "var _save_X = BMC.Copy(X);")
   (ppstr
    (compile::write-mh-saves
     (sexpr->rel '(:block (~ (@ y j) (dgamma a b)) (~ x (dnorm m s)))))))

  (assert-equal
   (strcat-lines
    "Y[J - 1] = _save_Y_lbJ_rb;"
    "X = _save_X;")
   (ppstr
    (compile::write-mh-restores
     (sexpr->rel '(:block (~ (@ y j) (dgamma a b)) (~ x (dnorm m s)))))))

  (assert-equal
   (strcat-lines
    "var _save_V = BMC.Copy(V);")
   (ppstr
    (compile::write-mh-saves
     (sexpr->rel
      '(:let (m emm) (:let (n 5) (~ v (dcat p))))))))

  (assert-equal
   (strcat-lines
    "V = _save_V;")
   (ppstr
    (compile::write-mh-restores
     (sexpr->rel
      '(:let (m emm) (:let (n 5) (~ v (dcat p))))))))

  (assert-equal
    (strcat-lines
     "var _save_Y_lbI_rb = BMC.Copy(Y[I - 1]);"
     "var _save_W_lbI_cm_spJ_rb = BMC.Copy(W[I - 1, J - 1]);")
    (ppstr
      (compile::write-mh-saves
        (sexpr->rel
	  '(:let (foo (+ bar 7))
	   (:let (baz (* bin boop))
	     (:block (~ (@ y i) (dnorm 0 1))
	             (~ (@ w i j) (dgamma 1 1)))))))))

  (assert-equal
    (strcat-lines
     "Y[I - 1] = _save_Y_lbI_rb;"
     "W[I - 1, J - 1] = _save_W_lbI_cm_spJ_rb;")
    (ppstr
      (compile::write-mh-restores
        (sexpr->rel
	  '(:let (foo (+ bar 7))
	   (:let (baz (* bin boop))
	     (:block (~ (@ y i) (dnorm 0 1))
	             (~ (@ w i j) (dgamma 1 1)))))))))

  (dolist (f '(#'compile::write-mh-saves #'compile::write-mh-restores))

    (assert-error 'error   ; Until I figure out how to handle this case...
      (ppstr
       (funcall f
        (sexpr->rel
          '(:let (m j) (:let (n 5) (~ (@ v m n) (dcat p))))))))

    (assert-error 'error   ; Until I figure out how to handle this case...
      (ppstr
       (funcall f
        (sexpr->rel
          '(:let (m j)
           (:let (n 5)
             (:block (~ (@ v m n) (dcat p))
	             (~ x (dcat q)))))))))

    (assert-error 'error   ; Until I figure out how to handle this case...
      (ppstr
       (funcall f
        (sexpr->rel '(:for j (k r) (~ (@ w j) (dnorm a b)))))))

    (assert-error 'error   ; Until I figure out how to handle this case...
      (ppstr
       (funcall f
        (sexpr->rel '(:if test (~ w (dnorm a b))
		               (~ y (dgamma a b))))))) )

  (assert-equal
"var _save_X = BMC.Copy(X);

X = BMC.DrawNorm(MU, SIGMA);
double _lar = SIGMA * (X - MU);

if (!BMC.Accept(_lar)) {
    X = _save_X;
}
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ()
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :log-acceptance-ratio (* sigma (- x mu))))
     nil)))

  (assert-equal
"var _save_X = BMC.Copy(X);

X = BMC.DrawNorm(MU, SIGMA);
double _lar = SIGMA * (X - MU);

bool _accepted = BMC.Accept(_lar);
if (_accepted) {
    _acceptance_monitor.AM(true);
}
else {
    X = _save_X;
    _acceptance_monitor.AM(false);
}
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ()
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :acceptmon (am)
		   :log-acceptance-ratio (* sigma (- x mu))))
     nil)))

  (assert-equal
"var _save_X = BMC.Copy(X);

X = BMC.DrawNorm(MU, SIGMA);
double _lar = SIGMA * (X - MU);

bool _accepted = BMC.Accept(_lar);
if (_accepted) {
    _acceptance_monitor.AM(true, I, A * B);
}
else {
    X = _save_X;
    _acceptance_monitor.AM(false, I, A * B);
}
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ()
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :acceptmon (am i (* a b))
		   :log-acceptance-ratio (* sigma (- x mu))))
     nil)))

  (assert-equal
"X = BMC.DrawNorm(MU, SIGMA);
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ()
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :log-acceptance-ratio 0))
     nil)))

  (assert-equal
"var MU = 2 * M;
var _save_X = BMC.Copy(X);

X = BMC.DrawNorm(MU, SIGMA);
double _lar = SIGMA * (X - MU);

if (!BMC.Accept(_lar)) {
    X = _save_X;
}
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ((mu (* 2 m)))
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :log-acceptance-ratio (* sigma (- x mu))))
     nil)))

  (assert-equal
"var MU = 2 * M;
X = BMC.DrawNorm(MU, SIGMA);
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ((mu (* 2 m)))
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :log-acceptance-ratio 0))
     nil)))

  (assert-equal
"var Y0 = BMC.Copy(Y);
{
    var W0 = BMC.Copy(W);
    BMC.DrawMVNorm(Y, Y0, SIGMAY);
    BMC.DrawMVNorm(W, W0, SIGMAW);
}
"
    (ppstr
     (compile::write-rel-draw
      (sexpr->rel '(:metropolis-hastings
		    :lets ((y0 y))
		    :proposal-distribution
		    (:let (w0 w)
		      (:block (~ y (dmvnorm y0 sigmay))
			      (~ w (dmvnorm w0 sigmaw))))
		    :log-acceptance-ratio 0.0))
      nil)))

  (assert-equal
"var Y0 = BMC.Copy(Y);
var _save_Y = BMC.Copy(Y);
var _save_W = BMC.Copy(W);

{
    var W0 = BMC.Copy(W);
    BMC.DrawMVNorm(Y, Y0, SIGMAY);
    BMC.DrawMVNorm(W, W0, SIGMAW);
}
double _lar = BMC.Dot(Y, Y0);

if (!BMC.Accept(_lar)) {
    Y = _save_Y;
    W = _save_W;
}
"
    (ppstr
     (compile::write-rel-draw
      (sexpr->rel '(:metropolis-hastings
		    :lets ((y0 y))
		    :proposal-distribution
		    (:let (w0 w)
		      (:block (~ y (dmvnorm y0 sigmay))
			      (~ w (dmvnorm w0 sigmaw))))
		    :log-acceptance-ratio (dot y y0)))
      nil)))

  (assert-equal
"for (int I = M; I <= N; ++I) {
    var MU = BMC.Dot(Y, GAMMA);
    var SIGMA = Math.Exp(U * V[I - 1]);
    var F = Z[I - 1];
    var SIGMA2 = F * F;
    var _save_Z_lbI_rb = BMC.Copy(Z[I - 1]);

    Z[I - 1] = BMC.DrawNormTruncated(MU, SIGMA, A, B);
    double _lar = Z[I - 1] + SIGMA2;

    if (!BMC.Accept(_lar)) {
        Z[I - 1] = _save_Z_lbI_rb;
    }
}
"
    (let ((r (sexpr->rel
	      '(:for i (m n)
		(:let (mu (dot y gamma))
		(:let (sigma (exp (* u (@ v i))))
		  (:metropolis-hastings
		   :lets ((f (@ z i)) (sigma2 (* f f)))
		   :proposal-distribution (~ (@ z i) (dnorm-trunc mu sigma a b))
		   :log-acceptance-ratio (+ (@ z i) sigma2))))))))
      (ppstr (compile::write-rel-draw r))))

  (assert-equal
    ""
    (ppstr (compile::write-rel-draw (make-relation-skip))))

  (assert-equal
    (strcat-lines
      "public void Draw() {"
      "    <body>"
      "}")
    (ppstr (compile::write-prior-draw (fn () (fmt "<body>")))))
)

#|
(define-test dag-check-tests
  (assert-equal
   (strcat-lines
     "private void UndefineAllVars() {"
     "    for (int i1 = 1; i1 <= N; ++i1) {"
     "        for (int i2 = 1; i2 <= 7; ++i2) {"
     "            Y[i1 - 1, i2 - 1] = Double.NaN;"
     "        }"
     "    }"
     "    M = BMC.InvalidInteger;"
     "    R = Double.NaN;"
     "    for (int i1 = 1; i1 <= N + 1; ++i1) {"
     "        Z[i1 - 1] = BMC.InvalidInteger;"
     "    }"
     "}")
   (ppstr (compile::write-undefine-all-vars
     (sexpr->decls
       '((y (realp n 7))
	 (m integer)
	 (r real)
	 (z (integerp0 (+ n 1))))))))
  ; TODO: DAG check tests
)
|#

(defun sexpr->rels (se-list) (mapcar #'sexpr->rel se-list))

(define-test var-type-tests
  (assert-equal
    '(a d m e h k)
    (compile::stochastic-vars
      (sexpr->rels
        '((~ a (dcat p))
	  (:if (< a b)
	    (:block
	      (~ d (dnorm mu sigma))))
	  (:let (x (+ a b))
	    (~ m (dcat q)))
	  (:if (< b c)
	     (:block
	       (~ e (dgamma alph beta)))
	     (:block
	       (~ h (dgamma 0.5 1.5))))
	  (:for i1 ((+ c d) (* f g))
	    (:block
	      (~ (@ k i1) (dnorm h g))))))))

)

(define-test write-csharp-check-tests
  (assert-equalp
"BMC.Check(x < y,
          \"x < y\");
" 
    (ppstr (compile::write-csharp-check (sexpr->expr '(< |x| |y|)))))
  (assert-equalp
"for (int J = M; J <= N; ++J) {
    BMC.Check(X[J - 1] == Y[J - 1],
              \"X[J - 1] == Y[J - 1]\");
}
"
    (ppstr (compile::write-csharp-check
	     (sexpr->expr '(:quant qand j (m n) (= (@ x j) (@ y j))))))) 
  (assert-equalp
"for (int J = M; J <= N; ++J) {
    if (B[J - 1]) {
        BMC.Check(X[J - 1] == Y[J - 1],
                  \"X[J - 1] == Y[J - 1]\");
    }
}
"
    (ppstr (compile::write-csharp-check
	     (sexpr->expr '(:quant qand j (m n) (@ b j)
				   (= (@ x j) (@ y j)))))))
)

(define-test write-csharp-updates-tests
  (assert-equal
"public void Update()
{
    Update_FOO();
    Update_BAR();
    Update_BAZ();
}

public void Update_FOO()
{
    FOO = BMC.DrawNorm(M, S);
}

public void Update_BAR()
{
    BAR = BMC.DrawCat(P);
}

public void Update_BAZ()
{
    BAZ = BMC.DrawGamma(A, B);
}
"
    (ppstr (compile::write-csharp-updates
	    `((foo . ,(sexpr->rel '(~ foo (dnorm m s))))
	      (bar . ,(sexpr->rel '(~ bar (dcat p))))
	      (baz . ,(sexpr->rel '(~ baz (dgamma a b))))))))
)

(define-test ljd-expr-tests
  (assert-equalp
    (sexpr->expr '(ddirch-density p alpha))
    (compile::rel->pdf (sexpr->rel '(~ p (ddirch alpha)))))
  (assert-equalp
    (sexpr->expr '(dcat-density x p))
    (compile::rel->pdf (sexpr->rel '(~ x (dcat p)))))
  (assert-equalp
    (sexpr->expr '(dnorm-density (@ y i) mu sigma))
    (compile::rel->pdf (sexpr->rel '(~ (@ y i) (dnorm mu sigma)))))
  (assert-equalp
    (sexpr->expr '(dgamma-density (@ v i j) a b))
    (compile::rel->pdf (sexpr->rel '(~ (@ v i j) (dgamma a b)))))
  (assert-equalp
    (sexpr->expr '(dmvnorm-density (@ x i :all (:range 1 k)) m s))
    (compile::rel->pdf
      (sexpr->rel
        '(~ (@ x i :all (:range 1 k)) (dmvnorm m s)))))
  (assert-equalp
    (sexpr->expr '(dwishart-density (@ y i :all :all) nu V))
    (compile::rel->pdf
      (sexpr->rel
        '(~ (@ y i :all :all) (dwishart nu V)))))
  (assert-equalp
    (sexpr->expr '(dinterval-density (@ s i) (@ x i) c))
    (compile::rel->pdf
      (sexpr->rel
        '(~ (@ s i) (dinterval (@ x i) c)))))
  (assert-equalp
    (sexpr->expr '(* (dnorm-density x m s) (dnorm-density y mm ss)))
    (compile::rel->pdf
      (sexpr->rel
        '(:block
	   (~ x (dnorm m s))
	   (~ y (dnorm mm ss))))))
  (assert-equalp
    (sexpr->expr '(:quant qprod i (m n) (dnorm-density (@ x i) m s)))
    (compile::rel->pdf
      (sexpr->rel '(:for i (m n) (~ (@ x i) (dnorm m s))))))
  (assert-equalp
    (sexpr->expr '(if-then-else (< j k)
				(dnorm-density x m s) (dgamma-density y a b)))
    (compile::rel->pdf
      (sexpr->rel '(:if (< j k) (~ x (dnorm m s)) (~ y (dgamma a b))))))
  (assert-equalp
    (sexpr->expr '(if-then-else (< 0 x) (dcat-density x p) 1))
    (compile::rel->pdf (sexpr->rel '(:if (< 0 x) (~ x (dcat p))))))
  (assert-equalp
    (sexpr->expr '(:let (sigma (^ a 2)) (dnorm-density x 0 sigma)))
    (compile::rel->pdf (sexpr->rel '(:let (sigma (^ a 2))
				      (~ x (dnorm 0 sigma))))))
  (assert-equalp
    (expr-const 1)
    (compile::rel->pdf (make-relation-skip)))
)

; TODO: write and test code to verify DAG
; TODO: test for case when model language name and C# name for function
;   are different, or where there is no C# operator corresponding to
;   a binary operator in the model language
; TODO: Extend loading of model arguments to allow use of defaults.
; TODO: Extend loading of model arguments so that some integer parameters
;   can be obtained from the dimensions of the data.
