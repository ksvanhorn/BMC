(defpackage :compile-tests
  (:use :cl :lisp-unit :compile :mcimpl :model :expr :symbols
	:utils :testing-utilities))
(in-package :compile-tests)

(defun sexpr->decls (decls) (mapcar #'model:sexpr->decl decls))
(defun sexpr->exprs (se) (mapcar #'sexpr->expr se))
(defun sexpr->rels (se) (mapcar #'sexpr->rel se))

(defun sort-unique (symbols)
  (remove-duplicates (sort (copy-list symbols) #'string<)))

(define-test compile-util-tests
  (assert-equal
    '(a b c) (sort-unique '(b c b b a c c a)))

  (assert-equal
    '(i4 k m n x)
    (sort-unique
      (compile::vars-in-expr-list
        (sexpr->exprs
	 '((* (/ x 4) (+ k 3) (- m n))
	   (:quant qand i4 (1 n) (< i4 k))
	   (^1/2 (^ x 6))
	   false)))))

  (assert-equal
    '(a c x y z)
    (sort-unique
      (compile::vars-in-expr
        (sexpr->expr '(+ a (:let (x (* y z)) (^ x c)))))))

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
    (sexpr->expr '(:quant qand i (1 (+ n 2))
			(:quant qand j (1 (- m 3)) (is-real (@ x i j)))))
    (compile::array-element-check
      (sexpr->expr '(is-real (@ x i j)))
      (sexpr->exprs '((+ n 2) (- m 3)))
      '(i j)))

  (assert-equalp
    (sexpr->expr '(:quant qand i (1 (+ n 2)) (< 0 (@ x i))))
    (compile::array-element-check
      (sexpr->expr '(< 0 (@ x i)))
      (sexpr->exprs '((+ n 2)))
      '(i)))

  (assert-equalp
    '()
    (compile::scalar-type-checks (expr-var 'a) 'integer))

  (assert-equalp
    '()
    (compile::scalar-type-checks (sexpr->expr '(+ 3 n)) 'realxn))

  (assert-equalp
    (list (sexpr->expr '(<= 0 (@ x i))))
    (compile::scalar-type-checks (sexpr->expr '(@ x i)) 'integerp0))

  (assert-equalp
    (list (sexpr->expr '(< 0 a)))
    (compile::scalar-type-checks (expr-var 'a) 'integerp))

  (assert-equalp
    (list (sexpr->expr '(is-real x)))
    (compile::scalar-type-checks (expr-var 'x) 'real))

  (assert-equalp
    (list (sexpr->expr '(:quant qand |i1| (1 n) (< 0 (@ x |i1|)))))
    (compile::array-element-checks 'x 'integerp (sexpr->exprs '(n))))

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
    (assert-equal 0 (funcall dim-fct 'k)))
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
  (assert-equal "BMC.Sum(M, N, (I => X[I - 1]))"
    (cexpr->string (sexpr->expr '(:quant qsum i (m n) (@ x i)))))
  (assert-equal "BMC.Sum(M, N, (I => W[I - 1] < X[I - 1]), (I => Y[I - 1]))"
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
  (let* ((rel 'rel-placeholder)
	 (model-vars '(bing bang))
	 (write-body (fn (rel1)
		       (assert-eq rel rel1)
		       (assert-equal "FOO" (cvar2str 'foo))
		       (assert-equal "_x.BING" (cvar2str 'bing))
		       (assert-equal "_x.BANG" (cvar2str 'bang))
		       (fmt "// ..."))))
    (assert-equal
"private static void TestAcceptanceRatio_PHI(TheClass _x)
{
    // ...
}
"
      (ppstr
       (compile::write-test-acceptance-ratio
	 "TheClass" 'phi rel model-vars write-body))))

  (assert-equal
"double _laf0 = SIGMA * (X - MU);
var _save_X = BMC.Copy(X);
double _ljd0 = _x.LogJointDensity();
double _ldd0 = BMC.LogDensityNorm(X, MU, SIGMA);

X = BMC.DrawNorm(MU, SIGMA);

double _laf1 = SIGMA * (X - MU);
double _ljd1 = _x.LogJointDensity();
double _ldd1 = BMC.LogDensityNorm(X, MU, SIGMA);
Assert.AreEqual(_ljd1 + _ldd1


if (!BMC.Accept(_laf1 - _laf0)) {
    X = _save_X;
}
"
    (ppstr
     (compile::write-test-acceptance-ratio-body
      (sexpr->rel '(:metropolis-hastings
		    :lets ()
		    :proposal-distribution (~ x (dnorm mu sigma))
		    :log-acceptance-factor (* sigma (- x mu)))))))
)

(defun cvar2str (s) (compile::variable->string s))

(defmacro in-test-outer-lets-env (model-vars &rest body)
  `(let ((compile::*variable->string* (compile::var2str-ext ,model-vars))
	 (compile::*write-rel-draw-visitor #'compile::default-wrd-visitor))
     ,@body))

(define-test compile-test-outer-lets-tests
  (let* ((rel 'rel-placeholder)
	 (model-vars '(foo bar))
	 (write-body (fn (rel1 rev-lets)
		       (assert-eq rel rel1)
		       (assert-eq '() rev-lets)
		       (assert-equal "A" (cvar2str 'a))
		       (assert-equal "_x.FOO" (cvar2str 'foo))
		       (assert-equal "_x.BAR" (cvar2str 'bar))
		       (assert-equal "B" (cvar2str 'b))
		       (fmt "// ..."))))
  (assert-equal
"private static void TestOuterLetsAreInvariant_THETA(PGM _x)
{
    // ...
}
"
    (ppstr
     (compile::write-test-outer-lets "PGM" 'theta rel model-vars write-body))))

  (assert-equal
"_x.Z = BMC.DrawCat(Q);
"
    (ppstr
      (in-test-outer-lets-env '(z u)
        (compile::write-test-outer-lets-body
          (sexpr->rel
            '(:metropolis-hastings
	      :lets ()
	      :proposal-distribution (~ z (dcat q))
	      :log-acceptance-factor %undef))
	  `()))))

  (assert-equal
"var X = A + _x.Z;
_x.Z = BMC.DrawCat(Q);
_x.Y = BMC.DrawNorm(A, _x.B);
Assert.IsTrue(BMC.Equal(A, _x.FOO - BAR), \"A should not change\");
Assert.IsTrue(BMC.Equal(Q, BMC.Vec(_x.U, V)), \"Q should not change\");
"
    (ppstr
      (in-test-outer-lets-env '(z u y b foo)
        (compile::write-test-outer-lets-body
          (sexpr->rel
            '(:metropolis-hastings
	      :lets ((x (+ a z)))
	      :proposal-distribution (:block (~ z (dcat q)) (~ y (dnorm a b)))
	      :log-acceptance-factor %undef))
	  `((q . ,(sexpr->expr '(vec u v)))
	    (a . ,(sexpr->expr '(- foo bar))))))))

  (assert-equal
"var A = U + V;
var B = U * V;
int _lo_R = LO;
int _hi_R = _x.HI;
for (int R = _lo_R; R <= _hi_R; ++R) {
    var C = _x.P[R - 1];
    var D = Math.Pow(B, A);
    _x.W = BMC.DrawGamma(C * D, B);
    Assert.IsTrue(BMC.Equal(V, BMC.Sqr(_x.VV)), \"V should not change\");
    Assert.IsTrue(BMC.Equal(U, Math.Exp(_x.UU)), \"U should not change\");
    Assert.IsTrue(BMC.Equal(A, U + V), \"A should not change\");
    Assert.IsTrue(BMC.Equal(B, U * V), \"B should not change\");
    Assert.IsTrue(BMC.Equal(_lo_R, LO), \"_lo_R should not change\");
    Assert.IsTrue(BMC.Equal(_hi_R, _x.HI), \"_hi_R should not change\");
    Assert.IsTrue(BMC.Equal(C, _x.P[R - 1]), \"C should not change\");
}
"
    (ppstr
      (in-test-outer-lets-env '(w uu vv p hi)
        (compile::write-test-outer-lets-body
	  (sexpr->rel
	    '(:let (a (+ u v))
	     (:let (b (* u v))
             (:for r (lo hi)
               (:let (c (@ p r))
                 (:metropolis-hastings
                   :lets ((d (^ b a)))
		   :proposal-distribution (~ w (dgamma (* c d) b))
		   :log-acceptance-factor %undef))))))
	  `((u . ,(sexpr->expr '(exp uu)))
	    (v . ,(sexpr->expr '(^2 vv))))))))
)

(define-test compile-test-is-dag-update-tests
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
		    :log-acceptance-factor (* (- y y0) s)))))

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

  ;; Outer part
  (assert-equal
"private static void TestIsDAG_Update_FOO(Mdl _x)
{
    bool _assigned_X = false;
    bool[] _assigned_Y = new bool[_x.Y.Length];
    BMatrix _assigned_Z = new BMatrix(_x.Z.NBRows, _x.Z.NBCols);
    // ...
}
"
    (let* ((rel 'rel-stub)
	   (model-vars 'model-vars-stub)
	   (assigned-vars '(x y z))
	   (dim-fct (fn (v) (case v ('a 0) ('x 0) ('y 1) ('z 2))))
	   (write-body (fn (rel1 mv1 dimfct1)
			 (assert-eq rel rel1)
			 (assert-eq model-vars mv1)
			 (assert-eq dim-fct dimfct1)
			 (fmt "// ..."))))
      (ppstr
        (compile::write-test-is-dag-update
	  "Mdl" 'foo rel model-vars assigned-vars dim-fct write-body))))

  ;; write-rel-draw-with-dag-test
  (flet ((wrd-with-dag-test (se model-vars dims-assoc)
           (ppstr
	     (compile::write-rel-draw-with-dag-test
	       (sexpr->rel se)
	       model-vars
	       (fn(v) (assoc-lookup v dims-assoc))))))

    (assert-equal
"for (int _idx = 0; _idx < _assigned_p.Length; ++_idx) {
    Assert.IsFalse(_assigned_p[_idx], \"p[{0}] assigned\", _idx);
    _assigned_p[_idx] = true;
}
BMC.DrawDirichlet(_x.p, _x.alpha_p);
"
      (wrd-with-dag-test
        '(~ |p| (ddirch |alpha_p|))
	'(|p| |alpha_p|)
	'((|p| . 1))))

    (assert-equal
"Assert.IsFalse(_assigned_x[i - 1], \"x[{0}] assigned\", i - 1);
_assigned_x[i - 1] = true;
_x.x[i - 1] = BMC.DrawCat(_x.p);
"
      (wrd-with-dag-test
	'(~ (@ |x| |i|) (dcat |p|))
	'(|p| |x|)
	'((|x| . 1))))

  (assert-equal
"Assert.IsFalse(_assigned_Y[i - 1, j - 1], \"Y[{0}, {1}] assigned\", i - 1, j - 1);
_assigned_Y[i - 1, j - 1] = true;
_x.Y[i - 1, j - 1] = BMC.DrawNorm(_x.MU, SIGMA);
"
    (wrd-with-dag-test
      '(~ (@ y |i| |j|) (dnorm mu sigma))
      '(y mu)
      '((y . 2))))

  (assert-equal
"Assert.IsFalse(_assigned_X, \"X assigned\");
_assigned_X = true;
_x.X = BMC.DrawGamma(_x.A * C, B / _x.D);
"
    (wrd-with-dag-test
      '(~ x (dgamma (* a c) (/ b d)))
      '(x a d)
      '((x . 0))))

   (assert-equal
"for (int _idx = 0; _idx < _assigned_V.Length; ++_idx) {
    Assert.IsFalse(_assigned_V[_idx], \"V[{0}] assigned\", _idx);
    _assigned_V[_idx] = true;
}
BMC.DrawMVNorm(_x.V, MU, _x.SIGMA);
"
    (wrd-with-dag-test
      '(~ v (dmvnorm mu sigma))
      '(v sigma)
      '((v . 1))))

  (assert-equal
"for (int _idx1 = 0; _idx1 < _assigned_Lambda.NBRows; ++_idx1) {
    for (int _idx2 = 0; _idx2 < _assigned_Lambda.NBCols; ++_idx2) {
        Assert.IsFalse(_assigned_Lambda[_idx1, _idx2], \"Lambda[{0}, {1}] assigned\", _idx1, _idx2);
        _assigned_Lambda[_idx1, _idx2] = true;
    }
}
BMC.DrawWishart(_x.Lambda, _x.nu + 3, V);
"
    (wrd-with-dag-test
      '(~ |Lambda| (dwishart (+ |nu| 3) V))
      '(|Lambda| |nu|)
      '((|Lambda| . 2))))

  (assert-equal
"Assert.IsFalse(_assigned_v[j - 1], \"v[{0}] assigned\", j - 1);
_assigned_v[j - 1] = true;
_x.v[j - 1] = BMC.DrawInterval(_x.u, c);
"
    (wrd-with-dag-test
      '(~ (@ |v| |j|) (dinterval |u| |c|))
      '(|v| |u|)
      '((|v| . 1))))

  (assert-equal
"var sigma = _x.alpha / Math.Sqrt(lambda);
Assert.IsFalse(_assigned_y, \"y assigned\");
_assigned_y = true;
_x.y = BMC.DrawNorm(0, sigma);
"
    (wrd-with-dag-test
      '(:let (|sigma| (/ |alpha| (^1/2 |lambda|)))
	     (~ |y| (dnorm 0 |sigma|)))
      '(|y| |alpha|)
      '((|y| . 0))))

  (assert-equal
"{
    var PVEC = BMC.ArrPlus(_x.FOO, BAR);
    Assert.IsFalse(_assigned_Y, \"Y assigned\");
    _assigned_Y = true;
    _x.Y = BMC.DrawCat(PVEC);
}
Assert.IsFalse(_assigned_Z, \"Z assigned\");
_assigned_Z = true;
_x.Z = BMC.DrawGamma(A, _x.B);
"
    (wrd-with-dag-test
      '(:block
	 (:let (pvec (@+ foo bar))
	   (~ y (dcat pvec)))
	 (~ z (dgamma a b)))
      '(y z foo b)
      '((z . 0) (y . 0))))

  (assert-equal
"if (_x.V[I - 1] < 4) {
    Assert.IsFalse(_assigned_Z[I - 1], \"Z[{0}] assigned\", I - 1);
    _assigned_Z[I - 1] = true;
    _x.Z[I - 1] = BMC.DrawNorm(_x.M, S);
}
else {
    Assert.IsFalse(_assigned_W[I + _x.K - 1], \"W[{0}] assigned\", I + _x.K - 1);
    _assigned_W[I + _x.K - 1] = true;
    _x.W[I + _x.K - 1] = BMC.DrawCat(_x.Q);
}
"
    (wrd-with-dag-test
      '(:if (< (@ v i) 4)
	 (~ (@ z i) (dnorm m s))
	 (~ (@ w (+ i k)) (dcat q)))
      '(z w v m q k)
      '((w . 1) (z . 1))))

  (assert-equal
"for (int i = M - 1; i <= _x.N + 2; ++i) {
    Assert.IsFalse(_assigned_Z[i - 1], \"Z[{0}] assigned\", i - 1);
    _assigned_Z[i - 1] = true;
    _x.Z[i - 1] = BMC.DrawNorm(0, Math.Sqrt(_x.Y[i - 1]));
}
"
    (wrd-with-dag-test
      '(:for |i| ((- m 1) (+ n 2))
	 (~ (@ z |i|) (dnorm 0 (^1/2 (@ y |i|)))))
      '(z n y)
      '((z . 1))))

  (assert-equal
"if (_x.V < SOME_THRESH) {
    Assert.IsFalse(_assigned_A, \"A assigned\");
    _assigned_A = true;
    _x.A = BMC.DrawGamma(M, _x.S);
}
"
    (wrd-with-dag-test
      '(:if (< v some_thresh) (~ a (dgamma m s)))
      '(a s v)
      '((a . 0))))

  (assert-equal
"double _laf0 = _x.SIGMA * (_x.U - MU);
var _save_U = BMC.Copy(_x.U);

Assert.IsFalse(_assigned_U, \"U assigned\");
_assigned_U = true;
_x.U = BMC.DrawNorm(MU, _x.SIGMA);

double _laf1 = _x.SIGMA * (_x.U - MU);
if (!BMC.Accept(_laf1 - _laf0)) {
    _x.U = _save_U;
}
"
    (wrd-with-dag-test
     '(:metropolis-hastings
        :lets ()
        :proposal-distribution (~ u (dnorm mu sigma))
        :log-acceptance-factor (* sigma (- u mu)))
     '(u sigma)
     '((u . 0))))

  (assert-equal
"var MU = 2 * _x.M;
double _laf0 = _x.SIGMA * (_x.Z - MU);
var _save_Z = BMC.Copy(_x.Z);

Assert.IsFalse(_assigned_Z, \"Z assigned\");
_assigned_Z = true;
_x.Z = BMC.DrawNorm(MU, _x.SIGMA);

MU = 2 * _x.M;
double _laf1 = _x.SIGMA * (_x.Z - MU);
if (!BMC.Accept(_laf1 - _laf0)) {
    _x.Z = _save_Z;
}
"
   (wrd-with-dag-test
     '(:metropolis-hastings
        :lets ((mu (* 2 m)))
	:proposal-distribution (~ z (dnorm mu sigma))
	:log-acceptance-factor (* sigma (- z mu)))
     '(z m sigma)
     '((z . 0))))

  (assert-equal
"for (int I = _x.M; I <= N; ++I) {
    var MU = BMC.Dot(_x.Y, _x.GAMMA);
    var SIGMA = Math.Exp(U * _x.V[I - 1]);
    var F = _x.Z[I - 1];
    var SIGMA2 = F * F;
    double _laf0 = _x.Z[I - 1] + SIGMA2;
    var _save_Z_lbI_rb = BMC.Copy(_x.Z[I - 1]);
    var _save_W = BMC.Copy(_x.W);

    Assert.IsFalse(_assigned_Z[I - 1], \"Z[{0}] assigned\", I - 1);
    _assigned_Z[I - 1] = true;
    _x.Z[I - 1] = BMC.DrawNormTruncated(MU, SIGMA, _x.A, _x.B);
    Assert.IsFalse(_assigned_W, \"W assigned\");
    _assigned_W = true;
    _x.W = BMC.DrawNorm(_x.Z[I - 1], 1);

    F = _x.Z[I - 1];
    SIGMA2 = F * F;
    double _laf1 = _x.Z[I - 1] + SIGMA2;
    if (!BMC.Accept(_laf1 - _laf0)) {
        _x.Z[I - 1] = _save_Z_lbI_rb;
        _x.W = _save_W;
    }
}
"
    (wrd-with-dag-test
      '(:for i (m n)
	 (:let (mu (dot y gamma))
	   (:let (sigma (exp (* u (@ v i))))
	     (:metropolis-hastings
	       :lets ((f (@ z i)) (sigma2 (* f f)))
	       :proposal-distribution
	         (:block
		    (~ (@ z i) (dnorm-trunc mu sigma a b))
		    (~ w (dnorm (@ z i) 1)))
	       :log-acceptance-factor (+ (@ z i) sigma2)))))
      '(z m y gamma v a b w)
      '((z . 1) (w . 0))))

))

(define-test compile-test-updates-tests
  (assert-equal
"Using System;
Using NUnit.Framework;
Using Estimation;
Using Common;

namespace Tests
{
    static class TestMyModelUpdates
    {
        public static void TestAllUpdates(MyModel x, double tol)
        {
            TestIsDAG_Update_ALPHA(x);
            TestIsDAG_Update_BETA(x);

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
    x = x.Copy();
    x.Draw();
    MyModel x1 = x.Copy();
    x1.Update_ALPHA();
    double ljd_diff = x1.LogJointDensity() - x.LogJointDensity();
    double ldd_diff = LogDrawDensity_ALPHA(x1) - LogDrawDensity_ALPHA(x);
    Assert.AreEqual(0.0, ldd_diff - ljd_diff, tol, \"ALPHA\");
}
"
    (ppstr (compile::write-test-gibbs-update "MyModel" 'alpha)))

  (assert-equal
"public static void TestUpdate_BETA(Mdl x, double tol)
{
    x = x.Copy();
    x.Draw();
    Mdl x1 = x.Copy();
    x1.Update_BETA();
    double ljd_diff = x1.LogJointDensity() - x.LogJointDensity();
    double ldd_diff = LogDrawDensity_BETA(x1) - LogDrawDensity_BETA(x);
    Assert.AreEqual(0.0, ldd_diff - ljd_diff, tol, \"BETA\");
}
"
    (ppstr (compile::write-test-gibbs-update "Mdl" 'beta)))

  (assert-equal
"private static double LogDrawDensity_GAMMA(MyMuddle _x)
{
    double _ldd = 0.0;
    // ...
    return _ldd;
}
"
    (let* ((rel (sexpr->rel '(~ a (dnorm m s))))
	   (model-vars '(m c))
	   (write-body 	(fn (rel1 mv1)
			  (assert-equalp rel rel1)
			  (assert-equalp model-vars mv1)
			  (fmt "// ..."))))
      (ppstr
       (compile::write-log-draw-density-of-update
	 "MyMuddle" 'gamma rel model-vars write-body))))

  (assert-equal
"public static void TestUpdate_ALPHA(MyModel x, double tol)
{
    TestOuterLetsAreInvariant_ALPHA(x);
    TestAcceptanceRatio_ALPHA(x, tol);
}
"
    (ppstr (compile::write-test-mh-update "MyModel" 'alpha)))

  (assert-equal
"public static void TestUpdate_BETA(Mdl x, double tol)
{
    TestOuterLetsAreInvariant_BETA(x);
    TestAcceptanceRatio_BETA(x, tol);
}
"
    (ppstr (compile::write-test-mh-update "Mdl" 'beta)))

  (let ((rel (sexpr->rel
	       '(:let (foo (+ a b))
		(:for k (em enn)
		  (:if (= (@ bar k) baz)
		    (:block
		      (~ (@ x k) (dnorm m s))
		      (~ (@ z k) (dgamma (exp x) 1)))))))))
    (assert-equal 't (compile::is-gibbs-update rel))
    (assert-equal nil (compile::is-mh-update rel)))

  (let ((rel (sexpr->rel
	       '(:for j (lo hi)
		  (:let (bar (@ z j))
		    (:if (< (@ m j) thresh)
		      (:metropolis-hastings
		       :lets ((baz (@ foo j j)))
		       :proposal-distribution (~ (@ x j) (dnorm (@ x j) sigma))
		       :log-acceptance-factor (* baz (exp (@ x j))))))))))
    (assert-equal nil (compile::is-gibbs-update rel))
    (assert-equal 't (compile::is-mh-update rel)))

  (let ((rel (sexpr->rel
	       '(:block
		  (~ x (dnorm m s))
		  (:metropolis-hastings
		   :lets ()
		   :proposal-distribution (~ y (dgamma a b))
		   :log-acceptance-factor (log y))))))
    (assert-equal nil (compile::is-gibbs-update rel))
    (assert-equal nil (compile::is-mh-update rel)))

  (let ((rel (sexpr->rel
	       '(:metropolis-hastings
		 :lets ()
		 :proposal-distribution
		   (:metropolis-hastings
		    :lets ()
		    :proposal-distribution (~ y (dgamma a b))
		    :log-acceptance-factor (* u v))
		 :log-acceptance-factor (log y)))))
    (assert-equal nil (compile::is-gibbs-update rel))
    (assert-equal nil (compile::is-mh-update rel)))
)

(define-test write-body-ldd-of-update-tests
  (assert-equal
    "_ldd += BMC.LogDensityDirichlet(_x.p, _x.alpha_p);
"
    (ppstr (compile::write-body-ldd-of-update
	     (sexpr->rel '(~ |p| (ddirch |alpha_p|)))
	     '(|p| |alpha_p|))))

  (assert-equal
    "_ldd += BMC.LogDensityCat(_x.x[i - 1], _x.p);
"
    (ppstr (compile::write-body-ldd-of-update
	     (sexpr->rel '(~ (@ |x| |i|) (dcat |p|)))
	     '(|x| |p|))))

  (assert-equal
    "_ldd += BMC.LogDensityNorm(_x.x[i - 1], MU, SIGMA);
"
    (ppstr (compile::write-body-ldd-of-update
	     (sexpr->rel '(~ (@ |x| |i|) (dnorm mu sigma)))
	     '(|x|))))

  (assert-equal
    "_ldd += BMC.LogDensityGamma(_x.x[i - 1], _x.A * C, _x.B / D);
"
    (ppstr (compile::write-body-ldd-of-update
	     (sexpr->rel '(~ (@ |x| |i|) (dgamma (* a c) (/ b d))))
	     '(|x| a b))))

  (assert-equal
    "_ldd += BMC.LogDensityMVNorm(_x.x, MU, _x.SIGMA);
"
    (ppstr (compile::write-body-ldd-of-update
	     (sexpr->rel '(~ |x| (dmvnorm mu sigma)))
	     '(|x| sigma))))

  (assert-equal
    "_ldd += BMC.LogDensityWishart(_x.Lambda, _x.nu + 3, V);
"
    (ppstr (compile::write-body-ldd-of-update
	     (sexpr->rel '(~ |Lambda| (dwishart (+ |nu| 3) V)))
	     '(|Lambda| |nu|))))

  (assert-equal
    "_ldd += BMC.LogDensityInterval(_x.v, _x.y, c);
"
    (ppstr (compile::write-body-ldd-of-update
	      (sexpr->rel '(~ |v| (dinterval |y| |c|)))
	      '(|v| |y|))))

  (assert-equal
    (strcat-lines
      "var sigma = 1 / Math.Sqrt(lambda);"
      "_ldd += BMC.LogDensityNorm(_x.Z, 0, sigma);")
    (ppstr (compile::write-body-ldd-of-update
	    (sexpr->rel '(:let (|sigma| (/ 1 (^1/2 |lambda|)))
			   (~ z (dnorm 0 |sigma|))))
	    '(z))))

  (assert-equal
    (strcat-lines "_ldd += BMC.LogDensityCat(_x.Y, P);"
		  "_ldd += BMC.LogDensityNorm(_x.X, 0, _x.SIGMA[_x.Y - 1]);")
    (ppstr (compile::write-body-ldd-of-update
	     (sexpr->rel '(:block
			    (~ y (dcat p))
			    (~ x (dnorm 0 (@ sigma y)))))
	     '(y x sigma))))

  (assert-equal
    (strcat-lines
	    "if (V[I - 1] < 4) {"
	    "    _ldd += BMC.LogDensityNorm(_x.Z[I - 1], _x.M, S);"
            "}"
	    "else {"
	    "    _ldd += BMC.LogDensityCat(_x.W[I - 1], P);"
	    "}")
    (ppstr (compile::write-body-ldd-of-update
	     (sexpr->rel '(:if (< (@ v i) 4)
			    (~ (@ z i) (dnorm m s))
			    (~ (@ w i) (dcat p))))
	     '(z m w))))

  (assert-equal
    (strcat-lines
      "for (int i = M - _x.K; i <= _x.N + A; ++i) {"
      "    _ldd += BMC.LogDensityGamma(_x.W[i - 1], Math.Sqrt(Y[i - 1]), 1);"
      "}")
    (ppstr (compile::write-body-ldd-of-update
	     (sexpr->rel '(:for |i| ((- m k) (+ n a))
                            (~ (@ w |i|) (dgamma (^1/2 (@ y |i|)) 1))))
	     '(k n w))))

  (assert-equal
    ""
    (ppstr (compile::write-body-ldd-of-update (make-relation-skip) '())))
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
    a = new double[2];
    b = new DMatrix(2, 2);
    c = new double[N];
    d = new DMatrix(N, M - 1);
    f = new IMatrix(K + 1, N);
    g = new int[K];
    h = new bool[N];
    i = new BMatrix(M, K);
}
"
      (ppstr (compile::write-csharp-allocate-vars (sexpr->decls vars)))))

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
    MU = loader.LoadRealArray(\"MU\", N + 1);
    IDX = loader.LoadIntegerArray(\"IDX\", 5);
    BVEC = loader.LoadBooleanArray(\"BVEC\", N);
    Sigma = loader.LoadDMatrix(\"Sigma\", N - 1, N - 1);
    FOO = loader.LoadIMatrix(\"FOO\", N, K);
    Bar = loader.LoadBMatrix(\"Bar\", K, K * N);
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

  (flet ((save-name-se (se) (compile::save-name (sexpr->rellhs se))))
    (assert-equal "_save_x" (save-name-se '|x|))
    (assert-equal "_save_foo__bar" (save-name-se '|foo_bar|))
    (assert-equal "_save_Y_lbI_rb" (save-name-se '(@ y i)))
    (assert-equal "_save_z_lbi_cm_sp_rb" (save-name-se '(@ |z| |i| :all))))

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
"double _laf0 = SIGMA * (X - MU);
var _save_X = BMC.Copy(X);

X = BMC.DrawNorm(MU, SIGMA);

double _laf1 = SIGMA * (X - MU);
if (!BMC.Accept(_laf1 - _laf0)) {
    X = _save_X;
}
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ()
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :log-acceptance-factor (* sigma (- x mu))))
     nil)))

  (assert-equal
"var MU = 2 * M;
double _laf0 = SIGMA * (X - MU);
var _save_X = BMC.Copy(X);

X = BMC.DrawNorm(MU, SIGMA);

MU = 2 * M;
double _laf1 = SIGMA * (X - MU);
if (!BMC.Accept(_laf1 - _laf0)) {
    X = _save_X;
}
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ((mu (* 2 m)))
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :log-acceptance-factor (* sigma (- x mu))))
     nil)))

  (assert-equal
"for (int I = M; I <= N; ++I) {
    var MU = BMC.Dot(Y, GAMMA);
    var SIGMA = Math.Exp(U * V[I - 1]);
    var F = Z[I - 1];
    var SIGMA2 = F * F;
    double _laf0 = Z[I - 1] + SIGMA2;
    var _save_Z_lbI_rb = BMC.Copy(Z[I - 1]);

    Z[I - 1] = BMC.DrawNormTruncated(MU, SIGMA, A, B);

    F = Z[I - 1];
    SIGMA2 = F * F;
    double _laf1 = Z[I - 1] + SIGMA2;
    if (!BMC.Accept(_laf1 - _laf0)) {
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
		   :log-acceptance-factor (+ (@ z i) sigma2))))))))
      (ppstr (compile::write-rel-draw r))))

  (assert-equal
    ""
    (ppstr (compile::write-rel-draw (make-relation-skip))))

  (assert-equal
    (strcat-lines
      "void Draw() {"
      "    <body>"
      "}")
    (ppstr (compile::write-prior-draw (fn () (fmt "<body>")))))
)

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
