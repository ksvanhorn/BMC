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

(define-test compile-test-update-is-dag-tests
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

  (flet ((dim-fct (var)
	   (case var
	     ('x0 0)
	     ('x1 1)
	     ('x2 2))))
    (assert-equalp
"Assert.IsFalse(_assigned_X0, \"X0 assigned\");
_assigned_X0 = true;
"
      (ppstr (compile::write-assigned-test (sexpr->rellhs 'x0) #'dim-fct)))

    (assert-equalp
"for (int _idx = 0; _idx < _assigned_X1.Length; ++_idx) {
    Assert.IsFalse(_assigned_X1[_idx], \"X1[{0}] assigned\", _idx);
    _assigned_X1[_idx] = true;
}
"
      (ppstr (compile::write-assigned-test (sexpr->rellhs 'x1) #'dim-fct)))

    (assert-equalp
"for (int _idx1 = 0; _idx1 < _assigned_X2.NBRows(); ++_idx1) {
    for (int _idx2 = 0; _idx2 < _assigned_X2.NBCols(); ++_idx2) {
        Assert.IsFalse(_assigned_X2[_idx1, _idx2], \"X2[{0},{1}] assigned\", _idx1, _idx2);
        _assigned_X2[_idx1, _idx2] = true;
    }
}
"
      (ppstr (compile::write-assigned-test (sexpr->rellhs 'x2) #'dim-fct)))

    (assert-equalp
"Assert.IsFalse(_assigned_X1[K - 1], \"X1[{0}] assigned\", K - 1);
_assigned_X1[K - 1] = true;
"
      (ppstr
       (compile::write-assigned-test (sexpr->rellhs '(@ x1 k)) #'dim-fct)))

    (assert-equalp
"Assert.IsFalse(_assigned_X2[J - 1, K - 1], \"X2[{0}, {1}] assigned\", J - 1, K - 1);
_assigned_X2[J - 1, K - 1] = true;
"
      (ppstr
       (compile::write-assigned-test (sexpr->rellhs '(@ x2 j k)) #'dim-fct)))
) ; flet

  ;*** HERE ***
  ;For each update, we need to write out a specific DAG test
  ;that begins by allocating all the required _assigned_* variables
  ;then does the draw for the update with the addtional write-assigned-tests
  ;included.
#|
  ;; write-test-update-rel-is-dag
  (assert-equal
    "BMC.DrawDirichlet(p, alpha_p);
    for (int _idx = 0; _idx < _assigned_p.Length; ++_idx) {
        Assert.IsFalse(_assigned_p[_idx], \"p[{0}]\", _idx);
        _assigned_p[_idx] = true;
    }
"
    (ppstr (compile::write-test-update-rel-is-dag
	    (sexpr->rel '(~ |p| (ddirch |alpha_p|))))))
|#

#|
  ;; write-test-update-is-dag
  (assert-equalp
    ?
    (ppstr
      (compile::write-test-update-is-dag
        'foo '(x) (sexpr->rel '(~ x (dnorm m s))))))
|#
)

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
            TestUpdate_ALPHA(x, tol);
            TestUpdate_BETA(x, tol);
        }

        // Implement TestUpdate_ALPHA
        // Implement TestUpdate_BETA
    }
}
"
    (ppstr (compile::write-test-updates
	     (lambda (x) (fmt "// Implement TestUpdate_~a" x))
	     "MyModel"
	     '(alpha beta))))

#|
"
        public static void TestUpdate_ALPHA(MyModel x, double tol)
        {
            x = x.Copy();
            x.Draw();
            MyModel x1 = x.Copy();
            x1.Update_ALPHA();
            double ljd_diff = x1.LogJointDensity() - x.LogJointDensity();
            double ldd_diff = LogDrawDensity_ALPHA(x1) - LogDrawDensity_ALPHA(x);
            Assert.AreEqual(0.0, ldd_diff - ljd_diff, tol, \"ALPHA\");
        }

        private static double LogDrawDensity_ALPHA(MyModel x)
        {
            // Implement ldd ALPHA
        }

        public static void TestUpdate_BETA(MyModel x, double tol)
        {
            x = x.Copy();
            x.Draw();
            MyModel x1 = x.Copy();
            x1.Update_BETA();
            double ljd_diff = x1.LogJointDensity() - x.LogJointDensity();
            double ldd_diff = LogDrawDensity_BETA(x1) - LogDrawDensity_BETA(x);
            Assert.AreEqual(0.0, ldd_diff - ljd_diff, tol, \"BETA\");
        }

        private static double LogDrawDensity_BETA(MyModel x)
        {
            // Implement ldd BETA
        }
"
|#

  (assert-equal
"double ldd = 0.0;
var foo = x[i - 1];
ldd += BMC.LogDensityNorm(y[i - 1], foo, s);
ldd += BMC.LogDensityGamma(z[i - 1, j - 1], a, b);
return ldd;
"
    (let ((alpha-rel
	   (sexpr->rel '(~ x (dnorm m s))))
	  (beta-rel
	   (sexpr->rel '(:let (|foo| (@ |x| |i|))
			(:block
			  (~ (@ |y| |i|) (dnorm |foo| |s|))
			  (~ (@ |z| |i| |j|) (dgamma |a| |b|))))))
	  (gamma-rel
	   (sexpr->rel '(~ p (ddirch alpha_p)))))
      (ppstr (compile::write-log-draw-density-body
	      'beta
	      `((alpha . ,alpha-rel)
		(beta . ,beta-rel)
		(gamma . ,gamma-rel))))))
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
	     "Foo" "Bar" (lambda () (fmt "<class body>")))))

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
	    (sexpr->rel '(~ |p| (ddirch |alpha_p|))))))

  (assert-equal
    "x[i - 1] = BMC.DrawCat(p);
"
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(~ (@ |x| |i|) (dcat |p|))))))

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
if (!Accept(_laf1 - _laf0)) {
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
if (!Accept(_laf1 - _laf0)) {
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
    if (!Accept(_laf1 - _laf0)) {
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
    (ppstr (compile::write-prior-draw (lambda () (fmt "<body>")))))
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

