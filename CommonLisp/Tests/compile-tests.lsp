(defpackage :compile-tests
  (:use :cl :lisp-unit :compile :model :expr :symbols :utils :testing-utilities))
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
    '(* + - / < false i4 k m n qand sqrt v x ^)
    (sort-unique
      (compile::symbols-in-sym-exprs
        'v (sexpr->exprs
	     '((* (/ x 4) (+ k 3) (- m n))
	       (qand i4 (1 n) (< i4 k))
	       (sqrt (^ x 6))
	       false)))))

  (assert-equal
    '(* + a c x y z ^)
    (sort-unique
      (compile::symbols-in-expr
        (sexpr->expr '(+ a (:let (x (* y z)) (^ x c)))))))

  (assert-equal
    '(* + .< .AND .IS-INTEGER < @ A AA ALPHA B BB BOOLEAN DGAMMA DNORM
      EXP FOO I II INTEGER K M N REAL REALP TRUE UPPER-X V W X Z ZZ)
    (sort-unique
      (compile::symbols-in-model
        (sexpr->model '(:model
			 (:args (x real)
				(n integer))
			 (:reqs (< n 10)
				(< 0 n))
			 (:vars (v (real n))
				(w (realp n))
				(z integer)
			        (alpha boolean))
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
    (sexpr->expr '(qand i (1 (+ n 2))
			(qand j (1 (- m 3)) (is-real (@ x i j)))))
    (compile::array-element-check
      (sexpr->expr '(is-real (@ x i j)))
      (sexpr->exprs '((+ n 2) (- m 3)))
      '(i j)))

  (assert-equalp
    (sexpr->expr '(qand i (1 (+ n 2)) (< 0 (@ x i))))
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
    (list (sexpr->expr '(qand |i1| (1 n) (< 0 (@ x |i1|)))))
    (compile::array-element-checks 'x 'integerp (sexpr->exprs '(n))))

  (assert-equalp
    (list (sexpr->expr '(qand |i1| (1 n)
			      (qand |i2| (1 k) (is-realp0 (@ v |i1| |i2|))))))
    (compile::array-element-checks 'v 'realp0 (sexpr->exprs '(n k))))

  (assert-equalp
    (list (sexpr->expr '(qand |i2| (1 (+ n |i1|)) (is-real (@ y |i2|)))))
    (compile::array-element-checks 'y 'real (sexpr->exprs '((+ n |i1|)))))

  (assert-equalp
    (list (sexpr->expr '(qand |i1| (1 n)
			      (qand |i3| (1 k) (is-realx (@ |i2| |i1| |i3|))))))
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
		    (qand |i1| (1 (+ k 1)) (<= 0 (@ A |i1|)))))
    (compile::decl-checks (sexpr->decl '(A (integerp0 (+ k 1))))))

  (assert-equalp
    (list (sexpr->expr '(= (array-length 1 x) n)))
    (compile::decl-checks (sexpr->decl '(x (realxn n)))))

  (assert-equalp
    (sexpr->exprs '((= (array-length 1 y) m)
		    (= (array-length 2 y) (* k 3))
		    (qand |i1| (1 m)
			  (qand |i2| (1 (* k 3)) (is-real (@ y |i1| |i2|))))))
    (compile::decl-checks (sexpr->decl '(y (real m (* k 3))))))

  (assert-equalp
    (sexpr->exprs '((< 0 n)
		    (<= 0 k)
		    (= (array-length 1 v) m)
		    (qand |i1| (1 m) (is-realp0 (@ v |i1|)))
		    (= (array-length 1 w) n)
		    (= (array-length 1 x) n)
		    (= (array-length 2 x) m)
		    (qand |i1| (1 n)
			  (qand |i2| (1 m) (is-realp (@ x |i1| |i2|))))
		    (= (array-length 1 y) m)
		    (= (array-length 2 y) n)
		    (< m n)
		    (qand i (1 m) (< (@ v i) 100))
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
			     (qand i (1 m) (< (@ v i) 100)))
		      (:vars (foo integer)
			     (bar (realp n)))
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
		      (:body)))))

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
		(cexpr->string (sexpr->expr '(sqrt |x|))))
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
;  (assert-equal "(let x = y ^ 2 in c * x)"
;		(cexpr->string (sexpr->expr '(:let (|x| (^ |y| 2))
;					       (* |c| |x|)))))
)

(define-test compile-ljd-tests
  (assert-equal
"first line
  second line
third line
"
    (strcat-lines "first line" "  second line" "third line"))

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
      "{"
      "    var sigma = 1 / Math.Sqrt(lambda);"
      "    lp += BMC.LogDensityNorm(X, 0, sigma);"
      "}")
    (ppstr (compile::write-ljd-accum-rel "lp"
	    (sexpr->rel '(:let (|sigma| (/ 1 (sqrt |lambda|)))
			   (~ x (dnorm 0 |sigma|)))))))

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
                            (~ (@ x |i|) (dgamma (sqrt (@ y |i|)) 1)))))))

  (assert-equal
    ""
    (ppstr (compile::write-ljd-accum-rel "lp" (make-relation-skip))))

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
    ljd1 += BMC.LogDensityDirichlet(P, ALPHA_P);
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
    return (Double.IsNaN(ljd1) ? Double.NegativeInfinity : ljd1);
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
	   (:body
	     (~ p (ddirch alpha_p))
	     (:for |i| (1 n)
	       (~ (@ x |i|) (dcat p)))
	     (~ lambda (dgamma a b))
	     (:let (sigma (/ 1 (sqrt lambda)))
	       (:for |i| (1 n)
	         (:if (= (@ x |i|) 1)
		   (~ (@ y |i|) (dnorm 0 sigma))
		   (~ (@ y |i|) (dgamma b a)))))))))))

; updating deterministic vars
)

(define-test prior-draw-tests
  (assert-equal
    "BMC.DrawDirichlet(p, alpha_p);
"
    (ppstr (compile::write-prior-draw-rel
	    (sexpr->rel '(~ |p| (ddirch |alpha_p|))))))

  (assert-equal
    "x[i - 1] = BMC.DrawCat(p);
"
    (ppstr (compile::write-prior-draw-rel
	     (sexpr->rel '(~ (@ |x| |i|) (dcat |p|))))))

  (assert-equal
    "Y[i - 1, j - 1] = BMC.DrawNorm(MU, SIGMA);
"
    (ppstr (compile::write-prior-draw-rel
	     (sexpr->rel '(~ (@ y |i| |j|) (dnorm mu sigma))))))

  (assert-equal
    "X = BMC.DrawGamma(A * C, B / D);
"
    (ppstr (compile::write-prior-draw-rel
	     (sexpr->rel '(~ x (dgamma (* a c) (/ b d)))))))
   (assert-equal
    "BMC.DrawMVNorm(V, MU, SIGMA);
"
    (ppstr (compile::write-prior-draw-rel
	     (sexpr->rel '(~ v (dmvnorm mu sigma))))))


  (assert-equal
    "BMC.DrawWishart(Lambda, nu + 3, V);
"
    (ppstr (compile::write-prior-draw-rel
	     (sexpr->rel '(~ |Lambda| (dwishart (+ |nu| 3) V))))))


  (assert-equal
    "v[j - 1] = BMC.DrawInterval(x, c);
"
    (ppstr (compile::write-prior-draw-rel
	      (sexpr->rel '(~ (@ |v| |j|) (dinterval |x| |c|))))))

  (assert-equal
    (strcat-lines
      "{"
      "    var sigma = alpha / Math.Sqrt(lambda);"
      "    x = BMC.DrawNorm(0, sigma);"
      "}")
    (ppstr (compile::write-prior-draw-rel
	     (sexpr->rel '(:let (|sigma| (/ |alpha| (sqrt |lambda|)))
			    (~ |x| (dnorm 0 |sigma|)))))))

  (assert-equal
    (strcat-lines "Y = BMC.DrawCat(PVEC);"
		  "X = BMC.DrawGamma(1, 1);")
    (ppstr (compile::write-prior-draw-rel
	     (sexpr->rel '(:block (~ y (dcat pvec)) (~ x (dgamma 1 1)))))))

  (assert-equal
    (strcat-lines
	    "if (X[I - 1] == 1) {"
	    "    Z = BMC.DrawNorm(A, B);"
            "}")
    (ppstr (compile::write-prior-draw-rel
	     (sexpr->rel '(:if (= (@ x i) 1) (~ z (dnorm a b)))))))
 
  (assert-equal
    (strcat-lines
	    "if (V[I - 1] < 4) {"
	    "    Z[I - 1] = BMC.DrawNorm(M, S);"
            "}"
	    "else {"
	    "    W[I - 1] = BMC.DrawCat(Q);"
	    "}")
    (ppstr (compile::write-prior-draw-rel
	     (sexpr->rel '(:if (< (@ v i) 4)
			    (~ (@ z i) (dnorm m s))
			    (~ (@ w i) (dcat q)))))))

  (assert-equal
    (strcat-lines
      "for (int i = M - 1; i <= N + 2; ++i) {"
      "    X[i - 1] = BMC.DrawNorm(0, Math.Sqrt(Y[i - 1]));"
      "}")
    (ppstr (compile::write-prior-draw-rel
	     (sexpr->rel '(:for |i| ((- m 1) (+ n 2))
                            (~ (@ x |i|) (dnorm 0 (sqrt (@ y |i|)))))))))

  (assert-equal
    ""
    (ppstr (compile::write-prior-draw-rel (make-relation-skip))))

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
    (sexpr->expr '(*! (dnorm-density x m s) (dnorm-density y mm ss)))
    (compile::rel->pdf
      (sexpr->rel
        '(:block
	   (~ x (dnorm m s))
	   (~ y (dnorm mm ss))))))
  (assert-equalp
    (sexpr->expr '(qprod! i (m n) (dnorm-density (@ x i) m s)))
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
    (expr-lit 1)
    (compile::rel->pdf (make-relation-skip)))
)

; TODO: write and test code to verify DAG
; TODO: test for case when model language name and C# name for function
;   are different, or where there is no C# operator corresponding to
;   a binary operator in the model language
; TODO: Extend loading of model arguments to allow use of defaults.
; TODO: Extend loading of model arguments so that some integer parameters
;   can be obtained from the dimensions of the data.

