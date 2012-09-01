(use-package :compile)

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
    '(* + .< .AND .IS-INTEGER < @ A AA ALPHA B BB BOOLEAN DNORM EXP FOO I II
      INTEGER K M N NMAX NMIN REAL REALP TRUE UPPER-X V W X Z ZZ)
    (sort-unique
      (compile::symbols-in-model
        (sexpr->model '(:model
			 (:args (x real)
				(n integer))
			 (:reqs (< n nmax)
				(< nmin n))
			 (:vars (v (real n))
				(w (realp n))
				(z integer)
			        (alpha boolean))
			 (:body
			   (:for i (m k)
			     (:block
			       (~ (@ v i) (dnorm a b))
			       (<- (@ w i) (exp (@ v i)))))
			   (:for ii (1 1) (<- alpha true))
			   (:if (and (< x upper-x) (is-integer (@ v 1)))
			      (<- z (+ zz 8)))
			   (:if (< 0 foo)
			     (<- z (+ 3 aa))
			     (<- z (* 4 bb)))))))))


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
  (assert-equal "Math.Sqrt(x)"
		(cexpr->string (sexpr->expr '(sqrt |x|))))
  (assert-equal "BMC.MatrixInverse(x)"
		(cexpr->string (sexpr->expr '(inv |x|))))
  (assert-equal "BMC.ArraySlice(x, i, BMC.Range(lo, hi), BMC.FullRange)"
		(cexpr->string
		  (sexpr->expr '(@ |x| |i| (:range |lo| |hi|) :all))))
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
)

(defun strcat-lines (&rest args)
  (format nil "~{~a~%~}" args))

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
    "ljd += BMC.LogDensityNorm(x[i], MU, SIGMA);
"
    (ppstr (compile::write-ljd-accum-rel "ljd"
	     (sexpr->rel '(~ (@ |x| |i|) (dnorm mu sigma))))))

  (assert-equal
    "ll += BMC.LogDensityGamma(x[i], A * C, B / D);
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
    "sigma = 1 / Math.Sqrt(lambda);
"
    (ppstr (compile::write-ljd-accum-rel "lp"
	    (sexpr->rel '(<- |sigma| (/ 1 (sqrt |lambda|)))))))

  (assert-equal
    (strcat-lines "lp += BMC.LogDensityCat(Y, P);"
		  "X = Y;")
    (ppstr (compile::write-ljd-accum-rel "lp"
	     (sexpr->rel '(:block (~ y (dcat p)) (<- x y))))))

  (assert-equal
    (strcat-lines
	    "if (X[I] == 1) {"
	    "    acc += BMC.LogDensityNorm(Y[I], A, B);"
            "}")
    (ppstr (compile::write-ljd-accum-rel "acc"
	     (sexpr->rel '(:if (= (@ x i) 1) (~ (@ y i) (dnorm a b)))))))

  (assert-equal
    (strcat-lines
	    "if (V[I] < 4) {"
	    "    acc += BMC.LogDensityNorm(Z[I], M, S);"
            "}"
	    "else {"
	    "    acc += BMC.LogDensityCat(W[I], P);"
	    "}")
    (ppstr (compile::write-ljd-accum-rel "acc"
	     (sexpr->rel '(:if (< (@ v i) 4)
			    (~ (@ z i) (dnorm m s))
			    (~ (@ w i) (dcat p)))))))

  (assert-equal
    (strcat-lines
      "for (int i = M - 1; i <= (N + 2); ++i) {"
      "    X[i] = Math.Sqrt(Y[i]);"
      "}")
    (ppstr (compile::write-ljd-accum-rel "lp"
	     (sexpr->rel '(:for |i| ((- m 1) (+ n 2))
                            (<- (@ x |i|) (sqrt (@ y |i|))))))))

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
    for (int i = 1; i <= (N); ++i) {
        ljd1 += BMC.LogDensityCat(X[i], P);
    }
    ljd1 += BMC.LogDensityGamma(LAMBDA, A, B);
    SIGMA = 1 / Math.Sqrt(LAMBDA);
    for (int i = 1; i <= (N); ++i) {
        if (X[i] == 1) {
            ljd1 += BMC.LogDensityNorm(Y[i], 0, SIGMA);
        }
        else {
            ljd1 += BMC.LogDensityGamma(Y[i], B, A);
        }
    }
    return (Double.IsNaN(ljd1) ? Double.NegativeInfinity : ljd1);
}
"
    (ppstr
      (compile::write-csharp-log-joint-density
        (sexpr->model
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
	     (<- sigma (/ 1 (sqrt lambda)))
	     (:for |i| (1 n)
	       (:if (= (@ x |i|) 1)
		 (~ (@ y |i|) (dnorm 0 sigma))
		 (~ (@ y |i|) (dgamma b a))))))))))

; updating deterministic vars
)

; TODO!!!: test that indices in array expressions have 1 subtracted when
;   expr->string called for generating C# code.
; TODO: write code that verifies that args, vars not used before defined.
; TODO: write and test code to verify DAG
; TODO: test to verify correct print options used in write-csharp-class
; TODO: test for case when model language name and C# name for function
;   are different, or where there is no C# operator corresponding to
;   a binary operator in the model language
; TODO: Extend loading of model arguments to allow use of defaults.
; TODO: Extend loading of model arguments so that some integer parameters
;   can be obtained from the dimensions of the data.

