(use-package :compile)

(defun sexpr->decls (decls) (mapcar #'model:sexpr->decl decls))
(defun sexpr->exprs (se) (mapcar #'sexpr->expr se))
(defun sexpr->rels (se) (mapcar #'sexpr->rel se))

(define-test compile-util-tests
  (assert-equalp
    '(|i1| |i2|)
    (compile::index-var-symbols 'v (sexpr->exprs '(j (+ k 3)))))

  (assert-equalp
    '(|i1| |i3| |i6|)
    (compile::index-var-symbols
      '|i2| (sexpr->exprs '((qand |i4| (1 n) (< |i4| m))
			    (+ (* |i5| 3) n)
			    (sqrt (^ x 6))))))

  (assert-equalp
    '(|i2| |i4| |i5|)
    (compile::index-var-symbols
      'w (sexpr->exprs '((+ z |i1|)
			 (+ |i3| x y)
			 (+ |i6| 27)))))

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

(defun cexpr->string (e)
  (let ((*print-options* (compile::compile-print-options)))
    (expr->string e)))

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
  (assert-equal "BMC.Vec(x, y, 0.0)"
		(cexpr->string (sexpr->expr '(vec |x| |y| 0.0))))
  (assert-equal "BMC.Vec(x, y)"
		(cexpr->string (sexpr->expr '(vec |x| |y|))))
  (assert-equal "x - y"
		(cexpr->string (sexpr->expr '(- |x| |y|))))
  (assert-equal "-(X) * Y + -(Z)"
		(cexpr->string (sexpr->expr '(+ (* (neg x) y) (neg z)))))
)

(define-test compile-ljd-tests
  (assert-equal
    "ljd += BMC.LogDensityDirichlet(p, alpha_p);
"
    (cppstr (compile::write-ljd-accum-rel "ljd"
	     (sexpr->rel '(~ |p| (ddirch |alpha_p|))))))

  (assert-equal
    "ljd += BMC.LogDensityCat(x, p);
"
    (cppstr (compile::write-ljd-accum-rel "ljd"
	     (sexpr->rel '(~ |x| (dcat |p|))))))

  (assert-equal
    "ljd += BMC.LogDensityNorm(x[i], MU, SIGMA);
"
    (cppstr (compile::write-ljd-accum-rel "ljd"
	     (sexpr->rel '(~ (@ |x| |i|) (dnorm mu sigma))))))

  (assert-equal
    "ll += BMC.LogDensityGamma(x[i], A * C, B / D);
"
    (cppstr (compile::write-ljd-accum-rel "ll"
	     (sexpr->rel '(~ (@ |x| |i|) (dgamma (* a c) (/ b d)))))))

  (assert-equal
    "ljd += BMC.LogDensityMVNorm(x, MU, SIGMA);
"
    (cppstr (compile::write-ljd-accum-rel "ljd"
	     (sexpr->rel '(~ |x| (dmvnorm mu sigma))))))

  (assert-equal
    "ljd += BMC.LogDensityWishart(Lambda, nu + 3, V);
"
    (cppstr (compile::write-ljd-accum-rel "ljd"
	     (sexpr->rel '(~ |Lambda| (dwishart (+ |nu| 3) V))))))

  (assert-equal
    "lp += BMC.LogDensityInterval(v, x, c);
"
    (cppstr (compile::write-ljd-accum-rel "lp"
	      (sexpr->rel '(~ |v| (dinterval |x| |c|))))))

  (assert-equal
    "sigma = 1 / Math.Sqrt(lambda);
"
    (cppstr (compile::write-ljd-accum-rel "lp"
	    (sexpr->rel '(<- |sigma| (/ 1 (sqrt |lambda|)))))))
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
    (cppstr (compile::write-csharp-class
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
      (cppstr (compile::gen-decls "Blah blah" (sexpr->decls decls)))))

  (let ((decls '((|a| (integerp |n|))
		 (|b| (realxn |m| |n|)))))
    (assert-equal
"// Array vars
public int[] a;
public DMatrix b;
"
      (cppstr (compile::gen-decls "Array vars" (sexpr->decls decls)))))

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
      (cppstr (compile::write-csharp-allocate-vars (sexpr->decls vars)))))

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
      (cppstr (compile::write-csharp-load-arguments (sexpr->decls args)))))

#|
  (assert-equal
"
"
    (ppstr
      (compile::write-log-joint-density-function
        (sexpr->rels
	 '((p ~ (ddirch alpha_p))
	   (:for i (1 n) (~ (@ x i) ~ dcat(p)))
	   (~ lambda (dgamma a b))
	   (<- sigma (/ 1 (sqrt lambda)))
	   (:for i (1 n)
	      (~ (@ logz i) (dnorm (neg (/ sigma 2)) (/ sigma 3)))
	      (<- (@ z i) <- (exp (@ logz i)))
	      (if (.= (@ x i) 1)
		  (~ (@ y i) (dnorm 0 sigma)))
	      (if (.= (@ x i) 2)
		  (~ (@ y i) (dgamma (@ z i) (@ z i)))))))))
|#
; log joint density function
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

