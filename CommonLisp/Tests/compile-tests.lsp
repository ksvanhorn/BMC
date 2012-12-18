(defpackage :compile-tests
  (:use :cl :lisp-unit :compile :mcimpl :model :variables :expr :symbols
   :type-inference :utils :testing-utilities))
(in-package :compile-tests)

(defun sexpr->decls (decls) (mapcar #'model:sexpr->decl decls))
(defun sexpr->exprs (se) (mapcar #'sexpr->expr se))
(defun sexpr->rels (se) (mapcar #'sexpr->rel se))

(defun sexpr->named-expr (se)
  (destructuring-bind (key . expr-se) se
    (cons key (sexpr->expr expr-se))))

(defun sexpr->named-expr-list (se) (mapcar #'sexpr->named-expr se))

;;; TODO: move this to utils?
(defun sort-unique (symbols)
  (remove-duplicates (sort (copy-list symbols) #'string<)))

(define-test compile-util-tests
  ;; TODO: move this to utils-tests?
  (assert-equal
    '(a b c) (sort-unique '(b c b b a c c a)))

  (assert-equal
    '(vars::i4 vars::k vars::m vars::n vars::x)
    (sort-unique
      (compile::vars-in-expr-list
	 '(#e(* (/ x 4) (+ k 3) (- m n))
	   #e(:quant qand i4 (1 n) (< i4 k))
	   #e(^1/2 (^ x 6))
	   #efalse))))

  (assert-equal
    '(vars::a vars::c vars::x vars::y vars::z)
    (sort-unique
      (compile::vars-in-expr
        #e(+ a (:let (x (* y z)) (^ x c))))))

  (assert-equal
    '(vars::A vars::AA vars::ALPHA vars::B vars::BB vars::FOO vars::I vars::II
      vars::K vars::M vars::N vars::UPPER-X vars::V vars::W vars::X vars::Z
      vars::ZZ)
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

  (assert-equalp
    #e(:quant qand i (1 (+ n 2))
	      (:quant qand j (1 (- m 3)) (is-real (@ x i j))))
    (compile::array-element-check
      #e(is-real (@ x i j))
      '(#e(+ n 2) #e(- m 3))
      '(vars::i vars::j)))

  (assert-equalp
    #e(:quant qand i (1 (+ n 2)) (< 0 (@ x i)))
    (compile::array-element-check
      #e(< 0 (@ x i))
      (list #e(+ n 2))
      '(vars::i)))

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

  (with-genvar-counter 3
    (assert-equalp
     '(#e(:quant qand |_3i| (1 n) (< 0 (@ x |_3i|))))
     (compile::array-element-checks 'vars::x 'integerp '(#e n))))

  (with-genvar-counter 7
    (assert-equalp
     '(#e(:quant qand |_7i| (1 n)
		 (:quant qand |_8i| (1 k)
			 (is-realp0 (@ v |_7i| |_8i|)))))
     (compile::array-element-checks 'vars::v 'realp0 '(#e n #e k))))

  (with-genvar-counter 4
    (assert-equalp
     '(#e(:quant qand |_4i| (1 (+ n |i1|)) (is-real (@ y |_4i|))))
     (compile::array-element-checks 'vars::y 'real '(#e(+ n |i1|)))))

  (with-genvar-counter 9
    (assert-equalp
     '(#e(:quant qand |_9i| (1 n)
		 (:quant qand |_10i| (1 k)
			 (is-realx (@ |i2| |_9i| |_10i|)))))
     (compile::array-element-checks 'vars::|i2| 'realx '(#e n #e k))))

  (assert-equalp
    '(#e(= (array-length 1 x) n))
    (compile::array-length-checks 'vars::x '(#en)))

  (assert-equalp
    '(#e(= (array-length 1 z) (+ n (* x y)))
      #e(= (array-length 2 z) (- m 3)))
    (compile::array-length-checks 'vars::z '(#e(+ n (* x y)) #e(- m 3))))

  (assert-equalp
    '(#e(is-realp x))
    (compile::decl-checks (sexpr->decl '(x realp))))

  (assert-equalp
    '()
    (compile::decl-checks (sexpr->decl '(n integer))))

  (assert-equalp
    '(#e(= (array-length 1 A) (+ k 1)))
    (compile::decl-checks (sexpr->decl '(A (integer (+ k 1))))))

  (with-genvar-counter 14
    (assert-equalp
     '(#e(= (array-length 1 A) (+ k 1))
	 #e(:quant qand vars::|_14i| (1 (+ k 1)) (<= 0 (@ A vars::|_14i|))))
     (compile::decl-checks (sexpr->decl '(A (integerp0 (+ k 1)))))))

  (assert-equalp
    '(#e(= (array-length 1 x) n))
    (compile::decl-checks (sexpr->decl '(x (realxn n)))))

  (with-genvar-counter 8
    (assert-equalp
      '(#e(= (array-length 1 y) m)
	  #e(= (array-length 2 y) (* k 3))
	  #e(:quant qand |_8i| (1 m)
		    (:quant qand |_9i| (1 (* k 3))
			    (is-real (@ y |_8i| |_9i|)))))
      (compile::decl-checks (sexpr->decl '(y (real m (* k 3)))))))

  (with-genvar-counter 13
    (assert-equalp
      '(#e(< 0 n)
	#e(<= 0 k)
	#e(= (array-length 1 v) m)
	#e(:quant qand |_13i| (1 m) (is-realp0 (@ v |_13i|)))
	#e(= (array-length 1 w) n)
	#e(= (array-length 1 x) n)
	#e(= (array-length 2 x) m)
	#e(:quant qand |_15i| (1 n)
		    (:quant qand |_16i| (1 m) (is-realp (@ x |_15i| |_16i|))))
	#e(= (array-length 1 y) m)
	#e(= (array-length 2 y) n)
	#e(< m n)
	#e(:quant qand i (1 m) (< (@ v i) 100))
	#e(is-integerp0 n))
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
		       (:body))))))

  (assert-equalp
    '(#e(is-integerp0 n))
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

  (with-genvar-counter 27
    (assert-equalp
     '(#e(is-realp x)
	 #e(< 0 y)
	 #e(= (array-length 1 z) n)
	 #e(:quant qand |_27i| (1 n) (is-realp (@ z |_27i|))))
     (compile::params-checks
      (sexpr->mcimpl
       '(:mcimpl
	 (:parameters (x realp) (y integerp) (z (realp n)))
	 (:acceptmons)
	 (:expectations)
	 (:updates))))))

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
    (assert-equal 2 (funcall dim-fct 'vars::b))
    (assert-equal 1 (funcall dim-fct 'vars::c))
    (assert-equal 0 (funcall dim-fct 'vars::k))
    (assert-error 'error (funcall dim-fct 'vars::n))
    (assert-error 'error (funcall dim-fct 'vars::m))
    (assert-error 'error (funcall dim-fct 'vars::a))
    (assert-error 'error (funcall dim-fct 'vars::foo)))

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
      (dolist (x '(vars::m vars::r vars::n vars::a vars::b vars::c))
	(assert-true (funcall is-class-var x)))
      (dolist (x '(vars::foo vars::bar vars::x vars::z))
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
)

(defun cexpr->string (e) (compile::expr->string e))

(define-test compile-expression-test
  (assert-equal "x" (cexpr->string #e|x|))
  (assert-equal "5" (cexpr->string #e5))
  (assert-equal "-3.25e+0" (cexpr->string #e-3.25))
  (assert-equal "true" (cexpr->string #etrue))
  (assert-equal "false" (cexpr->string #efalse))

  (assert-equal "x[i - 1]" (cexpr->string #e(@ |x| |i|)))
  (assert-equal "y[i + 2 - 1, j - 1 - 1]"
		(cexpr->string #e(@ |y| (+ |i| 2) (- |j| 1))))
  (assert-equal
    "BMC.ArraySlice(x, i - 1, BMC.Range(lo - 1, hi), BMC.FullRange)"
    (cexpr->string #e(@ |x| |i| (:range |lo| |hi|) :all)))

  (assert-equal "Math.Sqrt(x)" (cexpr->string #e(^1/2 |x|)))
  (assert-equal "BMC.MatrixInverse(x)" (cexpr->string #e(inv |x|)))
  (assert-equal "Math.Exp(x / 2)" (cexpr->string #e(exp (/ |x| 2))))
  (assert-equal "Math.Tanh(y)" (cexpr->string #e(tanh |y|)))
  (assert-equal "BMC.Vec(x, y, 0.0e+0)" (cexpr->string #e(vec |x| |y| 0.0)))
  (assert-equal "BMC.Vec(x, y)"	(cexpr->string #e(vec |x| |y|)))
  (assert-equal "x - y" (cexpr->string #e(- |x| |y|)))
  (assert-equal "-(X) * Y + -(Z)" (cexpr->string #e(+ (* (neg x) y) (neg z))))
  (assert-equal "BMC.QSum(M, N, (I => X[I - 1]))"
    (cexpr->string #e(:quant qsum i (m n) (@ x i))))
  (assert-equal "BMC.QSum(M, N, (I => X[I - 1]))"
    (cexpr->string #e(:quant qsum i (m n) true (@ x i))))
  (assert-equal "BMC.QSum(M, N, (I => W[I - 1] < X[I - 1]), (I => Y[I - 1]))"
    (cexpr->string #e(:quant qsum i (m n) (< (@ w i) (@ x i)) (@ y i))))
  (assert-equal "(x < y ? a + b : a * b)"
    (cexpr->string #e(if-then-else (< |x| |y|) (+ |a| |b|) (* |a| |b|))))
  (assert-equal "BMC.Let(y * y, (x => c * x))"
    (cexpr->string #e(:let (|x| (* |y| |y|)) (* |c| |x|))))
)

(define-test compile-ljd-tests
  (let-test-macros
      ((expect (rel => &rest result)
         `(assert-equal
	   (strcat-lines ,@result)
	     (ppstr (compile::write-ljd-accum-rel ,(sexpr->rel rel))))))

    (expect :rel (~ |p| (ddirch |alpha_p|)) =>
            "_ljd += BMC.LogDensityDirichlet(p, alpha_p);")

    (expect :rel (~ |x| (dcat |p|)) =>
    	  "_ljd += BMC.LogDensityCat(x, p);")

    (expect :rel (~ (@ |x| |i|) (dnorm mu sigma)) =>
            "_ljd += BMC.LogDensityNorm(x[i - 1], MU, SIGMA);")

    (expect :rel (~ (@ |x| |i|) (dgamma (* a c) (/ b d))) =>
            "_ljd += BMC.LogDensityGamma(x[i - 1], A * C, B / D);")

    (expect :rel (~ |x| (dmvnorm mu sigma)) =>
            "_ljd += BMC.LogDensityMVNorm(x, MU, SIGMA);")

    (expect :rel (~ |Lambda| (dwishart (+ |nu| 3) V)) =>
            "_ljd += BMC.LogDensityWishart(Lambda, nu + 3, V);")

    (expect :rel (~ |v| (dinterval |x| |c|)) =>
            "_ljd += BMC.LogDensityInterval(v, x, c);")

    (expect
      :rel (:let (|sigma| (/ 1 (^1/2 |lambda|)))
    	     (~ x (dnorm 0 |sigma|)))
      =>
      "{"
      "    var sigma = 1 / Math.Sqrt(lambda);"
      "    _ljd += BMC.LogDensityNorm(X, 0, sigma);"
      "}")

    (expect
      :rel (:block (~ y (dcat p))
		   (~ x (dnorm 0 (@ sigma y))))
      =>
      "_ljd += BMC.LogDensityCat(Y, P);"
      "_ljd += BMC.LogDensityNorm(X, 0, SIGMA[Y - 1]);")

    (expect
      :rel (:if (= (@ x i) 1)
    	     (~ (@ y i) (dnorm a b)))
      =>
      "if (X[I - 1] == 1) {"
      "    _ljd += BMC.LogDensityNorm(Y[I - 1], A, B);"
      "}")

    (expect
      :rel (:if (< (@ v i) 4)
    	     (~ (@ z i) (dnorm m s))
    	     (~ (@ w i) (dcat p)))
      =>
      "if (V[I - 1] < 4) {"
      "    _ljd += BMC.LogDensityNorm(Z[I - 1], M, S);"
      "}"
      "else {"
      "    _ljd += BMC.LogDensityCat(W[I - 1], P);"
      "}")

    (expect
      :rel (:for |i| ((- m 1) (+ n 2))
             (~ (@ x |i|) (dgamma (^1/2 (@ y |i|)) 1)))
      =>
      "for (int i = M - 1; i <= N + 2; ++i) {"
      "    _ljd += BMC.LogDensityGamma(X[i - 1], Math.Sqrt(Y[i - 1]), 1);"
      "}")

    ;; Test that brackets placed around "let" only when necessary.
    (expect
      :rel (:block
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
                  (~ w (dgamma a b))))))
      =>
"{
    var A = 1;
    var B = 2;
    _ljd += BMC.LogDensityGamma(X, A, B);
}
for (int I = M; I <= N; ++I) {
    var A = 1;
    var B = 2;
    _ljd += BMC.LogDensityGamma(Y[I - 1], A, B);
}
if (M < N) {
    var A = 1;
    var B = 2;
    _ljd += BMC.LogDensityGamma(Z, A, B);
}
else {
    var A = 1;
    var B = 2;
    _ljd += BMC.LogDensityGamma(W, A, B);
}")
))

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

  (let-test-macros
    ((expect (rel xform-pfx class-vars genvar-counter => &rest lines)
       (let ((var-xform (case xform-pfx
			      (new #'compile::new-val-var)
			      (old #'compile::old-val-var)
			      (otherwise (error "Unknown variable xform")))))
	 `(let* ((rel1 (sexpr->rel ',rel))
		 (cv (mapcar #'vars-symbol ',class-vars))
		 (is-class-var (fn (v) (member v cv))))
	    (with-genvar-counter ,genvar-counter
	      (assert-equal
	       (strcat-lines ,@lines)
	       (ppstr
		(compile::write-log-proposal-density
		 ,var-xform is-class-var rel1))))))))

    (expect 
      :rel (~ p (ddirch a))
      :xform-pfx new
      :class-vars (a p)
      :genvar-counter 34
       =>
       "_x.P = BMC.Copy(_new_P);"
       "_lpd += BMC.LogDensityDirichlet(_x.P, _x.A);")

    (expect
      :rel (~ y (dcat p))
      :xform-pfx old
      :class-vars (y)
      :genvar-counter 11
      =>
      "_x.Y = BMC.Copy(_old_Y);"
      "_lpd += BMC.LogDensityCat(_x.Y, P);")

    (expect
      :rel (~ (@ y i) (dnorm mu sigma))
      :xform-pfx new
      :class-vars (y mu)
      :genvar-counter 7
      =>
      "_x.Y[I - 1] = BMC.Copy(_new_Y[I - 1]);"
      "_lpd += BMC.LogDensityNorm(_x.Y[I - 1], _x.MU, SIGMA);")

    (expect
      :rel (~ (@ z r) (dgamma (* a c) (/ b d)))
      :xform-pfx old
      :class-vars (z c b)
      :genvar-counter 9
      =>
      "_x.Z[R - 1] = BMC.Copy(_old_Z[R - 1]);"
      "_lpd += BMC.LogDensityGamma(_x.Z[R - 1], A * _x.C, _x.B / D);")

    (expect
      :rel (~ v (dmvnorm mu sigma))
      :xform-pfx new
      :class-vars (v sigma)
      :genvar-counter 6
      =>
      "_x.V = BMC.Copy(_new_V);"
      "_lpd += BMC.LogDensityMVNorm(_x.V, MU, _x.SIGMA);")

    (expect
      :rel (~ |Lambda| (dwishart (+ nu 3) W))
      :xform-pfx old
      :class-vars (|Lambda|)
      :genvar-counter 24
      =>
      "_x.Lambda = BMC.Copy(_old_Lambda);"
      "_lpd += BMC.LogDensityWishart(_x.Lambda, NU + 3, W);")

    (expect
      :rel (~ u (dinterval y c))
      :xform-pfx new
      :class-vars (u y)
      :genvar-counter 6
      =>
      "_x.U = BMC.Copy(_new_U);"
      "_lpd += BMC.LogDensityInterval(_x.U, _x.Y, C);")

    (expect
      :rel (:let (sigma (/ 1 (^1/2 lambda)))
             (~ y (dnorm 0 sigma)))
      :xform-pfx old
      :class-vars (y lambda)
      :genvar-counter 85
      =>
      "{"
      "    var SIGMA = 1 / Math.Sqrt(_x.LAMBDA);"
      "    _x.Y = BMC.Copy(_old_Y);"
      "    _lpd += BMC.LogDensityNorm(_x.Y, 0, SIGMA);"
      "}")

    (expect
      :rel (:let (x0 x) (~ x (dnorm x0 sigma)))
      :xform-pfx old
      :class-vars (x sigma)
      :genvar-counter 91
      =>
      "{"
      "    var X0 = BMC.Copy(_x.X);"
      "    _x.X = BMC.Copy(_old_X);"
      "    _lpd += BMC.LogDensityNorm(_x.X, X0, _x.SIGMA);"
      "}")

    (expect
      :rel (:block
	     (~ y (dcat p))
	     (~ z (dnorm 0 (@ sigma y))))
      :xform-pfx new
      :class-vars (y z sigma)
      :genvar-counter 28
      =>
      "_x.Y = BMC.Copy(_new_Y);"
      "_lpd += BMC.LogDensityCat(_x.Y, P);"
      "_x.Z = BMC.Copy(_new_Z);"
      "_lpd += BMC.LogDensityNorm(_x.Z, 0, _x.SIGMA[_x.Y - 1]);")

    (expect
      :rel (:block
	     (:let (a (^2 g))
               (:let (b (^2 h))
                 (~ y (dgamma a b))))
	     (~ u (dnorm m y)))
      :xform-pfx old
      :class-vars (y u h)
      :genvar-counter 13
      =>
      "{"
      "    var A = BMC.Sqr(G);"
      "    var B = BMC.Sqr(_x.H);"
      "    _x.Y = BMC.Copy(_old_Y);"
      "    _lpd += BMC.LogDensityGamma(_x.Y, A, B);"
      "}"
      "_x.U = BMC.Copy(_old_U);"
      "_lpd += BMC.LogDensityNorm(_x.U, M, _x.Y);")
  )

  (let-test-macros
    ((expect (outer-lets rel class-vars genvar-counter => &rest lines)
       `(let* ((outer-lets-1 (sexpr->named-expr-list ',outer-lets))
	      (rel1 (sexpr->rel ',rel))
	      (cv (mapcar #'vars-symbol ',class-vars))
	      (is-class-var (fn (v) (member v cv)))
	      (dim-fct (fn (v) nil)))
	  (assert-equal
	    (strcat-lines ,@lines)
	    (ppstr
	      (with-genvar-counter ,genvar-counter
	        (compile::write-test-acceptance-ratio-mh
	          outer-lets-1 rel1 is-class-var dim-fct)))))))

   (expect
     :outer-lets ()
     :rel (:metropolis-hastings
	    :lets ()
	    :proposal-distribution (~ p (ddirch a))
	    :log-acceptance-ratio 0.0)
     :class-vars (p a)
     :genvar-counter 13
     =>
"double _ljdold = _x.LogJointDensity();
var _old_P = BMC.Copy(_x.P);
BMC.DrawDirichlet(_x.P, _x.A);
double _lar = 0.0;
var _new_P = BMC.Copy(_x.P);
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
_x.P = BMC.Copy(_old_P);
_lpd += BMC.LogDensityDirichlet(_x.P, _x.A);
double _lpdnew = _lpd;
Assert.IsTrue(BMC.Equal(_x.P, _old_P), \"Proposal must be reversible\");
_lpd = 0.0;
_x.P = BMC.Copy(_new_P);
_lpd += BMC.LogDensityDirichlet(_x.P, _x.A);
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");")

   (expect
     :outer-lets ()
     :rel (:metropolis-hastings
	    :lets ()
	    :proposal-distribution (~ (@ y i j) (dnorm mu sigma))
	    :log-acceptance-ratio 0.0)
     :class-vars (y mu)
     :genvar-counter 17
     =>
"double _ljdold = _x.LogJointDensity();
var _old_Y = BMC.Copy(_x.Y);
_x.Y[I - 1, J - 1] = BMC.DrawNorm(_x.MU, SIGMA);
double _lar = 0.0;
var _new_Y = BMC.Copy(_x.Y);
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
_x.Y[I - 1, J - 1] = BMC.Copy(_old_Y[I - 1, J - 1]);
_lpd += BMC.LogDensityNorm(_x.Y[I - 1, J - 1], _x.MU, SIGMA);
double _lpdnew = _lpd;
Assert.IsTrue(BMC.Equal(_x.Y, _old_Y), \"Proposal must be reversible\");
_lpd = 0.0;
_x.Y[I - 1, J - 1] = BMC.Copy(_new_Y[I - 1, J - 1]);
_lpd += BMC.LogDensityNorm(_x.Y[I - 1, J - 1], _x.MU, SIGMA);
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");")

  (expect
    :outer-lets ()
    :rel (:metropolis-hastings
	   :lets ((a (^1/2 z)))
	   :proposal-distribution (~ z (dnorm m s))
	   :log-acceptance-ratio 0.0)
    :class-vars (z m s)
    :genvar-counter 22
    =>
"double _ljdold = _x.LogJointDensity();
var _old_Z = BMC.Copy(_x.Z);
var A = Math.Sqrt(_x.Z);
_x.Z = BMC.DrawNorm(_x.M, _x.S);
double _lar = 0.0;
var _new_Z = BMC.Copy(_x.Z);
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
_x.Z = BMC.Copy(_old_Z);
_lpd += BMC.LogDensityNorm(_x.Z, _x.M, _x.S);
double _lpdnew = _lpd;
Assert.IsTrue(BMC.Equal(_x.Z, _old_Z), \"Proposal must be reversible\");
_lpd = 0.0;
_x.Z = BMC.Copy(_new_Z);
_lpd += BMC.LogDensityNorm(_x.Z, _x.M, _x.S);
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");")

  ;; Check that the scope of a local variable (sigma) defined in the proposal
  ;; distribution is appropriately limited, for a general M-H update.
  (expect
    :outer-lets ()
    :rel (:metropolis-hastings
	   :lets ()
	   :proposal-distribution
	     (:let (sigma (/ alpha (^1/2 lambda)))
               (~ y (dnorm 0 sigma)))
	   :log-acceptance-ratio (^2 (/ y sigma)))
    :class-vars (y alpha)
    :genvar-counter 34
    =>
"double _ljdold = _x.LogJointDensity();
var _old_Y = BMC.Copy(_x.Y);
var _34save_Y = BMC.Copy(_x.Y);

{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.DrawNorm(0, SIGMA);
}
double _lar = BMC.Sqr(_x.Y / SIGMA);
var _new_Y = BMC.Copy(_x.Y);
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.Copy(_old_Y);
    _lpd += BMC.LogDensityNorm(_x.Y, 0, SIGMA);
}
double _lpdnew = _lpd;
Assert.IsTrue(BMC.Equal(_x.Y, _old_Y), \"Proposal must be reversible\");
_lpd = 0.0;
{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.Copy(_new_Y);
    _lpd += BMC.LogDensityNorm(_x.Y, 0, SIGMA);
}
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");

if (!BMC.Accept(_lar)) {
    _x.Y = _34save_Y;
}")

  ;; Check that the scope of a local variable (sigma) defined in the proposal
  ;; distribution is appropriately limited, for a Gibbs update.
  (expect
    :outer-lets ()
    :rel (:metropolis-hastings
	   :lets ()
	   :proposal-distribution
	     (:let (sigma (/ alpha (^1/2 lambda)))
               (~ y (dnorm 0 sigma)))
	   :log-acceptance-ratio 0.0)
    :class-vars (y alpha)
    :genvar-counter 6
    =>
"double _ljdold = _x.LogJointDensity();
var _old_Y = BMC.Copy(_x.Y);
{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.DrawNorm(0, SIGMA);
}
double _lar = 0.0;
var _new_Y = BMC.Copy(_x.Y);
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.Copy(_old_Y);
    _lpd += BMC.LogDensityNorm(_x.Y, 0, SIGMA);
}
double _lpdnew = _lpd;
Assert.IsTrue(BMC.Equal(_x.Y, _old_Y), \"Proposal must be reversible\");
_lpd = 0.0;
{
    var SIGMA = _x.ALPHA / Math.Sqrt(LAMBDA);
    _x.Y = BMC.Copy(_new_Y);
    _lpd += BMC.LogDensityNorm(_x.Y, 0, SIGMA);
}
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");")

  (expect
    :outer-lets ((bar . (vec a b c)))
    :rel (:metropolis-hastings
	   :lets ()
	   :proposal-distribution
	     (:block
	       (:let (pvec (@+ foo bar))
		 (~ y (dcat pvec)))
	       (~ z (dgamma a b)))
	   :log-acceptance-ratio 0.0)
    :class-vars (y z foo b)
    :genvar-counter 11
    =>
"double _ljdold = _x.LogJointDensity();
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
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
{
    var PVEC = BMC.ArrPlus(_x.FOO, BAR);
    _x.Y = BMC.Copy(_old_Y);
    _lpd += BMC.LogDensityCat(_x.Y, PVEC);
}
_x.Z = BMC.Copy(_old_Z);
_lpd += BMC.LogDensityGamma(_x.Z, A, _x.B);
double _lpdnew = _lpd;
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
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");")

  (expect
    :outer-lets ()
    :rel (:metropolis-hastings
	  :lets ()
	  :proposal-distribution (~ u (dnorm mu sigma))
	  :log-acceptance-ratio (* sigma (- u mu)))
    :class-vars (u sigma)
    :genvar-counter 20
    =>
"double _ljdold = _x.LogJointDensity();
var _old_U = BMC.Copy(_x.U);
var _20save_U = BMC.Copy(_x.U);

_x.U = BMC.DrawNorm(MU, _x.SIGMA);
double _lar = _x.SIGMA * (_x.U - MU);
var _new_U = BMC.Copy(_x.U);
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
_x.U = BMC.Copy(_old_U);
_lpd += BMC.LogDensityNorm(_x.U, MU, _x.SIGMA);
double _lpdnew = _lpd;
Assert.IsTrue(BMC.Equal(_x.U, _old_U), \"Proposal must be reversible\");
_lpd = 0.0;
_x.U = BMC.Copy(_new_U);
_lpd += BMC.LogDensityNorm(_x.U, MU, _x.SIGMA);
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");

if (!BMC.Accept(_lar)) {
    _x.U = _20save_U;
}")

  (expect
    :outer-lets ()
    :rel (:metropolis-hastings
	   :lets ((f (@ z i)) (sigma2 (* f f)))
	   :proposal-distribution
	     (:block
	       (~ (@ z i) (dnorm-trunc mu sigma a b))
	       (~ w (dnorm (@ z i) 1)))
	   :log-acceptance-ratio (+ (@ z i) sigma2))
    :class-vars (z m y gamma v a b w)
    :genvar-counter 42
    =>
"double _ljdold = _x.LogJointDensity();
var _old_Z = BMC.Copy(_x.Z);
var _old_W = BMC.Copy(_x.W);
var F = _x.Z[I - 1];
var SIGMA2 = F * F;
var _42save_Z = BMC.Copy(_x.Z[I - 1]);
var _43save_W = BMC.Copy(_x.W);

_x.Z[I - 1] = BMC.DrawNormTruncated(MU, SIGMA, _x.A, _x.B);
_x.W = BMC.DrawNorm(_x.Z[I - 1], 1);
double _lar = _x.Z[I - 1] + SIGMA2;
var _new_Z = BMC.Copy(_x.Z);
var _new_W = BMC.Copy(_x.W);
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
_x.Z[I - 1] = BMC.Copy(_old_Z[I - 1]);
_lpd += BMC.LogDensityNormTruncated(_x.Z[I - 1], MU, SIGMA, _x.A, _x.B);
_x.W = BMC.Copy(_old_W);
_lpd += BMC.LogDensityNorm(_x.W, _x.Z[I - 1], 1);
double _lpdnew = _lpd;
Assert.IsTrue(BMC.Equal(_x.Z, _old_Z), \"Proposal must be reversible\");
Assert.IsTrue(BMC.Equal(_x.W, _old_W), \"Proposal must be reversible\");
_lpd = 0.0;
_x.Z[I - 1] = BMC.Copy(_new_Z[I - 1]);
_lpd += BMC.LogDensityNormTruncated(_x.Z[I - 1], MU, SIGMA, _x.A, _x.B);
_x.W = BMC.Copy(_new_W);
_lpd += BMC.LogDensityNorm(_x.W, _x.Z[I - 1], 1);
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");

if (!BMC.Accept(_lar)) {
    _x.Z[I - 1] = _42save_Z;
    _x.W = _43save_W;
}")

  (expect
    :outer-lets ((a . (^2 z))
		 (b . (exp y)))
    :rel (:metropolis-hastings
	   :lets ()
	   :proposal-distribution (~ z (dnorm m (* a b)))
	   :log-acceptance-ratio 0.0)
    :class-vars (y z m)
    :genvar-counter 5
    =>
"double _ljdold = _x.LogJointDensity();
var _old_Z = BMC.Copy(_x.Z);
_x.Z = BMC.DrawNorm(_x.M, A * B);
double _lar = 0.0;
var _new_Z = BMC.Copy(_x.Z);
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
_x.Z = BMC.Copy(_old_Z);
_lpd += BMC.LogDensityNorm(_x.Z, _x.M, A * B);
double _lpdnew = _lpd;
Assert.IsTrue(BMC.Equal(_x.Z, _old_Z), \"Proposal must be reversible\");
_lpd = 0.0;
_x.Z = BMC.Copy(_new_Z);
_lpd += BMC.LogDensityNorm(_x.Z, _x.M, A * B);
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");")

  (expect
    :outer-lets ((Q0 . (@* A B)) (Q . ($* (^-1 (sum Q0)) Q0)))
    :rel (:metropolis-hastings
	   :lets ((s0 (@ s r)))
	   :proposal-distribution (:block
				    (~ (@ s r) (dcat Q))
				    (:let (m (@ mu (@ s r)))
				      (~ (@ x r) (dnorm m sigma))))
	   :log-acceptance-ratio (- (@ p (@ s r)) (@ p s0)))
    :class-vars (s x A B p mu)
    :genvar-counter 53
    =>
"double _ljdold = _x.LogJointDensity();
var _old_S = BMC.Copy(_x.S);
var _old_X = BMC.Copy(_x.X);
var S0 = _x.S[R - 1];
var _53save_S = BMC.Copy(_x.S[R - 1]);
var _54save_X = BMC.Copy(_x.X[R - 1]);

_x.S[R - 1] = BMC.DrawCat(Q);
{
    var M = _x.MU[_x.S[R - 1] - 1];
    _x.X[R - 1] = BMC.DrawNorm(M, SIGMA);
}
double _lar = _x.P[_x.S[R - 1] - 1] - _x.P[S0 - 1];
var _new_S = BMC.Copy(_x.S);
var _new_X = BMC.Copy(_x.X);
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
_x.S[R - 1] = BMC.Copy(_old_S[R - 1]);
_lpd += BMC.LogDensityCat(_x.S[R - 1], Q);
{
    var M = _x.MU[_x.S[R - 1] - 1];
    _x.X[R - 1] = BMC.Copy(_old_X[R - 1]);
    _lpd += BMC.LogDensityNorm(_x.X[R - 1], M, SIGMA);
}
double _lpdnew = _lpd;
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
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");

if (!BMC.Accept(_lar)) {
    _x.S[R - 1] = _53save_S;
    _x.X[R - 1] = _54save_X;
}")

  (expect
    :outer-lets ()
    :rel
     (:metropolis-hastings
      :lets ()
      :proposal-distribution
      (:block 
        (~ y (dmvnorm mu_y sigma_y))
        (:for i (1 n)
          (:if (= 1 (@ z i))
            (:let (m (dot mu_v (@ u i :all)))
              (~ (@ v i) (dnorm m sigma_v))))))
      :log-acceptance-ratio 0.0)
    :class-vars (y v mu_y sigma_y u mu_v sigma_v)
    :genvar-counter 13
    =>
"double _ljdold = _x.LogJointDensity();
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
double _ljdnew = _x.LogJointDensity();
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
double _lpdnew = _lpd;
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
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");")

  (expect
    :outer-lets ()
    :rel
    (:metropolis-hastings
      :lets ((y0 y))
      :proposal-distribution
      (:let (w0 w)
	(:block
	  (~ y (dmvnorm m sigmay))
	  (~ w (dmvnorm w0 sigmaw))))
      :log-acceptance-ratio (dot y y0))
    :class-vars (y w m)
    :genvar-counter 61
    =>
"double _ljdold = _x.LogJointDensity();
var _old_Y = BMC.Copy(_x.Y);
var _old_W = BMC.Copy(_x.W);
var Y0 = BMC.Copy(_x.Y);
var _61save_Y = BMC.Copy(_x.Y);
var _62save_W = BMC.Copy(_x.W);

{
    var W0 = BMC.Copy(_x.W);
    BMC.DrawMVNorm(_x.Y, _x.M, SIGMAY);
    BMC.DrawMVNorm(_x.W, W0, SIGMAW);
}
double _lar = BMC.Dot(_x.Y, Y0);
var _new_Y = BMC.Copy(_x.Y);
var _new_W = BMC.Copy(_x.W);
double _ljdnew = _x.LogJointDensity();
double _lpd = 0.0;
{
    var W0 = BMC.Copy(_x.W);
    _x.Y = BMC.Copy(_old_Y);
    _lpd += BMC.LogDensityMVNorm(_x.Y, _x.M, SIGMAY);
    _x.W = BMC.Copy(_old_W);
    _lpd += BMC.LogDensityMVNorm(_x.W, W0, SIGMAW);
}
double _lpdnew = _lpd;
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
double _lpdold = _lpd;
Assert.AreEqual(_lar, (_ljdnew - _ljdold) + (_lpdnew - _lpdold), _tol, \"Log acceptance ratio\");

if (!BMC.Accept(_lar)) {
    _x.Y = _61save_Y;
    _x.W = _62save_W;
}")
  )

)

(defun cvar2str (s) (compile::variable->string s))

(define-test compile-test-is-valid-update-tests
  ;; model-variables-assigned-in
  (assert-equalp
    '(vars::x)
    (compile::model-variables-assigned-in
      (sexpr->rel '(~ x (dnorm mu sigma)))))
  (assert-equalp
    '(vars::y)
    (compile::model-variables-assigned-in
      (sexpr->rel '(~ y (dnorm mu sigma)))))
  (assert-equalp
    '(vars::z)
    (compile::model-variables-assigned-in
      (sexpr->rel '(~ (@ z i j) (dgamma a b)))))
  (assert-equalp
    '(vars::w)
    (compile::model-variables-assigned-in
      (sexpr->rel '(~ (@ w :all (:range m n) j) (dgamma a b)))))
  (assert-equalp
    '(vars::x vars::y)
    (compile::model-variables-assigned-in
      (sexpr->rel '(:block
		     (~ x (dnorm 0 1))
		     (~ y (dgamma 1 1))))))
  (assert-equalp
    '(vars::a vars::b)
    (compile::model-variables-assigned-in
      (sexpr->rel '(:if (< (@ x i) y)
                     (~ a (dcat p))
		     (~ b (dnorm-trunc 0 1))))))
  (assert-equalp
    '(vars::v)
    (compile::model-variables-assigned-in
      (sexpr->rel '(:for i (m n) (~ v (dwishart nu V))))))
  (assert-equalp
    '(vars::s)
    (compile::model-variables-assigned-in
      (sexpr->rel '(:let (a 1) (~ s (dcat q))))))
  (assert-equalp
    '()
    (compile::model-variables-assigned-in (make-relation-skip)))
  (assert-equalp
    '(vars::y)
    (compile::model-variables-assigned-in
      (sexpr->rel '(:metropolis-hastings
		    :lets ((a (+ x y)) (b 3))
		    :proposal-distribution (~ y (dnorm y0 s))
		    :log-acceptance-ratio (* (- y y0) s)))))

  ;; write-assigned-test
  (let ((dim-fct
	  (fn (var) (case var ('vars::x0 0) ('vars::x1 1) ('vars::x2 2))))
	(compile::*variable->string*
	  (compile::var2str-ext '(vars::x0 vars::x1 vars::x2))))
    (let-test-macros
      ((expect (lhs gvc => &rest lines)
	 `(assert-equal
	    (strcat-lines ,@lines)
	    (ppstr
	      (with-genvar-counter ,gvc
	        (compile::write-assigned-test
		  (sexpr->rellhs ',lhs) dim-fct))))))

      (expect :lhs x0 :gvc 6 =>
       "Assert.IsFalse(_assigned_X0, \"X0 assigned\");"
       "_assigned_X0 = true;")

      (expect :lhs x1 :gvc 12 =>
       "for (int _12idx = 0; _12idx < _assigned_X1.Length; ++_12idx) {"
       "    Assert.IsFalse(_assigned_X1[_12idx], \"X1[{0}] assigned\", _12idx);"
       "    _assigned_X1[_12idx] = true;"
       "}")

      (expect :lhs x2 :gvc 7 =>
       "for (int _7idx = 0; _7idx < _assigned_X2.NBRows; ++_7idx) {"
       "    for (int _8idx = 0; _8idx < _assigned_X2.NBCols; ++_8idx) {"
       "        Assert.IsFalse(_assigned_X2[_7idx, _8idx], \"X2[{0}, {1}] assigned\", _7idx, _8idx);"
       "        _assigned_X2[_7idx, _8idx] = true;"
       "    }"
       "}")

      (expect :lhs (@ x1 k) :gvc 13 =>
       "Assert.IsFalse(_assigned_X1[K - 1], \"X1[{0}] assigned\", K - 1);"
       "_assigned_X1[K - 1] = true;")

      (expect :lhs (@ x2 j k) :gvc 105 =>
       "Assert.IsFalse(_assigned_X2[J - 1, K - 1], \"X2[{0}, {1}] assigned\", J - 1, K - 1);"
       "_assigned_X2[J - 1, K - 1] = true;")
    ))

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
		(cv (mapcar #'vars-symbol class-vars))
		(not-cv (mapcar #'vars-symbol not-class-vars))
		(is-class-var
		 `(fn (,v) (member ,v ',cv))))
	   `(let* ((,expected (apply #'strcat-lines ',lines))
		   (MH ',MH)
		   (update (sexpr->rel ,upd))
		   (,dim-fct (fn (,v) 0))
		   (,write-mh
		    (fn (outer-lets mh is-cv df)
		      (assert-eq ,dim-fct df)
		      (dolist (,v ',cv)
			(assert-true (funcall is-cv ,v)))
		      (dolist (,v ',not-cv)
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
     :class-vars ()
     :not-class-vars (a b c foo)
     :lines ("// M-H: 0"))

    (write-tivu-body-test
     :upd `(:let (a (+ u v))
	     (:for r (lo (- hi 1))
	       ,(elt MH 1)))
    :class-vars (u hi)
    :not-class-vars (a r lo v foo)
    :lines  ("var A = _x.U + V;"
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
     :class-vars (y other)
     :not-class-vars ()
     :lines  ("var Y0 = BMC.Copy(_x.Y);"
	      "// M-H: 2"
	      "// Y0 = _x.Y"))

    (with-genvar-counter 15
    (write-tivu-body-test
     :upd `(:if (= 1 (@ seg r)) ,(elt MH 2))
     :class-vars (seg)
     :not-class-vars (r |_15if|)
     :lines  ("var _15if = 1 == _x.SEG[R - 1];"
	      "if (_15if) {"
	      "    // M-H: 2"
	      "    // _15if = 1 == _x.SEG[R - 1]"
              "}")))

    (with-genvar-counter 5
    (write-tivu-body-test
     :upd `(:if test_a (:if test_b (:if test_c ,(elt MH 1) ,(elt MH 5))
                                   ,(elt MH 4))
		       (:if test_d ,(elt MH 3)))
     :class-vars (test_c test_a)
     :not-class-vars (test_b test_d)
     :lines  ("var _5if = _x.TEST_A;"
	      "if (_5if) {"
              "    var _6if = TEST_B;"
              "    if (_6if) {"
              "        var _7if = _x.TEST_C;"
              "        if (_7if) {"
              "            // M-H: 1"
	      "            // _5if = _x.TEST_A"
              "            // _6if = TEST_B"
              "            // _7if = _x.TEST_C"
              "        }"
              "        else {"
              "            // M-H: 5"
	      "            // _5if = _x.TEST_A"
              "            // _6if = TEST_B"
              "            // _7if = _x.TEST_C"
              "        }"
              "    }"
              "    else {"
              "        // M-H: 4"
	      "        // _5if = _x.TEST_A"
              "        // _6if = TEST_B"
              "    }"
	      "}"
              "else {"
              "    var _8if = TEST_D;"
	      "    if (_8if) {"
	      "        // M-H: 3"
	      "        // _5if = _x.TEST_A"
              "        // _8if = TEST_D"
              "    }"
              "}")))

    (with-genvar-counter 61
    (write-tivu-body-test
     :upd `(:for s ((+ m 1) (- n 2))
	     (:if (< (@ y s) thresh)
	       (:let (b 28) ,(elt MH 3))
	       (:let (a 37) ,(elt MH 4))))
     :class-vars (y m)
     :not-class-vars (s n a b thresh |_lo_S| |_hi_S| |_61if|)
     :lines  ("var _lo_S = _x.M + 1;"
	      "var _hi_S = N - 2;"
	      "for (int S = _lo_S; S <= _hi_S; ++S) {"
	      "    var _61if = _x.Y[S - 1] < THRESH;"
	      "    if (_61if) {"
	      "        var B = 28;"
	      "        // M-H: 3"
	      "        // _lo_S = _x.M + 1"
	      "        // _hi_S = N - 2"
	      "        // _61if = _x.Y[S - 1] < THRESH"
	      "        // B = 28"
	      "    }"
	      "    else {"
	      "        var A = 37;"
	      "        // M-H: 4"
	      "        // _lo_S = _x.M + 1"
	      "        // _hi_S = N - 2"
	      "        // _61if = _x.Y[S - 1] < THRESH"
	      "        // A = 37"
	      "    }"
              "}"))))

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
	   (let* ((outer-lets-1 (sexpr->named-expr-list outer-lets))
		  (rel1 (sexpr->rel rel))
		  (cv (mapcar #'vars-symbol class-vars))
		  (is-class-var (fn (v) (member v cv)))
		  (xform-assoc
		    (lambda (x) (destructuring-bind (symbol . ndims) x
                                  (cons (vars-symbol symbol) ndims))))
		  (d (mapcar xform-assoc dims))
		  (dim-fct (fn (v) (assoc-lookup v d))))
	     (assert-equal
	       expected
	       (ppstr
		 (compile::write-test-is-valid-update-mh
		   outer-lets-1 rel1 is-class-var dim-fct))))))

    (with-genvar-counter 16
    (write-tivu-mh-test
"bool [] _assigned_p = new bool[_x.p.Length];
for (int _16idx = 0; _16idx < _assigned_p.Length; ++_16idx) {
    Assert.IsFalse(_assigned_p[_16idx], \"p[{0}] assigned\", _16idx);
    _assigned_p[_16idx] = true;
}
BMC.DrawDirichlet(_x.p, _x.alpha_p);
"
      :outer-lets '()
      :rel '(:metropolis-hastings
	     :lets ()
	     :proposal-distribution (~ |p| (ddirch |alpha_p|))
	     :log-acceptance-ratio 0)
      :class-vars '(|p| |alpha_p|)
      :dims '((|p| . 1))))

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

  (with-genvar-counter 9
  (write-tivu-mh-test
"bool [] _assigned_V = new bool[_x.V.Length];
for (int _9idx = 0; _9idx < _assigned_V.Length; ++_9idx) {
    Assert.IsFalse(_assigned_V[_9idx], \"V[{0}] assigned\", _9idx);
    _assigned_V[_9idx] = true;
}
BMC.DrawMVNorm(_x.V, MU, _x.SIGMA);
"
    :outer-lets '()
    :rel '(:metropolis-hastings
	   :lets ()
	   :proposal-distribution (~ v (dmvnorm mu sigma))
	   :log-acceptance-ratio 0)
    :class-vars '(v sigma)
    :dims '((v . 1))))

  (with-genvar-counter 34
  (write-tivu-mh-test
"BMatrix _assigned_Lambda = new BMatrix(_x.Lambda.NBRows, _x.Lambda.NBCols);
for (int _34idx = 0; _34idx < _assigned_Lambda.NBRows; ++_34idx) {
    for (int _35idx = 0; _35idx < _assigned_Lambda.NBCols; ++_35idx) {
        Assert.IsFalse(_assigned_Lambda[_34idx, _35idx], \"Lambda[{0}, {1}] assigned\", _34idx, _35idx);
        _assigned_Lambda[_34idx, _35idx] = true;
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
    :dims '((|Lambda| . 2))))

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

  (with-genvar-counter 12
  (write-tivu-mh-test
"bool [] _assigned_Z = new bool[_x.Z.Length];
bool [] _assigned_Y = new bool[_x.Y.Length];
var Z0 = BMC.Copy(_x.Z);
{
    var Y0 = BMC.Copy(_x.Y);
    for (int _12idx = 0; _12idx < _assigned_Z.Length; ++_12idx) {
        Assert.IsFalse(_assigned_Z[_12idx], \"Z[{0}] assigned\", _12idx);
        _assigned_Z[_12idx] = true;
    }
    BMC.DrawMVNorm(_x.Z, Z0, SIGMAZ);
    for (int _13idx = 0; _13idx < _assigned_Y.Length; ++_13idx) {
        Assert.IsFalse(_assigned_Y[_13idx], \"Y[{0}] assigned\", _13idx);
        _assigned_Y[_13idx] = true;
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
    :dims '((y . 1) (z . 1))))

  ;; Check that the scope of a local variable (sigma) defined in the proposal
  ;; distribution does not include the computation of the acceptance ratio.
  (with-genvar-counter 6
  (write-tivu-mh-test
"bool _assigned_y = false;
var _6save_y = BMC.Copy(_x.y);

{
    var sigma = _x.alpha / Math.Sqrt(lambda);
    Assert.IsFalse(_assigned_y, \"y assigned\");
    _assigned_y = true;
    _x.y = BMC.DrawNorm(0, sigma);
}
double _lar = BMC.Sqr(_x.y / sigma);

if (!BMC.Accept(_lar)) {
    _x.y = _6save_y;
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
    :dims '((|y| . 0))))

  ;; Check that the scope of a local variable (sigma) defined in the proposal
  ;; distribution does not include the computation of the acceptance ratio.
  (with-genvar-counter 17
  (write-tivu-mh-test
"bool _assigned_y = false;
var m = -2;
var _17save_y = BMC.Copy(_x.y);

{
    var sigma = _x.alpha / Math.Sqrt(lambda);
    Assert.IsFalse(_assigned_y, \"y assigned\");
    _assigned_y = true;
    _x.y = BMC.DrawNorm(m, sigma);
}
double _lar = BMC.Sqr((_x.y - m) / sigma);
Assert.IsTrue(BMC.Equal(lambda, BMC.Sqr(_x.z)), \"lambda should not change\");

if (!BMC.Accept(_lar)) {
    _x.y = _17save_y;
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
    :dims '((|y| . 0))))

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

  (with-genvar-counter 24
  (write-tivu-mh-test
"bool _assigned_U = false;
var _24save_U = BMC.Copy(_x.U);

Assert.IsFalse(_assigned_U, \"U assigned\");
_assigned_U = true;
_x.U = BMC.DrawNorm(MU, _x.SIGMA);
double _lar = _x.SIGMA * (_x.U - MU);

if (!BMC.Accept(_lar)) {
    _x.U = _24save_U;
}
"
    :outer-lets '()
    :rel '(:metropolis-hastings
        :lets ()
        :proposal-distribution (~ u (dnorm mu sigma))
        :log-acceptance-ratio (* sigma (- u mu)))
    :class-vars '(u sigma)
    :dims '((u . 0))))

  (with-genvar-counter 10
  (write-tivu-mh-test
"bool _assigned_Z = false;
var MU = 2 * _x.M;
var _10save_Z = BMC.Copy(_x.Z);

Assert.IsFalse(_assigned_Z, \"Z assigned\");
_assigned_Z = true;
_x.Z = BMC.DrawNorm(MU, _x.SIGMA);
double _lar = _x.SIGMA * (_x.Z - MU);

if (!BMC.Accept(_lar)) {
    _x.Z = _10save_Z;
}
"
   :outer-lets '()
   :rel '(:metropolis-hastings
	  :lets ((mu (* 2 m)))
	  :proposal-distribution (~ z (dnorm mu sigma))
	  :log-acceptance-ratio (* sigma (- z mu)))
   :class-vars '(z m sigma)
   :dims '((z . 0))))

  (with-genvar-counter 93
  (write-tivu-mh-test
"bool [] _assigned_Z = new bool[_x.Z.Length];
bool _assigned_W = false;
var F = _x.Z[I - 1];
var SIGMA2 = F * F;
var _93save_Z = BMC.Copy(_x.Z[I - 1]);
var _94save_W = BMC.Copy(_x.W);

Assert.IsFalse(_assigned_Z[I - 1], \"Z[{0}] assigned\", I - 1);
_assigned_Z[I - 1] = true;
_x.Z[I - 1] = BMC.DrawNormTruncated(MU, SIGMA, _x.A, _x.B);
Assert.IsFalse(_assigned_W, \"W assigned\");
_assigned_W = true;
_x.W = BMC.DrawNorm(_x.Z[I - 1], 1);
double _lar = _x.Z[I - 1] + SIGMA2;

if (!BMC.Accept(_lar)) {
    _x.Z[I - 1] = _93save_Z;
    _x.W = _94save_W;
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
    :dims '((z . 1) (w . 0))))

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

  (with-genvar-counter 74
  (write-tivu-mh-test
"bool [] _assigned_S = new bool[_x.S.Length];
var S0 = _x.S[R - 1];
var _74save_S = BMC.Copy(_x.S[R - 1]);

Assert.IsFalse(_assigned_S[R - 1], \"S[{0}] assigned\", R - 1);
_assigned_S[R - 1] = true;
_x.S[R - 1] = BMC.DrawCat(Q);
double _lar = _x.P[_x.S[R - 1] - 1] - _x.P[S0 - 1];
Assert.IsTrue(BMC.Equal(Q0, BMC.ArrTimes(_x.A, _x.B)), \"Q0 should not change\");
Assert.IsTrue(BMC.Equal(Q, BMC.ScalarTimesArr(BMC.Inv(BMC.Sum(Q0)), Q0)), \"Q should not change\");

if (!BMC.Accept(_lar)) {
    _x.S[R - 1] = _74save_S;
}
"
    :outer-lets '((Q0 . (@* A B)) (Q . ($* (^-1 (sum Q0)) Q0)))
    :rel '(:metropolis-hastings
	   :lets ((s0 (@ s r)))
	   :proposal-distribution (~ (@ s r) (dcat Q))
	   :log-acceptance-ratio (- (@ p (@ s r)) (@ p s0)))
    :class-vars '(s A B p)
    :dims '((S . 1))))
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
"protected virtual void AcceptanceMonitorFOO(bool _accepted, double X) { }
protected virtual void AcceptanceMonitorBAR(bool _accepted, int N, double Y) { }
protected virtual void AcceptanceMonitorBAZ(bool _accepted) { }
"
    (ppstr (compile::write-csharp-acceptmons-methods
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
    double _ljd = 0.0;
    {
        var ALPHA_P = BMC.Vec(1.5e+0, 2.0e+0, 1.0e+0);
        _ljd += BMC.LogDensityDirichlet(P, ALPHA_P);
    }
    for (int i = 1; i <= N; ++i) {
        _ljd += BMC.LogDensityCat(X[i - 1], P);
    }
    _ljd += BMC.LogDensityGamma(LAMBDA, A, B);
    {
        var SIGMA = 1 / Math.Sqrt(LAMBDA);
        for (int i = 1; i <= N; ++i) {
            if (X[i - 1] == 1) {
                _ljd += BMC.LogDensityNorm(Y[i - 1], 0, SIGMA);
            }
            else {
                _ljd += BMC.LogDensityGamma(Y[i - 1], B, A);
            }
        }
    }
    return _ljd;
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

  (with-genvar-counter 18
  (assert-equalp
"public void ZeroAccumulators()
{
    _expectations._N = 0;
    _expectations.ALPHA = 0.0;
    for (int _18idx = 0; _18idx < N; ++_18idx) {
        _expectations.BETA[_18idx] = 0.0;
    }
    for (int _19idx = 0; _19idx < M; ++_19idx) {
        for (int _20idx = 0; _20idx < K; ++_20idx) {
            _expectations.GAMMA[_19idx, _20idx] = 0.0;
        }
    }
}
"
    (ppstr
      (compile::write-csharp-zero-accum
        (sexpr->decls '((alpha real) (beta (real n)) (gamma (real m k))))))))

  (with-genvar-counter 3
  (assert-equalp
"public void Accumulate()
{
    _expectations._N += 1;
    _expectations.ALPHA += A;
    BMC.Check(_expectations.DELTA.Length == DELTA.Length,
              \"_expectations.DELTA.Length == DELTA.Length\");
    for (int _3idx = 0; _3idx < M; ++_3idx) {
        _expectations.DELTA[_3idx] += DELTA[_3idx];
    }
    BMC.Check(_expectations.GAMMA.NBRows == G.NBRows,
              \"_expectations.GAMMA.NBRows == G.NBRows\");
    BMC.Check(_expectations.GAMMA.NBCols == G.NBCols,
              \"_expectations.GAMMA.NBCols == G.NBCols\");
    for (int _4idx = 0; _4idx < N; ++_4idx) {
        for (int _5idx = 0; _5idx < K; ++_5idx) {
            _expectations.GAMMA[_4idx, _5idx] += G[_4idx, _5idx];
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
	     (gamma (real n k) g)))))))

  (with-genvar-counter 28
  (assert-equalp
"public void Accumulate()
{
    _expectations._N += 1;
    _expectations.ALPHA += Math.Exp(A);
    {
        var _28tmp = BMC.MatrixInversePD(LAMBDA);
        BMC.Check(_expectations.SIGMA.NBRows == _28tmp.NBRows,
                  \"_expectations.SIGMA.NBRows == _28tmp.NBRows\");
        BMC.Check(_expectations.SIGMA.NBCols == _28tmp.NBCols,
                  \"_expectations.SIGMA.NBCols == _28tmp.NBCols\");
        for (int _29idx = 0; _29idx < N; ++_29idx) {
            for (int _30idx = 0; _30idx < N; ++_30idx) {
                _expectations.SIGMA[_29idx, _30idx] += _28tmp[_29idx, _30idx];
            }
        }
    }
    {
        var _31tmp = BMC.QVec(1, M, (I => Math.Sqrt(M[I - 1])));
        BMC.Check(_expectations.DELTA.Length == _31tmp.Length,
                  \"_expectations.DELTA.Length == _31tmp.Length\");
        for (int _32idx = 0; _32idx < M; ++_32idx) {
            _expectations.DELTA[_32idx] += _31tmp[_32idx];
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
	     (delta (real m) (:quant qvec i (1 m) (^1/2 (@ m i))))))))))
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

  (with-genvar-counter 44
  (assert-equal
    (strcat-lines
      "{"
      "    var _44lo = M + 1;"
      "    var _45hi = 2 * N;"
      "    var _46buf = BMC.Buffer(V, BMC.Range(_44lo - 1, _45hi));"
      "    BMC.DrawMVNorm(_46buf, M, S);"
      "    BMC.CopyInto(V, BMC.Range(_44lo - 1, _45hi), _46buf);"
      "}")
    (ppstr (compile::write-rel-draw
	    (sexpr->rel '(~ (@ v (:range (+ m 1) (* 2 n))) (dmvnorm m s)))))))

  (with-genvar-counter 7
  (assert-equal
    (strcat-lines
      "{"
      "    var _7idx = I;"
      "    var _8buf = BMC.Buffer(V, _7idx - 1, BMC.FullRange);"
      "    BMC.DrawMVNorm(_8buf, M, S);"
      "    BMC.CopyInto(V, _7idx - 1, BMC.FullRange, _8buf);"
      "}")
    (ppstr (compile::write-rel-draw
	    (sexpr->rel '(~ (@ v i :all) (dmvnorm m s)))))))

  (with-genvar-counter 14
  (assert-equal
    (strcat-lines
      "{"
      "    var _14lo = LO;"
      "    var _15hi = HI;"
      "    var _16buf = BMC.Buffer(X, BMC.FullRange, BMC.Range(_14lo - 1, _15hi));"
      "    BMC.DrawWishart(_16buf, NU, W);"
      "    BMC.CopyInto(X, BMC.FullRange, BMC.Range(_14lo - 1, _15hi), _16buf);"
      "}")
    (ppstr (compile::write-rel-draw
	     (sexpr->rel '(~ (@ x :all (:range lo hi)) (dwishart nu W)))))))

  (let ((compile::*env* (assocs->env '((vars::|lambda| . #trealxn)))))
    (assert-equal
     (strcat-lines
      "{"
      "    double alpha = 4.0e+0;"
      "    double sigma = alpha / Math.Sqrt(lambda);"
      "    x = BMC.DrawNorm(0, sigma);"
      "}")
     (ppstr (compile::write-rel-draw
	     (sexpr->rel '(:let (|alpha| 4.0)
                          (:let (|sigma| (/ |alpha| (^1/2 |lambda|)))
				(~ |x| (dnorm 0 |sigma|)))))))))

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

  (let ((compile::*env* (assocs->env '((vars::foo . #t(realxn 1))))))
    (assert-equal
     (strcat-lines
      "for (int i = M - 1; i <= N + 2; ++i) {"
      "    int j = i - 2;"
      "    double MU = FOO[j - 1];"
      "    X[i - 1] = BMC.DrawNorm(MU, Math.Sqrt(Y[i - 1]));"
      "}")
     (ppstr (compile::write-rel-draw (sexpr->rel
       '(:for |i| ((- m 1) (+ n 2))
	      (:let (|j| (- |i| 2))
		    (:let (mu (@ foo |j|))
			  (~ (@ x |i|) (dnorm mu (^1/2 (@ y |i|))))))))))))

  (let-test-macros
    ((expect (rel gvc => save-lines restore-lines)
       `(let ((ht (compile::make-save-var-hash-table))
	      (rel (sexpr->rel ',rel)))
	  (with-genvar-counter ,gvc
	    (assert-equal
	      (strcat-lines ,@save-lines)
	      (ppstr (compile::write-mh-saves rel ht)))
	    (assert-equal
	      (strcat-lines ,@restore-lines)
	      (ppstr (compile::write-mh-restores rel ht)))))))

    (expect :rel (~ x (dnorm m s))
	    :gvc 5
	    =>
	    ("var _5save_X = BMC.Copy(X);")
	    ("X = _5save_X;"))

    (expect :rel (:block (~ (@ y j) (dgamma a b)) (~ x (dnorm m s)))
	    :gvc 2
	    =>
	    ("var _2save_Y = BMC.Copy(Y[J - 1]);"
	     "var _3save_X = BMC.Copy(X);")
	    ("Y[J - 1] = _2save_Y;"
	     "X = _3save_X;"))

    (expect :rel (:let (m emm) (:let (n 5) (~ v (dcat p))))
	    :gvc 10
	    =>
	    ("var _10save_V = BMC.Copy(V);")
	    ("V = _10save_V;"))

    (expect :rel (:let (foo (+ bar 7))
		   (:let (baz (* bin boop))
		     (:block (~ (@ y i) (dnorm 0 1))
			     (~ (@ w i j) (dgamma 1 1)))))
	    :gvc 8
	    =>
	    ("var _8save_Y = BMC.Copy(Y[I - 1]);"
	     "var _9save_W = BMC.Copy(W[I - 1, J - 1]);")
	    ("Y[I - 1] = _8save_Y;"
	     "W[I - 1, J - 1] = _9save_W;"))
  )

  (let-test-macros
    ((expect-error (rel)
       `(let ((ht (compile::make-save-var-hash-table))
	      (rel (sexpr->rel ',rel)))
	  (assert-error 'error
	    (progn (ppstr (compile::write-mh-saves rel ht))
		   (ppstr (compile::write-mh-restores rel ht)))))))

    (expect-error :rel ; Until I figure out how to handle this case...
      (:let (m j) (:let (n 5) (~ (@ v m n) (dcat p)))))

    (expect-error :rel ; Until I figure out how to handle this case...
      (:let (m j)
	(:let (n 5)
	  (:block (~ (@ v m n) (dcat p))
		  (~ x (dcat q))))))

    (expect-error :rel ; Until I figure out how to handle this case...
      (:for j (k r) (~ (@ w j) (dnorm a b))))

    (expect-error :rel ; Until I figure out how to handle this case...
      (:if test
	   (~ w (dnorm a b))
	   (~ y (dgamma a b)))))

  (with-genvar-counter 22
  (assert-equal
"var _22save_X = BMC.Copy(X);

X = BMC.DrawNorm(MU, SIGMA);
double _lar = SIGMA * (X - MU);

if (!BMC.Accept(_lar)) {
    X = _22save_X;
}
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ()
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :log-acceptance-ratio (* sigma (- x mu))))
     nil))))

  (with-genvar-counter 52
  (assert-equal
"var _52save_X = BMC.Copy(X);

X = BMC.DrawNorm(MU, SIGMA);
double _lar = SIGMA * (X - MU);

bool _accepted = BMC.Accept(_lar);
if (_accepted) {
    this.AcceptanceMonitorAM(true);
}
else {
    X = _52save_X;
    this.AcceptanceMonitorAM(false);
}
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ()
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :acceptmon (am)
		   :log-acceptance-ratio (* sigma (- x mu))))
     nil))))

  (with-genvar-counter 4
  (assert-equal
"var _4save_X = BMC.Copy(X);

X = BMC.DrawNorm(MU, SIGMA);
double _lar = SIGMA * (X - MU);

bool _accepted = BMC.Accept(_lar);
if (_accepted) {
    this.AcceptanceMonitorAM(true, I, A * B);
}
else {
    X = _4save_X;
    this.AcceptanceMonitorAM(false, I, A * B);
}
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ()
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :acceptmon (am i (* a b))
		   :log-acceptance-ratio (* sigma (- x mu))))
     nil))))

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

  (let ((compile::*env*
	  (assocs->env '((vars::m . #trealxn)
			 (vars::sigma . #trealxn)
			 (vars::x . #trealxn)))))
  (with-genvar-counter 11
  (assert-equal
"double MU = 2.0e+0 * M;
var _11save_X = BMC.Copy(X);

X = BMC.DrawNorm(MU, SIGMA);
double _lar = SIGMA * (X - MU);

if (!BMC.Accept(_lar)) {
    X = _11save_X;
}
"
   (ppstr
    (compile::write-rel-draw
     (sexpr->rel '(:metropolis-hastings
		   :lets ((mu (* 2.0 m)))
		   :proposal-distribution (~ x (dnorm mu sigma))
		   :log-acceptance-ratio (* sigma (- x mu))))
     nil)))))

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

  (with-genvar-counter 29
  (assert-equal
"var Y0 = BMC.Copy(Y);
var _29save_Y = BMC.Copy(Y);
var _30save_W = BMC.Copy(W);

{
    var W0 = BMC.Copy(W);
    BMC.DrawMVNorm(Y, Y0, SIGMAY);
    BMC.DrawMVNorm(W, W0, SIGMAW);
}
double _lar = BMC.Dot(Y, Y0);

if (!BMC.Accept(_lar)) {
    Y = _29save_Y;
    W = _30save_W;
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
      nil))))

  (with-genvar-counter 46
  (assert-equal
"for (int I = M; I <= N; ++I) {
    var MU = BMC.Dot(Y, GAMMA);
    var SIGMA = Math.Exp(U * V[I - 1]);
    var F = Z[I - 1];
    var SIGMA2 = F * F;
    var _46save_Z = BMC.Copy(Z[I - 1]);

    Z[I - 1] = BMC.DrawNormTruncated(MU, SIGMA, A, B);
    double _lar = Z[I - 1] + SIGMA2;

    if (!BMC.Accept(_lar)) {
        Z[I - 1] = _46save_Z;
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
      (ppstr (compile::write-rel-draw r)))))

  (assert-equal
    ""
    (ppstr (compile::write-rel-draw (make-relation-skip))))

  (assert-equal
    (strcat-lines
      "public void Draw() { Draw(new string[0]); }"
      ""
      "public void Draw(string[] _omit) {"
      "    bool _omit_ALPHA = Array.IndexOf(_omit, \"ALPHA\") >= 0;"
      "    bool _omit_BETA = Array.IndexOf(_omit, \"BETA\") >= 0;"
      "    bool _omit_GAMMA = Array.IndexOf(_omit, \"GAMMA\") >= 0;"
      ""
      "    <body>"
      "}")
    (ppstr (compile::write-prior-draw '(ALPHA BETA GAMMA) (fn () (fmt "<body>")))))

  (assert-equal 'vars::baz
    (compile::rellhs-main-var
      (sexpr->rellhs 'baz)))
  (assert-equal 'vars::bar
    (compile::rellhs-main-var
      (sexpr->rellhs '(@ bar (+ i 1) (* j 3)))))
  (assert-equal 'vars::bop
    (compile::rellhs-main-var
      (sexpr->rellhs '(@ bop :all (:range m n) k))))

  (assert-equal
    (strcat-lines
      "if (!_omit_Y) {"
      "    Y[I - 1] = BMC.DrawNorm(J + X[I - 1], Z[J - 1] * A[J - 1]);"
      "}")
    (ppstr
      (compile::prior-draw-wrd-stoch
        (sexpr->rellhs '(@ y i))
	(sexpr->distr '(dnorm (+ j (@ x i)) (* (@ z j) (@ a j)))))))

  (assert-equal
    (strcat-lines
      "if (!_omit_Z) {"
      "    Z = BMC.DrawNorm(MU, A);"
      "}")
    (ppstr
      (compile::prior-draw-wrd-stoch
        (sexpr->rellhs 'z)
	(sexpr->distr '(dnorm mu a)))))

  (with-genvar-counter 86
  (assert-equal
    (strcat-lines
      "if (!_omit_Y) {"
      "    {"
      "        var _86idx = I;"
      "        var _87buf = BMC.Buffer(Y, _86idx - 1, BMC.FullRange);"
      "        BMC.DrawMVNorm(_87buf, MU, SIGMA);"
      "        BMC.CopyInto(Y, _86idx - 1, BMC.FullRange, _87buf);"
      "    }"
      "}")
    (ppstr
      (compile::prior-draw-wrd-stoch
       (sexpr->rellhs '(@ y i :all))
       (sexpr->distr '(dmvnorm mu sigma))))))
)

(defun sexpr->rels (se-list) (mapcar #'sexpr->rel se-list))

(define-test var-type-tests
  (assert-equal
    '(vars::a vars::d vars::m vars::e vars::h vars::k)
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

(define-test lift-let-tests
  (macrolet ((expect (enew &key ((:from e)))
	       `(assert-equalp
		  ,(sexpr->expr enew)
		  (compile::expand-true-pred ,(sexpr->expr e)))))
    (expect (:lambda i true) :from %true-pred)
    (expect (+ 1 x) :from (+ 1 x))
    (expect (:lambda v (* v v)) :from (:lambda v (* v v)))
    (expect (:quant qsum i (m n) true (@ a i))
	    :from
	    (:quant qsum i (m n) (@ a i)))
  )
  (macrolet ((expect (enew &key ((:from e)))
	       `(assert-equalp
		  ,(sexpr->expr enew)
		  (compile::lift-let ,(sexpr->expr e)))))

    (expect v :from v)

    (expect 1 :from 1)

    (expect (+ v 3) :from (+ v 3))

    (expect (! f 3) :from (! f 3))

    (expect (:lambda v (* v v)) :from (:lambda v (* v v)))

    (expect (:let (a (+ x 3)) (* a a))
	    :from
	    (:let (a (+ x 3)) (* a a)))

    (expect (:let (x (^1/2 y)) (* (+ a b) (^2 (- z x))))
	    :from
	    (* (+ a b) (:let (x (^1/2 y)) (^2 (- z x)))))

    (expect (:let (x (^2 y))
	      (:let (z (- u v))
                (/ (+ 3 x) (+ 2 z))))
	    :from
	    (/ (:let (x (^2 y)) (+ 3 x))
	       (:let (z (- u v)) (+ 2 z))))

    (expect (:let (a 1)
	      (:let (b 2)
		(= 3 (+ a b))))
	    :from
	    (= 3 (:let (a 1) (:let (b 2) (+ a b)))))

    (expect (:let (z (^ y 3))
	      (:let (a (+ (* u v) (/ (+ c z) z)))
		(* a a)))
	    :from
	    (:let (a (+ (* u v)
			(:let (z (^ y 3)) (/ (+ c z) z))))
	      (* a a)))

    (expect (:let (z (^ y 3))
	      (:let (a (+ (* u v) (/ (+ c z) z)))
	        (:let (b (- a v 1))
        	  (^ a b))))
	    :from
	    (:let (a (+ (* u v)
			(:let (z (^ y 3)) (/ (+ c z) z))))
	      (:let (b (- a v 1))
		(^ a b))))

    (expect (:lambda v
	      (:let (b (* a v))
                (+ a (* b b))))
	    :from
	    (:lambda v (+ a (:let (b (* a v)) (* b b))))))

  ; x names two different bound let variables
  (assert-error 'error
    (compile::lift-let
      #e(+ (:let (x 3) (* x x)) (:let (x u) (+ u u)))))

  ; x names both a bound and unbound variable
  (assert-error 'error
    (compile::lift-let
      #e(+ (:let (x 3) (* x x)) (+ x u))))

  (macrolet ((expect (result &key ((:from arg)))
	       `(assert-equalp ,(sexpr->expr result)
			       (compile::uniquely-rename-bound-vars
				 ,(sexpr->expr arg)))))
    (expect x :from x)
    (expect 3 :from 3)
    (expect (+ u v) :from (+ u v))

    (with-genvar-counter 8
      (expect (+ (:let (_8x 3) (* _8x _8x)) (:let (_9y 4) (exp _9y)))
	      :from
	      (+ (:let (x 3) (* x x)) (:let (y 4) (exp y)))))

    (with-genvar-counter 5
      (expect (+ (:let (_5x 3) (* _5x _5x)) (:let (_6x 4) (exp _6x)))
	      :from
	      (+ (:let (x 3) (* x x)) (:let (x 4) (exp x)))))

    (with-genvar-counter 31
      (expect (+ (:let (_31x 3) (* _31x _31x)) (:let (_32x 4) (+ _32x x1)))
	      :from
	      (+ (:let (x 3) (* x x)) (:let (x 4) (+ x x1)))))

    (with-genvar-counter 47
      (expect (+ (:let (_47x 3) (* x1 _47x)) (:let (_48x 4) (exp _48x)))
	      :from
	      (+ (:let (x 3) (* x1 x)) (:let (x 4) (exp x)))))

    (with-genvar-counter 4
      (expect (+ x (:let (_4x 3) _4x) (:let (_5y 4) _5y) y)
	      :from
	      (+ x (:let (x 3) x) (:let (y 4) y) y)))

    (with-genvar-counter 11
      (expect (+ (:quant qsum _11i (m n) (@ v _11i))
	         (:quant qprod _12i (m n) (< (@ x _12i) 3) (@ w _12i)))
	      :from
              (+ (:quant qsum i (m n) (@ v i))
	         (:quant qprod i (m n) (< (@ x i) 3) (@ w i))))))

  (macrolet ((expect (enew &key ((:from e)))
	       `(assert-equalp
                  ,(sexpr->expr enew)
                  (compile::lettify-quant ,(sexpr->expr e)))))

    (expect h :from h)
    (expect 3 :from 3)
    (expect (* a (^2 b)) :from (* a (^2 b)))

    (expect (:let (x (* a a)) (+ x y))
	    :from
	    (:let (x (* a a)) (+ x y)))

    (expect (:let (q (:quant qsum i (m n) true (@ a i))) q)
	    :from
	    (:quant qsum i (m n) true (@ a i)))

    (expect (+ x (:let (q (:quant qsum i (m n) true (@ a i))) q))
	    :from
	    (+ x (:quant qsum i (m n) true (@ a i))))

    (expect (:let (x 3) (:let (q (:quant qand i (j k) true (< (@ a i) x))) q))
	    :from
	    (:let (x 3) (:quant qand i (j k) true (< (@ a i) x))))

    (expect (:let (x 3) (or w (:let (q (:quant qand i (j k) true (< (@ a i) x)))
				 q)))
	    :from
	    (:let (x 3) (or w (:quant qand i (j k) true (< (@ a i) x)))))

    (expect (:let (x (:quant qprod i (m n) true (@ a i))) (^ x x))
	    :from
	    (:let (x (:quant qprod i (m n) true (@ a i))) (^ x x)))

    (expect (:let (x (* a (:let (q (:quant qsum i (m n) true (@ a i))) q)))
	       (+ x x))
	    :from
	    (:let (x (* a (:quant qsum i (m n) true (@ a i))))
	       (+ x x)))

    (expect (:let (q (:quant qsum i (m n) true
		       (+ (:let (q (:quant qprod j (a b) true (@ c i j))) q)
			  (@ d i))))
		  q)
	    :from
	    (:quant qsum i (m n) true
              (+ (:quant qprod j (a b) true (@ c i j))
		 (@ d i))))

    (expect (* m
	       (:let (q (:quant qsum i (m n) true
	                  (+ (:let (q (:quant qprod j (a b) true (@ c i j))) q)
			     (@ d i))))
		 q))
	    :from
	    (* m (:quant qsum i (m n) true
		   (+ (:quant qprod j (a b) true (@ c i j))
		      (@ d i)))))

    ; let var is in scope only in body, not in RHS of definition
    (expect (+ v (:let (q (:quant qand i (q r) true (@ a i))) q))
	    :from
	    (+ v (:quant qand i (q r) true (@ a i))))

    ; let var is in scope only in body, not in RHS of definition
    (expect (+ v (:let (q (:quant qand i (r q) true (@ a i))) q))
	    :from
	    (+ v (:quant qand i (r q) true (@ a i))))

    ; let var is in scope only in body, not in RHS of definition
    (expect (+ v (:let (q (:quant qand i (m n) true (@ q i))) q))
	    :from
	    (+ v (:quant qand i (m n) true (@ q i))))

    (expect (+ (:let
                 (q (:quant qsum i (m n) true
                      (:let
                        (q (:quant qprod j (mm nn) true
			     (* (@ b i j)
                                (:let (q (:quant qmax k (1 3) true (@ v i j k)))
                                  q))))
			q)))
		 q)
	       y)
	    :from
	    (+ (:quant qsum i (m n) true
		 (:quant qprod j (mm nn) true
		    (* (@ b i j) (:quant qmax k (1 3) true (@ v i j k)))))
	       y))
  )

  ; integration test
  (macrolet ((expect (enew &key ((:from e)))
	       `(assert-equalp
                  ,(sexpr->expr enew)
                  (compile::lift-let-and-quant ,(sexpr->expr e)))))
    (with-genvar-counter 16
      (expect (:let (_16lhs (^2 a))
                (* u (/ _16lhs b)))
	      :from
	      (* u (:let (lhs (^2 a)) (/ lhs b)))))

    (with-genvar-counter 51
      (expect (:let (_51q (:quant qsum _52j (m n) true (@ a _52j))) _51q)
	      :from
	      (:quant qsum j (m n) (@ a j))))

    (with-genvar-counter 34
      (expect (:let (_34a (* u i))
                (:let (_35i (+ _34a j k))
                  (/ _34a _35i)))
	      :from
              (:let (a (* u i))
                (:let (i (+ a j k))
                  (/ a i)))))

    (with-genvar-counter 21
      (expect 
        (:let (_21q (:quant qsum _22i (m n) true
		      (:let (_23q (:quant qprod _24j (mm nn) true
                                    (@ a _22i _24j)))
                        _23q)))
	  (:let (_25y (:quant qmax _28i (beg end) (@ b _28i) (@ p _28i)))
            (:let (_26q (:quant qmin _27j (foo bar) true
                          (+ (@ baz _27j) (@ bop _27j))))
              (+ x
	         (/ q 3)
	         _21q
	         (* _25y _26q)
	         (^2 y)))))
	:from
	(+ x
	   (/ q 3)
	   (:quant qsum i (m n)
		   (:quant qprod j (mm nn) (@ a i j)))
	   (:let (y (:quant qmax i (beg end) (@ b i) (@ p i)))
		 (* y (:quant qmin j (foo bar) (+ (@ baz j) (@ bop j)))))
	   (^2 y)))))
)

(define-test write-let-assignment-tests
  (loop for (typ . str) in
	'((#trealxn . "double")
	  (#t(realxn 1) . "double[]")
	  (#t(realxn 2) . "DMatrix")
	  (#tinteger . "int")
	  (#t(integer 1) . "int[]")
	  (#t(integer 2) . "IMatrix")
	  (#tboolean . "bool")
	  (#t(boolean 1) . "bool[]")
	  (#t(boolean 2) . "BMatrix"))
    do
    (assert-equal str (compile::bare-type->string typ)))

  (let ((env (assocs->env '((vars::z . #trealxn) (vars::zi . #tinteger)
			    (vars::x . #trealxn) (vars::xi . #tinteger)
			    (vars::y . #trealxn) (vars::yi . #tinteger)
			    (vars::a . #trealxn) (vars::ai . #tinteger)
			    (vars::b . #trealxn) (vars::bi . #tinteger)
			    (vars::xvec . #t(realxn 1))
			    (vars::xivec . #t(integer 1))
			    (vars::xmat . #t(realxn 2))
			    (vars::zmat . #t(realxn 2))
			    (vars::n . #tinteger)
			    (vars::i . #tinteger)
			    (vars::j . #tinteger)))))
    (macrolet ((expect (var expr arrow &body strings)
	         (assert (eq '=> arrow))
	         `(assert-equal
		   (strcat-lines ,@strings)
		   (ppstr
		    (compile::write-let-assignment
		     ',var (sexpr->expr ',expr) env)))))
      (expect foo 1 =>
	      "int FOO = 1;")

      (expect |_tmp| (+ z (* x y)) =>
	      "double _tmp = Z + X * Y;")

      (with-genvar-counter 3
        (expect bar (+ y (:let (v (+ a b)) (* v v))) =>
	      "double BAR;"
	      "{"
	      "    double _3V = A + B;"
	      "    BAR = Y + _3V * _3V;"
	      "}"))

      (with-genvar-counter 8
        (expect baz (+ yi (:let (v (+ ai bi)) (* v v))) =>
	      "int BAZ;"
	      "{"
	      "    int _8V = AI + BI;"
	      "    BAZ = YI + _8V * _8V;"
	      "}"))

      (with-genvar-counter 73
        (expect foo (:let (yi (* a b)) (* 2.0 yi)) =>
	      "double FOO;"
	      "{"
	      "    double _73YI = A * B;"
              "    FOO = 2.0e+0 * _73YI;"
              "}"))

      (with-genvar-counter 26
        (expect bar (:let (i (+ xi 3))
                    (:let (j (- yi 2))
		      (* (@ xmat i i) (@ zmat j j)))) =>
	     "double BAR;"
	     "{"
             "    int _26I = XI + 3;"
             "    int _27J = YI - 2;"
             "    BAR = XMAT[_26I - 1, _26I - 1] * ZMAT[_27J - 1, _27J - 1];"
             "}"))

      (with-genvar-counter 12
      	(expect baz (:quant qsum idx ((+ ai 1) bi) (@ xivec idx)) =>
      	"int BAZ;"
      	"{"
      	"    int _12Q = 0;"
      	"    int _14last = BI;"
      	"    for (int _13IDX = AI + 1; _13IDX <= _14last; ++_13IDX) {"
      	"        _12Q = _12Q + XIVEC[_13IDX - 1];"
      	"    }"
      	"    BAZ = _12Q;"
      	"}"))

      (with-genvar-counter 7
      	(expect foo (:quant qsum idx (ai bi)
      	              (< 0.0 (@ xvec idx)) (@ xivec idx)) =>
      	"int FOO;"
      	"{"
      	"    int _7Q = 0;"
      	"    int _9last = BI;"
      	"    for (int _8IDX = AI; _8IDX <= _9last; ++_8IDX) {"
      	"        if (0.0e+0 < XVEC[_8IDX - 1]) {"
      	"            _7Q = _7Q + XIVEC[_8IDX - 1];"
      	"        }"
      	"    }"
      	"    FOO = _7Q;"
      	"}"))

      (with-genvar-counter 5
      	(expect bar (:quant qprod idx (1 ai) (@ xvec idx)) =>
      	"double BAR;"
      	"{"
      	"    double _5Q = 1.0e+0;"
      	"    int _7last = AI;"
      	"    for (int _6IDX = 1; _6IDX <= _7last; ++_6IDX) {"
      	"        _5Q = _5Q * XVEC[_6IDX - 1];"
      	"    }"
      	"    BAR = _5Q;"
      	"}"))

      (with-genvar-counter 23
      	(expect baz (:quant qnum ix (2 bi) (< (@ xvec ix) 0.0)) =>
      	"int BAZ;"
      	"{"
      	"    int _23Q = 0;"
      	"    int _25last = BI;"
      	"    for (int _24IX = 2; _24IX <= _25last; ++_24IX) {"
      	"        _23Q = _23Q + Convert.ToInt32(XVEC[_24IX - 1] < 0.0e+0);"
      	"    }"
      	"    BAZ = _23Q;"
      	"}"))

      (with-genvar-counter 6
      	(expect foo (:quant q@sum ix (1 j) (= 2 (@ xivec ix))
      	              (@ xmat ix :all) :shape (n))
      		      =>
      	"double[] FOO;"
      	"{"
      	"    double[] _6Q = new double[N];"
      	"    int _8last = J;"
      	"    for (int _7IX = 1; _7IX <= _8last; ++_7IX) {"
      	"        if (2 == XIVEC[_7IX - 1]) {"
      	"            _6Q = BMC.ArrPlus(_6Q, BMC.ArraySlice(XMAT, _7IX - 1, BMC.FullRange));"
      	"        }"
      	"    }"
      	"    FOO = _6Q;"
      	"}"))

      (with-genvar-counter 6
      	(expect foo (:quant q@sum ix (1 j) (= 2 (@ xivec ix))
      	              (@ xmat (:range ai bi) :all) :shape (i n))
      		      =>
      	"DMatrix FOO;"
      	"{"
      	"    DMatrix _6Q = new DMatrix(I, N);"
      	"    int _8last = J;"
      	"    for (int _7IX = 1; _7IX <= _8last; ++_7IX) {"
      	"        if (2 == XIVEC[_7IX - 1]) {"
      	"            _6Q = BMC.ArrPlus(_6Q, BMC.ArraySlice(XMAT, BMC.Range(AI - 1, BI), BMC.FullRange));"
      	"        }"
      	"    }"
      	"    FOO = _6Q;"
      	"}"))
      (setq compile::*fdebug-trace* nil)

      (with-genvar-counter 12
      	(expect bar (:quant qvec idx (j n) (* (real idx) (^2 (@ xvec idx))))
      		      =>
      	"double[] BAR;"
      	"{"
      	"    int _14first = J;"
      	"    int _15last = N;"
      	"    double[] _12Q = new double[_15last - _14first + 1];"
      	"    for (int _13IDX = _14first; _13IDX <= _15last; ++_13IDX) {"
      	"        _12Q[_13IDX - _14first] = Convert.ToDouble(_13IDX) * BMC.Sqr(XVEC[_13IDX - 1]);"
      	"    }"
      	"    BAR = _12Q;"
      	"}"))

      (with-genvar-counter 6
	(expect foo (:quant qmax idx (1 n) (@ xivec idx))
		=>
	"int FOO;"
	"{"
	"    int _6Q = Int32.MinValue;"
	"    int _8last = N;"
	"    for (int _7IDX = 1; _7IDX <= _8last; ++_7IDX) {"
	"        _6Q = Math.Max(_6Q, XIVEC[_7IDX - 1]);"
        "    }"
	"    FOO = _6Q;"
        "}"))

      (with-genvar-counter 6
	(expect foo (:quant qmax idx (1 n) (@ xvec idx))
		=>
	"double FOO;"
	"{"
	"    double _6Q = double.NegativeInfinity;"
	"    int _8last = N;"
	"    for (int _7IDX = 1; _7IDX <= _8last; ++_7IDX) {"
	"        _6Q = Math.Max(_6Q, XVEC[_7IDX - 1]);"
        "    }"
	"    FOO = _6Q;"
        "}"))

      (with-genvar-counter 6
	(expect foo (:quant qmin idx (1 n) (@ xivec idx))
		=>
	"int FOO;"
	"{"
	"    int _6Q = Int32.MaxValue;"
	"    int _8last = N;"
	"    for (int _7IDX = 1; _7IDX <= _8last; ++_7IDX) {"
	"        _6Q = Math.Min(_6Q, XIVEC[_7IDX - 1]);"
        "    }"
	"    FOO = _6Q;"
        "}"))

      (with-genvar-counter 6
	(expect foo (:quant qmin idx (1 n) (@ xvec idx))
		=>
	"double FOO;"
	"{"
	"    double _6Q = double.PositiveInfinity;"
	"    int _8last = N;"
	"    for (int _7IDX = 1; _7IDX <= _8last; ++_7IDX) {"
	"        _6Q = Math.Min(_6Q, XVEC[_7IDX - 1]);"
        "    }"
	"    FOO = _6Q;"
        "}"))

      (with-genvar-counter 34
      	(expect baz
      		(:let (tmp (* x y))
      		  (+ (:quant qsum i (1 ai)
      	               (:quant qsum j (xi yi) (@ xmat i j)))
      		     (:quant qprod i (2 bi)
      		       (:let (tmp (@ xvec i))
      			 (* tmp tmp)))
      		     tmp))
      		=>
      	"double BAZ;"
      	"{"
      	"    double _34TMP = X * Y;"
      	"    double _35Q = 0.0e+0;"
      	"    int _42last = AI;"
      	"    for (int _36I = 1; _36I <= _42last; ++_36I) {"
      	"        {"
      	"            double _43Q = 0.0e+0;"
      	"            int _45last = YI;"
      	"            for (int _44J = XI; _44J <= _45last; ++_44J) {"
      	"                _43Q = _43Q + XMAT[_36I - 1, _44J - 1];"
      	"            }"
      	"            _35Q = _35Q + _43Q;"
      	"        }"
      	"    }"
      	"    double _39Q = 1.0e+0;"
      	"    int _46last = BI;"
      	"    for (int _40I = 2; _40I <= _46last; ++_40I) {"
      	"        {"
      	"            double _47TMP = XVEC[_40I - 1];"
      	"            _39Q = _39Q * (_47TMP * _47TMP);"
      	"        }"
      	"    }"
      	"    BAZ = _35Q + _39Q + _34TMP;"
      	"}"))
; TODO: more quantifiers
      ))
)

;;; *** DID THIS ONE ***
(define-test initial-environment-tests
  (macrolet ((expect (d arrow a)
	       (assert (eq '=> arrow))
	       `(assert-equalp
		  ',a
		  (compile::decl->assoc (sexpr->decl ',d)))))
    (expect (x integer) => (vars::x . #tinteger))
    (expect (x1 integerp) => (vars::x1 . #tinteger))
    (expect (x2 integerp0) => (vars::x2 . #tinteger))

    (expect (y realxn) => (vars::y . #trealxn))
    (expect (y1 realx) => (vars::y1 . #trealxn))
    (expect (y2 real) => (vars::y2 . #trealxn))
    (expect (y3 realp0) => (vars::y3 . #trealxn))
    (expect (y4 realp) => (vars::y4 . #trealxn))

    (expect (z boolean) => (vars::z . #tboolean))

    (expect (a (integer k)) => (vars::a . #t(integer 1)))
    (expect (a1 (integerp k m n)) => (vars::a1 . #t(integer 3)))
    (expect (a2 (integerp0 m n)) => (vars::a2 . #t(integer 2)))

    (expect (b (realxn 4 n)) => (vars::b . #t(realxn 2)))
    (expect (b1 (realx 3)) => (vars::b1 . #t(realxn 1)))
    (expect (b2 (real j k 4)) => (vars::b2 . #t(realxn 3)))
    (expect (b3 (realp m)) => (vars::b3 . #t(realxn 1)))
    (expect (b4 (realp0 5 2)) => (vars::b4 . #t(realxn 2)))

    (expect (c (boolean m 3 7)) => (vars::c . #t(boolean 3)))
  )
  (macrolet ((model-with-decls (args vars)
	       `(raw-sexpr->model '(:model (:args ,@args) (:reqs)
	  		                   (:vars ,@vars) (:invs) (:body))))
	     (impl-with-parms (params)
	       `(sexpr->mcimpl '(:mcimpl (:parameters ,@params)
					 (:acceptmons)
					 (:expectations)
					 (:updates))))
	     (expect (model impl arrow assoc-list)
	       (assert (eq '=> arrow))
	      `(assert-equalp
		 ',assoc-list
		 (compile::initial-environment-assocs ,model ,impl))))
    (expect (model-with-decls () ())
	    (impl-with-parms ())
	    => ())
    (expect (model-with-decls ((x integer)) ((y realxn)))
	    (impl-with-parms ((z boolean)))
	    => ((vars::x . #tinteger)
		(vars::y . #trealxn)
		(vars::z . #tboolean)))
    (expect (model-with-decls ((a (boolean n))
                               (b integerp))
			      ((c (real m n))
			       (d (integerp0 3))))
	    (impl-with-parms ((e realp)
                              (f (integer n))
			      (g (realxn b b))))
	    =>
	    ((vars::a . #t(boolean 1))
	     (vars::b . #tinteger)
	     (vars::c . #t(realxn 2))
	     (vars::d . #t(integer 1))
	     (vars::e . #trealxn)
	     (vars::f . #t(integer 1))
	     (vars::g . #t(realxn 2))))
)
)


; TODO: write and test code to verify DAG
; TODO: test for case when model language name and C# name for function
;   are different, or where there is no C# operator corresponding to
;   a binary operator in the model language
; TODO: Extend loading of model arguments to allow use of defaults.
; TODO: Extend loading of model arguments so that some integer parameters
;   can be obtained from the dimensions of the data.
