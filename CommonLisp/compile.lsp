(in-package :compile)

(defconstant lar-var (special-var "lar")) ; log acceptance ratio
(defconstant ljd-var (special-var "ljd")) ; log joint density
(defconstant ljd-old-var (special-var "ljdold"))
(defconstant ljd-new-var (special-var "ljdnew"))
(defconstant lpd-var (special-var "lpd")) ; log proposal density
(defconstant lpd-old-var (special-var "lpdold"))
(defconstant lpd-new-var (special-var "lpdnew"))
(defconstant tol-var (special-var "tol")) ; tolerance
(defconstant accepted-var (special-var "accepted")) ; M-H proposal accepted?
(defconstant omit-param (special-var "omit"))
(defun old-val-var (var) (special-var "old" var))
(defun new-val-var (var) (special-var "new" var))
(defun assigned-var (var) (special-var "assigned" var))
(defun lo-var (var) (special-var "lo" var))
(defun hi-var (var) (special-var "hi" var))
(defun omit-var (var) (special-var "omit" var))

;;; Main function

(defun compile-to-csharp (csharp-name-space class-name mdl impl)
#|
  (let ((stoch-vars (stochastic-vars (model-body mdl))))
    (format t "Stochastic: ~a~%~%" stoch-vars))
  (let* ((e (rels->pdf (model-body mdl)))
	 (e1 (prove::expand-densities e))
	 (e1a (prove::simplify-expr e1))
	 (e2 (prove::eliminate-let-expressions e1a))
	 (e2a (prove::simplify-expr e2))
	 (e3 (prove::expand-products e2a))
	 (e3a (prove::simplify-expr e3))
	 (e4 (funcall (prove::expand-array-lengths (args-vars-dims mdl))
		      e3a))
	 (e4a (prove::simplify-expr e4))
	 (exp e4a))
    (let ((*fmt-ostream* *standard-output*))
      ;(format t "PDF:~%")
      ;(print-product e)
      ;(format t "~%PDFxd:~%")
      ;(print-product e1)
      (format t "~%PDFxp:~%")
      (print-product exp)))
|#
  (write-csharp-class csharp-name-space class-name
		      (lambda ()
			(write-csharp-class-body class-name mdl impl))))

(defun print-product (e)
  (assert (is-expr-apply e))
  (assert (eq '* (expr-apply-fct e)))
  (let ((*indent-amount* 2))
    (print-factors (expr-apply-args e))))

(defun print-factors (factors)
  (dolist (x factors) (print-factor x)))

(defun print-factor (x)
  (adt-case expr x
    ((apply fct args)
     (case fct
       ('* (fmt "*") (indent (print-factors args)))
       ('! (print-let args))
       ('qprod (print-qprod args))
       ('if-then-else (print-if-then-else args))
       (otherwise (fmt "~a" (expr:expr->string x)))))
    (otherwise
     (fmt "~a" (expr:expr->string x)))))

(defun print-let (args)
  (assert (= 2 (length args)))
  (destructuring-bind (f val) args
    (match-adt1 (expr-lambda var body) f
      (let ((var-s (expr:expr->string (expr-var var)))
	    (val-s (expr:expr->string val)))
	(fmt "LET ~a = ~a IN" var-s val-s)
	(indent (print-factor body))))))

(defun print-qprod (args)
  (destructuring-bind (lo hi filter f) args
    (unless (equalp (expr-const '%true-pred) filter)
      (error "Unimplemented: non-trivial filter in print-qprod."))
    (match-adt1 (expr-lambda var body) f
      (let ((lo-s (expr:expr->string lo))
	    (hi-s (expr:expr->string hi)))
	(fmt "QPROD ~a, ~a : ~a" var lo-s hi-s)
	(indent (print-factor body))))))

(defun print-if-then-else (args)
  (destructuring-bind (test exp-t exp-f) args
    (fmt "IF-THEN-ELSE ~a" (expr:expr->string test))
    (indent
      (print-factor exp-t)
      (print-factor exp-f))))

(defmacro bracket (&rest body)
  `(progn
     (fmt "{")
     (indent ,@body)
     (fmt "}")))

(defmacro bracket-end (&rest body)
  `(progn
     (indent ,@body)
     (fmt "}")))

(defmacro bracket-if (test &rest body)
  (let ((main (gensym)))
    `(flet ((,main () ,@body))
       (if ,test (bracket (,main)) (,main)))))

(defun write-csharp-class (csharp-name-space class-name write-body)
  (fmt "using System;")
  (fmt "using Common;")
  (fmt-blank-line)
  (fmt "namespace ~a" csharp-name-space)
  (bracket
    (fmt "[Serializable]")
    (fmt "public class ~a" class-name)
    (bracket
      (funcall write-body))))

;;; Printing expressions

(defun assignable-expr (e)
  (if (is-expr-variable e)
    (expr-call 'copy e)
    e))

(defun expr->string (e &optional (lprec -1) (rprec -1))
  (adt-case expr e
    ((const name)
     (const->string name))
    ((variable symbol)
     (variable->string symbol))
    ((lambda var body)
     (format nil "(~a => ~a)" var (expr->string body)))
    ((apply fct args)
     (aexpr->string fct args lprec rprec))))

(defun const->string (x)
  (cond ((integerp x) (format nil "~d" x))
	((realp x) (format nil "~,,,,,,'eE" x))
	(t (case x
	     ('true "true")
	     ('false "false")
	     ('@-all "BMC.FullRange")
	     ('%pi "Math.PI")
	     ('%e "Math.E")
	     ('%true-pred "(x => true)")
	     ('%infty- "double.NegativeInfinity")
	     ('%infty+ "double.PositiveInfinity")
	     ('%min-int "Int32.MinValue")
	     ('%max-int "Int32.MaxValue")
	     (t (error "Unimplemented case in compile::const->string: ~a."
		       x))))))

(defun default-variable->string (x)
  (let ((s (symbol-name x)))
    (unless (every #'identcharp s)
      (error "Illegal variable name: ~a." s))
    s))

(defparameter *variable->string* #'default-variable->string)

(defun variable->string (x) (funcall *variable->string* x))

(defun identcharp (c) (or (alphanumericp c) (eql #\_ c)))

(defun aexpr->string (fct args lprec rprec)
  (case fct
	('@ (@expr->string args))
	('@-slice (@-slice-expr->string args))
	(otherwise (fexpr->string fct args lprec rprec))))

(defun @expr->string (args)
  (unless (and (consp args) (< 1 (length args)))
    (error "@ expression must have at least two arguments"))
  (format nil "~a[~{~a~^, ~}]"
	 (array-expr->string (first args))
	 (mapcar #'index-expr->string (rest args))
	 #|(mapcar #'expr->string (mapcar #'dec-expr (rest args)))|#))

(defun index-expr->string (x)
  (expr->string (dec-expr x)))

(defun dec-expr (expr)
  (expr-call '- expr (expr-const 1)))

(defun @-slice-expr->string (args)
  (unless (and (consp args) (< 1 (length args)))
    (error "@-slice expression must have at least two arguments"))
  (format nil "BMC.ArraySlice(~a~{, ~a~})"
	  (array-expr->string (first args))
	  (mapcar #'expr->string (mapcar #'slice-dec-expr (rest args)))))

(defun slice-dec-expr (x)
  (cond ((and (is-expr-apply x) (eq '@-rng (expr-apply-fct x)))
	 (destructuring-bind (lo hi) (expr-apply-args x)
	   (expr-call '@-rng (dec-expr lo) hi)))
	((and (is-expr-apply x) (eq '@-idx (expr-apply-fct x)))
	 (destructuring-bind (e) (expr-apply-args x)
	   (dec-expr e)))
	((and (is-expr-const x) (eq '@-all (expr-const-name x)))
	 x)
	(t (error "Unimplementable case in slice-dec-expr."))))

(defun array-expr->string (e)
  (if (or (is-expr-const e) (is-expr-variable e))
    (expr->string e)
    (format t "(~a)" (expr->string e))))

(defun fexpr->string (fct args lprec rprec)
  (setf args (remove-default-filter-if-fquant fct args))
  (when (eq '! fct)
    (setf fct :let)
    (setf args (reverse args)))
  (cond ((is-binop fct)
	 (bexpr->string fct args lprec rprec))
	((eq '@-idx fct)
	 (apply #'expr->string (append args (list lprec rprec))))
	((eq 'if-then-else fct)
	 (let ((test (expr->string (first args)))
	       (true-branch (expr->string (second args)))
	       (false-branch (expr->string (third args))))
	   (format nil "(~a ? ~a : ~a)" test true-branch false-branch)))
	((eq 'real-zero-arr fct)
	 (case (length args)
	       (1 (format nil "new double[~a]" (expr->string (car args))))
	       (2 (format nil "new DMatrix(~a, ~a)"
			  (expr->string (first args))
			  (expr->string (second args))))
	       (otherwise (error "Unimplemented case in fexpr->string."))))
	(t
	 (format nil "~a(~{~a~^, ~})"
		 (fct-name fct) (mapcar #'expr->string args)))))

(defun remove-default-filter-if-fquant (fct args)
  (if (and (is-fquant-symbol fct)
	   (<= 3 (length args))
	   (is-always-true (third args)))
      (list* (first args) (second args) (nthcdr 3 args))
    args))

(defun is-always-true (filter)
  (or (equalp #e%true-pred filter)
      (adt-case expr filter
	((lambda var body) (equalp #etrue body))
	(otherwise nil))))

(defun bexpr->string (op args lprec rprec)
  (let* ((op-prec (precedences op))
	 (op-lprec (car op-prec))
	 (op-rprec (cdr op-prec))
	 (use-parens (or (< op-lprec lprec) (< op-rprec rprec)))
	 (len (length args))
	 (len1 (if (< len 2)
		   (error "Binary operator must have at least two args")
		   (1- len)))
	 (lprec-list (cons (if use-parens -1 lprec)
			   (make-list len1 :initial-element op-rprec)))
	 (rprec-list (append (make-list len1 :initial-element op-lprec)
			     (list (if use-parens -1 rprec))))
	 (aplist (zip lprec-list args rprec-list)))
    (with-output-to-string (s)
      (when use-parens (princ #\( s))
      (destructuring-bind (lpr e rpr) (car aplist)
	 (format s "~a" (expr->string e lpr rpr)))
      (dolist (x (cdr aplist))
	(destructuring-bind (lpr e rpr) x
	   (format s " ~a ~a" (fct-name op) (expr->string e lpr rpr))))
      (when use-parens (princ #\) s)))))

(defun precedences (op) (assoc-lookup op +precedences+))

(defun is-binop (x) (assoc x +precedences+))

(defconstant +precedences+
  '((<  50 . 50) (<= 50 . 50) (= 50 . 50) (!= 50 . 50) (> 50 . 50) (>= 50 . 50)
    (.<  50 . 50) (.<= 50 . 50) (.= 50 . 50) (.!= 50 . 50) (.> 50 . 50) (.>= 50 . 50)
    (and 40 . 41) (or 30 . 31)
    (.and 40 . 41) (.or 30 . 31)
    (+ 100 . 101) (- 100 . 101) (* 110 . 111) (/ 110 . 111)))

(defun fct-name (op)
  (let ((a (assoc op +oper-names+)))
    (when (null a)
      (error "Don't know C# name of function ~a." op))
    (cdr a)))

(defconstant +oper-names+
  '(
    (and . "&&")
    (array-length . "BMC.Length")
    (int . "Convert.ToInt32")
    (real . "Convert.ToDouble")
    (cons . "BMC.Cons")
    (cons-col . "BMC.ConsCol")
    (cons-row . "BMC.ConsRow")
    (diag_mat . "BMC.DiagonalMatrix")
    (dmvnorm-density . "BMC.DensityMVNorm")
    (dot . "BMC.Dot")
    (eigen . "BMC.Eigen")
    (exp . "Math.Exp")
    (fst . "BMC.Fst")
    (copy . "BMC.Copy")
    (inv . "BMC.MatrixInverse")
    (inv-pd . "BMC.MatrixInversePD")
    (is-integerp . "BMC.IsIntegerp")
    (is-integerp0 . "BMC.IsIntegerp0")
    (is-real . "BMC.IsReal")
    (is-realp . "BMC.IsRealp")
    (is-realp0 . "BMC.IsRealp0")
    (is-realx . "BMC.IsRealx")
    (is-symm-pd . "BMC.IsSymmetricPositiveDefinite")
    (:let . "BMC.Let")
    (log . "Math.Log")
    (max . "Math.Max")
    (min . "Math.Min")
    (neg . "-")
    (or . "||")
    (o^2 . "BMC.SelfOuterProduct")
    (q@sum . "BMC.QArrSum")
    (qand . "BMC.ForAll")
    (qmax . "BMC.QMax")
    (qmin . "BMC.QMin")
    (qnum . "BMC.Count")
    (qsum . "BMC.QSum")
    (qvec . "BMC.QVec")
    (rmat . "BMC.RowMatrix")
    (snd . "BMC.Snd")
    (sum . "BMC.Sum")
    (tanh . "Math.Tanh")
    (vec . "BMC.Vec")
    (vmax . "BMC.VMax")

    (+ . "+")
    (- . "-")
    (* . "*")
    (/ . "/")
    (!= . "!=")
    (< . "<")
    (<= . "<=")

    ($* . "BMC.ScalarTimesArr")
    (<=> . "BMC.Iff")
    (= . "==")
    (=> . "BMC.Implies")
    (@* . "BMC.ArrTimes")
    (@/ . "BMC.ArrDivide")
    (@+ . "BMC.ArrPlus")
    (@- . "BMC.ArrMinus")
    (@-idx . "BMC.Idx")
    (@-rng . "BMC.Range")
    (@-slice . "BMC.ArraySlice")
    (@^-1 . "BMC.ArrInv")
    (@^-2 . "BMC.ArrInvSqr")
    (@^2 . "BMC.ArrSqr")
    (^ . "Math.Pow")
    (^-1 . "BMC.Inv")
    (^-1/2 . "BMC.InvSqrt")
    (^-2 . "BMC.InvSqr")
    (^1/2 . "Math.Sqrt")
    (^2 . "BMC.Sqr")

    (.= . "==")
    (.<= . "<=")
    (.< . "<")
    (.> . ">")
    (.>= . ">=")
))

;;; End printing expressions

(defun de-alias-impl (mdl impl)
  (let ((dont-alias (dont-alias-predicate mdl)))
    (flet ((xform-upd (upd)
	     (cons (car upd) (de-alias-relation dont-alias (cdr upd)))))
      (match-adt1 (mcimpl parameters acceptmons expectations updates) impl
        (make-mcimpl :parameters parameters
		     :acceptmons acceptmons
		     :expectations expectations
		     :updates (mapcar #'xform-upd updates))))))

(defun de-alias-relation (dont-alias rel0)
  (flet*
    ((copy-val (e) (expr-call 'copy e))
     (de-alias-let (var-val)
       (destructuring-bind (var . val) var-val
         (if (some dont-alias (aliases val))
	     (cons var (copy-val val))
	   var-val)))
     (de-alias (rel)
       (adt-case relation rel
         ((stochastic lhs rhs)
	  rel)
         ((let var val body)
	  (let ((new-val (if (some dont-alias (aliases val))
			     (copy-val val)
			   val)))
	    (make-relation-let :var var :val new-val :body (de-alias body))))
         ((mh lets proposal-distribution acceptmon log-acceptance-ratio)
	  (make-relation-mh
	   :lets (mapcar #'de-alias-let lets)
	   :proposal-distribution (de-alias proposal-distribution)
	   :acceptmon acceptmon
	   :log-acceptance-ratio log-acceptance-ratio))
	 ((block members)
	  (make-relation-block :members (mapcar #'de-alias members)))
	 ((if condition true-branch false-branch)
	  (make-relation-if
	    :condition condition
	    :true-branch (de-alias true-branch)
	    :false-branch (de-alias false-branch)))
	 ((skip)
	  rel)
	 ((loop var lo hi body)
	  (make-relation-loop :var var
			      :lo lo
			      :hi hi
			      :body (de-alias body)))
      )))
    (de-alias rel0)))

(defun dont-alias-predicate (mdl)
  (let ((dont-alias-vars '()))
    (dolist (d (model-vars mdl))
      (let ((var-typ (decl->assoc d)))
	(unless (is-bare-type-scalar (cdr var-typ))
	  (push (car var-typ) dont-alias-vars))))
    (lambda (v) (member v dont-alias-vars))))

(defun class-vars-from (mdl impl)
  (append (params-names impl) (args-vars-names mdl)))

(defun class-var-pred-from (mdl impl)
  (let ((class-vars (class-vars-from mdl impl)))
    (lambda (v) (member v class-vars))))

(defun vars-in-expr (x)
  (adt-case expr x
    ((const name)
     '())
    ((variable symbol)
     (list symbol))
    ((apply fct args)
     (vars-in-expr-list args))
    ((lambda var body)
     (cons var (vars-in-expr body)))))

(defun vars-in-expr-list (xlist)
  (append-mapcar #'vars-in-expr xlist))

(defun vars-in-model (mdl)
  (match-adt1 (model args reqs vars body) mdl
    (append (append-mapcar #'vars-in-decl args)
	    (append-mapcar #'vars-in-expr reqs)
	    (append-mapcar #'vars-in-decl vars)
	    (append-mapcar #'vars-in-rel body))))

(defun vars-in-decl (d)
  (match-adt1 (decl var typ) d
    (cons var (vars-in-vtype typ))))

(defun vars-in-vtype (typ)
  (adt-case vtype typ
    ((scalar stype)
     '())
    ((array elem-type dims)
     (vars-in-expr-list dims))))

(defun vars-in-rel (rel)
  (adt-case relation rel
    ((stochastic lhs rhs)
     (append (vars-in-rellhs lhs)
	     (vars-in-distr rhs)))
    ((block members)
     (append-mapcar #'vars-in-rel members))
    ((if condition true-branch false-branch)
     (append (vars-in-expr condition)
	     (vars-in-rel true-branch)
	     (vars-in-rel false-branch)))
    ((loop var lo hi body)
     (append (list var)
	     (vars-in-expr lo)
	     (vars-in-expr hi)
	     (vars-in-rel body)))
    ((let var val body)
     (cons var (append (vars-in-expr val) (vars-in-rel body))))
    ((skip)
     '())))

(defun vars-in-rellhs (lhs)
  (adt-case rellhs lhs
    ((simple var)
     (list var))
    ((array-elt var indices)
     (cons var (vars-in-expr-list indices)))
    ((array-slice var indices)
     (cons var (append-mapcar #'vars-in-arr-slice-idx indices)))))

(defun vars-in-arr-slice-idx (x)
  (adt-case array-slice-index x
    ((scalar value)
     (vars-in-expr value))
    ((range lo hi)
     (vars-in-expr-list (list lo hi)))
    ((all)
     '())))

(defun vars-in-distr (d)
  (match-adt1 (distribution name args) d
    (vars-in-expr-list args)))

(defun rellhs-main-var (lhs)
  (adt-case rellhs lhs
    ((simple var) var)
    ((array-elt var indices) var)
    ((array-slice var indices) var)))

(defun write-csharp-class-body (class-name mdl impl)
  (write-csharp-declarations mdl impl)
  (fmt-blank-line)
  (write-csharp-copy class-name
		     (append (params-names impl) (args-vars-names mdl)))
  (fmt-blank-line)
  (write-csharp-load-arguments (model-args mdl))
  (fmt-blank-line)
  (write-csharp-validate-arguments mdl)
  (fmt-blank-line)
  (write-csharp-validate-parameters impl)
  (fmt-blank-line)
  (write-csharp-allocate-vars (model-vars mdl))
  (fmt-blank-line)
  (let* ((exp (mcimpl-expectations impl))
	 (exp-decls (mapcar #'car exp)))
    (write-csharp-allocate-accums exp-decls)
    (fmt-blank-line)
    (write-csharp-zero-accum exp-decls)
    (fmt-blank-line)
    (write-csharp-accumulate exp)
    (fmt-blank-line)
    (write-csharp-output-expectations exp-decls))
  (fmt-blank-line)
  (write-csharp-log-joint-density mdl)
  (fmt-blank-line)
  (let* ((rels (model-body mdl))
	 (vars (vars-names mdl))
	 (prior-draw-body (lambda ()
	 		    (dolist (r rels)
			      (write-rel-draw r t #'prior-draw-wrd-stoch)))))
    (write-prior-draw vars prior-draw-body))
  (fmt-blank-line)
  (let ((*env* (initial-environment mdl impl)))
    (write-csharp-updates (mcimpl-updates impl)))
  (fmt-blank-line)
  ; ... more ...
)

(defparameter *env* nil)

(defmacro extend-env (var typ &body body)
  `(let ((*env* (and *env* (add-var-type *env* ,var ,typ))))
     ,@body))

(defun prior-draw-wrd-stoch (lhs rhs)
  (fmt "if (!~a) {" (omit-var (rellhs-main-var lhs)))
  (bracket-end
   (write-rel-draw-stoch lhs rhs)))

(defun write-prior-draw (model-vars gen-body)
  (fmt "public void Draw() { Draw(new string[0]); }")
  (fmt-blank-line)
  (fmt "public void Draw(string[] ~a) {" omit-param)
  (bracket-end
    (dolist (v model-vars)
      (fmt "bool ~a = Array.IndexOf(~a, \"~a\") >= 0;"
	   (omit-var v) omit-param v))
    (fmt-blank-line)
    (funcall gen-body)))

(defun write-csharp-declarations (mdl impl)
  (fmt "[Serializable]")
  (fmt "public class Accumulators")
  (bracket
    (fmt "public int _N;")
    (gen-decls "" (mapcar #'car (mcimpl-expectations impl))))
  (fmt-blank-line)
  (write-csharp-acceptmons-methods (mcimpl-acceptmons impl))
  (fmt-blank-line)
  (gen-decls "Update parameters" (mcimpl-parameters impl))
  (fmt-blank-line)
  (fmt "// Accumulators")
  (fmt "Accumulators _expectations;")
  (fmt-blank-line)
  (gen-decls "Model Arguments" (model-args mdl))
  (fmt-blank-line)
  (gen-decls "Model Variables" (model-vars mdl)))

(defun write-csharp-copy (class-name vars)
  (fmt "public ~a Copy()" class-name)
  (bracket
    (fmt "~a the_copy = new ~a();" class-name class-name)
    (dolist (v vars)
      (fmt "the_copy.~a = BMC.Copy(this.~a);" v v))
    (fmt "return the_copy;")))

(defun gen-decls (comment decls)
  (fmt "// ~a" comment)
  (dolist (x decls)
    (gen-decl x)))

(defun gen-decl (d)
  (match-adt1 (decl var typ) d
    (fmt "public ~a ~a;" (type-string typ) (variable->string var))))

(defun type-string (typ)
  (adt-case vtype typ
    ((scalar stype)
     (csharp-scalar-type-name stype))
    ((array elem-type dims)
     (csharp-arr-type-name elem-type (length dims)))))

(defun base-type (stype)
  (assoc-lookup stype +base-types-from-scalar-types+))

(defconstant +base-types-from-scalar-types+
  '((boolean . boolean)
    (integer . integer) (integerp0 . integer) (integerp . integer)
    (realxn . realxn) (realx . realxn) (real . realxn)
    (realp0 . realxn) (realp . realxn)))

(defun csharp-scalar-type-name (stype)
  (assoc-lookup (base-type stype) +csharp-names-for-base-types+))

(defconstant +csharp-names-for-base-types+
  '((boolean . "bool") (integer . "int") (realxn . "double")))

(defun csharp-arr-type-name (etype ndim)
  (case ndim
	(1 (format nil "~a[]" (csharp-scalar-type-name etype)))
	(2 (csharp-matrix-type-name etype))
	(otherwise (error "Unimplemented case (ndim=~a) in ~
                           csharp-arr-type-name." ndim))))

(defun csharp-matrix-type-name (typ)
  (assoc-lookup (base-type typ) +csharp-names-for-base-matrix-types+))

(defconstant +csharp-names-for-base-matrix-types+
  '((boolean . "BMatrix") (integer . "IMatrix") (realxn . "DMatrix")))

(defun write-csharp-allocate-vars (var-decls)
  (fmt "public void AllocateModelVariables()")
  (bracket
    (wcs-allocate-vars var-decls)))

(defun wcs-allocate-vars (var-decls &optional prefix)
  (dolist (d var-decls)
    (match-adt1 (decl var typ) d
      (when (is-vtype-array typ)
	(let ((var-s (variable->string var))
	      (new-s (csharp-new-string typ)))
	(if prefix
	    (fmt "~a.~a = ~a;" prefix var-s new-s)
	  (fmt "if (~a == null) ~a = ~a;" var-s var-s new-s)))))))

(defun write-csharp-acceptmons-methods (acceptmons)
  (flet* ((decl2str (d)
	    (match-adt1 (decl var typ) d
              (format nil "~a ~a" (type-string typ) (variable->string var))))
	  (decls2strs (ds) (mapcar #'decl2str ds)))
    (dolist (x acceptmons)
      (destructuring-bind (name . decls) x
	(fmt "protected virtual void AcceptanceMonitor~a(bool ~a~{, ~a~}) { }"
	     name accepted-var (decls2strs decls))))))

(defun write-csharp-allocate-accums (decls)
  (fmt "public void AllocateAccumulators()")
  (bracket
    (fmt "_expectations = new Accumulators();")
    (fmt "_expectations._N = 0;")
    (wcs-allocate-vars decls "_expectations")))

(defun csharp-new-string (typ)
  (adt-case vtype typ
    ((array elem-type dims)
       (cond
	 ((= 1 (length dims))
	  (format nil "new ~a[~a]"
		  (csharp-scalar-type-name elem-type)
		  (expr->string (first dims))))
	 ((= 2 (length dims))
	  (format nil "new ~a(~a, ~a)"
		  (csharp-matrix-type-name elem-type)
		  (expr->string (first dims))
		  (expr->string (second dims))))
	 (otherwise (error "Unimplemented case in csharp-new-string"))))))

(defun write-csharp-load-arguments (arg-decls)
  (fmt "public void LoadArguments(BMC.Loader loader)")
  (bracket
    (dolist (d arg-decls)
      (match-adt1 (decl var typ) d
	(let ((vname (variable->string var))
	      (infix (csharp-load-type-name typ)))
	  (fmt "~a = loader.Load~a(\"~a\");" vname infix vname))))))

(defun type->dim-strings (typ)
  (adt-case vtype typ
    ((scalar stype) '())
    ((array elem-type dims) (mapcar #'expr->string dims))))

(defun csharp-load-type-name (typ)
  (adt-case vtype typ
    ((scalar stype) (csharp-load-scalar-type-name stype))
    ((array elem-type dims)
     (case (length dims)
	   (1 (strcat (csharp-load-scalar-type-name elem-type) "Array"))
	   (2 (csharp-matrix-type-name elem-type))
	   (otherwise
	     (error "Unimplemented case in csharp-load-type-name."))))))

(defun csharp-load-scalar-type-name (stype)
  (assoc-lookup (base-type stype) +csharp-load-type-names+))

(defconstant +csharp-load-type-names+
  '((boolean . "Boolean") (integer . "Integer") (realxn . "Real")))

(defun write-csharp-validate-arguments (mdl)
  (fmt "public void ValidateModelArguments()")
  (bracket
    (dolist (x (args-checks mdl))
      (write-csharp-check x))))

(defun write-csharp-validate-parameters (impl)
  (fmt "public void ValidateUpdateParameters()")
  (bracket
    (dolist (x (params-checks impl))
      (write-csharp-check x))))

(defun write-csharp-check (x)
  (if (is-qand-expr x)
      (write-csharp-check-qand x)
    (let ((bool-expr (expr->string x)))
      (fmt "BMC.Check(~a," bool-expr)
      (fmt "          \"~a\");" bool-expr))))

(defun is-qand-expr (x)
  (adt-case expr x
    ((apply fct args)
     (eq 'qand fct))
    (otherwise nil)))

(defun write-csharp-check-qand (x)
  (match-adt1 (expr-apply fct args) x
    (destructuring-bind (lo hi filter pred) args
      (let ((var (expr-lambda-var pred))
	    (body (expr-lambda-body pred)))		
	(fmt "for (int ~a = ~a; ~a <= ~a; ++~a) {"
	     var (expr->string lo) var (expr->string hi) var)
	(bracket-end
	  (if (equalp (expr-const '%true-pred) filter)
	      (write-csharp-check body)
	    (progn
	      (unless (eq var (expr-lambda-var filter))
		(error "Invalid QAND expression: ~a." x))
	      (fmt "if (~a) {" (expr->string (expr-lambda-body filter)))
	      (bracket-end
	        (write-csharp-check body)))))))))

(defun write-csharp-updates (updates)
  (fmt "public void Update()")
  (bracket
    (dolist (x updates)
      (fmt "Update_~a();" (car x))))
  (dolist (x updates)
    (fmt-blank-line)
    (destructuring-bind (name . upd) x
      (fmt "public void Update_~a()" name)
      (bracket
        (write-rel-draw upd nil)))))

(defun args-checks (mdl)
  (remove-if #'is-trivially-true
	     (remove-duplicates (args-checks-raw mdl) :test #'equalp)))

(defun params-checks (impl)
  (let ((checks0 (append-mapcar #'decl-checks (mcimpl-parameters impl))))
    (remove-if #'is-trivially-true (remove-duplicates checks0))))

(defun is-trivially-true (e)
  (adt-case expr e
    ((const name)
     (eq 'true name))
    ((apply fct args)
     (is-trivially-true-app fct args))
    (otherwise nil)))

(defun is-trivially-true-app (fct args)
  (and (eq 'is-integerp0 fct) (= 1 (length args))
       (adt-case expr (first args)
	 ((const name)
	  (and (integerp name) (<= 0 name)))
	 (otherwise nil))))

(defun args-checks-raw (mdl)
  (append (append-mapcar #'decl-checks (model-args mdl))
	  (model-reqs mdl)
	  (append-mapcar #'dim-checks (model-vars mdl))))

(defun dim-checks (d)
  (adt-case vtype (decl-typ d)
    ((scalar stype) '())
    ((array elem-type dims)
     (mapcar (lambda (e) (expr-call 'is-integerp0 e)) dims))))

(defun decl-checks (d)
  (match-adt1 (decl var typ) d
    (adt-case vtype typ
      ((scalar stype) (scalar-type-checks (expr-var var) stype))
      ((array elem-type dims) (array-type-checks var elem-type dims)))))

(defun scalar-type-checks (expr styp)
  (let ((x (assoc styp +scalar-base-type-checks+)))
    (if (null x)
      '()
      (list (apply-template (cdr x) expr)))))

(defconstant +scalar-base-type-checks+
  '((integerp0 <= 0) (integerp < 0) (realx is-realx)
    (real is-real) (realp0 is-realp0) (realp is-realp)))

(defun apply-template (template x)
  (destructuring-bind (fct-sym . args1) template
    (let ((eargs1 (mapcar #'sexpr->expr args1)))
      (expr-app fct-sym (append eargs1 (list x))))))

(defun array-type-checks (var-sym etype-sym dims)
  (append (array-length-checks var-sym dims)
	  (array-element-checks var-sym etype-sym dims)))

(defun array-length-checks (var-sym dims)
  (let ((vexpr (expr-var var-sym)))
    (mapcar (lambda (n d)
	      (expr-call '=
		(expr-call 'array-length (expr-const n) vexpr)
		d))
	    (int-range 1 (length dims)) dims)))

(defun array-element-checks (var-sym etype-sym dims)
  (let* ((idxvar-symbols (n-new-vars (length dims) "i"))
	 (idxvars (mapcar #'expr-var idxvar-symbols))
	 (var (expr-var var-sym))
	 (checks (scalar-type-checks
		   (expr-app '@ (cons var idxvars))
		   etype-sym)))
    (mapcar (lambda (ch) (array-element-check ch dims idxvar-symbols))
	    checks)))

(defun array-element-check (ch dims idxvars)
  (let ((di-list (reverse (zip dims idxvars))))
    (dolist (x di-list)
      (destructuring-bind (dim idxvar) x
	(setf ch (expr-call 'qand (expr-const 1) dim
			    (expr-const '%true-pred) (expr-lam idxvar ch)))))
    ch))

(defun model-variables-assigned-in (rel)
  (adt-case relation rel
    ((stochastic lhs)
     (model-vars-assigned-in-rellhs lhs))
    ((block members)
     (append-mapcar #'model-variables-assigned-in members))
    ((if condition true-branch false-branch)
     (append (model-variables-assigned-in true-branch)
	     (model-variables-assigned-in false-branch)))
    ((loop var lo hi body)
     (model-variables-assigned-in body))
    ((let var val body)
     (model-variables-assigned-in body))
    ((skip)
     '())
    ((mh lets proposal-distribution log-acceptance-ratio)
     (model-variables-assigned-in proposal-distribution))))

(defun model-vars-assigned-in-rellhs (lhs)
  (adt-case rellhs lhs
   ((simple var)
    (list var))
   ((array-elt var indices)
    (list var))
   ((array-slice var indices)
    (list var))))

(defun write-assigned-test (lhs dim-fct)
  (adt-case rellhs lhs
    ((simple var)
     (write-assigned-test-rellhs-simple var dim-fct))
    ((array-elt var indices)
     (write-assigned-test-rellhs-array-elt var indices dim-fct))
    ((array-slice var indices)
     (error "Unimplemented case in compile::write-assigned-test."))))

(defun write-assigned-test-rellhs-array-elt (var indices dim-fct)
  (let ((n (funcall dim-fct var))
	(avar (assigned-var var))
	(idxstr-list (mapcar #'index-expr->string indices)))
    (unless (= n (length indices))
      (error "Wrong number of indices in LHS of update: ~a."
	     (make-rellhs-array-elt :var var :indices indices)))
    (case n
      (1
       (let ((idxstr (first idxstr-list)))
         (fmt "Assert.IsFalse(~a[~a], \"~a[{0}] assigned\", ~a);"
	      avar idxstr var idxstr)
         (fmt "~a[~a] = true;" avar idxstr)))
      (2
       (let ((idxstr1 (first idxstr-list))
	     (idxstr2 (second idxstr-list)))
	 (fmt "Assert.IsFalse(~a[~a, ~a], \"~a[{0}, {1}] assigned\", ~a, ~a);"
	      avar idxstr1 idxstr2 var idxstr1 idxstr2)
	 (fmt "~a[~a, ~a] = true;" avar idxstr1 idxstr2)))
      (otherwise
       (error "Unimplemented case in write-assigned-test-rellhs-array-elt")))))

(defun write-assigned-test-rellhs-simple (var dim-fct)
  (let ((avar (assigned-var var)))
    (case (funcall dim-fct var)
      (0
       (fmt "Assert.IsFalse(~a, \"~a assigned\");" avar var)
       (fmt "~a = true;" avar))
      (1
       (let ((idxvar (new-var "idx")))
         (fmt "for (int ~a = 0; ~a < ~a.Length; ++~a) {"
	      idxvar idxvar avar idxvar)
	 (bracket-end
	  (fmt "Assert.IsFalse(~a[~a], \"~a[{0}] assigned\", ~a);"
	       avar idxvar var idxvar)
	  (fmt "~a[~a] = true;" avar idxvar))))
      (2
       (let ((idx1 (new-var "idx"))
	     (idx2 (new-var "idx")))
	 (fmt "for (int ~a = 0; ~a < ~a.NBRows; ++~a) {"
	      idx1 idx1 avar idx1)
	 (bracket-end
	  (fmt "for (int ~a = 0; ~a < ~a.NBCols; ++~a) {"
	       idx2 idx2 avar idx2)
	  (bracket-end
	   (fmt "Assert.IsFalse(~a[~a, ~a], \"~a[{0}, {1}] assigned\", ~a, ~a);"
		avar idx1 idx2 var idx1 idx2)
	   (fmt "~a[~a, ~a] = true;" avar idx1 idx2))))))))

(defun write-test-file (class-name mdl impl)
  (let ((update-names (mapcar #'car (mcimpl-updates impl)))
	(wtu-fct (make-write-test-update-fct class-name mdl impl)))
    (write-test-updates class-name update-names wtu-fct)))

(defun write-test-updates (class-name update-names write-test-update-fct)
  (dolist (x '("System" "NUnit.Framework" "Estimation" "Estimation.Samplers" "Common"))
    (fmt "using ~a;" x))
  (fmt-blank-line)
  (fmt "namespace Tests")
  (bracket
    (fmt "public static class Test~aUpdates" class-name)
    (bracket
      (fmt "public static void TestAllUpdates(~a x, double tol)" class-name)
      (bracket
       (fmt "x = x.Copy();")
       (fmt "x.Draw();")
        (dolist (update-name update-names)
          (fmt "TestUpdate_~a(x, tol);" update-name)))
      (fmt-blank-line)
      (dolist-inter (update-name update-names)
	(funcall write-test-update-fct update-name)
	(fmt-blank-line)))))

(defun make-write-test-update-fct (class-name mdl impl)
  (let ((is-class-var (class-var-pred-from mdl impl))
	(dim-fct (model->dim-fct mdl))
	(updates-assoc (mcimpl-updates impl))
	(env0 (initial-environment mdl impl)))
    (fn (upd-name)
      (let ((rel (assoc-lookup upd-name updates-assoc)))
	(write-test-update class-name upd-name rel is-class-var dim-fct env0)))))

(defun model->dim-fct (mdl)
  (flet ((var-dim (d)
	   (let ((n (adt-case vtype (decl-typ d)
		      ((scalar stype) 0)
		      ((array elem-type dims) (length dims)))))
	     (cons (decl-var d) n))))
    (let ((dim-lookup (mapcar #'var-dim (model-vars mdl))))
      (lambda (v) (assoc-lookup v dim-lookup)))))

(defun write-test-update (class-name update-name rel is-class-var dim-fct env0)
  (unless (is-update rel)
    (error "Invalid update: ~a." rel))
  (write-test-update-main class-name update-name)
  (fmt-blank-line)
  (write-test-is-valid-update class-name update-name rel is-class-var dim-fct)
  (fmt-blank-line)
  (let ((*env* env0))
    (write-test-acceptance-ratio
      class-name update-name rel is-class-var dim-fct)))

(defun write-test-update-main (class-name update-name)
  (fmt "public static void TestUpdate_~a(~a x, double tol)"
       update-name class-name)
  (bracket
    (fmt "TestIsValidUpdate_~a(x);" update-name)
    (fmt "TestAcceptanceRatio_~a(x, tol);" update-name)))

(defun var2str-ext (class-vars)
  (lambda (v)
    (let ((s (default-variable->string v)))
      (if (member v class-vars) (strcat "_x." s) s))))

(defun var2str-cv (pfx is-class-var)
  (lambda (v)
    (let ((s (default-variable->string v)))
      (if (funcall is-class-var v) (strcat pfx s) s))))

(defun write-test-is-valid-update
       (class-name update-name rel is-class-var dim-fct
	&optional (write-body #'write-test-is-valid-update-body))
  (fmt "private static void TestIsValidUpdate_~a(~a _x)" update-name class-name)
  (bracket
    (fmt "_x = _x.Copy();")
    (funcall write-body rel is-class-var dim-fct)))

(defun write-test-is-valid-update-body
       (rel is-class-var dim-fct &optional
	(write-mh #'write-test-is-valid-update-mh))
  (flet*
    ((write-tivu-body (rel rev-outer-lets)
       (adt-case relation rel
	 ((let var val body)
	  (if *env*
	    (write-let-assignment var val *env*)
	    (fmt "var ~a = ~a;"
		 var (expr->string (assignable-expr val))))
	  (extend-env var (infer-type val *env*)
	    (write-tivu-body body (cons (cons var val) rev-outer-lets))))
	 ((loop var lo hi body)
	  (let* ((lo-var (lo-var var))
		 (hi-var (hi-var var)))
	    (fmt "var ~a = ~a;" lo-var (expr->string lo))
	    (fmt "var ~a = ~a;" hi-var (expr->string hi))
	    (fmt "for (int ~a = ~a; ~a <= ~a; ++~a) {"
		 var lo-var var hi-var var)
	    (bracket-end
	      (extend-env var #tinteger
		(write-tivu-body 
		  body
		  `((,hi-var . ,hi) (,lo-var . ,lo) ,@rev-outer-lets))))))
	 ((if condition true-branch false-branch)
	  (let* ((if-test-var (new-var "if"))
		 (rol (cons (cons if-test-var condition) rev-outer-lets)))
	    (fmt "var ~a = ~a;" if-test-var (expr->string condition))
	    (fmt "if (~a) {" if-test-var)
	    (bracket-end (write-tivu-body true-branch rol))
	    (when (not (is-relation-skip false-branch))
	      (fmt "else {")
	      (bracket-end (write-tivu-body false-branch rol)))))
	 ((mh)
	  (let ((ol (reverse rev-outer-lets))
		(rel1 (remove-acceptmon rel)))
	    (funcall write-mh ol rel1 is-class-var dim-fct)))
	 (otherwise
	  (error "Unimplemented case in write-test-is-valid-update-body: ~a" rel)))))
    (let ((*variable->string* (var2str-cv "_x." is-class-var)))
      (write-tivu-body rel '()))))

(defun remove-acceptmon (mh)
  (match-adt1 (relation-mh lets proposal-distribution log-acceptance-ratio) mh
    (make-relation-mh :lets lets
		      :proposal-distribution proposal-distribution
		      :acceptmon nil
		      :log-acceptance-ratio log-acceptance-ratio)))

(defun write-test-is-valid-update-mh (outer-lets rel is-class-var dim-fct)
  (dolist (v (model-variables-assigned-in rel))
    (check-rel-var-is-class-var v is-class-var)
    (let ((avar (assigned-var v)))
      (case (funcall dim-fct v)
	(0 (fmt "bool ~a = false;" avar))
	(1 (fmt "bool [] ~a = new bool[_x.~a.Length];" avar v))
	(2 (fmt "BMatrix ~a = new BMatrix(_x.~a.NBRows, _x.~a.NBCols);"
		avar v v)))))
  (let ((wrd-stoch (fn (lhs rhs)
		       (write-assigned-test lhs dim-fct)
		       (write-rel-draw-stoch lhs rhs)))
	(visitor-after-proposal
	  (fn (prop-distr) (write-invariance-check outer-lets)))
	(var2str (var2str-cv "_x." is-class-var)))
    (write-rel-draw rel nil wrd-stoch visitor-after-proposal var2str)))

(defun write-invariance-check (outer-lets)
  (dolist (x outer-lets)
    (destructuring-bind (v . e) x
      (fmt "Assert.IsTrue(BMC.Equal(~a, ~a), \"~a should not change\");"
	   v (expr->string e) v))))

(defun check-rel-var-is-class-var (v is-class-var)
  (unless (funcall is-class-var v)
    (error "write-test-is-valid-update-mh: ~
	    ~a is assigned in relation but is not a class variable" v)))

(defun check-let-var-isnt-class-var (v is-class-var)
  (when (funcall is-class-var v)
    (error "Symbol ~a used both as a class variable and as a let variable" v)))

(defun write-log-proposal-density (var-xform is-class-var rel)
  (let ((*variable->string* (var2str-cv "_x." is-class-var))
	(*ljd-visitor-before*
	  (fn (lhs-str lhs)
	    (let ((rhs (expr-call 'copy (rellhs->expr (lhs-xformed lhs var-xform)))))
	      (fmt "~a = ~a;" lhs-str (expr->string rhs)))))
	(*ljd-accum* lpd-var))
    (write-ljd-acc-rel rel t)))

(defun lhs-var (lhs)
  (adt-case rellhs lhs
    ((simple var) var)
    ((array-elt var indices) var)
    ((array-slice var indices) var)))

(defun replace-lhs-var (lhs new-var)
  (adt-case rellhs lhs
    ((simple var)
     (make-rellhs-simple :var new-var))
    ((array-elt var indices)
     (make-rellhs-array-elt :var new-var :indices indices))
    ((array-slice var indices)
     (make-rellhs-array-slice :var new-var :indices indices))))

(defun lhs-xformed (lhs var-xform)
  (replace-lhs-var lhs (funcall var-xform (lhs-var lhs))))

(defun write-test-acceptance-ratio
       (class-name upd-name rel is-class-var dim-fct &optional
	(write-body #'write-test-acceptance-ratio-body))
  (fmt "private static void TestAcceptanceRatio_~a(~a _x, double ~a)"
       upd-name class-name tol-var)
  (bracket
    (fmt "_x = _x.Copy();")
      (funcall write-body rel is-class-var dim-fct)))

(defun write-test-acceptance-ratio-body (rel is-class-var dim-fct)
  ;; *** FIX: Need to rename write-test-is-valid-update-body ***
  (write-test-is-valid-update-body rel is-class-var dim-fct #'write-test-acceptance-ratio-mh))

(defun make-save-var-hash-table ()
  (make-hash-table :test #'equalp))

(defun save-var (lhs ht)
  (or (gethash lhs ht)
      (let ((name (strcat "save_" (symbol-name (rellhs-main-var lhs)))))
	(setf (gethash lhs ht) (new-var name)))))

(defun write-test-acceptance-ratio-mh (outer-lets rel is-class-var dim-fct)
  (fmt "double ~a = _x.LogJointDensity();" ljd-old-var)
  (let*((var2str (var2str-cv "_x." is-class-var))
	(wrd-stoch #'write-rel-draw-stoch)
	(lar (relation-mh-log-acceptance-ratio rel))
	(assigned-vars (model-variables-assigned-in rel))
	(visitor-after-proposal
	 (fn (prop-distr)
	   (when (equalp (expr-const 0) lar)
	     (fmt "double ~a = 0.0;" lar-var))
	   (dolist (v assigned-vars)
	     (fmt "var ~a = BMC.Copy(~a);"
		  (variable->string (new-val-var v))
		  (variable->string v)))
	   (fmt "double ~a = _x.LogJointDensity();" ljd-new-var)
	   ;; Compute proposal density from new state to old
	   (fmt "double ~a = 0.0;" lpd-var)
	   (write-log-proposal-density #'old-val-var is-class-var prop-distr)
	   (fmt "double ~a = ~a;" lpd-new-var lpd-var)
	   ;; Check that proposal is reversible
	   (dolist (v assigned-vars)
	     (let ((msg "Proposal must be reversible")
		   (v-str (variable->string v))
		   (vnew-str (variable->string (old-val-var v))))
	       (fmt "Assert.IsTrue(BMC.Equal(~a, ~a), \"~a\");"
		    v-str vnew-str msg)))
	   ;; Compute proposal density from old state to new
	   (fmt "~a = 0.0;" lpd-var)
	   (write-log-proposal-density #'new-val-var is-class-var prop-distr)
	   (fmt "double ~a = ~a;" lpd-old-var lpd-var)
	   (let ((e (format nil "(~a - ~a) + (~a - ~a)"
			    ljd-new-var ljd-old-var
			    lpd-new-var lpd-old-var)))
	     (fmt "Assert.AreEqual(~a, ~a, ~a, \"Log acceptance ratio\");"
		lar-var e tol-var)))))
    (let ((*variable->string* var2str))
      (dolist (v assigned-vars)
	(fmt "var ~a = BMC.Copy(~a);"
	     (variable->string (old-val-var v))
	     (variable->string v))))
    (write-rel-draw rel nil wrd-stoch visitor-after-proposal var2str)))

(defun write-csharp-log-joint-density (mdl)
  (fmt "public double LogJointDensity()")
  (bracket
   (fmt "double ~a = 0.0;" ljd-var)
   (dolist (r (model-body mdl))
     (write-ljd-accum-rel r))
   (fmt "return ~a;" ljd-var)))

(defun do-nothing (&rest args) nil)

(defparameter *ljd-visitor-before* nil)
(defparameter *ljd-accum* nil)

(defun write-ljd-accum-rel (rel)
  (let ((*variable->string* #'default-variable->string)
	(*ljd-visitor-before* #'do-nothing)
	(*ljd-accum* ljd-var))
    (write-ljd-acc-rel rel t)))

(defun write-ljd-acc-rel (rel &optional (let-needs-brackets t))
  (adt-case relation rel
    ((stochastic lhs rhs)
     (write-ljd-acc-rel-stoch lhs rhs))
    ((block members)
     (write-ljd-acc-rel-block members))
    ((if condition true-branch false-branch)
     (write-ljd-acc-rel-if condition true-branch false-branch))
    ((loop var lo hi body)
     (write-ljd-acc-rel-loop var lo hi body))
    ((let var val body)
     (write-ljd-acc-rel-let
       (let-list rel) (strip-lets body) let-needs-brackets))
    ((skip))))

(defun write-ljd-acc-rel-let (let-defs body needs-brackets)
  (bracket-if needs-brackets
    (dolist (x let-defs)
      (destructuring-bind (var . val) x
        (fmt "var ~a = ~a;"
	     (variable->string var)
	     (expr->string (assignable-expr val)))))
    (write-ljd-acc-rel body)))

(defun write-ljd-acc-rel-stoch (lhs rhs)
  (match-adt1 (distribution name args) rhs
    (let ((lhs-str (crellhs->string lhs))
	  (dname (csharp-distr-log-density-name name))
	  (args-str-list (mapcar #'expr->string args)))
      (funcall *ljd-visitor-before* lhs-str lhs)
      (fmt "~a += ~a(~a~{, ~a~});" *ljd-accum* dname lhs-str args-str-list))))

(defun crellhs->string (lhs)
  (expr->string (rellhs->expr lhs)))

(defun csharp-distr-log-density-name (distr-symbol)
  (strcat
    "BMC.LogDensity" (assoc-lookup distr-symbol +csharp-distr-name-assoc+)))

(defconstant +csharp-distr-name-assoc+
  '((ddirch . "Dirichlet")
    (dcat . "Cat")
    (dgamma . "Gamma")
    (dnorm . "Norm")
    (dnorm-trunc . "NormTruncated")
    (dwishart . "Wishart")
    (dmvnorm . "MVNorm")
    (dinterval . "Interval")))

(defun write-ljd-acc-rel-block (members)
  (dolist (r members)
    (write-ljd-acc-rel r)))

(defun write-ljd-acc-rel-if (condition true-branch false-branch)
  (fmt "if (~a) {" (expr->string condition))
  (bracket-end
    (write-ljd-acc-rel true-branch nil))
  (when (not (is-relation-skip false-branch))
    (fmt "else {")
    (bracket-end
      (write-ljd-acc-rel false-branch nil))))

(defun write-ljd-acc-rel-loop (var lo hi body)
  (let ((var-str (variable->string var))
	(lo-str (expr->string lo))
	(term-str (termination-test-string var hi)))
    (fmt "for (int ~a = ~a; ~a; ++~a) {" var-str lo-str term-str var-str)
    (bracket-end
      (write-ljd-acc-rel body nil))))

(defun termination-test-string (var hi)
  (expr->string (expr-call '.<= (expr-var var) hi)))

(defun do-nothing (&rest args) nil)

(defparameter *write-rel-draw-stoch* nil)
(defparameter *write-rel-draw-visitor-after-proposal* nil)

(defun write-rel-draw (rel &optional
			   (let-needs-brackets t)
			   (wrd-stoch #'write-rel-draw-stoch)
			   (visitor-after-proposal #'do-nothing)
			   (var2str #'default-variable->string))
  (let ((*write-rel-draw-stoch* wrd-stoch)
	(*write-rel-draw-visitor-after-proposal* visitor-after-proposal)
	(*variable->string* var2str))
    (write-rel-draw-main rel let-needs-brackets)))

(defun write-rel-draw-main (rel &optional (let-needs-brackets t))
  (adt-case relation rel
    ((stochastic lhs rhs)
     (funcall *write-rel-draw-stoch* lhs rhs))
    ((block members)
     (mapc #'write-rel-draw-main members))
    ((if condition true-branch false-branch)
     (write-rel-draw-if condition true-branch false-branch))
    ((loop var lo hi body)
     (write-rel-draw-loop var lo hi body))
    ((let var val body)
     (write-rel-draw-let (let-list rel) (strip-lets body) let-needs-brackets))
    ((mh lets proposal-distribution acceptmon log-acceptance-ratio)
     (write-rel-draw-mh
       lets proposal-distribution acceptmon log-acceptance-ratio
       let-needs-brackets))
    ((skip)
     )))

(defun let-list (rel)
  (adt-case relation rel
    ((let var val body)
     (cons (cons var val) (let-list body)))
    (otherwise
     '())))

(defun strip-lets (rel)
  (adt-case relation rel
    ((let var val body)
     (strip-lets body))
    (otherwise
     rel)))

(defun write-rel-draw-let (let-defs body needs-brackets)
  (bracket-if needs-brackets
    (let ((new-env *env*))
      (dolist (x let-defs)
	(destructuring-bind (var . val) x
	  (if new-env
	    (progn
	      (write-let-assignment var val new-env)
	      (setf new-env
		    (add-var-type new-env var (infer-type val new-env))))
	    (fmt "var ~a = ~a;"
		 (variable->string var)
		 (expr->string (assignable-expr val))))))
      (let ((*env* new-env))
	(write-rel-draw-main body nil)))))

(defun write-rel-draw-stoch (lhs rhs)
  (match-adt1 (distribution name args) rhs
    (cond
      ((is-scalar-distr name)
       (write-rel-draw-stoch-univariate lhs name args rhs))
      ((is-rellhs-array-slice lhs)
       (write-rel-draw-stoch-array-slice lhs name args))
      (t
       (write-rel-draw-stoch-multivariate lhs name args)))))

(defun write-rel-draw-stoch-univariate (lhs distr-name distr-args rhs)
  (when (is-rellhs-array-slice lhs)
    (error "Array slice used as LHS with scalar distribution in ~
            write-rel-draw-stoch: LHS = ~a, DISTR = ~a" lhs rhs))
  (fmt "~a = ~a(~{~a~^, ~});"
       (crellhs->string lhs)
       (csharp-distr-draw-name distr-name)
       (mapcar #'expr->string distr-args)))

(defun write-rel-draw-stoch-multivariate (lhs distr-name distr-args)
  (fmt "~a = ~a(~{~a~^, ~});"
       (crellhs->string lhs)
       (csharp-distr-draw-name distr-name)
       (mapcar #'expr->string distr-args)))

(defun write-rel-draw-stoch-array-slice (lhs distr-name distr-args)
  (bracket
    (match-adt1 (rellhs-array-slice var indices) lhs
      (let ((xformed-idxs '()))
	(dolist (x indices)
	  (push (xform-slice-arg x) xformed-idxs))
	(setf xformed-idxs (reverse xformed-idxs))
	(let ((xformed-idx-strs
	        (mapcar #'expr->string (mapcar #'slice-dec-expr xformed-idxs)))
	      (var-str (variable->string var))
	      (buf (new-var "buf")))
	  (fmt "var ~a = ~a(~{~a~^, ~});"
	       buf
	       (csharp-distr-draw-name distr-name)
	       (mapcar #'expr->string distr-args))
	  (fmt "BMC.CopyInto(~a~{, ~a~}, ~a);"
	       var-str xformed-idx-strs buf))))))

(defun xform-slice-arg (idx)
  (adt-case array-slice-index idx
    ((scalar value)
     (let ((v (new-var "idx")))
       (fmt "var ~a = ~a;" v (expr->string value))
       (expr-call '@-idx (expr-var v))))
    ((range lo hi)
     (let ((v-lo (new-var "lo"))
	   (v-hi (new-var "hi")))
       (fmt "var ~a = ~a;" v-lo (expr->string lo))
       (fmt "var ~a = ~a;" v-hi (expr->string hi))
       (expr-call '@-rng (expr-var v-lo) (expr-var v-hi))))
    ((all)
     (expr-const '@-all))))

(defun is-scalar-distr (symbol)
  (member symbol +scalar-distributions+))

(defconstant +scalar-distributions+
  '(dcat dnorm dgamma dinterval dnorm-trunc dgamma-trunc))

(defun csharp-distr-draw-name (symbol)
  (strcat
    "BMC.Draw" (assoc-lookup symbol +csharp-distr-name-assoc+)))

(defun write-rel-draw-if (condition true-branch false-branch)
  (fmt "if (~a) {" (expr->string condition))
  (bracket-end
    (write-rel-draw-main true-branch nil))
  (when (not (is-relation-skip false-branch))
    (fmt "else {")
    (bracket-end
      (write-rel-draw-main false-branch nil))))

(defun write-rel-draw-loop (var lo hi body)
  (let ((var-str (variable->string var))
	(lo-str (expr->string lo))
	(term-str (expr->string (expr-call '.<= (expr-var var) hi))))
    (fmt "for (int ~a = ~a; ~a; ++~a) {"
	 var-str lo-str term-str var-str)
    (bracket-end
      (extend-env var #tinteger
	(write-rel-draw-main body nil)))))

(defun write-rel-draw-mh (lets prop-distr acceptmon log-acc-ratio
			  let-needs-brackets)
  (bracket-if let-needs-brackets
    (let ((new-env *env*))
      (dolist (d lets)
	(destructuring-bind (v . val) d
	  (if *env*
	    (progn
	      (write-let-assignment v val new-env)
	      (setf new-env (add-var-type new-env v (infer-type val new-env))))
	    (fmt "var ~a = ~a;"
		 (variable->string v)
		 (expr->string (assignable-expr val))))))
      (let ((*env* new-env)
	    svht)
	(if (equalp (expr-const 0) log-acc-ratio)
	  (progn
	    (write-rel-draw-main prop-distr t)
	    (funcall *write-rel-draw-visitor-after-proposal* prop-distr))
	  (progn
	    (setf svht (make-save-var-hash-table))
	    (write-mh-saves prop-distr svht)
	    (fmt-blank-line)
	    (write-rel-draw-main prop-distr t)
	    (write-lar log-acc-ratio)
	    (funcall *write-rel-draw-visitor-after-proposal* prop-distr)
	    (fmt-blank-line)
	    (if acceptmon
	      (progn
		(flet ((report (accepted)
			 (fmt "this.AcceptanceMonitor~a(~a~{, ~a~});"
			      (car acceptmon)
			      accepted
			      (mapcar #'expr->string (cdr acceptmon)))))
		  (fmt "bool ~a = BMC.Accept(~a);" accepted-var lar-var)
		  (fmt "if (~a) {" accepted-var)
		  (bracket-end
		   (report "true"))
		  (fmt "else {")
		  (bracket-end
		   (write-mh-restores prop-distr svht)
		   (report "false"))))
	      (progn
		(fmt "if (!BMC.Accept(~a)) {" lar-var)
		(bracket-end
		 (write-mh-restores prop-distr svht))))))))))

(defun write-lar (log-acc-ratio)
  (if *env*
    (write-let-assignment lar-var log-acc-ratio *env*)
    (fmt "double ~a = ~a;" lar-var (expr->string log-acc-ratio))))

(defun write-mh-saves (prop-distr ht)
  (flet ((f (lhs-str sav-var)
	   (fmt "var ~a = BMC.Copy(~a);" sav-var lhs-str)))
    (write-mh-saves-restores
      prop-distr (list ht "write-mh-saves" #'f) '())))

(defun write-mh-restores (prop-distr ht)
  (flet ((f (lhs-str sav-var)
	   (fmt "~a = ~a;" lhs-str sav-var)))
    (write-mh-saves-restores
      prop-distr (list ht "write-mh-restores" #'f) '())))

(defun write-mh-saves-restores (prop-distr x let-vars)
  (adt-case relation prop-distr
    ((stochastic lhs rhs)
     (destructuring-bind (ht fct-name write-assignment-fct) x
       (let ((fv (free-vars-in-rellhs lhs)))
	 (dolist (v let-vars)
	   (when (member v fv)
	     (error "Unimplemented (problematic) case in ~a" fct-name))))
       (funcall write-assignment-fct (crellhs->string lhs) (save-var lhs ht))))
    ((block members)
     (dolist (r members)
       (write-mh-saves-restores r x let-vars)))
    ((let var val body)
     (write-mh-saves-restores body x (cons var let-vars)))
    (otherwise
     (error "Unimplemented case in ~a; prop-distr: ~a."
	    (second x) prop-distr))))

(defparameter *stoch-vars* nil)

(defun stochastic-vars (rels)
  (let ((*stoch-vars* '()))
     (mapc #'stochastic-vars-rel rels)
     (reverse *stoch-vars*)))

(defun stochastic-vars-rel (rel)
  (adt-case relation rel
    ((stochastic lhs rhs)
     (adt-case rellhs lhs
       ((simple var) (pushnew var *stoch-vars*))
       ((array-elt var indices) (pushnew var *stoch-vars*))
       ((array-slice var indices) (pushnew var *stoch-vars*))))
    ((block members)
     (mapc #'stochastic-vars-rel members))
    ((if condition true-branch false-branch)
     (stochastic-vars-rel true-branch)
     (stochastic-vars-rel false-branch))
    ((loop var lo hi body)
     (stochastic-vars-rel body))
    ((let var val body)
     (stochastic-vars-rel body))
    ((skip))))

(defun rels->pdf (rels)
  (expr-app '* (mapcar #'rel->pdf rels)))

(defun rel->pdf (rel)
  (adt-case relation rel
    ((stochastic lhs rhs)
     (rel-stoch->pdf lhs rhs))
    ((block members)
     (rels->pdf members))
    ((if condition true-branch false-branch)
     (expr-call
       'if-then-else condition (rel->pdf true-branch) (rel->pdf false-branch)))
    ((loop var lo hi body)
     (expr-call 'qprod lo hi (expr-const '%true-pred)
		(expr-lam var (rel->pdf body))))
    ((let var val body)
     (expr-call '! (expr-lam var (rel->pdf body)) val))
    ((skip)
     (expr-const 1))))

(defun rel-stoch->pdf (lhs rhs)
  (let ((lhs-expr (rellhs->expr lhs)))
    (match-adt1 (distribution name args) rhs
      (expr-app (density-name name)
		 (cons lhs-expr args)))))

(defun density-name (distr-name)
  (compound-symbol distr-name 'density))

(defun write-csharp-zero-accum (decls)
  (fmt "public void ZeroAccumulators()")
  (bracket
    (fmt "_expectations._N = 0;")
    (dolist (d decls)
      (match-adt1 (decl var typ) d
	(adt-case vtype typ
	  ((scalar stype)
	   (fmt "_expectations.~a = 0.0;" var))
	  ((array elem-type dims)
	   (write-zero-array-accum var dims '())))))))

(defun write-zero-array-accum (var dims reverse-idx-list)
  (cond
   ((null dims)
    (fmt "_expectations.~a[~{~a~^, ~}] = 0.0;"
	 var (reverse reverse-idx-list)))
    (t
     (let ((idx (new-var "idx")))
      (fmt "for (int ~a = 0; ~a < ~a; ++~a) {"
	   idx idx (expr->string (car dims)) idx)
      (bracket-end
        (write-zero-array-accum var (cdr dims) (cons idx reverse-idx-list)))))))

(defun write-csharp-accumulate (expectations)
  (fmt "public void Accumulate()")
  (bracket
    (fmt "_expectations._N += 1;")
    (dolist (e expectations)
      (destructuring-bind (decl . expr) e
	(match-adt1 (decl var typ) decl
	  (let ((expr-str (expr->string expr)))
            (adt-case vtype typ
              ((scalar stype)
               (fmt "_expectations.~a += ~a;" var expr-str))
	      ((array elem-type dims)
	       (if (is-expr-variable expr)
		   (write-csharp-accumulate-array var expr-str dims)
		 (let ((tmpvar (new-var "tmp")))
		   (bracket
		     (fmt "var ~a = ~a;" tmpvar expr-str)
		     (write-csharp-accumulate-array
		       var (symbol-name tmpvar) dims))))))))))))

(defun write-csharp-accumulate-array (var expr-str dims)
  (write-dim-check var expr-str (length dims))
  (write-csharp-array-accumulate var expr-str dims '()))

(defun write-dim-check (var expr-str n)
  (case n
    (1 (fmt "BMC.Check(_expectations.~a.Length == ~a.Length," var expr-str)
       (fmt "          \"_expectations.~a.Length == ~a.Length\");"
	    var expr-str))
    (2 (dolist (prop '("NBRows" "NBCols"))
	 (fmt "BMC.Check(_expectations.~a.~a == ~a.~a,"
	       var prop expr-str prop)
	 (fmt "          \"_expectations.~a.~a == ~a.~a\");"
	      var prop expr-str prop)))
    (otherwise
      (error "Unimplemented case in write-dim-check."))))

(defun write-csharp-array-accumulate (var expr-str dims rev-idx-vars)
  ; Currently only implement case that expr-str is name of array
  (cond
   ((null dims)
    (let ((idx-vars (reverse rev-idx-vars)))
      (fmt "_expectations.~a[~{~a~^, ~}] += ~a[~{~a~^, ~}];"
	     var idx-vars expr-str idx-vars)))
   (t
    (let ((idx (new-var "idx")))
      (fmt "for (int ~a = 0; ~a < ~a; ++~a) {"
	   idx idx (expr->string (car dims)) idx)
      (bracket-end
        (write-csharp-array-accumulate
	  var expr-str (cdr dims) (cons idx rev-idx-vars)))))))

(defun write-csharp-output-expectations (decls)
  (fmt "public void OutputExpectations(string _dirpath)")
  (bracket
    (fmt "double _N = (double)_expectations._N;")
    (fmt-blank-line)
    (let ((scalars '()))
      (dolist (d decls)
	(match-adt1 (decl var typ) d
	  (let ((var-str (variable->string var)))
	    (if (is-vtype-scalar typ)
		(progn
		  (fmt "_expectations.~a /= _N;" var-str)
		  (push var-str scalars))
	      (fmt "BMC.DivBy(_expectations.~a, _N);" var-str)))))
      (fmt-blank-line)
      (fmt "BMC.StoreScalars(_dirpath,")
      (let ((cnt (length scalars)))
	(dolist (v (reverse scalars))
	  (decf cnt)
	  (let ((terminator (if (zerop cnt) ");" ",")))
	    (fmt "  \"~a\", _expectations.~a~a" v v terminator))))
      (dolist (d decls)
	(match-adt1 (decl var typ) d
	  (unless (is-vtype-scalar typ)
	    (let ((var-str (variable->string var)))
	      (fmt "BMC.StoreArray(_dirpath, \"~a\", _expectations.~a);"
		   var-str var-str))))))))

(defun lift-let (e)
  (multiple-value-bind (lets e-new) (lift-let-main e)
    (check-lift-let lets e)
    (flet ((make-let (def expr)
             (destructuring-bind (var . val) def
               (expr-call '! (expr-lam var expr) val))))
      (reduce #'make-let lets :from-end t :initial-value e-new))))

(defun check-lift-let (lets e)
  (let ((let-vars (mapcar #'car lets)))
    (when (has-duplicates let-vars)
      (error "Bad argument to lift-let: ~
                multiple bound vars with same name."))
    (if (not (null let-vars))
      (let ((free-vars (free-vars-in-expr e)))
	(when (some (lambda (v) (member v free-vars)) let-vars)
	  (error "Bad argument to lift-let: ~
                bound variable and free variable with same name."))))))

(defun lift-let-main (e)
  (adt-case expr e
    ((apply fct args)
     (lift-let-apply fct args e))
    ((:lambda var body)
     (values '() (expr-lam var (lift-let body))))
    (otherwise (values '() e))))

(defun lift-let-apply (fct args e)
  (if (eq '! fct)
    (destructuring-bind (fct-exp arg-exp) args
      (lift-let-! fct-exp arg-exp e))
    (lift-let-fapply fct args e)))

(defun lift-let-! (fct-exp arg-exp e)
  (adt-case expr fct-exp
    ((lambda var body)
     (multiple-value-bind (arg-lets arg-new) (lift-let-main arg-exp)
       (multiple-value-bind (body-lets body-new) (lift-let-main body)
       (values (append arg-lets (cons (cons var arg-new) body-lets))
	       body-new))))
    (otherwise (lift-let-fapply '! (list fct-exp arg-exp) e))))

(defun lift-let-fapply (fct args e)
  (let ((lets '())
	(args1 '()))
    (dolist (a args)
      (multiple-value-bind (alets anew) (lift-let-main a)
	(push alets lets)
	(push anew args1)))
    (values (apply #'append (reverse lets))
	    (expr-app fct (reverse args1)))))

(defun find-new-var (vars-in-use)
  (flet* ((find-new-var (v candidate n)
	      (if (member candidate vars-in-use)
		(let ((vn (bmc-symb (format nil "~a~a" v n))))
		  (find-new-var v vn (1+ n)))
		(progn
		  (push candidate vars-in-use)
		  candidate))))
    (lambda (v) (find-new-var v v 1))))

(defun uniquely-rename-bound-vars (e)
  (flet* ((rename (x)
	    (adt-case expr x
	      ((const name)
	       x)
	      ((variable symbol)
	       x)
	      ((apply fct args)
	       (rename-apply fct args x))
	      ((lambda var body)
	       (rename-lambda var body))))

	  (rename-lambda (var body)
	    (let* ((new-var (new-var var))
		   (new-body (rename-var var new-var body)))
	      (expr-lam new-var (rename new-body))))

	  (rename-apply (fct args e)
	    (if (is-quant-expr e)
	      (rename-quant fct args)
	      (rename-normal-apply fct args)))

	  (rename-quant (fct args)
	    (destructuring-bind (lo hi filter body . shape) args
	      (destructuring-bind (rn-filter rn-body) (rename2lam filter body)
		(destructuring-bind (rn-lo rn-hi . rn-shape)
		                    (mapcar #'rename (list* lo hi shape))
		  (expr-app fct
			    (list* rn-lo rn-hi rn-filter rn-body rn-shape))))))

	  (rename2lam (e1 e2)
	    (let* ((e2-new (rename e2))
		   (var-new (expr-lambda-var e2-new))
		   (var1 (expr-lambda-var e1))
		   (body1 (expr-lambda-body e1))
		   (body1-new (rename (rename-var var1 var-new body1)))
		   (e1-new (expr-lam var-new body1-new)))
	      (list e1-new e2-new)))

	  (rename-normal-apply (fct args)
	    (expr-app fct (mapcar #'rename args))))

    (rename e)))

(defun lettify-quant (e &optional (is-rhs-let nil))
  (let ((quant-expansion (and (not is-rhs-let) (is-quant-expr e))))
    (when quant-expansion
      (return-from lettify-quant (lettify-quant-quant quant-expansion))))
  (let ((let-expansion (is-let-expr e)))
    (when let-expansion
      (return-from lettify-quant (lettify-quant-let let-expansion))))
  (adt-case expr e
    ((const name) e)
    ((variable symbol) e)
    ((apply fct args)
     (lettify-quant-apply fct args))
    ((lambda var body)
     (lettify-quant-lambda var body))))

(defun lettify-quant-quant (quant-expansion)
  (destructuring-bind (fct . args) quant-expansion
    (let ((q 'vars::q))
      (expr-call '! (expr-lam q (expr-var q)) (lettify-quant-apply fct args)))))

(defun lettify-quant-let (let-expansion)
  (destructuring-bind (var val body) let-expansion
    (expr-call '!
	       (lettify-quant-lambda var body)
	       (lettify-quant val t))))

(defun lettify-quant-lambda (var body)
  (expr-lam var (lettify-quant body)))

(defun lettify-quant-apply (fct args)
  (expr-app fct (mapcar #'lettify-quant args)))

(defun expand-true-pred (e)
  (adt-case expr e
    ((const name)
     (if (eq '%true-pred name)
       (expr-lam 'vars::i (expr-const 'true))
       e))
     ((variable symbol)
      e)
     ((apply fct args)
      (expr-app fct (mapcar #'expand-true-pred args)))
     ((lambda var body)
      (expr-lam var (expand-true-pred body)))))

(defun lift-let-and-quant (e)
  (let* ((e1 (lettify-quant (expand-true-pred e)))
	 (e2 (uniquely-rename-bound-vars e1)))
    (lift-let e2)))

(defun bare-scalar-type-symbol->string (x)
  (case x
    (realxn "double")
    (integer "int")
    (boolean "bool")))

(defun bt->string (typ)
  (adt-case bare-type typ
    ((scalar stype)
     (bare-scalar-type-symbol->string stype))
    ((pair fst-type snd-type)
     (format nil "Tuple<~a, ~a>" (bt->string fst-type) (bt->string snd-type)))
    ((array elem-type num-dims)
     (case num-dims
       (1 (strcat (bare-type->string elem-type) "[]"))
       (2 (when (is-bare-type-scalar elem-type)
	    (case (bare-type-scalar-stype elem-type)
	      (realxn "DMatrix")
	      (integer "IMatrix")
	      (boolean "BMatrix"))))))))

(defun bare-type->string (typ)
  (let ((s (bt->string typ)))
    (when (null s)
      (error "Unknown or unimplemented type in bare-type->string: ~a." typ))
    s))

(defun write-let-assignment (var expr env &optional (predeclared nil))
  (let* ((lhs (expr-var var))
	 (lhs-str (expr->string lhs)))
    (write-let-assignment-to-lhs lhs-str expr env predeclared)))

(defun write-let-assignment-to-lhs (lhs-str expr env predeclared)
  (flet
    ((write-assn (let-expansion e)
       (loop while let-expansion do
         (destructuring-bind (v val body) let-expansion
	   (let ((val-type (infer-type val env)))
	     (write-let val-type v val env)
	     (setf env (add-var-type env v val-type))
	     (setf e body)
	     (setf let-expansion (is-let-expr body)))))
       (fmt "~a = ~a;" lhs-str (expr->string e))))
    (let* ((e (lift-let-and-quant expr))
	   (typ-str (bare-type->string (infer-type e env)))
	   (let-expansion (is-let-expr e)))
      (if let-expansion
	(progn
	  (unless predeclared
	    (fmt "~a ~a;" typ-str lhs-str))
	  (bracket
	    (write-assn let-expansion e)))
	(let ((e-str (expr->string e)))
	  (if predeclared
	    (fmt "~a = ~a;" lhs-str e-str)
	    (fmt "~a ~a = ~a;" typ-str lhs-str e-str)))))))

(defun write-let (val-type var val env)
  (let ((quant-expansion (is-quant-expr val)))
    (if quant-expansion
      (destructuring-bind (fct . args) quant-expansion
	(cond
	  ((reduction-params fct val-type)
	   (write-let-quant val-type var fct args env))
	  ((eq 'qvec fct)
	   (write-let-qvec val-type var args env))
	  (t (write-let-simple val-type var val))))
      (write-let-simple val-type var val))))

(defun write-let-simple (val-type var val)
  (fmt "~a ~a = ~a;"
       (bare-type->string val-type)
       (variable->string var)
       (expr->string val)))

;;; REQUIRE: all variables in args are distinct from each other
;;; and from var.
;;;
(defun write-let-quant (val-type var fct args env)
  (destructuring-bind (lo hi filter body . maybe-shape) args
    (let* ((idx-var (expr-lambda-var filter))
	   (last-idx-var (new-var "last"))
	   (filter-body (expr-lambda-body filter))
	   (body-body (expr-lambda-body body))
	   (tmp (reduction-params fct val-type))
	   (reduction (car tmp))
	   (init-val (let ((val (cadr tmp)))
		       (cond ((is-fct-symbol val)
			      (expr-app val maybe-shape))
			     ((is-expr val)
			      val)
			     (t (error "Bad initial value in reduction for ~a."
				       fct)))))			     
	   (xform-fct (cddr tmp))
	   (xform (if xform-fct
		    (lambda (x) (expr-call xform-fct x))
		    #'identity)))
      (fmt "~a ~a = ~a;"
	   (bare-type->string val-type) var (expr->string init-val))
      (fmt "int ~a = ~a;" last-idx-var (expr->string hi))
      (fmt "for (int ~a = ~a; ~a <= ~a; ++~a) {"
	   idx-var (expr->string lo) idx-var last-idx-var idx-var)
      (bracket-end
       (flet ((write-body ()
		(write-let-assignment
		  var
		  (expr-call reduction (expr-var var) (funcall xform body-body))
		  (add-var-type (add-var-type env var val-type)
				idx-var #tinteger)
		  t)))
	 (if (is-const-true filter-body)
	   (write-body)
	   (progn
	     (fmt "if (~a) {" (expr->string filter-body))
	     (bracket-end
	      (write-body)))))))))

(defun is-const-true (e)
  (adt-case expr e
    ((const name) (eq 'true name))
    (otherwise nil)))

;;; REQUIRE: all variables in args are distinct from each other
;;; and from var.
;;;
(defun write-let-qvec (val-type var args env)
  (assert (equalp #t(realxn 1) val-type))
  (destructuring-bind (lo hi filter body . maybe-shape) args
    (assert (is-always-true filter))
    (let* ((idx-var (expr-lambda-var filter))
	   (first-idx-var (new-var "first"))
	   (last-idx-var (new-var "last"))
	   (body-body (expr-lambda-body body)))
      (fmt "int ~a = ~a;" first-idx-var (expr->string lo))
      (fmt "int ~a = ~a;" last-idx-var (expr->string hi))
      (fmt "~a ~a = new double[~a - ~a + 1];"
	   (bare-type->string val-type) var last-idx-var first-idx-var)
      (fmt "for (int ~a = ~a; ~a <= ~a; ++~a) {"
	   idx-var first-idx-var idx-var last-idx-var idx-var)
      (bracket-end
        (write-let-assignment-to-lhs
	  (format nil "~a[~a - ~a]" var idx-var first-idx-var)
	  body-body
	  (add-var-type (add-var-type env var val-type)
			idx-var #tinteger)
	  t)))))

(let (ht)
  (defun init-reduction-params ()
    (when (null ht)
      (setf ht (create-reduction-params))))
  (defun reduction-params (fct val-type &key (optional nil))
    (init-reduction-params)
    (or (gethash (cons fct val-type) ht)
	(and optional
	     (error "No reduction params found for (~a, ~a)." fct val-type)))))

(defun create-reduction-params ()
  (let ((ht (make-hash-table :test #'equalp)))
    (loop for (key . value) in *reduction-params* do
      (setf (gethash key ht) value))
    ht))

(defparameter *reduction-params*
  '(((qsum . #tinteger) . (+ #e0))
    ((qsum . #trealxn) . (+ #e0.0))
    ((qprod . #tinteger) . (* #e1))
    ((qprod . #trealxn) . (* #e1.0))
    ((qnum . #tinteger) . (+ #e0 . int))
    ((q@sum . #t(realxn 1)) . (@+ real-zero-arr))
    ((q@sum . #t(realxn 2)) . (@+ real-zero-arr))
    ((qmax . #tinteger) . (max #e%min-int))
    ((qmax . #trealxn) . (max #e%infty-))
    ((qmin . #tinteger) . (min #e%max-int))
    ((qmin . #trealxn) . (min #e%infty+))))

(defun initial-environment (mdl impl)
  (assocs->env (initial-environment-assocs mdl impl)))

(defun initial-environment-assocs (mdl impl)
  (let ((args (model-args mdl))
	(vars (model-vars mdl))
	(parms (mcimpl-parameters impl)))
    (mapcar #'decl->assoc (append args vars parms))))

(defun decl->assoc (d)
  (match-adt1 (decl var typ) d
    (cons
      var
      (adt-case vtype typ
	((scalar stype)
	 (make-bare-type-scalar :stype (base-type stype)))
	((array elem-type dims)
	 (make-bare-type-array
	   :elem-type (make-bare-type-scalar :stype (base-type elem-type))
	   :num-dims (length dims)))))))

(defun aliases (e)
  (adt-case expr e
    ((const name)
     '())
    ((variable symbol)
     (list symbol))
    ((lambda var body)
     '())
    ((apply fct args)
     (fct-app-aliases fct args))))

(defun fct-app-aliases (fct args)
  (case fct
    (vec
      (append-mapcar #'aliases args))
    (if-then-else
      (destructuring-bind (test true-branch false-branch) args
	(append (aliases true-branch) (aliases false-branch))))
    (qvec
      (destructuring-bind (lo hi filter int-map . maybe-shape) args
        (aliases (expr-lambda-body int-map))))
    (!
      (fct-app-aliases :let (reverse args)))
    (:let
      (destructuring-bind (val lambda-expr) args
	(match-adt1 (expr-lambda var body) lambda-expr
	  (let ((var-aliases (aliases val)))
	    (append-mapcar
	      (lambda (s) (if (eq var s) var-aliases (list s)))
	      (aliases body))))))
    (otherwise '())))

;;; END

#|
(defun compile-to-csharp (csharp-name-space class-name mdl os)
  (destructuring-bind (norm-axioms . norm-mdl) (normalize-model mdl)
    (let* ((assums0 (append (args-assums norm-mdl) norm-axioms *basic-axioms*))
	   (thms0 (var-lengths-stmts norm-mdl))
	   (failed (prove-thms-axs thms0 assums0)))
      (if (not (null failed))
	(error "The following could not be proven:~%~{  ~a~^~%~}" failed))))
)

(defun gen-args-checks (mdl)
  (let ((checks (args-checks mdl)))
    (fmt "public void Validate()")
    (fmt "{")
    (indent
      (dolist (x checks)
	(let ((bool-expr (print-expr-c# x)))
	  (fmt "BMC.Check(~a," bool-expr)
	  (fmt "          \"~a\");" bool-expr))))
    (fmt "}")))

(defun print-expr-c# (x)
  (with-print-options
    :is-binop #'is-binop
    :fct-name #'fct-name
    :quant-format #'quant-format
    (expr->string x)))


(defun normalize-model (mdl)
  (destructuring-bind (args-ax . new-args)
		      (normalize-decls (extract-args mdl))
  (destructuring-bind (reqs-ax . new-reqs)
		      (normalize-exprs (extract-reqs mdl))
  (destructuring-bind (vars-ax . new-vars)
		      (normalize-decls (extract-vars mdl))
  (destructuring-bind (rels-ax . new-rels)
		      (normalize-rels (extract-body mdl))
  (let ((new-mdl (make-model new-args new-reqs new-vars new-rels))
	(axioms (remove-duplicates (append args-ax reqs-ax vars-ax rels-ax))))
    (cons axioms new-mdl)))))))

(defun make-model (args reqs vars rels)
  `(:model (:args ,@args) (:reqs ,@reqs) (:vars ,@vars) (:rels ,@rels)))

(defun normalize-decls (decls)
  (let* ((xdecls (mapcar #'normalize-decl decls))
	 (axioms (append-mapcar #'car xdecls))
	 (ndecls (mapcar #'cdr xdecls)))
    (cons axioms ndecls)))

(defun normalize-decl (decl)
  (destructuring-bind (var-ax . new-var) (normalize-expr (decl-var decl))
  (destructuring-bind (typ-ax . new-typ) (normalize-type (decl-typ decl))
    (cons (append var-ax typ-ax)
	  (make-decl new-var new-typ)))))

(defun make-decl (var typ) (list var typ))

(defun normalize-type (typ)
  (case (type-class typ)
	(:scalar (cons '() typ))
	(:array
	  (let ((etyp (elem-type typ))
		(dims (type-dims typ)))
          (destructuring-bind (dims-ax . new-dims) (normalize-exprs dims)
	    (cons dims-ax (make-array-type etyp new-dims)))))))

(defun make-array-type (etyp dims) (cons etyp dims))

(defun normalize-rels (rels)
  (let* ((xrels (mapcar #'normalize-rel rels))
	 (axioms (append-mapcar #'car xrels))
	 (nrels (mapcar #'cdr xrels)))
    (cons axioms nrels)))

(defun normalize-rel (rel)
  (case (rel-class rel)
	(:deterministic (normalize-determ-rel rel))
	(:stochastic (normalize-stoch-rel rel))
	(:block (normalize-block-rel rel))
	(:if-then (normalize-if-then-rel rel))
	(:if-then-else (normalize-if-then-else-rel rel))
	(:loop (normalize-loop-rel rel))))

(defun normalize-determ-rel (rel) 
  (destructuring-bind (var-ax . nvar) (normalize-expr (rel-var rel))
  (destructuring-bind (val-ax . nval) (normalize-expr (rel-val rel))
    (cons (append var-ax val-ax)
	  (make-determ-rel nvar nval)))))

(defun make-determ-rel (var val) (list :<- var val))

(defun normalize-stoch-rel (rel)
  (destructuring-bind (var-ax . nvar) (normalize-expr (rel-var rel))
  (destructuring-bind (distr-ax . ndistr) (normalize-expr (rel-distr rel))
    (cons (append var-ax distr-ax)
	  (make-stoch-rel nvar ndistr)))))

(defun make-stoch-rel (var distr) (list :~ var distr))

(defun normalize-block-rel (rel)
  (destructuring-bind (axioms . rels) (normalize-rels (rel-block-body rel))
    (cons axioms (make-block-rel rels))))

(defun make-block-rel (rels) (cons :block rels))

(defun normalize-if-then-rel (rel)
  (destructuring-bind (test-axioms . ntest)
		      (normalize-expr (rel-if-condition rel))
  (destructuring-bind (then-axioms . nthen)
		      (normalize-rel (rel-true-branch rel))
    (cons (append test-axioms then-axioms)
	  (make-if-then-rel ntest nthen)))))

(defun make-if-then-rel (test then) (list :if test then))

(defun normalize-if-then-else-rel (rel)
  (destructuring-bind (test-axioms . ntest)
		      (normalize-expr (rel-if-condition rel))
  (destructuring-bind (then-axioms . nthen)
		      (normalize-rel (rel-true-branch rel))
  (destructuring-bind (else-axioms . nelse)
		      (normalize-rel (rel-false-branch rel))
    (cons (append test-axioms then-axioms else-axioms)
	  (make-if-then-else-rel ntest nthen nelse))))))

(defun make-if-then-else-rel (test then else) (list :if test then else))

(defun normalize-loop-rel (rel)
  (let ((bnds (rel-loop-bounds rel)))
  (destructuring-bind (lo-axioms . nlo) (normalize-expr (bounds-lo bnds))
  (destructuring-bind (hi-axioms . nhi) (normalize-expr (bounds-hi bnds))
  (destructuring-bind (body-axioms . nbody) (normalize-rel (rel-loop-body rel))
    (cons (append lo-axioms hi-axioms body-axioms)
	  (make-loop-rel (rel-loop-var rel) nlo nhi nbody)))))))

(defun make-loop-rel (var lo hi body) `(:for ,var (,lo ,hi) ,body))

(defun normalize-exprs (exprs)
  (let* ((xexprs (mapcar #'normalize-expr exprs))
	 (axioms (apply #'append (mapcar #'car xexprs)))
	 (nexprs (mapcar #'cdr xexprs)))
    (cons axioms nexprs)))

(defun normalize-expr (x)
  (case (expr-class x)
	(:literal-num (cons '() x))
	(:variable (cons '() x))
	(:quant (normalize-qexpr x))
	(:array-app (normalize-aexpr x))
	(:funct-app (normalize-fexpr x))))

(defun normalize-qexpr (x)
  (if (eq :qand (quant-op x))
    (cons '() (normalize-qand x))
    (error "Not implemented: normalize-qexpr")))

(defun logical-var (x)
  (intern (strcat "?" (symbol-name x)) "KEYWORD"))

(defun normalize-qand (x)
  (let* ((v0 (quant-var x))
	 (v (logical-var v0))
	 (bnds (quant-bounds x))
	 (lo (qbnds-lo bnds))
	 (hi (qbnds-hi bnds))
	 (body0 (quant-body x))
	 (body (subst v v0 body0)))
    `(:|all| (,v) (:=> (:|and| (:|is-integer| ,v)
		               (:<= ,lo ,v) (:<= ,v ,hi))
		       ,body))))

(defun normalize-aexpr (x)
  (destructuring-bind (iaxioms . nargs) (normalize-iexprs (array-args x))
  (destructuring-bind (aaxioms . nop) (normalize-expr (array-op x))
    (if (every (lambda (a) (eq :index (index-class a))) nargs)
      (cons (append iaxioms aaxioms) (make-array-expr nop nargs))
      (let* ((saxioms (array-slice-axioms (mapcar #'index-class nargs)))
	     (axioms (append iaxioms aaxioms saxioms))
	     (sargs (mapcar #'iexpr->expr nargs)))
	(cons axioms (make-fct-expr :|@-slice| (cons nop sargs))))))))

(defun make-array-expr (op args) (list* :@ op args))

(defun array-slice-axioms (index-classes)
  (assoc-lookup (symbolize index-classes) *asa-map*))

(defun symbolize (index-classes)
  (intern (format nil "~{~a~^-~}" index-classes) "KEYWORD"))

(defparameter *asa-map-template*
   '((:index-all
       (all (?a ?i)
         (=> (and
                (= (num-dims ?a) 2) (is-integerp ?i)
                (<= ?i (array-length 1 ?a)))
              (and
                (= (num-dims *ia-slice*) 1)
                (= (array-length 1 *ia-slice*) (array-length 2 ?a))
                (all (?j)
                  (=> 
                    (and (is-integerp ?j) (<= ?j (array-length 2 ?a)))
                    (= (@ *ia-slice* ?j) (@ ?a ?i ?j))))))))
    (:index-range
       (all (?a ?i ?j1 ?j2)
         (=> (and
                (= (num-dims ?a) 2)
                (is-integerp ?i) (<= ?i (array-length 1 ?a))
                (is-integerp ?j1) (<= ?j1 (+ 1 ?j2))
                (is-integerp ?j2) (<= ?j2 (array-length 2 ?a)))
              (and
                (= (num-dims *ir-slice* 1))
                (= (array-length 1 *ir-slice*) (+ 1 (- ?j2 ?j1)))
                (all (?j)
                  (=>
                    (and (is-integerp ?j) (<= ?j (+ 1 (- ?j2 ?j1))))
                    (= (@ *ir-slice* ?j)
                        (@ ?a ?i (+ ?j (- ?j1 1))))))))))))

(defparameter *asa-map*
  (let ((ia-slice '(@-slice ?a ?i @-all))
	(ir-slice '(@-slice ?a ?i (list ?j1 ?j2))))
    (subst ir-slice '*ir-slice*
      (subst ia-slice '*ia-slice* *asa-map-template*))))

(defun iexpr->expr (iexpr)
  (case (index-class iexpr)
	(:range (make-fct-expr :list (list (range-lo iexpr) (range-hi iexpr))))
	(:index iexpr)
	(:all :@-all)))

(defun normalize-iexprs (iexprs)
  (let ((x (mapcar #'normalize-iexpr iexprs)))
    (cons (apply #'append (mapcar #'car x))
	  (mapcar #'cdr x))))

(defun normalize-iexpr (ie)
  (case (index-class ie)
	(:all
	  (cons '() ie))
	(:range
	  (destructuring-bind (lo-axioms . lo) (normalize-expr (range-lo ie))
	  (destructuring-bind (hi-axioms . hi) (normalize-expr (range-hi ie))
            (cons (append lo-axioms hi-axioms) (make-index-range lo hi)))))
	(:index (normalize-expr ie))))

(defun make-index-range (lo hi) (list :range lo hi))

(defun normalize-fexpr (x)
  (destructuring-bind (axioms . nargs) (normalize-exprs (fct-args x))
    (cons axioms (make-fct-expr (fct-op x) nargs))))

(defun make-fct-expr (fct args) (cons fct args))

(defun var-lengths-stmts (mdl)
  (let ((exprs (flatten (mapcar #'length-exprs (extract-vars mdl)))))
    (remove-duplicates
      (mapcar (lambda (x) (list (type-predicate :integernn) x)) exprs)
      :test #'equal)))

(defun length-exprs (decl)
  (let ((typ (decl-typ decl)))
    (unless (eq :array (type-class typ))
      (return-from length-exprs '()))
    (type-dims typ)))	  

(defparameter *basic-axioms*
  (to-kw
    '((is-integer 0)
      (is-integer 1)
      (is-integer 2)
      (is-integer 3)
      (all (?x) (=> (and (is-integer ?x) (<= 0 ?x)) (is-integernn ?x)))
      (<= 0 3)
      (<= 0 2)
      (<= 0 1)
      (all (?x ?y ?z) (=> (and (is-integer ?x) (is-integer ?y) (is-integer ?z)
  			     (<= ?x ?y) (<= ?y ?z))
  			(<= ?x ?z)))
      (all (?x ?y) (=> (and (is-integer ?x) (is-integer ?y))
  		     (is-integer (- ?x ?y))))
      (is-integer 1)
      (all (?x ?y) (=> (and (is-integer ?x) (is-integer ?y)
  			  (<= ?y ?x))
  		     (<= 0 (- ?x ?y))))
      (<= 1 3))))
|#