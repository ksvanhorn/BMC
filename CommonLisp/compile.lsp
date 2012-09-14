(in-package :compile)

;;; Main function

(defun compile-to-csharp (csharp-name-space class-name mdl)
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
  (write-csharp-class csharp-name-space class-name
		      (lambda () (write-csharp-class-body mdl))))

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
  (destructuring-bind (lo hi f) args
    (match-adt1 (expr-lambda var body) f
      (let ((lo-s (expr:expr->string lo))
	    (hi-s (expr:expr->string hi)))
	(fmt "QPROD ~a, ~a : ~a," var lo-s hi-s)
	(indent (print-factor body))))))

(defun print-if-then-else (args)
  (destructuring-bind (test exp-t exp-f) args
    (fmt "IF-THEN-ELSE ~a" (expr:expr->string test))
    (indent
      (print-factor exp-t)
      (print-factor exp-f))))

(defun write-csharp-class (csharp-name-space class-name write-body)
  (fmt "using System;")
  (fmt "using Common;")
  (fmt "")
  (fmt "namespace ~a" csharp-name-space)
  (fmt "{")
  (indent
    (fmt "[Serializable]")
    (fmt "public class ~a" class-name)
    (fmt "{")
    (indent
      (funcall write-body))
    (fmt "}"))
  (fmt "}"))

;;; Printing expressions

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
	     (t (error "Unimplemented case in compile::const->string: ~a."
		       x))))))

(defun variable->string (x) (symbol-name x))

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
	 (mapcar #'expr->string (mapcar #'dec-expr (rest args)))))

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
	(t (dec-expr x))))

(defun array-expr->string (e)
  (if (or (is-expr-const e) (is-expr-variable e))
    (expr->string e)
    (format t "(~a)" (expr->string e))))

(defun fexpr->string (fct args lprec rprec)
  (cond ((is-binop fct)
	 (bexpr->string fct args lprec rprec))
	((eq '@-idx fct)
	 (apply #'expr->string (append args (list lprec rprec))))
	(t
	 (format nil "~a(~{~a~^, ~})"
		 (fct-name fct) (mapcar #'expr->string args)))))

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
    (if (null a) (symbol-name op) (cdr a))))

(defconstant +oper-names+
  '((array-length . "BMC.Length")
    (qand . "BMC.ForAll")
    (qsum . "BMC.Sum")
    (=> . "BMC.Implies")
    (<=> . "BMC.Iff")
    (^ . "Math.Pow")
    (exp . "Math.Exp")
    (sqrt . "Math.Sqrt")
    (tanh . "Math.Tanh")
    (neg . "-")
    (inv . "BMC.MatrixInverse")
    (vec . "BMC.Vec")
    (= . "==")
    (.= . "==")
    (.<= . "<=")
    (.< . "<")
    (.> . ">")
    (.>= . ">=")
    (@-slice . "BMC.ArraySlice")
    (@-idx . "BMC.Idx")
    (@-rng . "BMC.Range")
    (is-realp . "BMC.IsRealp")
    (is-realnn . "BMC.IsRealnn")
    (is-real . "BMC.IsReal")
    (is-realx . "BMC.IsRealx")
    (is-integerp0 . "BMC.IsIntegerp0")
    (is-integerp . "BMC.IsIntegerp")
    (is-symm-pd . "BMC.IsSymmetricPositiveDefinite")))	 

;;; End printing expressions

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

(defun write-csharp-class-body (mdl)
  (write-csharp-declarations mdl)
  (fmt "")
  (write-csharp-load-arguments (model-args mdl))
  (fmt "")
  (write-csharp-validate-arguments mdl)
  (fmt "")
  (write-csharp-allocate-vars (model-vars mdl))
  (fmt "")
  (write-csharp-log-joint-density mdl)
  (fmt "")
  (flet ((f () (mapc #'write-prior-draw-rel (model-body mdl))))
    (write-prior-draw #'f))
  (fmt "")
  (write-undefine-all-vars (model-vars mdl))
  ; ... more ...
)

(defun write-prior-draw (gen-body)
  (fmt "void Draw() {")
  (indent
    (funcall gen-body))
  (fmt "}"))  

(defun write-csharp-declarations (mdl)
  (gen-decls "Model Arguments" (model-args mdl))
  (fmt "")
  (gen-decls "Model Variables" (model-vars mdl)))

(defun gen-decls (comment decls)
  (fmt "// ~a" comment)
  (dolist (x decls)
    (gen-decl x)))

(defun gen-decl (d)
  (match-adt1 (decl var typ) d
    (fmt "public ~a ~a;" (type-string typ) (symbol-name var))))

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
  (fmt "{")
  (indent (wcs-allocate-vars var-decls))
  (fmt "}"))

(defun wcs-allocate-vars (var-decls)
  (dolist (d var-decls)
    (match-adt1 (decl var typ) d
      (when (is-vtype-array typ)
	(fmt "~a = ~a;" (symbol-name var) (csharp-new-string typ))))))

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
  (fmt "{")
  (indent
    (dolist (d arg-decls)
      (match-adt1 (decl var typ) d
	(let ((vname (symbol-name var))
	      (infix (csharp-load-type-name typ))
	      (dim-strings (type->dim-strings typ)))
	  (fmt "~a = loader.Load~a(\"~a\"~{, ~a~});"
	       vname infix vname dim-strings)))))
  (fmt "}"))

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
  (fmt "public void ValidateArguments()")
  (fmt "{")
  (indent
    (dolist (x (args-checks mdl))
      (let ((bool-expr (expr->string x)))
      (fmt "BMC.Check(~a, " bool-expr)
      (fmt "          \"~a\");" bool-expr))))
  (fmt "}"))

(defun args-checks (mdl)
  (remove-if #'is-trivially-true
	     (remove-duplicates (args-checks-raw mdl) :test #'equalp)))

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
  (let* ((excluded (cons var-sym (vars-in-expr-list dims)))
	 (idxvar-symbols (n-symbols-not-in (length dims) excluded))
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
	(setf ch (expr-call 'qand (expr-const 1) dim (expr-lam idxvar ch)))))
    ch))

(defun write-csharp-log-joint-density (mdl)
  (let* ((excluded (vars-in-model mdl))
	 (accum-var (symbol-not-in excluded "ljd")))
    (fmt "public double LogJointDensity()")
    (fmt "{")
    (indent
      (fmt "double ~a = 0.0;" accum-var)
      (dolist (r (model-body mdl))
	(write-ljd-accum-rel accum-var r))
      (fmt "return (Double.IsNaN(~a) ? Double.NegativeInfinity : ~a);"
	   accum-var accum-var))
    (fmt "}")))

(defun write-ljd-accum-rel (accum rel)
  (let ((*ljd-accum* accum))
    (write-ljd-acc-rel rel)))

(defparameter *ljd-accum* nil)

(defun write-ljd-acc-rel (rel)
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
     (write-ljd-acc-rel-let var val body))
    ((skip))))

(defun write-ljd-acc-rel-let (var val body)
  (fmt "{")
  (indent
    (fmt "var ~a = ~a;" (symbol-name var) (expr->string val))
    (write-ljd-acc-rel body))
  (fmt "}"))

(defun write-ljd-acc-rel-stoch (lhs rhs)
  (match-adt1 (distribution name args) rhs
    (let ((lhs-str (crellhs->string lhs))
	  (dname (csharp-distr-density-name name))
	  (args-str-list (mapcar #'expr->string args)))
      (fmt "~a += ~a(~a~{, ~a~});" *ljd-accum* dname lhs-str args-str-list))))

(defun crellhs->string (lhs)
  (expr->string (rellhs->expr lhs)))

(defun csharp-distr-density-name (distr-symbol)
  (strcat
    "BMC.LogDensity" (assoc-lookup distr-symbol +csharp-distr-name-assoc+)))

(defconstant +csharp-distr-name-assoc+
  '((ddirch . "Dirichlet")
    (dcat . "Cat")
    (dgamma . "Gamma")
    (dnorm . "Norm")
    (dwishart . "Wishart")
    (dmvnorm . "MVNorm")
    (dinterval . "Interval")))

(defun write-ljd-acc-rel-block (members)
  (dolist (r members)
    (write-ljd-acc-rel r)))

(defun write-ljd-acc-rel-if (condition true-branch false-branch)
  (fmt "if (~a) {" (expr->string condition))
  (indent
    (write-ljd-acc-rel true-branch))
  (fmt "}")
  (when (not (is-relation-skip false-branch))
    (fmt "else {")
    (indent
      (write-ljd-acc-rel false-branch))
    (fmt "}")))

(defun write-ljd-acc-rel-loop (var lo hi body)
  (let ((var-str (variable->string var))
	(lo-str (expr->string lo))
	(term-str (termination-test-string var hi)))
    (fmt "for (int ~a = ~a; ~a; ++~a) {" var-str lo-str term-str var-str)
    (indent
      (write-ljd-acc-rel body))
    (fmt "}")))

(defun termination-test-string (var hi)
  (expr->string (expr-call '.<= (expr-var var) hi)))

(defun write-prior-draw-rel (rel)
  (adt-case relation rel
    ((stochastic lhs rhs)
     (write-prior-draw-rel-stoch lhs rhs))
    ((block members)
     (mapc #'write-prior-draw-rel members))
    ((if condition true-branch false-branch)
     (write-prior-draw-rel-if condition true-branch false-branch))
    ((loop var lo hi body)
     (write-prior-draw-rel-loop var lo hi body))
    ((let var val body)
     (write-prior-draw-rel-let var val body))
    ((skip)
     )))

(defun write-prior-draw-rel-let (var val body)
  (fmt "{")
  (indent
    (fmt "var ~a = ~a;" (symbol-name var) (expr->string val))
    (write-prior-draw-rel body))
  (fmt "}"))

(defun write-prior-draw-rel-stoch (lhs rhs)
  (match-adt1 (distribution name args) rhs
    (if (is-scalar-distr name)
      (fmt "~a = ~a(~{~a~^, ~});"
	   (crellhs->string lhs)
	   (csharp-distr-draw-name name)
	   (mapcar #'expr->string args))
      (fmt "~a(~a~{, ~a~});"
	   (csharp-distr-draw-name name)
	   (crellhs->string lhs)
	   (mapcar #'expr->string args)))))

(defun is-scalar-distr (symbol)
  (member symbol +scalar-distributions+))

(defconstant +scalar-distributions+
  '(dcat dnorm dgamma dinterval))

(defun csharp-distr-draw-name (symbol)
  (strcat
    "BMC.Draw" (assoc-lookup symbol +csharp-distr-name-assoc+)))

(defun write-prior-draw-rel-if (condition true-branch false-branch)
  (fmt "if (~a) {" (expr->string condition))
  (indent
    (write-prior-draw-rel true-branch))
  (fmt "}")
  (when (not (is-relation-skip false-branch))
    (fmt "else {")
    (indent
      (write-prior-draw-rel false-branch))
    (fmt "}")))

(defun write-prior-draw-rel-loop (var lo hi body)
  (let ((var-str (variable->string var))
	(lo-str (expr->string lo))
	(term-str (expr->string (expr-call '.<= (expr-var var) hi))))
    (fmt "for (int ~a = ~a; ~a; ++~a) {"
	 var-str lo-str term-str var-str)
    (indent
      (write-prior-draw-rel body))
    (fmt "}")))

(defun write-undefine-all-vars (var-decls)
  (fmt "private void UndefineAllVars() {")
  (indent
    (dolist (d var-decls)
      (match-adt1 (decl var typ) d
        (adt-case vtype typ
	  ((scalar stype)
	   (write-undefine-scalar-var var stype))
	  ((array elem-type dims)
	   (write-undefine-array-var var elem-type dims))))))
  (fmt "}"))

(defun write-undefine-scalar-var (var stype)
  (let ((btyp (base-type stype)))
    (let ((vname (variable->string var))
	  (val (undefined-val btyp)))
      (fmt "~a = ~a;" vname val))))

(defun undefined-val (base-type)
  (case base-type
	('integer "BMC.InvalidInteger")
	('realxn "Double.NaN")
	(otherwise
	   (error "Unimplemented case in undefined-val: ~a" base-type))))

(defun write-undefine-array-var (var elem-type dims)
  (let* ((used-vars (vars-in-expr-list dims))
	 (idx-vars (n-symbols-not-in (length dims) used-vars))
	 (val (undefined-val (base-type elem-type)))
	 (lhs (make-rellhs-array-elt
	        :var var :indices (mapcar #'expr-var idx-vars)))
	 (lhs-s (crellhs->string lhs)))
    (labels ((f (iv-list dim-list)
	       (if (null dim-list)
		 (fmt "~a = ~a;" lhs-s val)
		 (let* ((iv (first iv-list))
			(iv-s (variable->string iv))
			(term-s (termination-test-string iv (car dim-list))))
		   (fmt "for (int ~a = 1; ~a; ++~a) {" iv-s term-s iv-s)
		   (indent (f (rest iv-list) (rest dim-list)))
		   (fmt "}")))))
      (f idx-vars dims))))

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
     (expr-call 'qprod lo hi (expr-lam var (rel->pdf body))))
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