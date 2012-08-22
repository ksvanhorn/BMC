(defmacro comment (&rest _) nil)

(defun $EXTRACT_MODEL_ARGS (mdl)
  (destructuring-bind
    (file-kw (args-kw &rest args) &rest _) mdl
    (verify (and (listp args-kw) (eql '$ARGS (car args-kw))))
    (mlist (mapcar #'second args))))

(defun $EXTRACT_MODEL_VARS (mdl)
  (destructuring-bind
    (file-kw args-section reqs-section (vars-kw &rest vars) &rest _) mdl
      (verify (and (listp vars-kw) (eql '$VARS (car vars-kw))))
      (mlist (mapcar #'second vars))))

(defun $MODEL_SYNTAX_ERROR (mdl)
  (let ((*bmc-vars-in-scope* +bmc-constants+)
	(*bmc-error-messages* nil))
    (if (not (is-headed-expr '$FILE mdl))
	(errmsg "Head must be 'file'"))
    (if (/= 5 (length mdl))
	(errmsg "Must have 4 arguments"))
    (when (null *bmc-error-messages*)
      (check-valid-args (second mdl))
      (check-valid-reqs (third mdl))
      (check-valid-vars (fourth mdl))
      (check-valid-rels (fifth mdl)))
    (and *bmc-error-messages*
	 (format nil "~{~a~^~%~}" *bmc-error-messages*))))

(defparameter *bmc-vars-in-scope* nil)
(defconstant +bmc-constants+ '($%E $%PI))
(defparameter *bmc-error-messages* nil)

(defun mstring (x) (mfuncall '$STRING x))

(defun errmsg (format-string &rest args)
  (push (apply #'format nil (mapcar #'mstring args)) *bmc-error-messages*))

(defun is-true-list (x)
  (do ((y x (cdr y)))
      ((not (consp y)) (null y))))

(defun is-headed-expr (s x)
  (and (consp x)
       (consp (car x))
       (eql s (caar x))))

(defun check-valid-args (x)
  (block nil
    (unless (is-headed-expr '$ARGS x)
      (errmsg "Head of first argument must be 'args'")
      (return))
    (let ((arg-decls (cdr x)))
      (unless (is-true-list args)
	(errmsg "Body of args section is not a list")
	(return))
      (dolist (decl arg-decls) (check-valid-arg-decl decl)))))

(defun check-valid-arg-decl (x)
  (block nil
    (unless (and (is-headed-expr 'MSETQ x) (= 3 (length x)))
      (errmsg "Invalid argument declaration: ~a" x)
      (return))
    (let ((lhs (second x))
	  (rhs (third x)))
      (cond
        ((not (symbolp lhs))
	 (errmsg "LHS of argument declaration must be a name: ~a" x))
	((member lhs *vars-in-scope*)
	 (errmsg "Argument redeclared: ~a" x))
	(t (push lhs *vars-in-scope*)))
      (unless (is-valid-type (cdr *vars-in-scope*) rhs)
	(errmsg "Invalid type in argument declaration: ~a" x)))))

(defun is-valid-type (legal-vars x)
  (or (is-atomic-type x) (is-array-type legal-vars x)))

(defun is-atomic-type (x)
  (member x '($REAL $REALP $REALNN $INTEGER $INTEGERP $INTEGERNN $BOOL)))

(defun is-array-type (legal-vars x)
  (and (is-true-list x)
       (< 1 (length x))
       (is-true-list (car x))
       (is-atomic-type (caar x))
       (member 'ARRAY (cdar x))
       (every (lambda (x) (only-vars-from legal-vars x)) (cdr x))))

(defconstant +bmc-functions+
  '(MPLUS MTIMES MEXPT %SUM %PRODUCT
    MGEQP MGREATERP MLEQP MLESSP EQUAL NOTEQUAL))

(defun only-vars-from (var-set x)
  ; *** FILL IN ***
)



; args: syntactic correctness, no arg declared twice, no arg referenced
;   before it is used.
;;; proof obligation: dimensions are nonnegative integers
;;; assumptions: type information
; reqs: syntactic correctness, only args referenced
;;; assumptions: the reqs
; vars: syntactic correctness, no var declared twice, only args referenced
;;; proof obligation: dimensions are nonnegative integers
;;; assumptions: vars are scalars or arrays of declared dimensions
; model: syntactic correctness, no undeclared vars referenced, each
;   element of each var defined only once.
;;; proof obligation:


(comment
(defun $EXTRACT_ASSUMPTIONS (mdl)
  (destructuring-bind
    (file-kw (args-kw &rest args)
	     (reqs-kw &rest reqs)
	     (vars-kw &rest vars)
	     (model-kw &rest rels)) mdl
    (mlist (append (extract-args-assumptions args)
		   reqs
		   (extract-vars-assumptions vars)))))
)

(defun mlist (x) `((MLIST) ,@x))

(comment

(defun extract-precondition (mdl)
  (destructuring-bind
    (file-kw (args-kw &rest args) (reqs-kw &rest reqs) &rest _) mdl
    (append (extract-type-reqs args) reqs)))

(defun $bar ()
  `((MLIST) (($IS_INTEGERNN) $NRESP) (($IS_ARR) $MU_NU $IS_REAL ((MLIST) 3))))

(defun $extract_assumptions (mdl-name)
  (let ((mdl (eval (maxima-symbol-to-lisp-symbol mdl-name))))
    ;(format t "mdl-name: ~a~%" mdl-name)
    (extract-assumptions mdl)))

(defun maxima-symbol-to-lisp-symbol (s)
  (intern (subseq (symbol-name s) 1)))

(defun extract-assumptions (mdl)
  (destructuring-bind
    (file-kw (args-kw &rest args)
	     (reqs-kw &rest reqs)
	     (vars-kw &rest vars) &rest _) mdl
    (let ((assums (append (mapcar #'extract-type-pred args)
		 	  reqs
			  (mapcar #'extract-type-pred vars))))
      (print assums)
      `((MLIST) ,@assums))))

(defun extract-type-pred (x)
  (destructuring-bind (var (pred &rest rest)) x
     (verify (symbolp var))
     (verify (symbolp pred))
     ;(format t "var: ~a~%" var)
     ;(format t "pred: ~a~%" pred)
     (let ((mvar (to-maxima-symbol var))
	   (mpred (list (to-maxima-symbol pred))))
       (if (eql 'is_arr pred)
	   (progn
	     (verify (= 2 (length rest)))
	     (let ((type_pred (to-maxima-symbol (car rest)))
		   (dims (mapcar #'to-maxima-expr (cadr rest))))
	       `(,mpred ,mvar ,type_pred ((MLIST) ,@dims))))
	 (progn
	   (verify (= 0 (length rest)))
	   `(,mpred ,mvar))))))

(defun to-maxima-expr (x)
  (cond
    ((symbolp x)
     (to-maxima-symbol x))
    ((numberp x)
     x)
    (t
     (destructuring-bind (op &rest args) x
	(let ((maxima-op (to-maxima-op op))
	      (maxima-args (mapcar #'to-maxima-expr args)))
	  (cond
	    ((and (eql '- op) (= 2 (length args)))
	     (let ((a1 (first maxima-args))
		   (a2 (second maxima-args)))
	       `((MPLUS) ,a1 ((MTIMES) -1 ,a2))))
	    ((and (eql '/ op) (= 2 (length args)))
	     (let ((a1 (first maxima-args))
		   (a2 (second maxima-args)))
	       `((MTIMES) ,a1 ((MEXPT) ,a2 -1))))
	    (t
	     `((,maxima-op) ,@maxima-args))))))))

(defun to-maxima-symbol (s)
  (intern (concatenate 'string "$" (symbol-name s))))

(defun to-maxima-op (x)
  (cond
    ((eql '+ x) 'MPLUS)
    ((eql '* x) 'MTIMES)
    ((eql '^ x) 'MEXPT)
    ((eql '>= x) 'MGEQP)
    ((eql '> x) 'MGREATERP)
    ((eql '<= x) 'MLEQP)
    ((eql '< x) 'MLESSP)
    ((eql '= x) '$EQUAL)
    ((eql '!= x) '$NOTEQUAL)
    (t (to-maxima-symbol x))))

(defmacro verify (expr)
  `(unless ,expr (error (format nil "Assertion failed: ~a" ',expr))))

(defun extract-type (x &key (strict nil))
  (destructuring-bind (type-ctor &rest type-args) x
     (cond ((member type-ctor '(is_real is_realnn is_realp))
	    (verify (eql nil type-args))
	    (verify (or (not strict) (eql 'is_real type-ctor)))
	    'real)
	   ((member type-ctor '(is_integer is_integernn is_integerp))
	    (verify (eql nil type-args))
	    (verify (or (not strict) (eql 'is_integer type-ctor)))
	    'integer)
	   ((eql type-ctor 'is_bool)
	    (verify (eql nil type-args))
	    'bool)
	   ((eql type-ctor 'is_arr)
	    (destructuring-bind (type-pred dims) type-args
	       (let ((eltype (extract-type (list type-pred) :strict strict)))
		 (list* 'arr eltype dims))))
	   (t (verify nil)))))

(defun extract-type-reqs (args)
  (remove-if #'null (mapcar #'extract-type-restriction-from-decl args)))

(defun extract-type-restriction-from-decl (x)
  (destructuring-bind (var (type-ctor &rest type-args)) x
    (cond ((member type-ctor '(is_realnn is_integernn))
	   `(<= 0 ,var))
	  ((member type-ctor '(is_realp is_integerp))
	   `(< 0 ,var))
	  ((eql type-ctor 'is_arr)
	   (destructuring-bind (eltype dims) type-args
	     (let ((pred (cond
		 	   ((member eltype '(is_realnn is_integernn))
			    (lambda (x) `(<= 0 ,x)))
			   ((member eltype '(is_realp is_integerp))
			    (lambda (x) `(< 0 ,x)))
			   (t nil))))
	       (and pred (arr-for-all var dims pred '())))))
	  (t nil))))

(defun arr-for-all (var dims pred idx_vars)
  (if (null dims)
    (funcall pred `(@ ,var ,@(reverse idx_vars)))
    (let* ((idx1 (gensym))
          (inner (arr-for-all var (cdr dims) pred (cons idx1 idx_vars))))
      `(for_all ,idx1 (1 ,(car dims)) ,inner))))
)
; List of args with programming-language types [DONE]
; List of state vars with programming-language types [DONE]
; Precondition on args [DONE]
;;; Generate code to check precondition
; Generate check of initial state for validity
; Check that we have a DAG
; Check that all expressions well-defined
; 
