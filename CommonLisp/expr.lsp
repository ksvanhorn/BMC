(in-package :expr)

(defadt expr
  (const name)
  (variable symbol)
  (apply fct args)
  (lambda var body))

(defun expr-call (fct-symbol &rest args)
  (expr-app fct-symbol args))

(defun expr-app (fct-symbol args)
  (make-expr-apply :fct fct-symbol :args args))

(defun expr-var (v)
  (make-expr-variable :symbol v))

(defun expr-const (x)
  (make-expr-const :name x))

(defun expr-lam (var body)
  (make-expr-lambda :var var :body body))

(defun free-vars-in-expr (e)
  (adt-case expr e
    ((const name) '())
    ((variable symbol) (list symbol))
    ((lambda var body)
     (remove var (free-vars-in-expr body)))
    ((apply fct args)
     (append-mapcar #'free-vars-in-expr args))))

(defun occurs-free (var-symbol e)
  (adt-case expr e
    ((const name)
     nil)
    ((variable symbol)
     (eq var-symbol symbol))
    ((lambda var body)
     (and (not (eq var-symbol var)) (occurs-free var-symbol body)))
    ((apply fct args)
     (some (lambda (x) (occurs-free var-symbol x)) args))))

(defun rename-var (old-var new-var e)
  (when (eq old-var new-var)
    (return-from rename-var e))
  (flet* ((subst (x)
	    (adt-case expr x
	      ((const name) x)
	      ((variable symbol)
	       (if (eq old-var symbol)
		 (expr-var new-var)
		 x))
	      ((apply fct args)
	       (expr-app fct (mapcar #'subst args)))
	      ((lambda var body)
	       (if (eq old-var var)
		 x
		 (expr-lam var (subst body)))))))
    (subst e)))

;;; Converting s-expressions to exprs

(defun xformer-sexpr->expr (stream subchar arg)
  (let ((sexp (read stream t)))
    (sexpr->expr sexp)))

(set-dispatch-macro-character #\# #\e #'xformer-sexpr->expr)

(defun sexpr->expr (x)
  (cond
    ((is-literal-value x)
     (make-expr-const :name x))
    ((is-const-symbol x)
     (make-expr-const :name x))
    ((symbolp x)
     (make-expr-variable :symbol (vars-symbol x)))
    ((consp x)
     (destructuring-bind (op . args) x
       (cond ((eq :quant op)
	      (sexpr->expr-quantifier args))
	     ((eq :let op)
	      (sexpr->expr-let args))
	     ((eq :lambda op)
	      (sexpr->expr-lambda args))
	     ((eq '@ op)
	      (sexpr->expr-array-app op args))
	     ((is-fct-symbol op)
	      (make-expr-apply
	        :fct (convert-function-symbol op)
		:args (mapcar #'sexpr->expr args)))
	     (t
	      (error "Illegal symbol (~W) at beginning of expression" op)))))
    (t
     (error "Unrecognized expression type: ~W" x))))

(defun sexpr->expr-let (args)
  (destructuring-bind ((var val) body) args
    (unless (is-variable-symbol var)
      (error "Invalid variable symbol in ~W." (cons :let args)))
    (make-expr-apply
      :fct '!
      :args (list (make-expr-lambda :var (vars-symbol var)
				    :body (sexpr->expr body))
		  (sexpr->expr val)))))

(defun sexpr->expr-lambda (args)
  (destructuring-bind (var body) args
    (make-expr-lambda :var (vars-symbol var) :body (sexpr->expr body))))

(defun split-quant-args (args)
  (let ((args-rev nil)
	(shape-arg nil))
    (loop while (consp args) do
      (if (eq :shape (car args))
	(let ((shape-args (cdr args)))
	  (unless (= 1 (length shape-args))
	    (error ":shape keyword must be followed by exactly one argument"))
	  (setf shape-arg (first shape-args))
	  (setf args '()))
	(progn
	  (push (car args) args-rev)
	  (setf args (cdr args)))))
    (cons shape-arg (reverse args-rev))))

(defun sexpr->expr-quantifier (args)
  (destructuring-bind (shape . args1) (split-quant-args args)
    (sexpr->expr-quantifier1 shape args1)))

(defun sexpr->expr-quantifier1 (shape-sexp args)
  (unless (or (= 4 (length args)) (= 5 (length args)))
    (error "Wrong number of args for quantifier: ~w" args))
  (destructuring-bind (op var (lo-x hi-x) filter-or-body . maybe-body) args
    (unless (is-fquant-symbol op)
      (error "Operator ~A of quantifier expression ~W ~
              is not a valid quantifier symbol" op (cons ':quant args)))
    (unless (is-variable-symbol var)
      (error "Index var ~W of quantifier expression ~W ~
              is not a valid variable symbol" var (cons ':quant args)))
    (let ((v (vars-symbol var))
	  (lo (sexpr->expr lo-x))
	  (hi (sexpr->expr hi-x))
	  (filter-x (if maybe-body filter-or-body nil))
	  (body-x (if maybe-body (car maybe-body) filter-or-body)))
      (let ((filter (if filter-x
			(expr-lam v (sexpr->expr filter-x))
		      (expr-const '%true-pred)))
	    (body (expr-lam v (sexpr->expr body-x)))
	    (shape (mapcar #'sexpr->expr shape-sexp)))
        (expr-app op (list* lo hi filter body shape))))))

(defun sexpr->expr-array-app (_ args)
  (unless (and (consp args) (< 1 (length args)))
    (error "Invalid array application: ~W." (cons '@ args)))
  (let ((indices (cdr args)))
    (if (every #'is-scalar-index indices)
      (make-expr-apply
        :fct '@ :args (mapcar #'sexpr->expr args))
      (make-expr-apply
        :fct '@-slice 
	:args (cons (sexpr->expr (car args))
		    (mapcar #'sexpr->slice-arg indices))))))

(defun is-scalar-index (x)
  (not (or (is-slice-all x) (is-slice-range x))))

(defun sexpr->slice-arg (x)
  (cond ((is-slice-all x) (make-expr-const :name '@-all))
	((is-slice-range x)
	 (destructuring-bind (lo-x hi-x) (cdr x)
	   (let ((lo (sexpr->expr lo-x))
		 (hi (sexpr->expr hi-x)))
	     (make-expr-apply :fct '@-rng :args (list lo hi)))))
	(t
	 (make-expr-apply :fct '@-idx :args (list (sexpr->expr x))))))

(defun is-slice-all (x) (eq :all x))

(defun is-slice-range (x) (starts-with :range x))

(defun is-literal-value (x) (realp x))

;;; pretty-printing exprs

(defun expr->string (e &optional (lprec -1) (rprec -1))
  (adt-case expr e
    ((const name)
     (const->string name))
    ((variable symbol)
     (variable->string symbol))
    ((lambda var body)
     (error "Cannot print lambda expression: ~w." e))
    ((apply fct args)
     (cond ((is-fquant-symbol fct)
	    (qexpr->string fct args))
	   ((and (eq '! fct) (= 2 (length args)))
	    (destructuring-bind (arg1 arg2) args
	      (if (is-expr-lambda arg1)
		(match-adt1 (expr-lambda var body) arg1
		  (lexpr->string var arg2 body))
		(aexpr->string fct args lprec rprec))))
	   (t
	    (aexpr->string fct args lprec rprec))))))

(defun lexpr->string (var val body)
  (format nil "(let ~a = ~a in ~a)"
	  (variable->string var) (expr->string val) (expr->string body)))

(defun const->string (x)
  (cond ((integerp x) (format nil "~d" x))
	((rationalp x) (format nil "~a" x))
	((realp x) (format nil "~,,,,,,'EE" x))
	((symbolp x) (symbol-name x))
	(t (error "Unimplemented case in expr::literal->string: ~a." x))))

(defun variable->string (x) (symbol-name x))

(defun qexpr->string (fct args) ;(op lo hi var body)
  (unless (and (= 4 (length args))
	       (is-expr-lambda (fourth args)))
    (error "Invalid argument list for ~a: ~w" fct args))
  (destructuring-bind (lo hi filter lexpr) args
    (match-adt1 (expr-lambda var body) lexpr
      (let ((op-s (fct-name fct))
	    (lo-s (expr->string lo))
	    (hi-s (expr->string hi))
	    (var-s (variable->string var))
	    (body-s (expr->string body)))
	(format nil "~a(~a, ~a : ~a, ~a)" op-s var-s lo-s hi-s body-s)))))

(defun aexpr->string (fct args lprec rprec)
  (case fct
	('@ (@expr->string args #'expr->string))
	('@-slice (@expr->string args #'iexpr->string))
	(otherwise (fexpr->string fct args lprec rprec))))

(defun @expr->string (args arg->string)
  (unless (and (consp args) (< 1 (length args)))
    (error "@ expression must have at least two arguments"))
  (format nil "~a[~{~a~^, ~}]"
	 (array-expr->string (first args))
	 (mapcar arg->string (rest args))))

(defun array-expr->string (e)
  (if (or (is-expr-const e) (is-expr-variable e))
    (expr->string e)
    (format nil "(~a)" (expr->string e))))

(defun iexpr->string (e)
  (adt-case expr e
    ((const name)
     (if (eq '@-all name) "" (expr->string e)))
    ((variable symbol)
     (expr->string e))
    ((lambda var body)
     (error "A lambda expression cannot be an array slice argument: ~w." e))
    ((apply fct args)
     (case fct
	   ('@-idx
	    (check-num-args '@-idx 1 args)
	    (expr->string (first args)))
	   ('@-rng
	    (check-num-args '@-rng 2 args)
	    (format nil "~a : ~a"
		    (expr->string (first args)) (expr->string (second args))))
	   (otherwise
	    (expr->string e))))))

(defun check-num-args (fct-symbol n args)
  (unless (= n (length args))
    (error "Function ~a has ~a arguments, requires ~a."
	   fct-symbol (length args) n)))

(defun fexpr->string (fct args lprec rprec)
  (if (is-binop fct)
    (bexpr->string fct args lprec rprec)
    (format nil "~a(~{~a~^, ~})"
	    (fct-name fct) (mapcar #'expr->string args))))

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
	   (format s " ~a ~a" (symbol-name op) (expr->string e lpr rpr))))
      (when use-parens (princ #\) s)))))

(defun precedences (op) (assoc-lookup op +precedences+))

(defun is-binop (x) (assoc x +precedences+))

(defparameter +precedences+
  '((<  50 . 50) (<= 50 . 50) (= 50 . 50) (!= 50 . 50) (> 50 . 50) (>= 50 . 50)
    (.<  50 . 50) (.<= 50 . 50) (.= 50 . 50) (.!= 50 . 50) (.> 50 . 50) (.>= 50 . 50)
    (and 40 . 41) (or 30 . 31) (=> 20 . 21)  (<=> 10 . 11)
    (.and 40 . 41) (.or 30 . 31) (.=> 20 . 21)  (.<=> 10 . 11)
    (+ 100 . 101) (- 100 . 101) (* 110 . 111) (*! 110 . 111) (/ 110 . 111) (^ 121 . 120)
    (! 200 . 201)))

(defun fct-name (op) (symbol-name op))

(defun convert-function-symbol (s)
  (if (and *convert-boolean-functions* (member s +boolean-functions+))
      (intern (strcat "." (symbol-name s)) "SYMBOLS")
      s))
(defparameter *convert-boolean-functions* nil)
(defparameter +boolean-functions+
  '(= != < <= > >= and or not => <=> qand qor is-symm-pd
    is-boolean is-integer is-integerp0 is-integerp
    is-realxn is-realx is-real is-realp0 is-realp))

(defun is-let-expr (e)
  (and (is-expr-apply e)
       (eq '! (expr-apply-fct e))
       (let ((args (expr-apply-args e)))
	 (and (is-list-of-length 2 args)
	      (destructuring-bind (f val) args
                (and (is-expr-lambda f)
		     (list (expr-lambda-var f) val (expr-lambda-body f))))))))

(defun is-quant-expr (e)
  (adt-case expr e
    ((apply fct args)
     (and (is-fquant-symbol fct)
	  (<= 4 (length args))
	  (every #'is-expr-lambda (subseq args 2 4))
	  (cons fct args)))
    (otherwise nil)))
