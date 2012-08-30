(in-package :expr)

(defadt expr
  (literal value)
  (const symbol)
  (variable symbol)
  (quantifier op lo hi var body)
  (apply fct args))

(defun expr-call (fct-symbol &rest args)
  (expr-app fct-symbol args))

(defun expr-app (fct-symbol args)
  (make-expr-apply :fct fct-symbol :args args))

(defun expr-lit (x)
  (make-expr-literal :value x))

(defun expr-var (v)
  (make-expr-variable :symbol v))

;;; Converting s-expressions to exprs

(defun sexpr->expr (x)
  (cond
    ((is-literal-value x)
     (make-expr-literal :value x))
    ((is-const-symbol x)
     (make-expr-const :symbol x))
    ((symbolp x)
     (make-expr-variable :symbol x))
    ((consp x)
     (destructuring-bind (op . args) x
       (cond ((is-fquant-symbol op)
	      (sexpr->expr-quantifier op args))
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

(defun sexpr->expr-quantifier (op args)
  (destructuring-bind (var (lo-x hi-x) body-x) args
    (let ((lo (sexpr->expr lo-x))
	  (hi (sexpr->expr hi-x))
	  (body (sexpr->expr body-x)))
      (unless (is-variable-symbol var)
	(error "Index var ~W of quantifier expression ~W ~
                is not a valid variable symbol" var (cons op args)))
      (make-expr-quantifier :op op :lo lo :hi hi :var var :body body))))

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
  (cond ((is-slice-all x) (make-expr-const :symbol '@-all))
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
    ((literal value)
     (write-to-string value))
    ((const symbol)
     (symbol-name symbol))
    ((variable symbol)
     (symbol-name symbol))
    ((quantifier op lo hi var body)
     (qexpr->string op lo hi var body))
    ((apply fct args)
     (aexpr->string fct args lprec rprec))))

(defun qexpr->string (op lo hi var body)
  (let ((op-s (fct-name op))
	(lo-s (expr->string lo))
	(hi-s (expr->string hi))
	(var-s (symbol-name var))
	(body-s (expr->string body)))
    (quant-format op-s lo-s hi-s var-s body-s)))

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
  (cond ((is-expr-literal e)
	 (error "Cannot dereference a literal: ~W." e))
	((or (is-expr-const e) (is-expr-variable e))
	 (expr->string e))
	(t
	 (format t "(~a)" (expr->string e)))))

(defun iexpr->string (e)
  (when (or (is-expr-literal e) (is-expr-variable e) (is-expr-quantifier e))
    (return-from iexpr->string (expr->string e)))
  (adt-case expr e
    ((const symbol)
     (if (eq '@-all symbol) "" (expr->string e)))
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
	   (format s " ~a ~a" (fct-name op) (expr->string e lpr rpr))))
      (when use-parens (princ #\) s)))))

; (precedences op)

(defun precedences (op) (assoc-lookup op +precedences+))

(defconstant +precedences+
  '((<  50 . 50) (<= 50 . 50) (= 50 . 50) (!= 50 . 50) (> 50 . 50) (>= 50 . 50)
    (.<  50 . 50) (.<= 50 . 50) (.= 50 . 50) (.!= 50 . 50) (.> 50 . 50) (.>= 50 . 50)
    (and 40 . 41) (or 30 . 31) (=> 20 . 21)  (<=> 10 . 11)
    (.and 40 . 41) (.or 30 . 31) (.=> 20 . 21)  (.<=> 10 . 11)
    (+ 100 . 101) (- 100 . 101) (* 110 . 111) (/ 110 . 111) (^ 121 . 120)))

(defun is-binop (x) (funcall *is-binop* x))
(defun default-is-binop (x) (assoc x +precedences+))
(defparameter *is-binop* #'default-is-binop)

(defun fct-name (x) (funcall *fct-name* x))
(defun default-fct-name (x) (symbol-name x))
(defparameter *fct-name* #'default-fct-name)

(defun quant-format (op-s lo-s hi-s var-s body-s)
  (funcall *quant-format* op-s lo-s hi-s var-s body-s))
(defun default-quant-format (op-s lo-s hi-s var-s body-s)
  (format nil "~a(~a, ~a : ~a, ~a)" op-s var-s lo-s hi-s body-s))
(defparameter *quant-format* #'default-quant-format)

(defun convert-function-symbol (s)
  (if (and *convert-boolean-functions* (member s +boolean-functions+))
      (intern (strcat "." (symbol-name s)))
      s))
(defparameter *convert-boolean-functions* nil)
(defconstant +boolean-functions+
  '(= != < <= > >= and or not => <=> qand qor is-symm-pd
    is-boolean is-integer is-integerp0 is-integerp
    is-realxn is-realx is-real is-realp0 is-realp))

(defmacro with-print-options (is-binop-kw is-binop-fct
			      fct-name-kw fct-name-fct
                              quant-format-kw quant-format
			      &rest body)
  (unless (and (eq :is-binop is-binop-kw)
	       (eq :fct-name fct-name-kw)
	       (eq :quant-format quant-format-kw))
    (error "with-print-options macro: bad argument list"))
  `(let ((*is-binop* ,is-binop-fct)
 	 (*fct-name* ,fct-name-fct)
	 (*quant-format* ,quant-format))
     ,@body))