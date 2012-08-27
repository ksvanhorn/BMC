(in-package :expr)

(defadt expr
  (literal value)
  (const symbol)
  (variable symbol)
  (quantifier op lo hi var body)
  (apply fct args))

(defun sexpr->expr (x)
  (cond
    ((is-literal-value x)
     (make-expr-literal :value x))
    ((is-const-symbol x)
     (make-expr-const :symbol x))
    ((symbolp x)
     (make-expr-variable :symbol x))
    ((consp x)
     (destructuring-bind (op args) x
       (cond ((is-fquant-symbol op)
	      (sexpr->expr-quantifier op args))
	     ((eq '@ op)
	      (sexpr->expr-array-app op args))
	     ((is-fct-symbol op)
	      (make-expr-apply :fct op :args (mapcar #'sexpr->expr args)))
	     (t
	      (error "Illegal symbol (~W) at beginning of expression" op)))))
    (t
     (error "Unrecognized expression type: ~W" x))))

(defun sexpr->expr-quantifier (op args)
  (destructuring-bind (lo-x hi-x var body-x) args
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
  (destructuring-bind (arr-x . indices) args
    (let ((arr (sexpr->expr) arr-x))
      (if (every #'is-scalar-index indices)
	  (make-expr-apply
	    :fct '@ :args (cons arr (mapcar #'sexpr->expr args)))
	  (make-expr-apply
	    :fct '@-slice :args (cons arr (mapcar #'sexpr->slice-arg args)))))))

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

(defun is-slice-all (x) (eq 'all x))

(defun is-slice-range (x) (starts-with 'range x))

(defun is-literal-value (x)
  (or (realp x) (eq x 'true) (eq x 'false)))
