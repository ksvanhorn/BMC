(in-package :simplify)

(defun is-one (x) (and (is-expr-const x) (eql 1 (expr-const-name x))))
(defun is-zero (x) (and (is-expr-const x) (eql 0 (expr-const-name x))))

(defun is-asae (x)
  (adt-case expr x
   ((const name)
    t)
   ((variable symbol)
    t)
   ((apply fct args)
    (cond
      ((eq '* fct)
       (and
	 (notany #'is-one args)
	 (notany #'is-zero args)
	 (<= (count-if #'is-literal args) 1)))
      (t nil)))
   (otherwise nil)))

(defun is-literal (x)
  (and (is-expr-const x) (numberp (expr-const-name x))))

(defun is-symbolic-const (x)
  (and (is-expr-const x) (symbolp (expr-const-name x))))

(defun is-fct (fct x)
  (and (is-expr-apply x) (eq fct (expr-apply-fct x))))

(defun is-ordinary-fct (x)
  (and (is-expr-apply x) (not (member (expr-apply-fct x) '(+ * ^ fac)))))

(defun expr-cmp (x y)
  (cond
    ((not (eql (is-expr-lambda x) (is-expr-lambda y)))
     (error "Comparing lambda expr and non lambda expr in expr-cmp."))

    ((and (is-expr-lambda x) (is-expr-lambda y))
     (let ((cmp (sym-cmp (expr-lambda-var x) (expr-lambda-var y))))
       (if (/= 0 cmp)
	 cmp
	 (expr-cmp (expr-lambda-body x) (expr-lambda-body y)))))

    ((is-literal x)
     (if (is-literal y)
       (num-cmp (expr-const-name x) (expr-const-name y))
       -1))

    ((is-symbolic-const x)
     (cond
       ((is-literal y) +1)
       ((is-symbolic-const y)
	(sym-cmp (expr-const-name x) (expr-const-name y)))
       (t -1)))

    ((is-fct '* x)
     (cond
       ((is-literal y) +1)
       ((is-symbolic-const y) +1)
       ((is-fct '* y)
	(prod-sum-args-cmp (expr-apply-args x) (expr-apply-args y)))
       (t
	(expr-cmp x (expr-call '* y)))))

    ((is-fct '^ x)
     (cond
       ((is-literal y) +1)
       ((is-symbolic-const y) +1)
       ((is-fct '* y)
        (- (expr-cmp y x)))
       ((is-fct '^ y)
        (pwr-args-cmp (expr-apply-args x) (expr-apply-args y)))
       (t
	(expr-cmp x (expr-call '^ y (expr-const 1))))))

    ((is-fct '+ x)
     (cond
       ((is-literal y) +1)
       ((is-symbolic-const y) +1)
       ((is-fct '* y)
	(- (expr-cmp y x)))
       ((is-fct '^ y)
	(- (expr-cmp y x)))
       ((is-fct '+ y)
	(prod-sum-args-cmp (expr-apply-args x) (expr-apply-args y)))
       (t
	(expr-cmp x (expr-call '+ y)))))

    ((is-fct 'fac x)
     (cond
       ((is-literal y) +1)
       ((is-symbolic-const y) +1)
       ((or (is-fct '* y) (is-fct '^ y) (is-fct '+ y))
	(- (expr-cmp y x)))
       ((is-fct 'fac y)
	(fac-args-cmp (expr-apply-args x) (expr-apply-args y)))
       (t
	(let ((e (expr-call 'fac y)))
	  (if (equalp x e)
	    +1
	    (expr-cmp x e))))))

    ((is-ordinary-fct x)
     (cond
      ((is-literal y) +1)
      ((is-symbolic-const y) +1)
      ((or (is-fct '* y) (is-fct '^ y) (is-fct '+ y) (is-fct 'fac y))
       (- (expr-cmp y x)))
      ((is-ordinary-fct y)
       (let ((fct-x (expr-apply-fct x))
	     (fct-y (expr-apply-fct y)))
	 (if (eq fct-x fct-y)
	     (fct-args-cmp (expr-apply-args x) (expr-apply-args y))
	   (sym-cmp fct-x fct-y))))
      (t
       (let ((sx (expr-apply-fct x))
	     (sy (expr-variable-symbol y)))
	 (if (eq sx sy)
	   +1
	   (sym-cmp sx sy))))))

    ((is-expr-variable x)
     (if (is-expr-variable y)
       (sym-cmp (expr-variable-symbol x) (expr-variable-symbol y))
       (- (expr-cmp y x))))

    (t (error "Unimplemented expression type in simplify::expr<"))
  ))

(defun num-cmp (x y)
  (cond
    ((< x y) -1)
    ((< y x) +1)
    (t 0)))

(defun sym-cmp (x y)
  (let ((x-str (symbol-name x))
	(y-str (symbol-name y)))
    (cond
      ((string< x-str y-str) -1)
      ((string< y-str x-str) +1)
      (t 0))))

(defun prod-sum-args-cmp (args-x args-y)
  (fct-args-cmp (reverse args-x) (reverse args-y)))

(defun fct-args-cmp (args-x args-y)
  (do ((ax args-x (cdr ax))
       (ay args-y (cdr ay)))
      ; end test
      ((or (null ax) (null ay))
       ; return value
       (cond ((not (null ay)) -1)
	     ((not (null ax)) +1)
	     (t 0)))
    ; body
    (let ((cmp (expr-cmp (car ax) (car ay))))
      (unless (zerop cmp)
	(return-from fct-args-cmp cmp)))))


(defun check-num-args (fct-name n args)
  (unless (= n (length args))
    (error "~a expression with ~a arguments" fct-name (length args))))

(defun pwr-args-cmp (args-x args-y)
  (check-num-args "power" 2 args-x)
  (check-num-args "power" 2 args-y)
  (destructuring-bind (base-x expt-x) args-x
    (destructuring-bind (base-y expt-y) args-y
      (if (equalp base-x base-y)
	(expr-cmp expt-x expt-y)
	(expr-cmp base-x base-y)))))

(defun fac-args-cmp (args-x args-y)
  (check-num-args "fac" 1 args-x)
  (check-num-args "fac" 1 args-y)
  (destructuring-bind (x-arg) args-x
    (destructuring-bind (y-arg) args-y
      (expr-cmp x-arg y-arg))))
