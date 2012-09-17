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

(defun is-prod (x)
  (and (is-expr-apply x) (eq '* (expr-apply-fct x))))

(defun is-sum (x)
  (and (is-expr-apply x) (eq '+ (expr-apply-fct x))))

(defun expr-cmp (x y)
  (cond
    ((and (is-literal x) (is-literal y))
     (num-cmp (expr-const-name x) (expr-const-name y)))

    ((and (is-symbolic-const x) (is-symbolic-const y))
     (str-cmp (symbol-name (expr-const-name x))
	      (symbol-name (expr-const-name y))))

    ((and (is-expr-variable x) (is-expr-variable y))
     (str-cmp (symbol-name (expr-variable-symbol x))
	      (symbol-name (expr-variable-symbol y))))

    ((and (is-sum x) (is-sum y))
     (prod-sum-args-cmp (expr-apply-args x) (expr-apply-args y)))

    ((and (is-prod x) (is-prod y))
     (prod-sum-args-cmp (expr-apply-args x) (expr-apply-args y)))
  ))

(defun num-cmp (x y)
  (cond
    ((< x y) -1)
    ((< y x) +1)
    (t 0)))

(defun str-cmp (x y)
  (cond
    ((string< x y) -1)
    ((string< y x) +1)
    (t 0)))

(defun prod-sum-args-cmp (args-x args-y)
  (do ((rx (reverse args-x) (cdr rx))
       (ry (reverse args-y) (cdr ry)))
      ; end test
      ((or (null rx) (null ry))
       ; return value
       (cond ((not (null ry)) -1)
	     ((not (null rx)) +1)
	     (t 0)))
    ; body
    (let ((cmp (expr-cmp (car rx) (car ry))))
      (unless (zerop cmp)
	(return-from prod-sum-args-cmp cmp)))))
