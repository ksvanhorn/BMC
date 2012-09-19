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

(defun simplify-expr (x)
  (adt-case expr x
    ((const name)
     x)
    ((variable symbol)
     x)
    ((apply fct args)
     (simplify-apply-expr x fct args))
))

(defconstant +zero+ (expr-const 0))
(defconstant +one+ (expr-const 1))
(defconstant +infty-pos+ (expr-const '%infty+))
(defconstant +infty-neg+ (expr-const '%infty-))
(defconstant +undef+ (expr-const '%undef))
(defconstant +true+ (expr-const 'true))
(defconstant +false+ (expr-const 'false))

(defun can-prove (e)
  (and *prover* (is-provable *prover* e)))

(defun simplify-apply-expr (x fct args)
  (if (and (is-strict-fct fct) (some #'is-undefined args))
      +undef+
    (case fct
      ('^ (simplify-power-expr x args))
      ('if-then-else (simplify-if-then-else-expr x args))
      ('fac (simplify-fac-expr x args))
      (otherwise x)
)))

(defun is-strict-fct (fct)
  (not (eq 'if-then-else fct)))

(defun simplify-power-expr (x args)
  (destructuring-bind (base expt) args
    (let ((base-num (and (is-expr-const base) (expr-const-name base)))
	  (expt-num (and (is-expr-const expt) (expr-const-name expt))))
      (cond
        ((eql 0 base-num)
	 (simplify-zero-power-expr x expt))
	((eql 1 base-num)
	 (simplify-one-power-expr x expt))
	((eql '%infty+ base-num)
	 (simplify-posinf-power-expr x expt))
	((eql '%infty- base-num)
	 (simplify-neginf-power-expr x expt))
	((integerp expt-num)
	 (simplify-integer-expt-expr x base base-num expt-num))
	(t x)
      ))))

(defun simplify-zero-power-expr (x expt)
  (cond
    ((equalp +zero+ expt)
     +undef+)              ; 0 ^ 0 --> undefined
    ((can-prove (is-positive-expr expt))
     +zero+)               ; 0 ^ positive --> 0
    ((can-prove (is-negative-expr expt))
     (let ((is-even-expr (expr-call 'is-even expt)))
       (cond               ; is-even implies is-integer
         ((can-prove is-even-expr)
	  +infty-pos+)     ; 0 ^ negative-even-integer --> infinity
	 ((can-prove (expr-call 'not is-even-expr))
	  +undef+))))      ; 0 ^ negative-not-even --> undefined
    (t x)))

(defun simplify-one-power-expr (x expt)
  (cond
    ((equalp +infty-pos+ expt)
     +undef+)  ; 1 ^ infinity --> undefined
    ((equalp +infty-neg+ expt)
     +undef+)  ; 1 ^ -infinity --> undefined
    ((can-prove (is-real-expr expt))
     +one+)    ; 1 ^ real-number --> 1
    (t x)))

(defun simplify-posinf-power-expr (x expt)
  (cond
    ((equalp +zero+ expt)
     +undef+)      ; infinity ^ 0 --> undefined
    ((can-prove (is-positive-expr expt))
     +infty-pos+)  ; infinity ^ positive --> infinity
    ((can-prove (is-negative-expr expt))
     +zero+)       ; infinity ^ negative --> 0
    (t x)))

(defun simplify-neginf-power-expr (x expt)
  (cond
    ((equalp +zero+ expt)
     +undef+)    ; -infinity ^ 0 --> undefined
    ((can-prove (expr-call 'not (is-integer-expr expt)))
     +undef+)    ; -infinity ^ non-integer --> undefined
    ((can-prove (is-negative-expr expt))
     (if (can-prove (is-integer-expr expt))
	 +zero+     ; -infinity ^ negative-integer --> 0
       x))
    ((can-prove (is-positive-expr expt))
     (cond
       ((can-prove (expr-call 'is-even expt))
	+infty-pos+)  ;; -infinity ^ positive-even-integer --> infinity
       ((can-prove (expr-call 'is-odd expt))
	+infty-neg+)  ;; -infinity ^ positive-odd-integer --> -infinity
       (t x)))
    (t x)))

(defun simplify-integer-expt-expr (x base base-num expt-num)
  (cond
    ((numberp base-num)
     (expr-const (expt base-num expt-num)))  ; constant folding
    ((and (zerop expt-num) (can-prove (is-nonzero-expr base))
	  (can-prove (is-real-expr base)))
     +one+)
    ((eql 1 expt-num)
     base)
#|
    ((and (is-expr-apply base) (eq '^ (expr-apply-fct base)))
     (match-adt1 expr-apply (fct args)
       (destructuring-bind (r s) args
         (let ((p (simplify-product (expr-call '* s expt-num))))
	|#   
    (t x)))

(defun simplify-if-then-else-expr (x args)
  (destructuring-bind (test tbranch fbranch) args
    (cond
      ((equalp +true+ test)
       tbranch)
      ((equalp +false+ test)
       fbranch)
      ((equalp +undef+ test)
       +undef+)
      (t x))))

(defun simplify-fac-expr (x args)
  (destructuring-bind (a) args
    (adt-case expr a
     ((const name)
      (if (integerp name)
	  (if (<= 0 name)
	      (expr-const (factorial name))
	    (expr-const '%undef))
	x))
     (otherwise x))))      
  
(defun factorial (n)
  (let ((result 1))
    (loop for k from 2 to n do
      (setf result (* result k)))
    result))

(defun is-positive-expr (e)
  (expr-call '< +zero+ e))

(defun is-negative-expr (e)
  (expr-call '< e +zero+))

(defun is-real-expr (e)
  (expr-call 'is-real e))

(defun is-realx-expr (e)
  (expr-call 'is-realx e))

(defun is-integer-expr (e)
  (expr-call 'is-integer e))

(defun is-nonzero-expr (e)
  (expr-call '!= +zero+ e))

(defun is-undefined (x)
  (and (is-expr-const x) (eq '%undef (expr-const-name x))))

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
