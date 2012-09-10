(in-package :prove)

(load "Classic-rtp/unify")
(load "Classic-rtp/cnf")
(load "Classic-rtp/prover")

(setq *resource-limit* 50000)

(defun prove-thms-axs (thms axioms)
  (let* ((assume (convert-to-cnf (conjunction axioms)))
	 (failed nil)
	 (count 0))
    (dolist (x thms)
      (let ((result (multiple-value-list (prove x :axioms assume))))
	(if (eq t (first result))
	  (push (convert-to-cnf x) assume)
	  (push x failed))))
    (values failed count)))

#|
(defmacro is-ap ((e1 e2) e &rest body)
  (let ((v (gensym)))
    `(let ((,v ,e))
       (and (is-expr-apply ,v)
	    (eq '! (expr-apply-fct ,v))
	    (destructuring-bind (,e1 ,e2) (expr-apply-args ,v)
              ,@body)))))

(defun reduce-I-pattern (e)  ; +I+ ! e2  ==>  e2
  (is-ap (e1 e2) e
    (and (equalp +I+ e1) e2)))

(defun reduce-K-pattern (e)  ; +K+ ! e2 ! e3  ==>  e2
  (is-ap (e1-2 e3) e
    (is-ap (e1 e2) e1-2
      (and (equalp +K+ e1) e2))))

(defun reduce-SBC-pattern (e)
  (is-ap (e1-2-3 e4) e
    (is-ap (e1-2 e3) e1-2-3
      (is-ap (e1 e2) e1-2
	(or
          (and (equalp +S+ e1)  ; +S+ ! e2 ! e3 ! e4  ==>  (e2 ! e4) ! (e3 ! e4)
	       (expr-call '! (expr-call '! e2 e4) (expr-call '! e3 e4)))
	  (and (equalp +B+ e1)  ; +B+ ! e2 ! e3 ! e4  ==>  e2 ! (e3 ! e4)
	       (expr-call '! e2 (expr-call '! e3 e4)))
	  (and (equalp +C+ e1)  ; +C+ ! e2 ! e3 ! e4  ==>  (e2 ! e3) ! e4
	       (expr-call '! (expr-call '! e2 e3) e4)))))))

(defun reduce-ap (e)
  (is-ap (e1 e2) e
    (let ((e1r (reduce-SKIBC-1 e1))
	  (e2r (reduce-SKIBC-1 e2)))
      (cond ((and e1r e2r) (expr-call '! e1r e2r))
	    (e1r (expr-call '! e1r e2))
	    (e2r (expr-call '! e1 e2r))
	    (t nil)))))

(defun reduce-SKIBC-1 (e)
  (or (reduce-I-pattern e)
      (reduce-K-pattern e)
      (reduce-SBC-pattern e)
      (reduce-ap e)))

(defun reduce-SKIBC (e)
  (is-ap (
  (cond ((is-I-pattern e)
	 
  (adt-case expr e
    ((const name)
     e)
    ((variable symbol)
     e)
    ((apply fct args)
     (cond ((eq '! fct)
	    (assert (= 2 (length args)))
	    (destructuring-bind (e1 e2) args
	      (cond ((equalp +I+ e1)
		     e2)
		    ((and (is-expr-apply e1)
			  (equalp +K+ (first (expr-apply-args e1))))
		     (second (expr-apply-args e1))
(and (equalp +K+ e1) (is-expr-apply e2)
			  (equalp
              )))
     ))))


(defun de-lambda (e)
  (adt-case expr e
    ((const name)
     e)
    ((variable symbol)
     e)
    ((apply fct args)
     (expr-app fct (mapcar #'de-lambda args)))
    ((lambda var body)
     (de-lambda1-lam var body))))

(defun de-lambda1-lam (v b)
  (unless (occurs-free v b)
    (return-from de-lambda1-lam (expr-call '! +K+ b)))
  (adt-case expr b
    ((variable symbol)
     (assert (eq v symbol))  ; (lambda (v) v)
     +I+)
    ((lambda var body)
     (de-lambda1-lam v (de-lambda1-lam var body)))
    ((apply fct args)
     (cond
       ((not (eq '! fct))
	(de-lambda1-lam v (curry fct args)))
       ((progn (assert (= 2 (length args))) nil)
	)
       ((equalp (expr-var v) (second args)) ; (lambda (v) (! f v))
	(first args))
       (t
	(destructuring-bind (e1 e2) args
	  (if (occurs-free v e1)
	    (if (occurs-free v e2)
	      ; v free in both e1 and e2
	      (expr-call '! (expr-call '! +S+ (de-lambda1-lam v e1))
			 (de-lambda1-lam v e2))
	      ; v free in e1 but not e2
	      (expr-call '! (expr-call '! +C+ (de-lambda1-lam v e1))
			 (de-lambda e2)))
	    ; v free in e2 but not e1 (since v free in (! e1 e2))
	    (expr-call '! (expr-call '! +B+ (de-lambda e1))
		       (de-lambda1-lam v e2)))))))))

; Constants for standard combinators
(defconstant +K+ (expr-const '[K]))
(defconstant +I+ (expr-const '[I]))
(defconstant +S+ (expr-const '[S]))
(defconstant +B+ (expr-const '[B]))
(defconstant +C+ (expr-const '[C]))

(defun fct-const (fct n)
  (expr-const (intern (format nil "{~a/~d}" fct n) "SYMBOLS")))

(defun curry (fct args)
  (reduce (lambda (x y) (expr-call '! x y))
	  (cons (fct-const fct (length args)) args)))
|#

#|
(defun expr->prover-expr (e)
  (adt-case expr e
    ((literal value)
     value)
    ((const symbol)
     symbol)
    ((variable symbol)
     symbol)
    ((apply fct args)
     (cons fct (mapcar #'expr->prover-expr args)))
    ((let var val body)
     (expr->prover-expr (subst-expr var val body)))
))
|#

(defun subst-expr (v replacement e)
  (let ((freev (free-vars-in-expr replacement)))
    (labels
      ((subst-expr-1 (x)
	 (adt-case expr x
	   ((const name)
	    x)
	   ((variable symbol)
	    (if (eq v symbol) replacement x))
	   ((apply fct args)
	    (expr-app fct (mapcar #'subst-expr-1 args)))
	   ((lambda var body)
	    (cond ((eq v var)
		   e)
		  ((not (member var freev))
		   (expr-lam var (subst-expr-1 body)))
		  (t
		   (let* ((new-var (symbol-not-in freev (symbol-name var)))
		          (new-body (subst-expr var (expr-var new-var) body)))
		     (expr-lam new-var (subst-expr-1 new-body)))))))))
      (subst-expr-1 e))))

#|
(defun expand-densities (e)
  (adt-case expr e
    ((const name)
     e)
    ((variable symbol)
     e)
    ((lambda var body)
     (expr-lam var (expand-densities body)))
    ((apply fct args)
     (let ((pattern (density-pattern fct)))
       (if (null pattern)
	 (expr-app fct (mapcar #'expand-densities args))
|#
 
