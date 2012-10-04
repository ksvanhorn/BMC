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

(defgeneric is-provable (prover boolean-expr)
  (:documentation "Returns t if prover |- boolean-expr."))

(defgeneric also-assume (prover0 boolean-exprs)
  (:documentation "Creates from prover0 a new prover object that assumes boolean-exprs to be true."))

(defparameter *prover* nil)

(defun can-prove (boolean-expr)
  (is-provable *prover* boolean-expr))

(defmacro assuming (boolean-exprs &rest body)
  `(let ((*prover* (also-assume *prover* ,boolean-exprs)))
     ,@body))

(defmacro assuming-se (boolean-sexprs &rest body)
  (let ((var (gensym)))
    `(let ((,var (mapcar #'sexpr->expr ,boolean-sexprs)))
       (assuming ,var ,@body))))

(defun multi-subst-expr (subs e)
  (dolist (sub subs)
    (destructuring-bind (v . replacement) sub
      (setf e (subst-expr v replacement e))))
  e)

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
		   x)
		  ((not (member var freev))
		   (expr-lam var (subst-expr-1 body)))
		  (t
		   (let* ((new-var (symbol-not-in freev (symbol-name var)))
		          (new-body (subst-expr var (expr-var new-var) body)))
		     (expr-lam new-var (subst-expr-1 new-body)))))))))
      (subst-expr-1 e))))

(defun is-pattern-var (symbol)
  (char= #\? (char (symbol-name symbol) 0)))

(defun pat-match (e pat)
  (adt-case expr pat
    ((const name)
     (pat-match-const name e pat))
    ((variable symbol)
     (pat-match-variable symbol e pat))
    ((apply fct args)
     (pat-match-apply fct args e pat))
    ((lambda var body)
     (pat-match-lambda var body e pat))))

(defconstant +no-match+ (cons nil nil))

(defun pat-match-const (name e pat)
  (if (equalp e pat)
    (cons t nil)
    +no-match+))

(defun pat-match-variable (symbol e pat)
  (cond
     ((is-pattern-var symbol)
      (cons t (list (cons symbol e))))
     ((equalp e pat)
      (cons t nil))
     (t +no-match+)))
    
(defun pat-match-apply (pat-fct pat-args e pat)
  (adt-case expr e
    ((apply fct args)
     (unless (and (eq fct pat-fct) (= (length args) (length pat-args)))
       (return-from pat-match-apply +no-match+))
     (let ((subs nil))
       (dolist (a pat-args)
	 (destructuring-bind (is-match . subs1) (pat-match (car args) a)
	   (unless is-match
	     (return-from pat-match-apply +no-match+))
	   (setf subs (append subs1 subs))
	   (setf args (cdr args))))
       (cons t subs)))
    (otherwise +no-match+)))

(defun pat-match-lambda (var-pat body-pat e pat)
  (error "Invalid pattern used in pattern matching"))

(defun closure (xform)
  (lambda (e)
    (do ((old nil new)
	 (new e (funcall xform new)))
	((equalp old new) old)
	nil)))

(defun recurse (xform)
  (labels
    ((f (e)
       (adt-case expr e
	 ((const name)
	  (funcall xform e))
	 ((variable symbol)
	  (funcall xform e))
	 ((apply fct args)
	  (funcall xform (expr-app fct (mapcar #'f args))))
	 ((lambda var body)
	  (funcall xform (expr-lam var (f body)))))))
    #'f))

(defun compose-xforms (&rest args)
  (lambda (e)
    (reduce (lambda (e1 xform) (funcall xform e1)) (cons e args))))

(defun pattern-xform (pr)
  (destructuring-bind (pattern . replacement) pr
    (lambda (e)
      (destructuring-bind (is-match . subs) (pat-match e pattern)
	(if is-match
	  (multi-subst-expr subs replacement)
	  e)))))

(defun se-pattern-xform (se)
  (destructuring-bind (lhs . rhs) se
    (pattern-xform (cons (sexpr->expr lhs) (sexpr->expr rhs)))))

(defun eliminate-extraneous-ops (e)
  (funcall +eliminate-extraneous-ops+ e))

(defconstant +eliminate-extraneous-ops-patterns+
  '(((neg ?x) . (* -1 ?x))
    ((/ ?x ?y) . (* ?x (^ ?y -1)))
    ((- ?x ?y) . (+ ?x (* -1 ?y)))
    ((exp ?x) . (^ %e ?x))
    ((^1/2 ?x) . (^ ?x 1/2))
    ((^2 ?x) . (^ ?x 2))))

(defconstant +eliminate-extraneous-ops+
  (let* ((xforms (mapcar #'se-pattern-xform
			 +eliminate-extraneous-ops-patterns+)))
    (recurse (apply #'compose-xforms xforms))))

(defun normalize (e))

  ; levelling
  ; (+) => 0
  ; (*) => 1
  ; (+ x) => x
  ; (* x) => x
  ; collect constants together in sum
  ; collection constants together in product

(defun expand-densities (e)
  (funcall +expand-densities-fct+ e))

(defconstant +expand-density-patterns+
  '(((dnorm-density ?x ?mu ?sigma) .
     (* (/ 1 (* (^1/2 (* 2 %pi)) ?sigma))
        (exp (* -1/2 (^2 (- ?x ?mu))))))

    ((dgamma-density ?x ?alpha ?beta) .
     (* (< 0 ?x) (/ (^ ?beta ?alpha) (gamma-fct ?alpha))
	(^ ?x (- ?alpha 1)) (exp (* -1 ?beta ?x))))

    ((dcat-density ?x ?p) .
     (* (<= 1 ?x) (<= ?x (length ?p)) (@ ?p ?x)))

    ((dinterval-density ?i ?y ?cut) .
     (if-then-else (< ?y (@ ?cut 1))
       (= ?i 1)
       (if-then-else (<= (@ ?cut (length ?cut)) ?y)
	 (= ?i (+ 1 (length ?cut)))
	 (* (<= (@ ?cut (- ?i 1)) ?y) (< ?y (@ ?cut ?i))))))

    ((ddirch-density ?q ?a) .
     (* (:quant qand i (1 (length ?q)) (< 0 (@ ?q i)))
	(= 1 (sum ?q))
	(/ (gamma-fct (sum ?a))
	   (:quant qprod i (1 (length ?a)) (gamma-fct (@ ?a i))))
	(:quant qprod i (1 (length ?a)) (^ (@ ?q i) (- (@ ?a i) 1)))))

    ((dwishart-density ?X ?n ?V) .
     (* (is-symm-pd ?X)
	(:let (k (array-length 1 ?V))
	  (* (^ 2 (* -1/2 ?n k))
	     (^ (abs-det ?V) (* -1/2 ?n))
	     (/ 1 (mv-gamma-fct k (* 1/2 ?n)))
	     (^ (abs-det ?X) (* 1/2 (- ?n k 1)))
	     (exp (* -1/2 (trace (dot (inv ?V) ?X))))))))

    ((dmvnorm-density ?x ?m ?S) .
     (:let (k (array-length 1 ?S))
       (* (^ (* 2 %pi) (* -1/2 k))
	  (^ (abs-det ?S) -1/2)
	  (exp (* -1/2 (quad (inv ?S) (@- ?x ?m)))))))
   ))

(defconstant +expand-densities-fct+
  (let* ((xforms (mapcar #'se-pattern-xform +expand-density-patterns+)))
    (recurse (apply #'compose-xforms xforms))))

(defun eliminate-let-expressions (e)
  (funcall (recurse #'eliminate-let-expr1) e))

(defun eliminate-let-expr1 (e)
  (adt-case expr e
    ((apply fct args)
     (if (and (eq '! fct) (= 2 (length args)))
       (destructuring-bind (f val) args
	 (adt-case expr f
	   ((lambda var body)
	    (subst-expr var val body))
	   (otherwise e)))
       e))
    (otherwise e)))


(defun expand-products (e)
  (let ((factors (expand-to-factors e)))
    (cond ((null factors)
	   (expr-const 1))
	  ((= 1 (length factors))
	   (first factors))
	  (t
	   (expr-app '* factors)))))

(defun expand-to-factors (e)
  (adt-case expr e
    ((apply fct args)
     (cond
       ((eq '* fct)
	(append-mapcar #'expand-to-factors args))
       ((and (eq '^ fct) (= 2 (length args)))
	(expand-power-to-factors args))
       ((eq 'qprod fct)
	(expand-qprod-to-factors args))
       ((eq '! fct)
	(expand-ap-to-factors args))
       ((eq 'if-then-else fct)
        (expand-if-then-else-to-factors args))
       (t
	(list e))))
    (otherwise (list e))))

;;; expand body of "let x = val in body" 
(defun expand-ap-to-factors (args)
  (destructuring-bind (f val) args
    (match-adt1 (expr-lambda var body) f
      (let ((new-body (expand-products body)))
	(list (expr-call '! (expr-lam var new-body) val))))))

;;; expand both branches of if-then-else
(defun expand-if-then-else-to-factors (args)
  (destructuring-bind (test true-branch false-branch) args
    (list (expr-call 'if-then-else test
		     (expand-products true-branch)
		     (expand-products false-branch)))))

;;; qprod(i, lo, hi, e1 * ... * en) ==> 
;;; qprod(i, lo, hi, e1) * ... * qprod(i, lo, hi, en)
(defun expand-qprod-to-factors (args)
  (destructuring-bind (lo hi filter f) args
    (adt-case expr f
      ((lambda var body)
       (mapcar (lambda (x) (expr-call 'qprod lo hi filter
				      (expr-lam var x)))
	       (expand-to-factors body))))))

;;; expand e1 ^ e2
(defun expand-power-to-factors (power-args)
  (destructuring-bind (e1 e2) power-args
    (adt-case expr e2
      ((apply fct args)
       (when (eq '+ fct)  ; e1 ^ (e2_1 + ... + e2_n)
	 (return-from expand-power-to-factors
	   (append-mapcar (lambda (x) (expand-power-to-factors (list e1 x)))
			  args))))
      (otherwise nil))
    (adt-case expr e1
      ((apply fct args)
       (cond
	 ((and (eq '^ fct) (= 2 (length args)))
	  ; (e1a ^ e1b) ^ e2
	  (destructuring-bind (e1a e1b) args
	    (expand-power-power-to-factors e1a e1b e2)))
	 ((and (eq 'qprod fct) (= 4 (length args)))
	  ; qprod(i, lo, hi, filter, body) ^ e2
	  (destructuring-bind (lo hi filter f) args
	    (expand-power-qprod-to-factors lo hi filter f e2)))
	 ((eq '* fct)
	  ; (e1_1 * ... * e1_n) ^ e2
	  (expand-power-prod-to-factors args e2))
	 (t (list (expr-call '^ e1 e2)))))
      (otherwise (list (expr-call '^ e1 e2))))))

;;; (e1a ^ e1b) ^ e2 ==> e1a ^ (e1b * e2)
(defun expand-power-power-to-factors (e1a e1b e2)
  (expand-to-factors (expr-call '^ e1a (expr-call '* e1b e2))))

;;; (e1_1 * ... * e1_n) ^ e2 ==> e1_1 ^ e2 * ... * e1_n ^ e2
(defun expand-power-prod-to-factors (prod-args expt)
  (append-mapcar
    (lambda (x) (expand-to-factors (expr-call '^ x expt)))
    (append-mapcar #'expand-to-factors prod-args)))

;;; qprod(i, lo, hi, filter, f(i)) ^ e2 ==> qprod(i, lo, hi, filter, f(i) ^ e2)
(defun expand-power-qprod-to-factors (lo hi filter f e2)
  (adt-case expr f
    ((lambda var body)
     (let ((excluded (free-vars-in-expr e2)))
       (when (member var excluded)
	 (let ((new-var (symbol-not-in excluded (symbol-name var))))
	   (setf body (subst-expr var (expr-var new-var) body))
	   (setf var new-var)))
       (expand-to-factors
	 (expr-call 'qprod lo hi filter
		    (expr-lam var (expr-call '^ body e2))))))))

(defun expand-quadratic (e)
  (let ((terms (expand-quadratic-terms e)))
    (cond ((null terms)
	   (expr-const 0))
	  ((= 1 (length terms))
	   (first terms))
	  (t
	   (expr-app '+ terms)))))

#|
(defun expand-quadratic-terms (e)
  (adt-case
   ((apply fct args)
    (cond
      ((eq '+ fct)  ; a1 + ... + an
       (append-mapcar #'expand-quadratic-terms args))
      ((and (eq '^ fct) (= 2 (length args)))
       (destructuring-bind (a1 a2) args   ; a1 ^ a2
	 (let ((n (expr-const-name a2)))
	   (cond
	     ((eql 1 n)
	      (expand-quadratic-terms a1))
	     ((eql 2 n)
	      (expand-quadratic-terms (expr-call '* a1 a2)))
	     (t
	      (list e))))))
      ((eq '* fct)
       (if (null args)
	 (list (expr-const 1))
	 (destructuring-bind (x1 . rest) args
	   (expr-app
       (cond
	 ((null args)
	  (list (expr-const 1)))
	 ((= 1 (length args))
	  (expand-quadratic-terms (first args)))
	 (t
	  (d
   (otherwise
    (list e))))
|#

(defun fold-constants-1 (e)
  (adt-case expr e
    ((apply fct args)
     (fold-constants-apply-1 e fct args))
    (otherwise e)))

(defun fold-constants-apply-1 (e fct-symbol args)
  (if (notevery #'is-expr-const args)
    e
    (let ((df (cdr (assoc fct-symbol +function-interps+))))
      (if (null df)
	e
	(destructuring-bind (is-in-domain . fct) df
	  (let ((arg-vals (mapcar #'expr-const-name args)))
	    (if (funcall is-in-domain arg-vals)
	      (expr-const (apply fct arg-vals))
	      e)))))))

(let ((fc (recurse #'fold-constants-1)))
  (defun fold-constants (e)
    (funcall fc e)))

(defun is-single-number (args)
  (and (= 1 (length args)) (numberp (first args))))

(defun are-two-reals (args)
  (and (= 2 (length args)) (are-reals args)))

(defun are-real-integer (args)
  (and (= 2 (length args))
       (realp (first args))
       (integerp (second args))))

(defun are-reals (args)
  (every #'realp args))

(defconstant +function-interps+
  '((neg is-single-number . -)
    (* are-reals . *)
    (+ are-reals . +)
    (^ are-real-integer . expt)
))    

(defun neg-product-xform (e)
  (adt-case expr e
    ((apply fct args)
     (let ((e1 (expr-app fct (mapcar #'neg-product-xform args))))
       (or (neg-product-xform-1 e1) e1)))       
    ((lambda var body)
     (expr-lam var (neg-product-xform body)))
    (otherwise e)))

(defun neg-product-xform-1 (e)
  (match-adt1 (expr-apply fct args) e
    (and
      (eq 'neg fct)
      (= 1 (length args))
      (destructuring-bind (neg-arg) args  ; e == (neg neg-arg)
	(and
	  (is-expr-apply neg-arg)
	  (match-adt1 (expr-apply fct args) neg-arg
	    (and 
	      (eq '* fct)
	      (consp args)
	      (destructuring-bind (a1 . rest) args ; e == (neg (* a1 ...))
	        (and
		  (is-expr-const a1)
		  (match-adt1 (expr-const name) a1
		    (and (numberp name)
			 (expr-app '* (cons (expr-const (- name))
					    rest)))))))))))))

(defun reduce-add-zero (e)
  (match-adt1 (expr-apply fct args) e
    (and
      (eq '+ fct)
      (let ((new-args (remove-if #'is-const-zero args)))
	(cond
	  ((null new-args)
	   (expr-const 0))
	  ((= 1 (length new-args))
	   (first new-args))
	  (t (expr-app '+ new-args)))))))

(defconstant +simplify-xforms+
  (list
    (se-pattern-xform '((+) . 0))
    (se-pattern-xform '((+ ?x) . ?x))
    (se-pattern-xform '((*) . 1))
    (se-pattern-xform '((* ?x) . ?x))
    (se-pattern-xform '((inv (inv ?x)) . ?x))
    (se-pattern-xform '((abs-det (inv ?x)) . (^ (abs-det ?x) -1)))
    
))
  

(defun simplify-add-zero-1 (e)
  (adt-case expr e
    ((apply fct args)
     (if (eq '+ fct)
       (simplify-add-zero-sum-expr args)
       e))
    (otherwise e)))

(defun simplify-add-zero-sum-expr (args)
  (let ((new-args (remove-if #'is-const-zero args)))
    (cond
      ((null new-args)
       (expr-const 0))
      ((= 1 (length new-args))
       (first new-args))
      (t (expr-app '+ new-args)))))

(let ((xform (recurse #'simplify-add-zero-1)))
  (defun simplify-add-zero (e)
    (funcall xform e)))

(defun is-const-zero (e)
  (adt-case expr e
    ((const name)
     (zerop name))
    (otherwise nil)))

(defun simplify-mul-one (e)
  (adt-case expr e
    ((apply fct args)
     (if (eq '* fct)
       (simplify-mul-one-prod-expr args)
       e))
    (otherwise e)))

(defun simplify-mul-one-prod-expr (args)
  (let ((new-args (remove-if #'is-const-one args)))
    (cond
      ((null new-args)
       (expr-const 1))
      ((= 1 (length new-args))
       (first new-args))
      (t (expr-app '* new-args)))))

(defun is-const-one (e)
  (and (is-expr-const e) (eql 1 (expr-const-name e))))

(let* ((xform (recurse
	        (compose-xforms
		  (se-pattern-xform
		    '((length ?x) .
		      (array-length 1 ?x)))
		  (se-pattern-xform
		    '((array-length 1 (qvec ?m ?n ?p ?f)) .
		      (max 0 (- ?n (- ?m 1)))))
		  (se-pattern-xform
		    '((array-length 1 (vec ?x ?y)) . 2))))))
  (defun simplify-lengths (e)
    (funcall xform e)))

(defun simplify-qprod-unvarying-body (e)
  (adt-case expr e
    ((apply fct args)
     (if (and (eq 'qprod fct) (= 4 (length args)))
       (destructuring-bind (lo hi filter f) args
         (adt-case expr f
	   ((lambda var body)
	    (if (or (occurs-free var body)
		    (not (equalp filter (expr-const '%true-pred))))
	      e
	      (expr-call '^ body
			 (expr-call '- hi (expr-call '- lo (expr-const 1))))))
	   (otherwise e)))
       e))
     (otherwise e)))

(let* ((patterns
	 '(((neg ?x) . (* -1 ?x))
	   ((/ ?x ?y) . (* ?x (^ ?y -1)))
	   ((- ?x ?y) . (+ ?x (* -1 ?y)))
	   ((- ?x ?y ?z) . (+ ?x (* -1 ?y) (* -1 ?z)))
	   ((exp ?x) . (^ %e ?x))
	   ((^1/2 ?x) . (^ ?x 1/2))
	   ((^2 ?x) . (^ ?x 2))
	   ((^ (^ ?x ?a) ?b) . (^ ?x (* ?a ?b))) 
	   ((length ?x) .
	    (array-length 1 ?x))
	   ((array-length 1 (qvec ?m ?n ?f)) .
	    (max 0 (- ?n (- ?m 1))))
	   ((array-length 1 (vec ?x ?y)) . 2)
	   ((inv (inv ?x)) . ?x)
	   ((array-length ?n (inv ?x)) . (array-length ?n ?x))
	   ((abs-det (inv ?x)) . (^ (abs-det ?x) -1))
       ))
       (basic-xforms (list* #'fold-constants-1
			    #'simplify-add-zero-1
			    #'simplify-mul-one
			    #'simplify-qprod-unvarying-body
			   (mapcar #'se-pattern-xform patterns)))
       (xform (closure (recurse (apply #'compose-xforms basic-xforms)))))
  (defun simplify-expr (e)
    (funcall xform e)))

(defun var-dims-pats (var-dims)
  (let ((n 1)
	(pats '()))
    (destructuring-bind (var . dims) var-dims
      (dolist (e dims)
	(push (cons (expr-call 'array-length (expr-const n) (expr-var var))
		    e)
	      pats)
	(incf n)))
    (reverse pats)))

(defun expand-array-lengths (var-dims-list)
  (let ((patterns (append-mapcar #'var-dims-pats (reverse var-dims-list))))
    (recurse
      (apply #'compose-xforms (mapcar #'pattern-xform patterns)))))

; (INV (INV ?x)) ==> ?x
; 