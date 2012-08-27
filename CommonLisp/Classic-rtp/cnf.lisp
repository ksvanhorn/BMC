;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Written by Norvig and Russell
;;;; Convert Expressions to Normal Form (Conjunctive, Implicative or Horn)
;;; This could be done much more efficiently.  Most often, a special
;;; representation is used for CNF, which eliminates the explicit ANDs
;;; and ORs.  This code is meant to be informative, not efficient.

;;;; Top-Level Functions

(defun ->cnf (p &optional vars)
  "Convert a sentence p to conjunctive normal form [p 279-280]."
  ;; That is, return (and (or ...) ...) where 
  ;; each of the conjuncts has all literal disjuncts.
  ;; VARS is a list of universally quantified variables that P is in scope of.
  (setf p (eliminate-implications (logic p)))
  (case (op p)
    (NOT (let ((p2 (move-not-inwards (arg1 p))))
	   (if (literal-clause? p2) p2 (->cnf p2 vars))))
    (AND (conjunction (mappend #'(lambda (q) (conjuncts (->cnf q vars)))
				(args p))))
    (OR  (merge-disjuncts (mapcar #'(lambda (q) (->cnf q vars))
				  (args p))))
    (ALL (let ((new-vars (mapcar #'new-variable  (mklist (arg1 p)))))
	   (->cnf (sublis (mapcar #'cons  (mklist (arg1 p)) new-vars)
			  (arg2 p))
		  (append new-vars vars))))
    (EXI (->cnf (skolemize (arg2 p) (arg1 p) vars) vars))
    (t   p) ; p is atomic
    ))


(defun logic (sentence)
  "Canonicalize a sentence into proper logical form."
  (cond ((stringp sentence) (string->prefix sentence))
	(t sentence)))

;;;; Auxiliary Functions

(defun cnf1->inf1 (p)
  ;; P is of the form (or (not a) (not b) ... c d ...)
  ;; Convert to: (=> (and a b ...) (or c d ...))
  ;; where a,b,c,d ... are positive atomic clauses
  (let ((lhs (mapcar #'arg1 (remove-if-not #'negative-clause? (disjuncts p))))
	(rhs (remove-if #'negative-clause? (disjuncts p))))
    `(=> ,(conjunction lhs) ,(disjunction rhs))))

(defun eliminate-implications (p)
  (if (literal-clause? p)
      p
    (case (op p)
      (=>  `(or ,(arg2 p) (not ,(arg1 p))))
      (<=> `(or (and ,(arg1 p) ,(arg2 p))
		(and (not ,(arg1 p)) (not ,(arg2 p)))))
      (t   (cons (op p) (mapcar #'eliminate-implications (args p)))))))

(defun move-not-inwards (p)
  "Given P, return ~P, but with the negation moved as far in as possible."
  (case (op p)
    (TRUE 'false)
    (FALSE 'true)
    (NOT (arg1 p))
    (AND (disjunction (mapcar #'move-not-inwards (args p))))
    (OR  (conjunction (mapcar #'move-not-inwards (args p))))
    (ALL (make-exp 'EXI (arg1 p) (move-not-inwards (arg2 p))))
    (EXI (make-exp 'ALL (arg1 p) (move-not-inwards (arg2 p))))
    (t (make-exp 'not p))))

(defun merge-disjuncts (disjuncts)
  "Return a CNF expression for the disjunction."
  ;; The argument is a list of disjuncts, each in CNF.
  ;; The second argument is a list of conjuncts built so far.
  (case (length disjuncts)
    (0 'false)
    (1 (first disjuncts))
    (t (conjunction
	(loop for y in (conjuncts (merge-disjuncts (rest disjuncts))) append
	      (loop for  x in (conjuncts (first disjuncts)) 
		    collect (disjunction (append (disjuncts x)
						  (disjuncts y)))))))))

(defun skolemize (p vars outside-vars)
  "Within the proposition P, replace each of VARS with a skolem constant,
  or if OUTSIDE-VARS is non-null, a skolem function of them."
  (sublis (mapcar #'(lambda (var)
		      (cons var (if (null outside-vars)
				    (skolem-constant var)
				  (cons (skolem-constant var) outside-vars))))
		  (mklist vars))
	  p))

(defun skolem-constant (name)
  "Return a unique skolem constant, a symbol starting with '$'."
  (intern (format nil "$~A_~D" name (incf *new-variable-counter*))))

(defun renaming? (p q &optional (bindings +no-bindings+))
  "Are p and q renamings of each other? (That is, expressions that differ
  only in variable names?)"
  (cond ((eq bindings +fail+) +fail+)
	((equal p q) bindings)
	((and (consp p) (consp q))
	 (renaming? (rest p) (rest q)
		    (renaming? (first p) (first q) bindings)))
	((not (and (variable? p) (variable? q)))
	 +fail+)
	;; P and Q are both variables from here on
	((and (not (get-binding p bindings)) (not (get-binding q bindings)))
	 (extend-bindings p q bindings))
	((or (eq (lookup p bindings) q) (eq p (lookup q bindings)))
	 bindings)
	(t +fail+)))

;;;; Utility Predicates and Accessors

(defconstant +logical-connectives+ '(and or not => <=>))
(defconstant +logical-quantifiers+ '(all exi))

(defun atomic-clause? (sentence)
  (not (or (member (op sentence) +logical-connectives+)
	   (member (op sentence) +logical-quantifiers+))))

(defun literal-clause? (sentence)
  (or (atomic-clause? sentence)
      (and (eq (op sentence) 'not) (atomic-clause? (arg1 sentence)))))

(defun negative-clause? (sentence)
  (eq (op sentence) 'not))

(defun conjunction? (sentence)
  (eq (op sentence) 'and))

(defun horn-clause? (sentence)
  (and (eq (op sentence) '=>)
       (every #'atomic-clause? (conjuncts (arg1 sentence)))
       (atomic-clause? (arg2 sentence))))

(defun conjuncts (sentence)
  "Return a list of the conjuncts in this sentence."
  (cond ((eq (op sentence) 'and) (args sentence))
	((eq sentence 'true) nil)
	(t (list sentence))))

(defun disjuncts (sentence)
  "Return a list of the disjuncts in this sentence."
  (cond ((eq (op sentence) 'or) (args sentence))
	((eq sentence 'false) nil)
	(t (list sentence))))

(defun conjunction (args)
  "Form a conjunction with these args."
  (case (length args)
    (0 'true)
    (1 (first args))
    (t (cons 'and args))))

(defun disjunction (args)
  "Form a disjunction with these args."
  (case (length args)
    (0 'false)
    (1 (first args))
    (t (cons 'or args))))

;;; An expression is a list consisting of a prefix operator followed by args,
;;; Or it can be a symbol, denoting an operator with no arguments.
;;; Expressions are used in Logic, and as actions for agents.

(defun make-exp (op &rest args) (cons op args))
(defun op (exp) (if (listp exp) (first exp) exp))
(defun args (exp) (if (listp exp) (rest exp) nil))
(defun arg1 (exp) (first (args exp)))
(defun arg2 (exp) (second (args exp)))

(defsetf args (exp) (new-value)
  `(setf (cdr ,exp) ,new-value))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))


(defun string->prefix (string)		; added by norvig
  "Convert a string to a prefix s-expression using the infix reader.
  If the argument is not a string, just return it as is."
  (if (stringp string)
      (with-input-from-string (stream (concatenate 'string "#I(" string ")"))
	(read stream))
      string))

(defun mklist (x)
  "If x is a list, return it; otherwise return a singleton list, (x)."
  (if (listp x) x (list x)))

(defun mappend (fn &rest lists)
  "Apply fn to respective elements of list(s), and append results."
  (reduce #'append (apply #'mapcar fn lists) :from-end t))
