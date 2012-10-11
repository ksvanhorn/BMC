(in-package :mcimpl)

(defadt1 mcimpl
  parameters derived updates)

(defun params-names (impl)
  (mapcar #'decl-var (mcimpl-parameters impl)))

(defun read-mcimpl (ifname)
  (sexpr->mcimpl (read-file ifname)))

(defun sexpr->mcimpl (se)
  (unless (and (starts-with ':mcimpl se) (is-list-of-length 4 se))
    (error "mcimpl file should have form (:mcimpl parameters derived updates)."))
  (let ((parameters (sexpr->parameters (second se)))
	(derived (sexpr->derived (third se)))
	(updates (sexpr->updates (fourth se))))
    (make-mcimpl :parameters parameters
		 :derived derived
		 :updates updates)))

(defun sexpr->parameters (se)
  (check-parameters-section se)
  (mapcar #'sexpr->decl (rest se)))

(defun sexpr->derived (se)
  (check-derived-section se)
  (flet
    ((sexpr->association (x)
       (check-derived-decl x)
       (cons (first x) (sexpr->expr (second x)))))
    (mapcar #'sexpr->association (rest se))))

(defun sexpr->updates (se)
  (check-updates-section se)
  (flet ((sexpr->upd (x)
	   (destructuring-bind (name . expr) x
	     (check-is-symbol name)
             (cons name (sexpr->rel expr)))))
    (mapcar #'sexpr->upd (list->pair-list (rest se)))))

(defun check-parameters-section (se)
  (unless (starts-with ':parameters se)
    (error "Parameters section of mcimpl file should be a list beginning with ':parameters.")))

(defun check-derived-section (se)
  (unless (starts-with ':derived se)
    (error "Derived section of mcimpl file should be a list beginning with ':derived.")))

(defun check-derived-decl (x)
  (unless (and (is-list-of-length 2 x) (symbolp (first x)))
    (error "Invalid form for declaration of derived value: ~w." x)))

(defun check-updates-section (se)
  (unless (starts-with ':updates se)
    (error "Updates section of mcimpl file should be a list beginning with ':updates.")))

(defun check-is-symbol (name)
  (unless (symbolp name)
    (error "Update label must be a symbol, not ~w." name)))

(defun mcimpl->substituted-updates (impl)
  (let ((derived (mcimpl-derived impl))
	(updates (mcimpl-updates impl)))
    (mapcar (lambda (x) (cons (car x) (subst-derived derived (cdr x))))
	    updates)))

(defun free-vars-in-rel (rel)
  (adt-case relation rel
    ((stochastic lhs rhs)
     (append (free-vars-in-rellhs lhs) (free-vars-in-distr rhs)))
    ((block members)
     (append-mapcar #'free-vars-in-rel members))
    ((if condition true-branch false-branch)
     (append (free-vars-in-expr condition)
	     (free-vars-in-rel true-branch)
	     (free-vars-in-rel false-branch)))
    ((loop var lo hi body)
     (append (free-vars-in-expr lo)
	     (free-vars-in-expr hi)
	     (remove var (free-vars-in-rel body))))
    ((let var val body)
     (append (free-vars-in-expr val)
	     (remove var (free-vars-in-rel body))))
    ((mh proposal-distribution log-acceptance-factor)
     (append (free-vars-in-rel proposal-distribution)
	     (free-vars-in-expr log-acceptance-factor)))
    ((skip)
     '())))

(defun free-vars-in-distr (distr)
  (match-adt1 (distribution name args) distr
    (append-mapcar #'free-vars-in-expr args)))

(defun free-vars-in-rellhs (lhs)
  (adt-case rellhs lhs
    ((simple var)
     (list var))
    ((array-elt var indices)
     (cons var (append-mapcar #'free-vars-in-expr indices)))
    ((array-slice var indices)
     (cons var (append-mapcar #'free-vars-in-array-slice-index indices)))))

(defun free-vars-in-array-slice-index (idx)
  (adt-case array-slice-index idx
    ((scalar value)
     (free-vars-in-expr value))
    ((range lo hi)
     (append (free-vars-in-expr lo) (free-vars-in-expr hi)))
    ((all)
     '())))

(defun subst-derived (derived rel)
  (subst-derived-1 (reverse derived) rel))

(defun subst-derived-1 (rderived rel)
  (flet ((op (r x)
	   (destructuring-bind (var . def) x
	     (if (member var (free-vars-in-rel r))
		 (make-relation-let :var var :val def :body r)
	       r))))
    (reduce #'op rderived :initial-value rel)))

#|
(defun subst-derived (derived rel)
  (dolist (d derived)
    (destructuring-bind (var . expr) d
      (setf rel (subst-expr-in-rel var expr rel))))
  rel)

(defun subst-expr-in-rel (v e rel)
  (let ((freev (free-vars-in-expr e)))
    (labels
      ((subst-expr-in-rel-1 (r)
	 (adt-case relation r
	   ((stochastic lhs rhs)
	    (make-relation-stochastic
	     :lhs (subst-expr-in-rellhs v e lhs)
	     :rhs (subst-expr-in-distribution v e rhs)))
	   ((block members)
	    (make-relation-block
	     :members (mapcar #'subst-expr-in-rel-1 members)))
	   ((if condition true-branch false-branch)
	    (make-relation-if
	     :condition (subst-expr v e condition)
	     :true-branch (subst-expr-in-rel-1 true-branch)
	     :false-branch (subst-expr-in-rel-1 false-branch)))
	   ((let var val body)
	    (let ((new-var var)
		  (new-val (subst-expr v e val))
		  (new-body body))
	      (cond
	        ((eq v var)
		 )
		((not (member var freev))
		 (setf new-body (subst-expr-in-rel-1 body)))
		(t
		 (setf new-var (symbol-not-in freev (symbol-name var)))
		 (setf new-body
		       (subst-expr-in-rel var (expr-var new-var) body))
		 (setf new-body (subst-expr-in-rel-1 new-body))))
	      (make-relation-let :var new-var :val new-val :body new-body)))
	   ((loop var lo hi body)
	    (let ((new-lo (subst-expr v e lo))
		  (new-hi (subst-expr v e hi))
		  (new-var var)
		  (new-body body))
	      (cond
	       ((eq v var)
		)
	       ((not (member var freev))
		(setf new-body (subst-expr-in-rel-1 body)))
	       (t 
		(setf new-var (symbol-not-in freev (symbol-name var)))
		(setf new-body
		      (subst-expr-in-rel var (expr-var new-var) body))
		(setf new-body (subst-expr-in-rel-1 new-body))))
	      (make-relation-loop
	       :var new-var :lo new-lo :hi new-hi :body new-body)))
	   ((skip)
	    r))))
      (subst-expr-in-rel-1 rel))))

(defun subst-expr-in-rellhs (v expr lhs)
  (adt-case rellhs lhs
    ((simple var)
     (check-subst-var-not-rellhs-var var v expr)
     lhs)
    ((array-elt var indices)
     (check-subst-var-not-rellhs-var var v expr)
     (make-rellhs-array-elt
       :var var
       :indices (mapcar (lambda (x) (subst-expr v expr x)) indices)))
    ((array-slice var indices)
     (check-subst-var-not-rellhs-var var v expr)
     (make-rellhs-array-slice
      :var var
      :indices
      (mapcar (lambda (x) (subst-array-slice-index v expr x)) indices)))))

(defun subst-array-slice-index (v expr asi)
  (adt-case array-slice-index asi
    ((scalar value)
     (make-array-slice-index-scalar :value (subst-expr v expr value)))
    ((range lo hi)
     (make-array-slice-index-range
      :lo (subst-expr v expr lo)
      :hi (subst-expr v expr hi)))
    ((all)
     asi)))

(defun check-subst-var-not-rellhs-var (var v expr)
  (when (eq v var)
    (error "Illegal rellhs substitution: ~a for ~a in (~~ ~a ...)"
	   expr v var)))

(defun subst-expr-in-distribution (var expr rhs)
  (match-adt1 (distribution name args) rhs
    (make-distribution
     :name name
     :args (mapcar (lambda (x) (subst-expr var expr x)) args))))
|#
