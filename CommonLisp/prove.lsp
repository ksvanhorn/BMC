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

(defun subst-expr (v replacement e)
  (let ((freev (free-vars-in-expr replacement)))
    (labels
      (
       (subst-expr-1 (x)
	 (adt-case expr x
	   ((literal value)
	    x)
	   ((const symbol)
	    x)
	   ((variable symbol)
	    (if (eq v symbol) replacement x))
	   ((apply fct args)
	    (expr-app fct (mapcar #'subst-expr-1 args)))
	   ((let var val body)
	    (subst-let var val body))
	   ((quantifier op lo hi var body)
	    (subst-quant op lo hi var body))))
	       ;((quantifier op lo hi var  body)

       (subst-let (var val body)
	 (let ((newval (subst-expr-1 val))
	       (newvar var)
	       (newbody body)
	       (freev-body (free-vars-in-expr body)))
	   (when (and (not (eq v var)) (member v freev-body))
	     (when (member var freev)
	       (setf newvar (symbol-not-in
			      (append freev freev-body)
			      (symbol-name var)))
	       (setf newbody (subst-expr var (expr-var newvar) body)))
	     (setf newbody (subst-expr-1 newbody)))
	   (make-expr-let :var newvar :val newval :body newbody)))

       (subst-quant (op lo hi var body)
	 (make-expr-quantifier
	   :op op :lo lo :hi hi :var var :body (subst-expr-1 body)))

       )

      (subst-expr-1 e)
)))

