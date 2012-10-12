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
  (flet ((op (r x)
	   (destructuring-bind (var . def) x
	     (if (member var (free-vars-in-rel r))
		 (make-relation-let :var var :val def :body r)
	       r))))
    (reduce #'op (reverse derived) :initial-value rel)))
