(defpackage :model
  (:use :common-lisp :expr :utils)
  (:export
    :read-model :extract-args :extract-reqs :extract-vars :extract-body
    :decl-var :decl-typ :type-class :elem-type :type-dims
    :rel-class :rel-var :rel-val :rel-distr :rel-block-body
    :rel-if-condition :rel-true-branch :rel-false-branch
    :rel-loop-var :rel-loop-bounds :rel-loop-body :bounds-lo :bounds-hi
    :args-base-decls :vars-base-decls :args-checks))
(in-package :model)

; TODO: fuller check of model structure?

(defun read-model (ifname)
  (let* ((uc (append *model-keywords* *quantifiers*))
 	 (mdl (read-file-upcasing-only uc ifname))
	 (dups (duplicate-vars mdl))
	 (badv (bad-rel-vars mdl))
	 (shadow (remove-duplicates (shadowing-indices mdl)))
	 (bad-ndims (remove-duplicates (bad-num-dimensions mdl))))
    (unless (null dups)
      (error-with-list "Multiple declarations for var(s)" dups))
    (unless (null badv)
      (error-with-list "Relation for unknown var(s)" badv))
    (unless (null shadow)
      (error-with-list "Loop indices that shadow other indices or variables"
		       shadow))
    (unless (null bad-ndims)
      (error-with-list "Variables used with wrong number of dimensions"
		       bad-ndims))
    mdl))

(defun error-with-list (format lst)
  (error (format nil (strcat format ": ~{~a~^, ~}") lst)))

(defparameter *scalar-types*
  `(:realp :realnn :real :realx :realxn :integerp :integernn :integer :boolean))

(defparameter *model-keywords*
  (append
    '(:model :args :reqs :vars :body :for :if :block :range :all)
    *scalar-types*))

(defun extract-args (mdl) (cdr (second mdl)))
(defun extract-reqs (mdl) (cdr (third mdl)))
(defun extract-vars (mdl) (cdr (fourth mdl)))
(defun extract-body (mdl) (cdr (fifth mdl)))

(defun decl-var (decl) (first decl))
(defun decl-typ (decl) (second decl))

(defun type-class (typ)
  (cond ((symbolp typ) :scalar)
	((listp typ) :array)
	(t (error (format nil "Invalid type: ~a" typ)))))

(defun elem-type (typ) (car typ))

(defun type-dims (typ) (if (symbolp typ) '() (cdr typ)))

(defun rel-class (rel)
  (case (first rel)
	(:<- :deterministic)
	(:~ :stochastic)
	(:block :block)
	(:if (case (length (rest rel))
		   (2 :if-then)
		   (3 :if-then-else)))
	(:for :loop)))

(defun rel-var (rel) (second rel))
(defun rel-val (rel) (third rel))
(defun rel-distr (rel) (third rel))
(defun rel-block-body (rel) (rest rel))
(defun rel-if-condition (rel) (second rel))
(defun rel-true-branch (rel) (third rel))
(defun rel-false-branch (rel) (fourth rel))
(defun rel-loop-var (rel) (second rel))
(defun rel-loop-bounds (rel) (third rel))
(defun rel-loop-body (rel) (fourth rel))
(defun bounds-lo (bnds) (first bnds))
(defun bounds-hi (bnds) (second bnds))

(defun args-names (mdl)
  (mapcar #'decl-var (extract-args mdl)))

(defun vars-names (mdl)
  (mapcar #'decl-var (extract-vars mdl)))

(defun args-vars-names (mdl)
  (append (args-names mdl) (vars-names mdl)))

(defun duplicate-vars (mdl)
  (let* ((names (args-vars-names mdl))
	 (seen nil)
	 (dups nil))
    (dolist (x names)
      (let ((n (length seen)))
	(pushnew x seen)
	(if (= n (length seen)) ; duplicate
	  (pushnew x dups))))
    dups))

(defun bad-rel-vars (mdl)
  (let ((declared-vars (vars-names mdl))
	(defined-vars (apply #'append (mapcar #'rel-vars (extract-body mdl))))
	(bad nil))
    (dolist (x defined-vars)
      (if (not (member x declared-vars))
	(pushnew x bad)))
    bad))

(defun rel-vars (rel)
  (case (rel-class rel)
	(:deterministic (rel-vars-0 rel))
	(:stochastic (rel-vars-0 rel))
	(:block (apply #'append (mapcar #'rel-vars (rel-block-body rel))))
	(:if-then (rel-vars (rel-true-branch rel)))
	(:if-then-else (append (rel-vars (rel-true-branch rel))
			       (rel-vars (rel-false-branch rel))))
	(:loop (rel-vars (rel-loop-body rel)))))

(defun rel-vars-0 (rel)
  (list (base-var (rel-var rel))))

(defun base-var (rel-lhs)
  (cond ((symbolp rel-lhs) rel-lhs)
	((eq :array-app (expr-class rel-lhs)) (array-op rel-lhs))
	(t (error "Invalid LHS for relation"))))

(defun shadowing-indices (mdl)
  (let ((names (args-vars-names mdl)))
    (append (shadowing-indices-decls (extract-args mdl) names)
	    (shadowing-indices-decls (extract-vars mdl) names)
	    (shadowing-indices-exprs (extract-reqs mdl) names)
	    (shadowing-indices-rels (extract-body mdl) names))))

(defun shadowing-indices-decls (decls names)
  (apply #'append (mapcar (lambda (d) (shadowing-indices-decl d names)) decls)))

(defun shadowing-indices-decl (d names)
  (append (shadowing-indices-expr (decl-var d) names)
	  (shadowing-indices-type (decl-typ d) names)))

(defun shadowing-indices-type (typ names)
  (case (type-class typ)
	(:scalar '())
	(:array (shadowing-indices-exprs (type-dims typ) names))))

(defun shadowing-indices-rels (rels names)
  (apply #'append (mapcar (lambda (r) (shadowing-indices-rel r names)) rels)))

(defun shadowing-indices-rel (r names)
  (case (rel-class r)
	(:deterministic
	  (shadowing-indices-exprs (list (rel-var r) (rel-val r)) names))
	(:stochastic
	  (shadowing-indices-exprs (list (rel-var r) (rel-distr r)) names))
	(:block
	  (shadowing-indices-rels (rel-block-body r) names))
	(:if-then
	  (append (shadowing-indices-expr (rel-if-condition r) names)
		  (shadowing-indices-rel (rel-true-branch r) names)))
	(:if-then-else
	  (append (shadowing-indices-expr (rel-if-condition r) names)
		  (shadowing-indices-rel (rel-true-branch r) names)
		  (shadowing-indices-rel (rel-false-branch r)) names))
	(:loop
	  (let* ((bnds (rel-loop-bounds r))
		 (elist (list (bounds-lo bnds) (bounds-hi bnds)))
		 (idx-var (rel-loop-var r))
		 (maybe-var nil)
		 (new-names names))
	    (if (member idx-var names)
	      (setf maybe-var (list idx-var))
	      (push idx-var new-names))
	    (append (shadowing-indices-exprs elist names)
		    maybe-var
		    (shadowing-indices-rel (rel-loop-body r) new-names))))))

(defun shadowing-indices-exprs (elist names)
  (apply #'append (mapcar (lambda (x) (shadowing-indices-expr x names)) elist)))

(defun shadowing-indices-expr (x names)
  (case (expr-class x)
	(:literal-num nil)
	(:variable nil)
	(:quant (shadowing-indices-quant x names))
	(:array-app (shadowing-indices-array-app x names))
	(:funct-app (shadowing-indices-funct-app x names))
	(t (error (format nil "Unknown expr class ~w" (expr-class x))))))

(defun shadowing-indices-array-app (x names)
  (append (shadowing-indices-expr (array-op x) names)
	  (shadowing-indices-iexprs (array-args x) names)))

(defun shadowing-indices-iexprs (x names)
  (apply #'append (mapcar (lambda (ie) (shadowing-indices-iexpr ie names)) x)))

(defun shadowing-indices-iexpr (x names)
  (case (index-class x)
	(:all nil)
	(:range
	  (shadowing-indices-exprs (list (range-lo x) (range-hi x)) names))
	(:index (shadowing-indices-expr x names))))

(defun shadowing-indices-funct-app (x names)
  (append (shadowing-indices-expr (fct-op x) names)
	  (shadowing-indices-exprs (fct-args x) names)))

(defun shadowing-indices-quant (x names)
  (let* ((bnds (quant-bounds x))
	 (elist (list (qbnds-lo bnds) (qbnds-hi bnds)))
	 (idx-var (quant-var x))
	 (maybe-var nil)
	 (new-names names))
    (if (member idx-var names)
      (setf maybe-var (list idx-var))
      (push idx-var new-names))
    (append (shadowing-indices-exprs elist names)
	    maybe-var
	    (shadowing-indices-expr (quant-body x) new-names))))

(defun bad-num-dimensions (mdl)
  (let* ((decls (append (extract-args mdl) (extract-vars mdl)))
	 (ndim-map (decls->ndim-assoc decls)))
    (append (bad-numdims-exprs (mapcar #'decl-var decls) ndim-map)
	    (bad-numdims-types (mapcar #'decl-typ decls) ndim-map)
	    (bad-numdims-exprs (extract-reqs mdl) ndim-map)
	    (bad-numdims-rels (extract-body mdl) ndim-map))))

(defun decls->ndim-assoc (decls)
  (let ((map nil))
    (dolist (x decls)
      (setf map (acons (decl-var x) (length (type-dims (decl-typ x))) map)))
    map))

(defun bad-numdims-exprs (elist ndmap)
  (apply #'append (mapcar (lambda (x) (bad-numdims-expr x ndmap)) elist)))

(defun bad-numdims-types (tlist ndmap)
  (apply #'append (mapcar (lambda (x) (bad-numdims-type x ndmap)) tlist)))

(defun bad-numdims-rels (rlist ndmap)
  (apply #'append (mapcar (lambda (x) (bad-numdims-rel x ndmap)) rlist)))

(defun bad-numdims-type (typ ndmap)
  (bad-numdims-exprs (type-dims typ) ndmap))

(defun bad-numdims-rel (r ndmap)
  (case (rel-class r)
	(:deterministic
	  (bad-numdims-exprs (list (rel-var r) (rel-val r)) ndmap))
	(:stochastic
	  (bad-numdims-exprs (list (rel-var r) (rel-distr r)) ndmap))
	(:block
	  (bad-numdims-rels (rel-block-body r) ndmap))
	(:if-then
	  (append (bad-numdims-expr (rel-if-condition r) ndmap)
		  (bad-numdims-rel (rel-true-branch r) ndmap)))
	(:if-then-else
	  (append (bad-numdims-expr (rel-if-condition r) ndmap)
		  (bad-numdims-rel (rel-true-branch r) ndmap)
		  (bad-numdims-rel (rel-false-branch r) ndmap)))
	(:loop
	  (append (bad-numdims-expr (bounds-lo (rel-loop-bounds r)) ndmap)
		  (bad-numdims-expr (bounds-hi (rel-loop-bounds r)) ndmap)
		  (bad-numdims-rel (rel-loop-body r) ndmap)))))

(defun bad-numdims-expr (x ndmap)
  (case (expr-class x)
	(:literal-num '())
	(:variable '())
	(:quant (bad-numdims-quant x ndmap))
	(:array-app (bad-numdims-array-app x ndmap))
	(:funct-app (bad-numdims-funct-app x ndmap))))

(defun bad-numdims-quant (x ndmap)
  (append (bad-numdims-expr (qbnds-lo (quant-bounds x)) ndmap)
	  (bad-numdims-expr (qbnds-hi (quant-bounds x)) ndmap)
	  (bad-numdims-expr (quant-body x) ndmap)))

(defun bad-numdims-array-app (x ndmap)
  (let ((avar (array-op x))
	(nidx (length (array-args x)))
	(maybe-var nil))
    (if (and (eq :variable (expr-class avar))
	     (/= nidx (assoc-lookup avar ndmap)))
      (setf maybe-var (list avar)))
    (append maybe-var
	    (bad-numdims-expr avar ndmap)
	    (bad-numdims-iexprs (array-args x) ndmap))))

(defun bad-numdims-iexprs (x ndmap)
  (apply #'append (mapcar (lambda (ie) (bad-numdims-iexpr ie ndmap)) x)))

(defun bad-numdims-iexpr (x ndmap)
  (case (index-class x)
	(:all nil)
	(:range (bad-numdims-exprs (list (range-lo x) (range-hi x)) ndmap))
	(:index (bad-numdims-expr x ndmap))))

(defun bad-numdims-funct-app (x ndmap)
  (append (bad-numdims-expr (fct-op x) ndmap)
	  (bad-numdims-exprs (fct-args x) ndmap)))


(defun args-base-decls (mdl)
  (mapcar #'base-decl (extract-args mdl)))

(defun vars-base-decls (mdl)
  (mapcar #'base-decl (extract-vars mdl)))

(defun base-decl (decl)
  (let ((var (decl-var decl))
	(typ (decl-typ decl)))
    (case (type-class typ)
	  (:scalar (list var (base-scalar-type typ) 0))
	  (:array (list var
			(base-scalar-type (elem-type typ))
			(length (type-dims typ)))))))

(defun base-scalar-type (typ)
  (cdr (assoc typ *base-scalar-types*)))

(defparameter *base-scalar-types*
  '((:realxn . :realxn) (:realx . :realxn) (:real . :realxn)
    (:realnn . :realxn) (:realp . :realxn)
    (:integer . :integer) (:integernn . :integer) (:integerp . :integer)
    (:boolean . :boolean)))

(defun args-checks (mdl)
  (append (flatten (mapcar #'decl-checks (extract-args mdl)))
	  (extract-reqs mdl)))

(defun decl-checks (decl)
  (let ((var (decl-var decl))
	(typ (decl-typ decl)))
    (case (type-class typ)
	  (:scalar (scalar-type-checks var typ))
	  (:array (array-type-checks var typ)))))

(defun scalar-type-checks (var typ)
  (let ((btyp (base-scalar-type typ)))
    (if (eq btyp typ)
	nil
        (list (simplify-check (list (type-predicate typ) var))))))

(defun type-predicate (typ)
  (assoc-lookup typ *type-predicates*))

(defparameter *type-predicates*
  '((:integerp . :is-integerp)
    (:integernn . :is-integernn)
    (:realp . :is-realp)
    (:realnn . :is-realnn)
    (:real . :is-real)
    (:realx . :is-realx)))

(defun simplify-check (check-expr)
  (destructuring-bind (pred var) check-expr
     (case pred
	   (:is-integernn `(:<= 0 ,var))
	   (:is-integerp `(:< 0 ,var))
	   (t check-expr))))

(defun array-type-checks (var typ)
  (let* ((etyp (elem-type typ))
	 (dims (type-dims typ))
	 (idxvars (index-vars (length dims) `(list ,var ,@dims))))
    (append (array-length-checks var dims 1)
	    (array-element-checks var etyp dims idxvars idxvars))))

(defun index-vars (n expr &optional (pfx "i"))
  (do ((result nil)
       (k 1 (1+ k)))
      ((= n 0) (reverse result))
     (let ((idxvar (var-symbol pfx k)))
       (when (fully-free-of idxvar expr)
	 (decf n)
	 (push idxvar result)))))

(defun var-symbol (pfx n)
  (intern (strcat pfx (write-to-string n)) "KEYWORD"))

(defun fully-free-of (var expr)
  (cond
    ((symbolp expr) (not (eq var expr)))
    ((consp expr) (and (fully-free-of var (car expr))
		       (fully-free-of var (cdr expr))))
    (t t)))

(defun array-length-checks (var dims n)
  (if (null dims)
      nil
      (cons `(:= (:array-length ,n ,var) ,(car dims))
	    (array-length-checks var (cdr dims) (1+ n)))))

(defun array-element-checks (var etyp dims idxvars all-idxvars)
  (if (null dims)
      (scalar-type-checks `(:@ ,var ,@all-idxvars) etyp)
      (let ((iv (car idxvars))
	    (idxvars1 (cdr idxvars))
	    (dims1 (cdr dims)))
	(mapcar (lambda (x) `(:qand ,iv (1 ,(car dims)) ,x))
		(array-element-checks var etyp dims1 idxvars1 all-idxvars)))))





