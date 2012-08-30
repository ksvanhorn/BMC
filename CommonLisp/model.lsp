(in-package :model)

(defadt1 model args reqs vars body)

(defadt1 decl var typ)

(defadt vtype
  (scalar stype)
  (array elem-type dims))

(defadt relation
  (deterministic lhs rhs)
  (stochastic lhs rhs)
  (block members)
  (if condition true-branch false-branch)
  (loop var lo hi body)
  (skip))

(defadt1 distribution name args)

(defadt rellhs
  (simple var)
  (array-elt var indices)
  (array-slice var indices))

(defadt array-slice-index
  (scalar value)
  (range lo hi)
  (all))

(defun read-model (ifname)
  (sexpr->model (read-file ifname)))

(defun sexpr->model (x)
  (check-model-top-level x)
  (let ((mdl (let ((*convert-boolean-functions* nil))
               (make-model :args (mapcar #'sexpr->decl (cdr (second x)))
			   :reqs (mapcar #'sexpr->expr (cdr (third x)))
			   :vars (mapcar #'sexpr->decl (cdr (fourth x)))
			   :body (let ((*convert-boolean-functions* t))
				   (mapcar #'sexpr->rel (cdr (fifth x))))))))
    (check-model mdl)
    mdl))

(defun check-model-top-level (x)
  (unless (and (starts-with :model x)
	       (= 5 (length x))
	       (starts-with :args (second x))
	       (starts-with :reqs (third x))
	       (starts-with :vars (fourth x))
	       (starts-with :body (fifth x)))
    (error "Model must have form ~
            '(:model (:args ...) (:reqs ...) (:vars ...) (:body ...))'.")))

(defun sexpr->decl (x)
  (when (and (consp x) (= 2 (length x)))
    (destructuring-bind (var typ) x
      (when (is-var-symbol var)
	(return-from sexpr->decl
	  (make-decl :var var :typ (sexpr->vtype typ))))))
  (error "Invalid declaration: ~W." x))

(defun is-var-symbol (x)
  (and (symbolp x) (not (is-const-symbol x))))

(defun sexpr->vtype (x)
  (cond ((is-scalar-type-symbol x)
	 (make-vtype-scalar :stype x))
	((is-array-type-sexpr x)
	 (sexpr->vtype-array x))
	(t
	 (error "Invalid type ~W." x))))

(defun is-array-type-sexpr (x)
  (and (consp x) (< 1 (length x)) (is-scalar-type-symbol (car x))))

(defun sexpr->vtype-array (x)
  (make-vtype-array
    :elem-type (car x)
    :dims (mapcar #'sexpr->expr (cdr x))))

(defun sexpr->rel (x)
  (cond ((starts-with '<- x) (sexpr->determ-rel (cdr x)))
	((starts-with '~ x) (sexpr->stoch-rel (cdr x)))
	((starts-with :block x) (sexpr->block-rel (cdr x)))
	((starts-with :if x) (sexpr->if-rel (cdr x)))
	((starts-with :for x) (sexpr->loop-rel (cdr x)))
	(t (error "Invalid relation: ~W." x))))

(defun sexpr->determ-rel (x)
  (check-determ-rel x)
  (make-relation-deterministic
    :lhs (sexpr->rellhs (first x))
    :rhs (sexpr->expr (second x))))

(defun check-determ-rel (x)
  (unless (and (consp x) (= 2 (length x)))
    (error "Invalid deterministic relation ~W." (cons '<- x))))

(defun sexpr->stoch-rel (x)
  (check-stoch-rel x)
  (make-relation-stochastic
    :lhs (sexpr->rellhs (first x))
    :rhs (sexpr->distr (second x))))

(defun check-stoch-rel (x)
  (unless (and (consp x) (= 2 (length x)))
    (error "Invalid stochastic relation ~W." (const '~ x))))

(defun sexpr->distr (x)
  (check-distr x)
  (make-distribution
    :name (first x)
    :args (mapcar #'sexpr->expr (rest x))))

(defun check-distr (x)
  (unless (and (consp x) (is-distr-symbol (car x)))
    (error "Invalid distribution ~W." x)))

(defun sexpr->rellhs (x)
  (flet ((errfct () (error "Invalid LHS of relation: ~W." x)))
    (adt-case expr (sexpr->expr x)
      ((variable symbol)
       (make-rellhs-simple :var symbol))
      ((apply fct args)
       (expr-apply->rellhs #'errfct fct args))
      ((literal value) (errfct))
      ((const symbol) (errfct))
      ((quantifier op lo hi var body) (errfct)))))

(defun check-rellhs-expr (errfct fct args)
  (unless (and (member fct '(@ @-slice)) (is-expr-variable (first args)))
    (funcall errfct)))

(defun expr-apply->rellhs (errfct fct args)
  (check-rellhs-expr errfct fct args)
  (let ((var (expr-variable-symbol (first args)))
	(indices (rest args))
	(convert-idx (lambda (e) (expr->arr-slice-idx errfct e))))
    (cond ((eq '@ fct)
	   (make-rellhs-array-elt :var var :indices indices))
	  ((eq '@-slice fct)
	   (make-rellhs-array-slice
	     :var var :indices (mapcar convert-idx indices))))))

(defun expr->arr-slice-idx (errfct e)
  (adt-case expr e
    ((const symbol)
     (expr-const->arr-slice-idx errfct symbol))
    ((apply fct args)
     (expr-apply->arr-slice-idx errfct fct args))
    ((literal value)
     (funcall errfct))
    ((variable symbol)
     (funcall errfct))
    ((quantifier op lo hi var body)
     (funcall errfct))))

(defun expr-const->arr-slice-idx (errfct symbol)
  (unless (eq '@-all symbol)
    (funcall errfct))
  (make-array-slice-index-all))

(defun expr-apply->arr-slice-idx (errfct fct args)
  (cond ((eq '@-idx fct)
	 (@-idx->arr-slice-idx errfct args))
	((eq '@-rng fct)
	 (@-rng->arr-slice-idx errfct args))
	(t
	 (funcall errfct))))

(defun @-idx->arr-slice-idx (errfct args)
  (unless (= 1 (length args))
    (funcall errfct))
  (make-array-slice-index-scalar :value (first args)))

(defun @-rng->arr-slice-idx (errfct args)
  (unless (= 2 (length args))
    (funcall errfct))
  (make-array-slice-index-range :lo (first args) :hi (second args)))

(defun sexpr->block-rel (x)
  (make-relation-block :members (mapcar #'sexpr->rel x)))

(defun sexpr->if-rel (x)
  (check-if-rel x)
  (destructuring-bind (condition tclause &optional (fclause nil fc-p)) x
    (let ((e-condition (sexpr->expr condition))
	  (r-tclause (sexpr->rel tclause))
	  (r-fclause (if fc-p (sexpr->rel fclause) (make-relation-skip))))
      (make-relation-if :condition e-condition
			:true-branch r-tclause :false-branch r-fclause))))

(defun check-if-rel (x)
  (let ((n (length x)))
    (unless (or (= 2 n) (= 3 n))
      (error "Invalid if-then(-else) relation: ~W" (cons 'if x)))))

(defun sexpr->loop-rel (x)
  (check-loop-rel x)
  (destructuring-bind (var (x-lo x-hi) x-body) x
    (let ((lo (sexpr->expr x-lo))
	  (hi (sexpr->expr x-hi))
	  (body (sexpr->rel x-body)))
      (make-relation-loop :var var :lo lo :hi hi :body body))))

(defun check-loop-rel (x)
  (unless (and (= 3 (length x))
	       (is-variable-symbol (first x))
	       (consp (second x))
	       (= 2 (length (second x))))
    (error "Invalid loop (for) relation: ~W." (cons 'for x))))

;;; Model checks.
;;; TODO: fuller check of model structure?

(defun check-model (mdl)
  (let ((dups (duplicate-vars mdl))
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
		       bad-ndims))))

(defun error-with-list (format lst)
  (error (format nil (strcat format ": ~{~a~^, ~}") lst)))

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

(defun args-vars-names (mdl)
  (append (args-names mdl) (vars-names mdl)))

(defun args-names (mdl)
  (mapcar #'decl-var (model-args mdl)))

(defun vars-names (mdl)
  (mapcar #'decl-var (model-vars mdl)))

(defun bad-rel-vars (mdl)
  (let ((declared-vars (vars-names mdl))
	(defined-vars (apply #'append (mapcar #'rel-vars (model-body mdl))))
	(bad nil))
    (dolist (x defined-vars)
      (if (not (member x declared-vars))
	(pushnew x bad)))
    bad))

(defun rel-vars (rel)
  (adt-case relation rel
    ((deterministic lhs rhs)
     (lhs-var lhs))
    ((stochastic lhs rhs)
     (lhs-var lhs))
    ((block members)
     (apply #'append (mapcar #'rel-vars members)))
    ((if condition true-branch false-branch)
     (append (rel-vars true-branch) (rel-vars false-branch)))
    ((loop var lo hi body)
     (rel-vars body))
    ((skip)
     '())))

(defun lhs-var (lhs)
  (adt-case rellhs lhs
    ((simple var) (list var))
    ((array-elt var indices) (list var))
    ((array-slice var indices) (list var))))

(defun shadowing-indices (mdl)
  (let ((names (args-vars-names mdl)))
    (append (shadowing-indices-decls (model-args mdl) names)
	    (shadowing-indices-decls (model-vars mdl) names)
	    (shadowing-indices-exprs (model-reqs mdl) names)
	    (shadowing-indices-rels (model-body mdl) names))))

(defun shadowing-indices-decls (decls names)
  (apply #'append
	 (mapcar (lambda (d) (shadowing-indices-decl d names)) decls)))

(defun shadowing-indices-decl (d names)
  (shadowing-indices-vtype (decl-typ d) names))

(defun shadowing-indices-vtype (typ names)
  (adt-case vtype typ
    ((scalar stype)
     '())
    ((array elem-type dims)
     (shadowing-indices-exprs dims names))))

(defun shadowing-indices-rels (rels names)
  (apply #'append (mapcar (lambda (r) (shadowing-indices-rel r names)) rels)))

(defun shadowing-indices-rel (r names)
  (adt-case relation r
    ((deterministic lhs rhs)
     (shadowing-indices-exprs (list (rellhs->expr lhs) rhs) names))
    ((stochastic lhs rhs)
     (shadowing-indices-exprs
       (cons (rellhs->expr lhs) (distribution-args rhs)) names))
    ((block members)
     (shadowing-indices-rels members names))
    ((if condition true-branch false-branch)
     (append (shadowing-indices-expr condition names)
	     (shadowing-indices-rel true-branch names)
	     (shadowing-indices-rel false-branch names)))
    ((loop var lo hi body)
     (shadowing-indices-loop var lo hi body names))
    ((skip) '())))

(defun rellhs->expr (lhs)
  (adt-case rellhs lhs
    ((simple var)
     (make-expr-variable :symbol var))
    ((array-elt var indices)
     (make-expr-apply
       :fct '@
       :args (cons (make-expr-variable :symbol var) indices)))
    ((array-slice var indices)
     (make-expr-apply
       :fct '@-slice 
       :args (cons (make-expr-variable :symbol var)
		   (mapcar #'idx-rellhs->expr indices))))))

(defun idx-rellhs->expr (index)
  (adt-case array-slice-index index
    ((scalar value)
     value)
    ((range lo hi)
     (make-expr-apply :fct '@-rng :args (list lo hi)))
    ((all)
     (make-expr-const :symbol '@-all))))

(defun shadowing-indices-loop (var lo hi body names)
  (let ((maybe-var '())
	(new-names names))
    (when (member var names)
      (push var maybe-var)
      (push var new-names))
    (append (shadowing-indices-expr lo names)
	    (shadowing-indices-expr hi names)
	    maybe-var
	    (shadowing-indices-rel body new-names))))

(defun shadowing-indices-exprs (exprs names)
  (apply #'append (mapcar (lambda (x) (shadowing-indices-expr x names)) exprs)))

(defun shadowing-indices-expr (x names)
  (adt-case expr x
    ((literal value) '())
    ((const symbol) '())
    ((variable symbol) '())
    ((quantifier op lo hi var body)
     (shadowing-indices-quant op lo hi var body names))
    ((apply fct args) 
     (shadowing-indices-exprs args names))))

(defun shadowing-indices-quant (op lo hi var body names)
  (let ((maybe-var '())
	(new-names names))
    (when (member var names)
      (push var maybe-var)
      (push var new-names))
    (append (shadowing-indices-expr lo names)
	    (shadowing-indices-expr hi names)
	    maybe-var
	    (shadowing-indices-expr body new-names))))

(defun bad-num-dimensions (mdl)
  (let* ((decls (append (model-args mdl) (model-vars mdl)))
	 (ndim-map (decls->ndim-assoc decls)))
    (append (bad-numdims-vtypes (mapcar #'decl-typ decls) ndim-map)
	    (bad-numdims-exprs (model-reqs mdl) ndim-map)
	    (bad-numdims-rels (model-body mdl) ndim-map))))

(defun decls->ndim-assoc (decls)
  (let ((map nil))
    (dolist (x decls)
      (setf map (acons (decl-var x) (num-dims-vtype (decl-typ x)) map)))
    map))

(defun num-dims-vtype (typ)
  (adt-case vtype typ
    ((scalar stype) 0)
    ((array elem-type dims) (length dims))))

(defun bad-numdims-exprs (elist ndmap)
  (apply #'append (mapcar (lambda (x) (bad-numdims-expr x ndmap)) elist)))

(defun bad-numdims-vtypes (tlist ndmap)
  (apply #'append (mapcar (lambda (x) (bad-numdims-vtype x ndmap)) tlist)))

(defun bad-numdims-rels (rlist ndmap)
  (apply #'append (mapcar (lambda (x) (bad-numdims-rel x ndmap)) rlist)))

(defun bad-numdims-vtype (typ ndmap)
  (adt-case vtype typ
    ((scalar stype) '())
    ((array elem-type dims) (bad-numdims-exprs dims ndmap))))

(defun bad-numdims-rel (r ndmap)
  (adt-case relation r
    ((deterministic lhs rhs)
     (bad-numdims-exprs (list (rellhs->expr lhs) rhs) ndmap))
    ((stochastic lhs rhs)
     (bad-numdims-exprs
       (cons (rellhs->expr lhs) (distribution-args rhs)) ndmap))
    ((block members)
     (bad-numdims-rels members ndmap))
    ((if condition true-branch false-branch)
     (append (bad-numdims-expr condition ndmap)
	     (bad-numdims-rel true-branch ndmap)
	     (bad-numdims-rel false-branch ndmap)))
    ((loop var lo hi body)
     (append (bad-numdims-exprs (list lo hi) ndmap)
	     (bad-numdims-rel body ndmap)))
    ((skip)
     '())))

(defun bad-numdims-exprs (xs ndmap)
  (apply #'append (mapcar (lambda (x) (bad-numdims-expr x ndmap)) xs)))

(defun bad-numdims-expr (x ndmap)
  (adt-case expr x
    ((literal value)
     '())
    ((const symbol)
     '())
    ((variable symbol)
     '())
    ((quantifier op lo hi var body)
     (append (bad-numdims-exprs (list lo hi) ndmap)
	     (bad-numdims-expr body ndmap)))
    ((apply fct args)
     (bad-numdims-apply fct args ndmap))))

(defun bad-numdims-apply (fct args ndmap)
  (let ((bad-numdims-args (bad-numdims-exprs args ndmap)))
    (if (member fct '(@ @-slice))
      (let ((arr-var (expr-variable-symbol (first args))))
	(append bad-numdims-args
		(if (= (length args) (1+ (ndims-var arr-var ndmap)))
		  '()
		  (list arr-var))))
      bad-numdims-args)))

(defun ndims-var (arr-var ndmap)
  (let ((x (assoc arr-var ndmap)))
    (if (null x) 0 (cdr x))))

;;; Pretty-printing

(defun vtype->string (typ)
  (adt-case vtype typ
    ((scalar stype)
     (scalar-type-name stype))
    ((array elem-type dims)
     (format nil "~a[~{~a~^, ~}]"
	     (scalar-type-name elem-type)
	     (mapcar #'expr->string dims)))))

(defun scalar-type-name (typ)
  (unless (is-scalar-type-symbol typ)
    (error "Invalid scalar type: ~a." typ))
  (symbol-name typ))

(defun distr->string (d)
  (match-adt1 (distribution name args) d
    (format nil "~a(~{~a~^, ~})"
	    (symbol-name name) (mapcar #'expr->string args))))

(defun rellhs->string (lhs)
  (expr->string (rellhs->expr lhs)))

(defun pp-decl (decl)
  (fmt "~a : ~a"
       (symbol-name (decl-var decl))
       (vtype->string (decl-typ decl))))

(defun pp-rel (rel)
  (adt-case relation rel
    ((deterministic lhs rhs)
     (pp-rel-deterministic lhs rhs))
    ((stochastic lhs rhs)
     (pp-rel-stochastic lhs rhs))
    ((block members)
     (pp-rel-block members))
    ((if condition true-branch false-branch)
     (pp-rel-if condition true-branch false-branch))
    ((loop var lo hi body)
     (pp-rel-loop var lo hi body))
    ((skip))))

(defun pp-rel-deterministic (lhs rhs)
  (fmt "~a <- ~a" (rellhs->string lhs) (expr->string rhs)))

(defun pp-rel-stochastic (lhs rhs)
  (fmt "~a ~~ ~a" (rellhs->string lhs) (distr->string rhs)))

(defun pp-rel-block (members)
  (mapc #'pp-rel members))

(defmacro pp-hdr-block (header &rest body)
  `(progn
     (fmt "~a {" ,header)
     (indent ,@body)
     (fmt "}")))

(defun pp-rel-if (condition true-branch false-branch)
  (pp-hdr-block
    (format nil "if (~a)" (expr->string condition))
    (pp-rel true-branch))
  (unless (is-relation-skip false-branch)
    (pp-hdr-block "else" (pp-rel false-branch))))

(defun pp-rel-loop (var lo hi body)
  (pp-hdr-block
    (format nil "for (~a in ~a : ~a)"
	    (symbol-name var) (expr->string lo) (expr->string hi))
    (pp-rel body)))

(defun pp-expr (x)
  (fmt  "~a" (expr->string x)))

(defun pp-model (mdl)
  (match-adt1 (model args reqs vars body) mdl
    (pp-hdr-block "args"
      (dolist (x args) (pp-decl x)))
    (pp-hdr-block "reqs"
      (dolist (x reqs) (pp-expr x)))
    (pp-hdr-block "vars"
      (dolist (x vars) (pp-decl x)))
    (pp-hdr-block "model"
      (dolist (x body) (pp-rel x)))))
   

#|

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

(defun args-assums (mdl)
  (append (flatten (mapcar #'decl-assums (extract-args mdl)))
	  (extract-reqs mdl)))

(defun decl-checks (decl)
  (let ((var (decl-var decl))
	(typ (decl-typ decl)))
    (case (type-class typ)
	  (:scalar (scalar-type-checks var typ))
	  (:array (array-type-checks var typ)))))

(defun decl-assums (decl)
  (let ((var (decl-var decl))
	(typ (decl-typ decl)))
    (case (type-class typ)
	  (:scalar (scalar-type-assums var typ))
	  (:array (array-type-assums var typ)))))

(defun scalar-type-checks (var typ)
  (let ((btyp (base-scalar-type typ)))
    (if (eq btyp typ)
	nil
        (list (simplify-check (list (type-predicate typ) var))))))

(defun scalar-type-assums (var typ)
  (list (list (type-predicate typ) var)))

(defun type-predicate (typ)
  (assoc-lookup typ *type-predicates*))

(defparameter *type-predicates*
  '((:integerp . :is-integerp)
    (:integernn . :is-integernn)
    (:integer . :is-integer)
    (:realp . :is-realp)
    (:realnn . :is-realnn)
    (:real . :is-real)
    (:realx . :is-realx)
    (:realxn . :is-realxn)))

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

(defun array-type-assums (var typ)
  (let* ((etyp (elem-type typ))
	 (dims (type-dims typ)))
    (append (array-length-assums var dims 1)
	    (array-element-assums var etyp dims))))

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

(defun array-length-assums (var dims n)
  (if (null dims)
      nil
      (cons `(:= (:array-length ,n ,var) ,(car dims))
	    (array-length-assums var (cdr dims) (1+ n)))))

(defun array-element-checks (var etyp dims idxvars all-idxvars)
  (if (null dims)
      (scalar-type-checks `(:@ ,var ,@all-idxvars) etyp)
      (let ((iv (car idxvars))
	    (idxvars1 (cdr idxvars))
	    (dims1 (cdr dims)))
	(mapcar (lambda (x) `(:qand ,iv (1 ,(car dims)) ,x))
		(array-element-checks var etyp dims1 idxvars1 all-idxvars)))))

(defun aidx-reqs (v len)
  `((:is-integerp ,v) (:<= ,v ,len)))

(defun array-element-assums (var etyp dims)
  (let* ((idx-vars (loop for d in dims collect (gensym "?")))
 	 (idx-reqs (flatten (mapcar #'aidx-reqs idx-vars dims)))
	 (antecedent (if (= 1 (length idx-reqs))
	 	       (car idx-reqs)
		       (cons :and idx-reqs)))
	 (tpred (type-predicate etyp)))
    `(:all (,@idx-vars) (:=> ,antecedent (,tpred (:@ ,var ,@idx-vars))))))
|#