(in-package :model)

(defadt1 model args reqs vars body)

(defadt1 decl var typ)

(defadt vtype
  (scalar stype)
  (array elem-type dims))

(defadt relation
  (stochastic lhs rhs)
  (block members)
  (if condition true-branch false-branch)
  (loop var lo hi body)
  (let var val body)
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
  (let ((mdl (raw-sexpr->model x)))
    (check-model mdl)
    mdl))

(defun raw-sexpr->model (x)
  (let ((*convert-boolean-functions* nil))
    (make-model :args (mapcar #'sexpr->decl (cdr (second x)))
		:reqs (mapcar #'sexpr->expr (cdr (third x)))
		:vars (mapcar #'sexpr->decl (cdr (fourth x)))
		:body (let ((*convert-boolean-functions* t))
			(mapcar #'sexpr->rel (cdr (fifth x)))))))

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
  (cond ((starts-with '~ x) (sexpr->stoch-rel (cdr x)))
	((starts-with :block x) (sexpr->block-rel (cdr x)))
	((starts-with :if x) (sexpr->if-rel (cdr x)))
	((starts-with :for x) (sexpr->loop-rel (cdr x)))
	((starts-with :let x) (sexpr->let-rel (cdr x)))
	(t (error "Invalid relation: ~W." x))))

(defun sexpr->let-rel (x)
  (check-let-rel x)
  (destructuring-bind ((var val) body) x
    (make-relation-let
      :var var
      :val (sexpr->expr val)
      :body (sexpr->rel body))))

(defun check-let-rel (x)
  (unless (and (consp x) (= 2 (length x))
	       (consp (first x)) (= 2 (length (first x)))
	       (symbolp (first (first x))))
    (error "Invalid let relation ~W." (cons :let x))))

(defun sexpr->stoch-rel (x)
  (check-stoch-rel x)
  (make-relation-stochastic
    :lhs (sexpr->rellhs (first x))
    :rhs (sexpr->distr (second x))))

(defun check-stoch-rel (x)
  (unless (and (consp x) (= 2 (length x)))
    (error "Invalid stochastic relation ~W." (cons '~ x))))

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
      (otherwise (errfct)))))

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
    ((const name)
     (expr-const->arr-slice-idx errfct name))
    ((apply fct args)
     (expr-apply->arr-slice-idx errfct fct args))
    (otherwise (funcall errfct))))

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
        (shadow (remove-duplicates (shadowing-vars mdl)))
        (bad-ndims (remove-duplicates (bad-num-dimensions mdl)))
	(premature-usage (used-before-declared mdl))
	(vars-in-dims (vars-used-in-dims mdl)))
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
    (unless (null premature-usage)
      (error-with-list "Variables used before declared" premature-usage))
    (unless (null vars-in-dims)
      (error-with-list "Model vars used to define array length" vars-in-dims))))

(defun error-with-list (format lst)
  (error (format nil (strcat format ": ~{~a~^, ~}") lst)))

(defun used-before-declared (mdl)
  (let ((seen nil)
	(undeclared nil))
    (dolist (d (model-args mdl))
      (match-adt1 (decl var typ) d
	 (setf undeclared
	       (undeclared-vars seen (vars-in-type typ) undeclared))
	 (push var seen)))
    (dolist (e (model-reqs mdl))
      (setf undeclared (undeclared-vars seen (free-vars-in-expr e) undeclared)))
    (dolist (d (model-vars mdl))
      (match-adt1 (decl var typ) d
	(setf undeclared
	      (undeclared-vars seen (vars-in-type typ) undeclared))
	(push var seen)))
    (reverse undeclared)))

(defun undeclared-vars (seen vars undeclared)
  (dolist (v vars)
    (when (not (member v seen))
      (pushnew v undeclared)))
  undeclared)

(defun vars-in-type (typ)
  (adt-case vtype typ
    ((scalar stype) '())
    ((array elem-type dims)
     (append-mapcar #'free-vars-in-expr dims))))

(defun vars-used-in-dims (mdl)
  (let* ((vars (mapcar #'decl-var (model-vars mdl)))
	 (typs (mapcar #'decl-typ (model-vars mdl)))
	 (used (append-mapcar #'vars-in-type typs))
	 (result nil))
    (dolist (v vars)
      (when (member v used)
	(pushnew v result)))
    (reverse result)))

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
	(defined-vars (append-mapcar #'rel-vars (model-body mdl)))
	(bad nil))
    (dolist (x defined-vars)
      (if (not (member x declared-vars))
	(pushnew x bad)))
    bad))

(defun rel-vars (rel)
  (adt-case relation rel
    ((stochastic lhs rhs)
     (lhs-var lhs))
    ((block members)
     (append-mapcar #'rel-vars members))
    ((if condition true-branch false-branch)
     (append (rel-vars true-branch) (rel-vars false-branch)))
    ((loop var lo hi body)
     (rel-vars body))
    ((let var val body)
     (rel-vars body))
    ((skip)
     '())))

(defun lhs-var (lhs)
  (adt-case rellhs lhs
    ((simple var) (list var))
    ((array-elt var indices) (list var))
    ((array-slice var indices) (list var))))

(defun shadowing-vars (mdl)
  (let ((names (args-vars-names mdl)))
    (append (shadowing-vars-decls (model-args mdl) names)
	    (shadowing-vars-decls (model-vars mdl) names)
	    (shadowing-vars-exprs (model-reqs mdl) names)
	    (shadowing-vars-rels (model-body mdl) names))))

(defun shadowing-vars-decls (decls names)
  (append-mapcar (lambda (d) (shadowing-vars-decl d names)) decls))

(defun shadowing-vars-decl (d names)
  (shadowing-vars-vtype (decl-typ d) names))

(defun shadowing-vars-vtype (typ names)
  (adt-case vtype typ
    ((scalar stype)
     '())
    ((array elem-type dims)
     (shadowing-vars-exprs dims names))))

(defun shadowing-vars-rels (rels names)
  (append-mapcar (lambda (r) (shadowing-vars-rel r names)) rels))

(defun shadowing-vars-rel (r names)
  (adt-case relation r
    ((stochastic lhs rhs)
     (shadowing-vars-exprs
       (cons (rellhs->expr lhs) (distribution-args rhs)) names))
    ((block members)
     (shadowing-vars-rels members names))
    ((if condition true-branch false-branch)
     (append (shadowing-vars-expr condition names)
	     (shadowing-vars-rel true-branch names)
	     (shadowing-vars-rel false-branch names)))
    ((loop var lo hi body)
     (shadowing-vars-loop var lo hi body names))
    ((let var val body)
     (shadowing-vars-let-rel var val body names))
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
     (make-expr-apply :fct '@-idx :args (list value)))
    ((range lo hi)
     (make-expr-apply :fct '@-rng :args (list lo hi)))
    ((all)
     (make-expr-const :name '@-all))))

(defun shadowing-vars-loop (var lo hi body names)
  (let ((maybe-var '())
	(new-names names))
    (if (member var names)
      (push var maybe-var)
      (push var new-names))
    (append (shadowing-vars-expr lo names)
	    (shadowing-vars-expr hi names)
	    maybe-var
	    (shadowing-vars-rel body new-names))))

(defun shadowing-vars-let-rel (var val body names)
  (let ((maybe-var '())
	(new-names names))
    (if (member var names)
      (push var maybe-var)
      (push var new-names))
    (append (shadowing-vars-expr val names)
	    maybe-var
	    (shadowing-vars-rel body new-names))))

(defun shadowing-vars-exprs (exprs names)
  (append-mapcar (lambda (x) (shadowing-vars-expr x names)) exprs))

(defun shadowing-vars-expr (x names)
  (adt-case expr x
    ((apply fct args)
     (shadowing-vars-exprs args names))
    ((lambda var body)
     (if (member var names)
       (cons var (shadowing-vars-expr body names))
       (shadowing-vars-expr body (cons var names))))
    (t '())))

(defparameter *ndim-map* nil)

(defun bad-num-dimensions (mdl)
  (let* ((decls (append (model-args mdl) (model-vars mdl)))
	 (*ndim-map* (decls->ndim-assoc decls)))
    (append (bad-numdims-vtypes (mapcar #'decl-typ decls))
	    (bad-numdims-exprs (model-reqs mdl))
	    (bad-numdims-rels (model-body mdl)))))

(defun decls->ndim-assoc (decls)
  (let ((map nil))
    (dolist (x decls)
      (setf map (acons (decl-var x) (num-dims-vtype (decl-typ x)) map)))
    map))

(defun num-dims-vtype (typ)
  (adt-case vtype typ
    ((scalar stype) 0)
    ((array elem-type dims) (length dims))))

(defun bad-numdims-exprs (elist)
  (append-mapcar #'bad-numdims-expr elist))

(defun bad-numdims-vtypes (tlist)
  (append-mapcar #'bad-numdims-vtype tlist))

(defun bad-numdims-rels (rlist)
  (append-mapcar #'bad-numdims-rel rlist))

(defun bad-numdims-vtype (typ)
  (adt-case vtype typ
    ((scalar stype) '())
    ((array elem-type dims) (bad-numdims-exprs dims))))

(defun bad-numdims-rel (r)
  (adt-case relation r
    ((stochastic lhs rhs)
     (bad-numdims-exprs
       (cons (rellhs->expr lhs) (distribution-args rhs))))
    ((block members)
     (bad-numdims-rels members))
    ((if condition true-branch false-branch)
     (append (bad-numdims-expr condition)
	     (bad-numdims-rel true-branch)
	     (bad-numdims-rel false-branch)))
    ((loop var lo hi body)
     (append (bad-numdims-exprs (list lo hi))
	     (bad-numdims-rel body)))
    ((let var val body)
     (append (bad-numdims-expr val)
	     (bad-numdims-rel body)))
    ((skip)
     '())))

(defun bad-numdims-expr (x)
  (adt-case expr x
    ((apply fct args)
     (bad-numdims-apply fct args))
    ((lambda var body)
     (bad-numdims-expr body))
    (t '())))

(defun bad-numdims-apply (fct args)
  (let ((bad-numdims-args (bad-numdims-exprs args)))
    (if (member fct '(@ @-slice))
      (let* ((arr-var (expr-variable-symbol (first args)))
	     (n (cdr (assoc arr-var *ndim-map*))))
	(if (or (null n) (= (length args) (1+ n)))
	   bad-numdims-args
	   (cons arr-var bad-numdims-args)))
      bad-numdims-args)))

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
    ((stochastic lhs rhs)
     (pp-rel-stochastic lhs rhs))
    ((block members)
     (pp-rel-block members))
    ((if condition true-branch false-branch)
     (pp-rel-if condition true-branch false-branch))
    ((loop var lo hi body)
     (pp-rel-loop var lo hi body))
    ((let var val body)
     (pp-rel-let var val body))
    ((skip))))

(defun pp-rel-let (var val body)
  (fmt "let ~a = ~a in" (symbol-name var) (expr->string val))
  (indent
    (pp-rel body)))

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
