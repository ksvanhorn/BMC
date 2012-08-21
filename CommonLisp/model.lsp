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
 	 (mdl (read-file-upcasing-only uc ifname)))
;   (check-model mdl)
    (check-model-has-sections '(:args :reqs :vars :body) mdl)
    ;(check-elements-are-decls (extract-args mdl))
    ;(check-elements-are-exprs (extract-reqs mdl))
    ;(check-elements-are-decls (extract-vars mdl))
    ;(check-elements-are-rels (extract-rels mdl))
    mdl))

(defun check-model-has-sections (sections mdl)
  (unless (and (listp mdl) (eq :model (first mdl)))
    (error "Model should start with ':model'"))
  (let ((rest (cdr mdl)))
    (unless (and (listp rest) (= 4 (length rest)))
      (error "Model should have 4 sections"))
    (dotimes (i (length sections))
      (check-has-header (1+ i) (nth i sections) (nth i rest)))))

(defun check-has-header (n hdr x)
  (unless (and (listp x) (not (null x)) (eq (car x) hdr))
    (error (format nil "Section ~a of model should have header '~a'." n hdr))))

(defparameter *model-keywords*
  '(:model :args :reqs :vars :body :for :if :block
    :range :all
    :realp :realnn :real :realx :realxn :integerp :integernn :integer :boolean))

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





