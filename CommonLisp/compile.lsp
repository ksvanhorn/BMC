(in-package :compile)

;;; Main function

(defun write-csharp-class-skeleton (csharp-name-space class-name write-body)
  (fmt "using System;")
  (fmt "using Common;")
  (fmt "")
  (fmt "namespace ~a" csharp-name-space)
  (fmt "{")
  (indent
    (fmt "[Serializable]")
    (fmt "public class ~a" class-name)
    (fmt "{")
    (indent
      (funcall write-body))
    (fmt "}"))
  (fmt "}"))

#|
(defun compile-to-csharp (csharp-name-space class-name mdl os)
  (destructuring-bind (norm-axioms . norm-mdl) (normalize-model mdl)
    (let* ((assums0 (append (args-assums norm-mdl) norm-axioms *basic-axioms*))
	   (thms0 (var-lengths-stmts norm-mdl))
	   (failed (prove-thms-axs thms0 assums0)))
      (if (not (null failed))
	(error "The following could not be proven:~%~{  ~a~^~%~}" failed))))
  (with-fmt-out os
    (fmt "using System;")
    (fmt "using Common;")
    (fmt "")
    (fmt "namespace ~a" csharp-name-space)
    (fmt "{")
    (indent
      (fmt "[Serializable]")
      (fmt "public class ~a" class-name)
      (fmt "{")
      (indent
         (gen-class-body mdl))
      (fmt "}"))
    (fmt "}")))

(defun gen-class-body (mdl)
  (gen-args mdl)
  (fmt "")
  (gen-vars mdl)
  (fmt "")
  (gen-args-checks mdl)
)

(defun gen-args (mdl)
  (fmt *lcom-fmt* "Inputs")
  (dolist (x (args-base-decls mdl))
    (gen-decl x)))

(defun gen-vars (mdl)
  (fmt *lcom-fmt* "Model variables")
  (dolist (x (vars-base-decls mdl))
    (gen-decl x)))

(defparameter *lcom-fmt* "// ~a")

(defun gen-decl (decl)
  (destructuring-bind (var typ ndim) decl
    (fmt "public ~a ~a;" (type-string typ ndim) var)))

(defun type-string (typ ndim)
  (let ((elem-type-str (assoc-lookup typ *csharp-scalar-types*)))
    (if (zerop ndim)
	elem-type-str
	(format nil "Array~aD<~a>" ndim elem-type-str))))

(defparameter *csharp-scalar-types*
  '((:realxn . "double") (:integer . "int") (:boolean . "bool")))

(defun gen-args-checks (mdl)
  (let ((checks (args-checks mdl)))
    (fmt "public void Validate()")
    (fmt "{")
    (indent
      (dolist (x checks)
	(let ((bool-expr (print-expr-c# x)))
	  (fmt "BMC.Check(~a," bool-expr)
	  (fmt "          \"~a\");" bool-expr))))
    (fmt "}")))

(defun print-expr-c# (x)
  (with-print-options
    :is-binop #'is-binop
    :fct-name #'fct-name
    :quant-format #'quant-format
    (expr->string x)))

(defun is-binop (op)
  (and (default-is-binop op) (not (member op *excluded-binops*))))

(defparameter *excluded-binops* '(=> ^))

(defun fct-name (op)
  (let ((a (assoc op *oper-names*)))
    (if (null a) (symbol-name op) (cdr a))))

(defun quant-format (op-str var-str lo-str hi-str body-str)
  (format nil "~a(~a, ~a, ~a => ~a)"
	  op-str lo-str hi-str var-str body-str))

(defparameter *oper-names*
  '((:array-length . "BMC.Length")
    (:qand . "BMC.ForAll")
    (:qsum . "BMC.Sum")
    (:=> . "BMC.Implies")
    (:= . "==")
    (:is-realp . "BMC.IsRealp")
    (:is-realnn . "BMC.IsRealnn")
    (:is-real . "BMC.IsReal")
    (:is-realx . "BMC.IsRealx")
    (:|is_symm_pd| . "BMC.IsSymmPD")))

(defun normalize-model (mdl)
  (destructuring-bind (args-ax . new-args)
		      (normalize-decls (extract-args mdl))
  (destructuring-bind (reqs-ax . new-reqs)
		      (normalize-exprs (extract-reqs mdl))
  (destructuring-bind (vars-ax . new-vars)
		      (normalize-decls (extract-vars mdl))
  (destructuring-bind (rels-ax . new-rels)
		      (normalize-rels (extract-body mdl))
  (let ((new-mdl (make-model new-args new-reqs new-vars new-rels))
	(axioms (remove-duplicates (append args-ax reqs-ax vars-ax rels-ax))))
    (cons axioms new-mdl)))))))

(defun make-model (args reqs vars rels)
  `(:model (:args ,@args) (:reqs ,@reqs) (:vars ,@vars) (:rels ,@rels)))

(defun normalize-decls (decls)
  (let* ((xdecls (mapcar #'normalize-decl decls))
	 (axioms (apply #'append (mapcar #'car xdecls)))
	 (ndecls (mapcar #'cdr xdecls)))
    (cons axioms ndecls)))

(defun normalize-decl (decl)
  (destructuring-bind (var-ax . new-var) (normalize-expr (decl-var decl))
  (destructuring-bind (typ-ax . new-typ) (normalize-type (decl-typ decl))
    (cons (append var-ax typ-ax)
	  (make-decl new-var new-typ)))))

(defun make-decl (var typ) (list var typ))

(defun normalize-type (typ)
  (case (type-class typ)
	(:scalar (cons '() typ))
	(:array
	  (let ((etyp (elem-type typ))
		(dims (type-dims typ)))
          (destructuring-bind (dims-ax . new-dims) (normalize-exprs dims)
	    (cons dims-ax (make-array-type etyp new-dims)))))))

(defun make-array-type (etyp dims) (cons etyp dims))

(defun normalize-rels (rels)
  (let* ((xrels (mapcar #'normalize-rel rels))
	 (axioms (apply #'append (mapcar #'car xrels)))
	 (nrels (mapcar #'cdr xrels)))
    (cons axioms nrels)))

(defun normalize-rel (rel)
  (case (rel-class rel)
	(:deterministic (normalize-determ-rel rel))
	(:stochastic (normalize-stoch-rel rel))
	(:block (normalize-block-rel rel))
	(:if-then (normalize-if-then-rel rel))
	(:if-then-else (normalize-if-then-else-rel rel))
	(:loop (normalize-loop-rel rel))))

(defun normalize-determ-rel (rel) 
  (destructuring-bind (var-ax . nvar) (normalize-expr (rel-var rel))
  (destructuring-bind (val-ax . nval) (normalize-expr (rel-val rel))
    (cons (append var-ax val-ax)
	  (make-determ-rel nvar nval)))))

(defun make-determ-rel (var val) (list :<- var val))

(defun normalize-stoch-rel (rel)
  (destructuring-bind (var-ax . nvar) (normalize-expr (rel-var rel))
  (destructuring-bind (distr-ax . ndistr) (normalize-expr (rel-distr rel))
    (cons (append var-ax distr-ax)
	  (make-stoch-rel nvar ndistr)))))

(defun make-stoch-rel (var distr) (list :~ var distr))

(defun normalize-block-rel (rel)
  (destructuring-bind (axioms . rels) (normalize-rels (rel-block-body rel))
    (cons axioms (make-block-rel rels))))

(defun make-block-rel (rels) (cons :block rels))

(defun normalize-if-then-rel (rel)
  (destructuring-bind (test-axioms . ntest)
		      (normalize-expr (rel-if-condition rel))
  (destructuring-bind (then-axioms . nthen)
		      (normalize-rel (rel-true-branch rel))
    (cons (append test-axioms then-axioms)
	  (make-if-then-rel ntest nthen)))))

(defun make-if-then-rel (test then) (list :if test then))

(defun normalize-if-then-else-rel (rel)
  (destructuring-bind (test-axioms . ntest)
		      (normalize-expr (rel-if-condition rel))
  (destructuring-bind (then-axioms . nthen)
		      (normalize-rel (rel-true-branch rel))
  (destructuring-bind (else-axioms . nelse)
		      (normalize-rel (rel-false-branch rel))
    (cons (append test-axioms then-axioms else-axioms)
	  (make-if-then-else-rel ntest nthen nelse))))))

(defun make-if-then-else-rel (test then else) (list :if test then else))

(defun normalize-loop-rel (rel)
  (let ((bnds (rel-loop-bounds rel)))
  (destructuring-bind (lo-axioms . nlo) (normalize-expr (bounds-lo bnds))
  (destructuring-bind (hi-axioms . nhi) (normalize-expr (bounds-hi bnds))
  (destructuring-bind (body-axioms . nbody) (normalize-rel (rel-loop-body rel))
    (cons (append lo-axioms hi-axioms body-axioms)
	  (make-loop-rel (rel-loop-var rel) nlo nhi nbody)))))))

(defun make-loop-rel (var lo hi body) `(:for ,var (,lo ,hi) ,body))

(defun normalize-exprs (exprs)
  (let* ((xexprs (mapcar #'normalize-expr exprs))
	 (axioms (apply #'append (mapcar #'car xexprs)))
	 (nexprs (mapcar #'cdr xexprs)))
    (cons axioms nexprs)))

(defun normalize-expr (x)
  (case (expr-class x)
	(:literal-num (cons '() x))
	(:variable (cons '() x))
	(:quant (normalize-qexpr x))
	(:array-app (normalize-aexpr x))
	(:funct-app (normalize-fexpr x))))

(defun normalize-qexpr (x)
  (if (eq :qand (quant-op x))
    (cons '() (normalize-qand x))
    (error "Not implemented: normalize-qexpr")))

(defun logical-var (x)
  (intern (strcat "?" (symbol-name x)) "KEYWORD"))

(defun normalize-qand (x)
  (let* ((v0 (quant-var x))
	 (v (logical-var v0))
	 (bnds (quant-bounds x))
	 (lo (qbnds-lo bnds))
	 (hi (qbnds-hi bnds))
	 (body0 (quant-body x))
	 (body (subst v v0 body0)))
    `(:|all| (,v) (:=> (:|and| (:|is-integer| ,v)
		               (:<= ,lo ,v) (:<= ,v ,hi))
		       ,body))))

(defun normalize-aexpr (x)
  (destructuring-bind (iaxioms . nargs) (normalize-iexprs (array-args x))
  (destructuring-bind (aaxioms . nop) (normalize-expr (array-op x))
    (if (every (lambda (a) (eq :index (index-class a))) nargs)
      (cons (append iaxioms aaxioms) (make-array-expr nop nargs))
      (let* ((saxioms (array-slice-axioms (mapcar #'index-class nargs)))
	     (axioms (append iaxioms aaxioms saxioms))
	     (sargs (mapcar #'iexpr->expr nargs)))
	(cons axioms (make-fct-expr :|@-slice| (cons nop sargs))))))))

(defun make-array-expr (op args) (list* :@ op args))

(defun array-slice-axioms (index-classes)
  (assoc-lookup (symbolize index-classes) *asa-map*))

(defun symbolize (index-classes)
  (intern (format nil "~{~a~^-~}" index-classes) "KEYWORD"))

(defparameter *asa-map-template*
   '((:index-all
       (all (?a ?i)
         (=> (and
                (= (num-dims ?a) 2) (is-integerp ?i)
                (<= ?i (array-length 1 ?a)))
              (and
                (= (num-dims *ia-slice*) 1)
                (= (array-length 1 *ia-slice*) (array-length 2 ?a))
                (all (?j)
                  (=> 
                    (and (is-integerp ?j) (<= ?j (array-length 2 ?a)))
                    (= (@ *ia-slice* ?j) (@ ?a ?i ?j))))))))
    (:index-range
       (all (?a ?i ?j1 ?j2)
         (=> (and
                (= (num-dims ?a) 2)
                (is-integerp ?i) (<= ?i (array-length 1 ?a))
                (is-integerp ?j1) (<= ?j1 (+ 1 ?j2))
                (is-integerp ?j2) (<= ?j2 (array-length 2 ?a)))
              (and
                (= (num-dims *ir-slice* 1))
                (= (array-length 1 *ir-slice*) (+ 1 (- ?j2 ?j1)))
                (all (?j)
                  (=>
                    (and (is-integerp ?j) (<= ?j (+ 1 (- ?j2 ?j1))))
                    (= (@ *ir-slice* ?j)
                        (@ ?a ?i (+ ?j (- ?j1 1))))))))))))

(defparameter *asa-map*
  (let ((ia-slice '(@-slice ?a ?i @-all))
	(ir-slice '(@-slice ?a ?i (list ?j1 ?j2))))
    (subst ir-slice '*ir-slice*
      (subst ia-slice '*ia-slice* *asa-map-template*))))

(defun iexpr->expr (iexpr)
  (case (index-class iexpr)
	(:range (make-fct-expr :list (list (range-lo iexpr) (range-hi iexpr))))
	(:index iexpr)
	(:all :@-all)))

(defun normalize-iexprs (iexprs)
  (let ((x (mapcar #'normalize-iexpr iexprs)))
    (cons (apply #'append (mapcar #'car x))
	  (mapcar #'cdr x))))

(defun normalize-iexpr (ie)
  (case (index-class ie)
	(:all
	  (cons '() ie))
	(:range
	  (destructuring-bind (lo-axioms . lo) (normalize-expr (range-lo ie))
	  (destructuring-bind (hi-axioms . hi) (normalize-expr (range-hi ie))
            (cons (append lo-axioms hi-axioms) (make-index-range lo hi)))))
	(:index (normalize-expr ie))))

(defun make-index-range (lo hi) (list :range lo hi))

(defun normalize-fexpr (x)
  (destructuring-bind (axioms . nargs) (normalize-exprs (fct-args x))
    (cons axioms (make-fct-expr (fct-op x) nargs))))

(defun make-fct-expr (fct args) (cons fct args))

(defun var-lengths-stmts (mdl)
  (let ((exprs (flatten (mapcar #'length-exprs (extract-vars mdl)))))
    (remove-duplicates
      (mapcar (lambda (x) (list (type-predicate :integernn) x)) exprs)
      :test #'equal)))

(defun length-exprs (decl)
  (let ((typ (decl-typ decl)))
    (unless (eq :array (type-class typ))
      (return-from length-exprs '()))
    (type-dims typ)))	  

(defparameter *basic-axioms*
  (to-kw
    '((is-integer 0)
      (is-integer 1)
      (is-integer 2)
      (is-integer 3)
      (all (?x) (=> (and (is-integer ?x) (<= 0 ?x)) (is-integernn ?x)))
      (<= 0 3)
      (<= 0 2)
      (<= 0 1)
      (all (?x ?y ?z) (=> (and (is-integer ?x) (is-integer ?y) (is-integer ?z)
  			     (<= ?x ?y) (<= ?y ?z))
  			(<= ?x ?z)))
      (all (?x ?y) (=> (and (is-integer ?x) (is-integer ?y))
  		     (is-integer (- ?x ?y))))
      (is-integer 1)
      (all (?x ?y) (=> (and (is-integer ?x) (is-integer ?y)
  			  (<= ?y ?x))
  		     (<= 0 (- ?x ?y))))
      (<= 1 3))))
|#