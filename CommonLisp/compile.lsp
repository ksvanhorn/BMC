(defpackage :compile
  (:use :common-lisp :model :print :utils)
  (:export :compile-to-csharp))
(in-package :compile)

; Main function

(defun compile-to-csharp (csharp-name-space class-name mdl os)
  (destructuring-bind (axioms . norm-mdl) (normalize-model mdl)
    nil)
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
	  (make-stoch-rel nvar distr)))))

(defun make-stoch-rel (var distr) (list :~ var distr))

(defun normalize-block-rel (rel)
  (destructuring-bind (axioms . rels) (normalize-rels (rel-block-body rel))
    (cons axioms (make-block-rel rels))))

(defun make-block-rel (rels) (cons :block rels))

(defun normalize-if-then-rel (rel)
  (destructuring-bind (test-axioms . ntest)
		      (normalize-expr (rel-if-condition rel))
  (destructuring-bind (then-axioms . nthen)
		      (normalize-rel (rel-true-branch))
    (cons (append test-axioms then-axioms)
	  (make-if-then-rel ntest nthen)))))

(defun make-if-then-rel (test then) (list :if test then))

(defun normalize-if-then-else-rel (rel)
  (destructuring-bind (test-axioms . ntest)
		      (normalize-expr (rel-if-condition rel))
  (destructuring-bind (then-axioms . nthen)
		      (normalize-rel (rel-true-branch))
  (destructuring-bind (else-axioms . nelse)
		      (normalize-rel (rel-false-branch))
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
  (let ((op (quant-op x))
	(var (quant-var x))
	(bnds (quant-bounds x))
	(body (quant-body x)))
  (*** HERE ***)))

(defun normalize-aexpr (x)
  (destructuring-bind (iaxioms . nidxs) (normalize-iexprs (array-args x))
  (destructuring-bind (aaxioms . narr) (normalize-expr (array-op x))
  (destructuring-bind (saxioms . sfct)
		      (array-slice-axioms (mapcar #'index-class (array-args x)))
    (cons (append iaxioms aaxioms saxioms)
	  (funcall sfct narr nidxs))))))

#| ; this is broken
(defun array-slice-axioms (index-classes)
  (if (every (lambda (x) (eq :index x)) index-classes)
    (cons '() #'make-arr-expr)
    (let ((sfct-name (slice-fct-name index-classes))
	  (axioms (make-slice-axioms sfct-name index-classes)))
      (cons axioms #'make-fct-expr))))

(defun slice-fct-name (index-classes)
  (flet ((ic-char (x) (case x (:all "a") (:range "r") (:index "i"))))
    (intern (apply #'strcat "@" (mapcar #'ic-char index-classes)) "KEYWORD")))

(defun make-slice-axioms (fct-name index-classes)
  (lambda (op args) (list 
|#

(defun normalize-iexprs (iexprs)
  (let ((x (mapcar #'normalize-iexpr iexprs)))
    (cons (apply #'append (mapcar #'car x)) (mapcar #'cdr x))))

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

(defun make-fct-expr (op args) (cons op args))
