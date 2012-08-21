(defpackage :compile
  (:use :common-lisp :model :print :utils)
  (:export :compile-to-csharp))
(in-package :compile)

; Main function

(defun compile-to-csharp (csharp-name-space class-name mdl os)
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

#|
(defun print-expr-c# (e &optional (prec -1))
  (case (expr-class e)
	(:literal-num (write-to-string e))
	(:variable (symbol-name e))
	(:array-app (print-array-app-expr-c# e))
	(:funct-app
	 (let ((oper (op e))
	        (a (args e)))
	   (cond ((quantifierp oper) (print-quantifier-expr-c# oper a))
		 ((is-binop oper) (print-binop-expr-c# e prec))
		 (t (print-funct-app-expr-c# oper a)))))))

(defun is-binop (oper)
  (and (binopp oper) (not (member oper *excluded-binops*))))

(defun print-array-app-expr-c# (e)
  (format nil "~a[~{~a~^, ~}]"
	 (print-expr-c# (array-op e))
	 (mapcar #'print-index-expr-c# (array-args e))))

(defun print-funct-app-expr-c# (oper a)
  (format nil "~a(~{~a~^, ~})"
	  (name-c# oper)
	  (mapcar #'print-expr-c# a)))

(defun print-binop-expr-c# (e context-prec)
  (let* ((oper (op e))
	 (prec (precedence oper))
	 (oper-s (name-c# oper))
	 (a (mapcar (lambda (x) (print-expr-c# x prec)) (args e))))
    (with-output-to-string (s)
       (if (> context-prec prec) (write-char #\Space s))
       (format s "~a" (car a))
       (dolist (x (cdr a)) (format s " ~a ~a" oper-s x))
       (if (> context-prec prec) (write-char #\) s)))))

(defun print-index-expr-c# (e)
  (case (index-class e)
	('all "")
	('range (format nil "~a : ~a" (range-lo e) (range-hi e)))
	('index (print-expr-c# e))))

(defun print-quantifier-expr-c# (oper a)
  (let ((opname (name-c# oper))
	(var (symbol-name (first a)))
	(lo (print-expr-c# (dec-expr (bounds-lo (second a)))))
	(hi (print-expr-c# (bounds-hi (second a))))
	(body (print-expr-c# (third a))))
    (format nil "~a(~a, ~a, ~a => ~a)" opname lo hi var body)))

(defun dec-expr (x)
  (if (numberp x) (1- x) `(- ,x 1)))

|#
