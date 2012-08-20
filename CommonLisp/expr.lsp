(defpackage :expr
  (:use :common-lisp :utils)
  (:export
    :expr-class
    :fct-op :fct-args
    :quant-op :quant-var :quant-bounds :quant-body
    :array-op :array-args :index-class :range-lo :range-hi))
(in-package :expr)

(defun expr-class (expr)
  (cond ((numberp expr) :literal-num)
	((symbolp expr) :variable)
	((is-qexpr expr) :quant)
	((is-aexpr expr) :array-app)
	((could-be-fexpr expr) :funct-app))) ; must come at end!

(defun could-be-fexpr (e) (and (listp e) (symbolp (first e))))
(defun fct-op (fexpr) (first fexpr))
(defun fct-args (fexpr) (rest fexpr))

(defun is-aexpr (e) (and (listp e) (eq :@ (first e)))) 
(defun array-op (aexpr) (second aexpr))
(defun array-args (aexpr) (nthcdr 2 aexpr))

(defun is-qexpr (x)
  (and (listp x) (member (first x) '(:qsum :qprod :qand :qor))))
(defun quant-op (x) (first x))
(defun quant-var (x) (second x))
(defun quant-bounds (x) (third x))
(defun quant-body (x) (fourth x))

(defun index-class (e)
  (cond
   ((eq :all e) :all)
   ((is-range e) :range)
   (t :index)))

(defun is-range (iexpr) (and (consp iexpr) (eq :range (first iexpr))))
(defun range-lo (rexpr) (second rexpr))
(defun range-hi (rexpr) (third rexpr))
