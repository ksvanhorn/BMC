(defpackage :deduction
  (:use :cl :utils :paiprolog :alexandria :iterate)
  (:export :clear-db :<- :and :deduce :with-assumptions :failed))
(in-package :deduction)

(defun clear-db ()
  "Remove all clauses (for all predicates) from the data base."
  (dolist (pred paiprolog::*db-predicates*)
    (setf (get pred 'paiprolog::clauses) nil))
  (setf paiprolog::*uncompiled* nil)
  (setf paiprolog::*db-predicates* nil))

;;; <- consequent antecedent*
;;;   "add a clause to the database"
;;; provided by paiprolog

(defun nop (&rest args)
  (declare (ignore args))
  t)

(defun replace-var-symbols (var-symbols expr)
  (let ((vars '()))
    (dolist (s var-symbols)
      (let ((var (?)))
	(push var vars)
	(setf expr (subst var s expr))))
    (values expr (reverse vars))))

(defun prolog-fct (pred arity)
  (symbol-function (paiprolog::make-predicate pred arity)))

(defun check-var-symbols (var-symbols)
  (let ((bad-symbols (iter (for s in var-symbols)
			   (unless (starts-with #\? (symbol-name s))
			     (collect s)))))
    (when bad-symbols
      (error "The following variables do not begin with '?': ~{~a~^, ~}." bad-symbols))))

(defun deduce (var-symbols expr)
  (check-var-symbols var-symbols)
  (with-output-to-string (*error-output*) (paiprolog::prolog-compile-symbols))
  (setf (fill-pointer paiprolog::*trail*) 0)
  (setf paiprolog::*var-counter* 0)

  (multiple-value-bind (expr1 vars) (replace-var-symbols var-symbols expr)
    (destructuring-bind (pred . args) expr1
      (let* ((result 'failed)
	     (succeed (lambda (&rest args)
			(setf result (mapcar #'paiprolog::var-binding vars)))))
	(apply (prolog-fct pred (length args)) (append args (list succeed)))
	result))))

(defmacro with-assumptions (clauses &body body) nil)
