(load "packages")
(load "symbols")
(load "adt")
(load "utils")
(load "expr")

(defpackage :foo (:use :cl :symbols :adt :utils :expr))
(in-package :foo)

(defun expr-dim (e)
  (adt-case expr e
    ((variable symbol)
     (var-dim symbol))
    ((apply fct args)
     (apply-dim fct args))
    ((const name)
     (cond
      ((numberp name) '())
      ((symbolp name) (var-dim name))
      (t (error "const name is neither number nor symbol"))))
    ((lambda var body)
     (error "Unimplemented"))))

(defun apply-dim (fct-name args)
  (case fct-name
    ('o^2 (let ((d (expr-dim (first args)))) (append d d)))
    ('$* (expr-dim (second args)))
    ('- '())
    ('@- (expr-dim (first args)))
    ('@ '())
    ('@-idx '())
    ('@-rng (list (expr-call '+
		    (expr-const 1)
		    (expr-call '- (second args) (first args)))))
    ('@-slice (apply #'append
		(mapcar #'slice-expr-dim
		  (var-dim (expr-variable-symbol (first args)))
		  (rest args))))
    (otherwise (error "Unimplemented case in apply-dim."))))

(defun slice-expr-dim (n x)
  (if (equalp x (expr-const '@-all))
      (list n)
    (expr-dim x)))

(defun var-dim (s)
  (case s
    ('x '(nresp nvars))
    ('r '())
    ('mu_y_form '())
    ('mu_x_form '(- nvars 1))
    ('nvars '())))

(dolist (x '((o^2 (@ x r (:range 2 nvars)))
	     ($* (- (@ x r 1) mu_y_form)
                              (@ x r (:range 2 nvars)))
	     (@ x r (:range 2 nvars))
	     (o^2 (@- (@ x r (:range 2 nvars))
                               mu_x_form))
	     (@ x r :all)))
  (format t "~a: ~a~%" x (expr-dim (sexpr->expr x))))
