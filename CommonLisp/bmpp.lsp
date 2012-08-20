#!/usr/bin/clisp

(load "utils")
(load "expr")
(load "model")

(defpackage :bmpp
  (:use :common-lisp :model :expr :utils))
(in-package :bmpp)

(defmacro pp-hdr-block (header &rest body)
  `(progn
     (fmt "~a {" ,header)
     (indent ,@body)
     (fmt "}")))

(defun main ()
  (assert (= 2 (length ext:*args*)))
  (let ((ifname (first ext:*args*))
	(ofname (second ext:*args*))
	(mdl))
    (assert (stringp ifname))
    (assert (stringp ofname))
    (setf mdl (read-model ifname))
    (with-open-file (ostrm ofname :direction :output)
      (with-indent-amt 2
        (pretty-print-model mdl ostrm)))))

(defun pretty-print-model (mdl ostrm)
  (with-fmt-out ostrm
     (pp-hdr-block "args"
       (dolist (a (extract-args mdl))
	 (pretty-print-decl a)))
     (pp-hdr-block "reqs"
       (dolist (r (extract-reqs mdl))
	 (pretty-print-expr r)))
     (pp-hdr-block "vars"
       (dolist (v (extract-vars mdl))
	 (pretty-print-decl v)))
     (pp-hdr-block "model"
       (dolist (rel (extract-body mdl))
	 (pretty-print-rel rel)))))

(defun pretty-print-decl (decl)
  (fmt "~a : ~a"
       (symbol-name (decl-var decl))
       (type->string (decl-typ decl))))

(defun type->string (typ)
  (case (type-class typ)
	(:scalar (scalar-type-name typ))
	(:array (format nil "~a[~{~a~^, ~}]"
			(scalar-type-name (elem-type typ))
			(mapcar #'expr->string (type-dims typ))))))

(defun scalar-type-name (typ)
  (unless (and (symbolp typ) (not (null typ)))
    (error (format nil "Invalid scalar type: ~a" typ)))
  (string-downcase typ))

(defun pretty-print-expr (expr)
  (fmt "~a" (expr->string expr)))

(defun pretty-print-rel (rel)
  (case (rel-class rel)
	(:deterministic (pp-rel-deterministic rel))
	(:stochastic (pp-rel-stochastic rel))
	(:block (pretty-print-block rel))
	(:if-then (pretty-print-if-then rel))
	(:if-then-else (pretty-print-if-then-else rel))
	(:loop (pretty-print-loop rel))))

(defun pp-rel-deterministic (rel)
  (fmt "~a <- ~a" (expr->string (rel-var rel)) (expr->string (rel-val rel))))

(defun pp-rel-stochastic (rel)
  (fmt "~a ~~ ~a" (expr->string (rel-var rel)) (expr->string (rel-distr rel))))

(defun pretty-print-block (rel)
  (mapc #'pretty-print-rel (rel-block-body rel)))

(defun pretty-print-if-then (rel)
  (pp-hdr-block
    (format nil "if (~a)" (expr->string (rel-if-condition rel)))
    (pretty-print-rel (rel-true-branch rel))))

(defun pretty-print-if-then-else (rel)
  (pretty-print-if-then rel)
  (pp-hdr-block "else"
    (pretty-print-rel (rel-false-branch rel))))

(defun pretty-print-loop (rel)
  (let* ((bounds (rel-loop-bounds rel))
	 (var-name (symbol-name (rel-loop-var rel)))
	 (lo (expr->string (bounds-lo bounds)))
	 (hi (expr->string (bounds-hi bounds))))
    (pp-hdr-block
      (format nil "for (~a in ~a : ~a)" var-name lo hi)
      (pretty-print-rel (rel-loop-body rel)))))

(defun expr->string (e &optional (lprec -1) (rprec -1))
  (case (expr-class e)
	(:literal-num (write-to-string e))
	(:variable (symbol-name e))
	(:quant (qexpr->string e))
	(:array-app (@expr->string e))
	(:funct-app (fexpr->string e lprec rprec))))

(defun qexpr->string (e)
  (let* ((op (symbol-name (quant-op e)))
	 (var (symbol-name (quant-var e)))
	 (bounds (quant-bounds e))
	 (body (expr->string (quant-body e)))
	 (lo (expr->string (bounds-lo bounds)))
	 (hi (expr->string (bounds-hi bounds))))
    (format nil "~a(~a, ~a : ~a, ~a)" op var lo hi body)))

(defun @expr->string (e)
  (format nil "~a[~{~a~^, ~}]"
	 (symbol-name (array-op e))
	 (mapcar #'iexpr->string (array-args e))))

(defun iexpr->string (e)
  (case (index-class e)
	(:all "")
	(:range (format nil "~a : ~a" (range-lo e) (range-hi e)))
	(:index (expr->string e))))

(defun fexpr->string (e lprec rprec)
  (let ((op (fct-op e))
	(args (fct-args e)))
    (if (is-binop op)
	(bexpr->string op args lprec rprec)
        (format nil "~a(~{~a~^, ~})"
		(symbol-name op) (mapcar #'expr->string args)))))

(defun bexpr->string (op args lprec rprec)
  (let* ((op-prec (precedences op))
	 (op-lprec (car op-prec))
	 (op-rprec (cdr op-prec))
	 (left (first args))
	 (right (second args))
	 (use-parens (or (< op-lprec lprec) (< op-rprec rprec))))
    (format nil "~a~a ~a ~a~a"
	    (if use-parens "(" "")
	    (expr->string left lprec op-lprec)
	    op
	    (expr->string right op-rprec rprec)
	    (if use-parens ")" ""))))

#|
(defun expand-binop-expr (op args)
  (unless (<= 2 args) (error "Binary operator must have at least two args"))
|#

(defun is-binop (x)
  (assoc x *precedences*))

(defun precedences (op) (assoc-lookup op *precedences*))

(defparameter *precedences*
  '((:<  5 . 5) (:<= 5 . 5) (:= 5 . 5) (:!= 5 . 5) (:> 5 . 5) (:>= 5 . 5)
    (:+ 10 . 11) (:- 10 . 11) (:* 20 . 21) (:/ 20 . 21) (:^ 31 . 30)))

; Begin test
(defun show-case (expr)
  (format t "~w~%" expr)
  (format t "~a~%~%" (expr->string expr)))

(defparameter *test-ops* '(:+ :- :* :/ :^))

(defun random-expr (depth)
  (if (zerop depth)
      (random 10)
    (let* ((n (random (length *test-ops*)))
	   (op (nth n *test-ops*)))
      (list op
	    (random-expr (1- depth))
	    (random-expr (1- depth))
	    (random-expr (1- depth))))))

(dotimes (i 20) (show-case (random-expr 4)))

; End test

(main)
