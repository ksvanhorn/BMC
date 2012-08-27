(defpackage :print
  (:use :common-lisp :expr :utils)
  (:export :expr->string :with-print-options
	   :default-is-binop :default-fct-name :default-quant-format))
(in-package :print)

(defun expr->string (e &optional (lprec -1) (rprec -1))
  (case (expr-class e)
	(:literal-num (write-to-string e))
	(:variable (symbol-name e))
	(:quant (qexpr->string e))
	(:array-app (@expr->string e))
	(:funct-app (fexpr->string e lprec rprec))))

(defun qexpr->string (e)
  (let* ((op (fct-name (quant-op e)))
	 (var (symbol-name (quant-var e)))
	 (bnds (quant-bounds e))
	 (body (expr->string (quant-body e)))
	 (lo (expr->string (qbnds-lo bnds)))
	 (hi (expr->string (qbnds-hi bnds))))
    (quant-format op var lo hi body)))

(defun @expr->string (e)
  (format nil "~a[~{~a~^, ~}]"
	 (expr->string (array-op e))
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
		(fct-name op) (mapcar #'expr->string args)))))

(defun bexpr->string (op args lprec rprec)
  (let* ((op-prec (precedences op))
	 (op-lprec (car op-prec))
	 (op-rprec (cdr op-prec))
	 (use-parens (or (< op-lprec lprec) (< op-rprec rprec)))
	 (len (length args))
	 (len1 (if (< len 2)
		   (error "Binary operator must have at least two args")
		   (1- len)))
	 (lprec-list (cons (if use-parens -1 lprec)
			   (make-list len1 :initial-element op-rprec)))
	 (rprec-list (append (make-list len1 :initial-element op-lprec)
			     (list (if use-parens -1 rprec))))
	 (aplist (zip lprec-list args rprec-list)))
    (with-output-to-string (s)
      (when use-parens (princ #\( s))
      (destructuring-bind (lpr e rpr) (car aplist)
	 (format s "~a" (expr->string e lpr rpr)))
      (dolist (x (cdr aplist))
	(destructuring-bind (lpr e rpr) x
	   (format s " ~a ~a" (fct-name op) (expr->string e lpr rpr))))
      (when use-parens (princ #\) s)))))

(defun precedences (op) (assoc-lookup op *precedences*))

(defparameter *precedences*
  '((:<  5 . 5) (:<= 5 . 5) (:= 5 . 5) (:!= 5 . 5) (:> 5 . 5) (:>= 5 . 5)
    (:+ 10 . 11) (:- 10 . 11) (:* 20 . 21) (:/ 20 . 21) (:^ 31 . 30)))

(defun is-binop (x) (funcall *is-binop* x))
(defun default-is-binop (x) (assoc x *precedences*))
(defparameter *is-binop* #'default-is-binop)

(defun fct-name (x) (funcall *fct-name* x))
(defun default-fct-name (x) (symbol-name x))
(defparameter *fct-name* #'default-fct-name)

(defun quant-format (op-s var-s lo-s hi-s body-s)
  (funcall *quant-format* op-s var-s lo-s hi-s body-s))
(defun default-quant-format (op-s var-s lo-s hi-s body-s)
  (format nil "~a(~a, ~a : ~a, ~a)" op-s var-s lo-s hi-s body-s))
(defparameter *quant-format* #'default-quant-format)

(defmacro with-print-options (is-binop-kw is-binop-fct
			      fct-name-kw fct-name-fct
                              quant-format-kw quant-format
			      &rest body)
  (unless (and (eq :is-binop is-binop-kw)
	       (eq :fct-name fct-name-kw)
	       (eq :quant-format quant-format-kw))
    (error "with-print-options macro: bad argument list"))
  `(let ((*is-binop* ,is-binop-fct)
 	 (*fct-name* ,fct-name-fct)
	 (*quant-format* ,quant-format))
     ,@body))
