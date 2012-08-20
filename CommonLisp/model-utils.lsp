(defpackage :model
  (:use :common-lisp :utils)
  (:export
    :read-model :extract-args :extract-reqs :extract-vars :extract-body
    ;:args-base-decls :vars-base-decls
    ;:expr-class :op :args :quantifierp :binopp
))
(in-package :model)


(defun print-decl (d)
  (let ((var-s (symbol-name (decl-var d)))
	(typ-s (print-type (decl-typ d))))
    (format nil "~a : ~a" var-s typ-s)))

(defun print-type (typ)
  (case (type-class typ)
	('scalar (symbol-name typ))
	('array (format nil "~a[~{~a~^, ~}]"
			(symbol-name (elem-type typ))
			(mapcar #'print-expr (type-dims typ))))))

(defun print-rel (indent rel)
  (let* ((stream (make-string-output-stream))
	 (*standard-output* stream))
    (print-rel1 indent rel)
    (get-output-stream-string stream)))

(defun output-sp (n)
  (dotimes (i n) (princ #\  )))

(defun print-if-common (indent rel)
  (output-sp indent)
  (format t "if (~a) {~%" (print-expr (rel-if-condition rel)))
  (print-rel1 (+ indent 2) (rel-true-branch rel))
  (output-sp indent)
  (format t "}~%"))

(defun print-rel1 (indent rel)
  (case (rel-class rel)

	('deterministic
	 (output-sp indent)
	 (format t "~a <- ~a~%"
		 (print-expr (rel-var rel)) 
		 (print-expr (rel-val rel))))

	('stochastic
	 (output-sp indent)
	 (format t "~a ~~ ~a~%"
		 (print-expr (rel-var rel))
		 (print-expr (rel-distr rel))))

	('block (mapc (lambda (r) (print-rel1 indent r))
		      (rel-block-body rel)))

	('if-then
	 (print-if-common indent rel))

	('if-then-else
	 (print-if-common indent rel)
	 (output-sp indent)
	 (format t "else {~%")
	 (print-rel1 (+ indent 2) (rel-false-branch rel))
	 (output-sp indent)
	 (format t "}~%"))

	('loop 
	 (output-sp indent)
	 (format t "for (~a in ~a : ~a) {~%"
		 (symbol-name (rel-loop-var rel))
		 (print-expr (bounds-lo (rel-loop-bounds rel)))
		 (print-expr (bounds-hi (rel-loop-bounds rel))))
	 (print-rel1 (+ indent 2) (rel-loop-body rel))
	 (output-sp indent)
	 (format t "}~%"))))

(defun model-string-case-xform (s)
  (let ((ostrm (make-string-output-stream))
	(istrm (make-string-input-stream s)))
    (model-case-xform istrm ostrm)
    (get-output-stream-string ostrm)))  

(defun model-case-xform (istrm ostrm)
  (do ((c (read-char istrm nil nil) (read-char istrm nil nil))
       (in-token nil)
       (in-identifier nil))
      ((null c))
      (cond
       ((member c '(#\( #\) #\Space #\Newline #\Tab #\Return #\Linefeed))
	(if in-identifier (write-char #\| ostrm))
	(setq in-token nil)
	(setq in-identifier nil))
       ((and (not in-token) (not in-identifier))
	(if (alpha-char-p c)
	    (progn
	      (write-char #\| ostrm)
	      (setq in-identifier t))
	    (setq in-token t))))
      (write-char c ostrm)))

(defun base-decl (decl)
  (let ((var (decl-var decl))
	(typ (decl-typ decl)))
    (case (type-class typ)
	  ('scalar (list var (base-scalar-type typ) 0))
	  ('array (list var
			(base-scalar-type (elem-type typ))
			(length (type-dims typ)))))))

(defun num-dims (base-array-typ)
  (second base-array-typ))

(defparameter *base-scalar-types*
  '((realxn . realxn) (realx . realxn) (real . realxn)
    (realnn . realxn) (realp . realxn)
    (integer . integer) (integernn . integer) (integerp . integer)
    (boolean . boolean)))

(defun scalar-types () (mapcar #'car *base-scalar-types*))

(defun base-scalar-type (typ)
  (cdr (assoc typ *base-scalar-types*)))

(defun args-base-decls (mdl)
  (mapcar #'base-decl (extract-args mdl)))

(defun vars-base-decls (mdl)
  (mapcar #'base-decl (extract-vars mdl)))

(defun args-checks (mdl)
  (append (flatten (mapcar #'decl-checks (extract-args mdl)))
	  (extract-reqs mdl)))

(defun flatten (lists) (apply #'append lists))

(defun decl-checks (decl)
  (let ((var (decl-var decl))
	(typ (decl-typ decl)))
    (case (type-class typ)
	  ('scalar (scalar-type-checks var typ))
	  ('array (array-type-checks var typ)))))

(defun scalar-type-checks (var typ)
  (let ((btyp (base-scalar-type typ)))
    (if (eq btyp typ)
	nil
        (list (simplify-check (list (concat-symbol 'is- typ) var))))))

(defun simplify-check (check-expr)
  (destructuring-bind (pred var) check-expr
     (cond ((eq 'is-integernn pred)
	    `(>= ,var 0))
	   ((eq 'is-integerp pred)
	    `(> ,var 0))
	   (t check-expr))))

(defun concat-symbol (&rest args)
  (intern (strcat args)))

(defun array-type-checks (var typ)
  (let* ((etyp (elem-type typ))
	 (dims (type-dims typ))
	 (idxvars (index-vars (length dims) `(list ,var ,@dims))))
    (append (array-length-checks var dims 1)
	    (array-element-checks var etyp dims idxvars idxvars))))

(defun array-length-checks (var dims n)
  (if (null dims)
      nil
      (cons `(= (array-length ,n ,var) ,(car dims))
	    (array-length-checks var (cdr dims) (1+ n)))))

(defun array-element-checks (var etyp dims idxvars all-idxvars)
  (if (null dims)
      (scalar-type-checks `(@ ,var ,@all-idxvars) etyp)
      (let ((iv (car idxvars))
	    (idxvars1 (cdr idxvars))
	    (dims1 (cdr dims)))
	(mapcar (lambda (x) `(QAND ,iv (1 ,(car dims)) ,x))
		(array-element-checks var etyp dims1 idxvars1 all-idxvars)))))

(defun map-range (lo hi fct)
  (do ((result nil (cons (funcall fct n) result))
       (n lo (1+ n)))
      ((> n hi) (reverse result))))

(defun index-vars (n expr &optional (pfx 'i))
  (do ((result nil)
       (k 1 (1+ k)))
      ((= n 0) (reverse result))
     (let ((idxvar (concat-symbol pfx k)))
       (when (fully-free-of idxvar expr)
	 (decf n)
	 (push idxvar result)))))

(defun fully-free-of (var expr)
  (cond
    ((symbolp expr) (not (eq var expr)))
    ((consp expr) (and (fully-free-of var (car expr))
		       (fully-free-of var (cdr expr))))
    (t t)))

(defun rel->density (rel)
  (case (rel-class rel)
    ('stochastic (density (rel-var rel) (rel-distr rel)))
    ('deterministic `(ind= ,(rel-var rel) ,(rel-val rel)))
    ('loop `(qprod! ,(rel-loop-var rel) ,(rel-loop-bounds rel)
		    ,(rel->density (rel-loop-body rel))))
    ('block (cons '*! (mapcar #'rel->density (rel-block-body rel))))
    ('if-then `(*! (ind ,(rel-if-condition rel))
		   ,(rel->density (rel-true-branch rel))))
))

(defun density (var distr)
  (unless (eq (expr-class distr) :funct-app)
    (error (format nil "Invalid distribution expression: ~a" distr)))
  (apply (density-fct (op distr)) (cons var (args distr))))

(defun density-fct (op)
  (cdr (assoc op *density-fcts*)))

(defparameter *density-fcts* nil)

(defmacro define-density (name args &rest body)
  `(push (cons ',name (lambda ,args ,@body)) *density-fcts*))

(define-density ddummy (x y)
  `(dummy ,x ,y))

(define-density dnorm (x mu sigma)
  `(*! (ind-real ,x)
       (* (^ (* 2 %pi) -1/2) (^ ,sigma -1)
	  (^ %e (* -1/2 (^ ,sigma -2) (^ (+ ,x (neg ,mu)) 2))))))

(define-density dinterval (x a b)
  `(*! (ind-real ,x) (indp (+ ,x (neg ,a))) (indp (+ ,b (neg ,x)))))

(define-density dmvnorm (x mu Sigma)
  (destructuring-bind (n) (index-vars 1 (list x mu Sigma) 'n)
  (destructuring-bind (i) (index-vars 1 (list x mu Sigma) 'i)
    `(let (,n (length ,mu))
       (*! (prod! ,i (1 ,n) (ind-real (@ ,x ,i)))
	   (* (^ (* 2 %pi) (* ,n -1/2))
	      (^ (abs (det ,Sigma)) -1/2)
	      (^ %e (* -1/2 (quadmul (inv ,Sigma) (vec- ,x ,mu))))))))))

(define-density dgamma (x alpha beta)
  `(*! (ind-realp (@ lambda i))
       (* (^ ,beta ,alpha) (^ (gamma ,alpha) -1) (^ ,x (- ,alpha 1))
	  (^ %e (neg (* ,beta ,x))))))

(define-density dwishart (X n W)
  (destructuring-bind (p) (index-vars 1 (list X n W) 'p)
  `(let (,p (nrows ,W))
     (*! (ind-symm-pd ,X)
	 (* (^ 2 (* -1/2 ,n ,p))
	    (^ (mvgamma ,p (* 1/2 ,n)) -1)
	    (^ (abs (det ,W)) (* -1/2 ,n))
	    (^ (abs (det ,X)) (* 1/2 (+ ,n (neg ,p) -1)))
	    (^ %e (* -1/2 (trace (matmul (inv ,W) ,X)))))))))

(define-density dcat (x pr)
  `(*! (indnn (+ ,x -1)) (indnn (+ (length ,pr) (neg ,x))) (@ ,pr ,x)))

(define-density ddirch (pr a)
  (destructuring-bind (n) (index-vars 1 (list pr a) 'n)
  (destructuring-bind (i) (index-vars 1 (list pr a) 'i)
    `(let (,n (length ,a))
       (*! (qprod! ,i (1 ,n) (indp (@ ,a ,i)))
	   (indp (qsum ,i (1 ,n) (@ ,a ,i)))
	   (* (qprod ,i (1 ,n) (gamma (@ ,a ,i)))
	      (^ (gamma (qsum ,i (1 ,n) (@ ,a ,i))) -1)
	      (qprod ,i (1 ,n) (^ (@ ,pr ,i) (+ (@ ,a ,i) -1)))))))))
