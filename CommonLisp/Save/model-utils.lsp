(defpackage :model
  (:use :common-lisp :utils)
  (:export
    :read-model :extract-args :extract-reqs :extract-vars :extract-body
    ;:args-base-decls :vars-base-decls
    ;:expr-class :op :args :quantifierp :binopp
))
(in-package :model)


#|
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
|#

(defun num-dims (base-array-typ)
  (second base-array-typ))

(defun scalar-types () (mapcar #'car *base-scalar-types*))

(defun concat-symbol (&rest args)
  (intern (strcat args)))

(defun map-range (lo hi fct)
  (do ((result nil (cons (funcall fct n) result))
       (n lo (1+ n)))
      ((> n hi) (reverse result))))

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
