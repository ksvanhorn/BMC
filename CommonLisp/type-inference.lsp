(in-package :type-inference)

(defadt bare-type
  (scalar stype)
  (array elem-type num-dims)
  (int-map return-type))

(defun is-base-type-name (x)
  (member x '(realxn integer boolean @-all-type)))

(defun sexpr->bare-type (se)
  (cond
   ((atom se)
    (assert (is-base-type-name se))
    (make-bare-type-scalar :stype se))
   ((eq 'int-map (car se))
    (let ((rest (cdr se)))
      (make-bare-type-int-map :return-type
	(sexpr->bare-type (if (null (cdr rest)) (car rest) rest)))))
   (t
    (destructuring-bind (typ n) se
      (assert (is-base-type-name typ))
      (assert (integerp n))
      (make-bare-type-array :elem-type typ :num-dims n)))))

(defun xformer-sexpr->bare-type (stream subchar arg)
  (let ((se (read stream t)))
    (sexpr->bare-type se)))

(set-dispatch-macro-character #\# #\t #'xformer-sexpr->bare-type)

(defun no-var-types () nil)

(defun add-var-type (env var btype) (acons var btype env))

(defun var-type (env var) (assoc-lookup var env))

(defun assocs->env (assocs)
  (let ((env (no-var-types)))
    (loop for (var . typ) in assocs do
      (setf env (add-var-type env var typ)))
    env))

(defun infer-type (e env)
  (let ((let-expansion (is-let-expr e)))
    (when let-expansion
      (return-from infer-type (infer-type-let-expr let-expansion env))))
  (adt-case expr e
    ((const name) (literal-type name))
    ((variable symbol) (var-type env symbol))
    ((apply fct args)
     (let ((arg-types (mapcar (lambda (a) (infer-type a env)) args)))
       (do ((signatures (function-signatures fct)
			(lcdr signatures)))
	   ((null signatures) (infer-type-error fct arg-types))
	 (let ((sig (lcar signatures)))
	   (if (equalp arg-types (cdr sig))
	     (return-from infer-type (car sig)))))))
    ((lambda var body)
     (let ((new-env (add-var-type env var #tinteger)))
       (make-bare-type-int-map :return-type (infer-type body new-env))))))

(defun infer-type-let-expr (let-expansion env)
  (destructuring-bind (v val body) let-expansion
    (let* ((val-type (infer-type val env))
	   (new-env (add-var-type env v val-type)))
      (infer-type body new-env))))

(defun infer-type-error (fct arg-types)
  (error "Could not find signature for ~a to match argument types (~{~a~^, ~})."
	 fct arg-types))

(let (ht)
  (defun literal-type (name)
     (cond
       ((integerp name) #tinteger)
       ((rationalp name) #trealxn)
       ((realp name) #trealxn)
       (t (when (null ht)
	    (setf ht (make-hash-table))
	    (loop for (key . value) in *literal-types* do
	      (setf (gethash key ht) value)))
	  (or (gethash name ht)
	      (error "No type known for const name ~a." name))))))

(defparameter *literal-types*
  '((true . #tboolean)
    (false . #tboolean)
    (%pi . #trealxn)
    (%infty+ . #trealxn)
    (%infty- . #trealxn)
    (%e . #trealxn)
    (@-all . #t@-all-type)
    (%true-pred . #t(int-map boolean))))

(let (ht)
  (defun function-signatures (fct)
    (when (null ht)
      (setf ht (create-function-signatures)))
    (or (gethash fct ht)
	(error "No type known for function ~a." fct))))

(defun create-function-signatures ()
  (let ((ht (make-hash-table)))
    (loop for (key . value) in *simple-function-signatures* do
      (setf (gethash key ht) value))
    (loop for (key . types) in *operator-signatures* do
       (setf (gethash key ht)
	     (apply #'interleave (mapcar #'operator-signatures types))))
    ht))

(defparameter *simple-function-signatures*
  '((<= (#tboolean #tinteger #tinteger))
    (< (#tboolean #trealxn #trealxn))
    (is-integerp (#tboolean #tinteger))
    (is-realp (#tboolean #trealxn))
    (is-real (#tboolean #trealxn))
    (is-symm-pd (#tboolean #t(realxn 2)))
    (@ (#tinteger #t(integer 1) #tinteger)
       (#trealxn #t(realxn 1) #tinteger)
       (#tinteger #t(integer 2) #tinteger #tinteger)
       (#trealxn #t(realxn 2) #tinteger #tinteger))
    (vmax (#tinteger #t(integer 1)))
    (qsum (#tinteger
	   #tinteger #tinteger #t(int-map boolean) #t(int-map integer))
	  (#trealxn
           #tinteger #tinteger #t(int-map boolean) #t(int-map realxn)))
    (qprod (#trealxn
            #tinteger #tinteger #t(int-map boolean) #t(int-map realxn)))
    (qand (#tboolean
	   #tinteger #tinteger #t(int-map boolean) #t(int-map boolean)))
))

(defparameter *operator-signatures*
  '((- #tinteger #trealxn)
    (+ #tinteger #trealxn)
    (* #tinteger #trealxn)))

(defun interleave (&rest lazy-lists)
  (let ((ll (remove-if #'null lazy-lists)))
    (when (null ll)
      (return-from interleave nil))
    (flet* ((prepend (xs)
              (if (null xs)
	        (apply #'interleave (mapcar #'lcdr ll))
	        (lcons (car xs) (prepend (cdr xs))))))
      (prepend (mapcar #'lcar ll)))))

(defun operator-signatures (typ)
  (flet* ((signatures-following (previous-sig)
	    (let ((sig (cons typ previous-sig)))
	      (lcons sig (signatures-following sig)))))
    (signatures-following nil)))
