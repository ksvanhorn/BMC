(in-package :type-inference)

(defadt bare-type
  (scalar stype)
  (array elem-type num-dims)
  (int-map return-type))

(defun is-base-type-name (x)
  (member x '(realxn integer boolean @-all-type @-rng-type @-idx-type)))

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
    (destructuring-bind (typ-se n) se
      (assert (integerp n))
      (make-bare-type-array
        :elem-type (sexpr->bare-type typ-se)
	:num-dims n)))))

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
  (handler-case
    (infer-type-main e env)
    (error (x) (rethrow-error x "Error in (infer-type ~a ...):~%  " e))))

(defun infer-type-main (e env)
  (let ((let-expansion (is-let-expr e)))
    (when let-expansion
      (return-from infer-type-main (infer-type-let-expr let-expansion env))))
  (adt-case expr e
    ((const name) (literal-type name))
    ((variable symbol) (var-type env symbol))
    ((apply fct args)
     (infer-type-apply fct args env))
    ((lambda var body)
     (let ((new-env (add-var-type env var #tinteger)))
       (make-bare-type-int-map :return-type (infer-type body new-env))))))

(defun infer-type-apply (fct args env)
  (let ((arg-types (mapcar (lambda (a) (infer-type a env)) args))
	(typing-fct (typing-function-for fct)))
    (funcall typing-fct arg-types)))

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
	    (loop for (key . value) in +literal-types+ do
	      (setf (gethash key ht) value)))
	  (or (gethash name ht)
	      (error "No type known for const name ~a." name))))))

(defconstant +literal-types+
  '((true . #tboolean)
    (false . #tboolean)
    (%pi . #trealxn)
    (%infty+ . #trealxn)
    (%infty- . #trealxn)
    (%e . #trealxn)
    (@-all . #t@-all-type)
    (%true-pred . #t(int-map boolean))))

(let (ht)
  (defun typing-function-for (fct)
    (when (null ht)
      (setf ht (create-typing-functions)))
    (or (gethash fct ht)
	(error "Do not know how to infer return type for function ~a." fct))))

(defun create-typing-functions ()
  (let ((ht (make-hash-table)))
    (setf (gethash '@-slice ht) #'infer-slice-type)
    (loop for (fct . signatures) in +simple-function-signatures+ do
      (setf (gethash fct ht) (simple-typing-function fct signatures)))
    (loop for (fct . types) in +operator-signatures+ do
      (setf (gethash fct ht) (operator-typing-function fct types)))
    (loop for (fct nargs . elem-types) in +lifted-fcts+ do
      (setf (gethash fct ht) (lifted-fct-typing-function fct nargs elem-types)))
    ht))

(defun simple-typing-function (fct signatures)
  (alambda (arg-types)
    (dolist (sig signatures)
      (when (equalp arg-types (cdr sig))
	(return-from self (car sig))))
    (infer-type-error fct arg-types)))

(defconstant +simple-function-signatures+
  '((<= (#tboolean #tinteger #tinteger))
    (< (#tboolean #trealxn #trealxn))
    (= (#tboolean #tinteger #tinteger)
       (#tboolean #trealxn #trealxn))
    (!= (#tboolean #tinteger #tinteger))
    (int (#tinteger #tboolean))
    (dot (#trealxn #t(realxn 1) #t(realxn 1))
	 (#t(realxn 1) #t(realxn 2) #t(realxn 1))
	 (#t(realxn 1) #t(realxn 1) #t(realxn 2))
	 (#t(realxn 2) #t(realxn 2) #t(realxn 2))
	 (#t(realxn 1) #t(realxn 2) #t(realxn 2) #t(realxn 1)))
    (/ (#trealxn #trealxn #trealxn))
    (^-2 (#trealxn #trealxn))
    (^-1 (#trealxn #trealxn))
    (^1/2 (#trealxn #trealxn))
    (^-1/2 (#trealxn #trealxn))
    (^2 (#trealxn #trealxn)
	(#tinteger #tinteger))
    (log (#trealxn #trealxn))
    (exp (#trealxn #trealxn))
    (tanh (#trealxn #trealxn))
    (neg (#trealxn #trealxn)
	 (#tinteger #tinteger))
    (real (#trealxn #tinteger))
    (dmvnorm-density (#trealxn #t(realxn 1) #t(realxn 1) #t(realxn 2)))
    (is-integerp (#tboolean #tinteger))
    (is-realp (#tboolean #trealxn))
    (is-real (#tboolean #trealxn))
    (is-symm-pd (#tboolean #t(realxn 2)))
    (@-idx (#t@-idx-type #tinteger))
    (@-rng (#t@-rng-type #tinteger #tinteger))
    (@ (#tinteger #t(integer 1) #tinteger)
       (#trealxn #t(realxn 1) #tinteger)
       (#tinteger #t(integer 2) #tinteger #tinteger)
       (#trealxn #t(realxn 2) #tinteger #tinteger)
       (#t(realxn 1) #t((realxn 1) 1) #tinteger)
       (#t(realxn 2) #t((realxn 2) 1) #tinteger))
    (vmax (#tinteger #t(integer 1)))
    (diag_mat (#t(realxn 2) #t(realxn 1))
	      (#t(integer 2) #t(integer 1)))
    (o^2 (#t(realxn 2) #t(realxn 1))
	 (#t(integer 2) #t(integer 1)))
    ($* (#t(realxn 1) #trealxn #t(realxn 1))
	(#t(realxn 2) #trealxn #t(realxn 2)))
    (inv-pd (#t(realxn 2) #t(realxn 2)))
    (real-zero-arr (#t(realxn 1) #tinteger)
		   (#t(realxn 2) #tinteger #tinteger))
    (sum (#trealxn #t(realxn 1)))
    (cons (#t(realxn 1) #trealxn #t(realxn 1)))
    (cons-col (#t(realxn 2) #t(realxn 1) #t(realxn 2)))
    (cons-row (#t(realxn 2) #t(realxn 1) #t(realxn 2)))
    (vec (#t(realxn 1) #trealxn)
         (#t(realxn 1) #trealxn #trealxn)
         (#t(realxn 1) #trealxn #trealxn #trealxn)
         (#t(realxn 1) #trealxn #trealxn #trealxn #trealxn)
	 (#t((realxn 1) 1) #t(realxn 1) #t(realxn 1) #t(realxn 1))
	 (#t((realxn 2) 1) #t(realxn 2) #t(realxn 2) #t(realxn 2)))
    (qvec (#t(realxn 1)
           #tinteger #tinteger #t(int-map boolean) #t(int-map realxn)))
    (qsum (#tinteger
	   #tinteger #tinteger #t(int-map boolean) #t(int-map integer))
	  (#trealxn
           #tinteger #tinteger #t(int-map boolean) #t(int-map realxn)))
    (qprod (#trealxn
            #tinteger #tinteger #t(int-map boolean) #t(int-map realxn)))
    (qand (#tboolean
	   #tinteger #tinteger #t(int-map boolean) #t(int-map boolean)))
    (qnum (#tinteger
	   #tinteger #tinteger #t(int-map boolean) #t(int-map boolean)))
    (qmax (#tinteger
	   #tinteger #tinteger #t(int-map boolean) #t(int-map integer))
	  (#trealxn
	   #tinteger #tinteger #t(int-map boolean) #t(int-map realxn)))
    (qmin (#tinteger
	   #tinteger #tinteger #t(int-map boolean) #t(int-map integer))
	  (#trealxn
	   #tinteger #tinteger #t(int-map boolean) #t(int-map realxn)))
    (q@sum (#t(realxn 1)
            #tinteger #tinteger #t(int-map boolean) #t(int-map realxn 1)
            #tinteger)
	   (#t(realxn 2)
            #tinteger #tinteger #t(int-map boolean) #t(int-map realxn 2)
            #tinteger #tinteger))
    (if-then-else (#trealxn #tboolean #trealxn #trealxn))
))

(defun operator-typing-function (fct types)
  (alambda (arg-types)
    (dolist (typ types)
      (when (every (lambda (argt) (equalp typ argt)) arg-types)
	(return-from self typ)))
    (infer-type-error fct arg-types)))

(defconstant +operator-signatures+
  '((- #tinteger #trealxn)
    (+ #tinteger #trealxn)
    (* #tinteger #trealxn)
    (and #tboolean)
    (max #tinteger #trealxn)
    (min #tinteger #trealxn)))

(defun infer-slice-type (arg-types)
  (assert (<= 2 (length arg-types)))
  (destructuring-bind (arr-type . idx-types) arg-types
    (let* ((nidx (count #t@-idx-type idx-types :test #'equalp))
	   (nrng (count #t@-rng-type idx-types :test #'equalp))
	   (nall (count #t@-all-type idx-types :test #'equalp))
	   (ntotal (length idx-types)))
      (assert (= ntotal (+ nidx nrng nall)))
      (adt-case bare-type arr-type
	((array elem-type num-dims)
         (assert (= ntotal num-dims))
	 (make-bare-type-array :elem-type elem-type
			       :num-dims (+ nrng nall)))))))

(defun lifted-fct-typing-function (fct nargs elem-types)
  (alambda (arg-types)
    (when arg-types
      (destructuring-bind (arg1-type . rest-arg-types) arg-types
        (when (and (or (eq '* nargs)
		       (= nargs (length arg-types)))
		   (is-bare-type-array arg1-type)
                   (every (lambda (a) (equalp arg1-type a)) rest-arg-types)
		   (member (bare-type-array-elem-type arg1-type) elem-types
			   :test #'equalp))
	  (return-from self arg1-type))))
    (infer-type-error fct arg-types)))

(defconstant +lifted-fcts+
  '((@^-2 1 #trealxn)
    (@^2 1 #trealxn)
    (@^-1 1 #trealxn)
    (@+ * #trealxn)
    (@* * #trealxn)
    (@- 2 #trealxn)
    (@/ 2 #trealxn)))
