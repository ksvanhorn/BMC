; 1: form
; 2: halo
; 3: garbage

(setq halo_model
'(file
  (args
    (nresp (is_integernn))
    (nvars (is_integer)) 
    (nlevels (is_integer))
    (mu_nu (is_arr is_real #(3)))
    (sigma_nu (is_arr is_realp #(3)))
    (cp (is_arr is_real #((- nlevels 1))))
    (sigma_mu_y_form (is_realp))
    (sigma_beta (is_realp))
    (alpha_lambda_y_form (is_realp))
    (beta_lambda_y_form (is_realp))
    (sigma_mu_x_form (is_realp))
    (dof_Sigma_x_form (is_realp))
    (inverse_scale_Sigma_x_form (is_arr is_real #((- nvars 1) (- nvars 1))))
    (sigma_mu_x_halo (is_arr is_realp #(nvars)))
    (sigma_gamma (is_realp))
    (alpha_sigma_x_halo (is_realp))
    (beta_sigma_x_halo (is_realp))
    (sigma_mu_x_garb (is_arr is_realp #(nvars)))
    (alpha_lambda_x_garb (is_realp))
    (beta_lambda_x_garb (is_realp))
    (alpha_p_segment (is_arr is_realnn #(3))))
  (reqs
    (<= 3 nvars)
    (<= 3 nlevels)
    (for_all k #(1 (- nlevels 2)) (< (@ cp k) (@ cp (+ 1 k))))   
    (> dof_Sigma_x_form (- nvars 2))
    (is_symm_pd inverse_scale_Sigma_x_form)
    (< 0 (sum alpha_p_segment)))
  (vars
    (mu_tls (is_arr is_real #(2)))
    (Sigma_tls (is_arr is_real #(2 2)))
    (nu (is_arr is_real #(3)))
    (tls (is_arr is_real #(nresp 2)))
    (tau (is_arr is_real #(nresp)))
    (scale (is_arr is_real #(nresp)))
    (v (is_arr is_integer #(nresp nvars)))
    (c (is_arr is_real #(nresp (- nlevels 1))))
    (x (is_arr is_real #(nresp nvars)))
    (segment (is_arr is_integer #(nresp)))
    (mu_y_form (is_real))
    (sigma_y_form (is_real))
    (lambda_y_form (is_real))
    (beta (is_arr is_real #((- nvars 1))))
    (mu_x_form (is_arr is_real #((- nvars 1))))
    (Sigma_x_form (is_arr is_real #((- nvars 1) (- nvars 1))))
    (ksi (is_arr is_real #(nresp)))
    (mu_x_halo (is_arr is_real #(nvars)))
    (gamma (is_arr is_real #(nvars)))
    (sigma_x_halo (is_arr is_real #(nvars)))
    (lambda_x_halo (is_arr is_real #(nvars)))
    (mu_x_garb (is_arr is_real #(nvars)))
    (sigma_x_garb (is_arr is_real #(nvars)))
    (lambda_x_garb (is_arr is_real #(nvars)))
    (p_segment (is_arr is_real #(3))))
  (model
    ; scale usage
    (<- mu_tls #(0 (neg (/ (@ Sigma_tls 2 2) 2))))
    (<- (@ Sigma_tls 1 1) (exp (@ nu 1)))
    (<- (@ Sigma_tls 2 2) (exp (@ nu 2)))
    (<- (@ Sigma_tls 1 2)
        (* (exp (/ (+ (@ nu 1) (@ nu 2)) 2)) (tanh (@ nu 3))))
    (<- (@ Sigma_tls 2 1) (@ Sigma_tls 1 2))
    (for h (1 3)
      (~ (@ nu h) (dnorm (@ mu_nu h) (@ sigma_nu h))))
    (for i (1 nresp)
      (for j (1 nvars)
        (~ (@ v i j) (dinterval (@ x i j) (@ c i nil))))
      (for k (1 (- nlevels 1))
	(<- (@ c i k) (/ (- (@ cp k) (@ tau i)) (@ scale i))))
      (~ (@ tls i nil) (dmvnorm mu_tls Sigma_tls))
      (<- (@ tau i) (@ tls i 1))
      (<- (@ scale i) (exp (@ tls i 2))))

    ; formers
    (for i (1 nresp)
      (if ((= 1 (@ segment i))
           (~ (@ x i 1)
              (dnorm (+ mu_y_form (dot (@ x i (rng 2 nvars)) (@ beta i nil)))
                     sigma_y_form))
           (~ (@ x i (rng 2 nvars))
              (dmvnorm mu_x_form Sigma_x_form)))))
    (~ mu_y_form (dnorm 0 sigma_mu_y_form))
    (<- sigma_y_form (/ 1 (sqrt lambda_y_form)))
    (~ lambda_y_form (dgamma alpha_lambda_y_form beta_lambda_y_form))
    (for j (1 (- nvars 1))
      (~ (@ beta j) (dnorm 0 sigma_beta))
      (~ (@ mu_x_form j) (dnorm 0 sigma_mu_x_form)))
    (~ Sigma_x_form
       (dinvwishart dof_Sigma_x_form inverse_scale_Sigma_x_form))

    ; haloers
    (for i (1 nresp)
      (if ((= 2 (@ segment i))
           (~ (@ ksi i) (dnorm 0 1))
           (for j (1 nvars)
             (~ (@ x i j)
             (dnorm (+ (@ mu_x_halo j) (* (@ gamma j) (@ ksi i)))
                    (@ sigma_x_halo j)))))))
    (for j (1 nvars)
      (~ (@ gamma j) (dnorm 0 sigma_gamma))
      (~ (@ mu_x_halo j)(dnorm 0 (@ sigma_mu_x_halo j)))
      (<- (@ sigma_x_halo j) (/ 1 (sqrt (@ lambda_x_halo j))))
      (~ (@ lambda_x_halo j)
         (dgamma alpha_sigma_x_halo beta_sigma_x_halo)))

    ; garbage
    (for i (1 nresp)
      (if ((= 3 (@ segment i))
           (for j (1 nvars)
             (~ (@ x i j)
                (dnorm (@ mu_x_garb j) (@ sigma_x_garb j)))))))
    (for j (1 nvars)
      (~ (@ mu_x_garb j) (dnorm 0 (@ sigma_mu_x_garb j)))
      (<- (@ sigma_x_garb j) (/ 1 (sqrt (@ lambda_x_garb j))))
      (~ (@ lambda_x_garb j)
         (dgamma alpha_lambda_x_garb beta_lambda_x_garb)))

    ; segments
    (for i (1 nresp)
      (~ (@ segment i) (dcat p_segment)))
    (~ p_segment (ddirch alpha_p_segment))))
)

(defun print-var-decl (x)
  (destructuring-bind
    (variable (type-ctor &rest type-ctor-args)) x
    (format t "  ~a : ~a~%"
	    (my-symbol-name variable) (type-name type-ctor type-ctor-args))))

(defun type-name (type-ctor args)
  (cond
    ((null args)
     (format nil "~a" (predicate->type-name type-ctor)))
    ((eql 'is_arr type-ctor)
     (destructuring-bind (predicate dim) args
       (format nil "~a[~{~a~^, ~}]"
	       (predicate->type-name predicate)
	       (mapcar #'make-expr-string dim))))
    (t (error "Invalid type predicate"))))

(defun predicate->type-name (predicate-symbol)
  (let ((predicate-name (my-symbol-name predicate-symbol)))
    (unless (string= "is_" (subseq predicate-name 0 3))
      (error (format nil "~a is not a valid predicate name" predicate-name)))
    (subseq predicate-name 3)))

(defun make-expr-string (expr)
  (make-expr-string-prec expr -1))

(defun make-expr-string-prec (expr prec)
  (cond
    ((null expr) "")
    ((symbolp expr)
     (my-symbol-name expr))
    ((numberp expr)
     (write-to-string expr))
    ((not (listp expr))
     (error "Invalid expression"))
    ((is-binop (car expr))
     (make-binop-expr-string expr prec))
    ((is-quantifier (car expr))
     (make-quantifier-expr-string expr))
    ((is-indexer (car expr))
     (make-index-expr-string expr))
    (t
     (make-fct-expr-string expr))))

(defun is-indexer (x) (eql x '@))

(defun make-index-expr-string (x)
  (destructuring-bind (at-symbol var &rest indices) x
    (format nil "~a[~{~a~^, ~}]"
	    (make-expr-string var)
	    (mapcar #'make-expr-string indices))))

(defun is-quantifier (x)
  (member x '(for_all exists)))

(defun make-quantifier-expr-string (x)
  (destructuring-bind (q var (lo hi) body) x
    (let ((lst (mapcar #'make-expr-string (list q var lo hi body))))
      (apply #'format nil "~a(~a, ~a, ~a, ~a)" lst))))

(defun make-fct-expr-string (x)
  (format nil "~a(~{~a~^, ~})"
	  (make-expr-string (car x))
	  (mapcar #'make-expr-string (cdr x))))

(defun make-binop-expr-string (expr context-prec)
  (let* ((op (car expr))
	 (prec (precedence op))
	 (op-str (op-name op))
	 (args (mapcar
		 (lambda (x) (make-expr-string-prec x prec))
		 (cdr expr))))
    (if (< (length args) 2)
	(error "Binary operator must have at least two arguments"))
    (with-output-to-string (s)
      (if (> context-prec prec) (write-char #\( s))
      (format s "~a" (car args))
      (dolist (x (cdr args)) (format s " ~a ~a" op-str x))
      (if (> context-prec prec) (write-char #\) s))
      s)))

(defun op-name (x)
  (if (eql 'rng x) ":" (my-symbol-name x)))

(defun is-binop (x)
  (member x '(< <= = >= > + - * / ^ rng)))

(defun precedence (op)
  (getf '(< 5 <= 5 = 5 > 5 >= 5 + 10 - 10 * 20 / 20 ^ 30 rng 6) op))

(defun print-expr (x) (format t "  ~a~%" (make-expr-string x)))

(defun print-spaces (n)
  (dotimes (i n) (princ #\ )))

(defun print-rel (indentation rel)
  (print-spaces indentation)
  (cond
    ((member (car rel) '(~ <-))
     (format t "~a ~a ~a~%"
	     (make-expr-string (cadr rel))
	     (car rel)
	     (make-expr-string (caddr rel))))
    ((eql 'for (car rel))
     (destructuring-bind
       (var (lo hi) &rest body) (cdr rel)
       (format t "for ~a in ~a : ~a~%"
	       (make-expr-string var)
	       (make-expr-string lo)
	       (make-expr-string hi))
       (dolist (r body) (print-rel (+ 2 indentation) r))))
    ((eql 'if (car rel))
     (print-if-rel indentation "if" (cdr rel)))
    (t
      (error "Invalid relation"))))

(defun print-if-rel (indentation keyword clauses)
  (unless (null clauses)
    (if (not (string= "if" keyword))
	(print-spaces indentation))
    (destructuring-bind ((test &rest rels) &rest rest) clauses
      (format t "~a ~a~%" keyword (make-expr-string test))
      (dolist (r rels) (print-rel (+ 2 indentation) r))
      (print-if-rel indentation "elsif" rest))))    

(defun print-model (model)
  (destructuring-bind
    (sym-file (sym-args &rest args)
	      (sym-reqs &rest reqs)
	      (sym-vars &rest vars)
              (sym-model &body rels)) model

    (format t "args~%")
    (mapc #'print-var-decl args)

    (format t "reqs~%")
    (mapc #'print-expr reqs)

    (format t "vars~%")
    (mapc #'print-var-decl vars)

    (format t "model~%")
    (mapc (lambda (x) (print-rel 2 x)) rels)
    nil
))

(defmacro with-output-to-file (filename &rest body)
  (let ((strm (gensym)))
    `(with-open-file (,strm ,filename :direction :output)
	(let ((*standard-output* ,strm)) ,@body))))

(defun my-symbol-name (s) (string-downcase (symbol-name s)))

(defmacro comment (&rest _) nil)


(defun extract-model-args (mdl)
  (destructuring-bind
    (file-kw (args-kw &rest args) &rest _) mdl
    (mapcar (lambda (x) (list (car x) (extract-type (cadr x)))) args)))

(defun extract-model-vars (mdl)
  (destructuring-bind
    (file-kw args-section reqs-section (vars-kw &rest vars) &rest _) mdl
      (mapcar (lambda (x) (list (car x) (extract-type (cadr x) :strict t)))
	      vars)))

(defun extract-precondition (mdl)
  (destructuring-bind
    (file-kw (args-kw &rest args) (reqs-kw &rest reqs) &rest _) mdl
    (append (extract-type-reqs args) reqs)))

(defun $bar ()
  `((MLIST) (($IS_INTEGERNN) $NRESP) (($IS_ARR) $MU_NU $IS_REAL ((MLIST) 3))))

(defun $extract_assumptions (mdl-name)
  (let ((mdl (eval (maxima-symbol-to-lisp-symbol mdl-name))))
    ;(format t "mdl-name: ~a~%" mdl-name)
    (extract-assumptions mdl)))

(defun maxima-symbol-to-lisp-symbol (s)
  (intern (subseq (symbol-name s) 1)))

(defun extract-assumptions (mdl)
  (destructuring-bind
    (file-kw (args-kw &rest args)
	     (reqs-kw &rest reqs)
	     (vars-kw &rest vars) &rest _) mdl
    (let ((assums (append (mapcar #'extract-type-pred args)
		 	  reqs
			  (mapcar #'extract-type-pred vars))))
      (print assums)
      `((MLIST) ,@assums))))

(defun extract-type-pred (x)
  (destructuring-bind (var (pred &rest rest)) x
     (verify (symbolp var))
     (verify (symbolp pred))
     ;(format t "var: ~a~%" var)
     ;(format t "pred: ~a~%" pred)
     (let ((mvar (to-maxima-symbol var))
	   (mpred (list (to-maxima-symbol pred))))
       (if (eql 'is_arr pred)
	   (progn
	     (verify (= 2 (length rest)))
	     (let ((type_pred (to-maxima-symbol (car rest)))
		   (dims (mapcar #'to-maxima-expr (cadr rest))))
	       `(,mpred ,mvar ,type_pred ((MLIST) ,@dims))))
	 (progn
	   (verify (= 0 (length rest)))
	   `(,mpred ,mvar))))))

(defun to-maxima-expr (x)
  (cond
    ((symbolp x)
     (to-maxima-symbol x))
    ((numberp x)
     x)
    (t
     (destructuring-bind (op &rest args) x
	(let ((maxima-op (to-maxima-op op))
	      (maxima-args (mapcar #'to-maxima-expr args)))
	  (cond
	    ((and (eql '- op) (= 2 (length args)))
	     (let ((a1 (first maxima-args))
		   (a2 (second maxima-args)))
	       `((MPLUS) ,a1 ((MTIMES) -1 ,a2))))
	    ((and (eql '/ op) (= 2 (length args)))
	     (let ((a1 (first maxima-args))
		   (a2 (second maxima-args)))
	       `((MTIMES) ,a1 ((MEXPT) ,a2 -1))))
	    (t
	     `((,maxima-op) ,@maxima-args))))))))

(defun to-maxima-symbol (s)
  (intern (concatenate 'string "$" (symbol-name s))))

(defun to-maxima-op (x)
  (cond
    ((eql '+ x) 'MPLUS)
    ((eql '* x) 'MTIMES)
    ((eql '^ x) 'MEXPT)
    ((eql '>= x) 'MGEQP)
    ((eql '> x) 'MGREATERP)
    ((eql '<= x) 'MLEQP)
    ((eql '< x) 'MLESSP)
    ((eql '= x) '$EQUAL)
    ((eql '!= x) '$NOTEQUAL)
    (t (to-maxima-symbol x))))

(defmacro verify (expr)
  `(unless ,expr (error (format nil "Assertion failed: ~a" ',expr))))

(defun extract-type (x &key (strict nil))
  (destructuring-bind (type-ctor &rest type-args) x
     (cond ((member type-ctor '(is_real is_realnn is_realp))
	    (verify (eql nil type-args))
	    (verify (or (not strict) (eql 'is_real type-ctor)))
	    'real)
	   ((member type-ctor '(is_integer is_integernn is_integerp))
	    (verify (eql nil type-args))
	    (verify (or (not strict) (eql 'is_integer type-ctor)))
	    'integer)
	   ((eql type-ctor 'is_bool)
	    (verify (eql nil type-args))
	    'bool)
	   ((eql type-ctor 'is_arr)
	    (destructuring-bind (type-pred dims) type-args
	       (let ((eltype (extract-type (list type-pred) :strict strict)))
		 (list* 'arr eltype dims))))
	   (t (verify nil)))))

(defun extract-type-reqs (args)
  (remove-if #'null (mapcar #'extract-type-restriction-from-decl args)))

(defun extract-type-restriction-from-decl (x)
  (destructuring-bind (var (type-ctor &rest type-args)) x
    (cond ((member type-ctor '(is_realnn is_integernn))
	   `(<= 0 ,var))
	  ((member type-ctor '(is_realp is_integerp))
	   `(< 0 ,var))
	  ((eql type-ctor 'is_arr)
	   (destructuring-bind (eltype dims) type-args
	     (let ((pred (cond
		 	   ((member eltype '(is_realnn is_integernn))
			    (lambda (x) `(<= 0 ,x)))
			   ((member eltype '(is_realp is_integerp))
			    (lambda (x) `(< 0 ,x)))
			   (t nil))))
	       (and pred (arr-for-all var dims pred '())))))
	  (t nil))))

(defun arr-for-all (var dims pred idx_vars)
  (if (null dims)
    (funcall pred `(@ ,var ,@(reverse idx_vars)))
    (let* ((idx1 (gensym))
          (inner (arr-for-all var (cdr dims) pred (cons idx1 idx_vars))))
      `(for_all ,idx1 (1 ,(car dims)) ,inner))))
    

; List of args with programming-language types [DONE]
; List of state vars with programming-language types [DONE]
; Precondition on args [DONE]
;;; Generate code to check precondition
; Generate check of initial state for validity
; Check that we have a DAG
; Check that all expressions well-defined
; 
