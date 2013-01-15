(defpackage :testing-utilities
  (:use :cl :compile :utils :symbols)
  (:export :ppstr :with-genvar-counter :let-test-macros :=> :=>-KEY))

(in-package :testing-utilities)

;;; Turn the output generated by expr (using fmt and indent)
;;; into a string

(defmacro ppstr (expr &key (indent-level 0) (indent-amount 4))
  (let ((s (gensym)))
    `(with-output-to-string (,s)
       (let ((*indent-level* ,indent-level)
	     (*indent-amount* ,indent-amount)
	     (*fmt-ostream* ,s))
	 ,expr))))

(defmacro with-genvar-counter (n &body body)
  `(let ((variables::*genvar-counter* ,n))
     ,@body))

(defmacro let-test-macros (defs &body body)
  (flet*
    ((def->macro (def) (apply #'dm def))
     (dm (name parameters &rest def-body)
       (multiple-value-bind (expanded-params keyword-checks) (expand parameters)
         `(,name (,@expanded-params) ,@keyword-checks ,@def-body)))
     (expand (parameters)
       (cond
	 ((null parameters)
	  (values nil nil))
	 ((eq '=> (car parameters))
	  (values (cons '=>-KEY (cdr parameters))
		  '((assert (eq '=> =>-KEY)))))
	 (t
	   (destructuring-bind (parm1 . parms-rest) parameters
	     (multiple-value-bind (expanded checks) (expand parms-rest)
               (let* ((parm1-kw-parm (compound-symbol parm1 "KEY"))
		      (parm1-keyword (intern (symbol-name parm1) "KEYWORD")))
		 (values (list* parm1-kw-parm parm1 expanded)
			 (cons `(assert (eq ,parm1-keyword ,parm1-kw-parm))
			       checks)))))))))
    (let ((macro-defs (mapcar #'def->macro defs)))
      `(macrolet ,macro-defs ,@body))))
