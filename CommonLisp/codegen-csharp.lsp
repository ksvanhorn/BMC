(require "model-utils")

(defun gen-decl-csharp (var typ ndim)
  (format nil "~a ~a;" (csharp-type-string typ ndim) var))

(defparameter *csharp-base-types*
  '((realxn . "double") (integer . "int") (boolean . "bool")))

(defun csharp-type-string (typ ndim)
  (let ((elem-type-str (cdr (assoc typ *csharp-base-types*))))
    (if (zerop ndim)
	elem-type-str
	(format nil "Array~aD<~a>" ndim elem-type-str))))

(defun gen-lcom-csharp (comment)
  (format nil "// ~a" comment))

(defun get-cb (kw x) (cdr (assoc kw x)))

(defun cb-csharp ()
  '((:gen-decl . gen-decl-csharp) (:gen-lcom . gen-lcom-csharp)))

(defun num-indent (n) (* 4 n))

(defparameter *indent-level* 0)

(defun indent (&optional (s *standard-output*))
  (dotimes (i (num-indent *indent-level*)) (princ #\Space s)))

(defmacro inc-indent-level (&rest body)
  `(let ((*indent-level* (1+ *indent-level*)))
     ,@body))

(defun fmt (&rest args)
  (indent)
  (apply #'format t args)
  (format t "~%"))

(defun gen-variables (mdl)
  (fmt "~a" (gen-lcom-csharp "Inputs"))
  (dolist (x (args-base-decls mdl))
    (fmt "~a" (apply #'gen-decl-csharp x)))
  (terpri)
  (fmt "~a" (gen-lcom-csharp "Model variables"))
  (dolist (x (vars-base-decls mdl))
    (fmt "~a" (apply #'gen-decl-csharp x))))

(defun gen-args-checks (mdl)
  (let ((checks (args-checks mdl)))
    (fmt "public void Validate()")
    (fmt "{")
    (inc-indent-level
      (dolist (x checks)
	(let ((bool-expr (print-expr-c# x)))
	  (fmt "BMC.Check(~a," bool-expr)
	  (fmt "          \"~a\");" bool-expr))))
    (fmt "}")))

(defun print-expr-c# (e &optional (prec -1))
  (case (expr-class e)
	('literal-num (write-to-string e))
	('variable (symbol-name e))
	('array-app (print-array-app-expr-c# e))
	('funct-app
	 (let ((oper (op e))
	        (a (args e)))
	   (cond ((quantifierp oper) (print-quantifier-expr-c# oper a))
		 ((is-binop oper) (print-binop-expr-c# e prec))
		 (t (print-funct-app-expr-c# oper a)))))))

(defparameter *excluded-binops* '(=> ^))

(defun is-binop (oper)
  (and (binopp oper) (not (member oper *excluded-binops*))))

(defun print-funct-app-expr-c# (oper a)
  (format nil "~a(~{~a~^, ~})"
	  (name-c# oper)
	  (mapcar #'print-expr-c# a)))

(defun print-binop-expr-c# (e context-prec)
  (let* ((oper (op e))
	 (prec (precedence oper))
	 (oper-s (name-c# oper))
	 (a (mapcar (lambda (x) (print-expr-c# x prec)) (args e))))
    (with-output-to-string (s)
       (if (> context-prec prec) (write-char #\Space s))
       (format s "~a" (car a))
       (dolist (x (cdr a)) (format s " ~a ~a" oper-s x))
       (if (> context-prec prec) (write-char #\) s)))))

(defun print-array-app-expr-c# (e)
  (format nil "~a[~{~a~^, ~}]"
	 (print-expr-c# (array-op e))
	 (mapcar #'print-index-expr-c# (array-args e))))

(defun print-index-expr-c# (e)
  (case (index-class e)
	('all "")
	('range (format nil "~a : ~a" (range-lo e) (range-hi e)))
	('index (print-expr-c# e))))

(defun print-quantifier-expr-c# (oper a)
  (let ((opname (name-c# oper))
	(var (symbol-name (first a)))
	(lo (print-expr-c# (dec-expr (bounds-lo (second a)))))
	(hi (print-expr-c# (bounds-hi (second a))))
	(body (print-expr-c# (third a))))
    (format nil "~a(~a, ~a, ~a => ~a)" opname lo hi var body)))

(defun dec-expr (x)
  (if (numberp x) (1- x) `(- ,x 1)))

(defparameter *oper-names-c#*
  '((array-length . "BMC.Length")
    (qand . "BMC.ForAll")
    (qsum . "BMC.Sum")
    (=> . "BMC.Implies")
    (= . "==")
    (is-realp . "BMC.IsRealp")
    (is-realnn . "BMC.IsRealnn")
    (is-real . "BMC.IsReal")
    (|is_symm_pd| . "BMC.IsSymmPD")))

(defun name-c# (oper)
  (let ((a (assoc oper *oper-names-c#*)))
    (if (null a) (symbol-name oper) (cdr a))))
