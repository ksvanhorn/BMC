(in-package :adt)
; Algebraic data types

(defun compound-symbol (x y)
  (intern (format nil "~a-~a" x y)))

(defun pred-name (x) (intern (format nil "IS-~a" x)))

(defun field-decl (x) `(,x nil :read-only t))

(defun has-no-duplicates (x) (= (length x) (length (remove-duplicates x))))

(defun check-defadt-subtype (base-type x)
  (unless (and (consp x) (every #'symbolp x))
    (error "In (defadt ~W ... ~W ...): ~
            subtype form must be nonempty list of symbols." base-type x))
  (unless (has-no-duplicates x)
    (error "In (defadt ~W ... ~W ...): ~
            subtype form must not have duplicate symbols." base-type x)))

(defun defadt-subtype (base-type x)
  (check-defadt-subtype base-type x)
  (let* ((subtype (compound-symbol base-type (first x)))
	 (pname (pred-name subtype))
         (field-decls (mapcar #'field-decl (rest x))))
    `(defstruct (,subtype (:include ,base-type) (:predicate ,pname))
       ,@field-decls)))

(defun check-expand-clause (base-type clause)
  (unless (and (consp clause) (consp (first clause))
	       (every #'symbolp (first clause)))
    (error "In (adt-case ~W ... ~W ...): ~
            clause form must begin with a list of symbols." base-type clause)))

(defun expand-clause (base-type clause)
  (check-expand-clause base-type clause)
  (destructuring-bind ((p-head &rest p-args) &rest body) clause
    (let* ((subtype (compound-symbol base-type p-head))
  	   (pred (pred-name subtype))
	   (guard `(,pred ,base-type)))
      (flet ((expand-p-arg (field)
  	       (let ((extractor (compound-symbol subtype field)))
	         `(,field (,extractor ,base-type)))))
        (let ((let-defs (mapcar #'expand-p-arg p-args)))
	  (if (null let-defs)
	    `(,guard ,@body)
	    `(,guard (let (,@let-defs) ,@body))))))))

; Example:
; (adt-case foo some-expression
;   ((bar a) (format t "OK") (f foo a))
;   ((baz a b) (g foo a b))
;   ((bop) (h foo)))
; =>
; (let ((foo some-expression))
;   (cond foo
;     ((is-foo-bar foo)
;      (let ((a (foo-bar-a foo)))
;        (format t "OK")
;        (f foo a)))
;     ((is-foo-baz foo)
;      (let ((a (foo-baz-a foo))
;            (b (foo-baz-b foo)))
;        (g foo a b)))
;     ((is-foo-bop foo)
;      (h foo))
;     (t (error "No match in adt-case."))))

(defmacro adt-case (base-type x &rest clauses)
  (let ((clause-expansions
	  (mapcar (lambda (c) (expand-clause base-type c)) clauses)))
    `(let ((,base-type ,x))
       (cond ,base-type
	     ,@clause-expansions
	     (t (error "No match in adt-case."))))))

; Example:
; (defadt foo (bar a) (baz a b) (bop))
; =>
; (progn
;   (defstruct (foo (:predicate is-foo)))
;   (defstruct (foo-bar (:include foo) (:predicate is-foo-bar))
;     (a nil :read-only t))
;   (defstruct (foo-baz (:include foo) (:predicate is-foo-baz))
;     (a nil :read-only t)
;     (b nil :read-only t))
;   (defstruct (foo-bop (:include foo) (:predicate is-foo-bop)))
;   nil)

(defmacro defadt (base-type &rest subtype-forms)
  (flet ((get-subtype (x)
	   (if (consp x) (car x) nil)))
  (let ((subtypes (remove nil (mapcar #'get-subtype subtype-forms))))
    (unless (has-no-duplicates subtypes)
      (error "In (defadt ~W~{ ~S~}): duplicate subtypes not allowed"
	     base-type subtype-forms))
  (let* ((pname (pred-name base-type))
	 (base-type-def `(defstruct (,base-type (:predicate ,pname))))
	 (subtype-defs
	   (mapcar (lambda (x) (defadt-subtype base-type x)) subtype-forms)))
    `(progn ,base-type-def ,@subtype-defs nil)))))

(defun check-defadt1(typ args)
  (unless (and (symbolp typ) (every #'symbolp args))
    (error "In (defadt1 ~W ~{~W~^ ~}): ~
            arguments must be nonempty list of symbols." typ args))
  (unless (has-no-duplicates (cons typ args))
    (error "In (defadt1 ~W ~{~W~^ ~}): ~
            argument list must not have duplicate symbols." typ args)))

; Example:
; (defadt1 foo bar baz)
; =>
; (defstruct (foo (:predicate is-foo))
;   (bar nil :read-only t)
;   (baz nil :read-only t))

(defmacro defadt1 (typ &rest args)
  (check-defadt1 typ args)
  (let ((pname (pred-name typ))
	(field-decls (mapcar #'field-decl args)))
    `(defstruct (,typ (:predicate ,pname)) ,@field-decls)))
