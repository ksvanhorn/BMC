(use-package :adt)

(define-test adt-expansion-tests
  (assert-expands 
   '(progn
     (defstruct (foo (:predicate is-foo)))
     (defstruct (foo-bar (:include foo) (:predicate is-foo-bar))
       (a nil :read-only t))
     (defstruct (foo-baz (:include foo) (:predicate is-foo-baz))
       (a nil :read-only t)
       (b nil :read-only t))
     (defstruct (foo-bop (:include foo) (:predicate is-foo-bop)))
     nil)
   (defadt foo (bar a) (baz a b) (bop)))

  (assert-expands
   '(let ((foo some-expression))
     (cond
       ((is-foo-bar foo)
	(let ((a (foo-bar-a foo)))
	  (format t "OK")
	  (f foo a)))
       ((is-foo-baz foo)
	(let ((a (foo-baz-a foo))
	      (b (foo-baz-b foo)))
	  (g foo a b)))
       ((is-foo-bop foo)
	(h foo))
       (t (error "No match in adt-case."))))
   (adt-case foo some-expression
     ((bar a) 
      (format t "OK")
      (f foo a))
     ((baz a b)
      (g foo a b))
     ((bop)
      (h foo))))

  (assert-expands
   '(defstruct (foo (:predicate is-foo))
     (bar nil :read-only t)
     (baz nil :read-only t))
   (defadt1 foo bar baz))

  (assert-expands
   '(let ((foo expr)
	  (a (foo-a foo))
	  (b (foo-b foo)))
      (side-effect a foo b)
      (main-val b a foo))
   (match-adt1 (foo a b) expr
     (side-effect a foo b)
     (main-val b a foo)))
)