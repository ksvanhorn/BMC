(load "../axiom-utils.lsp")

(define-test symbol->var-tests
  (assert-equal '?x (symbol->variable 'x))
  (assert-equal '|?foo| (symbol->variable '|foo|)))

(define-test occurs-tests
  (assert-true (occurs 'a '(= (+ x y) (/ 12 a))))
  (assert-false (occurs 'b '(< (^ x z) (* a y))))
  (assert-true (occurs '?x '(all (?x) (= ?x ?x)))))

(define-test defaxiom-tests
  (assert-expands
    '(<=> (foo) (body z))
    (defaxiom foo () (body z)))
  (assert-expands
    '(all (?x) (<=> (bar ?x) (hoo ?x (haw ?x y))))
    (defaxiom bar (x) (hoo x (haw x y))))
  (assert-expands
    '(all (?a ?b) (<=> (baz ?a ?b) (= (^ ?a 2) (/ ?b 7))))
    (defaxiom baz (a b) (= (^ a 2) (/ b 7))))

  (assert-error 'error (defaxiom-xform 3 nil t))
  (assert-error 'error (defaxiom-xform foo 5 t))
  (assert-error 'error (defaxiom-xform foo '(a . b) t))
  (assert-error 'error (defaxiom-xform foo '(a 5) t))
  (assert-error 'error (defaxiom-xform foo '(a b a) t))
  (assert-error 'error (defaxiom-xform foo '(a b) (< 0 ?b)))
)

#|
(define-test defdistr-tests
  (assert-expands
   '(list (defaxiom U-precond () t)
	  (defaxiom U-postcond (x) (and (< 0 x) (< x 1)))
 
   (defdistr U (x)
     t
     (and (< 0 x) (< x 1))
     1))
  #| 
#|
  (assert-equal
   '(all (?a ?b) (<=> (precond-foo ?a ?b)
		      (and (is-realp ?a) (is-integerp ?b))))
   (macro-expand1 '(def foo (a b)
		     (and (is-realp a) (is-integerp b)))))
|#

#|
     (all (?x ?a ?b) (<=> (postcond-foo ?x ?a ?b)
			  (is-realp ?x)))
     (all (?x ?a ?b) (=> (precond-foo ?a ?b)
			 (or (postcond-foo ?x ?a ?b) (= (foo ?x ?a ?b) 0))))
     (all (?x ?a ?b)
	  (=> (and (precond-foo ?a ?b) (postcond-foo ?x ?a ?b))
	      (= (foo ?x ?a ?b) (^ (/ ?x ?a) ?b)))))	      
   (macro-expand1
    '(defdistr foo (x a b)
       (and (is-realp a) (is-integerp b))
       (is-realp x)
       (^ (/ x a) b))))
|#


		