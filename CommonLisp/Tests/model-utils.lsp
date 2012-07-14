(load "../model-utils.lsp")

(define-test extract-tests
  (let ((mdl '(:model (:args a1 a2) (:reqs r1) (:vars v1 v2 v3) (:body b1 b2))))
    (assert-equal '(a1 a2) (extract-args mdl))
    (assert-equal '(r1) (extract-reqs mdl))
    (assert-equal '(v1 v2 v3) (extract-vars mdl))
    (assert-equal '(b1 b2) (extract-body mdl))))

(define-test decl-tests
  (let ((decl '(var typ)))
    (assert-equal 'var (decl-var decl))
    (assert-equal 'typ (decl-typ decl))))

(define-test type-tests
  (let ((scalar 'some-type)
	(array1 '(typ1 n1))
	(array2 '(typ2 m1 m2)))

    (assert-equal 'scalar (type-class scalar))
    (assert-equal 'array (type-class array1))
    (assert-equal 'array (type-class array2))

    (assert-equal 'typ1 (elem-type array1))
    (assert-equal 'typ2 (elem-type array2))

    (assert-equal '(n1) (type-dims array1))
    (assert-equal '(m1 m2) (type-dims array2))))

(define-test rel-tests
  (let ((rel-deterministic '(<- vard val))
	(rel-stochastic '(~ vars distr))
	(rel-if-then '(:if test1 rel))
	(rel-if-then-else '(:if test2 rel-true rel-false))
	(rel-loop '(:for i (lo hi) body)))

    (assert-equal 'deterministic (rel-class rel-deterministic))
    (assert-equal 'stochastic (rel-class rel-stochastic))
    (assert-equal 'if-then (rel-class rel-if-then))
    (assert-equal 'if-then-else (rel-class rel-if-then-else))
    (assert-equal 'loop (rel-class rel-loop))

    (assert-equal 'vard (rel-var rel-deterministic))
    (assert-equal 'val (rel-val rel-deterministic))

    (assert-equal 'vars (rel-var rel-stochastic))
    (assert-equal 'distr (rel-distr rel-stochastic))

    (assert-equal 'test1 (rel-if-condition rel-if-then))
    (assert-equal 'rel (rel-true-branch rel-if-then))

    (assert-equal 'test2 (rel-if-condition rel-if-then-else))
    (assert-equal 'rel-true (rel-true-branch rel-if-then-else))
    (assert-equal 'rel-false (rel-false-branch rel-if-then-else))

    (assert-equal 'i (rel-loop-var rel-loop))
    (assert-equal '(lo hi) (rel-loop-bounds rel-loop))
    (assert-equal 'body (rel-loop-body rel-loop))
    (assert-equal 'lo (bounds-lo '(lo hi)))
    (assert-equal 'hi (bounds-hi '(lo hi)))))

(define-test expr-tests
  (let ((app-expr '(fct arg1 arg2))
        (num-expr 37)
        (var-expr 'some-variable)
        (array-app-expr-1 '(@ v i))
        (array-app-expr-2 '(@ w i1 i2)))

    (assert-equal 'funct-app (expr-class app-expr))
    (assert-equal 'literal-num (expr-class num-expr))
    (assert-equal 'variable (expr-class var-expr))
    (assert-equal 'array-app (expr-class array-app-expr-1))
    (assert-equal 'array-app (expr-class array-app-expr-2))

    (assert-equal 'fct (op app-expr))
    (assert-equal '(arg1 arg2) (args app-expr))

    (assert-equal 'v (array-op array-app-expr-1))
    (assert-equal '(i) (array-args array-app-expr-1))

    (assert-equal 'w (array-op array-app-expr-2))
    (assert-equal '(i1 i2) (array-args array-app-expr-2))))

(define-test array-expr-tests
  (let ((simple-args (array-args '(@ v i)))
	(complex-args (array-args '(@ v :all (:range lo hi) i))))
    (assert-equal 'index (index-class (first simple-args)))
    (assert-equal 'all (index-class (first complex-args)))
    (assert-equal 'range (index-class (second complex-args)))
    (assert-equal 'index (index-class (third complex-args)))
    (let ((range-arg (second complex-args)))
      (assert-equal 'lo (range-lo range-arg))
      (assert-equal 'hi (range-hi range-arg)))))


;; expressions?
