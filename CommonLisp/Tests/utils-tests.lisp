(defpackage :utils-tests
  (:use :cl :lisp-unit :utils :testing-utilities))
(in-package :utils-tests)

(define-test utils-tests
  (assert-equal 3 (assoc-lookup 'b '((a . 1) (b . 3) (c . 0))))
  (assert-error 'error (assoc-lookup 'd '((a . 1) (b . 2))))

  (assert-true (has-duplicate-names '(g d a c a b f e)))
  (assert-true (has-duplicate-names '(g d b c a b f e)))
  (assert-false (has-duplicate-names '(g d c a b f e)))
  (assert-error 'error (has-duplicate-names '(c m 2 f a)))

  (assert-equal '((a 1) (b 2) (c 3))
		(zip '(a b c) '(1 2 3)))
  (assert-equal '((a 1 "eh") (b 2 "bee") (c 3 "sea"))
		(zip '(a b c) '(1 2 3) '("eh" "bee" "sea")))

  (assert-equal
"ind-a0
ind-a1
   ind-b
      ind-c1
      ind-c2
   ind-d
      ind-e
ind-f
"
    (with-output-to-string (s)
      (let ((*indent-amount* 3)
	    (*fmt-ostream* s))
	(fmt "ind-~a0" #\a)
	(fmt "ind-~a1" #\a)
	(indent
	  (fmt "ind-~a" #\b)
	  (indent
            (fmt "ind-~a1" #\c)
            (fmt "ind-~a2" #\c))
	  (fmt "ind-~a" #\d)
	  (indent
	    (fmt "ind-~a" #\e)))
	(fmt "ind-~a" #\f))))

  (assert-equal
    (string #\Newline)
    (with-output-to-string (s)
      (let ((*indent-amount* 4)
	    (*fmt-ostream* s))
	(fmt-blank-line))))

  (assert-equal "abeesead" (strcat "a" "bee" "sea" "d"))
  (assert-equal "foo
bar
baz
"
    (strcat-lines "foo" "bar" "baz"))

  (assert-equal
    '(macrolet
       ((expect1 (a-key a b-key b =>-key c)
	  (assert (eq :a a-key))
	  (assert (eq :b b-key))
	  (assert (eq '=> =>-key))
	  `(,a ,b ,c)))
       (expect1 :a format :b t => "foo" )
       (expect1 :a fmt :b "~a = 2" => 'bar))
    (macroexpand-1
      '(let-test-macros ((expect1 (a b => c) `(,a ,b ,c)))
	 (expect1 :a format :b t => "foo")
	 (expect1 :a fmt :b "~a = 2" => 'bar))))
      
  (assert-equal
    '(macrolet
       ((expect1 (a-key a b-key b =>-key &rest c)
	  (assert (eq :a a-key))
	  (assert (eq :b b-key))
	  (assert (eq '=> =>-key))
	  `(,a ,b ,@c)))
       (expect1 :a format :b t => "foo" "fum")
       (expect1 :a fmt :b "~a = 2" => 'bar 'baz))
    (macroexpand-1
      '(let-test-macros ((expect1 (a b => &rest c) `(,a ,b ,@c)))
	 (expect1 :a format :b t => "foo" "fum")
	 (expect1 :a fmt :b "~a = 2" => 'bar 'baz))))      
)
