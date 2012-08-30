(use-package :utils)

(define-test utils-tests
  (assert-true (starts-with 'a '(a)))
  (assert-true (starts-with 'b '(b 1 c)))
  (assert-false (starts-with 'c '()))
  (assert-false (starts-with 'c 'c))
  (assert-false (starts-with 'c 'a))

  (assert-equal 3 (assoc-lookup 'b '((a . 1) (b . 3) (c . 0))))
  (assert-error 'error (assoc-lookup 'd '((a . 1) (b . 2))))

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

  (assert-equal '(3 4 5) (int-range 3 5))
  (assert-equal '(5) (int-range 5 5))
  (assert-equal '() (int-range 4 3))
	
)