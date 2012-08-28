(load "packages")
(load "utils")
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
)
