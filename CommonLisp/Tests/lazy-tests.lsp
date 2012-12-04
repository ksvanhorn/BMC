(defpackage :lazy-tests
  (:use :cl :lisp-unit :lazy :utils))
(in-package :lazy-tests)

(define-test lazy-tests
  (assert-eql 1 (lcar (lcons 1 nil)))
  (assert-equal nil (lcdr (lcons 1 nil)))
  (let ((x "foo"))
    (assert-eql x (lcar (lcons x nil))))
  (let* ((two "two")
	 (x (lcons "one" (lcons two (lcons "three" nil)))))
    (assert-eql two (lcar (lcdr x))))

  (assert-equal nil (list->lazy nil))
  (assert-equal nil (lazy->list nil))

  (let ((x1 (list->lazy '(1)))
	(x2 (list->lazy '(1 2))))
    ; repeats make sure lcdr has no observable side-effects
    (dotimes (dummy 2)
      (assert-equal 1 (lcar x1))
      (assert-equal nil (lcdr x1)))
    
    ; repeats make sure lcdr has no observable side-effects
    (dotimes (dummy 2)
      (assert-equal 1 (lcar x2))
      (assert-equal 2 (lcar (lcdr x2)))
      (assert-equal nil (lcdr (lcdr x2)))))

  (flet* ((ints-from (n)
	    (lcons n (ints-from (1+ n)))))
    (let ((x (ints-from 1))
	  y)
      (assert-eql 1 (lcar x))
      (setf x (lcdr x))
      (assert-eql 2 (lcar x))
      (setf x (lcdr x))
      (assert-eql 3 (lcar x))))

  (flet* ((int-rng (m n)
	    (if (= m n)
	      nil
	      (lcons m (int-rng (1+ m) n)))))
    (assert-equalp '() (lazy->list (lappend)))
    (assert-equalp '() (lazy->list (lappend nil)))
    (assert-equalp '(1) (lazy->list (lappend (int-rng 1 2))))
    (assert-equalp '(1) (lazy->list (lappend (int-rng 1 2) nil)))
    (assert-equalp '(1) (lazy->list (lappend nil (int-rng 1 2))))
    (assert-equalp
      '(1 2 3)
      (lazy->list (lappend (int-rng 1 2) (int-rng 2 4))))
    (assert-equalp
      '(1 2 3)
      (lazy->list (lappend (int-rng 1 2) nil (int-rng 2 4))))
    (assert-equalp
      '(1 2 3 4 5 6)
      (lazy->list (lappend (int-rng 1 2) (int-rng 2 4) (int-rng 4 7)))))

#|
  (assert-equalp
    nil
    (lmapcar #'null nil))
  (assert-equalp
    '(2)
    (lmapcar #'1+ (list->lazy '(1))))
  (assert-equalp
    '(2 3 4)
    (lmapcar #'1+ (list->laz '(1 2 3))))
|#
)