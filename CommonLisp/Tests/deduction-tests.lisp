(defpackage :deduction-tests
  (:use :cl :lisp-unit :testing-utilities :deduction))
(in-package :deduction-tests)

(define-test one-pattern
  (clear-db)
  (<- (reach ?x ?x))

  (assert-eql nil (deduce '() '(reach a a)))
  (clear-db))

(define-test foo
  (clear-db)
  (<- (arrow a b))
  (assert-eql nil (deduce '() '(arrow a b)))
  (clear-db))

(define-test reachable
  (clear-db)
  (<- (reach ?x ?x))
  (<- (reach ?x ?y) (arrow ?x ?z) (reach ?z ?y))
  (<- (arrow a b))
  (<- (arrow a c))
  (<- (arrow b d))
  (<- (arrow c d))
  (<- (goal1 ?y) (reach c ?y) (reach ?y b))
  (<- (goal2 ?y) (reach c ?y) (reach b ?y))

  (assert-equal 'failed (deduce '(?x) '(goal1 ?x)))
  (assert-equal '() (deduce '() '(reach a d)))
  (assert-equal '(d) (deduce '(?z) '(goal2 ?z)))
  (clear-db))

