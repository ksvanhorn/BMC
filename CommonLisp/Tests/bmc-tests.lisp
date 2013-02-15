(defpackage #:bmc-tests
  (:use #:cl #:utils #:lisp-unit)
  (:export #:test-packages))

(in-package #:bmc-tests)

(defvar *package-tests*
  '(symbols variables adt utils expr type-inference model mcimpl simplify prove compile))

(defun test-one-package (name)
  (setq name (string name))
  (format t "--- TESTING ~a ---~%" name)
  (let ((test-results (run-tests :all (strcat name "-TESTS"))))
    (print-failures test-results)
    (print-errors test-results)
    (values (length (failed-tests test-results))
	    (length (error-tests test-results)))))

(defun print-summary (num-failures num-errors)
  (cond
    ((and (zerop num-failures) (zerop num-errors))
     (format t "====================~%")
     (format t "OK.~%"))
    (t
     (format t "********************~%")
     (format t "FAILED.~%")
     (format t "Total failures: ~d~%" num-failures)
     (format t "Total errors: ~d~%" num-errors))))

(defmacro test-packages (&rest packages)
  (when (equal '(:all) packages)
    (setf packages *package-tests*))
  (let (;(*print-failures* t)
	(num-failures 0)
	;(*print-errors* t)
	(num-errors 0))
    (dolist (name packages)
      (multiple-value-bind (f e) (test-one-package name)
	(incf num-failures f)
	(incf num-errors e)))
    (print-summary num-failures num-errors)))
