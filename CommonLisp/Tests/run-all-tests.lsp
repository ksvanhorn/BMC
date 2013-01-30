(ql:quickload "lisp-unit")
(ql:quickload "alexandria")
(load "code-to-test")
(load "Tests/testing-utilities")
(in-package :cl-user)

(defmacro runtests (name)
  (let* ((name-s (symbol-name name))
	 (pkg-path (utils:strcat "Tests/" (string-downcase name-s) "-tests"))
	 (pkg-name (intern (utils:strcat name-s "-TESTS"))))
    `(progn
       (format t "--- TESTING ~A ---~%" ',pkg-name)
       (load ,pkg-path)
       (let ((test-results (lisp-unit:run-tests :all ',pkg-name)))
	 (incf num-failures (length (lisp-unit:failed-tests test-results)))
	 (incf num-errors (length (lisp-unit:error-tests test-results)))))))

(setq lisp-unit:*print-failures* t)
(setq lisp-unit:*print-errors* t)
(defparameter num-failures 0)
(defparameter num-errors 0)

(defvar tests
  '(variables adt utils lazy expr type-inference model mcimpl simplify prove
    compile))

(dolist (name tests)
  (let* ((name-s (symbol-name name))
	 (pkg-path (utils:strcat "Tests/" (string-downcase name-s) "-tests"))
	 (pkg-name (utils:strcat name-s "-TESTS")))
    (format t "--- TESTING ~A ---~%" pkg-name)
    (load pkg-path)
    (let ((test-results (lisp-unit:run-tests :all pkg-name)))
      (incf num-failures (length (lisp-unit:failed-tests test-results)))
      (incf num-errors (length (lisp-unit:error-tests test-results))))))

(cond
  ((zerop (+ num-failures num-errors))
   (format t "====================~%")
   (format t "OK.~%"))
  (t
   (format t "********************~%")
   (format t "Total failures: ~d~%" num-failures)
   (format t "Total errors: ~d~%" num-errors)))
