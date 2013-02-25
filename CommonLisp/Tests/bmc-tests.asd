;;;; bmc-tests.asd

(asdf:defsystem #:bmc-tests
  :serial t
  :description "Unit tests for system bmc"
  :author "Kevin S. Van Horn"
  :license "GPL version 2"
  :depends-on (#:bmc
               #:lisp-unit
	       #:iterate)
  :components ((:file "testing-utilities")
	       (:file "utils-tests")
	       (:file "deduction-tests")
	       (:file "symbols-tests")
	       (:file "variables-tests")
	       (:file "adt-tests")
	       (:file "expr-tests")
	       (:file "type-inference-tests")
	       (:file "model-tests")
	       (:file "mcimpl-tests")
	       (:file "simplify-tests")
	       (:file "prove-tests")
	       (:file "compile-tests")
               (:file "bmc-tests")))

