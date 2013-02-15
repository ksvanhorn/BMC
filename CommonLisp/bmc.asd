;;;; bmc.asd

(asdf:defsystem #:bmc
  :serial t
  :description "Bayesian Model Compiler"
  :author "Kevin S. Van Horn"
  :license "GPL version 2"
  :depends-on (#:alexandria
	       #:iterate
               #:cl-fad)
  :components ((:file "utils")
	       (:file "symbols")
	       (:file "variables")
	       (:file "adt")
	       (:file "expr")
	       (:file "type-inference")
	       (:file "model")
	       (:file "mcimpl")
	       (:file "prove")
	       (:file "simplify")
	       (:file "compile")
               (:file "bmc")))

