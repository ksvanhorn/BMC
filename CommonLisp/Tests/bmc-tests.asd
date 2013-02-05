;;;; bmc-tests.asd

(asdf:defsystem #:bmc-tests
  :serial t
  :description "Describe bmc-tests here"
  :author "Kevin S. Van Horn"
  :license "GPL v2"
  :depends-on (#:bmc
               #:lisp-unit)
  :components ((:file "package")
               (:file "bmc-tests")))

