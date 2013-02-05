;;;; bmc.asd

(asdf:defsystem #:bmc
  :serial t
  :description "Describe bmc here"
  :author "Kevin S. Van Horn"
  :license "GPL v2"
  :depends-on (#:alexandria
               #:cl-fad)
  :components ((:file "package")
               (:file "bmc")))

