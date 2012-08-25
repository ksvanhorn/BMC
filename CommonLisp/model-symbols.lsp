(defpackage :model-symbols
  (:use :common-lisp)
  (:export :+model-symbols+))

(defconstant +model-symbols+
  '(:model :args :reqs :vars :body :<- :~ :for :if :block :range :all))
