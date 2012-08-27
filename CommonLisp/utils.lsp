(in-package :utils)

(defun starts-with (symbol sexpr)
  (and (consp sexpr) (eq symbol (car sexpr))))
