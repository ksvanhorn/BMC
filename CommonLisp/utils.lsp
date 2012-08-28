(in-package :utils)

(defun starts-with (symbol sexpr)
  (and (consp sexpr) (eq symbol (car sexpr))))

(defun assoc-lookup (key assoc-list)
  (let ((x (assoc key assoc-list)))
    (if (null x)
	(error (format nil "Assoc lookup failed to find ~w" key))
        (cdr x))))

(defun zip (&rest lists) (apply #'mapcar #'list lists))
