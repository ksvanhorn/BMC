(in-package :utils)

(defun starts-with (symbol sexpr)
  (and (consp sexpr) (eq symbol (car sexpr))))

(defun assoc-lookup (key assoc-list)
  (let ((x (assoc key assoc-list)))
    (if (null x)
	(error (format nil "Assoc lookup failed to find ~w" key))
        (cdr x))))

(defun zip (&rest lists) (apply #'mapcar #'list lists))

(defun int-range (lo hi)
  (let ((result '()))
    (loop for i from hi downto lo do
      (push i result))
    result))

(defun strcat (&rest args)
  (apply #'concatenate 'string args))

(defun read-file (ifname)
  (with-open-file (is ifname)
    (read is)))

; Indented output utils

(defmacro indent (&rest body)
  `(let ((*indent-level* (1+ *indent-level*)))
     ,@body))

(defun print-indent (&optional (s *standard-output*))
  (dotimes (i (* *indent-amount* *indent-level*)) (princ #\Space s)))

(defparameter *indent-amount* 4)
(defparameter *indent-level* 0)

(defun fmt (&rest args)
  (print-indent *fmt-ostream*)
  (apply #'format *fmt-ostream* args)
  (format *fmt-ostream* "~%"))

(defparameter *fmt-ostream* *standard-output*)

; TODO: add def for common pattern (apply #'append (mapcar <fct> <list>))