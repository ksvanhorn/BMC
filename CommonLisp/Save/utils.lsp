(defpackage :utils
  (:use :common-lisp)
  (:export :read-file :to-kw
	   :assoc-lookup
	   :indent :with-indent-size
	   :fmt :with-fmt-out
	   :strcat
	   :zip :flatten :map-range))
(in-package :utils)

; read utils

(defun read-file (ifname)
  (with-open-file (is ifname)
    (let ((*package* (find-package "KEYWORD")))
      (read is))))

#|
(defun read-file-preserve-case (ifname)
  (with-open-file (is ifname)
    (let ((*readtable* (copy-readtable nil))
	  (*package* (find-package "KEYWORD")))
      (setf (readtable-case *readtable*) :preserve)
      (read is))))
|#

; symbol utils

(defun to-kw (e)
  (cond ((consp e) (cons (to-kw (car e)) (to-kw (cdr e))))
	((null e) nil)
	((symbolp e) (intern (symbol-name e) "KEYWORD"))
	(t e)))

; assoc utils

(defun assoc-lookup (key assoc-list)
  (let ((x (assoc key assoc-list)))
    (if (null x)
	(error (format nil "Assoc lookup failed to find ~w" key))
        (cdr x))))

; Indentation utils

(defmacro indent (&rest body)
  `(let ((*indent-level* (1+ *indent-level*)))
     ,@body))

(defun print-indent (&optional (s *standard-output*))
  (dotimes (i (* *indent-amt* *indent-level*)) (princ #\Space s)))

(defmacro with-indent-size (n &rest body)
  `(let ((*indent-amt* ,n)) ,@body))

(defparameter *indent-amt* 4)
(defparameter *indent-level* 0)

; Output utils

(defun fmt (&rest args)
  (print-indent *fmt-ostrm*)
  (apply #'format *fmt-ostrm* args)
  (format *fmt-ostrm* "~%"))

(defmacro with-fmt-out (os &rest body)
  `(let ((*fmt-ostrm* ,os)) ,@body))

(defparameter *fmt-ostrm* *standard-output*)

; string utils

(defun strcat (&rest args)
  (apply #'concatenate 'string args))

; list utils

(defun zip (&rest lists) (apply #'mapcar #'list lists))

(defun flatten (lists) (apply #'append lists))

(defun map-range (beg end fct)
  (do ((x nil (cons (funcall fct i) x))
       (i beg (1+ i)))
      ((< end i) (reverse x))))
