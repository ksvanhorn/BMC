(defpackage :utils
  (:use :common-lisp)
  (:export :read-file-upcasing-only :read-file-preserve-case
	   :assoc-lookup
	   :indent :print-indent :with-indent-amt
	   :fmt :with-fmt-out
	   :strcat))
(in-package :utils)

; read utils

(defun read-file-upcasing-only (symbols ifname)
  (force-upcase symbols (read-file-preserve-case ifname)))

(defun read-file-preserve-case (ifname)
  (with-open-file (is ifname)
    (let ((*readtable* (copy-readtable nil))
	  (*package* *package*))
      (setf (readtable-case *readtable*) :preserve)
      (in-package "KEYWORD")
      (read is))))

(defun symbol-downcase (s)
  (intern (string-downcase s) "KEYWORD"))

(defun symbol-upcase (s)
  (intern (string-upcase s) "KEYWORD"))

(defun force-upcase (symbols sexpr)
  (dolist (s symbols)
    (setf sexpr (subst (symbol-upcase s) (symbol-downcase s) sexpr)))
  sexpr)

; assoc utils

(defun assoc-lookup (key assoc-list)
  (let ((x (assoc key assoc-list)))
    (if (null x)
	(error (format nil "Assoc lookup failed to find ~a" key))
        (cdr x))))

; Indentation utils

(defmacro indent (&rest body)
  `(let ((*indent-level* (1+ *indent-level*)))
     ,@body))

(defun print-indent (&optional (s *standard-output*))
  (dotimes (i (* *indent-amt* *indent-level*)) (princ #\Space s)))

(defmacro with-indent-amt (n &rest body)
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
