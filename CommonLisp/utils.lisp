(defpackage :utils
  (:use :cl :iterate)
  (:export :assoc-lookup
	   :compound-symbol
	   :fdebug 
	   :flet*
	   :fmt
	   :fmt-blank-line
	   :fn
	   :has-duplicate-names
	   :indent
	   :read-file
	   :rethrow-error
	   :strcat
	   :strcat-lines
	   :zip 
	   :*fmt-ostream*
	   :*indent-amount*
	   :*indent-level*))
(in-package :utils)

(defun read-file (ifname)
  (with-open-file (is ifname) (read-stream is)))

(defun read-stream (is)
  (let ((*read-default-float-format* 'long-float)
	(*package* (find-package 'symbols)))
    (read is)))

(defmacro fn (params &body body)
  `(lambda ,params ,@body))

(defmacro flet* (defs &body body) `(labels ,defs ,@body))

(defun assoc-lookup (key assoc-list)
  (let ((x (assoc key assoc-list)))
    (if (null x)
	(error (format nil "Assoc lookup failed to find ~w" key))
        (cdr x))))

(defun has-duplicate-names (x)
  (setq x (sort (copy-list x) #'string<))
  (iter (for s in x)
	(for sp previous s)
	(when (eql s sp)
	  (return-from has-duplicate-names t)))
  nil)

(defun zip (&rest lists) (apply #'mapcar #'list lists))

(defun strcat (&rest args)
  (apply #'concatenate 'string args))

(defun strcat-lines (&rest args)
  (format nil "~{~a~%~}" args))

(defun fdebug (format &rest args)
  (apply #'format *standard-output* (strcat "~&" format "~%") args)
  (finish-output *standard-output*))

(defun compound-symbol (x y)
  (intern (format nil "~a-~a" x y) (symbol-package x)))

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

(defun fmt-blank-line ()
  (format *fmt-ostream* "~%"))

(defparameter *fmt-ostream* *standard-output*)

(defun rethrow-error (x format-control &rest format-args)
  (let* ((format-control-x (simple-condition-format-control x))
	 (format-args-x (simple-condition-format-arguments x))
	 (full-format-control (strcat format-control format-control-x))
	 (full-format-args (append format-args format-args-x)))
    (apply #'error full-format-control full-format-args)))
