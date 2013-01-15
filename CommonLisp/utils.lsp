(in-package :utils)

(defmacro fn (params &body body)
  `(lambda ,params ,@body))

(defmacro λ (params &body body)
  `(lambda ,params ,@body))

(define-symbol-macro · funcall)

(defmacro flet* (defs &body body) `(labels ,defs ,@body))

(defmacro while (test &body body)
  `(loop while ,test do ,@body))

(defmacro dolist-inter ((var lst) action between)
  (let ((past-first (gensym)))
    `(let ((,past-first nil))
       (dolist (,var ,lst)
	 (when ,past-first ,between)
	 (setf ,past-first t)
	 ,action))))

(defun starts-with (symbol sexpr)
  (and (consp sexpr) (eq symbol (car sexpr))))

(defun assoc-lookup (key assoc-list)
  (let ((x (assoc key assoc-list)))
    (if (null x)
	(error (format nil "Assoc lookup failed to find ~w" key))
        (cdr x))))

(defun is-list-of-length (n x)
  (or (and (= 0 n) (eq nil x))
      (and (consp x) (is-list-of-length (- n 1) (cdr x)))))

(defun has-no-duplicates (x) (= (length x) (length (remove-duplicates x))))

(defun has-duplicates (x) (/= (length x) (length (remove-duplicates x))))

(defun zip (&rest lists) (apply #'mapcar #'list lists))

(defun list->pair-list (the-list)
  (if (null the-list)
      '()
    (destructuring-bind (a b . rest) the-list
      (cons (cons a b) (list->pair-list rest)))))

(defun append-mapcar (fct lst)
  (apply #'append (mapcar fct lst)))

(defun int-range (lo hi)
  (let ((result '()))
    (loop for i from hi downto lo do
      (push i result))
    result))

(defun strcat (&rest args)
  (apply #'concatenate 'string args))

(defun strcat-lines (&rest args)
  (format nil "~{~a~%~}" args))

(defun read-file (ifname)
  (with-open-file (is ifname) (read-stream is)))

(defun read-stream (is)
  (let ((*read-default-float-format* 'long-float)
	(*package* (find-package 'symbols)))
    (read is)))

(defun fdebug (format &rest args)
  (apply #'format *standard-output* (strcat "~&" format "~%") args)
  (finish-output *standard-output*))

(defun bmc-symb (s) (intern s))

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

; From Paul Graham's book _On Lisp_
(defmacro alambda (parms &body body)
  `(flet* ((self ,parms ,@body))
     #'self))

(defun rethrow-error (x format-control &rest format-args)
  (let* ((format-control-x (simple-condition-format-control x))
	 (format-args-x (simple-condition-format-arguments x))
	 (full-format-control (strcat format-control format-control-x))
	 (full-format-args (append format-args format-args-x)))
    (apply #'error full-format-control full-format-args)))
