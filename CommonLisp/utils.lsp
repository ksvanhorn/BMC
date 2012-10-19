(in-package :utils)

(defmacro fn (params &rest body)
  `(lambda ,params ,@body))

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
  (with-open-file (is ifname)
    (let ((*read-default-float-format* 'long-float))
      (read is))))

(defun fdebug (format &rest args)
  (apply #'format t (strcat "~&" format "~%") args))

(defun compound-symbol (x y)
  (intern (format nil "~a-~a" x y)))

(defun n-symbols-not-in (n excluded &optional (prefix "i"))
  (let* ((gen-next (next-symbol excluded prefix))
	 (result nil))
    (dotimes (i n)
      (push (funcall gen-next) result))
    (reverse result)))

(defun symbol-not-in (excluded &optional (prefix "i"))
  (first (n-symbols-not-in 1 excluded prefix)))

(defun next-symbol (excluded prefix)
  (let ((suffix 0))
    (lambda ()
      (loop
        (incf suffix)
	(let ((sym (intern (strcat prefix (write-to-string suffix)))))
	  (unless (member sym excluded)
	    (return sym)))))))

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

; TODO: add def for common pattern (apply #'append (mapcar <fct> <list>))