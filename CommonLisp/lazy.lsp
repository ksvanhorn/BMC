(in-package :lazy)

(defstruct (thunk
	     (:constructor make-thunk (closure))
	     (:predicate is-thunk))
  closure)

(defmacro delay (x) `(make-thunk (lambda () ,x)))

(defun force (x)
  (if (is-thunk x)
    (funcall (thunk-closure x))
    x))

(defmacro lcons (hd tl)
  `(cons ,hd (delay ,tl)))

(defun lcar (x) (car x))

(defun lcdr (x)
  (setf (cdr x) (force (cdr x))))

(defun lappend (&rest args)
  (if (null args)
    '()
    (destructuring-bind (a . rest) args
      (if (null a)
	(apply #'lappend rest)
	(lcons (lcar a)
	      (apply #'lappend (cons (lcdr a) rest)))))))

(defun list->lazy (x)
  (if (null x)
    nil
    (lcons (car x) (list->lazy (cdr x)))))

(defun lazy->list (lx)
  (do ((x nil))      ; binding
      ((null lx)     ; termination test
       (reverse x))  ; return value
    (setf x (cons (lcar lx) x))
    (setf lx (lcdr lx))))
