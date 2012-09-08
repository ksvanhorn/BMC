(defun is-atomic (e)
  (or (symbolp e) (numberp e)))

(defun is-application (e)
  (and (consp e) (not (eq 'lambda (car e)))))

(defun is-list-of-length (n x)
  (or (and (= 0 n) (eq nil x))
      (and (consp x) (is-list-of-length (- n 1) (cdr x)))))

(defun is-lambda-expr (e)
  (and (is-list-of-length 3 e) (eq 'lambda (first e)) (symbolp (second e))))

(defun occurs-free (x e)
  (cond
    ((is-atomic e)
     (eq x e))
    ((is-application e)
     (or (occurs-free x (car e))
	 (occurs-free x (cdr e))))
    ((is-lambda-expr e)
     (destructuring-bind (_ var body) e
       (and (not (eq x var)) (occurs-free x body))))
    (t (error "Invalid expression: ~w." e))))

(defun xform (e)
  (cond
    ((is-atomic e)
     e)
    ((is-application e)
     (cons (xform (car e)) (xform (cdr e))))
    ((is-lambda-expr e)
     (destructuring-bind (_ var-x body) e
       (cond ((eq var-x body) ; \ x x
	      'I)
	     ((and (is-application body) (eq var-x (cdr body))) ; \ x (f . x)
	      (xform (car body)))
	     ((not (occurs-free var-x body))
	      (cons 'K (xform body)))
	     ((is-lambda-expr body)
	      (xform `(lambda ,var-x ,(xform body))))
	     ((is-application body)
	      (destructuring-bind (left . right) body
		(if (occurs-free var-x left)
                  (if (occurs-free var-x right)
		    (cons (cons 'S (xform `(lambda ,var-x ,left)))
			  (xform `(lambda ,var-x ,right)))
		    (cons (cons 'C (xform `(lambda ,var-x ,left)))
			  (xform right)))
		  (cons (cons 'B (xform left))
			(xform `(lambda ,var-x ,right))))))
	     (t (error "Invalid expression: ~w." body)))))
    (t (error "Invalid expression: ~w." e))))

(defun is-I-x (e)
  (and (consp e) (eq 'I (car e))))

(defun is-K-x-y (e)
  (and (consp e) (consp (car e)) (eq 'K (caar E))))

(defun is-S-x-y-z (e)
  (and (consp e) (consp (car e)) (consp (caar e)) (eq 'S (caaar e))))

(defun is-B-x-y-z (e)
  (and (consp e) (consp (car e)) (consp (caar e)) (eq 'B (caaar e))))

(defun is-C-x-y-z (e)
  (and (consp e) (consp (car e)) (consp (caar e)) (eq 'C (caaar e))))

(defun reduce-SKIBC (e)
  (cond
    ((is-atomic e)
     e)
    ((is-I-x e)
     (destructuring-bind (_ . x) e
       (reduce-SKIBC x)))
    ((is-K-x-y e)
     (destructuring-bind ((_ . x) . y) e
       (reduce-SKIBC x)))
    ((is-S-x-y-z e)
     (destructuring-bind (((_ . x) . y) . z) e
       (reduce-SKIBC (cons (cons x z) (cons y z)))))
    ((is-B-x-y-z e)
     (destructuring-bind (((_ . f) . g) . x) e
       (reduce-SKIBC (cons f (cons g x)))))
    ((is-C-x-y-z e)
     (destructuring-bind (((_ . f) . x) . y) e
       (reduce-SKIBC (cons (cons f y) x))))
    (t
     (cons (reduce-SKIBC (car e)) (reduce-SKIBC (cdr e))))))

(defun reduce-SKIBC-all (e)
  (loop
    (let ((new-e (reduce-SKIBC e)))
      (if (equalp e new-e)
	(return e)
	(setf e new-e)))))

; ((S . ((S . (K . S)) . ((S . (K . (S . (K . ^)))) . +))) . (K . (K . 2)))