(defun check-model (mdl)
  (if (not (starts-with :model mdl))
    "Model should be a list starting with ':model'"
    (let ((rest (cdr mdl)))
      (if (/= 4 (length rest))
	"Model should have four sections."
	(let ((xargs (check-args (first rest)))
	      (xreqs (check-reqs (second rest)))
	      (xvars (check-vars (third rest)))
	      (xrels (check-rels (fourth rest))))
	  (if (and (null xargs) (null xreqs) (null xvars) (null xrels))
	    nil
	    (list :model xargs xreqs xvars xrels)))))))

(defun check-model-has-sections (sections mdl)
  (unless (and (listp mdl) (eq :model (first mdl)))
    (error "Model should start with ':model'"))
  (let ((rest (cdr mdl)))
    (unless (and (listp rest) (= 4 (length rest)))
      (error "Model should have 4 sections"))
    (dotimes (i (length sections))
      (check-has-header (1+ i) (nth i sections) (nth i rest)))))

(defun check-has-header (n hdr x)
  (unless (and (listp x) (not (null x)) (eq (car x) hdr))
    (error (format nil "Section ~a of model should have header '~a'." n hdr))))

(defun check-args (x) (check-section x :args #'check-decl))
(defun check-reqs (x) (check-section x :reqs #'check-expr))
(defun check-vars (x) (check-section x :vars #'check-decl))
(defun check-body (x) (check-section x :body #'check-rel))

(defun check-section (x header check-element)
  (if (not (starts-with header x))
    (format nil "Section must start with '~a'" header)
    (let ((elts (mapcar check-element (cdr x))))
      (if (every null v) nil (cons header elts)))))

(defun check-decl (x)
  (if (not (and (is-list x) (= 2 (length x))))
    "Declaration should have form (<var> <type>)"
    (let ((d (list (check-var (first x)) (check-typ (second x)))))
      (if (every null d) nil d))))

(defun check-var (x)
  (if (symbolp x) nil "Invalid name; expected a symbol"))

(defun check-typ (x)
  (cond ((symbolp typ) (check-scalar-type typ))
	((listp typ) (check-array-type typ))
	(t "Invalid type"))

(defun check-scalar-type (x)
  (if (member x *scalar-types*) nil "Invalid scalar type"))

(defun check-array-type (x)
  (if (not (and (is-list x) (<= 2 (length x))))
    "Invalid array type"
    (let* ((etyp (check-scalar-type (car x)))
	   (dims (mapcar #'check-expr (cdr x)))
	   (ch (cons etyp dims)))
      (if (every null ch) nil ch))))

(defun check-rel (x)
  (if (not (is-list x))
    "Invalid relation"
    (case (first x)
	  (:<- (check-det-rel (cdr x)))
	  (:~ (check-stoch-rel (cdr x)))
	  (:block (check-block-rel (cdr x)))
	  (:if (check-if-rel (cdr x)))
	  (:for (check-loop-rel (cdr x)))
	  (t "Invalid relation"))))

(defun check-det-rel (y)
  (if (/= 2 (length y))
    "Invalid deterministic relation"
    (let* ((lhs (check-rel-var (first y)))
	   (rhs (check-expr (second y)))
	   (ch (list lhs rhs)))
      (if (every null ch) nil ch))))

(defun is-list (x)
  (loop
    (when (null x) (return t))
    (unless (consp x) (return nil))
    (setf x (cdr x))))

(defun starts-with (s x)
  (and (consp x) (eq s (car x)) (is-list x)))

