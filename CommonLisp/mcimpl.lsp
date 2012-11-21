(in-package :mcimpl)

(defadt1 mcimpl
  parameters acceptmons expectations updates)

(defun params-names (impl)
  (mapcar #'decl-var (mcimpl-parameters impl)))

(defun read-mcimpl (ifname)
  (sexpr->mcimpl (read-file ifname)))

(defun sexpr->mcimpl (se)
  (unless (and (starts-with :mcimpl se) (is-list-of-length 5 se))
    (error "mcimpl object should have form (:mcimpl parameters acceptmons expectations updates)."))
  (destructuring-bind (se-parameters se-acceptmons se-expectations se-updates)
                      (cdr se)
    (make-mcimpl :parameters (sexpr->parameters se-parameters)
		 :acceptmons (sexpr->acceptmons se-acceptmons)
	         :expectations (sexpr->expectations se-expectations)
	         :updates (sexpr->updates se-updates))))

(defun sexpr->parameters (se)
  (check-parameters-section se)
  (mapcar #'sexpr->decl (rest se)))

(defun sexpr->acceptmons (se)
  (check-acceptmons-section se)
  (flet ((sexpr->mondef (x)
	   (destructuring-bind (name . decls) x
	     (cons name (mapcar #'sexpr->decl decls)))))
    (mapcar #'sexpr->mondef (cdr se))))

(defun check-acceptmons-section (se)
  (unless (starts-with :acceptmons se)
    (error "Acceptance monitors section of mcimpl file should be a list begginning with :acceptmons.")))

(defun sexpr->expectation (se)
  (destructuring-bind (name type expr) se
    (let ((d (sexpr->decl (list name type))))
      (check-expectation-type (decl-typ d))
      (cons d (sexpr->expr expr)))))

(defun check-expectation-type (type)
  (let ((elem-type (adt-case vtype type
		     ((scalar stype) stype)
		     ((array elem-type dims) elem-type))))
    (unless (eq 'real elem-type)
      (error "Expectations must have real type."))))

(defun sexpr->expectations (se)
  (check-expectations-section se)
  (let ((result (mapcar #'sexpr->expectation (cdr se))))
    (check-expectations result)
    result))

(defun check-expectations (es)
  (let ((names (mapcar (lambda (x) (decl-var (car x))) es)))
    (unless (= (length names) (length (remove-duplicates names)))
	(error "Same expectation defined multiple times in expectations section."))))

(defun check-expectations-section (se)
  (unless (starts-with ':expectations se)
    (error "Expectations section of mcimpl file should be a list beginning with ':expectations.")))

(defun sexpr->updates (se)
  (check-updates-section se)
  (flet ((sexpr->upd (x)
	   (destructuring-bind (name . expr) x
	     (check-is-symbol name)
             (cons name (sexpr->rel expr)))))
    (mapcar #'sexpr->upd (list->pair-list (rest se)))))

(defun check-parameters-section (se)
  (unless (starts-with ':parameters se)
    (error "Parameters section of mcimpl file should be a list beginning with ':parameters.")))

(defun check-updates-section (se)
  (unless (starts-with ':updates se)
    (error "Updates section of mcimpl file should be a list beginning with ':updates.")))

(defun check-is-symbol (name)
  (unless (symbolp name)
    (error "Update label must be a symbol, not ~w." name)))

(defun free-vars-in-rellhs (lhs)
  (adt-case rellhs lhs
    ((simple var)
     (list var))
    ((array-elt var indices)
     (cons var (append-mapcar #'free-vars-in-expr indices)))
    ((array-slice var indices)
     (cons var (append-mapcar #'free-vars-in-array-slice-index indices)))))

(defun free-vars-in-array-slice-index (idx)
  (adt-case array-slice-index idx
    ((scalar value)
     (free-vars-in-expr value))
    ((range lo hi)
     (append (free-vars-in-expr lo) (free-vars-in-expr hi)))
    ((all)
     '())))
