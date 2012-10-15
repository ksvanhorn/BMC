(in-package :mcimpl)

(defadt1 mcimpl
  parameters updates)

(defun params-names (impl)
  (mapcar #'decl-var (mcimpl-parameters impl)))

(defun read-mcimpl (ifname)
  (sexpr->mcimpl (read-file ifname)))

(defun sexpr->mcimpl (se)
  (unless (and (starts-with ':mcimpl se) (is-list-of-length 3 se))
    (error "mcimpl file should have form (:mcimpl parameters updates)."))
  (let ((parameters (sexpr->parameters (second se)))
	(updates (sexpr->updates (third se))))
    (make-mcimpl :parameters parameters
		 :updates updates)))

(defun sexpr->parameters (se)
  (check-parameters-section se)
  (mapcar #'sexpr->decl (rest se)))

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
