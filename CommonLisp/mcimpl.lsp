(in-package :mcimpl)

(defadt1 mcimpl
  parameters derived updates)

(defun sexpr->mcimpl (se) nil)

(defun sexpr->parameters (se)
  (check-parameters-section se)
  (mapcar #'sexpr->decl (rest se)))

(defun sexpr->derived (se)
  (check-derived-section se)
  (flet
    ((sexpr->association (x)
       (check-derived-decl x)
       (cons (first x) (sexpr->expr (second x)))))
    (mapcar #'sexpr->association (rest se))))

(defun check-parameters-section (se)
  (unless (starts-with ':parameters se)
    (error "Parameters section of mcimpl file should be a list beginning with ':parameters.")))

(defun check-derived-section (se)
  (unless (starts-with ':derived se)
    (error "Derived section of mcimpl file should be a list beggining with ':derived.")))

(defun check-derived-decl (x)
  (unless (and (is-list-of-length 2 x) (symbolp (first x)))
    (error "Invalid form for declaration of derived value: ~w." x)))
