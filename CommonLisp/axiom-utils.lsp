
(defun symbol->variable (x)
  (unless (symbolp x)
    (error "symbol->variable: argument is not a symbol"))
  (intern (concatenate 'string "?" (symbol-name x))))

(defun occurs (x expr)
  (cond
   ((eq expr x) t)
   ((consp expr) (or (occurs x (car expr)) (occurs x (cdr expr))))
   (t nil)))

; (pairlis keylist vallist)
; (sublis assoclist tree)

(defun defaxiom-xform (predicate-name arguments body)
  (unless (symbolp predicate-name)
    (error "defaxiom: predicate-name is not a symbol"))
  (unless (and (listp arguments) (every #'symbolp arguments))
    (error "defaxiom: arguments is not a list of symbols"))
  (unless (= (length arguments) (length (remove-duplicates arguments)))
    (error "defaxiom: arguments has duplicates"))
  (if (= 0 (length arguments))
    `(<=> (,predicate-name) ,body)
    (let* ((argvars (mapcar #'symbol->variable arguments))
	   (lookup (pairlis arguments argvars))
	   (body1 (sublis lookup body)))
      (unless (notany (lambda (x) (occurs x body)) argvars)
	(error "defaxiom: body contains arguments variable"))
      `(all ,argvars (<=> (,predicate-name ,@argvars) ,body1)))))

(defmacro defaxiom (predicate-name arguments body)
  (defaxiom-xform predicate-name arguments body))
      

    
