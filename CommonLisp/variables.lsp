(in-package :variables)

;;; Scheme for variables:
;;; - The names of generated temporary variables have form _<ident><number>,
;;;   where <ident> does not start with '_'.
;;; - The names of generated variables used in all models have form
;;;   _<ident>, where <ident> does not end in a digit.
;;; - <ident> matches the regular expression [_A-Za-z0-9]+.
;;; - <number> matches [0-9]+.

(defvar *genvar-counter* 0)

(defun new-var (prefix)
  (prog1
    (intern (format nil "_~a~d" prefix *genvar-counter*) 'variables)
    (incf *genvar-counter*)))

(defun n-new-vars (n prefix)
  (let ((vars nil))
    (dotimes (_ n) (push (new-var prefix) vars))
    (reverse vars)))
