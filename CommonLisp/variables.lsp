(in-package :variables)

(defvar *genvar-counter* 0)

(defun new-var (prefix)
  (prog1
    (intern (format nil "__~a~d" prefix *genvar-counter*) 'variables)
    (incf *genvar-counter*)))

(defun n-new-vars (n prefix)
  (let ((vars nil))
    (dotimes (_ n) (push (new-var prefix) vars))
    (reverse vars)))
