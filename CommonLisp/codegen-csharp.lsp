(require "model-utils")

(defun gen-decl-csharp (var typ ndim)
  (format nil "~a ~a;" (csharp-type-string typ ndim) var))

(defparameter *csharp-base-types*
  '((realxn . "double") (integer . "int") (boolean . "bool")))

(defun csharp-type-string (typ ndim)
  (let ((elem-type-str (cdr (assoc typ *csharp-base-types*))))
    (if (zerop ndim)
	elem-type-str
	(format nil "Array~aD<~a>" ndim elem-type-str))))

(defun gen-line-comment-csharp (indent comment)
  (let ((s (make-string-output-stream)))
    (dotimes (i indent) (princ #\Space s))
    (format s "// ~a" comment)
    (get-output-stream-string s)))
