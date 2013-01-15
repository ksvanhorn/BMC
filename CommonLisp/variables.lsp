(in-package :variables)

;;; Scheme for variable names:
;;; model-ident: <simple-ident>
;;; newly-generated-var: _[0-9]+<simple-ident>
;;; special-var: _[a-zA-Z]+(_<simple-ident>)?
;;; simple-ident: [a-zA-Z][a-zA-Z0-9_]*

(defparameter +vars-package+ (find-package :variables))

(defun vars-symbol (s)
  (if (eq +vars-package+ (symbol-package s))
    s
    (intern (symbol-name s) +vars-package+)))

(defun ensure-string (x)
  (if (symbolp x)
    (symbol-name x)
    x))

(defun is-special-name (s)
  (every #'alpha-char-p s))

(defun special-var (name &optional var)
  (setf name (ensure-string name))
  (assert (is-special-name name))
  (let ((suffix (if (null var) "" (format nil "_~a" var))))
    (intern (strcat "_" name suffix) +vars-package+)))

(defvar *genvar-counter* 0)

(defun new-var (root)
  (setf root (normalize-root (ensure-string root)))
  (prog1
    (intern (format nil "_~d~a" *genvar-counter* root) +vars-package+)
    (incf *genvar-counter*)))

;;; Strip off any prefix of form _[0-9]+.
;;; REQUIRE: If s begins with #\_ then this is followed by a digit.
;;;
(defun normalize-root (s)
  (let ((beg 0)
	(len (length s)))
    (cond
      ((or (zerop len) (char/= #\_ (char s 0)))
       s)
      (t
       (incf beg) ; skip past initial underscore
       (while (and (< beg len) (digit-char-p (char s beg)))
	 (incf beg))
       (when (= 1 beg) ; digit does not follow #\_
	 (error "Invalid name passed to new-var: ~a." s))
       (subseq s beg)))))

(defun n-new-vars (n prefix)
  (let ((vars nil))
    (dotimes (_ n) (push (new-var prefix) vars))
    (reverse vars)))
