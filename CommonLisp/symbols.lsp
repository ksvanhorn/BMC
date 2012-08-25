(defpackage :symbols
  (:use :common-lisp) (:export :def-symbols-package))
(in-package :symbols)

; Usage: (def-symbols-package <name> <sym_1> ... <sym_n>).
; Defines a package <name> which exports symbols <sym_1>, ... <sym_n>,
; as well as defining and exporting a constant +<name>+, which is
; the list (<sym_1> ... <sym_n>). Typically <name> will have the form
; <something>-symbols.

(defmacro def-symbols-package (name &rest symbols)
  (let* ((pkg (make-package name))
         (const-sym
	   (intern (concatenate 'string "+" (symbol-name name) "+") pkg))
	 (syms
	   (mapcar (lambda (s) (intern (symbol-name s) pkg)) symbols)))
    `(progn
       (defpackage ,name (:export ,const-sym ,@syms) (:use :cl))
       (in-package ,name)
       (defconstant ,const-sym '(,@syms)))))
