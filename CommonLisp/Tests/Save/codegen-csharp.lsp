(load "../codegen-csharp.lsp")
 
(define-test gen-decl-csharp-tests
  (assert-equal "double x;" (gen-decl-csharp '|x| 'realxn 0))
  (assert-equal "int y;" (gen-decl-csharp '|y| 'integer 0))
  (assert-equal "bool a;" (gen-decl-csharp '|a| 'boolean 0))

  (assert-equal "Array1D<double> V;" (gen-decl-csharp 'V 'realxn 1))
  (assert-equal "Array2D<int> W;" (gen-decl-csharp 'W 'integer 2))
  (assert-equal "Array3D<bool> X;" (gen-decl-csharp 'X 'boolean 3))
  (assert-equal "Array4D<int> Y;" (gen-decl-csharp 'Y 'integer 4))
)

(define-test gen-lcom-csharp-tests
  (assert-equal "// My comment" (gen-lcom-csharp "My comment"))
  (assert-equal "// The line" (gen-lcom-csharp "The line"))
)

(defun gen-decl-test (var typ ndim) (format nil "~a ~a ~a" var typ ndim))
(defun gen-lcom-test (comment) (format nil "; ~a" comment))

(define-test gen-variables-tests
  (assert-equal "
   ; Inputs
   x REALXN 0
   a BOOLEAN 0
   b INTEGER 0
   M REALXN 2
   c INTEGER 1

   ; Model variables
   alpha REALXN 0
   beta INTEGER 1
   foo BOOLEAN 3
"
   (gen-variables
    3
    '((:gen-decl . gen-decl-test)
     (:gen-lcom . gen-lcom-test))
    '(:model
      (:args
       (|x| realp)
       (|a| boolean)
       (|b| integer)
       (M (real 2 |b|))
       (|c| (integernn (+ |b| 1))))
      (:reqs)
      (:vars
       (|alpha| realnn)
       (|beta| (integerp 12))
       (|foo| (boolean |b| (|beta| 2) 4)))
      (:body))))
)


#|
(define-test gen-vars-tests
  (assert-equal
"
   a INTEGER 0
   b REALXN 1
   c BOOLEAN 2 "))
|#
