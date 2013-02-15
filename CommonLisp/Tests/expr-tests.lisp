(defpackage :expr-tests
  (:use :cl :lisp-unit :expr :symbols :utils))
(in-package :expr-tests)

(define-test sexpr->expr-tests
  (assert-equalp (make-expr-const :name 12)
                 (sexpr->expr 12))
  (assert-equalp (make-expr-const :name 'true)
                 (sexpr->expr 'true))
  (assert-equalp (make-expr-const :name 'false)
                 (sexpr->expr 'false))
  (assert-equalp (make-expr-const :name '@-all)
		 (sexpr->expr '@-all))

  (assert-equalp (make-expr-variable :symbol 'vars::bee)
                 (sexpr->expr 'expr-tests::bee))

  (assert-equalp (make-expr-apply
                   :fct '+ 
                   :args (list (make-expr-const :name 3)
                               (make-expr-const :name 10)))
                 (sexpr->expr '(+ 3 10)))

  (assert-equalp
    (make-expr-apply
     :fct 'qand
     :args (list
	     (make-expr-const :name 1)
	     (make-expr-const :name 4)
	     (make-expr-const :name '%true-pred)
	     (make-expr-lambda
	       :var 'vars::i
	       :body (make-expr-apply
		       :fct '@
		       :args (list (make-expr-variable :symbol 'vars::x)
				   (make-expr-variable :symbol 'vars::i))))))
    (sexpr->expr '(:quant qand i (1 4) (@ x i))))

  (assert-equalp
    (expr-call 'qand
      (expr-const 1) (expr-var 'vars::n)
      (expr-lam 'vars::k (expr-call 'is-even (expr-var 'vars::k)))
      (expr-lam 'vars::k
        (expr-call '@ (expr-var 'vars::x) (expr-var 'vars::k))))
    (sexpr->expr '(:quant qand k (1 n) (is-even k) (@ x k))))

  (assert-equalp
    (make-expr-apply
      :fct '@-slice
      :args (list
	      (make-expr-variable :symbol 'vars::v)
	      (make-expr-apply
	        :fct '@-idx
		:args (list (make-expr-variable :symbol 'vars::j)))
	      (make-expr-const :name '@-all)
	      (make-expr-apply
	        :fct '@-rng
		:args (list (make-expr-const :name 1)
			    (make-expr-variable :symbol 'vars::k)))))
    (sexpr->expr '(@ v j :all (:range 1 k))))
  
  (assert-equalp
    (make-expr-lambda :var 'vars::x
		      :body (make-expr-apply
			      :fct '+
			      :args (list (make-expr-variable :symbol 'vars::x)
					  (make-expr-const :name 3))))
    (sexpr->expr '(:lambda x (+ x 3))))

  (assert-equalp
    (make-expr-apply
      :fct '!
      :args (list
	      (make-expr-lambda
	        :var 'vars::x
		:body (make-expr-apply
		        :fct '*
			:args (list (make-expr-variable :symbol 'vars::x)
				    (make-expr-variable :symbol 'vars::x))))
	      (make-expr-apply
	        :fct '+
		:args (list (make-expr-variable :symbol 'vars::v)
			    (make-expr-const :name 1)))))
    (sexpr->expr
      '(:let (x (+ v 1))
	 (* x x))))

  (assert-equalp
    (sexpr->expr '(dot u v))
    (sexpr->expr '(:let (dot u v))))
  (assert-equalp
    (sexpr->expr '(:let (a 1)
		    (:let (b (/ u v))
		      (:let (c (dot x y)) (+ a b c)))))
    (sexpr->expr '(:let (a 1)
			(b (/ u v))
			(c (dot x y))
		    (+ a b c))))

  ;; recursive structure

  (assert-equalp
    (make-expr-apply
     :fct 'exp
     :args (list (make-expr-apply
		  :fct '@
		  :args (list (make-expr-variable :symbol 'vars::nu)
			      (make-expr-const :name 1)))))
    (sexpr->expr '(exp (@ nu 1))))

  (assert-equalp
    (make-expr-apply
      :fct 'qsum
      :args (list
	      (make-expr-apply
	        :fct '-
		:args (list (make-expr-variable :symbol 'vars::v)
			    (make-expr-const :name 2)))
	      (make-expr-apply
	        :fct '*
		:args (list (make-expr-const :name 4)
			    (make-expr-variable :symbol 'vars::w)))
	      (make-expr-const :name '%true-pred)
	      (make-expr-lambda
	        :var 'vars::j
		:body (make-expr-variable :symbol 'vars::j))))
    (sexpr->expr '(:quant qsum j ((- v 2) (* 4 w)) j)))

  (assert-equalp
    (make-expr-apply
      :fct 'q@sum
      :args (list
              (expr-var 'vars::m)
	      (expr-var 'vars::n)
	      (expr-lam 'vars::j
		(expr-call '=
                  (expr-const 2)
		  (expr-call '@ (expr-var 'vars::s) (expr-var 'vars::j))))
	      (expr-lam 'vars::j
		(expr-call '@-slice
		  (expr-var 'vars::x)
		  (expr-const '@-all)
		  (expr-call '@-idx (expr-var 'vars::j))))
	      (expr-call '- (expr-var 'vars::nv) (expr-const 1))))
    (sexpr->expr
     '(:quant q@sum j (m n) (= 2 (@ s j)) (@ x :all j) :shape ((- nv 1)))))

  (assert-equalp
    (make-expr-apply
      :fct 'q@sum
      :args (list
              (expr-var 'vars::m)
	      (expr-var 'vars::n)
	      (expr-lam 'vars::j
		(expr-call '=
                  (expr-const 2)
		  (expr-call '@ (expr-var 'vars::s) (expr-var 'vars::j))))
	      (expr-lam 'vars::j
		(expr-call '@-slice
		  (expr-var 'vars::x)
		  (expr-const '@-all)
		  (expr-call '@-idx (expr-var 'vars::j))
		  (expr-const '@-all)))
	      (expr-call '- (expr-var 'vars::nv) (expr-const 1))
	      (expr-call '- (expr-var 'vars::b) (expr-var 'vars::a))))
    (sexpr->expr
     '(:quant q@sum j (m n) (= 2 (@ s j))
	      (@ x :all j :all)
	      :shape ((- nv 1) (- b a)))))

  (assert-equalp
    (make-expr-apply
      :fct '@-slice
      :args
        (list
          (make-expr-variable :symbol 'vars::x)
          (make-expr-apply
            :fct '@-idx
            :args (list (make-expr-apply
                          :fct '/
                          :args (list (make-expr-variable :symbol 'vars::s)
                                      (make-expr-const :name 3)))))
          (make-expr-apply
            :fct '@-rng
            :args (list (make-expr-apply
                          :fct '-
                          :args (list (make-expr-variable :symbol 'vars::n)
                                      (make-expr-const :name 1)))
                        (make-expr-apply
                          :fct '+
                          :args (list (make-expr-variable :symbol 'vars::m)
                                      (make-expr-variable :symbol 'vars::k)))))))
    (sexpr->expr '(@ x (/ s 3) (:range (- n 1) (+ m k)))))

  ;; errors
  
  (assert-error 'error (sexpr->expr #\a))
  (assert-error 'error (sexpr->expr "foo"))
  (assert-error 'error (sexpr->expr '(:quant qsum (@ v i) (1 n) 3)))
  (assert-error 'error (sexpr->expr '(:quant qsum)))
  (assert-error 'error (sexpr->expr '(:quant qsum j)))
  (assert-error 'error (sexpr->expr '(:quant qsum j 3)))
  (assert-error 'error (sexpr->expr '(:quant qsum j (1))))
  (assert-error 'error (sexpr->expr '(:quant qsum j (1 n))))
  (assert-error 'error (sexpr->expr '(:quant qsum j (1 n 3) j)))
  (assert-error 'error (sexpr->expr '(:quant qfoo j (m n) 1)))
  (assert-error 'error (sexpr->expr '(@)))
  (assert-error 'error (sexpr->expr '(@ x)))
  (assert-error 'error (sexpr->expr '(:let)))
  ;(assert-error 'error (sexpr->expr '(:let v)))
  ;(assert-error 'error (sexpr->expr '(:let (v x))))
  (assert-error 'error (sexpr->expr '(:let (v) (+ v 1))))
  (assert-error 'error (sexpr->expr '(:let (v x 2) (+ v 1))))
  (assert-error 'error (sexpr->expr '(:let ((+ x 2) y) (+ x y))))
  (assert-error 'error (sexpr->expr '(:let (true 12) (* true true))))
)

(define-test is-quant-expr-tests
  (assert-equalp nil (is-quant-expr #e1))
  (assert-equalp nil (is-quant-expr #e%pi))
  (assert-equalp nil (is-quant-expr #ex))
  (assert-equalp nil (is-quant-expr #(+ x 1)))
  (assert-equalp
    '(qsum #e(+ m 1) #e(- n 1) #e(:lambda i (< (@ x i) 0))
	   #e(:lambda i (@ z i)))
    (is-quant-expr #e(:quant qsum i ((+ m 1) (- n 1)) (< (@ x i) 0) (@ z i))))
  (assert-equalp
    '(q@sum #em #en #e(:lambda j (= 2 (@ v j))) #e(:lambda j (@ z j)) #ek)
    (is-quant-expr
      #e(:quant q@sum j (m n) (= 2 (@ v j)) (@ z j) :shape (k))))
  (assert-equalp
    '(q@sum #em #en #e(:lambda j (@ p j)) #e(:lambda j (@ z j)) #ek #er)
    (is-quant-expr
      #e(:quant q@sum j (m n) (@ p j) (@ z j) :shape (k r))))
)

(define-test expr->string-tests
  (assert-equal "1" (expr->string (make-expr-const :name 1)))
  (assert-equal "TRUE" (expr->string (make-expr-const :name 'true)))
  (assert-equal "bee" (expr->string (make-expr-variable :symbol '|bee|)))
  (assert-equal "5.0E-1" (expr->string (make-expr-const :name 0.5)))
  (assert-equal "3/7" (expr->string (make-expr-const :name 3/7)))

  (assert-equal
    "3 + 10"
    (expr->string
      (make-expr-apply
        :fct '+
	:args (list (make-expr-const :name 3)
		    (make-expr-const :name 10)))))

  (assert-equal
    "QAND(i, 1 : 4, x[i])"
    (expr->string
     (make-expr-apply
       :fct 'qand
       :args (list
	       (make-expr-const :name 1)
	       (make-expr-const :name 4)
	       (expr-const '%true-pred)
	       (make-expr-lambda
		 :var '|i|
		 :body (make-expr-apply
			 :fct '@
			 :args (list (make-expr-variable :symbol '|x|)
				     (make-expr-variable :symbol '|i|))))))))

  (assert-equal
    "v[j, , 1 : k]"
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list (make-expr-variable :symbol '|v|)
		    (make-expr-apply
		      :fct '@-idx
		      :args (list (make-expr-variable :symbol '|j|)))
		    (make-expr-const :name '@-all)
		    (make-expr-apply
		      :fct '@-rng
		      :args (list (make-expr-const :name 1)
				  (make-expr-variable :symbol '|k|)))))))

  ;; recursive structure

  (assert-equal
    "EXP(nu[1])"
    (expr->string
     (make-expr-apply
      :fct 'exp
      :args (list (make-expr-apply
		    :fct '@
		    :args (list (make-expr-variable :symbol '|nu|)
				(make-expr-const :name 1)))))))

  (assert-equal
    "(let y = v * v in x + y)"
    (expr->string
      (make-expr-apply
        :fct '!
	:args (list
	        (make-expr-lambda
		  :var '|y|
		  :body (make-expr-apply
			  :fct '+
			  :args (list (make-expr-variable :symbol '|x|)
				      (make-expr-variable :symbol '|y|))))
		(make-expr-apply
		  :fct '*
		  :args (list (make-expr-variable :symbol '|v|)
			      (make-expr-variable :symbol '|v|)))))))

  (assert-equal
    "QSUM(j, v - 2 : 4 * w, j)"
    (expr->string
      (make-expr-apply
        :fct 'qsum
	:args
	  (list
	    (make-expr-apply
	      :fct '-
	      :args (list (make-expr-variable :symbol '|v|)
			  (make-expr-const :name 2)))
	    (make-expr-apply
	      :fct '*
	      :args (list (make-expr-const :name 4)
			  (make-expr-variable :symbol '|w|)))
	    (expr-const '%true-pred) 
	    (make-expr-lambda
	      :var '|j|
	      :body (make-expr-variable :symbol '|j|))))))

  (assert-equal
    "x[s / 3, n - 1 : m + k]"
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list
	        (make-expr-variable :symbol '|x|)
		(make-expr-apply
		  :fct '@-idx
		  :args (list 
			  (make-expr-apply
			    :fct '/
			    :args (list (make-expr-variable :symbol '|s|)
					(make-expr-const :name 3)))))
		(make-expr-apply
		  :fct '@-rng
		  :args (list
			  (make-expr-apply
			    :fct '-
			    :args (list (make-expr-variable :symbol '|n|)
					(make-expr-const :name 1)))
			  (make-expr-apply
			    :fct '+
			    :args (list
				    (make-expr-variable :symbol '|m|)
				    (make-expr-variable :symbol '|k|)))))))))

  ;; precedence for binary ops
  (let* ((sexpr
	  '(* (^ (/ (+ 4 4 8) (- 8 6 0) (* 5 9 0))
	         (^ (- 4 1 7) (* 3 4 4) (+ 0 0 1))
	         (/ (+ 6 1 1) (* 1 1 9) (+ 3 1 7)))
	      (^ (* (^ 4 2 6) (- 8 7 5) (- 6 9 6))
	         (/ (^ 7 0 2) (- 5 8 1) (* 7 3 0))
	         (^ (+ 5 6 7) (/ 0 6 8) (/ 3 0 6)))
	      (/ (* (- 1 1 1) (- 2 4 0) (- 8 1 5))
	         (+ (/ 2 2 7) (- 8 4 7) (* 8 5 0))
	         (+ (- 7 4 0) (/ 6 6 8) (* 5 7 5)))))
	 (expr (sexpr->expr sexpr))
	 (str (expr->string expr)))
    (assert-equal
      (strcat
        "((4 + 4 + 8) / (8 - 6 - 0) / (5 * 9 * 0)) "
        "^ ((4 - 1 - 7) ^ (3 * 4 * 4) ^ (0 + 0 + 1)) "
        "^ ((6 + 1 + 1) / (1 * 1 * 9) / (3 + 1 + 7)) "
        "* "
        "(4 ^ 2 ^ 6 * (8 - 7 - 5) * (6 - 9 - 6)) "
        "^ (7 ^ 0 ^ 2 / (5 - 8 - 1) / (7 * 3 * 0)) "
        "^ (5 + 6 + 7) ^ (0 / 6 / 8) ^ (3 / 0 / 6) "
        "* "
        "((1 - 1 - 1) * (2 - 4 - 0) * (8 - 1 - 5) "
        "/ (2 / 2 / 7 + (8 - 4 - 7) + 8 * 5 * 0) "
        "/ (7 - 4 - 0 + 6 / 6 / 8 + 5 * 7 * 5))")
      str))

  (assert-equal
   (strcat
     "X = Y AND X != Y AND X < Y AND X > Y AND X <= Y AND X >= Y "
     "OR A = B AND Z < Y OR (A <= B OR A > B) AND NOT(M = 3)")
   (expr->string
     (sexpr->expr
       '(or (and (and (= x y) (!= x y) (< x y)) (> x y) (<= x y) (>= x y))
	    (and (= a b) (< z y))
	    (and (or (<= a b) (> a b)) (not (= m 3)))))))

  (assert-equal
   (strcat
     "X .= Y .AND X .!= Y .AND X .< Y .AND X .> Y .AND X .<= Y .AND X .>= Y "
     ".OR A .= B .AND Z .< Y .OR (A .<= B .OR A .> B) .AND .NOT(M .= 3)")
   (expr->string
    (let ((*convert-boolean-functions* t))
     (sexpr->expr
       '(or (and (and (= x y) (!= x y) (< x y)) (> x y) (<= x y) (>= x y))
	    (and (= a b) (< z y))
	    (and (or (<= a b) (> a b)) (not (= m 3))))))))

  (assert-equal
   "X + Y - 3 < A * B + C AND M = N"
   (expr->string
    (sexpr->expr
     '(and (< (- (+ x y) 3) (+ (* a b) c))
           (= m n)))))

  ;; errors
  (assert-error 'error (expr->string (make-expr-apply :fct '@ :args '())))

  (assert-error 'error
    (expr->string (make-expr-apply
		    :fct '@ :args (list (make-expr-variable :symbol 'x)))))

  (assert-error 'error
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list (make-expr-variable :symbol 'x)
		    (make-expr-const :name '@-all)
		    (make-expr-apply :fct '@-idx :args '()))))) ; 0 args!

  (assert-error 'error
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list (make-expr-variable :symbol 'x)
		    (make-expr-const :name '@-all)
		    (make-expr-apply
		      :fct '@-idx
		      :args (list (make-expr-const :name 1)  ; 2 args!
				  (make-expr-variable :symbol 'z)))))))

  (assert-error 'error
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list (make-expr-variable :symbol 'x)
		    (make-expr-apply
		      :fct '@-rng
		      :args (list (make-expr-const :name 2)))))))  ; 1 arg!

  (assert-error 'error
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list (make-expr-variable :symbol 'x)
		    (make-expr-apply
		      :fct '@-rng
		      :args (list (make-expr-const :name 2)   ; 3 args!
				  (make-expr-variable :symbol 'a)
				  (make-expr-const :name 0)))))))
)

(define-test free-vars-in-expr-tests
  (assert-equalp
    '()
    (free-vars-in-expr (sexpr->expr 'true)))
  (assert-equalp
    '()
    (free-vars-in-expr (sexpr->expr 5)))
  (assert-equalp
    '(vars::v)
    (free-vars-in-expr (sexpr->expr 'v)))
  (assert-equalp
    '(vars::x vars::y)
    (free-vars-in-expr (sexpr->expr '(+ x y 3))))
  (assert-equalp
    '(vars::a vars::b)
    (free-vars-in-expr (sexpr->expr '(+ a (:quant qsum i (1 4) (* 3 b))))))
  (assert-equalp
    '(vars::y vars::z vars::x)
    (free-vars-in-expr (sexpr->expr '(:let (w (* 3 x)) (^ (+ y z) w)))))
  (assert-equalp
    '(vars::m vars::n vars::x)
    (free-vars-in-expr
      (sexpr->expr
        '(:quant qand j (m n) (:let (y (@ x j)) (* y y))))))
)

(define-test occurs-free-tests
  (let ((yes '(v
	       (+ a v)
	       (+ v a)
	       (^2 v)
	       (:quant qand i (1 n) (* v 7))
	       (+ (- a (/ v 2)) (* x y))))
	(no '(w
	      12
	      %pi
	      (+ a b)
	      (:quant qand i (1 n) (* w 7))
	      (:quant qand v (1 n) (* v 7)))))
    (dolist (e (mapcar #'sexpr->expr yes))
      (assert-true (occurs-free 'vars::v e)))
    (dolist (e (mapcar #'sexpr->expr no))
      (assert-false (occurs-free 'vars::v e)))))

(define-test rename-var-tests
  (assert-equalp
    #e1
    (rename-var 'vars::x 'vars::y #e1))

  (assert-equalp
    #e z
    (rename-var 'vars::x 'vars::y #e z))

  (assert-equalp
    #e y
    (rename-var 'vars::x 'vars::y #e x))

  (assert-equalp
    #e(+ a b)
    (rename-var 'vars::x 'vars::y #e(+ a b)))

  (assert-equalp
    #e(+ a xn)
    (rename-var 'vars::xo 'vars::xn #e(+ a xo)))

  (assert-equalp
    #e(+ xn b)
    (rename-var 'vars::xo 'vars::xn #e(+ xo b)))

  (assert-equalp
    #e(:lambda v (* v v))
    (rename-var 'vars::a 'vars::b #e(:lambda v (* v v))))
    
  (assert-equalp
    #e(:lambda a (* a x))
    (rename-var 'vars::a 'vars::b #e(:lambda a (* a x))))
    
  (assert-equalp
    #e(:lambda a (* a y))
    (rename-var 'vars::x 'vars::y #e(:lambda a (* a x))))

  (assert-equalp
    #e(+ x (* y (^2 x)))
    (rename-var 'vars::x 'vars::x #e(+ x (* y (^2 x)))))

  (assert-equalp
    #e(+ (* 3 y) (:quant qsum i (m (* 2 y)) (@ a y i)))
    (rename-var 'vars::x 'vars::y
      #e(+ (* 3 x) (:quant qsum i (m (* 2 x)) (@ a x i)))))
)
