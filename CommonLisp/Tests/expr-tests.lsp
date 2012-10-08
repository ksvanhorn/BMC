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

  (assert-equalp (make-expr-variable :symbol 'bee)
                 (sexpr->expr 'bee))

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
	       :var 'i
	       :body (make-expr-apply
		       :fct '@
		       :args (list (make-expr-variable :symbol 'x)
				   (make-expr-variable :symbol 'i))))))
    (sexpr->expr '(:quant qand i (1 4) (@ x i))))

  (assert-equalp
    (expr-call 'qand (expr-const 1) (expr-var 'n)
	       (expr-lam 'k (expr-call 'is-even (expr-var 'k)))
	       (expr-lam 'k (expr-call '@ (expr-var 'x) (expr-var 'k))))
    (sexpr->expr '(:quant qand k (1 n) (is-even k) (@ x k))))

  (assert-equalp
    (make-expr-apply
      :fct '@-slice
      :args (list
	      (make-expr-variable :symbol 'v)
	      (make-expr-apply
	        :fct '@-idx
		:args (list (make-expr-variable :symbol 'j)))
	      (make-expr-const :name '@-all)
	      (make-expr-apply
	        :fct '@-rng
		:args (list (make-expr-const :name 1)
			    (make-expr-variable :symbol 'k)))))
    (sexpr->expr '(@ v j :all (:range 1 k))))
  
  (assert-equalp
    (make-expr-lambda :var 'x
		      :body (make-expr-apply
			      :fct '+
			      :args (list (make-expr-variable :symbol 'x)
					  (make-expr-const :name 3))))
    (sexpr->expr '(:lambda x (+ x 3))))

  (assert-equalp
    (make-expr-apply
      :fct '!
      :args (list
	      (make-expr-lambda
	        :var 'x
		:body (make-expr-apply
		        :fct '*
			:args (list (make-expr-variable :symbol 'x)
				    (make-expr-variable :symbol 'x))))
	      (make-expr-apply
	        :fct '+
		:args (list (make-expr-variable :symbol 'v)
			    (make-expr-const :name 1)))))
    (sexpr->expr
      '(:let (x (+ v 1))
	 (* x x))))

  ;; recursive structure

  (assert-equalp (make-expr-apply
                   :fct 'exp
                   :args (list (make-expr-apply
                                 :fct '@
                                 :args (list (make-expr-variable :symbol 'nu)
                                             (make-expr-const :name 1)))))
                 (sexpr->expr '(exp (@ nu 1))))

  (assert-equalp
    (make-expr-apply
      :fct 'qsum
      :args (list
	      (make-expr-apply
	        :fct '-
		:args (list (make-expr-variable :symbol 'v)
			    (make-expr-const :name 2)))
	      (make-expr-apply
	        :fct '*
		:args (list (make-expr-const :name 4)
			    (make-expr-variable :symbol 'w)))
	      (make-expr-const :name '%true-pred)
	      (make-expr-lambda
	        :var 'j
		:body (make-expr-variable :symbol 'j))))
    (sexpr->expr '(:quant qsum j ((- v 2) (* 4 w)) j)))

  (assert-equalp
    (make-expr-apply
      :fct '@-slice
      :args
        (list
          (make-expr-variable :symbol 'x)
          (make-expr-apply
            :fct '@-idx
            :args (list (make-expr-apply
                          :fct '/
                          :args (list (make-expr-variable :symbol 's)
                                      (make-expr-const :name 3)))))
          (make-expr-apply
            :fct '@-rng
            :args (list (make-expr-apply
                          :fct '-
                          :args (list (make-expr-variable :symbol 'n)
                                      (make-expr-const :name 1)))
                        (make-expr-apply
                          :fct '+
                          :args (list (make-expr-variable :symbol 'm)
                                      (make-expr-variable :symbol 'k)))))))
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
  (assert-error 'error (sexpr->expr '(:let v)))
  (assert-error 'error (sexpr->expr '(:let (v x))))
  (assert-error 'error (sexpr->expr '(:let (v) (+ v 1))))
  (assert-error 'error (sexpr->expr '(:let (v x 2) (+ v 1))))
  (assert-error 'error (sexpr->expr '(:let ((+ x 2) y) (+ x y))))
  (assert-error 'error (sexpr->expr '(:let (true 12) (* true true))))
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
    '(v)
    (free-vars-in-expr (sexpr->expr 'v)))
  (assert-equalp
    '(x y)
    (free-vars-in-expr (sexpr->expr '(+ x y 3))))
  (assert-equalp
    '(a b)
    (free-vars-in-expr (sexpr->expr '(+ a (:quant qsum i (1 4) (* 3 b))))))
  (assert-equalp
    '(y z x)
    (free-vars-in-expr (sexpr->expr '(:let (w (* 3 x)) (^ (+ y z) w)))))
  (assert-equalp
    '(m n x)
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
      (assert-true (occurs-free 'v e)))
    (dolist (e (mapcar #'sexpr->expr no))
      (assert-false (occurs-free 'v e)))))