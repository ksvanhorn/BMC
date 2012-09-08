(defpackage :expr-tests
  (:use :cl :lisp-unit :expr :symbols :utils))
(in-package :expr-tests)

(define-test sexpr->expr-tests
  (assert-equalp (make-expr-literal :value 1)
                 (sexpr->expr 1))
  (assert-equalp (make-expr-const :symbol 'true)
                 (sexpr->expr 'true))
  (assert-equalp (make-expr-variable :symbol 'bee)
                 (sexpr->expr 'bee))

  (assert-equalp (make-expr-apply
                   :fct '+ 
                   :args (list (make-expr-literal :value 3)
                               (make-expr-literal :value 10)))
                 (sexpr->expr '(+ 3 10)))

  (assert-equalp (make-expr-quantifier
                   :op 'qand
                   :lo (make-expr-literal :value 1)
                   :hi (make-expr-literal :value 4)
                   :var 'i
                   :body (make-expr-apply
                           :fct '@
                           :args (list (make-expr-variable :symbol 'x)
                                       (make-expr-variable :symbol 'i))))
                 (sexpr->expr '(qand i (1 4) (@ x i))))

  (assert-equalp (make-expr-apply
                   :fct '@-slice
                   :args (list (make-expr-variable :symbol 'v)
                               (make-expr-apply
                                 :fct '@-idx
                                 :args (list (make-expr-variable :symbol 'j)))
                               (make-expr-const :symbol '@-all)
                               (make-expr-apply
                                 :fct '@-rng
                                 :args (list (make-expr-literal :value 1)
                                             (make-expr-variable :symbol 'k)))))
                 (sexpr->expr '(@ v j :all (:range 1 k))))

  (assert-equalp
    (make-expr-let
      :var 'x
      :val (make-expr-apply
	     :fct '+
	     :args (list (make-expr-variable :symbol 'v)
			 (make-expr-literal :value 1)))
      :body (make-expr-apply
	      :fct '*
	      :args (list (make-expr-variable :symbol 'x)
			   (make-expr-variable :symbol 'x))))
    (sexpr->expr
      '(:let (x (+ v 1))
	 (* x x))))

  ;; recursive structure

  (assert-equalp (make-expr-apply
                   :fct 'exp
                   :args (list (make-expr-apply
                                 :fct '@
                                 :args (list (make-expr-variable :symbol 'nu)
                                             (make-expr-literal :value 1)))))
                 (sexpr->expr '(exp (@ nu 1))))

  (assert-equalp (make-expr-quantifier
                   :op 'qsum
                   :lo
                     (make-expr-apply
                       :fct '-
                       :args (list (make-expr-variable :symbol 'v)
                                   (make-expr-literal :value 2)))
                   :hi
                     (make-expr-apply
                       :fct '*
                       :args (list (make-expr-literal :value 4)
                                   (make-expr-variable :symbol 'w)))
                   :var 'j
                   :body (make-expr-variable :symbol 'j))
                 (sexpr->expr '(qsum j ((- v 2) (* 4 w)) j)))

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
                                      (make-expr-literal :value 3)))))
          (make-expr-apply
            :fct '@-rng
            :args (list (make-expr-apply
                          :fct '-
                          :args (list (make-expr-variable :symbol 'n)
                                      (make-expr-literal :value 1)))
                        (make-expr-apply
                          :fct '+
                          :args (list (make-expr-variable :symbol 'm)
                                      (make-expr-variable :symbol 'k)))))))
    (sexpr->expr '(@ x (/ s 3) (:range (- n 1) (+ m k)))))

  ;; errors
  
  (assert-error 'error (sexpr->expr #\a))
  (assert-error 'error (sexpr->expr "foo"))
  (assert-error 'error (sexpr->expr '(not-a-recognized-fct 2 3)))
  (assert-error 'error (sexpr->expr '(dnorm mu sigma))) ; distr, not fct
  (assert-error 'error (sexpr->expr '(not-a-quant i (1 n) (* i 3))))
  (assert-error 'error (sexpr->expr '(qsum (@ v i) (1 n) 3)))
  (assert-error 'error (sexpr->expr '(qsum)))
  (assert-error 'error (sexpr->expr '(qsum j)))
  (assert-error 'error (sexpr->expr '(qsum j 3)))
  (assert-error 'error (sexpr->expr '(qsum j (1))))
  (assert-error 'error (sexpr->expr '(qsum j (1 n))))
  (assert-error 'error (sexpr->expr '(qsum j (1 n 3) j)))
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
  (assert-equal "1" (expr->string (make-expr-literal :value 1)))
  (assert-equal "TRUE" (expr->string (make-expr-const :symbol 'true)))
  (assert-equal "bee" (expr->string (make-expr-variable :symbol '|bee|)))

  (assert-equal
    "3 + 10"
    (expr->string
      (make-expr-apply
        :fct '+
	:args (list (make-expr-literal :value 3)
		    (make-expr-literal :value 10)))))

  (assert-equal
    "QAND(i, 1 : 4, x[i])"
    (expr->string
      (make-expr-quantifier
        :op 'qand
	:lo (make-expr-literal :value 1)
	:hi (make-expr-literal :value 4)
	:var '|i|
	:body (make-expr-apply
	        :fct '@
		:args (list (make-expr-variable :symbol '|x|)
			    (make-expr-variable :symbol '|i|))))))

  (assert-equal
    "v[j, , 1 : k]"
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list (make-expr-variable :symbol '|v|)
		    (make-expr-apply
		      :fct '@-idx
		      :args (list (make-expr-variable :symbol '|j|)))
		    (make-expr-const :symbol '@-all)
		    (make-expr-apply
		      :fct '@-rng
		      :args (list (make-expr-literal :value 1)
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
				(make-expr-literal :value 1)))))))

  (assert-equal
    "(let y = v * v in x + y)"
    (expr->string
      (make-expr-let
        :var '|y|
	:val (make-expr-apply
	       :fct '*
	       :args (list (make-expr-variable :symbol '|v|)
			   (make-expr-variable :symbol '|v|)))
	:body (make-expr-apply
	        :fct '+
		:args (list (make-expr-variable :symbol '|x|)
			    (make-expr-variable :symbol '|y|))))))

  (assert-equal
    "QSUM(j, v - 2 : 4 * w, j)"
    (expr->string
      (make-expr-quantifier
        :op 'qsum
	:lo (make-expr-apply
	      :fct '-
	      :args (list (make-expr-variable :symbol '|v|)
			  (make-expr-literal :value 2)))
	:hi (make-expr-apply
	      :fct '*
	      :args (list (make-expr-literal :value 4)
			  (make-expr-variable :symbol '|w|)))
	:var '|j|
	:body (make-expr-variable :symbol '|j|))))

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
					(make-expr-literal :value 3)))))
		(make-expr-apply
		  :fct '@-rng
		  :args (list
			  (make-expr-apply
			    :fct '-
			    :args (list (make-expr-variable :symbol '|n|)
					(make-expr-literal :value 1)))
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
    (expr->string (make-expr-apply
		    :fct '@ :args (list (make-expr-literal :value 2)
					(make-expr-variable :symbol 'y)))))

  (assert-error 'error
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list (make-expr-variable :symbol 'x)
		    (make-expr-const :symbol '@-all)
		    (make-expr-apply :fct '@-idx :args '()))))) ; 0 args!

  (assert-error 'error
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list (make-expr-variable :symbol 'x)
		    (make-expr-const :symbol '@-all)
		    (make-expr-apply
		      :fct '@-idx
		      :args (list (make-expr-literal :value 1)  ; 2 args!
				  (make-expr-variable :symbol 'z)))))))

  (assert-error 'error
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list (make-expr-variable :symbol 'x)
		    (make-expr-apply
		      :fct '@-rng
		      :args (list (make-expr-literal :value 2)))))))  ; 1 arg!

  (assert-error 'error
    (expr->string
      (make-expr-apply
        :fct '@-slice
	:args (list (make-expr-variable :symbol 'x)
		    (make-expr-apply
		      :fct '@-rng
		      :args (list (make-expr-literal :value 2)   ; 3 args!
				  (make-expr-variable :symbol 'a)
				  (make-expr-literal :value 0)))))))
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
    (free-vars-in-expr (sexpr->expr '(+ a (qsum i (1 4) (* 3 b))))))
  (assert-equalp
    '(x y z)
    (free-vars-in-expr (sexpr->expr '(:let (w (* 3 x)) (^ (+ y z) w)))))
  (assert-equalp
    '(m n x)
    (free-vars-in-expr
      (sexpr->expr
        '(qand j (m n) (:let (y (@ x j)) (* y y))))))
)
