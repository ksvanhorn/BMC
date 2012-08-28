(use-package :expr)
(use-package :symbols)

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
)
;;; TODO: recursive structure, errors
;;; TODO: deal with logical predicates vs. computational predicates