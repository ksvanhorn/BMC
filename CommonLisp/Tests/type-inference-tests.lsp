(defpackage :type-inference-tests
  (:use :cl :lisp-unit :type-inference :symbols :utils))
(in-package :type-inference-tests)

(define-test type-inference-tests
  (loop for  (sexpr . typ) in
       '((realxn . #s(bare-type-scalar :stype realxn))
	 (integer . #s(bare-type-scalar :stype integer))
	 (boolean . #s(bare-type-scalar :stype boolean))
	 (@-all-type . #s(bare-type-scalar :stype @-all-type))
	 (@-rng-type . #s(bare-type-scalar :stype @-rng-type))
	 (@-idx-type . #s(bare-type-scalar :stype @-idx-type))
     
	 ((realxn 1) . #s(bare-type-array :elem-type realxn :num-dims 1))
	 ((integer 2) . #s(bare-type-array :elem-type integer :num-dims 2))
	 ((boolean 3) . #s(bare-type-array :elem-type boolean :num-dims 3))

	 (((realxn 2) 3) . #s(bare-type-array
			      :elem-type #s(bare-type-array
					    :elem-type realxn :num-dims 2)
			      :num-dims 3))

	 ((int-map realxn) . #s(bare-type-int-map :return-type
			       #s(bare-type-scalar :stype realxn)))
	 ((int-map integer) . #s(bare-type-int-map :return-type
			        #s(bare-type-scalar :stype integer)))
	 ((int-map boolean) . #s(bare-type-int-map :return-type
				#s(bare-type-scalar :stype boolean)))

	 ((int-map realxn 2) .
	  #s(bare-type-int-map :return-type
	    #s(bare-type-array :elem-type realxn :num-dims 2)))
	 ((int-map integer 3) .
	  #s(bare-type-int-map :return-type
	    #s(bare-type-array :elem-type integer :num-dims 3)))
	 ((int-map boolean 1) .
	  #s(bare-type-int-map :return-type
	    #s(bare-type-array :elem-type boolean :num-dims 1))))
       do
       (assert-equalp typ (sexpr->bare-type sexpr)))

  (loop for (literal . sexp) in
	'((#trealxn . realxn)
	  (#t(integer 2) . (integer 2))
	  (#t(int-map boolean) . (int-map boolean))
	  (#t(int-map realxn 1) . (int-map realxn 1)))
	do
	(assert-equalp (sexpr->bare-type sexp) literal))

  (let* ((env0 (no-var-types))
	 (env1 (add-var-type env0 'x1 #tinteger))
	 (env2 (add-var-type env1 'x2 #t(realxn 1)))
	 (env3 (add-var-type env2 'x1 #t(boolean 2))))
    (assert-equalp #tinteger (var-type env1 'x1))
    (assert-equalp #tinteger (var-type env2 'x1))
    (assert-equalp #t(realxn 1) (var-type env2 'x2))
    (assert-equalp #t(boolean 2) (var-type env3 'x1))
    (assert-equalp #t(realxn 1) (var-type env3 'x2)))

  (let ((env (no-var-types)))
    (loop for (expr . typ) in
	 '((#e 1 . #tinteger)
	   (#e 1.0 . #trealxn)
	   (#e true . #tboolean)
	   (#e false . #tboolean)
	   (#e %pi . #trealxn)
	   (#e %infty+ . #trealxn)
	   (#e %infty- . #trealxn)
	   (#e %e . #trealxn)
	   (#e @-all . #t@-all-type)
	   (#e %true-pred . #t(int-map boolean))
	   (#e(:lambda y 1) . #t(int-map integer))
	   (#e(:lambda z 2.5) . #t(int-map realxn))
	   (#e(:lambda v true) . #t(int-map boolean)))
      do
      (assert-equalp typ (infer-type expr env))))

  (let ((env (compile::assocs->env
	       '((x . #tinteger)
		 (y . #trealxn)
		 (z . #t(integer 2))
		 (w . #t(int-map boolean))
		 (v . #t(int-map realxn 2))))))
    (assert-equalp #tinteger (var-type env 'x))
    (assert-equalp #trealxn (var-type env 'y))
    (assert-equalp #t(integer 2) (var-type env 'z))
    (assert-equalp #t(int-map boolean) (var-type env 'w))
    (assert-equalp #t(int-map realxn 2) (var-type env 'v)))

  (let ((env (compile::assocs->env ; do not include idx in env
	       '((i . #tinteger) (n . #tinteger)
		 (k . #tinteger)
		 (r . #trealxn)
		 (s . #trealxn)
		 (b . #tboolean)
		 (xvec . #t(realxn 1))
		 (yvec . #t(realxn 1))
		 (zvec . #t(realxn 1))
		 (veci . #t(integer 1))
		 (xmat . #t(realxn 2))
		 (ymat . #t(realxn 2))
		 (zmat . #t(realxn 2))))))
    (loop for (expr . typ) in
	  '((#e i . #tinteger)
	    (#e(<= 5 i) . #tboolean)
	    (#e(< r 2.7) . #tboolean)
	    (#e(- i 3) . #tinteger)
	    (#e(+ i 2) . #tinteger)
	    (#e(+ r 3.1) . #trealxn)
	    (#e(* i k) . #tinteger)
	    (#e(* s r) . #trealxn)
	    (#e(/ s r) . #trealxn)
	    (#e(= i k) . #tboolean)
	    (#e(^1/2 r) . #trealxn)
	    (#e(^-1/2 r) . #trealxn)
	    (#e(^-2 r) . #trealxn)
	    (#e(^-1 r) . #trealxn)
	    (#e(^2 r) . #trealxn)
	    (#e(^2 i) . #tinteger)
	    (#e(int false) . #tinteger)
	    (#e(real 12) . #trealxn)
	    (#e(@^-1 xvec) . #t(realxn 1))
	    (#e(@^-1 xmat) . #t(realxn 2))
	    (#e(@^-2 xvec) . #t(realxn 1))
	    (#e(@^-2 xmat) . #t(realxn 2))
	    (#e(@^2 xvec) . #t(realxn 1))
	    (#e(@^2 xmat) . #t(realxn 2))
	    (#e(@+ xvec yvec zvec) . #t(realxn 1))
	    (#e(@+ xmat ymat zmat) . #t(realxn 2))
	    (#e(@* xvec yvec zvec) . #t(realxn 1))
	    (#e(@* xmat ymat zmat) . #t(realxn 2))
	    (#e(@- xvec yvec) . #t(realxn 1))
	    (#e(@- xmat ymat) . #t(realxn 2))
	    (#e(is-integerp i) . #tboolean)
	    (#e(is-realp r) . #tboolean)
	    (#e(is-real r) . #tboolean)
	    (#e(is-symm-pd xmat) . #tboolean)
	    (#e(@ veci i) . #tinteger)
	    (#e(@ xmat k i) . #trealxn)
	    (#e(vmax veci) . #tinteger)
	    (#e(@-rng 2 i) . #t@-rng-type)
            (#e(@-idx k) . #t@-idx-type)
	    (#e(@ xmat :all (:range i k)) . #t(realxn 2))
	    (#e(@ xmat i :all) . #t(realxn 1))
	    (#e(@ xmat i (:range 1 k)) . #t(realxn 1))
	    (#e(dot xmat xvec) . #t(realxn 1))
	    (#e(dot xvec xmat) . #t(realxn 1))
	    (#e(dot xvec yvec) . #trealxn)
	    (#e(dot xmat ymat) . #t(realxn 2))
	    (#e(dot xmat ymat xvec) . #t(realxn 1))
	    (#e(diag_mat veci) . #t(integer 2))
	    (#e(diag_mat xvec) . #t(realxn 2))
	    (#e(o^2 xvec) . #t(realxn 2))
	    (#e(o^2 veci) . #t(integer 2))
	    (#e(inv-pd xmat) . #t(realxn 2))
	    (#e(vec r) . #t(realxn 1))
	    (#e(vec r s) . #t(realxn 1))
	    (#e(vec r s 2.7) . #t(realxn 1))
	    (#e(cons 1.3 xvec) . #t(realxn 1))
	    (#e(cons-col xvec xmat) . #t(realxn 2))
	    (#e(cons-row xvec xmat) . #t(realxn 2))
	    (#e(:quant qvec idx (i k) (* 2.3 (@ xmat 2 idx))) . #t(realxn 1))
	    (#e(:quant qand idx (1 i) b) . #tboolean)
	    (#e(:quant qsum idx (i 10) (@ veci idx)) . #tinteger)
	    (#e(:quant qnum idx (i k) b) . #tinteger)
	    (#e(:quant q@sum idx (i k) xvec :shape (n)) . #t(realxn 1))
	    (#e(:quant q@sum idx (1 i) xmat :shape (n k)) . #t(realxn 2))
	    (#e($* r xvec) . #t(realxn 1))
	    (#e($* r xmat) . #t(realxn 2))
	    (#e(:let (foo r) (* r r)) . #trealxn)
	    (#e(:let (foo i) (* i i)) . #tinteger))
      do
      (assert-equalp typ (infer-type expr env))))

)
