; logical connectives: and or not => <=>
; logical quantifiers: all exi
; (op expr): head of expr (if list), or just expr
; (args expr): tail of expr (if list), or nil
; anything whose op is not a logical connective or logical quantifier
;   is an atomic clause
; skolem constants: $<id>_<num>
; variables start with '?'
; internally generated variables have form ?<id>_<num>

(defun empty-axiom-set () nil)

(defun is-predicate (x)
  (and (symbolp x) (not (variable? x))))

(defun reflexive-axiom (rel pred)
  (check-arg (is-predicate oper))
  (check-arg (is-predicate pred))
  `(all (?x ?y) (=> (and (,pred ?x) (,pred ?y))
		    (=> (,rel ?x ?y) (,rel ?y ?x)))))

(all (?x) (= ?x ?x))
(all (?x ?y) (<=> (= ?x ?y) (= ?y ?x)))
(all (?x ?y ?z) (=> (and (= ?x ?y) (= ?y ?z)) (= ?x ?z)))
; add substitution axioms for each function

(all (?x ?y) (=> (and (is-real ?x) (is-real? y))
		 (and
		  (<=> (> ?x ?y) (< ?y ?x))
		  (<=> (>= ?x ?y) (<= ?y ?x))
		  (<=> (<= ?x ?y) (or (= ?x ?y) (< ?x ?y)))
		  (<= ?x ?x)
		  (=> (and (<= ?x ?y) (<= ?y ?x)) (= ?x ?y))
		  (=> (and (<= ?x ?y) (<= 
