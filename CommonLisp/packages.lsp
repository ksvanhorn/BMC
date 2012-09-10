(defpackage :symbols
  (:use :cl)
  (:export
    ;; symbol classes
    :is-fquant-symbol :is-const-symbol :is-fct-symbol :is-distr-symbol
    :is-variable-symbol :is-scalar-type-symbol

    ;; logical symbols
    :and :or :not :all :exi :=> :<=>
    :.and :.or :.not :.=> :.<=>

    ;; constants
    :@-all :true :false

    ;; scalar types
    :boolean
    :integer :integerp0 :integerp
    :realxn :realx :real :realp0 :realp

    ;; predicates for scalar types
    :is-boolean
    :is-integer :is-integerp0 :is-integerp
    :is-realxn :is-realx :is-real :is-realp0 :is-realp
    :.is-boolean
    :.is-integer :.is-integerp0 :.is-integerp
    :.is-realxn :.is-realx :.is-real :.is-realp0 :.is-realp

    ;; other predicates
    := :!= :< :<= :> :>= :is-symm-pd
    :.= :.!= :.< :.<= :.> :.>= :.is-symm-pd
    
    ;; functions
    :@ :@-slice :@-rng :@-idx :vec :array-length :num-dims
    :+ :- :* :*! :/ :^ :neg :exp :tanh :sqrt
    :sum :dot :inv :if-then-else :!

    ;; finite quantifiers
    :qand :qor :qsum :qprod :qprod! :qvec
    :.qand :.qor

    ;; distributions
    :ddirch :dcat :dinterval :dnorm :dmvnorm :dgamma :dwishart
    ;; densities
    :ddirch-density :dcat-density :dinterval-density
    :dnorm-density :dmvnorm-density :dgamma-density :dwishart-density

    ;; model symbols
    :~
    ))

(defpackage :utils
  (:use :cl)
  (:export :starts-with :assoc-lookup :zip :strcat :strcat-lines
	   :read-file :int-range :is-list-of-length
	   :append-mapcar :fdebug :compound-symbol
	   :n-symbols-not-in :symbol-not-in
	   :indent :fmt :*indent-level* :*indent-amount* :*fmt-ostream*))

(defpackage :adt
  (:use :cl :utils)
  (:export :defadt :defadt1 :adt-case :match-adt1))

(defpackage :expr
  (:use :cl :symbols :adt :utils)
  (:export
   :free-vars-in-expr
   :sexpr->expr :expr->string :is-scalar-index :is-slice-all :is-slice-range
   :*convert-boolean-functions*
   :expr-call :expr-app :expr-var :expr-const :expr-lam
   :is-expr
   :is-expr-const :make-expr-const :expr-const-name
   :is-expr-variable :make-expr-variable :expr-variable-symbol
   :is-expr-apply :make-expr-apply :expr-apply-fct :expr-apply-args
   :is-expr-lambda :make-expr-lambda :expr-lambda-var :expr-lambda-body))

(defpackage :model
  (:use :cl :symbols :adt :utils :expr)
  (:export
   :read-model :sexpr->model
   :sexpr->vtype :sexpr->decl :sexpr->decls :sexpr->distr
   :sexpr->rellhs :sexpr->rel
   :vtype->string :distr->string :rellhs->expr :pp-decl :pp-rel :pp-model

   :is-model :make-model :model-args :model-reqs :model-vars :model-body

   :is-decl :make-decl :decl-var :decl-typ

   :is-vtype
   :is-vtype-scalar :make-vtype-scalar :vtype-scalar-stype
   :is-vtype-array :make-vtype-array :vtype-array-elem-type :vtype-array-dims

   :is-distribution :make-distribution :distribution-name :distribution-args

   :is-relation
   :is-relation-stochastic :make-relation-stochastic
   :relation-stochastic-lhs :relation-stochastic-rhs
   :is-relation-block :make-relation-block :relation-block-members
   :is-relation-if :make-relation-if
   :relation-if-condition 
   :relation-if-true-branch :relation-if-false-branch
   :is-relation-loop :make-relation-loop :relation-loop-var
   :relation-loop-lo :relation-loop-hi :relation-loop-body
   :is-relation-let :make-relation-let
   :relation-let-var :relation-let-val :relation-let-body
   :is-relation-skip :make-relation-skip

   :is-rellhs
   :is-rellhs-simple :make-rellhs-simple :rellhs-simple-var
   :is-rellhs-array-elt :make-rellhs-array-elt
   :rellhs-array-elt-var :rellhs-array-elt-indices
   :is-rellhs-array-slice :make-rellhs-array-slice
   :rellhs-array-slice-var :rellhs-array-slice-indices

   :is-array-slice-index
   :is-array-slice-index-scalar :make-array-slice-index-scalar
   :array-slice-index-scalar-value
   :is-array-slice-index-range :make-array-slice-index-range
   :array-slice-index-range-lo :array-slice-index-range-hi
   :is-array-slice-index-all :make-array-slice-index-all))

(defpackage :prove
  (:use :cl :adt :expr :utils :symbols)
  (:export))

(defpackage :compile
  (:use :cl :model :expr :utils :adt :symbols)
  (:shadow :expr->string)
  (:export :compile-to-csharp))

