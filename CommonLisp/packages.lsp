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
    :+ :- :* :/ :^ :neg :exp :tanh :sqrt
    :sum :dot :inv

    ;; finite quantifiers
    :qand :qor :qsum :qprod
    :.qand :.qor

    ;; distributions
    :ddirch :dcat :dinterval :dnorm :dmvnorm :dgamma :dwishart
    ;; densities
    :ddirch-density :dcat-density :dinterval-density
    :dnorm-density :dmvnorm-density :dgamma-density :dwishart-density

    ;; model symbols
    :<- :~
    ))

(defpackage :adt
  (:use :cl)
  (:export :defadt :defadt1 :adt-case :match-adt1))

(defpackage :utils
  (:use :cl)
  (:export :starts-with :assoc-lookup :zip :strcat :read-file
	   :indent :fmt :*indent-level* :*indent-amount* :*fmt-ostream*))

(defpackage :expr
  (:use :cl :symbols :adt :utils)
  (:export
   :sexpr->expr :expr->string :is-scalar-index :is-slice-all :is-slice-range
   :with-print-options :default-is-binop :default-fct-name :default-quant-format
   :*convert-boolean-functions*
   :is-expr
   :is-expr-literal :make-expr-literal :expr-literal-value
   :is-expr-const :make-expr-const :expr-const-symbol
   :is-expr-variable :make-expr-variable :expr-variable-symbol
   :is-expr-quantifier :make-expr-quantifier :expr-quantifier-op
   :expr-quantifier-lo :expr-quantifier-hi :expr-quantifier-var
   :expr-quantifier-body
   :is-expr-apply :make-expr-apply :expr-apply-fct :expr-apply-args))

(defpackage :model
  (:use :cl :symbols :adt :utils :expr)
  (:export
   :read-model :sexpr->model
   :sexpr->vtype :sexpr->decl :sexpr->distr :sexpr->rellhs :sexpr->rel
   :vtype->string :distr->string :rellhs->string :pp-decl :pp-rel :pp-model

   :is-model :make-model :model-args :model-reqs :model-vars :model-body

   :is-decl :make-decl :decl-var :decl-typ

   :is-vtype
   :is-vtype-scalar :make-vtype-scalar :vtype-scalar-stype
   :is-vtype-array :make-vtype-array :vtype-array-elem-type :vtype-array-dims

   :is-distribution :make-distribution :distribution-name :distribution-args

   :is-relation
   :is-relation-deterministic :make-relation-deterministic
   :relation-deterministic-lhs :relation-deterministic-rhs
   :is-relation-stochastic :make-relation-stochastic
   :relation-stochastic-lhs :relation-stochastic-rhs
   :is-relation-block :make-relation-block :relation-block-members
   :is-relation-if :make-relation-if
   :relation-if-condition 
   :relation-if-true-branch :relation-if-false-branch
   :is-relation-loop :make-relation-loop :relation-loop-var
   :relation-loop-lo :relation-loop-hi :relation-loop-body
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

(defpackage :compile
  (:use :cl :model :expr :utils :adt :symbols)
  (:export :compile-to-csharp))
