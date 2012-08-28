(defpackage :symbols
  (:use :cl)
  (:export
    ;; symbol classes
    :is-fquant-symbol :is-const-symbol :is-fct-symbol :is-distr-symbol
    :is-variable-symbol

    ;; logical symbols
    :and :or :not :all :exi :=> :<=>

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

    ;; other predicates
    := :< :<= :> :>= :is_symm_pd
    
    ;; functions
    :@ :@-slice :@-rng :@-idx :vec :array-length :num-dims
    :+ :- :* :/ :^ :neg :exp :tanh :sqrt
    :sum :dot :inv

    ;; finite quantifiers
    :qand :qor :qsum :qprod

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
  (:export :starts-with :assoc-lookup :zip))

(defpackage :expr
  (:use :cl :symbols :adt :utils)
  (:export
   :sexpr->expr :expr->string :is-scalar-index :is-slice-all :is-slice-range
   :with-print-options :default-is-binop :default-fct-name :default-quant-format
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
   :is-model :make-model :model-args :model-reqs :model-vars :model-body

   :is-decl :make-decl :decl-var :decl-typ

   :is-type
   :is-type-scalar :make-type-scalar :type-scalar-stype
   :is-type-array :make-type-array :type-array-elem-type :type-array-dims

   :is-relation
   :is-relation-deterministic :make-relation-deterministic
   :relation-deterministic-lhs :relation-deterministic-rhs
   :is-relation-stochastic :make-relation-stochastic
   :relation-stochastic-lhs :relation-stochastic-rhs
   :is-relation-block :make-relation-block :relation-block-members
   :is-relation-if-then :make-relation-if-then
   :relation-if-then-condition :relation-if-then-body
   :is-relation-if-then-else :make-relation-if-then-else
   :relation-if-then-else-condition
   :relation-if-then-else-true-branch :relation-if-then-else-false-branch
   :is-relation-loop :make-relation-loop :relation-loop-var
   :relation-loop-lo :relation-loop-hi :relation-loop-body

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


