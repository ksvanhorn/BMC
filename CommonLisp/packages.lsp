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
    :@-all :true :false :%pi :%e :%undef :%infty+ :%infty- :%true-pred
    :%min-int :%max-int

    ;; scalar types
    :boolean
    :integer :integerp0 :integerp
    :realxn :realx :real :realp0 :realp
    :@-all-type :@-rng-type :@-idx-type

    ;; type constructors
    :int-map :pair

    ;; predicates for scalar types
    :is-boolean :is-number :is-numberu :is-integeru
    :is-integer :is-integerp0 :is-integerp :is-even :is-odd
    :is-realxn :is-realx :is-real :is-realp0 :is-realp
    :.is-boolean
    :.is-integer :.is-integerp0 :.is-integerp
    :.is-realxn :.is-realx :.is-real :.is-realp0 :.is-realp

    ;; other predicates
    := :!= :< :<= :> :>= :is-symm-pd
    :.= :.!= :.< :.<= :.> :.>= :.is-symm-pd
    
    ;; functions
    :copy :int #| real |#
    :@ :@-slice :@-rng :@-idx :vec :rmat :array-length :num-dims
    :abs-det :mv-gamma-fct :trace :quad :fac
    :+ :- :* :*! :/ :^ :neg :exp :log :tanh :max :min :vmax
    :sum :dot :inv :inv-pd :if-then-else :! :gamma-fct :length
    :^1/2 :^2 :^-1 :^-1/2 :^-2
    :@^1/2 :@^2 :@^-1 :@^-1/2 :@^-2
    :@+ :@- :@* :@/ :$*
    :fst :snd
    :diag_mat :o* :o^2 :cons :cons-col :cons-row :real-zero-arr
    :eigen

    ;; finite quantifiers and associated constants
    :qand :qor :qmin :qmax :qnum :qsum :qprod :qprod! :qvec :q@sum
    :.qand :.qor

    ;; distributions
    :ddirch :dcat :dinterval :dnorm :dmvnorm :dgamma :dwishart
    :dnorm-trunc
    ;; densities
    :ddirch-density :dcat-density :dinterval-density
    :dnorm-density :dmvnorm-density :dgamma-density :dwishart-density
    ;; log densities
    :ddirch-log-density :dcat-log-density :dinterval-log-density
    :dnorm-log-density :dmvnorm-log-density :dgamma-log-density
    :dwishart-log-density

    ;; model symbols
    :~
    ))

(defpackage :utils
  (:use :cl)
  (:export :flet* :while :starts-with :assoc-lookup :zip :strcat :strcat-lines
	   :read-file :int-range :is-list-of-length
	   :has-duplicates :has-no-duplicates
	   :list->pair-list :fn :dolist-inter
	   :append-mapcar :fdebug :compound-symbol :bmc-symb
	   :indent :fmt :fmt-blank-line
	   :*indent-level* :*indent-amount* :*fmt-ostream*
	   :alambda :self :rethrow-error))

(defpackage variables
  (:nicknames :vars)
  (:use :cl :utils)
  (:export :vars-symbol :special-var :new-var :n-new-vars))

(defpackage :adt
  (:use :cl :utils)
  (:export :defadt :defadt1 :adt-case :match-adt1))

(defpackage :lazy
  (:use :cl)
  (:export :lcdr :lcar :lcons :lappend :list->lazy :lazy->list))

(defpackage :expr
  (:use :cl :symbols :adt :utils :variables)
  (:export
   :free-vars-in-expr :occurs-free :rename-var
   :sexpr->expr :expr->string :is-scalar-index :is-slice-all :is-slice-range
   :*convert-boolean-functions* :is-let-expr :is-quant-expr
   :expr-call :expr-app :expr-var :expr-const :expr-lam
   :is-expr
   :is-expr-const :make-expr-const :expr-const-name
   :is-expr-variable :make-expr-variable :expr-variable-symbol
   :is-expr-apply :make-expr-apply :expr-apply-fct :expr-apply-args
   :is-expr-lambda :make-expr-lambda :expr-lambda-var :expr-lambda-body))

(defpackage :type-inference
  (:use :cl :utils :symbols :adt :lazy :expr)
  (:export :bare-type :bare-type-scalar :bare-type-array :bare-type-int-map
           :bare-type-pair :is-bare-type 
	   :is-bare-type-scalar :is-bare-type-array :is-bare-type-int-map
	   :is-bare-type-pair
	   :make-bare-type-scalar :make-bare-type-array :make-bare-type-int-map
	   :make-bare-type-pair
	   :bare-type-scalar-stype
	   :bare-type-array-elem-type :bare-type-array-num-dims
	   :bare-type-int-map-return-type
	   :bare-type-pair-fst-type :bare-type-pair-snd-type

           :sexpr->bare-type :infer-type
	   :var-type :no-var-types :add-var-type :assocs->env))

(defpackage :model
  (:use :cl :symbols :adt :utils :expr :variables)
  (:export
   :read-model :sexpr->model :raw-sexpr->model
   :sexpr->vtype :sexpr->decl :sexpr->decls :sexpr->distr
   :sexpr->rellhs :sexpr->rel
   :vtype->string :distr->string :rellhs->expr :rellhs->string
   :pp-decl :pp-rel :pp-model
   :vars-names :args-vars-names
   :is-update :is-pure-rel

   :is-model :make-model :model-args :model-reqs :model-vars :model-body

   :is-decl :make-decl :decl-var :decl-typ :args-vars-dims :decl-dims

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
   :is-relation-mh :make-relation-mh :relation-mh-lets
   :relation-mh-proposal-distribution :relation-mh-acceptmon
   :relation-mh-log-acceptance-ratio
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
  (:use :cl :adt :expr :utils :variables :symbols)
  (:import-from :alexandria :define-constant)
  (:export :is-provable :also-assume :*prover* :can-prove :assuming
	   :assuming-se :subst-expr))

(defpackage :mcimpl
  (:use :cl :model :expr :model :utils :symbols :adt :prove)
  (:export :make-mcimpl :is-mcimpl :mcimpl-parameters
	   :mcimpl-updates :mcimpl-expectations :mcimpl-acceptmons
	   :free-vars-in-rellhs
	   :sexpr->mcimpl :read-mcimpl :params-names))

(defpackage simplify
  (:use :cl :adt :expr :utils :symbols :prove)
  (:export :simplify-expr))

(defpackage :compile
  (:use :cl :mcimpl :model :variables :variables :expr :utils :adt :symbols :type-inference)
  (:shadow :expr->string)
  (:export :compile-to-csharp :write-test-file :write-test-updates :de-alias-impl))

