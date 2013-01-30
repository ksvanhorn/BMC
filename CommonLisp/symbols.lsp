(in-package :symbols)

(defun is-fquant-symbol (s)
  (member s '(qand .qand qmin qmax qor .qor qnum qsum qprod qprod! qvec q@sum)))

(defun is-const-symbol (s)
  (member s '(@-all true false %pi %e %undef %infty+ %infty- %true-pred
	      %min-int %max-int)))

(defun is-variable-symbol (s)
  (and (symbolp s) (not (is-const-symbol s))))

(defun is-fct-symbol (s)
  (or (member s +fct-symbols+) (is-fquant-symbol s)))

(defparameter +fct-symbols+
  '(and or not => <=>
    .and .or .not .=> .<=>
    is-number is-numberu is-integeru
    is-boolean is-integer is-integerp0 is-integerp is-even is-odd
    is-realxn is-realx is-real is-realp0 is-realp
    .is-boolean .is-integer .is-integerp0 .is-integerp
    .is-realxn .is-realx .is-real .is-realp0 .is-realp
    = != < <= > >= is-symm-pd
    .= .!= .< .<= .> .>= .is-symm-pd quad fac
    @ @-slice @-rng @-idx int real
    copy
    vec rmat array-length num-dims abs-det mv-gamma-fct trace
    + - * *! / ^ neg exp log tanh max min vmax
    sum dot inv inv-pd if-then-else ! gamma-fct length
    ^1/2 ^2 ^-1 ^-1/2 ^-2
    @^1/2 @^2 @^-1 @^-1/2 @^-2
    @+ @- @* @/ $*
    fst snd thd tuple
    diag_mat o* o^2 cons cons-col cons-row real-zero-arr const-array
    eigen mu-form form-covariance-decomp ksi-mean-coeffs-cov-decomp sigma-form
    covariance-decomp x-mean-coeffs-cov-decomp log-pnorm-interval
    ddirch-density dcat-density dinterval-density
    dnorm-density dmvnorm-density dgamma-density dwishart-density
    ddirch-log-density dcat-log-density dinterval-log-density
    dnorm-log-density dmvnorm-log-density dgamma-log-density dnormvec-log-density
    dwishart-log-density))

(defun is-scalar-type-symbol (x)
  (member x +scalar-type-symbols+))

(defparameter +scalar-type-symbols+
  '(boolean integer integerp0 integerp realxn realx real realp0 realp))

(defun is-distr-symbol (x)
  (member x +distr-symbols+))

(defparameter +distr-symbols+
  '(ddirch dcat dinterval dnorm dmvnorm dgamma dwishart dnorm-trunc dnormvec dnormvec-trunc))
