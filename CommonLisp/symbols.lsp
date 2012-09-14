(in-package :symbols)

(defun is-fquant-symbol (s)
  (member s '(qand .qand qor .qor qsum qprod qprod! qvec)))

(defun is-const-symbol (s)
  (member s '(@-all true false %pi %e)))

(defun is-variable-symbol (s)
  (and (symbolp s) (not (is-const-symbol s))))

(defun is-fct-symbol (s)
  (or (member s +fct-symbols+) (is-fquant-symbol s)))

(defconstant +fct-symbols+
  '(and or not => <=>
    .and .or .not .=> .<=>
    is-boolean is-integer is-integerp0 is-integerp
    is-realxn is-realx is-real is-realp0 is-realp
    .is-boolean .is-integer .is-integerp0 .is-integerp
    .is-realxn .is-realx .is-real .is-realp0 .is-realp
    = != < <= > >= is-symm-pd
    .= .!= .< .<= .> .>= .is-symm-pd quad
    @ @-slice vec array-length num-dims abs-det mv-gamma-fct trace mat*
    + - * *! / ^ neg exp tanh sqrt sqr max
    sum dot inv if-then-else ! gamma-fct length vec-
    ddirch-density dcat-density dinterval-density
    dnorm-density dmvnorm-density dgamma-density dwishart-density))

(defun is-scalar-type-symbol (x)
  (member x +scalar-type-symbols+))

(defconstant +scalar-type-symbols+
  '(boolean integer integerp0 integerp realxn realx real realp0 realp))

(defun is-distr-symbol (x)
  (member x +distr-symbols+))

(defconstant +distr-symbols+
  '(ddirch dcat dinterval dnorm dmvnorm dgamma dwishart))
