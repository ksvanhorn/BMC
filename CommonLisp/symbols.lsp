(in-package :symbols)

(defun is-fquant-symbol (s)
  (member s '(qand qor qsum qprod)))

(defun is-const-symbol (s)
  (member s '(@-all true false)))

(defun is-variable-symbol (s)
  (and (symbolp s) (not (is-const-symbol s))))

(defun is-fct-symbol (s)
  (member s +fct-symbols+))

(defconstant +fct-symbols+
  '(and or not => <=>
    is-boolean is-integer is-integerp0 is-integerp
    is-realxn is-realx is-real is-realp0 is-realp
    = < <= > >= is_sym_pd
    @ @-slice vec array-length num-dims
    + - * / ^ neg exp tanh sqrt
    sum dot inv
    ddirch-density dcat-density dinterval-density
    dnorm-density dmvnorm-density dgamma-density dwishart-density))

(defun is-distr-symbol (x)
  (member x +distr-symbols+))

(defconstant +distr-symbols+
  '(ddirch dcat dinterval dnorm dmvnorm dgamma dwishart))
