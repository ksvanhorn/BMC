#!/usr/bin/env clisp -ansi

(load "packages")
(load "symbols")
(load "adt")
(load "utils")
(load "expr")
(load "model")
;(load "prove")
;(load "compile")

(defpackage :bmc
  (:use :cl :symbols :adt :utils :expr :model))
(in-package :bmc)

(assert (<= 3 (length ext:*args*)))
(let ((ifname (first ext:*args*))
      (ofname (second ext:*args*))
      (class-name (third ext:*args*))
      (csharp-namespace "Estimation")
      (mdl))
  (assert (stringp ifname))
  (assert (stringp ofname))
  (assert (stringp class-name))
  (assert (stringp csharp-namespace))
  (setf mdl (read-model ifname))
  (with-open-file (ostrm ofname :direction :output)
    (let ((*indent-amount* 2)
	  (*fmt-ostream* ostrm))
    (compile-to-csharp csharp-namespace class-name mdl)))

  ; TODO: rework unit tests
  ; TODO: validation of initial Markov chain state
  ; TODO: generate updates for deterministic vars
  ; TODO: prove model var dimension lengths are nonnegative integers
  ; TODO: prove DAG, var constraints, etc.
  ; TODO: Use multi-dimensional array type that indexes from 1?

  ; TODO?: check that dim lengths in args section are integers
  ; TODO?: check that reqs section and if tests are boolean.
  ; TODO [CFG]: check that scalar types not used as vars
  ; TODO: When creating validation code, we need to account for array slices
  ;   [Not needed for halo]
