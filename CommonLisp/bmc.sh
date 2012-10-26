#!/usr/bin/env clisp

(load "packages")
(load "symbols")
(load "adt")
(load "utils")
(load "expr")
(load "model")
(load "mcimpl")
(load "prove")
(load "compile")

(defpackage :bmc
  (:use :cl :symbols :utils :model :compile :mcimpl))
(in-package :bmc)

(if (= 0 (length ext:*args*))
    (format t "Arguments: model-file-name impl-file-name output-file-name-base ~
                        class-name")
  (progn
    (assert (= 4 (length ext:*args*)))
    (let ((ifname (first ext:*args*))
	  (implfname (second ext:*args*))
	  (ofname-base (third ext:*args*))
	  (class-name (fourth ext:*args*))
	  (csharp-namespace "Estimation")
	  (mdl)
	  (impl))
      (assert (stringp ifname))
      (assert (stringp ofname-base))
      (assert (stringp implfname))
      (assert (stringp class-name))
      (assert (stringp csharp-namespace))
      (setf mdl (read-model ifname))
      (setf impl (read-mcimpl implfname))
      (with-open-file (ostrm (strcat ofname-base ".cs") :direction :output)
	(let ((*indent-amount* 4)
	      (*fmt-ostream* ostrm))
	  (compile-to-csharp csharp-namespace class-name mdl impl)))
      (with-open-file (ostrm (strcat ofname-base "-tests.cs")
			     :direction :output)
	(let ((*indent-amount* 4)
	      (*fmt-ostream* ostrm))
	  (write-test-file class-name mdl impl))))))

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
