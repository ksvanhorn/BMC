#!/usr/bin/env clisp -ansi

(load "packages")
(load "symbols")
(load "adt")
(load "utils")
(load "expr")
(load "model")

(defpackage :bmpp
  (:use :cl :symbols :adt :utils :expr :model))
(in-package :bmpp)

(defun main ()
  (assert (= 2 (length ext:*args*)))
  (let ((ifname (first ext:*args*))
	(ofname (second ext:*args*))
	(mdl))
    (assert (stringp ifname))
    (assert (stringp ofname))
    (setf mdl (read-model ifname))
    (with-open-file (ostrm ofname :direction :output)
      (let ((*indent-amount* 2)
	    (*fmt-ostream* ostrm))
        (pp-model mdl)))))

(main)
