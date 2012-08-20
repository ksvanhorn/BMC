#!/usr/bin/clisp

(load "utils")
(load "expr")
(load "model")
(load "compile")

(defpackage :bmc
  (:use :common-lisp :compile :model))
(in-package :bmc)

(assert (<= 3 (length ext:*args)))
(let ((ifname (first ext:*args*))
      (ofname (second ext:*args*))
      (class-name (third ext:*args*))
      (csharp-namespace "Estimation")
      (mdl))
  (assert (stringp ifname))
  (assert (stringp ofname))
  (assert (stringp updater-name))
  (assert (stringp csharp-namespace))
  (setf mdl (read-model ifname))
  (with-open-file (ostrm ofname :direction :output)
    (compile-to-csharp csharp-namespace class-name mdl ostrm)))
