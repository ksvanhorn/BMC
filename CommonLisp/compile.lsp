#!/usr/bin/clisp

(require "model-utils")
(require "codegen-csharp")

; BEGIN utilities
(defun read-model0 (ifname)
  (with-open-file (is ifname)
    (let ((*readtable* (copy-readtable nil)))
      (setf (readtable-case *readtable*) :preserve)
      (read is))))

(defun symbol-downcase (s)
  (intern (string-downcase s)))

(defun read-model (ifname)
  (let ((mdl (read-model0 ifname)))
    (dolist (s (scalar-types))
      (setf mdl (subst s (symbol-downcase s) mdl)))
    mdl))

(defun gen-csharp (mdl name-space class-name)
  (format t "using System;~%")
  (format t "using Common;~%~%")
  (format t "namespace ~a~%" name-space)
  (format t "{~%")
  (inc-indent-level
    (indent)
    (format t "[Serializable]~%")
    (indent)
    (format t "public class ~a~%" class-name)
    (indent)
    (format t "{~%")

    (inc-indent-level
      (gen-variables mdl)
      (terpri)
      (gen-args-checks mdl))

    (indent)
    (format t "}~%"))
  (format t "}~%")
)

; END utilities

(let ((mdl (read-model (first ext:*args*))))
  (gen-csharp mdl "Estimation" "HaloUpdater"))
