(require "model-utils")




(defun get-cb (kw x) (cdr (assoc kw x)))

(defun cb-csharp ()
  '((:gen-decl . gen-decl-csharp) (:gen-lcom . gen-lcom-csharp)))

; qand, qsum?
; quantifiers: Q(v, lo, hi, e) = Q(lo, hi, v => e) [for C# code]
;                              = Q#(lo, hi, v1, ..., vn) where
;                                v1, ..., vn are the vars appearing in e
; array slices: x[a, b:c, , d] = array_slice_i_r_a_i(x, a, (b, c), :none, d)