(load "model-utils")

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
; END utilities

(defun dag-proof-obligations (mdl)
  (po-list nil (extract-body mdl)))

(defun po-list (assumptions rels)
  (let ((to-prove nil)
	(assume assumptions))
    (dolist (r rels)
      (destructuring-bind
        ((new-assume . new-to-prove) (po assume r))
	(setf to-prove (append to-prove new-to-prove))
	(setf assume (append assume new-assume))))
    (cons assume to-prove)))

(defun po (assumptions rel)
  (let ((fct (case (rel-class rel)
		   ('deterministic #'po-determ)
		   ('stochastic #'po-stoch)
		   ('block #'po-block)
		   ('if-then #'po-if-then)
		   ('if-then-else #'po-if-then-else)
		   ('loop #'po-loop))))
    (funcall fct assumptions rel)))

(defun po-block (assumptions rel) (po-list assumptions (rel-block-body rel)))

(defun po-determ (assumptions rel)
  (let ((var (rel-var rel))
	(val (rel-val rel)))
    ; to-prove: val has type matching required type of var
    ; to-prove: if var is array elem or subrange, indices have required types
    ; new assumption: var has required type, and conclusions of each to-prove
)
)

(defun po-stoch (assumptions rel)
)

(defun po-if-then (assumptions rel)
  ; to-prove: test is boolean
  ; to-prove: body of if, with assumption that test is true
  ; test => body of if  
)

(defun po-if-then-else (assumptions rel)
  ; to-prove: test is boolean
  ; to-prove: body of if, with assumption that test is true
  ; to-prove: body of if, with assumption that test is false
  ; new assumption: test => if clause
  ; new assumption: (not test) => else clause
)

(defun po-loop (assumptions rel)
  ; to-prove: lower bound is integer
  ; to-prove: upper bound is integer
  ; to-prove: for all values of index var between upper and lower bound,
  ;           body of loop
  ; new assumption conclusions of all things proven
)

|#

(let ((mdl (read-model "halo-model.txt")))
  (with-open-file (ostrm "proof-obligations.txt" :direction :output)
    (prin1 (dag-proof-obligation mdl) ostrm)))
