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

(defun dag-proof-obligation (mdl)
  (po-list nil (extract-body mdl)))

(defun po-list (proof-obligations preceding rels)
  (do ()
      ((null rels) proof-obligations)
    (push (po preceding (car rels)) proof-obligations)
    (push (car rels) preceding)
    (setf rels (cdr rels))))

(defun po (preceding rel)
  `(=> ,(reverse preceding) ,rel))
#|
  (case (rel-class rel)
	('deterministic ???)
	('stochastic ???)
	('block ???)
	('if-then ???)
	('if-then-else ???)
	('loop ???)))
|#

(let ((mdl (read-model "halo-model.txt")))
  (with-open-file (ostrm "proof-obligations.txt" :direction :output)
    (prin1 (dag-proof-obligation mdl) ostrm)))
