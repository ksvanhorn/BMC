(in-package :prove)

(load "Classic-rtp/unify")
(load "Classic-rtp/cnf")
(load "Classic-rtp/prover")

(setq *resource-limit* 50000)

(defun prove-thms-axs (thms axioms)
  (let* ((assume (convert-to-cnf (conjunction axioms)))
	 (failed nil)
	 (count 0))
    (dolist (x thms)
      (let ((result (multiple-value-list (prove x :axioms assume))))
	(if (eq t (first result))
	  (push (convert-to-cnf x) assume)
	  (push x failed))))
    (values failed count)))
