(defun extract-args (mdl) (cdr (second mdl)))
(defun extract-reqs (mdl) (cdr (third mdl)))
(defun extract-vars (mdl) (cdr (fourth mdl)))
(defun extract-body (mdl) (cdr (fifth mdl)))

(defun decl-var (decl) (first decl))
(defun decl-typ (decl) (second decl))

(defun type-class (typ)
  (cond ((symbolp typ)
	 'scalar)
	((listp typ)
	 'array)))
(defun elem-type (typ) (car typ))
(defun type-dims (typ) (cdr typ))

(defun rel-class (rel)
  (case (first rel)
	('<- 'deterministic)
	('~ 'stochastic)
	(:if (case (length (rest rel))
		   (2 'if-then)
		   (3 'if-then-else)))
	(:for 'loop)))

(defun rel-var (rel) (second rel))
(defun rel-val (rel) (third rel))
(defun rel-distr (rel) (third rel))
(defun rel-if-condition (rel) (second rel))
(defun rel-true-branch (rel) (third rel))
(defun rel-false-branch (rel) (fourth rel))
(defun rel-loop-var (rel) (second rel))
(defun rel-loop-bounds (rel) (third rel))
(defun rel-loop-body (rel) (fourth rel))
(defun bounds-lo (bnds) (first bnds))
(defun bounds-hi (bnds) (second bnds))

(defun expr-class (expr)
  (cond ((numberp expr) 'literal-num)
	((symbolp expr) 'variable)
	((consp expr)
	 (let ((h (first expr)))
	   (cond ((eq '@ h) 'array-app)
		 ((symbolp h) 'funct-app))))))

(defun op (fct-expr) (first fct-expr))
(defun args (fct-expr) (rest fct-expr))

(defun array-op (arr-expr) (second arr-expr))
(defun array-args (arr-expr) (cddr arr-expr))

(defun index-class (index-expr)
  (cond
   ((eq :all index-expr)
    'all)
   ((and (consp index-expr) (eq :range (first index-expr)))
    'range)
   (t 'index)))

(defun range-lo (range-expr) (second range-expr))
(defun range-hi (range-expr) (third range-expr))
