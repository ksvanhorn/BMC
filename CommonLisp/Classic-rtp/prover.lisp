;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A Classic Resolution Theorem Prover.
;;;`
;;;  Vesrion 0.02  Last Modified: June 3rd, 2004
;;;
;;;  Written by Shaul Markovitch with some code borrowed from Russell and Norvig and
;;;  Mark Kantrowitz.
;;;
;;;  The main purpose of this system is to allow students to
;;;  experiment with a classic resolution theorem prover, and to
;;;  compare various resolution strategies.  The system is composed of
;;;  4 files: 
;;;  1. prover.lisp [written by Shaul Markovitch].  This is the main
;;;     file and contains the resolution theorem prover
;;;  2. unify.lisp  [written by Peter Norvig].  The file contains the
;;;      unify function and its auxilary functions.
;;;  3. cnf.lisp [Borrowed from Russell and Norvig].  Contains the
;;;     code to convert formula to cnf form.
;;;  4. infix.lisp [borrowed from Norvig and Russell who borrowed it
;;;     from Mark Kantrowitz]  This file is not necessary.  Its main
;;;     purpose is to allow writting axioms and theorems in infix form.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Change log:
;;;    3/6/04 - Add "setify" operation and applied it on a new
;;;             resolvant
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Planed changes:
;;;
;;;  1.  Extend the system to return binding of free and existential
;;;      variables in the query (theorem).  Currently the system is
;;;      only able to answer yes-no questions.
;;;  2.  Add a function that pretty prints the proof.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main algorithms and data structures.
;;;
;;; The main algorithm implements classic resolutions.  It starts with
;;; a clause set that contains the clauses of the axioms and the
;;; negated theorem.  It then repeatedly
;;; selects two clauses, resolve them, and add the resolvent to the
;;; clause set.  The algorithm stops when it finds an empty clause (in
;;; which case it returns T).  If there are no more candidates for unification,
;;; the algorithm returns NIL.  If the alloted resources are
;;; exhausted, the algorithm stops with no definitive answer.
;;;
;;; proof-node
;;;   A proof-node is a structure that holds one resolvent created
;;;   during the proof.  The structure consists of tghe clause itself,
;;;   the binding list resulted from the unification of its parent
;;;   clauses, pointers to the proof-nodes of its parent clauses, and
;;;   various flags and counters.  The proof-node that contains the
;;;   empty clause at the end of the proof process is in fact the rot
;;;   of the PROOF-TREE that can be traced by following the parent
;;;   links.  The algorithm starts with a list of proof-nodes, one for
;;;   each clause of the basic clause set (of the axioms and the
;;;   negated theorem).
;;;
;;; candidate-list
;;;   The purpose of the candidate list is to save time in finding
;;;   candidates for resolution.  A candidate is a pair ((n1 i1)(n2 i2)).
;;;   The meaning of a candidate: the i1 literal of the clause of node
;;;   n1 and the i2 literal of the clause of proof-node n2 contain
;;;   complementary predicates.  It does not mean that the literals
;;;   are unifiable, but only that they are potentially unifiable.
;;;   Note that the index is 0 based.  Whenever a new resolvent is
;;;   added, the system needs to test only for new candidates
;;;   resulting from the new resolvent.
;;;
;;; predicate-index
;;;   The purpose of the clause index is to make the process of
;;;   finding new candidates more efficient.  The index is a hash
;;;   table with an entry for each predicate name.  Each entry
;;;   contains two lists.  The first is a list of pointers to places
;;;   where the predicate apears in positive literals, the second is a
;;;   list of pointers to places where the predicate appears in a
;;;   negative literal.  A pointer is a pair.  The first element
;;;   points to a proof-node whose clause contains the literal, and
;;;   the second is an index of this literal within the clause.  When
;;;   a new clause is created, each of its literals is combined with
;;;   the appropriate list in the index to create a new list of
;;;   candidates.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Resolution Strategies
;;;
;;;    There are 4 built in mechanisms to control the resolutions 
;;;    process.  
;;;
;;;    candidate-filters
;;;       A veriable contains a list of function names or lambda 
;;;       expressions.  When a new candidate is generated the program
;;;       passes the candidate through all the filters, from left
;;;       to right.  For example, the set of support filter checks
;;;       that one of the candidates is from the set-of-support.
;;;       Each of the functions gets one argument - a candidate
;;;
;;;    clause-filters
;;;       Filters a newly generated clauses.
;;;       Each of the functions gets two arguments: the new clause
;;;       and the list of all proof-nodes.  For example, the uniqueness
;;;       filter tests whether the new clause already exists.
;;;
;;;    initial-clause-filters
;;;       Same as the above, but applied only during the initialization
;;;       to the axioms and negated-theorem clauses.
;;;
;;;    candidate-ordering-strategies
;;;       A list of functions used for sorting the candidates.  Each of 
;;;       the functions gets two arguments (two candidates) and returns
;;;       one of the three atoms: < > =.  The order of the strategies
;;;       determines a lexicographic order on the candidates.  The 
;;;       strategies are applied from left to right as long 
;;;       as = is returned.
;;;
;;;    All the control mechanisms are stored in a structure of type
;;;    resolution (described below).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; proof-node is the main data structure.  Each of the clauses is stored
;;; in such a structure.  The parent links allow retrival of the proof 
;;; tree.  New strategies and filters may need to add fields to this 
;;; record.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct proof-node 
   clause                    ; The clause (either from the axioms or a 
                             ; resolvent
   parent                    ; A pointer to the candidate that generated 
                             ; this clause.  The candidate contains pointers 
                             ; to the two nodes that 
                             ; contain the parent clauses.
   binding                   ; The binding list generated by unifying the 
                             ; parents
   setofsupport              ; A flag - indicates whether the clause is a
                             ; decendant of the set-of-support.
   (depth 0)                 ; The depth of the node = 1 + the max depth of 
                             ; the parents.
   )
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; proof-result
;;;    A structure used for returning the proof results.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct proof-result 
    answer                   ; either T (for success), NIL (for failure)
                             ; or :resource-limit (means that the prover 
                             ; stoped due to exhaustion of the alloted 
                             ; resources
    n-resolutions            ; The number of resolutions attempted.
    proof                    ; A pointer to the proof-node that contains 
                             ; the empty clause.  The proof tree is defined 
                             ; by the parent links. 
    )


(defparameter *resource-limit* 1000
    "The maximum number of resolutions allowed for proving one theorem")

(defvar *trace-prover* nil
    "When T, the prover displays each unification attempt")

(defvar *show-progress* 20
    "When not nil, shows a dot every K resolutions")
   
(defvar *renaming-table* (make-hash-table)
     "Currently not in use.  Will be used in later versions for displaying 
      query variables bindings")

(defvar *axioms* nil
      "A variable used to hold the axioms after reading them")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  User functions
;;;    At this stage the user has very limited interface with the prover.  
;;;    She can read axioms from a file and ask the program to prove theorems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; read-axioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-axioms (file)
  "Reads a file and returns the set of the axioms listed in the file
   in cnf.  The axioms are also stored in the global var *axioms*.
   The axioms are written in infix format, as described in the file 
   infix.lisp"
 (let ((axioms nil) new-line)
  (with-open-file (f file :direction :input )
     (setf new-line (read-line f nil :eof))
     (loop while (not (eql new-line :eof)) do
       (push (string->prefix new-line) axioms)
       (setf new-line (read-line f nil :eof))))
  (setf *axioms*
	(convert-to-cnf (conjunction (reverse axioms))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prove
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prove (theorem &key (axioms *axioms*))
  "The main user function.  Gets the theorem and axioms and calls the 
  theorem prover.  The axioms are assumed to be in cnf.  The theorem can 
  be either an infix expression in a string, for example
  exi(X,Y, mother(X,Y) ^ ~mother(Y,X))
  Or a prefix lisp expression, for example
  (EXI (?X ?Y) (AND (MOTHER ?X ?Y)(NOT (MOTHER ?Y ?X))))
  The function returns T if a proof was found, NIL if there is no  proof, 
  and :resource-limit if the alloted resources are exhausted.
  The second value returned is the number of resolutions executed during 
  the proving process." 
  (let ((result 
          (theorem-prover
             (if (stringp theorem)(string->prefix theorem)
		   theorem) axioms)))
    (values (proof-result-answer result) (proof-result-n-resolutions result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theorem-prover
;;;   The main resolution procedure. It expects a theorem in prefix
;;;   form and a list of axioms converted to cnf.  The reason for the
;;;   format difference: many times we want to load one set of axioms
;;;   and prove many theorems using these axioms.
;;;   The output is of type PROOF-RESULT.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun theorem-prover (theorem axiom-clauses)
  (setf *renaming-table* (make-hash-table))
  (let* ((theorem-clauses (convert-to-cnf (list 'not theorem)))
	 (theorem-nodes (loop for c in theorem-clauses
		      collect (make-proof-node :clause c
					       :setofsupport t)))
	 (axiom-nodes (loop for c in axiom-clauses
		      collect (make-proof-node :clause c)))
	 (proof-nodes (append axiom-nodes theorem-nodes))
	 (predicate-index (make-hash-table))
	 (candidates nil))
    ;; The following code initializes the candidate list.  It takes each
    ;; clause in the axioms+theorem clauses and merges its resolution
    ;; candidate with the the candidate list using the candidate ordering
    ;; strategies to determine the sort order.
    (setf proof-nodes (loop for node in proof-nodes
			    when (apply-initial-clause-filters
				  (proof-node-clause node)
				  proof-nodes)
			    collect node))
    (loop for proof-node in proof-nodes
          for new-candidates = (sort (get-clause-candidates proof-node
						   predicate-index)
						    #'candidate-ordering)
	  do
	  (setf candidates (merge 'list
			   new-candidates
			   candidates
			    #'candidate-ordering))
	  (update-predicate-index proof-node predicate-index))
    ;; The main loop.  Keeps poping candidates from the candidate list,
    ;; unifying them and adding the new clauses.
    (loop for n-resolutions from 1 with resolvant with binding with
	  cand with new-clause with new-candidates do
	  (when (and *show-progress* (= (mod n-resolutions
					     *show-progress*) 0))
		(format t ".")(force-output))
	  (when (> n-resolutions *resource-limit*)
					;failure due to time limit
		   (return  (make-proof-result
			       :answer :resource-limit
			       :n-resolutions n-resolutions)))
	  (when (null candidates)
					;not a theorem - no proof exists 
		(return (make-proof-result
			       :answer nil
			       :n-resolutions n-resolutions)))
	  (setf cand (pop candidates))
	  (when *trace-prover*
		(format t "~%Trying to unify: ~%    Literal ~d of ~A ~%    Literal ~d of ~A"
			(second (first cand))
			(proof-node-clause (first (first cand)))
			(second (second cand))
			(proof-node-clause (first (second cand)))))
	  (setf binding (unify-cand cand))
	  (when *trace-prover*
		(format t"~%    Unification result: ~A" binding))
	  (cond ((not (eql binding +fail+))
		 ;; Unification was successful. A new clause is
		 ;; generated, and its variables are renamed.
		(setf new-clause
                    (setify
		      (rename-variables
		        (subst-bindings binding (resolve-cand
							  cand)) t)))
		(cond ( (apply-clause-filters new-clause proof-nodes)
			;; The clause filters allow us to filter out
			;; newly generated clauses.
			
			(setf resolvant
			      (make-proof-node
			       :clause new-clause
			       :parent cand
			       :setofsupport (or (proof-node-setofsupport
						  (first (first cand)))
						 (proof-node-setofsupport
						  (first (second cand))))
			       :depth (1+ (max (proof-node-depth
						(first (first cand)))
					       (proof-node-depth
						(first (second cand)))))
			       :binding binding))
			(when *trace-prover*
			      (format t "~%    New Clause:~A"
				      (proof-node-clause resolvant)))
			(when (null (proof-node-clause resolvant))
			      (return (make-proof-result
				       :answer t
				       :n-resolutions n-resolutions
				       :proof resolvant)))
			(push resolvant proof-nodes)
			(setf new-candidates (sort (get-clause-candidates resolvant
						   predicate-index)
						   #'candidate-ordering))
			(setf candidates (merge 'list
			    new-candidates
			    candidates
			    #'candidate-ordering))
			(update-predicate-index resolvant predicate-index))
		      (t (when *trace-prover*
			       (format t "~%    Clause ~A was filtered out."
				       new-clause)))))
		(t (when *trace-prover*
			 (format t "~%    Unification failed."))))
		)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setify
;;;   Converts a list into a set (i.e. - removes duplicates)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setify (list &key (test #'equalp)(key #'identity))
  (let ((new-set nil))
    (loop for el in list do (setf new-set (adjoin el new-set :key key
					       :test test)))
    new-set))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convert-to-cnf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-to-cnf (p)
  "This function is somewhat redundant.  Its main purpose is to
   correct several problems with the cnf returned by Norvig's program"
  (let ((cnf (->cnf p)) clauses)
    (cond ((literal-clause? cnf)
	    (setf clauses (list (list cnf))))
	  ((eql (first cnf) 'or)
	   (setf clauses (list (rest cnf))))
	  (t 
	   (when (eql (first cnf) 'and)
		 (setf cnf (rest cnf)))
	   (setf clauses
		 (loop for c in cnf collect
		       (cond ((literal-clause? c)(list c))
			     ((eql (first c) 'or)(rest c))
			     (t c))))))
    (loop for clause in clauses
	  when (tautology-filter clause)
	  collect (rename-variables clause t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; update-predicate-index
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-predicate-index
  (clause-node index)
  "This funcation gets a new clause and the current predicate index
   and updates the entries for the predicate symbols appearing in the
   clause.  The predicate index is a hash table where the keys are
   the predicate names.  For each predicate, the entry contains two
   list.  The first has pointers to positive literals and the second
   to negative literals.  A pointer is a pair.  The first element
   points to the clause proof node and the second is the index of the
   literal in the clause."
  (loop for literal in (proof-node-clause clause-node)
	for literal-index from 0
	for predicate-name = (get-predicate-name literal)
	for hash-entry = (or (gethash predicate-name index)
			     (list nil nil))
	do (if (negative-literal literal)
	       (setf (second hash-entry)
		     (cons (list clause-node literal-index)
			   (second hash-entry)))
	     (setf (first  hash-entry)
		     (cons (list clause-node literal-index)
			   (first hash-entry))))
	(setf (gethash predicate-name index) hash-entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-predicate-name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-predicate-name
  (literal)
  "returns the predicate symbol of a literal."
  (if (negative-literal literal)(first (second literal))(first
							 literal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; negative-literal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun negative-literal (literal)
  "Returns true if a negative literal."
  (eql (first literal) 'not))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-clause-candidates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-clause-candidates
  (clause-node predicate-index)
  "Returns all the resolution candidates that were added by adding
   the new clause to the clause database.  For each positive lietral
   in the new clause, the new candidates are all the clauses
   where the literal predicate appears as a negative literal.
   This list is readily available in the predicate index.
   The same is done for a new negative literal.  The new candidates
   are filtered by the literal filters of the current resolution
   strategy."
  (loop for literal1 in (proof-node-clause clause-node)
	for index1 from 0
	for predicate-name = (get-predicate-name literal1)
	for hash-entry = (or (gethash predicate-name predicate-index)
			     (list nil nil))
	for clauses = (if (negative-literal literal1)
			   (first hash-entry)(second hash-entry))
	append (loop for literal-index in clauses
		     for cand = (list (list  clause-node index1)
				       literal-index)
		     when (apply-candidate-filters cand)
		     collect cand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; resolve-cand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun resolve-cand
  (cand)
  "This is the actual resolution procedure.  The candidate contains
   pointers to the two clauses and literals that where successfully
   unified.  The function collects  the union of literals of the two
   clauses except the two that were unified."
  (let ((new-clause nil))
    (loop for res in cand do
	  (loop for literal in (proof-node-clause (first res))
		for index from 0
		when (not (= index (second res)))
		do (pushnew (copy-tree literal) new-clause :test #'equalp)))
    (reverse  new-clause )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unify-cand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unify-cand
  (cand)
  "A candidate is a pair.  Each element is a pair.  The first element
   is a pointer to the proof-node of a clause and the second is an
   index of aliteral in that clause.  This function extracts the two
   literals and tries to unify them"
  (unify (without-not
	  (nth (second (first cand))(proof-node-clause (first (first cand)))))
	 (without-not
	  (nth (second (second cand))(proof-node-clause (first (second
							       cand)))))))


(defun without-not (exp)(if (eql (first exp) 'not)(second exp)exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eliminate-duplicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-duplicates 
  (clause)
  (loop for literal in clause
        for tail on clause 
        when (not (member literal (rest tail) :test #'equalp))
        collect literal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolution strategies
;;;   General explanations are at the header
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolution - a structure that determines the resolution strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct resolution
  (candidate-ordering-strategies '(trivial-ordering))
					;A list of functions used to impose a
					;lexicographic order over the set 
					;of candidate
  (candidate-filters '(trivial-candidate-filter))
					;A list of filter functions to filter out
					;resolution candidate
  (clause-filters '(trivial-clause-filter))
					;A list of filter functions to filter out
					;a new resolvent
  (initial-clause-filters '(trivial-clause-filter))
					;A list of filter functions to filter out
					;clauses in the initial set.
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolutions strategies.
;;;   A resolution strategy is created by filling in the fields of
;;;   the resolution record.  Below a few examples of such
;;;   combinations are given.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter  *default-resolution-strategy*
  (make-resolution
   :candidate-ordering-strategies '(shortest-min-clause shortest-sum)
   :candidate-filters '(setofsupport-filter)
   :clause-filters  '(tautology-filter uniqueness-filter)
   ))

(defparameter *setofsupport-strategy*
  (make-resolution
   :candidate-filters '(setofsupport-filter)
   :clause-filters  '(tautology-filter uniqueness-filter)
   ))

(defparameter *shortest-min-shortest-sum-strategy*
  (make-resolution
   :candidate-ordering-strategies '(shortest-min-clause shortest-sum)
   :clause-filters  '(tautology-filter uniqueness-filter)
   ))

(defparameter *shortest-min-clause-strategy*
  (make-resolution
   :candidate-ordering-strategies '(shortest-min-clause)
   :clause-filters  '(tautology-filter uniqueness-filter)
   ))

(defvar *current-resolution-strategy* *default-resolution-strategy*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  candidate-ordering
;;;   This function is used by the candidate sorting routines.  We use 
;;;   Common Lisp SORT and MERGE.  These functions require a predicate
;;;   that gets two items and return T if the first precedes the second.
;;;   The candidate ordering routine orders the candidates in lexicographic
;;;   order according to the list *candidate-ordering-strategies*.
;;;   The first strategy in the list is the most significant.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun candidate-ordering
  (cand1 cand2 &optional
	 (strategies (resolution-candidate-ordering-strategies
		      *current-resolution-strategy*)))
  (cond ((null strategies) t)
	(t (case (funcall (first strategies) cand1 cand2)
	      (< t)
	      (> nil)
	      (= (candidate-ordering cand1 cand2 (rest strategies)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Candidate ordering strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 
(defun shortest-min-clause (cand1 cand2)
  "A candidate ordering strategy.  Prefers pairs whose minimal clause 
  length is shorter"
  (let ((min1 (min (length (proof-node-clause (first (first cand1))))
	  (length (proof-node-clause (first (second cand1))))))
	(min2 (min (length (proof-node-clause (first (first cand2))))
	  (length (proof-node-clause (first (second cand2)))))))
    (numeric-relation min1 min2)))

(defun numeric-relation (n1 n2)
  (cond ((< n1 n2) '<)((> n1 n2) '>)(t '=)))

(defun shortest-sum (cand1 cand2)
  "A candidate ordering strategy.  Prefers pairs whose sum of clause 
  length is shorter"
  (let ((sum1 (+ (length (proof-node-clause (first (first cand1))))
	  (length (proof-node-clause (first (second cand1))))))
	(sum2 (+ (length (proof-node-clause (first (first cand2))))
	  (length (proof-node-clause (first (second cand2)))))))
    (numeric-relation sum1 sum2)))


(defun trivial-ordering (cand1 cand2)
  "A trivial ordering function.  It should be used when, during the 
  experimentation we need to test the prover with no ordering functions"
   (declare(ignore cand1)(ignore cand2)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Candidate filtering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-candidate-filters 
   (cand &optional
	 (filters (resolution-candidate-filters *current-resolution-strategy*)))
   (loop for filter in filters always (funcall filter cand)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Candidate filtering strategies
;;;    To add a strategy define the function according to the example 
;;;    below. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(defun setofsupport-filter (cand)
  "Set of support strategy.  One of the candidate clauses should be a 
  decendant of the set of support (the theorem negation)"
  (or (proof-node-setofsupport (first (first cand)))
      (proof-node-setofsupport (first (second cand)))))

(defun trivial-candidate-filter (cand) 
  "A trivial filter that always returns T.  Should be used when testing 
  the system without cadidate filtering"
   (declare (ignore cand)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Clause filtering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-clause-filters 
   (clause proof-nodes
   &optional (filters (resolution-clause-filters
		       *current-resolution-strategy*)))
   (loop for filter in filters always (funcall filter clause proof-nodes)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  clause filtering strategies
;;;    To add a strategy define the function according to the example 
;;;    below.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(defun uniqueness-filter (clause proof-nodes)
  "This filter makes sure that we don't add duplicate clause.   It look as 
  if it is always worthwhile to keep this filter.  Note however that the 
  cost of the test is high and proportional to the number of clauses"
  (not (member clause proof-nodes :key #'proof-node-clause
	       :test #'literal-set-equivalence)))

(defun literal-set-equivalence (set1 set2)
  (and (= (length set1)(length set2))
  (subsetp set1 set2 :test #'renaming?)))

(defun tautology-filter (clause &optional proof-nodes)
  (declare (ignore proof-nodes))
  (loop for literal in clause never
	(and (negative-literal literal)
	     (member (second literal) clause :test #'equalp))))
  
(defun trivial-clause-filter (clause nodes) 
  "A trivial filter that always returns T.  Should be used when testing 
  the system without cadidate filtering"
   (declare (ignore clause)(ignore nodes)) t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Initial clause filtering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-initial-clause-filters 
   (clause proof-nodes
   &optional (filters (resolution-initial-clause-filters
		       *current-resolution-strategy*)))
   (loop for filter in filters always (funcall filter clause proof-nodes)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  initial clause filtering strategies
;;;    To add a strategy define the function according to the example 
;;;    below.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compare-strategies  (strategies theorem &key (axioms
							  *axioms*))
  (loop for strategy in strategies
	for res = (list strategy
		      (let ((*current-resolution-strategy* strategy))
		         (multiple-value-list (prove theorem
						    :axioms
						    axioms))))
	collect (print res)))

(defun test1 ()
  (read-axioms "test1.lgc")
  (compare-strategies (list *default-resolution-strategy*
		            *shortest-min-shortest-sum-strategy*
			    *shortest-min-clause-strategy*
			    )
		      "mother(sara,yzhak)"))

			