(defpackage :model
  (:use :common-lisp :utils)
  (:export
    :read-model :extract-args :extract-reqs :extract-vars :extract-body
    :decl-var :decl-typ :type-class :elem-type :type-dims
    :rel-class :rel-var :rel-val :rel-distr :rel-block-body
    :rel-if-condition :rel-true-branch :rel-false-branch
    :rel-loop-var :rel-loop-bounds :rel-loop-body :bounds-lo :bounds-hi))
(in-package :model)

; TODO: fuller check of model structure?

(defun read-model (ifname)
  (let ((mdl (read-file-upcasing-only *model-keywords* ifname)))
    (check-model-has-sections '(:args :reqs :vars :body) mdl)
    ;(check-elements-are-decls (extract-args mdl))
    ;(check-elements-are-exprs (extract-reqs mdl))
    ;(check-elements-are-decls (extract-vars mdl))
    ;(check-elements-are-rels (extract-rels mdl))
    mdl))

(defun check-model-has-sections (sections mdl)
  (unless (and (listp mdl) (eq :model (first mdl)))
    (error "Model should start with ':model'"))
  (let ((rest (cdr mdl)))
    (unless (and (listp rest) (= 4 (length rest)))
      (error "Model should have 4 sections"))
    (dotimes (i (length sections))
      (check-has-header (1+ i) (nth i sections) (nth i rest)))))

(defun check-has-header (n hdr x)
  (unless (and (listp x) (not (null x)) (eq (car x) hdr))
    (error (format nil "Section ~a of model should have header '~a'." n hdr))))

(defparameter *model-keywords*
  '(:model :args :reqs :vars :body :for :if :block
    :range :all
    :realp :realnn :real :realx :realxn :integerp :integernn :integer :boolean))

(defun extract-args (mdl) (cdr (second mdl)))
(defun extract-reqs (mdl) (cdr (third mdl)))
(defun extract-vars (mdl) (cdr (fourth mdl)))
(defun extract-body (mdl) (cdr (fifth mdl)))

(defun decl-var (decl) (first decl))
(defun decl-typ (decl) (second decl))

(defun type-class (typ)
  (cond ((symbolp typ) :scalar)
	((listp typ) :array)
	(t (error (format nil "Invalid type: ~a" typ)))))

(defun elem-type (typ) (car typ))

(defun type-dims (typ) (if (symbolp typ) '() (cdr typ)))

(defun rel-class (rel)
  (case (first rel)
	(:<- :deterministic)
	(:~ :stochastic)
	(:block :block)
	(:if (case (length (rest rel))
		   (2 :if-then)
		   (3 :if-then-else)))
	(:for :loop)))

(defun rel-var (rel) (second rel))
(defun rel-val (rel) (third rel))
(defun rel-distr (rel) (third rel))
(defun rel-block-body (rel) (rest rel))
(defun rel-if-condition (rel) (second rel))
(defun rel-true-branch (rel) (third rel))
(defun rel-false-branch (rel) (fourth rel))
(defun rel-loop-var (rel) (second rel))
(defun rel-loop-bounds (rel) (third rel))
(defun rel-loop-body (rel) (fourth rel))
(defun bounds-lo (bnds) (first bnds))
(defun bounds-hi (bnds) (second bnds))

; START expr functions
