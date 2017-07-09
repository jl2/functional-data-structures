;;;; heap.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:fds)

(defclass heap ()
  ((predicate :initform #'< :initarg :predicate :reader predicate))
  (:documentation "A generic heap base class."))

(defgeneric empty-p (heap)
  (:documentation "Test if the heap is empty."))

(defgeneric find-min (heap)
  (:documentation "Find the key value pair in the heap with the minimum key value.")
  (:method ((heap (eql nil)))
    (error "find-min of an empty heap!")))

(defgeneric insert (heap new-key &optional new-value)
  (:documentation "Insert the key value pair into the heap and return a new heap."))

(defgeneric pop-min (heap)
  (:documentation "Remove the key value pair with the minimum key value and return a new heap and the pair.")
  (:method ((heap (eql nil)))
    (error "pop-min on an empty heap!")))

(defgeneric merge-heaps (heap1 heap2)
  (:documentation "Merge two heaps.")
  (:method ((a (eql nil)) (b (eql nil)))
    nil)
  (:method ((a (eql nil)) (b heap))
    b)
  (:method ((a heap) (b (eql nil)))
    a))

(defgeneric to-list (heap)
  (:documentation "Return a list containing the key/value pairs in the heap, with the minimum key first.")
  (:method ((heap (eql nil)))
    nil)
  )

(defun spaces (n)
  (format nil (format nil "~~~aa" (if n n 0)) ""))

(defgeneric show-heap (heap stream indentation)
  (:documentation "Pretty print a heap to stream.")

  (:method ((heap (eql nil)) stream indentation)
    (declare (ignorable indentation))
    (format stream "nil~%")
    heap))

(defgeneric to-dot (heap stream)
  (:documentation "Output dot digraph.")
  (:method ((heap (eql nil)) stream)
    (format stream "nil;~%")
    heap))
