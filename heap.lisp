;;;; heap.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:fds)

(defclass heap ()
  ((empty :initform t :accessor empty-p)
   (key :initarg :key :initform nil :accessor key)
   (predicate :initform #'< :initarg :predicate :reader predicate)
   (value :initform nil :initarg :value :accessor value)
   (left-child :initform nil :initarg :left-child :type (or 'heap nil) :accessor :left-child)
   (right-child :initform nil :initarg :right-child :type (or 'heap nil) :accessor :right-child))
  (:documentation "A generic heap base class."))

(defmethod initialize-instance :after ((heap heap) &key key)
  "Initialize the :empty slot depending on whether a :key parameter was given or not."
  (when key
    (setf (slot-value heap 'empty) nil)))
           
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

(defgeneric rank (heap)
  (:documentation "Return the length of the right 'spine' of the heap.")
  (:method ((heap (eql nil)))
    0))  

(defgeneric to-list (heap)
  (:documentation "Return a list containing the key/value pairs in the heap, with the minimum key first.")
  (:method ((heap (eql nil)))
    nil)
  (:method ((heap heap))
    (with-slots (key value left-child right-child) heap
        (concatenate 'list
                     (list (if value (cons key value) key))
                     (to-list left-child)
                     (to-list right-child)))))

(defun spaces (n)
  (format nil (format nil "~~~aa" (if n n 0)) ""))

(defgeneric show-heap (heap &optional stream indentation)
  (:documentation "Pretty print a heap to stream.")

  (:method ((heap (eql nil)) &optional stream indentation)
    (declare (ignorable indentation))
    (format stream "nil~%")
    heap)

  (:method ((heap heap) &optional stream indentation)
    (if (or (null heap) (empty-p heap))
        (format stream "nil~%")
        (let ((space-string (spaces indentation)))
          (format stream "~a ~@[(~a) ~]~%" (slot-value heap 'key) (slot-value heap 'value))
          (format stream "~a+-" space-string)
          (show-heap (slot-value heap 'left-child) stream (+ 2 indentation))
          (format stream "~a+-" space-string)
          (show-heap (slot-value heap 'right-child) stream (+ 2 indentation))))
    heap))
