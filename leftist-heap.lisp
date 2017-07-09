;;;; leftist-heap.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:fds)
(defclass leftist-node ()
  ((key :initarg :key :initform nil :accessor key)
   (value :initform nil :initarg :value :accessor value)
   (rank :initform 1 :initarg :rank :type fixnum :reader rank)
   (left-child :initform nil :initarg :left-child :type (or 'leftist-node null) :accessor :left-child)
   (right-child :initform nil :initarg :right-child :type (or 'leftist-node null) :accessor :right-child))
  (:documentation "A node in a leftist heap."))


(defmethod rank ((heap (eql nil)))
  0)

(defclass leftist (heap)
  ((nodes :initform nil :initarg :nodes :type (or leftist-node nil)))
  (:documentation "A [leftist heap](https://en.wikipedia.org/wiki/Leftist_heap) data structure."))

(defmethod empty-p ((heap leftist))
  "Test if the heap is empty."
  (null (slot-value heap 'nodes)))

(defun make-t (key val left-child right-child)
  "Create a new leftist heap with the given key value pair and the appropriate rank and children."
  (if (> (rank left-child) (rank right-child))
      (make-instance 'leftist-node
                     :key key
                     :value val
                     :rank (+ 1 (rank right-child))
                     :left-child left-child
                     :right-child right-child)
      (make-instance 'leftist-node
                     :key key
                     :value val
                     :rank (+ 1 (rank left-child))
                     :left-child right-child
                     :right-child left-child)))

(defun merge-nodes (predicate tree1 tree2)
  "Merge tree1 and tree2, using predicate for comparison."
  (cond ((null tree1) tree2)
        ((null tree2) tree1)
        (t 
         (with-slots ((key1 key) (v1 value) (left1 left-child) (right1 right-child)) tree1
           (with-slots ((key2 key) (v2 value) (left2 left-child) (right2 right-child)) tree2
             (if (funcall predicate key1 key2)
                 (make-t key1 v1 left1 (merge-nodes predicate right1 tree2))
                 (make-t key2 v2 left2 (merge-nodes predicate tree1 right2))))))))

(defmethod merge-heaps ((h1 leftist) (h2 leftist))
  "Merge two leftist heaps."
  (cond
    ((empty-p h1) h2)
    ((empty-p h2) h1)
    (t 
     (with-slots (predicate (tree1 nodes)) h1
       (with-slots ((tree2 nodes)) h2
         (make-instance 'leftist :nodes (merge-nodes predicate tree1 tree2)))))))

(defmethod insert ((heap leftist) new-key &optional new-value)
  "Insert new-key new-value pair into the heap."
  (make-instance 'leftist
                 :nodes (merge-nodes (slot-value heap 'predicate)
                                     (make-instance 'leftist-node :key new-key :value new-value)
                                     (slot-value heap 'nodes))))

(defmethod find-min ((heap leftist))
  "Return the key value pair with the minimum key value."
  (when (empty-p heap)
    (error "find-min of an empty heap!"))
  (with-slots (nodes) heap
    (with-slots (key value) nodes
        (values key value))))

(defmethod pop-min ((heap leftist))
  "Remove the key value pair with the minimum key value and return a new heap and the pair."
  (when (empty-p heap)
    (error "pop-min of an empty heap!"))
  (with-slots (predicate nodes) heap
    (with-slots (key value left-child right-child) nodes
      (let ((rval (make-instance 'leftist
                                 :predicate predicate
                                 :nodes (merge-nodes predicate left-child right-child))))
        (values rval key value)))))

(defmethod to-list ((nodes leftist-node))
  "Return a list representation of the leftist-node structure."
  (with-slots (key value left-child right-child) nodes
    (concatenate 'list
                 (list (if value (cons key value) key))
                 (to-list left-child)
                 (to-list right-child))))

(defmethod to-list ((heap leftist))
  "Return a list representation of the heap."
  (to-list (slot-value heap 'nodes)))


(defmethod show-heap ((nodes leftist-node) stream indentation)
  "Write a text representation of the leftist-node structure to stream."
  (if (null nodes)
      (format stream "nil~%")
      (let ((space-string (spaces indentation)))
        (with-slots (key value left-child right-child) nodes
          (format stream "~a ~@[(~a) ~]~%" key value)
          (format stream "~a+-" space-string)
          (show-heap left-child stream (+ 2 indentation))
          (format stream "~a+-" space-string)
          (show-heap right-child stream (+ 2 indentation)))))
  nodes)

(defmethod show-heap ((heap leftist) stream indentation)
  "Write a text representation of the leftist heap to stream."
  (show-heap (slot-value heap 'nodes) stream indentation)
  heap)

(defmethod to-dot ((nodes leftist-node) stream)
  "Write a dot digraph of the node structure to stream."
  (labels ((inner-to-dot (nodes stream count)
             (with-slots (key value left-child right-child) nodes
               (let ((o-count count)
                     (n-count count))
                 (format stream "~a [label=\"~a~@[ (~a) ~]\",shape=box];~%" count key value)
                 (when left-child 
                   (format stream "~a->~a;~%" count (+ 1 count))
                   (setf n-count (inner-to-dot left-child stream (+ 1 count))))
                 (when right-child
                   (format stream "~a->~a;~%" count (+ 1 n-count))
                   (setf n-count (inner-to-dot right-child stream (+ 1 n-count))))
                 n-count))))
    (inner-to-dot nodes stream 0)))


(defmethod to-dot ((heap leftist) stream)
  "Write a dot digraph of the heap to stream."
  (format stream "digraph G {~%")

  (to-dot (slot-value heap 'nodes) stream)

  (format stream "}~%")
  heap)
