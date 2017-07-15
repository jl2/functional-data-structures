;;;; binomial-heap.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:fds)

(defclass binomial-node ()
  ((key :initarg :key :initform nil :accessor key)
   (value :initform nil :initarg :value :accessor value)
   (rank :initform 1 :initarg :rank :type fixnum :reader rank)
   (children :initform nil :initarg :children :type cons :accessor children))
  (:documentation "A node in a binomial heap."))

(defun link (predicate t1 t2)
  (with-slots ((key1 key) (value1 value) (rank1 rank) (children1 children)) t1
    (with-slots ((key2 key) (value2 value) (children2 children)) t2
      (if (funcall predicate key1 key2)
          (make-instance 'binomial-node
                         :rank (1+ rank1)
                         :key key1
                         :value value1
                         :children (cons t2 children1))
          (make-instance 'binomial-node
                         :rank (1+ rank1)
                         :key key2
                         :value value2
                         :children (cons t1 children2))))))

(defun ins-tree (predicate node tree)
  (if (null tree)
      (cons node tree)
      (let ((tp (car tree))
            (tsp (cdr tree)))
        (if (< (slot-value node 'rank) (slot-value tp 'rank))
            (cons node tree)
            (ins-tree predicate (link predicate node tp) tsp)))))


(defclass binomial (heap)
  ((trees :initform nil :initarg :trees :type cons :reader trees))
  (:documentation "A [binomail heap](https://en.wikipedia.org/wiki/Binomial_heap) data structure."))


(defmethod insert ((heap binomial) new-key &optional new-value)
  (make-instance 'binomial :trees (ins-tree (slot-value heap 'predicate)
                                            (make-instance 'binomial-node
                                                           :key new-key
                                                           :value new-value)
                                            (slot-value heap 'trees))))

(defmethod empty-p ((heap binomial))
  "Test if the heap is empty."
  (null (slot-value heap 'trees)))

(defun merge-nodes (predicate tree1 tree2)
  "Merge tree1 and tree2, using predicate for comparison."
  (cond ((null tree1) tree2)
        ((null tree2) tree1)
        (t
         (let ((t1 (car tree1))
               (t2 (car tree2))
               (ts1 (cdr tree1))
               (ts2 (cdr tree2)))
           (if (< (slot-value t1 'rank) (slot-value t2 'rank))
               (cons t1 (merge-nodes predicate ts2 tree2))
               (if (< (slot-value t2 'rank) (slot-value t1 'rank))
                   (cons t2 (merge-nodes predicate tree1 ts2))
                   (ins-tree predicate (link predicate tree1 tree2) (merge-nodes predicate  ts1 ts2))))))))

(defmethod merge-heaps ((h1 binomial) (h2 binomial))
  "Merge two leftist heaps."
  (cond
    ((empty-p h1) h2)
    ((empty-p h2) h1)
    (t
     (make-instance 'binomial
                    :trees (merge-nodes (slot-value h2 'predicate) (slot-value h1 'trees) (slot-value h2 'trees))
                    :predicate (slot-value h1 'predicate)))))

(defmethod show-heap ((nodes binomial-node) stream indentation)
  "Write a text representation of the leftist-node structure to stream."
  (if (null nodes)
      (format stream "nil~%")
      (let ((space-string (spaces indentation)))
        (with-slots (key value children) nodes
          (format stream "~a ~@[(~a) ~]~%" key value)
          (dolist (child children)
            (format stream "~a+-" space-string)
            (show-heap child stream (+ 2 indentation))))))
  nodes)

(defmethod show-heap ((heap binomial) stream indentation)
  "Write a text representation of the leftist heap to stream."
  (declare (ignorable indentation stream))
  (format stream "trees: ~a~%" (slot-value heap 'trees))
  (dolist (tree (slot-value heap 'trees)) (show-heap tree stream indentation))
  heap)

(defmethod to-dot ((nodes binomial-node) stream)
  "Write a dot digraph of the node structure to stream."
  (labels ((inner-to-dot (nodes stream count)
             (with-slots (key value children) nodes
               (let ((o-count count)
                     (n-count count))
                 (format stream "~a [label=\"~a~@[ (~a) ~]\",shape=box];~%" o-count key value)
                 (when children
                   (dolist (child children)
                     (format stream "~a->~a;~%" count (+ 1 n-count))
                     (setf n-count (inner-to-dot left-child stream (+ 1 count))))
                 n-count)))))
    (inner-to-dot nodes stream 0)))


(defmethod to-dot ((heap binomial) stream)
  "Write a dot digraph of the heap to stream."
  (format stream "digraph G {~%")

  (to-dot (slot-value heap 'trees) stream)

  (format stream "}~%")
  heap)
