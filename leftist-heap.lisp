;;;; leftist-heap.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:fds)

(defclass leftist (binary-heap)
  ((rank :initform 1 :initarg :rank :type fixnum :reader rank))
  (:documentation "A [leftist heap](https://en.wikipedia.org/wiki/Leftist_heap) data structure."))

(defmethod initialize-instance :after ((heap leftist) &key key)
  "Initialize rank to 0 if the heap is empty (no key was specified)."
  (when (null key)
    (setf (slot-value heap 'rank) 0)))

(defun make-t (key val left-child right-child)
  "Create a new leftist heap with the given key value pair and the appropriate rank and children."
  (let ((pred (if left-child (slot-value left-child 'predicate)
                  (slot-value right-child 'predicate))))
    (if (> (rank left-child) (rank right-child))
        (make-instance 'leftist
                       :key key
                       :value val
                       :rank (+ 1 (rank right-child))
                       :left-child left-child
                       :right-child right-child
                       :predicate pred)
        (make-instance 'leftist
                       :key key
                       :value val
                       :rank (+ 1 (rank left-child))
                       :left-child right-child
                       :right-child left-child
                       :predicate pred))))

(defmethod merge-heaps ((h1 leftist) (h2 leftist))
  "Merge two leftist heaps."
  (cond
    ((empty-p h1) h2)
    ((empty-p h2) h1)
    (t 
     (with-slots (predicate (key1 key) (v1 value) (left1 left-child) (right1 right-child)) h1
       (with-slots ((y key) (v2 value) (left2 left-child) (right2 right-child)) h2
         (if (funcall predicate key1 y)
             (make-t key1 v1 left1 (merge-heaps right1 h2))
             (make-t y v2 left2 (merge-heaps h1 right2))))))))

(defmethod insert ((heap leftist) new-key &optional new-value)
  "Insert new-key new-value pair into the heap."
  (merge-heaps
   (make-instance 'leftist
                  :key new-key
                  :value new-value
                  :predicate (slot-value heap 'predicate))
   heap))

(defmethod find-min ((heap leftist))
  "Return the key value pair with the minimum key value."
  (when (empty-p heap)
    (error "find-min of an empty heap!"))
  (with-slots (key value) heap
    (values key value)))

(defmethod pop-min ((heap leftist))
  "Remove the key value pair with the minimum key value and return a new heap and the pair."
  (when (empty-p heap)
    (error "pop-min of an empty heap!"))
  (with-slots (key value (a left-child) (b right-child) predicate empty left-child right-child) heap
    (let ((rval (merge-heaps left-child b)))
      (values (if (null rval)
                  (make-instance 'leftist :predicate predicate)
                  rval)
              key value))))
