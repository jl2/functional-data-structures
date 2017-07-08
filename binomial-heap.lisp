;;;; binomial-heap.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:fds)

(defclass tree
(defclass binomial (heap)
  ((trees :initform nil :type cons :reader trees))
  (:documentation "A [binomail heap](https://en.wikipedia.org/wiki/Binomial_heap) data structure."))

(defmethod initialize-instance :after ((heap binomial) &key key)
  "Initialize rank to 0 if the heap is empty (no key was specified)."
  (when (null key)
    (setf (slot-value heap 'rank) 0)))


