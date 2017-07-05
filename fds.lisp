;;;; fds.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:fds)


(defmacro expect-pred (message pred &body body)
  (alexandria:with-gensyms (aval mess passed)
    `(let ((,mess ,message))
       (format t "~a...~%    ~a~%" ,mess
               (handler-case
                   (let* ((,aval (progn ,@body))
                          (,passed (funcall ,pred ,aval)))
                     (if ,passed
                         (format nil "PASSED")
                         (format nil "FAILED (~a)" ,aval)))
                 (error (condition)
                   (format nil "FAILED with condition (~a)~%" ,mess condition)))))))

(defmacro expect (message &body body)
  `(expect-pred ,message #'identity ,@body))

(defmacro expect-not (message &body body)
  `(expect-pred ,message (curry #'not) ,@body))

(defmacro expect-not-pred (message pred &body body)
  `(expect-pred ,message (compose #'not ,pred) ,@body))


(defmacro expect-error (message &body body)
  (alexandria:with-gensyms (mess)
    `(let* ((,mess ,message))
       (format t "~a~%" (concatenate 'string ,mess "..."))
       (if (handler-case 
               (prog1 nil
                 ,@body)
             (error (condition)
               (declare (ignorable condition))
               (format t "    PASSED (caught ~a)~%" condition)
               t))
           t
           (progn
             (format t "    FAILED: Expected an error but did not catch one~%")
             nil)))))

(defun run-tests ()
  (format t "Running tests for leftist-heap:~%")
  (test-leftist-heap))


(defun test-leftist-heap ()
  (let ((the-heap (make-instance 'leftist)))

    ;; Test some things about an empty heap
    (expect-pred "empty-p of empty heap" #'empty-p the-heap)
    (expect-pred "rank of empty heap is 0" #'zerop (rank the-heap))
    (expect-error "pop-min on empty heap signals error" (pop-min the-heap))
    (expect-error "find-min on empty heap signals error" (find-min the-heap))

    ;; Insert a single item into the heap
    (let ((one-heap (insert the-heap 42)))
      (expect-pred "find-min of heap with one entry is that entry" (curry #'= 42) (find-min one-heap))

      (multiple-value-bind (new-heap key val) (pop-min one-heap)
        (expect-pred "pop-min of one-entry heap returns empty" #'empty-p new-heap)
        (expect-pred "pop-min of single key that key" (curry #'= 42) key)
        (expect-not "pop-min nil value is nil" val)))

    ;; Insert a single item with a value
    (let ((one-heap (insert the-heap 42 5)))
      (expect-pred "find-min of heap with one entry is that entry" (curry #'= 42) (find-min one-heap))

      (multiple-value-bind (new-heap key val) (pop-min one-heap)
        (expect-pred "pop-min of one-entry heap returns empty" #'empty-p new-heap)
        (expect-pred "pop-min of single key that key" (curry #'= 42) key)
        (expect-pred "pop-min of single value is that" (curry #'= 5) val)))
    
    ;; Insert integers 0-10
    (dotimes (i 10)
      (setf the-heap (insert the-heap i)))

    (expect-pred "to-list of heap with 10 items has 10 items" (curry #'= 10) (length (to-list the-heap)))
    
    (expect-pred "find-min of heap is 0" (curry #'= 0) (find-min the-heap))

    ;; pop-min 10 times
    (dotimes (i 10)
      (multiple-value-bind (next-heap key) (pop-min the-heap)
        (setf the-heap next-heap)
        (expect-pred "values in heap are sorted" (curry #'= i) key)))
    (expect-error "heap is empty after ten calls to pop-min" (pop-min the-heap))

    ))
