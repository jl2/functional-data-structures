;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(defpackage #:fds
  (:use #:cl #:alexandria)
  (:export

   ;; Heap types
   #:binomial #:leftist #:fibonacci

   ;; Queries
   #:key #:value #:find-min #:predicate #:rank #:empty-p

   ;; Modifications
   #:insert #:pop-min

   ;; Helpful utilities
   #:merge-heaps #:to-list
   
   #:run-tests))

