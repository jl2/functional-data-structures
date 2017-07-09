;;;; fds.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:fds
  :description "Some data structures from the book '[Purely Functional Data Structures](https://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504)'"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC (BSD-like)"
  :serial t
  :depends-on (#:alexandria #:uiop)
  :components ((:file "package")
               (:file "fds")
               (:file "heap")
               (:file "leftist-heap")
               (:file "binomial-heap")))

