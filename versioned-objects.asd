
(asdf:defsystem :versioned-objects
  :name "Versioned Objects"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "BSD"
  :description
  "This library implements 'thin' versioned objects.  Basically these are 'thin'
versioned arrays generalized to any Common Lisp object.  This should work
functionally modifying any slot of any object that has a setf expansion."
  :components ((:file "versioned-objects")
               (:file "benchmarking") )
  :serial t
  :depends-on (:bordeaux-threads :modf) )
