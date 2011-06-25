
(asdf:defsystem :versioned-arrays-test
  :name "Versioned Arrays test suite"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "BSD"
  :components ((:file "test"))
  :serial t
  :depends-on (:iterate :versioned-arrays :stefil :bordeaux-threads :modf) )
