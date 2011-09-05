
(asdf:defsystem :versioned-objects-test
  :name "Versioned Arrays test suite"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "BSD"
  :components ((:file "versioned-objects-test"))
  :serial t
  :depends-on (:iterate :versioned-objects :stefil :bordeaux-threads :modf) )

