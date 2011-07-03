
(asdf:defsystem :versioned-arrays
  :name "Versioned Arrays"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "BSD"
  :description
  "This library implements thin versioned arrays.  These arrays allow for
somewhat cheap functional arrays.  Instead of copying an array, we mutate the
array, move it, and store a diff, or delta, and a pointer to the mutated array
in the location where the array used to be.  This allows you to make large
versioned trees of these arrays that only require storage O\(N+m) where N is the
number of elements and m is the number of deltas \(the naive alternative is
O\(N*m)).  Access time is O\(m), where m is the number of changes between you
and the actual array, so this is particularly well suited for cases where the
interesting versions of an array are only a few deltas apart.

After any access or modification, the array is rebased, or moved to the verion
you are at, meaning that changes or accesses from this version are now O\(1)."
  :components ((:file "versioned-arrays"))
  :serial t
  :depends-on (:bordeaux-threads :modf) )
