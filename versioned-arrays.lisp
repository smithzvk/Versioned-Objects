
(defpackage :versioned-arrays
    (:use :cl :bt :iter :modf)
  (:export #:make-versioned-array
           #:varef ))

(in-package :versioned-arrays)

(defun make-versioned-array (dimensions
                             &rest args
                             &key (element-type t)
                             initial-element initial-contents
                             adjustable fill-pointer
                             displaced-to displaced-index-offset )
  "Make a versioned array."
  (when (or adjustable fill-pointer displaced-to displaced-index-offset)
    (error "The capabilities: adjustable, fill-pointer, displaced-to, displaced-index-offset are not implemented yet") )
  (list (apply #'make-array dimensions args)) )

;; Basically this works like this.  A versioned array is a list whose last
;; element is an array.  When you access a value from the array, the array moves
;; to the first element of the list (making it a list of length 1) while at the
;; same time reversing the element of the list after it while reversing the
;; delta at the same time.

(defun varef (v-arr &rest idx)
  (if (arrayp (car v-arr))
      (apply #'aref (car v-arr) idx)
      (if (equal (rest (first v-arr)) idx)
          (first (first v-arr))
          (varef (rest v-arr)) )))

(define-modf-function varef 1 (new-val v-arr &rest idx)
  (cons (cons new-val idx) v-arr) )
