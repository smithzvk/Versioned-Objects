
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
  (apply #'make-array dimensions args) )

(defun varef (v-arr &rest idx)
  (if (consp v-arr)
      (if (equal (rest (first v-arr)) idx)
          (first (first v-arr))
          (varef (rest v-arr)) )
      (apply #'aref v-arr idx) ))

(define-modf-function varef 1 (new-val v-arr &rest idx)
  (cons (cons new-val idx) v-arr) )
