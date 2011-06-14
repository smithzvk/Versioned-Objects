
(defpackage :versioned-arrays
    (:use :cl :bt)
  (:export #:make-versioned-array
           #:varef ))

(in-package :versioned-arrays)

(modf-def:defstruct ver-array mods array)

(defun make-versioned-array (dimensions
                             &rest args
                             &key (element-type t)
                             initial-element initial-contents
                             adjustable fill-pointer
                             displaced-to displaced-index-offset )
  "Make a versioned array."
  (when (or adjustable fill-pointer displaced-to displaced-index-offset)
    (error "The capabilities: adjustable, fill-pointer, displaced-to, displaced-index-offset are not implemented yet") )
  (make-ver-array :mods nil :array (apply #'make-array dimensions args)) )

(defun varef (v-arr &rest idx)
  (let ((result
         (iter:iter (iter:for (new-val . jdx) in (ver-array-mods v-arr))
                    (iter:finding new-val such-that (equal idx jdx)) )))
    (if result
        result
        (apply #'aref (ver-array-array v-arr) idx) )))

(modf:define-modf-function varef 1 (new-val v-arr &rest idx)
  (modf:modf (ver-array-mods v-arr)
             (cons (cons new-val idx) (ver-array-mods v-arr)) ))
