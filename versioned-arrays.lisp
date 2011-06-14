
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
  (multiple-value-bind (array changes) (get-array v-arr)
    (invert-changes (cdr v-arr) changes v-arr)
    (setf (car v-arr) array
          (cdr v-arr) nil )
    (apply #'aref (car v-arr) idx) ))

(defun get-array (v-arr)
  (if (arrayp (car v-arr))
      (values (car v-arr) nil)
      (multiple-value-bind (array changes)
          (get-array (cdr v-arr))
        (destructuring-bind (new-val &rest idx) (car v-arr)
          (let ((old-val (apply #'aref array idx)))
            (setf (apply #'aref array idx) new-val)
            (values array
                    (cons (cons old-val idx) changes) ))))))

(defun invert-changes (old-v-arr changes last)
  (cond ((null changes) nil)
        (t (setf (cdr old-v-arr) last
                 (car old-v-arr) (car changes) )
         (invert-changes (cdr old-v-arr) (cdr changes) old-v-arr) )))

(define-modf-function varef 1 (new-val v-arr &rest idx)
  (let ((old-value
         ;; This moves the array to our version
         (apply #'varef v-arr idx) ))
    (let ((arr (car v-arr)))
      (setf (apply #'aref (car v-arr) idx) new-val)
      (setf (cdr v-arr) (list arr)
            (car v-arr) (cons old-value idx) )))
  (cdr v-arr) )
