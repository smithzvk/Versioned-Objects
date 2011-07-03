
(defpackage :versioned-objects
  (:use :cl :bt :modf)
  (:export #:make-versioned-object
           #:varef
           #:va-dimensions
           #:va-dimension))

(in-package :versioned-objects)

(defstruct (versioned-object (:constructor %make-versioned-object)
                             (:conc-name :vo-) )
  car cdr lock )

(defun make-versioned-object (dimensions
                              &rest args
                              &key (element-type t)
                                   initial-element initial-contents
                                   adjustable fill-pointer
                                   displaced-to displaced-index-offset )
  "Make a versioned object."
  (declare (ignorable element-type initial-element initial-contents
                      adjustable fill-pointer displaced-to displaced-index-offset ))
  (when (or adjustable fill-pointer displaced-to displaced-index-offset)
    (error "The capabilities: adjustable, fill-pointer, displaced-to, displaced-index-offset are not implemented yet") )
  (%make-versioned-object :car (apply #'make-object dimensions args)
                          :lock (bt:make-lock) ))

;; Basically this works like this.  A versioned object is a list whose last
;; element is the object.  When you access a value from the object, the object
;; moves to the first element of the list (making it a list of length 1) while
;; at the same time reversing the list after it and inverting the deltas.

(defun varef (v-arr &rest idx)
  (bt:with-lock-held ((vo-lock v-arr))
    (raise-object! v-arr)
    (apply #'aref (vo-car v-arr) idx) ))

(defun raise-object! (v-arr)
  "Bubble object to beginning of list, along the way back, reverse the list.
This assumes that locks are already held."
  (if (not (vo-cdr v-arr))
      nil
      (progn
        (raise-object! (vo-cdr v-arr))
        (destructuring-bind (new-val &rest idx)
            (vo-car v-arr)
          ;; Move the object
          (setf (vo-car v-arr)
                (vo-car (vo-cdr v-arr)) )
          ;; Invert delta
          (setf (vo-car (vo-cdr v-arr))
                (cons (apply #'aref (vo-car v-arr) idx) idx) )
          ;; Mutate object
          (setf (apply #'aref (vo-car v-arr) idx)
                new-val )
          ;; Reverse the list
          (setf (vo-cdr (vo-cdr v-arr))
                v-arr )
          ;; Terminate the list
          (setf (vo-cdr v-arr) nil) ))))

(define-modf-function varef 1 (new-val v-arr &rest idx)
  (bt:with-lock-held ((vo-lock v-arr))
    (raise-object! v-arr)
    (let* ((arr (vo-car v-arr))
           (old-value (apply #'aref arr idx)) )
      (setf (apply #'aref arr idx) new-val)
      (setf (vo-cdr v-arr) (%make-versioned-object
                            :car arr
                            :lock (vo-lock v-arr) )
            (vo-car v-arr) (cons old-value idx) ))
    (vo-cdr v-arr) ))

;; Some niceties...

(defun vo-dimensions (v-arr)
  (bt:with-lock-held ((vo-lock v-arr))
    (raise-object! v-arr)
    (object-dimensions (vo-car v-arr)) ))

(defun vo-dimension (v-arr n)
  (nth n (vo-dimensions v-arr)) )

(defmethod print-object ((obj versioned-object) str)
  (bt:with-lock-held ((vo-lock obj))
    (raise-object! obj)
    (print (vo-car obj) str) ))
