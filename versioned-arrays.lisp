
(defpackage :versioned-arrays
  (:use :cl :bt :modf)
  (:export #:make-versioned-array
           #:varef
           #:va-dimensions
           #:va-dimension))

(in-package :versioned-arrays)

(defstruct (versioned-array (:constructor %make-versioned-array)
                            (:conc-name :va-) )
  car cdr lock )

(defun make-versioned-array (dimensions
                             &rest args
                             &key (element-type t)
                                  initial-element initial-contents
                                  adjustable fill-pointer
                                  displaced-to displaced-index-offset )
  "Make a versioned array."
  (declare (ignorable element-type initial-element initial-contents
                      adjustable fill-pointer displaced-to displaced-index-offset ))
  (when (or adjustable fill-pointer displaced-to displaced-index-offset)
    (error "The capabilities: adjustable, fill-pointer, displaced-to, displaced-index-offset are not implemented yet") )
  (%make-versioned-array :car (apply #'make-array dimensions args)
                         :lock (bt:make-lock) ))

;; Basically this works like this.  A versioned array is a list whose last
;; element is an array.  When you access a value from the array, the array moves
;; to the first element of the list (making it a list of length 1) while at the
;; same time reversing the element of the list after it while reversing the
;; delta at the same time.

(defun varef (v-arr &rest idx)
  (bt:with-lock-held ((va-lock v-arr))
    (raise-array! v-arr)
    (apply #'aref (va-car v-arr) idx) ))

(defun raise-array! (v-arr)
  "Bubble array to beginning of list, along the way back, reverse the list.
This assumes that locks are already held."
  (if (not (va-cdr v-arr))
      nil
      (progn
        (raise-array! (va-cdr v-arr))
        (destructuring-bind (new-val &rest idx)
            (va-car v-arr)
          ;; Move the array
          (setf (va-car v-arr)
                (va-car (va-cdr v-arr)) )
          ;; Invert delta
          (setf (va-car (va-cdr v-arr))
                (cons (apply #'aref (va-car v-arr) idx) idx) )
          ;; Mutate array
          (setf (apply #'aref (va-car v-arr) idx)
                new-val )
          ;; Reverse the list
          (setf (va-cdr (va-cdr v-arr))
                v-arr )
          ;; Terminate the list
          (setf (va-cdr v-arr) nil) ))))

(define-modf-function varef 1 (new-val v-arr &rest idx)
  (bt:with-lock-held ((va-lock v-arr))
    (raise-array! v-arr)
    (let* ((arr (va-car v-arr))
           (old-value (apply #'aref arr idx)) )
      (setf (apply #'aref arr idx) new-val)
      (setf (va-cdr v-arr) (%make-versioned-array
                            :car arr
                            :lock (va-lock v-arr) )
            (va-car v-arr) (cons old-value idx) ))
    (va-cdr v-arr) ))

;; Some niceties...

(defun va-dimensions (v-arr)
  (bt:with-lock-held ((va-lock v-arr))
    (raise-array! v-arr)
    (array-dimensions (va-car v-arr)) ))

(defun va-dimension (v-arr n)
  (nth n (va-dimensions v-arr)) )

(defmethod print-object ((obj versioned-array) str)
  (bt:with-lock-held ((va-lock obj))
    (raise-array! obj)
    (print (va-car obj) str) ))
