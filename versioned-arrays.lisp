
(defpackage :versioned-arrays
    (:use :cl :bt :iter :modf)
  (:export #:make-versioned-array
           #:varef
           #:va-dimensions
           #:va-dimension))

(in-package :versioned-arrays)

(defstruct (versioned-array (:constructor %make-versioned-array))
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
                         :lock (bt:make-recursive-lock) ))

;; Basically this works like this.  A versioned array is a list whose last
;; element is an array.  When you access a value from the array, the array moves
;; to the first element of the list (making it a list of length 1) while at the
;; same time reversing the element of the list after it while reversing the
;; delta at the same time.

(defun varef (v-arr &rest idx)
  (bt:with-recursive-lock-held ((versioned-array-lock v-arr))
    (multiple-value-bind (array changes) (get-array v-arr)
      (invert-changes (versioned-array-cdr v-arr) changes v-arr)
      (setf (versioned-array-car v-arr) array
            (versioned-array-cdr v-arr) nil )
      (apply #'aref (versioned-array-car v-arr) idx) )))

(defun get-array (v-arr)
  (if (arrayp (versioned-array-car v-arr))
      (values (versioned-array-car v-arr) nil)
      (multiple-value-bind (array changes)
          (get-array (versioned-array-cdr v-arr))
        (destructuring-bind (new-val &rest idx) (versioned-array-car v-arr)
          (let ((old-val (apply #'aref array idx)))
            (setf (apply #'aref array idx) new-val)
            (values array
                    (cons (cons old-val idx) changes) ))))))

(defun invert-changes (old-v-arr changes last)
  (cond ((null changes) nil)
        (t (setf (versioned-array-cdr old-v-arr) last
                 (versioned-array-car old-v-arr) (car changes) )
         (invert-changes (versioned-array-cdr old-v-arr)
                         (cdr changes) old-v-arr ))))

(define-modf-function varef 1 (new-val v-arr &rest idx)
  (bt:with-recursive-lock-held ((versioned-array-lock v-arr))
    (let ((old-value
           ;; This moves the array to our version
           (apply #'varef v-arr idx) ))
      (let ((arr (versioned-array-car v-arr)))
        (setf (apply #'aref arr idx) new-val)
        (setf (versioned-array-cdr v-arr) (%make-versioned-array
                                           :car arr
                                           :lock (versioned-array-lock v-arr) )
              (versioned-array-car v-arr) (cons old-value idx) ))))
  (versioned-array-cdr v-arr) )

;; Some niceties...

(defun va-dimensions (v-arr)
  (bt:with-recursive-lock-held ((versioned-array-lock v-arr))
    (if (arrayp (versioned-array-car v-arr))
        (array-dimensions (versioned-array-car v-arr))
        (va-dimensions (versioned-array-cdr v-arr)) )))

(defun va-dimension (v-arr n)
  (nth n (va-dimensions v-arr)) )

(defmethod print-object ((obj versioned-array) str)
  (bt:with-recursive-lock-held ((versioned-array-lock obj))
    (varef obj 0 0)
    (print (versioned-array-car obj) str) ))
