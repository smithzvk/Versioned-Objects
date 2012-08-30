
(in-package :versioned-objects)

;; @\section{Benchmarking}

;; @Usually I would aschew focusing on benchmarks, but because of the subtle
;; nature of what is happening in this library, and the many ways that it could
;; be implemented, it seems necessary to understand in realistic terms how long
;; certain operations take.

;; @\subsection{Rebasing}

;; @The original functionality of the library basically rebased the version tree
;; at each access such that the version you are accessing is the root.  Here we
;; measure how long that takes as a function of $m$, the number of edits between
;; two versions.  We do this for arrays, hash tables, class instances, and
;; structure instances, four data structures where this library will likely find
;; the most use.

(defmacro time-operation ((&key (repeat 10000) (min-sample-time 5))
                          &body body)
  `(let* ((start-time (get-internal-real-time))
          (end-time start-time)
          (times (iter
                   (for times from 0)
                   (until (> (setf end-time (get-internal-real-time))
                             (+ start-time
                                (* ,min-sample-time
                                   internal-time-units-per-second))))
                   (iter (for i below ,repeat)
                     ,@body)
                   (finally (return times)))))
     (float
      (/ (* ,repeat times) (/ (- end-time start-time)
                              internal-time-units-per-second)) 0d0)))

(defun measure-rebase-time (vo n-edits)
  "Measure how long it takes to rebase the array."
  (let* ((arr1 vo)
         (arr2 (iter (for i below n-edits)
                 (for new-arr
                   initially (vmodf (aref arr1 (random 10)) (random 10))
                   then (vmodf (aref new-arr (random 10)) (random 10)))
                 (finally (return new-arr)))))
    (let ((rebases-per-second (time-operation ()
                                (raise-object! arr1)
                                (raise-object! arr2))))
      (values rebases-per-second
              ;; Deltas per second
              (* rebases-per-second n-edits)))))

;; @Preliminary results suggest that using a more speciallized approach to
;; versioning can push rebase speed up by ~40%.

(defun measure-version-time (vo n-versions-before-rebase)
  "Measure how much time it takes to create new versions of the data if building
off of the most recent version."
  (let ((versions-per-second
          (time-operation ()
            (iter (for i below n-versions-before-rebase)
              (for new-arr
                   initially (vmodf (aref vo (random 10)) (random 10))
                   then (vmodf (aref new-arr (random 10)) (random 10)))
              (finally (return new-arr))))))
    (values versions-per-second)))

(defun measure-in-place-walk-time (n-edits)
  "Measure how long it takes to read a value from the datastructure without
rebasing it."
  )

(defun measure-in-place-version-time (n-edits)
  "Measure how long it takes to add a new version to the data structure without
rebasing it.  This implicitly includes a read."
  )
