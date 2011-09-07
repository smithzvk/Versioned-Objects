
(defpackage :versioned-objects
  (:use :cl :bt :modf :iterate)
  (:import-from modf find-container)
  (:export #:version
           #:versioned-object
           #:vfuncall #:vapply
           #:with-versioned-object #:with-versioned-objects
           #:vmodf ))

(in-package :versioned-objects)

;; (defmacro cond-lock-held (&rest lock-clauses)
;;   (if (car lock-clauses)
;;       `(if (bt:acquire-lock ,(caar lock-clauses) nil)
;;            (unwind-protect
;;                 (progn ,@(cdar lock-clauses))
;;              (bt:release-lock ,(caar lock-clauses)) )
;;            (cond-lock-held ,(cdr lock-clauses)) )))

;; (cond-lock-held (*modifier-lock* t)
;;                 (*read-only-lock* t) )

;; @\section{introduction}

;; @Versioned-objects is a library that brings general verioning to Common Lisp.
;; This allows you to use arbitrary data structures in a functional way without
;; facing the cost of copying the data structure on modification.  Versioning of
;; data structures is a method that has very attractive performance (usually
;; constant time) in many use cases such as in backtracking search.

;; @This can be viewed as a sister project of Modf, another way to achieve
;; functional data manipulation in Common Lisp.  In many ways it achieves the
;; same goals as Modf but via drastically different mechanisms.

;; @1. Modf works well when the data structure you are using supports functional
;; manipulation while Versioned-objects only works for data with defined {\em
;; setf} expansions but performance determined by how quickly the data can be
;; accessed and mutated, not functionally modified.

;; @2. Modf allows you to work with the native datatypes in Common Lisp meaning
;; your data will work in any function without further consideration while
;; Versioned-objects relies of wrapping the data, and thus special care must be
;; taken when calling functions with your versioned data as arguments.

;; @3. Modf returns objects that may share memory, but are completely independent
;; otherwise, thereby making them perfect for multithreaded programs.
;; Versioned-objects creates a network of interconnected data which, other than
;; using a few special methods, cannot be used in any asynchronous fashion at
;; all (the structure needs to be locked access by only one thread at a time,
;; even for reads).

;; @4. Modf must resort to copying on any object without persistent
;; characteristics.  Versioned-objects only relies on copying when it is faster
;; to do so than use the versioning mechanism.

;; @In general, these two libraries overlap in functionality, but are useful at
;; two different limits of the data spectrum.  Modf is useful when objects are
;; small (and thus cheap to copy) or have persistent characteristics (like
;; singly linked lists or binary trees) or are involved in parallel execution.
;; Versioned-objects is useful when objects are large (and thus expensive to
;; copy) and have no persistent characteristics and are not frequently accessed
;; by multiple threads.

;; @\subsection{Simple Usage}

;; @Using Versioned-objects is simple.  You take any object and call the function
;; <<version>> on it.  This will transform the object into a versioned-object.
;; In general, the object passed to <<version>> should never be accessed or
;; mutated after calling <<version>>.  At this point you may make cheap copies
;; that require no more space than the different between the original and the
;; new copy.  There are a few options you can pass to <<version>> that will
;; determine how the version tree is maintained, but we'll leave that for later.

;; @To access the data in your versioned-object, you must use the functions
;; <<vfuncall>> and <<vapply>> which work precisely as their {\em sans v}
;; counterparts, but work with versioned data in their argument lists.  Later on
;; we will see how to define your own accessor functions which may be faster and
;; possibly more aesthetically pleasing, but for now we leave it at this.

;; @\section{Implementation}

;; @The structure <<versioned-object>> is a cons-cell like structure that makes
;; up the version tree for your object.

;;<<versioned-object>>=
(defstruct (versioned-object (:constructor %make-versioned-object)
                             (:conc-name :vo-) )
  car cdr lock )

;; @The function <<version>> takes an object and wraps it in a
;; <<versioned-object>> structure if it is not already a versioned object.  This
;; means that it is not possible to version the <<versioned-object>> structure.

;;<<>>=
(defun version (object
                &key (rebase :on-access)
                     copy-fn
                     copy-cost-fn
                     match-fn
                     (read-contention-resolution :error)
                     (tree-splitting-method :none) )
  "Convert an object into a versioned object."
  (if (versioned-object-p object)
      object
      (%make-versioned-object :car object
                              :lock (bt:make-lock) )))

;; @\subsection{Advanced control of the versioning of objects}

;; @The optional arguments to the <<version>> function control how the
;; versioning is done.

;; @{\em rebase} controls whether the object should be made the root of the
;; version tree with each operation.  It can have the value {\em :on-access} or
;; {\em t} for both reads and modification, {\em :on-read}, {\em
;; :on-modification}, or {\em nil} for never rebasing.

;; @{\em tree-splitting-method} controls when the tree should be split.
;; Splitting can speed up access from very different parts of the tree.  As of
;; now we offer methods {\em :never} to never split the tree, {\em
;; :probabilistic} for one that splits on modification with probability $1/N$
;; where $N$ is calculated using {\em copy-cost-fn}, and {\em :edit-length}
;; which will copy if the number of edits to the raw data is more than {\em
;; copy-cost-fn}.  Note that the {\em :edit-length} method most likely requires
;; approximately twice as long to access a particular value than just rebasing.
;; This is done in the hopes that further access from edits near the new copy
;; will make up for the difference.

;; @{\em read-contention-resolution} controls how the library should react if
;; someone tries to access two different versions of the same object for
;; arguments to a function call.  By default we just produce an error, but we
;; can also copy the array (incurring a perhaps severe performance penalty).

;; @{\em copy-fn} is used to specify a function that will copy the raw data.
;; This is necessary for wrapping user defined data structures.  Likewise, {\em
;; copy-cost-fn} is a function that will predict the cost of copying a object.
;; The return value should be the number of elements in the object.  This is a
;; bit ill-defined, but I hope you get the point.  We may offer an option to
;; specify this as a predicted time allowing for more precise optimization.

;; @\{em match-fn} is a function that determines if a given edit corresponds to
;; an accessor form.  This allows us to implement non-rebasing reads and edits.
;; These functions are predefined for arrays and hash tables and can be
;; autogenerated (in a costly manner) for arbitrary data.

;; @To put this all in not very clear language, this basically works like this.
;; A versioned object is a list whose last element is the object.  When you
;; access a value from the object, the object moves to the first element of the
;; list (making it a list of length 1) while at the same time reversing the list
;; after it and inverting the deltas.

;; @The `function' <<raise-object!>> makes your version of the object current.
;; This involves a lot of mutation and thus a lock is held.

;;<<>>=
(defun raise-object! (v-obj)
  "Bubble object to beginning of list, along the way back, reverse the list.
This assumes that locks are already held."
  (if (not (vo-cdr v-obj))
      nil ; This value is ignored
      (destructuring-bind (new-val getter setter)
          (vo-car v-obj)
        (raise-object! (vo-cdr v-obj))
        ;; Move the object
        (setf (vo-car v-obj)
              (vo-car (vo-cdr v-obj)) )
        ;; Invert delta
        (setf (vo-car (vo-cdr v-obj))
              (list (funcall getter)
                    getter setter ))
        ;; Mutate object
        (funcall setter new-val)
        ;; Reverse the list
        (setf (vo-cdr (vo-cdr v-obj))
              v-obj )
        ;; Terminate the list
        (setf (vo-cdr v-obj) nil) )))


;; @The macro <<vmodf>> acts as the main entry point.  It has the same syntax as
;; <<modf>> except no special treatment for <<modf-eval>>.

;; <<>>=
(defmacro vmodf (place value &rest more)
  "Add a new entry ot the version tree of the underlying container and return
that new version of the object.  Several place/value pairs can be given.  In
between each pair, a symbol must be given to specify where the result of the
previous calculation should be bound for the rest of the VMODF place/value
pairs."
  ;; First we need to find the "container" variable
  (let ((container (find-container place)))
    (alexandria:with-gensyms (val-sym v-obj new-version container-sym)
      `(let* ((,val-sym ,value)
              (,v-obj ,container)
              (,new-version
                (bt:with-lock-held ((vo-lock ,container))
                  (raise-object! ,container)
                  (let* ((,container-sym (vo-car ,v-obj))
                         (getter (lambda ()
                                   (let ((,container ,container-sym))
                                     ,place )))
                         (setter (lambda (new-val)
                                   (let ((,container ,container-sym))
                                     (setf ,place new-val) )))
                         ;; Grab the old value
                         (old-value (funcall getter))
                         (delta (list old-value getter setter)) )
                    ;; Set the new value
                    (funcall setter ,val-sym)
                    ;; Make a new versioned object with the change
                    (setf (vo-cdr ,v-obj) (%make-versioned-object
                                           :car ,container-sym
                                           :cdr nil
                                           :lock (vo-lock ,v-obj) )
                          (vo-car ,v-obj) delta )
                    ;; return that new object
                    (vo-cdr ,v-obj) ))))
         ,(if more `(let ((,(first more) ,new-version))
                      (vmodf ,@(cdr more)) )
              new-version )))))

;; @\section{Accessing data in a versioned object}

;; @I'm sorry to say, a versioned object isn't just a wrapper that magically
;; delivers new functionality.  Due to the inherent frailty that comes with data
;; mutation, accessing the data within a versioned object must be done with the
;; utmost of caution.  With this in mind the interface for accessing the data in
;; versioned objects is a bit convoluted.  We provide two functions,
;; <<vfuncall>> and <<vapply>> which you may call and use your versioned
;; structures as arguments.  This allows you to do most of what you probably
;; want to do.

(defun vfuncall (fn &rest args)
  "Like FUNCALL except you can use versioned structures.  Call the function
specified by the first argument with the rest of the arguments as its argument
list."
  (let ((held-locks))
    (unwind-protect
         (let ((new-args
                 (iter (for arg in (cons fn args))
                   (collecting
                    (cond ((versioned-object-p arg)
                           (bt:acquire-lock (vo-lock arg))
                           (push (vo-lock arg) held-locks)
                           (raise-object! arg)
                           (vo-car arg) )
                          (t arg) )))))
           (apply #'funcall new-args) )
      (iter (for lock in held-locks)
        (bt:release-lock lock) ))))

(defun vapply (fn &rest args)
  (apply #'vfuncall fn (apply #'list* args)) )

;; @We recognize that your data is less useful when it is locked into a
;; <<verioned-object>> structure, so we allow for a way to temporarily retrieve
;; it.  In order to get the raw data, use the macros <<with-versioned-object>>
;; and <<with-versioned-objects>>.  You may pass this underlying data to another
;; function that expects the raw object, but the object needs to be locked for
;; the duration of that function call and that function should not modify the
;; values in the object.

;; @The macros <<with-versioned-object>> and <<with-versioned-objects>> are a
;; couple macros that allow you to access your versioned data.  These will make
;; the version you are referencing current and bind the variable to the
;; underlying data.  A lock is held while inside this environment, so be wary of
;; deadlock issues (I am working on something to reduce the chances of deadlock
;; to a mere performance penalty).

;;<<>>=
(defmacro with-versioned-object (object &body body)
  `(bt:with-lock-held ((vo-lock ,object))
     (raise-object! ,object)
     (let ((,object (vo-car ,object)))
       ,@body )))

;;<<>>=
(defmacro with-versioned-objects (objects &body body)
  (if objects
      `(with-versioned-object ,(car objects)
         (with-versioned-objects ,(cdr objects)
           ,@body ))
      `(progn ,@body) ))

;; @\section{Printing}

;; @The printing method needs to make the specified version current, then it
;; prints it like any other object.

(defvar *versioned-object-printer* t
  "When T, objects are printed specially as \"#<Versioned <OBJECT>>\".  If NIL
then the true object is printed (a structure) whose representation can be quite
large.  This is useful for debugging as the special printer, in general, alters
the data structure." )

;;<<>>=
(defmethod print-object ((obj versioned-object) stream)
  (if *versioned-object-printer*
      (vfuncall 'format stream "#<Versioned ~A>" obj)
      (call-next-method) ))

;; @\section{Thread Safety}

;; @There are pretty serious concerns about deadlock.  If there are two threads
;; each accessing two version of the same data, and one is reading from an
;; object and needs to communicate with the other thread which also needs to
;; accessing the data, we have a classic deadlock situation.  This is
;; particularly dangerous here because even reads require locking the data which
;; most people take to be a non-locking operation.

;; @\section{Random Thoughts}

;; @vfuncall and vapply

;; @Eval arguments

;; @After evaluation, run through arguments and lock on any versioned structures
;; using the main object lock.

;; @In case of deadlock, try to use lock on the in-place object lock and use
;; in-place access and versioning tools to move ahead.  If all else fails, bail
;; out.

;; @Many in-place changes can happen at once, however it seems that the any
;; in-place operation is dangerous if the main lock is released.  This means
;; that we should either make the.

;; @We provide special treatment for arrays, hash tables, classes, and
;; structures.  Other objects can either have special functions defined for
;; them, or then can use the general interface which is slower and perhaps more
;; prone to failure (we'll see).

;; @\subsection{Why not implement it as a generalization of Modf?}

;; @While this could, in principle, be done, it is important to note that
;; Versioned-objects breaks the basic assumptions that make Modf work.  This is
;; because the mechanism which it uses is a completely different one.  For
;; instance, it never builds new objects, the primary purpose of Modf.  I
;; considered implementing it as a special syntax within Modf for the sake of
;; combining similar purposes if not similar mechanisms, but decided that this
;; approach would lead to a lot of confusion.  Versioned objects are inherently
;; tied to a central piece of data wrapped in a versioning data structure.  This
;; just doesn't jive with the mindset of Modf.  Further, there is no way to use
;; the Mod functionality and the Versioned-objects functionality at the same
;; time.  While it makes sense to say set the value of the n$^{th}$ element of a
;; versioned array to a Modf constructed new object, it cannot be done as a
;; single generalized Modf statement in any syntax I can think of.


;; @\subsection{Versioned objects are tricky}

;; @Take, for instance, the expression:

;; (let* ((arr1 (version (make-array '(10) :initial-element 0)))
;;        (arr2 (vmodf (aref arr1 0) 5)) )
;;   (aref arr1 (aref arr2 0)) )

;; @It is impossible to retrofit <<compare-arrays>> as a function that can use
;; versioned data.  This is because the access happens within the function,
;; completely out of our control.  Thus {\em arr1} and {\em arr2} must be Lisp
;; arrays yet they refer to the same array.  In such a case, it seems that a
;; copy is the only way to deal with this.

;; (defun compare-arrays (arr1 arr2)
;;   (iter
;;     (for val1 in-sequence arr1)
;;     (for val2 in-sequence arr2)
;;     (always (eql val1 val2)) ))

;; (let* ((arr1 (version (make-array '(10) :initial-element 0)))
;;        (arr2 (vmodf (aref arr1 0) 5)) )
;;   (compare-arrays arr1 arr2) )

;; @We must evaluate all the other arguments prior to the argument that contains
;; the versioned object.  These arguments might include other references to
;; other versions of the same object.

;; (defmacro version-access (form)
;;   (let ((cont-sym (find-container form)))
;;     `(with-versioned-object ,cont-sym
;;        ,form )))

;; @\subsection{On copy functions}

;; @While the basic functionality of this library is easy enough to understand,
;; it is pretty easy to find cases where the behavior is surprising if not down
;; right baffling.  These cases actually make sense once you really understand
;; what is happening.  Take for instance:

;; (defparameter *obj-orig* (version '(1 2 3 #(4 5 6))))

;; (defparameter *ver1* (vmodf (fourth *obj-orig*) #(x y z)))

;; (defparameter *ver2* (vmodf (aref (fourth *obj-orig*) 1) t))

;; @Both versions work exactly as expected.  However, if we specify a copy
;; function that does a shallow copy, i.e. doesn't copy the array in the fourth
;; element, <<*ver2*>> will fail and probably corrupt the entirety of the
;; version tree.  You must copy everything that you have versioning
;; modifications on.  A perfect deep copy would be safe, but this would be
;; inefficient and I'm not even sure possible in Common Lisp.  We would need
;; assurance that at some level every part of an object is an atomic data type
;; which is not something Common Lisp guarantees.

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

;; (defmacro time-operation ((&key (repeat 10000) (min-sample-time 5))
;;                           &body body )
;;   `(let* ((start-time (get-internal-real-time))
;;           (end-time start-time)
;;           (times (iter
;;                    (for times from 0)
;;                    (until (> (setf end-time (get-internal-real-time))
;;                              (+ start-time
;;                                 (* ,min-sample-time
;;                                    internal-time-units-per-second ))))
;;                    (iter (for i below ,repeat)
;;                      ,@body )
;;                    (finally (return times)) )))
;;      (float
;;       (/ (* ,repeat times) (/ (- end-time start-time)
;;                               internal-time-units-per-second )) 0d0)))

;; This doesn't work.  Why?  The printer!?!
;; (defun measure-rebase-time (n-edits)
;;   "Measure how long it takes to rebase the array."
;;   (let* ((arr1 (version (make-array '(10) :initial-element 0)))
;;          (arr2 (iter (for i below n-edits)
;;                  (for new-arr
;;                    initially (vmodf (aref arr1 (random 10)) (random 10))
;;                    then (vmodf (aref new-arr (random 10)) (random 10)) )
;;                  (finally (return new-arr)) )))
;;     (let ((rebases-per-second (time-operation ()
;;                                 (raise-object! arr1)
;;                                 (raise-object! arr2) )))
;;       (values rebases-per-second
;;               ;; Deltas per second
;;               (* rebases-per-second n-edits) ))))

;; @Preliminary results suggest that using a more speciallized approach to
;; versioning can push rebase speed up by ~40%.

;; (defun measure-version-time (n-versions-before-rebase)
;;   "Measure how much time it takes to create new versions of the data if building
;; off of the most recent version."
;;   (let ((arr1 (version (make-array '(10) :initial-element 0))))
;;     (let ((versions-per-second
;;             (time-operation ()
;;               (iter (for i below n-versions-before-rebase)
;;                 (for new-arr
;;                   initially (vmodf (aref arr1 (random 10)) (random 10))
;;                   then (vmodf (aref new-arr (random 10)) (random 10)) )
;;                 (finally (return new-arr)) ))))
;;       (values versions-per-second) )))

;; (defun measure-in-place-walk-time (n-edits)
;;   "Measure how long it takes to read a value from the datastructure without
;; rebasing it."
;;   )

;; (defun measure-in-place-version-time (n-edits)
;;   "Measure how long it takes to add a new version to the data structure without
;; rebasing it.  This implicitly includes a read."
;;   )
