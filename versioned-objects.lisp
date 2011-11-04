
(defpackage :versioned-objects
  (:use :cl :bt :modf :iterate)
  (:import-from modf find-container)
  (:export #:version
           #:versioned-object
           #:vfuncall #:vapply
           #:vaccess
           #:with-versioned-object #:with-versioned-objects
           #:vmodf ))

(in-package :versioned-objects)

(defvar *inefficiency-warnings* t
  "Display warnings when inefficient usage is detected." )

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

;; @Using Versioned-objects is simple.  You take any object and call the
;; function <<version>> on it.  This will transform the object into a
;; versioned-object, and immutable object (well, one that shouldn't be mutated).
;; At this point you may make cheap copies that require no more space than the
;; difference between the original and the new copy.  There are a few options
;; you can pass to <<version>> that will determine how the version tree is
;; maintained, but we'll leave that for later.

;; @To access the data in your versioned-object, you must use the functions
;; <<vfuncall>> and <<vapply>> which work precisely as their {\em sans v}
;; counterparts but work with versioned data in their argument lists.  To reduce
;; the number of times you need to write <<vfuncall>>, we provide the macro
;; <<vaccess>> which inserts <<vfuncall>> symbols into an access form.

;; @However, as simple as this all seems, any user of Versioned-Objects should
;; take the time to understand the issues outlined in the section \ref{tricky},
;; {\em Versioned Objects are Tricky}.  Versioned-Objects aren't quite as fool
;; proof as I'd like and it seems as though they will stay that way for the
;; foreseeable future.

;; @\section{Implementation}

;; @The structure <<versioned-object>> is a cons-cell like structure that makes
;; up the version tree for your object.

;;<<versioned-object>>=
(defstruct (versioned-object (:constructor %make-versioned-object)
                             (:conc-name :vo-) )
  car cdr (access-count 0)
  object-type
  copy-fn copy-cost-fn
  tree-splitting-method
  rebase
  contention-resolution
  rebase-lock )

;; @The function <<version>> takes an object and wraps it in a
;; <<versioned-object>> structure.

;;<<>>=
(defun collect-locks (lock-structures)
  (cons (car lock-structures)
        (when (cdr lock-structures)
          (append (collect-locks (cadr lock-structures))
                  (collect-locks (cddr lock-structures)) ))))

;;<<>>=
(defmacro with-rebase-locks* ((&rest objects) &body body)
  (let ((held-locks (gensym))
        (v-obj (gensym)) )
    `(let ((,held-locks nil))
       (unwind-protect
            (progn
              (iter (for ,v-obj in ,objects)
                (iter (for lock in (collect-locks (vo-rebase-lock ,v-obj)))
                  (handler-case (bt:acquire-lock lock)
                    (error ()
                      ;; This is to catch recursive locking attempts
                      (case (vo-contention-resolution ,v-obj)
                        (:error
                         (error
                          "We are already accessing a version of this structure" ))
                        (:split-tree
                         (raise-object! ,v-obj 0 0 t) ))))
                  (push lock ,held-locks) ))
              ,@body )
         (iter (for lock in ,held-locks)
           (handler-case (bt:release-lock lock)
             (error ()
               (warn "Error while releasing lock ~A" lock) )))))))

;;<<>>=
(defmacro with-rebase-locks ((&rest objects) &body body)
  (let ((held-locks (gensym))
        (v-obj (gensym)) )
    `(let ((,held-locks nil))
       (unwind-protect
            (progn
              (iter (for ,v-obj in ,(cons 'list objects))
                (iter (for lock in (collect-locks (vo-rebase-lock ,v-obj)))
                  (handler-case (bt:acquire-lock lock)
                    (error ()
                      ;; This is to catch recursive locking attempts
                      (case (vo-contention-resolution ,v-obj)
                        (:error
                         (error
                          "We are already accessing a version of this structure" ))
                        (:split-tree
                         (raise-object! ,v-obj 0 0 t) ))))
                  (push lock ,held-locks) ))
              ,@body )
         (iter (for lock in ,held-locks)
           (handler-case (bt:release-lock lock)
             (error ()
               (warn "Error while releasing lock ~A" lock) )))))))

;;<<>>=
(defmacro with-locks (locks &body body)
  (let ((held-locks (gensym)))
    `(let ((,held-locks nil))
       (unwind-protect
            (progn
              (iter (for lock in ,locks)
                (handler-case (bt:acquire-lock lock)
                  (error ()
                    ;; This is to catch recursive locking attempts
                    (case (vo-contention-resolution lock)
                      (:error
                       (error
                        "We are already accessing a version of this structure" ))
                      (:split-tree
                       (raise-object! v-obj 0 0 t) )))) 
                (push lock ,held-locks) )
              ,@body )
         (iter (for lock in ,held-locks)
           (handler-case (bt:release-lock lock)
             (error ()
               (warn "Error while releasing lock ~A" lock) )))))))

;;<<>>=
(defun version (object
                &key (object-type t)
                     (rebase :on-access)
                     copy-fn
                     copy-cost-fn
                     match-fn
                     (contention-resolution :error)
                     (tree-splitting-method :none) )
  "Convert an object into a versioned object."
  (declare (ignore match-fn contention-resolution))
  (%make-versioned-object :car object
                          :cdr nil
                          :rebase-lock (cons (bt:make-lock) nil)
                          :object-type object-type
                          :copy-fn copy-fn
                          :copy-cost-fn copy-cost-fn
                          :tree-splitting-method tree-splitting-method
                          :rebase rebase ))

;; @\subsection{Advanced control of the versioning of objects}

;; @The optional arguments to the <<version>> function control how the
;; versioning is done.

;; {\em object-type} is ignored for now and may be removed later.

;; % @\{\em type} determines how the versioning of the object should be done.
;; % Specifying a type here informs versioned objects that the object you are
;; % sending it is of a particular type and it may use certain

;; % The default value, {\em :shallow}, specifies a useful but safe(ish),
;; % middle ground.  This will allow you to version any object, but only using
;; % shallow access.  This means that you can only access and modify values in
;; % your object using single function calls.  Other options for {\em object-type}
;; % include {\em :array} and {\em :hash-table}.  Another option available to
;; % users who understand the dangers inherent with it, is {\em :arbitrary}.  This
;; % mode allows for the most flexibility but also the most danger (and typically
;; % lowest efficiency).  Make sure you understand the content of the section
;; % titled {\em Versioned objects are tricky} and understand the exercises at the
;; % end before using {\em :arbitrary}.

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

;; @\section{How it works}

;; @To put this all in not very clear language, this basically works like this.
;; A versioned object is a list whose last element is the object.  When you
;; access a value from the object, the object moves to the first element of the
;; list (making it a list of length 1) while at the same time reversing the list
;; after it and inverting the deltas.

;; @The `function' <<raise-object!>> makes your version of the object current.
;; This involves a lot of mutation and thus a lock is held.

;;<<>>=
(defun raise-object! (v-obj &optional (cost 0) (largest-access-count 0) force-copy)
  "Bubble object to beginning of list, along the way back, reverse the list.
This assumes that locks are already held.  When the `current' version is found,
it makes the decission to either copy it and return that copy along with the
value of the largest access-count, or return NIL."
  (if (not (vo-cdr v-obj))
      (cond ((or force-copy
                 (and (eql :edit-length (vo-tree-splitting-method v-obj))
                      (vo-copy-fn v-obj) (vo-copy-cost-fn v-obj)
                      (< (funcall (vo-copy-cost-fn v-obj) (vo-car v-obj))
                         cost )))
             ;; New locks
             (let ((new-data-structure (funcall (vo-copy-fn v-obj) (vo-car v-obj)))
                   (new-locks (cons (list (bt:make-lock "left"))
                                    (list (bt:make-lock "right")) )))
               (setf (cdr (vo-lock v-obj)) new-locks)
               (setf (vo-lock v-obj) (car new-locks))
               (values new-data-structure
                       largest-access-count
                       (cdr new-locks) )))
            (t ; Don't copy
             (values nil nil nil) ))
      (destructuring-bind (new-val getter setter)
          (vo-car v-obj)
        (multiple-value-bind (ret largest-count new-lock)
            (raise-object! (vo-cdr v-obj) (1+ cost)
                           (max largest-access-count
                                (vo-access-count v-obj) )
                           force-copy )
          (cond ((and ret (= largest-count (vo-access-count v-obj)))
                 ;;; Perform the split
                 ;; Move the object
                 (setf (vo-car v-obj) ret)
                 ;; Mutate object
                 (funcall setter new-val ret)
                 ;; New locks
                 (setf (vo-lock v-obj) new-lock)
                 ;; Terminate the list
                 (setf (vo-cdr v-obj) nil)
                 (values nil nil) )
                (t
                 ;; Increment the access counter
                 (incf (vo-access-count v-obj))
                 ;; Choose the shorter lock list
                 (setf (vo-lock v-obj)
                       (if (< (length (collect-locks (vo-lock v-obj)))
                              (length (collect-locks (vo-lock (vo-cdr v-obj)))) )
                           (vo-lock v-obj)
                           (vo-lock (vo-cdr v-obj)) ))
                 (cond (ret
                        ;; Mutate object
                        (funcall setter new-val ret)
                        ;; Pass the values to the next level
                        (values ret largest-count new-lock) )
                       (t
                        ;; Move the object
                        (setf (vo-car v-obj)
                              (vo-car (vo-cdr v-obj)) )
                        ;; Invert delta
                        (setf (vo-car (vo-cdr v-obj))
                              (list (funcall getter (vo-car v-obj))
                                    getter setter ))
                        ;; Mutate object
                        (funcall setter new-val (vo-car v-obj))
                        ;; Reverse the list
                        (setf (vo-cdr (vo-cdr v-obj))
                              v-obj )
                        ;; Terminate the list
                        (setf (vo-cdr v-obj) nil)
                        (values nil nil nil) ))))))))


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
                (let* ((getter (lambda (,container-sym)
                                 (let ((,container ,container-sym))
                                   ,place )))
                       (setter (lambda (new-val ,container-sym)
                                 (let ((,container ,container-sym))
                                   (setf ,place new-val) )))
                       (delta (list ,val-sym getter setter)) )
                  (let ((new (copy-structure ,v-obj)))
                    (setf (vo-car new) delta
                          (vo-cdr new) ,v-obj )
                    new ))))
         (when (or (eql :on-access (vo-rebase ,v-obj))
                   (eql :on-modification (vo-rebase ,v-obj)) )
           (with-rebase-locks (vo-lock ,new-version)
             (raise-object! ,new-version) ))
         ,(if more
              `(let ((,(first more) ,new-version))
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

;;<<>>=
(defun vfuncall (fn &rest args)
  "Like FUNCALL except you can use versioned structures.  Call the function
specified by the first argument with the rest of the arguments as its argument
list."
  (with-rebase-locks* (iter (for arg in (cons fn args))
                       (when (versioned-object-p arg)
                         (collect arg) ))
    (let ((new-args
            (iter (for arg in (cons fn args))
              (collecting
               (cond ((versioned-object-p arg)
                      (raise-object! arg)
                      (vo-car arg) )
                     (t arg) )))))
      (apply #'funcall new-args) )))

;;<<>>=
(defun vapply (fn &rest args)
  (apply #'vfuncall fn (apply #'list* args)) )

;;<<>>=
(defmacro vaccess (form)
  "The macro vaccess provides a slightly less annoying way to access a value
from a versioned-object.  This exands the access form by littering it with
VFUNCALLs."
  (if (atom form)
      form
      (list* 'vfuncall (list 'quote (first form))
             (mapcar (lambda (x) (macroexpand (list 'vaccess x))) (rest form)) )))

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
;; contention issues.

;;<<>>=
(defmacro with-versioned-object (object &body body)
  `(with-rebase-locks (list ,object)
     (raise-object! ,object)
     (let ((,object (vo-car ,object)))
       ,@body )))

;;<<>>=
(defmacro with-versioned-objects (objects &body body)
  `(with-rebase-locks (list ,@objects)
     ,@(mapcar (lambda (x) (list 'raise-object! x)) objects)
     (let (,(mapcar (lambda (x) (list x `(vo-car ,x))) objects))
       ,@body )))

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

;; @\section{Specialized versioned object types}

;; @The general interface is quite robust, but perhaps less efficient than we
;; would want.  After all, the general interface involves holding closures which
;; might be considerable bigger than an edit needs to be.  Some results suggest
;; that a specialized interface for arrays (for instance) could be around a
;; factor of two faster than the generic interface.  We intent to provide
;; special treatment for arrays, hash tables, classes, and structures.  We might
;; make a user interface for defining new specialized object interfaces or we
;; might just let them use the generic interface.  We haven't decided yet.

;; @\section{Why not implement it as a generalization of Modf?}

;; @This section is a bit outdated as I am considering making this an extension
;; of Modf using Modf's new extensibility features.

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


;; @\section{Versioned Objects are Tricky}

;; \label{tricky}

;; @\subsection{The immutable issue}

;; In general, the object passed to <<version>> should never be accessed or
;; mutated after calling <<version>>.  You can't do this:

;; In a much more concerning case, any data that is handled by versioned
;; objects, either by modifying a place using <<vmodf>> or reading a place,
;; needs be treated as a constant object.  Constant means not only immutation,
;; but also that it should not be modified using the versioning apparatus using
;; <<vmodf>>.  This is a subtle issue and thus I will attempt to demonstrate how
;; this can bite you.

;; To illustrate the issues involved here, we will be versioning lists.  I use
;; lists because their shared structure and the functions commonly used to
;; access data in lists provide an excellent example of how Versioned-Objects
;; can subtly produce errors.  Keep in mind, however, that there is never a good
;; reason to use this versioning system on lists as it will always have poorer
;; performance actually building new lists.

;; (defvar *test1* (version '(1 2 3 4)))
;; (defvar *test2* (vmodf (second *test1*) t))

;; (defvar *rest-test1* (vaccess (cdr *test1*)))

;; The symbols <<*test1*>> and <<*test2*>> are seemingly valid versions of the
;; data.  However, we accessed the <<cdr>> of <<*test1*>> (stored in
;; <<*rest-test1*>> meaning we have to treat <<cdr>> of the list as a constant
;; object which cannot be mutated or modified using versioning.  But we also
;; violated that restriction by modfying the second value in <<*test1*>>, which
;; is the first value in <<*rest-test1*>>.

;; Still don't see how something like this is going to cause trouble?  Try this
;; on:

;; (print *test1*) => #<Versioned: '(1 2 3 4)
;; (print *rest-test1*) => '(2 3 4)
;; (print *test2*) => #<Versioned: '(1 t 3 4)
;; (print *rest-test1*) => '(t 3 4)

;; By violating this rule, we have turned our Lisp system into a land where data
;; can magically change under our noses.  Things get much worse when we consider
;; that other threads could be accessing versioned data and, therefore,
;; modifying data with no warning what-so-ever.

;; It would be nice if we could provide some kind of safe-guards or checking
;; that would protect new users from this kind of issue.  However, I think this
;; is impossible at compile time, expensive at runtime, and quite possibly
;; impossible to ever do with any generality.  So, I am leaving this in an
;; unsafe state.  As long as you follow the rules, you will be okay.  If you
;; don't, I guarantee something will go wrong eventually.

;; @\subsection{The locking issue}

;; @While these issues are still important, we have found ways to circumvent
;; most (all) serious issues associated with this.  Despite this, not
;; understanding the concepts outlined in this section can lead you to write
;; code with poor performance, so take some time to understand the simple
;; concepts.

;; @The crux of the matter is this, it is impossible to use versioned objects
;; with a <<compare-arrays>> function when using the naive versioning apparatus.
;; This is because the arrays that would be given as arguments might be two
;; different versions from the same version tree, meaning that both locks cannot
;; be held at the same time.  When writting Versioned-Objects we chose to use
;; the native Lisp features whenever possible, meaning that we convert versioned
;; objects into ordinary Lisp objects and then send them to ordinary Lisp
;; functions.  If we didn't do this, we would have to write special
;; versioned-object versions of every function we would ever want to use.  This
;; has been a success as Versioned-Objects work very well almost everywhere.
;; The downside is that we can only have one versioned object converted into a
;; normal Lisp object at a time from the versioned object tree.  Which means we
;; can't use a normal Lisp function that compares two arrays element by element.

;; We can, however, use more advanced features such as contention resolution
;; where we perform a forced tree split if such a contentious case is realized.
;; This means that your code won't hit an error, but it will hit a performance
;; loss.  For this reason, it is important to keep this issue in might.

;; (defun compare-arrays (arr1 arr2)
;;   (iter
;;     (for val1 in-sequence arr1)
;;     (for val2 in-sequence arr2)
;;     (always (eql val1 val2)) ))

;; (let* ((arr1 (version (make-array '(10) :initial-element 0)))
;;        (arr2 (vmodf (aref arr1 0) 5)) )
;;   (vfuncall 'compare-arrays arr1 arr2) )

;; @\section{Tree Splitting}

;; As the version tree grows, we expect (and do see) a reduction of access time
;; and increase in the in memory size of the object.  The worst case access time
;; is scales as the longest path in the tree.  The memory usage scales as the
;; size of the version tree.  It is in our best interests to limit the size of
;; the tree in some way as to optimize for performance and/or the in-memory size
;; of the object.

;; As a side note, you may wonder, are there any memory reductions associated
;; with splitting the version tree.  At first glance, it appears that there are
;; not, as each node in the tree corresponds to a version, and thus even if the
;; tree is split, those versions are still valid.  The important part is to
;; realize that the vast majority of versions are actually not referenced in the
;; running program and only serve as a pathway of edits to get in between
;; versions that are being used.  This means that a well placed split, could
;; allow the garbage collector to reap many versions with no reference in the
;; running program.

;; @\subsection{Performance opimization}

;; @While there are other benefits, first and foremost, this library was
;; designed to give performance gains over copying large objects.  With this in
;; mind many have designed methods of reducing the tree size in order to
;; increase performance.

;; @\subsubsection{Probablistic copying}

;; @The absolute simplest method of tree size reduction is to split the tree at
;; modification time, thereby limiting the size of the tree.  This is typically
;; done probabilistically with probability $1/N$ where $N$ is the size of the
;; object.  This means that we expect to never travel more than $N$ edits to get
;; to the actual data.  This means that on any given access we might expect an
;; additional $O(N)$ cost.

;; @\subsubsection{Copy on long edit paths}

;; @Since we are walking the edit path in between, we can calculate the length
;; and decide whether a copy is appropriate based on comparing that length to
;; the size of the object.  This will split the tree only when it is proven that
;; it is more performant to do so.  It also means that it will add an additional
;; $O(N)$ cost on some data accesses, and preferentially add this to the
;; accesses that already take a long time to access.

;; When considering where we should break the tree, one might think that it
;; should be at the halfway point (a naive approach), or on an edge that most
;; equally splits the number of nodes between the two trees (perhaps good for
;; memory usage reduction).  The method I have developed is to split based on
;; the number of traversals.  This will split the tree at the most used edit,
;; removing it.  This splits the tree in a way that removes the most used path
;; in the graph.

;; @\subsection{$N$ and cost}

;; @In the preceding sections, we use the number of edits $m$ and the size of
;; the data $N$ as measures of how much work is being done.  This is fine except
;; for the fact that we are comparing quantities involving $N$ with quantities
;; involving $m$.  For tuning performance (memory or speed), we really should
;; consider the actual cost rather than something proportional to the cost.  For
;; instance, an edit might be considerably larger than an element in the data.
;; Likewise with the cost of actually traversing an edit and accessing a data
;; element.  As such, in the discussion above, we should really be concerned
;; with the true cost of accessing and copying data, and traversing and creating
;; edits.

;; @\subsection{Splitting and Locking}

;; @A secondary benefit of splitting version trees is that it reduces the
;; overall contention for resources.  By splitting the tree, we can use two
;; separate locks, allowing access to happen in parallel.  However, since we
;; don't have a global registry of all versions of the data, we have to use a
;; tricky method to update each side of the version tree with it's new locks.
;; The procedure I have developed works like this:

;; @\begin{enumerate}

;; @\item Each version object has a cons cell tree of locks it needs to hold in
;; order to access data.  This usually is a list of length one; one lock, $L_o$
;; for the entire structure.  This list is the same object for all versions in
;; the tree (so that it may be mutated later).

;; @\item When a tree is split, we define two new locks, $L_l$ and $L_r$ which
;; correspond to the two new trees, left and right.  The lock cons structure is
;; destructively modified to include $L_l$ and $L_r$.  It now looks like
;; {\texttt (Lo . ((Ll) . (Lr)))}.  This means that all objects in the original
;; tree must lock the original and both of these new locks, therefore locking
;; the entire tree to perform call <<raise-object!>>.  At this point we have two
;; trees but we lock on both to do anything.

;; @\item We then set the lock cons structure of the versions along the path on
;; the left side to simply the <<cadr>> of its old value and to the <<cddr>> on
;; the right side.  This has the effect of correctly setting the locks on the
;; path in each tree.  At this point we have two trees with a few correct
;; locking structures but we are guaranteed that the actual object is contained
;; in one of these objects with the correct locking structure.

;; @\item We then make have <<raise-object!>> check the locking structure on any
;; edit walk and set the locking structure of both to the minimum depth tree
;; between the two of them.  This will propogate the locking changes to any
;; version that is actually accessed.  Further, old versions that lock too
;; conservatively will not effect accesses with the newer locking structure.

;; @\end{enumerate}
