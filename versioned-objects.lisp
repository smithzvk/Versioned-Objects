
(defpackage :versioned-objects
  (:use :cl :bt :modf)
  (:import-from modf find-container)
  (:export #:version
           #:versioned-object
           #:with-versioned-object #:with-versioned-objects
           #:vmodf ))

(in-package :versioned-objects)

;; @Because this breaks the basic assumptions that make Modf work, this has to
;; be implemented as a new macro.  This means that the mechanism which it uses
;; is a completely different one.  For instance, it never builds new objects.  I
;; considered implementing it as a special syntax within Modf for the sake of
;; combining similar purposes if not similar mechanisms, but decided that this
;; approach would lead to a lot of confusion.  Versioned objects are inherently
;; tied to a central piece of data wrapped in a versioning data structure.  This
;; just doesn't jive with the mindset of Modf.

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
(defun version (object)
  "Convert an object into a versioned object."
  (if (versioned-object-p object)
      object
      (%make-versioned-object :car object
                              :lock (bt:make-lock) )))

;; @Basically this works like this.  A versioned object is a list whose last
;; element is the object.  When you access a value from the object, the object
;; moves to the first element of the list (making it a list of length 1) while
;; at the same time reversing the list after it and inverting the deltas.

;; Not being used right now.
(defvar *locked-by-form* nil
  "This variable marks what has been locked by a given WITH-VERSIONED form.
This will allow it do detect if a single form tries to hold the same lock
several times, something you cannot do, but might get confused and do." )

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

;; @The `function' <<raise-object!>> makes your version of the object current.
;; This involves a lot of mutation and thus a lock is held.

;;<<>>=
(defun raise-object! (v-obj)
  "Bubble object to beginning of list, along the way back, reverse the list.
This assumes that locks are already held."
  (if (not (vo-cdr v-obj))
      nil
      (progn
        (raise-object! (vo-cdr v-obj))
        (destructuring-bind (new-val getter setter &rest idx)
            (vo-car v-obj)
          ;; Move the object
          (setf (vo-car v-obj)
                (vo-car (vo-cdr v-obj)) )
          ;; Invert delta
          (setf (vo-car (vo-cdr v-obj))
                (list* (apply getter
                              (vo-car v-obj) idx )
                       getter setter idx ))
          ;; Mutate object
          (apply setter new-val (vo-car v-obj) idx)
          ;; Reverse the list
          (setf (vo-cdr (vo-cdr v-obj))
                v-obj )
          ;; Terminate the list
          (setf (vo-cdr v-obj) nil) ))))

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
    (alexandria:with-gensyms (val-sym v-obj new-version)
      `(let* ((,val-sym ,value)
              (,v-obj ,container)
              (,new-version
                (bt:with-lock-held ((vo-lock ,container))
                  (raise-object! ,container)
                  (let* ((,container (vo-car ,v-obj))
                         (getter (lambda () ,place))
                         (setter (lambda (new-val) (setf ,place new-val)))
                         ;; Grab the old value
                         (old-value (funcall getter))
                         (delta (list old-value getter setter)) )
                    ;; Set the new value
                    (funcall setter ,val-sym)
                    ;; Update the versioned object with the change
                    (setf (vo-cdr ,v-obj) (%make-versioned-object
                                           :car ,container
                                           :lock (vo-lock ,v-obj) )
                          (vo-car ,v-obj) delta )
                    (vo-cdr ,v-obj) ))))
         ,(if more `(let ((,(first more) ,new-version))
                      (vmodf ,@(cdr more)) )
              new-version )))))

;; @The printing method needs to make the specified version current, then it
;; prints it like any other object.

;; <<>>=
(defmethod print-object ((obj versioned-object) stream)
  (with-versioned-object obj
    (format stream "#<Versioned ~A>" obj) ))

;; @\section{Thread Safety}

;; @There are pretty serious concerns about deadlock.  If there are two threads
;; each accessing two version of the same data, and one is reading from an
;; object and needs to communicate with the other thread which also needs to
;; accessing the data, we have a classic deadlock situation.  This is
;; particularly dangerous here because even reads require locking the data which
;; most people take to be a non-locking operation.

;; @\section{Random Thoughts}

;; @We might want to implement <<with-versioned-object>> via
;; <<symbol-macrolet>>.  This will remove the possibility of of editting the
;; object, which would corrupt (potentially) all other versions of the object.

;; @Another thought.  Perhaps there is a setf expansion we can define for our
;; <<symbol-macrolet>> form that would allow us to detect that this needs to be
;; a versioned edit.  That way, we could actually use <<setf>> as <<modf>> in
;; the environment of a <<with-versioned-object>> form.  This would be nice, but
;; possibly confusing.  Perhaps the name should be renamed to
;; <<with-object-as-immutable>>.
