
(defpackage :versioned-objects-test
  (:use :cl :stefil :versioned-objects :alexandria)
  (:export :run-tests))

(in-package :versioned-objects-test)

(in-root-suite)

(defclass test-parent ()
  ((a :accessor a-of :initarg :a)
   (b :accessor b-of :initarg :b)))

(defclass test-child (test-parent)
  ((b :accessor b-of :initarg :b)
   (c :accessor c-of :initarg :c)))

(defstruct test a b)

(deftest object-tests ()
  "These are all extremely simple tests for fundamental functionality.  If these
fail, you probably did something pretty stupid."
  ;; List tests
  (let ((lst (version (copy-list '(1 2 3 4)))))
    (is (vfuncall 'equalp (vmodf (second lst) t) '(1 t 3 4)))
    (is (vfuncall 'equalp (vmodf (nth 2 lst) t) '(1 2 t 4)))
    (is (vfuncall 'equalp (vmodf (second lst) t
                                 & (nth 3 &) t) '(1 t 3 t)))
    (is (vfuncall 'equalp lst '(1 2 3 4))))
  ;; Array tests
  (let ((arr (version (make-array '(4) :initial-contents '(1 2 3 4)))))
    (is (vfuncall 'equalp (vmodf (aref arr 1) t) #(1 t 3 4)))
    (is (vfuncall 'equalp (vmodf (aref arr 1) t
                                 & (aref & 3) t) #(1 t 3 t)))
    (is (vfuncall 'equalp arr #(1 2 3 4))))
  ;; Hash table tests
  (let ((ht (version (alexandria:alist-hash-table
                      '((0 . 1) (1 . 2) (2 . 3))))))
    (is (equalp (sort (vfuncall 'alexandria:hash-table-alist
                                (vmodf (gethash 1 ht) t))
                      #'< :key 'first)
                '((0 . 1) (1 . t) (2 . 3))))
    (is (equalp (sort (vfuncall 'alexandria:hash-table-alist
                                (vmodf (gethash 0 ht) t
                                       & (gethash 2 &) t))
                      #'< :key 'first)
                '((0 . t) (1 . 2) (2 . t))))
    (is (equalp (sort (vfuncall 'alexandria:hash-table-alist ht)
                      #'< :key 'first)
                '((0 . 1) (1 . 2) (2 . 3)))))
  ;; Class instance tests
  (let ((parent (version (make-instance 'test-parent :a 1 :b 2)))
        (child (version (make-instance 'test-child :a 1 :b 3 :c 4))))
    (is (equal '(t 2) (let ((new-obj (vmodf (a-of parent) t)))
                        (list (vfuncall 'a-of new-obj) (vfuncall 'b-of new-obj)))))
    (is (equal '(t nil) (let ((new-obj (vmodf (a-of parent) t
                                              & (b-of &) nil)))
                          (list (vfuncall 'a-of new-obj) (vfuncall 'b-of new-obj)))))
    (is (equal '(1 2) (let ((new-obj parent))
                        (list (vfuncall 'a-of new-obj) (vfuncall 'b-of new-obj)))))
    (is (equal '(t 3 4) (let ((new-obj (vmodf (a-of child) t)))
                          (list (vfuncall 'a-of new-obj)
                                (vfuncall 'b-of new-obj)
                                (vfuncall 'c-of new-obj)))))
    (is (equal '(:a :b :c) (let ((new-obj (vmodf (a-of child) :a
                                                 & (b-of &) :b
                                                 & (c-of &) :c)))
                             (list (vfuncall 'a-of new-obj)
                                   (vfuncall 'b-of new-obj)
                                   (vfuncall 'c-of new-obj)))))
    (is (equal '(1 3 4) (let ((new-obj child))
                          (list (vfuncall 'a-of new-obj)
                                (vfuncall 'b-of new-obj)
                                (vfuncall 'c-of new-obj))))))
  ;; Structure instance tests
  (let ((struct (version (make-test :a 1 :b 2))))
    (is (equal '(t 2) (let ((new-obj (vmodf (test-a struct) t)))
                        (list (vfuncall 'test-a new-obj)
                              (vfuncall 'test-b new-obj)))))
    (is (equal '(t nil) (let ((new-obj (vmodf (test-a struct) t
                                              & (test-b &) nil)))
                          (list (vfuncall 'test-a new-obj)
                                (vfuncall 'test-b new-obj)))))
    (is (equal '(1 2) (let ((new-obj struct))
                        (list (vfuncall 'test-a new-obj)
                              (vfuncall 'test-b new-obj)))))))

(deftest nested-object-tests ()
  (let ((obj (version (list 1 2 3 (make-array '(3) :initial-contents '(4 5 6))))))
    (is (vfuncall 'equalp
                  '(1 2 3 #(4 t 6))
                  (vmodf (aref (nth 3 obj) 1) t)))
    (is (vfuncall 'equalp
                  '(1 nil 3 #(4 t 6))
                  (vmodf (aref (nth 3 obj) 1) t
                         & (nth 1 &) nil)))))

(deftest run-tests ()
  (object-tests)
  (nested-object-tests))

