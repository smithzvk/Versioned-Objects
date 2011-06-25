
(in-package :versioned-arrays)

(defpackage :versioned-arrays-test
    (:use :cl :iter :modf :stefil :versioned-arrays)
  (:export #:run-tests) )

(in-package :versioned-arrays-test)

(in-root-suite)

(defun run-tests ()
  (contention) )

(defsuite* :versioning)

(deftest term-by-term-equivalent-2d (a b &optional (test 'eql))
  (is (equal (va-dimensions a) (va-dimensions b))
      "Dimensionality mismatch" )
  (iter (for i below (va-dimension a 0))
        (iter (for j below (va-dimension a 1))
              (is (funcall test (varef a i j) (varef b i j))) )))

(deftest 2d-test ()
  (let* ((a1 (make-versioned-array
              '(3 3) :initial-contents
              '((1 2 3) (4 5 6) (7 8 9)) ))
         (a1-same (make-versioned-array
                   '(3 3) :initial-contents
                   '((1 2 3) (4 5 6) (7 8 9)) ))
         (a2 (modf (varef a1 0 0) -5))
         (a3 (modf (varef a2 0 0) 1))
         (a4 (modf (varef a2 1 1) -5) ))
    (term-by-term-equivalent-2d a1 a1-same)
    (term-by-term-equivalent-2d a1 a3)
    ;; call on a4.  This modifies the structure.
    (varef a4 2 2)
    (term-by-term-equivalent-2d a1 a3) ))

(defsuite* :thread-safety)

(deftest contention ()
  (let* ((arr (make-versioned-array
               '(3 3) :initial-contents
               '((1 2 3) (4 5 6) (7 8 9)) ))
         (new-arr arr) )
    (iter (for i below 1000)
          (let* ((rani (random 3))
                 (ranj (random 3))
                 (curr-value (varef arr rani ranj))
                 (ran-val (random 10)) )
            (setf new-arr (modf (varef new-arr rani ranj) ran-val))
            (setf new-arr (modf (varef new-arr rani ranj) curr-value)) ))
    ;; Now we have two arrays with a large delta between them.
    (bt:make-thread (lambda () (iter (for i below 100)
                                (varef arr (random 3) (random 3)) )))
    (bt:make-thread (lambda () (iter (for i below 100)
                                (varef new-arr (random 3) (random 3)) )))
    (term-by-term-equivalent-2d arr new-arr) ))

(defsuite* :timing)

(defun place-queen (board i j)
  "This complicated bit is to test if a queen can attack another."
  (if (and (iter (for k from 0 below (va-dimension board 0))
                 (never (and (/= i k) (varef board k j))) )
           (iter (for k from 0 below (va-dimension board 1))
                 (never (and (/= j k) (varef board i k))) )
           (iter (for k1 from i downto 0)
                 (for k2 from j downto 0)
                 (never (and (/= k1 i) (/= k2 j) (varef board k1 k2))) )
           (iter (for k1 from i below (va-dimension board 0))
                 (for k2 from j below (va-dimension board 1))
                 (never (and (/= k1 i) (/= k2 j) (varef board k1 k2))) )
           (iter (for k1 from i downto 0)
                 (for k2 from j below (va-dimension board 1))
                 (never (and (/= k1 i) (/= k2 j) (varef board k1 k2))) )
           (iter (for k1 from i below (va-dimension board 0))
                 (for k2 from j downto 0)
                 (never (and (/= k1 i) (/= k2 j) (varef board k1 k2))) ))
      (modf (varef board i j) t)
      nil ))

(defun n-queens (board n row)
  "Simple enough backtracking algorithm."
  (if (= row n)
      board
      (iter (for i below n)
            (let ((result (place-queen board i row)))
              (when result
                (thereis (n-queens result n (+ row 1))) )))))

(defun place-queen* (board i j)
  "This complicated bit is to test if a queen can attack another."
  (if (and (iter (for k from 0 below (array-dimension board 0))
                 (never (and (/= i k) (aref board k j))) )
           (iter (for k from 0 below (array-dimension board 1))
                 (never (and (/= j k) (aref board i k))) )
           (iter (for k1 from i downto 0)
                 (for k2 from j downto 0)
                 (never (and (/= k1 i) (/= k2 j) (aref board k1 k2))) )
           (iter (for k1 from i below (array-dimension board 0))
                 (for k2 from j below (array-dimension board 1))
                 (never (and (/= k1 i) (/= k2 j) (aref board k1 k2))) )
           (iter (for k1 from i downto 0)
                 (for k2 from j below (array-dimension board 1))
                 (never (and (/= k1 i) (/= k2 j) (aref board k1 k2))) )
           (iter (for k1 from i below (array-dimension board 0))
                 (for k2 from j downto 0)
                 (never (and (/= k1 i) (/= k2 j) (aref board k1 k2))) ))
      t
      nil ))

(defun n-queens* (board n row)
  "A bit more convoluted backtracking algorithm."
  (if (= row n)
      board
      (let (winning-board)
        (iter (for i below n)
              (let ((result (place-queen* board i row)))
                (when result
                  (setf (aref board i row) t)
                  (let ((try (n-queens* board n (+ row 1))))
                    (when try
                      (setf winning-board try)
                      (finish) )
                  (setf (aref board i row) nil) ))
              (finally (return winning-board)) )))))

(defun timing-test ()
  "Maybe I can put in a test that makes sure things are scaling right?"
  () )
