;;;; matrix.lisp

(in-package :trender)

(defvar a)
(defvar b)
(defvar v)

(setf a (make-array (list 3 3) :element-type 'double :initial-element 0.0))
(setf b (make-array (list 3 3) :element-type 'double :initial-element 0.0))
(setf v (make-array 3 :element-type 'double :initial-element 0.0))

(setf (aref v 0) 5.0
      (aref v 1) 10.0
      (aref v 2) 1.0
      )

(setf (aref a 0 0) 1.0)
(setf (aref a 1 1) 1.0)
(setf (aref a 2 2) 1.0)

(setf (aref b 0 0) 1.0)
(setf (aref b 1 1) 1.0)
(setf (aref b 2 2) 1.0)
(setf (aref b 2 0) 10.0)
(setf (aref b 2 1) 20.0)


(defvar row)
(defvar col)
(defvar c)
(setf row (array-dimension a 0)
      col (array-dimension b 1)
      c (make-array (list row col) :element-type 'double :initial-element 0.0)
      )

(defun matr-matr-mult(a b)
  "Перемножение матриц."
  (do* ( (row (array-dimension a 0))
	 (col (array-dimension b 1))
	 (n (array-dimension a 1))
	 (c (make-array (list row col) :element-type 'double :initial-element 0.0))
	 (i 0 (1+ i)))
       ( (>= i row) c)
    (do ( ( j 0 (1+ j)))
	( (>= j col) 'done)
      (do ( (k 0 (1+ k)))
	  ( (>= k n) 'donnee)
	(setf (aref c i j) (+ (aref c i j) (* (aref a i k)(aref b k j))))))))

(defun vector-matr-mult(v b)
  "Перемножение матриц."
  (do* ( (col (array-dimension b 1))
	 (n (array-dimension v 0))
	 (c (make-array col :element-type 'double :initial-element 0.0))
	 ( j 0 (1+ j)))
       ( (>= j col) c)
    (do ( (k 0 (1+ k)))
	( (>= k n) 'donnee)
      (setf (aref c j) (+ (aref c j) (* (aref v k)(aref b k j)))))))

(defclass matrix()
  ( (row :reader matrix-row
	 :initarg :row
	 :initform 3)
   (col :reader matrix-col
	:initarg :col
	:initform 3)
    (matr :accessor matrix-matr
	  :initarg :matr
	  :initform (make-array (list row col) :element-type 'double :initial-element 0.0))))

(defmethod matrix-single((x matrix))
  (do
   ((i 0 (1+ i)))
   ((>= i (matrix-row x)) x)
    (do ((j 0 (1+ j)))
	((>= j (matrix-col x)) 'done)
      (if (= i j)
	  (setf (aref (matrix-matr x) i j ) 1.0)
	  (setf (aref (matrix-matr x) i j ) 0.0)))))

(defmethod matrix-cross-product((a matrix) (b matrix))
  "Перемножение матриц."
  (do* ( (row (matrix-row a))
	 (col (matrix-col b))
	 (n (matrix-col a))
	 (c (make-instance 'matrix :row row :col col))
	 (i 0 (1+ i)))
       ( (>= i row) c)
    (do ( ( j 0 (1+ j)))
	( (>= j col) 'done)
      (do ( (k 0 (1+ k)))
	  ( (>= k n) 'donnee)
	(setf (aref (matrix-matr c) i j)
	      (+ (aref (matrix-matr c) i j)
		 (* (aref (matrix-matr a) i k)(aref (matrix-matr b) k j))))))))



(defvar matr)
(setf matr (make-instance 'matrix))

(matrix-row matr)
(matrix-col matr)
(matrix-matr matr)
(matrix-single matr)


(defun test_matr-matr-mult ()
  (let ((a (make-array (list 3 3) :element-type 'double :initial-element 0.0))
	(b (make-array (list 3 3) :element-type 'double :initial-element 0.0)))
    (setf
     (aref a 0 0) 1.0
     (aref a 1 1) 1.0
     (aref a 2 2) 1.0
     (aref b 0 0) 1.0
     (aref b 1 1) 1.0
     (aref b 2 2) 1.0
     (aref b 2 0) 10.0
     (aref b 2 1) 20.0)
    (matr-matr-mult a b)))

(defun test-vector-matr-mult()
  (let ((b (make-array (list 3 3) :element-type 'double :initial-element 0.0))
	 (v (make-array 3 :element-type 'double :initial-element 0.0)))
    (setf (aref v 0) 5.0
	  (aref v 1) 10.0
	  (aref v 2) 1.0
	  (aref b 0 0) 1.0
	  (aref b 1 1) 1.0
	  (aref b 2 2) 1.0
	  (aref b 2 0) 10.0
	  (aref b 2 1) 20.0)
  (vector-matr-mult v b)
  )
