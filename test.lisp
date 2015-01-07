;;;; test.lisp

(in-package #:trender)

(defun my-sqrt(x)
  (sqrt x))

(defun my-sqrt_1(x &key (d 0.0) (k 0.0) (tau 0.0) (d_tau 0.0) (r_size 0) (r_max 1000))
  (+ (sqrt x) d (/ (random r_size) r_max) (* k (sin (+ d_tau(* x tau))))))


;;;;--------------------------------------------------------------------------------


(defvar *time-list*)
(setf *time-list* (create-time-list 0 1000 5000))

(defun test_01()
  (multiple-graph
   (list 
    (make-xy-array
     *time-list*
     (mapcar
      #'(lambda (x) (my-sqrt_1 x :d 0.0 :k 1.5 :tau 0.15 :d_tau 0.78 :r_size 500))
      *time-list*))
    (make-xy-array
     *time-list*
     (mapcar
      #'(lambda (x) (my-sqrt_1 x :d 0.0 :k 2.5 :tau 0.25 :d_tau 0.78 :r_size 500))
      *time-list*))
    (make-xy-array
     *time-list*
     (mapcar
      #'(lambda (x) (my-sqrt_1 x :d 0.0 :k 0.5 :tau 0.45 :d_tau 0.78 :r_size 500))
      *time-list*))
    )
   (list 	  (xlib:make-color :blue 1.0 :green 0.2 :red 0.2)
		  (xlib:make-color :blue 0.2 :green 1.0 :red 0.2)
		  (xlib:make-color :blue 0.2 :green 0.2 :red 1.0)
		  )
   3150 950))

;;;;--------------------------------------------------------------------------------

(defvar assa)
(defvar bassa)

(setf assa (read-data-from-file "~/MyDoc/git/clisp/trender/data.txt"))
(setf bassa (array2d->list-array-first-2..n (list-list->array assa)))

(defun foo()
  (read-data-from-file "~/MyDoc/git/clisp/trender/data.txt"))

(defun test_02()
  (multiple-graph
   bassa
   (list 	  (xlib:make-color :blue 1.0 :green 0.2 :red 0.2)
		  (xlib:make-color :blue 0.2 :green 1.0 :red 0.2)
		  (xlib:make-color :blue 0.2 :green 0.2 :red 1.0)
		  (xlib:make-color :blue 0.2 :green 0.5 :red 0.5)
		  )
   1000 550))
