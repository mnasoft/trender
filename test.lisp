;;;; test.lisp

(in-package #:trender)

(defvar *time-list*)
(defvar assa)
(defvar bassa)

(setf *time-list* (create-time-list 0 1000 10000))

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
   (list "t04-01" "t04-02" "t04-03" "t04-04")
   (list 20 40 60 80 100 120 140)
   800 600))

(defun test_02()
  (setf
   assa
   (cond
     ((equal (software-type) "Win32")
      (read-data-from-file "D:/home/_namatv/git/clisp/trender/data.txt"))
     ((equal (software-type) "Linux")
      (read-data-from-file "~/MyDoc/git/clisp/trender/data.txt"))
     ))
  (setf bassa (array2d->list-array-first-2..n (list-list->array assa)))
  (multiple-graph
   bassa
   (list 	  (xlib:make-color :blue 1.0 :green 0.2 :red 0.2)
		  (xlib:make-color :blue 0.2 :green 1.0 :red 0.2)
		  (xlib:make-color :blue 0.2 :green 0.2 :red 1.0)
		  (xlib:make-color :blue 0.2 :green 0.5 :red 0.5)
		  )
   (list "t04-01" "t04-02" "t04-03" "t04-04")
   (list 20 40 60 80 100 120 140)
   1000 550))

(defun pick2numbers(x-range y-range)
  (let* ((display (xlib:open-display ""))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (window
	    (xlib:create-window
	     :parent (xlib:screen-root screen)
	     :class :input-output
	     :x 0
	     :y 0
	     :width x-range
	     :height y-range
	     :background black
	     :event-mask (xlib:make-event-mask
			  :button-press))))
    (xlib:change-property window
			  :wm_name
			  "Pick two numbers"
			  :string
			  8
			  :transform #'char-code)
      (xlib:map-window window)
      (xlib:event-case
	  (display :force-output-p t
		   :discard-p t)
	(:button-press
	 (x y)
	 (xlib:unmap-window window)
	 (xlib:destroy-window window)
	 (xlib:close-display display)
	 (cons x (- y-range (+ y 1)))))))

;(test_01)
;(test_02)
 


