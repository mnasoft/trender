;;;; test.lisp

(in-package #:trender)

(defun my-sqrt(x &key (d 0.0) (k 0.0) (tau 0.0) (d_tau 0.0) (r_size 0) (r_max 1000))
  (+ (sqrt x) d (/ (random r_size) r_max) (* k (sin (+ d_tau(* x tau))))))

(defun test_01()
  (let ((array2d
	 (lst-arr:list-list->array
	  (mapcar #'(lambda (x)
		      (list x
			    (my-sqrt x :d 0.0 :k 1.5 :tau 0.15 :d_tau 0.78 :r_size 500)
			    (my-sqrt x :d 0.0 :k 2.5 :tau 0.25 :d_tau 0.78 :r_size 500)
			    (my-sqrt x :d 0.0 :k 0.5 :tau 0.45 :d_tau 0.78 :r_size 500)))
		  (create-time-list 0 1000 10000))))
	(color-lst (list
		    (xlib:make-color :blue 1.0 :green 0.0 :red 0.0)
		    (xlib:make-color :blue 0.0 :green 1.0 :red 0.0)
		    (xlib:make-color :blue 0.0 :green 0.0 :red 1.0)))
	(tag-lst (list "t04-01" "t04-02" "t04-03" "t04-04"))
	(dy-lst (list 20 40 60 80 100 120 140))
	(window-width 800)
	(window-height 600))
    (multi-graph array2d color-lst tag-lst dy-lst window-width window-height)))

(defun test_02()
  (let* (
	 (assa
	  (cond ((equal (software-type) "Win32") (read-data-from-file "D:/home/_namatv/git/clisp/trender/data.txt"))
		((equal (software-type) "Linux") (read-data-from-file "~/MyDoc/git/clisp/trender/data.txt"))))
	 (bassa (lst-arr:list-list->array assa)) ; (array2d->list-array-first-2..n ... ) 
	 )
    (multi-graph bassa
		 (list (xlib:make-color :blue 1.0 :green 0.0 :red 0.0)
		       (xlib:make-color :blue 0.0 :green 1.0 :red 0.0)
		       (xlib:make-color :blue 0.0 :green 0.0 :red 1.0)
		       (xlib:make-color :blue 0.0 :green 0.5 :red 0.5) )
		 (list "t04-01" "t04-02" "t04-03" "t04-04")
		 (list 20 40 60 80 100 120 140) 1000 550)))

(defun test_03()
  (bit-to-string(list-to-bit (list "Num-6" "Windows" "Esc" "R-Ctrl" "\\"))))

;;(test_01)
;;(test_02)
;;(test_03)




