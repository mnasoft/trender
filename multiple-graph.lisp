;;;;  multiple-graph.lisp

(in-package #:trender)

(defun create-time-list (start end n)
  (do ( (rez nil)
	(i 0 (1+ i)))
      ((> i n) rez)
    (setf rez (cons (+ start (* (- end start )(/ (- n i) n))) rez))))

(defun create-x-list (time_lst func )
  (mapcar func time_lst)
  )

(defun make-xy-array(xl yl)
  "Собирает из двух списков одномерный массив
нечетными элементами которого, являются элементы списка xl;
четными -- yl.
Пример использования:
(make-xy-array '(0 1 2 3) '(10 11 12 13))
=>  #(0 10 1 11 2 12 3 13)
"
  (do* ( (pl (mapcar #'list xl yl))
	 (pl-len (length pl))
	 (a (make-array (* 2 pl-len)))
	 (p nil)
	 (i 0 (1+ i)))
       ((>= i pl-len) a)
    (setf p (car pl)
	  pl (cdr pl)
	  (aref a (* i 2)) (car p)
	  (aref a (1+ (* i 2))) (cadr p)
	  )))

(defun multiple-graph (p-lst color-lst width height &optional (host "127.0.0.1"))
  "
Пример использования:
(multiple-graph
 '(#(0 1.149919 2 2.9531503 4 3.9297805 6 4.3405547 8 4.275584 10 4.786599)
   #(0 1.9541985 2 3.8682532 4 4.5254917 6 4.725692 8 3.781838 10 3.025363)
   #(0 0.64563966 2 2.191235 4 2.3382676 6 2.5524974 8 2.5527945 10 2.8086839))
 (list 	  (xlib:make-color :blue 1.0 :green 0.2 :red 0.2)
	  (xlib:make-color :blue 0.2 :green 1.0 :red 0.2)
	  (xlib:make-color :blue 0.2 :green 0.2 :red 1.0)
	  )
 500
 300
 )
"
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
;;;;	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (grackon-lst (mapcar
		      #'(lambda(color)
			 (xlib:create-gcontext
			  :drawable root-window
			  :foreground (xlib:alloc-color (xlib:window-colormap root-window) color)
			  :background white
			  :line-width 1)
			 )
		      color-lst))
	 (my-window (xlib:create-window
		     :parent root-window
		     :x 0
		     :y 0
		     :width width
		     :height height
		     :background white
		     :event-mask (xlib:make-event-mask :exposure
						       :button-press
						       :structure-notify)))
	 (actual-width width)
	 (actual-height height))
    (mapc #'(lambda (grackon)(describe grackon)) grackon-lst)
    (xlib:map-window my-window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:configure-notify (width height)
			 (setf actual-width width actual-height height)
			 nil)
      (:exposure ()
		 (mapc #'(lambda(pl grackon)
			   (xlib:draw-lines my-window
					    grackon
					    (fit-xy-to-window pl actual-width  actual-height)))
		       p-lst
		       grackon-lst
		       )
		 nil)
      (:button-press () t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))

