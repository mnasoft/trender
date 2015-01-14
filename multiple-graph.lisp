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

(defun x-text(x text-x &optional (delta 50))
  (let
      ( (x-min (- x text-x ))
       )
    (if (> x-min delta)
	(values x-min T)
	(values x nil))))

(defun fit-xy-to-window-text-xy (xy-vec
				 w-width
				 w-height
				 &key
				   (d-width 50)
				   (d-height 50)				 
				   (text-width 100)
				   (text-height 20)
 )
  (let (
	(a (make-array (length xy-vec)))
	(width (x-text w-width text-width d-width))
	(height (x-text w-height text-height d-height))
	)
    (multiple-value-bind (x-min x-max y-min y-max)
	(bound-xy-vec xy-vec)
      (loop for i from 0 below (length xy-vec) do
	   (setf (aref a i)
		 (if (evenp i)
		     (round  (* width (- (aref xy-vec i) x-min))
			     (- x-max x-min))
		     (round (* height (- y-max (aref xy-vec i)))
			    (- y-max y-min)))))
      a)))


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

(defun dx-text(v)
  (- v 50)
  )

(defun display-close(my-window display)
  (xlib:unmap-window my-window)
  (xlib:destroy-window my-window)
  (xlib:close-display display))

(defun multiple-graph
    (p-lst
     color-lst
     note-lst
     note-dy
     width height
     &optional
       (host (cond (( equal (software-type) "Linux") "")
		   (( equal (software-type) "Win32") "127.0.0.1")
		   (T "")))
       ;;       (text-dx 50)
       )
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
						       :key-press
						       :button-press
						       :structure-notify)))
	 (actual-width width)
	 (actual-height height)
         (t-x-pos (- width 50))
	 )
    (setf bmap-lst nil)
    (mapc #'(lambda (grackon)(describe grackon)) grackon-lst)
    (xlib:map-window my-window)
    (xlib:event-case
	(display :force-output-p t
		 :discard-p t)
      (:configure-notify (width height)
			 (setf
			  actual-width width
                          t-x-pos (dx-text width)
			  actual-height height)
			 nil)
      (:exposure ()
		 (mapc #'(lambda(pl grackon note dy)
			   (xlib:draw-lines my-window
					    grackon
					    (fit-xy-to-window-text-xy pl actual-width  actual-height))
			   (xlib:draw-glyphs
			    my-window grackon t-x-pos dy (format nil "~A" note ) )
			   )
		       p-lst grackon-lst note-lst note-dy
		       )
		 nil)
      (:button-press (x y)
		     ;(display-close my-window display)
		     (cons x y)nil)
      (:key-press (code)
		  (setf bmap (xlib:query-keymap display)
			bmap-lst (append bmap-lst (list (list code bmap))))
		  (cond
		    ((or
		      (equal (list-to-bit '("L-Alt" "x")) bmap)
		      (equal (list-to-bit '("L-Ctrl" "q")) bmap)
		      (equal (list-to-bit '("R-Alt" "x")) bmap)
		      (equal (list-to-bit '("R-Ctrl" "q")) bmap))
		     (display-close my-window display) T)
		    ((equal (list-to-bit '("Home")) bmap)
		     (xlib:activate-screen-saver display) nil
;;;;(display-close my-window display) '("Home")
		     )
		    ((equal (list-to-bit '("End")) bmap)
		     (display-close my-window display) '("End")
		     )
		     ((equal (list-to-bit '("Left")) bmap)
		      (display-close my-window display) '("Left"))
		     ((equal (list-to-bit '("Right")) bmap)
		      (display-close my-window display) '("Right"))
		     ((or (equal (list-to-bit '("Right" "R-Ctrl")) bmap)
			  (equal (list-to-bit '("Right" "L-Ctrl")) bmap))
		      (display-close my-window display) '("Right" "Ctrl"))
		     ((or
		       (equal (list-to-bit '("Left" "L-Ctrl")) bmap)
		       (equal (list-to-bit '("Left" "R-Ctrl")) bmap))
		      (display-close my-window display) '("Left" "Ctrl"))
		     (T
		     (xlib:bell display)
		      nil))))))

;(test_01)
;(test_02)

(defun calc-matrix(x-lst y-lst width height)
  "Вычисляет матрицу преобразования такую, чтобы 
точки, заданные елементами списков x-lst и y-lst,
после преобразования с ее попощью вписывались в 
прямоугольную область с шириной width и высотой height."
  (multiple-value-bind (x-min x-max y-min y-max) (bound-xy-vec (make-xy-array  x-lst y-lst))
    (l-math:*
     (l-math:make-matrix 3 3 :initial-elements (list 1.0 0.0 0.0 0.0 -1.0 height 0.0 0.0 1.0))
     (l-math:create-scale-matrix (list (/ width (- x-max x-min) ) (/ height (- y-max y-min) ) 1.0d0))
     (l-math:create-translation-matrix (list (- 0.0  x-min) (- 0.0 y-min))))))

(defun m-xy(matrix xy-arr)
  "Возвращает целочисленный массив координат точек, являющийся результатом
умножения координат, заданных в массивом xy-arr, на матрицу преобразования matrix.
Пример использования:
(let ((x-lst (list 150.0 200.0 350.0 500.0))
      (y-lst (list 3.5 4.8 6.9 5.6))
      (width 300)
      (height 400))
  (m-xy (calc-matrix x-lst y-lst width height)
	(make-xy-array x-lst y-lst)))
"
  (do*
   (
    (len (array-dimension xy-arr 0))
    (rez (make-array len :element-type 'integer))
    (i 0 (+ i 2))
    (j 1 (+ j 2))
    (v-from (l-math:vector 0.0 0.0 1.0))
    (v-to(l-math:vector 0.0 0.0 1.0))
    )
   ((>= j len) rez)
    (setf
     (l-math:elt v-from 0) (aref xy-arr i)
     (l-math:elt v-from 1) (aref xy-arr j)
     v-to (l-math:* m v-from)
     (aref rez i) (round(l-math:elt v-to 0))
     (aref rez j) (round (l-math:elt v-to 1)))))

;;
(defvar x-lst)
(defvar y-lst)
(defvar width)
(defvar height)

(setf
 x-lst (list 150.0 200.0 350.0 500.0)
 y-lst (list 3.5 4.8 6.9 5.6)
 width 300
 height 400)

(l-math:*
 (calc-matrix x-lst y-lst width height)
 (l-math:vector 150.0 3.5 1.0))
