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
		     (display-close my-window display)
		     (cons x y))
      (:key-press (code)
		  (setf bmap (xlib:query-keymap display)
			bmap-lst (append bmap-lst (list (list code bmap))))
		  (cond
		    ((or
		      (equal
		       bmap
		       #*0000000000000000000000000000000000000000000000000000010000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
		      (equal
		       bmap
		       #*0000000000000000000000001000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
		     (display-close my-window display) T)
		    ( T
		     nil)
		    )))))

;bmap-lst (append bmap-lst (list (list code bmap)))

;(test_02)
'(
  ("Esc" 9)
  ("F1" 67) ("F2" 68) ("F3" 69) ("F4" 70)
  ("F5" 71) ("F6" 72) ("F7" 73) ("F8" 74)
  ("F9" 75) ("F10" 76) ("F11" 95) ("F12" 96)
  ("ScrollLock" 78) ("Pause" 127)
  ("`" 49) ("1" 10) ("2" 11) ("3" 12) ("4" 13) ("5" 14) ("6" 15) ("7" 16) ("8" 17) ("9" 18) ("0" 19) ("-" 20) ("=" 21) 
  ("q" 24) ("w" 25) ("e" 26) ("r" 27) ("t" 28) ("y" 29) ("u" 30) ("i" 31) ("o" 32) ("p" 33) ("[" 34) ("]" 35)
  ("a" 38) ("s" 39) ("d" 40) ("f" 41) ("g" 42) ("h" 43) ("j" 44) ("k" 45) ("l" 46) (";" 47) ("'" 48) ("\\" 51)
  ("z" 52) ("x" 53) ("c" 54) ("v" 55) ("b" 56) ("n" 57) ("m" 58) ("," 59) ("." 60) ("/" 61)
  ("Home" 110) ("Up" 111) ("PgUp" 112) ("Left" 113) ("Right" 114)
  ("End" 115) ("Down" 116) ("PgDn" 117) ("Insert" 118) ("Delete" 119)
  ("NumLock" 77) ("Num-/" 106) ("Num-*" 63) ("Num--" 82) ("Num-+" 86) ("Num-Enter" 104)
  ("Num-0" 90) ("Num-." 91) ("Num-1" 87) ("Num-2" 88) ("Num-3" 89) ("Num-4" 83) ("Num-5" 84) ("Num-6" 85) ("Num-7" 79) ("Num-8" 80) ("Num-9" 81) 
  ("Backspace" 22) ("Tab" 23)("Enter" 36) ("CapsLock" 66)
  ("L-Shift" 50)("R-Shift" 62)
  ("L-Ctrl" 37) ("R-Ctrl" 105)
  ("L-Alt" 64) ("Space" 65) ("R-Alt" 108) ("Windows" 133) ("Menu" 135) 
  )

(mapcar #'(lambda (el1 el2) (list el1 (car el2)))
	(list
	 "R-Shift"
	 "PgDn"
	 "Up"
	 "Down"
	 "Left"
	 "Right"
	 "Space"
	 "R-Alt"
	 "Fn"
	 "Menu"

	 "R-Ctrl"
	 "9"
	 "+"
	 "Enter"

	 "="
	 "Backspace"
	 "Delete"
	 "NumLock"
	 "/"
	 "*"
	 "-"
	 "k"
	 "l"
	 ";"
	 "'"
	 "\\"


	 "z"
	 "x"
	 "c"
	 "v"
	 "b"
	 "n"
	 "m"
	 ","
	 "."
	 "/")
	bmap-lst)

(defvar bmap)
(defvar bmap-lst)
(setf bmap-lst nil)
(setf bmap #*0000000000100000000000000000000000000100000000000000000000000000100000000000000000000000000000000000000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(aref bmap 10)
(equal #*010 #*010)


 
