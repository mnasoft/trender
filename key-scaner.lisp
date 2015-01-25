;;;;  key-scaner.lisp

(in-package #:trender)

(defun key-scaner (&key
		     (width 160)
		     (height 40)
		     (host (cond (( equal (software-type) "Linux") "")
				 (( equal (software-type) "Win32") "127.0.0.1") (T "")))
		     (str-lst 	      (list "Esc" "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10" "F11" "F12"
					    "ScrollLock" "Pause"
					    "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "="
					    "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]"
					    "a" "s" "d" "f" "g" "h" "j" "k"  "l" ";" "'" "\\"
					    "z" "x" "c" "v" "b" "n" "m" "," "." "/"
					    "Insert" "Delete"
					    "Home" "End"
					    "PgUp" "PgDn"
					    "Up" "Down" "Left" "Right"   
					    "NumLock" "Num-/" "Num-*"
					    "Num--" "Num-+" "Num-Enter"
					    "Num-0" "Num-." "Num-1" "Num-2" "Num-3"
					    "Num-4" "Num-5" "Num-6"
					    "Num-7" "Num-8" "Num-9"
					    "Backspace" "Enter"
					    "Tab" "CapsLock"
					    "L-Shift" "R-Shift"
					    "L-Ctrl" "R-Ctrl"
					    "L-Alt" "R-Alt"
					    "Space"
					    "Menu"
					    "L-Windows" "R-Windows")))
  (let* ((display (xlib:open-display host))	       ;; Дисплей
	 (screen (first (xlib:display-roots display))) ;; Экран
	 (white (xlib:screen-white-pixel screen))      ;;
	 (root-window (xlib:screen-root screen)) ;; Корневое окно
	 (b1 (xlib:create-gcontext :drawable root-window
				   :foreground (make-allocated-window-color
						root-window (xlib:make-color :blue 0.0 :green 0.0 :red 0.0)))) ;; Графический контекст тонкой чёрной линии
	 (w1 (xlib:create-gcontext :drawable root-window
				   :foreground (make-allocated-window-color
						root-window (xlib:make-color :blue 1.0 :green 1.0 :red 1.0))))
	 (my-window
	  (xlib:create-window :parent root-window :x 0 :y 0 :width width :height height :background white
			      :event-mask (xlib:make-event-mask :exposure :key-press :button-press :structure-notify))) ;; Окно для трендера
	 (actual-width width)	;; Текущая ширина экрана
	 (actual-height height) ;; Текущая высота экрана
	 (bmap nil) ;; Битовая карта, соответствующая нажатым клавишам клавиатуры
	 (bmap-list nil) ;; Список битовых карт, соответствующий нажатиям на клавиши
	 (i 0)		 ;; Количество отсканированных символов
	 (str-lst-len (length str-lst)) ;; Количество символов для сканирования
	 )
    (setf bmap-list nil)
    (xlib:map-window my-window)
    (xlib:event-case
	(display :force-output-p t
		 :discard-p t)
      (:configure-notify (width height)
			 (setf actual-width width
			       actual-height height)
			 nil)
      (:button-press (x y) (display-close my-window display)(values (mapcar #'(lambda(str map) (list str (car (bit-to-int-list map)))) str-lst bmap-list) (cons x y)))
      (:key-press ()
		  (setf bmap (xlib:query-keymap display)
			bmap-list (append bmap-list (list bmap))
			i (1+ i))
		  (if (>= i str-lst-len)
		      (progn
			(display-close my-window display)
			(mapcar #'(lambda(str map) (list str (car (bit-to-int-list map)))) str-lst bmap-list))
		      (progn
			(xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure))
			nil)))
      (:exposure ()
		 (xlib:draw-rectangle my-window w1 0 0 actual-width actual-height T )
		 (xlib:draw-glyphs my-window b1 (round actual-width 2) (round actual-height 2) (nth i str-lst))
		 nil))))

