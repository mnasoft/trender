;;;;  key-scaner.lisp

(in-package #:trender)

(defvar bmap-list)
(setf bmap-list nil)

(defun key-scaner (p-lst color-lst note-lst note-dy width height
		       &optional (host (cond (( equal (software-type) "Linux") "")
					     (( equal (software-type) "Win32") "127.0.0.1") (T ""))))
  (let* ((display (xlib:open-display host))	       ;; Дисплей
	 (screen (first (xlib:display-roots display))) ;; Экран
	 (white (xlib:screen-white-pixel screen))      ;;
	 (root-window (xlib:screen-root screen)) ;; Корневое окно
	 (i 0) ;; Индекс, задающий текущее положение курсора трендера
	 (w1 (xlib:create-gcontext
	      :drawable root-window
	      :foreground (make-allocated-window-color root-window
						       (xlib:make-color :blue 1.0 :green 1.0 :red 1.0)))) ;; Графический контекст для заднего фона окна
	 (b1 (xlib:create-gcontext :drawable root-window
				   :foreground (make-allocated-window-color
						root-window (xlib:make-color :blue 0.0 :green 0.0 :red 0.0)))) ;; Графический контекст тонкой чёрной линии
	 (grackon-lst (make-grakon-list root-window white color-lst)) ;; Список графических контекстов для трендов
	 (my-window
	  (xlib:create-window :parent root-window :x 0 :y 0 :width width :height height :background white
			      :event-mask (xlib:make-event-mask :exposure :key-press :button-press :structure-notify))) ;; Окно для трендера
	 (actual-width width)	;; Текущая ширина экрана
	 (actual-height height) ;; Текущая высота экрана
         (t-x-pos (- width 50)) ;; Смещение текста подписей относительно правой границы окна
	 (m-time (l-math:make-identity 3)) ;; Матрица преобразования временной шкалы
	 (m-lst (mapcar #'(lambda (xy-a)(l-math:* m-time (calc-matrix-xy-array xy-a width height))) p-lst)) ;; Создание списка матриц преобразования трендов
	 (showable T) ;; Переменная отвечает за отображение трендов
	 (bmap nil) ;; Битовая карта, соответствующая нажатым клавишам клавиатуры
	 )
    (mapc #'(lambda (grackon)(describe grackon)) grackon-lst) ;;
    (setf bmap-list nil)
    (xlib:map-window my-window)
    (xlib:event-case
	(display :force-output-p t
		 :discard-p t)
      (:configure-notify (width height)
			 (setf actual-width width
			       t-x-pos (dx-text width)
			       actual-height height
			       m-lst (mapcar #'(lambda (xy-a)(l-math:* m-time (calc-matrix-xy-array xy-a width height))) p-lst))
			 nil)
      (:exposure ()
		 (when showable
		   (xlib:draw-rectangle my-window w1 0 0 actual-width actual-height T )
		   (mapc #'(lambda(pl grackon note dy) (xlib:draw-glyphs my-window grackon t-x-pos dy (format nil "~A" note ))) p-lst grackon-lst note-lst note-dy)
		   (mapc #'(lambda(xy-arr grackon m) (xlib:draw-lines my-window grackon (m-xy m xy-arr))) p-lst grackon-lst m-lst)
		   (xlib:draw-line my-window b1 (round actual-width 2) 0 (round actual-width 2) actual-height)
		   )
		 (when (not showable)
		   (xlib:draw-line my-window b1 0 0 actual-width actual-height )
		   (xlib:draw-line my-window b1 actual-width 0  0 actual-height))
		 (setf showable nil)
		 )
      (:button-press (x y)(cons x y) nil)
      (:key-press (code)
		  (setf bmap (xlib:query-keymap display)
			bmap-list (append bmap-list (list bmap)))
		  (cond
		    ((or
		      (equal (list-to-bit '("L-Alt" "x")) bmap)
		      (equal (list-to-bit '("L-Ctrl" "q")) bmap)
		      (equal (list-to-bit '("R-Alt" "x")) bmap)
		      (equal (list-to-bit '("R-Ctrl" "q")) bmap))
		     (display-close my-window display) T)
		    ((equal (list-to-bit '("Num-+")) bmap)
		     (setf (l-math:matrix-elt m-time 0 0) (* (/ 16 10) (l-math:matrix-elt m-time 0 0))
			   m-lst (mapcar #'(lambda (xy-a)(l-math:* m-time (calc-matrix-xy-array xy-a actual-width actual-height))) p-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) ) nil)
		    ((equal (list-to-bit '("Num--")) bmap)
		     (setf (l-math:matrix-elt m-time 0 0) (* (/ 10 16) (l-math:matrix-elt m-time 0 0))
			   m-lst (mapcar #'(lambda (xy-a)(l-math:* m-time (calc-matrix-xy-array xy-a actual-width actual-height))) p-lst)
			   showable T)
		     (xlib:send-event my-window :exposure (xlib:make-event-mask :exposure)) nil)
		    ((equal (list-to-bit '("F5")) bmap)
		     (setf m-time (m-home m-time actual-width)
			   m-lst (mapcar #'(lambda (xy-a)(l-math:* m-time (calc-matrix-xy-array xy-a actual-width actual-height))) p-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) )
		     nil 
		     )
		    ((equal (list-to-bit '("F6")) bmap)
		     (setf m-time (m-end m-time actual-width)
			   m-lst (mapcar #'(lambda (xy-a)(l-math:* m-time (calc-matrix-xy-array xy-a actual-width actual-height))) p-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) )
		     nil 
		     )
		    (T
		     nil))))))

(defun test_04()
  (key-scaner
   (list (make-xy-array *time-list* (mapcar #'(lambda (x) (my-sqrt_1 x :d 0.0 :k 1.5 :tau 0.15 :d_tau 0.78 :r_size 500)) *time-list*)))
   (list (xlib:make-color :blue 1.0 :green 0.0 :red 0.0)) (list "t04-01") (list 20) 800 600))

;;(test_04)

(defun test_10(string-lst)
  "Функция для определения масок нажатия на клавиши.
Пример использования:
(test_04) - для выхода из программы необходимо использовать Ctrl+q или Alt+x.
Битовые массивы будут сохранены в переменной bmap-list."
  (mapcar #'(lambda(str map) (list str (bit-to-int-list map))) string-lst bmap-list))

(mapcar #'(lambda(el) (list (car el) (car(car(cdr el)))))
 (test_10 '("R-Alt" "L-Windows" "R-Windows" "Menu")))

;(mapcar  #'car key-list)

'("Esc" "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10" "F11" "F12"
  "ScrollLock" "Pause"
  "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "="
  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]"
  "a" "s" "d" "f" "g" "h" "j" "k"  "l" ";" "'" "\\"
  "z" "x" "c" "v" "b" "n" "m" "," "." "/"
  "Home" "Up" "PgUp" "Left" "Right" "End" "Down" "PgDn"
  "Insert" "Delete"
  "NumLock" "Num-/" "Num-*"
  "Num--" "Num-+" "Num-Enter"
  "Num-0" "Num-." "Num-1" "Num-2" "Num-3"
  "Num-4" "Num-5" "Num-6"
  "Num-7" "Num-8" "Num-9"
  "Backspace" "Tab" "Enter" "CapsLock"
  "L-Shift" "R-Shift"
  "L-Ctrl" "R-Ctrl"
  "L-Alt" "Space" "R-Alt" "L-Windows" "R-Windows" "Menu")
