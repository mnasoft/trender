;;;;  multiple-graph.lisp

(in-package :trender)

"Функция отображения трендов (тренд -- зависимость параметра от времени)
Параметры:
p-lst - список массивов; длины всех массивов должны быть равны;
количество элементов в каждом массиве должно быть чётным
В каждом массиве:
  - нечетные элементы, являются значениями абсцисс (ось x, ось времени);
  - четные элементы, являются значениями оординат (ось y, ось параметра)
color-lst - список, определяющих цвет соответствующей тренду
note-lst - список, содержащий подписи к трендам
note-dy - список, определяющий вертикальное смещение подписи к тренду
width - запрашиваемая ширина окна
height - запрашиваемая высота окна
host - имя хоста, на котором будет выводиться окно с трендами
Пример использования:
(multiple-graph
 '(#(0 1.149919 2 2.9531503 4 3.9297805 6 4.3405547 8 4.275584 10 4.786599)
   #(0 1.9541985 2 3.8682532 4 4.5254917 6 4.725692 8 3.781838 10 3.025363)
   #(0 0.64563966 2 2.191235 4 2.3382676 6 2.5524974 8 2.5527945 10 2.8086839))
 (list 	  (xlib:make-color :blue 1.0 :green 0.2 :red 0.2)
	  (xlib:make-color :blue 0.2 :green 1.0 :red 0.2)
	  (xlib:make-color :blue 0.2 :green 0.2 :red 1.0))
 '(|blue| |green| |red|) '(20 40 60 80 100) 500 300)"

(defun multiple-graph (p-lst color-lst note-lst note-dy width height
		       &optional (host (cond (( equal (software-type) "Linux") "")
					     (( equal (software-type) "Win32") "127.0.0.1") (T ""))))
  (let* ((display (xlib:open-display host))	       ;; Дисплей
	 (screen (first (xlib:display-roots display))) ;; Экран
	 (white (xlib:screen-white-pixel screen))      ;;
	 (root-window (xlib:screen-root screen)) ;; Корневое окно
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
      (:key-press () ; code
		  (setf bmap (xlib:query-keymap display))
		  (cond
		    ((or
		      (equal (list-to-bit '("L-Alt" "x")) bmap)
		      (equal (list-to-bit '("L-Ctrl" "q")) bmap)
		      (equal (list-to-bit '("R-Alt" "x")) bmap)
		      (equal (list-to-bit '("R-Ctrl" "q")) bmap))
		     (display-close my-window display) T)
		    ((equal (list-to-bit '("Num-+")) bmap)
		     (setf (l-math:matrix-elt m-time 0 0) (* 8/5 (l-math:matrix-elt m-time 0 0))
			   m-lst (mapcar #'(lambda (xy-a)(l-math:* m-time (calc-matrix-xy-array xy-a actual-width actual-height))) p-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) ) nil)
		    ((equal (list-to-bit '("Num--")) bmap)
		     (setf (l-math:matrix-elt m-time 0 0) (* 5/8 (l-math:matrix-elt m-time 0 0))
			   m-lst (mapcar #'(lambda (xy-a)(l-math:* m-time (calc-matrix-xy-array xy-a actual-width actual-height))) p-lst)
			   showable T)
		     (xlib:send-event my-window :exposure (xlib:make-event-mask :exposure)) nil)
		    ((equal (list-to-bit '("Home")) bmap)
		     (setf m-time (m-home m-time actual-width)
			   m-lst (mapcar #'(lambda (xy-a)(l-math:* m-time (calc-matrix-xy-array xy-a actual-width actual-height))) p-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) )
		     nil 
		     )
		    ((equal (list-to-bit '("End")) bmap)
		     (setf m-time (m-end m-time actual-width)
			   m-lst (mapcar #'(lambda (xy-a)(l-math:* m-time (calc-matrix-xy-array xy-a actual-width actual-height))) p-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) )
		     nil 
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
		     nil))))))
