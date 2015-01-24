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


(defun make-xy-array (x-lst y-lst)
  "Собирает из двух списков одномерный массив
нечетными элементами которого, являются элементы списка x-lst;
четными -- y-lst
Пример использования:
;(make-xy-array '(0 1 2 3 4) '(10 11 12 13))
;=>  #(0 10 1 11 2 12 3 13)"
  (do* ( (len (* 2 (min (length x-lst) (length y-lst))))
	 (a (make-array len)) (i 0 (+ i 2)) (j 1 (+ j 2)))
       ((>= j len) a)
    (setf (aref a i) (car x-lst) x-lst (cdr x-lst)
	  (aref a j) (car y-lst) y-lst (cdr y-lst))))

(defun calc-matrix(x-lst y-lst width height)
  "Вычисляет матрицу преобразования такую, чтобы 
точки, заданные елементами списков x-lst и y-lst,
после преобразования с ее помощью, вписывались в 
прямоугольную область с шириной width и высотой height
Пример использования:
;(let ((x-lst (list 150.0 200.0 350.0 500.0)) (y-lst (list 3.5 4.8 6.9 5.6)) (width 300) (height 400)) (calc-matrix x-lst y-lst width height))
;=> #<L-MATH:MATRIX 3 x 3
;0.857 0.000 -128.571 
;0.000 -117.647 811.765 
;0.000 0.000 1.000 >"
  (multiple-value-bind (x-min x-max y-min y-max) (bound-xy-vec (make-xy-array  x-lst y-lst))
    (l-math:*
     (l-math:make-matrix 3 3 :initial-elements (list 1.0 0.0 0.0 0.0 -1.0 height 0.0 0.0 1.0))
     (l-math:create-scale-matrix (list (/ width (- x-max x-min) ) (/ height (- y-max y-min) ) 1.0d0))
     (l-math:create-translation-matrix (list (- 0.0  x-min) (- 0.0 y-min))))))

(defun calc-matrix-xy-array(xy-array width height)
  "Вычисляет матрицу преобразования такую, чтобы 
точки, заданные елементами массива xy-array,
после преобразования с ее помощью, вписывались в 
прямоугольную область с шириной width и высотой height
; (let ((x-lst (list 150.0 200.0 350.0 500.0)) (y-lst (list 3.5 4.8 6.9 5.6)) (xy-array (make-xy-array x-lst y-lst)) (width 300) (height 400)) (calc-matrix-xy-array xy-array width height))
;=> #<L-MATH:MATRIX 3 x 3
;0.857 0.000 -128.571 
;0.000 -117.647 811.765 
;0.000 0.000 1.000 >
"
  (multiple-value-bind (x-min x-max y-min y-max) (bound-xy-vec xy-array)
    (l-math:*
     (l-math:make-matrix 3 3 :initial-elements (list 1.0 0.0 0.0 0.0 -1.0 height 0.0 0.0 1.0))
     (l-math:create-scale-matrix (list (/ width (- x-max x-min) ) (/ height (- y-max y-min) ) 1.0d0))
     (l-math:create-translation-matrix (list (- 0.0  x-min) (- 0.0 y-min))))))


(defun calc-matrix-xmin-xmax-ymin-ymax(width  ; Ширина окна
				       height ; Высота окна
				       x-min  ; Минимальное значение 
				       x-max
				       y-min
				       y-max
				       x 
				       &optional (xscale (/ width (- x-max x-min)))
					 )
    (l-math:*
     (l-math:make-matrix 3 3 :initial-elements (list 1.0 0.0 (* width 0.5) 0.0 -1.0 height 0.0 0.0 1.0))
     (l-math:create-scale-matrix (list xscale (/ height (- y-max y-min) ) 1.0d0))
     (l-math:create-translation-matrix (list (- x) (- y-min)))))

(defun m-xy(matrix xy-arr)
  "Возвращает целочисленный массив координат точек, являющийся результатом
умножения координат, заданных в массивом xy-arr, на матрицу преобразования matrix
Пример использования:
; (let ((x-lst (list 150.0 200.0 350.0 500.0)) (y-lst (list 3.5 4.8 6.9 5.6))(width 300) (height 400)) (m-xy (calc-matrix x-lst y-lst width height) (make-xy-array x-lst y-lst)))
;=> #(0 400 43 247 171 0 300 153)"
  (do*
   (
    (len (array-dimension xy-arr 0))
    (rez (make-array len :element-type 'integer))
    (i 0 (+ i 2))
    (j 1 (+ j 2))
    (v-from (l-math:vector 0.0 0.0 1.0))
    (v-to nil)
    )
   ((>= j len) rez)
    (setf
     (l-math:elt v-from 0) (aref xy-arr i)
     (l-math:elt v-from 1) (aref xy-arr j)
     v-to (l-math:* matrix v-from)
     (aref rez i) (round (l-math:elt v-to 0))
     (aref rez j) (round (l-math:elt v-to 1)))))

(defun dx-text(v)
  (- v 50)
  )

(defun make-allocated-window-color(window color)
  (xlib:alloc-color (xlib:window-colormap window) color))

(defun make-grakon-list(window color-background color-list &optional (line-width 2) )
  (mapcar
   #'(lambda(color)
       (xlib:create-gcontext
	:drawable window
	:foreground (make-allocated-window-color window color)
	:background color-background
	:line-width line-width)
       )
   color-list))

(defun display-close(my-window display)
  (xlib:unmap-window my-window)
  (xlib:destroy-window my-window)
  (xlib:close-display display))

(defun m-end(m w &optional (scale (l-math:matrix-elt m 0 0)))
  (let ((m1 (l-math:make-identity 3))
	(m2 (l-math:make-identity 3))
	(m3 (l-math:make-identity 3)))
    (setf
     (l-math:matrix-elt m1 0 2) (- w)
     (l-math:matrix-elt m2 0 0) scale
     (l-math:matrix-elt m3 0 2) (* 0.5 w))
    (l-math:* m3 m2 m1)))

(defun m-home (m w &optional (scale (l-math:matrix-elt m 0 0)))
  (let ((m1 (l-math:make-identity 3))
	(m2 (l-math:make-identity 3)))
    (setf
     (l-math:matrix-elt m1 0 0) scale
     (l-math:matrix-elt m2 0 2) (* 0.5 w))
    (l-math:* m2 m1)))

(defun m-x (m w x &optional (scale (l-math:matrix-elt m 0 0)) )
  (let ((m1 (l-math:make-identity 3))
	(m2 (l-math:make-identity 3))
	(m3 (l-math:make-identity 3))
	)
    (setf
     (l-math:matrix-elt m1 0 2) (- x)
     (l-math:matrix-elt m2 0 0) scale
     (l-math:matrix-elt m3 0 2) (* 0.5 w))
    (l-math:* m3 m2 m1))
  )
  
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
      (:key-press (code)
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

;(test_01)
;(test_02)

(defun bound-x-vec(x)
  (do ((index 0 (+ index 2))
       (x-min (aref x 0) (min x-min (aref x index)))
       (x-max (aref x 0) (max x-max (aref x index))))
      ((>= index (length x))
       (values x-min x-max))))

(defun bound-x-2d-array(x-2d-array)
  "Выполняет поиск минимального и максимального значения в двумерном массиве x-2d-array
Поиск ведется по столбцам
Возвращает два значения: 
- массив минимальных значений;
- массив максимальных значений
Пример использования:
;(bound-x-2d-array #2A((10 0.9999833 1.4999437 100 3.1622777) (11 1.0999779 1.6499251 121 3.3166249) (12 1.1999712 1.7999028 144 3.4641016) (13 1.2999634 1.9498764 169 3.6055512) (14 1.3999543 2.0998456 196 3.7416575)))
;=> #(10 0.9999833 1.4999437 100 3.1622777), #(14 1.3999543 2.0998456 196 3.7416575)"
  (let* ((n (array-dimension x-2d-array 0)) ;; Количество строк
	 (m (array-dimension x-2d-array 1)) ;; Количество столбцов
	 (x-min (make-array m))
	 (x-max (make-array m)))
    (do ((j 0 (1+ j)))
	((>= j m) 'done)
      (setf (aref x-min j) (aref x-2d-array 0 j)
	    (aref x-max j) (aref x-2d-array 0 j)))
    (do ((i 1 (1+ i)))
	((>= i n) 'done)
      (do ((j 0 (1+ j)))
	  ((>= j m) 'done)
	(setf (aref x-min j) (min (aref x-min j) (aref x-2d-array i j))
	      (aref x-max j) (max (aref x-max j) (aref x-2d-array i j)))))
    (format T "~Ax~A" n m)
    (values x-min x-max)))

(defun bound-x-2d-array-min-max(x-2d-array foo)
  "Выполняет поиск значения по предикату foo в двумерном массиве x-2d-array
Поиск ведется по столбцам
Возвращает два значения: 
- массив минимальных значений;
- массив максимальных значений
Пример использования:
(bound-x-2d-array-min-max  #2A((10 0.9999833 1.4999437 100 3.1622777)
			       (11 1.0999779 1.6499251 121 3.3166249)
			       (12 1.1999712 1.7999028 144 3.4641016)
			       (13 1.2999634 1.9498764 169 3.6055512)
			       (14 1.3999543 2.0998456 196 3.7416575)) #'min)
=> #(10 0.9999833 1.4999437 100 3.1622777)
(bound-x-2d-array-min-max  #2A((10 0.9999833 1.4999437 100 3.1622777)
			       (11 1.0999779 1.6499251 121 3.3166249)
			       (12 1.1999712 1.7999028 144 3.4641016)
			       (13 1.2999634 1.9498764 169 3.6055512)
			       (14 1.3999543 2.0998456 196 3.7416575)) #'max)
=> #(14 1.3999543 2.0998456 196 3.7416575)"
  (let* ((n (array-dimension x-2d-array 0)) ;; Количество строк
	 (m (array-dimension x-2d-array 1)) ;; Количество столбцов
	 (x (make-array m)))
    (do ((j 0 (1+ j)))
	((>= j m) 'done)
      (setf (aref x j) (aref x-2d-array 0 j)))
    (do ((i 1 (1+ i)))
	((>= i n) 'done)
      (do ((j 0 (1+ j)))
	  ((>= j m) 'done)
	(setf (aref x j) (funcall foo (aref x j) (aref x-2d-array i j)))))
    x))

(defun array1d->list(a)
  "Пример использования:
;(array1d->list(make-array 5 :initial-contents '(1 2 3 4 5)))
=> (1 2 3 4 5)"
  (do (
       (lst nil)
       (i 0 (1+ i)))
      ((>= i (array-dimension a 0)) (reverse lst))
    (setf lst (cons (aref a i) lst))))

(defun multi-graph (x-2d-array color-lst note-lst note-dy width height
		    &optional (host (cond (( equal (software-type) "Linux") "")
					  (( equal (software-type) "Win32") "127.0.0.1") (T ""))))
  (let* ((display (xlib:open-display host))	       ;; Дисплей
	 (screen (first (xlib:display-roots display))) ;; Экран
	 (white (xlib:screen-white-pixel screen))      ;;
	 (root-window (xlib:screen-root screen)) ;; Корневое окно
	 (i 0) ;; Индекс, задающий текущее положение курсора трендера
	 (xi (aref x-2d-array i 0)) ;; Значение переменой времени, соответствующее текущему положению трендера
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
	 (p-lst (array2d->list-array-first-2..n x-2d-array))
	 (m-time (l-math:make-identity 3)) ;; Матрица преобразования временной шкалы
	 (xy-min-lst (array1d->list(bound-x-2d-array-min-max x-2d-array #'min))) ;; Массив минимальных значений аргументов
	 (xy-max-lst (array1d->list(bound-x-2d-array-min-max x-2d-array #'max))) ;; Массив максимальных значений аргументов
	 (x-min (car xy-min-lst))
	 (x-max (car xy-max-lst))
	 (y-min-lst (cdr xy-min-lst))
	 (y-max-lst (cdr xy-max-lst))
	 (xscale (/ actual-width (- x-max x-min))) ; Масштаб по оси x
	 (m-lst (mapcar #'(lambda (y-min y-max)
			    (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max (* 0.5 (+ x-min x-max))))
			y-min-lst 
			y-max-lst)) ;; Создание списка матриц преобразования трендов
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
      (:key-press (code)
		  (setf bmap (xlib:query-keymap display))
		  (cond
		    ((or
		      (equal (list-to-bit '("L-Alt" "x")) bmap)
		      (equal (list-to-bit '("L-Ctrl" "q")) bmap)
		      (equal (list-to-bit '("R-Alt" "x")) bmap)
		      (equal (list-to-bit '("R-Ctrl" "q")) bmap))
		     (display-close my-window display) T)
		    ((equal (list-to-bit '("Num-+")) bmap)
		     (setf xscale (* (/ 16 10) xscale)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max x-min))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) ) nil)
		    ((equal (list-to-bit '("Num--")) bmap)
		     (setf xscale (* (/ 10 16) xscale)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max x-min))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event my-window :exposure (xlib:make-event-mask :exposure)) nil)
		    ((equal (list-to-bit '("Home")) bmap)
		     (setf
		      i 0
		      xi (aref x-2d-array 0 i)
		      m-lst (mapcar #'(lambda (y-min y-max)
					(calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max x-min))
				    y-min-lst
				    y-max-lst)
		      showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) )
		     nil 
		     )
		    ((equal (list-to-bit '("End")) bmap)
		     (setf
		      i (1- (array-dimension x-2d-array 0))
		      xi (aref x-2d-array i 0)
		      m-lst (mapcar #'(lambda (y-min y-max)
					(calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max x-max))
				    y-min-lst
				    y-max-lst)
		      showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure))
		     nil 
		     )
		    ((equal (list-to-bit '("Left")) bmap)
		     (setf i (max 0 (- i 1))
			   xi (aref x-2d-array i 0)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max xi))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) )
		     nil 
		     )
		    ((equal (list-to-bit '("Right")) bmap)
		     (setf i (min (1- (array-dimension x-2d-array 0)) (+ i 1))
			   xi (aref x-2d-array i 0)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max xi))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure))
		     nil
		     )
		    ((or (equal (list-to-bit '("Right" "R-Shift")) bmap)
			 (equal (list-to-bit '("Right" "L-Shift")) bmap))
		     (setf i (min (1- (array-dimension x-2d-array 0)) (+ i 10))
			   xi (aref x-2d-array i 0)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max xi))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure))
		     nil
		     )
		    ((or (equal (list-to-bit '("Left" "R-Shift")) bmap)
			 (equal (list-to-bit '("Left" "L-Shift")) bmap))
		     (setf i (max 0 (- i 10))
			   xi (aref x-2d-array i 0)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max xi))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) )
		     nil 
		     )
		    ((or (equal (list-to-bit '("Right" "R-Ctrl")) bmap)
			 (equal (list-to-bit '("Right" "L-Ctrl")) bmap))
		     (setf i (min (1- (array-dimension x-2d-array 0)) (+ i 60))
			   xi (aref x-2d-array i 0)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max xi))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure))
		     nil
		     )
		    ((or (equal (list-to-bit '("Left" "R-Ctrl")) bmap)
			 (equal (list-to-bit '("Left" "L-Ctrl")) bmap))
		     (setf i (max 0 (- i 60))
			   xi (aref x-2d-array i 0)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max xi))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) )
		     nil 
		     )
		    ((or (equal (list-to-bit '("Right" "R-Alt")) bmap)
			 (equal (list-to-bit '("Right" "L-Alt")) bmap))
		     (setf i (min (1- (array-dimension x-2d-array 0)) (+ i 600))
			   xi (aref x-2d-array i 0)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max xi))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure))
		     nil
		     )
		    ((or (equal (list-to-bit '("Left" "R-Alt")) bmap)
			 (equal (list-to-bit '("Left" "L-Alt")) bmap))
		     (setf i (max 0 (- i 600))
			   xi (aref x-2d-array i 0)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max xi))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) )
		     nil 
		     )
		    ((or (equal (list-to-bit '("Right" "R-Shift" "R-Ctrl")) bmap)
			 (equal (list-to-bit '("Right" "L-Shift" "L-Ctrl")) bmap))
		     (setf i (min (1- (array-dimension x-2d-array 0)) (+ i 3600))
			   xi (aref x-2d-array i 0)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max xi))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure))
		     nil
		     )
		    ((or (equal (list-to-bit '("Left" "R-Shift" "R-Ctrl")) bmap)
			 (equal (list-to-bit '("Left" "L-Shift" "L-Ctrl")) bmap))
		     (setf i (max 0 (- i 3600))
			   xi (aref x-2d-array i 0)
			   m-lst (mapcar #'(lambda (y-min y-max)
					     (calc-matrix-xmin-xmax-ymin-ymax actual-width actual-height x-min x-max y-min y-max xi))
					 y-min-lst
					 y-max-lst)
			   showable T)
		     (xlib:send-event  my-window :exposure (xlib:make-event-mask :exposure) )
		     nil 
		     )
		    (T nil))))))

;;(test_05)

(defun test_05()
  (setf assa (cond ((equal (software-type) "Win32")
		    (read-data-from-file "D:/home/_namatv/git/clisp/trender/data.txt"))
		   ((equal (software-type) "Linux")
		    (read-data-from-file "~/MyDoc/git/clisp/trender/data.txt"))))
  (setf bassa  (list-list->array assa)) ; (array2d->list-array-first-2..n ... ) 
  (multi-graph bassa
	       (list (xlib:make-color :blue 1.0 :green 0.2 :red 0.2)
		     (xlib:make-color :blue 0.2 :green 1.0 :red 0.2)
		     (xlib:make-color :blue 0.2 :green 0.2 :red 1.0)
		     (xlib:make-color :blue 0.2 :green 0.5 :red 0.5) )
	       (list "t04-01" "t04-02" "t04-03" "t04-04")
	       (list 20 40 60 80 100 120 140)
	       1000 550))
