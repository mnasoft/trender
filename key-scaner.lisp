;;;;  key-scaner.lisp

(in-package #:trender)

(defvar bmap-list)
(setf bmap-list nil)

(defun key-scaner (width height &optional (host (cond (( equal (software-type) "Linux") "")
						      (( equal (software-type) "Win32") "127.0.0.1") (T ""))))
  (let* ((display (xlib:open-display host))	       ;; Дисплей
	 (screen (first (xlib:display-roots display))) ;; Экран
	 (white (xlib:screen-white-pixel screen))      ;;
	 (root-window (xlib:screen-root screen)) ;; Корневое окно
	 (b1 (xlib:create-gcontext :drawable root-window
				   :foreground (make-allocated-window-color
						root-window (xlib:make-color :blue 0.0 :green 0.0 :red 0.0)))) ;; Графический контекст тонкой чёрной линии
	 (my-window
	  (xlib:create-window :parent root-window :x 0 :y 0 :width width :height height :background white
			      :event-mask (xlib:make-event-mask :exposure :key-press :button-press :structure-notify))) ;; Окно для трендера
	 (actual-width width)	;; Текущая ширина экрана
	 (actual-height height) ;; Текущая высота экрана
	 (m-time (l-math:make-identity 3)) ;; Матрица преобразования временной шкалы
	 (bmap nil) ;; Битовая карта, соответствующая нажатым клавишам клавиатуры
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
      (:exposure ()
		 (xlib:draw-line my-window b1 0 0 actual-width actual-height)
		 (xlib:draw-line my-window b1 actual-width 0  0 actual-height)
		 nil
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
		    (T
		     nil))))))

(defun test_04() (key-scaner 800 600))

;;(test_04)

(defun test_10(string-lst)
  "Функция для определения масок нажатия на клавиши.
Пример использования:
(test_04) - для выхода из программы необходимо использовать Ctrl+q или Alt+x.
Битовые массивы будут сохранены в переменной bmap-list."
  (mapcar #'(lambda(str map) (list str (bit-to-int-list map))) string-lst bmap-list))

;;(mapcar #'(lambda(el) (list (car el) (car(car(cdr el))))) (test_10 '("R-Alt" "L-Windows" "R-Windows" "Menu")))

;;(mapcar  #'car key-list)

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
