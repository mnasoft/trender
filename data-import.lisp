;;;; data-import.lisp

(in-package #:trender)

(defun read-data-from-file(ps)
  "Выполняет чтение данных из файла, задаваемого строкой ps.
В каждой считанной строке производится замена запятых на точки.
Возвращает список списков считанных значений.
Пример использования:

data.text:
3597	99,9923630558	65,1157667421	97,403186723	45,5835840049
3598	99,6161970783	69,2921906153	79,8477848573	-83,138389836
3599	98,2446989883	82,5596444065	-39,1243861496	-91,5306772002
3600	95,8915723414	96,9755582085	-96,302623624	30,3369291608

(ternder-read-data-from-file \"data.text\")
=>((3597 99.99236 65.11577 97.40318 45.583584)
   (3598 99.616196 69.29219 79.847786 -83.13839)
   (3599 98.2447 82.55965 -39.124386 -91.53068)
   (3600 95.89157 96.975555 -96.30262 30.33693))
"
  (let (
	(p (merge-pathnames ps))
	)
    (with-open-file (s p :if-does-not-exist (format T "!!!1"))
      (do
       ( (l (read-line s) (read-line s nil 'eof))
	 (lst nil)
	 (rez nil)
	 )
       ( (eq l 'eof) (reverse rez))
	(setf lst (read-from-string(format nil "(~A)" (substitute #\. #\, l))))
	(if lst
	    (setf rez (cons lst rez)))))))

(defun calc-max-list-list-size(l)
  "Выполняет поиск максимального количества элементов в списке списков.
Возвращает список первым элементом которого является количество элементов списке,
а вторым элементом - максимальное количество элементов в одном из элементов списка.
Пример использования:
(calc-max-list-list-size '((1 2)(2)(1 2 5)(1 2)))
=> (4 3)
(calc-max-list-list-size '(()()()()))
=> (4 0)
(calc-max-list-list-size '((1)(1 2 3 4 5 6)))
=> (2 6)
"
  (let (
	(i (length l))
	(j (apply #'max (mapcar #'length l)))
	)
    (list i j)))

(defun calc-min-list-list-size(l)
  "Выполняет поиск минимального количества элементов в списке списков.
Возвращает список первым элементом которого является количество элементов списке,
а вторым элементом - максимальное количество элементов в одном из элементов списка.
Пример использования:
(calc-min-list-list-size '((1 2)(2)(1 2 5)(1 2)))
=> (4 1)
(calc-min-list-list-size '(()()()()))
=> (4 0)
(calc-min-list-list-size '((1 2)(1 2 3 4 5 6)))
=> (2 2)
"
  (let (
	(i (length l))
	(j (apply #'min (mapcar #'length l)))
	)
    (list i j)))

(defun list-list->array(l)
  "Создает матрицу типа #2A, состоящую из элементов списка списков.
Если в некоторых подсписках элементов меньше чем в других - 
они заменяются 0.0
Пример использования:
(list-list->array '((1 2)(2)(1 2 5)(1 2)))
=> #2A((1 2 0.0) (2 0.0 0.0) (1 2 5) (1 2 0.0))
"
  (let* ((sz (calc-max-list-list-size l))
	 (i 0)
	 (j 0)
	 (a (make-array sz :initial-element 0.0)))
    (mapc #'(lambda (r)
	      (mapc #'(lambda(c)
			(setf (aref a i j) c)
			(incf j) )
		    r)
	      (setf j 0)
	      (incf i))
	  l)
    a))

(defun array2d->list-array-first-2..n(a)
  (do* ( (in (array-dimension a 0))
	 (jn (array-dimension a 1))
	 (j 1 (1+ j))
	 (rez nil)
	 )
       ((>= j jn) (reverse rez))
    (setf
     rez (cons
	  (do
	   ( (i 0 (1+ i) )
	     (k 0 (+ k 2) )
	     (arr (make-array (* in 2) :initial-element 0.0)) )
	   ((>= i in) arr)
	    (setf
	     (aref arr k ) (aref a i 0)
	     (aref arr (1+ k)) (aref a i j)))
	  rez))))

