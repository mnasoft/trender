;;;; package.lisp

(defpackage :trender
  (:use #:cl)
  (:export key-scaner)		   ; Сканирование клавиш на клавиатуре
  (:export multi-graph)		   ; Построение тренда
  (:export test_01)		   ; Тестирование функции построения тренда
  (:export test_02)		   ; Тестирование функции построения тренда
  (:export test_03)
  )

(in-package :trender)
