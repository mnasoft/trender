;;;; trender.lisp

(in-package #:trender)

(defvar bmap)
(defvar bmap-lst)

(setf bmap-lst nil)
(setf bmap #*0000000000100000000000000000000000000100000000000000000000000000100000000000000000000000000000000000000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

;;; "trender" goes here. Hacks and glory await!

(defun ququ (x)
  (+ 23 x))

(defun my-sqrt(x)
  (sqrt x))

(defun my-sqrt_1(x &key (d 0.0) (k 0.0) (tau 0.0) (d_tau 0.0) (r_size 0) (r_max 1000))
  (+ (sqrt x) d (/ (random r_size) r_max) (* k (sin (+ d_tau(* x tau))))))

