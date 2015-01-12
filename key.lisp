;;;;  key.lisp

(in-package #:trender)

(defvar key-list)
(defvar key-code-hash)
(defvar key-string-hash)

(setf
 key-list
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
   ("Num-0" 90) ("Num-." 91) ("Num-1" 87) ("Num-2" 88) ("Num-3" 89) ("Num-4" 83)
   ("Num-5" 84) ("Num-6" 85) ("Num-7" 79) ("Num-8" 80) ("Num-9" 81) 
   ("Backspace" 22) ("Tab" 23)("Enter" 36) ("CapsLock" 66)
   ("L-Shift" 50)("R-Shift" 62)
   ("L-Ctrl" 37) ("R-Ctrl" 105)
   ("L-Alt" 64) ("Space" 65) ("R-Alt" 108) ("Windows" 133) ("Menu" 135)))

(setf key-code-hash (make-hash-table :size 256))
(setf key-string-hash (make-hash-table :size 256 :test #'equal))
(mapc #'(lambda(el) (setf (gethash (cadr el) key-code-hash) (car el))) key-list)
(mapc #'(lambda(el) (setf (gethash (car el) key-string-hash) (cadr el))) key-list)

(defun bit-to-string(bm)
  (do* ((i 0 (1+ i))
	(bm-size (array-dimension bm 0))
	(rez "")
	(add-str "") )
       ((>= i 256) rez)
    (if (= (bit bm i) 1)
	(setf rez (concatenate 'string rez add-str (gethash i key-code-hash))
	      add-str "+"))))

(defun bit-to-list(bm)
  (do* ((i 0 (1+ i))
	(bm-size (array-dimension bm 0))
	(rez nil))
       ((>= i 256) rez)
    (if (= (bit bm i) 1)
	(setf rez (concatenate 'list rez (list(gethash i key-code-hash)))))))

(defun list-to-bit(lst)
  (let ((bmap (make-array 256 :element-type 'bit)))
    (mapc #'(lambda (key) (setf (bit bmap (gethash key key-string-hash)) 1 )) lst)
    bmap))

(defun test_03()
  (bit-to-string(list-to-bit (list "Num-6" "Windows" "Esc" "R-Ctrl" "\\"))))


