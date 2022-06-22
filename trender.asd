;;;; trender.asd

(defsystem #:trender
  :description "Trender - программа для отображения, навигации, извлечения даных, из тренда параметров"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on (#:clx #:l-math #:lst-arr)
  :serial t
  :components ((:file "package")
               (:file "trender")
	       (:file "data-import")
	       (:file "graph-f")
	       (:file "multiple-graph")	       
	       (:file "key")
	       (:file "test")
	       (:file "key-scaner")))
