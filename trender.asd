;;;; trender.asd

(asdf:defsystem #:trender
  :description "Describe trender here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:clx)
  :serial t
  :components ((:file "package")
               (:file "trender")
	       (:file "data-import")
	       (:file "graph-f")
	       (:file "multiple-graph")
	       (:file "test")))

