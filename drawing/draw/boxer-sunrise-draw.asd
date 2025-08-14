(asdf:defsystem "boxer-sunrise-draw"
  :version "3.4.23"
  :author "Steven Githens"
  :license "BSD-3-Clause"
  :depends-on (:boxer-sunrise-definitions)
  :components ((:file "draw-high-common")
               (:file "mesh")
               (:file "model")
               (:file "drawing-prims"))
  :description "Boxer High Level Draw Methods")
