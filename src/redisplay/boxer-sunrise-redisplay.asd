(asdf:defsystem "boxer-sunrise-redisplay"
  :version "3.4.23"
  :author "Steven Githens"
  :license "BSD-3-Clause"
  :depends-on (:boxer-sunrise-core
               :boxer-sunrise-opengl)
  :components ((:file "blinkers")
               (:file "disply")
               (:file "lodisp")
               (:file "dev-overlay")
               (:file "repaint-2024")
               (:file "repaint-pass-2")
               (:file "repaint")
               (:file "new-borders")
               (:file "boxtops")
               (:file "draw-graphics-commands"))
  :description "Boxer Redisplay/repaint")
