(asdf:defsystem "boxer-sunrise-redisplay"
  :version "3.4.23"
  :author "Steven Githens"
  :license "BSD-3-Clause"
  :depends-on (:boxer-sunrise-definitions
               :boxer-sunrise-opengl
               :boxer-sunrise-draw)
  :components ((:file "wrap-line")
               (:file "draw-gdispl-graphics")
               (:file "draw-scrollbars")
               (:file "blinkers")
               (:file "disply")
               (:file "lodisp")
               (:file "dev-overlay")
               (:file "repaint-2024")
               (:file "repaint-pass-2")
               (:file "repaint")
               (:file "new-borders")
               (:file "boxtops")
               (:file "draw-graphics-commands")
               (:file "draw-popups"))
  :description "Boxer Redisplay/repaint")
