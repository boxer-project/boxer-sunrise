(defsystem "boxer-sunrise-embedded"
  :version "3.4.26"
  :author "Steven Githens"
  :license ""
  :components ((:file "boxer-embedded")
              ;;  #+emscripten (:file "embedded-utils")
              ;;  #+emscripten (:file "music-prims")
              ;;  #+emscripten (:file "video-prims")
               )
  :depends-on (:boxer-sunrise-core))
