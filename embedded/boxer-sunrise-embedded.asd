(defsystem "boxer-sunrise-embedded"
  :version "3.4.27"
  :author "Steven Githens"
  :license ""
  :components ((:file "boxer-embedded")
               #+emscripten (:file "embedded-utils"))
  :depends-on (:boxer-sunrise-core))
