(defsystem "boxer-sunrise-glfw"
  :version "3.4.20"
  :author "Steven Githens"
  :license ""
  :components ((:file "boxwin-glfw"))
  :depends-on (:cl-glfw3
               :cl-freetype2
               :boxer-sunrise))
