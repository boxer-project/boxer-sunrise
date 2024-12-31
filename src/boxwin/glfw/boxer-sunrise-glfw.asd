(defsystem "boxer-sunrise-glfw"
  :version "3.4.22"
  :author "Steven Githens"
  :license ""
  :components ((:file "boxwin-glfw"))
  :depends-on (:cl-glfw3
               :cl-freetype2
               :boxer-sunrise))
