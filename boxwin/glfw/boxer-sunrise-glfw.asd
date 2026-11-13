(defsystem "boxer-sunrise-glfw"
  :version "3.4.29"
  :author "Steven Githens"
  :license ""
  :components ((:file "boxwin-glfw"))
  :depends-on (:cl-glfw3
               :boxer-sunrise
               :boxer-sunrise-redisplay))
