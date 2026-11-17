(defsystem "boxer-sunrise-glfw"
  :author "Steven Githens"
  :license ""
  :components ((:file "boxwin-glfw"))
  :depends-on (:cl-glfw3
               :boxer-sunrise
               :boxer-sunrise-redisplay))
