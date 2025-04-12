(asdf:defsystem "boxer-sunrise-capi"
  :version "3.4.23"
  :author "Steven Githens"
  :license "BSD-3-Clause"
  :depends-on ("boxer-sunrise")
  :components ((:file "error-dialogs")
               (:file "color-picker-menu")
               (:file "lw-toolbar")
               (:file "click-handlers")
               (:file "pane-callbacks")
               (:file "boxer-lw-opengl-canvas")
               (:file "scrolling")
               (:file "boxwin-opengl")
               (:file "clipboard")
               (:file "outline-tree")
               (:file "file-dialogs")
               (:file "lw-menu")
               (:file "preferences-dialog")
               (:file "confirm-quit-dialogs"))
  :description "CAPI GUI version of Boxer")
