#|
  This file is a part of boxer-sunrise2 project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

#|
  Author: Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise2-core"
  :version "3.4.2"
  :author "Steven Githens"
  :license ""
  :depends-on ("uiop")
  :components ((:module "src"
                :components
                ((:file "boxer-sunrise2")
                 (:file "package")
                 ;; DEFINITIONS
                 (:file "definitions/macros")
                 (:file "definitions/storage")
                 (:file "definitions/boxdef")
                 (:file "definitions/vrtdef")

                 ;; REDISPLAY
                 (:file "disdcl/disdcl")

                 ;; DRAW
                 ;;   OPEN / CANVAS Specific items
                 (:file "draw/draw-low-stubs")

                 (:file "draw/draw-high-common")
                 (:file "draw/draw-high-hardware-clip")

                 ;; DISPLAYDEFS
                 (:file "displaydefs/disdef")

                 ;; RELATIONSHIPS
                 (:file "relationships/infsup")

                 ;; EVALDEFS
                 (:file "evaldefs/evalmacs")
                )))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
)
