#|
  This file is a part of boxer-sunrise project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

#|
  Author: Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise-core"
  :version "3.4.8"
  :author "Steven Githens"
  :license ""
  :depends-on ("uiop")
  :components ((:module "src"
                :components
                ((:file "boxer-sunrise")
                 (:file "package")
                 ;; DEFINITIONS
                 (:file "definitions/macros")
                 (:file "definitions/storage")
                 (:file "definitions/boxdef")
                 (:file "definitions/vrtdef")
                 (:file "definitions/fonts")
                 (:file "definitions/disdcl")



;;                 ;; REDISPLAY
;;                 (:file "disdcl/disdcl")
;;
;;                 ;; DRAW
;;                 #-lispworks (:file "boxwin/libre/opengl")
;;                 (:file "boxwin/lw-capi/platform-utils")
;;                 ;;   OPEN / CANVAS Specific items
;;                ;;  (:file "draw/draw-low-stubs")
;;                 (:file "draw/opengl-utils")
;;                 (:file "draw/freetype-fonts")
;;                 (:file "draw/draw-low-opengl")
;;                 (:file "draw/draw-high-common")
;;                 (:file "draw/draw-high-hardware-clip")
;;
;;                 ;; DISPLAYDEFS
;;                 (:file "displaydefs/disdef")
;;
;;                 ;; RELATIONSHIPS
;;                 (:file "relationships/infsup")
;;
;;                 ;; EVALVARS
;;                 (:file "evalvars/varmacs")
;;                 (:file "evalvars/vars")
;;
;;                 ;; EVALDEFS
;;                 (:file "evaldefs/evalmacs")
;;                 (:file "evaldefs/funs")
;;                 (:file "evaldefs/fdeval")
;;                 (:file "evaldefs/stacks")
;;                 (:file "evaldefs/bind")
;;                 (:file "evaldefs/eval")
;;
;;                 ;; EVALPRIMS
;;                 (:file "evalprims/prims")
;;
;;                 ;; PROCESSES
;;                 (:file "processes/process")
;;
;;                 ;; BOXWIN
;;                 ;; boxwin-opengl....
;;                 ;; skip hardcopy print support for windows.
;;                ;;  (:file "boxwin/mousedoc") ;; package 'mp' does not exist
;;                 (:file "boxwin/boxapp-data")
;;
;;                 ;; BOXNET
;;                 (:file "boxnet/boxnet")
;;
;;                 ;; EDITOR
;;                 (:file "editor/editor")
;;
;;                 ;; REDISPLAY
;;                 (:file "redisplay/disply")
;;                 (:file "redisplay/lodisp")
;;                ;;  (:file "redisplay/repaint") requires CAPI...s
;;                ;;  (:file "redisplay/new-borders")
;;
;;                 ;; GRFDEFS
;;                 (:file "filesystem/fildfs")
;;                 (:file "grfdefs/grobjs")
;;                 (:file "grfdefs/grfdfs")
;;                 (:file "grfdefs/gdispl")
;;                ;; graphics-clear

                )))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "boxer-sunrise-core-test")))
)
