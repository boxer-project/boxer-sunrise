#|
  This file is a part of boxer-sunrise2 project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

#|
  Author: Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise2"
  :version "0.1.0"
  :author "Steven Githens"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "boxer-sunrise2")
                 (:file "macros")
                 (:file "boxdef")
                 (:file "storage")
                 (:file "vrtdef")
                 ;; This is the beginning of the redisplay module from original
                 ;; boxer, when I get a chance to split this up in to modules.
                 (:file "disdcl")
                 ;; Beginning of `DRAW` module
                 ;;(:file "opengl-utils")
                 (:file "draw-low-stubs")
                 (:file "draw-high-common")
                 (:file "draw-high-hardware-clip")
                 ;; Beginning of `DISPLAYDEFS` module
                 ;; "Some more Definitions for Drawing things"
                 (:file "disdef")
                 ;; Beginning of `RELATIONSHIPS` module
                 ;; "The basic underlying structure of the Editor is defined here"
                 (:file "infsup")
                 ;; Beginning of `evalvars` module
                 ;; "Definitions necessary for describing evaluator state variables"
                 (:file "varmacs")
                 (:file "vars")
                )))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "boxer-sunrise2-test"))))
