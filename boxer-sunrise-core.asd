#|
  This file is a part of boxer-sunrise project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

#|
  Author: Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise-core"
  :version "3.4.14"
  :author "Steven Githens"
  :license ""
  :depends-on ("uiop")
  :components ((:module "src"
                :components
                ((:file "boxer-sunrise")
                 (:file "package")
                 ;; DEFINITIONS
                 (:file "definitions/color")
                 (:file "definitions/macros")
                 (:file "definitions/storage")
                 (:file "definitions/boxdef")
                 (:file "definitions/vrtdef")
                 (:file "definitions/fonts")
                 (:file "definitions/disdcl")
                 (:file "definitions/canvas"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "boxer-sunrise-core-test"))))
