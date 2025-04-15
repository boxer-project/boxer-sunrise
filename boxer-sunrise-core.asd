#|
  This file is a part of boxer-sunrise project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

#|
  Author: Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise-core"
  :version "3.4.23"
  :author "Steven Githens"
  :license ""
  :depends-on ("uiop"
               #-shim-3d-math "3d-matrices")
  :components ((:module "src"
                :components
                ((:file "boxer-sunrise")
                 (:file "package")
                 (:file "definitions/boxlog" :if-feature :emscripten)
                 #+shim-3d-math
                 (:file "definitions/3d-math")
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
