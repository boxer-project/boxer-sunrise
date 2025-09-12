#|
  This file is a part of boxer-sunrise project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

#|
  Author: Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise-definitions"
  :version "3.4.23"
  :author "Steven Githens"
  :license ""
  :depends-on ("uiop"
               #-shim-3d-math "3d-matrices")
  :components ((:file "package")
               (:file "boxlog" :if-feature :emscripten)
               #+shim-3d-math
               (:file "3d-math")
               ;; DEFINITIONS
               (:file "color")
               (:file "macros")
               (:file "storage")
               (:file "boxdef")
               (:file "vrtdef")
               (:file "fonts")
               (:file "disdcl")
               (:file "canvas")
               #-embedded-boxer (:file "pixmap"))
  :description "")
