#|
  This file is a part of boxer-sunrise project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

#|
  Author: Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise-definitions"
  :version "3.4.26"
  :author "Steven Githens"
  :license ""
  :depends-on ("uiop"
               (:feature (:not :shim-3d-math) :3d-matrices))
  :components ((:file "package")
               (:file "boxlog" :if-feature :embedded-boxer)
               (:file "3d-math" :if-feature :shim-3d-math)
               ;; DEFINITIONS
               (:file "color")
               (:file "macros")
               (:file "storage")
               (:file "boxdef" :depends-on ("package" "macros"))
               (:file "vrtdef")
               (:file "fonts")
               (:file "disdcl" :depends-on ("storage" "3d-math"))
               (:file "canvas")
               (:file "pixmap"))
  :description "")
