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
                 (:file "vrtdef"))))

  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "boxer-sunrise2-test"))))
