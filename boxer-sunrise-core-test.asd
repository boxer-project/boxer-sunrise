#|
  This file is a part of boxer-sunrise project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise-core-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Steven Githens"
  :license ""
  :depends-on ("boxer-sunrise-core"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "boxer-sunrise")
                (:test-file "boxdef-tests")
                ;  (:test-file "stacks-tests")
                ;  (:test-file "boxapp-data-tests")
                ;  (:test-file "comdef-tests")
                ;  (:test-file "click-handlers-tests")
                ;  (:test-file "alternate-names-tests")
                ;;  (:test-file "chunker-tests")
 ;               (:test-file "vrtdef-tests")
                ; (:test-file "loader-tests")
                ; (:test-file "keydef-high-tests")
                ; (:test-file "gdispl-tests")
                 )))
  :description "Test system for boxer-sunrise-core"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
