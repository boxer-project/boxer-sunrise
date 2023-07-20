#|
  This file is a part of boxer-sunrise project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Steven Githens"
  :license ""
  :depends-on (:boxer-sunrise
               :prove
               :md5)
  :components ((:module "tests"
                :components
                ((:test-file "boxer-sunrise")
                 (:test-file "boxdef-tests")
                 (:test-file "stacks-tests")
                 (:test-file "boxapp-data-tests")
                 (:test-file "comdef-tests")
                 (:test-file "click-handlers-tests")
                 (:test-file "alternate-names-tests")
                ;;  (:test-file "chunker-tests")
                (:test-file "vrtdef-tests")
                (:test-file "loader-tests")
                (:test-file "keydef-high-tests")
                (:test-file "gdispl-tests")
                (:test-file "dumper-tests")
                (:test-file "formats-tests")
                ; (:test-file "eval-eval-tests")
                (:test-file "boxer-styles-tests")
                (:test-file "freetype-fonts-tests")
                (:test-file "disdcl-tests")
                (:test-file "draw-low-opengl-tests")
                (:test-file "wrap-line-tests")
                 )))
  :description "Test system for boxer-sunrise"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
