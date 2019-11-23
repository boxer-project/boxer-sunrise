(defpackage boxer-sunrise2-test
  (:use :cl
        :boxer-sunrise2
        :prove))
(in-package :boxer-sunrise2-test)

;; NOTE: To run this test file, execute `(asdf:test-system :boxer-sunrise2)' in your Lisp.

(plan nil)

(print "Sgithens from the test file")
(print boxer::*constant-folding-macros*)

;; blah blah blah.

(finalize)
