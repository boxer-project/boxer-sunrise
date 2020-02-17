(defpackage boxer-sunrise2-test
  (:use :cl
        :boxer-sunrise2
        :prove))
(in-package :boxer-sunrise2-test)

;; NOTE: To run this test file, execute `(asdf:test-system :boxer-sunrise2)' in your Lisp.

(plan nil)

(print boxer::*constant-folding-macros*)

;;
;; boxdef.lisp tests
;;
;; Tests for basic box and row data structures
;;

;; Test a simple "WORLD" box with the statement `1 + 5`

(defvar *simple-arithmetic-box* (make-instance 'boxer::data-box))
(setf (boxer::name *simple-arithmetic-box*) "WORLD")

(defvar *simple-row* (make-instance 'boxer::row))
(setf (boxer::chas-array *simple-row*) #(#(#\1 #\  #\+ #\  #\5) 5 NIL NIL))
(setf (boxer::superior-box *simple-row*) *simple-arithmetic-box*)
(setf (boxer::first-inferior-row *simple-arithmetic-box*) *simple-row*)

(is (boxer::name *simple-arithmetic-box*) "WORLD")
(is (boxer::chas-array (boxer::first-inferior-row *simple-arithmetic-box*))
  #(#(#\1 #\  #\+ #\  #\5) 5 NIL NIL) :test #'equalp)

(is (boxer::superior-box (boxer::first-inferior-row *simple-arithmetic-box*)) *simple-arithmetic-box*)
(is (boxer::superior-box *simple-row*) *simple-arithmetic-box*)

;; TODO add tests for display type flags, multiple rows, and other fields

(finalize)
