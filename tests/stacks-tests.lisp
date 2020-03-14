(in-package :boxer-sunrise2-test)

(plan nil)

(is (boxer-eval::make-stack-frame-cache 5)
    #(0 #(NIL NIL NIL NIL NIL)) :test #'equalp)

(is (boxer-eval::stack-frame-cache-contents #(0 #(NIL NIL NIL NIL NIL)))
    #(NIL NIL NIL NIL NIL) :test #'equalp)

(is (boxer-eval::make-stack-frame "TheStack" 5)
    '("TheStack" nil nil nil nil) :test #'equalp)

(is (boxer-eval::make-n-stack-frames 4 "AwesomeStack" 5)
    #(0 #(("AwesomeStack" NIL NIL NIL NIL) ("AwesomeStack" NIL NIL NIL NIL) ("AwesomeStack" NIL NIL NIL NIL) ("AwesomeStack" NIL NIL NIL NIL)))
    :test #'equalp)

(finalize)
