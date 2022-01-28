(in-package :boxer-sunrise2-test)

(plan nil)

;;; Tests for editor-abort-char?
;;; At the moment we have Ctrl-g and Ctrl-. set to the abort chars,
;;; those this is configurable via *editor-abort-chars*

(is (boxer::editor-abort-char? #\k) nil)

(is (boxer::editor-abort-char? #\c) nil)

(is (boxer::editor-abort-char? #\c 1) nil)

(is (boxer::editor-abort-char? #\c 2) nil)

(is (boxer::editor-abort-char? #\g 2) t)

(is (boxer::editor-abort-char? #\. 2) t)


(finalize)
