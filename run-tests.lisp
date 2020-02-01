(require "uiop")
(ql:quickload "prove-asdf")
(ql:quickload "cl-ppcre")
(ql:quickload "prove")

(setf asdf:*central-registry*
            (list* '*default-pathname-defaults*
                    (uiop:getcwd)
                asdf:*central-registry*))

(asdf:test-system :boxer-sunrise2)
