(require "asdf")

;; Sometimes lispworks doesn't seem to load the initialization file when running from
;; the command line, in which case this could be uncommented.
;; (load "~/quicklisp/setup.lisp")

(ql:quickload "prove-asdf")
(ql:quickload "cl-ppcre")
(ql:quickload "prove")

(setf asdf:*central-registry*
            (list* '*default-pathname-defaults*
                    (uiop:getcwd)
                asdf:*central-registry*))

(asdf:test-system :boxer-sunrise2)
