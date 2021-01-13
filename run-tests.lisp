(require "asdf")

;; Sometimes lispworks doesn't seem to load the initialization file when running from
;; the command line, in which case this could be uncommented.
;; (load "~/quicklisp/setup.lisp")

(ql:quickload "prove-asdf")
(ql:quickload "cl-ppcre")
(ql:quickload "prove")

;; This turns off the terminal color sequences and simplifies the characters in the
;; output so they display well in the lispworks listener.
(setf prove:*enable-colors* nil)
(setf prove::*default-reporter* :tap)

(setf asdf:*central-registry*
            (list* '*default-pathname-defaults*
                    (uiop:getcwd)
                asdf:*central-registry*))

(asdf:test-system :boxer-sunrise2 :reporter :list)
