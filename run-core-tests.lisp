(require "asdf")

;; Sometimes lispworks doesn't seem to load the initialization file when running from
;; the command line, in which case this could be uncommented.
;; (load "~/quicklisp/setup.lisp")

(ql:quickload "prove-asdf")
(ql:quickload "cl-ppcre")
(ql:quickload "prove")

(ql:quickload :cl-fad)
(ql:quickload :log4cl)
(ql:quickload :cffi)

(defvar *boxer-project-dir* (make-pathname :directory (pathname-directory *load-truename*)))

(pushnew
  (cl-fad:merge-pathnames-as-directory *boxer-project-dir* "data/boxersunrise.app/Contents/Frameworks/")
  cffi:*foreign-library-directories* :test #'equal)

(setf asdf:*central-registry*
            (list* '*default-pathname-defaults*
                    *boxer-project-dir*
                    asdf:*central-registry*))

;; This turns off the terminal color sequences and simplifies the characters in the
;; output so they display well in the lispworks listener.
(setf prove:*enable-colors* nil)
(setf prove::*default-reporter* :tap)

#+lispworks (load (example-file "opengl/examples/load"))

(asdf:test-system :boxer-sunrise-core :reporter :list)
