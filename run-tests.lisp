(require "asdf")
(require "uiop")

;; Sometimes lispworks doesn't seem to load the initialization file when running from
;; the command line, in which case this could be uncommented.
;; (load "~/quicklisp/setup.lisp")

(ql:quickload "prove-asdf")
(ql:quickload "cl-ppcre")
(ql:quickload "prove")

(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :zpng)
(ql:quickload :qbase64)
(ql:quickload :html-entities)

(ql:quickload :cffi)
(ql:quickload :cl-freetype2)

;; This turns off the terminal color sequences and simplifies the characters in the
;; output so they display well in the lispworks listener.
(setf prove:*enable-colors* nil)
(setf prove::*default-reporter* :tap)

;; TODO fix this to preserve the windows logical drive
(defvar *boxer-project-dir* (make-pathname :directory (butlast (pathname-directory *load-truename*))))


(setf asdf:*central-registry*
            (list* '*default-pathname-defaults*
                    *boxer-project-dir*
                    #P"/Users/sgithens/code/boxer-sunrise2/"
                asdf:*central-registry*))

#+lispworks (load (example-file "opengl/examples/load"))
(setf *features* (cons :opengl *features*))
(setf *features* (cons :freetype-fonts *features*))


(asdf:test-system :boxer-sunrise2 :reporter :list)
