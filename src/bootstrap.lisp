#|
    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


 This file contains the bootstrap script for running the tests and starting up the UI. Can be loaded
 directory in lispworks.

      (load #P"./src/bootstrap.lisp")

|#
; (setf *features* (cons :cl-opengl-no-check-error *features*))

(require "asdf")
(require "uiop")

#+win32 (load #P"Z:/quicklisp/setup.lisp")




(ql:quickload :cl-fad)
(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :zpng)
(ql:quickload :qbase64)
(ql:quickload :html-entities)

(ql:quickload :alexandria)
(ql:quickload :trivial-garbage)
(ql:quickload :log4cl)

(ql:quickload :cffi)
(ql:quickload :zip)
(ql:quickload :quri)

(ql:quickload :cl-opengl)
(ql:quickload :pngload)
(ql:quickload :3d-vectors)
(ql:quickload :3d-matrices)
(ql:quickload :cl-glu)
(ql:quickload :iterate)
(ql:quickload :for)

(log:config :info)

(defvar *boxer-project-dir* (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew
  (cl-fad:merge-pathnames-as-directory *boxer-project-dir* "data/boxersunrise.app/Contents/Frameworks/")
  cffi:*foreign-library-directories* :test #'equal)

#+win32 (pushnew #P"Z:/code/boxer-sunrise/"
        cffi:*foreign-library-directories* :test #'equal)

(setf asdf:*central-registry*
            (list* '*default-pathname-defaults*
                  (make-pathname :directory
                    (append (butlast (pathname-directory (lw:lisp-image-name)))
                    '("PlugIns"
                      #+(and lispworks8 arm64) "lw8-macos-arm"
                      "cl-freetype2")))
            asdf:*central-registry*))
    (log:debug "ASDF Registry: ~A" asdf:*central-registry*)

(ql:quickload :cl-freetype2)

(setf asdf:*central-registry*
               (list* '*default-pathname-defaults*
                      *boxer-project-dir*
                      #+win32 #P"Z:/code/boxer-sunrise/"
                asdf:*central-registry*))

#+(and lispworks x64) (load (cl-fad:merge-pathnames-as-file *boxer-project-dir* "src/opengl-lw-8/examples/load.lisp"))

;; debugging on Apple Silicon
; #+ARM64 (progn
;   ; (load "/Users/sgithens/code/lispworks-8.0-examples/opengl/host")
;   (load "/Applications/LispWorks 8.0 (64-bit)/Library/lib/8-0-0-0/examples/opengl/host")
;   (load "OPENGL:EXAMPLES;load"))

(setf *features* (cons :opengl *features*))
(setf *features* (cons :freetype-fonts *features*))
(asdf:load-system :boxer-sunrise)
(setf boxer::*capogi-font-directory* (merge-pathnames "data/boxersunrise.app/Contents/Resources/Fonts/" *boxer-project-dir*))
(setf boxer::*resources-dir* (merge-pathnames "data/boxersunrise.app/Contents/Resources/" *boxer-project-dir*))
(setf boxer::*shaders-dir* (merge-pathnames "src/draw-low-opengl330/shaders/" *boxer-project-dir*))

#+win32(setf boxer::*capogi-font-directory* #P"Z:/code/boxer-sunrise/data/boxersunrise.app/Contents/Resources/Fonts/")
#+win32(setf boxer::*resources-dir* #P"Z:/code/boxer-sunrise/data/boxersunrise.app/Contents/Resources/")



(boxer-window::window-system-specific-make-boxer)

(boxer-window::window-system-specific-start-boxer)
