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

;; sunrise-109 This should likely go in .lispworks, but I don't want to depend
;; on folks having that set up properly.
;; https://www.lispworks.com/documentation/lw70/LW/html/lw-684.htm
(lw:set-default-character-element-type 'character)

(require "asdf")
(require "uiop")

;; By default (quicklisp-quickstart:install) puts it at something like "C:/Users/<YOU>/quicklisp/setup.lisp".
;; This will be in most lisp installations init scripts, but startup scripts aren't available in LW Personal
;; #+win32 (load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))


(ql:quickload :cl-fad)
(ql:quickload :log4cl)
(ql:quickload :cffi)

(log:config :info)

(defvar *boxer-project-dir*
  (make-pathname :host (pathname-host *load-truename*) ; retain drive e.g. Z:/code/boxer-sunrise/
                 :directory (butlast (pathname-directory *load-truename*))))

(pushnew
  (cl-fad:merge-pathnames-as-directory *boxer-project-dir* "data/boxersunrise.app/Contents/Frameworks/")
  cffi:*foreign-library-directories* :test #'equal)

#+win32 (pushnew *boxer-project-dir*
        cffi:*foreign-library-directories* :test #'equal)

(setf asdf:*central-registry*
      (list* '*default-pathname-defaults*
        ;; This is relevant for compiled releases which include a "Plugins" subdir.
        ;; In development, cl-freetype2 goes into quicklisp/local-projects/; while this based on LW install
        ;; path may give something non-existent e.g. #P"C:/Program Files/PlugIns/cl-freetype2/".
        (make-pathname :host (pathname-host (lw:lisp-image-name))
                       :directory (append (butlast (pathname-directory (lw:lisp-image-name)))
                                          '("PlugIns"
                                          #+(and lispworks8 arm64) "lw8-macos-arm"
                                          "cl-freetype2")))
       asdf:*central-registry*))
    (log:debug "ASDF Registry: ~A" asdf:*central-registry*)

(ql:quickload :cl-freetype2)

(pushnew *boxer-project-dir* ql:*local-project-directories* )
(ql:register-local-projects)


#+(and lispworks x64) (load (cl-fad:merge-pathnames-as-file *boxer-project-dir* "src/opengl-lw-8/examples/load.lisp"))


;; debugging on Apple Silicon
; #+ARM64 (progn
;   ; (load "/Users/sgithens/code/lispworks-8.0-examples/opengl/host")
;   (load "/Applications/LispWorks 8.0 (64-bit)/Library/lib/8-0-0-0/examples/opengl/host")
;   (load "OPENGL:EXAMPLES;load"))

(ql:quickload :boxer-sunrise-capi)
(setf boxer::*capogi-font-directory* (merge-pathnames "data/boxersunrise.app/Contents/Resources/Fonts/" *boxer-project-dir*))
(setf boxer::*resources-dir* (merge-pathnames "data/boxersunrise.app/Contents/Resources/" *boxer-project-dir*))
(setf boxer::*shaders-dir* (merge-pathnames "src/draw-low-opengl330/shaders/" *boxer-project-dir*))


(boxer-window::window-system-specific-make-boxer)

(boxer-window::window-system-specific-start-boxer)
