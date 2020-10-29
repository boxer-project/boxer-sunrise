#|
    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

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
(require "asdf")
(ql:quickload :drakma)

;; TODO fix this to preserve the windows logical drive
(defvar *boxer-project-dir* (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(setf asdf:*central-registry*
               (list* '*default-pathname-defaults*
                      *boxer-project-dir*
                      #+win32 #P"Z:/code/boxer-sunrise2/"
                asdf:*central-registry*))

(load (example-file "opengl/examples/load"))
(setf *features* (cons :opengl *features*))
(asdf:load-system :boxer-sunrise2)
(setf boxer-window::*capogi-font-directory* (merge-pathnames "data/boxersunrise.app/Contents/Resources/Fonts/" *boxer-project-dir*))

#+win32(setf boxer-window::*capogi-font-directory* #P"Z:/code/boxer-sunrise2/data/boxersunrise.app/Contents/Resources/Fonts/")

;; (load (merge-pathnames "run-tests.lisp" *boxer-project-dir*))
(boxer-window::window-system-specific-make-boxer)
(boxer-window::window-system-specific-start-boxer)
(boxer-window::font-size-menu-action 3 nil)
