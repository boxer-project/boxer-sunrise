;;;;
;;;;      Boxer
;;;;      Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;      Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;      used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;      Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;      https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                           +-Data--+
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;  Work In Progress! GLFW Engine Bootstrap
;;;;
(require "asdf")
(require "uiop")

(ql:quickload :cl-fad)
(ql:quickload :log4cl)
(ql:quickload :cffi)

(defvar *project-dir* (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(setf asdf:*central-registry*
      (list* '*default-pathname-defaults*
              *project-dir*
      asdf:*central-registry*))

(sb-int:set-floating-point-modes :traps nil)

(ql:quickload :cl-freetype2)
(setf *features* (cons :opengl *features*))
(setf *features* (cons :freetype-fonts *features*))
(setf *features* (cons :glfw-engine *features*))
(asdf:load-system :boxer-sunrise)

(setf boxer::*capogi-font-directory* (merge-pathnames "data/boxersunrise.app/Contents/Resources/Fonts/" *project-dir*))
(setf boxer::*resources-dir* (merge-pathnames "data/boxersunrise.app/Contents/Resources/" *project-dir*))
(setf boxer::*shaders-dir* (merge-pathnames "src/draw-low-opengl330/shaders/" *project-dir*))

(boxer-window::window-system-specific-make-boxer)
(boxer-window::window-system-specific-start-boxer)
