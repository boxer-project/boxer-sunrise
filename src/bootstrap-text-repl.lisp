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
(ql:quickload :cl-fad)
(defvar *project-dir* (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(setf asdf:*central-registry*
      (list* '*default-pathname-defaults*
              *project-dir*
              (cl-fad:merge-pathnames-as-directory *project-dir*  "src/boxwin/text-repl/")
      asdf:*central-registry*))

(setf *features* (cons :text-repl-engine *features*))
(ql:quickload :boxer-sunrise-text-repl)

(bw::start-text-repl-boxer *project-dir*)

