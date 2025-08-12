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

(pushnew *project-dir* ql:*local-project-directories* )
(ql:register-local-projects)

#+ecl
(defmacro without-fpe-traps (&body body)
  `(let ((bits (si:trap-fpe 'cl:last t)))
     (unwind-protect
          (progn
            (si:trap-fpe t nil)
            ,@body)
       (si:trap-fpe bits t))))

(setf *features* (cons :glfw-engine *features*))
(ql:quickload :boxer-sunrise-glfw)

#+ecl
(without-fpe-traps
  (bw::start-glfw-boxer *project-dir*))

#-ecl
(bw::start-glfw-boxer *project-dir*)

