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
;;;;

(in-package :boxer-window)

(defparameter *boxer-pane-initialized* nil)

(defun boxer-pane-display-callback (canvas x y wid hei)
  ;; canvas ignore

  (unless *boxer-pane-initialized*
    ;; modernGL inits
    (opengl:rendering-on (*boxer-pane*)
      (boxer-opengl::opengl-enables)

      (setf bw::*boxgl-device* (boxer-opengl::make-boxgl-device wid hei))

      (gl-reshape wid hei)
      (update-gpu-matrices)
      (set-pen-color box::*foreground-color*))

    (boxer::initialize-fonts)

    (let ((boxer::%private-graphics-list nil))
      ;; needed by shape-box updater in the redisplay inits but not set until
      ;; (boxer-eval::setup-evaluator) farther down
      (run-redisplay-inits))
      (setf *boxer-pane-initialized* t)

    (opengl:rendering-on (*boxer-pane*)
      (log:debug "~%max-texture-size: ~A"  (gl:get-integer :max-texture-size))
      (init-freetype-fonts)
      (log:debug "~%Just created texture atlas"); boxer::*freetype-glyph-atlas*)
      )
    ))
