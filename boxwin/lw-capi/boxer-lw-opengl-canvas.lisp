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
;;;;                                               +-Data--+
;;;;                      This file is part of the | BOXER | system
;;;;                                               +-------+
;;;;
;;;;         Lispworks CAPI customizations of boxer-canvas
;;;;
(in-package :boxer-window)

(defclass boxer-lw-opengl-canvas (opengl::opengl-pane boxer::boxer-canvas)
 ;; For some reason, I have to redefine configuration, context, and reader-state from capi.lisp in the
 ;; opengl examples for things to work on win32. On macOS they inherit fine.
 ((configuration :initform *default-opengl-pane-configuration*
                  :initarg :configuration
                  :reader configuration)
   (context :initform nil :initarg :context :accessor context)
   (render-state :initform nil :accessor opengl-pane-render-state)))

(defmethod boxer::viewport-width ((self boxer-lw-opengl-canvas))
  "Give the width of the port or widget container the Boxer documents canvas. Calculation will be different
   depending on the GL and Widget toolkit used. Needed for scrollbars, panning, and other types of interactions."
  (capi:simple-pane-visible-width self))

(defmethod boxer::viewport-height ((self boxer-lw-opengl-canvas))
  "See viewport-width for description."
  (capi:simple-pane-visible-height self))
