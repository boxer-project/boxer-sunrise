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
;;;;         Routines for using stencil buffers in Boxer.
;;;;
(in-package :boxer-opengl)

(defun clear-stencil-buffer ()
  (gl:clear :stencil-buffer-bit))

(defun write-to-stencil (&key (clear-buffer t))
  "Anything drawn here renders to the stencil buffer. In Boxer we're largely using this to stamp
   a rectangle for the inner portion of a box where the inside content will be renderered. The
   general usage is:

   (write-to-stencil) ;; without arguments clears the previous stencil buffer
   (draw-rectangle etc etc)
   (render-inside-stencil)
   (draw-box-contents)
   (ignore-stencil)"
  (when clear-buffer
    (gl:clear-stencil 0)
    (gl:stencil-mask #xFF)
    (gl:clear :stencil-buffer-bit))
  (gl:stencil-func :always 1 #xFF)
  (gl:stencil-mask #xFF))

(defun render-inside-stencil ()
  (gl:stencil-func :equal 1 #xFF)
  (gl:stencil-mask #x00))

(defun ignore-stencil ()
  (gl:stencil-mask #x00)
  (gl:stencil-func :always 0 #xFF))
