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
;;;;      Simpler set of drawing routines using less vertices and the color from a global uniform.
(in-package :boxer)

(defun gl-draw-box-border (device)
  "Draw the box border lines accumulated in gl-add-box-border-line."
  (enable-gl-shader-program device (simple-shader device))
  (setf *max-border-verts* (max (border-vert-count *cur-border-cache*) *max-border-verts*))
  (gl:buffer-sub-data :array-buffer (arr *cur-border-cache*) :size (* *cffi-float-size* (border-vert-count *cur-border-cache*)))
  (gl:draw-arrays :triangles 0 (/ (border-vert-count *cur-border-cache*) 2)))

(defun setup-simple-shader (&key (vertex-shader "boxgl-simple.vs") (fragment-shader "boxgl-lines.fs"))
  (let* ((simple-program (gl:create-program))
         (simple-vao     (gl:gen-vertex-array))
         (simple-buffer  (gl:gen-buffer)))
    (gl:attach-shader simple-program (create-shader vertex-shader :vertex-shader))
    (gl:attach-shader simple-program (create-shader fragment-shader :fragment-shader))
    (gl:link-program simple-program)
    (log:debug "~%simple-program infolog: ~A" (gl:get-program-info-log simple-program))

    (gl:bind-vertex-array simple-vao)
    (gl:bind-buffer :array-buffer simple-buffer)

    ;; Really we just need enough to draw a box border for now.
    (%gl:buffer-data :array-buffer (* 3000 2 *cffi-float-size*) (cffi:null-pointer) :dynamic-draw)

    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 2 :float nil (* 2 *cffi-float-size*) (cffi:null-pointer))

    (%gl:uniform-block-binding simple-program (gl:get-uniform-block-index simple-program "Matrices") 0)

    (make-instance 'boxgl-shader-program
                    :program simple-program
                    :vao     simple-vao
                    :buffer  simple-buffer)))
