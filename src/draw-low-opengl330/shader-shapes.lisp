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
;;;;                                        +-Data--+
;;;;               This file is part of the | BOXER | system
;;;;                                        +-------+
;;;;
;;;;        Shapes drawn primarily with math in fragment shaders.  Also, scaffolding to run the types of
;;;;        examples found in The Book of Shaders.
;;;;
;;;;        Currently just a testbed/prototype.
(in-package :boxer)

(defun gl-add-circle-skip (device cx cy radius filled? &key (rgb (boxgl-device-pen-color device)))
   (enable-gl-shader-program device (canvas-shader device))
  (let ((res (resolution)))
     (gl:uniformf (circle-xyrad-uni device)
                  (coerce cx 'single-float) (coerce cy 'single-float) (coerce radius 'single-float)))
  (gl:draw-arrays :triangles 0 6))

(defun gl-test-canvas (device)
  "Running the test fragment shader"
  (enable-gl-shader-program device (canvas-shader device))
  (gl:draw-arrays :triangles 0 6))

(defun setup-canvas-program (&key (vertex-shader "boxgl-canvas.vs") (fragment-shader "boxgl-canvas.fs"))
  "Program to test and use shaders compatible with glslCanvas and examples from The Book of Shaders."
  (let* ((canvas-program (gl:create-program))
         (canvas-vao     (gl:gen-vertex-array))
         (canvas-buffer  (gl:gen-buffer)))
    (gl:attach-shader canvas-program (create-shader vertex-shader :vertex-shader))
    (gl:attach-shader canvas-program (create-shader fragment-shader :fragment-shader))
    (gl:link-program canvas-program)
    (log:debug "~%canvas-program infolog: ~A" (gl:get-program-info-log canvas-program))

    (gl:bind-vertex-array canvas-vao)
    (gl:bind-buffer :array-buffer canvas-buffer)

    (let* ((vertices #(-1.0 -1.0 1.0 -1.0 -1.0 1.0 -1.0 1.0 1.0 -1.0 1.0 1.0))
           (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 2 :float nil 0 (cffi:null-pointer))

    (make-instance 'boxgl-shader-program
                         :program canvas-program
                         :vao     canvas-vao
                         :buffer  canvas-buffer)))
