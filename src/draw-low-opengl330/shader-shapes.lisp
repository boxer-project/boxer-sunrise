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
(in-package :boxer)

;;;
;;; Prototyping / exploratory debug canvas similar to the one in The Book of Shaders
;;;

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

    (make-instance 'boxgl-shader-program :program canvas-program :vao canvas-vao :buffer canvas-buffer)))

;;;
;;; Circles on the Fragment Shader
;;;

(defun setup-circle-program (&key (vertex-shader "boxgl-circle.vs") (fragment-shader "boxgl-circle.fs"))
  "
  Sets up a program used to draw circles with two bounding rectangles, where the circle pixels are calculated
  in the fragment shader.

  Circles on the shader taking 10 floats, each vertex stride containing:
  vec3: x y z
  vec4: r g b a
  vec3: cx cy radius
  "
  (let* ((canvas-program (gl:create-program))
         (canvas-vao     (gl:gen-vertex-array))
         (canvas-buffer  (gl:gen-buffer)))
    (gl:attach-shader canvas-program (create-shader vertex-shader :vertex-shader))
    (gl:attach-shader canvas-program (create-shader fragment-shader :fragment-shader))
    (gl:link-program canvas-program)
    (log:debug "~%circle-program infolog: ~A" (gl:get-program-info-log canvas-program))

    (gl:bind-vertex-array canvas-vao)
    (gl:bind-buffer :array-buffer canvas-buffer)

    (gl:enable-vertex-attrib-array 0)
    ;; x, y, z
    (gl:vertex-attrib-pointer 0 3 :float :false (* 10 *cffi-float-size*) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 1)
    ;; r, g, b, a
    (gl:vertex-attrib-pointer 1 4 :float :false (* 10 *cffi-float-size*) (* 3 *cffi-float-size*))
    ;; cx, cy, radius
    (gl:enable-vertex-attrib-array 2)
    (gl:vertex-attrib-pointer 2 3 :float :false (* 10 *cffi-float-size*) (* 7 *cffi-float-size*))

    (make-instance 'boxgl-shader-program :program canvas-program :vao canvas-vao :buffer canvas-buffer)))

(defun gl-add-shader-circle (device cx cy radius filled? &key (rgb (boxgl-device-pen-color device)))
  (enable-gl-shader-program device (circle-shader device))

  (let* ((r  (coerce radius 'single-float))
         (x0 (coerce (- cx r) 'single-float)) (y0 (coerce (+ cy r) 'single-float))
         (x1 (coerce (+ cx r) 'single-float)) (y1 (coerce (+ cy r) 'single-float))
         (x2 (coerce (- cx r) 'single-float)) (y2 (coerce (- cy r) 'single-float))
         (x3 (coerce (+ cx r) 'single-float)) (y3 (coerce (- cy r) 'single-float))
         (cxf (coerce cx 'single-float))
         (cyf (coerce cy 'single-float))
         ;;// x y z radius r g b a
         (vertices `#(,x0 ,y0 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ,cxf ,cyf ,r   ; point 0
                      ,x1 ,y1 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ,cxf ,cyf ,r   ; point 1
                      ,x2 ,y2 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ,cxf ,cyf ,r   ; point 2

                      ,x1 ,y1 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ,cxf ,cyf ,r   ; point 1
                      ,x2 ,y2 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ,cxf ,cyf ,r   ; point 2
                      ,x3 ,y3 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ,cxf ,cyf ,r)) ; point 3
          (arr (gl:alloc-gl-array :float (length vertices))))
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))

  (gl:draw-arrays :triangles 0 6)
  (unenable-shader-programs device))
