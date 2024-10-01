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
;;; Circles on the Fragment Shader
;;;

(defun setup-circle-program (&key (vertex-shader "boxgl-circle.vs") (fragment-shader "boxgl-circle.fs"))
  "
  Sets up a program used to draw circles with two bounding rectangles, where the circle pixels are calculated
  in the fragment shader.

  Circles on the shader taking 11 floats, each vertex stride containing:
  vec3: x y z
  vec4: r g b a
  vec3: cx cy radius pen-width
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
    (gl:vertex-attrib-pointer 0 3 :float :false (* 11 *cffi-float-size*) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 1)
    ;; r, g, b, a
    (gl:vertex-attrib-pointer 1 4 :float :false (* 11 *cffi-float-size*) (* 3 *cffi-float-size*))
    ;; cx, cy, radius, pen-width
    ;; If pen-width is greater than 0 it will be rendered as a hollow circle with that width.
    (gl:enable-vertex-attrib-array 2)
    (gl:vertex-attrib-pointer 2 4 :float :false (* 11 *cffi-float-size*) (* 7 *cffi-float-size*))

    (make-instance 'boxgl-shader-program :program canvas-program :vao canvas-vao :buffer canvas-buffer)))

(defun gl-add-shader-circle (device cx cy radius filled? &key (rgb (boxgl-device-pen-color device))
                                                              (pen-size (boxgl-device-pen-size bw::*boxgl-device*)))
  (enable-gl-shader-program device (circle-shader device))

  (let* ((width (if filled? 0 pen-size))
         (x0 (- cx radius)) (y0 (+ cy radius))
         (x1 (+ cx radius)) (y1 (+ cy radius))
         (x2 (- cx radius)) (y2 (- cy radius))
         (x3 (+ cx radius)) (y3 (- cy radius))
         (cxf cx)
         (cyf cy)
         ;;// x y z radius r g b a
         (vertices (float-vector x0 y0 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width   ; point 0
                                 x1 y1 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width   ; point 1
                                 x2 y2 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width   ; point 2

                                 x1 y1 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width   ; point 1
                                 x2 y2 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width   ; point 2
                                 x3 y3 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width)) ; point 3
          (arr (gl:alloc-gl-array :float (length vertices))))
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))

  (gl:draw-arrays :triangles 0 6)
  (unenable-shader-programs device))

;;;
;;; Arcs on the Fragment Shader
;;;

(defun setup-arc-program (&key (vertex-shader "boxgl-arcs.vs") (fragment-shader "boxgl-arcs.fs"))
  "
  Sets up a program used to draw arcs with two bounding rectangles, where the arc pixels are calculated
  in the fragment shader.

  Arcs on the shader taking 13 floats, each vertex stride containing:
  vec3: x y z
  vec4: r g b a
  vec3: cx cy radius pen-width
  vec2: start-angle end-angle
  "
  (let* ((canvas-program (gl:create-program))
         (canvas-vao     (gl:gen-vertex-array))
         (canvas-buffer  (gl:gen-buffer)))
    (gl:attach-shader canvas-program (create-shader vertex-shader :vertex-shader))
    (gl:attach-shader canvas-program (create-shader fragment-shader :fragment-shader))
    (gl:link-program canvas-program)
    (log:debug "~%arc-program infolog: ~A" (gl:get-program-info-log canvas-program))

    (gl:bind-vertex-array canvas-vao)
    (gl:bind-buffer :array-buffer canvas-buffer)

    (gl:enable-vertex-attrib-array 0)
    ;; x, y, z
    (gl:vertex-attrib-pointer 0 3 :float :false (* 13 *cffi-float-size*) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 1)
    ;; r, g, b, a
    (gl:vertex-attrib-pointer 1 4 :float :false (* 13 *cffi-float-size*) (* 3 *cffi-float-size*))
    ;; cx, cy, radius, pen-width
    ;; If pen-width is greater than 0 it will be rendered as a hollow circle with that width.
    (gl:enable-vertex-attrib-array 2)
    (gl:vertex-attrib-pointer 2 4 :float :false (* 13 *cffi-float-size*) (* 7 *cffi-float-size*))
    ;; start-angle, end-angle pen-width
    (gl:enable-vertex-attrib-array 3)
    (gl:vertex-attrib-pointer 3 2 :float :false (* 13 *cffi-float-size*) (* 11 *cffi-float-size*))

    (make-instance 'boxgl-shader-program :program canvas-program :vao canvas-vao :buffer canvas-buffer)))

(defun gl-add-shader-arc (device cx cy radius start-angle arc-angle filled? &key (rgb (boxgl-device-pen-color device))
                                                              (pen-size (boxgl-device-pen-size bw::*boxgl-device*)))
  (enable-gl-shader-program device (arc-shader device))
  (let* ((width (if filled? 0 pen-size))
         (end-angle (mod (+ start-angle arc-angle) (* 2 pi)))
         (x0 (- cx radius)) (y0 (+ cy radius))
         (x1 (+ cx radius)) (y1 (+ cy radius))
         (x2 (- cx radius)) (y2 (- cy radius))
         (x3 (+ cx radius)) (y3 (- cy radius))
         (cxf cx)
         (cyf cy)
         ;;// x y z radius r g b a
         (vertices (float-vector x0 y0 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width start-angle end-angle   ; point 0
                                 x1 y1 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width start-angle end-angle   ; point 1
                                 x2 y2 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width start-angle end-angle   ; point 2

                                 x1 y1 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width start-angle end-angle   ; point 1
                                 x2 y2 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width start-angle end-angle   ; point 2
                                 x3 y3 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf radius width start-angle end-angle)) ; point 3
          (arr (gl:alloc-gl-array :float (length vertices))))
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))

  (gl:draw-arrays :triangles 0 6)
  (unenable-shader-programs device))

;;;
;;; Ellipses on the Fragment Shader
;;;

(defun setup-ellipse-program (&key (vertex-shader "boxgl-ellipse.vs") (fragment-shader "boxgl-ellipse.fs"))
  "
  Sets up a program used to draw ellipses with two bounding rectangles, where the ellipse pixels are calculated
  in the fragment shader.

  Ellipses on the shader taking 12 floats, each vertex stride containing:
  vec3: x y z
  vec4: r g b a
  vec3: cx cy pen-width
  vec2: width height
  "
  (let* ((canvas-program (gl:create-program))
         (canvas-vao     (gl:gen-vertex-array))
         (canvas-buffer  (gl:gen-buffer)))
    (gl:attach-shader canvas-program (create-shader vertex-shader :vertex-shader))
    (gl:attach-shader canvas-program (create-shader fragment-shader :fragment-shader))
    (gl:link-program canvas-program)
    (log:info "~%ellipse-program infolog: ~A" (gl:get-program-info-log canvas-program))

    (gl:bind-vertex-array canvas-vao)
    (gl:bind-buffer :array-buffer canvas-buffer)

    (gl:enable-vertex-attrib-array 0)
    ;; x, y, z
    (gl:vertex-attrib-pointer 0 3 :float :false (* 12 *cffi-float-size*) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 1)
    ;; r, g, b, a
    (gl:vertex-attrib-pointer 1 4 :float :false (* 12 *cffi-float-size*) (* 3 *cffi-float-size*))
    ;; cx, cy, pen-width
    ;; If pen-width is greater than 0 it will be rendered as a hollow ellipse with that width.
    (gl:enable-vertex-attrib-array 2)
    (gl:vertex-attrib-pointer 2 3 :float :false (* 12 *cffi-float-size*) (* 7 *cffi-float-size*))
    ;; width height
    (gl:enable-vertex-attrib-array 3)
    (gl:vertex-attrib-pointer 3 2 :float :false (* 12 *cffi-float-size*) (* 10 *cffi-float-size*))

    (make-instance 'boxgl-shader-program :program canvas-program :vao canvas-vao :buffer canvas-buffer)))

(defun gl-add-shader-ellipse (device cx cy width height filled? &key (rgb (boxgl-device-pen-color device))
                                                                     (pen-size (boxgl-device-pen-size bw::*boxgl-device*)))
  (enable-gl-shader-program device (ellipse-shader device))

  (let* ((pen-width (if filled? 0 pen-size))
         ; I believe this needs to be square in case it's rotated. TODO sgithens 2023-08-14
         ;  (x0 (- cx (/ width 2))) (y0 (+ cy (/ height 2)))
         ;  (x1 (+ cx (/ width 2))) (y1 (+ cy (/ height 2)))
         ;  (x2 (- cx (/ width 2))) (y2 (- cy (/ height 2)))
         ;  (x3 (+ cx (/ width 2))) (y3 (- cy (/ height 2)))
         (maxdim (max width height))
         (x0 (- cx (/ maxdim 2))) (y0 (+ cy (/ maxdim 2)))
         (x1 (+ cx (/ maxdim 2))) (y1 (+ cy (/ maxdim 2)))
         (x2 (- cx (/ maxdim 2))) (y2 (- cy (/ maxdim 2)))
         (x3 (+ cx (/ maxdim 2))) (y3 (- cy (/ maxdim 2)))

         (cxf cx)
         (cyf cy)
         ;;// x y z r g b a
         (vertices (float-vector x0 y0 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf pen-width width height    ; point 0
                                 x1 y1 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf pen-width width height    ; point 1
                                 x2 y2 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf pen-width width height    ; point 2

                                 x1 y1 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf pen-width width height    ; point 1
                                 x2 y2 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf pen-width width height    ; point 2
                                 x3 y3 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) cxf cyf pen-width width height )) ; point 3
          (arr (gl:alloc-gl-array :float (length vertices))))
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))

  (gl:draw-arrays :triangles 0 6)
  (unenable-shader-programs device))
