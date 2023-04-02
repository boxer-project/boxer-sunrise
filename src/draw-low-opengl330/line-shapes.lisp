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
;;;;        Shapes drawn with lines including obviously, lines and rectangles, and then a version
;;;;        of circles and arcs which use a configurable number of points to define their fidelity.
;;;;
(in-package :boxer)

;;;
;;;  Buffering commands to build up meshes of vertices.
;;;

;;;
;;;  Stand-alone draw commands. One command, immediate glDrawArrays.
;;;

(defun gl-add-line (device x0 y0 x1 y1 &key (rgb (boxgl-device-pen-color device))
                                            (pen-size (boxgl-device-pen-size bw::*boxgl-device*)))
  "Testing line thickness with 2 triangles"
  (unless (and (equal (coerce x0 'single-float) (coerce x1 'single-float)) (equal (coerce y0 'single-float) (coerce y1 'single-float)))
      (cond ((line-stipple device)
          (enable-gl-shader-program device (dashed-lines-shader device))
          (let ((res (resolution)))
            (gl:uniformf (gl:get-uniform-location (shader-program (dashed-lines-shader device)) "u_resolution")
                          (coerce (aref res 0) 'single-float) (coerce (aref res 1) 'single-float))))
          (t
          (enable-gl-shader-program device (lines-shader device))
          (gl:uniformf (gl:get-uniform-location (shader-program (lines-shader device)) "uTime")
                          (coerce (- (get-universal-time) (start-time device)) 'single-float))))



    (let* ((corners (line-by-width-corners x0 y0 x1 y1 pen-size))
          (vertices `#(,(coerce (aref corners 0) 'single-float) ,(coerce (aref corners 1) 'single-float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 0
                        ,(coerce (aref corners 2) 'single-float) ,(coerce (aref corners 3) 'single-float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 1
                        ,(coerce (aref corners 4) 'single-float) ,(coerce (aref corners 5) 'single-float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 2

                        ,(coerce (aref corners 2) 'single-float) ,(coerce (aref corners 3) 'single-float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 1
                        ,(coerce (aref corners 4) 'single-float) ,(coerce (aref corners 5) 'single-float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 2
                        ,(coerce (aref corners 6) 'single-float) ,(coerce (aref corners 7) 'single-float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 3
                        ))
          (arr (gl:alloc-gl-array :float (length vertices))))
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))

    (gl:draw-arrays :triangles 0 6)
    ; (gl:draw-arrays :lines 0 2)
 ) )

(defun gl-add-circle (device cx cy radius filled? &key (rgb (boxgl-device-pen-color device)))
  (enable-gl-shader-program device (lines-shader device))

  (let* ((num-slices (bw::num-slices radius))
         (theta (/ (* 2 pi) num-slices))
         (tangent-factor (tan theta))
         (radial-factor (cos theta))
         (x radius) ; start 3 O'clock
         (y 0)
         (num-vertices (* num-slices 7))
         (arr (gl:alloc-gl-array :float num-vertices)))
    (dotimes (i num-slices)
      (setf (gl:glaref arr (* i 7)) (coerce (+ x cx) 'single-float))
      (setf (gl:glaref arr (+ (* i 7) 1)) (coerce (+ y cy) 'single-float))
      (setf (gl:glaref arr (+ (* i 7) 2)) 0.0)
      (setf (gl:glaref arr (+ (* i 7) 3)) (aref rgb 1))
      (setf (gl:glaref arr (+ (* i 7) 4)) (aref rgb 2))
      (setf (gl:glaref arr (+ (* i 7) 5)) (aref rgb 3))
      (setf (gl:glaref arr (+ (* i 7) 6)) (aref rgb 4))
      (let ((tx (- y)) (ty x))
        (setq x (+ x (* tx tangent-factor))
              y (+ y (* ty tangent-factor)))
        (setq x (* x radial-factor)
              y (* y radial-factor))))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)

    (if filled?
      (gl:draw-arrays :triangle-fan 0 num-slices)
      (gl:draw-arrays :line-loop 0 num-slices))))

(defun gl-add-arc (device cx cy radius start-angle arc-angle filled? &key (rgb (boxgl-device-pen-color device)))
  (enable-gl-shader-program device (lines-shader device))

  (let* ((num-slices (round (* (bw::num-slices radius) (/ arc-angle (* 2 pi)))))
         (theta (/ arc-angle (1- num-slices)))
         (tangent-factor (tan theta))
         (radial-factor (cos theta))
         (x (* radius (cos start-angle)))
         (y (* radius (sin start-angle)))
         (num-vertices (if filled? (+ (* num-slices 7) 7) (* num-slices 7)))
         (arr (gl:alloc-gl-array :float num-vertices)))
    (dotimes (i num-slices)
      (setf (gl:glaref arr (* i 7)) (coerce (+ x cx) 'single-float))
      (setf (gl:glaref arr (+ (* i 7) 1)) (coerce (+ y cy) 'single-float))
      (setf (gl:glaref arr (+ (* i 7) 2)) 0.0)
      (setf (gl:glaref arr (+ (* i 7) 3)) (aref rgb 1))
      (setf (gl:glaref arr (+ (* i 7) 4)) (aref rgb 2))
      (setf (gl:glaref arr (+ (* i 7) 5)) (aref rgb 3))
      (setf (gl:glaref arr (+ (* i 7) 6)) (aref rgb 4))
      (let ((tx (- y)) (ty x))
        (setq x (+ x (* tx tangent-factor))
              y (+ y (* ty tangent-factor)))
        (setq x (* x radial-factor)
              y (* y radial-factor))))

    (when filled?
      (setf (gl:glaref arr (- num-vertices 7)) (coerce cx 'single-float))
      (setf (gl:glaref arr (- num-vertices 6)) (coerce cy 'single-float))
      (setf (gl:glaref arr (- num-vertices 5)) 0.0)
      (setf (gl:glaref arr (- num-vertices 4)) (aref rgb 1))
      (setf (gl:glaref arr (- num-vertices 3)) (aref rgb 2))
      (setf (gl:glaref arr (- num-vertices 2)) (aref rgb 3))
      (setf (gl:glaref arr (- num-vertices 1)) (aref rgb 4)))

    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)

    (if filled?
      (gl:draw-arrays :triangle-fan 0 (1+ num-slices))
      (gl:draw-arrays :line-strip 0 num-slices))))

(defun gl-add-poly (device points &key (filled? t) (rgb (boxgl-device-pen-color device)))
  (enable-gl-shader-program device (lines-shader device))

  (let* ((arr (gl:alloc-gl-array :float (* 7 (length points))))
         (i 0))
    (dolist (item points)
      (setf (gl:glaref arr (* i 7)) (coerce (car item) 'single-float))
      (setf (gl:glaref arr (+ (* i 7) 1)) (coerce (cadr item) 'single-float))
      (setf (gl:glaref arr (+ (* i 7) 2)) 0.0)
      (setf (gl:glaref arr (+ (* i 7) 3)) (aref rgb 1))
      (setf (gl:glaref arr (+ (* i 7) 4)) (aref rgb 2))
      (setf (gl:glaref arr (+ (* i 7) 5)) (aref rgb 3))
      (setf (gl:glaref arr (+ (* i 7) 6)) (aref rgb 4))
      (incf i)
    )
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)

    (if filled?
      (gl:draw-arrays :triangle-fan 0 (length points))
      (gl:draw-arrays :line-loop 0 (length points)))))

(defun gl-add-rect (device x y wid hei &key (rgb (boxgl-device-pen-color device)))
  (enable-gl-shader-program device (lines-shader device))

  ;; todo - use an elements array instead of 6 vertices
  (let* ((vertices `#(,(coerce x 'float) ,(coerce y 'float)                 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ,(coerce (+ x wid) 'float) ,(coerce (+ y hei) 'float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ,(coerce x 'float) ,(coerce (+ y hei) 'float)         0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)

                      ,(coerce x 'float) ,(coerce y 'float)                 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ,(coerce (+ x wid) 'float) ,(coerce y 'float)         0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ,(coerce (+ x wid) 'float) ,(coerce (+ y hei) 'float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ))
         (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  (gl:draw-arrays :triangles 0 6)
  (unenable-shader-programs device))


;;;
;;;  Shader Program Setup
;;;

(defun setup-xyz-rgba-vao (vao vbo num-entries)
  "Using the generated vao and vbo integers to setup the vao for an xyzrgba stride.
  Pre-allocates enough floats to the vbo for num-entries. Each entry takes 7 floats."
  (gl:bind-vertex-array vao)
  (gl:bind-buffer :array-buffer vbo)

  (%gl:buffer-data :array-buffer (* num-entries 7 *cffi-float-size*) (cffi:null-pointer) :dynamic-draw)

  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float nil (* 7 *cffi-float-size*) (cffi:null-pointer))
  (gl:enable-vertex-attrib-array 1)
  (gl:vertex-attrib-pointer 1 4 :float nil (* 7 *cffi-float-size*) (* 3 *cffi-float-size*))

  (gl:bind-vertex-array 0)
  (gl:bind-buffer :array-buffer 0))

(defun setup-lines-program (&key (vertex-shader "boxgl-lines.vs") (fragment-shader "boxgl-lines.fs"))
  (let* ((lines-program (gl:create-program))
         (lines-vao     (gl:gen-vertex-array))
         (lines-buffer  (gl:gen-buffer)))
    (gl:attach-shader lines-program (create-shader vertex-shader :vertex-shader))
    (gl:attach-shader lines-program (create-shader fragment-shader :fragment-shader))
    (gl:link-program lines-program)
    (log:debug "~%lines-program infolog: ~A" (gl:get-program-info-log lines-program))

    (setup-xyz-rgba-vao lines-vao lines-buffer 3000)

    (%gl:uniform-block-binding lines-program (gl:get-uniform-block-index lines-program "Matrices") 0)

    (make-instance 'boxgl-shader-program
                         :program lines-program
                         :vao     lines-vao
                         :buffer  lines-buffer)))
