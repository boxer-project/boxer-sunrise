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
;;;;
;;;;   OpenGL Drawing and Shader Routines
;;;;
;;;;   This is an implemention of the draw-defs drawing operations using modern OpenGL 3.30 core
;;;;   operations with proper vertex and fragment shaders. Outside of any context operations such
;;;;   as swapping buffers and wrapping the calls in contexts, this will use only generic openGL
;;;;   and math calls from libraries like cl-opengl and 3d-matrices. This should make it usable
;;;;   in other openGL environments without too much trouble.
;;;;
;;;;   Notes
;;;;     - gl:get-uniform-location is slow, so we store the uniform ids for each shader
(in-package :boxer)

(defvar *cffi-float-size* nil
  "Looking up the float size does take some time, performance wise.")

(defclass boxgl-device ()
  ((lines-program :accessor boxgl-device-lines-program)
   (lines-vao :accessor boxgl-device-lines-vao)
   (lines-ortho-uniform :accessor lines-ortho-uniform)
   (lines-transform-uniform :accessor lines-transform-uniform)
   (lines-buffer :accessor boxgl-device-lines-buffer)

   (ortho-matrix :accessor boxgl-device-ortho-matrix)
   (transform-matrix :accessor boxgl-device-transform-matrix)

   (ft-glyph-program :accessor boxgl-device-ft-glyph-program)
   (ft-glyph-vao     :accessor boxgl-device-ft-glyph-vao)
   (ft-glyph-buffer  :accessor boxgl-device-ft-glyph-buffer)
   (ft-glyph-ortho-uniform :accessor ft-glyph-ortho-uniform)
   (ft-glyph-transform-uniform :accessor ft-glyph-transform-uniform)

   (pixmap-program :accessor boxgl-device-pixmap-program)
   (pixmap-vao     :accessor boxgl-device-pixmap-vao)
   (pixmap-buffer  :accessor boxgl-device-pixmap-buffer)
   (pixmap-ortho-uniform :accessor pixmap-ortho-uniform)
   (pixmap-transform-uniform :accessor pixmap-transform-uniform)

   (pen-color :accessor boxgl-device-pen-color)) ; current color in #(:rgb 1.0 1.0 1.0 1.0) format
)

(defun gl-add-pixmap (device from-array tx ty wid hei fx fy)
  (let ((program (boxgl-device-pixmap-program device))
         (vao (boxgl-device-pixmap-vao device))
         (buffer (boxgl-device-pixmap-buffer device))
         (texture-id nil) ; (gl:gen-texture)) ;;(opengl::ogl-pixmap-texture from-array)) ;; todo, cache this on ogl-pixmap
         (data (opengl::ogl-pixmap-data from-array))
         (pwid (opengl::ogl-pixmap-width from-array))
         (phei (opengl::ogl-pixmap-height from-array))
         (existing-texture (opengl::ogl-pixmap-texture from-array))
         )
    (gl:use-program program)
    (gl:uniform-matrix-4fv (pixmap-ortho-uniform device) (boxgl-device-ortho-matrix device))
    (gl:uniform-matrix-4fv (pixmap-transform-uniform device) (boxgl-device-transform-matrix device))
    (gl:pixel-store :unpack-alignment 4)

    ;; TODO sgithens 2023-01-06 Still workign on caching textures. I believe in many cases the ogl-pixmap
    ;; is being created in a different capi thread, so when we try and update the texture and read the
    ;; data from it, we're not getting the updated copy of the struct.
    ; (if (= 0 existing-texture)
      (progn
    ; (when (= 0 existing-texture)
      (setf texture-id (gl:gen-texture))
      (gl:bind-texture :texture-2d texture-id)
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

      (gl:tex-image-2d :texture-2d 0 :rgba pwid phei 0 :rgba :unsigned-byte data)
      (gl:generate-mipmap :texture-2d)
      (setf (opengl::ogl-pixmap-texture from-array) texture-id)
      )
    ; )

    (gl:active-texture :texture1)
    (gl:bind-texture :texture-2d (opengl::ogl-pixmap-texture from-array))
    (gl:bind-vertex-array vao)

    (let* ((vertices `#(,(coerce tx 'single-float)         ,(coerce ty 'single-float)         0.0 0.0 1.0
                        ,(coerce tx 'single-float)         ,(coerce (+ ty hei) 'single-float) 0.0 0.0 0.0
                        ,(coerce (+ tx wid) 'single-float) ,(coerce ty 'single-float)         0.0 1.0 1.0
                        ,(coerce (+ tx wid) 'single-float) ,(coerce (+ ty hei) 'single-float) 0.0 1.0 0.0
                        ,(coerce tx 'single-float)         ,(coerce (+ ty hei) 'single-float) 0.0 0.0 0.0
                        ,(coerce (+ tx wid) 'single-float) ,(coerce ty 'single-float)         0.0 1.0 1.0
                        ))
           (arr (gl:alloc-gl-array :float (length vertices))))
      (gl:bind-buffer :array-buffer buffer)
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)
      (gl:draw-arrays :triangles 0 6))

  (gl:use-program 0)
  (gl:bind-vertex-array 0)
  (gl:bind-buffer :array-buffer 0)))

(defun gl-add-char (device x y ch &key (rgb (boxgl-device-pen-color device))
                                       (baseline-bot nil)
                                       (font-face (current-freetype-font *current-opengl-font*)))
  (let* ((program (boxgl-device-ft-glyph-program device))
         (vao (boxgl-device-ft-glyph-vao device))
         (color-uniform (gl:get-uniform-location program "textColor"))
         (glyph (find-box-glyph ch *current-opengl-font*))
         (bearing-x (box-glyph-bearing-x glyph))
         (bearing-y (box-glyph-bearing-y glyph))
         (width (box-glyph-width glyph))
         (w width)
         (h (box-glyph-rows glyph))
         (x (coerce x 'float))
         (y (coerce y 'float))
         (xpos (+ x bearing-x))
         (ypos (if baseline-bot
                 y
                 (- y bearing-y)))
         (ypos+h (+ ypos h))
         (xpos+w (+ xpos w))
         )
    (gl:use-program program)

    (gl:uniform-matrix-4fv (ft-glyph-ortho-uniform device) (boxgl-device-ortho-matrix device))
    (gl:uniform-matrix-4fv (ft-glyph-transform-uniform device) (boxgl-device-transform-matrix device))

    (gl:uniformf color-uniform (aref rgb 1) (aref rgb 2) (aref rgb 3))
    (gl:active-texture :texture0)
    (gl:bind-vertex-array vao)

    ;; make the vertices array
    (let* ((vertices `#(,xpos   ,ypos   0.0 0.0
                        ,xpos   ,ypos+h 0.0 1.0
                        ,xpos+w ,ypos+h 1.0 1.0
                        ,xpos   ,ypos   0.0 0.0
                        ,xpos+w ,ypos+h 1.0 1.0
                        ,xpos+w ,ypos   1.0 0.0))
           (arr (gl:alloc-gl-array :float (length vertices))))
      (gl:bind-buffer :array-buffer (boxgl-device-ft-glyph-buffer device))
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))

      (unless (box-glyph-texture-id glyph)
        (freetype2:load-char font-face ch)
        (let* ((glyphslot (freetype2:render-glyph font-face))
               (bitmap    (freetype2::ft-glyphslot-bitmap glyphslot))
               (width     (freetype2::ft-bitmap-width bitmap))
               (rows      (freetype2::ft-bitmap-rows bitmap))
               (buffer    (freetype2::ft-bitmap-buffer bitmap)))
          (setf (box-glyph-texture-id glyph)
                (create-glyph-texture width rows buffer))))
      (gl:bind-texture :texture-2d (box-glyph-texture-id glyph))
      (%gl::buffer-sub-data :array-buffer 0
                           (* (length vertices) *cffi-float-size*)
                           (gl::gl-array-pointer arr))
      (gl:draw-arrays :triangles 0 6)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)
      (gl:bind-texture :texture-2d 0))
    glyph))

(defun gl-add-string (device font string x y)
  (let ((cur-x x)
        (prev-glyph nil))
      (for:for ((c over string))
        (setf prev-glyph (gl-add-char device cur-x y c :baseline-bot t))
        (setf cur-x (+ cur-x (box-glyph-advance prev-glyph)))
        )))

(defun gl-add-rect (device x y wid hei &key (rgb (boxgl-device-pen-color device)))
  (let* ((program (boxgl-device-lines-program device)))
    (gl:use-program program)
    (gl:uniform-matrix-4fv (lines-ortho-uniform device) (boxgl-device-ortho-matrix device))
    (gl:uniform-matrix-4fv (lines-transform-uniform device) (boxgl-device-transform-matrix device))
    )
  ;; todo - use an elements array instead of 6 vertices
  (gl:bind-buffer :array-buffer (boxgl-device-lines-buffer device))
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


  (gl:bind-vertex-array (boxgl-device-lines-vao device))
  (gl:draw-arrays :triangles 0 6)
  (gl:use-program 0)
  (gl:bind-vertex-array 0)
  (gl:bind-buffer :array-buffer 0)
)

(defun gl-add-circle (device cx cy radius filled? &key (rgb (boxgl-device-pen-color device)))
  (gl:use-program (boxgl-device-lines-program device))
  (gl:uniform-matrix-4fv (lines-ortho-uniform device) (boxgl-device-ortho-matrix device))
  (gl:uniform-matrix-4fv (lines-transform-uniform device) (boxgl-device-transform-matrix device))

  (gl:bind-buffer :array-buffer (boxgl-device-lines-buffer device))
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

    (gl:bind-vertex-array (boxgl-device-lines-vao device))
    (if filled?
      (gl:draw-arrays :triangle-fan 0 num-slices)
      (gl:draw-arrays :line-loop 0 num-slices))

    (gl:use-program 0)
    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)))

(defun gl-add-point (device x0 y0 &key (rgb (boxgl-device-pen-color device)))
  (let* ((program (boxgl-device-lines-program device)))
    (gl:use-program program)
    (gl:uniform-matrix-4fv (lines-ortho-uniform device) (boxgl-device-ortho-matrix device))
    (gl:uniform-matrix-4fv (lines-transform-uniform device) (boxgl-device-transform-matrix device)))

  (gl:bind-buffer :array-buffer (boxgl-device-lines-buffer device))
  (let* ((vertices `#(,(coerce x0 'float) ,(coerce y0 'float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)))
         (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  (gl:bind-vertex-array (boxgl-device-lines-vao device))
  (gl:draw-arrays :points 0 1)
  (gl:use-program 0)
  (gl:bind-vertex-array 0)
  (gl:bind-buffer :array-buffer 0)
)

(defun gl-add-line (device x0 y0 x1 y1 &key (rgb (boxgl-device-pen-color device)))
  (let* ((program (boxgl-device-lines-program device)))
    (gl:use-program program)
    (gl:uniform-matrix-4fv (lines-ortho-uniform device) (boxgl-device-ortho-matrix device))
    (gl:uniform-matrix-4fv (lines-transform-uniform device) (boxgl-device-transform-matrix device)))

  (gl:bind-buffer :array-buffer (boxgl-device-lines-buffer device))
  (let* ((vertices `#(,(coerce x0 'float) ,(coerce y0 'float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ,(coerce x1 'float) ,(coerce y1 'float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ))
         (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  (gl:bind-vertex-array (boxgl-device-lines-vao device))
  (gl:draw-arrays :lines 0 2)
  (gl:use-program 0)
  (gl:bind-vertex-array 0)
  (gl:bind-buffer :array-buffer 0)
)

(defun create-shader (glsl-filename shader-type)
  "Creates and return the int id for a new shader. `glsl-filename` should be the relative
  filename, such as 'boxgl-stuff.vs'. Shader type should be the symbol type accepted by cl-opengl
  such as :vertex-shader or :fragment-shader"
  (let ((togo (gl:create-shader shader-type)))
    (gl:shader-source togo (read-shader-source glsl-filename))
    (gl:compile-shader togo)
    (log:debug "~%shader: ~A infolog: ~A" glsl-filename (gl:get-shader-info-log togo))
    togo))

(defun setup-freetype-program (device)
  (let* ((glyph-program   (gl:create-program))
         (glyph-vao       (gl:gen-vertex-array))
         (font-face       (boxer::make-freetype-face "LiberationSans-Regular.ttf"))
         (glyph-buffer    (gl:gen-buffer)))

    (gl:attach-shader glyph-program (create-shader "boxgl-freetype-glyph.vs" :vertex-shader))
    (gl:attach-shader glyph-program (create-shader "boxgl-freetype-glyph.fs" :fragment-shader))
    (gl:link-program glyph-program)
    (log:debug "~%glyph-program infolog: ~A" (gl:get-program-info-log glyph-program))

    (setf (boxer::boxgl-device-ft-glyph-program device) glyph-program)
    (setf (boxer::boxgl-device-ft-glyph-vao device) glyph-vao)

    (freetype2::ft-set-pixel-sizes font-face 0 48)

    (gl:pixel-store :unpack-alignment 1)

    (gl:bind-vertex-array glyph-vao)
    (gl:bind-buffer :array-buffer glyph-buffer)
    (%gl:buffer-data :array-buffer (* 4 6 *cffi-float-size*) (cffi:null-pointer) :dynamic-draw)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 4 :float :false (* 4 *cffi-float-size*) 0) ;; TODO Should the :false really be nil?
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)
    (gl:bind-texture :texture-2d 0)

    (setf (boxgl-device-ft-glyph-program device) glyph-program)
    (setf (boxgl-device-ft-glyph-vao device) glyph-vao)
    (setf (boxgl-device-ft-glyph-buffer device) glyph-buffer)

    (setf (boxer::ft-glyph-ortho-uniform device) (gl:get-uniform-location glyph-program "ortho"))
    (setf (boxer::ft-glyph-transform-uniform device) (gl:get-uniform-location glyph-program "transform"))))

(defun setup-pixmap-program (device)
  (let* ((pixmap-program   (gl:create-program))
         (pixmap-vao       (gl:gen-vertex-array))
         (pixmap-buffer    (gl:gen-buffer)))
    (gl:attach-shader pixmap-program (create-shader "boxgl-pixmap.vs" :vertex-shader))
    (gl:attach-shader pixmap-program (create-shader "boxgl-pixmap.fs" :fragment-shader))
    (gl:link-program pixmap-program)
    (log:debug "~%pixmap-program infolog: ~A" (gl:get-program-info-log pixmap-program))

    (setf (boxer::boxgl-device-pixmap-program device) pixmap-program)
    (setf (boxer::boxgl-device-pixmap-vao device) pixmap-vao)

    ;; Vertex attribute data for the pixmaps will be in the following format:
    ;;    layout 0       layout 1
    ;;      vertice        texture coordinate
    ;;      x    y    z     tx  ty
    ;; #( 0.5  0.5  0.0    1.0 1.0 ...
    ;;
    ;; Total stride is 5

    (gl:bind-vertex-array pixmap-vao)
    (gl:bind-buffer :array-buffer pixmap-buffer)
    ; (%gl:buffer-data :array-buffer (* 4 6 *cffi-float-size*) (cffi:null-pointer) :dynamic-draw)
    (gl:enable-vertex-attrib-array 0)
    ;; layout 0 is the x, y, z
    (gl:vertex-attrib-pointer 0 3 :float :false (* 5 *cffi-float-size*) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 1)
    ;; layout 1 is the texture coords tx, ty
    (gl:vertex-attrib-pointer 1 2 :float :false (* 5 *cffi-float-size*) (* 3 *cffi-float-size*))
    ; (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)
    (gl:bind-texture :texture-2d 0)

    (setf (boxgl-device-pixmap-program device) pixmap-program)
    (setf (boxgl-device-pixmap-vao device) pixmap-vao)
    (setf (boxgl-device-pixmap-buffer device) pixmap-buffer)

    (setf (boxer::pixmap-ortho-uniform device) (gl:get-uniform-location pixmap-program "ortho"))
    (setf (boxer::pixmap-transform-uniform device) (gl:get-uniform-location pixmap-program "transform"))))

(defun setup-lines-program (device)
  (let* ((lines-program (gl:create-program))
         (lines-vao     (gl:gen-vertex-array))
         (lines-buffer  (gl:gen-buffer)))
    (gl:attach-shader lines-program (create-shader "boxgl-lines.vs" :vertex-shader))
    (gl:attach-shader lines-program (create-shader "boxgl-lines.fs" :fragment-shader))
    (gl:link-program lines-program)
    (log:debug "~%lines-program infolog: ~A" (gl:get-program-info-log lines-program))

    (gl:bind-vertex-array lines-vao)
    (gl:bind-buffer :array-buffer lines-buffer)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float nil (* 7 *cffi-float-size*) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 4 :float nil (* 7 *cffi-float-size*) (* 3 *cffi-float-size*))

    (setf (boxer::boxgl-device-lines-program device) lines-program)
    (setf (boxer::boxgl-device-lines-vao device) lines-vao)
    (setf (boxer::boxgl-device-lines-buffer device) lines-buffer)

    (setf (boxer::lines-ortho-uniform device) (gl:get-uniform-location lines-program "ortho"))
    (setf (boxer::lines-transform-uniform device) (gl:get-uniform-location lines-program "transform"))))

(defun read-shader-source (filename)
  "Reads the relative filename of the shader without any directory, and loads the source into a
  string. Configured to use the Boxer file location for shaders. Returns a string with the source"
  (uiop:read-file-string
    (cl-fad:merge-pathnames-as-file "/Users/sgithens/code/boxer-sunrise/src/draw-low-opengl330/" filename)))

(defun create-ortho-matrix (wid hei)
  "Create an orthogonal projection matrix for use in our shaders with the given width and height."
  (let ((ortho (3d-matrices:mortho 0 wid hei 0 -1.0 1.0)))
    (3d-matrices:marr4 ortho)))

(defun create-transform-matrix (x y)
  "Create a transform matrix offset from the origin by x and y."
  (3d-matrices:marr4 (3d-matrices:mtranslation (3d-vectors:vec x y 0))))

(defun make-boxgl-device (wid hei)
  "Constructor to create a new boxgl-device renderer. Sets up initial shaders, buffers, etc.
Needs to be run inside an active openGL context. Using the Lispworks OpenGL impl that means
inside an opengl:rendering-on macro invocation."
  (let ((togo (make-instance 'boxgl-device)))
    (setf *cffi-float-size* (cffi:foreign-type-size :float))

    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (setup-freetype-program togo)
    (setup-pixmap-program togo)
    (setup-lines-program togo)

    (setf (boxgl-device-ortho-matrix togo)
          (create-ortho-matrix wid hei)

          (boxgl-device-transform-matrix togo)
          (create-transform-matrix 0 0)

          (boxer::boxgl-device-pen-color togo)
          #(:rgb 0.0 0.0 0.0 1.0))
  togo))
