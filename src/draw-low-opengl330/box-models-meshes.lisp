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
;;;;        Utilities for creating mesh and model structures in openGL.
;;;;
(in-package :boxer)

; Models and meshes for basic boxer items. Includes a mesh for primitives that can be drawn with
; triangles, such as lines, rectangles. Includes another mesh for characters that have their glyphs
; in the glyph atlas and can be drawn with triangles as well.

(defclass boxer-gl-mesh ()
  ((arr :initform (gl:alloc-gl-array :float (* 7 1024)))
   (vao :initform (gl:gen-vertex-array) :accessor mesh-vao)
   (vbo :initform (gl:gen-buffer) :accessor mesh-vbo)
   (size :initform (* 7 1024))
   (pos :initform 0 :accessor mesh-pos
     :documentation "Number of floats")))

(defun make-boxer-xyz-rgba-mesh (&key (size 1024))
  "Create a mesh for buffering boxer lines, rectangles and other items that can be drawn with triangles.
  Each vertice stride is 7 floats: x y z r g b a"
  (let ((mesh (make-instance 'boxer-gl-mesh)))
    (setup-xyz-rgba-vao (mesh-vao mesh) (mesh-vbo mesh) size)
    mesh))

(defun make-boxer-glyphs-xyz-txty-rgba-mesh (&key (size 4096))
  (let ((mesh (make-instance 'boxer-gl-mesh)))
    (setup-xyz-txty-rgba-vao (mesh-vao mesh) (mesh-vbo mesh) size)
    mesh))

(defclass boxer-gl-model ()
  ((xyz-rgba-mesh :initform nil)
   (dashed-xyz-rgba-mesh :initform nil)
   (glyphs-xyz-txty-rgba-mesh :initform nil)

   (cur-tick     :initform 0 :accessor cur-tick
    :documentation "A value to store and compare against in the future to see if the structure
                   this model draws has changed, and needs to be rebuffered.
                   This doesn't necessarily have to be an integer, but it should a string
                   or some lisp structure that can be compared using `equal`.")
   (needs-update :initform t :accessor needs-update)))

(defun make-boxer-gl-model (&key (lines-size 1024) (glyphs-size 4096))
  (let ((model (make-instance 'boxer-gl-model)))
    (setf (slot-value model 'xyz-rgba-mesh) (make-boxer-xyz-rgba-mesh :size lines-size))
    (setf (slot-value model 'glyphs-xyz-txty-rgba-mesh) (make-boxer-glyphs-xyz-txty-rgba-mesh :size glyphs-size))
    model))

(defmethod reset-meshes ((self boxer-gl-model))
  (setf (mesh-pos (slot-value self 'xyz-rgba-mesh)) 0)
  (setf (mesh-pos (slot-value self 'glyphs-xyz-txty-rgba-mesh)) 0)
)

(defmethod draw ((self boxer-gl-model))
  (let ((mesh (slot-value self 'xyz-rgba-mesh)))
    (enable-gl-objects bw::*boxgl-device* :program (shader-program (lines-shader bw::*boxgl-device*))
                                          :vao     (mesh-vao mesh)
                                          :buffer  (mesh-vbo mesh))
    (gl:draw-arrays :triangles 0 (/ (mesh-pos mesh) 7))
    (unenable-shader-programs bw::*boxgl-device*))

  (let ((mesh (slot-value self 'glyphs-xyz-txty-rgba-mesh)))
    (enable-gl-objects bw::*boxgl-device* :program (shader-program (glyph-atlas-shader bw::*boxgl-device*))
                                          :vao     (mesh-vao mesh)
                                          :buffer  (mesh-vbo mesh))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (glyph-atlas-texture-id *freetype-glyph-atlas*))

    (gl:draw-arrays :triangles 0 (/ (mesh-pos mesh) 9))
    ; (format t "~% drawing chas: ~A " (/ (mesh-pos mesh) 9))
    (gl:bind-texture :texture-2d 0)
    (unenable-shader-programs bw::*boxgl-device*))
  )

(defmethod check-tick ((self boxer-gl-model)))


;;; Drawing methods below



(defmethod add-line ((self boxer-gl-model) device x0 y0 x1 y1 &key (rgb (boxgl-device-pen-color device))
                                                                 (pen-size (boxgl-device-pen-size bw::*boxgl-device*))
                                                                 (buffered-size (* *cffi-float-size* (* 7 6))))
  (unless (and (equal (coerce x0 'single-float) (coerce x1 'single-float)) (equal (coerce y0 'single-float) (coerce y1 'single-float)))

    (let* ((mesh (slot-value self 'xyz-rgba-mesh))
           (corners (line-by-width-corners x0 y0 x1 y1 pen-size))
           (vertices (float-vector (aref corners 0) (aref corners 1) 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 0
                                   (aref corners 2) (aref corners 3) 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 1
                                   (aref corners 4) (aref corners 5) 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 2

                                   (aref corners 2) (aref corners 3) 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 1
                                   (aref corners 4) (aref corners 5) 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 2
                                   (aref corners 6) (aref corners 7) 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 3
                        ))
          (arr (slot-value mesh 'arr)))

      (enable-gl-objects device :program (shader-program (lines-shader device))
                                            :vao     (mesh-vao mesh)
                                            :buffer  (mesh-vbo mesh))

      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))

      (%gl::buffer-sub-data :array-buffer (* (mesh-pos mesh) *cffi-float-size*)
                           buffered-size
                           (gl::gl-array-pointer arr))

      (unenable-shader-programs device)
      (setf (mesh-pos mesh) (+ (mesh-pos mesh) (* 6 7))))))

(defmethod add-rect ((self boxer-gl-model) device x y wid hei &key (rgb (boxgl-device-pen-color device))
                                                                 (pen-size (boxgl-device-pen-size bw::*boxgl-device*))
                                                                 (buffered-size (* *cffi-float-size* (* 7 6))))
    (let* ((mesh (slot-value self 'xyz-rgba-mesh))
          ;  (corners (line-by-width-corners x0 y0 x1 y1 pen-size))
           (vertices (float-vector x        y         0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 0
                                  (+ x wid) (+ y hei) 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 1
                                  x         (+ y hei) 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 2

                                  x         y         0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 1
                                  (+ x wid) y         0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 2
                                  (+ x wid) (+ y hei) 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ; point 3
                        ))
          (arr (slot-value mesh 'arr)))

      (enable-gl-objects device :program (shader-program (lines-shader device))
                                            :vao     (mesh-vao mesh)
                                            :buffer  (mesh-vbo mesh))

      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))

      (%gl::buffer-sub-data :array-buffer (* (mesh-pos mesh) *cffi-float-size*)
                           buffered-size
                           (gl::gl-array-pointer arr))

      (unenable-shader-programs device)
      (setf (mesh-pos mesh) (+ (mesh-pos mesh) (* 6 7)))))

(defmethod add-char ((self boxer-gl-model) device x y ch &key (rgb (boxgl-device-pen-color device))
                                                              (baseline-bot nil)
                                                              (font *current-opengl-font*)
                                                              (font-zoom *font-size-baseline*)
                                                              (atlas *freetype-glyph-atlas*))
  "Version of draw-char that uses a glyph from a texture atlas."
  (if (null (get-glyph atlas `(,(opengl-font-fontspec font) ,ch ,(coerce font-zoom 'float))))
    (log:info "Could not look up glyph: ~A" `(,(opengl-font-fontspec font) ,ch ,(coerce font-zoom 'float)))
    (let* ((mesh (slot-value self 'glyphs-xyz-txty-rgba-mesh))
          (font-face (current-freetype-font font))
          (glyph (get-glyph atlas `(,(opengl-font-fontspec font) ,ch ,(coerce font-zoom 'float))))
          (bearing-x (box-glyph-bearing-x glyph))
          (bearing-y (box-glyph-bearing-y glyph))
          (width (box-glyph-width glyph))
          (w width)
          (h (box-glyph-rows glyph))
          (xpos (+ x bearing-x))
          (font-hei (bw::ogl-font-height font))
          (ypos (if baseline-bot
                  (- (- (+ y font-hei) bearing-y) (* font-hei 0.2)) ;; This scaling by 0.2 of the font height is some
                  (- (- y bearing-y) (* font-hei 0.2))))            ;; pixel pushing for the current repaint layout.
          (ypos+h (+ ypos h))
          (xpos+w (+ xpos w))
          (tx (box-glyph-tx glyph))
          (ty (box-glyph-ty glyph))
          (t-wid (box-glyph-t-width glyph))
          (t-hei (box-glyph-t-rows glyph)))
      (enable-gl-objects device :program (shader-program (glyph-atlas-shader device))
                                :vao     (mesh-vao mesh)
                                :buffer  (mesh-vbo mesh))

      ;; make the vertices array
      (let* ((vertices (float-vector xpos   ypos       0.0 tx                                         ty                                           (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ;; 0.0 0.0
                                     xpos   ypos+h     0.0 tx                                         (+ ty (/ t-hei (glyph-atlas-height atlas)))  (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ;;0.0 1.0
                                     xpos+w ypos+h     0.0 (+ tx (/ t-wid (glyph-atlas-width atlas))) (+ ty (/ t-hei (glyph-atlas-height atlas)))  (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4)

                                     ; These 2 can be removed when using the element buffer
                                     xpos   ypos       0.0 tx                                         ty                                           (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ;; 0.0 0.0
                                     xpos+w ypos+h     0.0 (+ tx (/ t-wid (glyph-atlas-width atlas))) (+ ty (/ t-hei (glyph-atlas-height atlas)))  (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4)

                                     xpos+w ypos       0.0 (+ tx (/ t-wid (glyph-atlas-width atlas))) ty                                           (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ;; 1.0 0.0
                                     ))
             (arr (gl:alloc-gl-array :float (length vertices)))
            )
        (dotimes (i (length vertices))
          (setf (gl:glaref arr i) (aref vertices i)))

        (%gl::buffer-sub-data :array-buffer (* (mesh-pos mesh) *cffi-float-size*)
                            (* (length vertices) *cffi-float-size*)
                            (gl::gl-array-pointer arr))
        (unenable-shader-programs bw::*boxgl-device*)
        (setf (mesh-pos mesh) (+ (mesh-pos mesh) (* 6 9)))
      glyph))))

