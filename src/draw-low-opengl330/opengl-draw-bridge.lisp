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
;;;;   OpenGL implementation of draw functions.
;;;;
(in-package :boxer-draw-bridge)

;; %bitblt-to-screen wid hei from-array from-x from-y to-x to-y))
;; %buffer-canvas-mesh device mesh pixmap wid hei))

(defun %cha-wid (char)
  (boxer-opengl::%cha-wid char))

(defun %clear-window (color)
  (boxer-opengl::%clear-window color))

;; %draw-c-arc x y radius start-angle sweep-angle filled?))
;; %draw-canvas-mesh mesh pixmap))

(defun %draw-cha (char x y &key (gl-model nil))
  (boxer-opengl::%draw-cha char x y :gl-model gl-model))

(defun %draw-circle (x y radius filled?)
  (boxer-opengl::%draw-circle x y radius filled?))

(defun %draw-ellipse (x y width height filled?)
  (boxer-opengl::%draw-ellipse x y width height filled?))

(defun %draw-line (x0 y0 x1 y1)
  (boxer-opengl::%draw-line x0 y0 x1 y1))

(defun %draw-poly (points)
  (boxer-opengl::%draw-poly points))

(defun %draw-rectangle (w h x y)
  (boxer-opengl::%draw-rectangle w h x y))

(defun %draw-string (font-no string region-x region-y)
  (boxer-opengl::%draw-string font-no string region-x region-y))

;; %flush-port-buffer pane))
;; %get-pixel view (floor x) (floor y))))

(defun %set-pen-color (color)
  (boxer-opengl::%set-pen-color color))

(defun %string-ascent (font-no)
  (boxer-opengl::%string-ascent font-no))

(defun %string-hei (font-no)
  (boxer-opengl::%string-hei font-no))

(defun %string-wid (font-no string)
  (boxer-opengl::%string-wid font-no string))

;; disable paint-tex))
;; enable paint-tex)
;; graphics-canvas-pixmap paint-tex))))
;; ignore-stencil)))))
;; load-freetype-faces)
;; make-boxer-gl-model) :gl-model)

(defun %pixblt-from-screen (pixmap fx fy wid hei)
  (boxer-opengl::%pixblt-from-screen pixmap fx fy wid hei))

(defun %make-boxer-gl-model ()
  (boxer-opengl::make-boxer-opengl-model))

(defun %find-glyph (spec)
  (boxer-opengl::get-glyph boxer-opengl::*freetype-glyph-atlas* spec))

;; make-glyph-atlas)))
;; make-graphics-canvas (ogl-pixmap-width  ,bitmap) (ogl-pixmap-height ,bitmap) ,bitmap)))
;; ogl-reshape window-width window-height))
;; opengl-enables)
;; render-inside-stencil)))

;; update-matrices-ubo bw::*boxgl-device*))

(defun %refresh-gpu-model-matrix ()
  (boxer-opengl::update-model-matrix-ubo bw::*boxgl-device*))

;; update-transform-matrix-ubo self))
;; write-to-stencil)
