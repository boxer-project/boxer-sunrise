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
;;;;     Emtpy stubs of draw functions
;;;;
(in-package :boxer-draw-bridge)

;; %bitblt-to-screen wid hei from-array from-x from-y to-x to-y))
;; %buffer-canvas-mesh device mesh pixmap wid hei))

(defun %cha-wid (char)
  10)

(defun %clear-window (color)
  nil)

;; %draw-c-arc x y radius start-angle sweep-angle filled?))
;; %draw-canvas-mesh mesh pixmap))

(defun %draw-cha (char x y &key (gl-model nil))
  nil)

(defun %draw-circle (x y radius filled?)
  nil)

(defun %draw-ellipse (x y width height filled?)
  nil)

(defun %draw-line (x0 y0 x1 y1)
  nil)

(defun %draw-poly (points)
  nil)

(defun %draw-rectangle (w h x y)
  nil)

(defun %draw-string (font-no string region-x region-y)
  nil)

;; %flush-port-buffer pane))
;; %get-pixel view (floor x) (floor y))))

(defun %set-pen-color (color)
  nil)

(defun %string-ascent (font-no)
  10)

(defun %string-hei (font-no)
  10)

(defun %string-wid (font-no string)
  10)

;; cur-tick))
;; disable paint-tex))
;; enable paint-tex)
;; graphics-canvas-pixmap paint-tex))))
;; ignore-stencil)))))
;; load-freetype-faces)

(defun %pixblt-from-screen (pixmap fx fy wid hei)
  nil)

(defun %make-boxer-gl-model ()
  (make-instance 'boxer:boxer-gl-model))

(defun %find-glyph (spec)
  nil)

;; make-glyph-atlas)))
;; make-graphics-canvas (ogl-pixmap-width  ,bitmap) (ogl-pixmap-height ,bitmap) ,bitmap)))
;; needs-update model) nil)
;; needs-update obj))
;; ogl-reshape window-width window-height))
;; opengl-enables)
;; render-inside-stencil)))

;; update-matrices-ubo bw::*boxgl-device*))
;; update-model-matrix-ubo bw::*boxgl-device*))
(defun %refresh-gpu-model-matrix ()
  nil)

;; update-transform-matrix-ubo self))
;; write-to-stencil)
;; *freetype-glyph-atlas* (
