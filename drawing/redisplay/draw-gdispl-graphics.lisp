;;;;  ;-*- Mode:Lisp; Package:boxer; Syntax: Common-Lisp; -*-
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
;;;;                  This file is part of the | Boxer | System
;;;;                                           +-------+
;;;;
;;;;
;;;;        Code for drawing graphics display lists and sprites
;;;;
(in-package :boxer)

;; from grmeth.lisp
(defmethod draw ((self button))
  (unless (null (shown? self))
    (let* ((prev-model (get-model-matrix))
           (final-mat (3d-matrices:m* prev-model (model-matrix self %drawing-half-width %drawing-half-height))))
      ;; sgithens TODO 2024-12-26 Remove the drawing-half-width/height from above and also from the graphics command
      ;; list playback, and move it to a matrix transform at a higher level, so individual drawback code doens't need
      ;; to worry about it, especially when the transforms become more complex in the future (z-axis, rotations, etc)

      (setf (get-model-matrix) final-mat)
      (refresh-gpu-model-matrix)

      ;; We need to override the drawing-half-width and drawing-half-height here
      ;; with the values from the shape box graphics-sheet, otherwise, if it's scaled or
      ;; someething it will use the values from the upper level graphics box.  This is
      ;; primarily for correct operation of draw-wrap-line which uses these global values.

      ;; Occasionally there seems to be a shape that has it's interface value, but not
      ;; the accompanying box
      ;; TODO: these heights really should only need calculating if something changed
      (let* ((shape-box (box-interface-box (slot-value self 'shape)))
             (gs (if shape-box
                    (graphics-sheet shape-box)
                    nil))
             (gl (box-interface-value (slot-value self 'shape)))
             (extents (multiple-value-list (graphics-list-extent gl)))
             (wid (if gs
                   (graphics-sheet-draw-wid gs)
                   (floor (first extents))))
             (hei (if gs
                   (graphics-sheet-draw-hei gs)
                   (floor (second extents))))
             (%drawing-half-width (/ wid 2))
             (%drawing-half-height (/ hei 2))
             (canvas nil)
             (mesh nil))

        (if *use-opengl-framebuffers*
          (progn
            ;; For frame buffered turtles, we keep them on the actual graphics-object turtle object, since it's
            ;; not guarenteed the sprite will actually have a shape box.
            (setf canvas (get-graphics-canvas-for-screen-obj self wid hei))
            (setf mesh (get-canvas-mesh self))


            (playback-graphics-list-incrementally gl canvas wid hei :mesh mesh)

            (let ((pixmap (graphics-canvas-pixmap canvas)))
              (setf final-mat (3d-matrices:m* final-mat
                                              (3d-matrices:mtranslation (3d-vectors:vec (- (/ wid 2)) (- (/ hei 2)) 0.0)) ))
              (setf (get-model-matrix) final-mat)
              (refresh-gpu-model-matrix)
              (draw-canvas-mesh mesh pixmap)))
          (boxer-playback-graphics-list gl :use-cur-model-matrix t)))

      (setf (get-model-matrix) prev-model)
      (refresh-gpu-model-matrix)

      (unless (eq (shown? self) ':no-subsprites)
        (dolist (subs (slot-value self 'subsprites))
          (draw subs))))))

;; from gdispl.lisp
;; this is used by the redisplay...

(defun playback-graphics-list-incrementally (gl canvas wid hei &key (mesh nil))
  "When framebuffers are being used, this will play back any additional graphics operations on to the mesh/framebuffer,
  only appending new ones since the last draw."
  (when (> (%%sv-fill-pointer gl) (op-count canvas))
              (enable canvas)
              (unless (graphics-command-list-hidden gl)
                (boxer-playback-graphics-list gl :start (op-count canvas)
                   :graphics-canvas canvas :translate? t))
              (setf (op-count canvas) (%%sv-fill-pointer gl))
              (disable canvas)
              (when mesh
                (buffer-canvas-mesh bw::*boxgl-device* mesh (graphics-canvas-pixmap canvas) wid hei))))

(defun redisplay-graphics-sheet-graphics-list (gs graphics-screen-box)
  "Draws the graphics-command-list of the graphics-sheet in member graphics-list. This typically contains
   everything that has been drawn or stamped by sprites and doesn't privately belong to them. This takes
   in to account if openGL framebuffers are used and can paint to the framebuffer attached to the
   screen-box."
  (with-graphics-vars-bound ((screen-obj-actual-obj graphics-screen-box))
    (if *use-opengl-framebuffers*
      (let* ((wid (graphics-sheet-draw-wid gs))
             (hei (graphics-sheet-draw-hei gs))
             (gl (graphics-sheet-graphics-list gs))
             (canvas (get-graphics-canvas-for-screen-obj graphics-screen-box wid hei)))
        (playback-graphics-list-incrementally gl canvas wid hei))
      ; else
      (let ((gl (graphics-sheet-graphics-list gs)))
        (unless (graphics-command-list-hidden gl)
          (boxer-playback-graphics-list gl :translate? t))))))

(defun redisplay-graphics-sheet-sprites (gs graphics-screen-box)
  "Draws all the sprites in the the object-list slot of the related graphics-sheet.  Draws the sprites private
   graphcis-command-list and issues each sprites draw method."
  (with-graphics-vars-bound ((screen-obj-actual-obj graphics-screen-box))
    (let ((sprites (graphics-sheet-object-list gs)))
      (dolist (sprite sprites)
        (when (turtle? sprite)
          (let ((pgl (slot-value sprite 'private-gl)))
            (unless (graphics-command-list-hidden pgl)
              (boxer-playback-graphics-list pgl)
              ))))
      (dolist (sprite sprites)
        (unless (null (shown? sprite))
          (draw sprite))))))
