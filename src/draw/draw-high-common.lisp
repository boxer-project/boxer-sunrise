;;;;; -*- Mode:LISP; Syntax: Common-Lisp; Package:BOXER; Base:8.-*-
;;;;
;;;;    Boxer
;;;;    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                         +-Data--+
;;;;                This file is part of the | BOXER | system
;;;;                                         +-------+
;;;;
;;;;   This file contains the low level drawing primitives which are
;;;;   machine independent.  This file is meant to coexist with various
;;;;   "xxx-draw-low" files which are the machine specific primitives.
;;;;
(in-package :boxer)

;;;
;;; DRAWING-ON-WINDOW macros
;;;

(defmacro drawing-on-window ((window) &body body)
  "DRAWING-ON-WINDOW is an &body macro which all the drawing macros in this
must be called inside of. It basically prepares the window to be drawn on
and binds all the magic variables that the drawing macros need including
the bootstrapping of the clipping and coordinate scaling variables."
  (once-only (window)
    `(with-drawing-port ,window
        . ,body)))

(defmacro drawing-on-bitmap ((bitmap) &body body)
  "Used instead of DRAWING-ON-WINDOW for bitmaps."
  (let ((bwidth-var (gensym)) (bheight-var (gensym)))
    `(let ((,bwidth-var (ogl-pixmap-width ,bitmap))
           (,bheight-var (ogl-pixmap-height ,bitmap)))
         (with-system-dependent-bitmap-drawing (,bitmap ,bwidth-var ,bheight-var)
           . ,body))))

;;;
;;; Scaling and Clipping Macros
;;;

(defmacro with-world-matrix ((screen-obj) &body body)
  (let ((cur-model-matrix (gensym)))
    `(let* ((,cur-model-matrix (boxer::boxgl-device-model-matrix bw::*boxgl-device*)))
       (unwind-protect
           (progn
             (apply-world-matrix ,screen-obj)
             . ,body)
         (progn
           (setf (boxer::boxgl-device-model-matrix bw::*boxgl-device*) ,cur-model-matrix)
           (update-model-matrix-ubo bw::*boxgl-device*)
           )
         ))))

(defmacro with-world-internal-matrix ((screen-obj) &body body)
  (let ((cur-model-matrix (gensym)))
    `(let* ((,cur-model-matrix (boxer::boxgl-device-model-matrix bw::*boxgl-device*)))
       (unwind-protect
           (progn
             (apply-world-internal-matrix ,screen-obj)
             . ,body)
         (progn
           (setf (boxer::boxgl-device-model-matrix bw::*boxgl-device*) ,cur-model-matrix)
           (update-model-matrix-ubo bw::*boxgl-device*)
           )
         ))))

(defun calculate-clip-rectangle (stack)
  "Takes a stack of lists each with 4 members: x1, y1, x2, y2 and finds the final
   rectangle to be clipped on screen.

   Assumes that each point x2,y2 is greater than x1,y1."
  (destructuring-bind (x1 y1 x2 y2) (car stack)
    (dolist (next (cdr stack))
      (destructuring-bind (next-x1 next-y1 next-x2 next-y2) next
        (setf x1 (max x1 next-x1) y1 (max y1 next-y1)
              x2 (min x2 next-x2) y2 (min y2 next-y2))))
    (list x1 y1 x2 y2)))

(defun clip-stencil-rectangle (clip-stack)
  (let ((prev-model (boxer::boxgl-device-model-matrix bw::*boxgl-device*))
        (clip-coords (calculate-clip-rectangle clip-stack)))
    (write-to-stencil)

    (setf (boxer::boxgl-device-model-matrix bw::*boxgl-device*) (3d-matrices:meye 4))
    (update-model-matrix-ubo bw::*boxgl-device*)

    (with-pen-color (*transparent*)
      (destructuring-bind (x1 y1 x2 y2) clip-coords
        (draw-rectangle (- x2 x1) (- y2 y1) x1 y1)))
    (render-inside-stencil)

    (setf (boxer::boxgl-device-model-matrix bw::*boxgl-device*) prev-model)
    (update-model-matrix-ubo bw::*boxgl-device*)))

(defmacro with-clipping-inside ((x y wid hei) &body body)
  `(unwind-protect
    (progn
      (push (list ,x ,y (+ ,x ,wid) (+ ,y ,hei)) *clipping-stack*) ;,(+ x wid) ,(+ y hei)) *clipping-stack*)
      (clip-stencil-rectangle *clipping-stack*)
      . ,body)
    ;; reset the old clip region
    (progn
      (pop *clipping-stack*)
      (if (car *clipping-stack*)
        (clip-stencil-rectangle *clipping-stack*)
        (ignore-stencil)))))

;;;
;;; Drawing functions
;;;

;; TODO for anything not in the texture map, just use the regular draw-cha for now
(defun draw-cha (char x y &key (gl-model nil))
  "Draw-cha needs to draw at the char's baseline rather than the top left corner.  In a
multifont row, the common reference point will be the baseline instead of the top edge"
  (%draw-cha x y char :gl-model gl-model))

(defun draw-circle (x y radius &optional filled?)
  (%draw-circle x y radius filled?))

(defun draw-ellipse (x y width height &optional filled?)
  (%draw-ellipse x y width height filled?))

(defun draw-line (x0 y0 x1 y1)
  (%draw-line x0 y0 x1 y1))

(defun draw-point (x y)
  (%draw-point x y))

(defun draw-poly (points)
  (%draw-poly points))

(defun draw-rectangle (w h x y)
  (%draw-rectangle w h x y))

(defun erase-rectangle (w h x y)
  (%erase-rectangle w h x y))

(defun draw-string (font-no string region-x region-y)
  (%draw-string font-no string region-x region-y))


(defun bitblt-to-screen (wid hei from-array from-x from-y to-x to-y)
  (%bitblt-to-screen wid hei from-array from-x from-y to-x to-y))

(defun bitblt-from-screen (wid hei to-array from-x from-y to-x to-y)
  (%bitblt-from-screen wid hei to-array from-x from-y to-x to-y))


(defun swap-graphics-buffers (&optional (pane *boxer-pane*))
  (%flush-port-buffer pane))

;;;
;;; Drawing operations to wrap Graphics List Playback to allow for varying performance
;;; optimizations
;;;
;;; For instance, in OpenGL we can put a bunch of lines on a C buffer and then draw them
;;; at once, and this requires keeping track of some state variables. The methods below
;;; allow you to perform these initializations, drawing, finalizations before and after
;;; iterating through a list of graphics commands, and before each graphic command is processed.
;;;

(defmethod get-boxgl-model-for-screen-obj ((self screen-obj))
  "Gets an instance of `boxer-gl-model` from the plist on the screen-obj, creating it if it doensn't exist
  yet. Also updates the cur-tick and needs-update properties of the model so we know if the screen-obj has
  changed and requires rebuffering of vertices."
  (let ((tick (get-updated-tick self))
        (model (getprop self  :gl-model)))

    (unless model
      (putprop self (make-boxer-gl-model) :gl-model)
      (setf model (getprop self  :gl-model)))

    (if (equal tick (slot-value model 'cur-tick))
      (setf (needs-update model) nil)
      (progn
        (reset-meshes model)
        (setf (slot-value model 'cur-tick) tick)))
    model))

(defmethod get-updated-tick ((self screen-obj))
  (actual-obj-tick (screen-obj-actual-obj self)))

(defmethod get-updated-tick ((self screen-row))
  "For checking if a screen row needs rebuffered, we are looking at the dimensions, offsets, and
   cliping. It's tick updates on nearly every repaint, so that's not usable here."
  (with-slots (actual-obj wid hei x-offset y-offset x-got-clipped? y-got-clipped?)
              self
    (list (actual-obj-tick actual-obj) wid hei x-offset y-offset x-got-clipped? y-got-clipped?)))

(defmethod get-updated-tick ((self screen-box))
  "Currently we're just using the screen-box model for drawing borders."
  (with-slots (wid hei actual-obj box-type) self
    (list wid hei (name actual-obj) box-type (get-css-style actual-obj :border-color)
          (display-style-border-style (display-style-list actual-obj)))))

(defmethod get-graphics-canvas-for-screen-obj ((self plist-subclass) &optional wid hei)
  (unless (slot-boundp self 'plist)
    (setf (plist self) nil))

  (let ((paint-tex (getprop self :graphics-canvas)))
    (cond
      ((null paint-tex)
       (putprop self (make-graphics-canvas wid hei) :graphics-canvas)
       (setf paint-tex (getprop self :graphics-canvas))
       (enable paint-tex)
       (gl:clear-color 0.0 0.0 0.0 0.0) ;; transparent
       (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit)
       (opengl-enables)
       (disable paint-tex))
      ((or (not (equal wid (ogl-pixmap-width (graphics-canvas-pixmap paint-tex))))
           (not (equal hei (ogl-pixmap-height (graphics-canvas-pixmap paint-tex)))))
       (enable paint-tex)
       (resize paint-tex wid hei)
       (disable paint-tex))
      (t nil))
    paint-tex))

(defparameter *cur-gl-model-screen-obj* nil
  "If this global object is set, then supported draw-xyzed commands will buffer vertices to this gl model
  rather immediately buffer and draw them.")

(defun start-drawing-screen-obj-model (screen-obj)
  "This set the gl-model to being drawing to for all supported drawing commands until stop-drawing-screen-obj-model
  is called.
  Currently used to set the model for drawing box borders, including name and type labels."
  (setf *cur-gl-model-screen-obj* (get-boxgl-model-for-screen-obj screen-obj)))

(defun stop-drawing-screen-obj-model ()
  "This will stop drawing to the screen-obj's model and draw it to the active buffer."
  (when *cur-gl-model-screen-obj*
    (draw *cur-gl-model-screen-obj*))
  (setf *cur-gl-model-screen-obj* nil))
