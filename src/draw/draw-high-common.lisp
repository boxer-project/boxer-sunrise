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
    `(prepare-sheet (,window)
       (drawing-on-window-without-prepare-sheet (,window) . ,body))))

(defmacro drawing-on-window-bootstrap-clipping-and-scaling ((x y wid hei) &body body)
  `(let* ((%origin-x-offset ,x) (%origin-y-offset ,y)
          ;; absolute clipping parameters
          (%clip-lef ,x) (%clip-top ,y)
    (%clip-rig (+& %clip-lef ,wid)) (%clip-bot (+& %clip-top ,hei))
          ;; relative clipping parameters
          (%local-clip-lef 0)    (%local-clip-top 0)
          (%local-clip-rig ,wid) (%local-clip-bot ,hei))
     %clip-rig %clip-bot %origin-x-offset %origin-y-offset ;bound but never...
     %local-clip-lef %local-clip-top %local-clip-rig %local-clip-bot
                ,@body))

(defmacro drawing-on-window-without-prepare-sheet ((window) &body body)
  "DRAWING-ON-WINDOW-WITHOUT-PREPARE-SHEET is a variant of Drawing-On-Window
which does everything Drawing-On-Window does except that it does not do a
PREPARE-SHEET of the window. Unless you really know what you are doing
you should only use this inside the :BLINK method for a blinker."
  (once-only (window)
    `(drawing-on-window-bootstrap-clipping-and-scaling
       (0 0
        (sheet-inside-width ,window) (sheet-inside-height ,window))
        . ,body)))

(defmacro drawing-on-bitmap ((bitmap) &body body)
  "Used instead of DRAWING-ON-WINDOW for bitmaps."
  (let ((bwidth-var (gensym)) (bheight-var (gensym)))
    `(let ((,bwidth-var (ogl-pixmap-width ,bitmap))
           (,bheight-var (ogl-pixmap-height ,bitmap)))
       (drawing-on-window-bootstrap-clipping-and-scaling
         (0 0 ,bwidth-var ,bheight-var)
         (with-system-dependent-bitmap-drawing (,bitmap ,bwidth-var ,bheight-var)
           . ,body)))))

;;;
;;; Scaling and Clipping Macros
;;;

;; origin gets reset in hardware by scaling macros so these are no ops
;; They need to be defined because other functions (usually sprite graphics)
;; will use them explicitly to convert coords.
(defmacro scale-x (x) x)
(defmacro scale-y (y) y)

(defmacro with-drawing-inside-region ((x y wid hei) &body body)
  "**** this is the reverse of the software version because the
WITH-CLIPPING-INSIDE macro should use the new coordinate system
set by WITH-ORIGIN-AT"
  `(with-origin-at (,x ,y)
     (with-clipping-inside (0 0 ,wid ,hei)
       . ,body)))

(defmacro with-origin-at ((x y) &body body)
  "Opengl set-origin is RELATIVE !"
  (let ((fx (gensym)) (fy (gensym)) (ux (gensym)) (uy (gensym)))
    `(let* ((,fx (float ,x)) (,fy (float ,y))
            (,ux (float-minus ,fx)) (,uy (float-minus ,fy))
            ;; keep track of scaling because bitblt doesn't respect OpenGL translation
            (%origin-x-offset (+ %origin-x-offset ,x))
            (%origin-y-offset (+ %origin-y-offset ,y)))
       (unwind-protect
           (progn
             (window-system-dependent-set-origin ,fx ,fy)
             . ,body)
         (window-system-dependent-set-origin ,ux ,uy)))))

(defmacro with-clipping-inside ((x y wid hei) &body body)
  `(with-window-system-dependent-clipping (,x ,y ,wid ,hei) . ,body))

(defmacro with-scrolling-origin ((scroll-x scroll-y) &body body)
  ;; do we need to readjust the clip region here ????
  `(with-origin-at (,scroll-x ,scroll-y)
     . ,body))

(defmacro with-turtle-clipping ((wid hei . args) &body body)
  "This MUST use the hardware clipping regardless of speed.
It is used only around bodies which do sprite graphics
so the frequency of use is MUCH less than it is in the redisplay

this adjusts the clipping to be width and height AT the current
scaled origin"
  `(with-window-system-dependent-clipping (0 0 ,wid ,hei . ,args) . ,body))

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
        (setf (needs-update model) t)
        (setf (slot-value model 'cur-tick) tick)
        (reset-meshes model)))
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

(defmethod get-graphics-canvas-for-screen-obj ((self screen-obj) &optional wid hei)
  (let ((paint-tex (getprop self :graphics-canvas)))
    (cond
      ((null paint-tex)
       (putprop self (make-graphics-canvas wid hei) :graphics-canvas)
       (setf paint-tex (getprop self :graphics-canvas))
       (enable paint-tex)
       (gl:clear-color 0.0 0.0 0.0 0.0) ;; transparent
       (gl:clear :color-buffer-bit :depth-buffer-bit)
       (gl:enable :line-smooth)
       (gl:enable :polygon-smooth)
       (gl:enable :blend)
       (gl:enable :multisample)
       (gl:enable :depth-test)
       (gl:blend-func :src-alpha :one-minus-src-alpha)
       (gl:hint :line-smooth-hint :nicest)
       (gl:hint :polygon-smooth-hint :nicest)
       (disable paint-tex))
      ((or (not (equal wid (ogl-pixmap-width (graphics-canvas-pixmap paint-tex))))
           (not (equal hei (ogl-pixmap-height (graphics-canvas-pixmap paint-tex)))))
       (resize paint-tex wid hei))
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
