; -*- Mode:LISP; Syntax: Common-Lisp; Package:BOXER; Base:8.-*-
#|


 $Header: draw-high.lisp,v 1.0 90/01/24 22:09:54 boxer Exp $


 $Log:	draw-high.lisp,v $
;;;Revision 1.0  90/01/24  22:09:54  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



   This file contains the low level drawing primitives which are
   machine independent.  This file is meant to coexist with various
   "xxx-draw-low" files which are the machine specific primitives.

   Only the primitives and macros which do not respect clipping are
   in this file.  The files draw-high-software/hardware-clip.lisp
   contain the low level functions and macros (predominately used
   in the redisplay) which pay attention to the clipping state.



|#

(in-package :boxer)

;; This version of the draw-high file contains software clipping
;; versions of drawing primitives used in the redisplay


;; Make an array of the same type as the screen array.  Args are screen,
;; width and height

;;;; DRAWING-ON-WINDOW

;;; DRAWING-ON-WINDOW is an &body macro which all the drawing macros in this
;;; must be called inside of. It basically prepares the window to be drawn on
;;; and binds all the magic variables that the drawing macros need including
;;; the bootstrapping of the clipping and coordinate scaling variables.

(defmacro drawing-on-window ((window) &body body)
  (once-only (window)
    `(prepare-sheet (,window)
       (drawing-on-window-without-prepare-sheet (,window) . ,body))))

;;; DRAWING-ON-WINDOW-WITHOUT-PREPARE-SHEET is a variant of Drawing-On-Window
;;; which does everything Drawing-On-Window does except that it does not do a
;;; PREPARE-SHEET of the window. Unless you really know what you are doing
;;; you should only use this inside the :BLINK method for a blinker.

(defmacro drawing-on-window-without-prepare-sheet ((window) &body body)
  (once-only (window)
    `(let ((%drawing-window ,window)
           (%drawing-array (sheet-screen-array ,window)))
       %drawing-window %drawing-array    ;bound but never...
       (drawing-on-window-bootstrap-clipping-and-scaling
         ((sheet-inside-left ,window) (sheet-inside-top  ,window)
          (sheet-inside-width ,window) (sheet-inside-height ,window))
          . ,body))))


;;; Used instead of DRAWING-ON-WINDOW for bitmaps

(defmacro drawing-on-bitmap ((bitmap) &body body)
  (let ((bwidth-var (gensym)) (bheight-var (gensym)))
    `(let ((%drawing-window ,bitmap) (%drawing-array ,bitmap)
             (,bwidth-var (offscreen-bitmap-width ,bitmap))
             (,bheight-var (offscreen-bitmap-height ,bitmap)))
         %drawing-window %drawing-array ; bound but never used errors....
         (drawing-on-window-bootstrap-clipping-and-scaling
           (0 0 ,bwidth-var ,bheight-var)
           (with-system-dependent-bitmap-drawing (,bitmap ,bwidth-var ,bheight-var)
       . ,body)))))

;;; Drawing functions which don't respect the clipping environment.
;;; They expect the hardware to do the clipping.  These are mostly
;;; used by sprite graphics and should NOT be used in the redisplay
;;; without careful consideration

(defun draw-point (x y)
  (%draw-point x y))

(defun draw-arc (alu x y wid hei start-angle sweep-angle)
  (%draw-arc %drawing-window alu (scale-x x) (scale-y y)
             wid hei start-angle sweep-angle))


(defun draw-filled-arc (alu x y wid hei start-angle sweep-angle)
  (%draw-filled-arc %drawing-window alu (scale-x x) (scale-y y)
                    wid hei start-angle sweep-angle))

;; should'nt transform the points because translation is done @ hardware level in OpenGL
(defun draw-poly (points)
  (unless (null points)
    (%draw-poly (boxer-points->window-system-points points (x x) (y y)))))

;;;; Scaling and Clipping Macros

;;; origin gets reset in hardware by scaling macros so these are no ops
;;; They need to be defined because other functions (usually sprite graphics)
;;; will use them explicitly to convert coords.

(defmacro scale-x (x) x)
(defmacro scale-y (y) y)

;;; Macros from draw-high, slightly altered

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
;     ;; **** since we are letting the hardware do the clipping, be sure
;     ;; to include the forms that invoke the hardware
;     (unwind-protect
;         (progn (window-system-dependent-set-origin %origin-x-offset
;                                                    %origin-y-offset)
                ,@body))
;      ;; return to some canonical state
;       (window-system-dependent-set-origin 0 0))))


;; **** this is the reverse of the software version because the
;; WITH-CLIPPING-INSIDE macro should use the new coordinate system
;; set by WITH-ORIGIN-AT
(defmacro with-drawing-inside-region ((x y wid hei) &body body)
  `(with-origin-at (,x ,y)
     (with-clipping-inside (0 0 ,wid ,hei)
       . ,body)))

;; **** changed, see draw-low-mcl for details...
;; Opengl set-origin is RELATIVE !
(defmacro with-origin-at ((x y) &body body)
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

;; **** changed, see draw-low-mcl for details...
(defmacro with-clipping-inside ((x y wid hei) &body body)
  `(with-window-system-dependent-clipping (,x ,y ,wid ,hei) . ,body))

;; do we need to readjust the clip region here ????
(defmacro with-scrolling-origin ((scroll-x scroll-y) &body body)
  `(with-origin-at (,scroll-x ,scroll-y)
     . ,body))

;;; This MUST use the hardware clipping regardless of speed.
;;; It is used only around bodies which do sprite graphics
;;; so the frequency of use is MUCH less than it is in the redisplay
;;;
;;; this adjusts the clipping to be width and height AT the current
;;; scaled origin
;;;
(defmacro with-turtle-clipping ((wid hei . args) &body body)
  `(with-window-system-dependent-clipping (0 0 ,wid ,hei . ,args) . ,body))

;;; Drawing functions

;;; sure there were no intervening clipping operations.

(defun draw-line (x0 y0 x1 y1)
  (%draw-line x0 y0 x1 y1))

(defun draw-rectangle (w h x y)
  (%draw-rectangle w h x y))

(defun erase-rectangle (w h x y)
  (%erase-rectangle w h x y %drawing-window))

;; useful for debugging erase-rectangle lossage
(defun flash-rectangle (w h x y)
  (dotimes (i 6)
    (%draw-rectangle w h x y)
    (sleep .1)))

(defun bitblt-to-screen (wid hei from-array from-x from-y to-x to-y)
  (%bitblt-to-screen wid hei from-array from-x from-y to-x to-y))

(defun bitblt-from-screen (wid hei to-array from-x from-y to-x to-y)
  (%bitblt-from-screen wid hei to-array from-x from-y to-x to-y))

;; NOTE: in the new multi font world, draw-cha needs to draw at the char's
;; baseline rather than the top left corner.  This is because in a multifont
;; row, the common reference point will be the baseline instead of the top
;; edge
(defun draw-cha (char x y)
  (%draw-cha x y char))

(defun draw-string (font-no string region-x region-y)
  (%draw-string font-no string region-x region-y))
