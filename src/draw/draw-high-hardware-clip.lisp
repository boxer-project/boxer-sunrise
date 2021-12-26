;;;;  -*- Mode:LISP; Syntax: Common-Lisp; Package:BOXER; Base:8.-*-
;;;;
;;;;     $Header$
;;;;
;;;;     $Log$
;;;;
;;;;      Boxer
;;;;      Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
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
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;
;;;;
;;;;     This file contains the low level drawing primitives for the REDISPLAY
;;;;     which are machine independent but expect to do any clipping in software.
;;;;     The clipping calculations are done BEFORE any drawing and only unclipped
;;;;     parts are actually drawn.
;;;;
;;;;     The complement of this file is the the draw-high-software-clipping.lisp
;;;;
;;;;     All window coordinate parameters in this file are "local".  That is
;;;;     they are relative to the containing screen structure (screen-row or
;;;;     screen-box) and should only be called within the proper clipping and
;;;;     scaling macros.
;;;;
;;;;     This file should be used by on top of draw-low-xxx files which
;;;;     support fast hardware clipping.  The redisplay will setup a
;;;;     new clipping environment for EVERY level of box and row.
;;;;
;;;;     It should be possible to recompile the system after changing which
;;;;     draw-high-xxx-clipping.lisp file to use in the boxsys.lisp file
;;;;     to see which version is faster.
;;;;
;;;;     This file is meant to coexist with various
;;;;     "xxx-draw-low" files which are the machine specific primitives.
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   2/11/03 merged current LW and MCL source
;;;;   5/02/01 allow for software clipping in %bitblt ops for LWWIN in bitblt-move-region
;;;;   4/03/01 draw-string now calls %draw-string with explicit parameter %drawing-window
;;;;           this fixes bug where draw-string inside drawing-on-bitmap on PC would
;;;;           draw to the screen instead of the bitmap
;;;;   6/05/00 with-turtle-clipping changed for LW port
;;;;   5/11/98 added comment describing change to interpretation of x,y in draw-cha
;;;;   5/11/98 started logging: source = boxer version 2.3
;;;;

(in-package :boxer)


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

(defun draw-line (x0 y0 x1 y1 alu end-point?)
  (declare (ignore alu end-point?))
  (%draw-line x0 y0 x1 y1))

(defun draw-rectangle (alu w h x y)
  (declare (ignore alu))
  (%draw-rectangle w h x y))

(defun erase-rectangle (w h x y)
  (%erase-rectangle w h x y %drawing-window))

;; useful for debugging erase-rectangle lossage
(defun flash-rectangle (w h x y)
  (dotimes (i 6)
    (%draw-rectangle w h x y)
    (sleep .1)))

(defun bitblt-to-screen (alu wid hei from-array from-x from-y to-x to-y)
  (declare (ignore alu))
  (%bitblt-to-screen wid hei from-array from-x from-y to-x to-y))

(defun bitblt-from-screen (alu wid hei to-array from-x from-y to-x to-y)
  (%bitblt-from-screen alu wid hei to-array from-x from-y to-x to-y))

;; NOTE: in the new multi font world, draw-cha needs to draw at the char's
;; baseline rather than the top left corner.  This is because in a multifont
;; row, the common reference point will be the baseline instead of the top
;; edge
(defun draw-cha (alu char x y)
  (%draw-cha x y char))

(defun draw-string (alu font-no string region-x region-y &optional window)
  (declare (ignore window))
  (%draw-string font-no string region-x region-y))
