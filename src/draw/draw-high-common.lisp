; -*- Mode:LISP; Syntax: Common-Lisp; Package:BOXER; Base:8.-*-
#|


 $Header: draw-high.lisp,v 1.0 90/01/24 22:09:54 boxer Exp $


 $Log:	draw-high.lisp,v $
;;;Revision 1.0  90/01/24  22:09:54  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

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

;;; WITH-FONT-MAP-BOUND is meant to be used by all those functions
;;; (like BOX-BORDER-FN's that have to be called in an environment where the
;;; font map is supposed to be bound but nothing else (like all those
;;; wonderful drawing type things and stuff) needs to be bound

(defmacro with-font-map-bound ((window) &body body)
  `(let ()
     . ,body))


;;; Used instead of DRAWING-ON-WINDOW for bitmaps

(defmacro drawing-on-bitmap ((bitmap) &body body)
  (let ((bwidth-var (gensym)) (bheight-var (gensym)))
    `(with-font-map-bound (*boxer-pane*)
       (let ((%drawing-window ,bitmap) (%drawing-array ,bitmap)
             (,bwidth-var (offscreen-bitmap-width ,bitmap))
             (,bheight-var (offscreen-bitmap-height ,bitmap)))
         %drawing-window %drawing-array ; bound but never used errors....
         (drawing-on-window-bootstrap-clipping-and-scaling
           (0 0 ,bwidth-var ,bheight-var)
           (with-system-dependent-bitmap-drawing (,bitmap ,bwidth-var ,bheight-var)
	     . ,body))))))

;;; Drawing functions which don't respect the clipping environment.
;;; They expect the hardware to do the clipping.  These are mostly
;;; used by sprite graphics and should NOT be used in the redisplay
;;; without careful consideration

(defun draw-point (alu x y) (%draw-point x y alu %drawing-window))

(defun draw-arc (alu x y wid hei start-angle sweep-angle)
  (%draw-arc %drawing-window alu (scale-x x) (scale-y y)
	     wid hei start-angle sweep-angle))


(defun draw-filled-arc (alu x y wid hei start-angle sweep-angle)
  (%draw-filled-arc %drawing-window alu (scale-x x) (scale-y y)
		    wid hei start-angle sweep-angle))

;; should'nt transform the points because translation is done @ hardware level in OpenGL
(defun draw-poly (alu points)
  (unless (null points)
    (%draw-poly (boxer-points->window-system-points points (x x) (y y))
		alu %drawing-window)))

;;; +++ maybe these are supposed to be the same, maybe not
(defvar char-bits-limit 4096)


;; Support for displaying control characters
(DEFVAR *CONTROL-CHARACTER-PREFIX-TABLE* (MAKE-ARRAY CHAR-BITS-LIMIT
						     :ELEMENT-TYPE 'CHARACTER
						     :INITIAL-ELEMENT #\^))

(DEFUN CONTROL-CHARACTER-DISPLAY-PREFIX (BITS)
  (AREF *CONTROL-CHARACTER-PREFIX-TABLE* BITS))

