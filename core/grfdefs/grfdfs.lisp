;; -*- Mode:LISP; Syntax:Common-Lisp;Package:BOXER;-*-
#|


 $Header: grfdfs.lisp,v 1.0 90/01/24 22:12:30 boxer Exp $

 $Log:	grfdfs.lisp,v $
;;;Revision 1.0  90/01/24  22:12:30  boxer
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

    This contains all of the Interface between Graphics sheets
    and the rest of the  BOXER Editor.  The functions and methods
    which manipulate pixels (as opposed  to graphics objects) can
    also be found here  In particular, the functions which are
    used to draw lines, regions, etc are here.


Modification History (most recent at top)

 1/29/13 removed fixnum math:drawing-on-turtle-slate, draw-wrap-line, with-graphics-screen-parameters-once
 1/29/08 added check for %private-graphics-list to with-graphics-vars-bound-internal
 5/11/06 quick conversion of graphics-sheet to graphics-info will have to come back
         and do this right later, but this should allow us to bootstrap opengl port
10/29/03 removed *sprite-drawing-flush* and (flush-port-buffer)
 9/05/03 #+carbon-compat (flush-port-buffer) in with-sprite-primitive-environment
 2/15/03 merged current LW and MCL files
 2/15/01 merged current LW and MCL files
11/06/99 save/restore-under-turtle checks for and hacks clip mode
 6/29/99 save/restore-under-turtle now handles wrapping
 6/30/98 changed caching scheme for get-visible-screen-objs
 6/30/98 started logging changes: source = Boxer version 2.3

|#

(in-package :boxer)

;;; drawing defs

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar %bit-array 'i-bet-you-forgot-to-bind-%bit-array
  "The bit-array of the graphics-box being operated on")

(defvar %drawing-width 'i-bet-you-forgot-to-bind-%drawing-width
  "The width of the bit-array of the graphics box in which we are allowed to draw")

(defvar %drawing-height 'i-bet-you-forgot-to-bind-%drawing-heigth
  "The height of the bit-array of the graphics box in which we are allowed to draw")

(defvar %graphics-box 'i-bet-you-forgot-to-bind-%graphics-box
  "The graphics box which is being operated on.")

(defvar %graphics-sheet 'i-bet-you-forgot-to-bind-%graphics-sheet
  "The graphics box which is being operated on.")

(defvar %draw-mode 'i-bet-you-forgot-to-bind-%draw-mode
  "Draw-mode of the graphics box in which we are allowed to draw")

(defvar %graphics-list 'i-bet-you-forgot-to-bind-%graphics-list
  "graphics-list of the graphics box in which we are allowed to draw")

(defvar %drawing-half-width 150.0)

(defvar %drawing-half-height 100.0)

(defmacro no-graphics? () '(eq %graphics-box :no-graphics))

) ; eval-when

;;; if drawing with more than one sprite becomes common, it may be worthwhile
;;; to optimize the macrolet'd with-sprites-hidden to only hide all the
;;; sprites once per graphics-box instead of once per turtle...

(defun graphics-sheet-from-box (box)
  (cond ((eq box ':no-graphics) nil)
        ((virtual-copy? box) (graphics-info-graphics-sheet (vc-graphics box)))
        (t (graphics-info box))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-graphics-vars-bound-internal (gr-sheet &body body)
  `(let ((%graphics-sheet ,gr-sheet)
         (%bit-array nil)
         (%drawing-width 0)
         (%drawing-height 0)
         ;; the command list
         (%graphics-list %learning-shape-graphics-list)
         ;; someday, maybe this will be useful
         (%draw-mode nil))
     (declare (fixnum %drawing-width %drawing-height))
     ;; should these be floats ???
     (let ((%drawing-half-width 0.0)
           (%drawing-half-height 0.0))
         (unless (null ,gr-sheet)
           ;; set the variables if it is possible to do so.  We
           ;; bind them and set them separately in order to centralize
           ;; this check
           (setq %bit-array (graphics-sheet-bit-array ,gr-sheet)
                 %drawing-width (graphics-sheet-draw-wid ,gr-sheet)
                 %drawing-height (graphics-sheet-draw-hei ,gr-sheet)
                 %graphics-list (or %learning-shape-graphics-list
                                    %private-graphics-list
                                    (graphics-sheet-graphics-list ,gr-sheet)
                                    (let ((new-gl(make-graphics-command-list)))
                                      (setf (graphics-sheet-graphics-list
                                             ,gr-sheet)
                                            new-gl)
                                      new-gl))
                 %draw-mode (graphics-sheet-draw-mode ,gr-sheet)
                 %drawing-half-width (/ %drawing-width 2.0)
                 %drawing-half-height (/ %drawing-height 2.0)))
         ;; now do the rest
         . ,body)))

(defmacro with-graphics-vars-bound ((to-box &optional (gr-sheet (gensym)))
                                    &body body)
  "This macro sets up an environment where commonly used
parameters of the graphics box are bound. "
  `(let ((,gr-sheet (graphics-sheet-from-box ,to-box))
         ;; sometimes it is convenient to be able to access the box
         (%graphics-box ,to-box))
     (with-graphics-vars-bound-internal ,gr-sheet . ,body)))
) ; eval-when

;;; Set Up Clipping and Offsets

(defun make-graphics-sheet (wid hei &optional box)
  (let ((new-gs (%make-graphics-sheet-with-graphics-list wid hei box)))
    (setf (graphics-sheet-graphics-list new-gs) (make-graphics-command-list))
    new-gs))

(defun make-graphics-sheet-with-graphics-list (wid
                                               hei
                                               &optional
                                               box
                                               (sheet
                                                (make-graphics-command-list)))
  (let ((new-gs (%make-graphics-sheet-with-graphics-list wid hei box)))
    (setf (graphics-sheet-graphics-list new-gs) sheet)
    new-gs))

(defun make-graphics-screen-sheet (actual-obj
                                   &optional (x-offset 0.) (y-offset 0.))
  (let ((togo (make-instance 'graphics-screen-sheet)))
    (setf (screen-obj-x-offset togo) x-offset)
    (setf (screen-obj-y-offset togo) y-offset)
    togo))

(defun set-graphics-screen-sheet-x-offset (graphics-screen-sheet new-x-offset)
  (setf (screen-obj-x-offset graphics-screen-sheet) new-x-offset))

(defun set-graphics-screen-sheet-y-offset (graphics-screen-sheet new-y-offset)
  (setf (screen-obj-y-offset graphics-screen-sheet) new-y-offset))

;;accessors for graphics boxes

(defmethod bit-array ((bg box))
  (let ((gs (slot-value bg 'graphics-info)))
    (and gs (graphics-sheet-bit-array gs))))

(defmethod graphics-sheet ((gb box))
  (slot-value gb 'graphics-info))

(defun drawing-width (graphics-sheet)
  ;; Returns the width of the area of a bit-array for a graphics
  ;; box.  Note that this doesn't have to be = to
  ;; ARRAY-DIMENSION-N because of BITBLT's multiple of 32.
  ;; requirement on the LispM
  (graphics-sheet-draw-wid graphics-sheet))

(defun drawing-height (graphics-sheet)
  (graphics-sheet-draw-hei graphics-sheet))

;; accessor methods for bit-arrays via graphics boxes
(defmethod bit-array-wid ((self box))
  (let ((gs (slot-value self 'graphics-info)))
    (and gs (graphics-sheet-draw-wid gs))))

(defmethod bit-array-hei ((self box))
  (let ((gs (slot-value self 'graphics-info)))
    (and gs (graphics-sheet-draw-hei gs))))

(defmethod graphics-sheet-size ((self box))
  (let ((graphics-sheet (slot-value self 'graphics-info)))
    (unless (null graphics-sheet)
      (values (graphics-sheet-draw-wid graphics-sheet)
              (graphics-sheet-draw-hei graphics-sheet)))))

(defmethod draw-mode ((self box))
  (let ((gs (slot-value self 'graphics-info)))
    (and gs (graphics-sheet-draw-mode gs))))

(defmethod set-draw-mode ((self box) new-mode)
  (setf (graphics-sheet-draw-mode (slot-value self 'graphics-info)) new-mode))

(defmethod graphics-object-list ((self box))
  (let ((gs (slot-value self 'graphics-info)))
    (and gs (graphics-sheet-object-list gs))))

(defmethod graphics-graphics-list ((self box))
  (let ((gs (slot-value self 'graphics-info)))
    (when gs (graphics-sheet-graphics-list gs))))

;; returns a list of all the graphics views of the graphics-box

(defun get-visible-screen-objs (graphics-box)
  (unless (null graphics-box)
    (let ((sbs nil))
      ;; first check the Box proper
      (dolist (sb (displayed-screen-objs graphics-box))
        (when (graphics-screen-box? sb)
          (push sb sbs)))
      ;; then check any ports to the box
      (dolist (port (ports graphics-box))
        (dolist (sb (displayed-screen-objs port))
          (when (graphics-screen-box? sb)
            (push sb sbs))))
      sbs)))

;;;; Sprite Access

;;; this is the sprite scoping function.  Basically, we walk up the static
;;; hierarchy looking for either a sprite box or a sprite-cache (see how
;;; TALK-TO works to understand sprite caches)

;; this is dependent on the evaluator internals
(defsubst static-root ()
  boxer-eval::*lexical-variables-root*)

;; defined in vars.lisp
;; (defvar *current-sprite* nil)

;;; No more WHO mechanism.
(defun get-sprites ()
  (or *current-sprite*
      (do ((box (static-root) (when (box? box) (superior-box box))))
          ((or (null box) (sprite-box? box) (and (virtual-copy? box)
                                                 (eq (vc-type box)
                                                     'sprite-box)))
           box)
        (let ((as (get-active-sprite box)))
          (unless (null as) (return as))))))

(defun get-graphics-box ()
  (do ((box (static-root) (when (box? box) (superior-box box))))
      ((or (null box) (graphics-box? box)
           (and (virtual-copy? box) (getf (vc-graphics box) 'graphics-sheet)))
       box)
    ))

;; stubware(tm)
;; supposed to look for active sprite INSIDE of vc
(defun get-vc-active-sprite (vc)
  (declare (ignore vc))
  nil)

(defun get-active-sprite (box)
  (if (virtual-copy? box)
      (get-vc-active-sprite box)
      (getprop box 'active-sprite)))


;; this understands virtual copies of sprite boxes
(defun get-sprite-turtle (sb)
  (cond ((sprite-box? sb) (slot-value sb 'graphics-info))
         ((and (virtual-copy? sb)) ; (eq (vc-type sb) 'sprite-box))
          (graphics-info-turtle (vc-graphics sb)))))

(defun get-graphics-box-from-sprite-box (sb)
  (let* ((turtle (get-sprite-turtle sb))
         (gb (and turtle (assoc-graphics-box turtle))))
    (cond ((not (null gb)) gb)
          ((null turtle)
           (error "The Sprite Box, ~A, has no turtle" sb))
          ((null gb) ':no-graphics)
          (t (error "Can't get a Graphics Box out of ~S" sb)))))

;;; this is used by prims that implicitly apply to either sprites or
;;; graphics boxes such as CS or GRAPHICS-MODE
(defun get-relevant-graphics-box ()
  (if (not (null *current-sprite*))
      (get-graphics-box-from-sprite-box *current-sprite*)
      (do ((box (static-root) (when (box? box) (superior-box box))))
          ((null box) nil)
        (cond ((or (sprite-box? box) (and (virtual-copy? box) (eq (vc-type box) 'sprite-box)))
               (return (values (get-graphics-box-from-sprite-box box) box)))
              ((or (graphics-box? box) (and (virtual-copy? box)
                                            (getf (vc-graphics box) 'graphics-sheet)))
               (return box))
              (t (let ((as (get-active-sprite box)))
                   (unless (null as)
                     (return (values (get-graphics-box-from-sprite-box as) as)))))))))


;;; ALL TURTLE functions are assumed to be called in an environment where the
;;; various turtle state variables as well as GRAPHICS vars (like BIT-ARRAY)
;;; are BOUND. This is what the macro WITH-GRAPHICS-VARS-BOUND is used for.
;;; The top level interface is DEFSPRITE-FUNCTION which dispatches the sprite
;;; function to the appropriate sprite(s).  The body of the function can assume
;;; that there is a sprite-box bound to sprite-var

;;; Cocoa drawing: sprites dont draw onto the screen, they just update the graphics
;;; list and periodically, the GB box space, on the screen is cleared, the
;;; graphics list is blasted out and then the sprites are redrawn (any existing
;;; background can also be draw first)
(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-sprite-primitive-environment ((sprite-var turtle-var
                                                         &optional
                                                         no-sprite-error
                                                         (gboxvar (gensym)))
                                             &body body)
  `(let ((active-sprites (get-sprites)))
     (cond ((null active-sprites)
            ,(if (null no-sprite-error)
                 '(boxer-eval::signal-error :sprite-error "Don't have a Sprite to Talk to")
                 'boxer-eval::*novalue*))
           (t
            (let* ((,sprite-var active-sprites)
                   (,turtle-var (get-sprite-turtle ,sprite-var))
                   (,gboxvar (get-graphics-box-from-sprite-box ,sprite-var)))
              ;; get rid of bound but never used warnings
              ,sprite-var ,turtle-var
              (with-graphics-vars-bound (,gboxvar sheet)
                  (prog1 (with-graphics-state (%graphics-list)
                             (update-graphics-state ,turtle-var)
                           ,@body))))))))

(defmacro defsprite-function (name-descriptor arglist (sprite-var turtle-var)
                              &body body)
  `(boxer-eval::defboxer-primitive ,name-descriptor ,arglist
     (with-sprite-primitive-environment (,sprite-var ,turtle-var)
        . ,body)))

(defmacro defsprite-trigger-function (name-descriptor arglist
                                                      (sprite-var turtle-var)
                                                      &body body)
  `(boxer-eval::defboxer-primitive ,name-descriptor ,arglist
     (with-sprite-primitive-environment (,sprite-var ,turtle-var t)
        . ,body)))
) ; eval-when


;;; This is used by sprite update functions when they are passed an illegal arg
;;; In general, we assume some value, tell the user and then bash whatever
;;; losing thing he may have put into the box
;;; In the technical vastness of the future, we may want this to do something
;;; that is more Boxer Like

(defmacro sprite-update-warning (format-string &rest format-args)
;;  `(warn ,format-string . ,format-args)
  ;;be more user viewable...
  `(boxer-editor-warning ,format-string . ,format-args))

;;;; Some useful variables

(defvar %mouse-usurped nil "Used in move-to to prevent changing boxes")

;; defined in vars.lisp
;; (defvar %learning-shape? nil "This is t when doing a set-shape")
;; (defvar %turtle-state nil
;;         "where to save a turtle's position, pen, and heading. ")

