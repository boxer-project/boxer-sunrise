;; -*- Mode:LISP; Syntax:Common-Lisp;Package:BOXER;-*-
#|


 $Header: grobjs.lisp,v 1.0 90/01/24 22:12:46 boxer Exp $

 $Log:	grobjs.lisp,v $
;;;Revision 1.0  90/01/24  22:12:46  boxer
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


 Graphics Object Definitions
 Coordinate Transformation and Drawing Utilities
 Also mouse-sensitivity code.



Modification History (most recent at top)

 2/11/13 moved redisplay init to gdispl.lisp to get dependencies correct
 1/28/08 added private graphics list slot to turtle class
 3/20/07 add/remove-graphics-object changed to handle graphics-info paradigm
 3/05/07 *default-turtle-shape* is now PD not XOR
 2/26/01 array-coordinate-x,y: added float coercion
 5/06/98 started logging: source = Boxer version 2.3

|#

(in-package :boxer)

(defvar *default-graphics-box-width* 300.)

(defvar *default-graphics-box-height* 200.)


(defvar *default-graphics-object-class* 'turtle)

(defvar *default-graphics-object-size* 15.0)

(defvar *default-graphics-object-shape*)

(defvar *turtle-height* *default-graphics-object-size*)

(defvar *turtle-half-base* 5.0)

(defvar *default-turtle-shape*
    #(#(
        #(32 2)
        #(33 1)
        #(35 -5.0 -5.0 5.0 -5.0)
        #(35 5.0 -5.0 0.0 10.0)
        #(35 0.0 10.0 -5.0 -5.0) NIL NIL NIL
       ) 5 NIL 2 1 49 NIL NIL)) ;;#<Pointer to type :LISP-SINGLE-FLOAT = #x007163A0> NIL)

;;; The actual def-redisplay-initialization moved to gdispl.lisp
;;; for ordering reasons
#|
;;; This has to be a redisplay init because make-turtle-shape
;;; depends upon the runtime value of *foreground-color* which
;;; is not defined until AFTER the windows are created for some
;;; window systems (like CLX)
(def-redisplay-initialization ; :turtle-shape
    (setq *default-graphics-object-shape*
	  (let ((%graphics-list (make-turtle-shape 8))
		(*graphics-command-recording-mode* ':boxer))
            (record-boxer-graphics-command-change-alu alu-seta)
	    (record-boxer-graphics-command-change-pen-width 1)
	    (record-boxer-graphics-command-centered-rectangle
	     0.0 0.0
	     *default-graphics-object-size* *default-graphics-object-size*)
	    %graphics-list)
	  *default-turtle-shape*
	  (let ((%graphics-list (make-turtle-shape 8))
		(*graphics-command-recording-mode* ':boxer))
            (record-boxer-graphics-command-change-alu alu-seta)
	    (record-boxer-graphics-command-change-pen-width 1)
	    ;; the base line
	    (record-boxer-graphics-command-line-segment
	     (- *turtle-half-base*) (- (/ *turtle-height* 3.0))
	     *turtle-half-base* (- (/ *turtle-height* 3.0)))
	    ;; the right side
	    (record-boxer-graphics-command-line-segment
	     *turtle-half-base* (- (/ *turtle-height* 3.0))
	     0.0 (* 2 (/ *turtle-height* 3)))
	    ;; the left side
	    (record-boxer-graphics-command-line-segment
	     0.0 (* 2 (/ *turtle-height* 3))
	     (- *turtle-half-base*) (- (/ *turtle-height* 3.0)))
	    %graphics-list)))

|#



;;;;; GRAPHICS Objects

;;;; The Basic Abstract Graphics Object
;;;
;;; Contains slots necessary to implement basic graphics functionality
;;;
;;; . A position (in a graphics box)
;;;
;;; . Pointers to the Box representation, and to the containing graphics box
;;;
;;; . Slots to implement nesting in the Boxer hierarchy
;;;

(eval-when (load compile eval)
(defclass graphics-object
    ()
  ((x-position         :initform (%make-vv-box-interface 0.0 'x-position))
   (y-position         :initform (%make-vv-box-interface 0.0 'y-position))
   (subsprites         :initform nil :accessor subsprites)
   (superior-turtle    :initform nil :accessor superior-turtle)
   (sprite-box         :initform nil :accessor sprite-box)
   (assoc-graphics-box :initform nil :accessor assoc-graphics-box))
  (:metaclass block-compile-class)
  ;; (:abstract-class t)
  (:documentation
   "Bare minimum for a graphics object, slots for box interface and position"))
)

(defgeneric graphics-object? (x) (:method (x) nil) (:method ((x graphics-object)) t))

;;;; Minimal Instantiable Graphics Object
;;;
;;; . Slots to cache information for a shape
;;;

(defclass button
    (graphics-object)
  ((shape      :initform nil)
   (save-under :initform nil
	       :accessor turtle-save-under)
   (window-shape :initform nil
		 :accessor turtle-window-shape))
  (:metaclass block-compile-class))

;;;; This has the capability to draw lines when it moves

(defclass graphics-cursor
    (button)
  ((shown?    :initform (%make-iv-box-interface T 'shown?))
   (pen       :initform (%make-iv-box-interface 'bu::down 'pen))
   (pen-width :initform (%make-vv-box-interface 1 'pen-width))
   (pen-color :initform (%make-sv-box-interface
			 *foreground-color* 'pen-color
			 nil 'pen-color-box-updater))
   (type-font :initform (%make-iv-box-interface
			 *sprite-type-font-no* 'type-font)))
  (:metaclass block-compile-class))


;;;; Our friend the turtle...

(defclass turtle
    (graphics-cursor)
  ((heading       :initform (%make-vv-box-interface 0 'heading))
   (home-position :initform (%make-iv-box-interface '(0.0 0.0) 'home-position))
   (sprite-size   :initform (%make-vv-box-interface 1.0 'sprite-size))
   (private-gl    :initform (make-graphics-command-list))
   )
  (:metaclass block-compile-class))


;;;; block compile metaclass epilogue

;; (block-compile-epilogue graphics-object)

;; (block-compile-epilogue button)

;; (block-compile-epilogue graphics-cursor)

;; (block-compile-epilogue turtle)






(defun make-turtle ()
  (make-instance 'turtle))

;; soon to be '(xor-redraw overlay)
(defvar *valid-save-under-keywords* '(xor-redraw))

(defstruct (save-under (:constructor make-save-under (bitmap middle size)))
  (bitmap nil)
  (middle 0)
  (size))


;; sgithens TODO, for some reason this complains that graphics-object is unbound...
;; (deftype-checking-macros graphics-object "A Graphics Object")
;; (deftype-checking-macros button "A Graphics Button")
;; (deftype-checking-macros graphics-cursor "A Graphics Cursor")
;; (deftype-checking-macros turtle "A Turtle")

;;; Some useful variables that various types of objects need

(defconstant *default-graphics-object-height* 10.0)

(defconstant *default-graphics-object-width* 10.0)

;;; adding and removing graphics-objects to/from GRAPHICS-BOXES

(defmethod add-graphics-object ((self box) new-object)
  (let ((gi (slot-value self 'graphics-info)))
    (cond ((graphics-sheet? gi)
           (set-assoc-graphics-box new-object self)
           (push new-object (graphics-sheet-object-list gi)))
          ((graphics-object? gi)
           (add-subturtle gi new-object)))))

(defmethod remove-graphics-object ((self box) old-object)
  (let ((gi (slot-value self 'graphics-info)))
    (cond ((graphics-sheet? gi)
           (when (eq (assoc-graphics-box old-object) self)
             (set-assoc-graphics-box old-object nil)
             (setf (graphics-sheet-object-list gi)
                   (fast-delq old-object (graphics-sheet-object-list gi)))))
          ((graphics-object? gi)
           (remove-subturtle gi old-object)))))


;;; coordinate transformations.
;;;
;;; ARRAY coordinates are referenced to the indices of the bit-array
;;; of the graphics box. Therefore in ARRAY coordinates, (0, 0) is in
;;;  the upper-left hand corner whereas...
;;; ...in USER coordinates, which refer to the coordinates in which the
;;; user talks to the object, (0, 0) will be more or less in the middle
;;; of the box.

;;; USER ==> ARRAY

(defun fix-array-coordinate-x (user-x)
  (values (floor (the boxer-float (array-coordinate-x user-x)))))

(defun array-coordinate-x (user-x)
  (float-plus %drawing-half-width (float user-x)))

(defun fix-array-coordinate-y (user-y)
  (values (floor (the boxer-float (array-coordinate-y user-y)))))

(defun array-coordinate-y (user-y)
  (float-minus %drawing-half-height
	       (float user-y) ; (* user-y *scrunch-factor*)
     ))


;;; ARRAY ==> USER

(defun user-coordinate-x (array-x)
  (float-minus (float array-x) %drawing-half-width))

(defun user-coordinate-y (array-y)
  ;(/ (- %drawing-half-height array-y) *scrunch-factor*)
  (float-minus %drawing-half-height (float array-y)))

(defun user-coordinate-fix-x (array-x)
  (declare (fixnum array-x))
  (float-minus (float array-x) %drawing-half-width))

(defun user-coordinate-fix-y (array-y)
  (declare (fixnum array-y))
  ;(/ (- %drawing-half-height array-y) *scrunch-factor*)
  (float-minus %drawing-half-height (float array-y)))


;;; these want ARRAY coordinates

(defun point-in-array? (x y)
  (and (x-in-array? x)
       (y-in-array? y)))

(defsubst x-in-array? (x)
  (and (>=& x 0) (<& x %drawing-width)))

(defsubst y-in-array? (y)
  (and (>=& y 0) (<& y %drawing-height)))



;;; normalize coordinates to the on screen position

(defun wrap-object-coords (object)
  (setf (slot-value object 'x-position)
	(wrap-x-coordinate (slot-value object 'x-position)))
  (setf (slot-value object 'y-position)
	(wrap-y-coordinate (slot-value object 'y-position))))

;;; Lucid (lcl3.0) specific versions try are written to
;;; minimize floating point CONSing

#-lcl3.0
(defun wrap-x-coordinate (user-x)
  (user-coordinate-x (float-modulo (array-coordinate-x user-x)
				   %drawing-width)))


;;; We go through these contortions in order to reduce
;;; the floating point CONSing.  Using this version seems to
;;; reduce the FP consing from about 8 DP-FP numbers per call to 2
#+lcl3.0
(defun wrap-x-coordinate (user-x)
  (let ((float-temp 0.0) (float-width (float (the fixnum %drawing-width))))
    (declare (float float-temp float-width))
    (setq float-temp (float-plus %drawing-half-width user-x))
    (float-minus (if (and (plusp float-temp) (< float-temp float-width))
		     float-temp
		     (let ((scratch 0.0))
		       (declare (float scratch))
		       (setq scratch float-temp)
		       (setq float-temp
			     (values (ffloor float-temp float-width)))
		       (setq float-temp (float-times float-temp float-width))
		       (setq float-temp (float-minus scratch float-temp))
		       (if (minusp float-temp)
			   (float-plus float-temp float-width)
			   float-temp)))
		 %drawing-half-width)))

#-lcl3.0
(defun wrap-y-coordinate (user-y)
  (user-coordinate-y (float-modulo (array-coordinate-y user-y)
				   %drawing-height)))

;;; We go through these contortions in order to reduce
;;; the floating point CONSing.  Using this version seems to
;;; reduce the FP consing from about 8 DP-FP numbers per call to 2
#+lcl3.0
(defun wrap-y-coordinate (user-y)
  (let ((float-temp 0.0) (float-height 0.0))
    (declare (float float-temp float-height))
    (setq float-height (float (the fixnum %drawing-height)))
    (setq float-temp (float-minus %drawing-half-height user-y))
    (float-minus %drawing-half-height
		 (if (and (plusp float-temp) (< float-temp float-height))
		     float-temp
		     (let ((scratch 0.0))
		       (declare (float scratch))
		       (setq scratch float-temp)
		       (setq float-temp
			     (values (ffloor float-temp float-height)))
		       (setq float-temp (float-times float-temp float-height))
		       (setq float-temp (float-minus scratch float-temp))
		       (if (minusp float-temp)
			   (float-plus float-temp float-height)
			   float-temp))))))

(defun float-modulo (num mod)
  (let ((fmod (float mod)))
    (if (and (plusp num) (< num fmod))
	num
	(let ((x (- num (* (floor num fmod) fmod))))
	  (if (minusp x) (+ x fmod) x)))))

