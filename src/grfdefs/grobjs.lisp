;;;; -*- Mode:LISP; Syntax:Common-Lisp;Package:BOXER;-*-
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
;;;;                                         +-Data--+
;;;;                This file is part of the | BOXER | system
;;;;                                         +-------+
;;;;
;;;;
;;;;   Graphics Object Definitions
;;;;   Coordinate Transformation and Drawing Utilities
;;;;   Also mouse-sensitivity code.
;;;;
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   2/11/13 moved redisplay init to gdispl.lisp to get dependencies correct
;;;;   1/28/08 added private graphics list slot to turtle class
;;;;   3/20/07 add/remove-graphics-object changed to handle graphics-info paradigm
;;;;   3/05/07 *default-turtle-shape* is now PD not XOR
;;;;   2/26/01 array-coordinate-x,y: added float coercion
;;;;   5/06/98 started logging: source = Boxer version 2.3
;;;;

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

(defclass graphics-object
  ()
  ((x-position         :initform (%make-vv-box-interface 0.0 'x-position))
   (y-position         :initform (%make-vv-box-interface 0.0 'y-position))
   (z-position         :initform (%make-vv-box-interface 0.0 'z-position))
   (subsprites         :initform nil :accessor subsprites)
   (superior-turtle    :initform nil :accessor superior-turtle)
   (sprite-box         :initform nil :accessor sprite-box)
   (assoc-graphics-box :initform nil :accessor assoc-graphics-box))
   (:documentation
    "Bare minimum for a graphics object, slots for box interface and position"))


(defgeneric graphics-object? (x) (:method (x) nil) (:method ((x graphics-object)) t))

;;;; Minimal Instantiable Graphics Object
;;;
;;; . Slots to cache information for a shape
;;;

(defclass button
  (graphics-object)
  ((shape      :initform nil
    :documentation
    "This is a special-value-box-interface vector. The value is a graphics-command-list, the slot
    is this shape slot, and the update function in the 5th vector position is shape-box-updater.

    At the moment, users can update a shape with primitives change, change-graphics, and set-shape.
    These have some slightly odd edge cases. See the following locations for where this happens.
    - gcmeth:          defmethod set-shape
    - recursive-prims: defrecursive-funcall-primitive update-shape
    - grprim3:         defboxer-primitive change-graphics")))

;;;; This has the capability to draw lines when it moves

(defun type-font-box-updater (bi)
  "This is the special-box-interface-update-function for the 'type-font box-interface."
  (let* ((font-no (box-interface-value bi))
         (box (box-interface-box bi))
         (font-name-box (boxer::make-box `((,(font-name font-no))))))
    (bash-box-to-list-value box
                          (list* (font-size font-no)
                                (font-styles font-no)))
    ;; This is a small hack since we can't insert box with the list bashing...
    (insert-cha-at-cha-no (first-inferior-row box) font-name-box 0)
    (flush-editor-box-caches box) (queue-editor-object-for-mutation bi)
))

(defclass graphics-cursor
  (button)
  ((shown?    :initform (%make-iv-box-interface T 'shown?))
   (pen       :initform (%make-iv-box-interface 'bu::down 'pen))
   (pen-width :initform (%make-vv-box-interface 1 'pen-width))
   (pen-color :initform (%make-sv-box-interface
                         *foreground-color* 'pen-color
                         nil 'pen-color-box-updater))
   (type-font :initform (%make-sv-box-interface
                         *sprite-type-font-no* 'type-font
                         nil 'type-font-box-updater))
  ))

;;;; Our friend the turtle...

(defclass turtle
  (graphics-cursor)
  ((heading       :initform (%make-vv-box-interface 0 'heading))
   (home-position :initform (%make-iv-box-interface '(0.0 0.0) 'home-position))
   (sprite-size   :initform (%make-vv-box-interface 1.0 'sprite-size))
   (private-gl    :initform (make-graphics-command-list))
   ))

(defgeneric turtle? (x) (:method (x) nil) (:method ((x turtle)) t))

(defun make-turtle ()
  (make-instance 'turtle))

;; sgithens TODO, for some reason this complains that graphics-object is unbound...
;; (deftype-checking-macros graphics-object "A Graphics Object")
;; (deftype-checking-macros button "A Graphics Button")
;; (deftype-checking-macros graphics-cursor "A Graphics Cursor")
;; (deftype-checking-macros turtle "A Turtle")


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
               (float user-y)
               ))

;;; ARRAY ==> USER

(defun user-coordinate-x (array-x)
  (float-minus (float array-x) %drawing-half-width))

(defun user-coordinate-y (array-y)
  (float-minus %drawing-half-height (float array-y)))

(defun user-coordinate-fix-x (array-x)
  (declare (fixnum array-x))
  (float-minus (float array-x) %drawing-half-width))

(defun user-coordinate-fix-y (array-y)
  (declare (fixnum array-y))
  (float-minus %drawing-half-height (float array-y)))

;;; these want ARRAY coordinates



;;; normalize coordinates to the on screen position

;; sgithens 2024-07-05 Doesn't seem to be used anywhere...
;; (defun wrap-object-coords (object)
;;   (setf (slot-value object 'x-position)
;;         (wrap-x-coordinate (slot-value object 'x-position)))
;;   (setf (slot-value object 'y-position)
;;         (wrap-y-coordinate (slot-value object 'y-position))))

;;; Lucid (lcl3.0) specific versions try are written to
;;; minimize floating point CONSing

(defun wrap-x-coordinate (user-x)
  (user-coordinate-x (float-modulo (array-coordinate-x user-x)
                                   %drawing-width)))

(defun wrap-y-coordinate (user-y)
  (user-coordinate-y (float-modulo (array-coordinate-y user-y)
                                   %drawing-height)))

(defun float-modulo (num mod)
  (let ((fmod (float mod)))
    (if (and (plusp num) (< num fmod))
      num
      (let ((x (- num (* (floor num fmod) fmod))))
        (if (minusp x) (+ x fmod) x)))))
