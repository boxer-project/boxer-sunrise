;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
#|


 $Header: grmeth.lisp,v 1.0 90/01/24 22:12:38 boxer Exp $

 $Log:	grmeth.lisp,v $
;;;Revision 1.0  90/01/24  22:12:38  boxer
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




  This file contains basic methods for graphics objects and
  for graphics button objects


Modification History (most recent at top)

 1/29/13 removed fixnum math sprite-at-window-point
11/05/09 draw-update: like draw, without the actual drawing
 1/26/08 #-opengl all actual drawing commands since opengl only uses the display lists
 3/28/07 set-assoc-graphics-box
 2/16/03 merged current LW and MCL files, no diffs, copyright updated
 2/15/01 merged current LW and MCL files
12/06/00 fixed touching? (how did the old one ever work ?)
12/06/00 Started logging changes: source = Boxer version 2.4.1


|#

(in-package :boxer)



;;;; Methods that ALL graphics Objects need

;;; Connections with the Boxer Hierarchy...

(defmethod set-sprite-box ((self graphics-object) box)
  (link-interface-slots self box)
  (setf (slot-value self 'sprite-box) box))

;;; this is used to clean up the sprite in preparation for a
;;; new graphics object.  e.g. when toggling to a different object
(defmethod unset-sprite-box ((self graphics-object) box)
  (unlink-interface-slots self box)
  (setf (slot-value self 'sprite-box) nil))

(defmethod set-assoc-graphics-box-instance-var ((self graphics-object) new-box)
  (setf (slot-value self 'assoc-graphics-box) new-box)
  (dolist (subs (slot-value self 'subsprites))
    (set-assoc-graphics-box-instance-var subs new-box)))

(defmethod set-assoc-graphics-box ((self graphics-object) new-box)
  (set-assoc-graphics-box-instance-var self new-box))

;;; Hierarchical Graphics Objects (which mirror (possible)
;;; hierarchical boxer structure)

(defmethod add-subturtle ((self graphics-object) subturtle)
  (setf (superior-turtle subturtle) self)
  (set-assoc-graphics-box subturtle
			  (slot-value self 'assoc-graphics-box))
  (unless (fast-memq subturtle (slot-value self 'subsprites))
    (setf (slot-value self 'subsprites)
	  (cons subturtle (slot-value self 'subsprites)))))

(defmethod remove-subturtle ((self graphics-object) subturtle)
  (set-assoc-graphics-box subturtle nil)
  (setf (superior-turtle subturtle) nil)
  (setf (slot-value self 'subsprites)
	(fast-delq subturtle (slot-value self 'subsprites))))


;;; Methods that handle the connection of box-interfaces

(defun install-interface-slot (interface box-name sprite-box)
  (setf (box-interface-sup-box interface) sprite-box)
  (let ((existing-box
	 (boxer-eval::lookup-static-variable-in-box-only sprite-box box-name)))
    (unless (null existing-box)
      (boxer-eval::remove-static-variable sprite-box box-name)
      (when (box? existing-box)
	(setf (box-interface-box interface) existing-box)))
    (boxer-eval::add-static-variable-pair sprite-box box-name interface)))

(defun uninstall-interface-slot (interface box-name sprite-box)
  (setf (box-interface-sup-box interface) nil)
  (cond ((box? (box-interface-box interface))
	 (boxer-eval::remove-static-variable sprite-box box-name)
	 (boxer-eval::add-static-variable-pair sprite-box box-name
					 (box-interface-box interface)))))

(defmethod all-interface-slots ((self graphics-object))
  (list 'x-position 'y-position))

(defmethod link-interface-slots ((self graphics-object) sprite-box)
  (install-interface-slot (slot-value self 'x-position)
			  'bu::x-position sprite-box)
  (install-interface-slot (slot-value self 'y-position)
			  'bu::y-position sprite-box))

(defmethod unlink-interface-slots ((self graphics-object) sprite-box)
  (uninstall-interface-slot (slot-value self 'x-position)
			    'bu::x-position sprite-box)
  (uninstall-interface-slot (slot-value self 'y-position)
			    'bu::y-position sprite-box))

(defvar *default-graphics-object-interface-boxes* '(x-position y-position))

(defvar *graphics-interface-boxes-in-box* ':default)
(defvar *graphics-interface-boxes-in-closet* ':default)

(defmethod default-interface-boxes ((self graphics-object))
  (declare (values in-box in-closet))
  (values *default-graphics-object-interface-boxes* nil))

(defmethod interface-boxes-to-make ((self graphics-object))
  (declare (values in-box in-closet))
  (multiple-value-bind (default-in-box default-in-closet)
      (default-interface-boxes self)
    (values (if (eq *graphics-interface-boxes-in-box* ':default)
		default-in-box
		*graphics-interface-boxes-in-box*)
	    (if (eq *graphics-interface-boxes-in-closet* ':default)
		default-in-closet
		*graphics-interface-boxes-in-closet*))))

(defun link-box-into-interface (int box)
  (setf (box-interface-box int) box)
  (let ((value (box-interface-value int)))
    ;; change the box to conform to the current value of the slot
    (cond ((sv-box-interface? int)
	   (funcall (special-box-interface-update-function int) int))
	  (t
	   (etypecase value
	     (number (bash-box-to-number box value))
	     (symbol (bash-box-to-single-value box value))
	     (vector )
	     (list (bash-box-to-list-value box value)))))
    ;; check if a trigger is there/is needed/needs to be installed
    (unless (boxer-eval::lookup-static-variable-in-box-only box
						      'bu::modified-trigger)
      (let ((closet? (slot-value box 'closets)))
	(if (null closet?)
	    (add-closet-row
	     box (make-row `(,(make-sprite-instance-var-trigger
			       (box-interface-slot-name int)))))
	    (append-cha closet? (make-sprite-instance-var-trigger
				 (box-interface-slot-name int))))))))

(defun unlink-box-interface (int)
  (setf (box-interface-box int) nil))

;;; Graphics Object Positions...

(defmethod x-position ((self graphics-object))
  (box-interface-value (slot-value self 'x-position)))

(defmethod y-position ((self graphics-object))
  (box-interface-value (slot-value self 'y-position)))


;;; These should be smarter, they should accumalate LOCAL offsets
;;; and rotations instead of having EACH child calculate the ABSOLUTE
;;; offsets and rotations...

(defmethod absolute-x-position ((self graphics-object))
  (let ((superior-turtle (slot-value self 'superior-turtle)))
    (if (null superior-turtle)
	(box-interface-value (slot-value self 'x-position))
	(let ((sup-heading (absolute-heading superior-turtle))
	      (sup-xpos (absolute-x-position superior-turtle))
	      (abs-size (absolute-size self)))
	  (+ sup-xpos
	     (* (cosd sup-heading)
		(box-interface-value (slot-value self 'x-position))
		abs-size)
	     (* (sind sup-heading)
		(box-interface-value (slot-value self 'y-position))
		abs-size))))))

(defmethod make-absolute ((self graphics-object) xpos ypos)
  (let ((superior-turtle (slot-value self 'superior-turtle)))
    (if (null superior-turtle)
	(values xpos ypos)
	(let ((sup-heading (absolute-heading superior-turtle))
	      (sup-xpos (absolute-x-position superior-turtle))
	      (abs-size (absolute-size self))
	      (sup-ypos (absolute-y-position superior-turtle)))
	  (values (+ sup-xpos
		     (* (cosd sup-heading) xpos abs-size)
		     (* (sind sup-heading) ypos abs-size))
		  (+ sup-ypos
		     (* (- (sind sup-heading)) xpos abs-size)
		     (* (cosd sup-heading) ypos abs-size)))))))

(defmethod absolute-y-position ((self graphics-object))
  (let ((superior-turtle (slot-value self 'superior-turtle)))
    (if (null superior-turtle)
	(box-interface-value (slot-value self 'y-position))
	(let ((sup-heading (absolute-heading superior-turtle))
	      (sup-ypos (absolute-y-position superior-turtle))
	      (abs-size (absolute-size self)))
	  (+ sup-ypos
	     (* (- (sind sup-heading))
		(box-interface-value (slot-value self 'x-position))
		abs-size)
	     (* (cosd sup-heading)
		(box-interface-value (slot-value self 'y-position))
		abs-size))))))



;;; Instance SLOT Linkup

(defmethod add-xpos-box ((self graphics-object) box)
  (setf (box-interface-box (slot-value self 'x-position)) box))


(defmethod add-ypos-box ((self graphics-object) box)
  (setf (box-interface-box (slot-value self 'y-position)) box))


(defmethod copy-graphics-object ((self graphics-object))
  (let ((new-go (make-instance (class-of self))))
    (copy-graphics-slots self new-go)
    new-go))

(defmethod copy-graphics-slots ((old-graphics-object graphics-object)
				(new-graphics-object graphics-object))
  (setf (box-interface-value (slot-value new-graphics-object 'x-position))
	(box-interface-value (slot-value old-graphics-object 'x-position)))
  (setf (box-interface-value (slot-value new-graphics-object 'y-position))
	(box-interface-value (slot-value old-graphics-object 'y-position))))



;;; default methods....

(defmethod heading ((self graphics-object))
  self ;; (declare (ignore self)) doesn't work right
  0)

(defmethod absolute-heading ((self graphics-object))
  (if (null (slot-value self 'superior-turtle))
      0
      (absolute-heading (slot-value self 'superior-turtle))))

(defmethod size ((self graphics-object))
  self ;; (declare (ignore self)) doesn't work right
  1.0)

(defmethod absolute-size ((self graphics-object))
  (if (null (slot-value self 'superior-turtle))
      1.0
      (absolute-size (slot-value self 'superior-turtle))))

(defmethod pen ((self graphics-object))
  self ;; (declare (ignore self)) doesn't work right
  'up)

(defmethod pen-up-or-xor? ((self graphics-object))
  t)

(defmethod shown? ((self graphics-object))
  self ;; (declare (ignore self)) doesn't work right
  t)

(defmethod absolute-shown? ((self graphics-object))
  self ;; (declare (ignore self)) doesn't work right
  t)

(defmethod subsprites-shown? ((self graphics-object))
  self ;; (declare (ignore self)) doesn't work right
  t)

(defmethod go-home ((self graphics-object))
  self
  nil)

(defmethod draw ((self graphics-object))
  (error "You MUST define a DRAW method for the ~S class"
	 (class-name (class-of self))))

(defmethod fast-erase ((self graphics-object))
  (error "You MUST define a FAST-ERASE method for the ~S class"
	 (class-name (class-of self))))

(defmethod erase ((self graphics-object))
  (error "You MUST define a ERASE method for the ~S class"
	 (class-name (class-of self))))

(defmethod enclosing-rectangle ((self graphics-object))
  (error "You MUST define a ENCLOSING-RECTANGLE method for the ~S class"
	 (class-name (class-of self))))





;;;; utilities for updating BOX values

;;; we can't use change cause we DON'T want to give
;;; Modifed-triggers a chance to run
;;; we need to take a little care in case the closet-row is around

(defun bash-box-to-single-value (box newvalue)
  (let ((value (convert-sprite-instance-symbol-value newvalue)))
    (cond ((null *evaluation-in-progress?*)
	   (kill-box-contents box)
	   (let ((new-row (make-initialized-row)))
	     (fast-string-into-chas-array (string value)
					  (chas-array new-row))
	     (insert-row-at-row-no box new-row 0)
	     (modified box)))
	  (t
	   (change-virtual-copy-rows box (list (make-evrow-from-entry value)))
	   ;; handle the changes to the editor box
	   (modify-editor-structure box t)))
    value))

(defvar *floating-point-box-printing-precision* 1000.)

(defun bash-box-to-number (box value)
  (cond ((null *evaluation-in-progress?*)
	 (kill-box-contents box)
	 (let ((new-row (make-initialized-row)))
	   (fast-string-into-chas-array (convert-number-to-string value)
					(chas-array new-row))
	   (insert-row-at-row-no box new-row 0)
	   (modified box)))
	(t
	 (change-virtual-copy-rows box (list (make-evrow-from-entry value)))
	 ;; handle the changes to the editor box
	 (modify-editor-structure box t)))
  value)

(defun bash-box-to-list-value (box value)
  (cond ((null *evaluation-in-progress?*)
	 (kill-box-contents box)
	 (let* ((new-row (make-initialized-row))
		(chas-array (chas-array new-row)))
	   (dolist (item value)
	     (print-thing-into-chas-array item new-row chas-array)
	     (fast-chas-array-append-cha chas-array #\space))
	   (insert-row-at-row-no box new-row 0)
	   (modified box)))
	(t
	 (change-virtual-copy-rows box
				   (list (make-evrow :pointers
						     (mapcar #'make-pointer
							     value))))
	 ;; handle the changes to the editor box
	 (modify-editor-structure box t)))
  value)




;;;; Mutators

(defmethod set-x-position ((self graphics-object) new-value)
  (let* ((x-slot (slot-value self 'x-position))
	 (box (box-interface-box x-slot)))
    (when (not-null box)
      (bash-box-to-number box new-value))
    (setf (box-interface-value x-slot) new-value)))

(defmethod set-y-position ((self graphics-object) new-value)
  (let* ((y-slot (slot-value self 'y-position))
	 (box (box-interface-box y-slot)))
    (when (not-null box)
      (bash-box-to-number box new-value))
    (setf (box-interface-value y-slot) new-value)))

(defmethod set-xy ((self graphics-object) new-x new-y
		   &optional dont-update-box)
  (let* ((x-slot (slot-value self 'x-position))
	 (box (box-interface-box x-slot)))
    (when (and (null dont-update-box) (not-null box))
      (bash-box-to-number box new-x))
    (setf (box-interface-value x-slot) new-x))
  (let* ((y-slot (slot-value self 'y-position))
	 (box (box-interface-box y-slot)))
    (when (and (null dont-update-box) (not-null box))
      (bash-box-to-number box new-y))
    (setf (box-interface-value y-slot) new-y)))



;;; turtle state

(defmethod return-state ((self graphics-object))
  (list (x-position self) (y-position self)))

(defmethod reset-turtle-and-return-state ((self graphics-object))
  (let ((turtle-state (return-state self)))
    (set-x-position self 0.0)
    (set-y-position self 0.0)
    turtle-state))

(defmethod restore-turtle-state ((self graphics-object) state)
  (set-x-position self (first state))
  (set-y-position self (second state)))




;;;; Intersection Routines

(defun update-turtle-window-extents (window-shape turtle
						  &optional
						  (array-x 0) (array-y 0))
  (let ((min-x array-x) (min-y array-y) (max-x array-x) (max-y array-y)
	(visible? (shown? turtle)))
    (unless (eq visible? ':subsprites)
      (with-graphics-state-bound
        (do-vector-contents (gc window-shape)
	  (multiple-value-bind (gc-min-x gc-min-y gc-max-x gc-max-y
				         state-change?)
	                       (graphics-command-extents gc)
	    (unless state-change?
	      (setq min-x (min gc-min-x min-x)
		    min-y (min gc-min-y min-y)
		    max-x (max gc-max-x max-x)
		    max-y (max gc-max-y max-y)))))))
    (unless (eq visible? ':no-subsprites)
      (dolist (subs (slot-value turtle 'subsprites))
	(when (absolute-shown? subs)
	  (multiple-value-bind (sub-min-x sub-min-y sub-max-x sub-max-y)
	    (enclosing-rectangle subs)
	    (setq min-x (min min-x sub-min-x)
		  min-y (min min-y sub-min-y)
		  max-x (max max-x sub-max-x)
		  max-y (max max-y sub-max-y))))))
    (setf (turtle-window-shape-min-graphics-x-extent window-shape) min-x
	  (turtle-window-shape-min-graphics-y-extent window-shape) min-y
	  (turtle-window-shape-max-graphics-x-extent window-shape) max-x
	  (turtle-window-shape-max-graphics-y-extent window-shape) max-y)
    (values min-x min-y max-x max-y)))

(defmethod touching? ((self graphics-object) other-turtle)
  (multiple-value-bind (left1 top1 right1 bottom1)
      (enclosing-rectangle self)
    (multiple-value-bind (left2 top2 right2 bottom2)
	(enclosing-rectangle other-turtle)
      (flet ((horiz-touch? ()
               (or (inclusive-between? left1 left2 right2)
                   (inclusive-between? right1 left2 right2)
                   (inclusive-between? left2 left1 right1)))
             (vert-touch? ()
               (or (inclusive-between? top1 top2 bottom2)
                   (inclusive-between? bottom1 top2 bottom2)
                   (inclusive-between? top2 top1 bottom1))))
        (and (horiz-touch?) (vert-touch?))))))



(defmethod all-sprites-in-contact ((self graphics-object))
  (let ((objects (graphics-object-list (slot-value self 'assoc-graphics-box)))
	turtles)
    (setq objects (fast-delq (top-sprite self) (copy-seq objects)))
    (dolist (object objects)
      (when (touching? self object)
	(setq turtles (cons object turtles))))
    turtles))



;;;; Useful Methods

(defmethod turtle-distance ((self graphics-object) x y)
  (let ((x-diff (- (x-position self) x))
	(y-diff (- (y-position self) y)))
    (sqrt (+ (* x-diff x-diff) (* y-diff y-diff)))))

(defun calc-name-position-x (length left right)
  (setq left (array-coordinate-x left)
	right (array-coordinate-x right))
  (if (> (+ right length) %drawing-width)
      (fixr (- left length 3.))
      (fixr (+ right 5.))))

(defun calc-name-position-y (height top bottom)
  (let ((center (+ (array-coordinate-y top)
		   (/ (- top bottom) 2))))
    (fixr (min (max center 0)
	       (- %drawing-height height 1.)))))

;;; Drawing the turtle's name

#|
CLOSED for renovations until I fix the string/font situation

(defmethod flash-name ((self turtle))
    (let* ((print-name (name sprite-box))
	   (name-length (sting-wid ))) ;;; fix this
      (multiple-value-bind (left top right bottom)
	  (enclosing-rectangle self)
	(let ((x-pos (calc-name-position-x name-length left RIGHT))
	      (Y-POS (CALC-NAME-POSITION-Y *FONT-HEIGHT* TOP BOTTOM)))
	  (DRAW-STRING-TO-GBOX PRINT-NAME X-POS Y-POS)
	  (PROCESS-SLEEP 120 "Pausing to flash name")
	  (DRAW-STRING-TO-GBOX PRINT-NAME X-POS Y-POS)))))

|#


(defmethod move-to ((self graphics-object) x-dest y-dest
		    &optional dont-update-box)
  (let ((x-position (slot-value self 'x-position))
	(y-position (slot-value self 'y-position))
	(pen-alu (get-alu-from-pen (pen self))))
    (cond  ((not (and (numberp x-dest) (numberp y-dest)))
	    (error "one of the args, ~s or ~s, was not a number"
		   x-dest y-dest))
	   (t
	    (unless (typep x-dest 'boxer-float)
	      (setq x-dest (coerce x-dest 'boxer-float)))
	    (unless (typep y-dest 'boxer-float)
	      (setq y-dest (coerce y-dest 'boxer-float)))
	    (cond ((not (null %learning-shape?))
		   ;; don't draw while learning shape.
		   (unless (null pen-alu)
		     (record-boxer-graphics-command-line-segment
		      (box-interface-value x-position)
		      (box-interface-value y-position)
		      x-dest y-dest))
		   ;; While in learning-shape, don't update any boxes
		   (setf (box-interface-value x-position) x-dest)
		   (setf (box-interface-value y-position) y-dest))
		  ;; Have to make fence mode work some other time
;		  ((and (eq %draw-mode ':fence)
;			(not (point-in-array? array-x-dest array-y-dest)))
;		   (error "you hit the fence"))
		  (t
		   (cond ((no-graphics?)
			  ;; this means we can't even do any wrapping
			  ;; calculations, so just set values
			  (set-xy self x-dest y-dest dont-update-box))
			 (t
			  (multiple-value-bind (array-x-dest array-y-dest)
			      (make-absolute self x-dest y-dest)
			    (setq array-x-dest (fix-array-coordinate-x
						array-x-dest)
				  array-y-dest (fix-array-coordinate-y
						array-y-dest))
			    (let ((array-x (fix-array-coordinate-x
					    (absolute-x-position self)))
				  (array-y (fix-array-coordinate-y
					    (absolute-y-position self))))
			      (without-interrupts
			       (when (and (null (slot-value self
							    'superior-turtle))
					  (eq %draw-mode ':wrap))
				 (setq x-dest (wrap-x-coordinate x-dest)
				       y-dest (wrap-y-coordinate y-dest)))
			       ;; this may have to change...
			       (cond ( %mouse-usurped
				      ;; don't update boxes during follow-mouse
				      (setf (box-interface-value x-position)
					    x-dest)
				      (setf (box-interface-value y-position)
					    y-dest))
				     (t
				      (set-xy self x-dest y-dest
					      dont-update-box)))
			       (when (and (not (null pen-alu))
					  (not (zerop (pen-width self))))
				 (record-boxer-graphics-command-line-segment
				  array-x array-y array-x-dest array-y-dest)
                                 #-opengl
				 (with-graphics-screen-parameters
				     (line-segment array-x array-y
						   array-x-dest array-y-dest))))
			      ;; invalidate the shape and extent caches
			      (invalidate-window-shape-and-extent-caches
			       self)))))))))))


;;;;;;;; Graphics BUTTON methods

(defmethod initialize-instance ((self button) &rest init-plist)
  init-plist ;; (declare (ignore init-plist)) doesn't work
  (shared-initialize self t)
  (let ((new-shape (copy-graphics-command-list
		    *default-graphics-object-shape*)))
    (setf (slot-value self 'shape)
	  (%make-sv-box-interface new-shape 'shape nil 'shape-box-updater))
    ;; the window shape is is used as a graphics cache
    (setf (slot-value self 'window-shape)
	  (make-turtle-window-shape new-shape)))
  self)

(defmethod shape ((self button))
  (box-interface-value (slot-value self 'shape)))

(defmethod add-shape-box ((self button) box)
  (setf (box-interface-box (slot-value self 'shape)) box))

(defmethod all-interface-slots ((self button))
  (append (call-next-method) (list 'shape)))

(defmethod link-interface-slots ((self button) sprite-box)
  (call-next-method)
  (install-interface-slot (slot-value self 'shape)
			  'bu::shape sprite-box))

(defmethod unlink-interface-slots ((self button) sprite-box)
  (call-next-method)
  (uninstall-interface-slot (slot-value self 'shape)
			    'bu::shape sprite-box))

(defmethod copy-graphics-slots ((old-button button) (new-button button))
  (call-next-method)
  (setf (box-interface-value (slot-value new-button 'shape))
	(copy-graphics-command-list
	 (box-interface-value (slot-value old-button 'shape))))
  (update-window-shape-allocation new-button))



;;;; Shape Handling

;;; The save-under slot-of a turtle can be the symbol 'XOR-REDRAW,
;;; or an offscreen bitmap or NIL (for a freshly CONSed turtle)
;;; XOR mode is faster than save-unders so if we can, use XOR mode
;;;
;;; Note that we deliberately allocate a backing store large enough
;;; to support any possible rotation of the sprite to avoid having to
;;; continually reallocate a backing store (slow !) for any rotation
;;; of the sprite.
;;;
;;; Need to put in support for overlay planes
;;;

(defmethod update-save-under ((self button))
  (let ((shape (shape self))
	(save-under (slot-value self 'save-under))
	(scale (sprite-size self)))
    (let ((size 0)
	  (non-xor-graphics-occurred? nil)
	  (in-xor? nil))
      (with-graphics-state-bound
        (do-vector-contents (com shape)
	  ;; there really ought to be a more modular way to handle this...
	  (when (and (not non-xor-graphics-occurred?)
		     ;; no need to check if there has already been
		     ;; graphics drawn in something other than XOR
		     (=& (svref& com 0)
		        ;;  sgithens TODO 2020-03-29 Why was this read operator syntax being used?
				;; #.(boxer::graphics-command-opcode 'boxer-change-alu)))
				(boxer::graphics-command-opcode 'boxer-change-alu)))
	    (setq in-xor? (=& (svref& com 1) alu-xor)))
	  (multiple-value-bind (lef top rig bot state-change?)
	      (graphics-command-extents com)
	    (unless state-change?
	      (unless (and in-xor? (not non-xor-graphics-occurred?))
	        (setq non-xor-graphics-occurred? t))
	      (setq size
		    (max size
			 (let* ((max-x (max (abs lef) (abs rig)))
			        (max-y (max (abs top) (abs bot)))
			        (max-t (max max-x max-y)))
			   (* 2 (* max-t max-t)))))))))
      ;; size is now the largest of the sum of squares we take the square
      ;; root of size to obtain the maximimum "radius" of the shape
      ;; remember to multiply by scale since graphics-command-extents does
      ;; not scale
      (setq size (*& 2 (values (ceiling (sqrt (* scale size))))))
      (cond ((null non-xor-graphics-occurred?)
	     ;; if ALL the graphics in the shape have been drawn
	     ;; in XOR, then we can use XOR-REDRAW
	     (setf (slot-value self 'save-under) 'xor-redraw))
	    ((or (null save-under) (eq save-under 'xor-redraw))
	     ;; no existing bitmap save under so allocate a new one
	     (setf (slot-value self 'save-under)
		   (make-save-under (make-offscreen-bitmap *boxer-pane*
							   size size)
				    (floor size 2)
				    size)))
	    (t
	     ;; at this point, we know we both have and want a bitmap backing
	     ;; store now check sizes to see if what we already have is big
	     ;; enough we may want to put in a shrinking bitmap clause but
	     ;; for now, we just leave big bitmaps in place trying to
	     ;; minimize more reallocation of bitmaps--trading space (extra
	     ;; bitmap size) for speed (no need to
	     ;; reallocate bitmaps in grow/shrink/grow cases)
	     (let ((existing-bitmap (save-under-bitmap save-under)))
	       (when (and (not (null existing-bitmap))
			  ;; don't need this but be paranoid since
			  ;; this is critical code
			  (or (>& size
				  (offscreen-bitmap-width  existing-bitmap))
			      (>& size
				  (offscreen-bitmap-height existing-bitmap))))
		 (setf (slot-value self 'save-under)
		       (make-save-under (make-offscreen-bitmap *boxer-pane*
							       size size)
					(round size 2) size)))))))))

(defmethod update-window-shape-allocation ((self button))
  (let ((window-shape (slot-value self 'window-shape))
	(shape (box-interface-value (slot-value self 'shape))))
    (unless (null window-shape)
      (clear-graphics-list window-shape))
    (do-vector-contents (gc shape)
      (sv-append window-shape (allocate-boxer->window-command gc)))))

(defun update-window-shape (shape window-shape
				  trans-x trans-y cos-scale sin-scale scale)
  (do-vector-contents (turtle-graphics-command shape :index-var-name idx)
    (translate-boxer->window-command turtle-graphics-command
				     (sv-nth idx window-shape)
				     trans-x trans-y cos-scale sin-scale
				     scale))
  (setf (turtle-window-shape-valid window-shape) t))

;;; When a window shape is invalidated, we have to recurse through the
;;; inferiors as well

(defmethod invalidate-window-shape-and-extent-caches ((self button))
  (setf (turtle-window-shape-valid (slot-value self 'window-shape))
	nil
	(turtle-window-shape-min-graphics-x-extent
	 (slot-value self 'window-shape))
	nil)
  ;; now recurse
  (dolist (ss (slot-value self 'subsprites))
    (invalidate-window-shape-and-extent-caches ss)))


;;;; drawing

(defmethod draw ((self button))
  (unless (or (eq self *current-active-sprite*) (null (shown? self)))
    (let (;(*supress-graphics-recording?* (not stamp?))
	  (ahead (absolute-heading self))
	  (asize (absolute-size self)))
      ;; update the turtle-window-shape
      (unless (turtle-window-shape-valid (slot-value self 'window-shape))
	;; minimal error checking...
	(unless (= (storage-vector-active-length
		    (box-interface-value (slot-value self 'shape)))
		   (storage-vector-active-length (slot-value self
							     'window-shape)))
	  (warn "The shape and the window-shape are out of synch, fixing...")
	  (update-window-shape-allocation self))
	(update-window-shape (box-interface-value (slot-value self 'shape))
			     (slot-value self 'window-shape)
			     (absolute-x-position self)
			     (absolute-y-position self)
			     (* (cosd ahead) asize) (* (sind ahead) asize)
			     asize))
      (playback-graphics-list-internal (slot-value self 'window-shape))
      (unless (eq (shown? self) ':no-subsprites)
	(dolist (subs (slot-value self 'subsprites))
	  (draw subs))))))

;;; like draw but without the actual drawing
(defmethod draw-update ((self button))
  (let ((ahead (absolute-heading self)) (asize (absolute-size self)))
    ;; update the turtle-window-shape
    (unless (turtle-window-shape-valid (slot-value self 'window-shape))
      ;; minimal error checking...
      (unless (= (storage-vector-active-length
                  (box-interface-value (slot-value self 'shape)))
                 (storage-vector-active-length (slot-value self 'window-shape)))
        (warn "The shape and the window-shape are out of synch, fixing...")
        (update-window-shape-allocation self))
      (update-window-shape (box-interface-value (slot-value self 'shape))
                           (slot-value self 'window-shape)
                           (absolute-x-position self)
                           (absolute-y-position self)
                           (* (cosd ahead) asize) (* (sind ahead) asize)
                           asize))
    (unless (eq (shown? self) ':no-subsprites)
      (dolist (subs (slot-value self 'subsprites))
        (draw-update subs)))))

;;; need to support overlay here...
(defmethod fast-erase ((turtle button))
  (unless (eq turtle *current-active-sprite*)
    (if (eq (turtle-save-under turtle) 'xor-redraw)
        (draw turtle)
        (restore-under-turtle turtle))))

(defmethod erase ((self button))
  (when (absolute-shown? self) (fast-erase self)))

(defmethod show-turtle ((self button))
  (set-shown? self t))

;; hide-turtle is called from inside a with-sprites-hidden macro
;; because hiding a PD sprite can obscure (parts of) other sprites
;; so they must all hide so they can be redrawn.
;; therefore, it needs to suppress the explicit erasing
(defmethod hide-turtle ((self button))
  (set-shown? self nil nil nil))



;;; stuff for mouse-sensitivity

;;;; These return values in WINDOW coordinates

(defmethod enclosing-rectangle ((self button))
  ;; first insure the validity of the window-shape
  (let ((ws (slot-value self 'window-shape)))
    (unless (turtle-window-shape-valid ws)
      (when *boxer-system-hacker*
	(warn "Updating window shape inside of ENCLOSING-RECTANGLE"))
      (let ((ahead (absolute-heading self))
	    (asize (absolute-size self)))
	(unless (= (storage-vector-active-length
		    (box-interface-value (slot-value self 'shape)))
		   (storage-vector-active-length (slot-value self
							     'window-shape)))
	  (warn "The shape and the window-shape are out of synch, fixing...")
	  (update-window-shape-allocation self))
	(update-window-shape (box-interface-value (slot-value self 'shape))
			     ws
			     (absolute-x-position self)
			     (absolute-y-position self)
			     (* (cosd ahead) asize) (* (sind ahead) asize)
			     asize)))
    ;; now fill the extents slots if needed
    (when (null (turtle-window-shape-min-graphics-x-extent ws))
      (update-turtle-window-extents ws self
				    ;; ?? is this neccesary ??
				    (fix-array-coordinate-x
				     (absolute-x-position self))
				    (fix-array-coordinate-y
				     (absolute-y-position self))))
    ;; finally return the values
    (values (turtle-window-shape-min-graphics-x-extent ws)
	    (turtle-window-shape-min-graphics-y-extent ws)
	    (turtle-window-shape-max-graphics-x-extent ws)
	    (turtle-window-shape-max-graphics-y-extent ws))))

(defun enclosing-sprite-coords (sprite)
  (multiple-value-bind (left top right bottom)
      (enclosing-rectangle sprite)
    (unless (no-graphics?)
      (values (user-coordinate-x left)  (user-coordinate-y top)
	      (user-coordinate-x right) (user-coordinate-y bottom)))))

;;; returns either NIL or the lowest sprite that contains the point
(defmethod sprite-at-window-point ((self button) window-x window-y)
  ;; first insure the validity of the window-shape
  (let ((ws (slot-value self 'window-shape)))
    (unless (turtle-window-shape-valid ws)
      (when *boxer-system-hacker*
	(warn "Updating window shape inside of ENCLOSING-RECTANGLE"))
      (let ((ahead (absolute-heading self))
	    (asize (absolute-size self)))
	(unless (= (storage-vector-active-length
		    (box-interface-value (slot-value self 'shape)))
		   (storage-vector-active-length (slot-value self
							     'window-shape)))
	  (warn "The shape and the window-shape are out of synch, fixing...")
	  (update-window-shape-allocation self))
	(update-window-shape (box-interface-value (slot-value self 'shape))
			     ws
			     (absolute-x-position self)
			     (absolute-y-position self)
			     (* (cosd ahead) asize) (* (sind ahead) asize)
			     asize)))
    ;; now fill the extents slots if needed
    (when (null (turtle-window-shape-min-graphics-x-extent ws))
      (update-turtle-window-extents ws self
				    ;; ?? is this neccesary ??
				    (fix-array-coordinate-x
				     (absolute-x-position self))
				    (fix-array-coordinate-y
				     (absolute-y-position self))))
    ;; now we can use the cache values
    (when (and (<= (turtle-window-shape-min-graphics-x-extent ws)
		    window-x
		    (turtle-window-shape-max-graphics-x-extent ws))
	       (<= (turtle-window-shape-min-graphics-y-extent ws)
		    window-y
		    (turtle-window-shape-max-graphics-y-extent ws)))
      ;; the point is within the sprite, now check any subsprites
      (or (dolist (sub (slot-value self 'subsprites))
	    (let ((under? (sprite-at-window-point sub window-x window-y)))
	      (when (not (null under?))
		(return under?))))
	  self))))
