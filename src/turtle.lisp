;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
#|


 $Header: turtle.lisp,v 1.0 90/01/24 22:18:47 boxer Exp $

 $Log:	turtle.lisp,v $
;;;Revision 1.0  90/01/24  22:18:47  boxer
;;;Initial revision
;;;







 Copyright 1986 - 1998 Regents of the University of California

 Enhancements and Modifications Copyright 1998 - 2003 Pyxisystems LLC



                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+




  This file contains basic turtle methods.

Modification History (most recent at top)

 3/26/03 merged current LW and Mac src


|#

#-(or lispworks mcl lispm) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+(or lispworks mcl)       (in-package :boxer)




(defmethod initialize-instance ((self turtle) &rest init-plist)
  init-plist ;; (declare (ignore init-plist)) doesn't work
  (shared-initialize self t)
  (let ((new-shape (copy-graphics-command-list
		    *default-turtle-shape*)))
    (setf (slot-value self 'shape)
	  (%make-sv-box-interface new-shape 'shape nil 'shape-box-updater)
	  (slot-value self 'window-shape)
	  (make-turtle-window-shape new-shape)))
  self)

(defmethod all-interface-slots ((self turtle))
  (append (call-next-method) (list 'heading 'sprite-size 'home-position)))

(defmethod link-interface-slots ((self turtle) sprite-box)
  (call-next-method)
  (install-interface-slot (slot-value self 'heading)
			    'bu::heading sprite-box)
  (install-interface-slot (slot-value self 'sprite-size)
			    'bu::sprite-size sprite-box)
  (install-interface-slot (slot-value self 'home-position)
			    'bu::home-position sprite-box))

(defmethod unlink-interface-slots ((self turtle) sprite-box)
  self ;; (declare (ignore self)) doesn't work yet
  (call-next-method)
  (uninstall-interface-slot (slot-value self 'heading)
			    'bu::heading sprite-box)
  (uninstall-interface-slot (slot-value self 'sprite-size)
			    'bu::sprite-size sprite-box)
  (uninstall-interface-slot (slot-value self 'home-position)
			    'bu::home-position sprite-box))


(defmethod copy-graphics-slots ((old-turtle turtle) (new-turtle turtle))
  (call-next-method)
  (setf (box-interface-value (slot-value new-turtle 'heading))
	(box-interface-value (slot-value old-turtle 'heading)))
  (setf (box-interface-value (slot-value new-turtle 'sprite-size))
	(box-interface-value (slot-value old-turtle 'sprite-size)))
  (setf (box-interface-value (slot-value new-turtle 'home-position))
	(box-interface-value (slot-value old-turtle 'home-position))))


(defmethod default-interface-boxes ((self turtle))
  (declare (values in-box in-closet))
  (multiple-value-bind (in-box in-closet)
      (call-next-method)
    (values (append in-box (list 'heading))
	    in-closet)))

(defmethod add-heading-box ((self turtle) box)
  (setf (box-interface-box (slot-value self 'heading)) box))

(defmethod heading ((self turtle))
  (box-interface-value (slot-value self 'heading)))

(defmethod absolute-heading ((self turtle))
  (if (null (slot-value self 'superior-turtle))
      (box-interface-value (slot-value self 'heading))
      (+ (box-interface-value (slot-value self 'heading))
	 (absolute-heading
	   (slot-value self 'superior-turtle)))))

(defmethod set-heading-instance-var ((self turtle) new-value
				     &optional dont-update-box)
  (let* ((h-slot (slot-value self 'heading))
	 (box (box-interface-box h-slot)))
    (when (and (null dont-update-box) (not-null box))
      (bash-box-to-number box new-value))
    (setf (box-interface-value h-slot) new-value)))

(defmethod home-position ((self turtle))
  (box-interface-value (slot-value self 'home-position)))

(defmethod set-home-position ((self turtle) new-x new-y
			      &optional dont-update-box)
  (let* ((slot (slot-value self 'home-position))
	 (box (box-interface-box slot))
	 (new (list new-x new-y)))
    (when (and (null dont-update-box) (not (null box)))
      (bash-box-to-list-value box new))
    (setf (box-interface-value slot) new)))

(defmethod add-home-position-box ((self turtle) box)
  (setf (box-interface-box (slot-value self 'home-position)) box))


(defmethod home-x-pos ((self turtle))
  (car (box-interface-value (slot-value self 'home-position))))

(defmethod home-y-pos ((self turtle))
  (cadr (box-interface-value (slot-value self 'home-position))))

;;for compatability....
(defmethod home-position-x ((self turtle))
  (car (box-interface-value (slot-value self 'home-position))))

(defmethod home-position-y ((self turtle))
  (cadr (box-interface-value (slot-value self 'home-position))))

(defmethod top-sprite ((self turtle))
  (let ((superior-turtle (slot-value self 'superior-turtle)))
    (if (not (null superior-turtle))
	(top-sprite superior-turtle)
	self)))

(defmethod scale-save-under ((self turtle) scale)
  (let ((su (slot-value self 'save-under)))
    (unless (or (null su) (eq su 'xor-redraw))
      (let ((new-size (ceiling (* (save-under-size su) scale))))
	(setf (save-under-size su) new-size
	      (save-under-middle su) (round new-size 2))
        (free-offscreen-bitmap (save-under-bitmap su))
	(setf (save-under-bitmap su)
	      (make-offscreen-bitmap *boxer-pane*
				     new-size new-size))))))

(defmethod set-sprite-size ((self turtle) new-size &optional dont-update-box)
  (cond ((<= new-size 0)
	 (error
	   "Argument to Set-sprite-size, ~d , was less than or equal to zero"
	   new-size))
	(t
	 (let* ((slot (slot-value self 'sprite-size))
		(box (box-interface-box slot)))
           (unless (= (box-interface-value slot) new-size)
	     (when (and (null dont-update-box) (not (null box)))
	       (bash-box-to-number box new-size))
	     (scale-save-under self (/ new-size (box-interface-value slot)))
	     (setf (box-interface-value slot) new-size))
	   ;; invalidate the shape and extent caches
	   (invalidate-window-shape-and-extent-caches self)))))

(defmethod absolute-size ((self turtle))
  (let ((superior-turtle (slot-value self 'superior-turtle)))
    (if (not (null superior-turtle))
	(* (box-interface-value (slot-value self 'sprite-size))
	   (absolute-size superior-turtle))
	(box-interface-value (slot-value self 'sprite-size)))))

(defmethod sprite-size ((self turtle))
  (box-interface-value (slot-value self 'sprite-size)))

;; for compatibility...
(defmethod size ((self turtle))
  (box-interface-value (slot-value self 'sprite-size)))

(defmethod add-sprite-size-box ((self turtle) box)
  (setf (box-interface-box (slot-value self 'sprite-size)) box))




;;;; The higher level stuff.


;;; Moving around

(defmethod forward ((self turtle) distance)
  (let* ((head (heading self))
	 (change-x (* distance (sind head)))
	 (change-y (* distance (cosd head))))
    (move-to self
	     (+ change-x (x-position self))
	     (+ change-y (y-position self)))))

(defmethod go-home ((self turtle))
  (move-to self (home-x-pos self) (home-y-pos self))
  (turn-to self 0))

;;; Turning around
(defmethod turn-to ((self turtle) new-heading &optional dont-update-box)
  (cond ((numberp new-heading)
	 (if (not (null %learning-shape?))
	     (set-heading-instance-var self (float-modulo new-heading 360.))
	     (without-interrupts
	       (set-heading-instance-var self
					 (float-modulo new-heading 360.)
					 dont-update-box)
	       ;; invalidate the shape and extent caches
	       (invalidate-window-shape-and-extent-caches self))))
	(t (error "the argument, ~s, was not a number" new-heading))))

(defmethod right ((self turtle) degrees)
  (turn-to self (+ (heading self) degrees)))

(defmethod turn-to-without-draw ((self turtle) new-heading)
  (cond ((numberp new-heading)
	 (set-heading-instance-var self (float-modulo new-heading 360.)))
	(t (error "the argument, ~s, was not a number" new-heading))))

(defmethod rotate ((self turtle) degrees)
  (dolist (subs (slot-value self 'subsprites))
    (turn-to-without-draw subs (- (heading subs) degrees)))
  (turn-to-without-draw self (+ (heading self) degrees)))


(defvar *vertical-tolerance* .0001)

(defmethod towards ((self turtle) x y)
  (let ((x-position (x-position self))
	(y-position (y-position self)))
    (cond ((and (< (abs (- x x-position)) *vertical-tolerance*)
		(> y y-position))
	   0)
	  ((< (abs (- x x-position)) *vertical-tolerance*)
	   180.)
	  (t (float-modulo (/ (* 180. (atan (- x x-position)
					    (- y y-position))) pi) 360.)))))

(defmethod set-heading ((self turtle) new-heading)
  (turn-to self new-heading))

;;; changing shape

#|
(defmethod save-state-and-reset ((self turtle))
  (setq %turtle-state
	(list (x-position self) (y-position self) (heading self)))
  (set-x-position self 0.0)
  (set-y-position self 0.0)
  (set-heading self 0))

(defmethod restore-state ((self turtle))
  (set-x-position self (first %turtle-state))
  (set-y-position self (second %turtle-state))
  (set-heading self (third %turtle-state))
  (setf (box-interface-value (slot-value self 'pen)) (fourth %turtle-state)))
|#

(defmethod return-state ((self turtle))
  (list (x-position self)
	(y-position self)
	(heading self)
	(pen self)
	(pen-color self)
	(pen-width self)
	(type-font self)))

(defmethod reset-turtle-and-return-state ((self turtle))
  (let ((turtle-state (return-state self)))
    (set-x-position self 0.0)
    (set-y-position self 0.0)
    (set-heading self 0)
    turtle-state))

(defmethod restore-turtle-state ((self turtle) state)
  (set-x-position self (first state))
  (set-y-position self (second state))
  (set-heading self (third state))
  (setf (box-interface-value (slot-value self 'pen)) (fourth state))
  (setf (box-interface-value (slot-value self 'pen-color)) (nth 4 state))
  (setf (box-interface-value (slot-value self 'pen-width)) (nth 5 state))
  (setf (box-interface-value (slot-value self 'type-font)) (nth 6 state)))










