;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
#|


 $Header: grmeth.lisp,v 1.0 90/01/24 22:12:38 boxer Exp $

 $Log:	grmeth.lisp,v $
;;;Revision 1.0  90/01/24  22:12:38  boxer
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
  ;; TODO sgithens 2024-10-28 Why isn't this  one calling next method?
  ;;                          Seems like a bug?
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

(defun graphics-command-list-extents (gc-displ)
  "Takes a graphics-command-list and calculates the extents of all the drawing operations,
   returning a list with the min/max values as floats in the order: '(min-x min-y max-x max-y)"
  (let ((min-x 0)
        (min-y 0)
        (max-x 0)
        (max-y 0))
    (with-graphics-state-bound
        (do-vector-contents (gc gc-displ)
          (multiple-value-bind (gc-min-x gc-min-y gc-max-x gc-max-y
                                         state-change?)
                               (graphics-command-extents gc)
            (unless state-change?
              (setq min-x (min gc-min-x min-x)
                    min-y (min gc-min-y min-y)
                    max-x (max gc-max-x max-x)
                    max-y (max gc-max-y max-y))))))

    (list min-x min-y max-x max-y)))

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

;;;; Useful Methods

(defmethod turtle-distance ((self graphics-object) x y)
  (let ((x-diff (- (x-position self) x))
        (y-diff (- (y-position self) y)))
    (sqrt (+ (* x-diff x-diff) (* y-diff y-diff)))))

(defmethod move-to ((self graphics-object) x-dest y-dest
                    &optional dont-update-box)
  (let* ((x-position (slot-value self 'x-position))
         (y-position (slot-value self 'y-position))
         (x-start (box-interface-value x-position))
         (y-start (box-interface-value y-position))
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
                  (t
                   (cond ((no-graphics?)
                          ;; this means we can't even do any wrapping
                          ;; calculations, so just set values
                          (set-xy self x-dest y-dest dont-update-box))
                         (t
                          ;; We record the graphics commend before potentially wrapping the dest,
                          ;; and then wrap the dest for the sprites new location. The line wrapping
                          ;; is taken care of at draw time by draw-wrap-line
                          (when (and (not (null pen-alu))
                                    (not (zerop (pen-width self))))
                            (record-boxer-graphics-command-line-segment
                              (coerce x-start 'single-float)
                              (coerce y-start 'single-float)
                              (coerce x-dest 'single-float)
                              (coerce y-dest 'single-float)))
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
                                        dont-update-box)))))))))))


;;;;;;;; Graphics BUTTON methods

(defmethod initialize-instance ((self button) &rest init-plist)
  init-plist ;; (declare (ignore init-plist)) doesn't work
  (shared-initialize self t)
  (let ((new-shape (copy-graphics-command-list
                    *default-graphics-object-shape*)))
    (setf (slot-value self 'shape)
          (%make-sv-box-interface new-shape 'shape nil 'shape-box-updater)))
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
         (box-interface-value (slot-value old-button 'shape)))))

;;;; drawing

(defmethod model-matrix ((self button) &optional (x-adjust 0) (y-adjust 0))
  "Generates the model matrix for the sprites current heading, size (scale), and rotation."
  (let* ((ahead (absolute-heading self))
         (asize (absolute-size self))
         (ahead-rad (* ahead (/ pi 180)))
         (trans-mat (3d-matrices:mtranslation
                      (3d-vectors:vec
                        (+ x-adjust (absolute-x-position self))
                        (- y-adjust (absolute-y-position self)) 0.0)))
         (rot-mat (3d-matrices:nmrotate
                      (3d-matrices:meye 4) 3d-vectors:+vz+
                      ahead-rad))
         (scale-mat (3d-matrices:nmscale (3d-matrices:meye 4) (3d-vectors:vec asize asize 0.0)))
         (final-mat (3d-matrices:m* trans-mat rot-mat scale-mat)))
    final-mat))

(defmethod draw ((self button))
  (unless (null (shown? self))
    (let* ((prev-model (boxgl-device-model-matrix bw::*boxgl-device*))
           (final-mat (3d-matrices:m* prev-model (model-matrix self %drawing-half-width %drawing-half-height))))
      ;; sgithens TODO 2024-12-26 Remove the drawing-half-width/height from above and also from the graphics command
      ;; list playback, and move it to a matrix transform at a higher level, so individual drawback code doens't need
      ;; to worry about it, especially when the transforms become more complex in the future (z-axis, rotations, etc)

      (setf (boxgl-device-model-matrix bw::*boxgl-device*) final-mat)
      (update-model-matrix-ubo bw::*boxgl-device*)

      ;; We need to override the drawing-half-width and drawing-half-height here
      ;; with the values from the shape box graphics-sheet, otherwise, if it's scaled or
      ;; someething it will use the values from the upper level graphics box.  This is
      ;; primarily for correct operation of draw-wrap-line which uses these global values.

      ;; Occasionally there seems to be a shape that has it's interface value, but not
      ;; the accompanying box
      ;; TODO: these heights really should only need calculating if something changed
      (let* ((gs (if (box-interface-box (slot-value self 'shape))
                    (graphics-sheet (box-interface-box (slot-value self 'shape)))
                    nil))
             (gl (box-interface-value (slot-value self 'shape)))
             (extents (multiple-value-list (graphics-list-extent gl)))
             (wid (if gs
                   (graphics-sheet-draw-wid gs)
                   (floor (first extents))))
             (hei (if gs
                   (graphics-sheet-draw-hei gs)
                   (floor (second extents))))
             (%drawing-half-width (/ wid 2))
             (%drawing-half-height (/ hei 2))
             (canvas nil)
             (mesh nil))

        (if *use-opengl-framebuffers*
          (progn
            (setf canvas (get-graphics-canvas-for-screen-obj self wid hei))
            (setf mesh (get-canvas-mesh self))

            (playback-graphics-list-incrementally gl canvas wid hei :mesh mesh)

            (let ((pixmap (graphics-canvas-pixmap canvas)))
              (setf final-mat (3d-matrices:m* final-mat
                                              (3d-matrices:mtranslation (3d-vectors:vec (- (/ wid 2)) (- (/ hei 2)) 0.0)) ))
              (setf (boxgl-device-model-matrix bw::*boxgl-device*) final-mat)
              (update-model-matrix-ubo bw::*boxgl-device*)
              (draw-canvas-mesh mesh pixmap)))
          (boxer-playback-graphics-list gl :use-cur-model-matrix t)))

      (setf (boxgl-device-model-matrix bw::*boxgl-device*) prev-model)
      (update-model-matrix-ubo bw::*boxgl-device*)

      (unless (eq (shown? self) ':no-subsprites)
        (dolist (subs (slot-value self 'subsprites))
          (draw subs))))))

(defmethod show-turtle ((self button))
  (set-shown? self t))

;; hide-turtle is called from inside a with-sprites-hidden macro
;; because hiding a PD sprite can obscure (parts of) other sprites
;; so they must all hide so they can be redrawn.
;; therefore, it needs to suppress the explicit erasing
(defmethod hide-turtle ((self button))
  (set-shown? self nil nil nil))


;;; stuff for mouse-sensitivity

(defmethod enclosing-rectangle ((self button))
  (let ((min-x 0)
        (min-y 0)
        (max-x 0)
        (max-y 0)
        (x (absolute-x-position self))
        (y (absolute-y-position self))
        (visible? (shown? self))
        (shape-gdispl (box-interface-value (slot-value self 'shape)))
        (ahead-rad (* (absolute-heading self) (/ pi 180)))
        (scale (box-interface-value (slot-value self 'sprite-size))))
    (unless (eq visible? ':subsprites)
      (with-graphics-state-bound
        (do-vector-contents (gc shape-gdispl)
          (multiple-value-bind (gc-min-x gc-min-y gc-max-x gc-max-y
                                         state-change?)
                               (graphics-command-extents gc)
            (unless state-change?
              (setq min-x (min gc-min-x min-x)
                    min-y (min gc-min-y min-y)
                    max-x (max gc-max-x max-x)
                    max-y (max gc-max-y max-y)))))))
    (unless (eq visible? ':no-subsprites)
      (dolist (subs (slot-value self 'subsprites))
        (when (absolute-shown? subs)
          (multiple-value-bind (sub-min-x sub-min-y sub-max-x sub-max-y)
            (enclosing-rectangle subs)
            (setq min-x (min min-x sub-min-x)
                  min-y (min min-y sub-min-y)
                  max-x (max max-x sub-max-x)
                  max-y (max max-y sub-max-y))))))

    (let ((top-left (3d-vectors:vec min-x max-y 0))
          (bottom-right (3d-vectors:vec max-x min-y 0))
          (trans-vector (3d-vectors:vec x y 0)))
      ;; scale, rotate, translate
      (3d-vectors:nv* top-left scale)
      (3d-vectors:nv* bottom-right scale)

      (3d-vectors:nvrot top-left (3d-vectors:vec 0 0 1) ahead-rad)
      (3d-vectors:nvrot bottom-right (3d-vectors:vec 0 0 1) ahead-rad)

      (3d-vectors:nv+ top-left trans-vector)
      (3d-vectors:nv+ bottom-right trans-vector)

      (setf min-x (min (3d-vectors:vx3 top-left) (3d-vectors:vx3 bottom-right))
            max-x (max (3d-vectors:vx3 top-left) (3d-vectors:vx3 bottom-right))
            min-y (min (3d-vectors:vy3 top-left) (3d-vectors:vy3 bottom-right))
            max-y (max (3d-vectors:vy3 top-left) (3d-vectors:vy3 bottom-right))))

    (values min-x max-y max-x min-y)))

;;; returns either NIL or the lowest sprite that contains the point
(defmethod sprite-at-window-point ((self button) window-x window-y)
  (let ((point-x (user-coordinate-x window-x))
        (point-y (user-coordinate-y window-y)))
    (multiple-value-bind (min-x max-y max-x min-y)
                         (enclosing-rectangle self)
      (when (and (<=  min-x
                      point-x
                      max-x)
                 (<=  min-y
                      point-y
                      max-y))
        ;; the point is within the sprite, now check any subsprites
        (or (dolist (sub (slot-value self 'subsprites))
              (let ((under? (sprite-at-window-point sub point-x point-y)))
                (when (not (null under?))
                  (return under?))))
            self)))))
