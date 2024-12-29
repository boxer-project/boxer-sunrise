;; -*- Mode:LISP; Syntax:Common-Lisp;Package:BOXER;-*-
#|


 $Header: sprite.lisp,v 1.0 90/01/24 22:17:32 boxer Exp $

 $Log:	sprite.lisp,v $
;;;Revision 1.0  90/01/24  22:17:32  boxer
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



  This File contains the Definition of Sprite boxes, the
  part of the graphics subsystem which is directly imbedded
  in the boxer editor hierarchy

Modification History (most recent at top)

 3/20/07 add/remove-graphics-object for sprite-boxes removed.  Functionality folded
         into plain BOX method in grobjs.lisp
 2/26/07 make-sprite-box
 3/26/03 Merged current LW and MCL src

|#

(in-package :boxer)

;;; The relationship between the slot value of the turtle
;;; and the box is defined by:
(defun sprite-instance-make-box-arg-from-slot-value (value)
  (etypecase value
     (number `((,value)))
     (symbol `((,(convert-sprite-instance-symbol-value value))))
     (vector (convert-graphics-list-to-make-box-format value))
     (list   `(,value))))

(defun convert-sprite-instance-symbol-value (value)
  (cond ((eq value t) 'bu::true)
        ((eq value nil) 'bu::false)
        (t value)))

(defun convert-graphics-list-to-make-box-format (value)
  (let ((last-x 0) (last-y 0) (box-rows nil))
    (declare (Special last-x last-y))
    (do-vector-contents (c value)
      (push (translate-graphics-command-to-sprite-primitive c)
            box-rows))
    (nreverse box-rows)))


(defun make-sprite-instance-var-trigger (slot-name)
  (make-box `((,(or (get slot-name 'update-function-name) 'bu::beep)))
            'doit-box
            'modified-trigger))

(defun make-sprite-instance-var (bi)
  (let* ((slot-name (box-interface-slot-name bi))
         (slot-value (box-interface-value bi))
         (closet-row (make-row
                      `(,(make-sprite-instance-var-trigger slot-name))))
         (varbox
          (cond ((sv-box-interface? bi)
                 (let ((box (make-box '(()) 'data-box slot-name)))
                   ;; hack because the update-function expects the box
                   ;; to be in the box-interface-box slot
                   (setf (box-interface-box bi) box)
                   (funcall (special-box-interface-update-function bi) bi)
                   box))
                (t
                 (make-box
                  (sprite-instance-make-box-arg-from-slot-value slot-value)
                  'data-box slot-name)))))
    (add-closet-row varbox closet-row)
    varbox))


(defun make-sprite-box (&optional (graphics-obj
                                   (make-instance
                                    *default-graphics-object-class*))
                                  sprite)
  (let* ((sprite-box (or sprite (make-initialized-box :type 'data-box
                                                      :graphics-info graphics-obj)))
         (1st-row (first-inferior-row sprite-box))
         (closet-row (make-row '())))
    ;; first set up the graphics-obj's back-pointer
    (set-sprite-box graphics-obj sprite-box)
    ;; attach the closet-row to the box
    (add-closet-row sprite-box closet-row)
    ;; Put some whitespace at the beginning of the first row
    ;; to make it easier for mouse wielding people to enter the
    ;; sprite-box
    (append-cha 1st-row #\Space)
    ;; create any default boxes here
    (multiple-value-bind (slots-to-be-created-in-box
                          slots-to-be-created-in-closet)
        (interface-boxes-to-make graphics-obj)
      (dolist (slot-name slots-to-be-created-in-box)
        (append-cha 1st-row
                    (make-sprite-instance-var (slot-value graphics-obj
                                                          slot-name)))
        (append-cha 1st-row #\Space))
      ;; insert command for concretizing if need be
      (unless (null (set-difference (all-interface-slots graphics-obj)
                                    (append slots-to-be-created-in-box
                                            slots-to-be-created-in-closet)))
        (let ((command-string "Show-Sprite-Properties"))
          (dotimes (i (length command-string))
            (append-cha closet-row (aref command-string i)))))
      (dolist (slot-name slots-to-be-created-in-closet)
        (append-cha closet-row
                    (make-sprite-instance-var (slot-value graphics-obj
                                                          slot-name))))
      sprite-box)))


;;; These Methods hookup sprite boxes and graphics boxes.

(defmethod add-inferior-sprite ((self box) sprite)
  (cond ((or (graphics-box? self)
             (sprite-box? self))
         (add-graphics-object self (slot-value sprite 'graphics-info)))
        (t
         (let ((superior-box (superior-box self)))
           (push sprite (getf (slot-value self 'plist) 'inferior-sprites))
           (unless (or (null (slot-value self 'exports))
                       (null superior-box))
             ;; if this box isn't exporting, then stop right here
             ;; The exports check should be more specific and
             ;; check for the actual name of the initial sprite
             ;; but since we don't have user interface for
             ;; limited exports, the null check is enough for now
             (add-inferior-sprite superior-box sprite))))))

(defmethod remove-inferior-sprite ((self box) sprite)
  (cond ((or (graphics-box? self) (sprite-box? self))
         (remove-graphics-object self (slot-value sprite 'graphics-info)))
        (t
         (let ((superior-box (superior-box self)))
           (setf (getf (slot-value self 'plist) 'inferior-sprites)
                 (fast-delq sprite
                            (getf (slot-value self 'plist) 'inferior-sprites)))
           (unless (or (null (slot-value self 'exports))
                       (null superior-box))
             ;; if this box isn't exporting, then stop right here
             ;; The exports check should be more specific and
             ;; check for the actual name of the initial sprite
             ;; but since we don't have user interface for
             ;; limited exports, the null check is enough for now
             (remove-inferior-sprite superior-box sprite))))))



;;; Make Interface Box Utilities

;; these are called by the evaluator

(defun connect-and-imbed-interface-slot (bi)
  (let ((new-box (make-sprite-instance-var bi))
        (closet-row (or (slot-value (box-interface-sup-box bi) 'closets)
                        (let ((new-closet (make-row '())))
                          (add-closet-row (box-interface-sup-box bi)
                                          new-closet)
                          new-closet))))
    (append-cha closet-row new-box)
    new-box))
