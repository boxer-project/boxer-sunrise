;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
#|


 $Header: gcmeth.lisp,v 1.0 90/01/24 22:12:11 boxer Exp $

 $Log:	gcmeth.lisp,v $
;;;Revision 1.0  90/01/24  22:12:11  boxer
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




  This file contains basic methods for graphics cursor objects


Modification History (most recent at top)

11/07/11 #+/-opengl for new-offscreen-copy
 9/27/11 set-shape now uses fast-vc-has-sprite? instead of fast-eval-sprite-box?
         handled #-opengl compilation warnings in set-shown?
 4/20/10 restored calls to drawing commands in arc, wedge, circle and hollow-circle
 1/02/10 removed explict calls to draw & erase via #-OpenGL in SET-SHOWN?
11/14/09 background-graphics-color does pixel to color conversion
11/05/09 fixed stamp method
 1/26/08 #-opengl all actual drawing commands since opengl only uses the display lists
 3/02/07 parse-color-box:alpha channel support
10/02/05 background-graphics-color can now check for pixel under sprite depending on
         the value of *check-bit-array-color*
 2/16/03 merged current LW and MCL files
 2/15/01 merged current LW and MCL files
 8/19/99 changed set-pen-color, synchronize-graphics-state to use color= instead
         of eql
 4/09/99 stamp-wedge and stamp-arc implemented
10/10/98 numeric RGB clause in get-color-from-color-box now uses %make-color
         instead of make-color-internal (which returned a box (how did this
         EVER work ????))
 7/06/98 add support for background bitmaps in graphics boxes to set-shape
 6/24/98 changed get-color-from-color-box to also check for RGB triple
 5/06/98 change set-type-font to handle new mac type format
 5/06/98 started logging: source = boxer version 2.3

|#

(in-package :boxer)



;;;;;;; Graphics CURSOR methods

(defmethod all-interface-slots ((self graphics-cursor))
  (append (call-next-method)
          (list 'shown? 'pen 'pen-width 'type-font 'pen-color)))

(defmethod link-interface-slots ((self graphics-cursor) sprite-box)
  (call-next-method)
  (install-interface-slot (slot-value self 'shown?)
                          'bu::shown? sprite-box)
  (install-interface-slot (slot-value self 'pen)
                          'bu::pen sprite-box)
  (install-interface-slot (slot-value self 'pen-width)
                          'bu::pen-width sprite-box)
  (install-interface-slot (slot-value self 'pen-color)
                          'bu::pen-color sprite-box)
  (install-interface-slot (slot-value self 'type-font)
                          'bu::type-font sprite-box))

(defmethod unlink-interface-slots ((self graphics-cursor) sprite-box)
  (call-next-method)
  (uninstall-interface-slot (slot-value self 'shown?)
                            'bu::shown? sprite-box)
  (uninstall-interface-slot (slot-value self 'pen)
                            'bu::pen sprite-box)
  (uninstall-interface-slot (slot-value self 'pen-width)
                            'bu::pen-width sprite-box)
  (uninstall-interface-slot (slot-value self 'pen-color)
                            'bu::pen-color sprite-box)
  (uninstall-interface-slot (slot-value self 'type-font)
                            'bu::type-font sprite-box))

(defmethod copy-graphics-slots ((old-gc graphics-cursor)
                                (new-gc graphics-cursor))
  (call-next-method)
  (setf (box-interface-value (slot-value new-gc 'shown?))
        (box-interface-value (slot-value old-gc 'shown?)))
  (setf (box-interface-value (slot-value new-gc 'pen))
        (box-interface-value (slot-value old-gc 'pen)))
  (setf (box-interface-value (slot-value new-gc 'pen-width))
        (box-interface-value (slot-value old-gc 'pen-width)))
  (setf (box-interface-value (slot-value new-gc 'pen-color))
        (box-interface-value (slot-value old-gc 'pen-color)))
  (setf (box-interface-value (slot-value new-gc 'type-font))
        (box-interface-value (slot-value old-gc 'type-font))))




;;;; Shape

(defun graphics-list-extent (gl)
  (let ((min-x 0) (min-y 0) (max-x 0) (max-y 0))
    (with-graphics-state-bound
      (do-vector-contents (gc gl)
        (multiple-value-bind (gc-min-x gc-min-y gc-max-x gc-max-y
                                       state-change?)
                             (graphics-command-extents gc)
                             (unless state-change?
                               (setq min-x (min gc-min-x min-x)
                                     min-y (min gc-min-y min-y)
                                     max-x (max gc-max-x max-x)
                                     max-y (max gc-max-y max-y))))))
    (values (* 2 (1+ (max (abs min-x) (abs max-x))))
            (* 2 (1+ (max (abs min-y) (abs max-y)))))))

(defun shape-box-updater-internal (box shape)
    ;; first make sure there is a graphics sheet
    ;; don't worry about the size, we'll be adjusting it...
    (when (null (graphics-info box))
      (setf (graphics-info box) (make-graphics-sheet 0 0 box)))
    ;; now that we are sure there is a graphics sheet,
    ;; figure out the size it is supposed to be
    (let ((gs (graphics-info box)))
      (multiple-value-bind (width height)
                           (graphics-list-extent shape)
                           (setf (graphics-sheet-draw-wid gs) (values (ceiling width))
                                 (graphics-sheet-draw-hei gs) (values (ceiling height))))
      ;; now update the graphics list
      (with-graphics-vars-bound (box)
        (clear-graphics-list %graphics-list)
        (do-vector-contents (shape-gc shape)
          (sv-append %graphics-list shape-gc))))
    ;; make sure we are in graphics mode
    (setf (display-style-graphics-mode? (display-style-list box)) t))

(defun shape-box-updater (bi)
  (let ((value (box-interface-value bi)) (box (box-interface-box bi)))
    (shape-box-updater-internal box value)
    (modified-graphics box)
    (flush-editor-box-caches box)
    (queue-editor-object-for-mutation bi)))


;; 2023-07-07 sgithens This has been updated to change both the shape slot and the graphics
;; of the containing shape box. The sprite-box? and virtual-copy? options below may still
;; need updates.
(defmethod set-shape ((self button) new-shape-box &optional dont-update-box)
  (let* ((shape-slot (slot-value self 'shape))
         (shape     (box-interface-value shape-slot)) ;; This is our graphics list we're updating in the shape box
         (shape-box (box-interface-box shape-slot))
         (new-extents nil))

    (clear-box shape-box)

    (cond ((sprite-box? new-shape-box)
           ;; just copy the sprite's shape into the current shape
           (dub-graphics-list (shape (slot-value new-shape-box
                                                 'associated-turtle))
                              shape :replace))
      ((and (virtual-copy? new-shape-box)
            (fast-vc-has-sprite? new-shape-box))
       ;; just copy the sprite's shape into the current shape
       (dub-graphics-list (shape (getf (vc-graphics new-shape-box) 'turtle))
                          shape :replace))
      (t
       ;; must be some flavor of graphics box
       ;; need to convert to from window to turtle commands
       (with-graphics-vars-bound (new-shape-box)
         (clear-graphics-list shape)

         (unless (null %bit-array)
           ;; if the new-shape-box has a background bitmap, lay that in first
           (sv-append shape (make-boxer-graphics-command-centered-bitmap
                             (new-offscreen-copy %bit-array)
                             0 0 (ogl-pixmap-width %bit-array)
                             (ogl-pixmap-height %bit-array))))
         (do-vector-contents (graphics-command %graphics-list)
           (sv-append shape (allocate-window->boxer-command
                              graphics-command)))

         (setf new-extents (boxer::graphics-command-list-extents shape))

         (setf (boxer::graphics-sheet-graphics-list (boxer::graphics-info shape-box))
               shape)

         (setf (boxer::graphics-sheet-draw-wid (boxer::graphics-info shape-box)) (floor (* 2 (+ (abs (nth 0 new-extents)) (abs (nth 2 new-extents))))))
         (setf (boxer::graphics-sheet-draw-hei (boxer::graphics-info shape-box)) (floor (* 2 (+ (abs (nth 1 new-extents)) (abs (nth 3 new-extents)))))))))

    ;; handle any box interface...
    ; (when (and (null dont-update-box)
    ;            (not (null (box-interface-box shape-slot))))
    ;   (shape-box-updater shape-slot))
))



;;; once again, need to figure out what we are doing here and flush all this
;;; multi-package symbol lossage
;;; the explicit arg is used to specify whether we need to draw/erase
;;; in the function or if it is already being taken care of (like inside
;;; a with-sprites-hidden macro)

(defmethod set-shown? ((self graphics-cursor) new-value
                                              &optional dont-update-box (explicit t))
  (let* ((slot (slot-value self 'shown?))
         (box (box-interface-box slot)))
    (multiple-value-bind (word value)
                         (case new-value
                           ((t bu::all bu::true) (values 'bu::true t))
                           ((nil bu::none bu::false) (values 'bu::false nil))
                           ((:subsprites bu::subsprites) (values 'bu::subsprites :subsprites))
                           ((:no-subsprites bu::no-subsprites)
                            (values 'bu::no-subsprites ':no-subsprites)))
                         (when (and (null dont-update-box) (not (null box)))
                           (bash-box-to-single-value box word))
                         (setf (box-interface-value slot) value))))

(defmethod shown?-symbol ((self graphics-cursor))
  (case (box-interface-value (slot-value self 'shown?))
    ((nil) 'false)
    ((:subsprites) 'subsprites)
    ((:no-subsprites) 'no-subsprites)
    (t 'true)))

(defmethod shown? ((self graphics-cursor))
  (box-interface-value (slot-value self 'shown?)))

(defmethod add-shown?-box ((self graphics-cursor) box)
  (setf (box-interface-box (slot-value self 'shown?)) box))

(defmethod subsprites-shown? ((self graphics-cursor))
  (let ((superior-turtle (slot-value self 'superior-turtle)))
    (if (not (null superior-turtle))
      (and (subsprites-shown? superior-turtle)
           (fast-memq (box-interface-value (slot-value self 'shown?))
                      '(t :subsprites)))
      (fast-memq (box-interface-value (slot-value self 'shown?))
                 '(t :subsprites)))))

(defmethod absolute-shown? ((self graphics-cursor))
  (let ((sh (box-interface-value (slot-value self 'shown?)))
        (superior-turtle (slot-value self 'superior-turtle)))
    (if superior-turtle
      (if (null sh)
        nil
        (subsprites-shown? superior-turtle))
      (not (null sh)))))

(defmethod pen ((self graphics-cursor))
  (box-interface-value (slot-value self 'pen)))

(defmethod add-pen-box ((self graphics-cursor) box)
  (setf (box-interface-box (slot-value self 'pen)) box))

(defmethod pen-up-or-xor? ((self graphics-cursor))
  (let ((pen (box-interface-value (slot-value self 'pen))))
    (or (eq pen 'bu::up) (eq pen 'bu::reverse))))

;; figure out what is going on and get rid of all these different
;; symbols from different packages !!!
(defun get-alu-from-pen (pen-mode)
  (case pen-mode
    ((bu::down down) alu-seta)
    ;; used to be alu-ior but that's wrong for color
    ((bu::up up) nil)
    ((bu::xor xor bu::reverse) alu-xor)
    ((bu::erase erase) alu-andca)))

(defmethod background-graphics-color ((self graphics-cursor))
  (or
   (let ((gb (slot-value self 'assoc-graphics-box)))
     (unless (null gb)
       (let ((gs (graphics-info gb)))
         (unless (null gs)
           (let ((bit-array (graphics-sheet-bit-array gs))
                 (background (graphics-sheet-background gs)))
             (cond ((not (null bit-array))
                    (with-graphics-vars-bound-internal gs
                      (let ((gb-x (fix-array-coordinate-x (x-position self)))
                            (gb-y (fix-array-coordinate-y (y-position self))))
                        (if (and (<=& 0 gb-x (graphics-sheet-draw-wid gs))
                                 (<=& 0 gb-y (graphics-sheet-draw-hei gs)))
                          (cond ((not (null bit-array))
                                 (offscreen-pixel-color gb-x gb-y bit-array))
                            ((not (null background))
                             background)
                            (t *background-color*))))))
               (t
                background)))))))
   *background-color*))

(defmethod set-pen ((self graphics-cursor) new-value &optional dont-update-box)
  (let ((pen-alu (get-alu-from-pen new-value))
        (slot (slot-value self 'pen)))
    (setf (box-interface-value slot) new-value)
    (when (null %learning-shape?)
      (let ((box (box-interface-box slot)))
        (when (and (null dont-update-box) (not (null box)))
          (bash-box-to-single-value box new-value))))
    (unless (or (null pen-alu) (eq %graphics-box :no-graphics))
      (let ((erase-color (when (eq new-value 'bu::erase)
                           (background-graphics-color self))))
        (cond ((eq self (graphics-command-list-agent %graphics-list))
               ;; if the current sprite is still the "owner" then just
               ;; record the change in alu with special handling for
               ;; erasing in a graphics box which has a background
               (cond (erase-color
                      (unless (eql *graphics-state-current-alu* alu-seta)
                        (record-boxer-graphics-command-change-alu alu-seta)
                        (change-alu alu-seta))
                      (unless (eql *graphics-state-current-pen-color*
                                   erase-color)
                        (record-boxer-graphics-command-change-graphics-color
                         erase-color)
                        (change-graphics-color erase-color)))
                 (t
                  (unless (eql *graphics-state-current-alu* pen-alu)
                    (record-boxer-graphics-command-change-alu pen-alu)
                    (change-alu pen-alu))
                  ;; check the validity of the color since it may
                  ;; have been changed by a previous erase command, note
                  ;; that we can't check the current value of the alu
                  ;; for 'bu::erase because there may have been
                  ;; an intervening PENUP command

                  ;; always record when learning shape because we
                  ;; can't side effect the sprite's state to
                  ;; check if the current color is correcta
                  (let ((gc (box-interface-value (slot-value self
                                                             'pen-color))))
                    (unless (eql *graphics-state-current-pen-color* gc)
                      (record-boxer-graphics-command-change-graphics-color
                       gc)
                      (change-graphics-color gc))))))
          (t
           ;; first, make sure the current state of the graphics list
           ;; matches the state of the drawing sprite, this is handled
           ;; in a different way if the sprite is erasing
           (cond (erase-color
                  (synchronize-graphics-state-for-erase self erase-color))
             (t (synchronize-graphics-state self)))
           ;; Now that everything agrees, set the new "owner"
           (setf (graphics-command-list-agent %graphics-list) self)))))))

(defmethod pen-width ((self graphics-cursor))
  (box-interface-value (slot-value self 'pen-width)))

(defmethod set-pen-width ((self graphics-cursor) new-value
                                                 &optional dont-update-box)
  (unless (typep new-value 'fixnum)
    (setq new-value (values (ceiling new-value))))
  ;; Bash slots in the data structure only if we are NOT recording a shape
  (when (null %learning-shape?)
    (let* ((slot (slot-value self 'pen-width))
           (box (box-interface-box slot)))
      (when (and (null dont-update-box) (not (null box)))
        (bash-box-to-single-value box new-value))))
  (setf (box-interface-value (slot-value self 'pen-width)) new-value)
  ;; Record appropriate changes into the graphics list
  (unless (null %graphics-list)
    (if (eq self (graphics-command-list-agent %graphics-list))
      ;; if the current sprite is still the "owner" then just
      ;; record the change in pen-width
      (unless (eql *graphics-state-current-pen-width* new-value)
        (record-boxer-graphics-command-change-pen-width new-value))
      ;; otherwise, we need to set a new "owner"
      (progn
       (synchronize-graphics-state self t)
       (setf (graphics-command-list-agent %graphics-list) self)))
    ;; finally, change the global value
    (change-pen-width new-value)))

(defvar *pen-color-graphics-size* 15)
(defvar *pen-color-graphics-width* 20)

;;; works on Virtual copies too !!
(defun get-graphics-sheet (box)
  (if (box? box)
    (graphics-info box)
    (getf (vc-graphics box) 'graphics-sheet)))

(defun get-color-from-color-box (box)
  (let ((gs (get-graphics-sheet box)))
    (cond ((null gs)
           ;; no graphics, check for 3 numbers...
           (let* ((entries (car (raw-unboxed-items box)))
                  (red (car entries)) (green (cadr entries))
                  (blue (caddr entries)))
             (when (and (numberp red)   (<= 0 red   100)
                        (numberp green) (<= 0 green 100)
                        (numberp blue)  (<= 0 blue  100))
               (%make-color red green blue))))
      (t (graphics-sheet-background gs)))))

(defun color-box? (box)
  (let* ((entries (car (raw-unboxed-items box)))
         (red (car entries)) (green (cadr entries)) (blue (caddr entries)))
    (and (numberp red)   (<= 0 red   100)
         (numberp green) (<= 0 green 100)
         (numberp blue)  (<= 0 blue  100))))

;;; returns RGB,alpha values.  Error checking should happen at a higher level
(defun parse-color-box (box)
  (let ((entries (car (raw-unboxed-items box))))
    (values (car entries) (cadr entries) (caddr entries) (cadddr entries))))

(defun pen-color-box-updater (bi)
  (let ((value (box-interface-value bi)) (box (box-interface-box bi)))
    ;; first make sure there is a graphics sheet
    (when (null (graphics-info box))
      (setf (graphics-info box)
            (%make-simple-graphics-sheet *pen-color-graphics-size*
                                         *pen-color-graphics-size*
                                         box)))
    ;; now that we are sure there is a graphics sheet, update
    ;; it by setting the background color
    (setf (graphics-sheet-background (graphics-info box)) value)
    ;; values in box...
    (bash-box-to-list-value box (pixel-rgb-values value))
    (modified-graphics box)
    (flush-editor-box-caches box) (queue-editor-object-for-mutation bi)))

(defmethod pen-color ((self graphics-cursor))
  (box-interface-value (slot-value self 'pen-color)))

(defmethod set-pen-color ((self graphics-cursor) new-value
                                                 &optional dont-update-box)
  ;; Bash slots in the data structure only if we are NOT recording a shape
  (when (null %learning-shape?)
    (let ((slot (slot-value self 'pen-color)))
      (when (and (null dont-update-box) (not (null (box-interface-box slot))))
        (pen-color-box-updater slot))))
  (setf (box-interface-value (slot-value self 'pen-color)) new-value)
  ;; Record appropriate changes into the graphics list
  (unless (null %graphics-list)
    (if (eq self (graphics-command-list-agent %graphics-list))
      ;; if the current sprite is still the "owner" then just
      ;; record the change in pen-color
      (unless (color= *graphics-state-current-pen-color* new-value)
        (record-boxer-graphics-command-change-graphics-color new-value))
      ;; otherwise, we need to set a new "owner"
      (progn
       (synchronize-graphics-state self t)
       (setf (graphics-command-list-agent %graphics-list) self)))
    ;; finally, change the global value
    (change-graphics-color new-value)))

(defmethod type-font ((self graphics-cursor))
  (box-interface-value (slot-value self 'pen-width)))

(defun font-name-from-font-no (font-no) font-no)


(defmethod set-type-font ((self graphics-cursor) new-value
                                                 &optional dont-update-box)
  ;; Bash slots in the data structure only if we are NOT recording a shape
  (when (null %learning-shape?)
    (let* ((slot (slot-value self 'type-font))
           (box (box-interface-box slot)))
      (when (and (null dont-update-box) (not (null box)))
        (bash-box-to-list-value box
                               (list* (make-box (list (list (font-name new-value))))
                                      (font-size new-value)
                                      (font-styles new-value)))
  )))
  (setf (box-interface-value (slot-value self 'type-font)) new-value)
  ;; Record appropriate changes into the graphics list
  (unless (null %graphics-list)
    (if (eq self (graphics-command-list-agent %graphics-list))
      ;; if the current sprite is still the "owner" then just
      ;; record the change in font
      (unless (eql *graphics-state-current-font-no* new-value)
        (record-boxer-graphics-command-change-graphics-font new-value))
      ;; otherwise, we need to set a new "owner"
      (progn
       (synchronize-graphics-state self t)
       (setf (graphics-command-list-agent %graphics-list) self)))
    ;; finally, change the global value
    (change-graphics-font new-value)))



;;; this is called BEFORE any drawing or recording of graphics commands
;;; It makes sure that the turtle's graphics state agrees with the
;;; currently bound graphics state and if it doesn't agree,
;;; the recording list is changed as well as the currently bound variables
;;; that describe the current graphics state

(defmethod synchronize-graphics-state ((agent graphics-cursor)
                                       &optional (force? nil))
  (let* ((pw (box-interface-value (slot-value agent 'pen-width)))
         (gc (box-interface-value (slot-value agent 'pen-color)))
         (pen-state (box-interface-value (slot-value agent 'pen)))
         (alu (get-alu-from-pen pen-state))
         (font (box-interface-value (slot-value agent 'type-font)))
         ;; this has to be bound explicitly here because this method can
         ;; be called INSIDE of update-shape
         ;; sgithens 2023-07-12 we are always in :boxer mode now
         ;  (*graphics-command-recording-mode* ':window)
         ;; Do not supress recording of these synchronization commands
         (*supress-graphics-recording?* nil))
    (unless (and (not force?) (eq pen-state 'up))
      ;; don't need to synchronize if the sprite isn't going to be
      (unless (eql *graphics-state-current-alu* alu)
        (unless (null alu)
          (record-boxer-graphics-command-change-alu alu))
        (setf (graphics-command-list-alu %graphics-list) alu)
        (change-alu (or alu alu-andca)))
      (unless (eql *graphics-state-current-pen-width* pw)
        (record-boxer-graphics-command-change-pen-width pw)
        (setf (graphics-command-list-pen-width %graphics-list) pw)
        (change-pen-width pw))
      (unless (color= *graphics-state-current-pen-color* gc)
        (record-boxer-graphics-command-change-graphics-color gc)
        (setf (graphics-command-list-pen-color %graphics-list) gc)
        (change-graphics-color gc))
      (unless (eql *graphics-state-current-font-no* font)
        (record-boxer-graphics-command-change-graphics-font font)
        (setf (graphics-command-list-font-no %graphics-list) font)
        (change-graphics-font font)))))

;; similiar to synchronize-graphics-state except we synch to erasing
;; values instead of the intrinsic values of the sprite
(defmethod synchronize-graphics-state-for-erase ((agent graphics-cursor)
                                                 erase-color)
  (let* ((pw (box-interface-value (slot-value agent 'pen-width)))
         (font (box-interface-value (slot-value agent 'type-font)))
         ;; this has to be bound explicitly here because this method can
         ;; be called INSIDE of update-shape
         ;; sgithens 2023-07-12 we are always in :boxer mode now
         ;  (*graphics-command-recording-mode* ':window)
         )
    (unless (eql *graphics-state-current-alu* alu-seta)
      (record-boxer-graphics-command-change-alu alu-seta)
      (change-alu alu-seta))
    (unless (eql *graphics-state-current-pen-width* pw)
      (record-boxer-graphics-command-change-pen-width pw)
      (change-pen-width pw))
    (unless (color= *graphics-state-current-pen-color* erase-color)
      (record-boxer-graphics-command-change-graphics-color erase-color)
      (change-graphics-color erase-color))
    (unless (eql *graphics-state-current-font-no* font)
      (record-boxer-graphics-command-change-graphics-font font)
      (change-graphics-font font))))

;; this is like synchronize-graphics-state EXCEPT that it
;; ONLY puts entries in the beginning of its graphics-command-list
;; and DOESN'T try to side effect any bindings
;;;
;; This ignores color until we decide what whether or not is will be
;; feasible/efficient to handle different colors in turtle shapes
;; obviously, it is desireable but the question is "at what price ?"

#|
;; this version inherits the pen state fron the turtle

(defmethod new-shape-preamble ((agent graphics-cursor) graphics-command-list)
  (setf (graphics-command-list-agent graphics-command-list) agent)
  (let ((pw (box-interface-value (slot-value agent 'pen-width)))
    (alu (or (get-alu-from-pen
          (box-interface-value (slot-value agent 'pen)))
         alu-andca))
    (font (box-interface-value (slot-value agent 'type-font)))
    (%graphics-list graphics-command-list))
    ;; the current alu of the turtle
    (setf (graphics-command-list-alu graphics-command-list) alu)
    (record-boxer-graphics-command-change-alu alu)
    ;; the current pen-width of the turtle
    (setf (graphics-command-list-pen-width graphics-command-list) pw)
    (record-boxer-graphics-command-change-pen-width pw)
    ;; the current type-font of the turtle...
    (setf (graphics-command-list-font-no graphics-command-list) font)
    (record-boxer-graphics-command-change-graphics-font font)))

|#

;; we may want to conditionalize some of these for different displays
(defun initial-shape-alu () alu-xor)
(defun initial-shape-pen-symbol () 'bu::reverse) ; this should match the alu
(defun initial-shape-pen-width () *initial-graphics-state-current-pen-width*)
(defun initial-shape-type-font () *sprite-type-font-no*)
(defun initial-shape-pen-color () *foreground-color*)

(defmethod new-shape-preamble ((agent graphics-cursor) graphics-command-list)
  (setf (graphics-command-list-agent graphics-command-list) agent)
  (let (;(font (box-interface-value (slot-value agent 'type-font)))
        (%graphics-list graphics-command-list))
    ;; the current alu of the turtle
    (setf (graphics-command-list-alu graphics-command-list)
          (initial-shape-alu))
    (record-boxer-graphics-command-change-alu (initial-shape-alu))
    ;; need to side effect the pen state of the turtle because the optimized
    ;; graphics command recording mechanism will not record if the turtle's
    ;; pen state is BU::UP which it will be if we are changing shapes while
    ;; the turtle's pen is up even though the canonical initial state for
    ;; drawing is XOR
    (setf (box-interface-value (slot-value agent 'pen))
          (initial-shape-pen-symbol))
    ;; the current pen-width of the turtle
    (setf (graphics-command-list-pen-width graphics-command-list)
          (initial-shape-pen-width))
    (record-boxer-graphics-command-change-pen-width (initial-shape-pen-width))
    ;; the pen color
    (setf (graphics-command-list-pen-color graphics-command-list)
          (initial-shape-pen-color))
    (record-boxer-graphics-command-change-graphics-color
     (initial-shape-pen-color))
    ;; the current type-font of the turtle...
    (setf (graphics-command-list-font-no graphics-command-list)
          (initial-shape-type-font))
    (record-boxer-graphics-command-change-graphics-font
     (initial-shape-type-font))))




;;;; Drawing Methods....

;;; turds....

(defmethod stamp-dot ((self graphics-cursor))
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-dot (x-position self)
                                          (y-position self)))
      ((not (no-graphics?))
       (record-boxer-graphics-command-dot (x-position self) (y-position self))))))

;;;general rects

(defmethod turtle-rect ((self graphics-cursor) wid hei
                                               &optional (orientation :centered))
  (declare (ignore orientation))	; a hook for later
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-centered-rectangle
        (x-position self) (y-position self)
        (coerce wid 'boxer-float) (coerce hei 'boxer-float)))
      ((not (no-graphics?))
         (record-boxer-graphics-command-centered-rectangle
          (x-position self) (y-position self) wid hei)))))

(defmethod hollow-turtle-rect ((self graphics-cursor) wid hei
                                                      &optional (orientation :centered))
  (declare (ignore orientation))	; a hook for later
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-hollow-rectangle
        (x-position self) (y-position self)
        (coerce wid 'boxer-float) (coerce hei 'boxer-float)))
      ((not (no-graphics?))
       (let ((array-x (fix-array-coordinate-x (absolute-x-position self)))
             (array-y (fix-array-coordinate-y (absolute-y-position self))))
         (record-boxer-graphics-command-hollow-rectangle
          array-x array-y wid hei))))))

;;; Circle, Ellipses and Arcs

(defmethod stamp-ellipse ((self graphics-cursor) wid hei
                                                 &optional (orientation :centered))
  (declare (ignore orientation))	; a hook for later
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-filled-ellipse
        (x-position self) (y-position self)
        (coerce wid 'boxer-float) (coerce hei 'boxer-float)))
      ((not (no-graphics?))
         (record-boxer-graphics-command-filled-ellipse
          (x-position self) (y-position self) wid hei)))))

(defmethod stamp-hollow-ellipse ((self graphics-cursor) wid hei
                                                        &optional (orientation :centered))
  (declare (ignore orientation))	; a hook for later
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-ellipse
        (x-position self) (y-position self)
        (coerce wid 'boxer-float) (coerce hei 'boxer-float)))
      ((not (no-graphics?))
         (record-boxer-graphics-command-ellipse
          (x-position self) (y-position self) wid hei)))))

(defmethod stamp-circle ((self graphics-cursor) radius)
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-filled-circle
        (x-position self) (y-position self) (coerce radius 'boxer-float)))
      ((not (no-graphics?))
         (record-boxer-graphics-command-filled-circle
          (x-position self) (y-position self) radius)
         (with-graphics-screen-parameters
           (filled-circle (x-position self) (y-position self) radius))))))

(defmethod stamp-hollow-circle ((self graphics-cursor) radius)
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-circle
        (x-position self) (y-position self) (coerce radius 'boxer-float)))
      ((not (no-graphics?))
         (record-boxer-graphics-command-circle (x-position self) (y-position self) radius)
         (with-graphics-screen-parameters
           (circle (x-position self) (y-position self) radius))))))

(defmethod stamp-wedge ((self graphics-cursor) radius sweep-angle)
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-wedge
        (x-position self) (y-position self) (coerce radius 'boxer-float)
        (heading self) sweep-angle))
      ((not (no-graphics?))
        (record-boxer-graphics-command-wedge
          (x-position self) (y-position self) radius (absolute-heading self) sweep-angle)
        (with-graphics-screen-parameters
          (wedge (x-position self) (y-position self) radius (absolute-heading self) sweep-angle))))))

(defmethod stamp-arc ((self graphics-cursor) radius sweep-angle)
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-arc
        (x-position self) (y-position self) (coerce radius 'boxer-float)
        (heading self) sweep-angle))
      ((not (no-graphics?))
         (record-boxer-graphics-command-arc
           (x-position self) (y-position self) radius (absolute-heading self) sweep-angle)
         (with-graphics-screen-parameters
           (arc (x-position self) (y-position self) radius (absolute-heading self) sweep-angle))))))


;;;; Pictures

(defun new-offscreen-copy (ba)
  (let* ((w (ogl-pixmap-width ba)) (h (ogl-pixmap-height ba))
                                         (new-bm (make-ogl-pixmap w h)))
    (copy-pixmap-data w h ba 0 0 new-bm 0 0)
    new-bm))

(defmethod stamp-bitmap ((self graphics-cursor) bitmap wid hei
                                                &optional (orientation :centered))
  (declare (ignore orientation)) ;; a hook for later
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-centered-bitmap
        ;; shouldn't we be copying this bitmap here ???
        (new-offscreen-copy bitmap) (x-position self) (y-position self)
        (coerce wid 'boxer-float) (coerce hei 'boxer-float)))
      ((not (no-graphics?))
         (record-boxer-graphics-command-centered-bitmap
          (new-offscreen-copy bitmap) (x-position self) (y-position self) wid hei)))))

;;; orientation can be :centered, :right or :left
(defmethod type-box ((self graphics-cursor) box
                                            &optional (orientation ':centered))
  (let ((alu (get-alu-from-pen (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (ecase orientation
              (:centered (record-boxer-graphics-command-centered-string
                          (x-position self) (y-position self)
                          (box-text-string box)))
              (:right (record-boxer-graphics-command-right-string
                       (x-position self) (y-position self) (box-text-string box)))
              (:left (record-boxer-graphics-command-left-string
                      (x-position self) (y-position self) (box-text-string box)))))
      ((not (no-graphics?))
         (ecase orientation
                (:centered (record-boxer-graphics-command-centered-string
                             (x-position self) (y-position self) (box-text-string box)))
                (:right (record-boxer-graphics-command-right-string
                          (x-position self) (y-position self) (box-text-string box)))
                (:left (record-boxer-graphics-command-left-string
                          (x-position self) (y-position self) (box-text-string box))))))))


(defmethod stamp ((self graphics-cursor))
  (let ((pen-mode (get-alu-from-pen (pen self))))
    (when (and pen-mode (not (no-graphics?)))
      (draw-update self)

; Ok, looking back at a previous tag, we need to add dub-graphics-list back, expect
; I'm not sure it will be translated... perhpaps we could just add a setxy command before
; it... but we'd have to keep track of the pen state and stuff. ugh.  Maybe we should
; just translate the graphics command list of the shape.

      (dub-graphics-list (box-interface-value (slot-value self 'shape)))
        ; :translate? t :trans-x (x-position self) :trans-y (y-position self))

      ;; reset the state values which may have been bashed during the
      ;; dub to be the state value of the shape
      (synchronize-graphics-state self t))))
