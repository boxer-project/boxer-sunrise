;;;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
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
;;;;                                        +-Data--+
;;;;               This file is part of the | BOXER | system
;;;;                                        +-------+
;;;;
;;;;
;;;;
;;;;     This file contains boxer functions which use the
;;;;     graphics subsystem via sprites
;;;;
;;;;
;;;;
;;;;  Modification History (most recent at the top)
;;;;
;;;;   1/25/11 fixed bug in touching? which still used sprite-box's "associated-turtle" mechanism
;;;;           {screen,background}-{pixel,color}-from-turtle, color-under?
;;;;  11/15/09 changed (off)screen-pixel-from-turtle to (off)screen-color-from-turtle to clarify pixel/color
;;;;           issues
;;;;           color-under prims all rewritten
;;;;   6/02/07 mouse throttling for opengl version of follow-mouse
;;;;   7/16/04 Fixed Copyright error message in STAMP-WEDGE, STAMP-ARC
;;;;   4/19/03 merged current LW and MCL files
;;;;  11/10/02 added STAMP-PIE and STAMP-CRUST
;;;;   9/03/02 UC free version of STAMP-WEDGE/ARC, new DRAW-WEDGE/ARC
;;;;   2/09/02 append-graphics-sheet-at now will splice in colored backgrounds as
;;;;           centered-rectangles
;;;;   2/15/01 merged current LW and MCL files
;;;;   4/09/99 added bu::stamp-wedge and bu::stamp-arc
;;;;  10/02/98 bu::follow-mouse is smarter about choosing the correct screen-box
;;;;   6/24/98 changed the input flavor for STAMP from port-to to dont-copy to avoid
;;;;           unhelpful porting to copies warnings
;;;;   6/24/98 Start logging changes: source = boxer version 2.3
;;;;

(in-package :boxer)



(defsprite-function bu::towards ((boxer-eval::numberize x) (boxer-eval::numberize y))
    (sprite turtle)
  (towards turtle x y))

(defsprite-function bu::distance ((boxer-eval::numberize x) (boxer-eval::numberize y))
    (sprite turtle)
  (turtle-distance turtle x y))


;;; (defsprite-function bu::flash-name ()
;;;         (sprite turtle)
;;;   (flash-name turtle)
;;;   boxer-eval::*novalue*)

(defsprite-function bu::touching? ((bu::port-to other-sprite))
    (sprite turtle)
  (let ((ot (graphics-info (box-or-port-target other-sprite))))
    (cond ((turtle? ot)
           (boxer-eval::boxer-boolean (touching? turtle ot)))
          (t (boxer-eval::primitive-signal-error :sprite "No graphics in " other-sprite)))))

(defsprite-function bu::enclosing-rectangle () (sprite turtle)
  (multiple-value-bind (Left top right bottom)
      (enclosing-rectangle turtle)
    (if (null left)
        (boxer-eval::primitive-signal-error :sprite-error
                                            "Sprite is not in a graphics box")
        (make-virtual-copy :rows
                           (list (make-evrow :pointers
                                             (list (make-pointer left)
                                                   (make-pointer top)
                                                   (make-pointer right)
                                                   (make-pointer bottom))))))))

;;; This only works with single sprites for now
(boxer-eval::defboxer-primitive bu::talk-to ((bu::port-to sprite))
  ;; first check for a "who" box
  (let ((who (boxer-eval::boxer-symeval 'bu::who)))
    (if (eq boxer-eval::*novalue* who)
        (boxer-eval::primitive-signal-error :sprite "needs a Box called WHO")
        (let ((raw-sprite (box-or-port-target sprite)))
          (cond ((sprite-box? raw-sprite)
                 (change who (make-vc (list (make-evrow-from-entry sprite)))))
                (t
                 ;; assume that it is a box full of ports to sprites
                 (change who (box-or-port-target sprite)))))))
  boxer-eval::*novalue*)

(defsprite-function bu::dot () (sprite turtle)
  (stamp-dot turtle)
  boxer-eval::*novalue*)

(defsprite-function bu::stamp-rect ((boxer-eval::numberize width)
                                    (boxer-eval::numberize height))
    (sprite turtle)
  (if (or (< width 0) (< height 0))
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The width and the height,"
                                          width "," height
                                          ", should both be 0 or greater")
      (turtle-rect turtle (fixr width) (fixr height)))
  boxer-eval::*novalue*)

(defsprite-function bu::stamp-rectangle ((boxer-eval::numberize width)
                                         (boxer-eval::numberize height))
    (sprite turtle)
  (if (or (< width 0) (< height 0))
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The width and the height,"
                                          width ","  height
                                          ", should both be 0 or greater")
      (turtle-rect turtle (fixr width) (fixr height)))
  boxer-eval::*novalue*)

(defsprite-function bu::stamp-hollow-rect ((boxer-eval::numberize width)
                                           (boxer-eval::numberize height))
    (sprite turtle)
  (if (or (< width 0) (< height 0))
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The width and the height,"
                                          width ","  height
                                          ", should both be 0 or greater")
      (hollow-turtle-rect turtle (fixr width) (fixr height)))
  boxer-eval::*novalue*)

(defsprite-function bu::stamp-hollow-rectangle ((boxer-eval::numberize width)
                                                (boxer-eval::numberize height))
    (sprite turtle)
  (if (or (< width 0) (< height 0))
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The width and the height,"
                                          width ","  height
                                          ", should both be 0 or greater")
      (hollow-turtle-rect turtle (fixr width) (fixr height)))
  boxer-eval::*novalue*)


(defsprite-function bu::stamp-ellipse ((boxer-eval::numberize width)
                                       (boxer-eval::numberize height))
    (sprite turtle)
  (if (or (< width 0) (< height 0))
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The width and the height,"
                                          width "," height
                                          ", should both be 0 or greater")
      (stamp-ellipse turtle (fixr width) (fixr height)))
  boxer-eval::*novalue*)

(defsprite-function bu::stamp-hollow-ellipse ((boxer-eval::numberize width)
                                              (boxer-eval::numberize height))
    (sprite turtle)
  (if (or (< width 0) (< height 0))
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The width and the height,"
                                          width "," height
                                          ", should both be 0 or greater")
      (stamp-hollow-ellipse turtle (fixr width) (fixr height)))
  boxer-eval::*novalue*)

(defsprite-function bu::stamp-circle ((boxer-eval::numberize radius))
    (sprite turtle)
  (if (< radius 0)
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The Radius, "
                                          radius
                                          "Should be 0 or greater")
      (stamp-circle turtle radius))
  boxer-eval::*novalue*)

(defsprite-function bu::stamp-hollow-circle ((boxer-eval::numberize radius))
    (sprite turtle)
  (if (< radius 0)
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The Radius, "
                                          radius
                                          "Should be a 0 or greater")
      (stamp-hollow-circle turtle radius))
  boxer-eval::*novalue*)

(defsprite-function bu::draw-wedge ((boxer-eval::numberize radius)
                                    (boxer-eval::numberize sweep-angle))
    (sprite turtle)
  (if (< radius 0)
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The Radius, "
                                          radius
                                          "Should be 0 or greater")
      (stamp-wedge turtle radius sweep-angle))
  boxer-eval::*novalue*)

;; better name courtesy of Andy
(defsprite-function bu::stamp-pie ((boxer-eval::numberize radius)
                                   (boxer-eval::numberize sweep-angle))
    (sprite turtle)
  (if (< radius 0)
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The Radius, "
                                          radius
                                          "Should be 0 or greater")
      (stamp-wedge turtle radius sweep-angle))
  boxer-eval::*novalue*)

(defsprite-function bu::draw-arc ((boxer-eval::numberize radius)
                                  (boxer-eval::numberize sweep-angle))
    (sprite turtle)
  (if (< radius 0)
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The Radius, "
                                          radius
                                          "Should be 0 or greater")
      (stamp-arc turtle radius sweep-angle))
  boxer-eval::*novalue*)

;; better name courtesy of Andy...
(defsprite-function bu::stamp-crust ((boxer-eval::numberize radius)
                                     (boxer-eval::numberize sweep-angle))
    (sprite turtle)
  (if (< radius 0)
      (boxer-eval::primitive-signal-error :sprite-error
                                          "The Radius, "
                                          radius
                                          "Should be 0 or greater")
      (stamp-arc turtle radius sweep-angle))
  boxer-eval::*novalue*)

(defsprite-function bu::stamp-bitmap ((bu::port-to graphics-box)) (sprite turtle)
  (let ((graphics-sheet (graphics-sheet (box-or-port-target graphics-box))))
    (cond ((null graphics-sheet)
           (boxer-eval::primitive-signal-error :sprite-error
                                               "The Box, " graphics-box
                                               ", does not have any graphics"))
          ((null (graphics-sheet-bit-array graphics-sheet))
           (boxer-eval::primitive-signal-error :sprite-error
                                               "The Box, " graphics-box
                                               ", does not have a bitmap"))
          (t
           (stamp-bitmap turtle
                         (graphics-sheet-bit-array graphics-sheet)
                         (graphics-sheet-draw-wid graphics-sheet)
                         (graphics-sheet-draw-hei graphics-sheet)))))
  boxer-eval::*novalue*)

(defsprite-function bu::stamp-self () (sprite turtle)
  (stamp turtle)
  boxer-eval::*novalue*)

;;; this loops through the graphics list and:
;;;   o copy the commands
;;;   o translates the commands to the current x,y or the sprite
;;;   o appends the new commands to the graphics-list
;;;   o draws the new commands in each visible screen-box
;;; similiar to dub-graphics-list except it can translate the list to be appended

(defun append-graphics-sheet-at (gs x y half-width half-height)
  (let ((gl (graphics-sheet-graphics-list gs))
        (ba (graphics-sheet-bit-array gs))
        (new-start-idx (storage-vector-active-length %graphics-list))
        (trans-x (- x half-width)) (trans-y (- y half-height)))
    (canonicalize-graphics-state %graphics-list)
                                        ;(reset-graphics-list-values %graphics-list)
    ;; handle bitmaps and backgrounds here....
    ;; note that the canonicalized state means OP=alu-seta and
    ;; pen-color=*foreground-color*
    (cond ((not (null ba))
           (record-boxer-graphics-command-centered-bitmap
            (copy-pixmap ba) x y
            (ogl-pixmap-width ba) (ogl-pixmap-height ba)))
          ((not (null (graphics-sheet-background gs)))
           ;; setup the color
           (record-boxer-graphics-command-change-graphics-color
            (graphics-sheet-background gs))
           (record-boxer-graphics-command-centered-rectangle
            x y (graphics-sheet-draw-wid gs) (graphics-sheet-draw-hei gs))
           ;; restore the canonical color
           (record-boxer-graphics-command-change-graphics-color
            *initial-graphics-state-current-pen-color*)))
    (do-vector-contents (gc gl)
      (let ((new-command (copy-graphics-command gc)))
        (sv-append %graphics-list (translate-graphics-command new-command x y))))
    ;; synch list values
    (setf (graphics-command-list-agent %graphics-list) nil
          (graphics-command-list-alu %graphics-list)
          (graphics-command-list-alu gl)
          (graphics-command-list-pen-width %graphics-list)
          (graphics-command-list-pen-width gl)
          (graphics-command-list-font-no %graphics-list)
          (graphics-command-list-font-no gl)
          (graphics-command-list-pen-color %graphics-list)
          (graphics-command-list-pen-color gl))
    ;; do we need to frob the global values ???
    (setq *graphics-state-current-alu*
          (graphics-command-list-alu %graphics-list)
          *graphics-state-current-pen-width*
          (graphics-command-list-pen-width %graphics-list)
          *graphics-state-current-font-no*
          (graphics-command-list-font-no %graphics-list)
          *graphics-state-current-pen-color*
          (graphics-command-list-pen-color %graphics-list))))

(defsprite-function bu::stamp ((boxer-eval::dont-copy graphicsbox))
    (sprite turtle)
  (let* ((graphics-box (box-or-port-target graphicsbox))
         (gs (get-graphics-sheet graphics-box)))
    (if (null gs)
        (boxer-eval::primitive-signal-error :sprite-error "No graphics in "
                                            (port-to-internal graphics-box))
        (append-graphics-sheet-at
          gs
          (x-position turtle) (y-position turtle)
          (floor (graphics-sheet-draw-wid gs) 2)
          (floor (graphics-sheet-draw-hei gs) 2))))
  boxer-eval::*novalue*)

(defvar *follow-mouse-movement-threshold* 2)

;;; add throttling, i.e. dont record movement if there hasn't been any (or enough)
(defsprite-function bu::follow-mouse () (sprite turtle)
  #+lispworks (let ((screen-box (or (car (fast-memq (bp-screen-box *mouse-bp*)
                                        ;; is *mouse-bp* valid ?
                                        (get-visible-screen-objs
                                         (slot-value turtle 'assoc-graphics-box))))
                        (car (displayed-screen-objs
                              ;; this is wrong but ignore ports for the moment
                              (slot-value turtle 'assoc-graphics-box))))))
    (multiple-value-bind (window-x-offset window-y-offset)
        (xy-position screen-box)
      (multiple-value-bind (left top right bottom)
          (box-borders-widths (box-type screen-box) screen-box)
        (let* ((min-x (truncate (+ window-x-offset left)))
               (min-y (truncate (+ window-y-offset top)))
               (superior-turtle (superior-turtle turtle))
               (sup-x (if (null superior-turtle) 0
                          (absolute-x-position superior-turtle)))
               (sup-y (if (null superior-turtle) 0
                          (absolute-y-position superior-turtle))))
          (flet ((translate-x (window-x)
                   (- (user-coordinate-x (- window-x min-x)) sup-x))
                 (translate-y (window-y)
                   (- (user-coordinate-y (- window-y min-y)) sup-y)))
            (multiple-value-bind (final-x final-y moved?)
                (let ((%mouse-usurped t))
                  (bw::with-mouse-tracking ((mouse-x min-x) (mouse-y min-y))
                    (let ((new-x (translate-x mouse-x)) (new-y (translate-y mouse-y))
                          (turtle-x (x-position turtle)) (turtle-y (y-position turtle)))
                      (when (or (> (abs (- new-x turtle-x))
                                   *follow-mouse-movement-threshold*)
                                (> (abs (- new-y turtle-y))
                                   *follow-mouse-movement-threshold*))
                        (move-to turtle new-x new-y)
                        (repaint)))))
              (when moved?
                (move-to turtle
                         (translate-x final-x) (translate-y final-y)))))))))
  boxer-eval::*novalue*)


(defsprite-function bu::type ((bu::dont-copy box)) (sprite turtle)
  (type-box turtle box)
  boxer-eval::*novalue*)

(defsprite-function bu::ctype ((bu::dont-copy box)) (sprite turtle)
  (type-box turtle box)
  boxer-eval::*novalue*)

(defsprite-function bu::ltype ((bu::dont-copy box)) (sprite turtle)
  (type-box turtle box :left)
  boxer-eval::*novalue*)

(defsprite-function bu::rtype ((bu::dont-copy box)) (sprite turtle)
  (type-box turtle box :right)
  boxer-eval::*novalue*)


#|



(defboxer-function bu:copy-self ()
(copy-box (sprite-box-near (box-being-told)) nil))

(defboxer-function bu:rotate (angle)
(tell-named-sprite :rotate (numberize angle))
':noprint)

(defboxer-function bu:single-touching-sprite ()
(let ((turtle (tell-named-sprite :sprite-under)))
(if (turtle? turtle)
(boxify (port-to-internal (tell turtle :sprite-box)))
(make-box nil))))

(defboxer-function bu:all-touching-sprites ()
(let ((turtles (tell-named-sprite :all-sprites-in-contact))
sprites)
(dolist (turtle turtles)
(setq sprites (cons (port-to-internal (tell turtle :sprite-box))
sprites)))
(make-box (list sprites))))

|#

;;; Color

(defun screen-color-from-turtle (turtle)
  "Returns an :rgb color from the graphics box wherever the turtle is."
  (let ((gb (assoc-graphics-box turtle)))
    (if (null gb)
        (boxer-eval::primitive-signal-error :graphics "Can't find a graphics box")
        (let ((sb (car (displayed-screen-objs gb))))
          (if (null sb)
              (boxer-eval::primitive-signal-error
               :graphics "Pixel at sprite location is not visible")
              (multiple-value-bind (box-x box-y)
                  (xy-position sb)
                (multiple-value-bind (lef top rig bot)
                    (box-borders-widths (box-type sb) sb)
                  (declare (ignore rig bot))
                  (with-graphics-vars-bound (gb)
                    (let ((gb-x (fix-array-coordinate-x (x-position turtle)))
                          (gb-y (fix-array-coordinate-y (y-position turtle))))
                      (if (and (<= 0 gb-x)
                               (<  gb-x (screen-obj-wid sb))
                               (<= 0 gb-y)
                               (<  gb-y (screen-obj-hei sb)))
                          (window-pixel-color (+ box-x lef gb-x)
                                        (+ box-y top gb-y))
                          (boxer-eval::primitive-signal-error
                           :graphics
                           "Pixel at sprite location is not visible")))))))))))

(defun background-color-from-turtle (turtle)
  "Returns an :rgb color from the graphics boxes background where the center of
   the turtle is."
  (let ((gb (assoc-graphics-box turtle)))
    (if (null gb)
        (boxer-eval::primitive-signal-error :graphics "Can't find a graphics box")
        (let* ((gs (graphics-sheet gb))
               (bit-array (graphics-sheet-bit-array gs))
               (background (graphics-sheet-background gs)))
          (with-graphics-vars-bound-internal gs
            (let ((gb-x (fix-array-coordinate-x (x-position turtle)))
                  (gb-y (fix-array-coordinate-y (y-position turtle))))
              (if (and (<=& 0 gb-x)
                       (<&  gb-x (graphics-sheet-draw-wid gs))
                       (<=& 0 gb-y)
                       (<&  gb-y (graphics-sheet-draw-hei gs)))
                  (cond ((not (null bit-array))
                         (pixmap-pixel-color bit-array gb-x gb-y))
                        ((not (null background)) background)
                        (t *background-color*))
                  (boxer-eval::primitive-signal-error
                   :graphics "Pixel is not in the graphics box"))))))))

(defsprite-function bu::color-under () (sprite turtle)
  (make-color-box-from-pixel (screen-color-from-turtle turtle)))

(defsprite-function bu::bg-color-under () (sprite turtle)
  (make-color-box-from-pixel (background-color-from-turtle turtle)))

(defsprite-function bu::bg-color-under? () (sprite turtle)
  (boxer-eval::boxer-boolean
   (color= (screen-color-from-turtle turtle)
           (background-color-from-turtle turtle))))

(defsprite-function bu::color-under= ((bu::port-to color))
    (sprite turtle)
  (let ((c (color-from-box (box-or-port-target color))))
    (if (null c)
        (boxer-eval::primitive-signal-error :graphics color "is not a color box")
        (boxer-eval::boxer-boolean
         (color= c (screen-color-from-turtle turtle))))))

(defsprite-function bu::bg-color-under= ((bu::port-to color))
    (sprite turtle)
  (let ((c (color-from-box (box-or-port-target color))))
    (if (null c)
        (boxer-eval::primitive-signal-error :graphics color "is not a color box")
        (boxer-eval::boxer-boolean
         (color= c (background-color-from-turtle turtle))))))


;;;; Sprite search order manipulation
(defsprite-function bu::push-to-back () (sprite turtle)
  (let ((gb (get-graphics-box-from-sprite-box sprite)))
    (when (box? gb)
      (let ((gs (graphics-sheet gb)))
        (when (fast-memq turtle (graphics-sheet-object-list gs))
          (setf (graphics-sheet-object-list gs)
                (append (fast-delq turtle (graphics-sheet-object-list gs))
                        (list turtle))))))
    boxer-eval::*novalue*))

(defsprite-function bu::bring-to-front () (sprite turtle)
  (let ((gb (get-graphics-box-from-sprite-box sprite)))
    (when (box? gb)
      (let ((gs (graphics-sheet gb)))
        (when (fast-memq turtle (graphics-sheet-object-list gs))
          (setf (graphics-sheet-object-list gs)
                (cons turtle
                      (fast-delq turtle (graphics-sheet-object-list gs)))))))
    boxer-eval::*novalue*))

