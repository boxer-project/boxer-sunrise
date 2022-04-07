;;;;  ;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
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
;;;;  Modification History (most recent at top)
;;;;
;;;;   6/15/10 *signal-error-for-sprite-pen-XOR* for PX, PR, PENXOR, PENREVERSE to signal
;;;;           an error
;;;;   1/01/10 clearscreen-internal, clean-internal changed to handle private graphics
;;;;  10/06/07 Opengl changes to set-background
;;;;   7/16/04 Fixed Copyright error message in CLEAR-GRAPHICS, CLEARGRAPHICS, CG
;;;;   4/19/03 merged current LW and MCL sources
;;;;   9/03/02 UC free version of CLEAR-GRAPHICS, CLEARGRAPHICS & CG
;;;;   2/27/02 font-from-box allows uninstalled font families for lwwin because of
;;;;           the possiblility of being passed mac font names, need a better more
;;;;           general mechanism (eventually)
;;;;   2/26/02 font-from-box now allows a box for 1st item to handle fonts which have
;;;;           space characters in their names
;;;;   2/12/02 added bitmap dirty? handling to set-background
;;;;   3/26/00 font-from-box fixes for LW
;;;;
;;;;   5/05/98 set-type-font now parses font specs
;;;;   5/05/98 started logging: source = Boxer version 2.3
;;;;

(in-package :boxer)




;;; Graphics functions for graphics boxes


;;; ALL TURTLE functions are assumed to be called in an environment where the
;;; various turtle state variables as well as GRAPHICS vars (like BIT-ARRAY)
;;; are BOUND. This is what the macro WITH-GRAPHICS-VARS-BOUND is used for.
;;; Except in unusual cases, though, you should use the top level interface,
;;; WITH-SPRITE-PRIMITIVE-ENVIRONMENT or the convenience macro
;;; DEFSPRITE-FUNCTION.

;;; As far as I can tell, only wrap is implemented (EhL)

;(defboxer-function bu:wrap ()
;  (tell (graphics-box-near (box-being-told))
;	:set-draw-mode :wrap)
;  :noprint)

; fence should be fixed before this command is implemented.
;(defboxer-function bu:fence ()
;  (tell (graphics-box-near (box-being-told))
;	:set-draw-mode :fence)
;  :noprint)

;(defboxer-function bu:window ()
;  (tell (graphics-box-near (box-being-told))
;	:set-draw-mode :window)
;  :noprint)



;;;; Update Functions

;; we need this because extract-number-from-box ONLY works
;; on virtual copies (as it should).  Returns NIL if the box was empty.
(defun extract-item-from-editor-box (box)
  (let ((vcrs (car (slot-value box 'virtual-copy-rows))))
    (let ((item (if (null vcrs)
        (car (chunks (car (data-rows box))))
        (car (evrow-pointers (car (vc-rows-entry-rows vcrs)))))))
      (and item (access-evrow-element nil item)))))

;;;; Graphics functions for Objects (especially turtles)

;; need to be careful so that clearscreen is only called once per
;; graphics box

(defun clean-internal (&optional (surface :foreground))
  (let ((gb (get-relevant-graphics-box)))
    (cond ((null gb)
     (boxer-eval::primitive-signal-error :graphics "No Sprite or Graphics Box"))
    ((eq gb :no-graphics)
     ;; do nothing here since it is ok to give this command
     ;; to a sprite even if it is not in a graphics box
     )
          ((and (eq surface :foreground) (not (null %private-graphics-list)))
           (clear-graphics-list %private-graphics-list))
    (t (clearscreen gb surface))))
  boxer-eval::*novalue*)

(defun clearscreen-internal (&optional (surface :foreground))
  (multiple-value-bind (gb sb)
      (get-relevant-graphics-box)
    (cond ((null gb)
     (boxer-eval::primitive-signal-error :graphics
           "No Sprite or Graphics Box to Clear"))
          ((and (eq surface :foreground) (not (null %private-graphics-list)))
           (clear-graphics-list %private-graphics-list))
    (t
     (let ((turtle (get-sprite-turtle sb)))
       (unless (null turtle)
         (let ((old-pen (pen turtle)))
     (setf (box-interface-value (slot-value turtle 'pen)) 'up)
     (with-graphics-vars-bound (gb) (go-home turtle))
     (setf (box-interface-value (slot-value turtle 'pen)) old-pen))))
     (unless (eq gb :no-graphics) (clearscreen gb surface)))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::cs ()
  (clearscreen-internal)
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::clearscreen ()
  (clearscreen-internal)
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::cb ()
  (clearscreen-internal :background))

(boxer-eval::defboxer-primitive bu::clearbackground ()
  (clearscreen-internal :background))

(boxer-eval::defboxer-primitive bu::clear-background ()
  (clearscreen-internal :background))

(boxer-eval::defboxer-primitive bu::set-background (color)
  (let ((gb (get-relevant-graphics-box)))
    (cond ((or (null gb) (eq gb :no-graphics))
     (boxer-eval::primitive-signal-error :graphics-error "No Sprite or Graphics Box"))
    (t
     (let ((pix (get-color-from-color-box color))
     (gs (get-graphics-sheet gb)))
       (cond ((null pix)
        (boxer-eval::primitive-signal-error :graphics
              "No color in: " color))
       ((not (null (graphics-sheet-bit-array gs)))
        (opengl::clear-ogl-pixmap (graphics-sheet-bit-array gs)
                                  (opengl::color->pixel pix))
        ;; mark the dirty? flag
        (setf (graphics-sheet-bit-array-dirty? gs) t)
        (modified-graphics gb))
       (t
        ;; first, set the color
        (setf (graphics-sheet-background gs) pix)
        ;; then show it (if its on the screen)
        (when (box? gb) (clearscreen gb :none)))))))
    boxer-eval::*novalue*))

;; clean leave sprites where they are....

(boxer-eval::defboxer-primitive bu::clean () (clean-internal))

(boxer-eval::defboxer-primitive bu::clean-background () (clean-internal :background))

;;;; Arg checking...

;; domain check, they should already be numbers from the numerizing input
(defun steps-arg-check (arg)
  (unless (or (typep arg 'fixnum)
        (< (- (max-window-coord) arg (max-window-coord))))
    (boxer-eval::primitive-signal-error :graphics-error
          arg " is Too many turtle steps")))

;;;; Movement

(defsprite-function bu::fd ((boxer-eval::numberize steps)) (sprite turtle)
  (steps-arg-check steps)
  (with-sprites-hidden t
      (forward turtle steps))
  boxer-eval::*novalue*)

(defsprite-function bu::forward ((boxer-eval::numberize steps))
        (sprite turtle)
  (steps-arg-check steps)
  (with-sprites-hidden t
      (forward turtle steps))
  boxer-eval::*novalue*)

(defsprite-function bu::bk ((boxer-eval::numberize steps))
        (sprite turtle)
  (steps-arg-check steps)
  (with-sprites-hidden t
      (forward turtle (- steps)))
  boxer-eval::*novalue*)

(defsprite-function bu::back ((boxer-eval::numberize steps))
        (sprite turtle)
  (steps-arg-check steps)
  (with-sprites-hidden t
      (forward turtle (- steps)))
  boxer-eval::*novalue*)

(defsprite-function bu::setxy ((boxer-eval::numberize x) (boxer-eval::numberize y))
        (sprite turtle)
  (steps-arg-check x) (steps-arg-check y)
  (with-sprites-hidden t
      (move-to turtle x y))
  boxer-eval::*novalue*)

(defun get-position-values (box)
  (let ((n (flat-box-items box)))
    (cond ((and (numberp (car n)) (numberp (cadr n)))
     (steps-arg-check (car n))
     (steps-arg-check (cadr n))
     (values (car n) (cadr n)))
    (t (boxer-eval::primitive-signal-error
              :sprite-error box " should be a box with 2 numbers")))))

(defsprite-function bu::position ()
  (sprite turtle)
  (make-vc (list (make-evrow-from-entries (list (x-position turtle)
            (y-position turtle))))))


(defsprite-function bu::setpos ((boxer-eval::dont-copy position))
  (sprite turtle)
  (multiple-value-bind (new-x new-y)
      (get-position-values position)
    (steps-arg-check new-x)  (steps-arg-check new-y)
    (with-sprites-hidden t
      (move-to turtle new-x new-y))
    boxer-eval::*novalue*))

(defsprite-function bu::setposition (position)
  (sprite turtle)
  (multiple-value-bind (new-x new-y)
      (get-position-values position)
    (with-sprites-hidden t
      (move-to turtle new-x new-y))
    boxer-eval::*novalue*))

(defsprite-function bu::set-home-position ((boxer-eval::dont-copy position))
  (sprite turtle)
  (multiple-value-bind (new-x new-y)
      (get-position-values position)
    (set-home-position turtle new-x new-y))
  boxer-eval::*novalue*)

(defsprite-function bu::home ()
  (sprite turtle)
  (with-sprites-hidden t
    (go-home turtle))
  boxer-eval::*novalue*)

;;;; Turning

(defsprite-function bu::rt ((boxer-eval::numberize turns))
  (sprite turtle)
  (with-sprites-hidden nil
    (right turtle turns))
  boxer-eval::*novalue*)

(defsprite-function bu::right ((boxer-eval::numberize turns))
  (sprite turtle)
  (with-sprites-hidden nil
    (right turtle turns))
  boxer-eval::*novalue*)

(defsprite-function bu::lt ((boxer-eval::numberize turns))
  (sprite turtle)
  (with-sprites-hidden nil
    (right turtle (- turns)))
  boxer-eval::*novalue*)

(defsprite-function bu::left ((boxer-eval::numberize turns))
  (sprite turtle)
  (with-sprites-hidden nil
    (right turtle (- turns)))
  boxer-eval::*novalue*)

(defsprite-function bu::seth ((boxer-eval::numberize heading))
  (sprite turtle)
  (with-sprites-hidden nil
    (set-heading turtle heading))
  boxer-eval::*novalue*)

(defsprite-function bu::setheading ((boxer-eval::numberize heading))
  (sprite turtle)
  (with-sprites-hidden nil
    (set-heading turtle heading))
  boxer-eval::*novalue*)

;;;; Pens

(defsprite-function bu::pu ()
  (sprite turtle)
  (set-pen turtle 'bu::up)
  boxer-eval::*novalue*)

(defsprite-function bu::penup ()
  (sprite turtle)
  (set-pen turtle 'bu::up)
  boxer-eval::*novalue*)

(defsprite-function bu::pd ()
  (sprite turtle)
  (set-pen turtle 'bu::down)
  boxer-eval::*novalue*)

(defsprite-function bu::pendown ()
  (sprite turtle)
  (set-pen turtle 'bu::down)
  boxer-eval::*novalue*)

(defsprite-function bu::pe ()
  (sprite turtle)
  (set-pen turtle 'bu::erase)
  boxer-eval::*novalue*)

(defsprite-function bu::penerase ()
  (sprite turtle)
  (set-pen turtle 'bu::erase)
  boxer-eval::*novalue*)

(defsprite-function bu::set-pen-width ((boxer-eval::numberize width))
  (sprite turtle)
  (cond ((and (numberp width) (not (minusp width)))
   (set-pen-width turtle width)
   boxer-eval::*novalue*)
  (t (boxer-eval::primitive-signal-error :sprite-error
           "The Pen Width Argument, " width
           ", should be a positive number"))))


(defsprite-function bu::set-pen-color ((boxer-eval::dont-copy new-color))
  (sprite turtle)
  (let ((color (get-color-from-color-box new-color)))
    (if (null color)
  (boxer-eval::primitive-signal-error :graphics "No color in: " new-color)
  (set-pen-color turtle color)))
  boxer-eval::*novalue*)
;; )
;;; temporary hack until we have a better scheme for
;;; naming fonts

(defun font-from-box (box &optional no-errorp)
  (let* ((items (car (raw-unboxed-items box)))
         (old-style? (when (integerp (car items)) (car items)))
         ;; old-style? supports previous font-map implementation
         (font (cond ((and old-style? (< old-style? 4))
                           *default-font-family*)
                     (old-style? *default-font-family*)
                     ((or (virtual-copy? (car items)) (box? (car items)))
                      ;; allow the use of a box for fonts with spaces in the name
                      (box-text-string (car items)))
                     (t (string (car items)))))
         (size (cond ((and old-style? (< old-style? 4 )) 10)
                     (old-style? 7)
                     (t (cadr items))))
         (styles (if old-style?
                   (case (mod old-style? 4)
                     (0 nil)
                     (1 '(:bold))
                     (2 '(:italic))
                     (3 '(:bold :italic)))
                   (cddr items))))
    ;; reality checking
    (cond ;; can be passed other platform fonts, rethink this
          ((not (member font *font-families* :test #'string-equal))
           (unless no-errorp
             (boxer-eval::primitive-signal-error
              :sprite-font font " is not an installed Font")))
          ;; size check
          ((not (numberp size))
           (unless no-errorp
             (boxer-eval::primitive-signal-error
              :sprite-font "The 2nd item should be a font size")))
          ;; styles check
          (t
           (catch 'bad-style
             (setq styles
                   (mapcar #'(lambda (s)
                               (cond ((symbolp s)
                                      (let ((canonical-style
                                             (intern (symbol-name s)
                                                     (find-package "KEYWORD"))))
                                        (if (fast-memq canonical-style
                                                       '(:plain :bold :italic
                                                         :underline :outline
                                                         :shadow :condense
                                                         :extend))
                                          canonical-style
                                          (if no-errorp
                                            (throw 'bad-style nil)
                                            (boxer-eval::primitive-signal-error
                                             :sprite-font
                                             s " is not a valid character style")))))
                                     (t (if no-errorp
                                          (throw 'bad-style nil)
                                          (boxer-eval::primitive-signal-error
                                           :sprite-font
                                           s " is not a valid character style")))))
                         styles))
             (make-boxer-font (list* font size styles)))))))

(defsprite-function bu::set-type-font ((boxer-eval::dont-copy fontspec))
  (sprite turtle)
  (set-type-font turtle (font-from-box fontspec))
  boxer-eval::*novalue*)
;;;; Hide and Show


(defsprite-function bu::ht ()
  (sprite turtle)
  (when (absolute-shown? turtle)
    (with-sprites-hidden nil (hide-turtle turtle)))
  boxer-eval::*novalue*)

(defsprite-function bu::hide ()
  (sprite turtle)
  (when (absolute-shown? turtle)
    (with-sprites-hidden nil (hide-turtle turtle)))
  boxer-eval::*novalue*)

(defsprite-function bu::hideturtle ()
  (sprite turtle)
  (when (absolute-shown? turtle)
    (with-sprites-hidden nil (hide-turtle turtle)))
  boxer-eval::*novalue*)

(defsprite-function bu::st ()
  (sprite turtle)
  (show-turtle turtle)
  boxer-eval::*novalue*)

(defsprite-function bu::show ()
  (sprite turtle)
  (show-turtle turtle)
  boxer-eval::*novalue*)

(defsprite-function bu::showturtle ()
  (sprite turtle)
  (show-turtle turtle)
  boxer-eval::*novalue*)

;;; hide/show subsprites

(defsprite-function bu::show-subsprites ()
  (sprite turtle)
  (set-shown? turtle ':subsprites)
  boxer-eval::*novalue*)

(defsprite-function bu::hide-subsprites ()
  (sprite turtle)
  (set-shown? turtle ':no-subsprites)
  boxer-eval::*novalue*)

;;; other instance vars.

(defsprite-function bu::set-sprite-size ((boxer-eval::numberize size))
  (sprite turtle)
  (cond ((and (numberp size) (> size 0))
   (with-sprites-hidden nil
       (set-sprite-size turtle size))
   boxer-eval::*novalue*)
  (t (boxer-eval::primitive-signal-error :sprite-error
           "The Sprite Size Argument, " size
           ", should be a positive number"))))

(defsprite-function bu::setshape ((bu::port-to shape))
  (sprite turtle)
  (let ((shape-box (box-or-port-target shape)))
    (with-sprites-hidden :change-shape
      (if (or (sprite-box? shape-box)
        (graphics-box? shape-box)
        (and (virtual-copy? shape-box)
       (not (null (vc-graphics shape-box)))))
    (set-shape turtle shape-box)
    (boxer-eval::primitive-signal-error :sprite-error "Can't find a shape in: "
          shape))))
  boxer-eval::*novalue*)
