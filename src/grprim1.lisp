;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
#|


 $Header: grprim.lisp,v 1.0 90/01/24 22:12:52 boxer Exp $

 $Log:	grprim.lisp,v $
;;;Revision 1.0  90/01/24  22:12:52  boxer
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



   This file contains boxer functions which use the
   graphics subsystem via sprites


Modification History (most recent at top)

 6/15/10 *signal-error-for-sprite-pen-XOR* for PX, PR, PENXOR, PENREVERSE to signal
         an error
 1/01/10 clearscreen-internal, clean-internal changed to handle private graphics
10/06/07 Opengl changes to set-background
 7/16/04 Fixed Copyright error message in CLEAR-GRAPHICS, CLEARGRAPHICS, CG
 4/19/03 merged current LW and MCL sources
 9/03/02 UC free version of CLEAR-GRAPHICS, CLEARGRAPHICS & CG
 2/27/02 font-from-box allows uninstalled font families for lwwin because of
         the possiblility of being passed mac font names, need a better more
         general mechanism (eventually)
 2/26/02 font-from-box now allows a box for 1st item to handle fonts which have
         space characters in their names
 2/12/02 added bitmap dirty? handling to set-background
 3/26/00 font-from-box fixes for LW

 5/05/98 set-type-font now parses font specs
 5/05/98 started logging: source = Boxer version 2.3

|#

#-(or lispworks mcl lispm) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+(or lispworks mcl)       (in-package :boxer)




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
#|
;; we need this because extract-number-from-box ONLY works
;; on virtual copies (as it should)
(defun extract-item-from-editor-box (box)
  (let ((vcrs (car (slot-value box 'virtual-copy-rows))))
    (access-evrow-element nil (if (null vcrs)
				  (car (chunks (car (data-rows box))))
				  (car (evrow-pointers
					(car (vc-rows-entry-rows vcrs))))))))
|#

;;;; Graphics functions for Objects (especially turtles)

;; need to be careful so that clearscreen is only called once per
;; graphics box

(defun clean-internal (&optional (surface :foreground))
  (let ((gb (get-relevant-graphics-box)))
    (cond ((null gb)
	   (eval::primitive-signal-error :graphics "No Sprite or Graphics Box"))
	  ((eq gb :no-graphics)
	   ;; do nothing here since it is ok to give this command
	   ;; to a sprite even if it is not in a graphics box
	   )
          ((and (eq surface :foreground) (not (null %private-graphics-list)))
           (clear-graphics-list %private-graphics-list))
	  (t (clearscreen gb surface))))
  eval::*novalue*)

(defun clearscreen-internal (&optional (surface :foreground))
  (multiple-value-bind (gb sb)
      (get-relevant-graphics-box)
    (cond ((null gb)
	   (eval::primitive-signal-error :graphics
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
  ;; change to (force-graphics-output) here ?
  #+clx (bw::display-force-output bw::*display*)
  eval::*novalue*)

;;; Old versions....
#|

(defun clean-internal (&optional (surface :foreground))
  (let* ((sb (get-sprites))
	 (gb (if (null sb)
		 (get-graphics-box)
		 (let* ((turtle (slot-value sb 'associated-turtle))
			(gb (and turtle (assoc-graphics-box turtle))))
		   (cond ((not (null gb)) gb)
			 ((null turtle)
			  (error "The Sprite Box, ~A, has no turtle" sb))
			 ((null gb)
			  (eval::primitive-signal-error
			   :sprite-error "Sprite is not in a Graphics Box"))
			 (t (error
			     "Can't get a Graphics Box out of ~S" sb)))))))
    (if (null gb)
	(eval::primitive-signal-error :graphics "No Sprite or Graphics Box")
	(clearscreen gb surface)))
  eval::*novalue*)

(defun clearscreen-internal (&optional (surface :foreground))
  (let* ((active-sprites (get-sprites))
	 (turtle (when active-sprites (get-sprite-turtle active-sprites)))
	 (old-pen (when turtle (box-interface-value (slot-value turtle 'pen))))
	 (gb (if (null turtle)
		 (get-graphics-box)
		 (get-graphics-box-from-sprite-box active-sprites))))
    (cond ((and (null turtle) (null gb))
	   (eval::primitive-signal-error :graphics
					 "No Sprite or Graphics Box to Clear"))
	  (t
	   (unless (null turtle)
	     (setf (box-interface-value (slot-value turtle 'pen)) 'up)
	     (with-graphics-vars-bound (gb) (go-home turtle))
	     (setf (box-interface-value (slot-value turtle 'pen)) old-pen))
	   (unless (eq gb ':no-graphics) (clearscreen gb surface)))))
  #+clx (bw::display-force-output bw::*display*)
  eval::*novalue*)

|#

(defboxer-primitive bu::cs ()
  (clearscreen-internal)
  eval::*novalue*)

(defboxer-primitive bu::clearscreen ()
  (clearscreen-internal)
  eval::*novalue*)

(defboxer-primitive bu::clear-graphics ()
  (cond ((not (null *uc-copyright-free*))
         (eval::primitive-signal-error :copyright
                                       'bu::clear-graphics
                                       " is no longer available"))
        (t
         (clearscreen-internal))))

(defboxer-primitive bu::cleargraphics ()
  (cond ((not (null *uc-copyright-free*))
         (eval::primitive-signal-error :copyright
                                       'bu::cleargraphics
                                       " is no longer available"))
        (t
         (clearscreen-internal))))

(defboxer-primitive bu::cg ()
  (cond ((not (null *uc-copyright-free*))
         (eval::primitive-signal-error :copyright
                                       'bu::cg
                                       " is no longer available"))
        (t
         (clearscreen-internal))))

(defboxer-primitive bu::cb ()
  (clearscreen-internal :background))

(defboxer-primitive bu::clearbackground ()
  (clearscreen-internal :background))

(defboxer-primitive bu::clear-background ()
  (clearscreen-internal :background))

(defboxer-primitive bu::set-background (color)
  (let ((gb (get-relevant-graphics-box)))
    (cond ((or (null gb) (eq gb :no-graphics))
	   (eval::primitive-signal-error :graphics-error "No Sprite or Graphics Box"))
	  (t
	   (let ((pix (get-color-from-color-box color))
		 (gs (get-graphics-sheet gb)))
	     (cond ((null pix)
		    (eval::primitive-signal-error :graphics
						  "No color in: " color))
		   ((not (null (graphics-sheet-bit-array gs)))
		    ;; there is already a bitmapped background
		    ;; we'll draw the color on the background -- alternatively,
		    ;; we could do nothing, preferring to give bitmaps precedence
                    #-opengl
		    (drawing-on-bitmap ((graphics-sheet-bit-array gs))
		       (with-pen-color (pix)
			 (draw-rectangle alu-seta
					 (graphics-sheet-draw-wid gs)
					 (graphics-sheet-draw-hei gs) 0 0)))
                    #+opengl
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
    eval::*novalue*))

;; clean leave sprites where they are....

(defboxer-primitive bu::clean () (clean-internal))

(defboxer-primitive bu::clean-background () (clean-internal :background))

;;;; Arg checking...

;; domain check, they should already be numbers from the numerizing input
(defun steps-arg-check (arg)
  (unless (or (typep arg 'fixnum)
	      (< (- (max-window-coord) arg (max-window-coord))))
    (eval::primitive-signal-error :graphics-error
				  arg " is Too many turtle steps")))

;;;; Movement

(defsprite-function bu::fd ((eval::numberize steps)) (sprite turtle)
  (steps-arg-check steps)
  (with-sprites-hidden t
      (forward turtle steps))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::forward ((eval::numberize steps))
		    (sprite turtle)
  (steps-arg-check steps)
  (with-sprites-hidden t
      (forward turtle steps))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::bk ((eval::numberize steps))
		    (sprite turtle)
  (steps-arg-check steps)
  (with-sprites-hidden t
      (forward turtle (- steps)))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::back ((eval::numberize steps))
		    (sprite turtle)
  (steps-arg-check steps)
  (with-sprites-hidden t
      (forward turtle (- steps)))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::setxy ((eval::numberize x) (eval::numberize y))
		    (sprite turtle)
  (steps-arg-check x) (steps-arg-check y)
  (with-sprites-hidden t
      (move-to turtle x y))
  #+X (xlib::xflush)
  eval::*novalue*)

(defun get-position-values (box)
  (let ((n (flat-box-items box)))
    (cond ((and (numberp (car n)) (numberp (cadr n)))
	   (steps-arg-check (car n))
	   (steps-arg-check (cadr n))
	   (values (car n) (cadr n)))
	  (t (eval::primitive-signal-error
              :sprite-error box " should be a box with 2 numbers")))))

(defsprite-function bu::position ()
  (sprite turtle)
  (make-vc (list (make-evrow-from-entries (list (x-position turtle)
						(y-position turtle))))))

(defsprite-function bu::setpos ((eval::dont-copy position))
  (sprite turtle)
  (multiple-value-bind (new-x new-y)
      (get-position-values position)
    (steps-arg-check new-x)  (steps-arg-check new-y)
    (with-sprites-hidden t
      (move-to turtle new-x new-y))
    eval::*novalue*))

(defsprite-function bu::setposition (position)
  (sprite turtle)
  (multiple-value-bind (new-x new-y)
      (get-position-values position)
    (with-sprites-hidden t
      (move-to turtle new-x new-y))
    eval::*novalue*))

(defsprite-function bu::set-home-position ((eval::dont-copy position))
  (sprite turtle)
  (multiple-value-bind (new-x new-y)
      (get-position-values position)
    (set-home-position turtle new-x new-y))
  eval::*novalue*)

(defsprite-function bu::home ()
  (sprite turtle)
  (with-sprites-hidden t
    (go-home turtle))
  #+X (xlib::xflush)
  eval::*novalue*)

;;;; Turning

(defsprite-function bu::rt ((eval::numberize turns))
  (sprite turtle)
  (with-sprites-hidden nil
    (right turtle turns))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::right ((eval::numberize turns))
  (sprite turtle)
  (with-sprites-hidden nil
    (right turtle turns))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::lt ((eval::numberize turns))
  (sprite turtle)
  (with-sprites-hidden nil
    (right turtle (- turns)))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::left ((eval::numberize turns))
  (sprite turtle)
  (with-sprites-hidden nil
    (right turtle (- turns)))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::seth ((eval::numberize heading))
  (sprite turtle)
  (with-sprites-hidden nil
    (set-heading turtle heading))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::setheading ((eval::numberize heading))
  (sprite turtle)
  (with-sprites-hidden nil
    (set-heading turtle heading))
  #+X (xlib::xflush)
  eval::*novalue*)



;;;; Pens

(defsprite-function bu::pu ()
  (sprite turtle)
  (set-pen turtle 'bu::up)
  eval::*novalue*)

(defsprite-function bu::penup ()
  (sprite turtle)
  (set-pen turtle 'bu::up)
  eval::*novalue*)

(defsprite-function bu::pd ()
  (sprite turtle)
  (set-pen turtle 'bu::down)
  eval::*novalue*)

(defsprite-function bu::pendown ()
  (sprite turtle)
  (set-pen turtle 'bu::down)
  eval::*novalue*)

(defsprite-function bu::pe ()
  (sprite turtle)
  (set-pen turtle 'bu::erase)
  eval::*novalue*)

(defsprite-function bu::penerase ()
  (sprite turtle)
  (set-pen turtle 'bu::erase)
  eval::*novalue*)

(defvar *signal-error-for-sprite-pen-XOR* t)

(defsprite-function bu::px ()
  (sprite turtle)
  (if *signal-error-for-sprite-pen-XOR*
      (eval::primitive-signal-error :obsolete
                                    "XOR pens are no longer supported")
    (set-pen turtle 'bu::reverse))
  eval::*novalue*)

(defsprite-function bu::penxor ()
  (sprite turtle)
  (if *signal-error-for-sprite-pen-XOR*
      (eval::primitive-signal-error :obsolete
                                    "XOR pens are no longer supported")
    (set-pen turtle 'bu::reverse))
  eval::*novalue*)

(defsprite-function bu::penreverse ()
  (sprite turtle)
    (if *signal-error-for-sprite-pen-XOR*
      (eval::primitive-signal-error :obsolete
                                    "XOR pens are no longer supported")
      (set-pen turtle 'bu::reverse))
  eval::*novalue*)

(defsprite-function bu::pr ()
  (sprite turtle)
    (if *signal-error-for-sprite-pen-XOR*
      (eval::primitive-signal-error :obsolete
                                    "XOR pens are no longer supported")
      (set-pen turtle 'bu::reverse))
  eval::*novalue*)

(defsprite-function bu::set-pen-width ((eval::numberize width))
  (sprite turtle)
  (cond ((and (numberp width) (not (minusp width)))
	 (set-pen-width turtle width)
	 eval::*novalue*)
	(t (eval::primitive-signal-error :sprite-error
					 "The Pen Width Argument, " width
					 ", should be a positive number"))))

(defsprite-function bu::set-pen-color ((eval::dont-copy new-color))
  (sprite turtle)
  (let ((color (get-color-from-color-box new-color)))
    (if (null color)
	(eval::primitive-signal-error :graphics "No color in: " new-color)
	(set-pen-color turtle color)))
  eval::*novalue*)

;;; temporary hack until we have a better scheme for
;;; naming fonts

(defun font-from-box (box &optional no-errorp)
  (let* ((items (car (raw-unboxed-items box)))
         (old-style? (when (integerp (car items)) (car items)))
         ;; old-style? supports previous font-map implementation
         (font (cond ((and old-style? (< old-style? 4))
                      #+mcl "Courier" #-mcl *default-font-family*)
                     (old-style? #+mcl "Geneva" #-mcl *default-font-family*)
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
    (cond #-lwwin ;; can be passed other platform fonts, rethink this
          ((not (member font
                        #-mcl *font-families*
                        #+mcl ccl::*font-list*
                        :test #'string-equal))
           (unless no-errorp
             (eval::primitive-signal-error
              :sprite-font font " is not an installed Font")))
          ;; size check
          ((not (numberp size))
           (unless no-errorp
             (eval::primitive-signal-error
              :sprite-font "The 2nd item should be a font size")))
          ((and (not #+mcl (member size *supported-font-sizes*)
                     #-mcl (find size *font-sizes*))
                (not (= size 7))) ; for compatibility
           (unless no-errorp
             (eval::primitive-signal-error
              :sprite-font size " is an usupported font size")))
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
                                            (eval::primitive-signal-error
                                             :sprite-font
                                             s " is not a valid character style")))))
                                     (t (if no-errorp
                                          (throw 'bad-style nil)
                                          (eval::primitive-signal-error
                                           :sprite-font
                                           s " is not a valid character style")))))
                         styles))
             (make-boxer-font (list* font size styles)))))))

(defsprite-function bu::set-type-font ((eval::dont-copy fontspec))
  (sprite turtle)
  (set-type-font turtle (font-from-box fontspec))
  eval::*novalue*)

;;;; Hide and Show


(defsprite-function bu::ht ()
  (sprite turtle)
  (when (absolute-shown? turtle)
    (with-sprites-hidden nil (hide-turtle turtle)))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::hide ()
  (sprite turtle)
  (when (absolute-shown? turtle)
    (with-sprites-hidden nil (hide-turtle turtle)))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::hideturtle ()
  (sprite turtle)
  (when (absolute-shown? turtle)
    (with-sprites-hidden nil (hide-turtle turtle)))
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::st ()
  (sprite turtle)
  (show-turtle turtle)
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::show ()
  (sprite turtle)
  (show-turtle turtle)
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::showturtle ()
  (sprite turtle)
  (show-turtle turtle)
  #+X (xlib::xflush)
  eval::*novalue*)

;;; hide/show subsprites

(defsprite-function bu::show-subsprites ()
  (sprite turtle)
  (set-shown? turtle ':subsprites)
  #+X (xlib::xflush)
  eval::*novalue*)

(defsprite-function bu::hide-subsprites ()
  (sprite turtle)
  (set-shown? turtle ':no-subsprites)
  #+X (xlib::xflush)
  eval::*novalue*)

;;; other instance vars.

(defsprite-function bu::set-sprite-size ((eval::numberize size))
  (sprite turtle)
  (cond ((and (numberp size) (> size 0))
	 (with-sprites-hidden nil
	     (set-sprite-size turtle size))
	 eval::*novalue*)
	(t (eval::primitive-signal-error :sprite-error
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
	  (eval::primitive-signal-error :sprite-error "Can't find a shape in: "
					shape))))
  eval::*novalue*)

