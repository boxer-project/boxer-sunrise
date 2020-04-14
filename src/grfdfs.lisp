;; -*- Mode:LISP; Syntax:Common-Lisp;Package:BOXER;-*-
#|


 $Header: grfdfs.lisp,v 1.0 90/01/24 22:12:30 boxer Exp $

 $Log:	grfdfs.lisp,v $
;;;Revision 1.0  90/01/24  22:12:30  boxer
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

    This contains all of the Interface between Graphics sheets
    and the rest of the  BOXER Editor.  The functions and methods
    which manipulate pixels (as opposed  to graphics objects) can
    also be found here  In particular, the functions which are
    used to draw lines, regions, etc are here.


Modification History (most recent at top)

 1/29/13 removed fixnum math:drawing-on-turtle-slate, draw-wrap-line, with-graphics-screen-parameters-once
 1/29/08 added check for %private-graphics-list to with-graphics-vars-bound-internal
 5/11/06 quick conversion of graphics-sheet to graphics-info will have to come back
         and do this right later, but this should allow us to bootstrap opengl port
10/29/03 removed *sprite-drawing-flush* and (flush-port-buffer)
 9/05/03 #+carbon-compat (flush-port-buffer) in with-sprite-primitive-environment
 2/15/03 merged current LW and MCL files
 2/15/01 merged current LW and MCL files
11/06/99 save/restore-under-turtle checks for and hacks clip mode
 6/29/99 save/restore-under-turtle now handles wrapping
 6/30/98 changed caching scheme for get-visible-screen-objs
 6/30/98 started logging changes: source = Boxer version 2.3

|#

(in-package :boxer)

;;; drawing defs

(defvar %bit-array 'i-bet-you-forgot-to-bind-%bit-array
  "The bit-array of the graphics-box being operated on")

(defvar %drawing-width 'i-bet-you-forgot-to-bind-%drawing-width
  "The width of the bit-array of the graphics box in which we are allowed to draw")

(defvar %drawing-height 'i-bet-you-forgot-to-bind-%drawing-heigth
  "The height of the bit-array of the graphics box in which we are allowed to draw")

(defvar %graphics-box 'i-bet-you-forgot-to-bind-%graphics-box
  "The graphics box which is being operated on.")

(defvar %graphics-sheet 'i-bet-you-forgot-to-bind-%graphics-sheet
  "The graphics box which is being operated on.")

(defvar %draw-mode 'i-bet-you-forgot-to-bind-%draw-mode
  "Draw-mode of the graphics box in which we are allowed to draw")

(defvar %graphics-list 'i-bet-you-forgot-to-bind-%graphics-list
  "graphics-list of the graphics box in which we are allowed to draw")

;; defined in vars.lisp
;; (defvar %learning-shape-graphics-list nil)

#| (defvar *hide-ALL-sprites-when-drawing* t) |#

(defvar %drawing-half-width 150.0)

(defvar %drawing-half-height 100.0)

;; This is bound by the erase/redraw pass in With-Sprite-Primitive-Environment
;; to the active sprite.  The possibility of the moving sprite being a
;; subsprite make the old method (checking for EQ inside of the erase all
;; other sprites loop) unworkable.  The eq check now has to be made inside
;; of the erase method (since it can be recursively invoked by superior
;; sprites)
;;

(defvar *currently-moving-sprite* nil)

;; this is set to Nil for the mac because of the bug which causes
;; allocation of new bitmaps to take a LONG time
(defvar *add-new-graphics-sheet-bit-array?* #+mcl nil #-mcl t
  "Automatically allocate new bit-arrays for graphics sheets when
   they need them e.g. the STAMP primitive")

(defmacro no-graphics? () '(eq %graphics-box :no-graphics))

;;; if drawing with more than one sprite becomes common, it may be worthwhile
;;; to optimize the macrolet'd with-sprites-hidden to only hide all the
;;; sprites once per graphics-box instead of once per turtle...

(defun graphics-sheet-from-box (box)
  (cond ((eq box ':no-graphics) nil)
	((virtual-copy? box) (graphics-info-graphics-sheet (vc-graphics box)))
	(t (graphics-info box))))

(defmacro with-graphics-vars-bound ((to-box &optional (gr-sheet (gensym)))
				    &body body)
  "This macro sets up an environment where commonly used
parameters of the graphics box are bound. "
  `(let ((,gr-sheet (graphics-sheet-from-box ,to-box))
	 ;; sometimes it is convenient to be able to access the box
	 (%graphics-box ,to-box))
     (with-graphics-vars-bound-internal ,gr-sheet . ,body)))

(defmacro with-graphics-vars-bound-internal (gr-sheet &body body)
  `(let ((%graphics-sheet ,gr-sheet)
	 (%bit-array nil)
	 (%drawing-width 0)
	 (%drawing-height 0)
	 ;; the command list
	 (%graphics-list %learning-shape-graphics-list)
	 ;; someday, maybe this will be useful
	 (%draw-mode nil))
     (declare (fixnum %drawing-width %drawing-height))
     ;; should these be floats ???
     (let ((%drawing-half-width 0.0)
	   (%drawing-half-height 0.0))
       ;; this macro is shadowed in with-sprite-primitive-environment
       ;; they are here for the benefit of graphics functions which
       ;; which do not use the sprite interface sprite
       (macrolet ((with-sprites-hidden (draw-p &body body)
                    ;; mostly a stub, we might want to use the command-can-draw?
                    ;; arg in the future as a hint to propagate MODIFIED info
                    `(progn
                       (when ,draw-p (queue-modified-graphics-box
                                      (graphics-sheet-superior-box ,',gr-sheet)))
                       (progn . ,body))))
;		    (declare (ignore draw-p))
;		    `(unwind-protect
;			  (progn
;			    (dolist (ttl (graphics-sheet-object-list
;					  ,',gr-sheet))
;			      (when (shown? ttl) (fast-erase ttl)))
;			    . ,body)
;		       ;; All the turtles' save-unders
;		       ;; have to updated since the moving turtle may
;		       ;; have drawn on the portion of the screen
;		       ;; where they are.
;		       (dolist (ttl (graphics-sheet-object-list ,',gr-sheet))
;			 (save-under-turtle ttl))
;		       (dolist (ttl (graphics-sheet-object-list ,',gr-sheet))
;			 (when (shown? ttl) (draw ttl))))))
	 (unless (null ,gr-sheet)
	   ;; set the variables if it is possible to do so.  We
	   ;; bind them and set them separately in order to centralize
	   ;; this check
	   (setq %bit-array (graphics-sheet-bit-array ,gr-sheet)
		 %drawing-width (graphics-sheet-draw-wid ,gr-sheet)
		 %drawing-height (graphics-sheet-draw-hei ,gr-sheet)
		 %graphics-list (or %learning-shape-graphics-list
                                    %private-graphics-list
				    (graphics-sheet-graphics-list ,gr-sheet)
				    (let ((new-gl(make-graphics-command-list)))
				      (setf (graphics-sheet-graphics-list
					     ,gr-sheet)
					    new-gl)
				      new-gl))
		 %draw-mode (graphics-sheet-draw-mode ,gr-sheet)
		 %drawing-half-width (/ %drawing-width 2.0)
		 %drawing-half-height (/ %drawing-height 2.0)))
	 ;; now do the rest
	 . ,body))))

;;; just in case...

(defmacro with-sprites-hidden (draw-p &body body)
  (declare (ignore draw-p body))
  (warn
   "WITH-SPRITES-HIDDEN being called outside of WITH-GRAPHICS-VARS-BOUND")
  `(error
    "WITH-SPRITES-HIDDEN being called outside of WITH-GRAPHICS-VARS-BOUND"))

(defmacro with-sprites-hidden-always (draw-p &body body)
  (declare (ignore draw-p body))
  (warn
   "WITH-ALL-SPRITES-HIDDEN being used outside of WITH-GRAPHICS-VARS-BOUND")
  `(error
    "WITH-ALL-SPRITES-HIDDEN being used outside of WITH-GRAPHICS-VARS-BOUND"))




;;; Set Up Clipping and Offsets

;;; this should use the pos-cache....
(defmacro with-turtle-slate-origins (screen-box &body body)
  ;; this macro sets x and y coordinates of top left of turtle array
  ;; not that the a SCREEN-SHEET may NOT have been allocated if this has
  ;; been called BEFORE Redisplay has had a chance to run
  `(let* ((screen-sheet (when ,screen-box (screen-sheet ,screen-box)))
	  (gs (graphics-screen-sheet-actual-obj screen-sheet)))
     (unless (null screen-sheet)
       (multiple-value-bind (box-x-offset box-y-offset)
	   (xy-position ,screen-box)
	 (multiple-value-bind (sheet-x sheet-y)
	     (graphics-screen-sheet-offsets screen-sheet)
	   (with-origin-at ((+& box-x-offset sheet-x)
			    (+& box-y-offset sheet-y))
	     . ,body))))))


(defun update-absolute-pos-cache (screen-box cache
					     box-x-offset box-y-offset
					     real-wid real-hei sheet-x sheet-y)
  (cond ((null cache)
	 (setf (cached-absolute-pos screen-box)
	       (setq cache (make-ab-pos-cache box-x-offset box-y-offset
					      real-wid     real-hei
					      sheet-x      sheet-y))))
	(t
	 (setf (ab-pos-cache-x cache) box-x-offset
	       (ab-pos-cache-y cache) box-y-offset
	       (ab-pos-cache-iw cache) real-wid
	       (ab-pos-cache-ih cache) real-hei
	       (ab-pos-cache-sx cache) sheet-x
	       (ab-pos-cache-sy cache) sheet-y
	       (ab-pos-cache-valid cache) t)
	 #+clx;; make sure the clip-cache is in sync with the new values
	 (unless (null (ab-pos-cache-clip-cache cache))
	   (bw::set-clip-state-values (ab-pos-cache-clip-cache cache)
				      (+& box-x-offset sheet-x)
				      (+& box-y-offset sheet-y)
				      real-wid real-hei))))
  ;; now keep track of the cache so it can be flushed at the proper time
  (unless (or (eq *absolute-position-caches-filled* ':toplevel)
	      (fast-memq cache *absolute-position-caches-filled*))
    (push cache *absolute-position-caches-filled*))
  ;; finally return the cache
  cache)

;; for use by handle-input ?
(defun invalidate-absolute-position-caches ()
  (if (eq *absolute-position-caches-filled* ':toplevel)
      (warn "Can't invalidate absolute position caches from Top Level")
      (dolist (cache *absolute-position-caches-filled*)
	(setf (ab-pos-cache-valid cache) nil))))

(defmacro drawing-on-turtle-slate (screen-box &body body)
  ;; first check the cache
  `(let ((pos-cache (cached-absolute-pos ,screen-box))
	 (screen-sheet (when ,screen-box (screen-sheet ,screen-box))))
     (cond ((null screen-sheet))
	   (t
	    (when (or (null pos-cache) (null (ab-pos-cache-valid pos-cache)))
	      ;; If the cache is not valid, fix it
	      (multiple-value-bind (box-x-offset box-y-offset)
		  (xy-position ,screen-box)
		(multiple-value-bind (inner-wid inner-hei)
		    (graphics-sheet-size (screen-obj-actual-obj ,screen-box))
		  (multiple-value-bind (sheet-x sheet-y)
		      (graphics-screen-sheet-offsets screen-sheet)
		    (multiple-value-bind (bl bt br bb)
			(box-borders-widths (box-type ,screen-box) ,screen-box)
		      (setq pos-cache
			    (update-absolute-pos-cache
			     ,screen-box pos-cache box-x-offset box-y-offset
			     (min inner-wid
				   (- (screen-obj-wid ,screen-box) bl br))
			     (min inner-hei
				   (- (screen-obj-hei ,screen-box) bt bb))
			     sheet-x sheet-y)))))))
	    ;; The position cache should now be valid....
	    (with-drawing-inside-region ((+ (ab-pos-cache-x pos-cache)
					     (ab-pos-cache-sx pos-cache))
					 (+ (ab-pos-cache-y pos-cache)
					     (ab-pos-cache-sy pos-cache))
					 (ab-pos-cache-iw pos-cache)
					 (ab-pos-cache-ih pos-cache))
	      (with-turtle-clipping ((ab-pos-cache-iw pos-cache)
				     (ab-pos-cache-ih pos-cache)
				     #+clx :cache #+clx pos-cache)
		(unwind-protect
		     (progn . ,body)
		  (when (eq *absolute-position-caches-filled* ':toplevel)
		    ;; if we are not inside the evaluator, then
		    ;; make sure we mark the cache as invalid after we are
		    ;; through using it
		    (setf (ab-pos-cache-valid pos-cache) nil)))))))))

;;; this has to be INSIDE of a with-graphics-vars-bound
;;; note that we can't combine the 2 macros into one because
;;; this macro has to be wrapped around BODY's which are
;;; STATEless because they may be executed multiple times
;;; For example, a sprite drawing one several screen-box's (as
;;; a result of port(s) to the original graphics box)

;;; The equivalent macro used to be imbedded directly into the
;;; various drawing primitives (draw-vector, draw-rect, etc) but
;;; it is out here now to allow the possibility of grouping
;;; several graphics calls within each turtle-slate binding

(defmacro with-graphics-screen-parameters (&body body)
  `(unless (no-graphics?)
     (dolist (screen-box (get-visible-screen-objs %graphics-box))
       (unless (or (eq ':shrunk (display-style screen-box))
                   (not (graphics-screen-box? screen-box)))
	 (drawing-on-turtle-slate screen-box ,@body)))))

;;; This is like the With-Graphics-Screen-Parameters Except that
;;; the body is only executed once (or not at all) on the "most
;;; acceptable screen-box". "Most appropriate" is defined as
;;; not clipped or if they are ALL clipped, the largest.

(defmacro with-graphics-screen-parameters-once (&body body)
  `(let ((best-screen-box nil))
     (unless (no-graphics?)
       (dolist (screen-box (get-visible-screen-objs %graphics-box))
	 (cond ((eq ':shrunk (display-style screen-box)))
               ((not (graphics-screen-box? screen-box)))
	       ((and (not (screen-obj-x-got-clipped? screen-box))
		     (not (screen-obj-y-got-clipped? screen-box)))
		(setq best-screen-box screen-box)
		(return))
	       ((null best-screen-box)
		(setq best-screen-box screen-box))
	       (t
		;; If we get to here, then both the best-screen-box
		;; and the current screen-box are clipped so pick the
		;; bigger one.  We could be a little more sophisticated
		;; about how we choose...
		(cond ((screen-obj-y-got-clipped? best-screen-box)
		       (cond ((null (screen-obj-y-got-clipped? screen-box))
			      (setq best-screen-box screen-box))
			     ((> (screen-obj-hei screen-box)
				  (screen-obj-hei best-screen-box))
			      (setq best-screen-box screen-box))))
		      ((screen-obj-y-got-clipped? screen-box)
		       ;;if the current box is clipped but the best one
		       ;; isn't, then leave things alone
		       )
		      ((screen-obj-x-got-clipped? best-screen-box)
		       (cond ((null (screen-obj-x-got-clipped? screen-box))
			      (setq best-screen-box screen-box))
			     ((> (screen-obj-wid screen-box)
				  (screen-obj-wid best-screen-box))
			      (setq best-screen-box screen-box))))))))
       (unless (null best-screen-box)
	 (drawing-on-turtle-slate best-screen-box ,@body)))))




(defvar *scrunch-factor* 1
  "the factor used to normalize the Y-coordinates so that squares really are")

(defvar *minimum-graphics-dimension* 15
  "The smallest you can make a graphics box")

(defun make-graphics-sheet (wid hei &optional box)
  (let ((new-gs (%make-graphics-sheet-with-graphics-list wid hei box)))
    (setf (graphics-sheet-graphics-list new-gs) (make-graphics-command-list))
    #+gl
    (setf (graphics-sheet-draw-mode new-gs) ':window)
    new-gs))

(defun make-graphics-sheet-with-graphics-list (wid
					       hei
					       &optional
					       box
					       (sheet
						(make-graphics-command-list)))
  (let ((new-gs (%make-graphics-sheet-with-graphics-list wid hei box)))
    (setf (graphics-sheet-graphics-list new-gs) sheet)
    #+gl
    (setf (graphics-sheet-draw-mode new-gs) ':window)
    new-gs))


(defun make-graphics-sheet-with-bitmap (wid hei &optional box)
  (%make-graphics-sheet-with-bitmap wid hei
     (make-offscreen-bitmap *boxer-pane* wid hei) box))


(defun make-graphics-screen-sheet (actual-obj
				   &optional (x-offset 0.) (y-offset 0.))
  (%make-g-screen-sheet actual-obj x-offset y-offset))

(defun graphics-screen-sheet-offsets (graphics-screen-sheet)
  (values (graphics-screen-sheet-x-offset graphics-screen-sheet)
	  (graphics-screen-sheet-y-offset graphics-screen-sheet)))

(defun set-graphics-screen-sheet-x-offset (graphics-screen-sheet new-x-offset)
  (setf (graphics-screen-sheet-x-offset graphics-screen-sheet) new-x-offset))

(defun set-graphics-screen-sheet-y-offset (graphics-screen-sheet new-y-offset)
  (setf (graphics-screen-sheet-y-offset graphics-screen-sheet) new-y-offset))

;;accessors for graphics boxes

(defmethod bit-array ((bg box))
  (let ((gs (slot-value bg 'graphics-info)))
    (and gs (graphics-sheet-bit-array gs))))

(defmethod graphics-sheet ((gb box))
  (slot-value gb 'graphics-info))

(defun drawing-width (graphics-sheet)
  ;; Returns the width of the area of a bit-array for a graphics
  ;; box.  Note that this doesn't have to be = to
  ;; ARRAY-DIMENSION-N because of BITBLT's multiple of 32.
  ;; requirement on the LispM
  (graphics-sheet-draw-wid graphics-sheet))

(defun drawing-height (graphics-sheet)
  (graphics-sheet-draw-hei graphics-sheet))

;; accessor methods for bit-arrays via graphics boxes
(defmethod bit-array-wid ((self box))
  (let ((gs (slot-value self 'graphics-info)))
    (and gs (graphics-sheet-draw-wid gs))))

(defmethod bit-array-hei ((self box))
  (let ((gs (slot-value self 'graphics-info)))
    (and gs (graphics-sheet-draw-hei gs))))

(defmethod graphics-sheet-size ((self box))
  (let ((graphics-sheet (slot-value self 'graphics-info)))
    (unless (null graphics-sheet)
      (values (graphics-sheet-draw-wid graphics-sheet)
	      (graphics-sheet-draw-hei graphics-sheet)))))

(defmethod draw-mode ((self box))
  (let ((gs (slot-value self 'graphics-info)))
    (and gs (graphics-sheet-draw-mode gs))))

(defmethod set-draw-mode ((self box) new-mode)
  (setf (graphics-sheet-draw-mode (slot-value self 'graphics-info)) new-mode))

(defmethod graphics-object-list ((self box))
  (let ((gs (slot-value self 'graphics-info)))
    (and gs (graphics-sheet-object-list gs))))

(defmethod graphics-graphics-list ((self box))
  (let ((gs (slot-value self 'graphics-info)))
    (when gs (graphics-sheet-graphics-list gs))))

;; returns a list of all the graphics views of the graphics-box

;;; desperately needs to cache but have to figure out a good time to flush
;;; the cache (modified won't work since graphics commands need to call
;;; modified on the graphics-box)
;(defun get-visible-screen-objs (graphics-box)
;  (or (getprop graphics-box 'cached-vis-objs)
;      (let ((sbs nil))
;	;; first check the Box proper
;	(dolist (sb (displayed-screen-objs graphics-box))
;	  (when (graphics-screen-box? sb)
;	    (push sb sbs)))
;	;; then check any ports to the box
;	(dolist (port (ports graphics-box))
;	  (dolist (sb (displayed-screen-objs port))
;	    (when (graphics-screen-box? sb)
;	      (push sb sbs))))
;	(putprop graphics-box sbs 'cached-vis-objs)
;	sbs)))

;; it will be OK to decache on modified now that I am going to implement
;; the queueing implementation for modified

;;
;; 6/30/98 decaching on modified loses when a screen box is obscured by
;; shrinking or graphics toggling a superior box.  No way to flush possible
;; visible objects without a map over all inferiors.
;; For now, modify the caching scheme to only persist for the suration of an
;; evaluation.  Even this will lose when people start programmatically shrinking
;; or toggling superiors, perhaps prims which affect the state of the screen
;; should also flush the vis-obj caches.
(defun decache-visible-screen-objs (box)
  (unless (null (getprop box 'cached-vis-objs))
    (putprop box nil 'cached-vis-objs)))

(defun get-visible-screen-objs (graphics-box)
  (unless (null graphics-box)
    (or ;(getprop graphics-box 'cached-vis-objs) ; turn caching OFF for now...
	(let ((sbs nil))
	  ;; first check the Box proper
	  (dolist (sb (displayed-screen-objs graphics-box))
	    (when (graphics-screen-box? sb)
	      (push sb sbs)))
	  ;; then check any ports to the box
	  (dolist (port (ports graphics-box))
	    (dolist (sb (displayed-screen-objs port))
	      (when (graphics-screen-box? sb)
		(push sb sbs))))
	  (putprop graphics-box sbs 'cached-vis-objs)
	  sbs))))

(defmethod clear-box ((self box) &key (bitmap-p t) (graphics-list-p t))
  (let ((graphics-sheet (slot-value self 'graphics-info)))
    (unless (null graphics-sheet)
      (let ((graphics-list (graphics-sheet-graphics-list graphics-sheet))
	    (bit-array (graphics-sheet-bit-array graphics-sheet))
	    (gswid (graphics-sheet-draw-wid graphics-sheet))
	    (gshei (graphics-sheet-draw-hei graphics-sheet))
	    (bg (graphics-sheet-background graphics-sheet)))
	;; first, clear the storage for each of the drawing surfaces
	(when (and graphics-list-p (not (null graphics-list)))
	  (clear-graphics-list graphics-list))
	(when bitmap-p
	  (cond ((not (null bit-array))
                 (clear-offscreen-bitmap bit-array (or bg *background-color*)))
		((color? bg)
		 (setf (graphics-sheet-background graphics-sheet) nil))
		;; tiling pattern code here
		)
          ;; mark the dirty? flag
          (setf (graphics-sheet-bit-array-dirty? graphics-sheet) nil))
	;; now erase stuff on the screen...
	(dolist (screen-box (get-visible-screen-objs  self))
	  (unless (eq ':shrunk (display-style screen-box))
	    (drawing-on-turtle-slate screen-box
               #-gl
	       (cond ((or (null bg) bitmap-p)
		      (erase-rectangle gswid gshei 0 0))
		     ((color? bg)
		      ;; looks like a color so draw a rectangle of that color
		      (with-pen-color (bg)
			(draw-rectangle alu-seta gswid gshei 0 0)))
		     ;; check for tiling pattern here
		     )
	       #+gl
	       (progn
		 (bw::pushattributes)
		 (bw::setpattern 0)
		 (if (numberp bg) (bw::color bg)(bw::color *background-color*))
		 (bw::rectfi
		  (- (round gswid 2)) (- (round gshei 2))
		  (round gswid 2) (- gshei 2))
		 (bw::popattributes))
	       ;; now, if only one of the drawing sufaces has been cleared,
	       ;; we need to regenerate the other surface
	       (when (and (not graphics-list-p) graphics-list)
		 ;; regenerate the graphics list
		 (playback-graphics-list-internal graphics-list))
	       (when (and (not bitmap-p) graphics-list-p bit-array)
		 ;; regenerate the background
		 (bitblt-to-screen alu-seta gswid gshei bit-array
				   0 0 0 0)))))))))


(defmethod clearscreen ((self box)
			&optional surface)
  (cond ((eq surface :background)
	 (clear-box self :bitmap-p t :graphics-list-p nil))
	((eq surface :foreground)
	 (clear-box self :bitmap-p nil :graphics-list-p t))
	((eq surface :none)
	 (clear-box self :bitmap-p nil :graphics-list-p nil))
	(t (clear-box self)))
;  (with-graphics-vars-bound (self sheet)
;    #-gl
;    (with-graphics-screen-parameters-once
;	(dolist (turtle (graphics-sheet-object-list sheet))
;	  ;; the save-under of the moving turtle has to be filled BEFORE ANY
;	  ;; turtles are drawn or else it might capture part of another
;	  ;; turtle's shape
;	  (save-under-turtle turtle)))
;    (with-graphics-screen-parameters
;	#+gl
;      (bw::clear-overlay-planes)
;      (dolist (turtle (graphics-sheet-object-list sheet))
;	(when (shown? turtle)
;	  (draw turtle)))))
  )



#+lispm
(compiler::make-obsolete make-graphics-box "Graphics Boxes have been flushed")




;;; Here is the line drawing stuff

;;;; This is the highest level drawing command.

(defun ck-mode-draw-line (from-x from-y to-x to-y alu)
 (if (eq %draw-mode ':wrap)
     (draw-wrap-line from-x from-y to-x to-y alu)
     (draw-clip-line from-x from-y to-x to-y alu)))

(defun draw-clip-line (from-x from-y to-x to-y alu)
  ;(draw-line from-x from-y to-x to-y alu t)
  ;; draw-line will clip the coordinates making the slope wrong...
  (%draw-line (scale-x from-x) (scale-y from-y) (scale-x to-x) (scale-y to-y)
	      alu t %drawing-window))

(defun draw-wrap-line (from-x from-y to-x to-y alu)
  (let* ((delta-x (- to-x from-x))
	 (delta-y (- to-y from-y))
	 (islope (unless (zerop delta-y) (/ delta-x delta-y)))
	 (slope (unless (zerop delta-x) (/ delta-y delta-x))))
    (flet ((wrap-y-coord-top (y) (+ y %drawing-height))
	   (wrap-y-coord-bottom (y) (- y %drawing-height))
	   (wrap-x-coord-left (x) (+ x %drawing-width))
	   (wrap-x-coord-right (x) (- x %drawing-width))
	   (beyond-top? (y) (minusp y))
	   (beyond-bottom? (y) (>= y %drawing-height))
	   (beyond-left? (x) (minusp x))
	   (beyond-right? (x) (>= x %drawing-width))
	   (top-x-intercept (x y) (unless (null islope)
				    (+ x (round (* islope (- y))))))
	   (bottom-x-intercept (x y) (unless (null islope)
				       (+ x
					   (round (* islope (- %drawing-height
								y 1))))))
	   (left-y-intercept (x y) (unless (null slope)
				     (+ y (round (* slope (- x))))))
	   (right-y-intercept (x y) (unless (null slope)
				      (+ y (round (* slope (- %drawing-width
								x 1)))))))
      (declare (inline wrap-y-coord-top wrap-y-coord-bottom
		       wrap-x-coord-right wrap-x-coord-left
		       beyond-top? beyond-bottom? beyond-right? beyond-left?))

      (flet ((line-right-then-continue (y-intercept)
	       (%draw-line (scale-x from-x) (scale-y from-y)
			   (scale-x (1- %drawing-width))
			   (scale-y y-intercept) alu t %drawing-array)
	       #+clx (bw::display-force-output bw::*display*)
	       ;; now recurse
	       (draw-wrap-line 0 y-intercept
			       (wrap-x-coord-right to-x) to-y alu))
	     (line-top-then-continue (x-intercept)
	       (%draw-line (scale-x from-x) (scale-y from-y)
			   (scale-x x-intercept) (scale-y 0)
			   alu t %drawing-array)
	       #+clx (bw::display-force-output bw::*display*)
	       (draw-wrap-line x-intercept (1- %drawing-height)
			       to-x (wrap-y-coord-top to-y) alu))
	     (line-left-then-continue (y-intercept)
	       (%draw-line (scale-x from-x) (scale-y from-y)
			   (scale-x 0) (scale-y y-intercept)
			   alu t %drawing-array)
	       #+clx (bw::display-force-output bw::*display*)
	       (draw-wrap-line (1- %drawing-width) y-intercept
			       (wrap-x-coord-left to-x) to-y alu))
	     (line-bottom-then-continue (x-intercept)
	       (%draw-line (scale-x from-x) (scale-y from-y)
			   (scale-x x-intercept) (scale-y (1- %drawing-height))
			   alu t %drawing-array)
	       #+clx (bw::display-force-output bw::*display*)
	       (draw-wrap-line x-intercept 0
			       to-x (wrap-y-coord-bottom to-y) alu))
	     (break-line-left (y-intercept)
	       (draw-wrap-line (wrap-x-coord-left from-x) from-y
			       (1- %drawing-width) y-intercept alu)
	       (draw-wrap-line 0 y-intercept to-x to-y alu))
	     (break-line-top (x-intercept)
	       (draw-wrap-line from-x (wrap-y-coord-top from-y)
			       x-intercept (1- %drawing-height) alu)
	       (draw-wrap-line x-intercept 0 to-x to-y alu))
	     (break-line-right (y-intercept)
	       (draw-wrap-line (wrap-x-coord-right from-x) from-y
			       0 y-intercept alu)
	       (draw-wrap-line (1- %drawing-width) y-intercept to-x to-y alu))
	     (break-line-bottom (x-intercept)
	       (draw-wrap-line from-x (wrap-y-coord-bottom from-y)
			       x-intercept 0 alu)
	       (draw-wrap-line x-intercept (1- %drawing-height)
			       to-x to-y alu)))
	(cond ((point-in-array? from-x from-y)
	       ;; check for the simple cases instead of falling
	       ;; to optimize the common case
	       (cond ((point-in-array? to-x to-y)
		      ;; the simple, simple case
		      (%draw-line (scale-x from-x) (scale-y from-y)
				  (scale-x to-x)   (scale-y to-y)
				  alu t %drawing-array))
		     ((beyond-right? to-x)
		      ;; note that if the line extends beyond a
		      ;; horizontal boundary, it can't be vertical
		      (let ((y-intercept (right-y-intercept from-x from-y)))
			(cond ((y-in-array? y-intercept)
			       ;; we are sure it intersects a vertical edge
			       (line-right-then-continue y-intercept))
			      ((beyond-top? to-y) ; y-intercept ?
			       ;; must intersect with the top edge instead
			       (line-top-then-continue
				(top-x-intercept from-x from-y)))
			      (t
			       ;; must intersect with the bottom edge
			       (line-bottom-then-continue
				(bottom-x-intercept from-x from-y))))))
		     ((beyond-left? to-x)
		      ;; if it's not inside, or beyond the right edge,
		      ;; it must be beyond the left edge
		      (let ((y-intercept (left-y-intercept from-x from-y)))
			(cond ((y-in-array? y-intercept)
			       ;; we are sure it intersects a vertical edge
			       (line-left-then-continue y-intercept))
			      ((beyond-top? to-y) ; y-intercept ?
			       ;; must intersect with the top edge instead
			       (line-top-then-continue
				(top-x-intercept from-x from-y)))
			      (t
			       ;; must intersect with the bottom edge
			       (line-bottom-then-continue
				(bottom-x-intercept from-x from-y))))))
		     ((beyond-top? to-y)
		      (line-top-then-continue (top-x-intercept from-x from-y)))
		     (t
		      (line-bottom-then-continue (bottom-x-intercept from-x
								     from-y)))))
	      ((beyond-right? from-x)
	       (let ((right-y-intercept (right-y-intercept to-x to-y)))
		 (cond ((and (not (beyond-right? to-x))
                             (y-in-array? right-y-intercept))
			;; break the line on the right edge
			(break-line-right right-y-intercept))
		       (t;; otherwise wrap, and try again...
			(draw-wrap-line (wrap-x-coord-right from-x) from-y
					(wrap-x-coord-right to-x)   to-y   alu)))))
	      ((beyond-left? from-x)
	       (let ((left-y-intercept (left-y-intercept to-x to-y)))
		 (cond ((and (not (beyond-left? to-x))
                             (y-in-array? left-y-intercept))
			;; break the line on the right edge
			(break-line-left left-y-intercept))
		       (t;; just wrap both coords and try again
			(draw-wrap-line (wrap-x-coord-left from-x) from-y
					(wrap-x-coord-left to-x)   to-y   alu)))))
	      ((beyond-top? from-y)
	       (let ((top-x-intercept (top-x-intercept to-x to-y)))
		 (cond ((and (not (beyond-top? to-y))
                             (x-in-array? top-x-intercept))
			(break-line-top top-x-intercept))
		       (t
			(draw-wrap-line from-x (wrap-y-coord-top from-y)
					to-x   (wrap-y-coord-top to-y)   alu)))))
	      (t;; from-y must be beyond the bottom line
	       (let ((bottom-x-intercept (bottom-x-intercept to-x to-y)))
		 (cond ((and (not (beyond-bottom? to-y))
                             (x-in-array? bottom-x-intercept))
			(break-line-bottom bottom-x-intercept))
		       (t
			(draw-wrap-line from-x (wrap-y-coord-bottom from-y)
					to-x   (wrap-y-coord-bottom to-y) alu))))))))))


#| old version

(defun draw-wrap-line (from-x from-y to-x to-y alu)
  (let* ((delta-x (-& to-x from-x))
	 (delta-y (-& to-y from-y))
	 (islope (unless (zerop& delta-y) (/ delta-x delta-y)))
	 (slope (unless (zerop& delta-x) (/ delta-y delta-x))))
    (flet ((wrap-y-coord-top (y) (+& y %drawing-height))
	   (wrap-y-coord-bottom (y) (-& y %drawing-height))
	   (wrap-x-coord-left (x) (+& x %drawing-width))
	   (wrap-x-coord-right (x) (-& x %drawing-width))
	   (beyond-top? (y) (minusp& y))
	   (beyond-bottom? (y) (>=& y %drawing-height))
	   (beyond-left? (x) (minusp& x))
	   (beyond-right? (x) (>=& x %drawing-width))
	   (top-x-intercept (x y) (unless (null islope)
				    (+& x (round (* islope (-& y))))))
	   (bottom-x-intercept (x y) (unless (null islope)
				       (+& x
					   (round (* islope (-& %drawing-height
								y 1))))))
	   (left-y-intercept (x y) (unless (null slope)
				     (+& y (round (* slope (-& x))))))
	   (right-y-intercept (x y) (unless (null slope)
				      (+& y (round (* slope (-& %drawing-width
								x 1)))))))
      (declare (inline wrap-y-coord-top wrap-y-coord-bottom
		       wrap-x-coord-right wrap-x-coord-left
		       beyond-top? beyond-bottom? beyond-right? beyond-left?))
      (drawing-body
        (flet ((line-right-then-continue (y-intercept)
	         (%draw-line (scale-x from-x) (scale-y from-y)
			     (scale-x (1-& %drawing-width))
			     (scale-y y-intercept) alu t %drawing-array)
	         #+clx (bw::display-force-output bw::*display*)
	         ;; now recurse
	         (draw-wrap-line 0 y-intercept
			         (wrap-x-coord-right to-x) to-y alu))
	       (line-top-then-continue (x-intercept)
	         (%draw-line (scale-x from-x) (scale-y from-y)
			     (scale-x x-intercept) (scale-y 0)
			     alu t %drawing-array)
	         #+clx (bw::display-force-output bw::*display*)
	         (draw-wrap-line x-intercept (1-& %drawing-height)
			         to-x (wrap-y-coord-top to-y) alu))
	       (line-left-then-continue (y-intercept)
	         (%draw-line (scale-x from-x) (scale-y from-y)
			     (scale-x 0) (scale-y y-intercept)
			     alu t %drawing-array)
	         #+clx (bw::display-force-output bw::*display*)
	         (draw-wrap-line (1-& %drawing-width) y-intercept
			         (wrap-x-coord-left to-x) to-y alu))
	       (line-bottom-then-continue (x-intercept)
	         (%draw-line (scale-x from-x) (scale-y from-y)
			     (scale-x x-intercept) (scale-y (1-& %drawing-height))
			     alu t %drawing-array)
	         #+clx (bw::display-force-output bw::*display*)
	         (draw-wrap-line x-intercept 0
			         to-x (wrap-y-coord-bottom to-y) alu))
	       (break-line-left (y-intercept)
	         (draw-wrap-line (wrap-x-coord-left from-x) from-y
			         (1- %drawing-width) y-intercept alu)
	         (draw-wrap-line 0 y-intercept to-x to-y alu))
	       (break-line-top (x-intercept)
	         (draw-wrap-line from-x (wrap-y-coord-top from-y)
			         x-intercept (1-& %drawing-height) alu)
	         (draw-wrap-line x-intercept 0 to-x to-y alu))
	       (break-line-right (y-intercept)
	         (draw-wrap-line (wrap-x-coord-right from-x) from-y
			         0 y-intercept alu)
	         (draw-wrap-line (1- %drawing-width) y-intercept to-x to-y alu))
	       (break-line-bottom (x-intercept)
	         (draw-wrap-line from-x (wrap-y-coord-bottom from-y)
			         x-intercept 0 alu)
	         (draw-wrap-line x-intercept (1-& %drawing-height)
			         to-x to-y alu)))
	  (cond ((point-in-array? from-x from-y)
	         ;; check for the simple cases instead of falling
	         ;; to optimize the common case
	         (cond ((point-in-array? to-x to-y)
		        ;; the simple, simple case
		        (%draw-line (scale-x from-x) (scale-y from-y)
				    (scale-x to-x)   (scale-y to-y)
				    alu t %drawing-array))
		       ((beyond-right? to-x)
		        ;; note that if the line extends beyond a
		        ;; horizontal boundary, it can't be vertical
		        (let ((y-intercept (right-y-intercept from-x from-y)))
			  (cond ((y-in-array? y-intercept)
			         ;; we are sure it intersects a vertical edge
			         (line-right-then-continue y-intercept))
			        ((beyond-top? to-y) ; y-intercept ?
			         ;; must intersect with the top edge instead
			         (line-top-then-continue
				  (top-x-intercept from-x from-y)))
			        (t
			         ;; must intersect with the bottom edge
			         (line-bottom-then-continue
				  (bottom-x-intercept from-x from-y))))))
		       ((beyond-left? to-x)
		        ;; if it's not inside, or beyond the right edge,
		        ;; it must be beyond the left edge
		        (let ((y-intercept (left-y-intercept from-x from-y)))
			  (cond ((y-in-array? y-intercept)
			         ;; we are sure it intersects a vertical edge
			         (line-left-then-continue y-intercept))
			        ((beyond-top? to-y) ; y-intercept ?
			         ;; must intersect with the top edge instead
			         (line-top-then-continue
				  (top-x-intercept from-x from-y)))
			        (t
			         ;; must intersect with the bottom edge
			         (line-bottom-then-continue
				  (bottom-x-intercept from-x from-y))))))
		       ((beyond-top? to-y)
		        (line-top-then-continue (top-x-intercept from-x from-y)))
		       (t
		        (line-bottom-then-continue (bottom-x-intercept from-x
								       from-y)))))
	        ((beyond-right? from-x)
	         (if (beyond-right? to-x)
		   ;; just wrap both coords and try again
		   (draw-wrap-line (wrap-x-coord-right from-x) from-y
				   (wrap-x-coord-right to-x)   to-y   alu)
		   (let ((y-intercept (right-y-intercept to-x to-y)))
		     (cond ((y-in-array? y-intercept)
			     ;; break the line on the right edge
			     (break-line-right y-intercept))
			    ((beyond-top? from-y)
			     (if (beyond-top? to-y)
				 (draw-wrap-line from-x(wrap-y-coord-top from-y)
						 to-x  (wrap-y-coord-top to-y)
						 alu)
				 ;; break the line on the top horizontal edge
				 (break-line-top (top-x-intercept to-x to-y))))
			    ((beyond-bottom? to-y)
			     ;; beyond bottom, but check the to-y
			     ;; in case we need to wrap both
			     (draw-wrap-line from-x (wrap-y-coord-bottom from-y)
					     to-x   (wrap-y-coord-bottom to-y)
					     alu))
			    (t
			     ;; break the line on the bottom horizontal edge
			     (break-line-bottom (bottom-x-intercept to-x
								    to-y)))))))
	      ((beyond-left? from-x)
	       ;; looks like from-x is beyond the left
	       (if (beyond-left? to-x)
		   ;; just wrap both coords and try again
		   (draw-wrap-line (wrap-x-coord-left from-x) from-y
				   (wrap-x-coord-left to-x)   to-y   alu)
		   (let ((y-intercept (left-y-intercept to-x to-y)))
		     (cond ((y-in-array? y-intercept)
                            ;; break the line on the right edge
                            (break-line-left y-intercept))
                           ((beyond-top? from-y)
                            (if (beyond-top? to-y)
                              (draw-wrap-line from-x(wrap-y-coord-top from-y)
                                              to-x  (wrap-y-coord-top to-y)
                                              alu)
                              ;; break the line on the top horizontal edge
                              (break-line-top (top-x-intercept to-x to-y))))
                           ((beyond-bottom? to-y)
                            ;; beyond bottom, but check the to-y
                            ;; in case we need to wrap both
                            (draw-wrap-line from-x (wrap-y-coord-bottom from-y)
                                            to-x   (wrap-y-coord-bottom to-y)
                                            alu))
                           (t
                            ;; break the line on the bottom horizontal edge
                            (break-line-bottom (bottom-x-intercept to-x
                                                                   to-y)))))))
	        ((beyond-top? from-y)
	         (if (beyond-top? to-y)
		   (draw-wrap-line from-x (wrap-y-coord-top from-y)
				   to-x   (wrap-y-coord-top to-y)   alu)
		   (break-line-top (top-x-intercept to-x to-y))))
	        ;; from-y must be beyond the bottom line
	        ((beyond-bottom? to-y)
	         (draw-wrap-line from-x (wrap-y-coord-bottom from-y)
			         to-x   (wrap-y-coord-bottom   to-y) alu))
	        (t
	         (break-line-bottom (bottom-x-intercept to-x to-y)))))))))
|#




;; switch this to use ONLY symbols in the BOXER package
(defvar *allowed-alus* '(up down erase xor :up :erase :down :xor))

(defun vlist-alu? (thing)
  ;; complain about bad alus but allow them for now
  (cond ((fast-memq thing '(:up :erase :down :xor))
	 (warn "~%The ALU, ~A, is not a valid alu, use symbols in the BOXER package" thing)
	 t)
	(t
	 (fast-memq thing *allowed-alus*))))




;; this tries to save/restore only the extents of the turtle rather than
;; the entire size of the allocated save-under which must take into account
;; possible rotations
;;
;; Also, clip to the containing graphics-box's dimensions

(defun save-under-turtle (turtle)
  (let ((save-under (turtle-save-under turtle)))
    (cond ((eq save-under 'xor-redraw))
	  (t
	   (when (null save-under)
	     (warn "The turtle, ~S, does not have a backing store" turtle)
	     (update-save-under turtle)
	     (setq save-under (turtle-save-under turtle)))
           (multiple-value-bind (minx miny maxx maxy)
               (enclosing-rectangle turtle)
             (let* ((gs (graphics-info (slot-value turtle 'assoc-graphics-box)))
                    (mode (graphics-sheet-draw-mode gs))
                    (sub (save-under-bitmap save-under)))
               (flet ((save-corner-wrap (lefx rigx topy boty)
                        (bitblt-from-screen alu-seta lefx topy sub 0 0 0 0)
                        (bitblt-from-screen alu-seta (-& %drawing-width rigx) topy
                                            sub rigx 0 lefx 0)
                        (bitblt-from-screen alu-seta lefx (-& %drawing-height boty)
                                            sub 0 boty 0 topy)
                        (bitblt-from-screen alu-seta (-& %drawing-width rigx)
                                            (-& %drawing-height boty) sub
                                            rigx boty lefx topy))
                      (save-horiz-wrap (lefx rigx)
                        (let* ((cminy (max& miny 0))
                               (cmaxy (min& maxy %drawing-height))
                               (hei (-& cmaxy cminy)))
                          (bitblt-from-screen alu-seta lefx hei sub 0 cminy 0 0)
                          (bitblt-from-screen alu-seta (-& %drawing-width rigx) hei
                                              sub rigx cminy lefx 0)))
                      (save-vert-wrap (topy boty)
                        (let* ((cminx (max& minx 0))
                               ;; clipped vars...
                               (cmaxx (min& maxx %drawing-width))
                               (wid (-& cmaxx cminx)))
                          (bitblt-from-screen alu-seta wid topy sub cminx 0 0 0)
                          (bitblt-from-screen alu-seta wid (-& %drawing-height boty)
                                              sub cminx boty 0 topy))))
                 ;; should we check for :wrap mode ?
                 ;; what if the user moves to the edge and then changes modes ?
                 ;; for now, always save info to cover this case
                 ;;
                 ;; There are potentially 8 wrapping cases, we search for wrapping
                 ;; cases as follows
                 ;; top-left, top-right, top
                 ;; bottom-left, bottom-right, bottom
                 ;; left, then right
                 ;; also remember that some sprites can be larger than
                 ;; the graphics box itself
                 (cond ((eq mode :clip)
                        (unless (or (<=& maxx 0) (>=& minx %drawing-width)
                                    (<=& maxy 0) (>=& miny %drawing-height))
                          ;; unless completely off the graphics box
                          ;; just draw what's visible
                          (let* ((cminx (max& minx 0)) (cminy (max& miny 0))
                                 (cmaxx (min& maxx %drawing-width))
                                 (cmaxy (min& maxy %drawing-height))
                                 (cwid (-& cmaxx cminx)) (chei (-& cmaxy cminy)))
                            (unless (or (zerop& cwid) (zerop& chei))
                              (bitblt-from-screen alu-seta cwid chei sub
                                                  cminx cminy 0 0)))))
                       ((minusp& miny)
                        ;; top wrap, bind useful vars and check for corners
                        (let ((topy (min& maxy %drawing-height))
                              (boty (max& 0 (+& miny %drawing-height))))
                          (cond ((minusp& minx)
                                 ;; top left corner
                                 (save-corner-wrap (min& maxx %drawing-width)
                                                   (max& 0 (+& minx %drawing-width))
                                                   topy boty))
                                ((>& maxx %drawing-width)
                                 ;; top right corner
                                 (save-corner-wrap (min& (-& maxx %drawing-width)
                                                         %drawing-width)
                                                   (min& minx %drawing-width)
                                                   topy boty))
                                (t ;; must be just the top
                                 (save-vert-wrap topy boty)))))
                       ((>& maxy %drawing-height)
                        ;; bottom wrap, bind useful vars and check for corners
                        (let ((topy (min& (-& maxy %drawing-height)
                                          %drawing-height))
                              (boty (max& miny 0)))
                          (cond ((minusp& minx)
                                 ;; bottom left corner
                                 (save-corner-wrap (min& maxx %drawing-width)
                                                   (max& 0 (+& minx %drawing-width))
                                                   topy boty))
                                ((>& maxx %drawing-width)
                                 ;; bottom right corner
                                 (save-corner-wrap (min& (-& maxx %drawing-width)
                                                         %drawing-width)
                                                   (min& minx %drawing-width)
                                                   topy boty))
                                (t ;; must be just the bottom
                                 (save-vert-wrap topy boty)))))
                       ((minusp& minx)
                        ;; horizontal left wrap case
                        (save-horiz-wrap (min& maxx %drawing-width)
                                         (max& 0 (+& minx %drawing-width))))
                       ((>& maxx %drawing-width)
                        ;; horizontal right wrap case
                        (save-horiz-wrap (min& (-& maxx %drawing-width)
                                               %drawing-width)
                                         (max& minx 0)))
                       (t ;; vanilla case
                        (bitblt-from-screen alu-seta
                                            (-& (min& maxx %drawing-width)
                                                (max& minx 0))
                                            (-& (min& maxy %drawing-height)
                                                (max& miny 0))
                                            sub
                                            (max& minx 0) (max& miny 0)
                                            0 0))))))))))

(defun restore-under-turtle (turtle)
  (let ((save-under (turtle-save-under turtle)))
    (cond ((eq save-under 'xor-redraw))
	  (t
	   (when (null save-under)
	     (cerror "Make a Save Under"
		     "The turtle, ~S, does not have a backing store" turtle)
	     (update-save-under turtle))
           (multiple-value-bind (minx miny maxx maxy)
               (enclosing-rectangle turtle)
             (let* ((gs (graphics-info (slot-value turtle 'assoc-graphics-box)))
                    (mode (graphics-sheet-draw-mode gs))
                    (sub (save-under-bitmap save-under)))
               (flet ((restore-corner-wrap (lefx rigx topy boty)
                        (bitblt-to-screen alu-seta lefx topy sub 0 0 0 0)
                        (bitblt-to-screen alu-seta (-& %drawing-width rigx) topy
                                            sub lefx 0 rigx 0)
                        (bitblt-to-screen alu-seta lefx (-& %drawing-height boty)
                                            sub 0 topy 0 boty)
                        (bitblt-to-screen alu-seta (-& %drawing-width rigx)
                                            (-& %drawing-height boty) sub
                                            lefx topy rigx boty))
                      (restore-horiz-wrap (lefx rigx)
                        (let* ((cminy (max& miny 0))
                               (cmaxy (min& maxy %drawing-height))
                               (hei (-& cmaxy cminy)))
                          (bitblt-to-screen alu-seta lefx hei sub 0 0 0 cminy)
                          (bitblt-to-screen alu-seta (-& %drawing-width rigx) hei
                                              sub lefx 0 rigx cminy)))
                      (restore-vert-wrap (topy boty)
                        (let* ((cminx (max& minx 0))
                               ;; clipped vars...
                               (cmaxx (min& maxx %drawing-width))
                               (wid (-& cmaxx cminx)))
                          (bitblt-to-screen alu-seta wid topy sub 0 0 cminx 0)
                          (bitblt-to-screen alu-seta wid (-& %drawing-height boty)
                                              sub 0 topy cminx boty))))
                 ;; should we check for :wrap mode ?
                 ;; what if the user moves to the edge and then changes modes ?
                 ;; for now, always save info to cover this case
                 ;;
                 ;; There are potentially 8 wrapping cases, we search for wrapping
                 ;; cases as follows
                 ;; top-left, top-right, top
                 ;; bottom-left, bottom-right, bottom
                 ;; left, then right
                 ;; also remember that some sprites can be larger than
                 ;; the graphics box itself
                 (cond ((eq mode :clip)
                        (unless (or (<=& maxx 0) (>=& minx %drawing-width)
                                    (<=& maxy 0) (>=& miny %drawing-height))
                          ;; unless completely off the graphics box
                          ;; just draw what's visible
                          (let* ((cminx (max& minx 0)) (cminy (max& miny 0))
                                 (cmaxx (min& maxx %drawing-width))
                                 (cmaxy (min& maxy %drawing-height))
                                 (cwid (-& cmaxx cminx)) (chei (-& cmaxy cminy)))
                            (unless (or (zerop& cwid) (zerop& chei))
                              (bitblt-to-screen alu-seta cwid chei sub
                                                0 0 cminx cminy)))))
                       ((minusp& miny)
                        ;; top wrap, bind useful vars and check for corners
                        (let ((topy (min& maxy %drawing-height))
                              (boty (max& 0 (+& miny %drawing-height))))
                          (cond ((minusp& minx)
                                 ;; top left corner
                                 (restore-corner-wrap (min& maxx %drawing-width)
                                                   (max& 0 (+& minx %drawing-width))
                                                   topy boty))
                                ((>& maxx %drawing-width)
                                 ;; top right corner
                                 (restore-corner-wrap (min& (-& maxx %drawing-width)
                                                         %drawing-width)
                                                   (min& minx %drawing-width)
                                                   topy boty))
                                (t ;; must be just the top
                                 (restore-vert-wrap topy boty)))))
                       ((>& maxy %drawing-height)
                        ;; bottom wrap, bind useful vars and check for corners
                        (let ((topy (min& (-& maxy %drawing-height)
                                          %drawing-height))
                              (boty (max& miny 0)))
                          (cond ((minusp& minx)
                                 ;; bottom left corner
                                 (restore-corner-wrap (min& maxx %drawing-width)
                                                   (max& 0 (+& minx %drawing-width))
                                                   topy boty))
                                ((>& maxx %drawing-width)
                                 ;; bottom right corner
                                 (restore-corner-wrap (min& (-& maxx %drawing-width)
                                                         %drawing-width)
                                                   (min& minx %drawing-width)
                                                   topy boty))
                                (t ;; must be just the bottom
                                 (restore-vert-wrap topy boty)))))
                       ((minusp& minx)
                        ;; horizontal left wrap case
                        (restore-horiz-wrap (min& maxx %drawing-width)
                                         (max& 0 (+& minx %drawing-width))))
                       ((>& maxx %drawing-width)
                        ;; horizontal right wrap case
                        (restore-horiz-wrap (min& (-& maxx %drawing-width)
                                               %drawing-width)
                                         (max& minx 0)))
                       (t ;; vanilla case
                        (bitblt-to-screen alu-seta
                                            (-& (min& maxx %drawing-width)
                                                (max& minx 0))
                                            (-& (min& maxy %drawing-height)
                                                (max& miny 0))
                                            sub
                                            0 0 (max& minx 0) (max& miny 0)))))))))))




;;;; Sprite Access

;;; this is the sprite scoping function.  Basically, we walk up the static
;;; hierarchy looking for either a sprite box or a sprite-cache (see how
;;; TALK-TO works to understand sprite caches)

;; this is dependent on the evaluator internals
(defsubst static-root ()
  boxer-eval::*lexical-variables-root*)

;; defined in vars.lisp
;; (defvar *current-sprite* nil)

;;; No more WHO mechanism.
(defun get-sprites ()
  (or *current-sprite*
      (do ((box (static-root) (when (box? box) (superior-box box))))
	  ((or (null box) (sprite-box? box) (and (virtual-copy? box)
						 (eq (vc-type box)
						     'sprite-box)))
	   box)
	(let ((as (get-active-sprite box)))
	  (unless (null as) (return as))))))

(defun get-graphics-box ()
  (do ((box (static-root) (when (box? box) (superior-box box))))
      ((or (null box) (graphics-box? box)
	   (and (virtual-copy? box) (getf (vc-graphics box) 'graphics-sheet)))
       box)
    ))


;;; The active sprite list lives on the Plist of the Box (for now)
;(defun cache-active-sprites (box sprites)
;  ;; arg check should happen higher up at the Boxer interface level
;  (putprop box sprites 'active-sprites))

;;; should cache the sprite info in the WHO box to avoid having to
;;; walk throught the the box everytime.  Need to figure out a
;;; fast way to flush the cache (that doesn't slow down ALL boxes)

#| no longer used since WHO mechanism got flushed

(defun extract-sprites (box)
  (let ((sprites nil))
    (multiple-value-bind (rows ignore1 ignore2 vcrs)
	(if (virtual-copy? box) (vc-rows box) (virtual-copy-rows box))
      (declare (ignore ignore1 ignore2))
      (dolist (evrow rows)
	(dolist (ptr (evrow-pointers evrow))
	  (let ((value (access-evrow-element (or vcrs box) ptr)))
	    (cond ((virtual-port? value)
		   (push (vp-target value) sprites))
		  ((port-box? value)
		   (push (ports value) sprites))
		  (t (error "~A was not a port" value)))))))
    sprites))
|#

;; stubware(tm)
;; supposed to look for active sprite INSIDE of vc
(defun get-vc-active-sprite (vc)
  (declare (ignore vc))
  nil)

(defun get-active-sprite (box)
  (if (virtual-copy? box)
      (get-vc-active-sprite box)
      (getprop box 'active-sprite)))


;; this understands virtual copies of sprite boxes
(defun get-sprite-turtle (sb)
  (cond ((sprite-box? sb) (slot-value sb 'graphics-info))
	((and (virtual-copy? sb)) ; (eq (vc-type sb) 'sprite-box))
	 (graphics-info-turtle (vc-graphics sb)))))

(defun get-graphics-box-from-sprite-box (sb)
  (let* ((turtle (get-sprite-turtle sb))
	 (gb (and turtle (assoc-graphics-box turtle))))
    (cond ((not (null gb)) gb)
	  ((null turtle)
	   (error "The Sprite Box, ~A, has no turtle" sb))
	  ((null gb) ':no-graphics)
	  (t (error "Can't get a Graphics Box out of ~S" sb)))))

;;; this is used by prims that implicitly apply to either sprites or
;;; graphics boxes such as CS or GRAPHICS-MODE
(defun get-relevant-graphics-box ()
  (if (not (null *current-sprite*))
      (get-graphics-box-from-sprite-box *current-sprite*)
      (do ((box (static-root) (when (box? box) (superior-box box))))
	  ((null box) nil)
	(cond ((or (sprite-box? box) (and (virtual-copy? box) (eq (vc-type box) 'sprite-box)))
	       (return (values (get-graphics-box-from-sprite-box box) box)))
	      ((or (graphics-box? box) (and (virtual-copy? box)
					    (getf (vc-graphics box) 'graphics-sheet)))
	       (return box))
	      (t (let ((as (get-active-sprite box)))
		   (unless (null as)
		     (return (values (get-graphics-box-from-sprite-box as) as)))))))))


;;; ALL TURTLE functions are assumed to be called in an environment where the
;;; various turtle state variables as well as GRAPHICS vars (like BIT-ARRAY)
;;; are BOUND. This is what the macro WITH-GRAPHICS-VARS-BOUND is used for.
;;; The top level interface is DEFSPRITE-FUNCTION which dispatches the sprite
;;; function to the appropriate sprite(s).  The body of the function can assume
;;; that there is a sprite-box bound to sprite-var

(eval-when (compile load eval)
(defmacro defsprite-function (name-descriptor arglist (sprite-var turtle-var)
			      &body body)
  `(boxer-eval::defboxer-primitive ,name-descriptor ,arglist
     (with-sprite-primitive-environment (,sprite-var ,turtle-var)
	. ,body)))
)

(defmacro defsprite-trigger-function (name-descriptor arglist
						      (sprite-var turtle-var)
						      &body body)
  `(boxer-eval::defboxer-primitive ,name-descriptor ,arglist
     (with-sprite-primitive-environment (,sprite-var ,turtle-var t)
	. ,body)))

;; used to avoid redundant erases and draws of subsprites
;; which happen to be the current active sprite
(defvar *current-active-sprite* nil)

(defvar *sprites-hidden* nil)

(defvar *prepared-graphics-box* nil)

;;; Cocoa drawing: sprites dont draw onto the screen, they just update the graphics
;;; list and periodically, the GB box space, on the screen is cleared, the
;;; graphics list is blasted out and then the sprites are redrawn (any existing
;;; background can also be draw first)
(defmacro with-sprite-primitive-environment ((sprite-var turtle-var
							 &optional
							 no-sprite-error
							 (gboxvar (gensym)))
					     &body body)
  `(let ((active-sprites (get-sprites)))
     (cond ((null active-sprites)
	    ,(if (null no-sprite-error)
		 '(boxer-eval::signal-error :sprite-error "Don't have a Sprite to Talk to")
		 'boxer-eval::*novalue*))
	   (t
	    (let* ((,sprite-var active-sprites)
		   (,turtle-var (get-sprite-turtle ,sprite-var))
		   (,gboxvar (get-graphics-box-from-sprite-box ,sprite-var)))
	      ;; get rid of bound but never used warnings
	      ,sprite-var ,turtle-var
	      (with-graphics-vars-bound (,gboxvar sheet)
		(macrolet
		    ((WITH-SPRITES-HIDDEN (command-can-draw? &body body)
                       ;; mostly a stub, we might want to use the command-can-draw?
                       ;; arg in the future as a hint to propagate MODIFIED info
		       `(let* ((draw-p (and ,command-can-draw?
                                            (not (eq (pen ,',turtle-var) 'bu::up)))))
                          (when draw-p (queue-modified-graphics-box ,',gboxvar))
                          (progn . ,body))))
		  (prog1 (with-graphics-state (%graphics-list)
			     (update-graphics-state ,turtle-var)
			   ,@body)))))))))


#|
(defmacro defsprite-update-function (name-descriptor arglist
				     (sprite-var turtle-var slot-name)
				     &body body)
  `(progn
     (when (and (symbolp ',name-descriptor)
		(not (fast-memq ',name-descriptor *sprite-update-functions*)))
       (push ',name-descriptor *sprite-update-functions*))
     (setf (get ',slot-name 'update-function-name) ',name-descriptor)
     (defsprite-function ,name-descriptor ,arglist (,sprite-var ,turtle-var)
       . ,body)))
|#



;;; This is used by sprite update functions when they are passed an illegal arg
;;; In general, we assume some value, tell the user and then bash whatever
;;; losing thing he may have put into the box
;;; In the technical vastness of the future, we may want this to do something
;;; that is more Boxer Like

(defmacro sprite-update-warning (format-string &rest format-args)
;;  `(warn ,format-string . ,format-args)
  ;;be more user viewable...
  `(boxer-editor-warning ,format-string . ,format-args))

;;;; Some useful variables

;;; When a turtle is created, all of the boxes corresponding
;;; to instance variables have also been created.  What needs
;;; to be done here is to put the boxes inthe right place
;;; At this time, that means the x-position, y-position and
;;; heading boxes are in the box proper and all the other slots
;;; live in the closet of the sprite-box.

(defvar *initially-visible-sprite-instance-slots*
	'(x-position y-position heading))

(defvar *turtle-slots-that-need-boxes*
	'(x-position y-position heading
		     shown? pen
		     home-position sprite-size shape))

(defvar *name-flashing-pause-time* 2.
  "Time in Seconds to Pause when flashing the name of a Sprite")


(defvar %mouse-usurped nil "Used in move-to to prevent changing boxes")
(defvar %new-shape nil "The new shape vectors are collected here when doing a set-shape")

;; defined in vars.lisp
;; (defvar %learning-shape? nil "This is t when doing a set-shape")
;; (defvar %turtle-state nil
;;         "where to save a turtle's position, pen, and heading. ")

