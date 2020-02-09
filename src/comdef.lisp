;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER ; Base:8.  -*-

#|

 $Header: comdef.lisp,v 1.0 90/01/24 22:08:18 boxer Exp $

 $Log:	comdef.lisp,v $
;;;Revision 1.0  90/01/24  22:08:18  boxer
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


 This file contains Macros and Variable Declarations for BOXER Editor Commands


Modification History (most recent at top)

12/09/13 *popup-doc-font* changed to %make-font-no-internal 0 0, size arg is idx not actual size
11/30/12 track-mouse-area now deals with non fixnum inputs
         animate-cursor-move, animate-scrolling: no more fixnum arithmetic
12/ 6/10 changed *mouse-doc-highlight-color* to a dark green
 9/20/10 *mouse-doc-highlight-color*
 2/22/07 #+opengl track-mouse-area: erases & draws, no more blting
 2/06/07 changed #+ mcl's and lwwins to apple and win32 - in this file they refer
         to physical devices, also "apple" seems to be the only feature that digitool
         and lispworks share for referring to the mac.
 7/25/05 final debug of new box properties stuff
 7/12/05 finished 1st cut of new box properties stuff
 7/01/05 started new box properties stuff (comdef.lisp)
10/25/03 removed force-graphics-output from track-mouse-area, the call to
         force-graphics-putput now occurs at a lower level in with-mouse-tracking
10/15/03 changed #+  for flushing graphics to generic force-graphics-output
         in track-mouse-area
         changed display-force-output to force-graphics-output in
         animate-{scrolling,cursor-move}
 9/02/03 #+carbon-compat (flush-port-buffer) added to track-mouse-area
 2/15/03 merged current LW and MCL files
10/08/02 code review: define-input-devices predates initial Lispworks port
         so no need to excise it for "clean" reimplementation
11/02/01 lwwin mouse names added to defbasic-mode region-mode
10/15/01 *popup-doc-on?* added
 4/14/01 *popup-doc-color* changed to be redisplay-init for #+lwwin
 4/10/01 added popup doc vars & defs
 3/01/01 add calls to enter to move-to-bp so binding root gets set
 2/12/01 merged with current MCL file
 4/07/00 added clear-editor-message
 3/03/00 added lwwin changes to mouse modes (retarget, region, suitcase)
 2/14/99 started logging changes for Harlequin Lispworks version

|#


#-(or mcl lispm lispworks) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+(or mcl lispworks)       (in-package :boxer)


(DEFVAR *BOXER-EDITOR-COMMANDS* NIL
  "A list of all the commands used in the editor. ")

(DEFUN INITIALIZE-EDITOR ()
  (SETQ *COLUMN* 0)
  (RESET-EDITOR-NUMERIC-ARG)
  (UNLESS (NULL (OR *REGION-BEING-DEFINED* (GET-CURRENT-REGION)))
    (FLUSH-REGION (OR *REGION-BEING-DEFINED* (GET-CURRENT-REGION)))))



;;;; Utilities for Numeric args


(DEFMACRO WITH-MULTIPLE-EXECUTION (&BODY BODY)
  ;; this is for turning single execution coms into ones that will take numeric
  ;; arguments. An  *EDITOR-NUMERIC-ARGUMENT* of NIL implies an arg of 1
  ;; Reset it to NIL before executing the body, since the body may wish
  ;; to call itself recursively or may call another editor function.
  `(dotimes (i (prog1 (or *editor-numeric-argument* 1)
		 (reset-editor-numeric-arg)))
	 . ,body))

(DEFUN RESET-EDITOR-NUMERIC-ARG ()
  (LET ((OLD-ARG *EDITOR-NUMERIC-ARGUMENT*))
    (SETQ *EDITOR-NUMERIC-ARGUMENT* NIL)
    (UNLESS (NULL OLD-ARG)
      (REDRAW-STATUS-LINE))))

(defun reset-region (&optional (reset-mouse-cursor? T))
  (unless (null (or *region-being-defined* (get-current-region)))
    (flush-region (or *region-being-defined* (get-current-region)))
    (exiting-region-mode reset-mouse-cursor?)))

(DEFUN SET-EDITOR-NUMERIC-ARG (NEW-ARG)
  (SETQ *EDITOR-NUMERIC-ARGUMENT* NEW-ARG)
  (REDRAW-STATUS-LINE))

(DEFUN BOXER-KEY-NAME? (NAME)
  (OR (SEARCH "-KEY" (STRING NAME))
      (SEARCH "MOUSE-" (STRING NAME))))

(DEFUN BOXER-EDITOR-COMMAND? (COM)
  (MEMBER COM *BOXER-EDITOR-COMMANDS*))

(DEFUN BOXER-COMMAND-DEFINE (COM-NAME DOC-STRING)
  (UNLESS (BOXER-EDITOR-COMMAND? COM-NAME)
    (PUSH COM-NAME *BOXER-EDITOR-COMMANDS*))
  (IF (STRINGP DOC-STRING)
      (SETF (GET COM-NAME 'EDITOR-DOCUMENTATION) DOC-STRING)
      (BARF "Boxer Editor Commands Require a Documentation String. ")))

(DEFMACRO DEFBOXER-COMMAND (COM-NAME ARGS DOC-STRING . BODY)
  `(PROGN #+LISPM 'COMPILE
     (BOXER-COMMAND-DEFINE ',COM-NAME ',DOC-STRING)
     (DEFUN ,COM-NAME ,ARGS
	 ,DOC-STRING
	 (progn (increment-key-tick)
		. ,BODY))))

;;; Editor no nos
;;; beeps for now but should be more informative in the future
;;; in the future, should do something with a string arg

;;; Use BOXER-EDITOR-ERROR for unanticipated problems with allowed usage
;;; for example, a string search that fails
(defun boxer-editor-error (string &rest args)
  (status-line-display 'boxer-editor-error (apply #'format nil string args))
  (beep))

(defun boxer-editor-warning (string &rest args)
  (status-line-display 'boxer-editor-error (apply #'format nil string args))
  (beep))

;; the same as warning except no beep
(defun boxer-editor-message (string &rest args)
  (status-line-display 'boxer-editor-error (apply #'format nil string args)))

(defun clear-editor-message () (status-line-undisplay 'boxer-editor-error))

(DEFMACRO EDITOR-BARF (STRING . ARGS)
  `(ERROR ,STRING . ,ARGS))



;;;; Useful information about where you are

(DEFUN BOX-POINT-IS-IN()		  ;returns the box the bp part of
  (BP-BOX *POINT*))			  ;*point* refers to

(DEFUN SCREEN-BOX-POINT-IS-IN ()	  ;returns the screen box the *point* is in
  (POINT-SCREEN-BOX))

(DEFUN BOX-SCREEN-POINT-IS-IN ()	  ;returns the box that the screen part of
  (SCREEN-OBJ-ACTUAL-OBJ (POINT-SCREEN-BOX)))	;*point* refers to


(DEFUN BOX-POINT-IS-NEAR ()
  (LET* ((ROW (BP-ROW *POINT*))
	 (CHA-NO (BP-CHA-NO *POINT*))
	 (CHA-BEFORE-BP (CHA-AT-CHA-NO ROW (- CHA-NO 1)))
	 (CHA-AFTER-BP  (CHA-AT-CHA-NO ROW CHA-NO)))
    (COND ((BOX? CHA-AFTER-BP) (VALUES CHA-AFTER-BP :AFTER))
	  ((BOX? CHA-BEFORE-BP) (VALUES CHA-BEFORE-BP :BEFORE))
	  (T NIL))))

(defun point-next-box ()
  (let ((start-row (point-row)) (start-cha-no (point-cha-no)))
    (do ((row start-row (next-row row)))
	((null row) nil)
      (let ((box (do-row-chas ((cha row :start
				    (if (eq row start-row) start-cha-no 0)))
		   (when (box? cha) (return cha)))))
	(unless (null box)
	  (return (values box (allocate-screen-obj-for-use-in
			       box (screen-box-point-is-in)))))))))

(defun point-previous-box ()
  (let ((start-row (point-row)) (start-cha-no (point-cha-no)))
    (do ((row start-row (previous-row row)))
	((null row) nil)
      (let ((box (do* ((chas-array (chas-array row))
		       (chas (chas-array-chas chas-array))
		       (length (chas-array-active-length chas-array))
		       (i (if (eq row start-row) (1-& start-cha-no)(1-& length))
			  (1-& i)))
		      ((minusp& i) nil)
		   (let ((cha (svref& chas i)))
		     (when (box? cha) (return cha))))))
	(unless (null box)
	  (return (values box (allocate-screen-obj-for-use-in
			       box (screen-box-point-is-in)))))))))



(DEFUN SCREEN-BOX-POINT-IS-NEAR ()
  (LET ((BPIN (BOX-POINT-IS-NEAR)))
    (UNLESS (NULL BPIN)
      (ALLOCATE-SCREEN-OBJ-FOR-USE-IN BPIN (SCREEN-BOX-POINT-IS-IN)))))



;;;; Macros iterating over characters in a row

(DEFMACRO MAP-OVER-CHAS ((START-BP DIRECTION) &BODY BODY)
  `(DO* ((ROW (BP-ROW ,START-BP) ROW)
	 (NEXT-OR-PREVIOUS-ROW (IF (PLUSP ,DIRECTION)
				   (WHEN (NOT-NULL ROW)(NEXT-ROW ROW))
				   (WHEN (NOT-NULL ROW)(PREVIOUS-ROW ROW)))
			       (IF (PLUSP ,DIRECTION)
				   (WHEN (NOT-NULL ROW)(NEXT-ROW ROW))
				   (WHEN (NOT-NULL ROW)(PREVIOUS-ROW ROW))))
	 (CHA-NO (BP-CHA-NO ,START-BP) (+ CHA-NO ,DIRECTION))
	 (CHA (CHA-AT-CHA-NO ROW (IF (PLUSP ,DIRECTION) CHA-NO (- CHA-NO 1)))
	      (CHA-AT-CHA-NO ROW (IF (PLUSP ,DIRECTION) CHA-NO (- CHA-NO 1)))))
	(NIL)
     (COND ((AND (NULL CHA) (NOT-NULL NEXT-OR-PREVIOUS-ROW))
	    (SETQ ROW NEXT-OR-PREVIOUS-ROW
		  CHA-NO (IF (PLUSP DIRECTION) 0
			     (LENGTH-IN-CHAS NEXT-OR-PREVIOUS-ROW))))
	   (T
	    . ,BODY))))

#+LISPM
(COMPILER:MAKE-OBSOLETE MAP-OVER-CHAS "Use MAP-OVER-CHAS-IN-LINE Instead. ")

(DEFMACRO MAP-OVER-CHAS-IN-LINE ((START-BP DIRECTION) &BODY BODY)
  `(DO* ((ROW (BP-ROW ,START-BP) ROW)
	 (NEXT-OR-PREVIOUS-ROW (IF (PLUSP ,DIRECTION)
				   (WHEN (NOT-NULL ROW)(NEXT-ROW ROW))
				   (WHEN (NOT-NULL ROW)(PREVIOUS-ROW ROW)))
			       (IF (PLUSP ,DIRECTION)
				   (WHEN (NOT-NULL ROW)(NEXT-ROW ROW))
				   (WHEN (NOT-NULL ROW)(PREVIOUS-ROW ROW))))
	 (CHA-NO (BP-CHA-NO ,START-BP) (+ CHA-NO ,DIRECTION))
	 (CHA (CHA-AT-CHA-NO ROW (IF (PLUSP ,DIRECTION) CHA-NO
					   (- CHA-NO 1)))
	      (CHA-AT-CHA-NO ROW (IF (PLUSP ,DIRECTION) CHA-NO
					   (- CHA-NO 1)))))
	(NIL)
     (COND ((AND (NULL NOT-FIRST-CHA?)
		 (NULL CHA)
		 (NOT-NULL NEXT-OR-PREVIOUS-ROW))
	    (SETQ ROW NEXT-OR-PREVIOUS-ROW
		  CHA-NO (IF (PLUSP DIRECTION) -1
					;gets incremented in loop above
					;immediately
			     (1+ (LENGTH-IN-CHAS NEXT-OR-PREVIOUS-ROW)))))
	   (T . ,BODY))))



;;; For Killing stuff

;for control-y
(DEFMACRO KILL-BUFFER-TOP ()
  '(CAR *KILL-BUFFER*))

;;;; Variables...

;;; Used by the Kill stuff
(defvar *kill-buffer-last-direction* nil)

(defvar *kill-buffer-length* 8)

(defvar *kill-buffer* (make-list *kill-buffer-length*))

(defvar *number-of-non-kill-commands-executed* 0)

;;; Used by search
(DEFVAR *CASE-AFFECTS-STRING-SEARCH* NIL)

;;; Documantations VArs

#|
(DEFVAR *TOP-LEVEL-HELP-BOX*
	(MAKE-BOX '(("Type one of the following:")
		    ("A (Display commands with a given string)")
		    ("C (Document a Particular Command)")
		    (""))))

(DEFVAR *COMMAND-DOCUMENTATION-HELP-BOX*
	(MAKE-BOX '(("Type a key to be documented: ")
		    ("")
		    (""))))

(DEFVAR *APROPOS-DOCUMENTATION-HELP-BOX*
	(MAKE-BOX `(("APROPOS (Substring): ")
		    ("")
		    (""))))

|#



;;; Generalized movement

;; this ought to stack CONs the BP
(defmacro with-temporary-bp ((bp-var bp-values-form) . body)
  (let ((row (gensym)) (cha-no (gensym)) (screen-box (gensym)))
    `(let ((,bp-var (make-bp ':fixed)))
       #+lucid (declare (lcl::dynamic-extent ,bp-var))
       (multiple-value-bind (,row ,cha-no ,screen-box)
	   ,bp-values-form
	 (progn
	   ;; use the setf versions for now.  If it turns out that
	   ;; there is a reason to use the SET-BP-xxx functions, then
	   ;; this need to turn into an unwind-protect which deletes the
	   ;; temporary BP from the row and screen-box
	   (setf (bp-row ,bp-var) ,row)
	   (setf (bp-cha-no ,bp-var) ,cha-no)
	   (setf (bp-screen-box ,bp-var) ,screen-box)
	   . ,body)))))

;;; Move the POINT to another (possibly non-local) location specified by BP
;;; This function performs all of the neccessary zooms, expands and scrolls
;;; so that the user has some idea of where he is going

(defvar *move-bp-zoom-pause-time* 0.05)

(defvar *cursor-animate-steps* 12.)
(defvar *cursor-animate-growth-quantum* 3)

(defun animate-cursor-move (dest-screen-box dest-row dest-cha-no)
  (unless (null *zoom-step-pause-time*)
    (drawing-on-window (*boxer-pane*)
      (with-temporary-bp (dest-bp(values dest-row dest-cha-no dest-screen-box))
	(multiple-value-bind (to-x to-y)
	    (bp-coordinates dest-bp)
	  (multiple-value-bind (from-x from-y)
	      (bp-coordinates *point*)
	    (let ((from-hei (get-cursor-height (bp-cha *point*)))
		  (to-hei (get-cursor-height (bp-cha dest-bp)))
		  (half-delta-x (fixr (/ (- to-x from-x) 2)))
		  (half-delta-y (fixr (/ (- to-y from-y) 2)))
		  (delta-size (* *cursor-animate-steps*
				  *cursor-animate-growth-quantum*)))
	      (flet ((draw-trail-up ()
		       (do ((i 0 (1+& i))
			    (wid 3 (+ wid *cursor-animate-growth-quantum*))
			    (hei from-hei (+ hei
					      *cursor-animate-growth-quantum*))
			    (x from-x (+ x (fixr(/ half-delta-x
						    *cursor-animate-steps*))))
			    (y from-y (+ y (fixr(/ half-delta-y
						    *cursor-animate-steps*)))))
			   ((>=& i *cursor-animate-steps*))
			 (draw-rectangle alu-xor wid hei x y)
                         (force-graphics-output)
			 (snooze *zoom-step-pause-time*)
			 (draw-rectangle alu-xor wid hei x y)
                         (force-graphics-output)))
		     (draw-trail-down ()
		       (do ((i 0 (1+& i))
			    (wid (+ 3 delta-size)
				 (- wid *cursor-animate-growth-quantum*))
			    (hei (+ to-hei delta-size)
				 (- hei *cursor-animate-growth-quantum*))
			    (x (- to-x half-delta-x)
			       (+ x (fixr (/ half-delta-x
					      *cursor-animate-steps*))))
			    (y (- to-y half-delta-y)
			       (+ y (fixr (/ half-delta-y
					      *cursor-animate-steps*)))))
			   ((>=& i *cursor-animate-steps*))
			 (draw-rectangle alu-xor wid hei x y)
                         (force-graphics-output)
			 (snooze *zoom-step-pause-time*)
			 (draw-rectangle alu-xor wid hei x y)
                         (force-graphics-output))))
	      ;; first get bigger...
	      (draw-trail-up)
	      ;; now get smaller
	      (draw-trail-down)))))))))

(defun animate-scrolling (screen-box)
  (unless (null *zoom-step-pause-time*)
    (drawing-on-window (*boxer-pane*)
      (multiple-value-bind (box-x box-y)
	  (xy-position screen-box)
	(multiple-value-bind (left top right bottom)
	    (box-borders-widths (box-type screen-box) screen-box)
	  (declare (ignore left))
	  (let ((x (- (+ box-x (screen-obj-wid screen-box)) right 4))
		(y (+ box-y top))
		(wid (+ right 8))
		(hei (- (screen-obj-hei screen-box) top bottom)))
	    (dotimes (i 12)
	      (draw-rectangle alu-xor wid hei x y)
              (force-graphics-output)
	      (snooze *zoom-step-pause-time*)
	      (draw-rectangle alu-xor wid hei x y)
              (force-graphics-output))))))))

(defun move-to-bp (bp &optional (moving-bp *point*))
  (let ((*zoom-step-pause-time* (if (zerop *move-bp-zoom-pause-time*)
				    nil
				    *move-bp-zoom-pause-time*)))
    (cond ((superior? (bp-box bp) (bp-box moving-bp))
	   (downward-move-to-bp bp moving-bp))
	  (t
	   ;; looks like we are going to have to go up before we can go down
	   (upward-move-to-common-box bp moving-bp)
	   (downward-move-to-bp bp moving-bp)))
    (enter (bp-box moving-bp))))

;; basically like MOVE-BP (and MOVE-POINT) except it will change the
;; state of the display (zooming in/out, scrolling) if it has to
;; CAREFUL: this does not do the checking to insure that the destination
;; is part of the boxer hierarchy.  You have to do this before or it will
;; blow out.  The minimal thing is (SUPERIOR? <TARGET-BOX> *INITIAL-BOX*)
;; There is no type checking here to allow the caller to be more specific
;; and be able to deliver a more appropriate error message
(defmacro move-to-bp-values (bp values-form)
  (let ((temp-bp (gensym)))
    `(with-temporary-bp (,temp-bp ,values-form)
       (move-to-bp ,temp-bp ,bp))))

;;; Move upward until we reach a place where BP is in some inferior
;;; of (POINT-BOX). We have to march up the screen structure rather than
;;; the actual structure because we might be inside of a port

(defun upward-move-to-common-box (bp &optional (moving-bp *point*))
  (let  ((box (bp-box moving-bp)))
    (cond ((superior? (bp-box bp) box))	;we have arrived
	  (t (unless (eq box *initial-box*)
	       (com-exit-box)
;	       (exit box (superior-screen-box (bp-screen-box moving-bp))
;		     (superior-box box) t)
	       )
	     (upward-move-to-common-box bp moving-bp)))))

;;; The destination is in some inferior of the current box
(defun downward-move-to-bp (bp &optional (moving-bp *point*))
  (let* ((row (bp-row bp))
	 (old-row (bp-row moving-bp))
	 (screen-row (visible-screen-obj-of-inferior-actual-obj
		      row (bp-screen-box moving-bp))))
    (cond ((not-null screen-row)
	   ;; the destination is visible already
	   (animate-cursor-move (screen-box screen-row)
				(bp-row bp) (bp-cha-no bp))
	   (move-bp moving-bp (bp-values bp))
	   (set-bp-screen-box moving-bp (screen-box screen-row)))
	  ((or (not (null (row-row-no (bp-box moving-bp) row)))
	       (and row (eq row (slot-value (bp-box moving-bp) 'closets))))
	   ;; the destination is in the current box but
	   ;; is scrolled out of sight
	   (move-bp moving-bp (bp-values bp))
	   (when (ensure-row-is-displayed (bp-row moving-bp)
					  (bp-screen-box moving-bp)
					  (if (row-> row old-row) 1 -1))
	     (animate-scrolling (bp-screen-box moving-bp))
	     ;; explicit call to redisplay to make sure screen structure
	     ;; gets created before more processing occurs
	     (repaint)))
	  (t
	   (let* ((path (find-path-from-superior-to-inferior
			 (bp-box moving-bp) (bp-box bp)))
		  (new-box (lowest-visible-box (bp-screen-box moving-bp)
					       path)))
	     (cond ((null path)
		    (editor-barf "the bp, ~a, is not in an inferior of ~a" bp
				 (bp-box moving-bp)))
		   ((null new-box)
		    ;; the downward chain of boxes is not visible probably
		    ;; because we are scrolled to the wrong place in the
		    ;; current screen box so we scroll to the correct
		    ;; row, then try again
		    (move-bp moving-bp (box-self-bp-values (car path)))
		    (when (ensure-row-is-displayed (bp-row moving-bp)
						   (bp-screen-box moving-bp)
						   (if (row-> (superior-row
							       (car path))
							      old-row)
						       1 -1))
		      (animate-scrolling (bp-screen-box moving-bp))
		      ;; explicit call to redisplay to make sure screen
		      ;; structure gets created before more processing occurs
		      (repaint))
		    ;; check to see if the new-box is visible now, if it
		    ;; isn't, it is probably because it is scrolled
		    ;; horizontally since we've scrolled vertically to the
		    ;; row it is suppose to be on
		    (let ((new-new-box (lowest-visible-box
					(bp-screen-box moving-bp) path)))
		      (when (null new-new-box)
			(let ((new-new-sb (allocate-screen-obj-for-use-in
					   (car path)
					   (bp-screen-box moving-bp))))
			  (move-bp moving-bp (box-first-bp-values (car path)))
			  (set-bp-screen-box moving-bp new-new-sb)
			  ;; now we can zoom
			  (push *outermost-screen-box*
				*outermost-screen-box-stack*)
			  (let ((*box-zoom-waiting-time*
				 (* *box-zoom-waiting-time* 2)))
			    ;; slow things down a bit
			    (set-outermost-box (bp-box moving-bp)
					       new-new-sb)))))
		    (downward-move-to-bp bp moving-bp))
		   (t
		    (when (display-style-graphics-mode?
			   (display-style-list new-box))
		      ;; the chain is in an inferior of a
		      ;; graphics/graphics-data-box which is currently
		      ;; in graphics mode so we have to toggle it
		      ;; before we can zoom it up
		      (toggle-view-internal new-box)
		      (repaint))
		    ;; move to lowest visible box, zoom, then try again
		    (animate-cursor-move
		     (visible-screen-obj-of-inferior-actual-obj
		      new-box (bp-screen-box moving-bp))
		     (first-inferior-row new-box) 0)
		    (move-bp moving-bp (box-first-bp-values new-box))
		    (set-bp-screen-box
		     moving-bp
		     (visible-screen-obj-of-inferior-actual-obj
		      new-box (bp-screen-box moving-bp)))
		    ;; now we can zoom
		    (push *outermost-screen-box*
			  *outermost-screen-box-stack*)
		    (let ((*box-zoom-waiting-time*
			   (* *box-zoom-waiting-time* 2)))
		      ;; slow things down a bit
		      (set-outermost-box (bp-box moving-bp)
					 (bp-screen-box moving-bp)))
		    ;; then try again
		    (downward-move-to-bp bp moving-bp))))))))



;;;; mouse tracking support

(defvar *border-tab-hysteresis* 5)

;;; handles tracking of the mouse.
;;; we may have to pass screen-box into restore-fun (getting the port strut right)
#-opengl
(defun track-mouse-area (hilight-fun &key x y width height)
  (let ((backing-store (allocate-backing-store width height)))
    (drawing-on-window (*boxer-pane*)
      (let ((min-x (-& x *border-tab-hysteresis*))
            (min-y (-& y *border-tab-hysteresis*))
            (max-x (+& x width *border-tab-hysteresis*))
            (max-y (+& y height *border-tab-hysteresis*))
            (icon-on? t))
        (flet ((icon-on ()
                 (erase-rectangle width height x y)
                 (funcall hilight-fun x y width height)
                 (force-graphics-output)
                 (setq icon-on? T))
               (icon-off ()
                 (repaint) ; gak !!
                 ;(bitblt-to-screen alu-seta width height backing-store 0 0 x y)
                 ;(force-graphics-output)
                 (setq icon-on? nil)))
          ;; 1st grab what's on the screen...
          (bitblt-from-screen alu-seta width height backing-store x y 0 0)
          ;; initially turn the icon on since that is how we got here in the 1st place
          (icon-on)
          (multiple-value-bind (final-x final-y)
              (with-mouse-tracking ((mouse-x x) (mouse-y y))
                (progn
                  (cond ((and (null icon-on?)
                              ;; if the icon is off, and we move back in
                              (<& min-x mouse-x max-x) (<& min-y mouse-y max-y))
                         ;; then turn the icon back on
                         (icon-on))
                        ((and icon-on?
                              ;; if the icon is on and we move out
                              (or (not (<& min-x mouse-x max-x))
                                  (not (<& min-y mouse-y max-y))))
                         ;; then turn off the visual indicator
                         (icon-off)))))
            ;; first turn the icon off if it is on...
            (unless (null icon-on?) (icon-off))
            (deallocate-backing-store backing-store)
            ;; now return whether we are still on...
            (and (<& min-x final-x max-x) (<& min-y final-y max-y))))))))

#+opengl
(defun track-mouse-area (hilight-fun &key x y width height)
  ;; inputs may be floats now...
  (setq x (floor x) y (floor y)
        width (ceiling width) height (ceiling height))
  (let ((min-x (-& x *border-tab-hysteresis*))
         (min-y (-& y *border-tab-hysteresis*))
         (max-x (+& x width *border-tab-hysteresis*))
         (max-y (+& y height *border-tab-hysteresis*))
         (icon-on? t))
    (flet ((icon-on ()
             (drawing-on-window (*boxer-pane*)
               (erase-rectangle width height x y)
               (funcall hilight-fun x y width height))
             (force-graphics-output)
             (setq icon-on? T))
           (icon-off ()
             (repaint)
             (setq icon-on? nil)))
      (icon-on)
      (multiple-value-bind (final-x final-y)
          (with-mouse-tracking ((mouse-x x) (mouse-y y))
            (progn
              (cond ((and (null icon-on?)
                          ;; if the icon is off, and we move back in
                          (<& min-x mouse-x max-x) (<& min-y mouse-y max-y))
                     ;; then turn the icon back on
                     (icon-on))
                    ((and icon-on?
                          ;; if the icon is on and we move out
                          (or (not (<& min-x mouse-x max-x))
                              (not (<& min-y mouse-y max-y))))
                     ;; then turn off the visual indicator
                     (icon-off)))))
        ;; first turn the icon off if it is on...
        (unless (null icon-on?) (icon-off))
        ;; now return whether we are still on...
        (and (<& min-x final-x max-x) (<& min-y final-y max-y))))))

(defun mouse-still-down-after-pause? (pause-time)
  (not
   (or (wait-with-timeout nil pause-time #'(lambda () (zerop& (mouse-button-state))))
       ;; one final check
       (zerop& (mouse-button-state)))))

;;;; new port stuff

;;;; dummy box is the box with the message "redirect me"
(defvar *dummy-box* nil)


;;;; generic port is the port created when no target is specified,
;;;; which can be redirected
(defvar *generic-port* nil)



;;;; Wrong! You should be using modes now !!!

#|
;;; leave it here until we manage to flush old code
;;; utilities for temporarily rebinding keys (like for
;;; copying/moving regions)

(defvar *saved-key-functions* nil)

;;; Note, this is saving and rebinding TOP LEVEL bindings
;;; shadowed bindings will remain unaffected
(defun save-and-rebind-key (key-name new-function)
  (let ((existing (if (boundp key-name) (caddr (symbol-value key-name)) ':unbound))
	(entry (fast-assq key-name *saved-key-functions*)))
    ;; record the old version
    (cond ((null entry) (push (cons key-name existing) *saved-key-functions*))
	  (t (setf (cdr entry) existing)))
    ;; now set it to the new version
    (eval::boxer-toplevel-set-nocache
     key-name
     (eval::make-compiled-boxer-function
      :arglist nil :precedence 0 :infix-p nil :object new-function))))

;; key should look like 'bu::crap
(defun restore-saved-function (key-name)
  (let ((entry (cdr (fast-assq key-name *saved-key-functions*)))
	(vanilla (lookup-mode-key *global-top-level-mode* key-name)))
    (cond ((null entry)
	   (cond ((null vanilla)
                 ;(warn "No saved function for ~A, Unbinding the key" key-name)
		  (eval::boxer-toplevel-nocache-unset key-name))
		 (t
		  (warn "No saved function for ~A, setting to top level value" key-name)
		  (eval::boxer-toplevel-set-nocache key-name vanilla))))
	  ((or (eq entry ':unbound) (eq entry eval::*novalue*))
	   (eval::boxer-toplevel-nocache-unset key-name))
	  ((eval::compiled-boxer-function? entry)
	   (eval::boxer-toplevel-set-nocache key-name entry))
	  (t
	   ;; probably means the previous binding for the key was
	   ;; to a box, right thing for now is to unbind the key
	   (if (null vanilla)
	       (eval::boxer-toplevel-nocache-unset key-name)
	       (progn
		 (warn "No saved function for ~A, setting to top level value" key-name)
		 (eval::boxer-toplevel-set-nocache key-name vanilla)))))))
|#


;;; mouse mode utilities

(defbasic-mode retarget-port-mode
  (#+(or apple win32) bu::mouse-click #-(or apple win32) bu::mouse-middle
                    com-redirect-generic-port)
  (#+(or apple win32) bu::mouse-click-on-graphics
   #-(or apple win32) bu::graphics-mouse-middle
   com-redirect-generic-port)
  (#+(or apple win32) bu::mouse-click-on-sprite
   #-(or apple win32) bu::sprite-mouse-middle
   com-redirect-generic-port))

;;; make a generic port, toggle the middle mouse cursor

(defun make-generic-port (&rest foo)
  (declare (ignore foo))
  (if (null *dummy-box*) (setq *dummy-box* (make-dummy-box)))
  (if (null *generic-port*)
      (progn (setq *generic-port* (port-to-internal *dummy-box*))
	     (INSERT-CHA *POINT* *generic-port*)
	     (set-mouse-cursor :retarget)
             (boxer-editor-message "Click on a Box to Select Port's Target")
             (add-mode (retarget-port-mode))
             #-opengl (add-redisplay-clue (point-row) ':insert))
      (boxer-editor-error "Use the generic port you have made."))
  eval::*novalue*)

#|
(defun make-generic-port (&rest foo)
  "make a generic port so the person may redirect it"
  (declare (ignore foo))
  (if (null *dummy-box*) (setq *dummy-box* (make-dummy-box)))
  (if (null *generic-port*)
      (progn (setq *generic-port* (port-to-internal *dummy-box*))
	     (INSERT-CHA *POINT* *generic-port*)
	     (set-mouse-cursor :retarget)
	     (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1 0)
				  #'com-redirect-generic-port)
	     (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1
                                                            0 :graphics)
				  #'com-redirect-generic-port)
	     (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1
                                                            0 :sprite)
				  #'com-redirect-generic-port)
	     (add-redisplay-clue (point-row) ':insert)
	     eval::*novalue*)
      (progn
	(boxer-editor-error "Use the generic port you have made.")
	eval::*novalue*)))
|#

(defvar *previous-port-target* nil)

(defun make-dummy-box ()
  (make-box '(("you may now redirect this port."))))

(defvar *inside-mouse-coords-for-prims?* nil)

;;; redirection of the port
(defun com-redirect-generic-port (&rest foo)
  "redirect the generic port to something. grabs box pointed to by mouse "
  (declare (ignore foo))
  (if (null *generic-port*)
      (boxer-editor-error "Need to make a generic port first.")
      (let* ((*inside-mouse-coords-for-prims?* T)
             (target :unspecified))
        (catch 'mouse-coord-cancel (setq target (get-box-under-mouse)))
        (unless (eq target :unspecified) (retarget-port *generic-port* target))
	(modified *generic-port*)
	(clean-mouse-port-state)))
  eval::*novalue*)

;;;;; get the box the mouse points to.
(defun get-box-under-mouse ()
  (drawing-on-window (*boxer-pane*)
    ;; need to get the offsets set up so the mouse coords
    ;; are returned relative to the *boxer-pane* rather than
    ;; the *boxer-frame*
    ;; This also binds the font map
    (multiple-value-bind (x y)
        (mouse-window-coords)
      (let* ((mbp (mouse-position-values x y))
	     (mrow (bp-row mbp)))
        (if (and (row? mrow) (not (null (superior-box mrow))))
	  (superior-box mrow)
	  ;; maybe should error out instead ?
	  nil)))))

(defun clean-mouse-port-state ()
  (reset-mouse-cursor)
  (remove-mode (retarget-port-mode))
  (setq *generic-port* nil))

#| ;; converted to using modes
(defun clean-mouse-port-state ()
  (reset-mouse-cursor)
  ;;; rebind the mouse-middle
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1 0))
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1
                                                    0 :graphics))
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1
                                                    0 :sprite))
  (setq *generic-port* nil))
|#



;; used right after the region is marked
(defbasic-mode region-mode
  (#+apple            bu::option-mouse-click
   #+win32          bu::alt-mouse-click
   #-(or apple win32) bu::mouse-left
   com-suck-region)
  (#+apple            bu::option-mouse-click-on-graphics
   #+win32            bu::alt-mouse-click-on-graphics
   #-(or apple win32) bu::graphics-mouse-left
   com-suck-region)
  (#+apple            bu::option-mouse-click-on-sprite
   #+win32            bu::alt-mouse-click-on-sprite
   #-(or apple win32) bu::sprite-mouse-left
   com-suck-region)
  (#+apple            bu::command-mouse-click
   #+win32            bu::ctrl-mouse-click
   #-(or apple win32) bu::mouse-right
   com-suck-copy-region)
  (#+apple            bu::command-mouse-click-on-graphics
   #+win32            bu::ctrl-mouse-click-on-graphics
   #-(or apple win32) bu::graphics-mouse-right
   com-suck-copy-region)
  (#+apple            bu::command-mouse-click-on-sprite
   #+win32            bu::ctrl-mouse-click-on-sprite
   #-(or apple win32) bu::sprite-mouse-right
   com-suck-copy-region))


;; used right after the region is cut/copied with the mouse
(defbasic-mode suitcase-mode
  (#+(or apple win32) bu::mouse-click
   #-(or apple win32) bu::mouse-middle
   com-bring-back-region)
  (#+(or apple win32) bu::mouse-click-on-graphics
   #-(or apple win32) bu::graphics-mouse-middle
   com-bring-back-region)
  (#+(or apple win32) bu::mouse-click-on-sprite
   #-(or apple win32) bu::sprite-mouse-middle
   com-bring-back-region))

;;; rebinds various mouse keys

(defun entering-region-mode ()
  (let ((main-cut-name #+(or apple win32) (current-mouse-click-name 0 2)
                       #-(or apple win32) (current-mouse-click-name 0 0))
        (main-copy-name #+(or apple win32) (current-mouse-click-name 0 1)
                        #-(or apple win32 )(current-mouse-click-name 2 0)))
    (boxer-editor-message "~A to cut, or ~A to copy the region"
                          main-cut-name main-copy-name)
    (set-mouse-cursor :region-mode)
    (add-mode (region-mode))))

#|
    (save-and-rebind-key main-cut-name #'com-suck-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 2 :graphics)
                         #-mcl (current-mouse-click-name 0 0 :graphics)
                         #'com-suck-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 2 :sprite)
                         #-mcl (current-mouse-click-name 0 0 :sprite)
                         #'com-suck-region)
    (save-and-rebind-key main-copy-name #'com-suck-copy-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 1 :graphics)
                         #-mcl (current-mouse-click-name 2 0 :graphics)
                         #'com-suck-copy-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 1 :sprite)
                         #-mcl (current-mouse-click-name 2 0 :sprite)
                         #'com-suck-copy-region)
|#

(defun exiting-region-mode (&optional (reset-mouse-cursor? t))
  (when reset-mouse-cursor? (reset-mouse-cursor))
  (remove-mode (region-mode)))

#|
  (restore-saved-function #+mcl (current-mouse-click-name 0 2)
                          #-mcl (current-mouse-click-name 0 0))
  (restore-saved-function #+mcl (current-mouse-click-name 0 2 :graphics)
                          #-mcl (current-mouse-click-name 0 0 :graphics))
  (restore-saved-function #+mcl (current-mouse-click-name 0 2 :sprite)
                          #-mcl (current-mouse-click-name 0 0 :sprite))
  (restore-saved-function #+mcl (current-mouse-click-name 0 1)
                          #-mcl (current-mouse-click-name 2 0))
  (restore-saved-function #+mcl (current-mouse-click-name 0 1 :graphics)
                          #-mcl (current-mouse-click-name 2 0 :graphics))
  (restore-saved-function #+mcl (current-mouse-click-name 0 1 :sprite)
                          #-mcl (current-mouse-click-name 2 0 :sprite))
|#

;;; cursor stuff
(defvar *suitcase-mode* nil)

;(defun change-cursor-to-suitcase ()
;  (set-mouse-cursor :suitcase)
;  (setq *suitcase-mode* t))

;(defun change-cursor-back-from-suitcase ()
;  (reset-mouse-cursor)
;  (setq *suitcase-mode* nil))

(defvar *editor-abort-chars*
  #-mcl (list (make-char #\g 1) (make-char #\. 1)
              ;; 200. is the STOP key on Suns
              #+sun (make-char (code-char 200))
              #+sun (make-char (code-char 200) 1))
  ;; MCL can't encode shift bits in character objects
  #+mcl (list #\Bell))

#+mcl
(defvar *unshifted-mac-editor-abort-chars* (list #\g #\.))

(defun editor-abort-char? (char &optional bits)
  #-mcl(declare (ignore bits))
  (and (characterp char)
       (or (member char *editor-abort-chars* :test #'char-equal)
           #+mcl (and (=& bits 1)
                      (member char *unshifted-mac-editor-abort-chars*
                              :test #'char-equal)))))

;; used in keydef-high
(defvar *defined-input-device-platforms* nil
  "A list of the input platforms for which we know how to make input names")


;; pushed onto in MAKE-INPUT-DEVICES
(defvar *bound-input-device-platforms* nil
  "A list of all the platforms for which we have made input device names")


(defmacro define-input-devices (platform shift-list mouse-string special-keys)
  `(progn
    (unless (member ',platform *defined-input-device-platforms*)
      (push ',platform *defined-input-device-platforms*))
    (setf (get ',platform :shift-list)   ',shift-list
     (get ',platform :mouse-string) ',mouse-string
     (get ',platform :special-keys) ,special-keys)))

(defun input-device-shift-list (platform)
  (get platform :shift-list))

(defun input-device-mouse-string (platform)
  (get platform :mouse-string))

(defun input-device-special-keys (platform)
  (get platform :special-keys))

(defun get-shift-names (shift-bits)
  (let ((name (nth (1- shift-bits)
		   (boxer::input-device-shift-list
		    boxer::*current-input-device-platform*))))
    (if (null name)
	(error "Can't find shift names for shift bits: ~D" shift-bits)
	name)))



;; popup vars & macros

(defvar *popup-area-doc-alist* nil)

(defvar *popup-docs* nil)

(defvar *popup-doc-font*
  #+lispworks (%make-font-number-internal 0 0)
  #+mcl (make-boxer-font '("Geneva" 10)))

;; a pale yellow...
;; try and get a close match to the usual Windows popup doc
(defvar *popup-doc-color* #-(or lwwin opengl) (%make-color 95 95 70))
(defvar *mouse-doc-highlight-color*)

#+(or lwwin opengl)
(def-redisplay-initialization (setq *popup-doc-color* (%make-color 95 95 70 60)
                                    *mouse-doc-highlight-color* (%make-color 10 30 10)))

;; should be as big as the largest popup-doc
(defvar *popup-doc-backing-store* nil)
(defvar *popup-doc-on?* nil)

(defvar *popup-doc-border-width* 1)
(defvar *popup-doc-padding* 2
  "Amount of space between text and border")


(defmacro define-popup-doc (border-area string)
  `(let ((existing-entry (assoc ,border-area *popup-area-doc-alist*))
         (newdoc (make-instance 'popup-doc :string ,string)))
     (cond ((null existing-entry)
            (push (cons ,border-area newdoc) *popup-area-doc-alist*)
            (push newdoc *popup-docs*))
           (t
            (setq *popup-docs* (delete (cdr existing-entry) *popup-docs*))
            (setf (cdr existing-entry) newdoc)
            (push newdoc *popup-docs*)))))

(defun get-popup-doc-for-area (border-area)
  (cdr (assoc border-area *popup-area-doc-alist*)))



;;; Box properties

;; these will define a box with the neccessary subboxes, accessors for the
;; subboxes, conversion to lisp values of the subboxes and a macro which
;; binds the lisp values of the relevant bu::new-box-properties box
;; also defstruct for print-hints for VC's and initialize-new-box-properties

(defvar *box-properties* nil)

(defmacro define-box-property ((name boxer-name)
                               init-args new-box-init-form ; should take 2 (prop box)
                               new-prop-arg set-new-prop-form ; 2 arg (prop value)
                               make-prop-box-form ; 0 args
                               )
  (let ((new-box-init-function-name  (intern (format nil "INIT-NEW-BOX-PROP-~A" name)))
        (set-prop-function-name (intern (format nil "SET-NEW-BOX-PROP-~A" name)))
        (make-prop-name  (intern (format nil "MAKE-NEW-BOX-PROP-~A" name))))
    ;; we could use gensyms for these since they get put on a plist
    ;; but then they would be a pain to debug when they blow out
    `(progn
       (unless (member ',name *box-properties*) (push ',name *box-properties*))
       (setf (get ,name :boxer-name) ',boxer-name)
       (defun ,new-box-init-function-name ,init-args
         ,new-box-init-form)
       (setf (get ,name :new-box-init) ',new-box-init-function-name)
       (defun ,set-prop-function-name ,new-prop-arg
         ,set-new-prop-form)
       (setf (get ,name :set-new-prop) ',set-prop-function-name)
       (defun ,make-prop-name ()
         ,make-prop-box-form)
       (setf (get ,name :make-prop-box) ',make-prop-name))))


;; display properties
(define-box-property (:always-zoom bu::always-zoom)
  ;; init
  (prop box) (set-always-zoom? box (eval::true? prop))
  ;; set prop
  (prop new-value) (change prop (eval::boxer-boolean new-value))
  ;; make
  (make-box '(("False")) 'data-box 'bu::always-zoom))

(define-box-property (:shrink-on-exit bu::shrink-on-exit)
  (prop box) (set-shrink-on-exit? box (eval::true? prop))
  (prop new-value) (change prop (eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::shrink-on-exit))

(define-box-property (:manual-size bu::manual-sizing)
  (prop box) (when (eval::true? prop)
               (set-bottom-right-hotspot-active? box t))
  (prop new-value) (change prop (eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::manual-sizing))

(define-box-property (:autofill bu::autofill)
  (prop box) (set-auto-fill? box (eval::true? prop))
  (prop new-value) (change prop (eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::autofill))

(define-box-property (:shrink-proof? bu::shrink-proof)
  (prop box) (set-shrink-proof? box (eval::true? prop))
  (prop new-value) (change prop (eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::shrink-proof))

;; link/file properties
(define-box-property (:read-only bu::read-only)
  (prop box) (set-read-only-box? box (eval::true? prop))
  (prop new-value) (change prop (eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::read-only))

(define-box-property (:relative-filename bu::relative-filename)
  (prop box) (set-relative-filename? box (eval::true? prop))
  (prop new-value) (change prop (eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::relative-filename))

(define-box-property (:auto-load bu::auto-load)
  (prop box) (set-autoload-file? box (eval::true? prop))
  (prop new-value) (change prop (eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::auto-load))

;; boxtops
;; member of (:standard :framed :graphics-named :folder-picture :name-only)
(define-box-property (:boxtop bu::initial-boxtop)
  (prop box) (let* ((bts (flat-box-items prop))
                    (boxtop-type (intern-keyword (car bts)))
                    (bt-arg (cadr bts)))
               (cond ((eq boxtop-type :named-graphic)
                      (putprop box bt-arg :boxtop))
                     (t (putprop box boxtop-type :boxtop))))
  (prop new-value) (change prop (make-vc (list (mapcar #'intern-in-bu-package
                                                       new-value))))
  (make-box '(("Standard")) 'data-box 'bu::initial-boxtop))



;; this should be fault tolerant to handle the possibility that an
;; older, local new-box-properties box might not have the full set of
;; brand newnew box properties
(defun get-prop-from-props-box (property-name props-box)
  (let ((local-prop (lookup-variable-in-box-only
                     props-box (get property-name :boxer-name)))
        (tl-prop (eval::static-variable-value (symbol-value 'bu::new-box-properties))))
    (cond ((and (null local-prop) (eq local-prop tl-prop)) nil)
          ((null local-prop)
           ;; try recursing upwards
           (get-prop-from-props-box property-name
                                    (eval::lookup-static-variable
                                     (superior-box (superior-box props-box))
                                     ;; just 1 superior-box will only get us to the box
                                     ;; where the props-box is already living
                                     'bu::new-box-properties)))
          (t local-prop))))

;; main properties interface
(defun initialize-new-box-properties (box &optional
                                          (props (eval::boxer-symeval
                                                  'bu::new-box-properties)))
  (dolist (property *box-properties*)
    (funcall (get property :new-box-init)
             (get-prop-from-props-box property props) box)))

(defun make-new-box-properties-box ()
  (make-box (mapcar #'(lambda (property)
                        (list (funcall (get property :make-prop-box))))
                    *box-properties*)))

(defun set-new-box-property (property new-value
                                      &optional
                                      (props (eval::boxer-symeval
                                              'bu::new-box-properties)))
  (funcall (get property :set-new-prop)
           (get-prop-from-props-box property props) new-value))

;; helps to get the wording right in dialogs
(defun top-level-props? ()
  (eq (eval::boxer-symeval 'bu::new-box-properties)
      (eval::static-variable-value
       (symbol-value 'bu::new-box-properties))))

