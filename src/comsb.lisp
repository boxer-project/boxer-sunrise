;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER; Base:8. -*-

#|


 $Header: comsb.lisp,v 1.0 90/01/24 22:08:58 boxer Exp $


 $Log:	comsb.lisp,v $
;;;Revision 1.0  90/01/24  22:08:58  boxer
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


	  This file contains top level definitions for
	  BOXER Editor Commands that deal with Boxes

Box Commands:
  COM-MAKE-BOX COM-TOGGLE-BOX-TYPE COM-MAKE-DATA-BOX
  COM-ENTER-BOX COM-MAKE-AND-ENTER-BOX
  COM-MAKE-AND-ENTER-DATA-BOX COM-EXIT-BOX
Shrinking and Expanding:
  COM-COLLAPSE-BOX COM-SHRINK-BOX COM-EXPAND-BOX COM-MAKE-SHRINK-PROOF-SCREEN
  COM-UNSHRINK-PROOF-SCREEN COM-SET-OUTERMOST-BOX COM-NAME-BOX COM-FIX-BOX-SIZE
  COM-UNFIX-BOX-SIZE
Ports:
  COM-MAKE-PORT COM-PLACE-PORT
Graphics:
  COM-MAKE-GRAPHICS-BOX COM-MAKE-GRAPHICS-DATA-BOX COM-MAKE-GRAPHICS-BOX
  COM-TOGGLE-BOX-VIEW COM-MAKE-SPRITE-BOX



Modification History (most recent at top)
 8/28/07 toggle-view-internal checks to make sure that we really have a graphics-sheet
         in the graphics-info slot to prevent (for now) toggling of sprite boxes
 1/15/07 make-turtle-box-internal, (redisplay)'s => (repaint)'s
 7/28/05 com-make-box should supply 'doit-box type arg for
         make-initialized-box-for-editor
 7/02/05 new function for making new boxes which use new-box-properties
         make-initialized-box-for-editor
 4/21/03 merged current LW and MCL files
 3/15/02 COM-EXPAND-BOX now checks the always-zoom? flag
 2/13/01 merged current LW and MCL files
10/21/99 autofill finished debugging (for now...)
10/17/99 new autofill stuff
01/06/98 COM-ENTER-NEXT-BOX opens the next box if it is shrunken
10/06/98 make-graphics/turtle-box-internal both changed to start with resizing
         automatically enabled
10/06/98 started logging changes: source = boxer version 2.3beta+

|#

#-(or lispworks mcl lispm) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+(or lispworks mcl)       (in-package :boxer)




;;; like make-initialized-box with box-property initialization
;;; used by com-boxify-region, com-make-(and-enter-)(data-)box, but
;;; not make-graphics/sprite-box
(defun make-initialized-box-for-editor (&optional (box-type 'data-box))
  (let ((box (make-initialized-box :type box-type)))
    (unless (fast-memq *global-top-level-mode* *active-modes*)
      ;; don't do property init if we are in vanilla mode
      (initialize-new-box-properties box))
    box))


;;;; Box Commands

(defboxer-command COM-BOXIFY-REGION (&optional (box-type 'data-box))
  "puts all of the characters in the current
region into a box. "
  (let ((region-to-box (or *region-being-defined* (get-current-region))))
    (cond ((name-row? (point-row))
	   (reset-region)
	   (boxer-editor-error "You cannot make boxes inside a name"))
	  ((not (null region-to-box))
	   (kill-region region-to-box)
	   (insert-cha *point* (make-initialized-box-for-editor box-type) ':fixed)
           (mark-file-box-dirty (point-row))
	   (com-enter-box)
	   (yank-region *point* region-to-box)
	   (flush-region region-to-box)
	   (exiting-region-mode)
	   (setq region-to-box nil))))
  (reset-editor-numeric-arg)
  eval::*novalue*)

;(defboxer-COMMAND COM-UNBOXIFY ()
;  "unboxes the point-box"
;  (reset-editor-numeric-arg)
; (with-multiple-execution
;  (unless (eq (point-box) *initial-box*)
;    (let* ((bp-1 (make-bp :moving))
;	   (bp-2 (make-bp :moving))
;	   (doomed-region (make-editor-region bp-1  bp-2)))
;
;    ;;; bracket the box
;      (move-bp bp-1
;	       (BOX-FIRST-BP-VALUES (superior-box (bp-row *point*))))
;      (move-bp bp-2
;	       (BOX-LAST-BP-VALUES (superior-box (bp-row *point*))))
;      (setf (interval-box doomed-region) (point-box))
;      ;;; kill region ,exit,rubout box ,yank stuff
;      (kill-region doomed-region)
;
;      (LET ((BOX (BOX-SCREEN-POINT-IS-IN)))
;	(UNLESS (EQ BOX *INITIAL-BOX*)
;	  (EXIT BOX (SUPERIOR-SCREEN-BOX (SCREEN-BOX-POINT-IS-IN))
;		(SUPERIOR-BOX BOX) t)))
;      (add-redisplay-clue (bp-row *point*) ':insert)
;      (delete-cha-at-cha-no (point-row) (1- (point-cha-no)))
;      (yank-region *point*  doomed-region)
;      (add-redisplay-clue (bp-row *point*) ':insert)
;
;      ;;; cleanup bps, editor-arg
;      (delete-bp (bp-row bp-2) bp-2)
;      (delete-bp (bp-row bp-1) bp-1)
;      (reset-editor-numeric-arg)
;
;    )))
;  eval::*novalue*)
;
;
;;;; unboxing regions and the point box
(defboxer-command COM-UNBOXIFY ()
  "unboxes the point-box, or each box in the *region-being-defined*"
  (cond (*region-being-defined*
	 (com-unboxify-region))
	 ((eql (point-box) *initial-box*) nil)
	 (t (com-unboxify-point-box)))
  (reset-editor-numeric-arg)
  (reset-region)
  (mark-file-box-dirty (point-row))
  eval::*novalue*)
;;;;;;;;;;;;;;;; THIS HAS BEEN SUPERCEDED BY UNBOX-BOX
;;;;;;;;;;;;;;;; To make unbox-region and unbox-point-box things do the same thing
;;;; A cheap hack. Kill the box's contents, insert it into the above
;;;; hierarchy, and kill the box.
;(defun com-unboxify-point-box ()
;  "unboxify the point-box"
;  (with-multiple-execution
;      (unless (eq (point-box) *initial-box*)
;	(let* ((bp-1 (make-bp :moving))
;	       (bp-2 (make-bp :moving))
;	       (doomed-region (make-editor-region bp-1  bp-2)))
;	  (move-bp bp-1
;		   (BOX-FIRST-BP-VALUES (superior-box (bp-row *point*))))
;	  (move-bp bp-2
;		   (BOX-LAST-BP-VALUES (superior-box (bp-row *point*))))
;	  (setf (interval-box doomed-region) (point-box))
;	  (kill-region doomed-region)
;	  (LET ((BOX (BOX-SCREEN-POINT-IS-IN)))
;	    (UNLESS (EQ BOX *INITIAL-BOX*)
;	      (EXIT BOX (SUPERIOR-SCREEN-BOX (SCREEN-BOX-POINT-IS-IN))
;		    (SUPERIOR-BOX BOX) t)))
;	  (add-redisplay-clue (bp-row *point*) ':insert)
;	  (delete-cha-at-cha-no (point-row) (1- (point-cha-no)))
;	  (yank-region *point*  doomed-region)
;	  (add-redisplay-clue (bp-row *point*) ':insert)
;	  (delete-bp (bp-row bp-2) bp-2)
;	  (delete-bp (bp-row bp-1) bp-1)
;	  ))))
(defun com-unboxify-point-box ()
  "unboxify the point box"
  (let ((box (box-screen-point-is-in)))
    (com-exit-box)
    (unbox-box box)))


;;;; given that the point is below or on the box, return the screen-row
;;;; that the point should have based on the new box (box)
(defun walk-out-screen-box (box sbox)
  (cond
    ((and (port-box? box) (eq (slot-value sbox 'actual-obj) (ports box)))
     sbox)
    ((and (not (port-box? box))(eq (slot-value sbox 'actual-obj) box))
     sbox)
    (t (walk-out-screen-box box (superior-screen-box sbox)))))
;;;; is the character a box, not a port, and without any ports?
(defun ok-to-unbox (box)
  (and (box? box) (not (port-box? box))))

(defun com-unboxify-region (&optional (region nil))
  "gather up a list of all boxes in the region, then
   unboxify each box in the region. This is necessary as
   unboxing the first box may throw more boxes to the region level"
  (with-region-top-level-bps ((if region region *region-being-defined*)
			      :start-bp-name start-bp
			      :stop-bp-name stop-bp)

    ;;;; move the point to a safe spot. must manage the screen-box
    (move-bp-1 *point*
	       (bp-row stop-bp)
	       (bp-cha-no stop-bp)
	       (walk-out-screen-box (bp-box stop-bp) (point-screen-box))
	       )


    (let ((list-of-boxes nil))
      (do ((row (bp-row start-bp) (next-row row)))
	  ((eql row (next-row(bp-row stop-bp))))

	;;;; need to iterate on the row chas.
	;;;; need stops and starts for the start and end, and a
	;;;; special case for one row regions

	(cond ((and (eql row (bp-row start-bp))
		    (eql row (bp-row stop-bp)))
	       (do-row-chas ((cha row :start (bp-cha-no start-bp)
				 :stop (bp-cha-no stop-bp)))
		 (if (ok-to-unbox cha)
		     (setq list-of-boxes (cons cha list-of-boxes)))
		 ))
	      ((eql row (bp-row start-bp))
	       (do-row-chas ((cha row :start (bp-cha-no start-bp)))
		 (if (ok-to-unbox cha)
		     (setq list-of-boxes (cons cha list-of-boxes)))))
	      ((eql row (bp-row stop-bp))
	       (do-row-chas ((cha row :stop (bp-cha-no stop-bp)))
		 (if (ok-to-unbox cha)
		     (setq list-of-boxes (cons cha list-of-boxes)))))
	      (t (do-row-chas ((cha row))
		   (if (and (box? cha) (not (port-box? cha)))
		       (setq list-of-boxes (cons cha list-of-boxes)))))))
      (dolist (b list-of-boxes)
	(unbox-box b))
      )))


(defun box-index (box)
  "given a box, find its index in its superior-row"
  (let ((index 0))
    (do-row-chas ((cha (superior-row box)))
      (if (eq box cha)
	  (return index)
	  (setq index (1+ index))))))

(defun copy-chas-array-with-same-boxes (from-chas new-row)
  (declare (ignore new-row))
  (with-fast-chas-array-manipulation (from-chas fast-from-chas)
    (flet ((make-length-n-chas-array (n)
	     (let ((array (make-chas-array (max& *chas-array-default-size*
						 n))))
	       (setf (chas-array-active-length array) n)
	       array)))
	(let* ((length (chas-array-active-length from-chas))
	       (to-chas (make-length-n-chas-array length)))
	  (with-fast-chas-array-manipulation (to-chas fast-to-chas)
	    (dotimes& (index length)
		  (setf (fast-chas-array-get-cha fast-to-chas index)
			(fast-chas-array-get-cha fast-from-chas index)))
	    to-chas)))))


(DEFUN make-row-with-same-chas-array
    (FROM-ROW &OPTIONAL NEW-PREVIOUS-ROW NEW-SUPERIOR-BOX)
  (LET ((NEW-ROW (MAKE-UNINITIALIZED-ROW)))
    (setf (actual-obj-tick new-row) -1)
    (SETF (SUPERIOR-BOX NEW-ROW) NEW-SUPERIOR-BOX)
    (SETF (PREVIOUS-ROW NEW-ROW) NEW-PREVIOUS-ROW)
    (SETF (CHAS-ARRAY NEW-ROW) (COPY-CHAS-ARRAY-with-same-boxes
				(CHAS-ARRAY FROM-ROW)
				NEW-ROW))
    NEW-ROW))
;;;; Takes a row and replaces the boxes in the row with ports to the
;;;; the boxes if necessary. It makes a copy of the row first.
(defun prepare-row-inside-port (row replacep)
  (if (null replacep) row
      (progn
	(setq row (make-row-with-same-chas-array row)
					;(copy-row row)
)
	(do* ((index 0 (1+ index))
	      (cha (cha-at-cha-no row index)(cha-at-cha-no row index)))
	     ((eql index (length-in-chas row)))
	  (if (box? cha)
	      (progn
		(delete-cha-at-cha-no row index)
		(insert-cha-at-cha-no
		 row
		 (port-to-internal cha)
		 index))))))
  row)




;;;; Unbox the given box.
;;;; Algorithm: delete the character. Tack the first row of the box onto
;;;; the row's end, copy out the middle rows, and tack on the final row.
;;;; For one row boxes, the first condition just splices in the characters.
;;;; For 2+ row boxes, the chas after the box get inserted between srow and
;;;; its next row. Then the first row of the box gets tacked onto srow.
;;;;     This is all somewhat complicated by the fact that one must move
;;;; the *point* in the case of the first row of a 2+ row box. In order
;;;; to handle this eventuality, there is a check that looks to see if
;;;; the point is on the same row as the box being unboxed. When the
;;;; chas after the box being unboxed are moved, the point gets moved
;;;; also to the new row.
;;;;
;;;; if the box being unboxed is a port, we have to replace all of its sub
;;;; boxes with ports to those boxes

(defun unbox-box (box)
  (let ((srow (superior-row box))
	(index (box-index box))
	(unboxing-a-port-box (port-box? box)))
    (delete-cha-at-cha-no srow index)
    ;;;; if the box (and not a port-box) has some ports, we need to
    ;;;; replace the box with a copy
    ;;;; of itself, and then ubox it, otherwise we trash the ports' rows
    (cond ((port-box? box))
	  ((ports box)
	   (progn
	     (print "found a box with ports")
	     (dolist (d (ports box))
	       (INFORM-PORT-THAT-TARGET-IS-GOING-AWAY d))
	     (setq box (copy-box box))
	     )))

    ;;;; iteration sets the insert row no. The insert-row  is where the rows get
    ;;;; put after being pulled out of the unboxed box.
    (let* ((sbox (superior-box srow)))
      (do* ((r (first-inferior-row box) nr)
	    (nr (next-row r)(if r (next-row r) nil) )
	    (insert-row-no (row-row-no sbox srow) (1+ insert-row-no)))
	   ((null r))
	(cond ((and (eq (previous-row r) nil)
		    (eq (next-row r) nil))
             ;;;; case for a one row box.
	     ;;;; We splice in the chas
	       (insert-row-chas-at-cha-no
		(row-at-row-no sbox insert-row-no)
	        ; filter boxes to ports if necessary
		(prepare-row-inside-port r unboxing-a-port-box)
		index))

            ;;;; Case for boxes with 2+ rows.
	    ;;;; 1. move the end of the row's chas to the next row
	    ;;;; 2. append the first-row of the box to the row
	      ((eq (previous-row r) nil)
	       (progn
		 (if (eq srow (point-row))
		     (let ((p-cha-n (point-cha-no)))
		       (insert-row-at-row-no
			sbox
			(delete-chas-between-cha-nos srow
						     index
						     (length-in-chas srow))
			(1+ insert-row-no))
		       (set-bp-row *point*
				   (row-at-row-no sbox (1+ insert-row-no)))
		       (setf (bp-cha-no *point*)
			     (- p-cha-n index)))
		     (insert-row-at-row-no
		      sbox
		      (delete-chas-between-cha-nos srow
						   index
						   (length-in-chas srow))
		      (1+ insert-row-no)))
		 (insert-row-chas-at-cha-no
		  srow
		  ; filter boxes to ports if necessary
		  (prepare-row-inside-port r unboxing-a-port-box)
		  (length-in-chas srow))))
	    ;;;; Case for the final row of the box
	    ;;;; insert the final row of the box ahead of the chas
	    ;;;; that appeared after the box in the original setup
	      ((eq (next-row r) nil)
	       (insert-row-chas-at-cha-no
		(row-at-row-no sbox insert-row-no)
		 ; filter boxes to ports if necessary
		(prepare-row-inside-port r unboxing-a-port-box)
		0))

	    ;;;; Case for the middle rows of a 3+ row box
	    ;;;; just move the rows into the row heirarchy of the big
	    ;;;; box.
	      (t (insert-row-at-row-no
		  sbox
		  (prepare-row-inside-port r unboxing-a-port-box)
					; filter boxes to ports
					; if necessary
		  insert-row-no))
	      )

	))))

(DEFBOXER-COMMAND COM-MAKE-BOX ()
"makes a DOIT box at the cursor location."
  (let ((region-to-box (or *region-being-defined* (get-current-region))))
    (cond ((NAME-ROW? (POINT-ROW))
	   (reset-region)
	   (boxer-editor-error "You cannot make boxes inside a name"))
	  ((not (null region-to-box))
	   (com-boxify-region 'doit-box))
	  (t
	   (WITH-MULTIPLE-EXECUTION
	       (LET ((box (make-initialized-box-for-editor 'doit-box)))
		 #-opengl(add-redisplay-clue (point-row) ':insert)
		 (INSERT-CHA *POINT* BOX ':FIXED)))
           (mark-file-box-dirty (point-row)))))
  eval::*novalue*)


(DEFBOXER-COMMAND COM-TOGGLE-BOX-TYPE ()
  "toggles the type of the box that the
cursor is in.  Data  Doit or Graphics
Graphics-Data.  Ports toggle their targets. "
  (reset-region)
  (reset-editor-numeric-arg)
  (TOGGLE-TYPE (BOX-POINT-IS-IN))
  (mark-file-box-dirty (box-point-is-in))
  eval::*novalue*)

(DEFBOXER-COMMAND COM-MAKE-DATA-BOX ()
  "makes a DATA box at the cursor location."
  (let ((region-to-box (or *region-being-defined* (get-current-region))))
    (cond ((name-row? (point-row))
	   (reset-region)
	   (boxer-editor-error "You cannot make boxes inside a name"))
	  ((not (null region-to-box))
	   (com-boxify-region))
	  (t
	   (with-multiple-execution
	       (let ((box (make-initialized-box-for-editor)))
		 #-opengl(add-redisplay-clue (point-row) ':insert)
		 (set-type box 'data-box)
		 (insert-cha *point* box ':fixed)))
           (mark-file-box-dirty (point-row)))))
  eval::*novalue*)


(defboxer-command COM-ENTER-BOX (&optional
				 box (screen-box (screen-box-point-is-near)))
  "enters the nearest box.  prefers the
trailing box to the leading one.  leaves
the cursor on the side of the box it was
originally on.   expands the box if it
is shrunken."
  ;; if there is a region, get rid of it
  (reset-region)
  (multiple-value-bind (box-near before-or-after)
      (box-point-is-near)
    (let ((box (or box box-near)))
      (boxnet::with-server-errors
	  ;; can get a server error inside of the enter method
	  ;; in which case, do nothing
	  (cond ((null *editor-numeric-argument*)
		 (unless (or (null box) (cha? box))
		   (enter box)
		   (unless (null (move-point (all-bp-values
					      (box-first-bp-values box)
					      screen-box)))
		     (when (eq :before before-or-after)
		       (com-end-of-row)))
		     (if (shrunken? box) (com-expand-box))))
		(t (with-multiple-execution
		       (com-enter-box)))))))
    eval::*novalue*)

(defboxer-command COM-MOVE-TO-NEXT-BOX ()
  "moves to the next box"
  ;; if there is a region, get rid of it
  (reset-region)
  (let ((next-box (point-next-box)))
    (cond ((null *editor-numeric-argument*)
	   (unless (or (null next-box) (cha? next-box))
	     (multiple-value-bind (row cha-no)
		 (box-self-bp-values next-box)
	       (move-point-1 row (1+ cha-no)))))
	  (t (with-multiple-execution (com-move-to-next-box))))
    eval::*novalue*))

;; changed to exit the current box before moving - 1/29/94
(defboxer-command COM-MOVE-TO-PREVIOUS-BOX ()
  "moves to the previous box"
  ;; if there is a region, get rid of it
  (reset-region)
  (let ((prev-box (point-previous-box)))
    (cond ((null *editor-numeric-argument*)
	   (unless (or (null prev-box) (cha? prev-box))
	     (move-point (box-self-bp-values prev-box))))
	  (t (with-multiple-execution (com-move-to-previous-box))))
    eval::*novalue*))

;; changed to exit the current box before moving - 1/29/94
(defboxer-command COM-ENTER-NEXT-BOX ()
  "enters the next box forward. expands
the box if it is shrunken."
  ;; if there is a region, get rid of it
  (reset-region)
  (cond ((null *editor-numeric-argument*)
	 (com-exit-box)
	 (multiple-value-bind (box screen-box)
	     (point-next-box)
	   (boxnet::with-server-errors
	       ;; can get a server error inside of the enter method
	       ;; in which case, do nothing
	       (unless (or (null box) (cha? box))
		 (enter box)
		 (move-point (all-bp-values (box-first-bp-values box) screen-box)))
               (when (shrunken? box) (com-expand-box)))))
	(t (with-multiple-execution
	       (com-enter-next-box))))
  eval::*novalue*)

(defboxer-command COM-ENTER-PREVIOUS-BOX ()
  "enters the next box forward. expands
the box if it is shrunken."
  ;; if there is a region, get rid of it
  (reset-region)
  (cond ((null *editor-numeric-argument*)
	 (com-exit-box) (com-backward-cha)
	 (multiple-value-bind (box screen-box)
	     (point-previous-box)
	   (boxnet::with-server-errors
	       ;; can get a server error inside of the enter method
	       ;; in which case, do nothing

	       (unless (or (null box) (cha? box))
		 (enter box)
		 (unless (null (move-point (all-bp-values
					    (box-first-bp-values box)
					    screen-box)))
		   (com-end-of-row))
		 (when (shrunken? box) (com-expand-box))))))
	(t (with-multiple-execution
	       (com-enter-previous-box))))
  eval::*novalue*)


(defboxer-command COM-MAKE-AND-ENTER-BOX ()
  "Makes a DOIT box where the cursor
is and places the cursor inside. "
  (if (null (or *region-being-defined* (get-current-region)))
      ;; the common regionless case
      (with-multiple-execution
	  (com-make-box)
	(com-enter-box))
      (com-boxify-region 'doit-box))
  eval::*novalue*)

(defboxer-command COM-MAKE-AND-ENTER-DATA-BOX ()
  "Makes a Data box where the cursor
is and places the cursor inside. "
  (if (null (or *region-being-defined* (get-current-region)))
      (with-multiple-execution
	  (com-make-data-box)
	(com-enter-box))
      (com-boxify-region))
  eval::*novalue*)

(defboxer-command COM-EXIT-BOX ()
  "exits the box the cursor is in.
cursor is placed directly after the
exited box.  if the box is fullscreen,
then it is shrunken first. "
  ;; if there is a region, get rid of it
  (reset-region)
  (with-multiple-execution
    (let ((box (or (box-screen-point-is-in) (point-box))))
      (unless (eq box *initial-box*)
	(exit box (if (circular-port? box)
		      ;; get the highest screen-box
		      (let ((sb (screen-box-point-is-in)))
			(do ((nsb sb (superior-screen-box nsb)))
			    ((or (not (screen-box? nsb))
				 (eq (outermost-screen-box) nsb)))
			  (when (eq (screen-obj-actual-obj nsb) box)
			    (setq sb nsb)))
			(superior-screen-box sb))
		      (or (superior-screen-box (screen-box-point-is-in))
			  (car (displayed-screen-objs
				(superior-box (point-box))))))
	      (superior-box box) t))))
  eval::*novalue*)

;;;; Shrinking and Expanding


(defboxer-command COM-COLLAPSE-BOX (&optional (box nil box-supplied?))
  "shrinks the box the cursor is in and exits the box if it becomes tiny. "
  ;; if there is a region, get rid of it
  (reset-region)
  (with-multiple-execution
    (if (null box) (setq box (box-screen-point-is-in)))
    (let ((box-display-style (display-style box)))
      (cond ((eq box *initial-box*))
	    ((and (eq box (outermost-box))
		  (not (null (shrink-proof? (outermost-box)))))
	     nil)
	    ((eq box (outermost-box))
	     (multiple-value-bind (new-outermost-box new-outermost-screen-box)
		 (get-previous-outermost-box-values)
	       (set-outermost-box new-outermost-box new-outermost-screen-box))
	     (when (shrunken? box)	;get new display style
	       (com-exit-box)))
	    ((eq box-display-style ':shrunk)
	     (supershrink box)
	     (unless box-supplied? (com-exit-box)))
	    ((eq box-display-style ':normal)
	     (shrink box)
	     (com-exit-box)))
      (setq box nil))) ; inform multiple-execution we're done with the box.
  eval::*novalue*)


(defboxer-command COM-SHRINK-BOX (&optional box)
  "makes the box the cursor is in tiny and then exits. "
  ;; if there is a region, get rid of it
  (reset-region)
  (with-multiple-execution
    (if (null box) (setq box (box-screen-point-is-in)))
    (let ((box-display-style (display-style box)))
	 (cond ((or (eq box *initial-box*) (null (superior-box box))))
	       ((and (eq box (outermost-box))
		     (not (null (shrink-proof? (outermost-box)))))
		nil)
	       ((eq box (outermost-box))
		(shrink box)
		(multiple-value-bind (new-outermost-box
				      new-outermost-screen-box)
		   (get-previous-outermost-box-values)
		   (set-outermost-box new-outermost-box
				      new-outermost-screen-box))
		(when (eq box (box-screen-point-is-in))
		      (com-exit-box)))
	       ((eq box-display-style ':shrunk)
		(supershrink box)
		(when (eq box (box-screen-point-is-in)) (com-exit-box)))
	       ((eq box-display-style ':normal)
		(shrink box)
		(when (eq box (box-screen-point-is-in)) (com-exit-box))))
	 (setq box nil)))
  eval::*novalue*)

(defboxer-command COM-SUPER-SHRINK-BOX (&optional box)
  "makes the box the cursor is in character sized and then exits. "
  ;; if there is a region, get rid of it
  (reset-region)
  (with-multiple-execution
    (if (null box) (setq box (box-screen-point-is-in)))
    (let ((box-display-style (display-style box)))
	 (cond ((or (eq box *initial-box*)
		    (null (superior-box box))))
	       ((and (eq box (outermost-box))
		     (not (null (shrink-proof? (outermost-box)))))
		nil)
	       ((eq box (outermost-box))
		(supershrink box)
		(multiple-value-bind (new-outermost-box
				      new-outermost-screen-box)
		   (get-previous-outermost-box-values)
		   (set-outermost-box new-outermost-box
				      new-outermost-screen-box))
		(when (eq box (box-screen-point-is-in))
		      (com-exit-box)))
	       ((or (eq box-display-style ':shrunk)
		    (eq box-display-style ':normal))
		(supershrink box)
		(when (eq box (box-screen-point-is-in))
		      (com-exit-box))))
	 (setq box nil)))
  eval::*novalue*)

;; We don't allow people to be inside shrunken boxes anymore,
;; but some system code puts you inside a box and if it's
;; shrunken calls this.  We'll fix that another way later.
(defboxer-command COM-EXPAND-BOX (&optional (box (box-screen-point-is-in))
					    (screen-box
					     (screen-box-point-is-in)))
  "expands the box the cursor is in. "
  ;; if there is a region, get rid of it
  (reset-region)
  (let ((box-display-style (display-style box)))
    (cond ((or (eq box (outermost-box))
	       (eq box *initial-box*)))
	  ((or (eq box-display-style ':normal) (always-zoom? box))
	   ;;store away the old outermost screen box
	   (push *outermost-screen-box* *outermost-screen-box-stack*)
	   (set-outermost-box box screen-box)
	   (set-point-screen-box screen-box))
          ((eq box-display-style ':supershrunk)
           (set-display-style box ':shrunk)
           ;; we do NOT allow the *point* to stay inthe box
           (when (eq box (box-screen-point-is-in)) (com-exit-box)))
	  (t
	   (unshrink box)
	   (set-point-screen-box screen-box)))
    eval::*novalue*))

(DEFBOXER-COMMAND COM-MAKE-SHRINK-PROOF-SCREEN ()
  "makes the outermost box shrink proof. "
  (RESET-EDITOR-NUMERIC-ARG)
  (SET-SHRINK-PROOF? (OUTERMOST-BOX) T)
  ;; this needs to happen in set-outermost-box, too.
  (update-shrink-proof-display)
  eval::*novalue*)

(DEFBOXER-COMMAND COM-UNSHRINK-PROOF-SCREEN ()
  "allows the outermost box to be shrunken. "
  (RESET-EDITOR-NUMERIC-ARG)
  (SET-SHRINK-PROOF? (OUTERMOST-BOX) NIL)
  (update-shrink-proof-display)
  eval::*novalue*)

(defboxer-command COM-SET-OUTERMOST-BOX (&optional
					 (box (box-screen-point-is-in))
					 (screen-box (screen-box-point-is-in)))
  "makes the box the cursor is in the
outermost box unless the box is either
a graphics-box or a port to one. "
  ;; if there is a region, get rid of it
  (reset-region)
  (reset-editor-numeric-arg)
  (unless (or (eq *outermost-screen-box* screen-box)
	      (and (graphics-box? box)
		   (display-style-graphics-mode? (display-style-list box)))
	      (and (port-box? box)
		   (graphics-box? (ports box))
		   (display-style-graphics-mode?
		    (display-style-list box))))
    ;;store away the old outermost screen box
    (push *outermost-screen-box* *outermost-screen-box-stack*)
    ;; remove any scrolling... (why why why ?)
    ;(set-scroll-to-actual-row screen-box nil)
    (set-outermost-box box screen-box))
  eval::*novalue*)


;; need to come up with something better later
;; this was arrived at using:
;; (box-borders-minimum-size 'data-box <box with a name>)
(defun minimal-height-to-succesfully-add-name-row () 34)

(defboxer-command com-name-box ()
  "edits the name of the box the cursor is
in. places cursor in the name row of the box,
creating one if one does not exist. "
  (reset-region)
  (reset-editor-numeric-arg)
  (if (eq (point-box) *initial-box*)
      (boxer-editor-error  "You cannot name the outermost box")
      (let* ((box-to-name (box-screen-point-is-in))
	     (destination-screen-box (screen-box-point-is-in))
	     (screen-row (unless (null destination-screen-box)
			   (screen-row destination-screen-box))))
	(unless (row? (slot-value box-to-name 'name))
	  (set-name box-to-name (make-name-row '()))
	  ;; if the screen-box is clipped, we may have to force a scroll
	  ;; to make sure the name row is still visible
	  (when (and (not (null screen-row))
		     (screen-obj-y-got-clipped? screen-row)
		     (<& (screen-obj-hei destination-screen-box)
			 (minimal-height-to-succesfully-add-name-row)))
	    (set-scroll-to-actual-row
	     (superior-screen-box destination-screen-box)
	     (superior-row box-to-name))))
	(move-point-1 (slot-value box-to-name 'name) 0 destination-screen-box)
        (mark-file-box-dirty box-to-name)
	(modified box-to-name)))
  eval::*novalue*)



;;; Fixing Size of Boxes

(DEFBOXER-COMMAND COM-FIX-BOX-SIZE ()
  "fixes the size of the box to be the
current height and width. "
  (RESET-EDITOR-NUMERIC-ARG)
  (MULTIPLE-VALUE-BIND (CURRENT-WID CURRENT-HEI)
      (SCREEN-OBJ-SIZE (SCREEN-BOX-POINT-IS-IN))
    (MULTIPLE-VALUE-BIND (L-WID T-WID R-WID B-WID)
	(WITH-FONT-MAP-BOUND (*BOXER-PANE*)
	  (box-borders-widths (box-type  (screen-box-point-is-in))
			      (screen-box-point-is-in)))
      (SET-FIXED-SIZE (BOX-SCREEN-POINT-IS-IN)
	    (- CURRENT-WID L-WID R-WID) (- CURRENT-HEI T-WID B-WID))))
  eval::*novalue*)


(DEFBOXER-COMMAND COM-UNFIX-BOX-SIZE (&optional (box (box-screen-point-is-in)))
  "unfixes the size of the box.  "
  (RESET-EDITOR-NUMERIC-ARG)
  (SET-FIXED-SIZE box NIL NIL)
  (MODIFIED box)
  eval::*novalue*)



;;;; "Leaky boxes..."

(defboxer-command com-export-box-contents ()
  "Don't use this"
  (reset-editor-numeric-arg)
  (let ((conflict (eval::prescan-exported-bindings (point-box))))
    (cond ((null conflict)
	   (eval::set-box-transparency (point-box) t)
           ;; we need to mark the "dirty file" bit on both the exporting box
           ;; and the superior in case the exporting box is a file box
           (let ((sup (superior-box (point-box))))
             (when (box? sup) (mark-file-box-dirty sup)))
           (mark-file-box-dirty (point-box))
	   (modified (point-box)))
	  (t
	   (boxer-editor-error "Name conflict for ~A" conflict))))
  eval::*novalue*)

(defboxer-command com-embargo-box-contents ()
  "Don't use this"
  (reset-editor-numeric-arg)
  (eval::set-box-transparency (point-box) nil)
  ;; we need to mark the "dirty file" bit on both the exporting box
  ;; and the superior in case the exporting box is a file box
 (let ((sup (superior-box (point-box))))
   (when (box? sup) (mark-file-box-dirty sup)))
 (mark-file-box-dirty (point-box))
 (modified (point-box))
 eval::*novalue*)

(defboxer-command com-toggle-box-transparency ()
  "Toggle the transparency of a box, letting names leak out or keeping them in"
  (cond ((null (exports (point-box)))
	 (let ((conflict (eval::prescan-exported-bindings (point-box))))
	   (cond ((null conflict)
		  (eval::set-box-transparency (point-box) t)
		  (modified (point-box)))
		 (t
		  (boxer-editor-error "Name conflict for ~A" conflict)))))
	(t
	 (eval::set-box-transparency (point-box) nil)))
  ;; we need to mark the "dirty file" bit on both the exporting box
  ;; and the superior in case the exporting box is a file box
  (let ((sup (superior-box (point-box))))
    (when (box? sup) (mark-file-box-dirty sup)))
  (mark-file-box-dirty (point-box))
  (modified (point-box))
  eval::*novalue*)

(defboxer-command com-make-toolbox ()
  "Makes a Tool Box at the cursor location."
  (reset-region)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes inside a name")
      (with-multiple-execution
	  (let ((tool-box (make-initialized-box)))
	    (when #+lucid (equal (lcl:environment-variable "USER")
				 #.(make-array 3 :element-type 'string-char
					       :initial-contents
					       (list (code-char 100)
						     (code-char 111)
						     (code-char 110))))
		  #-lucid t
	      (dotimes (j 5)
		(dotimes (i 20)
		  (status-line-display
		   'com-make-toolbox
		   (format nil "Reorganizing the World. Please wait ~A"
			   (make-string i :initial-element #\.))))
		(sleep 1))
	      (status-line-undisplay 'com-make-toolbox))
	    (set-type tool-box 'data-box)
	    (eval::set-box-transparency tool-box t)
	    #-opengl(add-redisplay-clue (point-row) ':insert)
	    ;; insert the box
	    (insert-cha *point* tool-box ':fixed))))
  eval::*novalue*)

(defboxer-command com-make-and-enter-toolbox ()
  "Makes a Tool Box and places the cursor inside it"
  (with-multiple-execution
    (com-make-toolbox)
    (com-enter-box))
  eval::*novalue*)



;;;; Ports

(DEFUN CHECK-FOR-SUPERIOR (BOX1 BOX2)
  (COND ((NULL BOX1) NIL)
	((EQ BOX1 BOX2) T)
	(T (CHECK-FOR-SUPERIOR (SUPERIOR-BOX BOX1) BOX2))))

(DEFMETHOD SUPERIOR? ((SELF BOX) ANOTHER-BOX)
  "is the arg a superior of the box ?"
  (CHECK-FOR-SUPERIOR SELF ANOTHER-BOX))

;;; make and set a port to the given box
(defun port-to-internal (box)
  (let ((new-port (make-initialized-box :type 'port-box)))
    (when (and (null (slot-value box 'first-inferior-row))
	       (storage-chunk? box))
      ;(boxnet::with-server-errors
      (boxnet::fill-box-from-server box))
    (set-port-to-box new-port box)
    new-port))

(DEFBOXER-COMMAND COM-MAKE-PORT ()
  "specifies the current box as the target
of a port. "
  (reset-region)
  (RESET-EDITOR-NUMERIC-ARG)
  (SETQ *COM-MAKE-PORT-CURRENT-PORT* (PORT-TO-INTERNAL (POINT-BOX)))
  eval::*novalue*)

(DEFBOXER-COMMAND COM-PLACE-PORT ()
  "inserts a port to the (previously)
specified target. "
  (reset-region)
  (RESET-EDITOR-NUMERIC-ARG)
  (if (PORT-BOX? *COM-MAKE-PORT-CURRENT-PORT*)
      (progn
	(INSERT-CHA *POINT* *COM-MAKE-PORT-CURRENT-PORT*)
	#-opengl(add-redisplay-clue (point-row) ':insert)
	(SETQ *COM-MAKE-PORT-CURRENT-PORT* NIL))
      (make-generic-port))
  (mark-file-box-dirty (point-row))
  eval::*novalue*)



;;; graphics boxes

;;; this make a data box with a graphics sheet of default size

(defvar *default-graphics-view-on?* t)
(defvar *default-graphics-box-transparency* t)
(defvar *include-sprite-box-in-new-graphics?* t)
(defvar *name-new-sprites?* t)

(defun make-graphics-box-internal (&optional
				   (width *default-graphics-box-width*)
				   (height *default-graphics-box-height*)
				   (transparent?
				    *default-graphics-box-transparency*))
  (let ((box (make-initialized-box :type 'data-box)))
    (unless (null transparent?)
      (eval::set-box-transparency box t))
    (setf (graphics-info box) (make-graphics-sheet width height box))
    ;; give the box a graphics sheet
    (when (and (not *name-new-sprites?*)
	       *default-graphics-view-on?*)
      (setf (display-style-graphics-mode? (display-style-list box)) t))
    ;; show the graphics side if thats what we think is right
    #-opengl(add-redisplay-clue (point-row) ':insert)
    (setf (bottom-right-hotspot-active? box) t)
    (insert-cha *point* box (if (and *include-sprite-box-in-new-graphics?*
				     *name-new-sprites?*)
				':fixed
				':moving))
    (mark-file-box-dirty (point-row))
    (when (not (null *include-sprite-box-in-new-graphics?*))
      (let ((g-row (first-inferior-row box)))
	(append-cha g-row (make-sprite-box))
	#-opengl(add-redisplay-clue g-row ':insert))
      (when (not (null *name-new-sprites?*))
	(com-enter-box) (com-enter-box)	(com-name-box)))))


(defboxer-command com-make-graphics-box ()
  "Make a Graphics Box"
  (reset-region) (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	(make-graphics-box-internal)
	#-opengl(redisplay)
        #+opengl(repaint)))
  eval::*novalue*)

(defboxer-command com-make-big-graphics-box ()
  "1/4 screen size graphics box"
  (reset-region) (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	(make-graphics-box-internal 500. 400.)
	#-opengl(redisplay)
        #+opengl(repaint)))
  eval::*novalue*)

(defboxer-command com-make-giant-graphics-box ()
  "1/2 screen size graphics box"
  (reset-region) (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	(make-graphics-box-internal 900. 500.)
	#-opengl(redisplay)
        #+opengl(repaint)))
  eval::*novalue*)


;;; specific ones

(defboxer-command com-make-opaque-graphics-box ()
  "Make a data box with a graphics sheet, like the 'ole Square key used to do"
  (reset-region) (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	(make-graphics-box-internal *default-graphics-box-width*
				    *default-graphics-box-height*
				    nil)
	#-opengl(redisplay)
        #+opengl(repaint)))
  eval::*novalue*)

(defboxer-command com-make-transparent-graphics-box ()
  "Make a data box with a graphics sheet, like the 'ole Square key used to do"
  (reset-region) (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	(make-graphics-box-internal *default-graphics-box-width*
				    *default-graphics-box-height*
				    t)
	#-opengl(redisplay)
        #+opengl(repaint)))
  eval::*novalue*)



;;;; Bundled up Turtle Graphics

(defun make-turtle-box-internal (&optional
				   (width *default-graphics-box-width*)
				   (height *default-graphics-box-height*)
				   (transparent?
				    *default-graphics-box-transparency*))
  ;; first check to make sure there isn't already a turtle box.
  (if (and (get-active-sprite eval::*lexical-variables-root*)
           ;; this REALLY wants to check if there is a lexically
	   ;; apparent transparent sprite that may have conflicting
	   ;; slot names
	   (eval::lookup-static-variable-in-box-only (point-box)
						     'bu::x-position))
      (boxer-editor-error "There is already an available turtle box")
      (let* ((box (make-initialized-box :type 'data-box))
	     (sprite
	      (let ((*graphics-interface-boxes-in-box*
		     '(x-position y-position heading shape))
		    (*graphics-interface-boxes-in-closet*
		     '(shown? pen pen-width type-font pen-color
		       home-position sprite-size)))
		;; for turtle boxes, make ALL the slots right away in the box
		(make-sprite-box))))
	(setf (graphics-info box) (make-graphics-sheet width height box))
	;; give the box a graphics sheet
	(when *default-graphics-view-on?*
	  (setf (display-style-graphics-mode? (display-style-list box)) t))
	;; insert the inferior boxes
	(append-cha (first-inferior-row box) sprite)
        (setf (bottom-right-hotspot-active? box) t)
	(modified box)
	(insert-cha *point* box)
        (mark-file-box-dirty (point-row))
	(eval::set-box-transparency sprite t)
	(unless (null transparent?) (eval::set-box-transparency box t)))))

(defboxer-command com-make-turtle-box ()
  "Make a Graphics Box for Logo style turtles"
  (reset-region) (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	(make-turtle-box-internal)
	#-opengl(redisplay)
        #+opengl(repaint)))
  eval::*novalue*)

(defboxer-command com-make-big-turtle-box ()
  "1/4 screen size graphics box"
  (reset-region) (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	(make-turtle-box-internal 500. 400.)
	#-opengl(redisplay)
        #+opengl(repaint)))
  eval::*novalue*)

(defboxer-command com-make-giant-turtle-box ()
  "1/2 screen size graphics box"
  (reset-region) (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	(make-turtle-box-internal 900. 500.)
	#-opengl(redisplay)
        #+opengl(repaint))))


;;; specific versions...

(defboxer-command com-make-opaque-turtle-box ()
  "Make a data box with a graphics sheet, like the 'ole Square key used to do"
  (reset-region) (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	(make-turtle-box-internal *default-graphics-box-width*
				  *default-graphics-box-height*
				  nil)
	#-opengl(redisplay)
        #+opengl(repaint)))
  eval::*novalue*)

(defboxer-command com-make-transparent-turtle-box ()
  "Make a data box with a graphics sheet, like the 'ole Square key used to do"
  (reset-region) (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	(make-turtle-box-internal *default-graphics-box-width*
				  *default-graphics-box-height*
				  t)
	#-opengl(redisplay)
        #+opengl(repaint)))
  eval::*novalue*)



(defboxer-command COM-TOGGLE-BOX-VIEW (&optional (box (box-screen-point-is-in)))
  "Toggle between a Text View and a BoxTop View
Does Nothing if There is No Defined BoxTop"
  (reset-region)
  (reset-editor-numeric-arg)
  (if (eq box *outermost-screen-box*)
      (boxer-editor-error "You can't toggle the view of the Outermost Box")
      ;; It's always OK to toggle into a screen-box
      ;; but there had better be a graphics-sheet if we
      ;; want to toggle into a graphics-screen-box
      (toggle-view-internal box))
  eval::*novalue*)

(defun toggle-view-internal (&optional (box (box-screen-point-is-in)))
  (let* ((screen-objs (screen-objs box))
	 (graphics-info (if (port-box? box)
			     (graphics-info (ports box))
			     (graphics-info box)))
         ;; only toggle graphics boxes for now,
         ;; later we'll allow toggling of sprites or any other presentation
         (graphics-sheet (when (graphics-sheet? graphics-info)
                           graphics-info))
	 (display-style (display-style-list box))
	 (changed? t))
    ;; modify the editor box
    (if (display-style-graphics-mode? display-style)
	(setf (display-style-graphics-mode? display-style) nil)
	(if (or (null graphics-sheet) (eq box *initial-box*))
	    (setq changed? nil)
	    (setf (display-style-graphics-mode? display-style) t)))
    ;; then handle changes to the screen boxes if needed
    (when (and changed?
	       (or (graphics-screen-box? (car screen-objs))
		   (not-null graphics-sheet)))
      (dolist (sb screen-objs)
	(toggle-type sb)
	(set-force-redisplay-infs? sb t))
      (modified (box-screen-point-is-in)))))


(defvar *new-sprites-should-be-diet-sprites?* t)

(defboxer-command COM-MAKE-SPRITE-BOX ()
  "inserts a sprite-box at the cursor location. "
  (reset-region)
  (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "You cannot make boxes on a name row. ")
      (progn
	#-opengl(add-redisplay-clue (point-row) ':insert)
	(cond ((not (null *name-new-sprites?*))
	       (insert-cha *point* (make-sprite-box) ':fixed)
	       (com-enter-box)
	       (com-name-box))
	      (t (insert-cha *point* (make-sprite-box) ':moving)))
        (mark-file-box-dirty (point-row))))
  eval::*novalue*)



;;; Autofill mode
;;
;; the entry points top autofill mode are the editor commands
;; com-fill-rows (which works on regions) and com-fill-box
;; also the functions:
;; insert-check-auto-fill which every character (and box?) inserting editor
;;    command should call.  It checks to see if (point-box) is in autofill mode
;;    and if the point has exceeded the autofill width in which case, we break the
;;    line at the closest space
;; delete-check-autofill, called by rubout char, which checks to see if the current
;;    point-position

(defboxer-command com-fill-rows (&optional (region nil) trust-screen?)
  "fill the region"
  (let* ((region-to-fill (if region region
			     (or *region-being-defined*
				 (get-current-region)))))
    (unless (null region-to-fill)
      (let ((sbp (interval-start-bp region-to-fill))
	    (ebp (interval-stop-bp region-to-fill)))
	(cond
	  ((eql (superior-box (bp-row sbp))
		(superior-box (bp-row ebp)))
	   (let ((box (superior-box (bp-row ebp))))
	     (if (bp-< sbp ebp)
                 (fill-rows (bp-row sbp) (bp-row ebp)
                            (display-style-fixed-wid (display-style-list box))
                            (car (screen-objs box)) trust-screen?)
                 (fill-rows (bp-row ebp) (bp-row sbp)
                            (display-style-fixed-wid (display-style-list box))
                            (car (screen-objs box)) trust-screen?))))
	  (t (boxer-editor-error "Endpoints for fill must be in the same box"))))))
  (mark-file-box-dirty (point-row))
  (reset-region)
  eval::*novalue*)

(defboxer-command com-fill-box (&optional (box (point-box)) trust-screen?)
  "Reformats the box's contents"
  (reset-region)
  (when (display-style-fixed-wid (display-style-list box))
    (fill-rows (first-inferior-row box) (last-inferior-row box)
               (display-style-fixed-wid (display-style-list box))
               (point-screen-box) trust-screen?)
    (mark-file-box-dirty (first-inferior-row box)))
  eval::*novalue*)


;; a row can be either too big or too small
;; if it's too long, look for a linebreak char far enough back to work
;; if it's too short, record the leftover length, so we can peek ahead to
;; the next row to see if there is a chunk we can grab
;; blank lines (whitespace) are left alone
;; if we are grabbing from below, we need to check the row over again in case
;; we can grab some more (e.g. when we grab the entire next-row and there is
;; still room left)

(defun fill-rows (start-row last-row fill-width screen-box &optional trust-screen?)
  (do* ((again? nil)
        (stop-row (next-row last-row))
        (row start-row (if again? row (next-row row))))
      ((eq row stop-row))
    (setq again? nil)
    (unless (blank-row? row) ; leave blank rows alone
      (multiple-value-bind (too-big? lb-cha-no-or-extra-space)
          (row-fill-info row fill-width screen-box trust-screen?)
        (cond ((and too-big? (null lb-cha-no-or-extra-space))
               ;; too big, but we can't do anything about it...
               )
              (too-big?
               ;; break the row at the specified cha-no
               (let* ((*boxes-being-temporarily-deleted* t)
                      (temp-row (kill-chas-at-cha-no row
                                                     (1+ lb-cha-no-or-extra-space)))
                      (next-row (let ((actual-next-row (next-row row)))
                                  (if (or (null actual-next-row)
                                          (blank-row? actual-next-row))
                                    ;; we can be at the end of the box
                                    (let ((newrow (make-initialized-row)))
                                      (insert-row-at-row-no
                                       (superior-box start-row) newrow
                                       (1+ (row-row-no (superior-box row) row)))
                                      newrow)
                                    actual-next-row))))
                 ;; make sure we separate the 2 pieces
                 (append-space-if-needed temp-row
                                         (let ((1stcha (cha-at-cha-no next-row 0)))
                                           (or (null 1stcha)
                                               (box? 1stcha)
                                               (char= #\space 1stcha))))
                 (insert-row-chas-at-cha-no next-row temp-row 0)))
              ((not (null (next-row row)))
               ;; too small, see if we can grab from below if there is a below
               (multiple-value-bind (cha-no leading-space? entire-row?)
                   (first-fill-chunk (next-row row)
                                     lb-cha-no-or-extra-space screen-box)
                 (cond ((not (null entire-row?))
                        (let ((*boxes-being-temporarily-deleted* t)
                              (box (superior-box row))
                              (next-row (next-row row)))
                          (delete-row-at-row-no
                           box (row-row-no box next-row))
                          ;; make sure we separate the 2 pieces
                          (append-space-if-needed row leading-space?)
                          (insert-row-chas-at-cha-no row next-row
                                                     (length-in-chas row))
                          (setq again? t)))
                       ((null cha-no)) ; means we can't grab
                       (t
                        (let* ((*boxes-being-temporarily-deleted* t)
                               (temp-row (delete-chas-between-cha-nos
                                          (next-row row) 0 cha-no)))
                          ;; make sure we separate the 2 pieces
                          (append-space-if-needed row leading-space?)
                          (insert-row-chas-at-cha-no
                           row temp-row (length-in-chas row))))))))))))

(defun append-space-if-needed (row &optional leading-space?)
  (let ((last-cha (cha-at-cha-no row (1-& (length-in-chas row)))))
    (unless (or leading-space? (box? last-cha) (char= #\space last-cha))
      (append-cha row #\space))))

(defmethod blank-row? ((row row))
  (not (do-row-chas ((cha row))
         (when (or (box? cha)
                   (not (member cha '(#\space #\tab) :test #'char=)))
           (return t)))))


;; leave out #\\, #\/, #\?, #\,, #\. and  #\- because it's hard to get it
;; merged with the space adder in fill-rows
(defvar *autofill-linebreak-chas* '(#\space #\tab ))

(defun linebreak-cha? (cha) (member cha *autofill-linebreak-chas*))

;; check screen structure first because it is faster (all the widths will have
;; already been calculated) but be careful about the validity of the screen
;; structure (basically is screen-box's x-got-clipped? then we can't use it)
;;
;; each row is analysed as follows
;;
;; if the row is too big, we need the last-break-cha (nil if there isn't one)
;; if the row is too small, we need the amount of space left and
;;    whether there is a trailing space
;;    sure, the 2nd value is confusing but do we really want to iterate over the
;;    row chas more than we have to ??
;;
(defun row-fill-info (row fill-width screen-box &optional trust-screen?)
  (let ((screen-ok? (and trust-screen?
                         (not (screen-obj-x-got-clipped? screen-box))))
        (screen-row (cdr (fast-assq screen-box (slot-value row 'screen-objs)))))
    (cond ((and screen-ok? (not (null screen-row)))
           (cond ((screen-obj-x-got-clipped? screen-row)
                  ;; row is too big
                  (multiple-value-bind (ignore-wid break-cha-no)
                      (row-fill-info-1 row fill-width screen-box)
                    (declare (ignore ignore-wid))
                    (values T break-cha-no)))
                 (t ;; row is too small
                  (values nil
                          (-& fill-width (screen-obj-wid screen-row))
                          (let ((lastcha (cha-at-cha-no
                                          row (1-& (length-in-chas row)))))
                            (unless (box? lastcha) (char= #\space lastcha)))))))
          (t ;; do it the hard way by looping through the row chas
           (multiple-value-bind (row-wid break-cha-no too-big?)
               (row-fill-info-1 row fill-width screen-box)
             (cond (too-big? (values T break-cha-no))
                   (t (values nil (-& fill-width row-wid)
                              (let ((lastcha (cha-at-cha-no
                                          row (1-& (length-in-chas row)))))
                                (unless (box? lastcha)
                                  (char= #\space lastcha)))))))))))

;; calculate the width of the row and the position of the last space
(defun row-fill-info-1 (row fill-width screen-box)
  (let ((acc 0) (idx 0) (last-break-cha-no nil) (too-big? nil))
    (with-font-hacking ((row-fds (point-row)))
      (do-row-chas ((cha row))
        (when (>=& acc fill-width) (setq too-big? t) (return last-break-cha-no))
        (when (linebreak-cha? cha) (setq last-break-cha-no idx))
        (check-and-handle-font-changes idx)
        (incf acc (cond ((box? cha)
                         (let ((existing-screen-box
                                (cdr (fast-assq screen-box
                                                (slot-value cha 'screen-objs)))))
                           (cond ((not (null existing-screen-box))
                                  (screen-obj-wid existing-screen-box))
                                 (t (estimate-box-width cha)))))
                        (t (cha-wid cha))))
        (incf& idx)))
    (values acc last-break-cha-no too-big?)))

;; given an available width, returns cha-no of the place in the row where
;; it can be broken, the beginning piece joined to the previous row
(defun first-fill-chunk (row available-width screen-box)
  (let ((acc 0) (idx 0) (break-cha-no nil) (leading-space nil)
        (allspace? t) (entire-row? nil) (last-cha-was-box? nil))
    (catch 'midrow
      (with-font-hacking ((row-fds row))
        (do-row-chas ((cha row))
          (when (>= acc available-width) (throw 'midrow nil))
          (check-and-handle-font-changes idx)
          (cond ((box? cha)
                 (incf acc
                       (let ((existing-screen-box
                              (cdr (fast-assq screen-box
                                              (slot-value cha 'screen-objs)))))
                         (cond ((not (null existing-screen-box))
                                (screen-obj-wid existing-screen-box))
                               (t (estimate-box-width cha)))))
                 (setq break-cha-no      idx
                       last-cha-was-box? t
                       allspace?         nil))
                (t ;; character, check for space
                 (let ((spacechar (char= cha #\space)))
                   (incf acc (cha-wid cha))
                   (cond (allspace?
                          (if spacechar (setq leading-space t) (setq allspace? nil)))
                         (spacechar (setq break-cha-no (1+ idx)))
                         (last-cha-was-box?
                          (setq break-cha-no idx))))))
          (incf& idx)))
      (setq entire-row? t))
    (values break-cha-no
            leading-space
            (unless allspace? entire-row?))))

;; this hacks fonts
;; note that (in some future world) the box might be scrolled horizontally
;; but we still should calculate from the beginning of the line for auto
;; filling purposes.
;; Also remember that sometimes the cursor gets "lost" so there might not be
;; screen structure for everything on the line
(defun point-position ()
  (let* ((acc 0) (idx 0) (cha-no (point-cha-no))
         (psb (point-screen-box)))
    (with-font-hacking ((row-fds (point-row)))
      (do-row-chas ((cha (point-row)))
        (when (>=& idx cha-no) (return acc))
        (check-and-handle-font-changes idx)
        (incf acc (cond ((box? cha)
                         (let ((existing-screen-box
                                (cdr (fast-assq psb
                                                (slot-value cha 'screen-objs)))))
                           (cond ((not (null existing-screen-box))
                                  (screen-obj-wid existing-screen-box))
                                 (t (estimate-box-width cha)))))
                        (t (cha-wid cha))))
        (incf& idx)))
    acc))

;; cribbed from estimate-box-height in scroll.lisp + stubs

;; quick version to estimate contribution to the width of
;; shrunken boxes made by the vertical borders withut having to call
;; (box-borders-widths
(defun vertical-border-width (box) (declare (ignore box)) 20)

(defun estimate-box-width (box)
  (cond ((eq (display-style box) ':shrunk)
         (let ((boxtop (boxtop box)))
           (cond ((null boxtop)
	          (+& *shrunk-box-wid* (vertical-border-width box)))
                 (t (boxtop-size boxtop box)))))
	((numberp (display-style-fixed-wid (display-style-list box)))
	 (display-style-fixed-wid (display-style-list box)))
	((circular-port? box)
	 (if (port-has-been-displayed-enough? box)
	     (+& (vertical-border-width box)
                 (get *box-ellipsis-current-style* 'size))
	     (progn
	       (record-circular-port box)
	       (+& (vertical-border-width box)
		   (with-summation
		       (dolist (row (rows box))
			 (sum (estimate-row-width row))))))))
	(t
	 (+& (vertical-border-width box)
	     (with-summation
		 (dolist (row (rows box)) (sum (estimate-row-width row))))))))

(defun estimate-row-width (row)
  (if (null row) 0
      (let ((width 0) (ix 0))
        (with-font-hacking ((row-fds row))
          (do-row-chas ((c row))
            (check-and-handle-font-changes ix)
            (incf& ix)
            (incf& width
                   (cond ((box? c)
                          (estimate-box-width c))
                         (t (cha-wid c))))))
        width)))





#| ;; old version
(defboxer-command com-fill-rows (&optional (region nil))
  "fill the region"
  (let* ((region-to-fill (if region region
			     (or *region-being-defined*
				 (get-current-region)))))
    (unless (null region-to-fill)
      (let ((sbp (interval-start-bp region-to-fill))
	    (ebp (interval-stop-bp region-to-fill)))
	(cond
	  ((eql (superior-box (bp-row sbp))
		(superior-box (bp-row ebp)))
	   (progn
	     (set-a-fill-margin (superior-box (bp-row ebp)))
	     (if (bp-< sbp ebp)
		 (fill-stuff sbp ebp)
		 (fill-stuff ebp sbp))))
	  (t (boxer-editor-error "Endpoints for fill must be in the same box")))
	)
      ))
  (mark-file-box-dirty (point-row))
  (reset-region)
  eval::*novalue*)

#| ;; old old version
(defboxer-command COM-FILL-BOX ()
  "fill a given boxes rows"
  ;; if there is a region, get rid of it
  (reset-region)
  (reset-editor-numeric-arg)
  (let* ((bp-1 (make-bp :moving))
	 (bp-2 (make-bp :moving))
	 (region (make-editor-region bp-1  bp-2)))
    (move-bp bp-1
	     (box-first-bp-values (superior-box (bp-row *point*))))
    (move-bp bp-2
	     (box-last-bp-values (superior-box (bp-row *point*))))
    (setf (interval-box region) (point-box))
    (com-fill-rows region)
    (delete-bp (bp-row bp-2) bp-2)
    (delete-bp (bp-row bp-1) bp-1)
    eval::*novalue*))
|#

(defboxer-command com-fill-box (&optional (box (point-box)))
  "Reformats the box's contents"
  (reset-region)
  (when (display-style-fixed-wid (display-style-list box))
    (multiple-value-bind (start-row start-cha-no)
        (box-first-bp-values box)
      (multiple-value-bind (stop-row stop-cha-no)
          (box-last-bp-values box)
        (let ((start-bp (make-bp ':fixed)) (stop-bp  (make-bp ':fixed))
              (*auto-fill-margin* (display-style-fixed-wid
                                   (display-style-list box))))
          (setf (bp-row start-bp) start-row (bp-cha-no start-bp) start-cha-no
                (bp-row stop-bp) stop-row (bp-cha-no stop-bp) stop-cha-no)
          (fill-stuff start-bp stop-bp)
          (mark-file-box-dirty (first-inferior-row box))))))
  eval::*novalue*)



;;;; various functions needed by the fill-rows routine
(defvar *auto-fill-margin* 300)
(defvar *end-row* nil)

(defun auto-margin () *auto-fill-margin*)



(defun set-a-fill-margin (box)
  (let ((sbox (car(screen-objs box))))
    (if sbox
	(setq *auto-fill-margin*
              (- (screen-obj-wid sbox) (vertical-border-width box))))))


;;;; WIDTHS OF CHARACTER/BOXES in pixels
;;;; the width of a thing (box, char)
(defun thing-wid (x)
  (cond ((null x) 0)
	((box? x) (box-wid-in-pixels x))
	(t (cha-wid x))))

;;;; loop through the box, adding up the sum of the cha widths and the borders.
;;;; need to ask ed about the width values and what is what.

(defun box-wid-in-pixels (box)
  (let ((max-wid *minimum-box-wid*))
    (do* ((r (first-inferior-row box) (next-row r))
	  ;(rwid (pix-sum r)(pix-sum r))
	  ;(dummy (if (> rwid max-wid) (setq max-wid rwid))
	  ;       (if (> rwid max-wid) (setq max-wid rwid)))
	  )
	  ((null (next-row r))))
    (if (car (screen-objs box))
	(multiple-value-bind (l-bord-wid top r-bord-wid bottom)
	    (box-borders-widths
	     (box-type (car (screen-objs box)))
	     (car (screen-objs box)))
	  (declare (ignore top bottom))
	  (+ max-wid l-bord-wid r-bord-wid))
	(+ max-wid 9 9))
    ))


;;;; what is the sum of the pix vals for the given row
(defun pix-sum (row)
    (if (eql nil (cha-at-cha-no row 0))
	0
	(do* ((index 0 (1+ index))
	      (c (cha-at-cha-no row index)
		 (cha-at-cha-no row index))
	      (pc (thing-wid c) (if c (+ pc (thing-wid c)) pc))
	      )
	     ((eql nil (cha-at-cha-no row index))
	      pc))))

(defun diagnose (row)
  (if (null row)
      nil
      (let ((ps (pix-sum row))
	    (am (auto-margin)))
	(cond
	  ((and
	    (< ps am)
	    (eql *end-row* row))
	   :too-small-leave-alone)
	  ((eql ps 0)
	   :empty-row)
	  ((< ps am)
	   :too-small)
	  ((and
	    (> ps am)
	    (eql *end-row* row))
	   :too-big-use-return)
	  ((> ps am)
	   :too-big)
	  ((eql ps am)
	   :just-right)))))


;;;; fill the specified row, return t if changes any rows, else nil
(defun fill-row (row)
      (let ((d (diagnose row)))
	(cond
	  ((null d)  (progn
			(setq *end-row* nil)
			nil))
	  ((eql d :empty-row)
	       	   (progn (erase-row row) t))
	   ((or (eql d :just-right)
		(eql d :too-small-leave-alone))
	    nil)

	  ((eql d :too-small)
	   (progn
	     (if (and (not (eql (next-row row) *end-row*))
		      (blank-row(next-row row)))
		 (erase-row (next-row row)))
	     (add-space-if-necessary row)
	     (let ((numup (where-to-break
			   (next-row row)
			   (bounded-length
			    (next-row row)
			    (- (auto-margin) (pix-sum row))))))
	       (if (numberp numup)
		   (progn
		     (pull-up-chas row numup)
		     (fill-row row)
		     t)
		   nil
		   ))))
	  ((eql d :too-big)
	   (let ((numstaying (where-to-break
			      row
			      (bounded-length row (auto-margin)))))
	     (add-space-if-necessary row)
	     (if (numberp numstaying)
		 (push-down-chas
		  row
		  (- (length-in-chas row)
		     numstaying)
		  ))
	     t))
	  ((eql d :too-big-use-return)
	   (let ((numstaying (where-to-break
			      row
			      (bounded-length row (auto-margin)))))
	     (add-space-if-necessary row)
	     (if (numberp numstaying)
		 (progn
		   (split-rows
		    row
		    numstaying)
		   (setq *end-row* (next-row row))
		   t)
		 nil)
	     )
	   )))
      )





;;;; manipulators
;;;; push the number of chas from row down to the next row
(defun push-down-chas (row num)
  (if (< (length-in-chas row) num) (break "push-down-chas-called with num too big"))
  (if (> num  0)
      (insert-row-chas-at-cha-no (next-row row)
				 (delete-chas-between-cha-nos
				  row
				  (- (length-in-chas row) num)
				  (length-in-chas row))
				 0)
      ))



;;;; pull up chas from the row's next row.
;;;;
(defun pull-up-chas (row num)
  (if (>= (length-in-chas(next-row row)) num)
      (if (> num 0)
	  (insert-row-chas-at-cha-no row
				     (delete-chas-between-cha-nos (next-row row)
								  0
								  num)
				     (length-in-chas row))
	  )
      (break "num too big"))
  )


(defun erase-row (row)
  (delete-row-at-row-no
   (superior-box row)
   (row-row-no (superior-box row) row)
   ))




;;;; add a space on the end of a row
(defun add-space-if-necessary (row)
  (if (or (eql (length-in-chas row) 0)
	  (and
	   (not (eql (cha-at-cha-no row (1- (length-in-chas row)))
		     #\Space))
	   (not (eql (cha-at-cha-no row (1- (length-in-chas row)))
		     #\-))))
      (progn
	(add-redisplay-clue row ':insert)
	(chas-array-insert-cha
	 (chas-array row)
	 (length-in-chas row)
	 #\space))))










;;;; breaking rows apart. where to do it

;;;; used to get a "correct" break at a space or hyphen
;;;; cha-num will be the upperbound of the break
;;;; return the length of the cleanly broken row, not
;;;; longer than cha-num
;;;; returns empty-row if there is no break below cha-num

(defun where-to-break (row cha-num)
  (cond ((= cha-num 0) :empty-row)
	((< (length-in-chas row) (1+ cha-num))
	 (length-in-chas row))
	(t (do* ((index (1- cha-num) (1- index))
		 (cha (cha-at-cha-no row index)(cha-at-cha-no row index))
		 )
		((or (box? cha)
		     (eql cha #\ )
		     (eql cha #\-)
		     (eql cha nil)
		     (eql index 0))
		 (cond
		   ((null cha) (progn  (print "special") cha-num))
		   ((box? cha) (1+ index))
		   ((> index 0) (1+ index))
		   ( t :empty-row)))))))

;;;; what is the greatest upper bound of the length of the row
;;;; that has a pixel count less than pcount
;;;; i.e. (bounded-length (row infinity)) is the length of the row.
;;;; (bounded-length (row 7)) should return 1 if the row's first cha is a
;;;; character


(defun bounded-length (row pcount)
  (if (null row)
      0
      (do* ((index 0 (1+ index))
	    (c (cha-at-cha-no row index)
	       (cha-at-cha-no row index))
	    (pc (thing-wid c)(+ pc (thing-wid c))))
	   ((or (eql nil (cha-at-cha-no row index))
		(> pc pcount)) index)
	)
      ))





;;;; split the row into two, with n-left-on chars in the top row
(defun split-rows (row n-left-on)
  (let ((drow (kill-chas-at-cha-no row n-left-on)))
    (insert-row-at-row-no
     (superior-box row)
     drow
     (1+ (row-row-no
	  (superior-box row)
	  row))
     )))


(defun blank-row (row)
  (cond ((null row) nil)
	(t (eql (cha-at-cha-no row 0) nil))))


;;;; do the filling, with the given rows as the borders

(defun fill-rows (start end)
  (setq *end-row* end)
  (fill-all-but-last-rows start)
  (fill-last-row))

(defun fill-all-but-last-rows (row)
  (if (or (null row) (eql row *end-row*))
      nil
      (progn
	(fill-row row)
	(fill-all-but-last-rows (next-row row)))))


(defun fill-last-row ()
  (if (blank-row *end-row*)
      (erase-row *end-row*)
      (if (fill-row *end-row*) (fill-last-row))))




;;; stuff to strip out spaces in a region

;;; kill spaces between two bp's args: start stop
(defun space-killer-iterator (bp1 bp2)
  (do ((row (bp-row bp1) (next-row row)))
      ((eql row (bp-row bp2))  (space-killer row))
    (space-killer row))
  )

(defun SPACE-KILLER (row)
  "kill xtra spaces on the given row"
  (let ((space-mode t))
    (do* ((index 0 (1+ index))
          (cha (cha-at-cha-no row index) (cha-at-cha-no row index)))
         ((eql index (length-in-chas row)))
      (cond ((eql cha #\Space)
             (if space-mode
               (progn
                 (delete-cha-at-cha-no row index)
                 (setq index (1- index)))
               (setq space-mode t)))
            (t (setq space-mode nil))))))

(defun fill-stuff (bp1 bp2)
  (let ((srow (bp-row bp1)) (erow (bp-row bp2)))
    (space-killer-iterator bp1 bp2)
    (fill-rows srow erow)))

; (defboxer-command com-fill-rows (&optional
;				  (region
;				   (or
;				    *region-being-defined*
;				    (get-current-region))
;				   ))
;   "fill the region"
;   (reset-editor-numeric-arg)
;   (unless (null region)
;     (let ((sbp (interval-start-bp region))
;	   (ebp (interval-stop-bp region)))
;       (cond
;	 ((eql (superior-box (bp-row sbp))
;	       (superior-box (bp-row ebp)))
;	  (progn
;	    (set-a-fill-margin (superior-box (bp-row ebp)))
;	    (if (bp-< sbp ebp)
;		(fill-stuff sbp ebp)
;		(fill-stuff ebp sbp))))
;	 (t
;	  (boxer-editor-error "Endpoints for fill must be in the same box")))
;       )
;     )
;   (reset-region)
;   eval::*novalue*)
;

|#
