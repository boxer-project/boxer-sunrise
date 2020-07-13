;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER; Base:8. -*-

#|


 $Header: comsa.lisp,v 1.0 90/01/24 22:08:49 boxer Exp $


 $Log:	comsa.lisp,v $
;;;Revision 1.0  90/01/24  22:08:49  boxer
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
	   the basic set of BOXER Editor Commands.

	   Most of the commands here are BOXER versions of
	   EMACS commands.  The file COMSB.LISP has the set
	   of Box manipulation commands.  Various other
	   COMSx.LISP files have other sets of commands grouped
	   by functionality.

The basics:
  COM-ABORT COM-INCREMENT-NUMERIC-ARG-BY-~D COM-QUADRUPLE-NUMERIC-ARG
  COM-QUOTE-SELF-INSERT COM-SPACE COM-RETURN COM-OPEN-LINE
Single Character Commands:
  COM-RUBOUT COM-DELETE COM-FORWARD-CHA COM-BACKWARD-CHA
Cursor Movement:
  COM-BEGINNING-OF-ROW COM-END-OF-ROW COM-BEGINNING-OF-BOX COM-END-OF-BOX
  COM-PREVIOUS-ROW COM-PREVIOUS-ROW-OR-NAME COM-NEXT-ROW
  COM-MOVE-TO-PORT-TARGET
Word Commands:
 COM-FORWARD-WORD COM-BACKWARD-WORD COM-RUBOUT-WORD COM-DELETE-WORD
Scrolling:
 COM-SCROLL-DN-ONE-SCREEN-BOX COM-SCROLL-UP-ONE-SCREEN-BOX
Killing Stuff:
 COM-KILL-TO-END-OF-ROW COM-YANK COM-YANK-NO-COPY COM-ROTATE-KILL-BUFFER
Random useful things:
 COM-FORCE-REDISPLAY COM-BREAK COM-BUG COM-GOTO-TOP-LEVEL



Modification History (most recent at the top)

 7/02/09 com-forward/backward-cha now call ensure-row-is-displayed when needed
 4/13/08 com-select-box-contents
 4/17/08 write-system-scrap for intel macs
 8/21/05 com-retrieve checks for null region after yank and only goes into region mode
         if the yank produced a change
 5/22/05 fixed com-bug so it can work from menus as well as keys
 5/20/05 new com-bug mechanism bug-mail-template, bu::(boxer-)bug-report-template
 4/21/02 merged current LW and MCl files
12/02/02 generalized write-mac-scrap to write-system-scrap which is extended to
         handle Windows clipboard
10/23/02 com-forget-place stubbified in preparation for reimplementation for
         a "UC Clean" release
 2/12/01 merged with current MCL file
12/19/00 added ensure-row-is-displayed to rubout-cha to handle obscure
         scrolling bugs
 4/11/00 move-to-place now calls ENTER to get triggers and binding root correct
11/07/99 make sure cursor movement only follows spaces in filling-com-space
10/17/99 autofill functions reorganized, lot of stuff moved to comsb.lisp
10/12/99 new functions: point-position & friends (estimate-box/row-width)
         used by new autofill mechanism
 9/08/99 COM-SELECT-BOX-CONTENTS: reset (possibly) existing region 1st
 6/25/99 %record-box-place added
         move-to-place fills :unfilled slots in position vector
 6/24/99 added com-forget-place
 7/30/98 Started Logging changes: source = boxer version 2.3beta

|#

(in-package :boxer)




;;;; The basics

(defboxer-command com-abort ()
  "aborts any editing in progress.  flushes
numeric arguments and removes the current
region. "
  ;; if there is a region, get rid of it
  (reset-region)
  ;; if the mouse is in a funny mode, get out of it
  (unless (null *generic-port*) (clean-mouse-port-state))
  (unless (null *suitcase-region*) (cleanup-suitcase))
  ;; if there is any saved port state, get rid of it
  (setq *com-make-port-current-port* nil)
  (setq *active-modes* nil)
  (reset-editor-numeric-arg)
  (status-line-clear)  ; clear any accumalated lines before the abort message
  (boxer-editor-error "Stopped!")
  ;; This next thing is hardly necessary.
  (throw 'boxer-editor-top-level nil))

(defun convert-key-code-to-number (kc)
  (ldb (byte 4 0) (char-code kc)))

(defmacro def-com-increment-arg (n) (print "Debugging def-com-increment-arg"))
;; sgithens TODO debugging (defmacro def-com-increment-arg (n)
;;   `(DEFBOXER-COMMAND ,(intern (symbol-format nil "COM-INCREMENT-NUMERIC-ARG-BY-~D" n)) ()
;;      "specifies part of the next command's numeric argument. "
;;      (DECREMENT-KEY-TICK)
;;      (SET-EDITOR-NUMERIC-ARG (+ ,n (* 10. (OR *EDITOR-NUMERIC-ARGUMENT* 0))))
;;      boxer-eval::*novalue*))
(def-com-increment-arg 0)
(def-com-increment-arg 1)
(def-com-increment-arg 2)
(def-com-increment-arg 3)
(def-com-increment-arg 4)
(def-com-increment-arg 5)
(def-com-increment-arg 6)
(def-com-increment-arg 7)
(def-com-increment-arg 8)
(def-com-increment-arg 9)

(defboxer-command com-quadruple-numeric-arg ()
  "multiplies the current numeric argument by 4. "
  (decrement-key-tick)
  (SET-EDITOR-NUMERIC-ARG (* (OR *EDITOR-NUMERIC-ARGUMENT* 1) 4))
  boxer-eval::*novalue*)

(defboxer-command com-quote-self-insert ()
  "inserts any keyboard character.
with a numeric argument, inserts that
many copies of the character. "
  (reset-region)
  (status-line-display 'com-quote-self-insert "Press any key...")
  (let ((cha (get-character-input *boxer-pane*)))
    (with-multiple-execution
	;; should this be in or out of the loop  ?
	#-opengl(add-redisplay-clue (point-row) ':insert)
      (insert-cha *point*
		  cha
		  :moving)))
  (mark-file-box-dirty (point-row))
  (status-line-undisplay 'com-quote-self-insert)
  boxer-eval::*novalue*)

(defboxer-command com-space ()
  "dispatch on filling mode"
  (if (and (display-style-fixed-wid (display-style-list (point-box)))
           (data-box? (point-box)) ;is it ever OK to fill doit boxes?
           (auto-fill? (point-box)))
      (filling-com-space)
      (nonfilling-com-space)))


;;;; versions of com-space that do filling and no filling

(defboxer-command nonfilling-com-space ()
  "inserts a space.  with a numeric
argument (n), inserts n spaces. "
  (reset-region)
  (with-multiple-execution
      #-opengl(add-redisplay-clue (point-row) ':insert)
      (insert-cha *point* #\space :moving))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

#| ;; old version
(defboxer-command filling-com-space ()
  "check margin, com-space, com-return, or format-line"
  (set-a-fill-margin (point-box))
  (if (equal (/ (auto-margin) (cha-wid #\a)) (bp-cha-no *point*))
      (com-return)
      (if (> (/ (auto-margin) (cha-wid #\a)) (bp-cha-no *point*))
          (nonfilling-com-space)
          (format-line))))
|#

(defboxer-command filling-com-space ()
  "(possily) break the at this point"
  (let ((allowed-width (autofill-size)))
    (cond ((>=& (point-position) allowed-width)
           (let ((*WORD-DELIMITERS* '(#\space)))
             (com-backward-word) (com-return) (com-forward-word))
           (nonfilling-com-space))
          (t (nonfilling-com-space)))))


(defun autofill-size (&optional (box (point-box)))
  (let ((in-wid (display-style-fixed-wid (display-style-list box))))
    (when (and (auto-fill? box) in-wid)
      in-wid)))

(defun at-beginningp (bp)
  "is the point at the left margin?"
  (eql 0 (bp-cha-no bp)))

(defun format-line ()
  (let ((temp-point nil) (chop-col nil))
    (nonfilling-com-space)
    (setq temp-point (bp-cha-no *point*))
    (backup)
    (when (= (bp-cha-no *point* ) 0)
      (com-forward-word) (com-delete)
      (setq temp-point (- temp-point 1)))
    (setq chop-col (bp-cha-no *point*))
    (com-return)
    (setf (bp-cha-no *point*) (- temp-point chop-col))
    (when (> (bp-cha-no *point*) 0)
      (com-rubout) (filling-com-space))))

(defun back-stopp ()
  "should backup quit?"
  (<= (bp-cha-no *point*) (/ (auto-margin) (cha-wid #\a))))

(defun backup ()
  (do ((s 1 (com-backward-word)))
      ((back-stopp))))



(DEFBOXER-COMMAND COM-RETURN ()
  "inserts a new line into the buffer
at the cursor location.  with a numeric
argument (n), inserts n new lines. When
in the name portion of a box, enters the
box itself. "
  (reset-region)
  (COND ((NAME-ROW? (POINT-ROW))
         (unshrink (point-box))
         (move-point (box-first-bp-values (point-box)))
	 (RESET-EDITOR-NUMERIC-ARG))
	(T
	 (WITH-MULTIPLE-EXECUTION
           (let ((new-row (make-initialized-row)))
	     (INSERT-ROW *POINT* new-row ':MOVING)))))
  (SETQ *COLUMN* 0)
  (ensure-row-is-displayed (point-row) (point-screen-box) 1)
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-OPEN-LINE ()
  "inserts a blank line after the cursor.
with a numeric arg (n), inserts n blank lines. "
  (reset-region)
  (cond ((name-row? (point-row))
         (with-multiple-execution
           (insert-row-at-row-no (point-box) (make-initialized-row) 0))
         (modified (point-box)))
        (t
         (WITH-MULTIPLE-EXECUTION
           (INSERT-ROW *POINT* (MAKE-INITIALIZED-ROW) ':MOVING)
           (MOVE-POINT (BP-BACKWARD-CHA-VALUES *POINT*)))))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)



;;;; Single Character Commands

(defun simple-delete-cha (bp &optional (force-bp-type nil))
  (action-at-bp-internal
   #-opengl(add-redisplay-clue (bp-row bp) :delete)
   (delete-cha-at-cha-no (bp-row bp) (bp-cha-no bp))))

(defun rubout-cha (bp &optional (force-bp-type nil))
  (action-at-bp-internal
   (let* ((row (bp-row bp))
	  (box (bp-box bp))
	  (row-no (when (not-null box)(row-row-no box row)))
	  (cha-no (bp-cha-no bp))
	  (cha-to-delete (unless (= cha-no 0)
			   (cha-at-cha-no row (1- cha-no)))))
     (cond ((> cha-no 0)
	    #-opengl(add-redisplay-clue row :delete)
	    (delete-cha-at-cha-no row (- cha-no 1)))
	   ((or (name-row? row) (zerop row-no)))
	   (t
	    (let* ((previous-row (row-at-row-no box (- row-no 1)))
		   (previous-row-length-in-chas (length-in-chas previous-row))
		   (*boxes-being-temporarily-deleted* t))
	      (delete-row-at-row-no box row-no)
	      #-opengl(add-redisplay-clue previous-row :insert)
	      (insert-row-chas-at-cha-no
	       previous-row row previous-row-length-in-chas)
              (ensure-row-is-displayed previous-row (point-screen-box) -1))))
     cha-to-delete)))

;; it doesn't look like anyone uses this AND it conflicts with the name of a method
;(defun delete-cha (bp &optional (force-bp-type nil))
;  (action-at-bp-internal
;    (let* ((row (bp-row bp))
;	   (row-length-in-chas (length-in-chas row))
;	   (cha-no (bp-cha-no bp)))
;      (cond ((< cha-no row-length-in-chas)
;	     (delete-cha-at-cha-no row cha-no))
;	    ((next-row row)
;	     (let* ((box (bp-box bp))
;		    (row-row-no (row-row-no box row))
;		    (row-next-row (row-at-row-no box (+ row-row-no 1))))
;	       (delete-row-at-row-no box (+ row-row-no 1))
;	       (insert-row-chas-at-cha-no row row-next-row row-length-in-chas)))))))

(defun editor-kill-region (existing-region)
  (kill-buffer-push (kill-region existing-region) ':forward)
  ;; need to figure out where and what redisplay clues to add here
  (flush-region existing-region)
  (exiting-region-mode))

;;; the redisplay clues are handled inside of RUBOUT-CHA
(DEFBOXER-COMMAND COM-RUBOUT ()
  "Rubs out one character.  With numeric
argument (n), rubs out n characters. "
;;  (reset-region)
;; instead, we'll make it Mac Like
  (let ((existing-region (or *region-being-defined* (get-current-region))))
    (cond ((not (null existing-region))
	   (reset-editor-numeric-arg)
	   (editor-kill-region existing-region))
	  (t
	   (with-multiple-execution
	       (let ((deleted-cha (rubout-cha *point* :moving)))
		 (kill-buffer-push deleted-cha :backward)
		 (setq *column* (bp-cha-no *point*)))))))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-DELETE ()
  "deletes one character.  with numeric
argument (n), delete n characters. "
;;  (reset-region)
;; instead, we'll make it Mac Like
  (let ((existing-region (or *region-being-defined* (get-current-region))))
    (cond ((not (null existing-region))
	   (reset-editor-numeric-arg)
           (editor-kill-region existing-region))
	  (t
	   (WITH-MULTIPLE-EXECUTION
	       (LET ((OLD-ROW (BP-ROW *POINT*))
		     (OLD-CHA-NO (BP-CHA-NO *POINT*)))
		 (MOVE-POINT (BP-FORWARD-CHA-VALUES *POINT*))
		 (when (OR (NOT (EQ OLD-ROW (BP-ROW *POINT*)))
			   (NOT (EQ OLD-CHA-NO (BP-CHA-NO *POINT*))))
		   (kill-buffer-push
		    (RUBOUT-CHA *POINT* ':MOVING)
		    ':forward)))))))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(defboxer-command COM-FORWARD-CHA ()
  "moves forward one character.  with
numeric argument (n), moves forward
n characters. "
    ;; if there is a region, get rid of it
  (reset-region)
  (let ((start-row (point-row)))
    (with-multiple-execution
      (move-point (bp-forward-cha-values *point*)))
    (unless (eq start-row (point-row)) (ensure-row-is-displayed (point-row) (point-screen-box))))
  boxer-eval::*novalue*)

(defboxer-command COM-BACKWARD-CHA ()
  "moves backward one character.  with
numeric argument (n), moves backward
n characters. "
  ;; if there is a region, get rid of it
  (reset-region)
  (let ((start-row (point-row)))
    (with-multiple-execution
      (move-point (bp-backward-cha-values *point*)))
    (unless (eq start-row (point-row)) (ensure-row-is-displayed (point-row) (point-screen-box))))
  boxer-eval::*novalue*)



;;;; Cursor Movement

(defboxer-command COM-BEGINNING-OF-ROW ()
  "moves to the beginning of the row. "
  ;; if there is a region, get rid of it
  (reset-region)
  (reset-editor-numeric-arg)
  (move-point (row-first-bp-values (bp-row *point*)))
  boxer-eval::*novalue*)

(defboxer-command COM-END-OF-ROW ()
  "moves to the end of the row. "
  ;; if there is a region, get rid of it
  (reset-region)
  (reset-editor-numeric-arg)
  (move-point (row-last-bp-values (bp-row *point*)))
  boxer-eval::*novalue*)

(defboxer-command COM-BEGINNING-OF-BOX ()
  "moves to the beginning of the box. "
  ;; if there is a region, get rid of it
  (reset-region)
  (reset-editor-numeric-arg)
  (move-point (box-first-bp-values (box-point-is-in)))
  (dolist (screen-box (screen-objs (box-screen-point-is-in)))
    (set-scroll-to-actual-row screen-box
			      (first-inferior-row (box-point-is-in))))
  boxer-eval::*novalue*)

(defboxer-command COM-END-OF-BOX ()
  "moves to the end of the box. "
  ;; if there is a region, get rid of it
  (reset-region)
  (reset-editor-numeric-arg)
  (move-point (box-last-bp-values (box-point-is-in)))
  (ensure-row-is-displayed (point-row) (point-screen-box) 1)
  boxer-eval::*novalue*)

(defboxer-command COM-PREVIOUS-ROW ()
  "moves up vertically to the previous
row.  With numeric argument (n), moves
up n rows.  Tries to stay as close as
possible to the original column. "
  ;; if there is a region, get rid of it
  (reset-region)
  (with-multiple-execution
    (let* ((row (bp-row *point*))
	   (previous-row (unless (null row) (previous-row row)))
	   (previous-row-length-in-chas
	     (unless (null previous-row) (length-in-chas previous-row)))
	   (cha-no (bp-cha-no *point*))
	   (current-row-length-in-chas (length-in-chas row)))
      (cond ((null previous-row) (com-name-box))
	    ((< cha-no current-row-length-in-chas)
	     (setq *column* cha-no)
	     (move-point-1 previous-row
			   (min previous-row-length-in-chas *column*)))
	    ((< *column* cha-no)
	     (setq *column* cha-no)
	     (move-point-1 previous-row
			   (min previous-row-length-in-chas *column*)))
	    (t
	     (move-point-1 previous-row
			   (min previous-row-length-in-chas *column*))))
      (ensure-row-is-displayed (point-row) (point-screen-box) -1)))
  boxer-eval::*novalue*)


;; this one goes to the name row if it's there
(defboxer-command com-previous-row-or-name ()
  "moves up vertically to the previous
row.  with numeric argument (n), moves
up n rows.  tries to stay as close as
possible to the original column. "
  ;; if there is a region, get rid of it
  (reset-region)
  (with-multiple-execution
    (let* ((row (bp-row *point*))
	   (previous-row (unless (null row) (previous-row row)))
	   (previous-row-length-in-chas
	     (unless (null previous-row) (length-in-chas previous-row)))
	   (cha-no (bp-cha-no *point*))
	   (current-row-length-in-chas (length-in-chas row)))
      (cond ((null previous-row)
	     (com-name-box))
	    ((< cha-no current-row-length-in-chas)
	     (setq *column* cha-no)
	     (move-point-1 previous-row
			   (min previous-row-length-in-chas *column*)))
	    ((< *column* cha-no)
	     (setq *column* cha-no)
	     (move-point-1 previous-row
			   (min previous-row-length-in-chas *column*)))
	    (t
	     (move-point-1 previous-row
			   (min previous-row-length-in-chas *column*))))
      (ensure-row-is-displayed (point-row) (point-screen-box))))
  boxer-eval::*novalue*)



(defboxer-command COM-NEXT-ROW ()
  "moves up vertically down the next
row.  with numeric argument (n), moves
down n rows.  tries to stay as close as
possible to the original column. "
  ;; if there is a region, get rid of it
  (reset-region)
  (with-multiple-execution
    (let* ((row (bp-row *point*))
	   (next-row (unless (null row) (next-row row)))
	   (next-row-length-in-chas (unless (null next-row)
				      (length-in-chas next-row)))
	   (cha-no (bp-cha-no *point*))
	   (current-row-length-in-chas (length-in-chas row)))
      (cond ((null next-row)
	     (com-end-of-row)
	     (com-return)
;	     (ensure-row-is-displayed (point-row) (point-screen-box) 1)
;	     (com-return) already calls this
	     )
	    ((< cha-no current-row-length-in-chas)
	     (setq *column* cha-no)
	     (move-point-1 next-row (min next-row-length-in-chas *column*))
	     (ensure-row-is-displayed (point-row) (point-screen-box) 1))
	    ((< *column* cha-no)
	     (setq *column* cha-no)
	     (move-point-1 next-row (min next-row-length-in-chas *column*))
	     (ensure-row-is-displayed (point-row) (point-screen-box) 1))
	    (t
	     (move-point-1 next-row (min next-row-length-in-chas *column*))
	     (ensure-row-is-displayed (point-row) (point-screen-box) 1)))))
  boxer-eval::*novalue*)


(defboxer-command COM-MOVE-TO-PORT-TARGET ()
  "moves the cursor to the place specified by bp"
  ;; if there is a region, get rid of it
  (reset-region)
  (let* ((current-port (box-screen-point-is-in))
	 (target (ports current-port))
	 (row (point-row)) (cha-no (point-cha-no)))
    (cond ((not (port-box? current-port))
	   ;; make sure we have a target
	   (boxer-editor-error "You can only zoom from a Port"))
	  ((and (box? target)
		;; make sure the target is still in the hierarchy
		(superior? target *initial-box*))
	   (with-temporary-bp (target-bp (box-self-bp-values target))
	     (move-to-bp target-bp))
	   ;; now that we are outside the target,
	   ;; move into it and go back to the original row/cha-no
	   (com-enter-box)
	   (move-point-1 row cha-no)
	   (ensure-row-is-displayed (point-row) (point-screen-box)))
	  (t (boxer-editor-error "could not move to ~a" target))))
  boxer-eval::*novalue*)



;;;; Word Commands

;;; primitives for word operations

;; search for cha but ignoring font info
(defun char-member (cha chas)
  (and (not (null cha))
       (not (box? cha))
       (member cha chas :test #'char-equal)))

(DEFUN BP-OVER-VALUES (BP DIRECTION DELIMITER-CHAS)
  (LET ((NOT-FIRST-CHA? NIL))
    (MAP-OVER-CHAS-IN-LINE
     (BP DIRECTION)
     (LET ((DELIMITER-CHA? (char-member cha delimiter-chas)))
       (COND ((AND (NULL CHA)
		   (NULL NEXT-OR-PREVIOUS-ROW))	;end/beginning of the box
	      (RETURN (VALUES ROW CHA-NO)))
	     ((AND (NULL CHA) NOT-FIRST-CHA?) ;end/beginning of the line
	      (RETURN (VALUES ROW CHA-NO)))
	     ((AND NOT-FIRST-CHA? (BOX? CHA)) ;go only one box at a time
	      (RETURN (VALUES ROW CHA-NO)))
	     ((AND NOT-FIRST-CHA? DELIMITER-CHA?) ;end of the word
	      (RETURN (VALUES ROW CHA-NO)))
	     ((NOT DELIMITER-CHA?)	;beginning of word
	      (SETQ NOT-FIRST-CHA? T)))))))
#| ; old stuff
(DEFUN BP-OVER-VALUES (BP DIRECTION DELIMITER-CHAS)
  (LET ((NOT-FIRST-CHA? NIL))
    (MAP-OVER-CHAS-IN-LINE (BP DIRECTION)
      (LET ((DELIMITER-CHA? (char-member cha delimiter-chas)))
	   (COND ((AND (NULL CHA)
		       (NULL NEXT-OR-PREVIOUS-ROW)) ;end/beginning of the box
		  (RETURN (VALUES ROW CHA-NO)))
		 ((AND (NULL CHA) NOT-FIRST-CHA?) ;end/beginning of the line
		  (RETURN (VALUES ROW CHA-NO)))
		 ((AND NOT-FIRST-CHA? DELIMITER-CHA?) ;end of the word
		  (RETURN (VALUES ROW CHA-NO)))
		 ((NOT DELIMITER-CHA?)	;beginning of word
		  (SETQ NOT-FIRST-CHA? T)))))))
|#

(DEFUN BP-FORWARD-WORD-VALUES (BP)
  (BP-OVER-VALUES BP 1 *WORD-DELIMITERS*))

(DEFUN BP-BACKWARD-WORD-VALUES (BP)
  (BP-OVER-VALUES BP -1 *WORD-DELIMITERS*))

(defboxer-command COM-FORWARD-WORD ()
  "moves forward one word. with numeric
argument (n), moves forward n words. "
  ;; if there is a region, get rid of it
  (reset-region)
  (with-multiple-execution
    (move-point (bp-forward-word-values *point*)))
  boxer-eval::*novalue*)

(defboxer-command COM-BACKWARD-WORD ()
  "moves backward one word. with numeric
argument (n), moves backward n words. "
  ;; if there is a region, get rid of it
  (reset-region)
  (with-multiple-execution
    (move-point (bp-backward-word-values *point*)))
  boxer-eval::*novalue*)



;;; The Redisplay Clues are wrong but we shouldn't fix it until
;;; we properly fix the case where the *point* is at the beginning
;;; of the next row and we SHOULD join the 2 rows

(defun rubout-over-values (bp direction delimiter-chas)
  (let ((not-first-cha? nil))
    (map-over-chas-in-line (bp direction)
      (let ((delimiter-cha? (char-member cha delimiter-chas))
	    (force-bp-type ':moving))
	(cond ((and (null cha) (null next-or-previous-row))
	       ;; end/beginning of the box
	       (return (values row cha-no)))
	      ((and not-first-cha? (null cha))
	       ;; end/beginning of the line
	       (return (values row cha-no)))
	      ((and not-first-cha? (box? cha))
	       ;; go only one box at a time
	       (return (values row cha-no)))
	      ((and not-first-cha? delimiter-cha?)
	       ;; end of the word
	       (return (values row cha-no)))
	      ((not delimiter-cha?)
	       ;; beginning of word
	       (setq not-first-cha? t)
	       (action-at-bp-internal
		 (kill-buffer-push (cha-at-cha-no row (1- cha-no)) ':backward)
		 #-opengl(add-redisplay-clue (bp-row bp) ':delete)
		 (delete-cha-at-cha-no row (1- cha-no))))
	      (t
	       ;; Delimiter Chas Before Word
	       (action-at-bp-internal
		 (kill-buffer-push (cha-at-cha-no row (1- cha-no)) ':backward)
		 #-opengl(add-redisplay-clue (bp-row bp) ':delete)
		 (delete-cha-at-cha-no row (1- cha-no)))))))))

;;old stuff
;(DEFUN RUBOUT-OVER-VALUES (BP DIRECTION DELIMITER-CHAS)
;  (LET ((NOT-FIRST-CHA? NIL))
;    (MAP-OVER-CHAS-IN-LINE (BP DIRECTION)
;      (LET ((DELIMITER-CHA? (char-member cha delimiter-chas))
;	    (FORCE-BP-TYPE ':MOVING))
;	(COND ((AND (NULL CHA)(NULL NEXT-OR-PREVIOUS-ROW));end/beginning of box
;	       (RETURN (VALUES ROW CHA-NO)))
;	      ((AND NOT-FIRST-CHA? (NULL CHA))      ;end/beginning of the line
;	       (RETURN (VALUES ROW CHA-NO)))
;	      ((AND NOT-FIRST-CHA? DELIMITER-CHA?)           ;end of the word
;	       (RETURN (VALUES ROW CHA-NO)))
;	      ((NOT DELIMITER-CHA?)                          ;beginning of word
;	       (SETQ NOT-FIRST-CHA? T)
;	       (ACTION-AT-BP-INTERNAL
;		 (kill-buffer-push (cha-at-cha-no row (1- cha-no)) ':backward)
;		 (DELETE-CHA-AT-CHA-NO ROW (1- CHA-NO))))
;	      (T                                    ;delimiter chas before word
;	       (ACTION-AT-BP-INTERNAL
;		 (kill-buffer-push (cha-at-cha-no row (1- cha-no)) ':backward)
;		 (DELETE-CHA-AT-CHA-NO ROW (1- CHA-NO)))))))))

;;; The Redisplay Clues are wrong but we shouldn't fix it until
;;; we properly fix the case where the *point* is at the end
;;; of the previous row and we SHOULD join the 2 rows

(DEFUN DELETE-OVER-VALUES (BP DELIMITER-CHAS)
  (DO* ((ROW (BP-ROW BP) ROW)
	(NEXT-ROW (UNLESS (NULL ROW) (NEXT-ROW ROW))
		  (UNLESS (NULL ROW) (NEXT-ROW ROW)))
	(CHA-NO (BP-CHA-NO BP))
	(CHA (CHA-AT-CHA-NO ROW CHA-NO)
	     (CHA-AT-CHA-NO ROW CHA-NO))
	(NOT-FIRST-CHA?))
       (NIL)
    (COND ((AND (NULL NOT-FIRST-CHA?)
		(NULL CHA)
		(NOT-NULL NEXT-ROW))
	   (SETQ ROW NEXT-ROW
		 CHA-NO 0))
	  (T (LET ((DELIMITER-CHA? (char-member cha delimiter-chas))
		   (FORCE-BP-TYPE ':MOVING))
	       (COND ((AND (NULL CHA) (NULL NEXT-ROW))
		      ;; end/beginning of the box
		      (RETURN (VALUES ROW CHA-NO)))
		     ((AND NOT-FIRST-CHA? (NULL CHA))
		      ;; end/beginning of the line
		      (RETURN (VALUES ROW CHA-NO)))
		     ((and not-first-cha? (box? cha))
		      ;; go only one box at a time
		      (RETURN (VALUES ROW CHA-NO)))
		     ((AND NOT-FIRST-CHA? DELIMITER-CHA?)
		      ;; end of the word
		      (RETURN (VALUES ROW CHA-NO)))
		     ((NOT DELIMITER-CHA?)
		      ;; beginning of word
		      (SETQ NOT-FIRST-CHA? T)
		      (ACTION-AT-BP-INTERNAL
			(kill-buffer-push (cha-at-cha-no row cha-no) ':forward)
			#-opengl(add-redisplay-clue row ':delete)
			(DELETE-CHA-AT-CHA-NO ROW CHA-NO )))
		     (T
		      ;; delimiter chas before word
		      (ACTION-AT-BP-INTERNAL
			(kill-buffer-push (cha-at-cha-no row cha-no) ':forward)
			#-opengl(add-redisplay-clue row ':delete)
			(DELETE-CHA-AT-CHA-NO ROW CHA-NO)))))))))

;;old stuff
;(DEFUN DELETE-OVER-VALUES (BP DELIMITER-CHAS)
;  (DO* ((ROW (BP-ROW BP) ROW)
;	(NEXT-ROW (UNLESS (NULL ROW) (NEXT-ROW ROW))
;		  (UNLESS (NULL ROW) (NEXT-ROW ROW)))
;	(CHA-NO (BP-CHA-NO BP)
;		(BP-CHA-NO BP))
;	(CHA (CHA-AT-CHA-NO ROW CHA-NO)
;	     (CHA-AT-CHA-NO ROW CHA-NO))
;	(NOT-FIRST-CHA?))
;       (NIL)
;    (COND ((AND (NULL NOT-FIRST-CHA?)
;		(NULL CHA)
;		(NOT-NULL NEXT-ROW))
;	   (SETQ ROW NEXT-ROW
;		 CHA-NO 0))
;	  (T (LET ((DELIMITER-CHA? (char-member cha delimiter-chas))
;		   (FORCE-BP-TYPE ':MOVING))
;	       (COND ((AND (NULL CHA) (NULL NEXT-ROW));end/beginning of the box
;		      (RETURN (VALUES ROW CHA-NO)))
;		     ((AND NOT-FIRST-CHA? (NULL CHA));end/beginning of the line
;		      (RETURN (VALUES ROW CHA-NO)))
;		     ((AND NOT-FIRST-CHA? DELIMITER-CHA?)  ;end of the word
;		      (RETURN (VALUES ROW CHA-NO)))
;		     ((NOT DELIMITER-CHA?)                 ;beginning of word
;		      (SETQ NOT-FIRST-CHA? T)
;		      (ACTION-AT-BP-INTERNAL
;			(kill-buffer-push (cha-at-cha-no row cha-no) ':forward)
;			(DELETE-CHA-AT-CHA-NO ROW CHA-NO )))
;		     (T                            ;delimiter chas before word
;		      (ACTION-AT-BP-INTERNAL
;			(kill-buffer-push (cha-at-cha-no row cha-no) ':forward)
;			(DELETE-CHA-AT-CHA-NO ROW CHA-NO)))))))))




(DEFUN RUBOUT-WORD (BP)
  (RUBOUT-OVER-VALUES BP -1 *WORD-DELIMITERS*))

(defboxer-command COM-RUBOUT-WORD ()
  "kills backward one word.  with numeric
argument (n), kills backward n words. "
  (let ((existing-region (or *region-being-defined* (get-current-region))))
    (cond ((not (null existing-region))
	   (reset-editor-numeric-arg)
	   (kill-buffer-push (kill-region existing-region) ':forward)
	   ;; need to figure out where and what redisplay clues to add here
	   (flush-region existing-region)
	   (exiting-region-mode))
	  (t
	   (with-multiple-execution
	       (move-point (rubout-word *point*))))))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(DEFUN DELETE-WORD (BP)
  (DELETE-OVER-VALUES BP  *WORD-DELIMITERS*))

(DEFBOXER-COMMAND COM-DELETE-WORD ()
  "kills forward one word.  with numeric
argument (n), kills forward n words. "
  (let ((existing-region (or *region-being-defined* (get-current-region))))
    (cond ((not (null existing-region))
	   (reset-editor-numeric-arg)
	   (kill-buffer-push (kill-region existing-region) ':forward)
	   ;; need to figure out where and what redisplay clues to add here
	   (flush-region existing-region)
	   (exiting-region-mode))
	  (t
	   (with-multiple-execution
	       (move-point (delete-word *point*))))))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)



;;;; Scrolling

(defboxer-command COM-SCROLL-DN-ONE-SCREEN-BOX (&optional
                                                (screen-boxs
                                                 (unless (null (box-point-is-in))
                                                   (displayed-screen-objs
                                                    (box-point-is-in)))))
  "displays the next box of text. "
  ;; if there is a region, get rid of it
  (reset-region)
  (reset-editor-numeric-arg)
  (let ((row-to-move-to nil))
    (dolist (screen-box screen-boxs)
      (let ((screen-box-new-scroll-row (scroll-dn-one-screen-box screen-box)))
	(when (row? screen-box-new-scroll-row)
	  (setq row-to-move-to
		(cond ((null row-to-move-to)
		       screen-box-new-scroll-row)
		      ((row-> screen-box-new-scroll-row row-to-move-to)
		       screen-box-new-scroll-row)
		      (t row-to-move-to))))))
    (unless (null row-to-move-to)
      (move-point-1 row-to-move-to (min (length-in-chas row-to-move-to)
					(bp-cha-no *point*)))))
  boxer-eval::*novalue*)

(defboxer-command COM-SCROLL-UP-ONE-SCREEN-BOX (&optional
                                                (screen-boxs
                                                 (unless (null (box-point-is-in))
		                                   (displayed-screen-objs
                                                    (box-point-is-in)))))
  "displays the previous box of text. "
  ;; if there is a region, get rid of it
  (reset-region)
  (reset-editor-numeric-arg)
  (let ((row-to-move-to nil))
    (dolist (screen-box screen-boxs)
      (scroll-up-one-screen-box screen-box)
      (let ((screen-box-new-scroll-row
	     (screen-obj-actual-obj (first-screen-row screen-box))))
	(when (row? screen-box-new-scroll-row)
	  (setq row-to-move-to
		(cond ((null row-to-move-to)
		       screen-box-new-scroll-row)
		      ((row-< screen-box-new-scroll-row row-to-move-to)
		       screen-box-new-scroll-row)
		      (t row-to-move-to))))))
    (when (row? row-to-move-to)
      (move-point-1 row-to-move-to (min (length-in-chas row-to-move-to)
					(bp-cha-no *point*)))))
  boxer-eval::*novalue*)

(defboxer-command COM-SCROLL-DN-ROW (&optional
                                     (screen-box (screen-box-point-is-in)))
  "Scrolls the box down by <numeric arg> rows."
  (let* ((box (screen-obj-actual-obj screen-box))
	 (row-length (length-in-rows box))
	 (scroll-row (scroll-to-actual-row screen-box))
	 (new-scroll-no (+ (if (null scroll-row) 0 (row-row-no box scroll-row))
			   (or *editor-numeric-argument* 1)))
	 (new-scroll-row (if (>= new-scroll-no row-length)
			     (row-at-row-no box (1- row-length))
			     (row-at-row-no box new-scroll-no))))
    (set-scroll-to-actual-row screen-box new-scroll-row)
    ;; make sure the point is still visible
    (when (row-> new-scroll-row (point-row))
      ;; if we have to move make sure we flush the region
      (reset-region)
      (move-point-1 new-scroll-row 0))
    boxer-eval::*novalue*))

(defboxer-command COM-SCROLL-UP-ROW (&optional
                                     (screen-box (screen-box-point-is-in)))
  "Scrolls the box up by <numeric arg> rows."
  (let* ((box (screen-obj-actual-obj screen-box))
	 (scroll-row (scroll-to-actual-row screen-box))
	 (new-scroll-no (if (null scroll-row)
			    0
			    (max& 0 (-& (row-row-no box scroll-row)
					(or *editor-numeric-argument* 1)))))
	 (new-scroll-row (row-at-row-no box new-scroll-no)))
    (set-scroll-to-actual-row screen-box new-scroll-row)
    ;; make sure the point is still visible
    (when (row-< new-scroll-row (point-row))
      ;; if we have to move make sure we flush the region
      (reset-region)
      (move-point-1 new-scroll-row 0))
    boxer-eval::*novalue*))





;;;; Killing Stuff

(DEFBOXER-COMMAND COM-KILL-TO-END-OF-ROW ()
  "Kills forward to the end of the line, and leaves and empty line.  If
called on an empty line, deletes the empty line.  If given a numeric
argument, deletes that many lines."
  (reset-region)
  (IF (NOT (NULL *EDITOR-NUMERIC-ARGUMENT*))
      (SET-EDITOR-NUMERIC-ARG (* 2 *EDITOR-NUMERIC-ARGUMENT*)))
  (WITH-MULTIPLE-EXECUTION
    (LET* ((ROW (BP-ROW *POINT*))
	   (NEXT-ROW (NEXT-ROW ROW))
	   (ROW-LENGTH-IN-CHAS (LENGTH-IN-CHAS ROW))
	   (CHA-NO (BP-CHA-NO *POINT*)))
      (COND ((< CHA-NO ROW-LENGTH-IN-CHAS)
	     #-opengl(add-redisplay-clue row ':delete)
	     (kill-buffer-push (DELETE-CHAS-TO-END-OF-ROW *POINT* :FIXED)
			       :FORWARD))
	    ((NULL NEXT-ROW))
	    (T (MOVE-POINT (BP-FORWARD-CHA-VALUES *POINT*))
	       ;; if we are at the end of the row,
	       ;; join the next row to the current one
	       #-opengl(add-redisplay-clue row ':insert)
	       (kill-buffer-push (rubout-cha *point* :moving) :forward)))))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(defboxer-command com-delete-line ()
  "Removes the line the point is on"
  (reset-region)
  (with-multiple-execution
      (let* ((delete-row (point-row))
	     (next-row (next-row delete-row))
	     (prev-row (previous-row delete-row))
	     (cha-no (point-cha-no)))
	(cond ((and (null next-row) (null prev-row))
	       ;; the delete-row is the ONLY row in the box
	       #-opengl(add-redisplay-clue delete-row ':delete)
	       (move-point-1 delete-row 0)
	       (kill-buffer-push (delete-chas-to-end-of-row *point* :fixed) :forward))
	      (t
	       (if (null next-row)
		   (move-point-1 prev-row (min (length-in-chas prev-row) cha-no))
		   (move-point-1 next-row (min (length-in-chas next-row) cha-no)))
	       (delete-row (point-box) delete-row)
	       (kill-buffer-push delete-row :forward)))))
  (mark-file-box-dirty (point-box))
  (modified (point-box))
  boxer-eval::*novalue*)

(defboxer-command COM-SELECT-BOX-CONTENTS ()
  "Selects the contents of a box as the current region"
  (reset-region)
  (drawing-on-window-without-prepare-sheet (*boxer-pane*)
    (with-drawing-port *boxer-pane*
      ;; this is essentially all of drawing-on-window EXCEPT
      ;; blinker management, probably should have a separate macro
      ;; for this eventually
      (multiple-value-bind (start-row start-cha-no)
          (box-first-bp-values (point-box))
        (multiple-value-bind (stop-row stop-cha-no)
            (box-last-bp-values (point-box))
          (let ((start-bp (make-bp ':fixed))
                (stop-bp  (make-bp ':fixed)))
            (setf (bp-row start-bp) start-row (bp-cha-no start-bp) start-cha-no
                  (bp-row stop-bp) stop-row (bp-cha-no stop-bp) stop-cha-no)
            (setq *region-being-defined*
                  (make-editor-region start-bp stop-bp))
            #-opengl (turn-on-interval *region-being-defined*)
            (push *region-being-defined* *region-list*)
            #-opengl (interval-update-redisplay-all-rows *region-being-defined*)
            (entering-region-mode))))))
  boxer-eval::*novalue*)

(defboxer-command COM-COPY-REGION ()
  "Copies an existing region to the Kill Buffer"
  (reset-editor-numeric-arg)
  (let ((existing-region (or *region-being-defined* (get-current-region))))
    (cond ((not (null existing-region))
           (kill-buffer-push
            (let* ((boxer-eval::*primitive-shadow-warning-on?* nil) ; don't warn
                   (region (copy-interval existing-region)))
              (write-system-scrap region)
              region)
            ':forward)
	   ;; should we flush the region here ?
	   ;; on one hand, what else would we want to do with it ?
	   ;; on the other hand, "It's not like the Mac"
	   (flush-region existing-region)
	   (exiting-region-mode))
	  (t
	   (boxer-editor-error "There is no region"))))
  boxer-eval::*novalue*)

(defboxer-command COM-CUT-REGION ()
  "Delete the region, placing it in the kill buffer"
  (let ((existing-region (or *region-being-defined* (get-current-region))))
    (when (not (null existing-region))
      (reset-editor-numeric-arg)
      (let ((killed-region (kill-region existing-region)))
        (write-system-scrap killed-region)
        (kill-buffer-push killed-region ':forward))
      ;; need to figure out where and what redisplay clues to add here
      (flush-region existing-region)
      (exiting-region-mode)))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(defboxer-command COM-YANK ()
  "Inserts the last piece of text that was killed
and makes a copy of it for further yanking."
  ;(reset-region)
  (with-multiple-execution
    (let ((item (kill-buffer-top)))
      (setf (kill-buffer-top) (copy-thing item))
      ;; mac, replacing region behavior
      (let ((r (or *region-being-defined* (get-current-region))))
        (unless (null r) (editor-kill-region r)))
      (top-level-insert-things item)))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(defboxer-command COM-YANK-NO-COPY ()
  "Inserts the last piece of text that was killed and
removes it from the kill buffer.  No copy is made."
  (reset-region)
  (reset-editor-numeric-arg)
  (let ((item (kill-buffer-top)))
    (setf (kill-buffer-top) nil)
    (top-level-insert-things item))
  (com-rotate-kill-buffer)
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(defvar *last-retrieved-region* nil)

(defboxer-command COM-RETRIEVE ()
  "Inserts last piece of text, leaves it highlited"
  (reset-editor-numeric-arg)
  ;; mac, replacing region behavior
  (let ((r (or *region-being-defined* (get-current-region))))
    ;; first, if there is ANY region, kill it
    (unless (null r) (editor-kill-region r) (com-rotate-kill-buffer)))
  ;; now yank (no copy) the top and mark it as the region being defined
  (let ((start-bp (make-bp :fixed)) (stop-bp (make-bp :moving)))
    (set-bp-row start-bp (point-row)) (set-bp-cha-no start-bp (point-cha-no))
    (set-bp-row stop-bp (point-row)) (set-bp-cha-no stop-bp (point-cha-no))
    (com-yank-no-copy)
    ;; the BP's should be in the right place now
    (unless (bp-= start-bp stop-bp)
      (setq *region-being-defined* (make-editor-region start-bp stop-bp)
	    *last-retrieved-region* *region-being-defined*)
      (turn-on-interval *region-being-defined*)
      (push *region-being-defined* *region-list*)
      (entering-region-mode))
    (mark-file-box-dirty (point-row))
    boxer-eval::*novalue*))


(defun insert-string-chas (string)
  #-opengl(add-redisplay-clue (point-row) ':insert)
  (map nil #'(lambda (cha) (insert-cha *point* cha)) string))

(defun top-level-insert-things (things)
  (if (name-row? (point-row))
      (catch 'name-row-insert-done (name-row-insert-things things))
      (insert-list-of-things things)))

(defun name-row-insert-things (things)
  (cond ((null things))
	((box? things)
	 (boxer-editor-error "Tried to Insert a box into a name row"))
	((cha? things)
	 (if (or (char= things #\return) (char= things #\newline))
	     (throw 'name-row-insert-done nil)
	     (insert-cha *point* things :moving)))
	((row? things)
	 (insert-row-chas *point* things :moving)
	 (throw 'name-row-insert-done nil))
	((interval? things)
	 (do-interval-chas (cha things :output-newlines? t)
	   (name-row-insert-things cha)))
	((eq things :newline) (throw 'name-row-insert-done nil))
	((listp things)
	 (dolist (thing things) (name-row-insert-things thing)))
	(t
	 (boxer-editor-error "Unusual Object Found In Boxer Kill Buffer"))))


(defun insert-list-of-things (things)
  (if (listp things)
      (dolist (thing things) (insert-thing thing))
      (insert-thing things)))

;;;FROM coms
(defun insert-thing (thing)
  (cond ((null thing))
	((or (box? thing) (cha? thing))
	 #-opengl(add-redisplay-clue (point-row) ':insert)
	 (insert-cha *point* thing :moving))
	((row? thing) (if (zerop (length-in-chas thing))
			  (insert-row *point* thing :moving)
			  (progn
			    #-opengl(add-redisplay-clue (point-row) ':insert)
			    (insert-row-chas *point* thing :moving))))
	((interval? thing)
	 (yank-region *point* thing)
	 #-opengl(add-redisplay-clue (point-row) ':insert)
	 (unless *highlight-yanked-region*
	   (turn-off-interval thing))
	 (setq *current-editor-region* thing))
	((eq thing :newline)
	 (insert-row *point* (make-initialized-row) :moving))
	((listp thing) (insert-list-of-things thing))
	(t (boxer-editor-error "Unusual Object Found In Boxer Kill Buffer"))))


(defun kill-buffer-push (item direction)
  (when (null item) (setq item :newline))
  (if (<= *number-of-non-kill-commands-executed* 1)
      (if (eq direction *kill-buffer-last-direction*)
	  (cond ((eq direction ':forward)
		 (ensure-list item)
		 (ensure-list (car *kill-buffer*))
		 (setf (car *kill-buffer*)
		       (nconc (car *kill-buffer*) item)))
		((eq direction ':backward)
		 (ensure-list (car *kill-buffer*))
		 (setf (car *kill-buffer*)
		       (cons item (car *kill-buffer*)))))
	  (push item *kill-buffer*))
      (push item *kill-buffer*))
  ;; We don't want every deleted char to be in the clipboard, so adding to the
  ;; clipboard is now limited to the kill-region and copy-region commands
;  (write-system-scrap (car *kill-buffer*))
  (when (> (length *kill-buffer*) *kill-buffer-length*)
    (let ((objs-for-deallocation (car (nthcdr *kill-buffer-length* *kill-buffer*))))
      (rplacd (nthcdr (1- *kill-buffer-length*) *kill-buffer*) nil)
      (queue-editor-objs-for-deallocation objs-for-deallocation)))
  (setq *kill-buffer-last-direction* direction)
  (setq *number-of-non-kill-commands-executed* 0)
  *kill-buffer*)

;for control-meta-y, sort of.
(defboxer-command COM-ROTATE-KILL-BUFFER ()
  "rotates the kill buffer. "
  (with-multiple-execution
      (let ((rots 0))
	(loop
	 (setq *kill-buffer* (nconc (cdr *kill-buffer*) (list (car *kill-buffer*))))
	 (incf& rots)
	 (when (or (not (null (car *kill-buffer*)))
		   (>=& rots *kill-buffer-length*))
	   (return)))))
  (write-system-scrap (car *kill-buffer*))
  boxer-eval::*novalue*)

;;; Called whenever a yank is done.  Is this imposing too much load? Alternatively,
;;; we could advise get-internal-scrap to look at the boxer kill buffer.

(defun write-system-scrap (thing)
  (let ((text (textify-thing thing)))
    (when text
      #+mcl (ccl:put-scrap :text text)
      #+lispworks (capi::set-clipboard *boxer-frame* text)
      ;; NOTE:for LW and X, use capi::set-selection
      #-(or mcl lispworks) (warn "Write System Scrap undefined for ~A on ~A"
                             (lisp-implementation-type) (machine-type)))))


(defun textify-thing (thing &optional stream)
  (if (not stream)
    (with-output-to-string (stream)
      (textify-thing thing stream))
    (flet ((textify-row (row)
             (let* ((chas (chas-array row))
                    (length (chas-array-active-length chas)))
               (dotimes (n length)
                 (let ((char (chas-array-get-cha chas n)))
                   (textify-thing char stream))))))
      (typecase thing
        (interval
         (with-region-top-level-bps (thing :start-bp-name start-bp
                                           :stop-bp-name  stop-bp)
           (flet ((first-row? (row) (eq row (bp-row start-bp)))
                  (last-row?  (row) (eq row (bp-row stop-bp)))
                  )
             (do-region-rows (rr thing)
               (textify-row rr)
               (unless (last-row? rr)
                 (terpri stream))))))
        (row
         (textify-row thing))
        (port-box
         (format stream "[PORT-BOX]"))
        (box
         (write-char #\[ stream)
         (do-box-rows ((row thing))
           (textify-row row)
           (when (next-row  row) (terpri stream)))
         (write-char #\] stream))
        (character (write-char thing stream))
        (list (dolist (elem thing)
                (textify-thing elem stream)))
        (symbol
         (case thing
           (:newline (write-char #\newline stream))
           (t (error "Couldn't translate ~A to text" thing))))
        (t (error "Couldn't translate ~A to text" thing)
           nil)))))



;this function copys things if they're boxer structures that have uniqueness.
;This should probably return a PRE-BOX but I can't figure out how they work.
(defun copy-thing (boxer-thing)
  (bw::with-mouse-cursor (:processing)
    (let ((boxer-eval::*warn-about-primitive-shadowing* nil))
      (cond ((box? boxer-thing) (copy-top-level-box boxer-thing :name))
	    ((row? boxer-thing) (copy-row boxer-thing nil nil :name))
	    ((cha? boxer-thing) boxer-thing)
	    ((interval? boxer-thing) (copy-interval boxer-thing :name))
	    ((listp boxer-thing) (mapcar #'copy-thing boxer-thing))
	    (t boxer-thing)))))		;aw, who cares?

;; This is called by the function which handles keystrokes every time it
;; executes a command.  If, when we execute a killing/saving command
;; (i.e., call kill-buffer-push) the count is not 1, then the last command
;; wasn't a kill and we should make a new entry into the kill buffer.
(DEFUN INCREMENT-KEY-TICK ()
  (INCF *NUMBER-OF-NON-KILL-COMMANDS-EXECUTED*))

;; Call this function inside things like numeric argument and such functions
;; that shouldn't be considered to interrupt the kill buffering.
(DEFUN DECREMENT-KEY-TICK ()
  (DECF *NUMBER-OF-NON-KILL-COMMANDS-EXECUTED*))



;;;; Random useful things

(defboxer-command COM-FORCE-REDISPLAY (&optional redraw-status-line?)
  "clears and then redisplays the screen. "
  (reset-editor-numeric-arg)
  #-opengl(add-redisplay-clue (outermost-box) :clear-screen)
  (when redraw-status-line? (redraw-status-line))
  #-opengl(force-redisplay)
  #+opengl(force-repaint)
  boxer-eval::*novalue*)

(defboxer-command COM-FORCE-REDISPLAY-ALL ()
  "Clears and then Redisplays the screen including the status line"
  (com-force-redisplay t)
  boxer-eval::*novalue*)

(defboxer-command COM-BREAK ()
  "enters a LISP breakpoint. "
  (reset-editor-numeric-arg)
  (let ((*inside-lisp-breakpoint-p* t))
    #+Symbolics (si:break-internal 'boxer)
    #-Symbolics (break "Boxer"))
  (bw::flush-input)
  boxer-eval::*novalue*)

;; should include boxer-version, extensions loaded , any site info ? & instructions
;; sending mechanism (exit, button, ???)
(defvar *bug-report-address* "bug-boxer@pyxisystems.com")

(defvar *bug-report-instructions-rows*
  '(("Please describe the problem.")
    ("A small example which reproduces the problem would be especially helpful")
    ("When you are done, clicking on the \"Send Mail\" button will send this box")
    ("or you can save the box in a file by clicking on the \"Save File\" button")))

(defun make-bug-mail-template ()
  (let* ((system-string (system-version 'boxer))
         (xtens (mapcar #'boxer-extension-pretty-name *boxer-extensions*))
         (file-button (make-box (list
                                 (list
                                  (make-box (list (list "save"))
                                            'doit-box "MOUSE-CLICK")))
                                'data-box "Save File"))
         (mail-button (make-box (list
                                 (list
                                  (make-box (list
                                             (list "mail-document"
                                                   (make-box (list
                                                              (list *bug-report-address*)))))
                                            'doit-box "MOUSE-CLICK")))
                                'data-box "Send Mail"))
         (template (make-box (append *bug-report-instructions-rows*
                                     (list (list file-button mail-button))
                                     (list (list system-string))))))
    (unless (null xtens) (append-row template (make-row xtens)))
    (dotimes (i 2) (append-row template (make-row '())))
    (putprop file-button :name-only :boxtop) (shrink file-button)
    (putprop mail-button :name-only :boxtop) (shrink mail-button)
    (set-storage-chunk? template t)
    (mark-file-box-dirty template)
    (set-border-style template :thick)
    (modified template)
    template))

(boxer-eval::defboxer-primitive bu::bug-report-template () (make-bug-mail-template))
(boxer-eval::defboxer-primitive bu::boxer-bug-report-template () (make-bug-mail-template))

;;; we want to insert the bug-report-box, not just return the value so it can
;;; work from menus
(defboxer-command COM-BUG ()
  "sends a bug report about BOXER. "
  (reset-region)
  (reset-editor-numeric-arg)
  (multiple-value-bind (result error?)
      (boxer-eval::boxer-eval '(bu::bug-report-template))
    (cond ((not (null error?)))
          (t (insert-cha *point* result))))
  boxer-eval::*novalue*)

(defboxer-command COM-GOTO-TOP-LEVEL ()
  "moves to the top of the WORLD box. "
  (reset-region)
  (reset-editor-numeric-arg)
  (move-point (all-bp-values (box-first-bp-values *initial-box*)
			     (car (screen-objs *initial-box*))))
  (setq *outermost-screen-box-stack* nil)
  (set-outermost-box *initial-box* (car (screen-objs *initial-box*)))
  (enter (point-box))
  boxer-eval::*novalue*)

(defboxer-command com-print-screen ()
  "make a temporary ps file, and spool. Remove the temp file."
  (let ((*print-outermost-box* (null *editor-numeric-argument*)))
    (when (status-line-y-or-n-p (format nil "Print the screen on ~A (y or n) ?"
					*ps-postscript-printer*))
      (status-line-clear)
      (make-ps-file (outermost-screen-box) nil))
    boxer-eval::*novalue*))

;; phase out, use numeric arg as above
(defboxer-command com-print-screen-no-border ()
  "make a temporary ps file, and spool. Remove the temp file."
  (let ((*print-outermost-box* nil))
    (when (status-line-y-or-n-p (format nil "Print the screen on ~A (y or n) ?"
					*ps-postscript-printer*))
      (status-line-clear)
      (make-ps-file (outermost-screen-box) nil))
    boxer-eval::*novalue*))

(defboxer-command com-print-screen-to-file ()
  "Writes a postscript file from the screen into the file
   specified by Postscript-Filename"
  (let ((*print-outermost-box* (null *editor-numeric-argument*)))
    (when (status-line-y-or-n-p (format nil "Print the screen to file ~A: ?"
					*ps-file*))
      (status-line-clear)
      (make-ps-file (outermost-screen-box) *ps-file*))
    boxer-eval::*novalue*))

;; phase out, use numeric arg as above
(defboxer-command com-print-screen-to-file-no-border ()
  "Writes a postscript file from the screen into the file
   specified by Postscript-Filename"
  (let ((*print-outermost-box* nil))
    (when (status-line-y-or-n-p (format nil "Print the screen to file ~A: ?"
					*ps-file*))
      (status-line-clear)
      (make-ps-file (outermost-screen-box) *ps-file*))
    boxer-eval::*novalue*))




;;;; MODES

(defboxer-command com-toggle-vanilla-mode ()
  "Makes all keys use the original bindings"
  (reset-region) ; flush region because we'll be rebinding region special keys
  (cond ((fast-memq *global-top-level-mode* *active-modes*)
	 (remove-mode *global-top-level-mode*)
	 (boxer-editor-warning "Leaving Top Level Mode"))
	(t
	 (add-mode *global-top-level-mode*)
	 (boxer-editor-warning "Entering Top Level Mode")))
  boxer-eval::*novalue*)

(defboxer-command com-reset-modes ()
  "removes all active modes"
  (reset-modes)
  boxer-eval::*novalue*)




;;;; Places

;;; we use the process-doit-cursor-position vectors to record places in
;;; the editor.  It's pretty close to what we need (perhaps a little over
;;; specific) but better than inventing yet another object

(defvar *previous-place-stack* nil)

(defvar *recorded-place-alist* nil)

(defun %record-current-place ()
  (fill-doit-cursor-position-vector (make-process-doit-cursor-position)))

(defun %record-box-place (box)
  (let ((pvect (make-process-doit-cursor-position)))
    (multiple-value-bind (row cha-no) (box-first-bp-values box)
      (setf (process-doit-cursor-position-box pvect)    box
            (process-doit-cursor-position-row pvect)    row
            (process-doit-cursor-position-cha-no pvect) cha-no
            (process-doit-cursor-position-row-no pvect) 0)
      ;; these can't be filled until the box has been inserted into
      ;; the hierarchy so leave a marker
      (setf (process-doit-cursor-position-box-stack  pvect) :unfilled
            (process-doit-cursor-position-screen-box pvect) :unfilled))
    pvect))

;; in many ways similiar to RESTORE-POINT-POSITION except we use
;; MOVE-TO-BP to (possibly) animate cursor movement rather than
;; just setting the *point* values
;; we also expect to be moving which another difference with
;; RESTORE-POINT-POSITION which hopes that it is already in the right place
;;
;; This will lose when the place is at the end of a port chain
;; The only way to really win is to keep track of the full box chain
;; as we go along (via enter/exit methods).  The
;; FILL-DOIT-CURSOR-POSITION-VECTOR calculates the box stack, but can
;; lose if the outermost screen box is under a port which has already been
;; zoomed to full screen.
;;
(defun move-to-place (position-object)
  (let ((start-box (point-box))
        (to-box (process-doit-cursor-position-box position-object))
	(to-row (process-doit-cursor-position-row position-object))
	(to-cha-no (process-doit-cursor-position-cha-no position-object))
	(to-row-no (process-doit-cursor-position-row-no position-object))
	(to-box-stack (process-doit-cursor-position-box-stack position-object))
	(to-screen-box (process-doit-cursor-position-screen-box
			position-object)))
    (declare (ignore to-screen-box))
    (cond ((superior? to-box *initial-box*)
	   ;; looks like the box is still in the hierarchy
	   (cond ((eq (superior-box to-row) to-box)
	          ;; if the row is still a part of the box, we can go right there
	          (move-to-bp-values *point*
				     (values to-row
					     (min to-cha-no
					          (length-in-chas to-row))))
                  ;; make sure to ENTER the destination box
                  (enter to-box (not (superior? start-box to-box))))
                  (t
	           ;; use the row-no instead
	           (let ((row (row-at-row-no to-box
					     (min to-row-no
					          (1- (length-in-rows to-box))))))
                     (move-to-bp-values *point*
				        (values row
					        (min to-cha-no
						     (length-in-chas row))))
                     (let ((dest-box (superior-box row)))
                       (enter dest-box (not (superior? start-box dest-box)))))))
           ;; check if we need to fill other slots
           (when (eq to-box-stack :unfilled)
             (setf (process-doit-cursor-position-screen-box position-object)
                   (point-screen-box)
                   (process-doit-cursor-position-box-stack position-object)
                   (with-collection
	             (do* ((screen-box (point-screen-box)
			               (unless (null screen-box)
				         (superior-screen-box screen-box)))
		           (box (if (null screen-box)
			          (point-box)
			          (screen-obj-actual-obj screen-box))
		                (if (null screen-box)
			          (superior-box box)
			          (screen-obj-actual-obj screen-box))))
		          ((or (eq box *initial-box*)
		               (when (null box)
		                 (warn "The *point* is not in the editor hierarchy")
		                 t)))
	               (collect box))))))
	  (t
	   ;; if the box isn't in the editor, we could use the box
	   ;; stack to try and get close or else just punt
	   ;; for now, we'll punt by returning an ':error value
	   ':error))))

(defboxer-command com-set-mark ()
  "Records the current location on the previous place stack"
  (push (%record-current-place) *previous-place-stack*)
  (status-line-display 'boxer-editor-error "Mark set")
  boxer-eval::*novalue*)

(defboxer-command com-goto-previous-place ()
  "Go to the previous recorded place that is still in the editor"
  (cond ((null *previous-place-stack*))
	(t (let ((tos (pop *previous-place-stack*)))
             (move-to-place tos)
             (setq *previous-place-stack*
                   (nconc *previous-place-stack* (list tos))))))
  boxer-eval::*novalue*)

(defboxer-command com-exchange-point-and-mark ()
  "Put the mark where point is now, and point where the mark is now."
  (let ((place (pop *previous-place-stack*)))
    (push (%record-current-place) *previous-place-stack*)
    (move-to-place place)
    boxer-eval::*novalue*))

(defboxer-command com-point-to-register ()
  "Store current location of point in a register.
Argument is a character, naming the register."
  (beep) ;; alert the user that action needs to be taken
  ;; place names can now be strings....
  (multiple-value-bind (string cancelled?)
      (get-string-from-status-line
       "Type a Name for the Current Place(ctrl-G to stop):")
    (let ((existing (assoc string *recorded-place-alist* :test #'string-equal)))
      (cond ((not (null cancelled?)) (boxer-editor-warning "Cancelled !"))
            ((null existing)
             (status-line-display
              'boxer-editor-error
              (format nil "This place saved by the name:~A" string))
	     (push (cons string (%record-current-place)) *recorded-place-alist*))
	    (t
             (status-line-display
              'boxer-editor-error
              (format nil "Place ~A changed to this place" string))
	     (setf (cdr existing) (%record-current-place))))))
  boxer-eval::*novalue*)

#| ;; the old character based place naming scheme
  (status-line-display 'boxer-editor-error "Type a character to name place:")
  (multiple-value-bind (char bits)
      (get-character-input *boxer-pane* :plain-char-wanted? t)
    #-mcl (declare (ignore bits))
    (let ((existing (assoc char *recorded-place-alist* :test #'char-equal)))
      (cond ((editor-abort-char? char #+mcl bits)
             (boxer-editor-warning "Cancelled !"))
	    ((null existing)
             (status-line-display 'boxer-editor-error
			          (format nil "This place recorded as register:~C"
                                          (char-upcase char)))
	     (push (cons char (%record-current-place)) *recorded-place-alist*))
	    (t
             (status-line-display 'boxer-editor-error
			          (format nil "Register ~C changed to this place"
                                          (char-upcase char)))
	     (setf (cdr existing) (%record-current-place))))))
|#

(defboxer-command com-register-to-point ()
  "Move point to location stored in a register.
Argument is a character, naming the register."
  (beep) ;; alert the user that action needs to be taken
  (multiple-value-bind (string cancelled?)
      (get-string-from-status-line "Place to move to:")
    (let ((existing (assoc string *recorded-place-alist* :test #'string-equal)))
      (cond ((not (null cancelled?)) (boxer-editor-warning "Cancelled !"))
            ((null existing)
             (status-line-display
              'boxer-editor-error
              (format nil "Place ~A is undefined" string)))
	    (t
             (status-line-display 'boxer-editor-error
			          (format nil "Moving to Place in Register ~A"
                                          string))
             (let ((status (move-to-place (cdr existing))))
               (when (eq status ':error)
                 (setq *recorded-place-alist*
                       (fast-delq existing *recorded-place-alist*))
                 (boxer-editor-warning "Place ~A is no longer in the editor"
                                       string)))))))
    boxer-eval::*novalue*)

#|
  (status-line-display 'boxer-editor-error
                       "Type a character to name place register:")
  (multiple-value-bind (char bits)
      (get-character-input *boxer-pane* :plain-char-wanted? t)
    #-mcl (declare (ignore bits))
    (let ((existing (assoc char *recorded-place-alist* :test #'char-equal)))
      (cond ((editor-abort-char? char #+mcl bits)
             (boxer-editor-warning "Cancelled !"))
	    ((null existing)
	     (boxer-editor-warning "Register ~C is undefined" (char-upcase char)))
	    (t
             (status-line-display 'boxer-editor-error
			          (format nil "Moving to Place in Register ~C"
                                          (char-upcase char)))
             (let ((status (move-to-place (cdr existing))))
               (when (eq status ':error)
                 (setq *recorded-place-alist*
                       (fast-delq existing *recorded-place-alist*))
                 (boxer-editor-warning "Place ~C is no longer in the editor"
                                       char)))))))
|#

(defboxer-command com-clear-register ()
  "Clear a location stored in a register,
Argument is a character, naming the register."
  (multiple-value-bind (string cancelled?)
      (get-string-from-status-line "Place to clear:")
    (let ((existing (assoc string *recorded-place-alist* :test #'string-equal)))
      (cond ((not (null cancelled?)) (boxer-editor-warning "Cancelled !"))
            ((null existing)
             (boxer-editor-warning (format nil "No place named ~A" string)))
            (t (setq *recorded-place-alist*
                     (fast-delq existing *recorded-place-alist*))))))
  boxer-eval::*novalue*)

(defun bp-place= (bp place)
  (and (eq (bp-row bp)    (process-doit-cursor-position-row place))
       (=  (bp-cha-no bp) (process-doit-cursor-position-cha-no place))
       ;(or (null (bp-screen-box bp))
       ;    (eq (bp-screen-box bp)
       ;        (process-doit-cursor-position-screen-box place)))
       ))

;; stubbified for "UC clean" reimplementation
(defboxer-command com-forget-place ()
  "Clear the location stored in a register that records the current cursor position"
  )


#|
  (status-line-display 'boxer-editor-error "Register to Point:")
  (multiple-value-bind (char bits)
      (get-character-input *boxer-pane*)
    #-mcl (declare (ignore bits))
    (let ((existing (assoc char *recorded-place-alist* :test #'char-equal)))
      (status-line-display 'boxer-editor-error
			   (format nil "Register to Point: ~C" char))
      (cond ((editor-abort-char? char #+mcl bits)
             (boxer-editor-warning "Cancelled !"))
	    ((null existing)
	     (boxer-editor-warning "No place in Register ~C !" char))
	    (t (setq *recorded-place-alist*
                     (fast-delq existing *recorded-place-alist*))))))
|#

