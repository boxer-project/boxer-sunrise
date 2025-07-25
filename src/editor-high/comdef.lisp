;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER ; Base:8.  -*-

#|

 $Header: comdef.lisp,v 1.0 90/01/24 22:08:18 boxer Exp $

 $Log:	comdef.lisp,v $
;;;Revision 1.0  90/01/24  22:08:18  boxer
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

(in-package :boxer)

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
          `(PROGN
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


;;;; Variables...

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
           ;; sgithens TODO 2025-06-23
           ;;  (animate-cursor-move (screen-box screen-row)
           ;;                       (bp-row bp) (bp-cha-no bp))
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
         ;; sgithens TODO 2025-06-23
         ;; (animate-scrolling (bp-screen-box moving-bp))
       ))
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
              ;; sgithens TODO 2025-06-23
              ;; (animate-scrolling (bp-screen-box moving-bp))
            )
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
            )
            ;; move to lowest visible box, zoom, then try again
            ;; sgithens TODO 2025-06-23
            ;; (animate-cursor-move
            ;;  (visible-screen-obj-of-inferior-actual-obj
            ;;   new-box (bp-screen-box moving-bp))
            ;;  (first-inferior-row new-box) 0)
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
             (setq icon-on? T))
           (icon-off ()
             (setq icon-on? nil)))
          (icon-on)
          ;; TODO need a cross platform with-mouse-tracking
          #+lispworks (multiple-value-bind (final-x final-y)
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

;;;; new port stuff

;;;; dummy box is the box with the message "redirect me"
(defvar *dummy-box* nil)


;;;; generic port is the port created when no target is specified,
;;;; which can be redirected
(defvar *generic-port* nil)

;;; mouse mode utilities

(eval-when (load eval)
  (defbasic-mode retarget-port-mode
    (#+(or apple win32) bu::mouse-click #-(or apple win32) bu::mouse-middle
      com-redirect-generic-port)
    (#+(or apple win32) bu::mouse-click-on-graphics
      #-(or apple win32) bu::graphics-mouse-middle
      com-redirect-generic-port)
    (#+(or apple win32) bu::mouse-click-on-sprite
      #-(or apple win32) bu::sprite-mouse-middle
      com-redirect-generic-port))
)

;;; make a generic port, toggle the middle mouse cursor

(defun make-generic-port (&rest foo)
  (declare (ignore foo))
  (if (null *dummy-box*) (setq *dummy-box* (make-dummy-box)))
  (if (null *generic-port*)
    (progn (setq *generic-port* (port-to-internal *dummy-box*))
           (INSERT-CHA *POINT* *generic-port*)
           (set-mouse-cursor :retarget)
           (boxer-editor-message "Click on a Box to Select Port's Target")
           (add-mode (retarget-port-mode)))
    (boxer-editor-error "Use the generic port you have made."))
  boxer-eval::*novalue*)

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
  boxer-eval::*novalue*)

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
    (set-mouse-cursor :region-mode)
    (add-mode (region-mode)))

(defun exiting-region-mode (&optional (reset-mouse-cursor? t))
  (when reset-mouse-cursor? (reset-mouse-cursor))
  (remove-mode (region-mode)))

;;; cursor stuff
(defvar *suitcase-mode* nil)

;(defun change-cursor-to-suitcase ()
;  (set-mouse-cursor :suitcase)
;  (setq *suitcase-mode* t))

;(defun change-cursor-back-from-suitcase ()
;  (reset-mouse-cursor)
;  (setq *suitcase-mode* nil))

(defvar *editor-abort-chars* '((#\g 2) (#\. 2)))

(defun editor-abort-char? (ch &optional (bits 0))
  (let ((keystroke (list ch bits)))
    (if (member keystroke *editor-abort-chars* :test #'equal) t nil)))

;; used in keydef-high
(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *defined-input-device-platforms* nil
  "A list of the input platforms for which we know how to make input names")

;; pushed onto in MAKE-INPUT-DEVICES
(defvar *bound-input-device-platforms* nil
  "A list of all the platforms for which we have made input device names")
)


(defmacro define-input-devices (platform shift-list mouse-string)
  `(progn
    (unless (member ',platform *defined-input-device-platforms*)
      (push ',platform *defined-input-device-platforms*))
    (setf (get ',platform :shift-list)   ',shift-list
          (get ',platform :mouse-string) ',mouse-string)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun input-device-shift-list (platform)
  (get platform :shift-list))

(defun input-device-mouse-string (platform)
  (get platform :mouse-string))
)

;; popup vars & macros

(defvar *popup-area-doc-alist* nil)

(defvar *popup-docs* nil)

(defvar *popup-doc-font*
  (make-boxer-font '("Arial" 8)))

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
  (prop box) (set-always-zoom? box (boxer-eval::true? prop))
  ;; set prop
  (prop new-value) (change prop (boxer-eval::boxer-boolean new-value))
  ;; make
  (make-box '(("False")) 'data-box 'bu::always-zoom))

(define-box-property (:shrink-on-exit bu::shrink-on-exit)
  (prop box) (set-shrink-on-exit? box (boxer-eval::true? prop))
  (prop new-value) (change prop (boxer-eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::shrink-on-exit))

(define-box-property (:manual-size bu::manual-sizing)
  (prop box) (when (boxer-eval::true? prop)
               (set-bottom-right-hotspot-active? box t))
  (prop new-value) (change prop (boxer-eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::manual-sizing))

(define-box-property (:autofill bu::autofill)
  (prop box) (set-auto-fill? box (boxer-eval::true? prop))
  (prop new-value) (change prop (boxer-eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::autofill))

(define-box-property (:shrink-proof? bu::shrink-proof)
  (prop box) (set-shrink-proof? box (boxer-eval::true? prop))
  (prop new-value) (change prop (boxer-eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::shrink-proof))

;; link/file properties
(define-box-property (:read-only bu::read-only)
  (prop box) (set-read-only-box? box (boxer-eval::true? prop))
  (prop new-value) (change prop (boxer-eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::read-only))

(define-box-property (:relative-filename bu::relative-filename)
  (prop box) (set-relative-filename? box (boxer-eval::true? prop))
  (prop new-value) (change prop (boxer-eval::boxer-boolean new-value))
  (make-box '(("False")) 'data-box 'bu::relative-filename))

(define-box-property (:auto-load bu::auto-load)
  (prop box) (set-autoload-file? box (boxer-eval::true? prop))
  (prop new-value) (change prop (boxer-eval::boxer-boolean new-value))
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
        (tl-prop (boxer-eval::static-variable-value (symbol-value 'bu::new-box-properties))))
    (cond ((and (null local-prop) (eq local-prop tl-prop)) nil)
      ((null local-prop)
       ;; try recursing upwards
       (get-prop-from-props-box property-name
                                (boxer-eval::lookup-static-variable
                                 (superior-box (superior-box props-box))
                                 ;; just 1 superior-box will only get us to the box
                                 ;; where the props-box is already living
                                 'bu::new-box-properties)))
      (t local-prop))))

;; main properties interface
(defun initialize-new-box-properties (box &optional
                                          (props (boxer-eval::boxer-symeval
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
                                      (props (boxer-eval::boxer-symeval
                                              'bu::new-box-properties)))
  (funcall (get property :set-new-prop)
           (get-prop-from-props-box property props) new-value))

;; helps to get the wording right in dialogs
(defun top-level-props? ()
  (eq (boxer-eval::boxer-symeval 'bu::new-box-properties)
      (boxer-eval::static-variable-value
       (symbol-value 'bu::new-box-properties))))

