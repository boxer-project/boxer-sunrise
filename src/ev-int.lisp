;;;-*- Mode:Lisp; Syntax: Common-Lisp; Base: 10.;package: (Boxer :use (lisp sm pcl) :nicknames (box));-*-


#|


 $Header: ev-int.lisp,v 1.0 90/01/24 22:11:08 boxer Exp $

 $Log:	ev-int.lisp,v $
;;;Revision 1.0  90/01/24  22:11:08  boxer
;;;Initial revision
;;;






  Copyright 1986 - 1998 Regents of the University of California

  Additional Portions Copyright 1999 - 2010 PyxiSystems LLC

                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+

  This file contains functions which the evaluator needs to call but
  which have to be defined in the BOXER package, or functions which
  the evaluator defines in the EVAL package but which the editor
  needs to call.


Modification History (most recent at top)

11/09/10 don't force-graphics-output in top-level-eval-wrapper for opengl
11/23/09 top-level-eval-wrapper
 3/02/07 added with-screen-box-modification-tracking to top-level-eval-wrapper 
11/15/03 fill-doit-cursor-position-vector changed to take &optional bp arg so we
         can use it to record mouse-bp's as well as the *point*
         fixes to restore-point-position to make it more general purpose
10/15/03 display-force-output changed to force-graphics-output in *-eval-wrapper's
 2/15/03 merged current LW and MCl files
 1/15/03 restore-point-position more robust to handle cases where program cursor control
         has hidden the point
         fill-doit-cursor-position-vector now only warns when *boxer-system-hacker* is T
10/17/02 ALTERNATE-PLATFORM-CLICK-NAMES for mouse, HANDLE-BOXER-MOUSE-CLICK uses it
10/12/02 ALTERNATE-PLATFORM-INPUT-NAMES smart about multiple platform names
         HANDLE-BOXER-KEY uses it to check all possibilities
10/07/02 stubbified ALTERNATE-PLATFORM-INPUT-NAME in preparation for clean
         reimplementation of platform independent key/click handling
         Saved version of this file as ev-int-10-7-2002.lisp as a stubbified
         reference and ev-int-original.lisp
 1/18/02 handle-boxer-{key,mouse-click} changed to pass a VC of the doit box 
         instead of the NAME of the doit box when handling doit boxes
 1/06/02 convert-doit-result-for-printing binds font to default
10/29/01 fixed handle-boxer-mouse-click to recognized shifted mouse clicks
 3/04/01 changed the search order to check for alternate name binding BEFORE 
         regular click name.  This was put in to cover the case of mouse bindings 
         in mac worlds being ignored because of useless (beeping) defaults
 2/14/01 merged current LW and MCL files
10/17/02 ALTERNATE-PLATFORM-CLICK-NAMES for mouse, HANDLE-BOXER-MOUSE-CLICK uses it
10/12/02 ALTERNATE-PLATFORM-INPUT-NAMES smart about multiple platform names
         HANDLE-BOXER-KEY uses it to check all possibilities
10/07/02 stubbified ALTERNATE-PLATFORM-INPUT-NAME in preparation for clean
         reimplementation of platform independent key/click handling
         Saved version of this file as ev-int-10-7-2002.lisp as a stubbified
         reference and ev-int-original.lisp
 1/18/02 handle-boxer-{key,mouse-click} changed to pass a VC of the doit box 
         instead of the NAME of the doit box when handling doit boxes
 1/06/02 convert-doit-result-for-printing binds font to default
10/29/01 fixed handle-boxer-mouse-click to recognized shifted mouse clicks
 3/04/01 changed the search order to check for alternate name binding BEFORE 
         regular click name.  This was put in to cover the case of mouse bindings 
         in mac worlds being ignored because of useless (beeping) defaults
 2/14/01 merged current LW and MCL files
11/17/00 made fill-doit-cursor-position-vector more robust for possible 
         null (point-box)
 7/03/00 eval wrappers for lispworks now explicitly setq *EVALUATION-IN-PROGRESS*
         instead of LET binding it
 3/21/00 added alternate platform name lookup to handle-boxer-key/mouse-click
 6/23/98 Change handle-boxer-key-doit-box to always report errors via status line
         because inserting error boxes (especially for mouse clicks) can mess
         things up
 6/23/98 started logging changes: source = Boxer version 2.3


|#

#-(or lispworks mcl lispm) (in-package 'boxer)
#+(or lispworks mcl)       (in-package :boxer)



;;; Primitive should use this function to do NUMBERIZE, and give an
;;; error if they expect a number and get NIL.

;;; NUMBERIZE
(defun numberize-or-nil (x)
  (if (numberp x) 
      x
      (let ((box-rows (get-box-rows x)))
	(when (null (cdr box-rows))
	  (let ((entries (evrow-pointers (car (get-box-rows x)))))
	    (when (and entries (null (cdr entries)))
	      (let ((object (access-evrow-element x (car entries))))
		(when (numberp object) 
		  object))))))))

(defun numberize-or-error (x)
  (if (numberp x) x
      (let ((box-rows (get-box-rows x)))
	(if (and box-rows (null (cdr box-rows)))
	    (let ((entries (evrow-pointers (car (get-box-rows x)))))
	      (if (and entries (null (cdr entries)))
		  (let ((object (access-evrow-element x (car entries))))
		    (if (numberp object) 
			object
			(eval::primitive-signal-error :number-expected x)))
		  (eval::primitive-signal-error :number-expected x)))
	    (eval::primitive-signal-error :number-expected x)))))


;;; Similarly, this function does the work of DATAFY.
;;; DATAFY
(defun data-boxify (object)
  (make-vc `(,(make-evrow-from-entry object))))


;;;
;;; The EVAL function for the editor.
;;;


;;; RESTORE-POINT-POSITION:
;;; Tries to keep the *point* in the same place even if you cut the
;;; ground out from under it (usually by changing the box you happen
;;; to be in)
;;; This only works to one level of box.  Presumably we could be
;;; more elaborate and describe a path from the top of the world
;;; to our current position by a series of row numbers and character
;;; numbers but changes larger than one level of box are likely to
;;; be sufficiently radical that it is better to punt gracefully and
;;; move to the beginning of the last box that was known to be part of 
;;; the original hierarchy.

;;; Note: we can't use a stack of screen-box's because of the possibility
;;; that the user could call a REDISPLAY in his code which would render
;;; the EQness of particular screen-box's unreliable ESPECIALLY because
;;; of the deallocation/reallocation of screen objects

(defstruct process-doit-cursor-position
  box
  row
  cha-no
  row-no
  box-stack
  screen-box)

;; the box has to be in the editor hierarchy but NOT in a hidden
;; closet.  This means we have to be more careful as we walk up the 
;; hierarchy because the superior pointers are MAINTAINED in the
;; closet rows so as we walk upwards, we have to look back downwards
;; to make sure we are not in a hidden closet
(defun maybe-visible-in-editor? (box &optional lower-row)
  (cond ((eq box *initial-box*) t)
        ((null box) nil)
        ((and lower-row
              (eq (slot-value box 'closets) lower-row)
              (not (row-row-no box lower-row)))
         ;; this is the hidden in closet case
         nil)
        (t (maybe-visible-in-editor? (superior-box box) (superior-row box)))))

(defun restore-point-position (position-object &optional force-cursor-move?)
  (let ((original-box (process-doit-cursor-position-box
		       position-object))
	(original-row (process-doit-cursor-position-row
		       position-object))
	(original-cha-no (process-doit-cursor-position-cha-no
		          position-object))
	(original-row-no (process-doit-cursor-position-row-no
		          position-object))
	(original-box-stack (process-doit-cursor-position-box-stack
			     position-object))
	(original-screen-box (process-doit-cursor-position-screen-box
			      position-object)))
    (cond ((and (superior? original-box *initial-box*)
	        ;; is the box we started in still part of the hierarchy ?
	        ;; if so, be more detailed
	        (not (null (screen-obj-actual-obj original-screen-box)))
                (superior? (screen-obj-actual-obj original-screen-box)
                           *initial-box*))
           ;; make sure the screen-box we are in is also still
           ;; in the hierarchy
           (let ((new-superior-box (superior-box original-row)))
             (cond ((and (eq new-superior-box original-box)
                         (eq original-row (bp-row *point*))
                         (<=& original-cha-no
                              (length-in-chas original-row)))
                    ;; there is still a place for us in the new world
                    ;; so we don't need to do anything, unless we're
                    ;; supposed to move:
                    (when force-cursor-move?
                      (move-point (values original-row original-cha-no 
                                          (car (screen-objs
                                                original-box))))
                      ;(move-to-bp-values
                      ; *point*
                      ; (values original-row original-cha-no 
                      ;   (car (screen-objs original-box))))
                      ))
                   ((and (eq new-superior-box original-box)
                         (eq original-row (bp-row *point*)))
                    ;; the original row is still there but it seems to be
                    ;; shorter and we have fallen of the edge so move back
                    ;; to the edge of the new version of the row
                    (setf (bp-cha-no *point*)
                          (length-in-chas original-row)))
                   (t
                    ;; at this point, we know that the original row is 
                    ;; no longer in the original box
                    (let* ((new-length-in-rows
                            (length-in-rows original-box))
                           (new-row (if (< original-row-no
                                           new-length-in-rows)
                                      ;; either there are enough rows to
                                      ;; move to the equivalent place
                                      (row-at-row-no original-box
                                                     original-row-no)
                                      ;; or else we move to the last row
                                      (row-at-row-no
                                       original-box
                                       (1- new-length-in-rows))))
                           (new-length-in-chas (length-in-chas new-row))
                           (new-cha-no (if (<=& original-cha-no
                                                new-length-in-chas)
                                         ;; there is room on this row to
                                         ;; remain in the equivalent
                                         ;; place
                                         original-cha-no
                                         ;; there isn't room so move to 
                                         ;; the end of the row
                                         new-length-in-chas)))
                      (set-bp-row *point* new-row)
                      (unless (=& new-cha-no original-cha-no)
                        (setf (bp-cha-no *point*) new-cha-no))
                      (set-bp-screen-box *point* original-screen-box)
                      ;(move-to-bp-values *point*
                      ;	     (values new-row new-cha-no))
                      )))))
          ((maybe-visible-in-editor? original-box)
           ;; looks like the box we used to be in (visually) was removed
           ;; from the editor even though the original row still seems
           ;; to be part of the editor.  This can happen when we are inside
           ;; ports which are inside a box which is changed
           ;;
           ;; we walk DOWN the box stack looking for where the chain
           ;; of boxes was snapped during EVAL
           ;;
           ;; Note: with the new cursor motion and scrolling prims, there are now
           ;; more ways to "lose" the cursor 1/18/03
           (let* ((osb (outermost-screen-box)))
             (dolist (bsb original-box-stack)
               (let ((sb (car (displayed-screen-objs bsb))))
                 (when (and sb (superior? sb osb))
                   (move-point (screen-box-first-visible-bp-values sb))
                   (setf (bp-screen-box *point*) sb)
                   (enter bsb)
                   (return nil)))))
;           (let ((last-connected-box *initial-box*))
;             (dolist (box (nreverse original-box-stack))
;               (cond ((and (superior? box *initial-box*)
;                           (not (null (displayed-screen-objs box))))
;                      (setq last-connected-box box))
;                     (t
;                      (move-point
;                       (box-first-bp-values last-connected-box))
;                      ;			    (move-to-bp-values
;                      ;			     *point* (box-first-bp-values last-connected-box))
;                      (setf (bp-screen-box *point*)
;                            ;; There you go again....
;                            (car (displayed-screen-objs
;                                  last-connected-box)))
;                      ;; reset the binding root
;                      (enter last-connected-box)
;                      ;; now stop
;                      (return nil)))))
           )
	  (t 
	   ;; uh oh, looks like the original box we were in is no longer
	   ;; part of the hierarchy so move upwards until we reach
	   ;; familiar territory
	   (dolist (old-box (cdr original-box-stack))
	     (when (superior? old-box *initial-box*)
	       ;; box is still connected to the hierarchy
	       (move-point (box-first-bp-values old-box))
               ;(move-to-bp-values *point* (box-first-bp-values old-box))
	       ;; blechh, however, in the general case, the last
	       ;; possible superior may not even be visible so we need
	       ;; to handle THAT first.
	       ;; Solution probably involves walking up the stack of
	       ;; screen-boxes to decide on a reasonable outermost
	       ;; screen box and then making sure the new point-screen-box
	       ;; is part of the new outermost-screen-box.  
               (let ((old-screen-box (car (displayed-screen-objs old-box))))
                 (cond ((not (null old-screen-box))
                        ;; box was visible so we can just set
	                (setf (bp-screen-box *point*) old-screen-box)
                        (enter old-box))
                       (t 
                        ;; box is not visible, this can mean that the box was
                        ;; in some previous outermost box
                        (let ((found-sb nil))
                          (loop
                            (let ((nob (pop *outermost-screen-box-stack*)))
                              (cond ((null nob) (return nil))
                                    (t (set-outermost-box 
                                        (screen-obj-actual-obj nob) nob))))
                            #-opengl (redisplay)
                            (when (not (null (car (displayed-screen-objs old-box))))
                              (setq found-sb t)
                              (return nil)))
                          (cond (found-sb 
                                 (move-point (box-first-bp-values old-box))
                                 (setf (bp-screen-box *point*)
                                       (car (displayed-screen-objs old-box)))
                                 (enter old-box))
                                (t (com-goto-top-level)))))))
               (return nil)))))))

(defmacro maintaining-point-position (&body body)
  `(progn
     (fill-doit-cursor-position-vector eval::*process-doit-cursor-position*)
     (unwind-protect
	  (progn . ,body)
       (restore-point-position eval::*process-doit-cursor-position*))))

(defun fill-doit-cursor-position-vector (vector &optional (bp *point*))
  (setf (process-doit-cursor-position-box vector) (bp-box bp))
  (setf (process-doit-cursor-position-row vector) (bp-row bp))
  (setf (process-doit-cursor-position-cha-no vector) (bp-cha-no bp))
  (setf (process-doit-cursor-position-row-no vector)
	(when (bp-box bp) (row-row-no (bp-box bp) (bp-row bp))))
  (setf (process-doit-cursor-position-box-stack vector)
	(with-collection
	    (do* ((screen-box (bp-screen-box bp)
			      (unless (null screen-box)
				(superior-screen-box screen-box)))
		  (box (if (null screen-box)
			   (bp-box bp)
			   (screen-obj-actual-obj screen-box))
		       (if (null screen-box)
			   (superior-box box)
			   (screen-obj-actual-obj screen-box))))
		((or (eq box *initial-box*)
		     (when (null box)
                       (when *boxer-system-hacker*
		         (warn "The *point* is not in the editor hierarchy"))
		       t)))
	      (collect box))))
  (setf (process-doit-cursor-position-screen-box vector)
	(bp-screen-box bp))
  vector)

(defvar *mouse-cursor-change-during-eval?* t)

;;; this can eventually update the mouse cursor periodically
;;; during the polling function 
(defmacro with-mouse-cursor-on (&body body)
  `(unwind-protect
	(progn
	  (unless (null *mouse-cursor-change-during-eval?*)
	    (set-mouse-cursor :eval))
	  . ,body)
     (reset-mouse-cursor)))

;;; Hack floating point lossage

#+lcl3.0
(defmacro with-floating-point-exceptions-handled (&body body)
  `(lcl::with-floating-point-traps
       (nil '(lcl:floating-point-overflow lcl:floating-point-underflow
	      lcl:floating-point-invalid-operation lcl:division-by-zero))
     (progn . ,body)))

#-lcl3.0
(defmacro with-floating-point-exceptions-handled (&body body)
  `(progn . ,body))

;;; Lots of things want to be queued during and eval and processed afterwards
;;; This has to be wrapped around the OUTERMOST invocation.  
;;; Specifically, printing of returned values has to take place BEFORE the
;;; cleanup forms inside of with-editor-mutation-queueing.
;;; NOTE: #+lispworks explicit setq with unwind-protect because interrupt handlers
;;;       can't extract the boxer process binding for *evaluation-in-progress?*
(defmacro top-level-eval-wrapper (&body body)
  `(prog1
       (maintaining-point-position
         (bw::with-suppressed-exposure-handling
         (with-mouse-cursor-on
           (with-modified-queueing
             (with-non-lisp-structure-deallocation
               (drawing-on-window (*boxer-pane*)
                 ;; for the benefit of turtle graphics
                 ;; do drawing-on-window once instead of with EVERY draw line
                 #-opengl
                 (force-graphics-output)
                 ;; do the force-output so the cursor will disappear immediately.
                 (with-port-retargetting
                   (unwind-protect
                       (let (#-lispworks (*evaluation-in-progress?* t)
                             (eval::*doit-key-process* eval::*current-process*))
                         #+lispworks (setq *evaluation-in-progress?* t)
                         (with-editor-mutation-queueing
                           (with-floating-point-exceptions-handled
                             (with-screen-box-modification-tracking
                               (with-absolute-position-cache-recording
                                 . ,body)))))
                     #+lispworks (setq *evaluation-in-progress?* nil)))))))))
     (unless (null *post-eval-hook*)
       (if (listp *post-eval-hook*) 
           (dolist (f *post-eval-hook*) (funcall f))
           (funcall *post-eval-hook*)))))


(defmacro background-eval-wrapper (&body body)
  `(with-modified-queueing
     (with-non-lisp-structure-deallocation
       (drawing-on-window (*boxer-pane*)
         ;; for the benefit of turtle graphics
         ;; do drawing-on-window once instead of with EVERY
         ;; draw line
         (force-graphics-output)
         ;; do the force-output so the cursor will
         ;; disappear immediately.
         (with-port-retargetting
           (with-editor-mutation-queueing
             (unwind-protect
                 (let (#-lispworks(*evaluation-in-progress?* t))
                   #+lispworks (setq *evaluation-in-progress?* t)
                   (with-absolute-position-cache-recording . ,body))
               #+lispworks (setq *evaluation-in-progress?* nil)
               )))))))

;;; Set this variable to T to cause the cursor to be permanently restored
;;; to process start position when the process finishes.  NIL means
;;; just restore it temporarily while doing DOIT-PRINT-RETURNED-VALUE.
;;; NIL doesn't work right.
(defvar *doit-restore-cursor-position* t)

(defun returned-value-printable-place? ()
  (not (eq (class-name (class-of (point-row))) 'name-row)))

(defun error-value-printable-place? ()
  (not (or (eq (class-name (class-of (point-row))) 'name-row)
	   ;; underneath a graphics box
	   (do* ((screen-box (point-screen-box) (superior screen-box))
		 (box (when (screen-box? screen-box)
			(screen-obj-actual-obj screen-box))
		      (when (screen-box? screen-box)
			(screen-obj-actual-obj screen-box))))
		((not (box? box)) nil)
	     (when (and (graphics-box? box)
			(display-style-graphics-mode?
			 (display-style-list box)))
	       (return t))))))

(defvar *last-unprintable-returned-value* eval::*novalue*)

(defvar *last-unprintable-error-box* eval::*novalue*)

(defun unprintable-sound () (beep))

(defun unprintable-returned-value-warning (value)
  (setq *last-unprintable-returned-value* value)
  (unprintable-sound)
  (status-line-display
   'boxer-editor-error 
   "Can't print returned value, Invisible-Value returns it"))

(defun unprintable-error-box-warning (error)
  (setq *last-unprintable-error-box* error)
  (unprintable-sound)
  (status-line-display 'boxer-editor-error
		       "Can't print error box, Invisible-Error returns it"))

(defun print-returned-value-when-possible (result &optional error?)
  (cond ((and error? (not (error-value-printable-place?)))
	 (unprintable-error-box-warning result))
	((returned-value-printable-place?)
	 (doit-print-returned-value result))
	((eq result eval::*novalue*))
	(t (unprintable-returned-value-warning result))))

(defun doit-internal (&optional list-to-eval)
  (if *evaluation-in-progress?* 
      (boxer-editor-error "Can't do COM-DOIT inide COM-DOIT")
      (unwind-protect
	   (let ((row (point-row))
		 (process eval::*current-process*))
	     (unless (name-row? row)
	       (top-level-eval-wrapper
		(multiple-value-bind (result error?)
		    (eval::boxer-eval (or list-to-eval (eval-objs row)))
		  ;; this test not be good enough.  what if
		  ;; another process moves the cursor, and then
		  ;; resumes this process?  Well, maybe that's
		  ;; OK -- we can define that to be OK.  This
		  ;; will keep doit-print-returned-value from
		  ;; messing with other processes cursor positions,
		  ;; yet allow keyboard macros to work.
		  ;; maybe we should store the invoking key/function
		  ;; in the doit-cursor-position, and base it on
		  ;; who did started it originally?
		  (unless (eq process eval::*current-process*)
		    (restore-point-position
		     eval::*process-doit-cursor-position* t))
		  (print-returned-value-when-possible result error?)
		  (unless (or *doit-restore-cursor-position*
			      (eq process eval::*current-process*))
		    (restore-point-position 
		     (eval::process-variable
		      process
		      eval::*process-doit-cursor-position*) t))))))
	(reset-editor-numeric-arg))))


;; flush?
(defun exit-edit-box-internal ()
  (doit-internal '(bu::process-finish)))
    

;;;
;;; *** The main Editor/Evaluator functions. ***
;;;
(defun eval::eval-point-row ()
  (eval::boxer-eval (eval-objs (point-row))))

(defun eval::eval-top-level (form)
  (top-level-eval-wrapper
   (eval::boxer-eval form)))


(defun eval::keyboard-idle-background-process-restart ()
  (let ((bg-process (eval::next-restartable-process)))
    (unless (null bg-process)
      (let ((eval::*initial-poll-count* eval::*background-poll-count*)
	    (eval::*poll-count* eval::*background-poll-count*))
	(background-eval-wrapper
	 (eval::boxer-eval nil :process-state bg-process))))))

;;;
;;;Eval with timing for debugging:
;;;
(defun eval::F (&optional exp)
  (top-level-eval-wrapper
    (when (null exp) (setq exp (eval-objs (point-row))))
    (time (eval::boxer-eval exp))))

#+symbolics
(defun Fwc (&optional exp)
  (top-level-eval-wrapper
    (when (null exp) (setq exp (eval-objs (point-row))))
    (time (eval::boxer-eval exp) t)))

#+symbolics
(defun Fwm (&optional exp)
  (top-level-eval-wrapper
    (when (null exp) (setq exp (eval-objs (point-row))))
    (meter:with-monitoring  t (eval::boxer-eval exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BOXER Editor interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; we fall through to these functions if a click or key name is not 
;; found.  The theory is that there might be an existing binding for 
;; the mac version, so try that name before giving up
;; For example, a microworld from the mac might have OPTION-MOUSE-CLICK
;; bound.  The corresponding input on the PC would generate an ALT-MOUSE-CLICK
;; input name which would, in all likelyhood not be bound so we would then
;; come to these functions which would take the ALT-MOUSE-CLICK name and then
;; produce the OPTION-MOUSE-CLICK
(defvar *try-alternate-platform-names* T)

;; stubbified in anticipation of "clean" reimplementation (10/07/02)
;; "clean" replacement should take a key/click name and return the
;; analogous key/click name in another platform
;; in addition, we should try and link up with DEFINE-INPUT-DEVICES & friends
;; in keydef-high.lisp

;(defun alternate-platform-input-name (name) name)

;; alternate platform names should return a list instead to allow for the
;; future possibility of multiple alternate platforms...
;; In addition, we have handle-boxer-input (keydef-high.lisp) pass key codes
;; and shift bits to handle-boxer-key to make this work better
;;
;; We don't want to precons all the possible alternate input names, but we will
;; use a lookup array as a cache of alternate names
;; Note: We also look for alternative names HERE, instead of producing the entire
;; list of possible names for handle-boxer-input in order to limit the check 
;; for alternatives to the case where an existing binding is missing

(defun alternate-platform-input-names (code bits)
  (let ((existing (aref *alternate-key-names* code bits)))
    (cond ((eq existing :no-alternates) nil)
          ((not (null existing)) ; cached alternates...
           existing)
          (t 
           ;; calculate and cache 
           (let ((handled-shifts 
                  (unless (zerop& bits)
                    (list (nth (1-& bits) (input-device-shift-list 
                                           *current-input-device-platform*)))))
                 (alternates nil))
             (dolist (platform *defined-input-device-platforms*)
               (let ((other-shift (unless (zerop& bits)
                                    (nth (1-& bits)
                                         (input-device-shift-list platform)))))
                 (unless (or (null other-shift)
                             (member other-shift handled-shifts
                                     :test #'string-equal))
                   (push other-shift handled-shifts)
                   (push (intern-in-bu-package
                          (concatenate 'string
                                       other-shift "-"
                                       ;; get the plain (unshifted) key name
                                       (string (lookup-key-name code 0)))) 
                         alternates))))
             (cond ((null alternates)
                    (setf (aref *alternate-key-names* code bits) 
                          :no-alternates)
                    nil)
                   (t (setf (aref *alternate-key-names* code bits)
                            alternates))))))))

;;; Boxer keys are functions that take no arguments, or data boxes.
;;; Primitives get funcalled, data boxes get copied&inserted,
;;; and user functions get evaluated.
;;; We avoid the evaluator unless the object is a doit box.
;;; In all cases the result of the "evaluation" gets inserted.
;;; Return NIL if the function was undefined or invalid.
;;; These two functions can't be called from inside the evaluator
;;; because of the trigger check and possible call to eval.
(defun handle-boxer-key (name keycode shift-bits)
  (let ((value (or (mode-key name) 
                   (let ((key-value (eval::boxer-symeval name)))
                     (unless (eq key-value eval::*novalue*)
                       key-value))
                   (when *try-alternate-platform-names*
                     (dolist (alt-name (alternate-platform-input-names keycode
                                                                       shift-bits)
                                       eval::*novalue*)
                       (let ((alt-value (eval::boxer-symeval alt-name)))
                         (unless (eq alt-value eval::*novalue*)
                           (return alt-value))))))))
    (cond ((eq value eval::*novalue*) nil)
	  ((eval::boxer-function? value)
	   (cond ((eval::compiled-boxer-function? value)
		  (let ((returned-value
			 (funcall(eval::compiled-boxer-function-object value))))
		    (unless (or (null returned-value)
				(eq returned-value eval::*novalue*))
		      (insert-cha *point*
				  (convert-doit-result-for-printing
				   returned-value))))
		  (maybe-handle-trigger-in-editor))
		 (t (error "Unusual object ~S in handle-boxer-key" value)))
	   t)
	  ((doit-box? value)
	   (handle-boxer-key-doit-box (virtual-copy value))
	   t)
	  ((or (data-box? value)
	       (sprite-box? value))
	   (handle-boxer-key-data-box value)
	   t)
	  ((port-box? value)
	   (if (doit-box? (ports value))
	       (handle-boxer-key-doit-box name)
	       (handle-boxer-key-data-box value))
	   t)
	  (t nil))))

;; mostly right, it still doesn't hack the translation of the actual click
;; name i.e. GRAPHICS-MOUSE-MIDDLE <==> MOUSE-CLICK-ON-GRAPHICS but since we
;; are unlikely to be looking at any (working) LispM code, it's probably OK
(defun alternate-platform-click-names (click bits area)
  (let* ((lookup-table (if (null area)
                           *alternate-mouse-click-name-translation-table*
                         (get area 'alternate-click-name-table)))
         (existing (aref lookup-table click bits)))
    (cond ((eq existing :no-alternates) nil)
          ((not (null existing)) ; cached alternates...
           existing)
          (t 
           ;; calculate and cache 
           (let ((handled-shifts 
                  (unless (zerop& bits)
                    (list (nth (1-& bits) (input-device-shift-list 
                                           *current-input-device-platform*)))))
                 (alternates nil))
             (dolist (platform *defined-input-device-platforms*)
               (let ((other-shift (unless (zerop& bits)
                                    (nth (1-& bits)
                                         (input-device-shift-list platform)))))
                 (unless (or (null other-shift)
                             (member other-shift handled-shifts
                                     :test #'string-equal))
                   (push other-shift handled-shifts)
                   (push (intern-in-bu-package
                          (concatenate 'string
                                       other-shift "-"
                                       (string (lookup-click-name click 0 area)))) 
                         alternates))))             
             (cond ((null alternates) 
                    (setf (aref lookup-table click bits) :no-alternates)
                    nil)
                   (t (setf (aref lookup-table click bits) alternates))))))))

;;; Look up the key click in the box where the mouse button was pressed.
;;; If the button definition is a data box or doit box, then move
;;; the cursor to the position (via com-mouse-move-point) before
;;; executing the code.  Otherwise, just execute the primitive.
;;; This is kind of messed up, especially wrt. triggers.
;;; Also it would be nice not to have the distinction between
;;; user/system code here, but until we figure it out better
;;; we have to have it like this, or else clicking left to shrink
;;; a box will make its entry trigger get run and double clicking
;;; right to zoom a box will first enter it and thus open it and
;;; disturb its current open/closed state.
;;
;; 3/5/01 changed the search order to check for alternate name binding BEFORE
;; regular click name.

(defun handle-boxer-mouse-click (name window x y mouse-bp clicked?
                                      click bits area)
  (unless (null (bp-row mouse-bp))
    (let ((value (let ((eval::*lexical-variables-root* (bp-box mouse-bp)))
		   (or (mode-key name) 
                       (when *try-alternate-platform-names*
                         (dolist (altname (alternate-platform-click-names 
                                           click bits area))
                           (let ((altvalue (eval::boxer-symeval altname)))
                             (unless (eq altvalue eval::*novalue*)
                               (setq name altname)
                               (return altvalue)))))
                       (eval::boxer-symeval name)))))
      (cond ((eq value eval::*novalue*) nil)
	    ((eval::boxer-function? value)
	     (cond ((eval::compiled-boxer-function? value)
		    (let ((result
			   (funcall
			    (eval::compiled-boxer-function-object value)
			    window x y mouse-bp clicked?)))
		      (unless (or (null result) (eq result eval::*novalue*))
			(insert-cha
			 *point* (convert-doit-result-for-printing result))))
		    (maybe-handle-trigger-in-editor)
		    t)
		   (t
		    (com-mouse-move-point window x y mouse-bp clicked?)
		    (handle-boxer-key-doit-box name)
		    t)))
	    ((data-box? value)
	     (com-mouse-move-point window x y mouse-bp clicked?)
	     (handle-boxer-key-data-box value)
	     t)
	    ((doit-box? value)
	     (com-mouse-move-point window x y mouse-bp clicked? t)
	     (handle-boxer-key-doit-box (virtual-copy value)) 
	     t)
	    (t nil)))))

(defun handle-boxer-key-doit-box (value)
  (if *evaluation-in-progress?*
      (boxer-editor-error "Can't do DOIT keys while evaluating an expression")
      (let ((process eval::*current-process*))
	(top-level-eval-wrapper
	 (multiple-value-bind (result error?)
	     (eval::boxer-eval (list value))
	   ;; this test is wrong -- see doit-print-returned-value above.
	   (cond ((eq process eval::*current-process*)
		  (cond ((not (null error?))
			 (unprintable-error-box-warning result))
			((and (returned-value-printable-place?)
			      (not (or (null result)
				       (eq result eval::*novalue*))))
			 (insert-cha *point*
				     (convert-doit-result-for-printing
				      result)))
			((eq result eval::*novalue*))
			(t (unprintable-returned-value-warning result))))
		 (t (restore-point-position
		     eval::*process-doit-cursor-position* t)
		     (cond ((and error? (not (error-value-printable-place?)))
			    (unprintable-error-box-warning result))
			   ((returned-value-printable-place?)
			    (doit-print-returned-value result))
			   ((eq result eval::*novalue*))
			   (t (unprintable-returned-value-warning result)))
		    (unless *doit-restore-cursor-position*
		      (restore-point-position
		       (eval::process-variable 
			process 
			eval::*process-doit-cursor-position*)))))))
	(maybe-handle-trigger-in-editor))))

(defun handle-boxer-key-data-box (value)
  (let ((eval::*primitive-shadow-warning-on?* nil))
    ;; don't warn about shadowing
    (if (name-row? (point-row))
	(unprintable-returned-value-warning (copy-top-level-box value))
	(insert-cha *point* (copy-top-level-box value)))
    (maybe-handle-trigger-in-editor)))

(defun convert-doit-result-for-printing (result)
  (let ((*current-font-descriptor* *default-font-descriptor*))
    (cond ((numberp result) (make-box `((,result))))
	  ((data-box? result) result)
	  ((fast-eval-data-box? result)
	   (top-level-print-vc result))
	  ((fast-eval-port-box? result)
	   (top-level-print-vp result))
	  (t (error "Unknown object ~S in HANDLE-BOXER-KEY" result)))))
  
;; We must check for returned-value, which might
;; be an error.  We ignore other returned value.
;; BOXER-EVAL returns T as a second value if there was
;; an error.
(defun maybe-handle-trigger-in-editor ()
  (when (not (null eval::*trigger-list-to-run*))
    (multiple-value-bind (result errorp)
	(eval::eval-top-level (prog1 eval::*trigger-list-to-run*
				(setq eval::*trigger-list-to-run* nil)))
      (when (and errorp (box? result)) (insert-cha *point* result)))))


;;;
;;; Input Prompting
;;;
(defun arglist-for-prompter (object)
  (mapcar #'(lambda (u)
	      (if (consp u) (cadr u) u))
	  (cond ((or (numberp object) (data-box? object)) nil)
		((eq object eval::*novalue*)
		 (boxer-editor-error "No Box with this name"))
		((symbolp object) 
		 (arglist-for-prompter (eval::boxer-symeval object)))
		((and (eval::possible-eval-object? object)
                      (eval::compiled-boxer-function? object))
		 (eval::boxer-function-arglist object))
		((doit-box? object)
		 (eval::boxer-function-arglist (eval::cached-code-editor-box object)))
		((port-box? object)
		 (arglist-for-prompter (get-port-target object)))
		(t (boxer-editor-error "Object is not a boxer function")))))


;;;
;;; Status line messages
;;;

(defvar *evaluator-helpful* T)

(defun eval::evaluator-helpful-message (message)
  (when *evaluator-helpful*
    (boxer-editor-warning message)))


;;;
;;; init
;;;

(defun eval::setup-evaluator ()
  (eval::initialize-special-stack-frame-initializers)
  (eval::init-global-eval-vars)
  (setq eval::*false*
	(boxer::make-vc (list (boxer::make-evrow-from-entry 'bu::false))))
  (setq eval::*true*
	(boxer::make-vc (list (boxer::make-evrow-from-entry 'bu::true))))
  #+sun (setup-boxer-send)
  nil)
