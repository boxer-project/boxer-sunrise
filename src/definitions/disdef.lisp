;; -*- Mode: LISP;Syntax:Common-Lisp; Package:BOXER;-*-

#|


 $Header: disdef.lisp,v 1.0 90/01/24 22:09:31 boxer Exp $


 $Log:	disdef.lisp,v $
;;;Revision 1.0  90/01/24  22:09:31  boxer
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

    This file contains variables and macros for the display code.

Modification History (most recent at the top)

 2/ 2/12 allocate-screen-obj-internal: graphics mode check should only be done for boxes
 1/22/12 revised all intial-no-of-free-*** upwards
 1/21/12 sprite-screen-box capabilities added
12/01/10 force-graphics-output changed to use only #+'s
11/08/10 *{default,current}-font-descriptor* init moved to redisplay init
11/14/08 *repaint-during-eval?*
 3/01/07 with-screen-box-modification-tracking, queue-modified-graphics-box,
         *screen-boxes-modified*
 6/12/06 changed the def-redisplay-init for opengl stuff
10/15/03 added force-graphics-output for generic graphics buffer flushing
 2/11/03 merge current LW and MCL files
 2/13/01 merged current LW and MCL files
12/08/99 added #+lwwin (initialize-colors) to def-redisplay-init
 9/14/99 removed calls to maintaining-drawing-font because it is now folded into
         rebind-font-info
 1/11/99 defer font inits until after the *boxer-pane* exists
 1/03/99 added font inits for PC
 5/02/98 make with-font-hacking use maintaing-drawing-font macro
 5/01/98 more fixes to with-font-hacking for non zero start cha no
 4/29/98 make with-font-hacking intialize to closest-bfd instead of default
         especially inportant when start-cha-no > 0
 4/28/98 added macrolet'd recheck-font-state to with-font-hacking
 4/20/98 changed with-font-hacking to detect and use pen colors
         added initializations for various font parameters
 4/18/98 Change log started


|#

(in-package :boxer)

;;; Graphics defs and macros

; (defvar *use-glist-performance* t
;   "Whether or not to use the new c-buffering for the opengl drawing, although this
;   can be used for other types of graphics command list performance work that is in
;   progress as well.")

(defvar *use-opengl-framebuffers* t
  "t or nil. If t, then use openGL framebuffers to back the graphics display list drawing
   on turtle canvases.")

(defvar *reload-shaders* nil
  "When set to true, reload the shader programs before next repaint.")

(DEFVAR *DEFAULT-GRAPHICS-SHEET-WIDTH* 320.)

(DEFVAR *DEFAULT-GRAPHICS-SHEET-HEIGHT* 200.)

(DEFVAR *MAKE-TURTLE-WITH-NEW-GRAPHICS-BOX* NIL
        "Determines if graphics boxes are created with a turtle already in it. ")

(DEFSUBST MAKE-SCREEN-CHA (ACTUAL-CHA)
          ACTUAL-CHA)

(DEFSUBST SCREEN-CHA? (SC) (CHARACTERP SC))

(DEFVAR FREE-SCREEN-ROWS NIL
        "A list of free screen-rows.")

(DEFVAR FREE-SCREEN-BOXS NIL
        "A list of free screen-boxs.")

(DEFVAR FREE-GRAPHICS-SCREEN-BOXS NIL
        "A list of free graphics-screen-boxs.")

(defvar free-sprite-screen-boxs nil
  "A list of free sprite-screen-boxs.")

;;; these numbers can all be much larger
;;; they were devised in a world of megabyte sized memory
(DEFVAR INITIAL-NO-OF-FREE-SCREEN-ROWS 150)

(DEFVAR INITIAL-NO-OF-FREE-SCREEN-BOXS 300)

(DEFVAR INITIAL-NO-OF-FREE-GRAPHICS-SCREEN-BOXS 20)

(DEFVAR initial-no-of-free-sprite-screen-boxs 20)

(DEFUN ALLOCATE-GRAPHICS-SCREEN-BOX-INTERNAL (GRAPHICS-BOX)
       (LET ((GRAPHICS-SCREEN-BOX (OR (POP FREE-GRAPHICS-SCREEN-BOXS)
                                      (MAKE-INSTANCE 'GRAPHICS-SCREEN-BOX))))
            (RE-INIT GRAPHICS-SCREEN-BOX GRAPHICS-BOX)
            GRAPHICS-SCREEN-BOX))

(defun allocate-sprite-screen-box-internal (sprite-box)
  (let ((sprite-screen-box (or (pop free-sprite-screen-boxs)
                               (make-instance 'sprite-screen-box))))
    (re-init sprite-screen-box sprite-box)
    sprite-screen-box))

(DEFUN ALLOCATE-SCREEN-ROW-INTERNAL (ACTUAL-ROW)
       (LET ((SCREEN-ROW (OR (POP FREE-SCREEN-ROWS) (MAKE-INSTANCE 'SCREEN-ROW))))
            (RE-INIT SCREEN-ROW ACTUAL-ROW)
            SCREEN-ROW))

(DEFUN ALLOCATE-SCREEN-BOX-INTERNAL (ACTUAL-BOX)
       (LET ((SCREEN-BOX (OR (POP FREE-SCREEN-BOXS) (MAKE-INSTANCE 'SCREEN-BOX))))
            (RE-INIT SCREEN-BOX ACTUAL-BOX)
            SCREEN-BOX))

(DEFUN ALLOCATE-GRAPHICS-SCREEN-SHEET-INTERNAL (GRAPHICS-SHEET)
       (MAKE-GRAPHICS-SCREEN-SHEET GRAPHICS-SHEET))


(DEFUN ALLOCATE-SCREEN-OBJ-INTERNAL (ACTUAL-OBJ)
       (COND ((and (box? actual-obj)
                   (display-style-graphics-mode? (display-style-list actual-obj)))
              (let ((gi (graphics-info actual-obj)))
                (cond ((graphics-sheet? gi)
                       (ALLOCATE-GRAPHICS-SCREEN-BOX-INTERNAL ACTUAL-OBJ))
                  ((screen-obj? gi)
                   (allocate-sprite-screen-box-internal actual-obj))
                  (t
                   (barf "Can't allocate a graphics screen obj for ~S"
                         actual-obj)))))
             ((and (port-box? actual-obj)
                   (graphics-box? (ports actual-obj))
                   (display-style-graphics-mode? (display-style-list actual-obj)))
              (ALLOCATE-GRAPHICS-SCREEN-BOX-INTERNAL ACTUAL-OBJ))
             ((BOX? ACTUAL-OBJ) (ALLOCATE-SCREEN-BOX-INTERNAL ACTUAL-OBJ))
             ((ROW? ACTUAL-OBJ) (ALLOCATE-SCREEN-ROW-INTERNAL ACTUAL-OBJ))
             ((GRAPHICS-SHEET? ACTUAL-OBJ)
              (ALLOCATE-GRAPHICS-SCREEN-SHEET-INTERNAL ACTUAL-OBJ))
             (T (BARF  "Can't allocate a screen-obj for ~S" ACTUAL-OBJ))))

(DEFUN DEALLOCATE-SCREEN-ROW-INTERNAL (SCREEN-ROW)
       (de-allocate-init screen-row)
       (PUSH SCREEN-ROW FREE-SCREEN-ROWS))

(DEFUN DEALLOCATE-SCREEN-BOX-INTERNAL (SCREEN-BOX)
       (de-allocate-init screen-box)
       (PUSH SCREEN-BOX FREE-SCREEN-BOXS))

(DEFUN DEALLOCATE-GRAPHICS-SCREEN-BOX-INTERNAL (GRAPHICS-SCREEN-BOX)
       (de-allocate-init graphics-screen-box)
       (PUSH GRAPHICS-SCREEN-BOX FREE-GRAPHICS-SCREEN-BOXS))

(defun deallocate-sprite-screen-box-internal (sprite-screen-box)
  (de-allocate-init sprite-screen-box)
  (push sprite-screen-box free-sprite-screen-boxs))

(DEFUN DEALLOCATE-SCREEN-OBJ-INTERNAL (SCREEN-OBJ)
       (COND ((GRAPHICS-SCREEN-BOX? SCREEN-OBJ)
              (DEALLOCATE-GRAPHICS-SCREEN-BOX-INTERNAL SCREEN-OBJ))
             ((SCREEN-BOX? SCREEN-OBJ) (DEALLOCATE-SCREEN-BOX-INTERNAL SCREEN-OBJ))
             ((SCREEN-ROW? SCREEN-OBJ) (DEALLOCATE-SCREEN-ROW-INTERNAL SCREEN-OBJ))
             ((graphics-screen-sheet? screen-obj)
              (deallocate-graphics-screen-sheet-internal screen-obj))
             ((sprite-screen-box? screen-obj)
              (deallocate-sprite-screen-box-internal screen-obj))
             (T (BARF "Can't deallocate ~S" SCREEN-OBJ))))

;; make this do something if we ever make the allocate use resources
(defun deallocate-graphics-screen-sheet-internal (gss)
  (declare (ignore gss))
  nil)

(DEFVAR *SHRUNK-BOX-WID* 30.)
(DEFVAR *SHRUNK-BOX-HEI* 15.)

;;;; Stuff for circular structures in the redisplay
(DEFVAR PORT-REDISPLAY-HISTORY NIL)

(DEFVAR *PORT-REDISPLAY-DEPTH* 3)

(DEFVAR *BOX-ELLIPSIS-WID* 40.)
(DEFVAR *BOX-ELLIPSIS-HEI* 40.)
;;; Maybe these should be related to BOX-BORDER-PARAMETERS or something...
(DEFVAR *BOX-ELLIPSIS-THICKNESS* 1.)
(DEFVAR *BOX-ELLIPSIS-SPACING*   2.)

;;; The various types of Ellipsi (Ellipses (?)) are stored as symbols in the screen-row
;;; slots of the screen-box.  The drawing function is the DRAW-SELF property of the symbol
(DEFVAR *DEFINED-BOX-ELLIPSIS-STYLES* NIL)

(DEFUN BOX-ELLIPSIS-STYLE? (THING)
       (AND (SYMBOLP THING) (MEMBER THING *DEFINED-BOX-ELLIPSIS-STYLES*)))

(DEFMACRO DEFINE-BOX-ELLIPSIS-STYLE (NAME)
          (let ((erase-name (intern (symbol-format nil "~A-ERASE-SELF" name)))
                (size-name  (intern (symbol-format nil "~A-SIZE" name))))
            `(PROGN 'COMPILE
                    (PUSH ',NAME *DEFINED-BOX-ELLIPSIS-STYLES*)   ;; default erase adn size properties
                    ;; we can overide this with some other definition later
                    (defun ,erase-name (x-coord y-coord)
                      (erase-rectangle *box-ellipsis-wid* *box-ellipsis-hei*
                                       x-coord y-coord))
                    (setf (get ',name 'erase-self) ',erase-name)
                    (DEFUN ,size-name ()
                            (VALUES *BOX-ELLIPSIS-WID* *BOX-ELLIPSIS-HEI*))
                    (setf (get ',name 'size) ',size-name))))

(DEFVAR *BOX-ELLIPSIS-CURRENT-STYLE* 'BOX-ELLIPSIS-SOLID-LINES)

;;;****************************************************************;;;
;;;                      REDISPLAY MACROS                          ;;;
;;;****************************************************************;;;

(DEFMACRO QUEUEING-SCREEN-OBJS-DEALLOCATION (&BODY BODY)
          `(LET ((SCREEN-OBJS-DEALLOCATION-QUEUE NIL))
                (DECLARE (SPECIAL SCREEN-OBJS-DEALLOCATION-QUEUE))
                (UNWIND-PROTECT
                 (PROGN . ,BODY)
                 (DOLIST (QUEUED-SCREEN-OBJ SCREEN-OBJS-DEALLOCATION-QUEUE)
                         (DEALLOCATE-SELF QUEUED-SCREEN-OBJ)))))

(DEFMACRO PORT-REDISPLAYING-HISTORY ((ACTUAL-BOX) &BODY BODY)
          `(LET ((PORT-REDISPLAY-HISTORY (UPDATE-PORT-REDISPLAY-HISTORY ,ACTUAL-BOX)))
                . ,BODY))

(DEFMACRO REDISPLAYING-WINDOW ((WINDOW) &BODY BODY)
  `(LET* ((*OUTERMOST-SCREEN-BOX* (OUTERMOST-SCREEN-BOX ,WINDOW)))
        (QUEUEING-SCREEN-OBJS-DEALLOCATION
          (UNWIND-PROTECT
            (PROGN . ,BODY)
            (SET-OUTERMOST-SCREEN-BOX-IN-WINDOW ,WINDOW *OUTERMOST-SCREEN-BOX*)))))

;;; random useful structs and stuff

;;;; Fonts

;; Arial is pretty standard and exists on most platforms
;; This is currently being initialized in the boxwin-opengl startup.
(defvar *default-font* nil)

(defmacro check-and-handle-font-changes (cha-no)
  (declare (ignore cha-no))
  (warn "check-and-handle-font-changes should be inside of with-font-hacking")
  '(error "check-and-handle-font-changes called outside of with-font-hacking"))

(defmacro recheck-font-state (cha-no)
  (declare (ignore cha-no))
  (warn "recheck-font-state should be inside of with-font-hacking")
  '(error "recheck-font-state called outside of with-font-hacking"))

(defmacro maintaining-drawing-font (&body body)
  (let ((font-var (gensym)))
    `(let ((,font-var *current-opengl-font*))
       (unwind-protect
        (progn . ,body)
        ;; NOTE: fonts aren't necessarily EQ
        (unless (eql *current-opengl-font* ,font-var)
          (setq *current-opengl-font* ,font-var))))))

(defmacro rebind-font-info ((font-no) &body body)
  `(let ((%drawing-font-cha-hei %drawing-font-cha-hei)
         (%drawing-font-cha-ascent %drawing-font-cha-ascent))
     (unless (null ,font-no)
       (maintaining-drawing-font
        (set-font-info ,font-no)
        ,@body))))

(defmacro maintaining-pen-color (&body body)
  (let ((old-color (gensym)))
    `(let ((,old-color (boxgl-device-pen-color bw::*boxgl-device*)))
       (unwind-protect
        (progn . ,body)
          (unless (equalp ,old-color (boxgl-device-pen-color bw::*boxgl-device*))
            (setf (boxgl-device-pen-color bw::*boxgl-device*) ,old-color))))))

(defmacro with-font-hacking ((font-descriptors
                              &key (start-cha-no 0) (cha-drawing? nil))
                             &body body)
  (let ((initial-font-no (gensym)))
    `(macrolet ((check-and-handle-font-changes (cha-no)
                                               `(cond ((null next-font-change))
                                                  ((=& ,cha-no next-font-change)
                                                   (set-font-info
                                                    (bfd-font-no (car remaining-font-descriptors)))
                                                   ,',(when cha-drawing?
                                                        '(set-pen-color
                                                          (bfd-color (car remaining-font-descriptors))))
                                                   (setq remaining-font-descriptors
                                                         (cdr remaining-font-descriptors))
                                                   (setq next-font-change
                                                         (and remaining-font-descriptors
                                                              (bfd-cha-no
                                                               (car remaining-font-descriptors)))))))
                (recheck-font-state (cha-no)
                                    `(set-font-info (bfd-font-no
                                                     (closest-bfd-internal
                                                      ,',font-descriptors ,cha-no)))))
               (let* ((,initial-font-no (bfd-font-no
                                         (closest-bfd-internal ,font-descriptors
                                                                ,start-cha-no)))
                      (remaining-font-descriptors
                       (if (zerop& ,start-cha-no)
                         ,font-descriptors
                         (do ((leftovers ,font-descriptors (cdr leftovers)))
                           ((or (null leftovers)
                                (>& (bfd-cha-no (car leftovers)) ,start-cha-no))
                            leftovers)
                           ;(setq ,initial-font-no (bfd-font-no (car leftovers)))
                           )))
                      (next-font-change (and remaining-font-descriptors
                                             (bfd-cha-no (car remaining-font-descriptors)))))
                 (rebind-font-info (,initial-font-no)
                                   (maintaining-pen-color ,@body))))))

;; this is used when it is important to know about font info
;; during the body of the iteration.  Examples would be pass-1
;; width calculations, pass-2 drawing, or calculating the width
;; of characters for bitblt or erasing.

(defmacro do-screen-chas-with-font-info ((var screen-chas
                                           &key start stop index-var-name
                                           font-descriptors (cha-drawing? nil))
                                         &body body)
  (let ((index-name (or index-var-name (gensym))))
    `(let ((fds (or ,font-descriptors (screen-chas-array-fds ,screen-chas))))
       (with-font-hacking (fds :start-cha-no (or ,start 0)
                               :cha-drawing? ,cha-drawing?)
         (do-vector-contents (,var ,screen-chas
                               :start ,start
                               :stop ,stop
                               :index-var-name ,index-name)
           (check-and-handle-font-changes ,index-name)
           . ,body)))))
