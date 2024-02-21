;; -*- Mode:LISP; Syntax:Common-Lisp;Package:BOXER;-*-
#|


 $Header: gdispl.lisp,v 1.0 90/01/24 22:12:18 boxer Exp $

 $Log:	gdispl.lisp,v $
;;;Revision 1.0  90/01/24  22:12:18  boxer
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



   Graphics Display Lists and Primitives that operate on them



Modification History (most recent at the top)

 2/11/13 turtle-shape redisplay-init moved here from grobjs.lisp
 1/22/13 removed fixnum arithmetic from xxx-boxtop-size & draw-xxx-boxtop functions
12/31/12 changed font command to handle ttf fonts
 8/14/12 handle string-wid floats: centered-string
12/31/11 draw-xref-boxtop, xref-boxtop-size changed calls to xxx-mac-file-ref-xxx to xxx-xref-xxx
 7/31/11 new *update-bitmap?* special var for use in resize-graphics-sheet
         #+opengl cases added for bitmaps in resize-graphics-sheet
 2/02/11 change-graphics-font now handles (possible) unfilled font parameters
 4/20/10 circle, filled-circle,
10/03/08 draw-text-boxtop
 7/22/07 reallocate-pixel-color
 3/03/07 alpha channel support: canonicalize-pixel-color, reallocate-pixel-color
10/11/05 added more alternatives for draw-file-boxtop for #+lwwin
10/04/05 update-graphics-state checks for possible new penerase colors
 6/01/05 boxtop-size, draw-boxtop handles possible graphics in xref
10/22/04 draw-xref-boxtop, centered icon above text for OSX case
 4/05/04 draw-xref-boxtop updated for OSX
 4/01/04 boxer file icon drawing put back in for OSX mac
 8/27/03 draw-file-boxtop for carbon-compat
 5/20/03 merged src files again...
 5/13/03 draw-file-icon made smarter for #+lwwin
 4/19/03 merged current LW and MCL files
 4/09/03 added #+lwwin version for draw-file-boxtop
 2/15/03 merged current LW and MCL files
11/06/01 switched from floating to generic arithmetic in boxer extents form
         for wedge, arc, (filled-)circle. Fixnum radius arg seg faults in LWW
 2/26/01 removed invalid floating point arithmetic in centered-bitmap
         boxer command extents handler
 2/15/01 merged current LW and MCL files
10/07/00 added :file handlers for boxtop-size and draw-boxtop for LW version
 8/22/00 reallocate-pixel-color bug with save->read->save blowout for sprite shapes
12/15/99 *boxtop-text-font*: (%make-font-number-internal 0 10 :bold) for lwwin
         reallocate-pixel-color: added #+LWWIN integer clause to handle old code
10/15/99 draw-file-boxtop, file-boxtop-size use *file-boxtop-name-margin*
         to offset the name
8/19/99 with-graphics-state, canonicalize-graphics-state,
        modularized to use color= for color comparisons instead of = (LW change)
 5/10/99 changed draw-boxtop & boxtop-size to give defined boxtops precedence
         over :file
 5/01/99 fixed typo in arc graphics command
 4/25/99 added support for :file boxtops
 4/09/99 finished wedge and arc
 3/05/99 added wedge and arc commands
1/01/99 changed *initial-graphics-state-current-font-no* to a var instead of
        a constant because in some implementations, the value won't be known until
        AFTER the boxer window has been instantiated (LW change)
7/10/98 walk-body-for-args searches for mutators and
        with-graphics-command-slots-bound uses that info
7/09/98 added support for post processing for copy and boxer->window allocators
        in defgraphics-command
6/25/98 load-form for change-graphics-font now hacks compatibility with old files
         using make-font-from-file-value
5/11/98 set mac specific version of *boxtop-text-font*
5/06/98 added load and dump forms for type-font and changed them for pen-color
4/18/98 Start logging changes: source = boxer version 2.2r4


|#

(in-package :boxer)

;;;; Graphics Command Lists
;;; The result of a series of graphics commands can be captured
;;; using a graphics command lists.  This is just a list of
;;; various types of graphics commands.  There are two basic
;;; types of graphics command elements:
;;;
;;;   o A particular command to perform some type of graphical
;;;     output, such as a line or a rectangle, along with
;;;     arguments to the command such as coordinates, width, etc
;;;
;;;   o A command describing some sort of state change
;;;     such as the current-color or drawing-function
;;;
;;;   o There are 2 flavors of commands for the graphics commands
;;;     that draw.  They are distinguished by the type and meaning of
;;;     their arguments (slots). The 2 types are:
;;;
;;;       o Commands in the WINDOW SYSTEM coordinate space specified
;;;         as an offset from the upper-left corner of the containing
;;;         screen object.  The type of these offsets and args (such
;;;         as width or height) will be INTEGERS. The idea is that
;;;         these commands can be fed directly to window system
;;;         functions without conversion or coercion.
;;;
;;;       o Commands in BOXER GRAPHICS BOX coordinate space where (0,0)
;;;         indicates the CENTER of the graphics box and Y increases
;;;         in the "up" direction.  The type of these offsets and args
;;;         will be FLOATING POINT numbers to retain precision independent
;;;         of the granularity of the coordinate system
;;;
;;;; These are the graphics commands stored in the
;;;; *GRAPHICS-COMMAND-DESCRIPTOR-TABLE*
;;;; The floating point ops have are 32 and above. The fixnums are
;;;; 31 and below.
;;;;
;;;;
;;;; Here is the code to make the table:
;;;; (dotimes (d 64)
;;;;   (let ((thunk (aref *graphics-command-descriptor-table* d)))
;;;;    (if thunk
;;;;	(format t "~A ~5T~A  ~50T~A~%"
;;;;		d
;;;;		(graphics-command-descriptor-name thunk)
;;;;		(graphics-command-descriptor-slots thunk))
;;;;	(format t "~A~%" d))))

;;;;;;;;;;;;;;             THE TABLE     ;;;;;;;;;;;;;;;;;;;;;;
;;;; 0    CHANGE-ALU                                   (NEW-ALU)
;;;; 1    CHANGE-PEN-WIDTH                             (NEW-WIDTH)
;;;; 2    CHANGE-GRAPHICS-FONT                         (NEW-FONT-NO)
;;;; 3    LINE-SEGMENT                                 (X0 Y0 X1 Y1)
;;;; 4    CHANGE-GRAPHICS-COLOR                        (NEW-COLOR)
;;;; 5
;;;; 6
;;;; 7    CENTERED-STRING                              (X Y STRING)
;;;; 8    LEFT-STRING                                  (X Y STRING)
;;;; 9    RIGHT-STRING                                 (X Y STRING)
;;;; 10   CENTERED-RECTANGLE                           (X Y WIDTH HEIGHT)
;;;; 11   DOT                                          (X Y)
;;;; 12   HOLLOW-RECTANGLE ???                         (X Y WIDTH HEIGHT)
;;;; 13
;;;; 14
;;;; 15   CENTERED-BITMAP                             (BITMAP X Y WIDTH HEIGHT)
;;;; 16
;;;; 17
;;;; 18
;;;; 19
;;;; 20
;;;; 21
;;;; 22
;;;; 23
;;;; 24
;;;; 25
;;;; 26   WEDGE ???                                    (X Y RADIUS START-ANGLE SWEEP-ANGLE)
;;;; 27   ARC ???                                      (X Y RADIUS START-ANGLE SWEEP-ANGLE)
;;;; 28   FILLED-ELLIPSE                               (X Y WIDTH HEIGHT)
;;;; 29   ELLIPSE                                      (X Y WIDTH HEIGHT)
;;;; 30   FILLED-CIRCLE                                (X Y RADIUS)
;;;; 31   CIRCLE                                       (X Y RADIUS)
;;;; ----------------------------------------------------------------------
;;;; 32   BOXER-CHANGE-ALU                             (NEW-ALU)
;;;; 33   BOXER-CHANGE-PEN-WIDTH                       (NEW-WIDTH)
;;;; 34   BOXER-CHANGE-GRAPHICS-FONT                   (NEW-FONT-NO)
;;;; 35   BOXER-LINE-SEGMENT                           (X0 Y0 X1 Y1)
;;;; 36   BOXER-CHANGE-GRAPHICS-COLOR                  (NEW-COLOR)
;;;; 37   BOXER-TRANSFORM-MATRIX                       (MATRIX) 4x4 matrix packed in 1x16 single-float vector
;;;; 38
;;;; 39   BOXER-CENTERED-STRING                        (X Y STRING)
;;;; 40   BOXER-LEFT-STRING                            (X Y STRING)
;;;; 41   BOXER-RIGHT-STRING                           (X Y STRING)
;;;; 42   BOXER-CENTERED-RECTANGLE                     (X Y WIDTH HEIGHT)
;;;; 43   BOXER-DOT                                    (X Y)
;;;; 44   BOXER-HOLLOW-RECTANGLE ???                   (X Y WIDTH HEIGHT)
;;;; 45
;;;; 46
;;;; 47   BOXER-CENTERED-BITMAP                        (BITMAP X Y WIDTH HEIGHT)
;;;; 48
;;;; 49
;;;; 50
;;;; 51
;;;; 52
;;;; 53
;;;; 54
;;;; 55
;;;; 56
;;;; 57
;;;; 58   BOXER-WEDGE ???                              (X Y RADIUS START-ANGLE SWEEP-ANGLE)
;;;; 59   BOXER-ARC ???                                (X Y RADIUS START-ANGLE SWEEP-ANGLE)
;;;; 60   BOXER-FILLED-ELLIPSE                         (X Y WIDTH HEIGHT)
;;;; 61   BOXER-ELLIPSE                                (X Y WIDTH HEIGHT)
;;;; 62   BOXER-FILLED-CIRCLE                          (X Y RADIUS)
;;;; 63   BOXER-CIRCLE                                 (X Y RADIUS)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *default-graphics-list-initial-length* 16.)

;; defined in vars.lisp
;;(defvar *supress-graphics-recording?* nil
;;  "When this is T, graphics commands are not recorded")

;; defined in vars.lisp
;;(defvar *graphics-command-recording-mode* ':window
;;  "Should be either :WINDOW or :BOXER")

) ; eval-when

;;; store information about the graphics-command that
;;; might be useful for other macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct graphics-command-descriptor
    name
    slots
    transform-template)

  (defun get-graphics-command-descriptor (opcode)
    (let ((gc (gethash opcode *graphics-commands*)))
      (make-graphics-command-descriptor :name (slot-value gc 'name)
                                        :slots (slot-value gc 'command-args)
                                        :transform-template (slot-value gc 'transform-template))
    ))
)

(defun graphics-command-descriptor (graphics-command)
  (get-graphics-command-descriptor (svref& graphics-command 0)))

(defun graphics-command-name (graphics-command)
  (graphics-command-descriptor-name
   (get-graphics-command-descriptor (svref& graphics-command 0))))

(defun graphics-command-slots (graphics-command)
  (graphics-command-descriptor-slots
   (get-graphics-command-descriptor (svref& graphics-command 0))))

(defun copy-graphics-command (graphics-command)
  (copy (gethash (aref graphics-command 0) *graphics-commands*) graphics-command))

(defun graphics-command-extents (graphics-command)
  (declare (values min-x min-y max-x max-y))
  (extents (gethash (aref graphics-command 0) *graphics-commands*) (cdr (coerce graphics-command 'list))))

(defun dump-graphics-command (command stream)
  (dump-gc (gethash (aref command 0) *graphics-commands*) command stream))

;; this is actually more of a post load processing kinda thing
(defun load-graphics-command (command &optional (type :turtle-shape))
  (declare (ignore type))
  ;; Bit of a hack for old files, because of the graphics post load ordering, this is
  ;; looked up before we convert any older lower number window commands (<32) to boxer commands. (>=32)
  (let ((opcode (aref command 0)))
    (when (< opcode 32)
      (setf opcode (+ opcode 32)))
    (load-gc (gethash opcode *graphics-commands*) command)))

(defun translate-graphics-command-to-sprite-primitive (command)
  (sprite-gc (gethash (aref command 0) *graphics-commands*) (cdr (coerce command 'list))))

(defun allocate-window->boxer-command (graphics-command)
  (if (< (aref graphics-command 0) 32)
    (let* ((com (gethash (+ 32 (aref graphics-command 0)) *graphics-commands*))
           (template (slot-value com 'transformation-template))
           (togo (copy-seq graphics-command))
           (idx 1))
      (setf (aref togo 0) (+ 32 (aref graphics-command 0)))
      (unless (null template)
        (dolist (item template)
          (case item
            (:x-transform (setf (aref togo idx) (user-coordinate-fix-x (aref togo idx))))
            (:y-transform (setf (aref togo idx) (user-coordinate-fix-y (aref togo idx))))
            (:coerce (setf (aref togo idx) (make-single-float (aref togo idx))))
            (t nil))
          (incf idx)))
      togo)
    graphics-command))

(defun deallocate-graphics-command-marker (graphics-command)
  (deallocate-gc (gethash (aref graphics-command 0) *graphics-commands*) graphics-command))

;;; these should NEVER be changed.  These are the starting values
;;; for playback of graphics lists.  Changing these values will
;;; break any dumped out graphics-lists

(defconstant *initial-graphics-state-current-alu* alu-seta)
(defconstant *initial-graphics-state-current-pen-width* 1)
;; in some implementations, these can't be known until AFTER the
;; boxer window is instantiated
(defvar *initial-graphics-state-current-font-no* *sprite-type-font-no*)
(defvar *initial-graphics-state-current-pen-color* *foreground-color*)

;;;; Graphics Command Lists
;;; these are built out of storage vectors but they also contain
;;; state about the value(s) of various drawing variables
;;; such as the ALU, PEN-WIDTH, (PEN-COLOR), etc
;;;

;;; Used to record (and playback) commands inside of graphics boxes
;;; Commands will be of the window system/integer variety

(defstruct (graphics-command-list (:type vector)
                                  (:include storage-vector)
                                  ;; we define our own versions or these
                                  (:copier %%copy-graphics-command-list)
                                  (:constructor %%make-graphics-command-list))
  agent
  (alu *initial-graphics-state-current-alu*)
  (pen-width *initial-graphics-state-current-pen-width*)
  (font-no *initial-graphics-state-current-font-no*)
  (pen-color *initial-graphics-state-current-pen-color*)
  (hidden nil))

;; used by hi speed incremental redisplay to keep track
(defstruct (graphics-command-list-state (:copier %copy-graphics-command-list-state) ; prevent conflict
                                        (:constructor %make-graphics-command-list-state))
  (alu *initial-graphics-state-current-alu*)
  (pen-width *initial-graphics-state-current-pen-width*)
  (font-no *initial-graphics-state-current-font-no*)
  (pen-color *initial-graphics-state-current-pen-color*)
  ;; the index is how far the Redisplay got the last time the GL was rendered
  (index 0))

(defsubst graphics-command-list? (thing)
  (simple-vector-p thing))

;;; Used to record the commands that describe the shape of a turtle
;;; Commands will be of the boxer/floating point variety

(defstruct (turtle-shape (:type vector)
                         (:include graphics-command-list)
                         ;;
                         (:copier %%copy-turtle-shape)
                         (:constructor %%make-turtle-shape))
  )

(defsubst turtle-shape? (thing)
  (simple-vector-p thing))


(defmacro expand-mutators-and-body (args initial-index &body body)
  (cond ((null args)
         `(progn . ,body))
    (t `(macrolet ((,(intern (symbol-format nil "SET-~A" (car args)))
                    (new-value)
                    `(setf (svref& .graphics-command. ,,initial-index)
                           ,new-value)))
                  (expand-mutators-and-body ,(cdr args)
                                             ,(incf initial-index)
                                             ,@body)))))

;;; A template (which should be the same length as the args) specifies
;;; how to convert particular slots of a window-system-integer-command
;;; from/to boxer-graphics-floating-command.
;;; template-actions can be:
;;;
;;;      :X-TRANSFORM - transform the coordinates and do the
;;;                     appropriate type coercion
;;;      :Y-TRANSFORM - transform the coordinates and do the
;;;                     appropriate type coercion
;;;      :COERCE      - coerce the slot to the appropriate type of
;;;                     number.  The value stays the same.
;;;      NIL          - Leave the slot alone
;;;

(defun make-single-float (arg)
  (coerce arg 'single-float))

;;; this is mostly for readability
;; (eval (compile load eval)
(defmacro defgraphics-state-change ((name opcode) args
                                                  &key
                                                  sprite-command
                                                  body)
  `(defun ,name ,args
             ,@args
             (progn ,@body)))
;; )

;;; sgithens 2024-02-20 Old notes from before defstandard-graphics-handlers
;;;
;;; Putting it all together...
;;;
;;; Note that we are using the standard Boxer trick of spending memory to
;;; increase speed.  The Instantiations of shapes are cached in
;;; turtle-window-shapes and are draw from here.  It is perfectly reasonable
;;; (and the old implementation did this) to draw the turtle's shape
;;; directly from its shape without indirecting through a cache.  To do
;;; this, you will need to define a graphic-handler for drawing sprites
;;; and change the DRAW method of turtles to use it.
;;;
;;; The current tradeoff (using the cache strategy) is based on the following
;;; influences:
;;;
;;;   o calculating the rotated, scaled, translated shape
;;;     it floating point intensive
;;;
;;;   o Shapes are usually drawn at least twice in the same place in the
;;;     common case (draw and erase)
;;;
;;;   o When there is more than one sprite (again, a not uncommon event)
;;;     ALL the sprites have to erased and redrawn whenever ANY sprite
;;;     wants to draw on the graphics box.  The only exception being the
;;;     XOR hack (all the sprite have to have XOR shapes and the drawing
;;;     sprite also needs to be drawing in XOR).
;;;




;;; for now, on a monochrome monitor, we only need to record
;;; the drawing-function (alu) and the pen-width
;;; we may later want to add things like the current stipple
;;; pattern or the color

(defvar *graphics-state-current-alu* *initial-graphics-state-current-alu*)

(defvar *graphics-state-current-pen-width*
  *initial-graphics-state-current-pen-width*)

(defvar *graphics-state-current-font-no*
  *initial-graphics-state-current-font-no* )

(defvar *graphics-state-current-pen-color* *foreground-color*)

;;; the foreground color is usually undefined until boxer startup time
(def-redisplay-initialization
  (progn
  (setq *initial-graphics-state-current-pen-color* *foreground-color*
        *graphics-state-current-pen-color* *foreground-color*)

  (setq *default-graphics-object-shape*
        (let ((%graphics-list (make-turtle-shape 8))
              (*graphics-command-recording-mode* ':boxer))
          (record-boxer-graphics-command-change-alu alu-seta)
          (record-boxer-graphics-command-change-pen-width 1)
          (record-boxer-graphics-command-centered-rectangle
           0.0 0.0
           *default-graphics-object-size* *default-graphics-object-size*)
          %graphics-list)
        *default-turtle-shape*
        (let ((%graphics-list (make-turtle-shape 8))
              (*graphics-command-recording-mode* ':boxer))
          (record-boxer-graphics-command-change-alu alu-seta)
          (record-boxer-graphics-command-change-pen-width 1)
          ;; the base line
          (record-boxer-graphics-command-line-segment
           (- *turtle-half-base*) (- (/ *turtle-height* 3.0))
           *turtle-half-base* (- (/ *turtle-height* 3.0)))
          ;; the right side
          (record-boxer-graphics-command-line-segment
           *turtle-half-base* (- (/ *turtle-height* 3.0))
           0.0 (* 2 (/ *turtle-height* 3)))
          ;; the left side
          (record-boxer-graphics-command-line-segment
           0.0 (* 2 (/ *turtle-height* 3))
           (- *turtle-half-base*) (- (/ *turtle-height* 3.0)))
          %graphics-list))))

;;; puts a graphics list into the initial state by issuing
;;; state changing commands when neccessary
;;; actually this could probably use a peephole optimizer at some point....

(defun canonicalize-graphics-state (gl)
  (let ((%graphics-list gl))
    (unless (=& (graphics-command-list-alu gl)
                *initial-graphics-state-current-alu*)
      (record-boxer-graphics-command-change-alu
       *initial-graphics-state-current-alu*))
    (unless (=& (graphics-command-list-pen-width gl)
                *initial-graphics-state-current-pen-width*)
      (record-boxer-graphics-command-change-pen-width
       *initial-graphics-state-current-pen-width*))
    (unless (color= (graphics-command-list-pen-color gl)
                    *initial-graphics-state-current-pen-color*)
      (record-boxer-graphics-command-change-graphics-color
       *initial-graphics-state-current-pen-color*))
    (unless (=& (graphics-command-list-font-no gl)
                *initial-graphics-state-current-font-no*)
      (record-boxer-graphics-command-change-graphics-font
       *initial-graphics-state-current-font-no*))))

(defmacro with-graphics-state ((gcl &optional use-initial-values) &body body)
  (let ((old-pen-width (gensym))
        (old-pen-color (gensym))
        (old-font-no (gensym)))
    `(let ((*graphics-state-current-alu* *initial-graphics-state-current-alu*)
           (*graphics-state-current-font-no*
            *initial-graphics-state-current-font-no*)
           (,old-pen-width *graphics-state-current-pen-width*)
           (*graphics-state-current-pen-width*
            *initial-graphics-state-current-pen-width*)
           (,old-pen-color *graphics-state-current-pen-color*)
           (*graphics-state-current-pen-color*
            *initial-graphics-state-current-pen-color*)
           (,old-font-no *graphics-state-current-font-no*))
       (unwind-protect
        (progn
         (unless (null ,gcl)
           ,(when (null use-initial-values)
              `(setq *graphics-state-current-alu*
                     (graphics-command-list-alu ,gcl)
                     *graphics-state-current-font-no*
                     (or (graphics-command-list-font-no ,gcl)
                         *graphics-state-current-font-no*)
                     *graphics-state-current-pen-width*
                     (or (graphics-command-list-pen-width ,gcl)
                         *graphics-state-current-pen-width*)
                     *graphics-state-current-pen-color*
                     (or (graphics-command-list-pen-color ,gcl)
                         *graphics-state-current-pen-color*)))
           (when (not (=& ,old-pen-width
                           *graphics-state-current-pen-width*))
             (%set-pen-size *graphics-state-current-pen-width*))
           (when (not (color= ,old-pen-color
                               *graphics-state-current-pen-color*))
             (%set-pen-color *graphics-state-current-pen-color*)))
         . ,body)
        (unless (null ,gcl)
          (setf (graphics-command-list-alu ,gcl) *graphics-state-current-alu*)
          (setf (graphics-command-list-font-no ,gcl)
                *graphics-state-current-font-no*)
          (setf (graphics-command-list-pen-width ,gcl)
                *graphics-state-current-pen-width*)
          (setf (graphics-command-list-pen-color ,gcl)
                *graphics-state-current-pen-color*)
          ;; restore any state variables that
          ;; might have been bashed in the body
          ,old-font-no ; placeholder and to supress warnings
          (when (not (=& ,old-pen-width *graphics-state-current-pen-width*))
            (%set-pen-size ,old-pen-width))
          (when (not (color= ,old-pen-color *graphics-state-current-pen-color*))
            (%set-pen-color ,old-pen-color)))))))

;; a lightweight version of with-graphics-state.  Only the value of the
;; variables should be changed in the BODY (and NOT the window system's
;; internal graphics state) so we don't need the unwind-protect
;; This is used mostly for things like looping through graphics commands
;; which don't need to draw but do need to side effect these vars
;; like extents functions
(defmacro with-graphics-state-bound (&body body)
  `(let ((*graphics-state-current-alu* *graphics-state-current-alu*)
         (*graphics-state-current-font-no* *graphics-state-current-font-no*)
         (*graphics-state-current-pen-width* *graphics-state-current-pen-width*)
         (*graphics-state-current-pen-color* *graphics-state-current-pen-color*))
     . ,body))

;;; should only be called inside of with-graphics-state AND
;;; an environment where %graphics-list is bound to something meaningful

(defun update-graphics-state (agent)
  (let ((pen (pen agent)))
    (cond ((null %graphics-list))
      ((not (graphics-command-list? %graphics-list))
       (error "%graphics-list, ~A, is not bound to a Graphics Command List"
              %graphics-list))
      ((eq agent (graphics-command-list-agent %graphics-list))
       ;; if the agent is the same, then we do nothing, except
       ;; updating the penerase color if neccessary
       (let ((bgc nil)) ;; stack allocation
         (when (and (eq pen 'bu::erase)
                    (not (color= *graphics-state-current-pen-color*
                                 (setq bgc (background-graphics-color agent)))))
           (record-boxer-graphics-command-change-graphics-color bgc)
           (setf (graphics-command-list-pen-color %graphics-list) bgc)
           (change-graphics-color bgc))))
      ((eq pen 'bu::up)) ; the agent might do a PD
      ;; if the agent is not drawing, do nothing
      ;; also need a clause here for agents that do not use pens
      (t
       ;; otherwise, we need to update the graphics
       ;; state from the values of the agent
       (setf (graphics-command-list-agent %graphics-list) agent)
       (synchronize-graphics-state agent)))))

(defun reset-graphics-list-values (gl)
  (setf (graphics-command-list-agent gl) nil
        (graphics-command-list-alu gl) *initial-graphics-state-current-alu*
        (graphics-command-list-pen-width gl)
        *initial-graphics-state-current-pen-width*
        (graphics-command-list-font-no gl)
        *initial-graphics-state-current-font-no*
        (graphics-command-list-pen-color gl)
        *initial-graphics-state-current-pen-color*)
  gl)



;;;; Operations on Graphics Command Lists

(defun make-graphics-command-list (&optional
                                   (length
                                    *default-graphics-list-initial-length*))
  (%%make-graphics-command-list :contents (allocate-c-vector length)))

(defun clear-graphics-list (gl)
  (do-vector-contents (graphics-command gl)
    (deallocate-graphics-command-marker graphics-command))
  (reset-graphics-list-values gl)
  (sv-delete-to-end gl 0))

(defun copy-graphics-command-list-state (old-graphics-list new-graphics-list)
  (setf (graphics-command-list-alu new-graphics-list)
        (graphics-command-list-alu old-graphics-list))
  (setf (graphics-command-list-pen-width new-graphics-list)
        (graphics-command-list-pen-width old-graphics-list))
  (setf (graphics-command-list-font-no new-graphics-list)
        (graphics-command-list-font-no old-graphics-list))
  (setf (graphics-command-list-pen-color new-graphics-list)
        (graphics-command-list-pen-color old-graphics-list)))

(defun copy-graphics-command-list (command-list)
  (let* ((command-list-length (storage-vector-active-length command-list))
         (new-list (make-graphics-command-list (1+& command-list-length)))
         (new-contents (%sv-contents new-list)))
    (copy-graphics-command-list-state command-list new-list)
    (do-vector-contents (item command-list :index-var-name idx)
      (setf (svref& new-contents idx) (copy-graphics-command item)))
    (setf (%sv-fill-pointer new-list) command-list-length)
    new-list))


(defun make-turtle-shape (&optional (length
                                     *default-graphics-list-initial-length*))
  (%%make-turtle-shape :contents (allocate-c-vector length)))




(defvar *update-bitmap?* t)

;; *update-bitmap?* is for use inside of a mouse resize box loop to avoid thrashing
;; the memory with every twitch of the mouse
(defun resize-graphics-sheet (sheet new-wid new-hei)
  (let* ((old-wid (graphics-sheet-draw-wid sheet))
         (old-hei (graphics-sheet-draw-hei sheet)))
    (when (and *update-bitmap?* (not (null (graphics-sheet-bit-array sheet))))
      (let ((old-bitmap (graphics-sheet-bit-array sheet))
            (new-bitmap (make-ogl-pixmap new-wid new-hei)))
        ;; if the new bitmap is bigger, initialize it
        (when (or (> new-wid old-wid) (> new-hei old-hei))
          (clear-offscreen-bitmap new-bitmap (or (graphics-sheet-background sheet)
                                                 *background-color*)))
        ;; now move the old contents into the new bitmap
        (copy-pixmap-data
          (min& old-wid new-wid) (min& old-hei new-hei)
          old-bitmap
          (max& 0 (round (-& old-wid new-wid) 2))
          (max& 0 (round (-& old-hei new-hei) 2))
          new-bitmap
          (max& 0 (round (-& new-wid old-wid) 2))
          (max& 0 (round (-& new-hei old-hei) 2)))
        ;; now install the new bitmap
        (setf (graphics-sheet-bit-array sheet) new-bitmap)
        ;; we might not want to do this if sprites are allowed to have
        ;; pointers to raw pixmaps in their shapes
        (ogl-free-pixmap old-bitmap)))
    (when (not (null (graphics-sheet-object-list sheet)))
      ;; (maybe) move sprites to a new position
      ;; don't move if there are no sprites or if the box is in :clip mode
      (dolist (obj (graphics-sheet-object-list sheet))
        ;; leave things where they are unless they
        ;; would be clipped, in which case send them
        ;; home unless the sheet is in :clip mode
        (when (and (or (>= (abs (x-position obj)) (/ new-wid 2))
                       (>= (abs (y-position obj)) (/ new-hei 2)))
                  (not (eq (graphics-sheet-draw-mode sheet) ':clip)))
          (let ((%drawing-width new-wid) (%drawing-height new-hei)
                                        (%drawing-half-width (/ new-wid 2.0))
                                        (%drawing-half-height (/ new-hei 2.0)))
            (set-xy obj (wrap-x-coordinate (x-position obj))
                    (wrap-y-coordinate (y-position obj))))))))
    (setf (graphics-sheet-draw-wid sheet) new-wid)
    (setf (graphics-sheet-draw-hei sheet) new-hei))

;;; Graphics State Changes

(defun canonicalize-file-alu (alu)
  (case alu
    (#.alu-andca 'alu-andca)
    (#.alu-seta 'alu-seta)
    (#.alu-xor 'alu-xor)
    (#.alu-ior 'alu-ior)
    (#.alu-setz 'alu-setz)
    (otherwise (warn "Unknown ALU:~A" alu) alu)))

(defun reallocate-file-alu (alu)
  (case alu
    (alu-andca alu-andca)
    (alu-seta alu-seta)
    (alu-xor alu-xor)
    (alu-ior alu-ior)
    (alu-setz alu-setz)
    ;; handle alus which originate from old sun files
    #+mcl (6 alu-xor) #+mcl (5 alu-seta)
    (otherwise alu)))

(defun make-font-from-file-value (file-font)
  (cond ((and (typep file-font 'fixnum) (<=& 0 file-font 7))
         ;; compatibility with old files...
         (make-boxer-font
          (ecase file-font
                 (0 '("Courier New" 10))
                 (1 '("Courier New" 10 :bold))
                 (2 '("Courier New" 10 :italic))
                 (3 '("Courier New" 10 :bold :italic))
                 (4 '("Arial" 10))
                 (5 '("Arial" 10 :bold))
                 (6 '("Arial" 10 :italic))
                 (7 '("Arial" 10 :bold :italic)))))
    ((typep file-font 'fixnum)
      ;; If the number isn't between 1 and 7 we'll return this
      '("Arial" 10 :bold))
    (t (make-boxer-font file-font))))

;; check for foreground, background and predefined colors
(defun canonicalize-pixel-color (pixel &optional box)
  (multiple-value-bind (r g b a)
                       (lookup-color-values-in-box box pixel)
                       (if (null r)
                         (pixel-rgb-values pixel)
                         (list r g b (if (null a) 100 a)))))

;; for *file-bin-version* > 12, colors are fixnums again
(defun reallocate-pixel-color (color)
  (etypecase color
             (null    color);; NIL is the default color for turtle shapes
             (vector  (if (rgb-p color)
                        color
                        nil))
             (integer (%make-color-from-bytes (ldb (byte 8 16) color)
                                              (ldb (byte 8 8)  color)
                                              (ldb (byte 8 0)  color)))
             (list   (if (> (length color) 3)
                       (%make-color (car color) (cadr color)(caddr color)(cadddr color))
                       (%make-color (car color) (cadr color) (caddr color))))))

(defun sprite-commands-for-new-position (new-x new-y)
  (list 'bu::penup 'bu::setxy new-x new-y 'bu::pendown))

;; the default copy functions only copy slots. For bitmaps, we need
;; a separate copy of the bitmap as well
;; sgithens TODO fix these duplicate def warnings for sbcl
#-sbcl(defun copy-boxer-graphics-command-centered-bitmap (command)
  (make-boxer-graphics-command-centered-bitmap
   (copy-pixmap (boxer-graphics-command-centered-bitmap-bitmap command))
   (boxer-graphics-command-centered-bitmap-x command)
   (boxer-graphics-command-centered-bitmap-y command)
   (boxer-graphics-command-centered-bitmap-width command)
   (boxer-graphics-command-centered-bitmap-height command)))

;; the def macro should be changed so we don't have to do this...

;; this is used by the redisplay...

(defun redisplay-graphics-sheet-graphics-list (gs graphics-screen-box)
  "Draws the graphics-command-list of the graphics-sheet in member graphics-list. This typically contains
   everything that has been drawn or stamped by sprites and doesn't privately belong to them. This takes
   in to account if openGL framebuffers are used and can paint to the framebuffer attached to the
   screen-box."
  (with-graphics-vars-bound ((screen-obj-actual-obj graphics-screen-box))
    (if *use-opengl-framebuffers*
      (let* ((wid (graphics-sheet-draw-wid gs))
             (hei (graphics-sheet-draw-hei gs))
             (gl (graphics-sheet-graphics-list gs))
             (canvas (get-graphics-canvas-for-screen-obj graphics-screen-box wid hei)))
        (when (> (%%sv-fill-pointer gl) (op-count canvas))
          (enable canvas)
          (unless (graphics-command-list-hidden gl)
            (boxer-playback-graphics-list gl :start (op-count canvas) :graphics-canvas canvas :translate? t))
          (setf (op-count canvas) (%%sv-fill-pointer gl))
          (disable canvas)))
      ; else
      (let ((gl (graphics-sheet-graphics-list gs)))
        (unless (graphics-command-list-hidden gl)
          (boxer-playback-graphics-list gl :translate? t))))))

(defun redisplay-graphics-sheet-sprites (gs graphics-screen-box)
  "Draws all the sprites in the the object-list slot of the related graphics-sheet.  Draws the sprites private
   graphcis-command-list and issues each sprites draw method."
  (with-graphics-vars-bound ((screen-obj-actual-obj graphics-screen-box))
    (let ((sprites (graphics-sheet-object-list gs)))
      (dolist (sprite sprites)
        (when (turtle? sprite)
          (let ((pgl (slot-value sprite 'private-gl)))
            (unless (graphics-command-list-hidden pgl)
              (boxer-playback-graphics-list pgl)
              ))))
      (dolist (sprite sprites)
        (unless (null (shown? sprite))
          (draw sprite))))))


;;; show probably do some type checking about compatibility
;;; of the from and to args
(defun dub-graphics-list (from-gl &key
                                  (to-gl %graphics-list)
                                  (action ':append)
                                  (model-matrix nil)
                                  (inverse-matrix nil))
  ;; Transform the drawing commands if a model has been provided
  (when model-matrix
    (sv-append to-gl `#(37 ,model-matrix)))
  (ecase action
         (:append
          ;; first, set the drawing parameters to the basic state
          (canonicalize-graphics-state to-gl)
          ;; now dub the contents
          (do-vector-contents (command from-gl)
            (sv-append to-gl (copy-graphics-command command))))
         (:replace
          (clear-graphics-list to-gl)
          (do-vector-contents (command from-gl)
            (sv-append to-gl (copy-graphics-command command)))))
  ;; Invert the transformation if one has been supplied
  (when inverse-matrix
    (sv-append to-gl `#(37 ,inverse-matrix)))
  ;; now make sure the to-gl has the same current state as the from-gl
  (setf (graphics-command-list-agent to-gl) nil
        (graphics-command-list-alu to-gl) (graphics-command-list-alu from-gl)
        (graphics-command-list-pen-width to-gl) (graphics-command-list-pen-width from-gl)
        (graphics-command-list-font-no to-gl) (graphics-command-list-font-no from-gl)
        (graphics-command-list-pen-color to-gl) (graphics-command-list-pen-color from-gl))
  (setq *graphics-state-current-alu* (graphics-command-list-alu to-gl)
        *graphics-state-current-pen-width* (graphics-command-list-pen-width to-gl)
        *graphics-state-current-font-no* (graphics-command-list-font-no to-gl)
        *graphics-state-current-pen-color* (graphics-command-list-pen-color to-gl)))

;;; Debugging fun...

(defun show-graphics-command (com stream)
  (format stream "~%~A:" (graphics-command-name com))
  (do ((i 1 (+& i 1)))
    ((=& i (svlength com)))
    (format stream " ~A" (svref& com i))))

(defun show-graphics-list (gl &optional
                              (first-n nil) (stream *standard-output*))
  (format stream "~%STATE is [agent:~A],[alu:~A],~
                             [font-no:~A],[pen-width:~A],[pen-color:~A]  values are:"
          (graphics-command-list-agent gl) (graphics-command-list-alu gl)
          (graphics-command-list-font-no gl)
          (graphics-command-list-pen-width gl)
          (graphics-command-list-pen-color gl))
  (let ((counter 0))
    (do-vector-contents (command gl)
      (show-graphics-command command stream)
      (incf& counter)
      (when (and first-n (= counter first-n))
        (return nil)))))

(defun show-graphics-state (gl &optional (label "Current Graphics State"))
  (let ((agent (and gl (graphics-command-list-agent gl))))
    (format t "~&~A" label)
    (unless (null gl)
      (format t "~&    Current global state: [alu:~A],[font-no:~A],[pen-width:~A],[pen-color:~A]"
              *graphics-state-current-alu* *graphics-state-current-font-no*
              *graphics-state-current-pen-width* *graphics-state-current-pen-color*)
      (if (null agent) (format t "~&    No Current Sprite")
        (format t "~&    Sprite values are:  [alu:~A],[font-no:~A],[pen-width:~A],[pen-color:~A]"
                (get-alu-from-pen (pen agent)) (type-font agent)
                (pen-width agent) (pen-color agent)))
      (format t "~&    Graphics List")
      (show-graphics-list gl))))

(defun clear-gl (box)
  (clear-graphics-list (graphics-sheet-graphics-list (graphics-sheet box))))

#|
(defun test (box x y)
  (drawing-on-window (*boxer-pane*)
         (with-graphics-state
         (with-graphics-vars-bound (box)
         (change-pen-width 1)
         (ellipse x y 60 60)
         (centered-rectangle x y 42 42)
         (line-segment (- x 30) y (- x 60) (- y 20))
         (line-segment (- x 30) y (- x 60) (+ y 20))
         (change-pen-width 4)
         (line-segment (+ x 30) y (+ x 60) (- y 20))
         (line-segment (+ x 30) y (+ x 60) (+ y 20))
         (centered-string x (+ y 40) "Alien")))))
|#


