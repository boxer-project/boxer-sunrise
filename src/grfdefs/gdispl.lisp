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
;;;; 12
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
;;;; 26
;;;; 27
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
;;;; 37
;;;; 38
;;;; 39   BOXER-CENTERED-STRING                        (X Y STRING)
;;;; 40   BOXER-LEFT-STRING                            (X Y STRING)
;;;; 41   BOXER-RIGHT-STRING                           (X Y STRING)
;;;; 42   BOXER-CENTERED-RECTANGLE                     (X Y WIDTH HEIGHT)
;;;; 43   BOXER-DOT                                    (X Y)
;;;; 44
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
;;;; 58
;;;; 59
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

(defvar *initial-graphics-command-dispatch-table-size* 32.
  "This is the expected maximum number of DIFFERENT graphics commands.
   The actual table sizes will be twice this number with the bottom
   half for window coordinate based commands and the top half for
   boxer coodinate based commands.")


(defvar *boxer-graphics-command-mask* 32.) ;


(defvar *graphics-command-descriptor-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil)
  "This is useful for decoding what a graphics command is")

(defvar *graphics-command-name-opcode-alist* nil
  "Used to map names back into their opcodes")

(defvar *graphics-command-copier-table*
    (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
                :initial-element nil))

(defvar *graphics-command-dispatch-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-size-values-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-binding-values-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-dumper-dispatch-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-loader-dispatch-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-translation-table*
  (make-array *initial-graphics-command-dispatch-table-size*
              :initial-element nil))

(defvar *graphics-command-translation-and-scaling-table*
  (make-array *initial-graphics-command-dispatch-table-size*
              :initial-element nil))

(defvar *graphics-command-boxer->window-translation-table*
  (make-array *initial-graphics-command-dispatch-table-size*
              :initial-element nil))

(defvar *graphics-command-window->boxer-translation-table*
  (make-array *initial-graphics-command-dispatch-table-size*
              :initial-element nil))

(defvar *graphics-command-deallocation-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *turtle-translation-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-sprite-command-translation-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

) ; eval-when

;;; store information about the graphics-command that
;;; might be useful for other macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct graphics-command-descriptor
    name
    slots
    transform-template)

  (defun graphics-command-opcode (command-name)
    (let ((entry (fast-assq command-name *graphics-command-name-opcode-alist*)))
      (if (null entry)
        (error "No Graphics Command Opcode for ~S" command-name)
        (cdr entry))))

  (defun get-graphics-command-descriptor (name-or-opcode)
    (etypecase name-or-opcode
              (integer )
              (symbol (setq name-or-opcode (graphics-command-opcode name-or-opcode))))
    (let ((des (svref& *graphics-command-descriptor-table* name-or-opcode)))
      (if (null des)
        (error "No Descriptor found for ~S" name-or-opcode)
        des)))
)

(defun graphics-command-descriptor (graphics-command)
  (get-graphics-command-descriptor (svref& graphics-command 0)))

(defun graphics-command-name (graphics-command)
  (graphics-command-descriptor-name
   (get-graphics-command-descriptor (svref& graphics-command 0))))

(defun graphics-command-slots (graphics-command)
  (graphics-command-descriptor-slots
   (get-graphics-command-descriptor (svref& graphics-command 0))))

(eval-when (:compile-toplevel :load-toplevel :execute)
           (defun graphics-command-transform-template (graphics-command)
             (graphics-command-descriptor-transform-template
              (get-graphics-command-descriptor (svref& graphics-command 0))))


           (defun graphics-command-slot-offset (descriptor slot-name)
             (let ((pos (position slot-name
                                  (graphics-command-descriptor-slots descriptor))))
               (if (null pos)
                 (error "The slot, ~S, does not seem to be in the Graphics Command ~S"
                        slot-name (graphics-command-descriptor-name descriptor))
                 (1+ pos))))
           ) ; eval-when


(defun copy-graphics-command (graphics-command)
  (funcall (svref& *graphics-command-copier-table*
                   (svref& graphics-command 0))
           graphics-command))

(defmacro bind-graphics-handlers ((table) &body body)
  `(let ((*graphics-command-dispatch-table* ,table))
     . ,body))

(defmacro process-graphics-command-marker (graphics-command &rest args)
  `(let ((handler (svref& *graphics-command-dispatch-table*
                          (svref& ,graphics-command 0))))
     (unless (null  handler)
       (funcall handler ,graphics-command ,@args))))

(defun graphics-command-extents (graphics-command)
  (declare (values min-x min-y max-x max-y))
  (let ((handler (svref& *graphics-command-size-values-table*
                         (svref& graphics-command 0))))
    (if (null handler)
      (values 0 0 0 0)
      (funcall handler graphics-command))))

(defun dump-graphics-command (command stream &optional (type :turtle-shape))
  (declare (ignore type))
  (let ((handler (svref& *graphics-command-dumper-dispatch-table*
                         (svref& command 0))))
    (if (null handler)
      (dump-boxer-thing command stream)
      (funcall handler command stream))))

;; this is actually more of a post load processing kinda thing
(defun load-graphics-command (command &optional (type :turtle-shape))
  (declare (ignore type))
  (let ((handler (svref& *graphics-command-loader-dispatch-table*
                         (svref& command 0))))
    (unless (null handler)
      (funcall handler command))))

(defun translate-graphics-command-to-sprite-primitive (command)
  (let* ((x (if (>= (svref& command 0) 32)
              (-& (svref& command 0) 32)
              (svref& command 0)))
         (handler (svref& *graphics-command-sprite-command-translation-table*
                          x)))
    (unless (null handler) (funcall handler command))))

(defun translate-graphics-command (graphics-command trans-x trans-y)
  (let ((handler (svref& *graphics-command-translation-table*
                         (svref& graphics-command 0))))
    (unless (null handler)
      (funcall handler graphics-command trans-x trans-y))))

(defun translate-and-scale-graphics-command (graphics-command
                                             trans-x trans-y
                                             scale-x scale-y)
  (let ((handler (svref& *graphics-command-translation-and-scaling-table*
                         (svref& graphics-command 0))))
    (unless (null handler)
      (funcall handler graphics-command trans-x trans-y scale-x scale-y))))

(defun translate-boxer->window-command (from-graphics-command
                                        to-graphics-command
                                        trans-x trans-y
                                        cos-scale sin-scale scale)
  (let ((handler (svref& *turtle-translation-table*
                         (svref& from-graphics-command 0))))
    (unless (null handler)
      (funcall handler
               from-graphics-command to-graphics-command
               trans-x trans-y cos-scale sin-scale scale))))

(defun allocate-boxer->window-command (graphics-command)
  (let ((handler (svref& *graphics-command-boxer->window-translation-table*
                         (let ((opcode (svref& graphics-command 0)))
                           ;; this is a crock to handle cases where
                           ;; we (maybe) need to convert old graphics
                           ;; commands from obsolete files
                           (if (>=& opcode 32) (-& opcode 32) opcode)))))
    (if (null handler)
      (error "No translation allocator for ~A" graphics-command)
      (funcall handler graphics-command))))

(defun allocate-window->boxer-command (graphics-command)
  (let ((handler (svref& *graphics-command-window->boxer-translation-table*
                         (svref& graphics-command 0))))
    (if (null handler)
      (error "No translation allocator for ~A" graphics-command)
      (funcall handler graphics-command))))

(defmacro graphics-command-values (command-name-or-opcode
                                   graphics-command &body body)
  (let ((opcode (etypecase command-name-or-opcode
                           (number command-name-or-opcode)
                           (symbol (graphics-command-opcode command-name-or-opcode)))))
    `(,(svref& *graphics-command-binding-values-table* opcode)
      ,graphics-command
       ,@body)))


(defun deallocate-graphics-command-marker (graphics-command)
  (funcall (svref& *graphics-command-deallocation-table*
                   (svref& graphics-command 0))
           graphics-command))




;;; these should NEVER be changed.  These are the starting values
;;; for playback of graphics lists.  Changing these values will
;;; break any dumped out graphics-lists

(defconstant *initial-graphics-state-current-alu* alu-seta)
(defconstant *initial-graphics-state-current-pen-width* 1)
;; in some implementations, these can't be known until AFTER the
;; boxer window is instantiated
(defvar *initial-graphics-state-current-font-no* *sprite-type-font-no*)
(defvar *initial-graphics-state-current-pen-color* *foreground-color*)

;;;; COLOR

(defstruct (boxer-color :named (:type vector)
                        (:constructor %make-boxer-color (red green blue)))
  (red   0)
  (green 0)
  (blue  0))

;;; color tables map color indices (internal fixnums) to boxer
;;; color structures.  For each window system, there should be
;;; a way to obtain an index from a color description.
;;; The index returned will be a suitable value for the turtle's pen-color





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

;;; Like a Graphics-Command-List with extra slots.  This is used as
;;; a cache for the rendering of a turtle shape at a particular
;;; location and heading.  Subsequent calls to draw the turtle
;;; can just access the cached values instead of recalculating.
;;; Commands will be of the window system/integer variety

(defstruct (turtle-window-shape (:type vector)
                                (:include graphics-command-list)
                                ;; we need to define our own versions of
                                (:copier %%copy-turtle-window-shape)
                                (:constructor %%make-turtle-window-shape))
  (valid nil)
  ;; some places to cache popular quantities
  min-graphics-x-extent
  max-graphics-x-extent
  min-graphics-y-extent
  max-graphics-y-extent)

(defsubst turtle-window-shape? (thing)
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

(defvar *type-check-during-template-conversion* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-transform-template-item (arg template-action direction)
    (ecase direction
          (:boxer->window (case template-action
                            (:x-transform (list 'fix-array-coordinate-x arg))
                            (:y-transform (list 'fix-array-coordinate-y arg))
                            (:coerce      (list 'round arg))
                            (t            arg)))
          (:window->boxer (case template-action
                            (:x-transform (list 'user-coordinate-fix-x arg))
                            (:y-transform (list 'user-coordinate-fix-y arg))
                            (:coerce      (list 'float arg))
                            (t            arg)))))
)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun arglist-argnames (arglist)
  (let ((revargnames nil))
    (dolist (arg arglist)
      (cond ((fast-memq arg lambda-list-keywords))
        ((consp arg) (push (car arg) revargnames))
        (t (push arg revargnames))))
    (nreverse revargnames)))

;; this is not smart about special forms
(defun walk-body-for-args (args body)
  (let ((mutators (mapcar #'(lambda (s) (intern (symbol-format nil "SET-~A" s)))
                          args))
        (found nil)
        (mutators-found nil))
    (cond ((symbolp body)
          (when (and (member body args) (not (member body found)))
            (push body found))
          (when (and (member body mutators) (not (member body mutators-found)))
            (push body mutators-found)))
      ((listp body)
      (dolist (thing body)
        (cond ((symbolp thing)
                (when (and (member thing args) (not (member thing found)))
                  (push thing found))
                (when (and (member thing mutators) (not (member thing mutators-found)))
                  (push thing mutators-found)))
          ((listp thing)
            (multiple-value-bind (a m)
                                (walk-body-for-args args thing)
                                (setq found (union found a)
                                      mutators-found (union mutators-found m))))))))
    (values found mutators-found)))

(defmacro with-graphics-command-slots-bound (gc-arg args body)
  (multiple-value-bind (args-used mutators-used)
                      (walk-body-for-args args body)
                      (cond ((and (null args-used) (null mutators-used)) `,body)
                        (T
                          (let ((mutators (mapcar #'(lambda (s)
                                                            (intern (symbol-format nil "SET-~A" s)))
                                                  args)))
                            ;; easier than passing a list of mutators
                            `(let ,(mapcar #'(lambda (arg)
                                                    (list arg `(svref& ,gc-arg
                                                                        ,(1+ (position arg args)))))
                                          args-used)
                              (flet ,(mapcar #'(lambda (mut)
                                                        (list mut '(new-value)
                                                              `(setf (svref& ,gc-arg
                                                                              ,(1+ (position
                                                                                    mut mutators)))
                                                                    new-value)))
                                              mutators-used)
                                      ,body)))))))

(defmacro defgraphics-command ((name opcode
                                    &optional (optimize-recording? nil))
                              args
                              extents-form boxer-extents-form
                              dump-args dump-form
                              load-args load-form
                              deallocate-args deallocate-form
                              sprite-command
                              transform-template
                              copy-post-processing
                              &body draw-body)
  (flet ((numeric-declaration-args ()
                                  (with-collection
                                    (do* ((list-of-args args (cdr list-of-args))
                                          (arg (car list-of-args) (car list-of-args))
                                          (template-items transform-template (cdr template-items))
                                          (template-item (car template-items) (car template-items)))
                                      ((null list-of-args))
                                      (unless (null template-item)
                                        (collect arg))))))
        (let* ((boxer-command-name (intern (symbol-format nil "BOXER-~A" name)))
              (boxer-command-opcode (+ opcode *boxer-graphics-command-mask*))
              (wstruct-name
                (intern (symbol-format nil "WINDOW-GRAPHICS-COMMAND-~A" name)))
              (wmake-name
                (intern (symbol-format nil "MAKE-WINDOW-GRAPHICS-COMMAND-~A" name)))
              (wcopy-name
                (intern (symbol-format nil "COPY-WINDOW-GRAPHICS-COMMAND-~A" name)))
              (wcopy-struct-name (if (null copy-post-processing) wcopy-name
                                    (intern
                                    (symbol-format
                                      nil "%COPY-WINDOW-GRAPHICS-COMMAND-~A" name))))
              (bstruct-name
                (intern (symbol-format nil "BOXER-GRAPHICS-COMMAND-~A" name)))
              (bmake-name
                (intern (symbol-format nil "MAKE-BOXER-GRAPHICS-COMMAND-~A" name)))
              (bcopy-name
                (intern (symbol-format nil "COPY-BOXER-GRAPHICS-COMMAND-~A" name)))
              (bcopy-struct-name (if (null copy-post-processing) bcopy-name
                                    (intern
                                    (symbol-format
                                      nil "%COPY-BOXER-GRAPHICS-COMMAND-~A" name))))
              (window->boxer-name
                (intern (symbol-format nil "GRAPHICS-WINDOW->BOXER-~A-ALLOCATOR" name)))
              (boxer->window-name
                (intern (symbol-format nil "GRAPHICS-BOXER->WINDOW-~A-ALLOCATOR" name)))
              (recording-function
                (intern (symbol-format nil "RECORD-BOXER-GRAPHICS-COMMAND-~A" name)))
              (process-function
                (intern (symbol-format nil "Process Graphics Command ~A" name)))
              (window-binding-values-macro
                (intern (symbol-format nil "~A Window Binding Values Macro" name)))
              (boxer-binding-values-macro
                (intern (symbol-format nil "~A Boxer Binding Values Macro" name)))
              (window-extents-function
                (intern (symbol-format nil "~A Window Command Extents" name)))
              (boxer-extents-function
                (intern (symbol-format nil "~A Boxer Command Extents" name)))
              (dump-function-name
                (intern (symbol-format nil "GRAPHICS COMMAND ~A DUMPER" name)))
              (load-function-name
                (intern (symbol-format nil "GRAPHICS COMMAND ~A LOADER" name)))
              (sprite-command-translator-name
                (intern (symbol-format nil "~A SPRITE TRANSLATOR" name)))
              (wdeallocate-function (gensym))
              (bdeallocate-function (gensym)))
          `(progn
            (defstruct (,wstruct-name (:type vector)
                                      (:constructor ,wmake-name ,args)
                                      (:copier ,wcopy-struct-name))
              ;; slot 0 is used as an index into the dispatch table
              (type ,opcode)
              ,@args)
            ,(unless (null copy-post-processing)
              `(defun ,wcopy-name (gc)
                  (let ((graphics-command (,wcopy-struct-name gc)))
                    ,copy-post-processing
                    graphics-command)))
            ,(when (>=& opcode (svlength *graphics-command-copier-table*))
              ;; for that compile time error checking appeal
              (error "The *graphics-command-copier-table* is too short"))
            (setf (svref& *graphics-command-copier-table* ,opcode)
                  ',wcopy-name)
            (defstruct (,bstruct-name (:type vector)
                                      (:constructor ,bmake-name ,args)
                                      (:copier ,bcopy-struct-name))
              ;; slot 0 is used as an index into the dispatch table
              (type ,boxer-command-opcode)
              ,@args)
            ,(unless (null copy-post-processing)
              `(defun ,bcopy-name (gc)
                  (let ((graphics-command (,bcopy-struct-name gc)))
                    ,copy-post-processing
                    graphics-command)))
            ,(when (>=& boxer-command-opcode
                        (svlength *graphics-command-copier-table*))
              ;; for that compile time error checking appeal
              (error "The *graphics-command-copier-table* is too short"))
            (setf (svref& *graphics-command-copier-table*
                          ,boxer-command-opcode) ',bcopy-name)
            ;; this indirection is provided because we may want to
            ;; switch to some resource scheme for these command markers
            ;; instead of just consing them up on the fly
            (defun ,recording-function ,args
              (unless (not (null *supress-graphics-recording?*))
                ,(if optimize-recording?
                  `(if (eq *graphics-command-recording-mode* ':boxer)
                      (unless (check-existing-graphics-state-entries
                              ,boxer-command-opcode ,@args %graphics-list)
                        (sv-append %graphics-list (,bmake-name ,@args)))
                      (unless (check-existing-graphics-state-entries
                              ,opcode ,@args %graphics-list)
                        (sv-append %graphics-list (,wmake-name ,@args))))
                  `(sv-append %graphics-list
                              (if (eq *graphics-command-recording-mode*
                                      ':boxer)
                                (,bmake-name ,@args)
                                (,wmake-name ,@args))))))

            ;; extents
            (defun ,window-extents-function (graphics-command)
              (with-graphics-command-slots-bound graphics-command ,args
                ,extents-form))
            ,(when (>=& opcode (svlength *graphics-command-size-values-table*))
              ;; for that compile time error checking appeal
              (error "The *graphics-command-size-values-table* is too short"))
            (setf (svref& *graphics-command-size-values-table* ,opcode)
                  ',window-extents-function)
            (defun ,boxer-extents-function (graphics-command)
              (with-graphics-command-slots-bound graphics-command ,args
                ,boxer-extents-form))
            ,(when (>=& boxer-command-opcode
                        (svlength *graphics-command-size-values-table*))
              ;; for that compile time error checking appeal
              (error "The *graphics-command-size-values-table* is too short"))
            (setf (svref& *graphics-command-size-values-table*
                          ,boxer-command-opcode)
                  ',boxer-extents-function)

            ;; record the descriptor
            ,(when (>=& boxer-command-opcode
                        (svlength *graphics-command-descriptor-table*))
              ;; for that compile time error checking appeal
              (error "The *graphics-command-descriptor-table* is too short"))
            (setf (svref& *graphics-command-descriptor-table* ,opcode)
                  (make-graphics-command-descriptor :name ',name
                                                    :slots ',args
                                                    :transform-template
                                                    ',transform-template))
            ;; some other macros further down need this info to be defined at
            ;; macroexpansion time.  This is not a problem for implementations which
            ;; macroexpand as they eval (depth 1st, sort of).  For implementations
            ;; which macroexpand fully, before eval, we have to make sure these
            ;; important bits of info get defined
            ;; NOTE: we still need the other form to define the vars during load
            #+lispworks
            ,(progn (setf (svref& *graphics-command-descriptor-table* opcode)
                          (make-graphics-command-descriptor :name name
                                                            :slots args
                                                            :transform-template
                                                            transform-template))
                    nil)
            ;; and the back mapping
            (let ((entry (fast-assq ',name *graphics-command-name-opcode-alist*)))
              (if (null entry)
                (push (cons ',name ,opcode) *graphics-command-name-opcode-alist*)
                (setf (cdr entry) ,opcode)))
            ;; needed for decoding during further macroexpansion
            #+lispworks
            ,(when (null (fast-assq name *graphics-command-name-opcode-alist*))
              (push (cons name opcode) *graphics-command-name-opcode-alist*)
              nil)

            (setf (svref& *graphics-command-descriptor-table*
                          ,boxer-command-opcode)
                  (make-graphics-command-descriptor :name ',boxer-command-name
                                                    :slots ',args
                                                    :transform-template
                                                    ',transform-template))
            ;; needed for decoding during further macroexpansion
            #+lispworks
            ,(progn (setf (svref& *graphics-command-descriptor-table*
                                  boxer-command-opcode)
                          (make-graphics-command-descriptor :name boxer-command-name
                                                            :slots args
                                                            :transform-template
                                                            transform-template))
                    nil)
            ;; and the back mapping
            (let ((entry (fast-assq ',boxer-command-name
                                    *graphics-command-name-opcode-alist*)))
              (if (null entry)
                (push (cons ',boxer-command-name ,boxer-command-opcode)
                      *graphics-command-name-opcode-alist*)
                (setf (cdr entry) ,boxer-command-opcode)))
            ;; needed for decoding during further macroexpansion
            #+lispworks
            ,(when (null (fast-assq boxer-command-name
                                    *graphics-command-name-opcode-alist*))
              (push (cons boxer-command-name boxer-command-opcode)
                    *graphics-command-name-opcode-alist*)
              nil)

            ;; the default handlers for drawing the
            ;; objects directly into the graphics box
            (defun ,process-function (graphics-command)
              (with-graphics-command-slots-bound graphics-command ,args
                (progn ,@draw-body)))

            ;	   (let ,(let ((idx 0))
            ;		   (mapcar #'(lambda (arg)
            ;			       (incf idx)
            ;			       (list arg
            ;				     `(svref& graphics-command ,idx)))
            ;			   args))
            ;	     (declare (fixnum ,@(numeric-declaration-args)))
            ;	     ,@args
            ;	     (progn ,@draw-body)))
            ;; now install the drawing function in the dispatch table
            ,(when (>=& opcode (svlength *graphics-command-dispatch-table*))
              ;; for that compile time error checking appeal
              (error "The *graphics-command-dispatch-table* is too short"))
            (setf (svref& *graphics-command-dispatch-table* ,opcode)
                  ',process-function)

            ;; establishes a lexical environment for a particular graphics
            ;; command where the slots are bound to the names of the args
            ;; the default handler does this by itself, these macros are for
            ;; the benefit of various defgraphics-handlers to use
            (defmacro ,window-binding-values-macro (graphics-command &body body)
              `(let ((.graphics-command. ,graphics-command))
                (let
                  ,',(let ((idx 0))
                        (mapcar #'(lambda (arg)
                                          (incf idx)
                                          (list arg `(svref& .graphics-command. ,idx)))
                                args))
                  (declare (fixnum . ,',(numeric-declaration-args)))
                  ;; prevent bound but never used errors
                  (progn . ,',args)
                  ;; define local mutators...
                  (expand-mutators-and-body ,',args 1 ,@body))))
            ;; now install it
            ,(when (>=& opcode (svlength  *graphics-command-binding-values-table*))
              ;; for that compile time error checking appeal
              (error "The  *graphics-command-binding-values-table* is too short"))
            (setf (svref&  *graphics-command-binding-values-table* ,opcode)
                  ',window-binding-values-macro)

            (defmacro ,boxer-binding-values-macro (graphics-command &body body)
              `(let ((.graphics-command. ,graphics-command))
                (let
                  ,',(let ((idx 0))
                        (mapcar #'(lambda (arg)
                                          (incf idx)
                                          (list arg `(svref& .graphics-command. ,idx)))
                                args))
                  ;; sgithens TODO boxer-sunrise-22 Currently looking in to some issues where this comes
                  ;; in as a fixnum rather than a float.
                  ;; (declare (type boxer-float . ,',(numeric-declaration-args)))
                  ;; prevent bound but never used errors
                  (progn . ,',args)
                  ;; define local mutators...
                  (expand-mutators-and-body ,',args 1 ,@body))))
            ;; now install it
            ,(when (>=& boxer-command-opcode
                        (svlength  *graphics-command-binding-values-table*))
              ;; for that compile time error checking appeal
              (error "The  *graphics-command-binding-values-table* is too short"))
            (setf (svref&  *graphics-command-binding-values-table*
                          ,boxer-command-opcode) ',boxer-binding-values-macro)

            ;; if we move to a resource scheme, this will be useful and we
            ;; will have to install it into its own dispatch table like the
            ;; drawing functions
            (defun ,wdeallocate-function ,deallocate-args
              (with-graphics-command-slots-bound ,(car deallocate-args) ,args
                ,deallocate-form))
            ;; install it
            ,(when (>=& opcode (svlength *graphics-command-deallocation-table*))
              ;; for that compile time error checking appeal
              (error "The *graphics-command-deallocation-table* is too short"))
            (setf (svref& *graphics-command-deallocation-table* ,opcode)
                  ',wdeallocate-function)

            (defun ,bdeallocate-function ,deallocate-args
              (with-graphics-command-slots-bound ,(car deallocate-args) ,args
                ,deallocate-form))
            ;; install it
            ,(when (>=& boxer-command-opcode
                        (svlength *graphics-command-deallocation-table*))
              ;; for that compile time error checking appeal
              (error "The *graphics-command-deallocation-table* is too short"))
            (setf (svref& *graphics-command-deallocation-table*
                          ,boxer-command-opcode)
                  ',bdeallocate-function)

            ;; Conversion functions from Window->Boxer coordinates and back
            ;; these rely on being called within a with-graphics-vars-bound
            (defun ,window->boxer-name (window-command)
              (let ((graphics-command
                    (,bmake-name ,@(let ((idx 0))
                                      (mapcar #'(lambda (arg)
                                                        (declare (ignore arg))
                                                        (incf idx)
                                                        (expand-transform-template-item
                                                        `(svref& window-command ,idx)
                                                        (nth (1- idx) transform-template)
                                                        ':window->boxer))
                                              args)))))
                ,copy-post-processing
                graphics-command))
            ;; install it
            (setf (svref& *graphics-command-window->boxer-translation-table*
                          ,opcode)
                  ',window->boxer-name)
            (defun ,boxer->window-name (boxer-graphics-command)
              (let ((graphics-command
                    (,wmake-name ,@(let ((idx 0))
                                      (mapcar #'(lambda (arg)
                                                        (declare (ignore arg))
                                                        (incf idx)
                                                        (expand-transform-template-item
                                                        `(svref& boxer-graphics-command ,idx)
                                                        (nth (1- idx) transform-template)
                                                        ':boxer->window))
                                              args)))))
                ,copy-post-processing
                graphics-command))
            (setf (svref& *graphics-command-boxer->window-translation-table*
                          ,opcode)
                  ',boxer->window-name)

            ;; File system interface
            (defun ,dump-function-name ,dump-args
              ,@(arglist-argnames dump-args)
              ,dump-form)
            (setf (svref& *graphics-command-dumper-dispatch-table* ,opcode)
                  ',dump-function-name
                  (svref& *graphics-command-dumper-dispatch-table*
                          ,boxer-command-opcode)
                  ',dump-function-name)
            (defun ,load-function-name ,load-args
              ,@(arglist-argnames load-args)
              ,load-form)
            (setf (svref& *graphics-command-loader-dispatch-table* ,opcode)
                  ',load-function-name
                  (svref& *graphics-command-loader-dispatch-table*
                          ,boxer-command-opcode)
                  ',load-function-name)

            ;; back translation to sprite commands
            (defun ,sprite-command-translator-name (command)
              (declare (special last-x last-y))
              (with-graphics-command-slots-bound command ,args
                ,sprite-command))
            (setf (svref& *graphics-command-sprite-command-translation-table*
                          ,opcode)
                  ',sprite-command-translator-name)

            ;; now make the function for drawing on the window (that also records)
            (defun ,name ,args
              ,@args
              (progn ,@draw-body))

            ;; finally return the name (as opposed to random returned values)
            ',name))))

) ; eval-when

;;; this is mostly for readability
;; (eval (compile load eval)
(defmacro defgraphics-state-change ((name opcode) args
                                                  &key
                                                  extents-form boxer-extents-form
                                                  (dump-args '(command stream &optional
                                                                       (type :turtle-shape)))
                                                  (dump-form
                                                   '(dump-boxer-thing command stream))
                                                  (load-args '(command &optional
                                                                       (type :turtle-shape)))
                                                  (load-form 'command)
                                                  (deallocate-args '(graphics-command))
                                                  (deallocate-form 'graphics-command)
                                                  sprite-command
                                                  body)
  `(defgraphics-command (,name ,opcode t)
     ,args
     ,(or extents-form '(progn graphics-command (values 0 0 0 0 t)))
     ,(or boxer-extents-form extents-form '(progn graphics-command
                                                  (values 0.0 0.0 0.0 0.0 t)))
     ,dump-args ,dump-form
     ,load-args ,load-form
     ,deallocate-args ,deallocate-form
     ,sprite-command
     ,(make-list (length args)) nil ,body))
;; )

(defmacro defgraphics-handler ((name &optional
                                     (table
                                      '*graphics-command-translation-table*))
                               extra-args &body body)
  (let ((handler-name (intern (symbol-format nil "~A Graphics Handler ~A"
                                             name (gensym))))
        (handler-opcode (graphics-command-opcode name)))
    ;; some of that ole' compile time error checking appeal...
    (cond ((null table)
           (error "Need a table to put the handlers in"))
      ((vectorp table)
       (when (>= handler-opcode (svlength table))
         (error "The table, ~A, is too short for an opcode of ~D"
                table handler-opcode)))
      ((symbolp table)
       (let ((value (symbol-value table)))
         (if (vectorp value)
           (when (>= handler-opcode (svlength value))
             (error "The table, ~A, is too short for an opcode of ~D"
                    table handler-opcode))
           (error "Hey, ~A doesn't look like a handler table" table))))
      (t (error "fooey !")))
    (cond ((null body)
           ;; a null body means that we should copy the default handler
           `(if (null (svref& *graphics-command-dispatch-table*
                              ,handler-opcode))
              (error "There is NO default command for ~S" ',name)
              (setf (svref& ,table ,handler-opcode)
                    (svref& *graphics-command-dispatch-table*
                            ,handler-opcode))))
      (t
       `(progn
         (defun ,handler-name (graphics-command . ,extra-args)
           ,@extra-args
           (graphics-command-values ,name graphics-command
                                     . ,body))
         (setf (svref& ,table ,handler-opcode) ',handler-name)
         ',name)))))


;;; Used to define arbitrary transformations between the boxer/floating
;;; representations and the window/fixnum representations
;;; translation clauses should be a list of forms.
;;; The CAR of each form should be the name of a slot in the graphics command
;;; and the CADR of each form should be a form to be called which translates
;;; the slot.  The translating form is called in an environment where the
;;; slots of the originating form as well as the EXTRA-ARGS are bound
;;;
;;; This tries to be smart and use info from the transformation-template
;;; when none is provided
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro defgraphics-translator ((name &optional
                                        (table '*turtle-translation-table*)
                                        (direction :boxer->window))
                                  extra-args translation-clauses)
  (let* ((handler-name (intern (symbol-format nil "~A Graphics Translator ~A"
                                              name (gensym))))
        (handler-opcode (if (eq direction :window->boxer)
                          (graphics-command-opcode name)
                          (+ (graphics-command-opcode name)
                              *boxer-graphics-command-mask*)))
        (command-descriptor (get-graphics-command-descriptor handler-opcode))
        (template (graphics-command-descriptor-transform-template
                    command-descriptor))
        )
    ;; some of that ole' compile time error checking appeal...
    (cond ((null table)
          (error "Need a table to put the handlers in"))
      ((vectorp table)
      (when (>= handler-opcode (svlength table))
        (error "The table, ~A, is too short for an opcode of ~D"
                table handler-opcode)))
      ((symbolp table)
      (let ((value (symbol-value table)))
        (if (vectorp value)
          (when (>= handler-opcode (svlength value))
            (error "The table, ~A, is too short for an opcode of ~D"
                    table handler-opcode))
          (error "Hey, ~A doesn't look like a handler table" table))))
      (t (error "fooey !")))
    `(progn
      (defun ,handler-name (from-gc to-gc . ,extra-args)
        ,@extra-args   ;; handle bound but never used errors
        (graphics-command-values ,handler-opcode from-gc
                                  ,@(with-collection
                                      (dolist (slot (graphics-command-descriptor-slots
                                                    command-descriptor))
                                        (let ((tform (assoc slot translation-clauses))
                                              (offset (graphics-command-slot-offset
                                                      command-descriptor slot)))
                                          (collect
                                          `(setf (svref& to-gc ,offset)
                                                  ,(if (not (null tform))
                                                    (cadr tform)
                                                    (let ((template-action (nth (1- offset)
                                                                                template)))
                                                      (expand-transform-template-item
                                                        slot template-action direction))))))))))
      (setf (svref& ,table ,handler-opcode) ',handler-name)
      ',handler-name)))

)


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

(defmacro defstandard-graphics-handlers ((name opcode)
                                         &key
                                         command-args
                                         extents-form
                                         (boxer-extents-form extents-form)
                                         (dump-args '(command stream &optional
                                                              (type :turtle-shape)))
                                         (dump-form
                                          '(dump-boxer-thing command stream))
                                         (load-args '(command &optional
                                                              (type :turtle-shape)))
                                         (load-form 'command)
                                         sprite-command
                                         transformation-template
                                         copy-post-processing
                                         command-body
                                         translation-args
                                         translation-body
                                         translation-and-scaling-args
                                         translation-and-scaling-body
                                         turtle-translator-args
                                         turtle-translator-clauses
                                         (deallocate-args '(graphics-command))
                                         (deallocate-form 'graphics-command))
  `(progn
    (defgraphics-command (,name ,opcode)
      ,command-args
      ,extents-form ,boxer-extents-form
      ,dump-args ,dump-form ,load-args ,load-form
      ,deallocate-args ,deallocate-form
      ,sprite-command
      ,transformation-template ,copy-post-processing
      ,command-body)
    (defgraphics-handler (,name *graphics-command-translation-table*)
      ,translation-args
      ,translation-body)
    (defgraphics-handler (,name
                          *graphics-command-translation-and-scaling-table*)
      ,translation-and-scaling-args
      ,translation-and-scaling-body)
    (defgraphics-translator (,name)
      ,turtle-translator-args ,turtle-translator-clauses)))




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
  (setq *initial-graphics-state-current-pen-color* *foreground-color*
        *graphics-state-current-pen-color* *foreground-color*))

(def-redisplay-initialization ; :turtle-shape
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
          %graphics-list)))

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

;;; peephole optimizer support

;; walk back looking for entries in the graphics list that
;; can be changed rather than blindly appending to the graphics list
;; for now, we just peek at the last entry.  At some point, we may
;; want to continue backwards until we hit a non state change entry.
(defun check-existing-graphics-state-entries (opcode new-value graphics-list)
  (let ((al (storage-vector-active-length graphics-list)))
    (unless (zerop& al)
      (let ((last-entry (sv-nth (1-& al) graphics-list)))
        (cond ((=& opcode (svref& last-entry 0))
               (setf (svref& last-entry 1) new-value)
               T)
          (t nil))))))

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

(defun make-turtle-window-shape (&optional (shape *default-turtle-shape*))
  (let ((tws (%%make-turtle-window-shape
              :contents (allocate-c-vector
                         (storage-vector-max-length shape)))))
    (do-vector-contents (gc shape)
      (sv-append tws (allocate-boxer->window-command gc)))
    tws))


(defun translate-graphics-command-list (gl trans-x trans-y)
  (do-vector-contents (graphics-command gl)
    (translate-graphics-command graphics-command trans-x trans-y)))

(defun translate-and-scale-graphics-command-list (gl trans-x trans-y
                                                     scale-x scale-y)
  (do-vector-contents (graphics-command gl)
    (translate-and-scale-graphics-command graphics-command
                                          trans-x trans-y
                                          scale-x scale-y)))

;;; should go somewhere else eventually, doesn't hack clipping
;;; should also handle "boxer-bit-gravity" eventually , for now,
;;; the "bit gravity" will be :TOP-RIGHT but we might want to
;;; make it be :CENTER to follow the sprite coordinate system
;;; (maybe have scaling too)

(defvar *boxer-graphics-box-bit-gravity* ':center)

(defvar *scale-on-resize?* nil)

(defvar *update-bitmap?* t)

;; *update-bitmap?* is for use inside of a mouse resize box loop to avoid thrashing
;; the memory with every twitch of the mouse
(defun resize-graphics-sheet (sheet new-wid new-hei)
  (let* ((old-wid (graphics-sheet-draw-wid sheet))
         (old-hei (graphics-sheet-draw-hei sheet))
         (wid-scale (/ new-wid (float old-wid)))
         (hei-scale (/ new-hei (float old-hei))))
    (when (and *update-bitmap?* (not (null (graphics-sheet-bit-array sheet))))
      (let ((old-bitmap (graphics-sheet-bit-array sheet))
            (new-bitmap (make-offscreen-bitmap *boxer-pane*
                                               new-wid new-hei)))
        ;; if the new bitmap is bigger, initialize it
        (when (or (> new-wid old-wid) (> new-hei old-hei))
          (clear-offscreen-bitmap new-bitmap (or (graphics-sheet-background sheet)
                                                 *background-color*))
          )
        ;; now move the old contents into the new bitmap
        (case *boxer-graphics-box-bit-gravity*
          (:top-right
           (copy-offscreen-bitmap
            alu-seta (min& old-wid new-wid) (min& old-hei new-hei)
            old-bitmap 0 0 new-bitmap 0 0))
          (:center
           (copy-offscreen-bitmap
            alu-seta (min& old-wid new-wid) (min& old-hei new-hei)
            old-bitmap
            (max& 0 (round (-& old-wid new-wid) 2))
            (max& 0 (round (-& old-hei new-hei) 2))
            new-bitmap
            (max& 0 (round (-& new-wid old-wid) 2))
            (max& 0 (round (-& new-hei old-hei) 2)))))
        ;; now install the new bitmap
        (setf (graphics-sheet-bit-array sheet) new-bitmap)
        ;; we might not want to do this if sprites are allowed to have
        ;; pointers to raw pixmaps in their shapes
        (free-offscreen-bitmap old-bitmap)))
    (when (not (null (graphics-sheet-graphics-list sheet)))
      (ecase *boxer-graphics-box-bit-gravity*
             (:top-right
              (when *scale-on-resize?*
                (translate-and-scale-graphics-command-list
                 (graphics-sheet-graphics-list sheet)
                 0 0 wid-scale hei-scale)))
             (:center
              (cond ((null *scale-on-resize?*)
                     (translate-graphics-command-list
                      (graphics-sheet-graphics-list sheet)
                      (round (-& new-wid old-wid) 2)
                      (round (-& new-hei old-hei) 2))
                     )
                (t
                 (translate-graphics-command-list
                  (graphics-sheet-graphics-list sheet)
                  (-& (round old-wid 2)) (-& (round old-hei 2)))
                 (translate-and-scale-graphics-command-list
                  (graphics-sheet-graphics-list sheet)
                  (round new-wid 2) (round new-hei 2)
                  wid-scale hei-scale))))))
    (when (not (null (graphics-sheet-object-list sheet)))
      ;; (maybe) move sprites to a new position
      ;; don't move if there are no sprites or if the box is in :clip mode
      (ecase *boxer-graphics-box-bit-gravity*
             (:top-right)
             (:center
              (cond ((null *scale-on-resize?*)
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
                                   (wrap-y-coordinate (y-position obj))))

                         ;		    (let* ((home-pos (home-position obj))
                         ;			   (home-x (car home-pos))
                         ;			   (home-y (cadr home-pos)))
                         ;		      (set-x-position obj (* (signum home-x)
                         ;					     (max (abs home-x)
                         ;						  (/ new-wid 2))))
                         ;		      (set-y-position obj (* (signum home-y)
                         ;					     (max (abs home-y)
                         ;						  (/ new-wid 2)))))
                         )
                       ;; if there is a window-shape cache, invalidate it
                       (flush-window-shape-cache obj)))
                (t
                 (dolist (obj (graphics-sheet-object-list sheet))
                   (when (not (eq (graphics-sheet-draw-mode sheet) ':clip))
                     (set-x-position obj (* (x-position obj) wid-scale))
                     (set-y-position obj (* (y-position obj) hei-scale)))
                   ;; if there is a window-shape cache, invalidate it
                   (flush-window-shape-cache obj)))))))
    (setf (graphics-sheet-draw-wid sheet) new-wid)
    (setf (graphics-sheet-draw-hei sheet) new-hei)))


(defun flush-window-shape-cache (turtle)
  (let ((ws (slot-value turtle 'window-shape)))
    (setf (turtle-window-shape-valid ws) nil)
    ;; extents are checks separately (see sprite-at-window-point for details)
    (setf (turtle-window-shape-min-graphics-x-extent ws) nil)
    (dolist (ss (subsprites turtle)) (flush-window-shape-cache ss))))



;;; Graphics State Changes

(eval-when (:compile-toplevel :load-toplevel :execute)
(defgraphics-state-change (change-alu 0) (new-alu)
  :dump-form
  (let ((existing-alu (svref& command 1)))
    (unwind-protect
    (progn (setf (svref& command 1) (canonicalize-file-alu existing-alu))
            (dump-boxer-thing command stream))
    (setf (svref& command 1) existing-alu)))
  :load-form
  (setf (svref& command 1) (reallocate-file-alu (svref& command 1)))
  :sprite-command
  (list (case new-alu
          (#.alu-xor 'bu::penreverse)
          ((#.alu-ior #.alu-seta) 'bu::pendown)
          ((#.alu-andca #.alu-setz) 'bu::penerase)
          (t (warn "Untranslatable alu ~A, assuming PENDOWN" new-alu)
            'bu::pendown)))
  :body
  (unless (=& new-alu *graphics-state-current-alu*)
    (setq *graphics-state-current-alu* new-alu)))
)

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

(defgraphics-translator (change-alu) (trans-x trans-y cos-scale sin-scale
                                              scale)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
           (defgraphics-state-change (change-pen-width 1) (new-width)
             :extents-form (progn (setq *graphics-state-current-pen-width* new-width)
                                  ;; need to update because other graphics command
                                  ;; extent forms rely on an accurate value for pen-width
                                  (values 0 0 0 0 t))
             :sprite-command
             (list 'bu::set-pen-width new-width)
             :body
             (unless (=& new-width *graphics-state-current-pen-width*)
               (setq *graphics-state-current-pen-width* new-width)
               (%set-pen-size new-width)))
)

(defgraphics-translator (change-pen-width) (trans-x trans-y
                                                    cos-scale sin-scale
                                                    scale)
  ())

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

(eval-when (:compile-toplevel :load-toplevel :execute)
(defgraphics-state-change (change-graphics-font 2) (new-font-no)
  :extents-form (progn (setq *graphics-state-current-font-no* new-font-no)
                      ;; need to update because other graphics command
                      ;; extent forms rely on an accurate value for pen-width
                      (values 0 0 0 0 t))
  :dump-form (cond ((>=& *version-number* 12)
                    ;; guts of a dump-array
                    (enter-table 'fake-array)
                    (write-file-word bin-op-initialize-and-return-array stream)
                    (dump-array-1 stream 2 nil) (dump-boxer-thing 2 stream)
                    (dump-boxer-thing (svref& command 0) stream)
                    (dump-font (svref& command 1) stream))
              (t (dump-boxer-thing command stream)))
  :load-form (when (>=& *version-number* 12)
              (setf (svref& command 1)
                    (make-font-from-file-value (svref& command 1))))
  :sprite-command
  (list 'bu::set-type-font new-font-no)
  :body
  (unless (=& new-font-no *graphics-state-current-font-no*)
    ;; have to check for possible font
    (setq *graphics-state-current-font-no* new-font-no)))
)

(defgraphics-translator (change-graphics-font) (trans-x trans-y
                                                        cos-scale sin-scale
                                                        scale)
  ())

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
             #+mcl
             (fixnum color);; old version, don't barf, just return a reasonable value
             ;   (symbol (allocate-named-color color)) ;;a predefined color(unused for now)
             (null   color);; NIL is the default color for turtle shapes
             #+lispworks
             (integer (%make-color-from-bytes (ldb (byte 8 16) color)
                                              (ldb (byte 8 8)  color)
                                              (ldb (byte 8 0)  color)))
             (list   (if (> (length color) 3)
                       (%make-color (car color) (cadr color)(caddr color)(cadddr color))
                       (%make-color (car color) (cadr color) (caddr color))))))

;; should opengl use :deallocate-form to free color memory ?  is color in use elsewhere ?
(eval-when (:compile-toplevel :load-toplevel :execute)
           (defgraphics-state-change (change-graphics-color 4) (new-color)
             :dump-form
             (let ((existing-pixel (svref& command 1)))
               (unwind-protect
                (progn (setf (svref& command 1)
                             (if (>=& *version-number* 12)
                               (pixel-dump-value existing-pixel)
                               (canonicalize-pixel-color existing-pixel)))
                       (dump-boxer-thing command stream))
                (setf (svref& command 1) existing-pixel)))
             :load-form
             (setf (svref& command 1)
                   (reallocate-pixel-color (svref& command 1)))
             :sprite-command
             (list 'bu::set-pen-color new-color)
             :body
             (unless (color= new-color *graphics-state-current-pen-color*)
               (setq *graphics-state-current-pen-color* new-color)
               (%set-pen-color new-color)))
)

(defgraphics-translator (change-graphics-color) (trans-x trans-y
                                                         cos-scale sin-scale
                                                         scale)
  ())



(defun sprite-commands-for-new-position (new-x new-y)
  (list 'bu::penup 'bu::setxy new-x new-y 'bu::pendown))

;;;; Commands that draw

;;; Lines

;;; temporary fix to keep Window systems from blowing out when
;;; some kid types FORWARD 239823094230923490
;;;
;;; In theory, this should get captured at a higher level
;;; in the STEPS-ARG-CHECK function but that doesn't deal in
;;; window coords so it can be fooled
;;;
(defun ensure-legal-window-coordinate (n)
  (cond ((< n #.(min-window-coord))
         (warn "window system coordinate ~D too small, changing to ~D"
               n #.(min-window-coord))
         #.(min-window-coord))
    ((>= n #.(max-window-coord))
     (warn "window system coordinate ~D too large, changing to ~D"
           n #.(max-window-coord))
     #.(max-window-coord))
    (t n)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defstandard-graphics-handlers (line-segment 3)
  :COMMAND-ARGS (x0 y0 x1 y1)
  :EXTENTS-FORM
  (let ((delta #-mcl (ceiling *graphics-state-current-pen-width* 2)
              ;; this has to stay until the non centered thick line bug in
              ;; the mac implementation gets fixed
              #+mcl *graphics-state-current-pen-width*))
    (values (-& (min& x0 x1) delta) (-& (min& y0 y1) delta)
            (+& (max& x0 x1) delta) (+& (max& y0 y1) delta)))
  :BOXER-EXTENTS-FORM
  (let ((delta #-mcl (ceiling *graphics-state-current-pen-width* 2)
              ;; this has to stay until the non centered thick line bug in
              ;; the mac implementation gets fixed
              #+mcl *graphics-state-current-pen-width*))
    (values (- (min x0 x1) delta) (- (min y0 y1) delta)
            (+ (max x0 x1) delta) (+ (max y0 y1) delta)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :x-transform :y-transform)
  :COMMAND-BODY
  (unless (zerop *graphics-state-current-pen-width*)
    (ck-mode-draw-line x0 y0 x1 y1 *graphics-state-current-alu*)
  )
  :sprite-command
  (cond ((and (= x0 last-x) (= y0 last-y))
        (setq last-x x1 last-y y1)
        (list 'bu::setxy x1 y1))
    (t
    (setq last-x x1 last-y y1)
    (append (sprite-commands-for-new-position x0 y0)
            (list 'bu::setxy x1 y1))))
  :TRANSLATION-ARGS
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x0 (+& x0 trans-x)) (set-y0 (+& y0 trans-y))
        (set-x1 (+& x1 trans-x)) (set-y1 (+& y1 trans-y)))
  ;; translation and scaling (for window GC's)
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x0 (+& (fixr (* x0 scale-x)) trans-x))
        (set-y0 (+& (fixr (* y0 scale-y)) trans-y))
        (set-x1 (+& (fixr (* x1 scale-x)) trans-x))
        (set-y1 (+& (fixr (* y1 scale-y)) trans-y)))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x0 (fix-array-coordinate-x
        (+ trans-x (+ (* cos-scale x0) (* sin-scale y0)))))
  (y0 (fix-array-coordinate-y
        (+ trans-y (- (* cos-scale y0) (* sin-scale x0)))))
  (x1 (fix-array-coordinate-x
        (+ trans-x (+ (* cos-scale x1) (* sin-scale y1)))))
  (y1 (fix-array-coordinate-y
        (+ trans-y (- (* cos-scale y1) (* sin-scale x1))))))
  )
) ;eval-when


;;; opcode 4 is used by change-color


;;; Strings

(eval-when (:compile-toplevel :load-toplevel :execute)
(defstandard-graphics-handlers (centered-string 7)
  :COMMAND-ARGS (x y string)
  :EXTENTS-FORM
  (let ((height 0) (s string) (width 0) (wx x))
    (loop
      (setq height (+ height (1+& (string-hei
                                  *graphics-state-current-font-no*)))
            width (max& (ceiling (string-wid *graphics-state-current-font-no*
                                            (subseq s 0 (position #\newline s))))
                        width)
            wx (min& wx (fixr (- x (/ width 2)))))
      ;; If we have handled the last line (the current line has no CR's)
      (if (not (position #\newline s))
        (return (values wx y (+& wx width) (+& y height)))
        (setq s (subseq s (let ((p (position #\newline s)))
                            (if (null p) 0 (1+& p))))))))
  :BOXER-EXTENTS-FORM
  (let ((height 0) (s string) (width 0) (wx x))
    (loop
      (setq height
            (+ height (1+(string-hei *graphics-state-current-font-no*)))
            width (max (string-wid *graphics-state-current-font-no*
                                  (subseq s 0 (position #\newline s)))
                      width)
            wx (min wx (- x (/ width 2))))
      ;; If we have handled the last line (the current line has no CR's)
      (if (not (position #\newline s))
        (return (values (coerce wx 'boxer-float) (coerce y 'boxer-float)
                        (coerce (+ wx width) 'boxer-float)
                        (coerce (+ y height) 'boxer-float)))
        (setq s (subseq s (let ((p (position #\newline s)))
                            (if (null p) 0 (1+& p))))))))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil)
  :COMMAND-BODY
  ;; Yuck, rewrite these to remove all the string CONSing
  (do* ((height (1+& (string-hei *graphics-state-current-font-no*)))
        (s string (subseq s (let ((p (position #\newline s)))
                              (if (null p) 0 (1+& p)))))
        (trimmed-string (subseq s 0 (position #\newline s))
                        (subseq s 0 (position #\newline s)))
        (width (ceiling (string-wid *graphics-state-current-font-no* trimmed-string))
              (ceiling (string-wid *graphics-state-current-font-no* trimmed-string)))
        (wx (fixr (- x (/ width 2))) (fixr (- x (/ width 2))))
        (wy (fixr y) (+& wy height)))
    ((not (position #\newline s))
    (draw-string *graphics-state-current-font-no* trimmed-string
                  (ensure-legal-window-coordinate (scale-x wx))
                  (ensure-legal-window-coordinate (scale-y wy))))
    (draw-string *graphics-state-current-font-no* trimmed-string
                  (ensure-legal-window-coordinate (scale-x wx))
                  (ensure-legal-window-coordinate (scale-y wy))))
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::type (make-box (list (list (coerce string
                                                      'simple-string))))))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::type
                  (make-box (list (list (coerce string
                                                'simple-string))))))))
  :TRANSLATION-ARGS
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y)))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))))

(defstandard-graphics-handlers (left-string 8)
  :COMMAND-ARGS (x y string)
  :EXTENTS-FORM
  (let ((height 0) (s string) (width 0))
    (loop
      (setq height (+ height 1(string-hei *graphics-state-current-font-no*))
            width (max (string-wid *graphics-state-current-font-no*
                                  (subseq s 0 (position #\newline s)))
                      width))
      ;; If we have handled the last line (the current line has no CR's)
      (if (not (position #\newline s))
        (return (values x y (+ x width) (+ y height)))
        (setq s (subseq s (let ((p (position #\newline s)))
                            (if (null p) 0 (1+& p))))))))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil)
  :COMMAND-BODY
  (let ((current-y y) (s string))
    (loop
      (draw-string *graphics-state-current-font-no*
                    (subseq s 0 (position #\newline s))
                    (ensure-legal-window-coordinate (scale-x(fixr x)))
                    (ensure-legal-window-coordinate (scale-y(fixr current-y))))
      ;; If we have drawn the last line (the current line has no CR's)
      (if (not (position #\newline s))
        (return)
        (setq s (subseq s (let ((p (position #\newline s)))
                            (if (null p) 0 (1+& p))))
              current-y (+ current-y 1
                          (string-hei *graphics-state-current-font-no*))))))
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::ltype (make-box (list (list (coerce string
                                                        'simple-string))))))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::ltype
                  (make-box (list (list (coerce string
                                                'simple-string))))))))
  :TRANSLATION-ARGS
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y)))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))))

(defstandard-graphics-handlers (right-string 9)
  :COMMAND-ARGS (x y string)
  :EXTENTS-FORM
  (let ((height 0) (s string) (width 0))
    (loop
      (setq height (+ height 1 (string-hei *graphics-state-current-font-no*))
            width (max (string-wid *graphics-state-current-font-no*
                                  (subseq s 0 (position #\newline s)))
                      width))
      ;; If we have handled the last line (the current line has no CR's)
      (if (not (position #\newline s))
        (return (values (- x width) y x (+ y height)))
        (setq s (subseq s (let ((p (position #\newline s)))
                            (if (null p) 0 (1+& p))))))))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil)
  :COMMAND-BODY
  (let ((current-y y) (s string) (width 0) (trimmed-string ""))
    (loop
      (setq trimmed-string (subseq s 0 (position #\newline s))
            width (string-wid *graphics-state-current-font-no* trimmed-string))
      (draw-string *graphics-state-current-font-no*
                    trimmed-string
                    (ensure-legal-window-coordinate
                    (scale-x (fixr (- x width))))
                    (ensure-legal-window-coordinate
                    (scale-y (fixr current-y))))
      ;; If we have drawn the last line (the current line has no CR's)
      (if (not (position #\newline s))
        (return)
        (setq s (subseq s (let ((p (position #\newline s)))
                            (if (null p) 0 (1+& p))))
              current-y (+ current-y
                          (1+ (string-hei
                                *graphics-state-current-font-no*)))))))
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::rtype (make-box (list (list (coerce string
                                                        'simple-string))))))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::rtype
                  (make-box (list (list (coerce string
                                                'simple-string))))))))
  :TRANSLATION-ARGS
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y)))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))))


;;; Rectangles

(defstandard-graphics-handlers (centered-rectangle 10)
  :COMMAND-ARGS (x y width height)
  :EXTENTS-FORM
  ;    (let ((half-width (values (round width 2)))
  ;	  (half-height (values (round height 2))))
  ;      (declare (fixnum half-width half-height))
  ;      (values (-& x half-width) (-& y half-height)
  ;	      (+& x half-width) (+& y half-height)))

  ;    (values (-& x (values (floor width 2))) (-& y (values (floor height 2)))
  ;            (+& x (values (ceiling width 2))) (+& y (values (ceiling height 2))))
  ;; should be the same as floor, ceiling but half the ops
  (multiple-value-bind (half-width wfudge) (truncate width 2)
                      (multiple-value-bind (half-height hfudge) (truncate height 2)
                                            (values (-& x half-width) (-& y half-height)
                                                    (+& x half-width wfudge) (+& y half-height hfudge))))
  :BOXER-EXTENTS-FORM
  (let ((half-width (values (/ width 2.0)))
        (half-height (values (/ height 2.0))))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-rect width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-rect width height))))
  :COMMAND-BODY
  (unless (or (zerop width) (zerop height))
    (draw-rectangle (fixr width) (fixr height)
                    (ensure-legal-window-coordinate
                      (scale-x (-& x (floor (the fixnum width) 2))))
                    (ensure-legal-window-coordinate
                      (scale-y (-& y (floor (the fixnum height) 2))))))
  :TRANSLATION-ARGS
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y))
        (set-width (fixr (* width scale-x)))
        (set-height (fixr (* height scale-y))))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))
  (width  (fixr (* width  scale)))
  (height (fixr (* height scale)))))


;; maybe this should be a circle ? Need to check relative speeds
;; also, should probably use the pen-width ?
(defstandard-graphics-handlers (dot 11)
  :COMMAND-ARGS (x y)
  :EXTENTS-FORM
  (multiple-value-bind (half-size fudge)
                      (truncate *graphics-state-current-pen-width* 2)
                      (values (-& x half-size) (-& y half-size)
                              (+& x half-size fudge) (+& y half-size fudge)))
  :BOXER-EXTENTS-FORM
  (let ((half-size (/ *graphics-state-current-pen-width* 2.0)))
    (declare (type boxer-float half-size))
    (values (float-minus x half-size) (float-minus y half-size)
            (float-plus x half-size) (float-plus y half-size)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::dot))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::dot))))
  :COMMAND-BODY
  (unless (zerop *graphics-state-current-pen-width*)
    (draw-rectangle *graphics-state-current-pen-width*
                    *graphics-state-current-pen-width*
                    (ensure-legal-window-coordinate
                      (scale-x (-& x
                                  (floor
                                    (the fixnum
                                        *graphics-state-current-pen-width*)
                                    2))))
                    (ensure-legal-window-coordinate
                      (scale-y (-& y
                                  (floor
                                    (the fixnum
                                        *graphics-state-current-pen-width*)
                                    2))))))
  :TRANSLATION-ARGS
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y)))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))))

(defstandard-graphics-handlers (hollow-rectangle 12)
  :COMMAND-ARGS (x y width height)
  :EXTENTS-FORM
  (multiple-value-bind (half-width wfudge) (truncate width 2)
                      (multiple-value-bind (half-height hfudge) (truncate height 2)
                                            (values (-& x half-width) (-& y half-height)
                                                    (+& x half-width wfudge) (+& y half-height hfudge))))
  :BOXER-EXTENTS-FORM
  (let ((half-width (values (/ width 2.0)))
        (half-height (values (/ height 2.0))))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-hollow-rect width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-hollow-rect width height))))
  :COMMAND-BODY
  (unless (or (zerop width) (zerop height))
    (let ((xxx (ensure-legal-window-coordinate
                (scale-x (-& x (floor (the fixnum width) 2)))))
          (yyy (ensure-legal-window-coordinate
                (scale-y (-& y (floor (the fixnum height) 2)))))
          (thick (fixr *graphics-state-current-pen-width*))
          (wid (fixr width)) (hei (fixr height)))
      (cond ((or (>=& (*& thick 2) wid)
                (>=& (*& thick 2) hei))
            ;; degenerate cases where walls touch
            (draw-rectangle wid hei xxx yyy))
        (t
        ;; left wall
        (draw-rectangle thick hei xxx yyy)
        ;; top
        (draw-rectangle (-& wid (*& 2 thick)) thick (+& xxx thick) yyy)
        ;; right
        (draw-rectangle thick hei (+& xxx (-& wid thick)) yyy)
        ;; bottom
        (draw-rectangle (-& wid (*& 2 thick)) thick (+& xxx thick)
                          (+& yyy (-& hei thick)))))))
  :TRANSLATION-ARGS
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y))
        (set-width (fixr (* width scale-x)))
        (set-height (fixr (* height scale-y))))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))
  (width  (fixr (* width  scale)))
  (height (fixr (* height scale)))))

(defstandard-graphics-handlers (centered-bitmap 15)
  :COMMAND-ARGS (bitmap x y width height)
  :EXTENTS-FORM
  (multiple-value-bind (half-width wfudge) (truncate width 2)
                      (multiple-value-bind (half-height hfudge) (truncate height 2)
                                            (values (-& x half-width) (-& y half-height)
                                                    (+& x half-width wfudge) (+& y half-height hfudge))))
  :BOXER-EXTENTS-FORM
  (let ((half-width (/ width 2.0))
        (half-height (/ height 2.0)))
    (declare (type boxer-float half-width half-height))
    ;; removed float-plus/minus because x & y are not floats
    (values (- x half-width) (- y half-height)
            (+ x half-width) (+ y half-height)))
  :TRANSFORMATION-TEMPLATE
  (nil :x-transform :y-transform :coerce :coerce)
  :copy-post-processing
  (with-graphics-command-slots-bound graphics-command (bitmap x y width height)
    (set-bitmap (new-offscreen-copy bitmap)))
  :deallocate-args (graphics-command)
  :deallocate-form (free-offscreen-bitmap bitmap)
  :DUMP-FORM ;; need special handling for the bitmap...
  (with-graphics-command-slots-bound command (bitmap x y width height)
    (progn
    ;; faking a dump of a simple array (which is the command)
    (enter-table command) ; we need to do this so load table index is correct
    (write-file-word bin-op-initialize-and-return-array stream)
    (multiple-value-bind (dims opts) (decode-array command)
                          (dump-array-1 stream dims opts))
    (dump-boxer-thing (length command) stream)
    ;; now the contents
    (dump-boxer-thing (svref& command 0) stream) ; opcode
    (dump-pixmap bitmap stream)
    (dump-boxer-thing x stream)
    (dump-boxer-thing y stream)
    (dump-boxer-thing width stream)
    (dump-boxer-thing height stream)))
  :COMMAND-BODY
  (%bitblt-to-screen (fixr width) (fixr height)
                    bitmap 0 0
                    (ensure-legal-window-coordinate
                      (scale-x (-& x (floor (the fixnum width) 2))))
                    (ensure-legal-window-coordinate
                      (scale-y (-& y (floor (the fixnum height) 2)))))
  :TRANSLATION-ARGS
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y))
        (set-width (fixr (* width scale-x)))
        (set-height (fixr (* height scale-y))))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))
  ;; don't scale bitmaps yet.  Easy to do on the mac harder to do on
  ;; other platforms, maybe change this later
  ))
) ;eval-when for a large block of defstandard-graphics-handlers...

;; the default copy functions only copy slots. For bitmaps, we need
;; a separate copy of the bitmap as well
;; sgithens TODO fix these duplicate def warnings for sbcl
#-sbcl(defun copy-window-graphics-command-centered-bitmap (command)
  (make-window-graphics-command-centered-bitmap
   (new-offscreen-copy (window-graphics-command-centered-bitmap-bitmap command))
   (window-graphics-command-centered-bitmap-x command)
   (window-graphics-command-centered-bitmap-y command)
   (window-graphics-command-centered-bitmap-width command)
   (window-graphics-command-centered-bitmap-height command)))

#-sbcl(defun copy-boxer-graphics-command-centered-bitmap (command)
  (make-boxer-graphics-command-centered-bitmap
   (new-offscreen-copy (boxer-graphics-command-centered-bitmap-bitmap command))
   (boxer-graphics-command-centered-bitmap-x command)
   (boxer-graphics-command-centered-bitmap-y command)
   (boxer-graphics-command-centered-bitmap-width command)
   (boxer-graphics-command-centered-bitmap-height command)))

;; the def macro should be changed so we don't have to do this...



;;; Arcs, Ellipses, and Circles

(eval-when (:compile-toplevel :load-toplevel :execute)
(defstandard-graphics-handlers (wedge 26)
  :COMMAND-ARGS (x y radius start-angle sweep-angle)
  :EXTENTS-FORM ;leave as circle for now, get smarter about this later
  (let ((radius (fixr radius)))
    (values (-& x radius) (-& y radius) (+& x radius) (+& y radius)))
  :BOXER-EXTENTS-FORM
  (values (- x radius) (- y radius)
          (+ x radius) (+ y radius))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil nil nil)
  :SPRITE-COMMAND ; !!!what abut synching HEADING ???
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-wedge radius sweep-angle))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-wedge radius sweep-angle))))
  :COMMAND-BODY
  (when (plusp radius)
    (%draw-c-arc (ensure-legal-window-coordinate (scale-x x))
                (ensure-legal-window-coordinate (scale-y y))
                radius
                start-angle sweep-angle T)
    )
  :TRANSLATION-ARGS
  ;; translation
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y))
        (set-radius (fixr (* radius (min scale-x scale-y)))))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))
  (radius (fixr (* radius scale)))))

(defstandard-graphics-handlers (arc 27)
  :COMMAND-ARGS (x y radius start-angle sweep-angle)
  :EXTENTS-FORM  ;leave as circle for now, get smarter about this later
  (let ((radius (fixr radius)))
    (values (-& x radius) (-& y radius) (+& x radius) (+& y radius)))
  :BOXER-EXTENTS-FORM
  (values (- x radius) (- y radius)
          (+ x radius) (+ y radius))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil nil nil)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-arc radius sweep-angle))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-arc radius sweep-angle))))
  :COMMAND-BODY
  (unless (zerop radius)
    (%draw-c-arc (ensure-legal-window-coordinate (scale-x x))
                (ensure-legal-window-coordinate (scale-y y))
                radius
                start-angle sweep-angle nil)
    )
  :TRANSLATION-ARGS
  ;; translation
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y))
        (set-radius (fixr (* radius (min scale-x scale-y)))))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))
  (radius (fixr (* radius scale)))))

(defstandard-graphics-handlers (filled-ellipse 28)
  :COMMAND-ARGS (x y width height)
  :EXTENTS-FORM
  (multiple-value-bind (half-width wfudge) (truncate width 2)
                      (multiple-value-bind (half-height hfudge) (truncate height 2)
                                            (values (-& x half-width) (-& y half-height)
                                                    (+& x half-width wfudge) (+& y half-height hfudge))))
  :BOXER-EXTENTS-FORM
  (let ((half-width (/ width 2.0))
        (half-height (/  height 2.0)))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-ellipse width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-ellipse width height))))
  :COMMAND-BODY
  (unless (or (zerop width) (zerop height))
    (let ((half-width (floor width 2))
          (half-height (floor height 2)))
      (%draw-filled-arc %drawing-array *graphics-state-current-alu*
                        (ensure-legal-window-coordinate
                        (scale-x (-& x half-width)))
                        (ensure-legal-window-coordinate
                        (scale-y (-& y half-height)))
                        (fixr width) (fixr height)
                        0 360)))
  :TRANSLATION-ARGS
  ;; translation
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y))
        (set-width (fixr (* width scale-x)))
        (set-height (fixr (* height scale-y))))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))
  (width  (fixr (* width  scale)))
  (height (fixr (* height scale)))))

(defstandard-graphics-handlers (ellipse 29)
  :COMMAND-ARGS (x y width height)
  :EXTENTS-FORM
  (multiple-value-bind (half-width wfudge) (truncate width 2)
                      (multiple-value-bind (half-height hfudge) (truncate height 2)
                                            (values (-& x half-width) (-& y half-height)
                                                    (+& x half-width wfudge) (+& y half-height hfudge))))
  :BOXER-EXTENTS-FORM
  (let ((half-width (/ width 2.0))
        (half-height (/  height 2.0)))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-hollow-ellipse width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-hollow-ellipse width height))))
  :COMMAND-BODY
  (unless (or (zerop width) (zerop height))
    (let ((half-width (floor width 2))
          (half-height (floor height 2)))
      (%draw-arc %drawing-array *graphics-state-current-alu*
                (ensure-legal-window-coordinate (scale-x (-& x half-width)))
                (ensure-legal-window-coordinate (scale-y (-& y half-height)))
                (fixr width) (fixr height)
                0 360)))
  :TRANSLATION-ARGS
  ;; translation
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y))
        (set-width (fixr (* width scale-x)))
        (set-height (fixr (* height scale-y))))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))
  (width  (fixr (* width  scale)))
  (height (fixr (* height scale)))))

(defstandard-graphics-handlers (filled-circle 30)
  :COMMAND-ARGS (x y radius)
  :EXTENTS-FORM
  (let ((radius (fixr radius)))
    (values (-& x radius) (-& y radius) (+& x radius) (+& y radius)))
  :BOXER-EXTENTS-FORM
  (values (- x radius) (- y radius) (+ x radius) (+ y radius))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-circle radius))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-circle radius))))
  :COMMAND-BODY
  (when (plusp radius)
    (draw-circle (ensure-legal-window-coordinate (scale-x x))
                  (ensure-legal-window-coordinate (scale-y y))
                  radius
                  T)

    )
  :TRANSLATION-ARGS
  ;; translation
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y))
        (set-radius (fixr (* radius (min scale-x scale-y)))))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))
  (radius (fixr (* radius scale)))))

(defstandard-graphics-handlers (circle 31)
  :COMMAND-ARGS (x y radius)
  :EXTENTS-FORM
  (let ((radius (fixr radius)))
    (values (-& x radius) (-& y radius) (+& x radius) (+& y radius)))
  :BOXER-EXTENTS-FORM
  (values (- x radius) (- y radius) (+ x radius) (+ y radius))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-hollow-circle radius))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-hollow-circle radius))))
  :COMMAND-BODY
  (unless (zerop radius)
    (draw-circle (ensure-legal-window-coordinate (scale-x x))
                  (ensure-legal-window-coordinate (scale-y y))
                  radius
                  nil)
    )
  :TRANSLATION-ARGS
  ;; translation
  (trans-x trans-y)
  :TRANSLATION-BODY
  (progn (set-x (+& x trans-x)) (set-y (+& y trans-y)))
  ;; translation and scaling
  :TRANSLATION-AND-SCALING-ARGS
  (trans-x trans-y scale-x scale-y)
  :TRANSLATION-AND-SCALING-BODY
  (progn (set-x (+& (fixr (* x scale-x)) trans-x))
        (set-y (+& (fixr (* y scale-y)) trans-y))
        (set-radius (fixr (* radius (min scale-x scale-y)))))
  ;; sprite shape translation
  :TURTLE-TRANSLATOR-ARGS
  (trans-x trans-y cos-scale sin-scale scale)
  :TURTLE-TRANSLATOR-CLAUSES
  ((x (fix-array-coordinate-x
      (+ trans-x (+ (* cos-scale x) (* sin-scale y)))))
  (y (fix-array-coordinate-y
      (+ trans-y (- (* cos-scale y) (* sin-scale x)))))
  (radius (fixr (* radius scale)))))
) ;eval-when for another large block of defstandard-graphics-handlers...

;; this is used by the redisplay...

(defmacro playback-graphics-list-internal (gl &rest args)
  `(with-graphics-state (,gl t)
     (draw-before-graphics-list-playback ,gl)

     (with-blending-on
       (do-vector-contents (command ,gl)
         (draw-before-graphics-command-marker command ,gl)
         (process-graphics-command-marker command . ,args)
         )

     (draw-after-graphics-list-playback ,gl))))

(defun redisplay-graphics-sheet (gs graphics-screen-box)
  (with-graphics-vars-bound ((screen-obj-actual-obj graphics-screen-box))
    ;; first the items in the list
    (let ((gl (graphics-sheet-graphics-list gs)))
      (unless (graphics-command-list-hidden gl) (playback-graphics-list-internal gl)))
    ;; and then any sprites
    (let ((sprites (graphics-sheet-object-list gs)))
      (dolist (sprite sprites)
        (when (turtle? sprite)
          (let ((pgl (slot-value sprite 'private-gl)))
            (unless (graphics-command-list-hidden pgl) (playback-graphics-list-internal pgl)))))
      (dolist (sprite sprites)
        (unless (null (shown? sprite))
          (draw sprite))))))



;;; show probably do some type checking about compatibility
;;; of the from and to args
(defun dub-graphics-list (from-gl &optional
                                  (to-gl %graphics-list)
                                  (action ':append))
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




;;;; Boxtops
;; should probably go somewhere else
;; maybe a late display file after gdispl is loaded...

;; moved to boxdef.lisp (for the DEFVAR) and
;(defvar *boxtop-text-font* #+mcl   (make-boxer-font '("Courier" 10 :bold))
;                           #+lwwin (%make-font-number-internal 0 10 :bold)
;                           #-(or mcl lwwin) 1)

(defun boxtop-size (boxtop editor-box)
  (let ((bp (getprop editor-box :boxtop)))
    (cond ((eq bp :name-only) (text-boxtop-size boxtop t))
      ((eq bp :folder)    (folder-boxtop-size boxtop))
      ((eq bp :framed)    (graphics-boxtop-size boxtop t))
      ((graphics-sheet? boxtop) (graphics-boxtop-size boxtop))
      ((eq bp :xref)      (xref-boxtop-size boxtop))
      ;; Note: we can find :FILE inside mac files.  It means to
      ;; draw the boxer file icon
      ((eq bp :file)      (file-boxtop-size boxtop))
      (t (Error "Don't know how to handle boxtop ~A using ~S" boxtop bp)))))


(defun graphics-boxtop-size (gs &optional framed?)
  (if framed?
    (values (+& (graphics-sheet-draw-wid gs) 2)
            (+& (graphics-sheet-draw-hei gs) 2))
    (values (graphics-sheet-draw-wid gs) (graphics-sheet-draw-hei gs))))

(defun text-boxtop-size (text &optional framed?)
  (let ((textwid (if (listp text)
                   (reduce #'(lambda (a b)
                                     (max (string-wid *boxtop-text-font* a)
                                          (string-wid *boxtop-text-font* b)))
                           text)
                   (string-wid *boxtop-text-font* text)))
        (texthei (if (listp text)
                   ;(reduce #'(lambda (a b) (+& (string-hei *boxtop-text-font*)
                   ;                            (string-hei *boxtop-text-font*)))
                   ;        text)
                   (*& (string-hei *boxtop-text-font*) (length text))
                   (string-hei *boxtop-text-font*))))
    (if framed?
      (values (+ (ceiling textwid) 4) (+ texthei 4))
      (values (ceiling textwid) texthei))))

(defvar *folder-graphic-width* 30)
(defvar *folder-graphic-height* 20)
(defvar *folder-graphic-tab-height* 5)
(defvar *folder-graphic-tab-width* 10)
(defvar *folder-graphic-tab-delta* 5)

(defun folder-graphics-wid () *folder-graphic-width*)
(defun folder-graphics-hei ()
  (+& *folder-graphic-tab-height* *folder-graphic-height*))

(defun folder-boxtop-size (name)
  (multiple-value-bind (name-wid name-hei) (text-boxtop-size name)
                       (values (max (folder-graphics-wid) name-wid)
                               (+ (folder-graphics-hei) name-hei))))

;; 32x32 are the dimensions of a standard mac icon
(defun xref-boxtop-size (xref)
  (let* ((pathname (xref-pathname xref))
         (name (unless (null pathname) (pathname-name pathname))))
    (values (max 32 (ceiling (string-wid *boxtop-text-font* name)))
            (+ 32   (string-hei *boxtop-text-font*)))))

(defvar *file-boxtop-margin* 3)

;; means to make it look like an system icon with the name underneath
(defun file-boxtop-size (boxtop)
  (values (max& 32 (+ *file-boxtop-margin*
                      (ceiling (string-wid *boxtop-text-font* boxtop))))
          (+ 32 (string-hei *boxtop-text-font*))))

(defun draw-folder-graphic (x y)
  ;; the tab
  (draw-line x (+ y *folder-graphic-tab-height*)
             (+ x *folder-graphic-tab-delta*) y)
  (draw-line (+ x *folder-graphic-tab-delta*) y
             (+ x *folder-graphic-tab-delta* *folder-graphic-tab-width*) y)
  (draw-line (+ x *folder-graphic-tab-delta* *folder-graphic-tab-width*) y
             (+ x (*& 2 *folder-graphic-tab-delta*) *folder-graphic-tab-width*)
             (+ y *folder-graphic-tab-height*))
  ;; the main rectangle (l,t,r,b)
  (draw-rectangle 1 *folder-graphic-height*
                  x (+ y *folder-graphic-tab-height*))
  (draw-rectangle *folder-graphic-width* 1
                  x (+ y *folder-graphic-tab-height*))
  (draw-rectangle 1 *folder-graphic-height*
                  (+ x *folder-graphic-width* -1)
                  (+ y *folder-graphic-tab-height*))
  (draw-rectangle *folder-graphic-width* 1
                  x (+ y *folder-graphic-tab-height* *folder-graphic-height*)))

;; Note that the clipping, and origin has already been setup inside the redisplay
(defun draw-boxtop (boxtop editor-box x y wid hei)
  (let ((bp (getprop editor-box :boxtop)))
    (cond ((eq bp :name-only) (draw-text-boxtop editor-box boxtop x y wid hei))
      ((eq bp :folder) (draw-folder-boxtop editor-box boxtop x y))
      ((eq bp :framed) (draw-graphics-boxtop boxtop x y wid hei t))
      ((graphics-sheet? boxtop) (draw-graphics-boxtop boxtop x y wid hei))
      ((eq bp :xref)   (draw-xref-boxtop boxtop x y wid))
      ;; Note: we can find :FILE inside mac files.  It means to
      ;; draw the boxer file icon
      ((eq bp :file)      (draw-file-boxtop boxtop x y wid))
      (t (Error "Don't know how to handle boxtop ~A using ~S" boxtop bp)))))

;; text is always framed (for now)
(defun draw-text-boxtop (actual-obj text x y wid hei)
  (with-border-drawing-styles (actual-obj)
    (draw-rectangle 1 hei x y) ; left
    (draw-rectangle wid 1 x y) ; top
    (draw-rectangle 1 hei (+ x wid -1) y) ; right
    (draw-rectangle wid 1 x (+ y hei -2)) ; bottom ( -2 ?)
    (draw-string *boxtop-text-font* text (+ x 2) (+ y 2))))

(defun draw-folder-boxtop (actual-obj text x y)
  (with-border-drawing-styles (actual-obj)
    (let* ((stringw (ceiling (String-wid *boxtop-text-font* text)))
          (wdiff (- stringw (folder-graphics-wid))))
      (draw-folder-graphic (if (plusp wdiff) (+ x (/ wdiff 2)) x) y)
      (draw-string *boxtop-text-font* text
                  x (+ y (folder-graphics-hei))))))

;;

(defun draw-xref-boxtop (boxtop x y wid)
  (let* ((ic (xref-icon-cache boxtop))
         (mt (xref-mime-type  boxtop))
         (pathname (xref-pathname boxtop))
         (name (unless (null pathname) (pathname-name pathname)))
         (horiz-fudge (/ (- wid 32) 2)))
    (cond ((not (null ic))
           ;; if there is a mac icon, prefer it...
           #-carbon-compat
           (%bitblt-to-screen 32 32 ic 0 0 (+ x horiz-fudge) y)
           #+carbon-compat
           (draw-iconref ic (+ x horiz-fudge) y)
           )
      ((not (null mt))
       ;; no icon, so try and do somthing reasonable from the MIME info
       (draw-rectangle 32 32 (+ x horiz-fudge) y))
      (t ; no icon, no mime info
         (draw-rectangle 32 32 (+ x horiz-fudge) y)))
    ;; and now, the name
    (draw-string *boxtop-text-font* name x (+ y 32))))

(defun draw-graphics-boxtop-internal (boxtop x y wid hei)
  (unless (null (graphics-sheet-background boxtop))
    (with-pen-color ((graphics-sheet-background boxtop))
      (draw-rectangle wid hei x y)))
  (unless (null (graphics-sheet-bit-array boxtop))
    (#-x bitblt-to-screen #+(and sun x) bitblt-pixrect-to-screen
      wid hei (graphics-sheet-bit-array boxtop) 0 0 x y))
  ;; then handle any sprite graphics...
  (unless (null (graphics-sheet-graphics-list boxtop))
    (with-graphics-vars-bound-internal boxtop
      (playback-graphics-list-internal
       (graphics-sheet-graphics-list boxtop)))))

(defun draw-graphics-boxtop (boxtop x y wid hei &optional framed?)
  (cond ((null framed?)
         ;; just do the graphics
         (draw-graphics-boxtop-internal boxtop x y wid hei))
    (t ; there is a frame so handle that first (l,t,r,b)
       (draw-rectangle 1 hei x y)
       (draw-rectangle wid 1 x y)
       (draw-rectangle 1 hei (+ x wid -1) y)
       (draw-rectangle wid 1 x (+ y hei -1))
       ;; then move the origin over before graphics
       (with-drawing-inside-region (1 1 (- wid 2) (- hei 2))
         (draw-graphics-boxtop-internal boxtop x y (- wid 2) (- hei 2))))))

(defun draw-file-boxtop (boxtop x y wid)
  (let ((horiz-offset (floor (- wid 32) 2)))
    (draw-rectangle 32 32 (+ x horiz-offset) y)
    ;; now, the name
    (draw-string *boxtop-text-font* boxtop x (+ y 32))))


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


