;; -*- Mode:LISP; Syntax:Common-Lisp; Package:(BOXER :USE (LISP) :NICKNAMES (BOX)) -*-
#|

 $Header: boxdef.lisp,v 1.0 90/01/24 22:06:55 boxer Exp $

 $Log:	boxdef.lisp,v $
;;;Revision 1.0  90/01/24  22:06:55  boxer
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


 This file contains the defs for Boxer.


Modification History (most recent at top)

 7/17/13 removed *{bold,italics,tiny}-font-no* vars, clarifying comment for boxer-font-descriptor
 3/17/12 added new fill-row class for text justification hacking
 3/ 6/11 added (defvar *boxtop-text-font*) to keep all the defined font vars
         in the same place
 no more sprite boxes
 changed graphics-sheet slot in BOX class to "graphics-info"
 9/13/05 added more stuff to define-box-flag for new show-box-flags method
 2/10/03 PC & mac source merge
 9/01/02 added *uc-copyright-free*
 4/16/02 added intern-keyword
 3/20/02 added shrink-on-exit? flag
 3/15/02 added always-zoom? box flag
 1/27/02 added bit-array-dirty? flag to graphics-sheet structure def
 6/24/98 changed values for *minimum-box-wid/hei*
 6/24/98 started logging changes: source = boxer version 2.3


|#

(in-package :boxer)

(defvar *boxer-version* "3.4.13 2023-05-08")

(defun boxer-component-version ()
  "Returns the current semver version of boxer based on it's asdf configuration."
  ;; Oddly this value seems to get cached sometimes in asdf such that it sometimes returns the wrong version
  ;; if there are multiple boxers on the system.
  ;; (slot-value (asdf/system:find-system :boxer-sunrise) 'asdf/component:version)
  *boxer-version*)

(defun system-version ()
  "Returns a version string for boxer that looks like:
    Boxer version 3.2 Public OPENGL Release

  TODO: What was the passed in box argument used for previously?"
  (concatenate 'string "Boxer version " (boxer-component-version)
               " BSD License OpenGL 3.2 Shaders Release"))

(defvar *resources-dir* nil
  "This is the directory on disc where we can expect to find things like the Fonts and Images directories.")

;;;; Boxer Object Definitions
;;;  These are low level SUBCLASSes designed to be combined into higher level
;;;  BOXER objects Which eventually get instantiated

;;;  This gives BOXER objects their very own PLIST

(defclass plist-subclass
  ()
  ((plist :initform nil :accessor plist)))

(defmethod getprop ((obj plist-subclass) indicator &optional default)
  (or (getf (slot-value obj 'plist) indicator) default))

(defmethod removeprop ((obj plist-subclass) indicator)
  (if (eq (car (slot-value obj 'plist)) indicator)
    (setf (slot-value obj 'plist) (cddr (slot-value obj 'plist)))
    (do* ((rest (slot-value obj 'plist) (cddr rest))
          (ind (caddr rest) (caddr rest)))
      ((null rest))
      (when (eq ind indicator) (setf (cddr rest) (cddddr rest))))))

;; we really should make SETF win
(defmethod putprop ((obj plist-subclass) value indicator)
  (setf (getf (slot-value obj 'plist) indicator) value))

;;; This has Slots That are used by the Virtual Copy Mechanism.

(defclass virtual-copy-subclass
  ()
  ((virtual-copy-rows :initform nil)
   (contained-links :initform nil
                    :accessor contained-links)
   (branch-links :initform nil
                 :accessor branch-links))
  (:documentation "Interface to virtual copy"))

;; All of the methods are defined in the virtcopy files

;;;; Stuff that is particular to boxer.

;;;; DEFVARS

(defvar *time-zone* 8) ;Pacific Time Zone by default

(DEFVAR *INSIDE-LISP-BREAKPOINT-P* NIL)

(DEFVAR *EVALUATION-IN-PROGRESS?* NIL
  "This is bound to T by the top level DOIT command")

(DEFVAR *POINT* NIL)

(DEFVAR *MINIMUM-BOX-WID* 35. ; 25. ; changed to allow room for type label
  "The minimum width any box will be drawn on the screen.")

;;; +++ This depends on the height of the default font.  An alternative would be to change
;;; box-borders-minimum-size or whatever to look at the font, but I wasn't sure of that.
(DEFVAR *MINIMUM-BOX-HEI* 25
  "The minimum height any box will be drawn on the screen.")

(DEFVAR *COM-MAKE-PORT-CURRENT-PORT* NIL
  "This variable is used to store newly created ports until they are inserted into the
   World. ")

(defvar *print-boxer-structure* nil
  "Controls how verbose to print boxer structures (like *print-array*)")

(DEFVAR *INITIAL-BOX* NIL
  "The initial box the editor starts with, this box cannot be deleted
   killed etc.")

(DEFVAR *EDITOR-NUMERIC-ARGUMENT* NIL
  "Stores the value of whatever numeric argument for an editor function has accumalated. ")

;;;Region Variables

(DEFVAR *CURRENT-EDITOR-REGION* NIL)

(DEFVAR *REGION-BEING-DEFINED* NIL
  "Bound to a region which is in the process of being delineated.  NIL Otherwise.")

(DEFVAR *KILLED-REGION-BUFFER* NIL
  "this should be integrated into the generic kill buffer eventually")

(DEFVAR *REGION-LIST* NIL)

(defvar *following-mouse-region* nil)

;;;; Variables Having To Do With Redisplay.

(DEFVAR *REDISPLAY-WINDOW* NIL
  "Inside of REDISPLAYING-WINDOW, this variable is bound to the window
   being redisplayed.")

(DEFVAR *OUTERMOST-SCREEN-BOX* NIL
  "Inside of REDISPLAYING-WINDOW, this variable is bound to the window
   being redisplayed's outermost-screen-box. This is the screen box which
   represents that window outermost-box.")

(DEFVAR *SPACE-AROUND-OUTERMOST-SCREEN-BOX* #-mcl 9.  #+mcl 3.
  "This is the number of pixels between the outside of the outermost screen
   box and the inside of the window. This space exists to allow the user to
   move the mouse out of the outermost box.")

(DEFVAR *TICK* 0
  "This is the global variable used by the (TICK) function to generate
   a continuously increasing series of integers. This is mostly used by
   the redisplay code although it wouldn't mess things up if (TICK)
   was called by other sections of code.")

(DEFVAR *BOX-ZOOM-WAITING-TIME* 0.1
  "The amount of time spent waiting between the individual steps when zooming a box. ")

(defun tick (&optional how-many)
  (declare (ignore how-many))
  (setq *tick* (+ *tick* 1)))

(DEFVAR *OUTERMOST-SCREEN-BOX-STACK* NIL
  "Keeps track of the previous outermost screen boxes so that they can be returned to. ")

;;;; Fonts

;;; The editor interface to fonts is via font numbers (small fixnums) are
;;; used as an index into a per window array of fonts
;;; The variables defined below are basically the bare minimum that
;;; we would like to support.
;;; These refer to the *boxer-pane*'s font map, the *name-pane*'s font map
;;; need only be of length 1
;;; Note that it is some of these fonts might me identical and that it is
;;; up to whatever initializes the font map in boxwin-xxx to "do the right
;;; thing"

;;; NOTE: 3/6/11 these initial values are meaningless, expect all these vars
;;;       to be initialized in a draw-low-xxx file

(defvar *normal-font-no* nil)

;; the mac versions are setup in draw-low-mcl.lisp
(defvar *box-border-label-font-no* nil)

;; avoid problems with italics overstriking the
;; right edge of the name tab on the mac
;; fix this later by when we redo the font/char interface
(defvar *box-border-name-font-no* nil )


(defvar *sprite-type-font-no* nil)

(defvar *boxtop-text-font* nil)


;;;editor variables...

(DEFVAR *COLUMN* 0
  "the cha-no of the point for use with cntrl-p and cntrl-n")

(defvar *word-delimiters* (do ((i 31 (1+ i))
                               (list nil))
                            ((= i 256) list)
                            (when (and (standard-char-p (code-char i))
                                       (not (alphanumericp (code-char i))))
                              (push (code-char i) list))))


(DEFVAR *BOXER-VERSION-INFO* NIL
        "This variable keeps track of what version of boxer is currently loaded
   and being used.  Versions for general release are numbered while specific
   development versions have associated names.")

(defvar *boxes-being-temporarily-deleted* nil
  "This variable is bound to T inside editor commands that delete
   but then re-insert boxes.")

;;; STEPPING VARS

(defvar *step-flag* nil
  "Controls whether the (interim) stepper is in operation.")

(defvar *box-copy-for-stepping* nil
  "Should be an evaluator variable, when we have one.  A
   copy of the the currently-executing box, placed in the stepping
   window.  The :funcall method needs the actual box so it can
   flash lights inside it.")

;; a function or list of functions to be run after the evaluator exits
;; (so far) used in boxwin-mcl to process inout recieved during eval
(defvar *post-eval-hook* nil)

;;; debugging variables
(defvar *boxer-system-hacker* nil)

;;;; EDITOR OBJECT DEFINITIONS

;;; see the file chars-xxx.lisp for details on how characters work in Boxer

;;; This Subclass is neccessary for an Editor Object to be redisplayed


(defclass actual-obj-subclass
  ()
  ((screen-objs :initform nil
                :accessor actual-obj-screen-objs)
   (tick :initform 1
         :accessor actual-obj-tick))
  (:documentation "Used by editor objects to interface with the redisplay" ))

;;;; Instantiable Objects...

(defclass row
  (actual-obj-subclass  plist-subclass)
  ((superior-box :initform nil :accessor superior-box :initarg :superior-box)
   (previous-row :initform nil :accessor previous-row :initarg :previous-row)
   (next-row :initform nil :accessor next-row :initarg :next-row)
   (chas-array :initform nil :accessor chas-array :initarg :chas-array)
   (cached? :initform nil :accessor cached?)
   (cached-chas :initform nil :accessor cached-chas)
   (cached-chunks :initform nil :accessor cached-chunks)
   (cached-eval-objs :initform NIL :accessor cached-eval-objs)
   (cached-evrow :initform nil :accessor cached-evrow)
   (inferior-links :initform nil)))

(defgeneric row? (x) (:method (x) nil) (:method ((x row)) t))


(defclass name-row
  (row)
  ((cached-name :initform nil :accessor cached-name :initarg :cached-name))
  ;; used for environmental info--a symbol in the BU package
  )

(defgeneric name-row? (x) (:method (x) nil) (:method ((x name-row)) t))

(defclass fill-row
  (row)
  ())

(defstruct (display-style (:predicate display-style?))
  "
  Display style is used primarily as a slot on `box` and `screenbox` classes, and contains
  information about various aspects of it's current display.

  - `style` The style slot takes a symbol which is one of :supershrunk, :shrunk, or :normal to
    indicate the current size of the box. Note that when a box is full screened, it will retain
    the style that it was before being full screened. This means the style could still be :shrunk
    while the box is full screened, especially if the box property 'Shrink on Exit' is set.
  "
  (style :normal)
  (fixed-wid nil)
  (fixed-hei nil)
  (graphics-mode? nil)
  (border-style nil))

;; changed graphics-sheet to graphics-info to hold all graphical
;; objects - name change should help catch undone things

(defclass box
  (virtual-copy-subclass actual-obj-subclass plist-subclass)
  ((superior-row :initform nil :accessor superior-row :initarg :superior-row)
   (first-inferior-row :initform nil :accessor first-inferior-row
                       :initarg :first-inferior-row)
   (cached-rows :initform nil :accessor cached-rows)
   (ports :initform nil :accessor ports)
   (display-style-list :initform (make-display-style)
                       :accessor display-style-list)
   (name :initform nil :accessor name)
   (static-variables-alist :initform nil :accessor static-variables-alist)
   (exports :initform nil :accessor exports)
   (closets :initform nil :accessor closets :initarg :closets)
   (region :initform nil :accessor region)
   (cached-code :initform nil :accessor cached-code)
   (static-variable-cache :initform nil :accessor static-variable-cache)
   (trigger-cache :initform nil :accessor trigger-cache)
   (current-triggers :initform nil :accessor current-triggers)
   (graphics-info :initform nil :accessor graphics-info)
   (flags :initform 0 :accessor box-flags)))

(defgeneric box? (x) (:method (x) nil) (:method ((x box)) t))

(defclass doit-box
  (box)
  ())

(defgeneric doit-box? (x) (:method (x) nil) (:method ((x doit-box)) t))

(defclass data-box
  (box)
  ())

(defgeneric data-box? (x) (:method (x) nil) (:method ((x data-box)) t))

(defclass port-box
  (box)
  ())

(defgeneric port-box? (x) (:method (x) nil) (:method ((x port-box)) t))

;; A sprite box has a graphics-object as the graphics-info. See grobjs.lisp
(defun sprite-box? (thing)
  (and (box? thing)
       (graphics-object? (slot-value thing 'graphics-info))))

;; graphics-box? is now defined as a box with a non-null graphics-sheet
(defun graphics-box? (thing)
  (and (box? thing)
       (graphics-sheet? (slot-value thing 'graphics-info))))


;;; Box type labels These are usually used for the labels on the borders, but
;;; could be used elsewhere.

(defun box-type-label (input-box)
  "Returns a label for the type of box, such as Doit, Sprite, Data, Graphics, etc.
   These are typically used when rendering the labels on box borders, but could be
   useful for other situations."
   ;; I was going to implement this as a set of generic methods, but becuase of the odd
   ;; way graphics and sprites currently are, this is simpler. Also graphics boxes can
   ;; flipped from Doit<->Data as well, so they need to be checked first.
   (cond ((sprite-box? input-box) "Sprite")
         ((graphics-box? input-box) "Graphics")
         ((doit-box? input-box) "Doit")
         ((data-box? input-box) "Data")
         ((port-box? input-box) "Port")
         (t ""))
   )

;; for delineating regions in the editor...

(defstruct (interval
            (:predicate interval?)
            (:copier nil)
            (:print-function print-interval)
            (:constructor %make-interval (start-bp stop-bp)))
  (start-bp nil)
  (stop-bp nil)
  (blinker-list nil)
  (box nil))

(defun print-interval (interval stream level)
  (declare (ignore level))
  (format stream "#<INTERVAL [~A,~D] [~A,~D]>"
          (bp-row (interval-start-bp interval))
          (bp-cha-no (interval-start-bp interval))
          (bp-row (interval-stop-bp interval))
          (bp-cha-no (interval-stop-bp interval))))


(defstruct (graphics-sheet (:constructor
                            %make-simple-graphics-sheet
                            (draw-wid draw-hei superior-box))
                           (:constructor
                            %make-graphics-sheet-with-graphics-list
                            (draw-wid draw-hei superior-box))
                           (:constructor make-graphics-sheet-from-file
                                         (draw-wid draw-hei draw-mode))
                           (:copier nil)
                           (:predicate graphics-sheet?)
                           (:print-function
                            (lambda (gs s depth)
                                    (declare (ignore depth))
                                    (format s "#<Graphics-Sheet W-~D. H-~D.>"
                                            (graphics-sheet-draw-wid gs)
                                            (graphics-sheet-draw-hei gs)))))
  (draw-wid *default-graphics-sheet-width*)
  (draw-hei *default-graphics-sheet-height*)
  (screen-objs nil)
  (bit-array nil)  ;; This should be an ogl-pixmap
  (object-list nil)
  (superior-box nil)
  (draw-mode ':wrap)
  (graphics-list nil)
  (background nil)
  (colormap nil)
  (transform nil) ; opengl transform matrix

  ;; these are obsolete....
  (prepared-flag nil)
  ;; used to avoid redundant prepare sheets (see bu::with-sprites-hidden)
  (bit-array-dirty? nil)
  ;; used to avoid saving cleared bitmap backgrounds
  )

;; this can stay here cause its for a Struct and not a PCL Class
;; sgithens - See above on definition of deftype-checking-macros
;;(deftype-checking-macros GRAPHICS-SHEET "A Bit Array for Graphics Boxes")

;;;; Flags

;;; if we use bit positions in a fixnum for various boolean values of
;;; a box, we can save A LOT OF SPACE at a modest cost in speed.
;;; Right now, each flag costs a word of storage vs 1 bit
;;; The speed cost is about an extra 12 MC68020 (10 sparc instructions)
;;; per flag reference

(defvar *defined-box-flags* (make-array 32 :initial-element nil))

(defmacro define-box-flag (name position)
  `(progn
    (setf (svref *defined-box-flags* ,position) ',name)
    (defsubst ,(intern (symbol-format nil "BOX-FLAG-~A" name)) (word)
      (not (zerop& (ldb& ',(byte 1 position) word))))
    (defsubst ,(intern (symbol-format nil "SET-BOX-FLAG-~A" name)) (word t-or-nil)
      (dpb& (if t-or-nil 1 0) ',(byte 1 position) word))
    (defmethod ,name ((self box))
      (not (zerop& (ldb& ',(byte 1 position) (slot-value self 'flags)))))
    (defmethod ,(intern (symbol-format nil "SET-~A" name)) ((self box) t-or-nil)
      (setf (slot-value self 'flags)
            (dpb& (if t-or-nil 1 0)
                  ',(byte 1 position)
                  (slot-value self 'flags)))
      t-or-nil)
    (defmethod (setf ,name) (t-or-nil (self box))
      (setf (slot-value self 'flags)
            (dpb& (if t-or-nil 1 0)
                  ',(byte 1 position)
                  (slot-value self 'flags)))
      t-or-nil)))

(define-box-flag shrink-proof? 0)

(define-box-flag build-template? 1)

(define-box-flag storage-chunk? 2)

(define-box-flag load-box-on-login? 3)

(define-box-flag read-only-box? 4)

(define-box-flag copy-file? 5)

(define-box-flag foreign-server 6)

(define-box-flag fake-file-box 7)

(define-box-flag file-modified? 8)

;; similiar to load-on-login? but we want to avoid conflicts with client code
(define-box-flag autoload-file? 9)

;; a flag which tells the printer that the box and all it's inferiors
;; is guaranteed to be freshly CONSed with no links to any other existing
;; editor structure.  It is therefore OK to just incorporate the box into
;; the printed result without copying
(define-box-flag all-new-box? 10)

;; closet locks

(defvar *lock-all-closets* t)

(define-box-flag locked-closet? 11)

;; support for local control of hotspots
(define-box-flag top-left-hotspot-active?     12)
(define-box-flag top-right-hotspot-active?    13)
(define-box-flag bottom-left-hotspot-active?  14)
(define-box-flag bottom-right-hotspot-active? 15)
(define-box-flag type-tag-hotspot-active?     16)

(define-box-flag auto-fill?                   17)

(define-box-flag relative-filename?           18)

(define-box-flag always-zoom?                 19)
(define-box-flag shrink-on-exit?              20)

(defmethod show-box-flags ((self box) &optional show-all?)
  (let ((flags (slot-value self 'flags)))
    (dotimes (i (length *defined-box-flags*))
      (let ((name (svref *defined-box-flags* i)))
        (cond ((null name))
          (t (let ((flag-set? (not (zerop (ldb (byte 1 i) flags)))))
               (cond ((and (null show-all?) (null flag-set?)))
                 (t (format t "~&~A: ~A" name
                            (if flag-set? "true" "false")))))))))))

;; move graphics-view? here ?

;;;BP's are pointers which are used to move within REAL(that is, ACTUAL)
;;;structure.  Note that they have nothing to do with SCREEN structure...
;;;The *point* is a BP as is the *mark*
;;;however, operations which move the *point* and the *mark* also update the
;;;global variable's  *current-screen-box* and *marked-screen-box*

(DEFSTRUCT (BP (:TYPE LIST) :NAMED          ;Easier to Debug
               (:CONSTRUCTOR MAKE-BP (TYPE))
               (:CONSTRUCTOR MAKE-INITIALIZED-BP (TYPE ROW CHA-NO))
               (:CONC-NAME   BP-))
           (ROW    NIL)
           (CHA-NO 0)
           (SCREEN-BOX NIL)
           (TYPE ':FIXED))

(DEFSUBST BP? (X)
          (AND (CONSP X) (EQ (CAR X) 'BP)))

(DEFMACRO CHECK-BP-ARG (X)
          `(CHECK-TYPE ,X (SATISFIES BP?) "A Boxer Editor Buffer-Pointer (BP)."))

(defsubst row-bps (row) (bps row))

(defsetf row-bps set-bps)

(defmacro move-bp (bp form)
  `(multiple-value-bind (new-row new-cha-no new-screen-box)
                        ,form
                        (when (row? new-row)
                          (move-bp-1 ,bp new-row new-cha-no new-screen-box))))

(defmacro move-point (form)
  `(multiple-value-bind (new-row new-cha-no new-screen-box)
                        ,form
                        (when (row? new-row)
                          (move-point-1 new-row new-cha-no new-screen-box))))

(DEFUN BP-CHA (BP)
       (CHA-AT-CHA-NO (BP-ROW BP) (BP-CHA-NO BP)))

;;;; Font Descriptors
;;; This is the main datastructure for specifying font info in the editor
;;; Note that the "size" encoded in the font-no is relative, see
;;; These are stored in a slot in the chas-array (like BP's) and need to
;;; be updated (like BP's) by things like SLIDE-CHAS-ARRAY-CHAS

;; a draw-low-xxx variable, defined here to suppress warnings in the following
;; Defstruct
(defvar *foreground-color* #(:rgb 0.0 0.0 0.0 1.0))

(defstruct (boxer-font-descriptor (:conc-name bfd-)
                                  (:predicate bfd?)
                                  (:constructor make-bfd (cha-no font-no))
                                  (:constructor make-cfd (cha-no font-no color))
                                  (:print-function
                                   (lambda (bfd stream depth)
                                           (declare (ignore depth))
                                           (format stream "#<Bfd ~D ~X ~X>"
                                                   (bfd-cha-no bfd)
                                                   (bfd-font-no bfd)
                                                   (bfd-color bfd)))))
  (cha-no 0 :type fixnum)
  (font-no 0 :type fixnum)
  (color *foreground-color*))

(defvar *default-font-descriptor* nil
  "The font descriptor used when no FD is explicitly specified")

(defvar *current-font-descriptor* nil
  "The font descriptor used by newly inserted characters")

;;;
;;; package stuff that would be in pkg except it must be compiled
;;;

(defun intern-in-bu-package (symbol)
  (intern (string symbol) 'bu))

(defun intern-keyword (symbol-or-string)
  (intern (if (symbolp symbol-or-string)
            (string symbol-or-string)
            (string-upcase symbol-or-string))
          'keyword))

;;;; Box Interface Structures

;;; these can be found in static-variable-alists
;;; they have 4 slots:
;;;
;;; . a Type slot
;;;
;;; . a pointer to the "real" value
;;;
;;; . a pointer to the "interface" box (or NIL)
;;;
;;; . a pointer to the superior-box that the "interface" box is intended
;;;   to live in (if it ever get created)
;;;

;;;
;;; There are 3 flavors of these interface boxes
;;; the difference between them has to do with whether or not
;;; the raw VALUE is a legitimate boxer object (usually a number)
;;; This issue arises when we want to virtual copy the box-interface
;;; object.  We can avoid CONSing the box and just return the raw
;;; VALUE iff the value doesn't depend on EQness AND it is a valid
;;; boxer object.  The port-to case will force the creation of the Box
;;; no matter what.
;;;
;;; Some interface boxes have special properties that are NOT represented
;;; by the usual text representation of boxes (usually graphical).  The
;;; interface structs for these boxes have an additional slot which contains
;;; the function used to convert between the "real" value and the particular
;;; special box property.  The back conversion (box ==> "real" value) is
;;; handled by the update function on the modified trigger of the interface box
;;;

(defmacro box-interface-type (bi) `(svref& ,bi 0))

(defmacro box-interface-value (bi) `(svref& ,bi 1))

(defmacro box-interface-box (bi) `(svref& ,bi 2))

(defmacro box-interface-sup-box (bi) `(svref& ,bi 3))

(defmacro box-interface-slot-name (bi) `(svref& ,bi 4))

(defun %make-vv-box-interface (value &optional slot-name (sup-box nil))
  (let ((bi (make-array 5)))
    (setf (svref& bi 0) 'valid-value-box-interface)
    (setf (box-interface-value     bi) value)
    (setf (box-interface-box       bi) nil)
    (setf (box-interface-sup-box   bi) sup-box)
    (setf (box-interface-slot-name bi) slot-name)
    bi))

(defun %make-iv-box-interface (value &optional slot-name (sup-box nil))
  (let ((bi (make-array 5)))
    (setf (svref& bi 0) 'invalid-value-box-interface)
    (setf (box-interface-value     bi) value)
    (setf (box-interface-box       bi) nil)
    (setf (box-interface-sup-box   bi) sup-box)
    (setf (box-interface-slot-name bi) slot-name)
    bi))

(defsubst vv-box-interface? (thing)
  (and (simple-vector-p thing)
       (eq (svref& thing 0) 'valid-value-box-interface)))

(defsubst iv-box-interface? (thing)
  (and (simple-vector-p thing)
       (eq (svref& thing 0) 'invalid-value-box-interface)))

(defsubst sv-box-interface? (thing)
  (and (simple-vector-p thing)
       (eq (svref& thing 0) 'special-value-box-interface)))

(defsubst box-interface? (thing)
  (and (simple-vector-p thing)
       (or (eq (svref& thing 0) 'valid-value-box-interface)
           (eq (svref& thing 0) 'invalid-value-box-interface)
           (eq (svref& thing 0) 'special-value-box-interface))))

(defmacro special-box-interface-update-function (bi) `(svref& ,bi 5))

(defun %make-sv-box-interface (value &optional slot-name (sup-box nil)
                                     update-fun)
  (let ((bi (make-array 6)))
    (setf (svref& bi 0) 'special-value-box-interface)
    (setf (box-interface-value     bi) value)
    (setf (box-interface-box       bi) nil)
    (setf (box-interface-sup-box   bi) sup-box)
    (setf (box-interface-slot-name bi) slot-name)
    (setf (special-box-interface-update-function bi) update-fun)
    bi))


;; sgithens
(defvar *GLOBAL-HOTSPOT-CONTROL?* nil)
;; flags for global control, these were previously in popup.lisp
(defvar *top-left-hotspots-on?*     t)
(defvar *top-right-hotspots-on?*    t)
(defvar *bottom-left-hotspots-on?*  t)
(defvar *bottom-right-hotspots-on?* t)

;;;; Commands for Mouse border Areas.... these were in coms-oglmouse.lisp
(defvar *warn-about-disabled-commands* t)
(defvar *only-shrink-wrap-text-boxes* nil)

;; From draw folder
;; These specify how to combine bits already there with the drawing bits
;; The common lisp boole-xxx constants are the right thing
;; (see LispWorks User Guide 15.3, pg 126)

(defconstant alu-andca boole-andc1   "Erase")
(defconstant alu-seta  boole-1       "Drawing bits have priority")
(defconstant alu-xor   boole-xor     "XORs the bits")
(defconstant alu-and   boole-and     "ANDs the bits")
(defconstant alu-ior   boole-ior     "ORs the bits")
(defconstant alu-setz  boole-clr     "Set to Zero")

;;; **** used in sprite graphics for parameter checking
;;; **** are these the right numbers for MCL ?
(defun max-window-coord () #.(expt 2 15))
(defun min-window-coord () #.(- (expt 2 15)))
