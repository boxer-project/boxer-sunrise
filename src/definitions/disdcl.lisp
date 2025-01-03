;;;;  ; -*- Mode:LISP; Syntax: Common-Lisp; Package:BOXER;-*-
;;;;
;;;;      Boxer
;;;;      Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;      Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;      used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;      Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;      https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                           +-Data--+
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;    This file contains only the variable declarations and Macro definitions that
;;;;    need to loaded in BEFORE all of the other files in the DISPLAYDEFS module.
;;;;
;;;;
;;;;
;;;;  Modification History (most recent at the top)
;;;;
;;;;   1/21/12 changed the definition of sprite-screen-box class
;;;;   9/18/10 show-redisplay-init-code
;;;;   6/04/09 support for redisplaying changed horizontal scrolling in redisplaying-unit
;;;;   2/10/09 removed inf-shift, cached-border-info slots from SB's and out-of-sync-mark slot from SR's
;;;;   2/07/09 removed new-{wid,hei} and new-{x,y}-got-clipped? slots from screen-obj
;;;;           added max-scroll-wid for screen-boxes
;;;;   added sprite-screen-box class  for sprites representation in redisplay
;;;;   2/01/05 added *redisplay-id*, *redisplay-in-progress?*, redisplaying-unit &
;;;;           related accessors
;;;;   6/18/04 change %origin-y-offset initial value for #+mcl to be 0 (from 15)
;;;;           because it was breaking mouse doc tracking.  Keep and eye out for
;;;;           possible future problems though %origin-y-offset is usually accessed
;;;;           in a rebound environment
;;;;   2/11/03 merge LW and MCL source, updated copyright
;;;;   4/28/98 Started Logging changes: source = boxer version 2.2.r4
;;;;


(in-package :boxer)

;;;; Objects Having To Do With Redisplay.


(defvar *screen-chas-array-default-size* 32.)

(defstruct (screen-chas-array (:type vector)
                              (:include storage-vector)
                              (:constructor %make-screen-chas-array)
                              (:copier nil) ;; we do this ourselves
                              )
  (fds nil))

(defun make-screen-chas-array (&optional
                               (length *screen-chas-array-default-size*))
  (%make-screen-chas-array :contents (make-array length)))

;; also see the ACTUAL-OBJ-SUBCLASS which is neccessary
;; for Editor objects to be displayed
;;; OpenGL Note: can probably drop half of these slots
;;; in particular, new-??, needs-redisplay-pass-2? & force-redisplay-infs?
;;; need to think more about whether the clipped? stuff is neccessary

(defclass SCREEN-OBJ
  (plist-subclass)
  ((actual-obj :initform nil :accessor screen-obj-actual-obj)
   (x-offset :initform 0 :accessor screen-obj-x-offset)
   (y-offset :initform 0 :accessor screen-obj-y-offset)
   (wid :initform 0 :accessor screen-obj-wid
    :documentation "Width of the screen-obj, always a fixnum.")
   (hei :initform 0 :accessor screen-obj-hei
    :documentation "Height of the screen-obj, always a fixnum.")
   (content-wid :initform 0 :accessor content-wid
    :documentation "Width of the internal content, may be smaller than the wid for scrolled content.")
   (content-hei :initform 0 :accessor content-hei
    :documentation "Height of the internal content, may be smaller than the hei for scrolled content.")
   (x-got-clipped? :initform nil :accessor screen-obj-x-got-clipped?)
   (y-got-clipped? :initform nil :accessor screen-obj-y-got-clipped?)
   (tick :initform -1 :accessor screen-obj-tick)

   ;; local-matrix
   (local-matrix :initform (create-transform-matrix 0 0) :accessor local-matrix
    :documentation "Local matrix for this Box's Scene Graph.")
   ;; local-internal-matrix
   (local-internal-matrix :initform (3d-matrices:meye 4) :accessor local-internal-matrix
    :documentation "Local matrix for the internal area, inside any borders, margin, and padding for the Box's content.")
   ;; world-matrix
   (world-matrix :initform (create-transform-matrix 0 0) :accessor world-matrix
    :documentation "The scene graph location for this Box in world space.")
   (world-x-offset :initform 0 :accessor world-x-offset
    :documentation "The X location of this Box in world space.")
   (world-y-offset :initform 0 :accessor world-y-offset
    :documentation "The Y location of this Box in world space.")
   (world-internal-matrix :initform (3d-matrices:meye 4) :accessor world-internal-matrix
    :documentation "The scene graph location for the internal area (content without borders, margin, and padding)
                    in world space.")))

(defmethod screen-obj-hei ((self screen-obj))
  (floor (slot-value self 'hei)))

(defmethod (setf screen-obj-hei) (value (self screen-obj))
  (setf (slot-value self 'hei) (floor value)))

(defmethod screen-obj-wid ((self screen-obj))
  (floor (slot-value self 'wid)))

(defmethod (setf screen-obj-wid) (value (self screen-obj))
  (setf (slot-value self 'wid) (floor value)))

(defgeneric screen-obj? (x) (:method (x) nil) (:method ((x screen-obj)) t))

;; These only exist as a mixin for the box flavor
(defclass SCREEN-CHAR-SUBCLASS
  (screen-obj)
  ((screen-row :initform nil :accessor screen-row)))

(defclass SCREEN-ROW
  (screen-obj)
  ((screen-box :initform nil :accessor screen-box)
   (screen-chas :initform (make-screen-chas-array) :accessor screen-chas)
   (baseline :initform 0 :accessor baseline)))

(defgeneric screen-row? (x) (:method (x) nil) (:method ((x screen-row)) t))


(defclass SCREEN-BOX
  (screen-char-subclass)
  ((screen-rows :initform (allocate-storage-vector 8.) :accessor screen-rows)
   (scroll-to-actual-row :initform nil :accessor scroll-to-actual-row)
   (name :initform nil :accessor name)
   (box-type :initform ':doit-box :accessor box-type)
   (bps :initform nil :accessor bps)
   (display-style-list :initform (make-display-style :style nil)
                       :accessor display-style-list)
   (superior-screen-box :initform nil :accessor superior-screen-box)
   ;; scrolling vars
   (scroll-y-offset :initform 0 :accessor scroll-y-offset
    :documentation "A negative value results in the box being scrolled down.
                    Essentially, neither the scroll-y/x-offset can be positive.")
   (scroll-x-offset :initform 0 :accessor scroll-x-offset
    :documentation "A negative value results in the box being scrolled to the right.")
   (depth :initform 0 :accessor depth
    :documentation "The depth of this screenbox from the outermost screen-box.")))

(defgeneric screen-box? (x) (:method (x) nil) (:method ((x screen-box)) t))

(DEFUN CHECK-SCREEN-CHA-ARG (SCREEN-CHA)
       (OR (CHARACTERP SCREEN-CHA)
           (SCREEN-BOX? SCREEN-CHA)))

(defclass GRAPHICS-SCREEN-BOX
  (screen-box)
  ())

(defgeneric graphics-screen-box? (x) (:method (x) nil) (:method ((x graphics-screen-box)) t))

(defclass graphics-screen-sheet
  (screen-obj)
  ((screen-box :initform nil :accessor screen-box)))

(defgeneric graphics-screen-sheet? (x) (:method (x) nil) (:method ((x graphics-screen-sheet)) t))

;;; this is for graphics display of sprite boxes when the containing
;;; box is in text mode.  It has to inherit from screen-box so it can be toggled
;;; to/from vanilla screen-box's
(defclass sprite-screen-box
  (screen-box)
  ())

(defgeneric sprite-screen-box? (x) (:method (x) nil) (:method ((x sprite-screen-box)) t))

;;;; Variable Declarations

(defvar *redisplay-related-initializations* nil
  "Forms on this list are run BEFORE the Redislay is
   called usually after the window is created")

(defvar *redisplay-initialization-list* nil
  "A list of symbols to funcall.  The symbols are generated from
   the forms in *redisplay-related-initializations*")

;;;; Font Vars....

(defvar %drawing-font-cha-hei 12)
(defvar %drawing-font-cha-ascent 12)

;;;; Inits

(defmacro def-redisplay-initialization (form)
  (let ((name (intern (symbol-format nil "INIT-REDISPLAY-FUN-~A" (gensym)))))
    `(progn
      (defun ,name () ,form)
      (unless (member ',form *redisplay-related-initializations* :test #'equal)
        (push ',form *redisplay-related-initializations*)
        (push ',name *redisplay-initialization-list*)))))


(defun run-redisplay-inits ()
  (dolist (initfun (reverse *redisplay-initialization-list*))
    (funcall initfun)))


(defun show-redisplay-init-code (fun)
  (pprint (nth (position fun *redisplay-initialization-list*)
           *redisplay-related-initializations*)))
