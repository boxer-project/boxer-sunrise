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
   (x-got-clipped? :initform nil :accessor screen-obj-x-got-clipped?)
   (y-got-clipped? :initform nil :accessor screen-obj-y-got-clipped?)
   (tick :initform -1 :accessor screen-obj-tick)))

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
   (cached-absolute-pos :initform nil :accessor cached-absolute-pos)
   ;; scrolling vars
   (scroll-y-offset :initform 0)
   (scroll-x-offset :initform 0)
   (max-scroll-wid  :initform nil)))

(defgeneric screen-box? (x) (:method (x) nil) (:method ((x screen-box)) t))

(DEFUN CHECK-SCREEN-CHA-ARG (SCREEN-CHA)
       (OR (CHARACTERP SCREEN-CHA)
           (SCREEN-BOX? SCREEN-CHA)))

(defclass GRAPHICS-SCREEN-BOX
  (screen-box)
  ())

(defgeneric graphics-screen-box? (x) (:method (x) nil) (:method ((x graphics-screen-box)) t))

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

(DEFVAR %DRAWING-ARRAY NIL
        "Inside of a drawing-on-window, this variable is bound to %drawing-window's
   screen-array (Note that this value is valid because drawing-on-window does
   a prepare-sheet of drawing-window.")

(DEFVAR %ORIGIN-X-OFFSET 0
        "Inside of a drawing-on-window, this variable is bound to x-offset of the
   current drawing origin from the screen's actual x origin. With-origin-at
   rebinds this variable (and %origin-y-offset) to change the screen position
   of the drawing origin.")

(DEFVAR %ORIGIN-Y-OFFSET 0
        "Inside of a drawing-on-window, this variable is bound to y-offset of the
   current drawing origin from the screen's actual y origin. With-origin-at
   rebinds this variable (and %origin-x-offset) to change the screen position
   of the drawing origin.")

(DEFVAR %CLIP-LEF 0)
(DEFVAR %CLIP-TOP 0)
(DEFVAR %CLIP-RIG 0)
(DEFVAR %CLIP-BOT 0)

;;;; Font Vars....

(defvar %drawing-font-cha-hei 12)
(defvar %drawing-font-cha-ascent 12)


(defvar *redisplay-id* 0)
(defvar *redisplay-in-progress?* nil)
(defvar *redisplay-encore?* nil)
(defvar *allow-redisplay-encore? nil)

;; the innards of repaint-cursor can side effect the horizontal scrolling
;; of the (point-screen-box).  If it does, then it will set this flag and then
;; throw to 'scroll-x-changed TAG

(defmacro redisplaying-unit (&body body)
  `(let ((*redisplay-in-progress?* t)
         (*redisplay-encore?* nil))
     (catch 'scroll-x-changed
       (unwind-protect
        (progn . ,body)
        (when (not (null *redisplay-encore?*))
          (progn . ,body))
        (setq *redisplay-id* (tick))))))

(defun redisplay-in-progress? () *redisplay-in-progress?*)
(defun redisplay-id () *redisplay-id*)



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
