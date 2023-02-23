;;;;  -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER;-*-
;;;;
;;;;      Boxer
;;;;      Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;      Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those ;;;;  portions may be
;;;;      used for any purpose, including commercial ones, providing that notice of MIT copyright is ;;;;  retained.
;;;;
;;;;      Licensed under the 3-Clause BSD license. You may not use this file except in compliance with ;;;;  this license.
;;;;
;;;;      https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                               +-Data--+
;;;;                      This file is part of the | BOXER | system
;;;;                                               +-------+
;;;;
;;;;     this file contains random codehaving to do with screen structure
;;;;     such as allocation/deallocation code, mouse tracking stuff and box
;;;;     border functions
;;;;
;;;;     All of the high level redisplay code is in the file REDISP and
;;;;     the low level code for accessing and patching up screen structure is
;;;;     to be found in the file LODISP
;;;;
;;;;      LOW-LEVEL SCREEN-OBJ allocation/deallocation code.
;;;;
;;;;      Use our own resource allocation/deallocation scheme here because the
;;;;      Lispm's DEALLOCATE-RESOURCE is so slow that it significantly slows
;;;;      down the whole redisplay code.
;;;;
;;;;
;;;;  Modification History (most recent at the top)
;;;;
;;;;  11/26/12 set-fixed-size rounds wid/hei for resize-graphics-sheet
;;;;   9/ 2/12 removed fixnum arithmetic from:
;;;;           erase-chas-to-eol, erase-screen-chas, still-inside-rdp1-info
;;;;           erase-and-queue-for-deallocation-screen-rows-from
;;;;           fixed-size (screen-box), window-{xy,x,y}-position (screen-obj)
;;;;           outermost-screen-box-size, bp-coordinates{-via-editor},
;;;;           screen-obj-absolute-coordinates-1
;;;;   8/26/12 removed functions which are no longer being used...
;;;;           move-screen-{rows,chas,obj,graphics-sheet}, slide-screen-chas,
;;;;           move-inferior-screen-objs,GRAY-SIZE-AND-OFFSETS,MOVE-GRAY-REGION
;;;;   1/21/12 sprite-screen-box hacking for setup-redisplay, toggle-type
;;;;   6/08/09 bp-coordinates takes into account unseen chars when calculating x position in row
;;;;   2/11/09 removed usage of cached-border-info from (de-allocate-init (screen-box))
;;;;   2/ 9/09 removed screen-object-new-{width, height}
;;;;   6/01/05 boxtop handles possible graphics in xref
;;;;  10/18/04 added fixed-size-1 method for graphics-screen-boxes because the
;;;;           containing box always adjusts its size to he contained sheet
;;;;  10/17/04 set-fixed-size (box): don't resize the graphics-sheet for gboxes
;;;;           when we fix the size of the text view of the box
;;;;   7/08/02 boxtop now handles possible VC in 'bu::boxtop lookup
;;;;   2/14/01 merged current LW and MCL files
;;;;  10/08/00 added :file clause to boxtop function
;;;;   5/10/99 changed boxtop function to give defined boxtops precedence over :file
;;;;   1/11/99 added #+lispworks versions as needed
;;;;   4/25/99 added support for :file boxtop in boxtop function
;;;;  10/22/98 allocate-screen-obj-for-use-in now tries to set scroll row for newly
;;;;           allocated screen boxes
;;;;   5/11/98 de-allocate-init for screen-rows now also resets various font parameters
;;;;   4/29/98 added &optional fds arg to slide-screen-chas to allow either screen
;;;;           or actual row font descriptors to be used (default is screen fds)
;;;;   4/27/98 Started Logging changes: source = boxer version 2.2.r4 + changes for fonts
;;;;

(in-package :boxer)

(defun setup-redisplay (&optional (cons-new? nil))
  (when (or cons-new?
            (zerop (length free-screen-rows))
            (zerop (length free-screen-boxs)))
    (setq free-screen-rows nil
          free-screen-boxs nil
          free-graphics-screen-boxs nil
          free-sprite-screen-boxs nil)
    (dotimes (i initial-no-of-free-screen-rows)
      (push (make-instance 'screen-row) free-screen-rows))
    (dotimes (i initial-no-of-free-screen-boxs)
      (push (make-instance 'screen-box) free-screen-boxs))
    (dotimes (i initial-no-of-free-graphics-screen-boxs)
      (push (make-instance 'graphics-screen-box)
            free-graphics-screen-boxs))
    (dotimes (i initial-no-of-free-sprite-screen-boxs)
      (push (make-instance 'sprite-screen-box)
            free-sprite-screen-boxs))))

(def-redisplay-initialization (setup-redisplay))

;;; Call DE-ALLOCATE-INIT before pushing the object onto the free list.
;;; It removes any pointers that might hold on to horrible things like
;;; editor box structure.  It should leave the object in the same state
;;; as a newly-consed one.
(defmethod de-allocate-init ((self screen-obj))
  (setf (screen-obj-actual-obj self) nil
        (screen-obj-wid self) 0
        (screen-obj-hei self) 0
        (screen-obj-x-got-clipped? self) nil
        (screen-obj-y-got-clipped? self) nil
        (screen-obj-tick self) -1))


;;; temporary hack to debug screen-box allocation problem
(defvar *currently-allocated-screen-boxes* nil)

(defun debug-sb-alloc (sb)
  (if (fast-memq sb *currently-allocated-screen-boxes*)
    (cerror "go ahead" "The screen box, ~S, is ALREADY allocated" sb)
    (push sb *currently-allocated-screen-boxes*)))

(defun debug-sb-dealloc (sb)
  (if (fast-memq sb *currently-allocated-screen-boxes*)
    (setq *currently-allocated-screen-boxes*
          (fast-delq sb *currently-allocated-screen-boxes*))
    (cerror
     "go ahead"
     "The screen box being deallocated, ~S, does not seem to be in use"
     sb)))




;;; MAKE-INSTANCE calls INITIALIZE.
;;; ALLOCATE-SCREEN-OBJ-* calls RE-INIT on an old object that it's
;;; about to re-use.
;;; These methods might go away at some point, since most of the
;;; functionality has been moved to DE-ALLOCATE-INIT.
;;; DE-ALLOCATE-INIT should probably be DE-ALLOCATE-RESET or CLEAR


;;; screen objs
(defmethod re-init ((self screen-obj) new-actual-obj)
  (setf (screen-obj-actual-obj self) new-actual-obj))


;;; screen boxes

(defmethod de-allocate-init ((self screen-box))
  ;(debug-sb-dealloc self)
  (call-next-method)
  (setf (slot-value self 'superior-screen-box) nil
        (slot-value self 'scroll-to-actual-row) nil
        (slot-value self 'scroll-x-offset) 0
        (slot-value self 'scroll-x-offset) 0)
  (unless (null (slot-value self 'cached-absolute-pos))
    (setf (ab-pos-cache-valid (slot-value self 'cached-absolute-pos)) nil))
  (set-display-style self nil)
  (cond ((or (graphics-screen-box? self) (sprite-screen-box? self))
         ;; these go back on the graphics queue
         )
    ((box-ellipsis-style? (slot-value self 'screen-rows))
     (setf (slot-value self 'screen-rows) (allocate-storage-vector 8)))
    (t
     (clear-storage-vector (slot-value self 'screen-rows))))
  (setf (slot-value self 'screen-row) nil
        (bps self) nil
        (slot-value self 'name) nil))


;;; screen rows

(defmethod re-init ((self screen-box) new-actual-obj)
  ;  (debug-sb-alloc self)
  (call-next-method)
  (setf (slot-value self 'scroll-x-offset) 0
        (slot-value self 'scroll-y-offset) 0)
  (setf (box-type self) (class-name (class-of new-actual-obj))))

;;; graphics-screen-boxes redisplay their inferiors ONLY when
;;; the force-redisplay-infs? slot is set to T.  This is to
;;; avoid gratuitous redisplays of the box as a result of
;;; MODIFIED propagation from inferior boxes (like instance
;;; variables of sprites).  The first time we see a
;;; graphics-box, we'll want to look at the infs so explicitly set
;;; the flag.

(defmethod re-init ((self graphics-screen-box) new-actual-obj)
  (declare (ignore new-actual-obj))
  (call-next-method))

;; sprite-screen-box RE-INIT: inherit from screen-box for now...

(defmethod de-allocate-init ((self screen-row))
  (call-next-method)
  ;; should this restore the original size ?
  (clear-storage-vector (slot-value self 'screen-chas))
  (setf (screen-chas-array-fds (slot-value self 'screen-chas)) nil)
  (setf (slot-value self 'baseline) 0
        (slot-value self 'screen-box) nil))



;;;; HIGH-LEVEL SCREEN-OBJ allocation/deallocation code.

;;; This code is responsible for allocating screen-objs to represent actual
;;; objs. This code isn't terribly complicated, but it is basic to the rest
;;; of the display code, so it is probably a good idea to understand how it
;;; works. So, listen carefully... This code is based on the following basic
;;; assumptions:
;;;
;;;   No actual object can be displayed more than once at any
;;;    "level". For example, the same box cannot be displayed
;;;    right next to itself. On the other hand a port to a box
;;;    can be displayed right next to the box since the lispm
;;;    port object is neq to the lispm box object.
;;;
;;;   That whenever moving of actual objs is implemented (this
;;;;   includes boxing and unboxing operations) redisplay clues
;;;    which tell what happened will be added and this code will
;;;    be updated to take these clues into account.
;;;
;;; Given these assumptions, and given that:
;;;    ACTUAL-OBJ
;;;        is an actual obj to be displayed (a screen-obj is
;;;        needed in order to display it)
;;;    SUPERIOR-SCREEN-BOX
;;;        is the screen-box in which the actual obj is going
;;;        to be displayed
;;;    SCREEN-OBJ
;;;        is the screen-obj which represents the actual obj
;;;        when it is displayed in that particular superior
;;;        screen-box
;;; Then:
;;;
;;;     (ACTUAL-OBJ , SUPERIOR-SCREEN-OBJ)  SCREEN-OBJ
;;;
;;; The :ALLOCATE-SCREEN-OBJ-FOR-USE-IN method uses this mapping to allocate
;;; screen-objs to represent actual objs. Calling this method is the only
;;; correct way to get screen-objs which represent actual objs.

(defmethod allocate-screen-obj-for-use-in ((self actual-obj-subclass)
                                           use-in-screen-box)
  (let ((existing-screen-obj (assoc use-in-screen-box
                                    (slot-value self 'screen-objs))))
    (if (not-null existing-screen-obj)		;
      (cdr existing-screen-obj)
      (let ((new-screen-obj (allocate-screen-obj-internal self)))
        (push (cons use-in-screen-box new-screen-obj)
              (slot-value self 'screen-objs))
        (when (screen-box? new-screen-obj)
          (setf (superior-screen-box new-screen-obj) use-in-screen-box)
          (let ((name-row (name-row self)))
            (unless (null name-row)
              (setf (name new-screen-obj) (text-string name-row))))
          ;; see if there is any scrolling info which needs to be transferred
          (let* ((new-screen-port-chain (get-port-chain use-in-screen-box))
                 (current-scroll-info (getf (slot-value self 'plist)
                                            'scroll-info))
                 (match (assoc new-screen-port-chain current-scroll-info
                               :test #'equal)))
            (when (not (null match))
              (setf (slot-value new-screen-obj 'scroll-to-actual-row)
                    (cdr match)))))
        new-screen-obj))))

(DEFUN ALLOCATE-SCREEN-SHEET-FOR-USE-IN (GRAPHICS-SHEET USE-IN-SCREEN-BOX)
       (LET* ((SCREEN-OBJS (GRAPHICS-SHEET-SCREEN-OBJS GRAPHICS-SHEET))
              (EXISTING-SCREEN-OBJ (fast-assq USE-IN-SCREEN-BOX SCREEN-OBJS)))
             (IF (NOT-NULL EXISTING-SCREEN-OBJ)
                 (CDR EXISTING-SCREEN-OBJ)
                 (LET ((NEW-SCREEN-OBJ (ALLOCATE-SCREEN-OBJ-INTERNAL GRAPHICS-SHEET)))
                      (SETF (GRAPHICS-SHEET-SCREEN-OBJS GRAPHICS-SHEET)
                            (PUSH (CONS USE-IN-SCREEN-BOX NEW-SCREEN-OBJ) SCREEN-OBJS))
                      NEW-SCREEN-OBJ))))

(DEFUN SCREEN-STRUCTURE-ACTUAL-SUPERIOR-BOX (SCREEN-BOX)
       (LET ((SUPERIOR-SCREEN-BOX (WHEN (NOT-NULL SCREEN-BOX)
                                        (SUPERIOR-SCREEN-BOX SCREEN-BOX))))
            (WHEN (SCREEN-BOX? SUPERIOR-SCREEN-BOX)
                  (SCREEN-OBJ-ACTUAL-OBJ SUPERIOR-SCREEN-BOX))))

(defmethod allocate-outermost-screen-box-for-use-in ((self actual-obj-subclass)
                                                     window
                                                     &optional
                                                     (screen-box (bp-screen-box
                                                                  *point*)))
  (let ((actual-superior-box (screen-structure-actual-superior-box
                              screen-box)))
    (allocate-screen-obj-for-use-in
     self
     (if actual-superior-box
       (allocate-outermost-screen-box-for-use-in
        actual-superior-box window
        (superior-screen-box screen-box))
       window))))

(defmethod screen-objs ((self actual-obj-subclass))
  (mapcar #'cdr (actual-obj-screen-objs self)))

;;; Whenever any section of code is done with a screen-obj which they got by
;;; calling Allocate-Screen-Obj-For-Use-In they should deallocate that
;;; screen-obj by sending it a deallocate-self message. If there are no more
;;; users of that screen-obj, it will be returned to the pool of free
;;; screen-objs of that type.

(defmethod deallocate-self ((self screen-row))
  (when (null (slot-value self 'screen-box))
    (deallocate-inferiors self)
    (unless (null (screen-obj-actual-obj self))
      (delete-screen-obj (screen-obj-actual-obj self) self)
      (deallocate-screen-obj-internal self))))

(defmethod deallocate-self ((self screen-box))
  (when (null (slot-value self 'screen-row))
    (cond ((member self *outermost-screen-box-stack*)
           (format t "~%Trying to deallocate a screen box which is on *outermost-screen-box-stack*"))
      (t
       (deallocate-inferiors self)
       (delete-screen-obj (screen-obj-actual-obj self) self)
       (deallocate-screen-obj-internal self)))))

(defun clear-graphics-screen-sheet (gss)
  (setf (graphics-screen-sheet-actual-obj gss) nil))

(defmethod deallocate-self ((self graphics-screen-box))
  (delete-screen-obj (screen-obj-actual-obj self) self)
  (when (typep (screen-sheet self) 'graphics-screen-sheet)
    (clear-graphics-screen-sheet (screen-sheet self)))
  (deallocate-screen-obj-internal self)
  (set-screen-sheet self nil))

(defmethod deallocate-self ((self sprite-screen-box))
  (delete-screen-obj (screen-obj-actual-obj self) self)
  (deallocate-sprite-screen-box-internal self))

(defmethod deallocate-inferiors ((self screen-box))
  (when (storage-vector? (slot-value self 'screen-rows))
    ;; check because it could be a box ellipsis
    (do-vector-contents (screen-row-to-da (slot-value self 'screen-rows))
      ;; crash-fix Sometimes these vectors are padded with nil's
      (unless (null screen-row-to-da)
        ;; we have to sever the superior pointer before we call deallocate
        (setf (screen-box screen-row-to-da) nil)
        (deallocate-self screen-row-to-da)))
    (kill-screen-rows-from self 0)))

(defmethod deallocate-inferiors ((self screen-row))
  (do-vector-contents (screen-cha-to-da (slot-value self 'screen-chas))
    (when (screen-box? screen-cha-to-da)
      ;; sever the superior link...
      (setf (screen-row screen-cha-to-da) nil)
      (deallocate-self screen-cha-to-da)))
  (kill-screen-chas-from self 0))

(defmethod delete-screen-obj ((self actual-obj-subclass) screen-obj)
  (setf (actual-obj-screen-objs self)
        (delete (rassoc screen-obj (actual-obj-screen-objs self))
                (actual-obj-screen-objs self))))

(defun queue-screen-obj-for-deallocation (screen-obj)
  (declare (special screen-objs-deallocation-queue))
  (splice-item-onto-list screen-objs-deallocation-queue screen-obj))

(defun queue-screen-objs-for-deallocation-from (sv start &optional stop)
  (do-vector-contents (obj sv :start start :stop stop)
    (unless (screen-cha? obj)
      (queue-screen-obj-for-deallocation obj))))

(defun screen-obj-offsets (screen-obj)
  (values (screen-obj-x-offset screen-obj)
          (screen-obj-y-offset screen-obj)))

(defun set-screen-obj-offsets (screen-obj new-x-offset new-y-offset)
  (setf (screen-obj-x-offset screen-obj) new-x-offset)
  (setf (screen-obj-y-offset screen-obj) new-y-offset))
  ;; sgithens TODO debugging boxer-sunrise-22
  ;; (setf (screen-obj-x-offset screen-obj) (floor new-x-offset))
  ;; (setf (screen-obj-y-offset screen-obj) (floor new-y-offset)))

(defun screen-obj-size (screen-obj)
  (values (screen-obj-wid screen-obj)
          (screen-obj-hei screen-obj)))

(DEFUN SCREEN-BOXES-AND-WHITESPACE-SIZE (SCREEN-BOXES &AUX(WID 0) (HEI 0))
       (LET ((FIRST-BOX (CAR SCREEN-BOXES))
             (LAST-BOX (CAR (LAST SCREEN-BOXES))))
            (SETQ WID (- (+ (SCREEN-OBJ-X-OFFSET LAST-BOX) (SCREEN-OBJ-WID LAST-BOX))
                         (SCREEN-OBJ-X-OFFSET FIRST-BOX)))
            (DOLIST (SCREEN-BOX SCREEN-BOXES)
                    (SETQ HEI (MAX (SCREEN-OBJ-HEI SCREEN-BOX) HEI)))
            (VALUES WID HEI)))

(DEFUN MAP-OVER-SCREEN-OBJ (SCREEN-OBJ FN)
       (FUNCALL FN SCREEN-OBJ)
       (MAP-OVER-SCREEN-OBJS (INFERIORS SCREEN-OBJ) FN))

(DEFUN MAP-OVER-SCREEN-OBJS (LIST-OF-SCREEN-OBJS FN)
       (DOLIST (SCREEN-OBJ LIST-OF-SCREEN-OBJS)
               (MAP-OVER-SCREEN-OBJ SCREEN-OBJ FN)))


(defun screen-obj-zero-size (screen-obj)
  (setf (screen-obj-wid screen-obj) 0)
  (setf (screen-obj-hei screen-obj) 0))

(defun screen-object-width (screen-object)
  (when screen-object
    (if (screen-cha? screen-object)
      (cha-wid screen-object)
      (screen-obj-wid screen-object))))

(defun screen-object-height (screen-object)
  (when screen-object
    (if (screen-cha? screen-object)
      (cha-hei)
      (screen-obj-hei screen-object))))

;;; this erases ONLY characters to the end of the line
;;; NOTE: this CAN'T just iterate through the screen chas erasing
;;; characters BECAUSE the screen-chas may have been side effected
;;; HOWEVER, any boxes will still have valid offsets and widths so
;;; we use the boxes to delimit regions to erase.
;;; If there are no boxes, we simply erase to the end of the row
;;; This may actually turn out to be faster (especially in the X
;;; implementation) because of fewer graphics commands

(defun erase-chas-to-eol (cha-no screen-chas x-offset y-offset row-width
                                 &optional boxes-too)
  (let ((erase-height 0)
        (current-x-offset x-offset))
    (do-screen-chas-with-font-info (cha screen-chas :start cha-no)
      (cond ((screen-cha? cha)
             (setq erase-height (max erase-height (cha-hei))))
        ((not (null boxes-too))
         ;; we want to erase boxes as well as characters
         ;; first, adjust the size of the erasing rectangle
         (setq erase-height (max erase-height (screen-obj-hei cha)))
         ;; now do all the things erase-screen-box would have done
         ;(screen-obj-zero-size cha) ; huh ? this seems to break things
         ;; sgithens 2021-11-18 Removing these un-needed slots below.
         ;; The next question is, are any of these erase methods even
         ;; necessary anymore in the OpenGL double buffer version? TODO
         ;;  (set-needs-redisplay-pass-2? cha t)
         ;;  (set-force-redisplay-infs?   cha t)
         )
        (t
         ;; looks like we hit a box, and we want to preserve the box
         ;; erase from the current-x-offset to the beginning of the box
         (erase-rectangle (- (screen-obj-x-offset cha) current-x-offset)
                          erase-height
                          current-x-offset y-offset)
         ;; now setup the values for the next block
         (setq erase-height 0)
         (setq current-x-offset (+ (screen-obj-x-offset cha)
                                   (screen-obj-wid      cha))))))
    ;; now finish off the rest of the row
    (erase-rectangle (- row-width current-x-offset) erase-height
                     current-x-offset y-offset)))

(defun erase-screen-cha (screen-cha x-offset y-offset)
  (if (not-null screen-cha)
    (let ((wid (cha-wid screen-cha))
          (hei (cha-hei)))
      (erase-rectangle wid hei x-offset y-offset))
    (barf "null screen-cha for some reason")))

(defun erase-screen-box (screen-box x-offset y-offset)
  (multiple-value-bind (wid hei)
                       (screen-obj-size screen-box)
                       (erase-rectangle wid hei x-offset y-offset))
  (screen-obj-zero-size screen-box))

(defun erase-screen-chas (chas start-cha-no x-offset y-offset
                               &optional stop-cha-no)
  (do-screen-chas-with-font-info (cha-to-erase chas
                                               :start start-cha-no
                                               :stop stop-cha-no)
    (let (obj-wid)
      (cond ((screen-cha? cha-to-erase)
             (setq obj-wid (cha-wid cha-to-erase))
             (erase-screen-cha cha-to-erase x-offset y-offset))
        (t
         (setq obj-wid (screen-obj-wid cha-to-erase))
         (erase-screen-box cha-to-erase x-offset y-offset)))
      ;; now increment the x-offset (y-ofset doesn't change on the row)
      (setq x-offset  (+ x-offset obj-wid)))))

(defun erase-screen-obj (screen-obj)
  (when (not-null screen-obj)
    ;; sgithens TODO (check-screen-obj-arg screen-obj)
    (multiple-value-bind (wid hei)
                         (screen-obj-size screen-obj)
                         (multiple-value-bind (x-offset y-offset)
                                              (screen-obj-offsets screen-obj)
                                              (erase-rectangle wid hei x-offset y-offset)
                                              (screen-obj-zero-size screen-obj)))))



;;;; Utilities for Handling the screen-row-rdp1-info

;;; informs pass-2 that pass-1 erased all the characters from the
;;; optional cha-no arg to the end of the line
(defun set-rdp1-info-to-erase-to-eol (info &optional cha-no offset)
  (unless (eq (sr-rdp1-info-no-of-chas info) 'to-eol)
    (setf (sr-rdp1-info-action info) ':insert)
    (unless (null cha-no)
      (setf (sr-rdp1-info-from-cha-no info) cha-no))
    (unless (null offset)
      (setf (sr-rdp1-info-from-offset info) offset))
    (setf (sr-rdp1-info-no-of-chas  info) 'to-eol)))

(defun rdp1-info-is-eol? (info)
  (eq (sr-rdp1-info-no-of-chas info) 'to-eol))

(defun still-inside-rdp1-info (info cha-no)
  (cond ((numberp (sr-rdp1-info-no-of-chas info))
         (<= cha-no
             (+ (sr-rdp1-info-from-cha-no info)
                (sr-rdp1-info-no-of-chas  info))))
    ((eq (sr-rdp1-info-no-of-chas  info)
         'to-eol))))


;;; this cycles through the contents of the storage vector
;;; starting from FROM and erases them an queues them for deallocation
;;; returns the difference in offsets of any objects that would come
;;; after th eobjects which have been erased
(defun erase-and-queue-for-deallocation-screen-rows-from (sv from
                                                             &optional to
                                                             no-erase?)
  (declare (values delta-x-offset delta-y-offset))
  (unless (>=& from (storage-vector-active-length sv))
    (multiple-value-bind (x y)
                         (screen-obj-offsets (sv-nth from sv))
                         (let ((wid 0) (hei 0))
                           (do-vector-contents (screen-row sv :start from :stop to)
                             (setq wid (max wid (screen-obj-wid screen-row))
                                   hei (+  hei (screen-obj-hei screen-row)))
                             ;; dunno why these next 3 should be here but leave it in for now...
                             (screen-obj-zero-size screen-row)
                             (queue-screen-obj-for-deallocation screen-row))
                           ;; finally do the actual erasing
                           (unless no-erase? (erase-rectangle wid hei x y))
                           (values 0 hei)))))

(defun queue-for-deallocation-screen-rows-from (sv from &optional to)
  (declare (values delta-x-offset delta-y-offset))
  (unless (>=& from (storage-vector-active-length sv))
    (do-vector-contents (screen-row sv :start from :stop to)
      ;; dunno why these next 3 should be here but leave it in for now...
      (screen-obj-zero-size screen-row)
      (queue-screen-obj-for-deallocation screen-row))))


;:SHRUNK   USE *SHRUNK-BOX-WID* AND *SHRUNK-BOX-HEI*
;:NORMAL  IF ACTUAL-BOX HAS FIXED-SIZE USE IT OTHERWISE USE OTHER CONSTRAINT
;:OUTERMOST USE OUTERMOST-SIZE

;; Note that with name tabs on the sides of boxes we have to make sure that
;; the fixed size refers to the part of the box with actual contents in it
;; rather than the size of the entire box label included

(defmethod set-display-style-list ((box box) new-style)
  (setf (slot-value box 'display-style-list) new-style))

(defmethod set-display-style-list ((box screen-box) new-style)
  (setf (slot-value box 'display-style-list) new-style))

(defmethod display-style ((box box))
  (display-style-style (slot-value box 'display-style-list)))

;;; IMPORTANT.  The numbers returned by the various FIXED-SIZE methods
;;; refer to the size that the INFERIORS want to be and NOT the size of
;;; the entire box since the size of the NAME can change

(defmethod fixed-size ((self box))
  (case (display-style-style (slot-value self 'display-style-list))
    (:shrunk     (values *shrunk-box-wid* *shrunk-box-hei*))
    (:normal     (fixed-size-1 self))
    (otherwise   (fixed-size-1 self))))

(defmethod fixed-size-1 ((self box))
  (let ((display-style (slot-value self 'display-style-list)))
    (values (display-style-fixed-wid display-style)
            (display-style-fixed-hei display-style))))

(defmethod fixed-size? ((self box))
  (let ((ds (slot-value self 'display-style-list)))
    (or (eq (display-style-style ds) ':fixed)
        (numberp (display-style-fixed-wid ds))
        (numberp (display-style-fixed-hei ds)))))

(defmethod set-display-style ((self box) new-value)
  (setf (display-style-style (slot-value self 'display-style-list)) new-value))


;; boxes dimensions are now floats !
(defmethod set-fixed-size ((self box) new-fixed-wid new-fixed-hei)
  (let ((display-style (slot-value self 'display-style-list)))
    (unless (display-style-graphics-mode? display-style)
      ;; Special case:  if we were in graphics mode
      ;; then we want to leave the box in a shrink wrapping
      ;; state since the intent was probably to just resize the graphics
      ;; area and not to fix the size of the text view.
      (setf (display-style-fixed-wid display-style) new-fixed-wid)
      (setf (display-style-fixed-hei display-style) new-fixed-hei))
    (unless (or (null (graphics-sheet self))
                (null new-fixed-wid) (null new-fixed-hei)
                (not (display-style-graphics-mode? display-style))
                )
      (resize-graphics-sheet (graphics-sheet self)
                             ;; graphics sheets need integer dimensions
                             (round new-fixed-wid) (round new-fixed-hei)))
    ;; removed 6/26/95, no longer seems neccessary (except for gboxes)
    ;; A crock to get characters that were clipped to be redisplayed.
    ; removed totally, 1/24/12, shouldn't be neccessary when opengl repaints everything
    ;    (dolist (sbox (screen-objs self))
    ;      (when (graphics-screen-box? sbox) (set-force-redisplay-infs? sbox)))
    ))

(defmethod shrunken? ((self box))
  (let ((style (display-style-style (slot-value self 'display-style-list))))
    (or (eq style :shrunk) (eq style :supershrunk))))

(defmethod shrunken? ((self screen-box))
  (let ((style (display-style-style (slot-value self 'display-style-list))))
    (or (eq style :shrunk) (eq style :supershrunk))))

;;; border styles

(defmethod border-style ((self box))
  (display-style-border-style (slot-value self 'display-style-list)))

(defmethod set-border-style ((self box) new-style)
  (let ((display-style (slot-value self 'display-style-list)))
    (setf (display-style-border-style display-style) new-style)))

;;; this will do until we have ports that export (hopefully never)

(defmethod border-style ((self port-box))
  (let ((target (slot-value self 'ports)))
    (unless (null target)
      (display-style-border-style (slot-value target 'display-style-list)))))

(defmethod set-border-style ((self port-box) new-style)
  (let ((target (slot-value self 'ports)))
    (unless (null target)
      (let ((display-style (slot-value target 'display-style-list)))
        (setf (display-style-border-style display-style) new-style)))))

;;;; Display Styles for Screen Boxes

(defmethod display-style ((self screen-box))
  (let ((actual-obj-display-style-list
         (display-style-list (screen-obj-actual-obj self))))
    (or (display-style-style (slot-value self 'display-style-list))
        (display-style-style  actual-obj-display-style-list))))

(defmethod fixed-size ((self screen-box))
  (case (display-style self)
    (:shrunk
     (let ((*minimum-box-wid* 0) (*minimum-box-hei* 0))
       (multiple-value-bind (min-wid min-hei)
                            (box-borders-minimum-size (slot-value self 'box-type) self)
                            (multiple-value-bind (lb tb rb bb)
                                                 (box-borders-widths (slot-value self 'box-type) self)
                                                 (values (max *shrunk-box-wid* (- min-wid lb rb))
                                                         (max *shrunk-box-hei* (- min-hei tb bb)))))))
    (:normal     (fixed-size-1 self))
    (otherwise   (fixed-size-1 self))))

(defmethod fixed-size-1 ((self screen-box))
  (multiple-value-bind (actual-obj-fixed-wid actual-obj-fixed-hei)
                       (fixed-size-1 (screen-obj-actual-obj self))
                       (let ((display-style (slot-value self 'display-style-list)))
                         (values (or (display-style-fixed-wid display-style)
                                     actual-obj-fixed-wid)
                                 (or (display-style-fixed-hei display-style)
                                     actual-obj-fixed-hei)))))

;; graphics-box's never have a fixed size, they shrink to fit the
;; contained graphics sheet
(defmethod fixed-size-1 ((self graphics-screen-box))
  (values nil nil))

;; same for sprites
(defmethod fixed-size-1 ((self sprite-screen-box))
  (values nil nil))

(defmethod set-display-style ((self screen-box) new-value)
  (setf (display-style-style (slot-value self 'display-style-list))
        new-value))

(defmethod border-style ((self screen-box))
  (display-style-border-style (slot-value self 'display-style-list)))

(defmethod set-border-style ((self screen-box) new-style)
  (let ((display-style (slot-value self 'display-style-list)))
    (setf (display-style-border-style display-style) new-style)))

(defmethod set-fixed-size ((self screen-box) new-fixed-wid new-fixed-hei)
  (let ((display-style (slot-value self 'display-style-list)))
    (setf (display-style-fixed-wid display-style) new-fixed-wid)
    (setf (display-style-fixed-hei display-style) new-fixed-hei)))

(DEFMETHOD SHRINK ((SELF BOX))
           (SET-DISPLAY-STYLE SELF ':SHRUNK)
           (MODIFIED SELF))

(defmethod supershrink ((self box))
  (set-display-style self ':supershrunk)
  (modified self))

(DEFMETHOD UNSHRINK ((SELF BOX))
           (SET-DISPLAY-STYLE SELF ':NORMAL)
           (MODIFIED SELF))


(DEFMETHOD SHRINK ((SELF SCREEN-BOX))
           (SHRINK (SCREEN-OBJ-ACTUAL-OBJ SELF)))

(DEFMETHOD UNSHRINK ((SELF SCREEN-BOX))
           (UNSHRINK (SCREEN-OBJ-ACTUAL-OBJ SELF)))



(defmethod toggle-type ((self screen-box))
  (unless (null (first-screen-row self))
    (kill-screen-row self (first-screen-row self)))
  (let* ((eb (screen-obj-actual-obj self))
         (gi (graphics-info eb)))
    (cond ((graphics-sheet? gi)
           (change-class self (find-class 'graphics-screen-box)))
      ((graphics-object? gi)
       (change-class self (find-class 'sprite-screen-box)))))
  self)

(defmethod toggle-type ((self graphics-screen-box))
  ;; probably should be a little more modular here and have a
  ;; kill graphics-screen-sheet method or something
  (setf (slot-value self 'screen-rows) (allocate-storage-vector 8))
  (change-class self (find-class 'screen-box))
  self)

(defmethod toggle-type ((self sprite-screen-box))
  ;; probably should be a little more modular here and have a
  ;; kill graphics-screen-sheet method or something
  (setf (slot-value self 'screen-rows) (allocate-storage-vector 8))
  (change-class self (find-class 'screen-box))
  self)


;;;stuff for BOXTOPS
;; boxtop is supposed to return something to draw,
;; a graphics-sheet for :standard, and :named-graphic
;; a string for :folder and :name-only
;; a null boxtop means to just use the usual shrunken box appearance

(defun boxtop (editor-box)
  (let ((boxtop-prop (getprop editor-box :boxtop)))
    ;; nil prop means the same as standard...
    ;; NOTE: prop can be a graphics box if the box is a black box with a boxtop
    (cond ((graphics-sheet? boxtop-prop)
           ;; special case in top level of fileboxes
           boxtop-prop)
      ((or (null boxtop-prop)
           (eq boxtop-prop :standard)
           ;; for compatibility with mac files
           (eq boxtop-prop :file)
           (eq boxtop-prop :framed))
       (let* ((target (box-or-port-target editor-box))
              (bt-box (cond ((box? target)
                             (boxer-eval::lookup-static-variable-in-box-only
                              target 'bu::boxtop))
                        ((virtual-copy? target)
                         (lookup-variable-in-virtual-copy target
                                                          'bu::boxtop)))))
         (cond ((box? bt-box) (graphics-sheet bt-box))
           ((virtual-copy? bt-box)
            (graphics-info-graphics-sheet (vc-graphics bt-box)))
           (t (let ((cache (getprop editor-box :cached-boxtop)))
                (if (and (eq boxtop-prop :file) (null cache))
                  (boxtop-namestring editor-box)
                  cache))))))
      ((and (symbolp boxtop-prop)
            (eq (symbol-package boxtop-prop) pkg-bu-package))
       (let* ((boxer-eval::*lexical-variables-root* editor-box)
              (bt-box (boxer-eval::boxer-symeval boxtop-prop)))
         (cond ((box? bt-box) (graphics-sheet bt-box))
           ((virtual-copy? bt-box)
            (graphics-info-graphics-sheet (vc-graphics bt-box)))
           (t nil))))
      ((eq boxtop-prop :xref)
       ;; allow xrefs to have user boxtops
       (let* ((target (box-or-port-target editor-box))
              (bt-box (cond ((box? target)
                             (boxer-eval::lookup-static-variable-in-box-only
                              target 'bu::boxtop))
                        ((virtual-copy? target)
                         (lookup-variable-in-virtual-copy target
                                                          'bu::boxtop)))))
         (cond ((box? bt-box) (graphics-sheet bt-box))
           ((virtual-copy? bt-box)
            (graphics-info-graphics-sheet (vc-graphics bt-box)))
           (t (getprop editor-box :xref)))))
      ((fast-memq boxtop-prop '(:name-only :folder))
       (boxtop-namestring editor-box))
      (t boxtop-prop))))

(defmethod boxtop-namestring ((box box))
  (let ((nr (slot-value box 'name)))
    (typecase nr
              (string nr)
              (row (text-string nr))
              (t " "))))

;; used to compare boxtops to decide if a redisplay is needed
(defun boxtop= (bt1 bt2)
  (when (eq (type-of bt1) (type-of bt2))
    (typecase bt1
              (string (string= bt1 bt2))
              (t (eq bt1 bt2)))))

;;; DRAW-BOXTOP moved to gdispl.lisp because of macro dependencies




;;; Things having to do with a window's outermost screen box.

(DEFUN OUTERMOST-BOX (&OPTIONAL (WINDOW *BOXER-PANE*))
       (SCREEN-OBJ-ACTUAL-OBJ (boxer-window::outermost-screen-box WINDOW)))

(DEFMETHOD DISPLAYED-SCREEN-OBJS ((SELF ACTUAL-OBJ-SUBCLASS)
                                  &OPTIONAL (WINDOW *BOXER-PANE*))
           (LET ((ALL-SCREEN-OBJS (SCREEN-OBJS SELF))
                 (boxer-window::outermost-screen-box (boxer-window::outermost-screen-box WINDOW)))
                (WITH-COLLECTION
                 (DOLIST (SCREEN-OBJ ALL-SCREEN-OBJS)
                         (IF (SUPERIOR? SCREEN-OBJ boxer-window::outermost-screen-box)
                             (COLLECT SCREEN-OBJ))))))

(DEFMETHOD SUPERIOR? ((SELF SCREEN-OBJ) SCREEN-OBJ)
           "Is the Arg a superior of the instance ?"
           (LET ((SUPERIOR (SUPERIOR SELF)))
                (OR (EQ SCREEN-OBJ SELF)
                    (EQ SCREEN-OBJ SUPERIOR)
                    (AND (SCREEN-OBJ? SUPERIOR)
                         (SUPERIOR? SUPERIOR SCREEN-OBJ)))))

;;; Stuff for zooming in and out of boxes

(DEFUN GET-PREVIOUS-OUTERMOST-BOX-VALUES ()
       (LET ((PREVIOUS-OUTERMOST-SCREEN-BOX (POP *OUTERMOST-SCREEN-BOX-STACK*)))
            (IF (NULL PREVIOUS-OUTERMOST-SCREEN-BOX)
                (VALUES *INITIAL-BOX* (CAR (SCREEN-OBJS *INITIAL-BOX*)))
                (VALUES (SCREEN-OBJ-ACTUAL-OBJ PREVIOUS-OUTERMOST-SCREEN-BOX)
                        PREVIOUS-OUTERMOST-SCREEN-BOX))))

(defun set-outermost-box (new-outermost-box
                          &optional (new-outermost-screen-box
                                     (car (when (not-null new-outermost-box)
                                            (displayed-screen-objs
                                             new-outermost-box))))
                          (window boxer-window::*boxer-pane*))
  (let ((old-outermost-screen-box *outermost-screen-box*))
    (cond ((or (and (graphics-box? new-outermost-box)
                    (display-style-graphics-mode?
                     (display-style-list new-outermost-box)))
               (and (port-box? new-outermost-box)
                    (graphics-box? (ports new-outermost-box))
                    (display-style-graphics-mode?
                     (display-style-list new-outermost-box))))
           (beep))
      (t
       (when (name-row? (point-row))
         (move-point (box-first-bp-values new-outermost-box)))
       ;; We don't do this anymore.
       ;;;	   (redraw-status-line (name-string new-outermost-box))
       ;; outermost box based save document should uncomment next line...
       ;(set-window-name (current-file-status new-outermost-box))
       (set-outermost-screen-box
        (allocate-outermost-screen-box-for-use-in new-outermost-box window
                                                  new-outermost-screen-box)
        window)
       (update-shrink-proof-display)
       ))))



;;;these should go somewhere else eventually...
(defun update-shrink-proof-display ()
  (if (shrink-proof? (outermost-box))
    (status-line-display 'shrink-proof-screen "(Unshrinkable)")
    (status-line-undisplay 'shrink-proof-screen)))


;;;these should go somewhere else eventually...
(DEFMETHOD VISIBLE? ((SCREEN-OBJ SCREEN-OBJ))
           (MEMBER SCREEN-OBJ
                   (DISPLAYED-SCREEN-OBJS (SCREEN-OBJ-ACTUAL-OBJ SCREEN-OBJ))))


(DEFUN SET-OUTERMOST-SCREEN-BOX (NEW-OUTERMOST-SCREEN-BOX
                                 &OPTIONAL (WINDOW *BOXER-PANE*))
       (REDISPLAYING-WINDOW (WINDOW)
                            (UNLESS (EQ NEW-OUTERMOST-SCREEN-BOX *OUTERMOST-SCREEN-BOX*)
                                    (DECONFIGURE-SCREEN-BOX-TO-BE-OUTERMOST-BOX *OUTERMOST-SCREEN-BOX*
                                                                                WINDOW)
                                    (CONFIGURE-SCREEN-BOX-TO-BE-OUTERMOST-BOX NEW-OUTERMOST-SCREEN-BOX
                                                                              WINDOW)
                                    (ERASE-SCREEN-OBJ *OUTERMOST-SCREEN-BOX*)
                                    (SETQ *OUTERMOST-SCREEN-BOX* NEW-OUTERMOST-SCREEN-BOX)))
      ;  (SETQ *OUTERMOST-SCREEN-BOX* (boxer-window::outermost-screen-box)) ; why ??
       (setf *outermost-screen-box* new-outermost-screen-box)
       (LET ((OLD-SCREEN-ROW (UNLESS (NULL NEW-OUTERMOST-SCREEN-BOX)
                                     (SCREEN-ROW NEW-OUTERMOST-SCREEN-BOX))))
            (WHEN (SCREEN-ROW? OLD-SCREEN-ROW)
                  ;; we need to break up the screen-structure
                  (KILL-SCREEN-CHAS-FROM OLD-SCREEN-ROW 0)
                  (if (fast-memq (superior old-screen-row) *outermost-screen-box-stack*)
                    (deallocate-inferiors (superior old-screen-row))
                    (deallocate-self (superior old-screen-row))))
            (repaint-window window)))

(DEFUN CONFIGURE-SCREEN-BOX-TO-BE-OUTERMOST-BOX (SCREEN-BOX
                                                 &OPTIONAL
                                                 (WINDOW *BOXER-PANE*))
       (MULTIPLE-VALUE-BIND (MAX-WID MAX-HEI)
                            (OUTERMOST-SCREEN-BOX-SIZE WINDOW)
                            (MULTIPLE-VALUE-BIND (X-OFFSET Y-OFFSET)
                                                 (OUTERMOST-SCREEN-BOX-POSITION WINDOW)
                                                 (SET-DISPLAY-STYLE SCREEN-BOX ':NORMAL)
                                                 (SET-FIXED-SIZE SCREEN-BOX MAX-WID MAX-HEI)
                                                 (SET-OFFSETS SCREEN-BOX X-OFFSET Y-OFFSET))))

(defun deconfigure-screen-box-to-be-outermost-box (screen-box &optional ignore)
  (declare (ignore ignore))
  (unless (null screen-box)
    (set-display-style screen-box nil)
    (set-fixed-size screen-box nil nil)
    (set-offsets screen-box 0 0)))

(DEFUN OUTERMOST-SCREEN-BOX? (SCREEN-OBJ)
       (AND (SCREEN-BOX? SCREEN-OBJ)
            (EQ SCREEN-OBJ (boxer-window::outermost-screen-box))))

(defmethod xy-position ((self screen-row))
  (let ((superior (slot-value self 'screen-box)))
    (multiple-value-bind (superior-x-off superior-y-off)
                         (cond ((null superior) (values 0 0))
                           (t (xy-position superior)))
                         (values (+ superior-x-off (screen-obj-x-offset self)
                                    (slot-value superior 'scroll-x-offset))
                                 (+ superior-y-off (screen-obj-y-offset self)
                                    (slot-value superior 'scroll-y-offset))))))

(defmethod xy-position ((self screen-box))
  (multiple-value-bind (superior-x-off superior-y-off)
                       (cond ((outermost-screen-box? self)
                              (values 0 0))
                         (t
                          (xy-position (superior self))))
                       (values (+ superior-x-off (slot-value self 'x-offset))
                               (+ superior-y-off (slot-value self 'y-offset)))))

(defmethod window-xy-position ((self screen-obj))
  (multiple-value-bind (superior-x-off superior-y-off)
                       (cond ((outermost-screen-box? self)
                              (values 0 0))
                         (t
                          (xy-position (superior self))))
                       (values (+ superior-x-off (screen-obj-x-offset self))
                               (+ superior-y-off (screen-obj-y-offset self)))))

(defmethod window-x-position ((self screen-obj))
  (cond ((outermost-screen-box? self) (slot-value self 'x-offset))
    (t (+ (slot-value self 'x-offset)
          (window-x-position (superior self))))))

(defmethod window-y-position ((self screen-obj))
  (cond ((outermost-screen-box? self) (slot-value self 'y-offset))
    (t (+ (slot-value self 'y-offset)
          (window-y-position (superior self))))))


(DEFMETHOD NEXT-SCREEN-CHA-POSITION ((SELF SCREEN-CHAR-SUBCLASS))
           (MULTIPLE-VALUE-BIND (X Y)
                                (XY-POSITION SELF)
                                (VALUES (+ X (SCREEN-OBJ-WID SELF)) Y)))

(DEFMETHOD FIRST-SCREEN-CHA-POSITION ((SELF SCREEN-ROW))
           (XY-POSITION SELF))

(DEFMETHOD FIRST-SCREEN-CHA-POSITION ((SELF SCREEN-BOX))
           (MULTIPLE-VALUE-BIND (X Y)
                                (XY-POSITION SELF)
                                (MULTIPLE-VALUE-BIND (IL IT)
                                                     (box-borders-widths (slot-value self 'box-type) self)
                                                     (VALUES (+ X IL) (+ Y IT)))))



(defun outermost-screen-box-size (&optional (window *boxer-pane*))
  (multiple-value-bind (window-inner-wid window-inner-hei)
                       (window-inside-size window)
                       (values (- window-inner-wid (* 2 *space-around-outermost-screen-box*))
                               (- window-inner-hei (* 2 *space-around-outermost-screen-box*)))))

(defun outermost-screen-box-position (&optional ignore)
  (declare (ignore ignore))
  (values *space-around-outermost-screen-box*
          *space-around-outermost-screen-box*))



;;;; Operations Particular to SCREEN-BPs.


(DEFUN CURRENT-SCREEN-ROW (ACTUAL-ROW &OPTIONAL
                                      (SCREEN-BOX (BP-SCREEN-BOX *POINT*)))
       (LET ((SCREEN-ROWS (DISPLAYED-SCREEN-OBJS ACTUAL-ROW)))
            (DOLIST (SCREEN-ROW SCREEN-ROWS)
                    (WHEN (EQ (SUPERIOR SCREEN-ROW) SCREEN-BOX)
                          (RETURN SCREEN-ROW)))))

;;;; Cursor Tracker
;;; This is supposed to take a BP, usually the *POINT* and
;;; return the ABSOLUTE screen coordinates where the cursor
;;; should be drawn.
;;;
;;; In the simple case, we can just walk up the screen hierarchy
;;; summing local offsets.  What complicates this is that it is
;;; possible for the cursor to no longer be in the screen hierarchy.
;;; Some examples are pressing the MAKE-AND-ENTER-BOX key while
;;; inside a graphics box or resizing the boxer window to clip off
;;; the row where the cursor is.
;;;

(defun bp-coordinates (bp)
  (declare (values abs-x abs-y size-hint))
  (check-bp-arg bp)
  (let* ((row (bp-row bp))
         (cha-no (bp-cha-no bp))
         (screen-box (bp-screen-box bp))
         (screen-row (current-screen-row row screen-box)))
    (cond ((and (name-row? row)
                (not (null screen-box)))
           (screen-box-name-row-bp-position screen-box cha-no))
      ((not (null screen-row))
       ;; looks like there is a screen-row corresponding to where
       ;; the *point* is.  The only thing that can go wrong here is
       ;; that we might no longer be connected to the rest of the
       ;; screen hierarchy.
       (multiple-value-bind (x y)
                            (screen-obj-absolute-coordinates screen-row)
                            (cond ((null x)
                                   ;; we are not connected to the screen hierarchy
                                   ;; so call in someone smarter to figure out
                                   ;; where we should be
                                   (bp-coordinates-via-editor (screen-obj-actual-obj
                                                               screen-box)))
                              (t
                               ;; looks like we are CONNECTED
                               ;; now we have to find out the X offset in the row
                               (values (+ x
                                          (let ((x-offset 0) (last-screen-cha-no 0))
                                            (do-screen-chas-with-font-info
                                              (sc (slot-value screen-row
                                                              'screen-chas)
                                                  :index-var-name cha-pos)
                                              (cond ((= cha-no cha-pos)
                                                     (setq last-screen-cha-no cha-pos)
                                                     (return x-offset))
                                                (t (incf x-offset (screen-object-width sc))
                                                   (setq last-screen-cha-no cha-pos))))
                                            (when (> cha-no last-screen-cha-no)
                                              ;; if there are additional chars off the end of the row
                                              ;; which aren't in the screen structure, try and account for
                                              ;; their size as well
                                              (with-font-hacking ((row-fds row))
                                                (do* ((rest-cha-no (1+& last-screen-cha-no) (+& rest-cha-no 1))
                                                      (rest-cha (cha-at-cha-no row rest-cha-no)
                                                                (cha-at-cha-no row rest-cha-no)))
                                                  ((null rest-cha) x-offset)
                                                  (check-and-handle-font-changes rest-cha-no)
                                                  (incf x-offset (if (screen-cha? rest-cha)
                                                                   (cha-wid rest-cha)
                                                                   ;; just guess for a box since we dont want to
                                                                   ;; have to recurse for unseen boxes
                                                                   (unseen-box-width rest-cha))))))
                                            x-offset))
                                       y)))))
      ((not (null screen-box))
       (multiple-value-bind (x y)
                            (screen-obj-absolute-coordinates screen-box)
                            (cond ((null x)
                                   ;; the screen box is NOT connected so do the
                                   ;; complicated thing
                                   (bp-coordinates-via-editor (screen-obj-actual-obj
                                                               screen-box)))
                              ((null (superior-screen-box screen-box))
                               ;; looks like the screen-box has been deallocated
                               (let* ((bp-box (bp-box bp))
                                      (other-sbs (displayed-screen-objs bp-box))
                                      (matching-sb
                                       (dolist (sb other-sbs)
                                         (when (and (eq (screen-obj-actual-obj sb)
                                                        bp-box)
                                                    (not (eq screen-box sb)))
                                           (return sb)))))
                                 (if (not (null matching-sb))
                                   (progn (setf (bp-screen-box bp) matching-sb)
                                          ;; try again with a new bp-screen-box
                                          (bp-coordinates bp))
                                   (bp-coordinates-via-editor bp-box))))
                              (t
                               ;; screen-box is connected but the row is not
                               (multiple-value-bind (left top right bottom)
                                                    (box-borders-widths (box-type screen-box) screen-box)
                                                    (case (editor-row-location-in-screen-box row screen-box)
                                                      (:end (values (- (+ x (screen-obj-wid screen-box))
                                                                       right)
                                                                    (- (+ y (screen-obj-hei screen-box))
                                                                       bottom)
                                                                    ':end))
                                                      ;; for now, :beginning is the same as the default
                                                      (otherwise (values (+ x left)
                                                                         (+ y top)
                                                                         ':beginning))))))))
      (t ;; return SOME coords
         (values 0 0)))))

(defun screen-box-name-row-bp-position (screen-box cha-no)
  (multiple-value-bind (x y)
                       (screen-obj-absolute-coordinates screen-box)
                       (if (null x)
                         (bp-coordinates-via-editor (screen-obj-actual-obj screen-box))
                         (multiple-value-bind (tab-x tab-y)
                                              (box-borders-name-tab-position screen-box x y cha-no)
                                              (values tab-x tab-y ':name)))))

;;; This checks to see if the editor row is part of the screen-box but doesn't
;;; happen to have screen structure because of either scrolling or clipping
;;; of the box.  Returns a keyword (:end or :beginning) indicating where in
;;; the box it can be found.
(defun editor-row-location-in-screen-box (editor-row screen-box)
  (cond ((eq (superior-box editor-row) (screen-obj-actual-obj screen-box))
         ;; sanity check, make sure that the editor row is part of the
         ;; editor-box that the screen-box is representing
         (if (row-> editor-row (first-screen-row screen-box))
           ':end
           ':beginning))
    (t
     ;; maybe this should error out instead
     (if *boxer-system-hacker*
       (cerror "Ignore"
               "The Row, ~A, is not in the editor box for ~A"
               editor-row screen-box)
       nil))))

;;; this walks up editor structure until it finds a box with screen structure
;;; that is connected and then returns those coordinates
(defun bp-coordinates-via-editor (editor-box)
  (cond ((null editor-box)
         (values 0 0 ':lost))
    (t
     (let ((sb (car (displayed-screen-objs editor-box))))
       (cond ((null sb)
              (bp-coordinates-via-editor (superior-box editor-box)))
         (t
          (multiple-value-bind (x y)
                               (screen-obj-absolute-coordinates sb)
                               (cond ((null x)
                                      ;; still not connected to the hierarchy
                                      (bp-coordinates-via-editor (superior-box
                                                                  editor-box)))
                                 (t
                                  ;; this could be a little smarter....
                                  (multiple-value-bind (left top right bottom)
                                                       (box-borders-widths (box-type sb) sb)
                                                       (declare (ignore left top))
                                                       (values (- (+ x (screen-obj-wid sb))
                                                                  right)
                                                               (- (+ y (screen-obj-hei sb))
                                                                  bottom)
                                                               sb)))))))))))


;;; Note: We can't use the XY-POSITION method because it assumes an unbroken
;;; chain of screen objects all the way up to the outermost screen box.

(defun screen-obj-absolute-coordinates (so)
  (catch 'not-in-hierarchy
    (screen-obj-absolute-coordinates-1 so)))

(defun screen-obj-absolute-coordinates-1 (so)
  (let ((superior (superior so)))
    (multiple-value-bind (superior-x-offset superior-y-offset)
                         (cond ((or (outermost-screen-box? so)
                                    (not (screen-obj? superior)))
                                (values 0 0))
                           ((null superior)
                            (throw 'not-in-hierarchy nil))
                           (t
                            (screen-obj-absolute-coordinates-1 superior)))
                         (values (+ superior-x-offset (screen-obj-x-offset so)
                                    (if (screen-box? so) (slot-value so 'scroll-x-offset) 0))
                                 (+ superior-y-offset (screen-obj-y-offset so)
                                    (if (screen-box? so) (slot-value so 'scroll-y-offset) 0))))))



(defmethod cha-no->x-coord ((row screen-row) cha-no)
  (with-summation
    (do-vector-contents (cha (slot-value row 'screen-chas) :stop cha-no)
      (sum (screen-object-width cha)))))


(DEFMETHOD SCREEN-BP ((SELF SCREEN-CHAR-SUBCLASS))
           (LET ((BP (MAKE-BP 'FIXED)))
                (MOVE-BP BP (CHA-BP-VALUES (SCREEN-OBJ-ACTUAL-OBJ SELF)))
                BP))

(DEFMETHOD NEXT-SCREEN-BP ((SELF SCREEN-CHAR-SUBCLASS))
           (LET ((BP (MAKE-BP 'FIXED)))
                (MOVE-BP BP (CHA-NEXT-BP-VALUES (SCREEN-OBJ-ACTUAL-OBJ SELF)))
                BP))

(DEFMETHOD FIRST-SCREEN-BP ((SELF SCREEN-ROW))
           (LET ((BP (MAKE-BP 'FIXED)))
                (MOVE-BP BP (ROW-FIRST-BP-VALUES (SCREEN-OBJ-ACTUAL-OBJ SELF)))
                BP))

(DEFMETHOD LAST-SCREEN-BP ((SELF SCREEN-ROW))
           (LET ((BP (MAKE-BP 'FIXED)))
                (MOVE-BP BP (ROW-LAST-BP-VALUES (SCREEN-OBJ-ACTUAL-OBJ SELF)))
                BP))

(DEFMETHOD FIRST-SCREEN-BP ((SELF SCREEN-BOX))
           (LET ((BP (MAKE-BP 'FIXED)))
                (MOVE-BP BP (BOX-FIRST-BP-VALUES (SCREEN-OBJ-ACTUAL-OBJ SELF)))
                BP))

(DEFMETHOD LAST-SCREEN-BP ((SELF SCREEN-BOX))
           (LET ((BP (MAKE-BP 'FIXED)))
                (MOVE-BP BP (BOX-LAST-BP-VALUES (SCREEN-OBJ-ACTUAL-OBJ SELF)))
                BP))

;;; returns the screen obj of box which is within
(DEFUN INF-CURRENT-SCREEN-BOX (BOX)
       (CAR (MEMBER (BP-SCREEN-BOX *POINT*) (DISPLAYED-SCREEN-OBJS BOX)
                    :TEST  #'(LAMBDA (SUPERIOR-BOX BOX)
                                     (EQ SUPERIOR-BOX (SUPERIOR-SCREEN-BOX BOX))))))



;;; circular structure support

(DEFUN PORT-HAS-BEEN-DISPLAYED-ENOUGH? (PORT)
       (LET ((ENTRY (CDR (ASSOC PORT PORT-REDISPLAY-HISTORY))))
            (AND ENTRY (>= ENTRY *PORT-REDISPLAY-DEPTH*))))

(DEFUN UPDATE-PORT-REDISPLAY-HISTORY (PORT)
       (LET ((ENTRY (ASSOC PORT PORT-REDISPLAY-HISTORY)))
            (IF (NULL ENTRY) (APPEND PORT-REDISPLAY-HISTORY (LIST (CONS PORT 1)))
                (LET ((NEW-HISTORY (COPY-SEQ PORT-REDISPLAY-HISTORY)))
                     (SETF (CDR (ASSOC PORT NEW-HISTORY)) (1+ (CDR ENTRY)))
                     NEW-HISTORY))))

;;; some styles...

(define-box-ellipsis-style box-ellipsis-solid-lines)

(defun box-ellipsis-solid-lines-draw-self (x-coord y-coord)
  (do ((x x-coord (+ x *box-ellipsis-thickness* *box-ellipsis-spacing*))
       (y y-coord (+ y *box-ellipsis-thickness* *box-ellipsis-spacing*))
       (wid (- *box-ellipsis-wid* (* 2 *box-ellipsis-thickness*))
            (- wid (* 2 (+ *box-ellipsis-thickness* *box-ellipsis-spacing*))))
       (hei *box-ellipsis-hei*
            (- hei (* 2 (+ *box-ellipsis-thickness* *box-ellipsis-spacing*)))))
    ((or (>= x (+ x-coord (floor (/ *box-ellipsis-wid* 2.))))
         (>= y (+ y-coord (floor (/ *box-ellipsis-wid* 2.))))
         (<= wid 0)
         (<= hei 0)))
    (draw-rectangle *box-ellipsis-thickness* hei x y)
    (draw-rectangle wid *box-ellipsis-thickness*
                    (+ x *box-ellipsis-thickness*) y)
    (draw-rectangle *box-ellipsis-thickness* hei
                    (+ x wid *box-ellipsis-thickness*) y)
    (draw-rectangle wid *box-ellipsis-thickness*
                    (+ x *box-ellipsis-thickness*)
                    (+ y hei (- *box-ellipsis-thickness*)))))

(setf (get 'box-ellipsis-solid-lines 'draw-self)
      'box-ellipsis-solid-lines-draw-self)

(define-box-ellipsis-style box-ellipsis-corner-dots)

(defun box-ellipsis-corner-dots-draw-self (x-coord y-coord)
  (do ((x x-coord (+ x *box-ellipsis-thickness* *box-ellipsis-spacing*))
       (y y-coord (+ y *box-ellipsis-thickness* *box-ellipsis-spacing*))
       (wid (- *box-ellipsis-wid* (* 2 *box-ellipsis-thickness*))
            (- wid (* 2 (+ *box-ellipsis-thickness* *box-ellipsis-spacing*))))
       (hei *box-ellipsis-hei*
            (- hei (* 2 (+ *box-ellipsis-thickness* *box-ellipsis-spacing*)))))
    ((or (>= x (+ x-coord (floor (/ *box-ellipsis-wid* 2.))))
         (>= y (+ y-coord (floor (/ *box-ellipsis-wid* 2.))))
         (<= wid 0)
         (<= hei 0)))
    (draw-rectangle *box-ellipsis-thickness* *box-ellipsis-thickness*
                    x y)
    (draw-rectangle *box-ellipsis-thickness* *box-ellipsis-thickness*
                    (+ x wid *box-ellipsis-thickness*) y)
    (draw-rectangle *box-ellipsis-thickness* *box-ellipsis-thickness*
                    x (+ y hei (- *box-ellipsis-thickness*)))
    (draw-rectangle *box-ellipsis-thickness* *box-ellipsis-thickness*
                    (+ x wid *box-ellipsis-thickness*)
                    (+ y hei (- *box-ellipsis-thickness*)))))

(setf (get 'box-ellipsis-corner-dots 'draw-self)
      'box-ellipsis-corner-dots-draw-self)
