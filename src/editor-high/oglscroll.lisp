;;; -*- Mode:lisp;Syntax:Common-Lisp; Package:BOXER; Base:10.-*-

#|

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                          +-Data--+
                 This file is part of the | BOXER | system
                                          +-------+

 This file contains support for scrolling


Modification History (most recent at top)

11/ 1/12 v-scrollable? handles port ellipsis case
 9/15/12 de-fixnum arithmetic: initialize-horizontal-border-thicknesses, estimate-box-height,
                               assure-head-room-in-box, ensure-row-is-displayed,
                               screen-box-is-scrollable?, h-scrollable?, draw-scroll-info,
                               {h,v}-scroll-info, scroll-buttons-extent
                               draw-{vertical,horizontal}-{elevator,scroll-buttons},
                               mouse-in-h-scroll-bar-internal
11/28/09 draw-scroll-info moves vertical scrolling GUI buttons/elevator 1 pixel over for file boxes
         so they aren't 1/2 obscured by thicker border
 2/15/03 merged current LW and MCL files
 2/17/01 merged current LW and MCL files
12/19/00 screen-box-is-scrollable? changed to handled partially scrolled 1st row
12/15/00 ensure-row-is-displayed did not allow for bottom box border width
         in calculating room available for new rows
10/25/00 LW draw/erase-scroll-buttons bug seems fixed by recompilation
10/13/99 added support for boxtops in estimate-box-height
10/12/99 estimate-row-height fixed
10/09/98 estimate-row-height changed to use font info
 5/14/98 handle 0 char defaults for baseline and hei in
         update-screen-row-for-scrolling added 0-char-height for use by
         estimate-row-height assure-head-room-in-box rewritten flushed
         max-char-height which is no longer appropriate in a multi font world
 5/11/98 update-screen-row-for-scrolling includes baseline calculations using
         screen-cha-increment-superior-parameters
 5/02/98 started logging source = boxer version 2.2r4 + fonts

|#

(in-package :boxer)



;;;; Support for scrolling (after a c-N from the bottom of a box for example)

;; Estimate the size of a row from the actual structure
;; we are assuming that boxes are ALWAYS bigger than chas
;; this assumes that the font map is already bound
;; it is used by ASSURE-HEAD-ROOM-IN-BOX which bind the font map

(defun 0-char-height (row)
  (rebind-font-info ((bfd-font-no (closest-bfd row 0))) (cha-hei)))

(defun estimate-row-height (row)
  (if (null row) 0
    (let ((height 0) (ix 0))
      (with-font-hacking ((row-fds row))
        (do-row-chas ((c row))
          (check-and-handle-font-changes ix)
          (incf& ix)
          (when (box? c) (setq height (max height (estimate-box-height c))))))
      (max height (0-char-height row)))))


;; this assumes that the font map is already bound
;; it is used by ASSURE-HEAD-ROOM-IN-BOX which bind the font map

;; this should be initialized somewhere inside of setup-editor
;; but guess at it for now.  The estimate should err toward being too big
;; rather than too small

(defvar *average-border-height* 20)

(defvar *data-box-horizontal-border-thickness* 0)
(defvar *doit-box-horizontal-border-thickness* 0)
(defvar *port-box-horizontal-border-thickness* 0)
(defvar *sprite-box-horizontal-border-thickness* 0)

(defun initialize-horizontal-border-thicknesses ()
  (flet ((horizontal-border-thickness (box)
                                      (let ((screen-box (allocate-screen-obj-internal box)))
                                        (multiple-value-bind (left top right bottom)
                                                             (box-borders-widths (class-name (class-of box)) screen-box)
                                                             (declare (ignore left right))
                                                             (deallocate-screen-obj-internal screen-box)
                                                             (+ top bottom)))))
          (setq *data-box-horizontal-border-thickness*
                (horizontal-border-thickness (make-instance 'data-box))
                *doit-box-horizontal-border-thickness*
                (horizontal-border-thickness (make-instance 'doit-box))
                *port-box-horizontal-border-thickness*
                (horizontal-border-thickness (let ((box (make-instance 'port-box)))
                                               (setf (ports box)
                                                     (make-instance 'data-box))
                                               box)))))

(defun horizontal-border-height (box)
  (case (class-name (class-of box))
    (data-box *data-box-horizontal-border-thickness*)
    (doit-box *doit-box-horizontal-border-thickness*)
    (port-box *port-box-horizontal-border-thickness*)
    (otherwise *average-border-height*)))

(defun record-circular-port (port)
  (let ((entry (fast-assq port port-redisplay-history)))
    (if (null entry)
      (push (cons port 1) port-redisplay-history)
      (setf (cdr entry) (1+ (cdr entry))))))

;; added support for boxtops 10/13/99
(defun estimate-box-height (box)
  (cond ((eq (display-style box) ':shrunk)
         (let ((boxtop (boxtop box)))
           (cond ((null boxtop)
                  (+ *shrunk-box-hei* (horizontal-border-height box)))
             (t (multiple-value-bind (btwid bthei)
                                     (boxtop-size boxtop box)
                                     (declare (ignore btwid))
                                     bthei)))))
    ((numberp (display-style-fixed-hei (display-style-list box)))
     (display-style-fixed-hei (display-style-list box)))
    ((circular-port? box)
     (if (port-has-been-displayed-enough? box)
       (+ (horizontal-border-height box)
          (multiple-value-bind (ewid ehei)
                               (funcall (get *box-ellipsis-current-style* 'size))
                               (declare (ignore ewid)) ehei))
       (progn
        (record-circular-port box)
        (+ (horizontal-border-height box)
           (with-summation
             (dolist (row (rows box))
               (sum (estimate-row-height row))))))))
    (t
     (+ (horizontal-border-height box)
        (with-summation
          (dolist (row (rows box)) (sum (estimate-row-height row))))))))

(defun assure-head-room-in-box (last-row screen-box)
  "This starts at LAST-ROW and returns the highest up row that can
   be the 1st row and still have LAST-ROW be displayed based on
   the current size of SCREEN-BOX. "
  (let* ((available-room (- (screen-obj-hei screen-box)
                            (horizontal-border-height
                             (screen-obj-actual-obj screen-box))))
         (port-redisplay-history nil)
         (room-left (- available-room (estimate-row-height last-row)))
         (fits-row nil))
    (do ((row last-row (previous-row row)))
      ((null row) (or fits-row (first-inferior-row (superior-box last-row))))
      (let ((remaining (- room-left (estimate-row-height row))))
        (cond ((<= remaining 0)
               (return (or fits-row row)))
          (t
           (setq fits-row row room-left remaining)))))))

(defun assure-leg-room-in-box (row screen-box)
  (declare (ignore screen-box))
  row)

;; does the row have screen structure within the screen box
(defmethod row-has-screen-structure? ((self row)
                                      &optional (current-screen-box
                                                 (point-screen-box)))
  (cdr (assoc current-screen-box (actual-obj-screen-objs self))))

(defmethod ensure-row-is-displayed ((row row) screen-box
                                              &optional (direction -1) scroll-anyway)
  "make sure that the screen box's scroll to actual row is such
   that ROW will be seen. A DIRECTION of 1 specifies that we are
   moving downward, -1 upward. "
  (let ((screen-row (row-has-screen-structure? row screen-box))
        (box (screen-obj-actual-obj (point-screen-box))))
    #| what is THIS supposed to do ?????
    (if (and screen-row (fixed-size box)
             (<= (+ (screen-obj-y-offset screen-row)
                    (horizontal-border-height box)
                    );; fudge factor to solve a boundary cond.
                 (screen-obj-hei screen-box)))
      nil) ; extra close paren for emacs' puny mind
    |#
    (cond
      ((and (not screen-row) (not box))
        nil) ;; 2021-03-23 sgithens This is a crash fix for bad cases.
      ((eq row (slot-value box 'closets))
           (unless (row-row-no box row)
             ;; if the closet is not opened, open it
             (insert-row-at-row-no box row 0 t)
             (modified row))
           ;; then scroll to the top
           (set-scroll-to-actual-row screen-box nil))
      ((or scroll-anyway
           ;; We should scroll if we have been told to, OR
           (and screen-row (screen-obj-y-got-clipped? screen-row))
           ;; the screen structure that is there is clipped, OR
           (row-< row (scroll-to-actual-row screen-box))
           ;; the destination row is above the current scroll row
           (and (or (fixed-size box)
                    (screen-obj-y-got-clipped? screen-box))
                ;; various cases where the box can't get any bigger
                (or (and (null screen-row)
                         ;; no screen-row
                         (or (minusp& direction)
                             ;; could be a new row, check to
                             ;; see if there is room
                             (if (last-screen-row screen-box) (let* ((lsr (last-screen-row screen-box))
                                    (ler (screen-obj-actual-obj lsr))
                                    (available-room (- (screen-obj-hei
                                                        screen-box)
                                                       ;; quick and dirty
                                                       (round
                                                        (horizontal-border-height
                                                         box) 2)
                                                       ;; the real deal
                                                       ;(multiple-value-bind
                                                       ;  (lef top rig bot)
                                                       ;  (box-borders-widths
                                                       ;   (box-type box)
                                                       ;   screen-box)
                                                       ;  (declare (ignore
                                                       ;            lef top rig))
                                                       ;  bot)
                                                       )))
                               (if (row-< row ler)
                                 ;; if the row is before the last
                                 ;; visible row don't have to scroll
                                 ;; since we've already made sure that
                                 ;; the row is AFTER the current
                                 ;; scrolled row
                                 nil
                                 (do* ((edrow ler (next-row edrow))
                                       (y (+ (screen-obj-y-offset
                                              lsr)
                                             (screen-obj-hei lsr))
                                          (+ y (estimate-row-height
                                                edrow))))
                                   ((or (null edrow)
                                        (>= y available-room))
                                    (not (null edrow)))
                                   (when (eq edrow row)
                                     (return nil))))))))
                    (and screen-row
                         (> (+ (screen-obj-y-offset screen-row)
                               (horizontal-border-height box))
                            (screen-obj-hei screen-box))))))
       (set-scroll-to-actual-row
        screen-box
        (if (minusp direction)
          (let* ((new-row (assure-head-room-in-box row screen-box))
                 (prev-row (previous-row new-row)))
            (if (and scroll-anyway
                     (eq new-row (scroll-to-actual-row screen-box))
                     (not (null prev-row)))
              prev-row
              new-row))
          ;; sounds like a box is a luxury car
          ;; (or want to be one)
          (assure-leg-room-in-box  row screen-box)))))))


(defmethod ensure-row-is-displayed ((row name-row) screen-box
                                                   &optional (direction -1) scroll-anyway)
  (declare (ignore screen-box direction scroll-anyway))
  nil)


;; should return 2 boolean values, whether vert scrolling &  horiz scrolling
(defmethod screen-box-is-scrollable? ((screen-box screen-box))
  (declare (values top? bottom? last-row-at-top?))
  (let ((top (slot-value screen-box 'scroll-to-actual-row))
        (lsr (last-screen-row screen-box)))
    ;; is this the right place to interfere ?
    (when (and (row? top)
               (not (eq (let ((eb (slot-value screen-box 'actual-obj)))
                          (if (port-box? eb) (ports eb) eb))
                        (superior-box top))))
      ;; looks the the scrolling row is no longer part of the box
      (set-scroll-to-actual-row screen-box nil)
      (setq top nil))
    (let* ((edbox (slot-value screen-box 'actual-obj))
           (last-ed-row (if (port-box? edbox)
                          (last-inferior-row (ports edbox))
                          (last-inferior-row edbox))))
      (values (or (and top (or (not (eq top (first-inferior-row edbox)))))
                  (not (zerop (slot-value screen-box 'scroll-y-offset))))
              (and lsr (not (eq (screen-obj-actual-obj lsr) last-ed-row)))
              ; using this allows all except 1-row boxes to scroll
              ;	         (eq (or top (first-inferior-row
              ;			       (slot-value screen-box 'actual-obj)))
              ;		     (and lsr (screen-obj-actual-obj lsr)))
              (eq last-ed-row (slot-value screen-box 'scroll-to-actual-row))
              ))))

(defmethod screen-box-is-scrollable? ((screen-box graphics-screen-box))
  (values nil nil))

(defun dont-show-scroll-buttons? (screen-box)
  (or ;(null screen-box)  ; should already have been checked
    (null (superior-screen-box screen-box))
      (graphics-screen-box? screen-box)
      (and (not (outermost-screen-box? screen-box))
           (fast-memq (display-style (screen-obj-actual-obj screen-box))
                      '(:shrunk :supershrunk :boxtop)))))

;; this is a hook for a later time when we might want to have
;; the ability to lock unlock individual boxes
(defun closet-locked? (screen-box)
  (declare (ignore screen-box))
  (or *lock-all-closets*
      ))

(defmethod closet-opened? ((screen-box screen-box))
  (let* ((actual-obj (screen-obj-actual-obj screen-box))
         (closet-row (slot-value actual-obj 'closets)))
    (when (row? closet-row)
      (member screen-box (screen-objs closet-row)
              :test #'(lambda (sb row) (eq sb (screen-box row)))))))

(defmethod closet-opened? ((box box))
  (let ((cl (slot-value box 'closets)))
    (and cl (row-row-no box cl))))



;;;; New Stuff

(defmethod v-scrollable? ((self screen-box))
  (screen-obj-y-got-clipped? self))

(defmethod h-scrollable? ((self screen-box))
  (screen-obj-x-got-clipped? self))

;;; scroll bar support
;;; scroll bars are displayed if:
;;; {X,Y}-got-clipped?, or the scroll-{x,y}-offset is non zero, or scroll-to-actual-row is non null
;;;
(defmethod draw-scroll-info ((self screen-box))
  (with-slots (actual-obj wid hei box-type scroll-to-actual-row content-hei content-wid
                          scroll-x-offset scroll-y-offset ;;vertical elevator drawing should use this...
                          max-scroll-wid)
    self
    (multiple-value-bind (il it ir ib)
                         (box-borders-widths box-type self)
                         (let* ((inner-wid (- wid il ir))
                                (inner-hei (- hei it ib))
                                (type-label-width (border-label-width (box-type-label actual-obj)))
                                (vert-x (- wid ir (- (border-thickness (border-style actual-obj)) 1))))
                           (when (v-scrollable? self)
                             ;; ok need to draw vertical scroll GUI
                             (draw-vertical-scrollbar vert-x it inner-hei
                                                      (* (/ inner-hei content-hei) inner-hei)
                                                      ;; percent scrolled
                                                      (/ scroll-y-offset (- content-hei inner-hei))))
                           (when (h-scrollable? self)
                             ;; ok, need to draw horizontal scroll GUI
                             ;; It is possible for scroll-x-offset to exceed max-scroll-wid under certain conditions in
                             ;; particular, vertical scrolling away from an extra wide section which horizontally scrolled
                             (draw-horizontal-scrollbar (+ type-label-width il) (- hei ib)
                                                        inner-wid (* (/ inner-wid content-wid) inner-wid)
                                                        ;; percent scrolled
                                                        (/ scroll-x-offset (- content-wid inner-wid))))))))

(defmethod draw-scroll-info ((self graphics-screen-box))
  (with-slots (actual-obj wid hei box-type)
    self
    (multiple-value-bind (il it ir ib)
                         (box-borders-widths box-type self)
                         (let* ((gs (graphics-sheet actual-obj))
                                (gsw (graphics-sheet-draw-wid gs))
                                (gsh (graphics-sheet-draw-hei gs))
                                (inner-wid (- wid il ir))
                                (inner-hei (- hei it ib)))
                           (unless (>= inner-hei gsh)
                             ;; need to draw vertical scroll GUI
                             )
                           (unless (>= inner-wid gsw)
                             ;; need to draw horizontal scroll GUI
                             )
                           ))))

(defvar *scroll-info-offset* 0 "How far to indent scroll info from the inner (text) part of the box")
(defvar *scroll-elevator-thickness* 6)
(defvar *scroll-elevator-color*)

; init
(def-redisplay-initialization (setq *scroll-elevator-color* *gray*))

(defun draw-vertical-scrollbar (x y hei scroll-bar-hei percent-scrolled)
  (let ((scrolled (* percent-scrolled (- hei scroll-bar-hei))))
    (with-pen-color (*scroll-elevator-color*)
      (draw-rectangle *scroll-elevator-thickness* scroll-bar-hei
                      (+ x *scroll-info-offset*) (- y scrolled)))))

(defun draw-horizontal-scrollbar (x y wid scroll-bar-wid percent-scrolled)
  (let ((scrolled (* percent-scrolled (- wid scroll-bar-wid))))
    (with-pen-color (*scroll-elevator-color*)
      (draw-rectangle scroll-bar-wid *scroll-elevator-thickness*
                      (- x scrolled) (+ y *scroll-info-offset*)))))

;; scroll position tracking.  Get-position-in-border only returns :Scroll-bar
;; which causes a generic mouse-scrolling com to be invoked.  Finer grained
;; tracking (elevator, button, scroll bar, horizontal or vertical) of the position
;; in the scrolling area is done here

;; Note, we SHOULD already be in a scrolling area since this is called from within a
;; mouse scrolling command to dispatch

;; possible return values are:
;; :v-up-button, :v-down-button, :h-left-button, :h-right-button, :v-bar, :h-bar or NIL
;; NIL means no-op, it can occur if the mouse is in the scroll area but on an inactive spot
;; for example if it is on a scroll up button when the box is already all the way scrolled to the top

(defun get-scroll-position (x y screen-box &optional (box-type (box-type screen-box)))
  (multiple-value-bind (box-window-x box-window-y)
                       (xy-position screen-box)
                       (multiple-value-bind (left top right bottom)
                                            (box-borders-widths box-type screen-box)
                                            (declare (ignore left top))
                                            (multiple-value-bind (wid hei)
                                                                 (screen-obj-size screen-box)
                                                                 (multiple-value-bind (scroll-top scroll-bottom last-is-top?)
                                                                                      (screen-box-is-scrollable? screen-box)
                                                                                      (declare (ignore scroll-bottom))
                                                                                      (let* (;; these are the demarcation lines between the elevator space
                                                                                             ;; and the buttons
                                                                                             (v-div (+ box-window-y (- hei bottom)))
                                                                                             (h-div (+ box-window-x (- wid right))))
                                                                                        ;; now check for horizontal stuff, must be in the horizontal
                                                                                        ;; because it isn't in the vertical space and we can only come here
                                                                                        ;; as a result of a previous tracking returning :scroll-bar
                                                                                        (cond
                                                                                          ((< y v-div) :v-bar)
                                                                                          ((< x h-div) :h-bar))))))))

(defun maybe-move-point-after-scrolling (screen-box direction)
  (when (eq screen-box (point-screen-box))
    (let ((psr (point-screen-row)))
      (cond ((and (null psr) (eq direction :right))
             ;; can happen if the row has been scrolled out of sight...
             (move-point (screen-box-first-visible-bp-values screen-box)))
        ((null psr) ;; moving toward the beginning of the box...
                    (move-point (screen-box-last-visible-bp-values screen-box)))
        (t
         (multiple-value-bind (1st-cha last-cha)
                              (visible-cha-extents psr)
                              (cond ((null 1st-cha) ; the row *point* is on is not visible
                                     ;; need to find a row that is....
                                                    (if (eq direction :right)
                                                      (search-downward-for-visible-row psr)
                                                      (search-upward-for-visible-row psr)))
                                ((<= 1st-cha (point-cha-no) last-cha)) ; current *point* is visible...
                                ((eq direction :right)
                                 (set-bp-cha-no *point* (max 1st-cha (1- last-cha))))
                                ((eq direction :left)
                                 (set-bp-cha-no *point* (min (1+ 1st-cha) last-cha))))))))))

;; loop through the screen-row's chars returning the 1st and last visible char
;; repaint will have run so we can trust any cached dimensions
;; returns (possibly) 2 values or NIL if none of the row's chars are visible
(defmethod visible-cha-extents ((self screen-row))
  (let* ((sb (screen-box self))
         (remaining-offset (slot-value sb 'scroll-x-offset))
         (sbwid (screen-obj-wid sb)) ; should subtract border widths ?
         (1st-visible-cha nil))
    (with-font-hacking ((row-fds (screen-obj-actual-obj self)))
      (do-vector-contents (row-obj (slot-value self 'screen-chas) :index-var-name obj-no)
        (check-and-handle-font-changes obj-no)
        (incf remaining-offset (screen-object-width row-obj))
        (when (and (null 1st-visible-cha) (> remaining-offset 0))
          (setq 1st-visible-cha obj-no))
        (when (> remaining-offset sbwid)
          (return (values 1st-visible-cha obj-no)))))
    (cond ((null 1st-visible-cha) nil)
      (t (values 1st-visible-cha (max 1st-visible-cha (1- (screen-chas-length self))))))))

(defmethod search-downward-for-visible-row ((self screen-row) &optional (continue? t))
  (let* ((sb (screen-box self))
         (start-sr-no (1+ (screen-row-row-no sb self)))
         (moved? nil))
    (do* ((sr-no start-sr-no (1+ sr-no))
          (sr (screen-row-at-row-no sb sr-no) (screen-row-at-row-no sb sr-no)))
      ((null sr))
      (multiple-value-bind (1st last)
                           (visible-cha-extents sr)
                           (unless (null 1st)
                             (set-bp-row    *point* (screen-obj-actual-obj sr))
                             (set-bp-cha-no *point* (max 1st (1- last)))
                             (setq moved? t))))
    ;; if nothing has been moved, start looking in the other direction...
    (when (and (null moved?) (not (null continue?)))
      (search-upward-for-visible-row self nil))))

(defmethod search-upward-for-visible-row ((self screen-row) &optional (continue? t))
  (let* ((sb (screen-box self))
         (start-sr-no (1+ (screen-row-row-no sb self)))
         (moved? nil))
    (do ((sr-no start-sr-no (1- sr-no)))
      ((minusp sr-no))
      (let ((sr (screen-row-at-row-no sb sr-no)))
        (unless (null sr) ;; sgithens TODO adding this check to avoid crashing, but when would sr be nil?
          (multiple-value-bind (1st last)
                             (visible-cha-extents sr)
                             (unless (null 1st)
                               (set-bp-row    *point* (screen-obj-actual-obj sr))
                               (set-bp-cha-no *point* (min (1+ 1st) last))
                               (setq moved? t))))))
    ;; if nothing has been moved, start looking in the other direction...
    (when (and (null moved?) (not (null continue?)))
      (search-downward-for-visible-row self nil))))

(defun mouse-in-v-scroll-bar-internal (screen-box x y click-only?)
  ;; bind these so we dont have to calculate them for each iteration
  ;; of the tracking loop
  (let ((orig-scroll-y-offset (slot-value screen-box 'scroll-y-offset)))
    #+lispworks (boxer-window::with-mouse-tracking ((mouse-x x) (mouse-y y))
                (declare (ignore mouse-x))
                (let* ((diff (- y mouse-y))
                       (new-scroll (+ orig-scroll-y-offset diff)))
                  (when (> new-scroll 0)
                    (setf new-scroll 0))
                  (when (> (- new-scroll) (screen-obj-hei screen-box))
                    (setf new-scroll (- (screen-obj-hei screen-box))))
                  (setf (slot-value screen-box 'scroll-y-offset) new-scroll))
                (repaint t))))

(defun mouse-in-h-scroll-bar-internal (screen-box x y)
(let ((orig-scroll-x-offset (slot-value screen-box 'scroll-x-offset)))
    #+lispworks (boxer-window::with-mouse-tracking ((mouse-x x) (mouse-y y))
                (declare (ignore mouse-y))
                (let* ((diff (- x mouse-x))
                       (new-scroll (+ orig-scroll-x-offset diff)))
                  (when (> new-scroll 0)
                    (setf new-scroll 0))
                  (when (> (- new-scroll) (content-wid screen-box))
                    (setf new-scroll (- (content-wid screen-box))))
                  (setf (slot-value screen-box 'scroll-x-offset) new-scroll))
                (repaint t))))







