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

(defun record-circular-port (port)
  (let ((entry (fast-assq port port-redisplay-history)))
    (if (null entry)
      (push (cons port 1) port-redisplay-history)
      (setf (cdr entry) (1+ (cdr entry))))))

(defmethod ensure-row-is-displayed ((self row) screen-box
                                              &optional (direction -1) scroll-anyway)
  ;; (repaint) ;; TODO we really only need to rerun the pass-1 layout algorithm
  (when (screen-objs (point-row))
    (let* ((point-row-y-offset (screen-obj-y-offset (car (screen-objs (point-row)))))
           (point-row-baseline-y-offset (- point-row-y-offset (baseline (car (screen-objs (point-row))))))
           (adjusted-row-y-offset (+ point-row-y-offset (slot-value screen-box 'scroll-y-offset)))
           (bp-coords (multiple-value-list (bp-coordinates *point*)))
           (bp-y-coord (cadr bp-coords)))

      ;; Eventually this may need to be properly recursive.
      ;; But for now, let's just check if we're typing in a scrolled box, and if not whether the top
      ;; level scrollbars are active.
      (cond
        ((and (v-scrollable? screen-box) (> adjusted-row-y-offset (inner-hei screen-box)))
        (setf (slot-value screen-box 'scroll-y-offset) (- (- point-row-y-offset (inner-hei screen-box)))))
        ((and (v-scrollable? screen-box) (< point-row-baseline-y-offset (- (slot-value screen-box 'scroll-y-offset))))
        (setf (slot-value screen-box 'scroll-y-offset) (- (- point-row-baseline-y-offset 4))))
        ((and (v-scrollable? *boxer-pane*)
              (> (+ bp-y-coord (vertical-scroll *boxer-pane*)) (viewport-height *boxer-pane*)))
         (setf (vertical-scroll *boxer-pane*) (- (+ bp-y-coord (vertical-scroll *boxer-pane*)))))
        (t
        nil)))))

(defmethod ensure-row-is-displayed ((row name-row) screen-box
                                                   &optional (direction -1) scroll-anyway)
  (declare (ignore screen-box direction scroll-anyway))
  nil)

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
  *lock-all-closets*)

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
                                                                 (let* (;; these are the demarcation lines between the elevator space
                                                                         ;; and the buttons
                                                                         (v-div (+ box-window-y (- hei bottom)))
                                                                         (h-div (+ box-window-x (- wid right))))
                                                                   ;; now check for horizontal stuff, must be in the horizontal
                                                                   ;; because it isn't in the vertical space and we can only come here
                                                                   ;; as a result of a previous tracking returning :scroll-bar
                                                                   (cond
                                                                     ((< y v-div) :v-bar)
                                                                     ((< x h-div) :h-bar)))))))



(defmethod inner-hei ((self screen-box))
  (with-slots (content-hei box-type hei) self
    (multiple-value-bind (il it ir ib) (box-borders-widths box-type self)
      (- hei it ib))))

(defmethod v-scroll-multiplier ((self screen-box) &key (inverse nil))
  "Find the multiplier needed to adjust the outer scroll distance to the internal height.
   The ratio of the content-hei / inner-hei. This is multiplied by the distance the mouse
   moves such that it stays in line with the scroll bar."
     (/ (content-hei self) (inner-hei self)))

(defmethod check-v-scroll-limits ((self screen-box) new-scroll)
  (let* ((scroll-multiplier (v-scroll-multiplier self))
         (slug-hei (* (/ (inner-hei self) (content-hei self)) (inner-hei self)))
         (adjusted-scroll (+ (* (- new-scroll) (/ 1 scroll-multiplier)) slug-hei))
         (max-inner-scroll (* (- (inner-hei self) slug-hei) scroll-multiplier)))
    (cond ((> new-scroll 0)
          0)
          ((> adjusted-scroll (inner-hei self))
           (- max-inner-scroll))
          (t new-scroll))))

(defun mouse-in-v-scroll-bar-internal (screen-box x y click-only?)
  "When the mouse clicks inside a manually sized boxes scrollbar, this method is
   called to track the mouse and scroll the box until the mouse is let up."
  (with-slots (content-hei hei) screen-box
    (let ((orig-scroll-y-offset (slot-value screen-box 'scroll-y-offset))
          (scroll-multiplier (v-scroll-multiplier screen-box)))
      #+lispworks
      (with-mouse-tracking ((mouse-x x) (mouse-y y))
        (declare (ignore mouse-x))
        (let* ((diff (- y mouse-y))
               (new-scroll (+ (* scroll-multiplier diff) orig-scroll-y-offset)))
          (setf (slot-value screen-box 'scroll-y-offset) (check-v-scroll-limits screen-box new-scroll)))
        (repaint t)))))

(defmethod scroll-vertical ((self screen-box) scroll-amount)
  (with-slots (scroll-y-offset) self
    (let ((new-scroll (+ scroll-y-offset (- scroll-amount))))
      (setf scroll-y-offset (check-v-scroll-limits self new-scroll)))))

(defmethod inner-wid ((self screen-box))
  (with-slots (content-wid box-type wid) self
    (multiple-value-bind (il it ir ib) (box-borders-widths box-type self)
      (- wid it ib))))

(defmethod h-scroll-multiplier ((self screen-box))
  "See v-scroll-multiplier for explanation, but to the horizontal scroll bar."
        (/ (content-wid self) (inner-wid self)))

(defmethod check-h-scroll-limits ((self screen-box) new-scroll)
  (let* ((scroll-multiplier (h-scroll-multiplier self))
         (slug-wid (* (/ (inner-wid self) (content-wid self)) (inner-wid self)))
         (adjusted-scroll (+ (* (- new-scroll) (/ 1 scroll-multiplier)) slug-wid))
         (max-inner-scroll (* (- (inner-wid self) slug-wid) scroll-multiplier)))
    (cond ((> new-scroll 0)
          0)
          ((> adjusted-scroll (inner-wid self))
           (- max-inner-scroll))
          (t new-scroll))))

(defun mouse-in-h-scroll-bar-internal (screen-box x y)
  (with-slots (content-wid wid) screen-box
  (let ((orig-scroll-x-offset (slot-value screen-box 'scroll-x-offset))
        (scroll-multiplier (h-scroll-multiplier screen-box)))
    #+lispworks
    (with-mouse-tracking ((mouse-x x) (mouse-y y))
      (declare (ignore mouse-y))
      (let* ((diff (- x mouse-x))
             (new-scroll (+ (* scroll-multiplier diff) orig-scroll-x-offset)))
        (setf (slot-value screen-box 'scroll-x-offset) (check-h-scroll-limits screen-box new-scroll)))
      (repaint t)))))

(defmethod scroll-horizontal ((self screen-box) scroll-amount)
  (with-slots (scroll-x-offset) self
    (let ((new-scroll (+ scroll-x-offset (- scroll-amount))))
      (setf scroll-x-offset (check-h-scroll-limits self new-scroll)))))

;;;
;;; Page Up/Down for Boxes with Scrollbars
;;;

(defmethod scroll-dn-one-screen-box ((self screen-box))
  (with-slots (scroll-y-offset y-got-clipped?) self
    (when y-got-clipped?
      (setf scroll-y-offset (check-v-scroll-limits self (- scroll-y-offset (inner-hei self) (- 20)))))))

(defmethod scroll-up-one-screen-box ((self screen-box))
  (with-slots (scroll-y-offset y-got-clipped?) self
    (when y-got-clipped?
      (setf scroll-y-offset (check-v-scroll-limits self (+ scroll-y-offset (inner-hei self)  20))))))

(defmethod scroll-dn-one-screen-box ((self graphics-screen-box))
  ;  (declare (ignore self))
  nil)

(defmethod scroll-up-one-screen-box ((self graphics-screen-box))
  ;  (declare (ignore self))
  nil)

