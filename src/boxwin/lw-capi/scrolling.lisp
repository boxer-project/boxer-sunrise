;;;;        Boxer
;;;;        Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;        Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;        used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;        Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;        https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                             +-Data--+
;;;;                    This file is part of the | BOXER | system
;;;;                                             +-------+
;;;;
;;;;          Global scrollbars, trackpad, and other OS window scrolling support.
;;;;
(in-package :boxer-window)

(defun outer-scrollbars-padhack ()
  (* 20 (zoom-level *boxer-pane*)))

(defmethod max-vertical-scroll ((self boxer::boxer-canvas) screen-box)
  (let* ((zoom     (box::zoom-level self))
         (pane-hei (capi:simple-pane-visible-height self))
         (box-hei  (* zoom (box::screen-obj-hei screen-box))))
    (* (/ 1 zoom) (- box-hei pane-hei (- (outer-scrollbars-padhack))))))

(defmethod max-horizontal-scroll ((self boxer::boxer-canvas) screen-box)
  (let* ((zoom     (box::zoom-level self))
         (pane-wid (capi:simple-pane-visible-width self))
         (box-wid  (* zoom (box::screen-obj-wid screen-box))))
    (* (/ 1 zoom) (- box-wid pane-wid (- (outer-scrollbars-padhack))))))

(defun update-outer-scrollbars (screen-box frame)
  ;; A few things.
  ;; 1. If the outermost screen box fits on the entire canvas (either way) then that scroll-bar should
  ;;    not be enabled.
  ;; 2. After that we need to look the total size of the outermost screen-box and work our slug sizes to that
  (let* (
         (zoom             (box::zoom-level *boxer-pane*))
         (pane-hei         (capi:simple-pane-visible-height *boxer-pane*))  ;(slot-value *boxer-frame* 'bw::outer-vertical-scroll)))
         (box-hei          (* zoom (box::screen-obj-hei screen-box))) ;  (+ top bot (screen-obj-hei screen-box)))
         ;; The slug start and end are in the coordinate space of the box height (not the pane height)
         (max-vertical-scroll  (max-vertical-scroll *boxer-pane* screen-box)) ;(* (/ 1 zoom) (- box-hei pane-hei (- (boxer::outer-scrollbars-padhack)))))
         (vert-slug-size   (- box-hei max-vertical-scroll))
         (slug-start  (- (box::vertical-scroll *boxer-pane*)))
         (slug-end (+ vert-slug-size slug-start)))

    (capi:range-set-sizes (slot-value *boxer-frame* 'bw::outer-vertical-scroll)
                          :start 0
                          :end box-hei
                          ;; :end (+ 40 (* zoom box-hei))
                          :slug-start slug-start
                          :slug-end slug-end
                          :redisplay t))

  ;; Horizontal Scrolling
  (let* ((pane-wid         (capi:simple-pane-visible-width (slot-value *boxer-frame* 'bw::outer-horizontal-scroll)))
         (zoom             (zoom-level *boxer-pane*))
         (box-wid          (* zoom (box::screen-obj-wid screen-box)))
         (max-horizontal-scroll  (max-horizontal-scroll *boxer-pane* screen-box))
         (horiz-slug-size  (- box-wid max-horizontal-scroll))
         (slug-start (- (horizontal-scroll *boxer-pane*)))
         (slug-end (+ horiz-slug-size slug-start)))

    (capi:range-set-sizes (slot-value *boxer-frame* 'bw::outer-horizontal-scroll)
                          :start 0
                          :end box-wid
                          :slug-start slug-start
                          :slug-end slug-end
                          :redisplay t)))

(defun vertical-scroll-callback (interface sb how where)
  "For the OS outer scrollbars"
  (declare (ignorable interface))
  ;; sgithens TODO a bunch of this is cut and pasted from defmethod scroll-vertical below
  (let ((new-scroll-amount (- where)) ;(+ (vertical-scroll *boxer-pane*) (- scroll-amount)))
        (box-hei (* (boxer::zoom-level *boxer-pane*) (boxer::screen-obj-hei (outermost-screen-box))))
        (pane-hei (gp:port-height *boxer-pane*))
        (max-vertical-scroll  (max-vertical-scroll *boxer-pane* (outermost-screen-box))))
    (cond ((< (- new-scroll-amount) 0)
           (setf new-scroll-amount 0))
          ((> (- new-scroll-amount) max-vertical-scroll)
           (setf new-scroll-amount (- max-vertical-scroll)))
          (t nil))
    (setf (vertical-scroll *boxer-pane*) new-scroll-amount))
  (boxer::repaint))

(defun horizontal-scroll-callback (interface sb how where)
  "For the OS outer scrollbars"
  (declare (ignorable interface))
  ;; sgithens TODO a bunch of this is cut and pasted from defmethod scroll-horizontal below
  (let ((new-scroll-amount (- where));;(+ (horizontal-scroll self) (- scroll-amount)))
        (screen-box (outermost-screen-box))
        (pane-wid (gp:port-width *boxer-pane*))
        (max-horizontal-scroll  (max-horizontal-scroll *boxer-pane* (outermost-screen-box))))
    (cond ((< (- new-scroll-amount) 0)
           (setf new-scroll-amount 0))
          ((> (- new-scroll-amount) max-horizontal-scroll)
           (setf new-scroll-amount (- max-horizontal-scroll)))
          (t nil))
    (setf (horizontal-scroll *boxer-pane*) new-scroll-amount))
  (boxer::repaint))

(defun scroll-handler (output-pane direction scroll-operation scroll-amount-original &key interactive)
  "Scrolls the screen box that the mouse is in based on the direction of the scrolling gestures from
   a trackpad or mouse."
  ;; We need to do a bit more work to support gesture scrolling during evalution to prevent locking
  ;; up, but for now, checking this *suppress-expose-handler* let's us know if the system is currently
  ;; evaluating, and if it is, we'll just ignore scroll gestures for now.  The real problem here, is
  ;; that scroll gesture mechanisms are issuing repaints, and in order for this to not lock up the
  ;; evaluation polling repaints, they need to be queued, or just not done at all, letting the evaluator
  ;; polling repaint take care of it.
  (unless (not (null *suppress-expose-handler*))
    (multiple-value-bind (x y) (boxer-pane-mouse-position)
      (let* ((scroll-amount (* scroll-amount-original 4)) ;; TODO This should really be pulled from the OS trackpad/mouse scroll settings
            (mouse-bp (boxer::mouse-position-values x y))
            (mouse-screen-box (boxer::bp-screen-box mouse-bp))
            (mouse-actual-obj (slot-value mouse-screen-box 'boxer::actual-obj))
            (mouse-style (boxer::display-style-style (boxer::display-style-list mouse-actual-obj)))
            ;; If the mouse is over a supershrunk box we should scroll it's containing box.
            (screen-box-to-scroll (if (eq mouse-style :SUPERSHRUNK)
                                      (slot-value mouse-screen-box 'boxer::superior-screen-box)
                                      mouse-screen-box))
            (v-screen-box-to-scroll (do* ((cur-screen-box mouse-screen-box (slot-value cur-screen-box 'boxer::superior-screen-box))
                                          (cur-actual-obj mouse-actual-obj (slot-value cur-screen-box 'boxer::actual-obj))
                                          (cur-mouse-style mouse-style (boxer::display-style-style (boxer::display-style-list cur-actual-obj)))
                                          )
                                          ((cond ((equal cur-screen-box boxer::*outermost-screen-box*)
                                                  t)
                                                ((member cur-mouse-style '(:SHRUNK :SUPERSHRUNK :BOXTOP))
                                                  nil)
                                                ((boxer::v-scrollable? cur-screen-box)
                                                  t)
                                                (t nil))
                                          cur-screen-box)
                                      ))
            (h-screen-box-to-scroll (do* ((cur-screen-box mouse-screen-box (slot-value cur-screen-box 'boxer::superior-screen-box))
                                          (cur-actual-obj mouse-actual-obj (slot-value cur-screen-box 'boxer::actual-obj))
                                          (cur-mouse-style mouse-style (boxer::display-style-style (boxer::display-style-list cur-actual-obj)))
                                          )
                                          ((cond ((equal cur-screen-box boxer::*outermost-screen-box*)
                                                  t)
                                                ((member cur-mouse-style '(:SHRUNK :SUPERSHRUNK :BOXTOP))
                                                  nil)
                                                ((boxer::h-scrollable? cur-screen-box)
                                                  t)
                                                (t nil))
                                          cur-screen-box)
                                      ))
            (v-scrollable (boxer::v-scrollable? v-screen-box-to-scroll))
            (h-scrollable (boxer::h-scrollable? h-screen-box-to-scroll))
            )
        (cond ((and v-scrollable (equal direction :vertical))
               (boxer::scroll-vertical v-screen-box-to-scroll scroll-amount))
              ((and h-scrollable (equal direction :horizontal))
               (boxer::scroll-horizontal h-screen-box-to-scroll scroll-amount))

              ;; These are for the global scrollbars
              ((equal direction :horizontal)
               (scroll-horizontal output-pane scroll-amount))
              ((equal direction :vertical)
               (scroll-vertical output-pane scroll-amount))
              (t nil))
       (boxer::repaint t)))))

(defmethod scroll-vertical ((self capi:output-pane) scroll-amount)
  (let ((new-scroll-amount (+ (vertical-scroll self) (- scroll-amount)))
        (box-hei (* (boxer::zoom-level *boxer-pane*) (boxer::screen-obj-hei (outermost-screen-box))))
        (pane-hei (gp:port-height self))
        (max-vertical-scroll  (max-vertical-scroll *boxer-pane* (outermost-screen-box))))
    (cond ((< (- new-scroll-amount) 0)
           (setf new-scroll-amount 0))
          ((> (- new-scroll-amount) max-vertical-scroll)
           (setf new-scroll-amount (- max-vertical-scroll)))
          (t nil))
    (setf (vertical-scroll self) new-scroll-amount)))

(defmethod scroll-horizontal ((self capi:output-pane) scroll-amount)
  (let ((new-scroll-amount (+ (horizontal-scroll self) (- scroll-amount)))
        (screen-box (outermost-screen-box))
        (pane-wid (gp:port-width self))
        (max-horizontal-scroll  (max-horizontal-scroll *boxer-pane* (outermost-screen-box))))
    (cond ((< (- new-scroll-amount) 0)
           (setf new-scroll-amount 0))
          ((> (- new-scroll-amount) max-horizontal-scroll)
           (setf new-scroll-amount (- max-horizontal-scroll)))
          (t nil))
    (setf (horizontal-scroll self) new-scroll-amount)))
