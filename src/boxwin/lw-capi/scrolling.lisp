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


(defun vertical-scroll-callback (interface sb how where)
  "For the OS outer scrollbars"
  (declare (ignorable interface))
  ;; sgithens TODO a bunch of this is cut and pasted from defmethod scroll-vertical below
  (format t "Vertical Scrolled ~a where ~a : ~a~%"
          how where (capi:range-slug-start sb))
  (let ((new-scroll-amount (- where)) ;(+ (vertical-scroll *boxer-pane*) (- scroll-amount)))
        (screen-box (outermost-screen-box))
        (pane-hei (gp:port-height *boxer-pane*)))
    (cond ((< (- new-scroll-amount) 0)
           (setf new-scroll-amount 0))
          ((> (- new-scroll-amount) (- (boxer::screen-obj-hei screen-box) pane-hei -20))
           (setf new-scroll-amount (- (- (boxer::screen-obj-hei screen-box) pane-hei -20))))
          (t nil))
    (setf (vertical-scroll *boxer-pane*) new-scroll-amount))
  (boxer::repaint))

(defun horizontal-scroll-callback (interface sb how where)
  "For the OS outer scrollbars"
  (declare (ignorable interface))
  ;; sgithens TODO a bunch of this is cut and pasted from defmethod scroll-horizontal below
  (format t "Horizontal Scrolled ~a where ~a : ~a~%"
          how where (capi:range-slug-start sb))
  (let ((new-scroll-amount (- where));;(+ (horizontal-scroll self) (- scroll-amount)))
        (screen-box (outermost-screen-box))
        (pane-wid (gp:port-width *boxer-pane*)))
    (cond ((< (- new-scroll-amount) 0)
           (setf new-scroll-amount 0))
          ((> (- new-scroll-amount) (- (boxer::screen-obj-wid screen-box) pane-wid -20))
           (setf new-scroll-amount (- (- (boxer::screen-obj-wid screen-box) pane-wid -20))))
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
  (ignore-errors (unless (not (null *suppress-expose-handler*))
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
       (boxer::repaint t))))))

(defmethod scroll-vertical ((self capi:output-pane) scroll-amount)
  (let ((new-scroll-amount (+ (vertical-scroll self) (- scroll-amount)))
        (screen-box (outermost-screen-box))
        (pane-hei (gp:port-height self)))
    (cond ((< (- new-scroll-amount) 0)
           (setf new-scroll-amount 0))
          ((> (- new-scroll-amount) (- (boxer::screen-obj-hei screen-box) pane-hei -20))
           (setf new-scroll-amount (- (- (boxer::screen-obj-hei screen-box) pane-hei -20))))
          (t nil))
    (setf (vertical-scroll self) new-scroll-amount)))

(defmethod scroll-horizontal ((self capi:output-pane) scroll-amount)
  (let ((new-scroll-amount (+ (horizontal-scroll self) (- scroll-amount)))
        (screen-box (outermost-screen-box))
        (pane-wid (gp:port-width self)))
    (cond ((< (- new-scroll-amount) 0)
           (setf new-scroll-amount 0))
          ((> (- new-scroll-amount) (- (boxer::screen-obj-wid screen-box) pane-wid -20))
           (setf new-scroll-amount (- (- (boxer::screen-obj-wid screen-box) pane-wid -20))))
          (t nil))
    (setf (horizontal-scroll self) new-scroll-amount)))
