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
;;;;                                               +-Data--+
;;;;                      This file is part of the | BOXER | system
;;;;                                               +-------+
;;;;
;;;;         Methods to draw scroll bars on manually/fixed size boxes

(in-package :boxer)

;;; scroll bar support
;;; scroll bars are displayed if:
;;; {X,Y}-got-clipped?, or the scroll-{x,y}-offset is non zero, or scroll-to-actual-row is non null
;;;
(defmethod draw-scroll-info ((self screen-box))
  (with-slots (actual-obj wid hei box-type scroll-to-actual-row content-hei content-wid
                          scroll-x-offset scroll-y-offset)
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
                                                      (if (= (- content-hei inner-hei) 0)
                                                        0
                                                        (/ scroll-y-offset (- content-hei inner-hei)))))
                           (when (h-scrollable? self)
                             ;; ok, need to draw horizontal scroll GUI
                             (draw-horizontal-scrollbar (+ type-label-width il) (- hei ib)
                                                        inner-wid (* (/ inner-wid content-wid) inner-wid)
                                                        ;; percent scrolled
                                                        (if (= (- content-wid inner-wid) 0)
                                                          0
                                                          (/ scroll-x-offset (- content-wid inner-wid)))))))))

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
