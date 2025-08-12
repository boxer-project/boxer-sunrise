;;;;    Boxer
;;;;    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                         +-Data--+
;;;;                This file is part of the | BOXER | system
;;;;                                         +-------+
;;;;    Exported routine draw-wrap-line for wrapped turtle lines in their native float coordinates.
;;;;    Adapted from the previous version that worked on the window-system type integer coordinates.
;;;;
(in-package :boxer-wrap)

(declaim (inline wrap-y-coord-top wrap-y-coord-bottom wrap-x-coord-right wrap-x-coord-left
                 beyond-top? beyond-bottom? beyond-right? beyond-left?))

(defun point-in-array? (x y)
  (and (x-in-array? x)
       (y-in-array? y)))

(defun x-in-array? (x)
  (and (>= x (- %drawing-half-width)) (<= x %drawing-half-width)))

(defun y-in-array? (y)
  (and (>= y (- %drawing-half-height)) (<= y %drawing-half-height)))

(defun wrap-y-coord-top (y) (- y %drawing-height))
(defun wrap-y-coord-bottom (y) (+ y %drawing-height))
(defun wrap-x-coord-left (x) (+ x %drawing-width))
(defun wrap-x-coord-right (x) (- x %drawing-width))

(defun beyond-top? (y) (> y %drawing-half-height))
(defun beyond-bottom? (y) (<= y (- %drawing-half-height)))
(defun beyond-left? (x) (< x (- %drawing-half-width)))
(defun beyond-right? (x) (>= x %drawing-half-width))

;;; Point-slope form x/y intercepts

(defun top-x-intercept (x y slope)
  (if (or (null slope) (= 0 slope))
    x
    (- (- (/ (- y %drawing-half-height) slope) x))))

(defun bottom-x-intercept (x y slope)
  (if (or (null slope) (= 0 slope))
    x
    (- (- (/ (- y (- %drawing-half-height) 1) slope) x))))

(defun left-y-intercept (x y slope)
  (unless (null slope)
    (- (- (* slope (- x (- %drawing-half-width))) y))))

(defun right-y-intercept (x y slope)
  (unless (null slope)
    (- (- (* slope (- x %drawing-half-width 1)) y))))

(defun draw-line-yflip (x1 y1 x2 y2)
  (draw-line x1 (- y1) x2 (- y2)))

(defun draw-wrap-line (from-x from-y to-x to-y &optional (draw-func #'draw-line-yflip) )
  (let* ((delta-x (- to-x from-x))
         (delta-y (- to-y from-y))
         (islope (unless (zerop delta-y) (/ delta-x delta-y)))
         (slope (unless (zerop delta-x) (/ delta-y delta-x))))
      (flet ((line-right-then-continue (y-intercept)
               (funcall draw-func from-x from-y %drawing-half-width y-intercept)
               ;; now recurse
               (draw-wrap-line (- %drawing-half-width) y-intercept
                               (wrap-x-coord-right to-x) to-y draw-func))

             (line-top-then-continue (x-intercept)
               (funcall draw-func from-x from-y x-intercept %drawing-half-height)
               (draw-wrap-line x-intercept (- %drawing-half-height)
                               to-x (wrap-y-coord-top to-y) draw-func))

             (line-left-then-continue (y-intercept)
               (funcall draw-func from-x from-y
                                       (- %drawing-half-width) y-intercept)
               (draw-wrap-line %drawing-half-width y-intercept
                               (wrap-x-coord-left to-x) to-y draw-func))

             (line-bottom-then-continue (x-intercept)
               (funcall draw-func from-x from-y
                                       x-intercept (- %drawing-half-height))
               (draw-wrap-line x-intercept %drawing-half-height
                               to-x (wrap-y-coord-bottom to-y) draw-func))

             (break-line-left (y-intercept)
               (draw-wrap-line (wrap-x-coord-left from-x) from-y
                                %drawing-half-width y-intercept  draw-func)
               (draw-wrap-line (- %drawing-half-width) y-intercept to-x to-y draw-func))

             (break-line-top (x-intercept)
               (draw-wrap-line from-x (wrap-y-coord-top from-y)
                               x-intercept (- %drawing-half-height)  draw-func)
               (draw-wrap-line x-intercept %drawing-half-height to-x to-y draw-func))

             (break-line-right (y-intercept)
               (draw-wrap-line (wrap-x-coord-right from-x) from-y
                               (- %drawing-half-width) y-intercept  draw-func)
               (draw-wrap-line %drawing-half-width y-intercept to-x to-y draw-func))

             (break-line-bottom (x-intercept)
               (draw-wrap-line from-x (wrap-y-coord-bottom from-y)
                               x-intercept %drawing-half-height  draw-func)
               (draw-wrap-line x-intercept (- %drawing-half-height)
                               to-x to-y  draw-func)))
        (cond ((point-in-array? from-x from-y)
               ;; check for the simple cases instead of falling
               ;; to optimize the common case
               (cond ((point-in-array? to-x to-y)
                      ;; the simple, simple case
                      (funcall draw-func from-x from-y to-x to-y))
                     ((beyond-right? to-x)
                      ;; note that if the line extends beyond a
                      ;; horizontal boundary, it can't be vertical
                      (let ((y-intercept (right-y-intercept from-x from-y slope)))
                        (cond ((y-in-array? y-intercept)
                               ;; we are sure it intersects a vertical edge
                               (line-right-then-continue y-intercept))
                              ((beyond-top? to-y) ; y-intercept ?
                               ;; must intersect with the top edge instead
                               (line-top-then-continue
                                (top-x-intercept from-x from-y slope)))
                              (t
                               ;; must intersect with the bottom edge
                               (line-bottom-then-continue
                                (bottom-x-intercept from-x from-y slope))))))
                     ((beyond-left? to-x)
                      ;; if it's not inside, or beyond the right edge,
                      ;; it must be beyond the left edge
                      (let ((y-intercept (left-y-intercept from-x from-y slope)))
                        (cond ((y-in-array? y-intercept)
                               ;; we are sure it intersects a vertical edge
                               (line-left-then-continue y-intercept))
                              ((beyond-top? to-y) ; y-intercept ?
                               ;; must intersect with the top edge instead
                               (line-top-then-continue
                                (top-x-intercept from-x from-y slope)))
                              (t
                               ;; must intersect with the bottom edge
                               (line-bottom-then-continue
                                (bottom-x-intercept from-x from-y slope))))))
                     ((beyond-top? to-y)
                      (line-top-then-continue (top-x-intercept from-x from-y slope)))
                     (t
                      (line-bottom-then-continue (bottom-x-intercept from-x
                                                                     from-y slope)))))
              ((beyond-right? from-x)
               (let ((right-y-intercept (right-y-intercept to-x to-y slope)))
                 (cond ((and (not (beyond-right? to-x))
                             (y-in-array? right-y-intercept))
                        ;; break the line on the right edge
                        (break-line-right right-y-intercept))
                       (t;; otherwise wrap, and try again...
                        (draw-wrap-line (wrap-x-coord-right from-x) from-y
                                        (wrap-x-coord-right to-x)   to-y draw-func)))))
              ((beyond-left? from-x)
               (let ((left-y-intercept (left-y-intercept to-x to-y slope)))
                 (cond ((and (not (beyond-left? to-x))
                             (y-in-array? left-y-intercept))
                        ;; break the line on the right edge
                        (break-line-left left-y-intercept))
                       (t;; just wrap both coords and try again
                        (draw-wrap-line (wrap-x-coord-left from-x) from-y
                                        (wrap-x-coord-left to-x)   to-y draw-func)))))
              ((beyond-top? from-y)
               (let ((top-x-intercept (top-x-intercept to-x to-y slope)))
                 (cond ((and (not (beyond-top? to-y))
                             (x-in-array? top-x-intercept))
                        (break-line-top top-x-intercept))
                       (t
                        (draw-wrap-line from-x (wrap-y-coord-top from-y)
                                        to-x   (wrap-y-coord-top to-y)    draw-func)))))
              (t;; from-y must be beyond the bottom line
               (let ((bottom-x-intercept (bottom-x-intercept to-x to-y slope)))
                 (cond ((and (not (beyond-bottom? to-y))
                             (x-in-array? bottom-x-intercept))
                        (break-line-bottom bottom-x-intercept))
                       (t
                        (draw-wrap-line from-x (wrap-y-coord-bottom from-y)
                                        to-x   (wrap-y-coord-bottom to-y) draw-func)))))))))
