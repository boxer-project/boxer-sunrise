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
;;;;        All of the graphics display list command declarations using defstandard-graphics-handlers.
;;;;
(in-package :boxer)

;;; 0 Change ALU

(defgraphics-state-change (change-alu 0) (new-alu)
  :body
  (unless (=& new-alu *graphics-state-current-alu*)
    (setq *graphics-state-current-alu* new-alu)))

;;; 1 Change Pen Width

(defgraphics-state-change (change-pen-width 1) (new-width)
  :body
  (unless (=& new-width *graphics-state-current-pen-width*)
    (setq *graphics-state-current-pen-width* new-width)
    (%set-pen-size new-width)))

;;; 2 Change Graphics Font

(defgraphics-state-change (change-graphics-font 2) (new-font-no)
  :body
  (unless (=& new-font-no *graphics-state-current-font-no*)
    ;; have to check for possible font
    (setq *graphics-state-current-font-no* new-font-no)))

;;; 3 Line Segment

(defstandard-graphics-handlers (line-segment 3)
  :COMMAND-ARGS (x0 y0 x1 y1)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :x-transform :y-transform))

;;; 4 Change Graphics Color

(defgraphics-state-change (change-graphics-color 4) (new-color)
             :body
             (unless (color= new-color *graphics-state-current-pen-color*)
               (setq *graphics-state-current-pen-color* new-color)
               (%set-pen-color new-color)))

(defgraphics-state-change (transform-matrix 5) (matrix)
             :body
             nil)

;;; 7 Centered String

(defstandard-graphics-handlers (centered-string 7)
  :COMMAND-ARGS (x y string)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil))

;;; 8 Left String

(defstandard-graphics-handlers (left-string 8)
  :COMMAND-ARGS (x y string)
  ; :EXTENTS-FORM
  ; (let ((height 0) (s string) (width 0))
  ;   (loop
  ;     (setq height (+ height 1(string-hei *graphics-state-current-font-no*))
  ;           width (max (string-wid *graphics-state-current-font-no*
  ;                                 (subseq s 0 (position #\newline s)))
  ;                     width))
  ;     ;; If we have handled the last line (the current line has no CR's)
  ;     (if (not (position #\newline s))
  ;       (return (values x y (+ x width) (+ y height)))
  ;       (setq s (subseq s (let ((p (position #\newline s)))
  ;                           (if (null p) 0 (1+& p))))))))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil))

;;; 9 Right String

(defstandard-graphics-handlers (right-string 9)
  :COMMAND-ARGS (x y string)
  ; :EXTENTS-FORM
  ; (let ((height 0) (s string) (width 0))
  ;   (loop
  ;     (setq height (+ height 1 (string-hei *graphics-state-current-font-no*))
  ;           width (max (string-wid *graphics-state-current-font-no*
  ;                                 (subseq s 0 (position #\newline s)))
  ;                     width))
  ;     ;; If we have handled the last line (the current line has no CR's)
  ;     (if (not (position #\newline s))
  ;       (return (values (- x width) y x (+ y height)))
  ;       (setq s (subseq s (let ((p (position #\newline s)))
  ;                           (if (null p) 0 (1+& p))))))))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil))

;;; 10 Centered Rectangle

(defstandard-graphics-handlers (centered-rectangle 10)
  :COMMAND-ARGS (x y width height)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce))

;;; 11 Dot

;; maybe this should be a circle ? Need to check relative speeds
;; also, should probably use the pen-width ?
(defstandard-graphics-handlers (dot 11)
  :COMMAND-ARGS (x y)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform))

;;; 12 Hollow Rectangle

(defstandard-graphics-handlers (hollow-rectangle 12)
  :COMMAND-ARGS (x y width height)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce))

;;; 15 Centered Bitmap

(defstandard-graphics-handlers (centered-bitmap 15)
  :COMMAND-ARGS (bitmap x y width height)
  :TRANSFORMATION-TEMPLATE
  (nil :x-transform :y-transform :coerce :coerce))

;;; 26 Wedge

(defstandard-graphics-handlers (wedge 26)
  :COMMAND-ARGS (x y radius start-angle sweep-angle)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil nil nil))

;;; 27 Arc

(defstandard-graphics-handlers (arc 27)
  :COMMAND-ARGS (x y radius start-angle sweep-angle)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil nil nil))

;;; 28 Filled Ellipse

(defstandard-graphics-handlers (filled-ellipse 28)
  :COMMAND-ARGS (x y width height)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce))


;;; 29 Ellipse

(defstandard-graphics-handlers (ellipse 29)
  :COMMAND-ARGS (x y width height)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce))

;;; 30 Filled Circle

(defstandard-graphics-handlers (filled-circle 30)
  :COMMAND-ARGS (x y radius)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil))

;;; 31 Circle

(defstandard-graphics-handlers (circle 31)
  :COMMAND-ARGS (x y radius)
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil))
