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

;;; 8 Left String


;;; 9 Right String


;;; 10 Centered Rectangle

;;; 11 Dot

;;; 12 Hollow Rectangle


;;; 15 Centered Bitmap

;;; 26 Wedge

;;; 27 Arc

;;; 28 Filled Ellipse

;;; 29 Ellipse

;;; 30 Filled Circle

;;; 31 Circle
