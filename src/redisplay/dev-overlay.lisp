;;;;    Boxer
;;;;    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
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
;;;;
;;;;        This files contains the routines to paint the development/debug overlay on the boxer canvas with
;;;;        various bits of helpful information.
(in-package :boxer)

(defvar *show-dev-overlay* nil
  "Whether or not paint the development and debug overlay in the corner.")

(defun repaint-dev-overlay (&optional (process-state ""))
  "Debug information overlay on top of drawing canvas for things like framerate and zoom level"
  (if *show-dev-overlay*
    (drawing-on-window (*boxer-pane*)
      (let* ((code-font (make-boxer-font '("Courier New" 14)))
            (maxish-width (string-wid code-font "Repaint: 1000.00ms/fr"))
            (line-height (string-hei code-font))
            (x (- (slot-value *boxer-pane* 'graphics-ports::width) (+ maxish-width 40)))
            (y (- (slot-value *boxer-pane* 'graphics-ports::height) 120)))
      (with-pen-color (*blue*)
        (draw-string alu-seta code-font (format nil "Repaint:   ~$ms/fr" *current-framerate*) x 30)
        (draw-string alu-seta code-font (format nil "Font Zoom: ~A%" (* *font-size-baseline* 100)) x (+ 30 line-height))
        (draw-string alu-seta code-font (format nil "Process:   ~A" process-state) x (+ 30 (* 2 line-height)))
        (draw-string alu-seta code-font (format nil "x: ~A y: ~A d: ~A" bw::*track-mouse-x* bw::*track-mouse-y* bw::*mouse-down-p*) x (+ 30 (* 3 line-height))))))))
