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
;;;;
;;;;        This files contains the routines to paint the development/debug overlay on the boxer canvas with
;;;;        various bits of helpful information.
(in-package :boxer)

(defvar *show-dev-overlay* nil
  "Whether or not paint the development and debug overlay in the corner.")

(defun repaint-dev-overlay (&optional (process-state ""))
  "Debug information overlay on top of drawing canvas for things like framerate and zoom level"
  #+lispworks
  (if *show-dev-overlay*  ;; factor out lispworks specific graphics-ports calls
    (drawing-on-window (*boxer-pane*)
      (let* ((code-font (make-boxer-font '("Courier New" 14)))
            (maxish-width (string-wid code-font "Repaint: 1000.00ms/fr"))
            (line-height (string-hei code-font))
            (x (- (slot-value *boxer-pane* 'graphics-ports::width) (+ maxish-width 300)))
            (y (- (slot-value *boxer-pane* 'graphics-ports::height) 120))
            (mx bw::*track-mouse-x*)
            (my bw::*track-mouse-y*)
            (mouse-bp (boxer::mouse-position-values mx my))
            (mouse-screen-box (boxer::bp-screen-box mouse-bp))
            (mouse-actual-obj (if mouse-screen-box (slot-value mouse-screen-box 'boxer::actual-obj) nil))
            (mouse-style (if mouse-actual-obj (boxer::display-style-style (boxer::display-style-list mouse-actual-obj)) nil))
            (debug-num 0)
          )
      (with-pen-color (*blue*)
        (dolist (item (list (format nil "Repaint:   ~$ms/fr" *current-framerate*)
                            (format nil "Font Zoom: ~A%" (* *font-size-baseline* 100))
                            (format nil "Real Zoom: ~A%" (zoom-level *boxer-pane*))
                            (format nil "Process:   ~A" process-state)
                            (format nil "mx: ~A my: ~A d: ~A" bw::*track-mouse-x* bw::*track-mouse-y* bw::*mouse-down-p*)
                            (format nil "dx: ~A dy: ~A" bw::*document-mouse-x* bw::*document-mouse-y*)
                            (format nil "mouse status: ~A" (bw::mouse-doc-status-place))
                            (format nil "mouse x: ~A" (bw::mouse-doc-status-x))
                            (format nil "mouse y: ~A" (bw::mouse-doc-status-y))
                            (format nil "popup-doc: ~A" (bw::mouse-doc-status-popup-doc))
                            (format nil "popup-x: ~A" (bw::mouse-doc-status-popup-x))
                            (format nil "popup-y: ~A" (bw::mouse-doc-status-popup-y))
                            (format nil "mouse-style: ~A" mouse-style)
                            ;; (format nil "name: ~A" (name mouse-actual-obj))
                            ;; (format nil "x-offset: ~A" (screen-obj-x-offset mouse-screen-box))
                            ;; (format nil "y-offset: ~A" (screen-obj-y-offset mouse-screen-box))
                            ;; (format nil "wid: ~A" (screen-obj-wid mouse-screen-box))
                            ;; (format nil "hei: ~A" (screen-obj-hei mouse-screen-box))
                            ;; (format nil "x-got-clipped?: ~A" (screen-obj-x-got-clipped? mouse-screen-box))
                            ;; (format nil "y-got-clipped?: ~A" (screen-obj-y-got-clipped? mouse-screen-box))
                            ))
          (draw-string code-font item x (+ 30 (* line-height debug-num)))
          (setf debug-num (1+ debug-num))
        )
        )))))
