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
;;;;     Boxer Text REPL
;;;;     Primarily used for testing separeation of the evaluation engine and the graphics layer
(in-package :boxer-window)

(defclass text-repl-boxer-pane (boxer::boxer-canvas)
  ())

(defmethod boxer::viewport-width ((self text-repl-boxer-pane))
  100)

(defmethod boxer::viewport-height ((self text-repl-boxer-pane))
  100)

(defclass test-repl-boxer-name-pane ()
  ())

(defun window-system-specific-make-boxer ()
  (setf *boxer-pane* (make-instance 'text-repl-boxer-pane) ;(slot-value *boxer-frame* 'boxer-pane)
        *name-pane*  (make-instance 'test-repl-boxer-name-pane)  ;(slot-value *boxer-frame* 'name-pane)
        (boxer::point-blinker *boxer-pane*) (boxer::make-blinker))

  (boxer::initialize-fonts)
  (progn ;; moved here because FD's need init'd colors
         (setq boxer::*default-font-descriptor* (boxer::make-bfd -1 boxer::*default-font*)
               boxer::*current-font-descriptor* (boxer::make-bfd -1 boxer::*default-font*))))

(defun window-system-specific-start-boxer ()
  (setq boxer-eval::*current-process* nil)
  (setup-editor boxer::*initial-box*)
  (boxer-eval::setup-evaluator)
  (unless boxer::*boxer-version-info*
    (setq boxer::*boxer-version-info*
          (format nil "~:(~A~) Boxer" (machine-instance))))

  ;;; START contents of boxer-process-top-level-fn
  (boxer::enter (boxer::point-box)))

(defun start-text-repl-boxer (project-dir)
  (format t "~%Preparing Hello text repl boxer5!")
  (window-system-specific-make-boxer)
  (window-system-specific-start-boxer)
  (setf bw::*boxgl-device* (make-instance 'boxgl-device))
      (setf
          (boxgl-device-projection-matrix bw::*boxgl-device*)
          (create-ortho-matrix 800 600)

          (boxgl-device-transform-matrix bw::*boxgl-device*)
          (create-transform-matrix 0 0)

          (boxgl-device-pen-color bw::*boxgl-device*)
          #(:rgb 0.0 0.0 0.0 1.0)

          (boxgl-device-pen-size bw::*boxgl-device*)
          1)

  (format t "~%Initial box: ~%~A" (boxer::textify-thing boxer::*initial-box*))
  (boxer::repaint-guts)
  (handle-boxer-input #\B)
  (handle-boxer-input #\o)
  (handle-boxer-input #\x)
  (handle-boxer-input #\R)
  (handle-boxer-input #\E)
  (handle-boxer-input #\P)
  (handle-boxer-input #\L)
  (format t "~%Initial box: ~%~A" (boxer::textify-thing boxer::*initial-box*))
  (boxer::repaint-guts))
