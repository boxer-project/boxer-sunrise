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
;;;;     Embedded Boxer
;;;;     Primarily used for testing separeation of the evaluation engine and the graphics layer
(in-package :boxer-window)

(defun window-system-dependent-redraw-status-line (string)
  nil)

(defun beep ()
  nil)

(defclass embedded-boxer-pane (boxer::boxer-canvas)
  ())

(defmethod boxer::viewport-width ((self embedded-boxer-pane))
  100)

(defmethod boxer::viewport-height ((self embedded-boxer-pane))
  100)

(defclass embedded-boxer-name-pane ()
  ())

(defun window-system-specific-make-boxer ()
  (setf *boxer-pane* (make-instance 'embedded-boxer-pane)
        *name-pane*  (make-instance 'embedded-boxer-name-pane)
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

(defun print-world-as-text (&optional (prompt ""))
  (format t "~%~A Initial box: ~%~A" prompt (boxer::textify-thing boxer::*initial-box*)))

(defun type-stuff (stuff)
  (loop for char across stuff
    do (handle-boxer-input char)))

(defun start-embedded-boxer (project-dir)
  (format t "~%Preparing Embedded Boxer!")

  (window-system-specific-make-boxer)
  (window-system-specific-start-boxer)

  (print-world-as-text "1.")

  (type-stuff "2 + 3 + 7.1")
  (print-world-as-text "2.")

  (boxer::doit-internal)
  (print-world-as-text "3.2")

  (handle-boxer-input #\Return)
  (type-stuff "boxer-version")
  (print-world-as-text)

  (boxer::doit-internal)
  (print-world-as-text))
