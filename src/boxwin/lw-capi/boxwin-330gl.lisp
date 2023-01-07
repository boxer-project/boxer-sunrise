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
;;;;
;;;;                                           +-Data--+
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;
;;;;  CAPI interface and demo harness for OpenGL 330 Shaders use
(in-package :boxer)

(defclass boxwin-330gl-device ()
  ((draw-device)
   (frame)
   (pane))
)

(defun test-draw-line-2 (device next)
  (gl-add-line (slot-value device 'draw-device) 550.0 150.0 next next)
)

(defun test-draw-line (gl-pane device next)
  (opengl:rendering-on (gl-pane)
    (test-draw-line-2 device next)
  )
)

(capi:define-interface boxgl-frame (capi:interface)
  ()
  (:panes
    (gl-pane opengl::opengl-pane
        :configuration '(:rgba t :depth nil :double-buffered t :modern t)
        :visible-min-width 600
        :visible-min-height 600)
  )
  (:layouts
   (default-layout capi:column-layout
                   '(gl-pane)))
  (:default-initargs
   :title "Boxer GL330"
  )
)

(defun make-demo-300 ()
  (let* ((gl-frame (make-instance 'boxgl-frame))
         (gl-pane  (slot-value gl-frame 'gl-pane))
         (device (make-boxwin-330gl-device gl-frame gl-pane))
         (next 0.0))
    (opengl:swap-buffers gl-pane)
    (dolist (i '(0.0 .1 .2 .3 .4 .5 .6 .7 .8 .9 1.0))
      (setf next (* 300.0 i))
      (opengl:rendering-on (gl-pane)

        (gl:clear-color 0.8 0.9 0.9 1.0)
        (gl:clear :color-buffer-bit :depth-buffer-bit)

        (gl-add-line (slot-value device 'draw-device) 550.0 550.0 next next)
        (gl-add-char (slot-value device 'draw-device) 100 100 #\g)
        (gl-add-char (slot-value device 'draw-device) 300 300 #\G)

        (gl-add-string (slot-value device 'draw-device) nil "Boxer Sunrise - sgithens" 100 200)
        (gl-add-line (slot-value device 'draw-device) 90 200 500 200)

        (gl-add-rect (slot-value device 'draw-device) 250 100 10 50)

        (gl-add-circle (slot-value device 'draw-device) 75 300 30 t)
        (gl-add-circle (slot-value device 'draw-device) 150 300 30 nil)

      )
      (opengl:rendering-on (gl-pane)
        (gl-add-line (slot-value device 'draw-device) 550.0 50.0 next next)
      )
      (test-draw-line gl-pane device next)
      (opengl:swap-buffers gl-pane)

      (format t "~%Doing next: ~A process: ~A" next mp:*current-process*)
      (print next)
      (sleep 1))
    device))

(defun make-boxwin-330gl-device (gl-frame gl-pane &key (wid 600) (hei 600))
  (let* ((gl-device nil)
         (device (make-instance 'boxwin-330gl-device)))
    (capi:display gl-frame)
    (opengl:rendering-on (gl-pane)
      (setf gl-device (make-boxgl-device wid hei))
      (setf (slot-value device 'frame) gl-frame)
      (setf (slot-value device 'pane) gl-pane)
      (setf (slot-value device 'draw-device) gl-device)
    )
    device))
