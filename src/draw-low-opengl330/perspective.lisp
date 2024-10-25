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
(in-package :boxer)

(defun create-perspective-matrix (wid hei)
  "Create an orthogonal projection matrix for use in our shaders with the given width and height."
  (3d-matrices:mperspective 45.0 (/ wid hei) 0.1 3000.0))

(defun rotations (view-matrix)
  (let ((x-rot 0)
        (y-rot 0)
        (z-rot 0))
    (setf  *box-depth-mult* 1)
    (sleep 1) (next-paint view-matrix x-rot y-rot z-rot)
    (loop for i
          from 0 to 100
          do (progn
              (decf z-rot 0.02)
              (incf x-rot 0.01)
              (incf *box-depth-mult* 0.12))
          do (sleep 0.01) do (next-paint view-matrix x-rot y-rot z-rot))
    (loop for i
          from 0 to 428
          do (decf z-rot 0.02)
          do (sleep 0.01) do (next-paint view-matrix x-rot y-rot z-rot))
    (loop for i
          from 0 to 95
          do (progn
              (decf z-rot 0.02)
              (decf x-rot 0.01)
              (decf *box-depth-mult* 0.12))
          do (sleep 0.01) do (next-paint view-matrix x-rot y-rot z-rot))
    (setf *box-depth-mult* 1)))

(defun next-paint (view-matrix x-rot y-rot z-rot)
  (let ((view-matrix (3d-matrices:meye 4))
        (flip (3d-matrices:meye 4)))

    (setf (aref (3d-matrices:marr4 flip) 5) -1.0)
    (3d-matrices:nmtranslate view-matrix (3d-vectors:vec 0 0 -1900))
    (setf view-matrix (3d-matrices:m* view-matrix flip))

    (3d-matrices:nmrotate view-matrix 3d-vectors:+vx+ x-rot)
    (3d-matrices:nmrotate view-matrix 3d-vectors:+vy+ y-rot)
    (3d-matrices:nmrotate view-matrix 3d-vectors:+vz+ z-rot)

    (setf (boxer::boxgl-device-transform-matrix bw::*boxgl-device*)
          view-matrix))
    (update-matrices-ubo bw::*boxgl-device*)
    (repaint-window))

(defun rotation-demo ()
  ;; (bw::queue-event 'box::rotation-demo)
  #+lispworks (opengl:rendering-on (*boxer-pane*)
   (let* ((prev-proj (boxgl-device-projection-matrix bw::*boxgl-device*))
          (prev-trans (boxgl-device-transform-matrix bw::*boxgl-device*))
          (view-matrix (3d-matrices:meye 4))
          (flip (3d-matrices:meye 4)))

    (setf (aref (3d-matrices:marr4 flip) 5) -1.0)

    (3d-matrices:nmtranslate view-matrix (3d-vectors:vec 0 0 -1900))
    (setf view-matrix (3d-matrices:m* view-matrix flip))

    (setf (boxgl-device-projection-matrix bw::*boxgl-device*)
          (create-perspective-matrix (viewport-width *boxer-pane*) (viewport-height *boxer-pane*))

          (boxgl-device-transform-matrix bw::*boxgl-device*)
          view-matrix)

    (update-matrices-ubo bw::*boxgl-device*)
    (rotations view-matrix)

    (setf (boxgl-device-projection-matrix bw::*boxgl-device*) prev-proj
          (boxgl-device-transform-matrix bw::*boxgl-device*) prev-trans)
    (update-matrices-ubo bw::*boxgl-device*))))
