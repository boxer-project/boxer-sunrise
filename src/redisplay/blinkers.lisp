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
;;;;         Code for painting the cursor/blinker.
(in-package :boxer)

;; timing
(defvar delta-time 0.0) ;; time between current frame and last frame
(defvar last-frame 0.0)
(defvar blinker-time 0.0)
(defvar blinker-on t
  "Is the blinker cursor currently visible?")

(defun toggle-blinker ()
  (cond
    (blinker-on
      (setf boxer::*point-color* #(:rgb .3 .3 .9 .5))
      (setf blinker-on nil)
    )
    (t
      (setf boxer::*point-color* #(:rgb .3 .3 .9 .0))
      (setf blinker-on t))))

(defun draw-blinker (blinker &key (color *blinker-color*))
  (with-pen-color (color)
      (draw-rectangle (blinker-wid blinker) (blinker-hei blinker)
                      (blinker-x blinker)   (blinker-y blinker))))
