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

(defun draw-blinker (blinker)
  (with-pen-color (bw::*blinker-color*)
    (let ((horiz (horizontal-scroll *boxer-pane*))
          (vert  (vertical-scroll *boxer-pane*)))
      (draw-rectangle (blinker-wid blinker) (blinker-hei blinker)
                      (+ horiz (blinker-x blinker))
                      (+ vert (blinker-y blinker))))))
