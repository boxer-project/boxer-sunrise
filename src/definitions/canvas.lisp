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
;;;;         This file contains classes and structures to store the data needed for each boxer canvas, which could be
;;;;         on a separate window or tab (or in a separate div in a webGL future).  Historically a lot of these pieces
;;;;         of structure where in global variables, making it difficult to separate out to support multiple running
;;;;         Boxer canvases.
;;;;
(in-package :boxer)

(defstruct (blinker (:conc-name blinker-))
  (x 0)
  (y 0)
  (wid 0)
  (hei 0))

(defmethod update-blinker((self blinker) x y wid hei)
  (setf (blinker-x self) x (blinker-y self) y (blinker-wid self) wid (blinker-hei self) hei))

(defstruct (region-row-blinker (:include blinker))
  "Region Row Blinkers. For selected regions in the document. The uid will typically be set to
  the screen-row the region is highlighting."
  (uid nil))

(defclass boxer-canvas
  ()
  ((point-blinker :accessor point-blinker
    :documentation "Contains a `blinker` struct for the current location of the cursor.")))
