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

(in-package :boxer)

(defvar *drawing-devices* nil
  "Currently instantiated drawing-device implementation to use.")


(defvar *drawing-device* nil
  "Currently instantiated drawing-device implementation to use.")

(defclass drawing-device () ()
)

(defmethod add-cha ((device drawing-device) char x y)
)

(defmethod add-line ((device drawing-device) x0 y0 x1 y1)
)

(defmethod add-lines ((device drawing-device) &rest x-and-y-s)
)

(defmethod add-string ((device drawing-device) font-no string region-x region-y)
)

(defmethod swap-buffers ((device drawing-device))
)

(defclass multi-drawing-device (drawing-device) ())

(defmethod add-cha ((device multi-drawing-device) char x y)
  (dolist (dev *drawing-devices*)
    (add-cha dev char x y))
)

(defmethod add-line ((device multi-drawing-device) x0 y0 x1 y1)
  (dolist (dev *drawing-devices*)
    (add-line dev x0 y0 x1 y1))
)

(defmethod add-lines ((device multi-drawing-device) &rest x-and-y-s)
  (dolist (dev *drawing-devices*)
    (add-lines dev (car x-and-y-s)))
)

(defmethod add-string ((device multi-drawing-device) font-no string region-x region-y)
  (dolist (dev *drawing-devices*)
    (add-string dev font-no string region-x region-y))
)

(defmethod swap-buffers ((device multi-drawing-device))
  (dolist (dev *drawing-devices*)
    (swap-buffers dev))
)
