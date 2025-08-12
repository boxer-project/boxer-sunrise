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
;;;;         Platform independent GL Model
;;;;

(in-package :boxer)

(defclass boxer-gl-model ()
 ((cur-tick     :initform 0 :accessor cur-tick
    :documentation "A value to store and compare against in the future to see if the structure
                   this model draws has changed, and needs to be rebuffered.
                   This doesn't necessarily have to be an integer, but it should a string
                   or some lisp structure that can be compared using `equal`.")
  (needs-update :initform t :accessor needs-update)))

(defmethod draw ((self boxer-gl-model))
  nil)

(defmethod reset-meshes ((self boxer-gl-model))
  nil)
