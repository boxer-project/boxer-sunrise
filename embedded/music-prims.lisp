;;;; -*- Mode:lisp;Syntax:Common-Lisp; Package:BOXER; Base:10.-*-
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
;;;;                                            +-Data--+
;;;;                   This file is part of the | BOXER | system
;;;;                                            +-------+
;;;;
;;;;   Prims for playing music notes and sound files.  Currently part of the Godot frontend, and using
;;;;   the SimpleSampler Godot AddOn.
(in-package :boxer)

(boxer-eval::defboxer-primitive bu::play-note ((boxer-eval::dont-copy note))
  (let ((note-str (coerce (string-upcase (box-text-string note)) 'string)))
    (format t "~%Play Boxer Note: ~A ~A" (type-of note-str) note-str)
    (godot-call-main "play_note" note-str))
  boxer-eval::*novalue*)
