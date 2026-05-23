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
;;;;   Prims for playing videos as well as seeking, pausing, adjusting the speed and other
;;;;   things. Part of the Boxer Godot frontend.
(in-package :boxer)

;;;
;;; Turtle Video Prims
;;;

(defsprite-function bu::open-video ((boxer-eval::dont-copy video-path))
  (sprite turtle)
  (let ((godot-obj (fetch-godot-obj turtle))
        (video-string (coerce (box-text-string video-path) 'string)))
    (godot-call godot-obj "boxer_open_video" video-string ))
  boxer-eval::*novalue*)

(defsprite-function bu::close-video ()
  (sprite turtle)
  (let ((godot-obj (fetch-godot-obj turtle)))
    ;; TODO
    )
  boxer-eval::*novalue*)

(defsprite-function bu::play-video ()
  (sprite turtle)
  (let ((godot-obj (fetch-godot-obj turtle)))
    (godot-call godot-obj "play"))
  boxer-eval::*novalue*)

(defsprite-function bu::loop-video ()
  (sprite turtle)
  (let ((godot-obj (fetch-godot-obj turtle)))
    (godot-call godot-obj "boxer_loop_video"))
  boxer-eval::*novalue*)

(defsprite-function bu::stop-video ()
  (sprite turtle)
  (let ((godot-obj (fetch-godot-obj turtle)))
    (godot-call godot-obj "boxer_stop_video"))
  boxer-eval::*novalue*)

(defsprite-function bu::pause-video ()
  (sprite turtle)
  (let ((godot-obj (fetch-godot-obj turtle)))
    (godot-call godot-obj "boxer_pause"))
  boxer-eval::*novalue*)

(defsprite-function bu::seek-video ((boxer-eval::numberize position))
  (sprite turtle)
  (let ((godot-obj (fetch-godot-obj turtle)))
    (godot-call godot-obj "boxer_seek" position))
  boxer-eval::*novalue*)

(defsprite-function bu::set-video-speed ((boxer-eval::numberize speed))
  (sprite turtle)
  (let ((godot-obj (fetch-godot-obj turtle)))
    (godot-call godot-obj "boxer_video_speed" speed))
  boxer-eval::*novalue*)

(defmethod set-video-length ((self turtle) length)
  (putprop self length :video-length))

(defsprite-function bu::get-video-length ()
  (sprite turtle)
  (getprop turtle :video-length))

(defmethod set-video-position ((self turtle) position)
  (putprop self position :video-position))

(defsprite-function bu::get-video-position ()
  (sprite turtle)
  (getprop turtle :video-position))
