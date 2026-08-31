;;;;  ;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
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
;;;;                                        +-Data--+
;;;;               This file is part of the | BOXER | system
;;;;                                        +-------+
;;;;
;;;;    Video prims for macOS utilitizing quicktime as an external player
;;;;
(in-package :boxer)

(defvar *quicktime-looping* nil
 "Whether or not quicktime is currently set to looping.")

(boxer-eval::defboxer-primitive bu::open-video ((boxer-eval::dont-copy video-path))
  (let ((video-string (coerce (box-text-string video-path) 'string)))
    ;; osascript -e "tell application \"QuickTime Player\" to open POSIX file /Users/sgithens/Movies/Eroica.mov"
    (setf *quicktime-looping* nil)
    (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to open POSIX file \"~A\"" video-string)))
    (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to set looping of document 1 to false"))))
  boxer-eval::*novalue*)

 (boxer-eval::defboxer-primitive bu::close-video ()
  ;; osascript -e 'tell application "QuickTime Player" to close document 1'
  (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to close document 1")))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::play-video ()
  ;; osascript -e 'tell application "QuickTime Player" to play document 1'
  (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to play document 1")))
  boxer-eval::*novalue*)


(boxer-eval::defboxer-primitive bu::loop-video ()
  (setf *quicktime-looping* (not *quicktime-looping*))
  (if *quicktime-looping*
    (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to set looping of document 1 to true")))
    (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to set looping of document 1 to false"))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::stop-video ()
  ;; pause and seek to 0
  (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to pause document 1")))
  (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to set current time of document 1 to 0")))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::pause-video ()
  ;; osascript -e 'tell application "QuickTime Player" to pause document 1'
  (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to pause document 1")))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::seek-video ((boxer-eval::numberize position))
  ;; osascript -e 'tell application "QuickTime Player" to set current time of document 1 to 500'
  (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to set current time of document 1 to ~A" position)))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::set-video-speed ((boxer-eval::numberize speed))
  ;; osascript -e 'tell application "QuickTime Player" to set rate of document 1 to 2.0'
  (external-program:run "osascript" (list "-e" (format nil "tell application \"QuickTime Player\" to set rate of document 1 to ~A" speed)))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::get-video-length ()
  (let ((proc nil))
    (setf proc
          (external-program:start "osascript"
                                  (list "-e" (format nil "tell application \"QuickTime Player\" to get duration of document 1"))
                                  :output :stream))
    (sleep 0.25)
    (read (EXTERNAL-PROGRAM:PROCESS-OUTPUT-STREAM proc))))

(boxer-eval::defboxer-primitive bu::get-video-position ()
  (let ((proc nil))
    (setf proc
          (external-program:start "osascript"
                                  (list "-e" (format nil "tell application \"QuickTime Player\" to get current time of document 1"))
                                  :output :stream))
    ;; 2026-08-13 TODO sgithens - For some reason on lispworks the status of the process is not changing from :running
    ;;                            Same issue above for get-video-length
    ;; (loop while (eq (external-program:process-status proc) :running)
    ;;             do (progn
    ;;                  (sleep 0.5)
    ;;                  (format t "Waiting12: ~A~%" (external-program:process-status proc))))
    (sleep 0.25)
    (read (EXTERNAL-PROGRAM:PROCESS-OUTPUT-STREAM proc))))
