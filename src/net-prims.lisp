;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
#|

 $Header: net-prims.lisp,v 1.0 90/01/24 22:15:07 boxer Exp $

 $Log:	net-prims.lisp,v $
;;;Revision 1.0  90/01/24  22:15:07  boxer
;;;Initial revision
;;;








     Copyright 1989 - 1997 Regents of the University of California

 Enhancements and Modifications Copyright 1998 - 2003 Pyxisystems LLC


                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+



   This file contains all of the boxer functions which use the
   file system.


Modification History (most recent at top)

 4/21/03 merged current LW and MCL files, no diffs, updated copyright

|#

#-(or lispworks mcl lispm) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+(or lispworks mcl)       (in-package :boxer)

(defboxer-primitive bu::send-box ((eval::dont-copy where)(bu::port-to box))
  (let ((hostname (box-text-string where))
	(box (get-port-target box)))
    (let ((guaranteed-editor-box (if (box? box) box (top-level-print-vc box))))
      (boxnet::with-open-port-stream (stream hostname)
	(dump-top-level-box-to-stream guaranteed-editor-box stream)))
    eval::*novalue*))

(defvar *net-boxes-to-be-inserted* nil)

;;; Two low-level implementations: polling and processes
;;; polling happens inside BOXER-COMMAND-LOOP.  If you have
;;; a brain-damaged implementation that won't let you do that,
;;; or if you feel it's really important to receive those sends
;;; while Boxer programs are running (even though you won't
;;; actually GET the sends), then use processes.
;;; Processes will probably go away, because no one can be
;;; that brain damaged and still get the sends, since we
;;; use the same mechanism.
;;;
;;; But we need the polling to happen more often at toplevel.
;;;
;;; Also, we need some error trapping around the receive thing.

;;; NIL means off.
;;; :POLL means polling
;;; :INTERRUPT means processes.
(defvar *boxer-send-server-status* nil)

;;; This function is called from setup-evaluator.  Make it
;;; do whatever you want.
(defun setup-boxer-send ()
  (enable-boxer-send-polling))

(defboxer-primitive bu::enable-boxer-send-polling ()
  (enable-boxer-send-polling)
  eval::*novalue*)

(defun enable-boxer-send-polling ()
  (when (eq *boxer-send-server-status* :interrupt) (boxnet::close-server))
  (boxnet::setup-server)
  (boxnet::enable-polling 'net-interrupt-function)
  (setq *boxer-send-server-status* :poll))

(defboxer-primitive bu::disable-boxer-send-polling ()
  (disable-boxer-send-polling)
  eval::*novalue*)

(defun disable-boxer-send-polling ()
  (boxnet::disable-polling)
  (boxnet::close-server)
  (setq *boxer-send-server-status* nil))

(defboxer-primitive bu::enable-boxer-send-interrupts ()
  (enable-boxer-send-interrupts)
  eval::*novalue*)

(defun enable-boxer-send-interrupts ()
  (when (eq *boxer-send-server-status* :poll) (boxnet::disable-polling))
  (boxnet::setup-server)
  (boxnet::setup-connection-waiting-process 'net-interrupt-function)
  (setq *boxer-send-server-status* :interrupt))

(defboxer-primitive bu::disable-boxer-send-interrupts ()
  (disable-boxer-send-interrupts)
  eval::*novalue*)

(defun disable-boxer-send-interrupts ()
  (boxnet::close-server)
  (setq *boxer-send-server-status* nil))

(defboxer-primitive bu::receive-boxer-send ()
  (receive-boxer-send))

(defun receive-boxer-send ()
  (let ((box (pop *net-boxes-to-be-inserted*)))
    (when (null *net-boxes-to-be-inserted*) (status-line-undisplay 'handle-net-interrupt))
    (or box eval::*novalue*)))

;;;runs at interrupt time or at poll time, when we first read the message itself
;;; from the network.
(defun net-interrupt-function (stream)
  (let ((box (safe-load-binary-box-from-stream-internal stream)))
    (unless (null box)
      (push box *net-boxes-to-be-inserted*)
      (push 'handle-net-interrupt bw::*boxer-command-loop-event-handlers-queue*))))

;;; we don't want errors (like EOF) to trash the Boxer.  Just give up for now.
(defun safe-load-binary-box-from-stream-internal (stream)
  (let ((result (#+Lucid lcl::ignore-errors #-Lucid progn
			 (load-binary-box-from-stream-internal stream))))
    (cond ((null result)
	   ;; we would like to complain here but since we're running at an
	   ;; interrupt time we can't do a boxer-editor-error.
	   (restart-send-server)
	   nil)
	  (t result))))

;;;Runs inside BOXER-COMMAND-LOOP.
(defun handle-net-interrupt ()
  (beep) (beep)
  (status-line-display 'handle-net-interrupt
		       "*** Incoming Box Send --- Type META-R to Receive ***")
  (restart-send-server))

(defun restart-send-server ()
  (boxnet::setup-server)
  (when (eq *boxer-send-server-status* :interrupt)
    (boxnet::setup-connection-waiting-process 'net-interrupt-function)))
