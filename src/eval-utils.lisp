;;; -*- Syntax: Common-Lisp; Base: 10; Package: EVAL -*-

#|


 $Header: eval-utils.lisp,v 1.0 90/01/24 22:11:18 boxer Exp $

 $Log:	eval-utils.lisp,v $
;;;Revision 1.0  90/01/24  22:11:18  boxer
;;;Initial revision
;;;






  Copyright 1985, 1986 Massachusetts Institute of Technology 

 Permission to use, copy, modify, distribute, and sell this software
 and its documentation for any purpose is hereby granted without fee,
 provided that the above copyright notice appear in all copies and that
 both that copyright notice and this permission notice appear in
 supporting documentation, and that the name of M.I.T. not be used in
 advertising or publicity pertaining to distribution of the software
 without specific, written prior permission.  M.I.T. makes no
 representations about the suitability of this software for any
 purpose.  It is provided "as is" without express or implied warranty.



  Enhancements Copyright 1986 - 1996 Regents of the University of California

  Additional Portions Copyright 1999 - 2003 Pyxisystems LLC


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


  The Explicit Control Evaluator

Modification History (most recent at top)

 2/15/03 merged current LW and MCL files, no diffs, updated copyright

|#

#-(or lispworks  mcl lispm) (in-package 'eval)
#+(or lispworks mcl)        (in-package :eval)

;;;
;;; Triggers
;;;

;;; Triggers are lists to be evaluated at various times.  Sometimes
;;; they are inside primitives, and sometimes they are inside editor
;;; functions.  Of course, sometimes editor primitives are called
;;; inside the evaluator.

;;; The following function sets a variable to the list to be run.
;;; The evaluator checks the list after calling an sfun in case
;;; something that a primitive did caused it to be set.
;;; Similarly, the two handle-boxer-key and handle-boxer-mouse-click
;;; functions check this variable in case an editor function set it.


#| ;; this loses when a box has both exit and modified triggers
(defun arrange-for-list-to-be-run (list)
  (when (not (null *trigger-list-to-run*))
     (error "A trigger was tripped when an unprocessed trigger was pending: ~S, ~S"
	    *trigger-list-to-run* list))
  (setq *trigger-list-to-run* list))
|#

(defun arrange-for-list-to-be-run (list)
  (if (null *trigger-list-to-run*)
    (setq *trigger-list-to-run* list)
    (setq *trigger-list-to-run* (append *trigger-list-to-run* list))))                                           


;;this function is kind of crocked up.  the value stuff is all wrong.
(defun handle-trigger-list-in-eval (current-value rest-of-line)
  (declare (ignore rest-of-line))
  (case *sfun-continuation*
     (*std-sfun-continuation*
      (prog1
	  (make-interpreted-procedure-from-list
	   (list *trigger-list-to-run*
		 (list (if (eq current-value *novalue*)
			   '%novalue-internal
			 current-value))))
	(setq *sfun-continuation* '*ufuncall-sfun-result-sfun-continuation*)
	(setq *trigger-list-to-run* nil)))
     (*eval-loop-sfun-continuation*
      (error "Can't handle the trigger and CONTINUE at the same time."))
     (*macroexpand-sfun-continuation*
      (make-interpreted-procedure-from-list
       (append *trigger-list-to-run*
	       (make-doit-vc-from-list
		(interpreted-boxer-function-text current-value)))))
     (*ufuncall-sfun-result-sfun-continuation*
      (make-interpreted-procedure-from-list
       (append (list *trigger-list-to-run*)
	       (interpreted-boxer-function-text current-value))))
     (*run-list-sfun-continuation*
       (error "Can't handle a trigger and *run-list-sfun-continuation*"))
     (otherwise
      (error "Unknown sfun continuation type"))))

(defun make-doit-vc-from-list (list)
  (boxer::make-vc 
   (list
    (boxer::make-evrow-from-pointers
     (mapcar #'boxer::make-pointer list)))
   'boxer::doit-box))


;;;
;;; Special Tokens
;;;

(defun make-squid (item)
  (let ((array (make-array 3)))
    (setf (svref& array 0) 'special-eval-token)
    (setf (svref& array 1) 'self-quoting-internal-datum)
    (setf (svref& array 2) item)
    array))
