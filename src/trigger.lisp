;;; -*- Mode:lisp;Syntax:Common-Lisp; Package:BOXER; Base:10.-*-

#|


 $Header: trigger.lisp,v 1.0 90/01/24 22:18:40 boxer Exp $

 $Log:	trigger.lisp,v $
;;;Revision 1.0  90/01/24  22:18:40  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                          +-Data--+
                 This file is part of the | BOXER | system
                                          +-------+

            This file contains the implementation or box triggers


Modification History (most recent at top)

 2/16/03 merged current LW and MCL files, no diffs, copyright updated

|#


#-(or lispworks  mcl lispm) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+(or lispworks mcl)        (in-package :boxer)



;;; The proposed interface to box triggers is the existence in the LOCAL
;;; namespace of a box with a (magic) name of either BU::ENTRY-TRIGGER,
;;; BU::EXIT-TRIGGER or BU::MODIFIED-TRIGGER.  Triggers are enabled when
;;; they contain a box (magic) named BU::ENABLED that has a TRUE value.

;;; The problem with this interface is that we have to do all of this
;;; (slow) box destructuring at (possible) trigger run time.  Since we
;;; will be using triggers as a general mechanism for turtle graphics,
;;; we need to optimize this quite a bit.  We do this by caching.
;;; The caches are filled by the naming methods which check for the
;;; reserved names.  For now, flush the caches on modified (we can be
;;; smarter about this later)

;;; we may want to allocate more slots for trigger caches rather than
;;; having lists to cdr down. However, at the moment, there are only
;;; 3 types of triggers and the common case will most likely be that
;;; a box will only be using one type of triggwer at a time.

(defstruct (trigger-cache (:constructor %make-trigger-cache))
  (type 'bu::modified-trigger)
  (box nil)
  (enabled? nil)
  (proc nil)
  (timestamp -1) ; used for version checking with the superior box
  (cached-time -1)) ; used to insure cache consistency with the trigger box

;;; Predicates

(defun has-modified-trigger? (box)
  (fast-memq 'bu::modified-trigger (slot-value box 'current-triggers)))

(defun has-entry-trigger? (box)
  (fast-memq 'bu::entry-trigger (slot-value box 'current-triggers)))

(defun has-exit-trigger? (box)
  (fast-memq 'bu::exit-trigger (slot-value box 'current-triggers)))

(defun has-trigger? (box &optional (trigger-type 'bu::modified-trigger))
  (fast-memq trigger-type (slot-value box 'current-triggers)))



;;; Making a trigger


#|
(defvar *optimize-trigger-procedures?* t)

;; this is a hook to be able to adaptively optimize certain
;; trigger procs automagically.  Right now, what is does is
;; analyze proc and if it is just a single symbol inside of a doit
;; box, and that symbol is a compiled-boxer-function, then we
;; essentially skip a level of doit box funcall.

(defsubst maybe-optimize-trigger-proc (doit)
  (flet ((is-sprite-update-fun? (ip)
	   (let ((code (eval::interpreted-boxer-function-text ip)))
	     (and (= 1 (length code))		; there's 1 line
		  (= 1 (length (car code)))	; with one element
;		  (sprite-update-function?
;		    (caar code))
		  ))))		; that's an update function
    (let ((interpreted-proc
	    (eval::convert-editor-box-to-interpreted-procedure-internal
	      doit)))
      (cond ((null *optimize-trigger-procedures?*) interpreted-proc)
	    ((is-sprite-update-fun? interpreted-proc)
	     (let ((update-fun
		     (eval::boxer-symeval		; symbol-value instead ?
		       (caar
			 (eval::interpreted-boxer-function-text
			   interpreted-proc)))))
	       (if (eval::boxer-function? update-fun)
		   update-fun
		   interpreted-proc)))
	    (t interpreted-proc)))))
|#

;;; No longer optimized.  This way we get the function run in the scope
;;; of the trigger itself, which is pretty close to running it in the box,
;;; which is what we really want.
(defun maybe-optimize-trigger-proc (doit-box)
  (list (port-to doit-box)))
#|
;;; This is the new scheme for optimizing triggers.  We take a doit box
;;; and look inside it.  If there are args or local variables, or the
;;; box has more than one line, then we just return a list containing
;;; the box.  Otherwise we return a list of the first
;;; row contents.  I.e., we might return (UPDATE-SPRITE-STUFF).
;;; We use eval::convert-editor-box-to-interpreted-procedure-internal
;;; to avoid keeping the cached-code if we decide to cache the innards
;;; at this level instead.

  (let ((code (eval::convert-editor-box-to-interpreted-procedure-internal
	       doit-box)))
    (if (or (eval::interpreted-boxer-function-arglist code)
	    (eval::interpreted-boxer-function-locals code)
	    (cdr (eval::interpreted-boxer-function-text code)))
	(list (top-level-virtual-copy-editor-box doit-box))
	(car (eval::interpreted-boxer-function-text code))))
|#

;;; for now, proc should be a doit box.  If it's a data box, then we
;;; make an empty cache, since it might be changed to a doit box later.
(defun make-trigger-cache (type doit)
  (if (doit-box? doit)
      (let* ((enabled?-box
	      (eval::lookup-static-variable-in-box-only doit
							'bu::enabled?))
	     (enabled? (or (null enabled?-box)
			   ;; this ought to be more modular but I don't
			   ;; want to make a VC just to see if the box
			   ;; is true
			   (eq (access-evrow-element
				nil
				(car (evrow-pointers
				      (car (virtual-copy-rows enabled?-box)))))
			       'bu::true)))
	     (proc (maybe-optimize-trigger-proc doit)))
	(%make-trigger-cache :type type
			     :box doit
			     :proc proc
			     :enabled? enabled?
			     :cached-time (actual-obj-tick doit)))
      (%make-trigger-cache :type type
			   :box doit
			   :proc nil
			   :enabled? nil
			   :cached-time (actual-obj-tick doit))))


;; called when the cached-time on the cache is less than the tick of the box
(defun update-trigger-cache-values (cache)
  ;; the box and type slots remain the same
  (let ((box (trigger-cache-box cache)))
    (if (doit-box? box)
	(let* ((enabled?-box
		(eval::lookup-static-variable-in-box-only box
							  'bu::enabled?))
	       (enabled? (or (null enabled?-box)
			     ;; this ought to be more modular but I don't
			     ;; want to make a VC just to see if the box
			     ;; is true
			     (eq (access-evrow-element
				  nil
				  (car (evrow-pointers
					(car (virtual-copy-rows
					      enabled?-box)))))
				 'bu::true)))
	       (proc (maybe-optimize-trigger-proc box)))
	  (setf (trigger-cache-proc cache) proc
		(trigger-cache-enabled? cache) enabled?
		(trigger-cache-cached-time cache) (actual-obj-tick box)))
	(setf (trigger-cache-proc cache) nil
	      (trigger-cache-enabled? cache) nil
	      (trigger-cache-cached-time cache) (actual-obj-tick box)))))


;;;; Editor Interface

;;; This is the beginning of a "Magic Names" interface for the editor
;;; Basically, the naming code should check for either a MAGIC-NAME-INSERT
;;; or a MAGIC-NAME-DELETE property on each symbol and if there is one,
;;; funcall it with the (named) box and it's superior as args

(defun find-trigger-holder (in-box &optional trigger-type)
  (do ((b in-box (superior-box b)))
      ((or (null b)
	   (not (or (eq (exports b) eval::*exporting-all-variables-marker*)
		    (fast-memq trigger-type (exports b)))))
	b)
    ))

(defun cache-trigger-proc (trigger-type trigger-box in-box)
  (let ((superior-box (find-trigger-holder in-box trigger-type)))
    (unless (null superior-box)
      (let ((ct (and superior-box (slot-value superior-box 'current-triggers)))
	    (cs (and superior-box (slot-value superior-box 'trigger-cache)))
	    (new-trigger (make-trigger-cache trigger-type trigger-box)))
	(unless (fast-memq trigger-type ct)
	  (setf (slot-value superior-box 'current-triggers)
		(cons trigger-type ct)))
	;; set up timestamp for cache
	;; can't do this here cause modified is ALWAYS called AFTER the call to
	;; cache-trigger-proc
;    (setf (trigger-cache-timestamp new-trigger)
;	  (actual-obj-tick superior-box))
	(cond ((member-if #'(lambda (tc)
			      (eq (trigger-cache-type tc)
				  trigger-type))
			  cs)
	       ;; there is an old trigger here so we
	       ;; need to remove it first
	       (setf (slot-value superior-box 'trigger-cache)
		     (delete-if #'(lambda (tc)
				    (eq (trigger-cache-type tc)
					trigger-type))
				cs))
	       ;; now push the new trigger
	       (setf (slot-value superior-box 'trigger-cache)
		     (cons new-trigger cs)))
	      (t
	       ;; all we need to do is to push the trigger
	       (setf (slot-value superior-box 'trigger-cache)
		     (cons new-trigger cs))))))))


(defun remove-trigger-proc (trigger-type trigger-box from-box)
  (declare (ignore trigger-box))
  (let ((superior-box (find-trigger-holder from-box trigger-type)))
    (setf (slot-value superior-box 'current-triggers)
	  (fast-delq trigger-type
		     (slot-value superior-box 'current-triggers)))
    (setf (slot-value superior-box 'trigger-cache)
	  (delete-if #'(lambda (tc)
			 (eq (trigger-cache-type tc)
			     trigger-type))
		     (slot-value superior-box 'trigger-cache)))))

(defun flush-trigger-cache (trigger-type trigger-box from-box)
  (declare (ignore trigger-box))
  (let ((superior-box (find-trigger-holder from-box trigger-type)))
    (unless (null superior-box)
      (setf (slot-value superior-box 'trigger-cache) nil))))

;;;  Install caching and decaching on the trigger names

(defmacro install-trigger (trigger-type name action)
  (let ((fun-name (gensym)))
    `(progn
       (defun ,fun-name (box sup-box)
	 (,action ',trigger-type box sup-box))
       (setf (get ',trigger-type ',name) #',fun-name))))

(install-trigger bu::modified-trigger
		 magic-name-insert
		 cache-trigger-proc)

(install-trigger bu::entry-trigger
		 magic-name-insert
		 cache-trigger-proc)

(install-trigger bu::exit-trigger
		 magic-name-insert
		 cache-trigger-proc)

(install-trigger bu::modified-trigger
		 magic-name-delete
		 remove-trigger-proc)

(install-trigger bu::entry-trigger
		 magic-name-delete
		 remove-trigger-proc)

(install-trigger bu::exit-trigger
		 magic-name-delete
		 remove-trigger-proc)

(install-trigger bu::modified-trigger
		magic-name-modified
		flush-trigger-cache)

(install-trigger bu::entry-trigger
		magic-name-modified
		flush-trigger-cache)

(install-trigger bu::exit-trigger
		magic-name-modified
		flush-trigger-cache)



;;; These assume that the existence of the appropriate trigger has
;;; already been checked for.  That is, it is not safe to call them
;;; without having called has-<mumble>-trigger? first.

(defun trigger-enabled? (box trigger-type)
  (let ((cache (car (member-if #'(lambda (tc)
				   (eq (trigger-cache-type tc)
				       trigger-type))
			       (slot-value box 'trigger-cache)))))
    (cond ((null cache)
	   ;; cache has been flushed so we need to recache
	   (let ((new-cache
		  (make-trigger-cache
		   trigger-type
		   (eval::lookup-static-variable-in-box-only box
							     trigger-type))))
	     (push new-cache (slot-value box 'trigger-cache))
	     (trigger-cache-enabled? new-cache)))
	  (t
	   ;; first, make sure the cache is up to date
	   (unless (<= (actual-obj-tick (trigger-cache-box cache))
		       (trigger-cache-cached-time cache))
	     (update-trigger-cache-values cache))
	   (trigger-cache-enabled? cache)))))


;;;; Running Triggers
;;;  This is the interface to the editor, these procedures are what
;;;  the various enter and exit messages can use.
;;;  Note that modified needs to be called by all side-effecting primitives
;;;  when they operate on an editor box (though a port of course !)

;;; NEED a Boxer-Funcall !!!!!

(defun run-trigger (box type)
  (let ((cache (find type (slot-value box 'trigger-cache)
		     :key #'trigger-cache-type)))
    (when (eq type 'bu::modified-trigger)
      ;; modified triggers differ from the standard by recording
      ;; the "version" (timestamp) of the box so that if the box
      ;; is modified (by the editor), we can compare "versions" on
      ;; box exit to decide if the modified-trigger needs to be run
      (setf (trigger-cache-timestamp cache) (actual-obj-tick box)))
    (eval::arrange-for-list-to-be-run (trigger-cache-proc cache))))


;; standard function to use for most triggers by
;; editor functions that perform the triggering action
;; this checks on conditions local to the trigger, that is,
;; whether the trigger exists and is enabled.  It is up to
;; the calling editor method to decide when to invoke the trigger

(defun maybe-run-trigger (box &optional (trigger-type 'bu::entry-trigger))
  (when (and (has-trigger?     box trigger-type)
	     (trigger-enabled? box trigger-type))
    ;; at this point, the cache will be filled
    (run-trigger box trigger-type)))

;; This is used in situations where is is not feasible to
;; call maybe-run-trigger explicitly.  The common case
;; is when the box is being incrementally changed by the
;; editor and then at some point the editing changes are
;; temporily finished (usually on box exit)
;; the concept of "modified" is based on recording timestamps
;; when the modified trigger is run and comparing them to the
;; timestamp of the box

(defun maybe-run-modified-trigger (edbox)
  (let ((box (box-or-port-target edbox)))
    (when (and (has-trigger?     box 'bu::modified-trigger)
	       (trigger-enabled? box 'bu::modified-trigger))
      ;; cache will be filled so check timestamps
      (let ((cache (car (member-if #'(lambda (tc)
				       (eq (trigger-cache-type tc)
					   'bu::modified-trigger))
				   (slot-value box 'trigger-cache)))))
        (cond ((minusp (trigger-cache-timestamp cache))
	       ;; brand new trigger
	       (setf (trigger-cache-timestamp cache) (actual-obj-tick box))
	       (run-trigger box 'bu::modified-trigger))
	      ((< (trigger-cache-timestamp cache)
		  (actual-obj-tick box))
	       ;; the trigger last ran an older version of the box
	       (run-trigger box 'bu::modified-trigger)))))))


;;;; Help for NAME-HELP

(eval-when (load)
  (setf (get 'bu::modified-trigger 'unbound-name-help-message)
	"is checked for, and run when found, whenever a box is changed"
	(get 'bu::entry-trigger 'unbound-name-help-message)
	"is checked for, and run when found, when the cursor enters a box"
	(get 'bu::exit-trigger 'unbound-name-help-message)
	"is checked for, and run when found, when the cursor leaves a box")
  )
