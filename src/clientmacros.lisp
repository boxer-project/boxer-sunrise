;-*- mode:lisp; syntax:common-lisp;  package:boxnet -*-
#|

 $Header$

 $Log$

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


 Macro Definitions and Variable Declarations for the
 Client side of the File system


Modification History (most recent at top)

 2/16/03 merged current LW and MCL files, no diffs, copyright updated

|#

#-(or lispworks mcl lispm) (in-package 'boxnet)
#+(or lispworks mcl)       (in-package :boxnet)



;;;; Important Variables

(defvar *dump-out-file-box-inferiors?* t)

(defvar *box-bid-delete-list* nil)

(defvar *loading-via-box-server?* nil)

(defvar *during-login-action?* nil
  "Bound to T during the loading of the top level box")

(defvar *login-postpass-list* nil)



;;;; Debugging Support

(defvar *include-debugging-code* t)

(defvar *debug-box-client?* nil)

(defun client-debug () (setq *debug-box-client?* t))

(defun client-nodebug () (setq *debug-box-client?* nil))

(defun client-single-step-mode () (setq *debug-box-client?* :single-step))

(defvar *client-debug-stream* *trace-output*)

;;; Logging variables
(defvar *opcode-logging-enabled?* nil)
(defvar *log-file* "/tmp/boxer-client.log")

;;; The Opcodes...

(defvar *server-opcode-names* (make-array 255 :initial-element nil))

(defvar *opcode-debugging-format-string* "Sending Opcode ~D (~A)")

(defun print-debugging-message-internal (format-string &rest format-args)
  #+lucid (declare (lcl::dynamic-extent format-args))
  (when *debug-box-client?*
    (apply #'format *client-debug-stream* format-string format-args)
    (if (eq *debug-box-client?* :single-step)
	(let ((string (read-line)))
	  (cond ((string-equal string "break")
		 (break))))
	(terpri *client-debug-stream*))
    (force-output *client-debug-stream*)))


(defmacro debugging-message (format-string . format-args)
  (when *include-debugging-code*
    `(print-debugging-message-internal ,format-string . ,format-args)))





;;; net chars to lisp chars

;; these will do for now....

(defmacro netchar->char (byte)
  `(code-char ,byte))

(defmacro char->netchar (char)
  `(char-code ,char))

;;;; File Info

(defvar *box-bid-table* (make-hash-table))

(defvar *bid-box-table* nil)

;;; these info structs function as write-through caches for
;;; file info on the server
(defstruct (server-box-info (:conc-name sbi-)
			    (:predicate server-box-info?)
			    (:constructor %make-server-box-info))
  (bid 1)
  (box nil)
  (owner nil)
  (read-date 0)
  (write-date 0)
  (boxname "UnNamed")  ; not neccessarily the same as the box's name
  (inferiors nil)
  (forwarding-table nil)
  (superior-box-bid 0)
  (server nil)
  (local-timestamp -1))

(defun make-server-box-info (bid box
				 &optional
				 (write-date 0)
				 (owner (bsui-username *box-server-user*))
				 (supid 0)
				 table
				 (server (get-server)))
  (%make-server-box-info :bid bid :box box
			 :write-date write-date
			 :forwarding-table table
			 :superior-box-bid supid
			 :owner owner
			 :server server))


;;;; User Info

(defvar *box-server-user* nil)

(defvar *cache-server-password?* nil)

(defvar *default-box-server-user* "boxer")

(defstruct (box-server-user-info (:conc-name bsui-)
				 (:predicate box-server-user-info?)
				 (:constructor %make-box-server-user-info))
  (username nil)
  (pretty-name nil)
  (cached-password nil)
  (hostname (machine-instance))
  (top-box-id -1))

;; on the sparcs, we may want to eventually play a sound file here
(defun warning-sound ()
  (bw::beep))



;;;; Finding the current server

(defvar *top-level-bfs-server* nil)

(defun get-server (&optional(box eval::*lexical-variables-root*))
  (if (not (box::box? box)) *top-level-bfs-server*
      (or (box::getprop box 'bfs-server)
	  (get-server (box::superior-box box)))))

;;; Cross File Port and Forwarding Table Utilities

(defvar *cross-file-link-id-counter* 0)

(defvar *link-id-table* (make-hash-table :test #'eq))


(defstruct (cross-file-link (:predicate %cross-file-link?)
			    (:constructor %make-cross-file-link))
  (id -1))

(defstruct (inferior-file-link (:include cross-file-link)
			       (:predicate %inferior-file-link?)
			       (:constructor %make-inferior-file-link))
  (port-branch nil)
  (target-branch nil))

(defstruct (file-branch-link (:include cross-file-link)
			     (:predicate %cross-file-branch-link))
  (inferior-link nil) ; backpointer
  (box)               ; terminating box, either a target or a port
  (lowest nil)
  (broken nil))

(defstruct (file-port-branch-link (:include file-branch-link)
				  (:predicate %file-port-branch-link?)
				  (:constructor %make-file-port-branch-link)
				  (:print-function %print-file-port-branch-link))
  )

(defstruct (file-target-branch-link (:include file-branch-link)
				    (:predicate %file-target-branch-link?)
				    (:constructor
				     %make-file-target-branch-link)
				    (:print-function
				     %print-file-target-branch-link))
  )

#|

;; a port UID of 0 in the port target reference means
;; that the last box in the chain is the target

(defmacro do-pttt-entries ((var table) &body body)
  `(dolist (,var ,table) . ,body))

(defun make-pttt-table (storage-box)
  (let ((table nil)
	(tid 1))
    (dolist (bl (box::branch-links storage-box))
      (when (box::target-branch-link? bl)
	(let ((superior-storage-box (superior-storage-box
				     (box::link-target bl))))
	  (when (and (eq superior-storage-box storage-box)
		     (not (eq (box::link-target bl) storage-box)))
	    (push (cons tid (box::link-target bl)) table)
	    (incf& tid)))))
    table))

;; returns a new table and a flag indicating whether or not the
;; table had to be modifed
(defun update-pttt-table (storage-box old-table)
  (let ((tid (length old-table))
	(modified nil))
    (dolist (bl (box::branch-links storage-box))
      (when (box::target-branch-link? bl)
	(let ((superior-storage-box (superior-storage-box
				     (box::link-target bl))))
	  (when (and (eq superior-storage-box storage-box)
		     (not (eq (box::link-target bl) storage-box)))
	    (unless (rassoc (box::link-target bl) old-table :test #'eq)
	      (push (cons tid (box::link-target bl)) old-table)
	      (setq modified t)
	      (incf& tid))))))
    ;; check for obsolete entries
    (do-pttt-entries (entry old-table)
      (when (not (member (cdr entry) (box::branch-links storage-box)
			 :test #'(lambda (a b) (eq a (box::link-target b)))))
	(setq modified t)
	(setf (cdr entry) 'not-here-anymore)))
    (values old-table modified)))

;;; like update-pttt-table except it doesn't cons a new table
;;; we don't bother to remove bad entries, just look for new ones
;;; which should be there
(defun check-pttt-table (storage-box)
  (let ((old-table (get-port-target-translation-table storage-box)))
    (dolist (bl (box::branch-links storage-box))
      (when (box::target-branch-link? bl)
	(let ((superior-storage-box (superior-storage-box
				     (box::link-target bl))))
	  (when (and (eq superior-storage-box storage-box)
		     (not (eq (box::link-target bl) storage-box)))
	    (unless (rassoc (box::link-target bl) old-table :test #'eq)
	      (return t))))))))

(defsubst make-pttt-entry (uid target)
  (cons uid target))

(defsubst pttt-uid (trans) (car trans))

(defsubst pttt-target (trans) (cdr trans))

(defun find-target-uid (target table)
  (declare (list table))
  (let ((entry (rassoc target table :test #'eq)))
    (unless (null entry) (pttt-uid entry))))

(defun get-port-target-translation-table (box)
  (boxer::getprop box :port-target-translation-table))

(defun set-port-target-translation-table (box new-table)
  (boxer::putprop box new-table :port-target-translation-table))

|#

(defvar *current-forwarding-table* nil)

(defvar *new-forwarding-inferiors* nil
  "Any storage-chunk encountered during the filling of box contents
   should be pushed onto this list so that the top level box's
   forwarding table can be pushed down to the next level of inferiors")

;;; this needs to be used by the various get-box and get-box-and-info
;;; server interface functions to automagically do any neccessary forwarding
(defmacro with-forwarding-table-bound ((bid server-info) &body body)
  `(let* ((*current-forwarding-table* (unless (null ,server-info)
					(sbi-forwarding-table ,server-info)))
	  (*new-forwarding-inferiors* nil))
     (prog1 (progn . ,body)
       ;; if the body finishes without error, then the forwarding
       ;; table needs to be pushed down to the next level of
       ;; inferiors and we can reset the forwarding table for this
       ;; particular box
       (unless (null *current-forwarding-table*)
	 (dolist (inferior-bid *new-forwarding-inferiors*)
	   (set-bid-forwarding-table inferior-bid
				     (merge-forwarding-tables
				      (get-bid-forwarding-table inferior-bid)
				      *current-forwarding-table*)))
	 (set-bid-forwarding-table ,bid NIL)))))

(defmacro current-forwarding-table () '*current-forwarding-table*)

(defun record-inferior-storage-chunk (bid)
  (push bid *new-forwarding-inferiors*))

;;; ??? old entries should take precedence over new ones ??
(defun merge-forwarding-tables (old-table new-table)
  (let ((newnew nil))
    ;; loop through the old looking for an entry in the new
    (do* ((remaining-pairs old-table (cddr remaining-pairs))
	  (orig-bid  (car remaining-pairs) (car remaining-pairs))
	  (trans-bid (cadr remaining-pairs) (cadr remaining-pairs))
	  (new-trans (find-bid trans-bid new-table)))
	 ((null orig-bid))
      (cond ((find-bid orig-bid new-table)
	     ;; old entry is handled in new table, so do nothing
	     )
	    ((not (null new-trans))
	     ;; old translation is obsolete, convert it to a new one
	     (setq newnew (append newnew (list orig-bid new-trans))))
	    (t
	     ;; old entry has nothing to do with the new table so preserve it
	     (setq newnew (append newnew (list orig-bid trans-bid))))))
    ;; now append the new table to the built up table of
    ;; translated old references and return it
    (append newnew new-table)))

;; remember that the CAR is a port-target-uid and not part of the bid path
(defun translate-port-target-path (path)
  (let ((ft (current-forwarding-table)))
    (cond ((null ft) path)
	  (t
	   (let ((newpath (list (car path))))
	     (dolist (bid (cdr path))
	       (let ((trans (member bid ft :test #'=)))
		 (push (if (null trans) bid (cadr trans)) newpath)))
	     (nreverse newpath))))))




(defmacro with-server ((server-variable) &body body)
  `(let ((,server-variable (get-server)))
     . ,body))

(defmacro with-info ((info-var) &body body)
  `(let ((,info-var (%make-server-box-info)))
     #+lucid (declare (lcl::dynamic-extent ,info-var))
     . ,body))

(defmacro with-bfs-standard-dumping-environment (&body body)
  `(let ((*dump-out-file-box-inferiors?* nil))
     . ,body))

(defun in-bfs-environment? () (not *dump-out-file-box-inferiors?*))

;;; Accessor and mutator generating macro for server-box-info's
;;; remember that that the info structs are supposed to be
;;; write through caches of information on the server

(defmacro defsbi-info-slot (slot-name)
  (let ((box-accessor-name (intern (symbol-format nil "GET-BOX-~A" slot-name)))
	(box-mutator-name  (intern (symbol-format nil "SET-BOX-~A" slot-name)))
	(bid-accessor-name (intern (symbol-format nil "GET-BID-~A" slot-name)))
	(bid-mutator-name  (intern (symbol-format nil "SET-BID-~A" slot-name)))
	(slot-accessor-name (intern (symbol-format nil "SBI-~A" slot-name))))
    `(progn
       ;; Box designated accessor
       (defun ,box-accessor-name (box &optional (cons-info? nil))
	 (let ((existing-info (box-server-info box cons-info?)))
	   (unless (null existing-info) (,slot-accessor-name existing-info))))
       ;; Box designated mutator
       (defun ,box-mutator-name (box new-value &optional (cons-info? nil))
	 (let ((existing-info (box-server-info box cons-info?)))
	   (unless (null existing-info)
	     (setf (,slot-accessor-name existing-info) new-value)
	     (set-box-info (get-server)
			   (sbi-bid existing-info) existing-info))))
       ;; BId designated accessor
       (defun ,bid-accessor-name (bid)
	 (let ((existing-info (bid-server-info bid)))
	   (cond ((null existing-info)
		  ;; we have to get it from the server directly
		  (with-info (info)
		    (,slot-accessor-name
		     (get-box-info (get-server) bid info))))
		 (t
		  (,slot-accessor-name existing-info)))))
       ;; BId desginated mutator
       (defun ,bid-mutator-name (bid new-value)
	 (let ((existing-info (bid-server-info bid)))
	   (with-server (server)
	     (cond ((null existing-info)
		    ;; get it from the server
		    (with-info (info)
		      (setf (,slot-accessor-name
			     (get-box-info server bid info))
			  new-value)
		    (set-box-info server bid info)))
		   (t
		    (setf (,slot-accessor-name existing-info) new-value)
		    (set-box-info server bid existing-info)))))))))



;;;; Errors

;;; server errors can happen in both the editor and the evaluator
;;; so we need to be able to handle either


;;; used to establish a catch for server errors inside
;;; of editor functions.  Primitives have the usual
;;; evaluator error handling to use, although they might
;;; want to use this mechanism as well.

(defvar *inside-server-error-handler* nil)

(defmacro with-server-errors (&body body)
  `(let ((*inside-server-error-handler* t))
     (catch 'server-error
       . ,body)))

(defun server-error (format-string &rest format-args)
  (cond ((not (null eval::*give-lisp-errors*))
	 (apply #'error format-string format-args))
	((or (null box::*evaluation-in-progress?*)
	     (box::edit-during-eval?))
	 ;; we are editing, either at top level or recursively
	 (apply 'box::boxer-editor-error format-string format-args)
	 ;; If we can, throw
	 (when *inside-server-error-handler*
	   (throw 'server-error nil)))
	(t
	 ;; looks like we are solidly in the evaluator
	 (eval::primitive-signal-error
	  :server-error (apply #'format nil format-string format-args)))))

