;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER ;-*-

#|


 $Header: lodisp.lisp,v 1.0 90/01/24 22:14:12 boxer Exp $

 $Log:	lodisp.lisp,v $
;;;Revision 1.0  90/01/24  22:14:12  boxer
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

  This file contains the low level code for the connection/disconnection of
  screen objects.  The file INFSUP has the analogous methods for editor objects


Modification History (most recent at top)

 1/23/12 {first,last}-screen-row methods for sprite-screen-box
 2/15/03 merged current LW and MCL files, no changes, updated copyright
 4/30/98 started logging, change copyright, source = boxer version 2.2r4 + fonts

|#

(in-package :boxer)

;;; these have all been changed to use the new storage substrate,
;;; I'm commenting out the ones that aren't currently used so I
;;; don't have to port them all....

;;; useful for debugging
(defmethod show-infs ((self screen-row))
  (dotimes (i (storage-vector-active-length (slot-value self 'screen-chas)))
    (format t " ~S " (sv-nth i (slot-value self 'screen-chas)))))

(defmethod show-infs ((self screen-box))
  (dotimes (i (storage-vector-active-length (slot-value self 'screen-rows)))
    (format t " ~S " (sv-nth i (slot-value self 'screen-rows)))))

; sgithens TODO ... this defun doesn't seem to be anywhere, unless it's generated
; somewhere by a macro
(defun check-screen-row-arg (screen-row) t)

;;; LOW-LEVEL methods to handle connection and disconnection of screen-
;;; objs. These methods take care of all adding/removal of screen-chas
;;; to/from screen-rows, and all adding/removal of screen-rows to/from
;;; screen-boxes.
;;; Like all the other methods which are concerned with inferior/superior
;;; relations between screen-objs the connection/disconnection methods
;;; have specific names for the specific screen-objs involved, and also
;;; have abtract names which deal with the abstract superior/inferior
;;; relation between those screen-objs. The abstract names are aliases
;;; for the specific names.

;;;   :INSERT-SCREEN-CHA <new-screen-cha> <before-screen-cha>
;;;   :INSERT-SCREEN-ROW <new-screen-row> <before-screen-row>
;;;   :INSERT-SCREEN-OBJ <new-screen-obj> <before-screen-obj>
;;; These methods all cause the screen-obj which receives the message
;;; to insert <new-screen-obj> in their screen inferiors just before
;;; <before-screen-obj>. For convenience, if <before-screen-obj> is
;;; null, <new-screen-obj> is appended to the existing inferiors.
;;; These methods also all have variants which take a list of screen-
;;; objs as their first argument, and insert the entire list before
;;; their second argument.

(defmethod insert-screen-cha-at-cha-no ((self screen-row) new-screen-cha cha-no)
  (sv-insert-at (slot-value self 'screen-chas) cha-no new-screen-cha)
  (when (screen-box? new-screen-cha)
    (setf (screen-row new-screen-cha) self)))

(defmethod insert-screen-chas-at-cha-no ((self screen-row)
					 new-screen-chas-list cha-no)
  (sv-multiple-insert-at (slot-value self 'screen-chas) cha-no
			 new-screen-chas-list)
  (dolist (new-screen-cha new-screen-chas-list)
    (when (screen-box? new-screen-cha)
      (setf (screen-row new-screen-cha) self))))

;(DEFMETHOD INSERT-SCREEN-CHA ((SELF SCREEN-ROW) NEW-SCREEN-CHA BEFORE-SCREEN-CHA)
;  (CHECK-SCREEN-CHA-ARG NEW-SCREEN-CHA)
;  (COND ((NULL (SCREEN-ROW NEW-SCREEN-CHA))
;	 (SETF (SCREEN-ROW NEW-SCREEN-CHA) SELF)
;	 (IF (NOT-NULL BEFORE-SCREEN-CHA)
;	     (SPLICE-ITEM-INTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHA BEFORE-SCREEN-CHA)
;	     (SPLICE-ITEM-ONTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHA)))
;	(T
;	 ;; Oops..
;	 (BARF "The screen-cha ~S is already part of ~S"
;	       NEW-SCREEN-CHA (SCREEN-ROW NEW-SCREEN-CHA)))))

;(DEFMETHOD INSERT-SCREEN-CHAS ((SELF SCREEN-ROW) NEW-SCREEN-CHAS BEFORE-SCREEN-CHA)
;  (CHECK-SCREEN-CHA-ARG (CAR NEW-SCREEN-CHAS))
;  (COND ((NULL (SCREEN-ROW (CAR NEW-SCREEN-CHAS)))
;	 (DOLIST (NEW-SCREEN-CHA NEW-SCREEN-CHAS)
;	   (SETF (SCREEN-ROW NEW-SCREEN-CHA) SELF))
;	 (IF (NOT-NULL BEFORE-SCREEN-CHA)
;	     (SPLICE-LIST-INTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHAS BEFORE-SCREEN-CHA)
;	     (SPLICE-LIST-ONTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHAS)))
;	(T
;	 ;; Oops..
;	 (BARF "I have only checked the first one, but the screen-chas ~S~%~
;                seem to already be part of ~S"
;	       NEW-SCREEN-CHAS (SCREEN-ROW (CAR NEW-SCREEN-CHAS))))))

(defmethod insert-screen-row ((self screen-box) new-screen-row before-screen-row)
  (check-screen-row-arg new-screen-row)
  (cond ((null (screen-box new-screen-row))
	 (setf (screen-box new-screen-row) self)
	 (if (not-null before-screen-row)
	     (sv-insert-before (slot-value self 'screen-rows)
			       new-screen-row before-screen-row)
	     (sv-append (slot-value self 'screen-rows) new-screen-row)))
	(t
	 (barf "The screen-row ~S is already part of ~S"
	       new-screen-row (screen-box new-screen-row)))))

;(DEFMETHOD INSERT-SCREEN-ROWS ((SELF SCREEN-BOX) NEW-SCREEN-ROWS BEFORE-SCREEN-ROW)
;  (CHECK-SCREEN-ROW-ARG (CAR NEW-SCREEN-ROWS))
;  (COND ((NULL (SCREEN-BOX (CAR NEW-SCREEN-ROWS)))
;	 (DOLIST (NEW-SCREEN-ROW NEW-SCREEN-ROWS)
;	   (SETF (SCREEN-BOX NEW-SCREEN-ROW) SELF))
;	 (IF (NOT-NULL BEFORE-SCREEN-ROW)
;	     (SPLICE-LIST-INTO-LIST (SCREEN-ROWS SELF) NEW-SCREEN-ROWS BEFORE-SCREEN-ROW)
;	     (SPLICE-LIST-ONTO-LIST (SCREEN-ROWS SELF) NEW-SCREEN-ROWS)))
;	(T
;	 (BARF "I have only checked the first one, but the screen-rows ~S~%~
;                seem to already be part of ~S"
;	       NEW-SCREEN-ROWS (SCREEN-ROW (CAR NEW-SCREEN-ROWS))))))

;;; Alias for the abstract :INSERT-SCREEN-OBJs methods.
;(DEFMETHOD INSERT-SCREEN-OBJ ((SELF SCREEN-ROW) NEW-SCREEN-CHA BEFORE-SCREEN-CHA)
;  (CHECK-SCREEN-CHA-ARG NEW-SCREEN-CHA)
;  (COND ((NULL (SCREEN-ROW NEW-SCREEN-CHA))
;	 (SETF (SCREEN-ROW NEW-SCREEN-CHA) SELF)
;	 (IF (NOT-NULL BEFORE-SCREEN-CHA)
;	     (SPLICE-ITEM-INTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHA BEFORE-SCREEN-CHA)
;	     (SPLICE-ITEM-ONTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHA)))
;	(T
;	 ;; Oops..
;	 (BARF "The screen-cha ~S is already part of ~S"
;	       NEW-SCREEN-CHA (SCREEN-ROW NEW-SCREEN-CHA)))))

;(DEFMETHOD INSERT-SCREEN-OBJS ((SELF SCREEN-ROW) NEW-SCREEN-CHAS BEFORE-SCREEN-CHA)
;  (CHECK-SCREEN-CHA-ARG (CAR NEW-SCREEN-CHAS))
;  (COND ((NULL (SCREEN-ROW (CAR NEW-SCREEN-CHAS)))
;	 (DOLIST (NEW-SCREEN-CHA NEW-SCREEN-CHAS)
;	   (SETF (SCREEN-ROW NEW-SCREEN-CHA) SELF))
;	 (IF (NOT-NULL BEFORE-SCREEN-CHA)
;	     (SPLICE-LIST-INTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHAS BEFORE-SCREEN-CHA)
;	     (SPLICE-LIST-ONTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHAS)))
;	(T
;	 ;; Oops..
;	 (BARF "I have only checked the first one, but the screen-chas ~S~%~
;                seem to already be part of ~S"
;	       NEW-SCREEN-CHAS (SCREEN-ROW (CAR NEW-SCREEN-CHAS))))))

;(DEFMETHOD INSERT-SCREEN-OBJ ((SELF SCREEN-BOX) NEW-SCREEN-ROW BEFORE-SCREEN-ROW)
;  (CHECK-SCREEN-ROW-ARG NEW-SCREEN-ROW)
;  (COND ((NULL (SCREEN-BOX NEW-SCREEN-ROW))
;	 (SETF (SCREEN-BOX NEW-SCREEN-ROW) SELF)
;	 (IF (NOT-NULL BEFORE-SCREEN-ROW)
;	     (SPLICE-ITEM-INTO-LIST (SCREEN-ROWS SELF) NEW-SCREEN-ROW BEFORE-SCREEN-ROW)
;	     (SPLICE-ITEM-ONTO-LIST (SCREEN-ROWS SELF) NEW-SCREEN-ROW)))
;	(T
;	 (BARF "The screen-row ~S is already part of ~S"
;	       NEW-SCREEN-ROW (SCREEN-BOX NEW-SCREEN-ROW)))))

;(DEFMETHOD INSERT-SCREEN-OBJS ((SELF SCREEN-BOX) NEW-SCREEN-ROWS BEFORE-SCREEN-ROW)
;  (CHECK-SCREEN-ROW-ARG (CAR NEW-SCREEN-ROWS))
;  (COND ((NULL (SCREEN-BOX (CAR NEW-SCREEN-ROWS)))
;	 (DOLIST (NEW-SCREEN-ROW NEW-SCREEN-ROWS)
;	   (SETF (SCREEN-BOX NEW-SCREEN-ROW) SELF))
;	 (IF (NOT-NULL BEFORE-SCREEN-ROW)
;	     (SPLICE-LIST-INTO-LIST (SCREEN-ROWS SELF) NEW-SCREEN-ROWS BEFORE-SCREEN-ROW)
;	     (SPLICE-LIST-ONTO-LIST (SCREEN-ROWS SELF) NEW-SCREEN-ROWS)))
;	(T
;	 (BARF "I have only checked the first one, but the screen-rows ~S~%~
;                seem to already be part of ~S"
;	       NEW-SCREEN-ROWS (SCREEN-BOX (CAR NEW-SCREEN-ROWS))))))



;;;   :APPEND-SCREEN-CHA <new-screen-cha>
;;;   :APPEND-SCREEN-ROW <new-screen-row>
;;;   :APPEND-SCREEN-OBJ <new-screen-obj>
;;; These methods all cause the screen-obj which receives the message
;;; to append <new-screen-obj> to their existing screen inferiors.
;;; Note that this is just like :insert-screen-obj with a null second
;;; argument. Just like :insert-screen-obj methods, :append-screen-obj
;;; methods have variants that take a list of new-screen-objs and append
;;; the entire list to the existing screen inferiors.

(defmethod append-screen-cha ((self screen-row) new-screen-cha)
  (sv-append (slot-value self 'screen-chas) new-screen-cha)
  (when (screen-box? new-screen-cha)
    (setf (screen-row new-screen-cha) self)))

;(DEFMETHOD APPEND-SCREEN-CHAS ((SELF SCREEN-ROW) NEW-SCREEN-CHAS)
;  (SPLICE-LIST-ONTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHAS)
;  (DOLIST (NEW-SCREEN-CHA NEW-SCREEN-CHAS)
;    (WHEN (SCREEN-BOX? NEW-SCREEN-CHA)
;      (SETF (SCREEN-ROW NEW-SCREEN-CHA) SELF))))

(defmethod append-screen-row ((self screen-box) new-screen-row)
  (check-screen-row-arg new-screen-row)
  (cond ((null (screen-box new-screen-row))
	 (setf (screen-box new-screen-row) self)
	 (sv-append (slot-value self 'screen-rows) new-screen-row))
	(t
	 ;; Oops..
	 (barf "The screen row ~s is already part of ~S"
	       new-screen-row (screen-box new-screen-row)))))

;(DEFMETHOD APPEND-SCREEN-ROWS ((SELF SCREEN-BOX) NEW-SCREEN-ROWS)
;  (CHECK-SCREEN-ROW-ARG (CAR NEW-SCREEN-ROWS))
;  (COND ((NULL (SCREEN-BOX (CAR NEW-SCREEN-ROWS)))
;	 (DOLIST (NEW-SCREEN-ROW NEW-SCREEN-ROWS)
;	   (SETF (SCREEN-BOX NEW-SCREEN-ROW) SELF))
;	 (SPLICE-LIST-ONTO-LIST (SCREEN-ROWS SELF) NEW-SCREEN-ROWS))
;	(T
;	 ;; Oops
;	 (BARF "I have only checked the first one, but the screen-rows ~S~%~
;                seem to already be part of ~S"
;	       NEW-SCREEN-ROWS (SCREEN-BOX (CAR NEW-SCREEN-ROWS))))))

;;; Alias for the abstract APPEND-SCREEN-OBJs methods.
;(DEFMETHOD APPEND-SCREEN-OBJ ((SELF SCREEN-ROW) NEW-SCREEN-CHA)
;  (SPLICE-ITEM-ONTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHA)
;  (WHEN (SCREEN-BOX? NEW-SCREEN-CHA)
;    (SETF (SCREEN-ROW NEW-SCREEN-CHA) SELF)))
;
;(DEFMETHOD APPEND-SCREEN-OBJS ((SELF SCREEN-ROW) NEW-SCREEN-CHAS)
;  (SPLICE-LIST-ONTO-LIST (SCREEN-CHAS SELF) NEW-SCREEN-CHAS)
;  (DOLIST (NEW-SCREEN-CHA NEW-SCREEN-CHAS)
;    (WHEN (SCREEN-BOX? NEW-SCREEN-CHA)
;      (SETF (SCREEN-ROW NEW-SCREEN-CHA) SELF))))
;
;(DEFMETHOD APPEND-SCREEN-OBJ ((SELF SCREEN-BOX) NEW-SCREEN-ROW)
;  (CHECK-SCREEN-ROW-ARG NEW-SCREEN-ROW)
;  (COND ((NULL (SCREEN-BOX NEW-SCREEN-ROW))
;	 (SETF (SCREEN-BOX NEW-SCREEN-ROW) SELF)
;	 (SPLICE-ITEM-ONTO-LIST (SCREEN-ROWS SELF) NEW-SCREEN-ROW))
;	(T
;	 ;; Oops..
;	 (BARF "The screen row ~s is already part of ~S"
;	       NEW-SCREEN-ROW (SCREEN-BOX NEW-SCREEN-ROW)))))
;
;(DEFMETHOD APPEND-SCREEN-OBJS ((SELF SCREEN-BOX) NEW-SCREEN-ROWS)
;  (CHECK-SCREEN-ROW-ARG (CAR NEW-SCREEN-ROWS))
;  (COND ((NULL (SCREEN-BOX (CAR NEW-SCREEN-ROWS)))
;	 (DOLIST (NEW-SCREEN-ROW NEW-SCREEN-ROWS)
;	   (SETF (SCREEN-BOX NEW-SCREEN-ROW) SELF))
;	 (SPLICE-LIST-ONTO-LIST (SCREEN-ROWS SELF) NEW-SCREEN-ROWS))
;	(T
;	 ;; Oops
;	 (BARF "I have only checked the first one, but the screen-rows ~S~%~
;                seem to already be part of ~S"
;	       NEW-SCREEN-ROWS (SCREEN-BOX (CAR NEW-SCREEN-ROWS))))))



;;;   :DELETE-SCREEN-CHA <screen-cha>
;;;   :DELETE-SCREEN-ROW <screen-row>
;;;   :DELETE-SCREEN-OBJ <screen-obj>
;;; These methods all cause the screen-obj which receives the message
;;; to delete <screen-obj> from their screen inferiors. To help with
;;; deleting multiple inferior screen objs, these methods have variants
;;; (called :delete-between-screen-objs <from-screen-obj> <to-screen-obj>
;;; which delete all the inferior screen-objs between <from-screen-obj>
;;; (inclusive) and <to-screen-obj> (exclusive).

(defmethod delete-screen-cha-at-cha-no ((self screen-row) cha-no)
  (let ((cha-to-delete (sv-nth cha-no (slot-value self 'screen-chas))))
    (sv-delete-at (slot-value self 'screen-chas) cha-no)
    (when (screen-box? cha-to-delete)
      (setf (screen-row cha-to-delete) nil))))

(defmethod delete-screen-chas-from-to ((self screen-row) from-cha-no to-cha-no)
  (do-vector-contents (cha-to-delete (slot-value self 'screen-chas)
				     :start from-cha-no :stop to-cha-no)
    (when (screen-box? cha-to-delete)
      (setf (screen-row cha-to-delete) nil)))
  (sv-delete-from-to (slot-value self 'screen-chas) from-cha-no to-cha-no))

;(DEFMETHOD DELETE-SCREEN-CHA ((SELF SCREEN-ROW) SCREEN-CHA-TO-DELETE)
;  (CHECK-SCREEN-CHA-ARG SCREEN-CHA-TO-DELETE)
;  (COND ((EQ (SCREEN-ROW SCREEN-CHA-TO-DELETE) SELF)
;	 (SETF (SCREEN-ROW SCREEN-CHA-TO-DELETE) NIL)
;	 (SPLICE-ITEM-OUT-OF-LIST (SCREEN-CHAS SELF) SCREEN-CHA-TO-DELETE))
;	(T
;	 ;; Oops..
;	 (BARF "The screen-cha ~S is not part of the screen-row ~S"
;	       SCREEN-CHA-TO-DELETE SELF))))

;(DEFMETHOD DELETE-BETWEEN-SCREEN-CHAS ((SELF SCREEN-ROW) FROM-SCREEN-CHA TO-SCREEN-CHA)
;  (CHECK-SCREEN-CHA-ARG FROM-SCREEN-CHA)
;  (CHECK-SCREEN-CHA-ARG TO-SCREEN-CHA)
;  (COND ((AND (EQ (SCREEN-ROW FROM-SCREEN-CHA) SELF)
;	      (EQ (SCREEN-ROW TO-SCREEN-CHA) SELF))
;	 (LET ((DELETED-SCREEN-CHAS (SELF-AND-NEXT-SCREEN-CHAS FROM-SCREEN-CHA)))
;	   (SPLICE-BETWEEN-ITEMS-OUT-OF-LIST (SCREEN-CHAS SELF) FROM-SCREEN-CHA TO-SCREEN-CHA)
;	   (DOLIST (DELETED-SCREEN-CHA DELETED-SCREEN-CHAS)
;	     (SETF (SCREEN-ROW DELETED-SCREEN-CHA) NIL))))
;	(T
;	 ;; Oops..
;	 (BARF "The screen-chas ~S and ~S are not both part of the screen row ~S"
;	      FROM-SCREEN-CHA TO-SCREEN-CHA SELF))))

(defmethod delete-screen-row ((self screen-box) screen-row-to-delete)
  (check-screen-row-arg screen-row-to-delete)
  (cond ((eq (screen-box screen-row-to-delete) self)
	 (setf (screen-box screen-row-to-delete) nil)
	 (sv-delete-item (slot-value self 'screen-rows) screen-row-to-delete))
	(t
	 ;; Oops..
	 (barf "The screen-row ~S is not part of the screen-box ~S"
	       screen-row-to-delete self))))

(defmethod delete-screen-rows-from-to ((self screen-box
					     ) from-row-no to-row-no)
  (do-vector-contents (row-to-delete (slot-value self 'screen-rows)
				     :start from-row-no :stop to-row-no)
    (setf (screen-box row-to-delete) nil))
  (sv-delete-from-to (slot-value self 'screen-rows) from-row-no to-row-no))

;(DEFMETHOD DELETE-BETWEEN-SCREEN-ROWS ((SELF SCREEN-BOX) FROM-SCREEN-ROW TO-SCREEN-ROW)
;  (CHECK-SCREEN-ROW-ARG FROM-SCREEN-ROW)
;  (CHECK-SCREEN-ROW-ARG TO-SCREEN-ROW)
;  (COND ((AND (EQ (SCREEN-BOX FROM-SCREEN-ROW) SELF)
;	      (EQ (SCREEN-BOX TO-SCREEN-ROW) SELF))
;	 (LET ((DELETED-SCREEN-ROWS (SELF-AND-NEXT-SCREEN-ROWS FROM-SCREEN-ROW)))
;	   (SPLICE-BETWEEN-ITEMS-OUT-OF-LIST (SCREEN-ROWS SELF) FROM-SCREEN-ROW TO-SCREEN-ROW)
;	   (DOLIST (DELETED-SCREEN-ROW DELETED-SCREEN-ROWS)
;	     (SETF (SCREEN-BOX DELETED-SCREEN-ROW) NIL))))
;	(T
;	 ;; Oops..
;	 (BARF "The screen-rows ~S and ~S are not both part of the screen box ~S"
;	      FROM-SCREEN-ROW TO-SCREEN-ROW SELF))))

;;; Alias for the abstract :DELETE-SCREEN-OBJ methods.
;(DEFMETHOD DELETE-SCREEN-OBJ ((SELF SCREEN-ROW) SCREEN-CHA-TO-DELETE)
;  (CHECK-SCREEN-CHA-ARG SCREEN-CHA-TO-DELETE)
;  (COND ((EQ (SCREEN-ROW SCREEN-CHA-TO-DELETE) SELF)
;	 (SETF (SCREEN-ROW SCREEN-CHA-TO-DELETE) NIL)
;	 (SPLICE-ITEM-OUT-OF-LIST (SCREEN-CHAS SELF) SCREEN-CHA-TO-DELETE))
;	(T
;	 ;; Oops..
;	 (BARF "The screen-cha ~S is not part of the screen-row ~S"
;	       SCREEN-CHA-TO-DELETE SELF))))
;
;(DEFMETHOD DELETE-BETWEEN-SCREEN-OBJS ((SELF SCREEN-ROW) FROM-SCREEN-CHA TO-SCREEN-CHA)
;  (CHECK-SCREEN-CHA-ARG FROM-SCREEN-CHA)
;  (CHECK-SCREEN-CHA-ARG TO-SCREEN-CHA)
;  (COND ((AND (EQ (SCREEN-ROW FROM-SCREEN-CHA) SELF)
;	      (EQ (SCREEN-ROW TO-SCREEN-CHA) SELF))
;	 (LET ((DELETED-SCREEN-CHAS (SELF-AND-NEXT-SCREEN-CHAS FROM-SCREEN-CHA)))
;	   (SPLICE-BETWEEN-ITEMS-OUT-OF-LIST (SCREEN-CHAS SELF) FROM-SCREEN-CHA TO-SCREEN-CHA)
;	   (DOLIST (DELETED-SCREEN-CHA DELETED-SCREEN-CHAS)
;	     (SETF (SCREEN-ROW DELETED-SCREEN-CHA) NIL))))
;	(T
;	 ;; Oops..
;	 (BARF "The screen-chas ~S and ~S are not both part of the screen row ~S"
;	      FROM-SCREEN-CHA TO-SCREEN-CHA SELF))))
;
;(DEFMETHOD DELETE-SCREEN-OBJ ((SELF SCREEN-BOX) SCREEN-ROW-TO-DELETE)
;  (CHECK-SCREEN-ROW-ARG SCREEN-ROW-TO-DELETE)
;  (COND ((EQ (SCREEN-BOX SCREEN-ROW-TO-DELETE) SELF)
;	 (SETF (SCREEN-BOX SCREEN-ROW-TO-DELETE) NIL)
;	 (SPLICE-ITEM-OUT-OF-LIST (SCREEN-ROWS SELF) SCREEN-ROW-TO-DELETE))
;	(T
;	 ;; Oops..
;	 (BARF "The screen-row ~S is not part of the screen-box ~S"
;	       SCREEN-ROW-TO-DELETE SELF))))
;
;(DEFMETHOD DELETE-BETWEEN-SCREEN-OBJS ((SELF SCREEN-BOX) FROM-SCREEN-ROW TO-SCREEN-ROW)
;  (CHECK-SCREEN-ROW-ARG FROM-SCREEN-ROW)
;  (CHECK-SCREEN-ROW-ARG TO-SCREEN-ROW)
;  (COND ((AND (EQ (SCREEN-BOX FROM-SCREEN-ROW) SELF)
;	      (EQ (SCREEN-BOX TO-SCREEN-ROW) SELF))
;	 (LET ((DELETED-SCREEN-ROWS (SELF-AND-NEXT-SCREEN-ROWS FROM-SCREEN-ROW)))
;	   (SPLICE-BETWEEN-ITEMS-OUT-OF-LIST (SCREEN-ROWS SELF) FROM-SCREEN-ROW TO-SCREEN-ROW)
;	   (DOLIST (DELETED-SCREEN-ROW DELETED-SCREEN-ROWS)
;	     (SETF (SCREEN-BOX DELETED-SCREEN-ROW) NIL))))
;	(T
;	 ;; Oops..
;	 (BARF "The screen-rows ~S and ~S are not both part of the screen box ~S"
;	      FROM-SCREEN-ROW TO-SCREEN-ROW SELF))))




;;;   :KILL-SCREEN-CHA <screen-cha>
;;;   :KILL-SCREEN-ROW <screen-row>
;;;   :KILL-SCREEN-OBJ <screen-obj>
;;; These methods all cause the screen-obj which receives the message
;;; to delete <screen-obj> and all the inferior screen-objs which
;;; follow <screen-obj> from their screen inferiors.

(defmethod kill-screen-chas-from ((self screen-row) no-of-first-obj-to-kill)
  (do-vector-contents (cha-to-kill (slot-value self 'screen-chas)
				   :start no-of-first-obj-to-kill)
    (when (screen-box? cha-to-kill)
      (setf (screen-row cha-to-kill) nil)))
  (sv-delete-to-end (slot-value self 'screen-chas) no-of-first-obj-to-kill))

(defmethod kill-screen-cha ((self screen-row) screen-cha-to-kill)
  (check-screen-cha-arg screen-cha-to-kill)
  (cond ((eq (screen-row screen-cha-to-kill) self)
	 (do-self-and-next-sv-contents (cha-to-kill
					 (slot-value self 'screen-chas)
					 screen-cha-to-kill)
	   (when (screen-box? cha-to-kill)
	     (setf (screen-row cha-to-kill) nil)))
	 (sv-delete-from-item-to-end (slot-value self 'screen-chas)
				     screen-cha-to-kill))
	(t
	 ;; Oops..
	 (barf "The screen cha ~S is not part of the screen row ~S"
	       screen-cha-to-kill self))))

;;; this does the same thing except it conses a list of the killed chas
(defmethod kill-screen-chas-and-return-them ((self screen-row) screen-cha-to-kill)
  (check-screen-cha-arg screen-cha-to-kill)
  (cond ((eq (screen-row screen-cha-to-kill) self)
	 (let ((chas-to-return nil))
	   (do-self-and-next-sv-contents (cha-to-kill
					   (slot-value self 'screen-chas)
					   screen-cha-to-kill)
	     (push cha-to-kill chas-to-return)
	     (when (screen-box? cha-to-kill)
	       (setf (screen-row cha-to-kill) nil)))
	   (sv-delete-from-item-to-end (slot-value self 'screen-chas)
				       screen-cha-to-kill)
	   (nreverse chas-to-return)))
	(t
	 ;; Oops..
	 (barf "The screen cha ~S is not part of the screen row ~S"
	       screen-cha-to-kill self))))

(defmethod kill-screen-rows-from ((self screen-box) no-of-first-obj-to-kill)
  (do-vector-contents (row-to-kill (slot-value self 'screen-rows)
				   :start no-of-first-obj-to-kill)
    (setf (screen-box row-to-kill) nil))
  (sv-delete-to-end (slot-value self 'screen-rows) no-of-first-obj-to-kill))

(defmethod kill-screen-row ((self screen-box) screen-row-to-kill)
  (check-screen-row-arg screen-row-to-kill)
  (cond ((eq (screen-box screen-row-to-kill) self)
	 (do-self-and-next-sv-contents (row-to-kill
					 (slot-value self 'screen-rows)
					 screen-row-to-kill)
	   (setf (screen-box row-to-kill) nil))
	 (sv-delete-from-item-to-end (slot-value self 'screen-rows)
				     screen-row-to-kill))
	(t
	 ;; Oops..
	 (barf "The screen row ~S is not part of the screen box ~S"
	       screen-row-to-kill self))))

;;; Alias for the abstract :KILL-SCREEN-OBJ methods.
;(DEFMETHOD KILL-SCREEN-OBJ ((SELF SCREEN-ROW) SCREEN-CHA-TO-KILL)
;  (CHECK-SCREEN-CHA-ARG SCREEN-CHA-TO-KILL)
;  (COND ((EQ (SCREEN-ROW SCREEN-CHA-TO-KILL) SELF)
;	 (LET ((KILLED-SCREEN-CHAS (MEMBER SCREEN-CHA-TO-KILL (SCREEN-CHAS SELF))))
;	   (DOLIST (KILLED-SCREEN-CHA KILLED-SCREEN-CHAS)
;	     (WHEN (SCREEN-BOX? KILLED-SCREEN-CHA)
;	       (SETF (SCREEN-ROW KILLED-SCREEN-CHA) NIL)))
;	   (SPLICE-ITEM-AND-TAIL-OUT-OF-LIST (SCREEN-CHAS SELF) SCREEN-CHA-TO-KILL)))
;	(T
;	 ;; Oops..
;	 (BARF "The screen cha ~S is not part of the screen row ~S"
;	       SCREEN-CHA-TO-KILL SELF))))
;
;(DEFMETHOD KILL-SCREEN-OBJ ((SELF SCREEN-BOX) SCREEN-ROW-TO-KILL)
;  (CHECK-SCREEN-ROW-ARG SCREEN-ROW-TO-KILL)
;  (COND ((EQ (SCREEN-BOX SCREEN-ROW-TO-KILL) SELF)
;	 (LET ((KILLED-SCREEN-ROWS (MEMBER SCREEN-ROW-TO-KILL (SCREEN-ROWS SELF))))
;	   (DOLIST (KILLED-SCREEN-ROW KILLED-SCREEN-ROWS)
;	     (SETF (SCREEN-BOX KILLED-SCREEN-ROW) NIL))
;	   (SPLICE-ITEM-AND-TAIL-OUT-OF-LIST (SCREEN-ROWS SELF) SCREEN-ROW-TO-KILL)))
;	(T
;	 ;; Oops..
;	 (BARF "The screen row ~S is not part of the screen box ~S"
;	       SCREEN-ROW-TO-KILL SELF))))


;;; LOW-LEVEL screen-obj accessors. All of these do the obvious thing.
#+lispm(compiler:make-obsolete SCREEN-OBJS-AT-AND-AFTER
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD SCREEN-OBJS-AT-AND-AFTER ((SELF SCREEN-ROW) NO-OF-FIRST-OBJ)
;  (NTHCDR NO-OF-FIRST-OBJ (SCREEN-CHAS SELF)))

#+lispm(compiler:make-obsolete SCREEN-OBJS-AFTER
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD SCREEN-OBJS-AFTER ((SELF SCREEN-ROW) NO-OF-FIRST-OBJ)
;  (NTHCDR (+ 1 NO-OF-FIRST-OBJ) (SCREEN-CHAS SELF)))

(defmethod screen-chas-length ((self screen-row))
  (storage-vector-active-length (slot-value self 'screen-chas)))

(defmethod screen-rows-length ((self screen-box))
  (storage-vector-active-length (slot-value self 'screen-rows)))

(defmethod first-screen-cha ((self screen-row))
  (unless (zerop& (storage-vector-active-length (slot-value self 'screen-chas)))
    (sv-nth 0 (slot-value self 'screen-chas))))

(defmethod last-screen-cha ((self screen-row))
  (sv-nth (1-& (storage-vector-active-length
		 (slot-value self 'screen-chas)))
	  (slot-value self 'screen-chas)))

;; screen-rows can be a symbol (port ellipsis style)
(defmethod first-screen-row ((self screen-box))
  (let ((srs (slot-value self 'screen-rows)))
    (unless (or (symbolp srs) (zerop& (storage-vector-active-length srs)))
      (sv-nth 0 (slot-value self 'screen-rows)))))

(defmethod last-screen-row ((self screen-box))
  (let ((srs (slot-value self 'screen-rows)))
    (unless (or (symbolp srs) (zerop& (storage-vector-active-length srs)))
      (sv-nth (1-& (storage-vector-active-length (slot-value self 'screen-rows)))
	      (slot-value self 'screen-rows)))))

(defmethod first-screen-row ((self graphics-screen-box))
  ;(declare (ignore self))
  nil)

(defmethod last-screen-row ((self graphics-screen-box))
  ;(declare (ignore self))
  nil)

(defmethod first-screen-row ((self sprite-screen-box))
  ;(declare (ignore self))
  nil)

(defmethod last-screen-row ((self sprite-screen-box))
  ;(declare (ignore self))
  nil)

(defmethod screen-cha-at-cha-no ((self screen-row) cha-no)
  (when (<& cha-no
	    (storage-vector-active-length
	      (slot-value self 'screen-chas)))
    (sv-nth cha-no (slot-value self 'screen-chas))))

(defmethod screen-cha-cha-no ((self screen-row) cha)
  (sv-place cha (slot-value self 'screen-chas)))

(defmethod screen-row-at-row-no ((self screen-box) row-no)
  (when (<& row-no
	    (storage-vector-active-length
	      (slot-value self 'screen-rows)))
    (sv-nth row-no (slot-value self 'screen-rows))))

(defmethod screen-row-row-no ((self screen-box) row)
  (sv-place row (slot-value self 'screen-rows)))

;;; Graphics-screen-box accessors
;;; since graphics boxes have NO rows we use the SCREEN-ROWS instance variable
;;; which should be renamed immediate inferiors or some such to reflect the fact
;;; that it can contain SHEETS
(DEFMETHOD SCREEN-SHEET ((SELF GRAPHICS-SCREEN-BOX))
  (SCREEN-ROWS SELF))

(defmethod set-screen-sheet ((self graphics-screen-box) new-sheet)
  (setf (screen-rows self) new-sheet)
  (cond ((null new-sheet))
	((graphics-screen-sheet? new-sheet)
	 (setf (graphics-screen-sheet-screen-box new-sheet) self))
	(t (error "Trying to set the screen-sheet for ~A to ~A"
		  self new-sheet))))

;;; just go to the actual obj instead ?
(defmethod screen-sprite ((self sprite-screen-box))
  (screen-rows self))


;;;obselete no one should be calling these;;;;;;;;;;;;;;;;
#+lispm(compiler:make-obsolete NEXT-SCREEN-CHA
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD NEXT-SCREEN-CHA ((SELF SCREEN-CHAR-SUBCLASS) )
;  (CADR (SELF-AND-NEXT-SCREEN-CHAS SELF)))

#+lispm(compiler:make-obsolete NEXT-SCREEN-CHAS
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD NEXT-SCREEN-CHAS ((SELF SCREEN-CHAR-SUBCLASS) )
;  (CDR (SELF-AND-NEXT-SCREEN-CHAS SELF)))

#+lispm(compiler:make-obsolete SELF-AND-NEXT-SCREEN-CHAS
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD SELF-AND-NEXT-SCREEN-CHAS ((SELF SCREEN-CHAR-SUBCLASS) )
;  (MEMBER SELF (SCREEN-CHAS (SCREEN-ROW SELF))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+lispm(compiler:make-obsolete NEXT-SCREEN-OBJ
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD NEXT-SCREEN-OBJ ((SELF SCREEN-CHAR-SUBCLASS))
;  (CADR (SELF-AND-NEXT-SCREEN-CHAS SELF)))

#+lispm(compiler:make-obsolete NEXT-SCREEN-OBJS
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD NEXT-SCREEN-OBJS ((SELF SCREEN-CHAR-SUBCLASS))
;  (CDR (SELF-AND-NEXT-SCREEN-CHAS SELF)))

#+lispm(compiler:make-obsolete SELF-AND-NEXT-SCREEN-OBJS
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD SELF-AND-NEXT-SCREEN-OBJS ((SELF SCREEN-CHAR-SUBCLASS))
;  (MEMBER SELF (SCREEN-CHAS (SCREEN-ROW SELF))))

#+lispm(compiler:make-obsolete NEXT-SCREEN-ROW
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD NEXT-SCREEN-ROW ((SELF SCREEN-ROW))
;  (CADR (SELF-AND-NEXT-SCREEN-ROWS SELF)))

#+lispm(compiler:make-obsolete NEXT-SCREEN-ROWS
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD NEXT-SCREEN-ROWS ((SELF SCREEN-ROW))
;  (CDR (SELF-AND-NEXT-SCREEN-ROWS SELF)))

#+lispm(compiler:make-obsolete SELF-AND-NEXT-SCREEN-ROWS
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD SELF-AND-NEXT-SCREEN-ROWS ((SELF SCREEN-ROW) )
;  (MEMBER SELF (SCREEN-ROWS (SCREEN-BOX SELF))))

#+lispm(compiler:make-obsolete NEXT-SCREEN-OBJ
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD NEXT-SCREEN-OBJ ((SELF SCREEN-ROW))
;  (CADR (SELF-AND-NEXT-SCREEN-ROWS SELF)))

#+lispm(compiler:make-obsolete NEXT-SCREEN-OBJS
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD NEXT-SCREEN-OBJS ((SELF SCREEN-ROW))
;  (CDR (SELF-AND-NEXT-SCREEN-ROWS SELF)))

#+lispm(compiler:make-obsolete SELF-AND-NEXT-SCREEN-OBJS
			       "Screen Object Inferiors are no longer lists !!")
;(DEFMETHOD SELF-AND-NEXT-SCREEN-OBJS ((SELF SCREEN-ROW) )
;  (MEMBER SELF (SCREEN-ROWS (SCREEN-BOX SELF))))

(DEFMETHOD INFERIORS ((SCREEN-OBJ SCREEN-ROW))
  (SLOT-VALUE SCREEN-OBJ 'SCREEN-CHAS))
(DEFMETHOD INFERIORS ((SCREEN-OBJ SCREEN-BOX))
  (SLOT-VALUE  SCREEN-OBJ 'SCREEN-ROWS))

(DEFMETHOD SUPERIOR ((SCREEN-OBJ SCREEN-ROW))
  (SLOT-VALUE SCREEN-OBJ 'SCREEN-BOX))
(DEFMETHOD SUPERIOR ((SCREEN-OBJ SCREEN-CHAR-SUBCLASS))
  (SLOT-VALUE SCREEN-OBJ 'SCREEN-ROW))

(DEFMETHOD SUPERIOR-SCREEN-BOX ((SCREEN-OBJ SCREEN-ROW))
  (SCREEN-BOX SCREEN-OBJ))

(DEFMETHOD FIRST-SCREEN-OBJ ((SCREEN-OBJ SCREEN-ROW))
  (CAR (SLOT-VALUE SCREEN-OBJ 'SCREEN-CHAS)))
(DEFMETHOD FIRST-SCREEN-OBJ ((SCREEN-OBJ SCREEN-BOX))
  (CAR (SLOT-VALUE SCREEN-OBJ 'SCREEN-ROWS)))


(DEFMETHOD SCREEN-BOX ((SELF SCREEN-CHAR-SUBCLASS))
  (LET ((SR (SCREEN-ROW SELF)))
    (IF (SCREEN-ROW? SR) (SCREEN-BOX SR) SR)))

(DEFMETHOD SCREEN-BOX ((SELF SCREEN-BOX))
  (SUPERIOR-SCREEN-BOX SELF))

(DEFMETHOD LOWEST-SCREEN-BOX ((SELF SCREEN-CHAR-SUBCLASS))
  (LOWEST-SCREEN-BOX (SCREEN-ROW SELF)))

(DEFMETHOD LOWEST-SCREEN-BOX ((SELF SCREEN-ROW))
  (SCREEN-BOX SELF))

(DEFMETHOD LOWEST-SCREEN-BOX ((SELF SCREEN-BOX))
  SELF)

(defmethod offsets ((self screen-obj))
  (values (slot-value self 'x-offset)
	  (slot-value self 'y-offset)))

(defmethod set-offsets ((self screen-obj) new-x-offset new-y-offset)
  (setf (slot-value self 'x-offset) new-x-offset)
  (setf (slot-value self 'y-offset) new-y-offset))

;;; Changing from/to SCREEN-BOXES and GRAPHICS-SCREEN-BOXES

;(DEFMETHOD (SCREEN-BOX :BEFORE :SET-FLAVOR) (IGNORE)
;  (DOLIST (SCR-ROW SCREEN-ROWS)
;    (setf (SCREEN-BOX SCR-ROW) NIL)
;    (DEALLOCATE-SELF SCR-ROW)
;    (SETQ SCREEN-ROWS NIL)))
;
;(DEFMETHOD (GRAPHICS-SCREEN-BOX :BEFORE :SET-FLAVOR) (IGNORE)
;  (LET ((GRAPHICS-SHEET (AND SCREEN-ROWS (GRAPHICS-SCREEN-SHEET-ACTUAL-OBJ SCREEN-ROWS))))
;    (UNLESS (NULL GRAPHICS-SHEET)
;      (SETF (GRAPHICS-SHEET-SCREEN-OBJS GRAPHICS-SHEET)
;	    (DELETE (ASSOC SELF (GRAPHICS-SHEET-SCREEN-OBJS GRAPHICS-SHEET))
;		  (GRAPHICS-SHEET-SCREEN-OBJS GRAPHICS-SHEET))))
;    (SETQ SCREEN-ROWS NIL)))

;;; Methods that support the interaction between BP's and SCREEN BOXEs

(DEFMETHOD SET-BPS ((SELF SCREEN-BOX) NEW-VALUE)
  (ASSERT #+ti '(LAMBDA (X) (AND (LISTP X) (EVERY X 'BP?)))
	  #-ti #'(LAMBDA (X) (AND (LISTP X) (EVERY X 'BP?)))
	  (NEW-VALUE)
	  "A list of Boxer BP's")
  (SETF (BPS SELF) NEW-VALUE))

(DEFMETHOD ADD-BP ((SELF SCREEN-BOX) NEW-BP)
  (CHECK-BP-ARG NEW-BP)
  (PUSH NEW-BP (BPS SELF)))

(DEFMETHOD DELETE-BP ((SELF SCREEN-BOX) BP)
  (CHECK-BP-ARG BP)
  (SETF (BPS SELF) (DELETE BP (BPS SELF))))
