;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER ;-*-

#|


 $Header: lodisp.lisp,v 1.0 90/01/24 22:14:12 boxer Exp $

 $Log:	lodisp.lisp,v $
;;;Revision 1.0  90/01/24  22:14:12  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

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
;; (defmethod show-infs ((self screen-row))
;;   (dotimes (i (storage-vector-active-length (slot-value self 'screen-chas)))
;;     (format t " ~S " (sv-nth i (slot-value self 'screen-chas)))))

;; (defmethod show-infs ((self screen-box))
;;   (dotimes (i (storage-vector-active-length (slot-value self 'screen-rows)))
;;     (format t " ~S " (sv-nth i (slot-value self 'screen-rows)))))

; sgithens TODO ... this defun doesn't seem to be anywhere, unless it's generated
; somewhere by a macro
;; (defun check-screen-row-arg (screen-row) t)

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

;; sgithens TODO 2024-04-17 appears to no longer be used...
;; (defmethod insert-screen-cha-at-cha-no ((self screen-row) new-screen-cha cha-no)
;;   (sv-insert-at (slot-value self 'screen-chas) cha-no new-screen-cha)
;;   (when (screen-box? new-screen-cha)
;;     (setf (screen-row new-screen-cha) self)))

;; (defmethod insert-screen-chas-at-cha-no ((self screen-row)
;;                                          new-screen-chas-list cha-no)
;;   (sv-multiple-insert-at (slot-value self 'screen-chas) cha-no
;;                          new-screen-chas-list)
;;   (dolist (new-screen-cha new-screen-chas-list)
;;     (when (screen-box? new-screen-cha)
;;       (setf (screen-row new-screen-cha) self))))

;; (defmethod insert-screen-row ((self screen-box) new-screen-row before-screen-row)
;;   (check-screen-row-arg new-screen-row)
;;   (cond ((null (screen-box new-screen-row))
;;          (setf (screen-box new-screen-row) self)
;;          (if (not-null before-screen-row)
;;            (sv-insert-before (slot-value self 'screen-rows)
;;                              new-screen-row before-screen-row)
;;            (sv-append (slot-value self 'screen-rows) new-screen-row)))
;;     (t
;;      (barf "The screen-row ~S is already part of ~S"
;;            new-screen-row (screen-box new-screen-row)))))


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

;; sgithens TODO 2024-04-17 appears to no longer be used...
;; (defmethod append-screen-row ((self screen-box) new-screen-row)
;;   (check-screen-row-arg new-screen-row)
;;   (cond ((null (screen-box new-screen-row))
;;          (setf (screen-box new-screen-row) self)
;;          (sv-append (slot-value self 'screen-rows) new-screen-row))
;;     (t
;;      ;; Oops..
;;      (barf "The screen row ~s is already part of ~S"
;;            new-screen-row (screen-box new-screen-row)))))


;;;   :DELETE-SCREEN-CHA <screen-cha>
;;;   :DELETE-SCREEN-ROW <screen-row>
;;;   :DELETE-SCREEN-OBJ <screen-obj>
;;; These methods all cause the screen-obj which receives the message
;;; to delete <screen-obj> from their screen inferiors. To help with
;;; deleting multiple inferior screen objs, these methods have variants
;;; (called :delete-between-screen-objs <from-screen-obj> <to-screen-obj>
;;; which delete all the inferior screen-objs between <from-screen-obj>
;;; (inclusive) and <to-screen-obj> (exclusive).

;; sgithens TODO 2024-04-17 appears to no longer be used...
;; (defmethod delete-screen-cha-at-cha-no ((self screen-row) cha-no)
;;   (let ((cha-to-delete (sv-nth cha-no (slot-value self 'screen-chas))))
;;     (sv-delete-at (slot-value self 'screen-chas) cha-no)
;;     (when (screen-box? cha-to-delete)
;;       (setf (screen-row cha-to-delete) nil))))

;; (defmethod delete-screen-chas-from-to ((self screen-row) from-cha-no to-cha-no)
;;   (do-vector-contents (cha-to-delete (slot-value self 'screen-chas)
;;                                      :start from-cha-no :stop to-cha-no)
;;     (when (screen-box? cha-to-delete)
;;       (setf (screen-row cha-to-delete) nil)))
;;   (sv-delete-from-to (slot-value self 'screen-chas) from-cha-no to-cha-no))

;; (defmethod delete-screen-row ((self screen-box) screen-row-to-delete)
;;   (check-screen-row-arg screen-row-to-delete)
;;   (cond ((eq (screen-box screen-row-to-delete) self)
;;          (setf (screen-box screen-row-to-delete) nil)
;;          (sv-delete-item (slot-value self 'screen-rows) screen-row-to-delete))
;;     (t
;;      ;; Oops..
;;      (barf "The screen-row ~S is not part of the screen-box ~S"
;;            screen-row-to-delete self))))

;; (defmethod delete-screen-rows-from-to ((self screen-box
;;                                              ) from-row-no to-row-no)
;;   (do-vector-contents (row-to-delete (slot-value self 'screen-rows)
;;                                      :start from-row-no :stop to-row-no)
;;     (setf (screen-box row-to-delete) nil))
;;   (sv-delete-from-to (slot-value self 'screen-rows) from-row-no to-row-no))


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

;; sgithens TODO 2024-04-17 appears to no longer be used...
;; (defmethod kill-screen-cha ((self screen-row) screen-cha-to-kill)
;;   (check-screen-cha-arg screen-cha-to-kill)
;;   (cond ((eq (screen-row screen-cha-to-kill) self)
;;          (do-self-and-next-sv-contents (cha-to-kill
;;                                         (slot-value self 'screen-chas)
;;                                         screen-cha-to-kill)
;;            (when (screen-box? cha-to-kill)
;;              (setf (screen-row cha-to-kill) nil)))
;;          (sv-delete-from-item-to-end (slot-value self 'screen-chas)
;;                                      screen-cha-to-kill))
;;     (t
;;      ;; Oops..
;;      (barf "The screen cha ~S is not part of the screen row ~S"
;;            screen-cha-to-kill self))))

;;; this does the same thing except it conses a list of the killed chas
;; (defmethod kill-screen-chas-and-return-them ((self screen-row) screen-cha-to-kill)
;;   (check-screen-cha-arg screen-cha-to-kill)
;;   (cond ((eq (screen-row screen-cha-to-kill) self)
;;          (let ((chas-to-return nil))
;;            (do-self-and-next-sv-contents (cha-to-kill
;;                                           (slot-value self 'screen-chas)
;;                                           screen-cha-to-kill)
;;              (push cha-to-kill chas-to-return)
;;              (when (screen-box? cha-to-kill)
;;                (setf (screen-row cha-to-kill) nil)))
;;            (sv-delete-from-item-to-end (slot-value self 'screen-chas)
;;                                        screen-cha-to-kill)
;;            (nreverse chas-to-return)))
;;     (t
;;      ;; Oops..
;;      (barf "The screen cha ~S is not part of the screen row ~S"
;;            screen-cha-to-kill self))))

(defmethod kill-screen-rows-from ((self screen-box) no-of-first-obj-to-kill)
  (do-vector-contents (row-to-kill (slot-value self 'screen-rows)
                                   :start no-of-first-obj-to-kill)
    (setf (screen-box row-to-kill) nil))
  (sv-delete-to-end (slot-value self 'screen-rows) no-of-first-obj-to-kill))

(defmethod kill-screen-row ((self screen-box) screen-row-to-kill)
  ;; (check-screen-row-arg screen-row-to-kill)
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


;;; LOW-LEVEL screen-obj accessors. All of these do the obvious thing.

(defmethod screen-chas-length ((self screen-row))
  (storage-vector-active-length (slot-value self 'screen-chas)))

(defmethod screen-rows-length ((self screen-box))
  ;; There is an issue where sometimes boxer::*box-ellipsis-current-style* is used
  ;; has a marker variable in the screen-rows slot instead of an actual screen-rows
  ;; object. When that happens, this method shouldn't be called, but sometimes it still
  ;; does. sgithens 2020-10-06
  (if (vectorp (slot-value self 'screen-rows))
    (storage-vector-active-length (slot-value self 'boxer::screen-rows))
    0))

;; sgithens TODO 2024-04-17 appears to no longer be used...
;; (defmethod first-screen-cha ((self screen-row))
;;   (unless (zerop& (storage-vector-active-length (slot-value self 'screen-chas)))
;;     (sv-nth 0 (slot-value self 'screen-chas))))

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

;; sgithens TODO 2024-04-17 appears to no longer be used...
;; (defmethod screen-cha-at-cha-no ((self screen-row) cha-no)
;;   (when (<& cha-no
;;             (storage-vector-active-length
;;              (slot-value self 'screen-chas)))
;;     (sv-nth cha-no (slot-value self 'screen-chas))))

;; (defmethod screen-cha-cha-no ((self screen-row) cha)
;;   (sv-place cha (slot-value self 'screen-chas)))

(defmethod screen-row-at-row-no ((self screen-box) row-no &optional actual-obj)
  (let ((togo nil))
    (cond
      ((< row-no (storage-vector-active-length (slot-value self 'screen-rows)))
       (setf togo (sv-nth row-no (slot-value self 'screen-rows)))
       (unless togo
         (setf togo (make-instance 'screen-row))
         (re-init togo actual-obj)
        ;;  (setf (screen-box togo) actual-obj)
         (sv-insert-at (slot-value self 'screen-rows) row-no togo))
      )
      (t ;; we'll need to increase the vector length
       (resize-storage-vector (screen-rows self) (1+ row-no))
       (setf togo (make-instance 'screen-row))
       (re-init togo actual-obj)
      ;;  (setf (screen-box togo) actual-obj)
       (sv-insert-at (slot-value self 'screen-rows) row-no togo)
      ))
    togo
    )

  ;; (when (< row-no
  ;;          (storage-vector-active-length
  ;;           (slot-value self 'screen-rows)))
  ;;   (sv-nth row-no (slot-value self 'screen-rows)))
    )

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
