;; -*- Mode:LISP; Package:(BOXER LISP 1000); Base:10.-*-
#|


 $Header: infsup.lisp,v 1.0 90/01/24 22:13:08 boxer Exp $

 $Log:	infsup.lisp,v $
;;;Revision 1.0  90/01/24  22:13:08  boxer
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




  This file contains low-level code which deals with the inferior/superior
  relations between primitive Boxer objects. These relations include the
  connection/disconnection of primitive Boxer objects from their superiors
  and from groups of co-inferiors.



Modification History (most recent at the top)

 2/11/03 merged current LW and MCL files
 2/15/01 merged current LW and MCL files
11/15/00 compact-fds hacks fonted empty row case. Make sure compact-fds is called
         last in chas-array-delete-cha and chas-array-insert-cha
10/23/99 char-array-insert-bfd: fixed bug when adding a BFD with a higher cha-no
         to a chas-array with existing BFD's
 5/06/99 chas-array-move-chas special case for bfd at 0
 7/30/98 copy-fd-at is a convenience for copying FD values at a new cha-no
         change chas-array-move-chas and insert-cha-at-cha-no to use it
 5/11/98 changed compact-fds to prefer rightward FD's to fix rubout bug
 5/04/98 more font fixes, chas-array-move-chas, insert-cha-at-cha-no
 5/03/98 chas-array-slide-fds no longer compacts use compact-fds instead
 4/30/98 chas-array-insert-bfd didn't hack the case where the new FD goes 1st
 4/29/98 insert-cha-at-cha-no: lots o' fixes
         chas-array-insert-fd hacks colors now.  Also, just sets to new font
         instead of logior'ing
 4/27/98 Started Logging changes: source = boxer version 2.2.r4 + changes for fonts


|#

(in-package :boxer)

;;; Rows have a fairly hairy scheme for keeping track of their chas, the order
;;; they are in etc. The main data structure used to implement this scheme is
;;; the CHAS-ARRAY. Chas-Arrays are just what their name says, arrays of chas.
;;; In addition chas-arrays keep track of all the BPs that point to the chas
;;; in them so that whenever there is a change to a chas-array, those bps can
;;; be updated to account for the change. One way of thinking of chas-arrays
;;; is as Lispm Strings which are just arrays of Lispm character codes.

(defvar *chas-array-default-size* 32.)

(defstruct (chas-array (:type vector)
           (:include storage-vector)
           (:constructor %make-chas-array)
           (:copier nil) ;; we do this ourselves
           )
  (bps nil)
  (fds nil))


(defun make-chas-array (&optional (length *chas-array-default-size*))
  (%make-chas-array :contents (allocate-c-vector (max& length 8))))

;;; emulate some of the old accessors as macros
(defmacro chas-array-chas (chas-array)
  `(%sv-contents ,chas-array))

(defmacro chas-array-active-length (chas-array)
  `(%sv-fill-pointer ,chas-array))


;;; Instead of DEFTYPE-CHECKING-MACROS because the explicit type
;;; declaration means that CHAS-ARRAY's aren't in the type lattice
(defun chas-array? (thing)
  (and (simple-vector-p thing)
       (simple-vector-p (%sv-contents thing))))

(defmacro check-chas-array-arg (thing)
  `(check-type ,thing (satisfies chas-array?) "A Character Array"))

(defmacro with-fast-chas-array-manipulation ((chas-array name) &body body)
  `(let ((,name (chas-array-chas ,chas-array)))
     . , body))

(defun check-chas-array-old-cha-no-arg (chas-array arg)
  (check-chas-array-arg chas-array)
  (let ((active-length (chas-array-active-length chas-array)))
    (cond ((and (integerp arg) (>=& arg 0) (<& arg active-length)))
    (t
     (barf "Bad Arg, ~A." arg)))))

(defun check-chas-array-new-cha-no-arg (chas-array arg)
  (check-chas-array-arg chas-array)
  (let ((active-length (chas-array-active-length chas-array)))
    (cond ((and (integerp arg) (>=& arg 0) (<=& arg active-length)))
    (t
     (barf "Bad Arg, ~A" arg)))))

(defmacro fast-chas-array-get-cha (chas cha-no)
  `(svref& ,chas ,cha-no))

(defmacro fast-chas-array-set-cha (chas cha-no new-value)
  `(setf (svref& ,chas ,cha-no) ,new-value))

(defmacro fast-chas-array-room (chas)
  `(length (the simple-vector ,chas)))

(defsubst chas-array-get-cha (chas-array cha-no)
  (fast-chas-array-get-cha (chas-array-chas chas-array) cha-no))

(defsubst chas-array-set-cha (chas-array cha-no new-value)
  (fast-chas-array-set-cha (chas-array-chas chas-array) cha-no new-value))

(defsubst chas-array-room (chas-array)
  (fast-chas-array-room (chas-array-chas chas-array)))

(defsubst chas-array-assure-room (chas-array required-room)
  (sv-assure-room chas-array required-room))

;;; CHAS-ARRAY-SLIDE-CHAS the primitive function that functions which need to
;;; slide chas around in a chas-array should call. This function takes care of
;;; adjusting the BPs that point to the chas-array to compensate for the slide.
;;; This function also takes care of assuring that there is enough room in the
;;; chas-array to perform the slide. Like all functions which may need to make
;;; a new chas-array, chas-array-slide-chas always returns the (new) chas-array


(defun chas-array-slide-chas (chas-array strt-cha-no distance)
  (let ((old-active-length (chas-array-active-length chas-array)))
    (chas-array-assure-room chas-array (+& old-active-length distance))
    (cond ((and (plusp& distance) (not (>=& strt-cha-no old-active-length)))
     (chas-array-slide-chas-pos chas-array strt-cha-no
              distance old-active-length)
           (chas-array-slide-fds chas-array strt-cha-no distance))
    ((minusp& distance)
     (chas-array-slide-chas-neg chas-array strt-cha-no
              distance old-active-length)
           (chas-array-slide-fds chas-array strt-cha-no distance)))
    (chas-array-slide-bps chas-array strt-cha-no distance)))

(defun chas-array-slide-chas-pos (chas-array strt-cha-no
          distance old-active-length)
  (do ((chas (chas-array-chas chas-array))
       (orig-cha-no (-& old-active-length 1) (-& orig-cha-no 1)))
      ((<& orig-cha-no strt-cha-no))
    (fast-chas-array-set-cha chas
           (+& orig-cha-no distance)
           (fast-chas-array-get-cha chas orig-cha-no))))

(defun chas-array-slide-chas-neg (chas-array strt-cha-no
          distance old-active-length)
  (do ((chas (chas-array-chas chas-array))
       (orig-cha-no strt-cha-no (+& orig-cha-no 1)))
      ((>=& orig-cha-no old-active-length))
    (fast-chas-array-set-cha chas
      (+& orig-cha-no distance)
      (fast-chas-array-get-cha chas orig-cha-no))))

(defun chas-array-slide-bps (chas-array strt-cha-no distance)
  (dolist (bp (chas-array-bps chas-array))
    (cond ((or (>& (bp-cha-no bp) strt-cha-no)
         (and (=& (bp-cha-no bp) strt-cha-no)
        (eq (bp-type bp) ':moving)))
     (incf& (bp-cha-no bp) distance)))))

(defun chas-array-slide-fds (chas-array strt-cha-no distance)
  (unless (null (chas-array-fds chas-array))
      (dolist (fd (chas-array-fds chas-array))
        (cond ((>=& (bfd-cha-no fd) strt-cha-no)
         (incf& (bfd-cha-no fd) distance)))))
    ;; now remove any Font Descriptors that have coalesced
  ; use compact-fds at some later time to avoid premature removal of a
  ; FD that chas-array-move-chas might need
;    (setf (chas-array-fds chas-array)
;	  (delete-duplicates (chas-array-fds chas-array)
;			      :test #'(lambda (fd1 fd2)
;					(=& (bfd-cha-no fd1)
;					    (bfd-cha-no fd2)))))
  )

;;; CHAS-ARRAY-INSERT-CHA-1 is an internal function used by all of the
;;; functions which insert chas into a chas-array. Functions which want
;;; to call this function must have taken care of sliding the chas from
;;; the insert position on out of the way, and must also take care of
;;; updating the chas-array's active-length. This exists as a seperate
;;; function so that functions which do multiple insert-chas can avoid
;;; multiple calls to chas-array-slide-chas

(defsubst chas-array-insert-cha-1 (into-chas-array cha-no cha)
  (chas-array-set-cha into-chas-array cha-no cha))

;;; CHAS-ARRAY-INSERT-CHA is the correct function to call to insert a
;;; cha into a chas array. It does everything that needs to be done,
;;; specifically:
;;;  - It type checks the chas-array and the cha-no.
;;;  - It slides the chas following the insert point out
;;;    of the way.
;;;  - It makes the correct call to chas-array-insert-cha-1.
;;;  - It icrements the chas-array's active length.

(defun chas-array-insert-cha (into-chas-array cha-no cha)
  (check-chas-array-new-cha-no-arg into-chas-array cha-no)
  (chas-array-slide-chas into-chas-array cha-no 1)
  (chas-array-insert-cha-1 into-chas-array cha-no cha)
  (incf& (chas-array-active-length into-chas-array) 1)
  (compact-fds into-chas-array))

;;; CHAS-ARRAY-DELETE-CHA is the correct function to call to delete a
;;; cha from a chas-array. It does everything that needs to be done,
;;; specifically:
;;;  - It type checks the chas-array, and the cha-no.
;;;  - It slides the chas following the delete point over
;;;    to delete that cha.
;;;  - It tells the cha about its new-superior-row.
;;;  - It decrements the chas-array's active-length.

(defun chas-array-delete-cha (from-chas-array cha-no)
  (check-chas-array-old-cha-no-arg from-chas-array cha-no)
  (chas-array-slide-chas from-chas-array (+& cha-no 1) -1)
  (decf& (chas-array-active-length from-chas-array) 1)
  ;; NOTE: these were in reverse order for LW version during 2/11/03 src merge
  (compact-fds from-chas-array))

;;; CHAS-ARRAY-MOVE-CHAS is the fundamental function used to move chas
;;; from one chas-array to another chas-array. This function takes care
;;; of doing everything that needs to be done when moving groups of chas
;;; from one chas-array to another chas-array, specifically:
;;;  - It type checks both chas-arrays, and the cha-nos
;;;    in those arrays.
;;;  - It takes care of moving the chas, and adjusting the
;;;    active-lengths of the two chas-arrays.
;;;  - It takes care of moving and adjusting the BPs and Font
;;;    Descriptors  that pointed to the moved chas.

;; convenience for copying FD values at a new cha-no
(defun copy-fd-at (fd cha-no) (make-cfd cha-no (bfd-font-no fd) (bfd-color fd)))

(defun chas-array-move-chas (from-chas-array from-chas-array-strt-cha-no
           into-chas-array into-chas-array-strt-cha-no
           no-of-chas-to-move superior-row)
  (let ((from-chas-array-stop-cha-no (+& from-chas-array-strt-cha-no
           no-of-chas-to-move))
        ;; these have to calculated here BEFORE any FD's are slid around
        (from-starting-bfd (closest-bfd-internal (chas-array-fds from-chas-array)
                                                 from-chas-array-strt-cha-no))
        (into-starting-bfd (closest-bfd-internal (chas-array-fds into-chas-array)
                                                 into-chas-array-strt-cha-no)))
    ;; First we be real good and check all our args like we promised.
    (check-chas-array-new-cha-no-arg from-chas-array
             from-chas-array-strt-cha-no)
    (check-chas-array-new-cha-no-arg from-chas-array
             from-chas-array-stop-cha-no)
    (check-chas-array-new-cha-no-arg into-chas-array
             into-chas-array-strt-cha-no)
    ;; make room for the new characters
    (chas-array-slide-chas into-chas-array
                           into-chas-array-strt-cha-no no-of-chas-to-move)
    ;; actually move the characters from one chas-array to the other
    (dotimes& (cha-no no-of-chas-to-move)
      (let ((from-cha-no (+& from-chas-array-strt-cha-no cha-no))
      (into-cha-no (+& into-chas-array-strt-cha-no cha-no)))
  (chas-array-insert-cha-1 into-chas-array
         into-cha-no
         (chas-array-get-cha from-chas-array
                 from-cha-no))))
    ;; handle the gap left by the characters that were moved
    (chas-array-slide-chas
     from-chas-array from-chas-array-stop-cha-no (-& no-of-chas-to-move))

    ;; Adjust the fill pointers of the 2 chas-arrays
    (incf& (chas-array-active-length into-chas-array) no-of-chas-to-move)
    (decf& (chas-array-active-length from-chas-array) no-of-chas-to-move)

    ;; move any BP's
    (dolist (bp (chas-array-bps from-chas-array))
      (let ((bp-cha-no (bp-cha-no bp)))
  (cond ((or (and (>& bp-cha-no from-chas-array-strt-cha-no)
      (<& bp-cha-no (-& from-chas-array-stop-cha-no 1)))
       (and (=& bp-cha-no from-chas-array-strt-cha-no)
      (eq (bp-type bp) ':moving)))
         (move-bp-1 bp superior-row
        (+& into-chas-array-strt-cha-no
            (-& bp-cha-no from-chas-array-strt-cha-no)))))))
    ;; handle FD's we may have to add extra FD's if the beginning of the
    ;; moved section in not coincidental with an earlier BFD and to preserve the
    ;; fontness of the chas AFTER the inserted row-chas
    (unless (bfd= from-starting-bfd into-starting-bfd)
      ;; make sure the newly inserted chas start with the same BFD they did
      ;; when they were in the from-chas-array
      (chas-array-insert-bfd into-chas-array
                             (copy-fd-at from-starting-bfd
                                         into-chas-array-strt-cha-no)))
    ;; now move all the FD's in the section being moved
    (dolist (bfd (chas-array-fds from-chas-array))
      (let ((bfd-cha-no (bfd-cha-no bfd)))
        (cond ((>& bfd-cha-no (-& from-chas-array-stop-cha-no 1))
               (return))
              ((and (zerop& bfd-cha-no) (zerop& from-chas-array-strt-cha-no))
               ;; special case: leave the bfd, but copy it for the other row also
               (chas-array-insert-bfd into-chas-array
                                      (copy-fd-at bfd
                                                  into-chas-array-strt-cha-no)))
              ((>=& bfd-cha-no from-chas-array-strt-cha-no)
               (chas-array-delete-bfd from-chas-array bfd)
               (setf (bfd-cha-no bfd)
                     (+& into-chas-array-strt-cha-no
                         (-& bfd-cha-no from-chas-array-strt-cha-no)))
               (chas-array-insert-bfd into-chas-array bfd)))))
    ;; now make sure the chas in the into-chas-array which have been pushed
    ;; down the row start with the same BFD that they did before
    (let ((into-chas-array-stop-cha-no (+& into-chas-array-strt-cha-no
                                           no-of-chas-to-move)))
      (unless (or (=& into-chas-array-strt-cha-no into-chas-array-stop-cha-no)
                  (bfd= into-starting-bfd
                        (closest-bfd-internal (chas-array-fds into-chas-array)
                                              into-chas-array-stop-cha-no)))
        (chas-array-insert-bfd into-chas-array
                               (copy-fd-at into-starting-bfd
                                           into-chas-array-stop-cha-no))))
    ;; it is safe to compact FD's now
    (compact-fds into-chas-array) (compact-fds from-chas-array)))

(defun compact-fds (chas-array)
  (let* ((last-fd nil) (unneeded-fds nil))
    (dolist (fd (chas-array-fds chas-array))
      (unless (null last-fd)
        (cond ((bfd= last-fd fd) (push fd unneeded-fds))
              ((<=& (bfd-cha-no fd) (bfd-cha-no last-fd))
               ;; prefer the last-fd EXCEPT for empty row case
               (if (and (zerop (bfd-cha-no fd))
                        (zerop (chas-array-active-length chas-array)))
                 (push fd unneeded-fds)
                 (push last-fd  unneeded-fds)))))
      (setq last-fd fd))
    (dolist (ufd unneeded-fds) (chas-array-delete-bfd chas-array ufd))))


;;; Methods that support the interaction between rows and BP's.
;;; and between rows and Font Descriptors

(defmethod bps ((self row))
  (chas-array-bps (slot-value self 'chas-array)))

(defmethod set-bps ((self row) new-value)
  (assert (and (listp new-value) (every #'bp? new-value)) (new-value)
    "A list of Boxer BP's")
  (setf (chas-array-bps (slot-value self 'chas-array)) new-value))

(defmethod add-bp ((self row) bp)
  (check-bp-arg bp)
  (let ((chas-array (slot-value self 'chas-array)))
    (unless (fast-memq bp (chas-array-bps chas-array))
      (push bp (chas-array-bps chas-array)))))

(defmethod delete-bp ((self row) bp)
  (check-bp-arg bp)
  (let ((chas-array (slot-value self 'chas-array)))
    (setf (chas-array-bps chas-array)
    (fast-delq bp (chas-array-bps chas-array)))))


;;; First, some Font Descriptor utilities

(defun new-bfd (cha-no font &optional color)
  (let ((new (make-bfd cha-no font)))
    (when color (setf (bfd-color new) color))
    new))

;;; The Redisplay requires that Font Descriptors be ordered sequentially

(defun chas-array-insert-bfd (chas-array bfd)
  (if (null (chas-array-fds chas-array))
      (push bfd (chas-array-fds chas-array))
      ;; looks like we have to loop through to figure out the right place
      (do* ((fds (chas-array-fds chas-array) (cdr fds))
      (next-fds (cdr fds) (cdr fds))
      (current-fd (car fds) (car fds))
      (next-fd (car next-fds) (car next-fds))
      (bfd-cha-no (bfd-cha-no bfd)))
     ((null fds))
  (cond ((<& bfd-cha-no (bfd-cha-no current-fd))
               ;; this can happen if the new bfd is to be inserted BEFORE
               ;; all of the existing BFD's
               (setf (chas-array-fds chas-array) (list* bfd fds))
               (return))
              ((=& bfd-cha-no (bfd-cha-no current-fd))
         ;; if there already is a FD at the right place, bash
         ;; its values rather than splicing a new FD into the list
         (setf (bfd-font-no current-fd) (bfd-font-no bfd)
                     (bfd-color   current-fd) (bfd-color   bfd))
         (return))
        ((>& bfd-cha-no (bfd-cha-no current-fd))
         (cond ((null next-fd)
          (setf (cdr fds) (list bfd))
          (return))
         ((<& bfd-cha-no (bfd-cha-no next-fd))
          ;; we are in between 2 FD's so we should splice in here
          (setf (cdr fds) (cons bfd next-fds))
          (return))
         ((=& bfd-cha-no (bfd-cha-no next-fd))
          (setf (bfd-font-no next-fd) (bfd-font-no bfd)
                            (bfd-color   next-fd) (bfd-color   bfd))
          (return))))))))

(defun chas-array-delete-bfd (chas-array bfd)
  (setf (chas-array-fds chas-array)
  (fast-delq bfd (chas-array-fds chas-array))))

(defmethod row-fds ((self row))
  (chas-array-fds (slot-value self 'chas-array)))

(defmethod set-fds ((self row) new-value)
  (assert (and (listp new-value) (every #'bfd? new-value)) (new-value)
    "A list of Boxer Font Descriptors")
  (setf (chas-array-fds (slot-value self 'chas-array)) new-value))

(defmethod insert-bfd ((self row) new-bfd)
  (chas-array-insert-bfd (slot-value self 'chas-array) new-bfd))

(defmethod delete-bfd ((self row) bfd)
  (chas-array-delete-bfd (slot-value self 'chas-array) bfd))


;;; These are the functions (to rows) that other sections of code may call
;;; to find out about or modify the connection structure of rows and chas:
;;;
;;; length-in-chas
;;; cha-at-cha-no
;;; cha-cha-no
;;;
;;; chas
;;;
;;; insert-cha-at-cha-no
;;; insert-row-chas-at-cha-no
;;; delete-cha-at-cha-no
;;; delete-chas-between-cha-nos
;;; kill-chas-at-cha-no
;;;
;;; insert-cha-before-cha
;;; insert-cha-after-cha
;;; insert-row-chas-before-cha
;;; insert-row-chas-after-cha
;;; delete-cha
;;; delete-between-chas
;;; kill-cha
;;;
;;; In addition the macro DO-ROW-CHAS ((<var> <row>) <body>) is defined to
;;; be used by other sections of code to iterate through a row's chas.
;;;

(defmethod set-chas-array ((self row) na)
  (setf (slot-value self 'chas-array) na))

(defmacro do-row-chas (((var row &key (start 0) stop) . other-do-vars)
           &body body)
  (let ((chas-array (gensym))
  (chas (gensym))
  (active-length (gensym))
  (cha-no (gensym)))
  `(let* ((,chas-array (chas-array ,row))
    (,chas (chas-array-chas ,chas-array))
    (,active-length (if (null ,stop)
                              (chas-array-active-length ,chas-array)
                              ,stop)))
     (let ((,var nil))				       	;Note that there is a
       (do ((,cha-no ,start (+& ,cha-no 1))  	       	;good reason for using
      . ,other-do-vars)			       	;this weird
     ((>=& ,cha-no ,active-length))                  ;(LET ((,VAR NIL)) ...)
   (setq ,var
         (fast-chas-array-get-cha ,chas ,cha-no));(SETQ ,VAR <foo>)
         (macrolet ((change-cha (new-cha)
                      `(setf (fast-chas-array-get-cha ,',chas ,',cha-no)
                             ,new-cha)))
     . ,body))))))

(defmethod length-in-chas ((self row))
  (chas-array-active-length (slot-value self 'chas-array)))

(defmethod cha-at-cha-no ((self row) n)
  (let ((chas-array (slot-value self 'chas-array)))
    (cond ((or (<& n 0)
         (>=& n (chas-array-active-length chas-array))) nil)
    (t (chas-array-get-cha chas-array n)))))

;;; this is useful for changing case and fonts and such
(defmethod change-cha-at-cha-no ((self row) n new-cha)
  (let ((chas-array (slot-value self 'chas-array)))
    (cond ((or (<& n 0)
         (>=& n (chas-array-active-length chas-array))) nil)
    (t (let ((old-cha (chas-array-get-cha chas-array n)))
         (unless (cha? old-cha)
     (delete-self-action old-cha)))
       (chas-array-set-cha chas-array n new-cha)
       (modified self)
       (unless (cha? new-CHA)
         (SET-SUPERIOR-ROW new-CHA SELF)
         (insert-self-action new-cha))))))

(defmethod cha-cha-no ((row row) cha-to-get-cha-no-of)
  (do-row-chas ((cha row)
    (cha-no 0 (+& cha-no 1)))
    (cond ((eq cha cha-to-get-cha-no-of)
     (return cha-no)))))

(DEFMETHOD CHAS ((ROW ROW))
  (OR (CACHED-CHAS ROW) (CACHE-CHAS ROW)))

(DEFMETHOD CACHE-CHAS ((ROW ROW))
  (LET ((CHAS (WITH-COLLECTION (DO-ROW-CHAS ((CHA ROW)) (COLLECT CHA)))))
    (SETF (CACHED-CHAS ROW) CHAS)
    CHAS))

(defmethod chas-between-cha-nos ((self row) start
         &optional (stop (length-in-chas self)))
  (with-collection
    (do ((cha-no start (1+& cha-no)))
        ((=& cha-no stop))
      (collect (cha-at-cha-no self cha-no)))))

(DEFMETHOD BOXES-IN-ROW ((ROW ROW))
  (WITH-COLLECTION
    (DO-ROW-CHAS ((CHA ROW))
      (unless (cha? CHA) (COLLECT CHA)))))

(defmethod boxes-between-cha-nos  ((row row) strt-cha-no stop-cha-no)
  (with-collection
      (do* ((index strt-cha-no (+& index 1))
      (cha (cha-at-cha-no row index)
     (cha-at-cha-no row index)))
     ((=& index stop-cha-no))
  (unless (cha? cha)
    (collect cha)))))

(defun copy-current-bfd (at-cha-no)
  (let ((new (make-bfd at-cha-no (bfd-font-no *current-font-descriptor*))))
    (setf (bfd-color new) (bfd-color *current-font-descriptor*))
    new))

(DEFMETHOD INSERT-CHA-AT-CHA-NO ((SELF ROW) CHA CHA-NO)
  (CHAS-ARRAY-INSERT-CHA (CHAS-ARRAY SELF) CHA-NO CHA)
  (cond ((cha? CHA) ; do possible font stuff
         (multiple-value-bind (current-bfd following-bfd)
             (bfd-hood self cha-no)
           (unless (bfd= *current-font-descriptor* current-bfd)
             (if (=& cha-no (bfd-cha-no current-bfd))
                 (setf (bfd-font-no current-bfd)
                       (bfd-font-no *current-font-descriptor*)
                       (bfd-color   current-bfd)
                       (bfd-color   *current-font-descriptor*))
                 (insert-bfd self (copy-current-bfd cha-no)))
             ;; preserve the font following the new insertion
             ;; unless there are no following chars OR we are inserting at
             ;; the end of a row
             (cond ((=& cha-no (length-in-chas self))) ; inserting at end of row
                   ((and following-bfd (=& (1+& cha-no)
                                           (bfd-cha-no following-bfd)))
                    ;; no need for trailing BFD but in this case we should
                    ;; check to see if the slid over following-bfd needs
                    ;; to be removed
                    (when (bfd= *current-font-descriptor* following-bfd)
                      (delete-bfd self following-bfd)))
                   (t
                    (insert-bfd self (copy-fd-at current-bfd (1+& cha-no))))))))
        (t ; do box stuff
         (SET-SUPERIOR-ROW CHA SELF)
         (insert-self-action cha)))
  (MODIFIED SELF))


(defmethod insert-list-of-chas-at-cha-no ((self row) list-of-chas cha-no)
  (do* ((remaining-chas list-of-chas (cdr remaining-chas))
  (CHA (CAR REMAINING-CHAS))
  (present-cha-no cha-no (1+ present-cha-no)))
       ((null remaining-chas))
    (insert-cha-at-cha-no self cha present-cha-no)
    (unless (cha? CHA)
      (SET-SUPERIOR-ROW CHA SELF)
      (insert-self-action cha))))

(DEFMETHOD DELETE-CHA-AT-CHA-NO ((SELF ROW) CHA-NO)
  (LET ((CHA (CHA-AT-CHA-NO SELF CHA-NO)))
    (CHAS-ARRAY-DELETE-CHA (CHAS-ARRAY SELF) CHA-NO)
    (unless (cha? CHA)
      (delete-self-action cha)
      (set-superior-row cha nil))
    (MODIFIED SELF)))

(defmethod insert-row-chas-at-cha-no ((self row) row cha-no
              &optional
              (from-start-cha-no 0)
              (from-stop-cha-no (length-in-chas row)))
  (unless (>& from-start-cha-no from-stop-cha-no)
    (let ((row-chas-array (chas-array row)))
      (do-row-chas ((c row :start from-start-cha-no :stop from-stop-cha-no))
       (unless (cha? c)
         (set-superior-row c self)
         (insert-self-action c)))
      (chas-array-move-chas row-chas-array from-start-cha-no
          (chas-array self) cha-no
          (-& from-stop-cha-no from-start-cha-no)
          self))
    (modified self)))

(defmethod delete-chas-between-cha-nos ((row row) strt-cha-no stop-cha-no)
  (let* ((return-row (make-initialized-row))
   (return-row-chas-array (chas-array return-row)))
    (chas-array-move-chas
      (chas-array row) strt-cha-no return-row-chas-array
      0 (-& stop-cha-no strt-cha-no) return-row)
    (modified row)
    (modified return-row)
    (do-row-chas ((c return-row))
      (unless (cha? c)
  ;; note that the Boxes here still think they are inferiors of
  ;; the original row So that DELETE-SELF-ACTION can win
  (delete-self-action c)
  (set-superior-row c return-row)))
    return-row))

(defmethod kill-chas-at-cha-no  ((self row) strt-cha-no)
  (let ((stop-cha-no (chas-array-active-length (chas-array self))))
    (delete-chas-between-cha-nos self strt-cha-no stop-cha-no)))


(defmethod insert-cha-before-cha ((self row) cha before-cha)
  (let ((before-cha-cha-no (cha-cha-no self before-cha)))
    (insert-cha-at-cha-no self before-cha-cha-no cha)))

(defmethod insert-cha-after-cha ((self row) cha after-cha)
  (let ((after-cha-cha-no (cha-cha-no self after-cha)))
    (insert-cha-at-cha-no self cha (+ after-cha-cha-no 1))))


(defmethod delete-cha ((self row) cha)
  (let ((cha-cha-no (cha-cha-no self cha)))
    (unless (null cha-cha-no)
      (delete-cha-at-cha-no self cha-cha-no))))

(defmethod append-cha ((self row) cha)
  (insert-cha-at-cha-no self cha (chas-array-active-length (chas-array self))))

(defmethod append-list-of-chas ((self row) list-of-chas)
  (insert-list-of-chas-at-cha-no self list-of-chas
  (chas-array-active-length (chas-array self))))


;;; Box rows are kept a doubly linked list. The box points to its first row,
;;; and each row has pointers to its next and previous rows. The first row in
;;; a box has  a previous-row pointer of nil, and the last row in a box has a
;;; next row pointer of nil.

(defmethod set-previous-row ((row row) new)
  (setf (previous-row row) new))

(defmethod set-next-row ((row row) new)
  (setf (next-row row) new))

(defmethod set-first-inferior-row ((box box) new)
  (setf (first-inferior-row box) new))


;;; These are the messages (to boxs) that other sections of code may call
;;; to find out about or modify the connection structure of boxs and rows:
;;;
;;; length-in-rows
;;; length-in-chas
;;; row-at-row-no
;;; row-row-no
;;;
;;; rows
;;;
;;; insert-row-at-row-no
;;; delete-row-at-row-no
;;; delete-row
;;;
;;; insert-row-before-row
;;; insert-row-after-row
;;;
;;; kill-box-contents
;;;
;;; In addition the macro DO-BOX-ROWS ((<var> <box>) <body>) is defined to
;;; be used by other sections of code to iterate through a box's rows.
#|
 ;;; obsolete:
 ;;; :INSERT-BOX-ROWS-AT-ROW-NO
 ;;; :DELETE-ROWS-BETWEEN-ROW-NOS
 ;;; :KILL-ROWS-AT-ROW-NO
 ;;; :DELETE-BETWEEN-ROWS
 ;;; :KILL-ROW
 ;;; :INSERT-BOX-ROWS-BEFORE-ROW
;;; :INSERT-BOX-ROWS-AFTER-ROW

|#



;(defmethod superior-box ((row row)) (slot-value row 'superior-box))

(defmethod superior-box ((box box))
  (let ((sup-row (superior-row box)))
    (unless (null sup-row) (superior-box sup-row))))

(defmethod set-superior-box ((row row) new) (setf (superior-box row) new))

(defmethod set-superior-row ((box box) new-row)
  (setf (slot-value box 'superior-row) new-row))

;; mayber these should be calling the accessor instead of slot-value ????
(defmethod superior-obj ((row row)) (slot-value row 'superior-box))
(defmethod superior-obj ((box box)) (slot-value box 'superior-row))

(defmacro do-box-rows (((var box) . other-do-vars) &body body)
  `(do ((,var (first-inferior-row ,box) (next-row ,var))
  . ,other-do-vars)
       ((null ,var))
     . ,body))

(defmethod length-in-rows ((self box))
  (do ((row (first-inferior-row self) (next-row row))
       (length 0 (+& length 1)))
      ((null row) length)))

(defmethod last-inferior-row ((self box))
  (do ((row (first-inferior-row self) (next-row row))
       (prev-row nil))
      ((null row) prev-row)
    (setq prev-row row)))

(defmethod length-in-chas ((self box))
  (with-summation
    (do-box-rows ((row self)) (sum (length-in-chas row)))))

(defmethod row-at-row-no ((self box) row-no)
  (unless (minusp row-no)
    (do ((row (first-inferior-row self) (next-row row))
   (i row-no (-& i 1)))
  ((or (null row) (<& i 1)) row))))

(defmethod row-row-no ((self box) row)
  (do ((inf-row (first-inferior-row self) (next-row inf-row))
       (row-no 0 (+& row-no 1)))
      ((null inf-row))
    (when (eq inf-row row)
    (return row-no))))

(defmethod cache-rows ((box box))
  (let ((rows (with-collection (do-box-rows ((row box)) (collect row)))))
    (setf (cached-rows box) rows)
    rows))

(defmethod rows ((box box))
  (or (slot-value box 'cached-rows) (cache-rows box)))

(defmethod insert-row-at-row-no ((box box) row row-no
         &optional (check-closet t))
  (let* ((row-at-row-no (row-at-row-no box row-no))
   (row-before-row-no  ;; was (row-at-row-no box (- row-no 1))
    (or (and row-at-row-no (previous-row row-at-row-no))
        (row-at-row-no box (-& row-no 1)))))
    (set-superior-box row box)
    (set-previous-row row row-before-row-no)
    (set-next-row row row-at-row-no)
    (if (null row-before-row-no)
  (set-first-inferior-row box row)
  (set-next-row row-before-row-no row))
    (unless (null row-at-row-no)
      (set-previous-row row-at-row-no row))
    (when (or (null check-closet)
        (not (eq row (slot-value box 'closets))))
      (do-row-chas ((c row))
  (unless (cha? c) (insert-self-action c))))))


(defmethod delete-row-at-row-no ((box box) pos &optional (check-closet t))
  ;; It is really convenient to be able to assume
  ;; that each box has at least one row in it.
  (unless (null (next-row (first-inferior-row box)));(= (length-in-rows box) 1)
    (let* ((row (row-at-row-no box pos))
     (row-prev-row (unless (null row) (previous-row row)))
     (row-next-row (unless (null row) (next-row row))))
      (unless (null row)
  (when (or (null check-closet)
        (not (eq row (slot-value box 'closets))))
    (do-row-chas ((c row))
      (unless (cha? c) (delete-self-action c)))
    (set-superior-box row nil))
  (set-previous-row row nil)
  (set-next-row row nil))
      (if (eq row (first-inferior-row box))
    (setf (first-inferior-row box) row-next-row)
    (unless (null row-prev-row)
      (set-next-row row-prev-row row-next-row)))
      (unless (null row-next-row)
  (set-previous-row row-next-row row-prev-row)))))


;;; These are no longer used.  They work (6/6/88), but do much useless work,
;;; and KILL-ROW was ever called.  See KILL-BOX-CONTENTS, the more efficient
;;; replacement.
#|
(defmethod insert-box-rows-at-row-no ((self box) box row-no)
  (let ((box-first-row (kill-row box (first-inferior-row box))))
    (unless (null box-first-row)
      (let ((row-at-row-no (row-at-row-no self row-no))
      (row-bf-row-no (row-at-row-no self (- row-no 1)))
      (box-last-row (do* ((next-box-row (next-row box-first-row)
                (next-row box-row))
        (box-row box-first-row next-box-row))
             (())
          (set-superior-box box-row self)
          (do-row-chas ((c box-row)) (unless (cha? c) (insert-self-action c)))
          (if (null next-box-row) (return box-row)))))
  (set-previous-row box-first-row row-bf-row-no)
  (set-next-row box-last-row row-at-row-no)
  (set-next-row row-bf-row-no box-first-row)
  (set-previous-row row-at-row-no box-last-row)))))

(defmethod delete-rows-between-row-nos ((self box) strt-row-no stop-row-no
          &optional (check-closet t))
  (let* ((strt-row (row-at-row-no self strt-row-no))
   (stop-row (row-at-row-no self stop-row-no))
   (strt-row-prev-row (unless (null strt-row) (previous-row strt-row)))
         (stop-row-next-row (unless (null stop-row) (next-row stop-row)))
   (return-box (make-initialized-box))
   (closet-row (slot-value self 'closets)))
    (do ((row strt-row (next-row row)))
  ((null row))
      (when (or (null check-closet) (not (eq row closet-row)))
  (do-row-chas ((c row)) (unless (cha? c) (delete-self-action c)))
  (set-superior-box row nil)))
    (set-previous-row strt-row nil)
    (set-next-row strt-row nil)
    (if (null strt-row-prev-row)
  (set-first-inferior-row self stop-row-next-row)
  (set-next-row strt-row-prev-row stop-row-next-row))
    (unless (null stop-row-next-row)
      (set-previous-row stop-row-next-row strt-row-prev-row))
    (append-row return-box strt-row)
    return-box))

(defmethod delete-between-rows ((self box) strt-row stop-row
        &optional (check-closet t))
  (let ((strt-row-prev-row (when (not-null strt-row)(previous-row strt-row)))
  (stop-row-next-row (when (not-null stop-row)(next-row stop-row)))
  (return-box (make-initialized-box))
  (closet-row (slot-value self 'closets)))
    (do ((row strt-row (next-row row)))
  ((eq row stop-row-next-row))
      (when (or (null check-closet) (not (eq row closet-row)))
  (do-row-chas ((c row)) (unless (cha? c) (delete-self-action c)))
  (set-superior-box row nil)))
    (set-previous-row strt-row nil)
    (set-next-row stop-row nil)
    (if (null strt-row-prev-row)
  (set-first-inferior-row self stop-row-next-row)
  (set-next-row strt-row-prev-row stop-row-next-row))
    (when (not-null stop-row-next-row)
      (set-previous-row stop-row-next-row strt-row-prev-row))
    (set-first-inferior-row return-box strt-row)
    return-box))

(defmethod kill-rows-at-row-no ((self box) strt-row-no)
  (let ((stop-row-no (length-in-rows self)))
    (delete-rows-between-row-nos self strt-row-no stop-row-no)))

(defmethod kill-row ((self box) row)
  (kill-rows-at-row-no self (row-row-no self row)))
|#


;;; Operations that take existing box rows as position specifiers. These
;;; operations are built on top of the operations that take row positions
;;; as position specifiers.

(defmethod insert-row-before-row ((box box) row before-row
          &optional (check-closet t))
  (let ((prev-row (previous-row before-row)))
    (set-superior-box row box)
    (set-previous-row row prev-row)
    (set-previous-row before-row row)
    (set-next-row row before-row)
    (if (not-null prev-row)    ; we are not inserting into the beginning...
  (set-next-row prev-row row)
  (set-first-inferior-row box row)))
  (when (or (null check-closet) (not (eq row (slot-value box 'closets))))
    (do-row-chas ((c row)) (unless (cha? c) (insert-self-action c)))))

(defmethod insert-row-after-row ((box box) row after-row
         &optional (check-closet t))
  (let ((after-after-row (next-row after-row)))
    (set-superior-box row box)
    (set-next-row after-row row)
    (set-previous-row row after-row)
    (set-next-row row after-after-row)
    (unless (null after-after-row)
      (set-previous-row after-after-row row)))
  (when (or (null check-closet) (not (eq row (slot-value box 'closets))))
    (do-row-chas ((c row)) (unless (cha? c) (insert-self-action c)))))

(defmethod append-row ((self box) row)
  (insert-row-at-row-no self row (length-in-rows self)))

(defmethod delete-row ((self box) row &optional (check-closet t))
  (when (eq (superior-box row) self)
    (let ((prev-row (previous-row row))
    (next-row (next-row row)))
      (when (or (null check-closet)
    (not (eq row (slot-value self 'closets))))
  (do-row-chas ((c row))
         (unless (cha? c) (delete-self-action c)))
  (set-superior-box row nil))
      (set-previous-row row nil)
      (set-next-row row nil)
      (if (eq row (first-inferior-row self))
    (setf (first-inferior-row self) next-row)
    (unless (null prev-row) (set-next-row prev-row next-row)))
      (unless (null next-row) (set-previous-row next-row prev-row)))))


;; Use this instead of (kill-row box (row-at-row-no 0 box)).
;; Later, we can optimize out the DELETE-SELF-ACTION stuff:
;; First, we delete all static variables except those
;; defined in the closet row.
;;
;; Second, if the box is transparent, we must delete all exported names
;; from the  superior box and look through the box for more transparent boxes.
;;
;; Third, we flush the various caches.
;;
;; Fourth, we deal with port links (if that's necessary?)
;;
;; Fifth, we remove the sprites from the graphics box.
;; For now we'll just let the delete self actions take care of this.

(defmethod kill-box-contents ((self box) &optional (check-closet t))
  (let ((closet-row (and check-closet (slot-value self 'closets))))
    (do ((row (first-inferior-row self)
        (next-row row)))
  ((null row))
      (unless (eq row closet-row)
  (do-row-chas ((c row)) (unless (cha? c) (delete-self-action c self)))
  (set-superior-box row nil)))
    (setf (first-inferior-row self) nil)
    ;; if there are any screen-boxes, then we also need to reset their
    ;; scrolling row since it could still be pointing at one
    ;; of the editor-rows we just removed
    (dolist (sb (screen-objs self))
      (setf (scroll-to-actual-row sb) nil))))

(defmacro action-at-bp-internal (&body do-action-form)
  `(let ((old-bp-type (bp-type bp)))
     (unwind-protect
       (progn (setf (bp-type bp) (if force-bp-type force-bp-type old-bp-type))
        . ,do-action-form)
       (setf (bp-type bp) old-bp-type))))

(defun insert-cha (bp cha &optional (force-bp-type nil))
  (action-at-bp-internal
    (insert-cha-at-cha-no (bp-row bp) cha (bp-cha-no bp))))

(defun insert-row-chas (bp row &optional (force-bp-type nil))
  (action-at-bp-internal
    (insert-row-chas-at-cha-no (bp-row bp) row (bp-cha-no bp))))

(defun insert-row (bp row &optional (force-bp-type nil))
  (action-at-bp-internal
    (let* ((bp-box (bp-box bp))
     (bp-row (bp-row bp))
     (bp-row-row-no (row-row-no bp-box bp-row))
     (temp-row (let ((*boxes-being-temporarily-deleted* t))
            (delete-chas-to-end-of-row bp force-bp-type))))
      (insert-row-at-row-no bp-box row (+ bp-row-row-no 1))
      (move-point (row-last-bp-values row))
      (let ((*boxes-being-temporarily-deleted* t))
  (insert-row-chas bp temp-row :fixed)))))

(defun delete-chas-to-end-of-row (bp &optional (force-bp-type nil))
  (action-at-bp-internal
    (let ((row (bp-row bp))
    (cha-no (bp-cha-no bp)))
      (kill-chas-at-cha-no row cha-no))))


#|  obsolete, give it 6 months and then remove from source (7/19/91)
(defun delete-rows-to-end-of-box (bp &optional (force-bp-type nil))
  (action-at-bp-internal
    (let ((box (bp-box bp))
    (row (bp-row bp)))
      (unless (null box)
  (kill-rows-at-row-no box (+ (row-row-no box row) 1))))))
|#


;;;; FIND-LOWEST-COMMON-SUPERIOR-BOX
;;; This function takes two boxes as its inputs and find the lowest box
;;; which is a superior of both of those boxes. It is slightly bummed
;;; for speed since it gets called a fair amount, and I liked the way
;;; I bummed it.

(defun find-lowest-common-superior-box (box1 box2)
  (let ((mark-this-pass (cons nil nil))) ;might even be able to use INCF.
    (do ((box1 box1 (and box1 (superior-box box1)))
   (box2 box2 (and box2 (superior-box box2))))
  (())
      (cond ((eq box1 box2)
       (return box1))
      ((eq (and box1 (getprop box1 ':lowest-common-superior-mark))
     mark-this-pass)
       (return box1))
      ((eq (and box2 (getprop box2 ':lowest-common-superior-mark))
     mark-this-pass)
       (return box2))
      (t
       (and box1 (putprop box1 mark-this-pass
        ':lowest-common-superior-mark))
       (and box2 (putprop box2 mark-this-pass '
        :lowest-common-superior-mark)))))))

(defun obj-contains-obj? (outer inner)
  (do ((inner inner (superior-obj inner)))
      ((null inner) nil)
    (cond ((eq inner outer)
     (return t)))))

(defun box-contains-box? (outer-box inner-box)
  (do ((inner (superior-box inner-box) (superior-box inner)))
      ((null inner) nil)
    (and (eq inner outer-box)
   (return t))))

(defun level-of-superiority (outer-box inner-box)
  (do ((i 0 (1+& i))
       (box inner-box (superior-box box)))
      ((or (null box) (eq box outer-box)) i)))

(defun nth-superior-box (box n)
  (do ((i 0 (1+& i))
       (superior box (superior-box superior)))
      ((null superior) nil)
    (and (=& i n) (return superior))))


;;;;FIND-PATH

;; The FIND-PATH function is used to find the "path" between two boxes.
;; It returns two values
;;  first value   --   Box to throw to
;;  second value  --   Chain of boxes to enter
;; Note that either of these values can be NIL.
;;
;; Example:
;;
;; +-------------------------------------------------+
;; | call this box TOP                               |
;; |                                                 |
;; | +------------------+     +------------------+   |
;; | | call this box A1 |     | call this box B1 |   |
;; | |                  |     |                  |   |
;; | | +--------------+ |     | +--------------+ |   |
;; | | |call this A2  | |     | | call this B2 | |   |
;; | | |              | |     | |              | |   |
;; | | | +----------+ | |     | | +----------+ | |   |
;; | | | | this A3  | | |     | | | this B3  | | |   |
;; | | | |          | | |     | | |          | | |   |
;; | | | +----------+ | |     | | +----------+ | |   |
;; | | +--------------+ |     | +--------------+ |   |
;; | +------------------+     +------------------+   |
;; +-------------------------------------------------+
;;
;; (FIND-PATH A3 TOP)  -->  TOP  NIL
;; (FIND-PATH TOP A3)  -->  NIL  (A1 A2 A3)
;; (FIND-PATH A3 B3)   -->  TOP  (B1 B2 B3)
;; (FIND-PATH A3 A3)   -->  NIL  NIL

(defun find-path (from-box to-box)
  (declare (values box-to-throw-to downward-entry-chain))
  (cond ((eq from-box to-box)
   (values nil
     nil))
  ((box-contains-box? to-box from-box)
   (values to-box
     nil))
  ((box-contains-box? from-box to-box)
   (values nil
     (find-path-from-superior-to-inferior from-box to-box)))
  (t
   (let ((lowest-common-superior-box
     (find-lowest-common-superior-box from-box
              to-box)))
     (values lowest-common-superior-box
       (find-path-from-superior-to-inferior
         lowest-common-superior-box to-box))))))


(defun find-path-from-superior-to-inferior (superior-box inferior-box)
  (do ((box inferior-box (superior-box box))
       (result nil))
      ((eq box superior-box) result)
    (push box result)))
#|
(defun find-path-from-superior-to-inferior (superior-box inferior-box)
  (nreverse
    (with-collection
      (do ((box inferior-box (superior-box box)))
    ((eq box superior-box))
  (collect box)))))
|#
;;; This moves the *point* upward in the hierarchy via the exit method
;;; until it reaches the destination-box
;;; the one-step-up? arg controls whether or not triggers are run in
;;; the exit method.  Right now, EVERY trigger on the upward path will
;;; get a chance to run. It is not clear that this is the right thing

(defun send-exit-messages (destination-box destination-screen-box
         &optional(one-step-up? t))
  (let ((current-box (point-box)))
    (cond ((eq (find-lowest-common-superior-box current-box destination-box)
         current-box)
     nil)
    ((superior? destination-screen-box (point-screen-box)) nil)
    ((null (superior-box current-box)) nil)
    (t (exit current-box
       (superior-screen-box (bp-screen-box *point*))
       (superior-box current-box)
       one-step-up?)
       (maybe-handle-trigger-in-editor)
       (send-exit-messages destination-box
         destination-screen-box)))))


;; Needs these to keep reDisplay code alive.

(defmethod first-inferior-obj ((self row))
  (cha-at-cha-no self 0))

(defmethod next-obj ((self box))
  (let ((superior-row (slot-value self 'superior-row)))
    (cha-at-cha-no superior-row (+ (cha-cha-no superior-row self) 1))))

(defmethod first-inferior-obj ((box box))
  (slot-value box 'first-inferior-row))

(defmethod next-obj ((row row))
  (slot-value row 'next-row))


;;; New, faster mapping function.
;;; Uses DOLIST paradigm instead of MAPC paradigm to eliminate lexical context
;;; switch in cases where it's not necessary.
;;; Uses modern faster aref access.  Calls CHA? instead of BOX?.

;;; NOTE: uses (slot-value <> 'first-inferior-row) rather than the
;;;       first-inferior-row method to avoid port redundancy and, in
;;;       the case of circular ports, certain lossage

(defmacro do-for-all-inferior-boxes-fast ((var the-box) &body body)
  ;; you must promise not to insert or delete things from the rows
  ;; since we cache the row chas.
  `(labels ((do-for-internal-body (,var) . ,body)
      (do-for-internal-iterator (superior-box)
        (do* ((row (slot-value superior-box 'first-inferior-row)
       (next-row row)))
       ((null row))
    (let* ((chas-array (chas-array-chas (chas-array row)))
           (length (chas-array-active-length (chas-array row))))
      (do ((cha-no 0 (1+& cha-no)))
          ((=& cha-no length))
        (let ((cha (aref chas-array cha-no)))
          (when (null cha) (return nil))
          (when (not (cha? cha))
      (do-for-internal-body cha)
      (do-for-internal-iterator cha))))))))
    (do-for-internal-iterator ,the-box)))

;;;; From region.lisp
;;; needs to be here so it will be defined when editor.lisp gets compiled

(defmacro do-interval-chas ((cha interval &key (output-newlines? nil))
          &body body)
  (let ((real-start-bp (gensym))
  (real-stop-bp  (gensym))
  (current-row   (gensym))
  (current-stop-cha-no  (gensym))
  (current-cha-no (gensym)))
    `(multiple-value-bind (,real-start-bp ,real-stop-bp)
   (order-bps (interval-start-bp ,interval)
        (interval-stop-bp  ,interval))
       (let* ((,current-row (bp-row ,real-start-bp))
        (,current-cha-no (bp-cha-no ,real-start-bp))
        (,current-stop-cha-no (if (or (null ,current-row)
              (eq ,current-row
            (bp-row ,real-stop-bp)))
          (bp-cha-no ,real-stop-bp)
          (length-in-chas ,current-row))))
   (labels ((last-row? () (or (null ,current-row)
            (eq ,current-row (bp-row ,real-stop-bp))))
      (end-of-row? () (=& ,current-cha-no ,current-stop-cha-no))
      (next-row-values ()
        (setq ,current-row (next-row ,current-row)
        ,current-cha-no 0
        ,current-stop-cha-no
        (if (last-row?)
            (bp-cha-no ,real-stop-bp)
            (length-in-chas ,current-row)))))
     (loop
      (let ((,cha (if (end-of-row?)
         #\Newline
         (cha-at-cha-no ,current-row ,current-cha-no))))
        (cond ((and (last-row?) (end-of-row?))
         (return nil))
        ((and (null ,output-newlines?) (end-of-row?)))
        (t . ,body))
        (if (end-of-row?)
      (next-row-values)
      (incf& ,current-cha-no)))))))))
