;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER ;-*-

#|


 $Header: storage.lisp,v 1.0 90/01/24 22:17:58 boxer Exp $

 $Log:	storage.lisp,v $
;;;Revision 1.0  90/01/24  22:17:58  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-------+
                This file is part of the | BOXER | system
                                         +-------+

  This file contains a storage substrate used by the redisplay (although anyone
  else can use it as well) to keep track of inferior items



Modification History (most recent at top)

 2/10/03 merged current PC & mac source
10/25/98 Additions for Harlequin Lispworks for Windows 4.1

|#

(in-package :boxer)

;; sgithens - 2019-11-23 Removing this optimization, which I no longer
;; think is needed these days. Either way, compiling on current SBCL this
;; causes some pretty intense C like unhandled memory errors.
;; (eval-when (compile)
;;   (progn
;;     (format t "~&***NOTE: Throwing caution to the winds, SAFETY=0")
;;     (proclaim '(optimize (speed 3) (safety 0))))
;; )

;;;; Da Data Structure
;;; Storage vectors (SV's) are a 2 element simple-vector:
;;; the first element is a simple vector of contents and the second element
;;; is a fill pointer.  If the fill pointer needs to grow past the length of
;;; the contents, then we allocate a new storage vector and move all
;;; the contents over to the new one.  We maintain free lists of these
;;; vectors and pop/push them as required.

(defconstant *default-initial-storage-vector-length* 8.)

;;; we define a defstruct here so that storage vectors can be we can :INCLUDEd
;;; in other structures (such as graphics command lists)

(defstruct (storage-vector (:type vector)
			   (:constructor %%make-storage-vector)
			   (:copier %%copy-storage-vector)
			   (:conc-name %%sv-))
  (contents (make-array *default-initial-storage-vector-length*))
  (fill-pointer 0))


;;;

(eval-when
    (:compile-toplevel :load-toplevel :execute)
	(defvar *compile-in-storage-vector-error-checking* t)
	(defvar *compile-in-storage-metering-info* t)
)

(defmacro %sv-fill-pointer (sv)
  `(svref& ,sv 1))

(defmacro %sv-contents (sv)
  `(svref& ,sv 0))

(defmacro check-sv-arg (thing)
  (unless (null *compile-in-storage-vector-error-checking*)
    `(unless (and (simple-vector-p ,thing)
		  (numberp (%sv-fill-pointer ,thing))
		  (simple-vector-p (%sv-contents ,thing)))
       (error "The arg, ~S, was not a storage vector" ,thing))))

;; this can be casual or strict...
(defmacro storage-vector? (thing)
  (if (null *compile-in-storage-vector-error-checking*)
      `(simple-vector-p ,thing)
      `(and (simple-vector-p ,thing)
	    (numberp (%sv-fill-pointer ,thing))
	    (simple-vector-p (%sv-contents ,thing)))))

(defmacro check-number-arg (arg &rest conditions)
  (unless (null *compile-in-storage-vector-error-checking*)
    `(unless (and (numberp ,arg) . ,conditions)
       (error "The arg, ~S, was not an appropriate number" ,arg))))

;; this makes a SV with the fill pointer set to 0
(defun make-storage-vector (&optional
			    (length *default-initial-storage-vector-length*))
  (%%make-storage-vector :contents (make-array length)
			 :fill-pointer 0))

(defun clear-storage-vector (sv)
  (check-sv-arg sv)
  (setf (%sv-fill-pointer sv) 0)
  ;; return the cleared storage vector
  sv)

(defmacro storage-vector-active-length (sv)
  `(%sv-fill-pointer ,sv))

(defun storage-vector-max-length (sv)
  (check-sv-arg sv)
  (svlength (%sv-contents sv)))

;;;; Allocation / DeAllocation

;;; we maintain free lists of available SV's.  We might want to add
;;; another slot to the SV header and link them together that way instead

(defvar *storage-vectors* nil)

(defvar *storage-vector-sizes* '(8. 16. 32. 64. 128.))

;; for each storage vector size, we need to associate the number of
;; contents-vectors to pre-cons
(eval-when
    (:compile-toplevel :load-toplevel :execute)
	(defvar *initial-storage-setup-alist*
		'((8 . 512) (16 . 512) (32 . 512) (64 . 32) (128 . 4)
		;; if we are forced to cons these, we might as well
		;; have a place to save them
		;; on the other hand, saving them will force the GC
		;; to have to transport them so the tradeoff is unclear
		(256 . 0) (1024 . 0)))
)

;;; increment of growth after we have passed the point where there
;;; may be pre-allocated vectors
(defconstant *storage-vector-terminal-growth-quantum* 256.)

;; we store pre-consed contents-vectors of certain sizes in free-lists

(defvar *storage-vector-free-lists* nil)

(defvar *free-list-init-flag* nil)

(defun initialize-storage-vector-free-lists ()
  (unless (not (null *free-list-init-flag*))
    ;; first setup the top level vector of free lists
    (setq *storage-vector-free-lists*
	  (make-array (length *initial-storage-setup-alist*)
		      :initial-element nil))
    ;; now fill the free lists
    (dotimes (i (length *initial-storage-setup-alist*))
      (dotimes (j (cdr (nth i *initial-storage-setup-alist*)))
	(push (make-array (car (nth i *initial-storage-setup-alist*)))
	      (svref& *storage-vector-free-lists* i))))))

;;;; Metering info.

;;; Whether or not a lot of this gets included in
;;; compiled code  will depend upon the COMPILE time
;;; value of *compile-in-storage-metering-info*

(defconstant *max-storage-meters* 8)

(eval-when
    (:compile-toplevel :load-toplevel :execute)
	(defvar *storage-substrate-metering-info* (make-array *max-storage-meters*))
)

(defvar *storage-meter-initializers* nil)

(defvar *storage-meter-entry-report-string* "~6D")
(defvar *storage-meter-reporters* nil)

(defmacro defstorage-meter (name index)
  (let ((access-name (intern (symbol-format nil "STORAGE-SUBSTRATE-~A-METER" name)))
	(init-name (intern (symbol-format nil "INITIALIZE-~A-STORAGE-SUBSTRATE-METER"
				   name)))
	(print-name (intern (symbol-format nil "PRINT-~A-STORAGE-SUBSTRATE-METER"
				   name)))
	(rec-name (intern (symbol-format nil "RECORD-STORAGE-SUBSTRATE-~A-EVENT"
				  name))))
    `(progn
       (defmacro ,access-name ()
	 (svref& *storage-substrate-metering-info* ,index))
       ;; initialization...
       (defun ,init-name ()
	 (setf (svref& *storage-substrate-metering-info* ,index)
	       (make-array (1+ (length *initial-storage-setup-alist*))
			   :element-type 'fixnum
			   :initial-element 0)))
       (unless (member ',init-name *storage-meter-initializers*)
	 (push ',init-name *storage-meter-initializers*))
       ;; printing
       (defun ,print-name (&optional (stream *standard-output*))
	 (format stream "~%~A Statistics~%" ',name)
	 (dotimes (i (length *initial-storage-setup-alist*))
	   (format stream *storage-meter-entry-report-string*
		   (aref (the #+lucid (simple-array (signed-byte 32) (*))
			      #-lucid (array fixnum)
			      (svref& *storage-substrate-metering-info*
				      ,index))
			 i))))
       (unless (member ',print-name *storage-meter-reporters*)
	 (push ',print-name *storage-meter-reporters*))
       ;; recording...
       (defmacro ,rec-name (pos)
	 (if *compile-in-storage-metering-info*
	     `(incf& (aref
		      (the #+lucid(simple-array (signed-byte 32) (*))
			   #-lucid(array fixnum)
			   (svref& *storage-substrate-metering-info* ,',index))
		      ,pos))
	     nil))
       ',name)))


;;;; The meters


(defstorage-meter alloc 0)

(defstorage-meter newly-consed 1)

(defstorage-meter dealloc 2)

(defstorage-meter grown 3)

(defstorage-meter resized 4)


(defun initialize-storage-meters ()
  (dolist (init *storage-meter-initializers*)
    (funcall init)))

(defun print-storage-meters (&optional
			     (verbose nil) (stream *standard-output*))
    (format stream "~%Storage Substrate Statistics...")
    (terpri stream)
    (dolist (pair *initial-storage-setup-alist*)
      (format stream *storage-meter-entry-report-string* (car pair)))
    (terpri stream)
    (when verbose
      (dolist (pf (reverse *storage-meter-reporters*))
	(funcall pf stream)))
    ;; print current pool sizes
    (format stream "~%Current Resource Pool:")
    (terpri stream)
  (let ((total 0))
    (dotimes (i (length *storage-vector-free-lists*))
      (format stream *storage-meter-entry-report-string*
	      (let ((n (length (aref *storage-vector-free-lists* i))))
		(incf total (* n (car (nth i *initial-storage-setup-alist*))))
		n)))
    (format stream "~%Contains a total of ~D words (~D bytes)"
	    total (* 4 total)))
  ;; now print summaries...
  (labels ((total-size (meter-array)
	     (let ((total 0))
	       (do* ((idx 0 (1+ idx))
		     (pairs *initial-storage-setup-alist* (cdr pairs))
		     (size (caar pairs) (caar pairs)))
		    ((null pairs) (values total (aref meter-array idx)))
		 (incf total (* size (aref meter-array idx))))))
	   (meter-summary (meter-array meter-string &optional extra-string)
	     (multiple-value-bind (regular-size extra-value)
		 (total-size meter-array)
	       (if (null extra-string)
		   (format stream "~%~D words (~D bytes) ~A."
			   regular-size (* regular-size 4) meter-string)
		   (format stream "~%~D words (~D bytes) ~A.  ~D ~A"
			   regular-size (* regular-size 4) meter-string
			   extra-value extra-string)))))
    (meter-summary (storage-substrate-alloc-meter) "allocated"
		   "oversized arrays were extended")
    (meter-summary (storage-substrate-dealloc-meter) "deallocated"
		   "odd sized arrays could not be deallocated")
    (meter-summary (storage-substrate-newly-consed-meter) "consed")
    ))

(eval-when (eval load)
  (initialize-storage-meters)
  )

;;; this used to be a redisplay related init but now that we
;;; are using these for lots of other things (like chas-arrays
;;; and graphics-command-lists), they have to be made earlier

(eval-when (eval load)
    (initialize-storage-vector-free-lists)
    (setq *free-list-init-flag* t)
  )

;(defmacro c-vect-dispatch (number &rest what-to-do)
;  `(cond ,@(let ((clauses nil))
;	     (dotimes  (i (length *initial-storage-setup-alist*))
;	       (macrolet ((free-list () `(svref *storage-vector-free-lists* ,i)))
;		 (let ((vect-length (car (nth i *initial-storage-setup-alist*))))
;		   ;; now make each cond clause
;		   (push `((<=& ,number ,vect-length)
;			   ;; now the consequent
;			   . ,what-to-do)
;			 clauses))))
;	     (nreverse clauses))))

;; this is dependent on the values of *initial-storage-setup-alist*
;; but I can't convince the macro to work write so those values are
;; hardcoded into both allocate- and free-c-vector

(defmacro expand-allocate-clauses (key)
  (let ((clauses (list 'cond)))
    (cond ((null *compile-in-storage-metering-info*)
	   (dolist (spec *initial-storage-setup-alist*)
	     (push `((<=& ,key ,(car spec))
		     (or (pop (svref& *storage-vector-free-lists*
				      ,(position
					spec
					*initial-storage-setup-alist*)))
			 (make-array ,(car spec))))
		   clauses))
	   ;; the fall through case...
	   (push `(t (make-array (+ ,key
				    *storage-vector-terminal-growth-quantum*)))
		 clauses))
	  (t
	   ;; otherwise, compile in the metering info
	   (dolist (spec *initial-storage-setup-alist*)
	     (push `((<=& ,key ,(car spec))
		     (let ((new (pop (svref&
				      *storage-vector-free-lists*
				      ,(position
					spec
					*initial-storage-setup-alist*)))))
		       (cond ((null new)
			      (record-storage-substrate-newly-consed-event
			       ,(position spec *initial-storage-setup-alist*))
			      (make-array ,(car spec)))
			     (t
			      (record-storage-substrate-alloc-event
			       ,(position spec *initial-storage-setup-alist*))
			      new))))
		   clauses))
	   ;; the fall through case...
	   (push `(t
		   (record-storage-substrate-alloc-event
		    ,(length *initial-storage-setup-alist*))
		   (make-array (+ ,key
				  *storage-vector-terminal-growth-quantum*)))
		 clauses)))
    (nreverse clauses)))

;+++ this doesn't compile properly first time through
(defun allocate-c-vector (length)
  (check-number-arg length (not (minusp& length)))
  (expand-allocate-clauses length))

(defmacro expand-deallocate-clauses (key arg)
  (let ((clauses (list 'cond)))
    (cond ((null *compile-in-storage-metering-info*)
	   (dolist (spec *initial-storage-setup-alist*)
	     (push `((=& ,key ,(car spec))
		     (push ,arg
			   (svref& *storage-vector-free-lists*
				   ,(position spec
					      *initial-storage-setup-alist*))))
		   clauses)))
	  (t
	   (dolist (spec *initial-storage-setup-alist*)
	     (push `((=& ,key ,(car spec))
		     (record-storage-substrate-dealloc-event
		      ,(position spec *initial-storage-setup-alist*))
		     (push ,arg
			   (svref& *storage-vector-free-lists*
				   ,(position spec
					      *initial-storage-setup-alist*))))
		   clauses))
	   ;; the fall through case only records the fact that we
	   ;; couldn't deallocate an odd sized vector
	   (push `(t (record-storage-substrate-dealloc-event
		      ,(length *initial-storage-setup-alist*)))
		 clauses)))
    ;; no need for a fall through clause, we'll just let the GC handle it
    (nreverse clauses)))

(defun free-c-vector (cvect)
  (let ((length (svlength cvect)))
    (expand-deallocate-clauses length cvect)))


;; we might want to pre-cons some headers but for now, I'll just cons them on
;; the fly
(defun allocate-sv-header ()
  (make-array 2 :initial-contents '(nil 0)))

(defun allocate-storage-vector (length)
  (let ((cvect (allocate-c-vector length))
	(header (allocate-sv-header)))
    (setf (%sv-contents header) cvect)
    header))

(defun free-storage-vector (sv &optional (remove-inferiors? t))
  (let ((contents (%sv-contents sv)))
    (when remove-inferiors?
      (dotimes& (i (%sv-fill-pointer sv)) (setf (svref& contents i) nil)))
    ;; we might have to make sure that there isn't anything big in
    ;; the contents-vector because of the GC consequences
    (free-c-vector contents))
  (setf (%sv-contents sv) nil)
  ;; if we are saving old headers, then do so now
  )


;;; this is called when the number of contents want to exceed the available
;;; room in the current contents vector
(defun grow-storage-vector (sv)
  (let* ((old-cv (%sv-contents sv))
	 (old-length (svlength old-cv))
	 (new-cv (allocate-c-vector (1+& old-length))))
    ;; move the contents to the new vector AND
    ;; bash the locations in the old one
    (dotimes& (i old-length)
      (setf (svref& new-cv i) (svref& old-cv i))
      (setf (svref& old-cv i) nil))
    ;; now install the new-cv
    (setf (%sv-contents sv) new-cv)
    ;; and deallocate the old-cv
    (free-c-vector old-cv))
  sv)

;;; this is more general (can be used to grow a lot at once or to shrink)

(defun resize-storage-vector (sv desired-new-size)
  (let* ((old-cv (%sv-contents sv))
	 (fp (%sv-fill-pointer sv))
	 (new-cv (allocate-c-vector desired-new-size)))
    ;; move the contents to the new vector AND
    ;; bash the locations in the old one
    (if (<=& desired-new-size fp)
	(warn "Not resizing ~A because the fill pointer, ~D, is larger than ~D"
	      sv fp desired-new-size)
	(progn
	  (dotimes& (i fp)
	    (setf (svref& new-cv i) (svref& old-cv i))
	    (setf (svref& old-cv i) nil))
	  ;; now install the new-cv
	  (setf (%sv-contents sv) new-cv)
	  ;; and deallocate the old-cv
	  (free-c-vector old-cv))))
  sv)

(defun sv-assure-room (sv required-room)
  (cond ((<& required-room (storage-vector-max-length sv))
	 sv)
	;; I suppose we ought to be TOTALLY safe and check
	;; AGAIN, AFTER we've called grow-storage-vector once
	(t (grow-storage-vector sv)
	   (sv-assure-room sv required-room))))

(defun slide-sv-contents (sv start-pos distance)
  (cond ((plusp distance)
	 ;; if we're expanding in the positive direction, first make
	 ;; sure that we have room to do so
	 (sv-assure-room sv (+& distance (storage-vector-active-length sv)))
	 ;; then slide the contents
	 (sv-slide-contents-pos (%sv-contents sv) start-pos
				distance (storage-vector-active-length sv)))
	(t
	 (sv-slide-contents-neg (%sv-contents sv) start-pos
				distance (storage-vector-active-length sv)))))

(defun sv-slide-contents-pos (cvect strt-no distance old-active-length)
  (do ((orig-no (-& old-active-length 1) (-& orig-no 1)))
      ((<& orig-no strt-no))
    (setf (svref& cvect (+& orig-no distance)) (svref& cvect orig-no))))

(defun sv-slide-contents-neg (cvect strt-no distance old-active-length)
  (do ((orig-no strt-no (+& orig-no 1)))
      ((>=& orig-no old-active-length))
    (setf (svref& cvect (+& orig-no distance)) (svref& cvect orig-no))))

;;;; The Outside Interface

;;; iteration macros
(defmacro do-vector-contents ((var sv &key start stop index-var-name)
			      &body body)
  (let ((index-var (or index-var-name (gensym))))
    `(let ((last (if (null ,stop)
		     (storage-vector-active-length ,sv)
		     ,stop))
	   (contents (%sv-contents ,sv)))
       (unless (>=& ,(or start 0) last)
	 (do* ((,index-var ,(or start 0) (1+& ,index-var))
	       (,var (svref& contents ,index-var)
		     (svref& contents ,index-var)))
	      ((=& ,index-var last))
	   . ,body)))))

(defmacro do-self-and-next-sv-contents ((var sv thing) &body body)
  (let ((index-var (gensym)))
    `(let* ((last (storage-vector-active-length ,sv))
	    (contents (%sv-contents ,sv))
	    (start (svposition ,thing contents)))
       (unless (or (null start) (>=& start last))
	 (do* ((,index-var start (1+& ,index-var))
	       (,var (svref& contents ,index-var)
		     (svref& contents ,index-var)))
	      ((=& ,index-var last))
	   . ,body)))))

;;; access and info
(defsubst sv-nth (n sv)
  (if (<& n (storage-vector-active-length sv))
      (svref& (%sv-contents sv) n)
      (error "The index, ~D, was beyond the length, ~A, of ~A"
	     n (storage-vector-active-length sv) sv)))

(defsubst sv-place (thing sv)
  (svposition thing (%sv-contents sv)))

;;; munging SV's

(defun sv-append (sv thing)
  (sv-assure-room sv (1+& (storage-vector-active-length sv)))
  (setf (svref& (%sv-contents sv) (storage-vector-active-length sv)) thing)
  (incf& (storage-vector-active-length sv))
  thing)

(defun sv-insert-at (sv pos thing)
  (let ((al (storage-vector-active-length sv)))
    (cond ((=& pos al) (sv-append sv thing))
	  ((>& pos al)
	   (error "Trying to insert something at ~A~%~
                 which is beyond the active length of ~A"
		  pos sv))
	  (t (slide-sv-contents sv pos 1)
	     (setf (svref& (%sv-contents sv) pos) thing)
	     (incf& (storage-vector-active-length sv))
	     thing))))

;;; things should be a LIST
(defun sv-multiple-insert-at (sv pos things)
  (cond ((>& pos (storage-vector-active-length sv))
	 (error "Trying to insert something at ~A~%~
                 which is beyond the active length of ~A"
		  pos sv))
	(t
	 (let ((length (length things))
	       (al (storage-vector-active-length sv))
	       (cpos pos))
	   (sv-assure-room sv (+& al length))
	   (unless (=& cpos al)
	     (slide-sv-contents sv cpos length))
	   (dolist (thing things)
	     (setf (svref& (%sv-contents sv) cpos) thing)
	     (incf& cpos))
	   (incf& (storage-vector-active-length sv) length)))))

(defun sv-insert-before (sv thing before-thing)
  (let ((before-pos (svposition before-thing (%sv-contents sv))))
    (cond ((null before-pos) (sv-insert-at sv 0 thing))
	  (t (sv-insert-at sv before-pos thing)))))

(defun sv-delete-at (sv pos)
  (let ((al (storage-vector-active-length sv)))
    (cond ((>=& pos al)
	   (error "Trying to delete something at ~A~%~
                 which is beyond the active length of ~A"
		  pos sv))
	  ((=& pos (1-& al))
	   (setf (svref& (%sv-contents sv) pos) nil)
	   (decf& (storage-vector-active-length sv))
	   sv)
	  (t (slide-sv-contents sv (1+& pos) -1)
	     (decf& (storage-vector-active-length sv))
	     sv))))

(defun sv-delete-last (sv)
  (setf (svref& (%sv-contents sv) (1-& (storage-vector-active-length sv))) nil)
  (decf& (storage-vector-active-length sv))
  sv)

(defun sv-delete-to-end (sv pos)
  (let ((al (storage-vector-active-length sv)))
    (cond ((>=& pos al))
	  (t
	   (let ((contents (%sv-contents sv)))
	     (do ((i pos (1+& i)))
		 ((=& i al))
	       (setf (svref& contents i) nil)))
	   (setf (storage-vector-active-length sv) pos)
	   sv))))

(defun sv-delete-from-to (sv from to)
  (let ((al (storage-vector-active-length sv))
	(diff (-& to from)))
    (cond ((>& from to)
	   (error "The Starting number: ~S is greater than the ending number ~S"
	    from to))
	  ((>=& from al))
	  ((=& from to)
	   (sv-delete-at sv from))
	  ((>& to al)
	   (sv-delete-to-end sv from))
	  (t
	   (slide-sv-contents sv to (-& diff))
	   (decf& (storage-vector-active-length sv) diff)
	   sv))))

(defun sv-delete-item (sv item)
  (let ((item-pos (svposition item (%sv-contents sv))))
    (cond ((null item-pos))
	  (t (sv-delete-at sv item-pos)))))

(defun sv-delete-from-item-to-end (sv item)
  (let ((item-pos (svposition item (%sv-contents sv))))
    (unless (null item-pos)
      (sv-delete-to-end sv item-pos))))

(defun copy-storage-vector (svector)
  (let* ((fill-ptr (storage-vector-active-length svector))
	 (new-vector (allocate-storage-vector fill-ptr))
	 (new-contents (%sv-contents new-vector))
	 (old-contents (%sv-contents svector)))
    (dotimes& (i fill-ptr)
      (setf (svref& new-contents i) (svref& old-contents i)))
    (setf (storage-vector-active-length new-vector) fill-ptr)
    new-vector))

;; See note at top with `proclaim` usage
;; (eval-when (compile)
;;   (progn
;;     (format t "~&***NOTE: Restoring, SPEED=1, SAFETY=3")
;;     (proclaim '(optimize (speed 1) (safety 3)))))
