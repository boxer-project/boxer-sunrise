;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER -*-

#|


 $Header: simple-stream.lisp,v 1.0 90/01/24 22:17:27 boxer Exp $

 $Log:	simple-stream.lisp,v $
;;;Revision 1.0  90/01/24  22:17:27  boxer
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



  A simple Implementation of a Boxer stream for the chunker.

  Each Boxer stream MUST support:

  simple-read-char
  simple-peek-char
  IWBNI these could be READ-CHAR and PEEK-CHAR instead

  stream-position
  formatting-array


Modification History (most recent at top)

 2/15/03 merged current LW and MCL files, no changes, updated copyright

|#

(in-package :boxer)

;;; setup the defaults
;;; ALL boxer streams should be built out of this

(defclass basic-boxer-stream
    ()
  ()
  )


(defmethod simple-read-char ((self basic-boxer-stream)
			     &optional eof-errorp eof-value recursive-p)
  (declare (ignore eof-errorp eof-value recursive-p))
  (error "SIMPLE-READ-CHAR is not implemented for ~A" self))

(defmethod simple-peek-char (peek-type (self basic-boxer-stream)
				       &optional eof-errorp eof-value
				       recursive-p)
  (declare (ignore peek-type eof-errorp eof-value recursive-p))
  (error "SIMPLE-PEEK-CHAR is not implemented for ~A" self))

(defmethod stream-position ((self basic-boxer-stream))
  (error "STREAM-POSITION is not implemented for ~A" self))

(defmethod formatting-array ((self basic-boxer-stream))
  (error "FORMATTING-ARRAY is not implemented for ~A" self))


(defmethod print-object ((self basic-boxer-stream) stream)
  (format stream "#< ~A (~A) >"
	  (class-name (class-of self)) (stream-position self)))







(defclass simple-row-stream
	  (basic-boxer-stream)
	  ((row :initform nil :accessor row-stream-row)
	   (cha-no :initform 0 :accessor row-stream-cha-no))
	  )

(defun make-simple-row-stream-from-row (row &optional (cha-no 0))
  (let ((ns (allocate-instance (find-class 'simple-row-stream))))
    (setf (row-stream-row ns) row)
    (setf (row-stream-cha-no ns) cha-no)
    ns))

;; (deftype-checking-macros simple-row-stream
    ;; "A Simple Row Stream Used by the CHUNKER.")


(defmethod simple-read-char ((self simple-row-stream)
			     &optional eof-errorp eof-value recursive-p)
  (declare (ignore recursive-p))
    (let ((cha (cha-at-cha-no (row-stream-row self) (row-stream-cha-no self))))
      (cond ((and (null cha) eof-errorp)
	     (error "Reached the EOF in the stream, ~S." self))
	    ((null cha)
	     eof-value)
	    (t	(incf (row-stream-cha-no self))
		cha))))

(defmethod simple-peek-char (peek-type (self simple-row-stream)
				       &optional eof-errorp eof-value
				       recursive-p)
  (declare (ignore peek-type recursive-p))
    (let ((cha (cha-at-cha-no (row-stream-row self) (row-stream-cha-no self))))
      (cond ((and (null cha) eof-errorp)
	     (error "Reached the EOF in the stream, ~S." self))
	    ((null cha)
	     eof-value)
	    (t cha))))

(defmethod stream-position ((self simple-row-stream))
  (slot-value self 'cha-no))

(defmethod formatting-array ((self simple-row-stream))
  (let ((array (make-array (length-in-chas (slot-value self 'row)))))
    (do-row-chas ((cha (slot-value self 'row))
		  (idx 0 (1+& idx)))
      (setf (svref& array idx) cha))
    array))


;;; sometimes it is neccessary to do stream operations on evrows
;;; In particular, BUILD needs to rechunk the contents of evrows
;;; using a different chunk table

(defclass simple-evrow-stream
    (basic-boxer-stream)
  ((array :initform nil :accessor evrow-stream-array)
   (ptr   :initform 0   :accessor evrow-stream-pointer)))

(defvar *format-only-chunk-marker* 'format-only)

(defun make-simple-evrow-stream-from-evrow (evrow superior)
  (let ((stream (allocate-instance 'simple-evrow-stream)))
    (if (null (evrow-original-row evrow))
	;; build it from scratch
	(let ((array (make-array 32 :adjustable t :fill-pointer 0))
	      (length 0))
	  (flet ((chunk-handler (chunk &optional last?)
		   ;; add the left formatting property and pname
		   (do-fi-chas (cha (chunk-left-format chunk))
		     (incf& length) (vector-push-extend cha array))
		   ;; if value is a box, use that instead
		   (cond ((or (and (null (chunk-chunk chunk))
				   (null (chunk-pname chunk)))
			      (eq (chunk-chunk chunk)
				  *format-only-chunk-marker*)))
			 ((not (or (symbolp (chunk-chunk chunk))
				   (numberp (chunk-chunk chunk))))
			  (incf& length)
			  (vector-push-extend (chunk-chunk chunk) array))
			 (t (do-fi-chas (cha (chunk-pname chunk))
			      (incf& length) (vector-push-extend cha array))))
		   (when last?
		     (do-fi-chas (cha (chunk-right-format chunk))
		       (incf& length) (vector-push-extend cha array))))
		 (value-handler (value)
		   ;; make sure it has a space separation
		   (let ((lastchar (aref array (1-& length))))
		     (unless (and (cha? lastchar)
				  (not (char= lastchar #\space)))
		       (incf& length) (vector-push-extend #\space array)))
		   ;; now add the value
		   (cond ((or (numberp value) (symbolp value))
			  (let ((string (format nil "~A" value)))
			    (dotimes (i (length string))
			      (incf& length)
			      (vector-push-extend (aref string i) array))))
			 (t (incf& length)
			    (vector-push-extend value array)))))
	    (do* ((pointers (evrow-pointers evrow) (cdr pointers))
		  (pointer (car pointers) (car pointers))
		  (rawchunk(handle-pointer-for-unboxing pointer superior nil)
			   (handle-pointer-for-unboxing pointer superior nil)))
		 ((null (cdr pointers))
		  (if (chunk-p rawchunk)
		      (chunk-handler rawchunk t)
		      (value-handler rawchunk)))
	      (cond ((chunk-p rawchunk) (chunk-handler rawchunk))
		    (t (value-handler rawchunk)))))
	  (setf (evrow-stream-array stream)
		(let ((simple-array (make-array length)))
		  (dotimes (i length)
		    (setf (svref& simple-array i) (aref array i)))
		  simple-array)
		(evrow-stream-pointer stream) 0))
	;; looks like we can get what we want from the original row
	(setf (evrow-stream-array stream)
	      (formatting-array (evrow-original-row evrow))
	      (evrow-stream-pointer stream) ; should be 0 but make sure
	      0))
    stream))

(defmethod simple-read-char ((self simple-evrow-stream)
			     &optional eof-errorp eof-value recursive-p)
  (declare (ignore recursive-p))
  (with-slots (array ptr) self
    (cond ((>=& ptr (length array))
	   (if eof-errorp
	       (error "EOF reached for ~A" self)
	       eof-value))
	  (t
	   (prog1 (svref& array ptr)
	     (incf& ptr))))))

(defmethod simple-peek-char (peek-type (self simple-evrow-stream)
				       &optional eof-errorp eof-value
				       recursive-p)
  (declare (ignore peek-type recursive-p))
  (with-slots (array ptr) self
    (cond ((>=& ptr (length array))
	   (if eof-errorp
	       (error "EOF reached for ~A" self)
	       eof-value))
	  (t (aref array ptr)))))

(defmethod stream-position ((self simple-evrow-stream))
  (slot-value self 'ptr))

(defmethod formatting-array ((self simple-evrow-stream))
  (slot-value self 'array))





;;; Used for debugging the chunker

(defclass simple-test-stream
    (basic-boxer-stream)
  ((array :initform nil :accessor test-stream-array)
   (ptr :initform 0 :accessor test-stream-ptr))
  )


(defun %make-simple-test-stream (array)
  (let ((st (make-instance 'simple-test-stream)))
    (setf (test-stream-array st) array)
    st))

(defun make-test-stream (&rest contents)
  #+lucid (declare (user::dynamic-extent contents))
  (flet ((item-length (item)
	   (typecase item
	     (string (length item))
	     (symbol (length (symbol-name item)))
             (box 1)
	     (number (length (format nil "~A" item)))
	     (otherwise (warn "Unknown item ,~A, assuming length of 1" item)
			1))))
    (let ((array (make-array (let ((ll 0))
			       (dolist (item contents)
				 (incf& ll (item-length item)))
			       ll)))
	  (idx 0))
      (flet ((append-item (item)
	       (typecase item
		 (string (dotimes (i (length item))
			   (setf (svref& array idx) (aref item i))
			   (incf& idx)))
		 (symbol (let ((string (symbol-name item)))
			   (dotimes (i (length string))
			     (setf (svref& array idx) (aref string i))
			     (incf& idx))))
		 (box (setf (svref& array idx) item)
		      (incf& idx))
		 (number (let ((string (format nil "~A" item)))
			   (dotimes (i (length string))
			     (setf (svref& array idx) (aref string i))
			     (incf& idx))))
		 (otherwise (setf (svref& array idx) item)
			    (incf& idx)))))
	(dolist (item contents)
	  (append-item item))
	(%make-simple-test-stream array)))))

(defmethod simple-read-char ((self simple-test-stream)
			     &optional eof-errorp eof-value recursive-p)
  (declare (ignore recursive-p))
  (with-slots (array ptr) self
    (cond ((>=& ptr (length array))
	   (if eof-errorp
	       (error "EOF reached for ~A" self)
	       eof-value))
	  (t
	   (prog1 (svref& array ptr)
	     (incf& ptr))))))

(defmethod simple-peek-char (peek-type (self simple-test-stream)
				       &optional eof-errorp eof-value
				       recursive-p)
  (declare (ignore peek-type recursive-p))
  (with-slots (array ptr) self
    (cond ((>=& ptr (length array))
	   (if eof-errorp
	       (error "EOF reached for ~A" self)
	       eof-value))
	  (t (svref& array ptr)))))

(defmethod stream-position ((self simple-test-stream))
  (slot-value self 'ptr))

(defmethod formatting-array ((self simple-test-stream))
  ; (copy-seq (slot-value self 'array))
  (slot-value self 'array))



;;; If we want region streams, this is the place to put them

