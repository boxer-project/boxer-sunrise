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

  An implementation of the Box File System which uses local
  files.  That is, all box oriented commands will be built
  out of vanilla common lisp functions which manipulate files.

  This file contains support for access and
  modification of other server worlds.


Modification History (most recent at top)

 2/16/03 merged current LW and MCL files, no diffs, copyright updated

|#

#-(or lispworks mcl) (in-package 'boxnet)
#+(or lispworks mcl) (in-package :boxnet)




;; returns 2 values, a server type symbol (useable by make-instance) and
;; a world name string to be passed to that server

;; use colon to delimit server type
(defvar *server-world-delimiter-cha* #\:)
(defun parse-foreign-world-name (namestring)
  (let ((colpos (position *server-world-delimiter-cha* namestring)))
    (cond ((null colpos)
	   (values (default-foreign-server (get-server)) namestring))
	  (t
	   (values (case (intern (string-upcase (subseq namestring 0 colpos)))
		     ((floppy tape) 'local-floppy-server)
		     (local 'local-world-server)
		     (t (default-foreign-server (get-server))))
		   (subseq namestring (1+ colpos)))))))


;; returns 2 values specifying people and files
;; the first value can either be the keyword :ALL or a list of usernames
;; the 2nd value can either be the keyword :ALL or a list of box
;; specifiers.  A box specifier is a list whose CAR is a BId and whose
;; CDR, (possibly NULL) is a plist acceptable to make-box-from-server-spec
;; The 1st value can be NIL if the line is empty or a comment line

(defun ex-eol? (char) (or (char= char #\return) (char= char #\newline)))
(defun flush-remaining-chars-on-line (filestream)
  (do* ((eof (list 'eof))
	(char (read-char filestream nil eof) (read-char filestream nil eof)))
      ((or (eq char eof) (ex-eol? char)))))

(defvar keyword-package (find-package "KEYWORD"))

(defun string->BId (buffer)
  (let ((hi 0) (low 0) (where :start))
    (dotimes (i (length buffer))
      (let* ((char (aref buffer i))
	     (digit (digit-char-p char)))
	(cond ((and (eq where :start) (not digit)))
	      ((eq where :start) (setq where :hi hi digit))
	      ((and (eq where :hi) (not digit)) (setq where :low))
	      ((not (null digit))
	       (if (eq where :hi)
		   (setq hi (+& digit (*& hi 10)))
		   (setq low (+& digit (*& low 10))))))))
    (make-bid hi low)))

(defun handle-people-spec (spec)
  (if (and (null (cdr spec)) (string-equal (car spec) "all"))
      ':all
      spec))

(defun read-exports-file-line (filestream &optional (buffer *string-buffer*))
  (buffer-clear buffer)
  (do* ((eof (list 'eof))
	(people-spec nil) (box-specs nil) (box-spec nil) (in-box-spec? nil) ;
	(char (read-char filestream nil eof) (read-char filestream nil eof)))
       ((or (eq char eof) (ex-eol? char))
	(cond ((zerop& (fill-pointer buffer)))
		 ((null in-box-spec?)
		  (push (copy-seq buffer) people-spec) (buffer-clear buffer))
		 ((null box-spec)
		  ;; Looks like no plist, is the buffer a BId or "all"
		  (if (string-equal buffer "all")
		      (return (values (handle-people-spec people-spec)
				      ':all))
		      (push (string->BId buffer) box-specs))
		  (buffer-clear buffer))
		 (t (buffer-clear buffer)))
	(values (handle-people-spec people-spec) box-specs))
    (case char
      ((#\space #\tab) ; keep whitespace to help separate BId components
       (vector-push-extend #\space buffer))
      ((#\; #\#)
       (flush-remaining-chars-on-line filestream)
       (return (values people-spec box-specs)))
      (#\: (unless (zerop& (fill-pointer buffer))
	     (push (copy-seq buffer) people-spec)
	     (buffer-clear buffer))
	   (when (null in-box-spec?) (setq in-box-spec? t)))
      (#\( (cond ((null in-box-spec?)
		  (warn "Found an Open Paren in the user spec of ~A"
			filestream))
		 (t
		  (setq box-spec (string->BId buffer))
		  (buffer-clear buffer)
		  (push (cons box-spec
			      (read-exports-file-box-plist filestream))
			box-specs)
		  (setq box-spec nil))))
      (#\, (cond ((zerop& (fill-pointer buffer)))
		 ((null in-box-spec?)
		  (push (copy-seq buffer) people-spec) (buffer-clear buffer))
		 ((null box-spec)
		  ;; Looks like no plist, is the buffer a BId or "all"
		  (if (string-equal buffer "all")
		      (return (values (handle-people-spec people-spec)
				      ':all))
		      (push (string->BId buffer) box-specs))
		  (buffer-clear buffer))
		 (t (buffer-clear buffer))))
      (t (vector-push-extend char buffer)))))


(defun read-exports-file-box-plist (filestream &optional
					       (buffer *string-buffer*))
  (buffer-clear buffer)
  (do* ((eof (list 'eof))
	(plist nil)
	(char (read-char filestream nil eof) (read-char filestream nil eof)))
       ((or (eq char eof) (ex-eol? char) (char= char #\)))
	(unless (zerop& (fill-pointer buffer))
	  (push (copy-seq buffer) plist) (buffer-clear buffer))
	(nreverse plist))
    (case char
      ((#\space #\tab)) ; ignore whitespace
      (#\, (unless (zerop& (fill-pointer buffer))
	     (push (copy-seq buffer) plist) (buffer-clear buffer)))
      (#\: (unless (zerop& (fill-pointer buffer))
	     (push (intern (string-upcase buffer) keyword-package) plist)
	     (buffer-clear buffer)))
      (t (vector-push-extend char buffer)))))


(defmacro do-export-file-lines ((people-spec-var box-spec-var filestream)
				&body body)
  `(do* ((eof (list 'eof)) (test (peek-char nil ,filestream nil eof)
				 (peek-char nil ,filestream nil eof)))
	((eq test eof) nil)
     (multiple-value-bind (,people-spec-var ,box-spec-var)
	 (read-exports-file-line ,filestream)
       . ,body)))

(defun check-foreign-load-permissions (server username)
  (let ((permission-file
	 (make-pathname :name (format nil "~D" (slot-value server 'top-box-id))
			:type (permission-file-extension server)
			:directory (pathname-directory
				    (slot-value server 'directory)))))
    (unless (and (probe-file permission-file)
		 (with-open-file (s permission-file)
		   (do-export-file-lines (people box s)
		     (declare (ignore box))
		     (when (or (eq people :all)
			       (and (listp people)
				    (member username people
					    :test #'string-equal)))
		       (return T)))))
      (server-error "Access to ~A denied for ~A"
		    (slot-value server 'world-name) username))))


;;;; Fake file ...

(defun fake-file? (bid server)
  (probe-file (make-pathname :name (bid-filename bid)
			     :type (permission-file-extension server)
			     :defaults (slot-value server 'directory))))


(defun access-list (bid server)
  (let ((perm-file (make-pathname :name (bid-filename bid)
				  :type (permission-file-extension server)
				  :defaults (slot-value server 'directory)))
	(username (slot-value server 'login-name))
	(all-list nil) (user-list nil))
    (with-open-file (s perm-file :direction :input)
      (do-export-file-lines (user access s)
	(cond ((eq user :all) (setq all-list access))
	      ((or (eq user username)
		   (and (listp user)
			(member username user :test #'string-equal)))
	       (setq user-list access)))))
    (or user-list all-list)))

;;; these check the permission file and can  signal "permission denied" errors
(defun fake-file-info (bid info server)
  (let ((access-list (access-list bid server)))
    (cond ((null access-list)
	   (server-error "You do not have permission to access Box ~D" bid))
	  ((eq access-list :all)
	   (get-box-info-internal bid info (slot-value server 'directory)))
	  (t
	   (let ((vanilla-info (get-box-info-internal
				bid info (slot-value server 'directory))))
	     ;; change the irrelevant slots and return the new info
	     (setf (sbi-inferiors vanilla-info) (mapcar #'car access-list))
	     vanilla-info)))))

(defun fake-file-size (bid server)
  (let ((access-list (access-list bid server)))
    (cond ((null access-list)
	   (server-error "You do not have permission to access Box ~D" bid))
	  ((eq access-list :all)
	   (with-open-file (s (merge-pathnames (bid-filename bid)
					       (slot-value server 'directory))
			      :direction :input
			      :element-type '(unsigned-byte 8))
	     (file-length s)))
	  (t (length access-list)))))

;;; an access-spec is a list whose CAR is a BoxID and an
;;; optional CDR plist
(defun make-box-from-access-spec (spec)
  (let ((fake-box (box::make-box '(())
				 (or (getf (cdr spec) :type) 'boxer::data-box)
				 (getf (cdr spec) :name))))
    ;; now set the various slots and flags to make this a file box
    (setf (slot-value fake-box 'boxer::first-inferior-row) nil)
    (boxer::set-storage-chunk? fake-box t)
    (boxer::putprop fake-box (car spec) :server-box-id)
    (boxer::shrink fake-box)
    fake-box))

(defun fake-file-box-and-info (bid info server)
  (let ((access-list (access-list bid server)))
    (cond ((null access-list)
	   (server-error "You do not have permission to access Box ~D" bid))
	  ((eq access-list :all)
	   (get-box-and-info-internal bid info (slot-value server 'directory)))
	  (t
	   (let ((vanilla-info (get-box-info-internal
				bid info (slot-value server 'directory)))
		 (return-box (boxer::make-box '(()))))
	     ;; change the irrelevant slots and return the new info
	     (setf (sbi-inferiors vanilla-info) (mapcar #'car access-list))
	     ;; now synthesize a new box
	     (dolist (spec access-list)
	       (boxer::append-row return-box
				  (boxer::make-row
				   (list (make-box-from-access-spec spec)))))
	     ;; fix up attributes of the return-box
	     (set-fake-file-box-attributes return-box)
	     (values return-box vanilla-info))))))

;;;

(defun set-fake-file-box-attributes (box)
  ;; you can NEVER write out a fake file box
  (boxer::set-read-only-box? box t)
  (boxer::set-fake-file-box box t))

(defun set-foreign-server-props (box server)
  (boxer::set-foreign-server box t)
  (unless (slot-value server 'shared?) (boxer::set-read-only-box? box t))
  (boxer::putprop box server 'bfs-server))

