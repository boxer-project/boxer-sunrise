;-*- mode:lisp; syntax:common-lisp;  package:boxnet -*-
#|

 $Header$

 $Log$





           Copyright 1991 - 1996 Regents of the University of California

     Enhancements and Modifications Copyright 1999 - 2003 Pyxisystems LLC

                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+

  An implementation of the Box File System which uses local
  files.  That is, all box oriented commands will be built
  out of vanilla common lisp functions which manipulate files.


Modification History (most recent at top)

 2/16/03 merged current LW and MCL files, no diffs, copyright updated


|#

#-(or lispworks mcl) (in-package 'boxnet)
#+(or lispworks mcl) (in-package :boxnet)



;; This variable is a site init (see the file site.lisp for details)
(defvar *local-server-directory* "/usr/local/lib/boxer/Server/")


;;;; Utilities for reading and writing BFS file headers

(defvar *box-file-system-header-termination-string*
  "### End of Header info ### Binary Data Starts Next Line ###")

(defvar *header-buffer* (make-array 100 :element-type '(unsigned-byte 8)
				    :fill-pointer 0 :adjustable t))

(defvar *string-buffer* (make-array 64 :element-type #-lucid 'character
                                                     #+lucid 'string-char
				    :fill-pointer 0 :adjustable t))

;; remember that the stream will have been opened with an
;; element-type of (unsigned-byte 8) for the binary data that follows
;; that means we have to coerce the bytes to chars

(defun bfs-eol? (byte)
  (or (=& byte #.(char-code #\return)) (=& byte #.(char-code #\newline))))

(defun buffer-clear (buffer) (setf (fill-pointer buffer) 0))

(defun whitespace-byte? (byte)
  (or (=& byte #.(char-code #\space)) (=& byte #.(char-code #\tab))))

(defun end-of-header? (filestream &optional (skip-first-char t))
  ;; the 1st byte has already been pulled out of the filestream
  (do* ((eof (list 'eof))
	(term-length (1-& (length
			   *box-file-system-header-termination-string*)))
	(byte (read-byte filestream nil eof)
	      (read-byte filestream nil eof))
	(eoh-idx (if skip-first-char 1 0) (1+& eoh-idx))
	(eoh-char (aref *box-file-system-header-termination-string* eoh-idx)
		  (aref *box-file-system-header-termination-string* eoh-idx)))
       ((>=& eoh-idx term-length)
	;; this means we really have gotten to the end
	(unless (bfs-eol? byte) (flush-remaining-line filestream))
	t)
    (cond ((not (=& byte (char-code eoh-char)))
	   ;; if there is ever a mismatch, return nil
	   (flush-remaining-line filestream)
	   (return nil))
	  ((eq byte eof)
	   ;; if we reach the end of the file,return an additional EOF value
	   (return (values nil 'eof)))
	  ((bfs-eol? byte)
	   ;; if we reach the end of a line, return nil
	   (return nil)))))

(defun flush-remaining-line (filestream)
  (do* ((eof (list 'eof))
	(byte (read-byte filestream nil eof) (read-byte filestream nil eof)))
      ((or (eq byte eof) (bfs-eol? byte)))))

(defun read-bfs-header-line (filestream &optional (string *string-buffer*))
  (declare (values key value))
  (let ((eof (list 'eof)) (key nil))
    (flet ((comment-byte? (byte)
	     (or (=& byte #.(char-code #\#)) (=& byte #.(char-code #\;))))
	   (separator-byte? (byte) (=& byte #.(char-code #\:))))
      ;; first, clear the buffer
      (buffer-clear string)
      ;; now, try and form the key
      (do ((byte (read-byte filestream nil eof)(read-byte filestream nil eof)))
	  ((or (eq byte eof) (bfs-eol? byte))
	   (unless (zerop (length string))
	     (debugging-message "WARNING: incomplete header line [~A]" string))
	   (return-from read-bfs-header-line nil))
	(cond ((and (whitespace-byte? byte) (null key)
		    (not (zerop& (fill-pointer string))))
	       ;; 1st trailing whitespace
	       (setq key (intern string)))
	      ((whitespace-byte? byte)
	       ;; ignore all other whitespace
	       )
	      ((and (separator-byte? byte) (null key))
	       (setq key (intern string)) (return))
	      ((separator-byte? byte) (return))
	      ((comment-byte? byte)
	       ;; check to see it this is the end of header comment
	       ;; flush the rest of the line and return
	       (return-from read-bfs-header-line
		 (when (end-of-header? filestream) 'end-of-header)))
	      (t
	       (vector-push-extend (char-upcase (code-char byte)) string))))
      ;; now that the key is complete and we are at a position
      ;; immediately following the ":", use the key to get a value
      (case key
	((readdate accesslength writelength
		   appendlength inferiors forwarding_table)
	 ;; these keys want a number for their value
	 (multiple-value-bind (number end?)
	     (read-bfs-number-internal filestream)
	   (cond ((null number)
		  (error "Couldn't get a number from ~A for ~A"filestream key))
		 ((null end?)
		  (flush-remaining-line filestream)
		  (values key number))
		 (t (values key number)))))
	((owner boxname)
	 ;; these keys want a string for their value
	 (buffer-clear string)
	 (do ((byte (read-byte filestream nil eof)
		    (read-byte filestream nil eof)))
	     ((or (eq byte eof) (bfs-eol? byte))
	      (values key (copy-seq string)))
	   (unless (whitespace-byte? byte)
	     (vector-push-extend (code-char byte) string))))
	(superiorBID
	 ;; this key wants a BId for its value
	 (multiple-value-bind (number end?)
	     (read-bfs-number-internal filestream)
	   (cond ((not (null end?)) (values key number))
		 (t
		  (let ((top number))
		    (multiple-value-bind (number end?)
			(read-bfs-number-internal filestream)
		      (when (null end?) (flush-remaining-line filestream))
		      (if (null number)
			  (values key top)
			  (values key (make-bid top number)))))))))
	(otherwise
	 (debugging-message "Unhandled Key: ~A" key)
	 nil)))))

;;; specialized line readers (these also assume streams
;;; which are :element-type '(unsigned-byte 8))

;;; a BID line consists of 1 or more BID's layed out as
;;; high 32bit value followed by low 32bit value separated
;;; by whitespace.  If there is more than one BId, then they
;;; should be comma separated.  BId's with only one value will
;;; be assumed to have a high 32bit value of 0.

(defun read-bfs-header-bid-line (filestream)
  (let ((numlist nil) (top 0) (fill-top? t))
    (loop
     (multiple-value-bind (number end?)
	 (read-bfs-number-internal filestream)
       (cond ((not (null end?))
	      ;; complete the numlist for various cases
	      (cond ((and fill-top? (null number))
		     ;; numlist looks good and we have nothing to add
		     )
		    ((and (null fill-top?) (not (null number)))
		     ;; numlist needs another value and we have the last piece
		     (push (make-bid top number) numlist))
		    ((null number)
		     ;; numlist is incomplete but we don't have any
		     ;; more pieces so use what we have(assuming top is bottom)
		     (debugging-message "Incomplete BId: using ~D as low 32"
					top)
		     (push top numlist))
		    (t
		     ;; somthing to add, but looks like no bid being formed to
		     ;; add it to.  Use what we get as the entire BId
		     (debugging-message "Incomplete BId: using ~D as low 32"
					number)
		     (push number numlist)))
	      ;; finally return
	      (if (null (cdr numlist)) ; only one entry
		  (return (car numlist))
		  (progn (setq numlist (nreverse numlist)) numlist)))
	     ((null number)) ; perhaps a warning is appropriate ?
	     ((null fill-top?)
	      (push (make-bid top number) numlist)
	      (setq fill-top? nil))
	     (t
	      (setq top number)
	      (setq fill-top? nil)))))))

(defun make-bid (high low) (dpb high (byte 32. 32.) low))

(defun read-bfs-number-internal (filestream &optional (radix 16.))
  (let ((num nil) (eof (list 'eof)))
    (do ((byte (read-byte filestream nil eof) (read-byte filestream nil eof)))
	((or (eq byte eof) (bfs-eol? byte))
	 (values num t))
      (cond ((and (null num) (or (whitespace-byte? byte)
				 (=& byte #.(char-code #\,))))
	     ;; leading whitespace or delimiter chars, ignore them
	     )
	    ((or (whitespace-byte? byte) (=& byte #.(char-code #\,)))
	     ;; trailing whitespace or delimiter char, so return
	     (return num))
	    (t
	     (let ((x (digit-char-p (code-char byte) radix)))
	       (cond ((null x)
		      (error "Got ~C instead of digit while forming number"
			     (code-char byte)))
		     ((null num) (setq num x))
		     (t (setq num (+& (*& num 10) x))))))))))

;;; reads one or more strings, multiple strings are delineated by
;;;  either comma or whitespace
;;; leading whitespace is ignored

(defun read-bfs-header-string-line (filestream &optional
					       (string-buffer *string-buffer*))
  (let ((stringlist nil) (in-string? nil) (eof (list 'eof)))
    (buffer-clear string-buffer)
    (do ((byte (read-byte filestream nil eof) (read-byte filestream nil eof)))
	((or (eq byte eof) (bfs-eol? byte))
	 (cond ((and (null stringlist) in-string?)
		;; we are in the middle of making a single string
		(copy-seq string-buffer))
	       (in-string?
		;; we are in the middle of making the last of several strings
		(push (copy-seq string-buffer) stringlist)
		(nreverse stringlist)
		stringlist)
	       ((null stringlist)
		;; reached EOL or EOF before we got any strings
		nil)
	       (t (nreverse stringlist) stringlist)))
      (cond ((and in-string? (or (whitespace-byte? byte)
				 (=& byte  #.(char-code #\,))))
	     ;; end of a string
	     (push (copy-seq string-buffer) stringlist)
	     (buffer-clear string-buffer)
	     (setq in-string? nil))
	    ((whitespace-byte? byte)
	     ;; if we are not in-string? then it must be
	     ;; leading whitespace so ignore it
	     )
	    (t
	     ;; it's not whitespace so add it to the buffer
	     (vector-push-extend (code-char byte) string-buffer)
	     (when (null in-string?) (setq in-string? t)))))))

;;; the stream is an '(unsigned-byte 8) stream and is assumed to
;;; have already been positioned to the right place
(defun read-box-server-info (stream info)
  (loop (multiple-value-bind (key value)
	    (read-bfs-header-line stream)
	  (unless (null key) ; ignore empty lines
	    (case key
	      (end-of-header (return))
	      (OWNER (setf (sbi-owner info) value))
	      (READDATE (setf (sbi-read-date info) value))
	      (ACCESSLENGTH
	       ;; obsolete slot, still need to pull out the data
	       (dotimes (i value) (read-bfs-header-string-line stream)))
	      (WRITELENGTH
	       ;; obsolete slot, still need to pull out the data
	       (dotimes (i value) (read-bfs-header-string-line stream)))
	      (APPENDLENGTH
	       ;; obsolete slot, still need to pull out the data
	       (dotimes (i value) (read-bfs-header-string-line stream)))
	      (SUPERIORBID (setf (sbi-superior-box-bid info) value))
	      (INFERIORS (setf (sbi-inferiors info)
			       (let ((bids nil))
				 (dotimes (i value bids)
				   (let ((bid (read-bfs-header-bid-line
					       stream)))
				     (setq bids
					   (nconc bids (if (consp bid)
							   bid
							   (list bid)))))))))
	      (BOXNAME nil) ; do nothing (a separate clause to avoid warning)
	      (FORWARDING_TABLE
	       (setf (sbi-forwarding-table info)
		     (let ((bids nil))
		       (dotimes (i value bids)
			 (let ((bid (read-bfs-header-bid-line
				     stream)))
			   (setq bids
				 (nconc bids (if (consp bid)
						 bid
						 (list bid)))))))))
	      (OTHERWISE (debugging-message "Unhandled BFS Header Key:~A"
					    key))))))
  ;; set the write-date in the info, we get this directly from the file
  (setf (sbi-write-date info) (file-write-date stream))
  info)

;;; grab the 1st to byte of an '(unsigned-byte 8) stream
;;; and make sure they are the beginning of a header
(defun header-start? (s)
  (and (=& (read-byte s) (ldb boxer::%%bin-op-top-half
			      boxer::bin-op-box-server-info-start))
       (=& (read-byte s) (ldb boxer::%%bin-op-low-half
			      boxer::bin-op-box-server-info-start))))

;;; For use by the READ primitive which isn't interested in the header info
;;; another '(unsigned-byte 8) stream
(defun skip-box-server-info (stream)
  (loop (when (end-of-header? stream nil) (return))))

;;; these use streams with :element-type 'string-char
;;; this will work because we can open the file once, write the header,
;;; close it, then open it again with :append in a binary mode to
;;; write the data

(defun write-bsi-bid (stream bid)
  (format stream "~D ~D" (ldb (byte 32. 32.) bid) (ldb (byte 32. 0.) bid)))

(defun write-bsi-list (stream name list)
  (declare (string name) (list list))
  (format stream "~%~A: ~D" name (length list))
  (cond ((null list))
	((stringp (car list))
	 (dolist (item list)  (format stream "~%~A" item)))
	((numberp (car list))
	 (dolist (item list) (terpri stream) (write-bsi-bid stream item)))
	(t (error "The Box server list, ~A, doesn't have strings or BId's"))))

(defun write-header-preamble (stream)
  (write-char (code-char 240) stream)
  (write-char (code-char 57)  stream)
  (terpri stream))

(defun write-header-finish (stream)
  (format stream "~A~%" *box-file-system-header-termination-string*))

(defun write-box-server-info (stream info)
  ;; first, send the 16 bit header opcode
  (write-header-preamble stream)
  ;; now the header data
  (format stream "readdate: ~A~%owner: ~A"
	  (sbi-read-date info) (sbi-owner info))
  (format stream "~%boxname: ~A" (sbi-boxname info))
  (format stream "~%superiorBID: ")
  (write-bsi-bid stream (sbi-superior-box-bid info))
  (write-bsi-list stream "inferiors" (sbi-inferiors info))
  (write-bsi-list stream "forwarding_table" (sbi-forwarding-table info))
  ;; finally write out the end of header line
  (terpri stream)
  (write-header-finish stream))



;;;; User file Utilities

(defvar *line-buffer* (make-array 100 :element-type #-lucid 'character #+lucid 'string-char
				  :fill-pointer 0 :adjustable t))

;;; Searches for a particular user in the user file and
;;; returns the password, pretty-name, directory and top-box-id
(defun user-values (user)
  (declare (values password pretty-name directory top-box-id))
  (with-open-file (s (merge-pathnames "userfile" *local-server-directory*)
		     :direction :input)
    (catch 'eof
      (loop
       (multiple-value-bind (account password pretty-name directory
				     uid top-box-id)
	   (user-line-values s)
	 (when (string-equal account user)
	   (return (values password pretty-name directory
			   uid top-box-id))))))))

;;; Returns a unique user ID
;;; found by scanning the userfile
;;; for now, it just returns MAX+1, we might want to be more
;;; clever in the future by looking for gaps
(defun new-user-id ()
  (with-open-file (s (merge-pathnames "userfile" *local-server-directory*)
		     :direction :input)
    (let ((max-id 0))
      (catch 'eof
	(loop
	 (multiple-value-bind (account password pretty-name directory
				       uid top-box-id)
	     (user-line-values s)
	   (declare (ignore account password pretty-name directory top-box-id))
	   (setq max-id (max uid max-id)))))
      (1+ max-id))))

(defmacro do-server-users ((account password pretty-name
				    directory uid top-box-id)
			   &body body)
  `(with-open-file (s (merge-pathnames "userfile" *local-server-directory*)
		      :direction :input)
     (catch 'eof
       (loop
	(multiple-value-bind (,account ,password ,pretty-name ,directory
				       ,uid ,top-box-id)
	    (user-line-values s)
	  ,@body)))))

;;; stream is supposed to be a char stream and is expected to be
;;; positioned at the beginning of a line
;;; A userfile line consists of colon separated fields.  The fileds are:
;;; Username:Password:Pretty Name:Directory Name:UID:Top Level Box ID
(defun user-line-values (stream)
  (declare (values account password pretty-name directory top-box-id
		   uid-prefix))
  (let ((values (make-array 6)) (idx 0) (eof (list 'eof)))
    (setf (fill-pointer *line-buffer*) 0)
    (do ((char (read-char stream nil eof) (read-char stream nil eof)))
	((or (eq char #\newline) (eq char #\return))
	 (when (< idx (1-& (length values)))
	   (warn "Incomplete user file line"))
	 (setf (svref& values idx)
	       ;; the last value should be coerced into a Box ID
	       (string-to-bid *line-buffer*))
	 (values (svref& values 0) (svref& values 1) (svref& values 2)
		 (svref& values 3) (svref& values 4) (svref& values 5)))
      (cond ((eq char eof)
	     (throw 'eof nil))
	    ((char-equal char #\:)
	     (setf (svref& values idx)
		   (if (=& idx 4) ; the UID needs to coerced into a number
		       (string-to-number *line-buffer*)
		       (copy-seq *line-buffer*)))
	     (incf& idx)
	     (setf (fill-pointer *line-buffer*) 0))
	    (t (vector-push-extend char *line-buffer*))))))

(defun string-to-number (string &optional (idx 0) (radix 10.))
  (let ((number nil) (end-idx (1-& (length string))))
    (do ((char (aref string idx) (aref string idx)))
	((=& idx end-idx) (let ((last-digit (digit-char-p char radix)))
			    (cond ((null last-digit)
				   (values number idx))
				  ((null number)
				   (values last-digit idx))
				  (t
				   (values (+& (*& number radix) last-digit)
					   idx)))))
      (let ((digit (digit-char-p char radix)))
	(cond ((and (null digit) (null number)))
	      ;; whitespace or garbage, ignore it
	      ((null digit)
	       ;; number must be finished
	       (return (values number idx)))
	      ((null number)
	       ;; we've hit the first digit
	       (setq number digit))
	      (t
	       ;; in the middle of making a number
	       (setq number (+& (*& radix number)
				digit)))))
      (incf& idx))))

(defun string-to-bid (string)
  (multiple-value-bind (top idx)
      (string-to-number string 0 16.)
    (cond ((null top) (error "can't get a BID out of ~S" string))
	  (t (let ((bottom (string-to-number string idx 16.)))
	       (if (null bottom) (make-bid 0 top) (make-bid top bottom)))))))



;;;; Filename utilities

;;; NO Leading 0's !!!!!

(defun bid-filename (bid) (format nil "~D" bid))

(defvar *backup-file-suffix* "~")

(defun bid-backup-filename (bid) (format nil "~D~A" bid *backup-file-suffix*))

(defun backup-pathname (pathname)
  (merge-pathnames
   (format nil "~A~A" (pathname-name pathname) *backup-file-suffix*)
   pathname))

(defun bid-file-name? (pathname)
  (every #'digit-char-p (pathname-name pathname)))

(defun backup-bid-file-name? (pathname)
  (let ((name (pathname-name pathname)))
    (and (char= (char name (1-& (length name))) (char *backup-file-suffix* 0))
	 (dotimes (i (1-& (length name)) T)
	   (unless (digit-char-p (char name i)) (return nil))))))

;; probably want to make implementation specific versions of these for speed
(defun get-file-size (pathname) (with-open-file (s pathname) (file-length s)))

;;; this MUST not return until we are sure that the incremented
;;; file count has been written back
;;; This is to prevent possible duplication of allocated Box ID's
;;; in case of a crash while writing.

(defun increment-counter-file (counter-file)
  (when (null (probe-file counter-file))
    (error "The counter file, ~A, is missing" counter-file))
  (let ((old-count 0))
    (with-open-file (s counter-file :direction :input)
      (setq old-count (string-to-number (read-line s))))
    ;; now write back the new count
    (with-open-file (s (merge-pathnames "newcount" counter-file)
		       :direction :output :if-exists :supersede)
      (format s "~D~%" (1+ old-count)))
    ;; now backup the old counter file
    (rename-file counter-file (merge-pathnames "oldcount" counter-file))
    ;; install the new counter file
    (rename-file (merge-pathnames "newcount" counter-file) counter-file)
    (delete-file (merge-pathnames "oldcount" counter-file))
    ;; now if all that has worked, we can safely return the new count
    (1+ old-count)))

;;; these are for locking individual files
;;; for groups, see with-single-transaction
;;; these should use gensyms or something to be able
;;; to recognize who set the lock

(defmacro with-write-lock ((file lock-string) &body body)
  `(let ((lock-file (merge-pathnames ".LOCK" ,file)))
     (if (probe-file lock-file)
	 (server-error "The file, ~A, is locked for ~A"
		       ,file (with-open-file (ls lock-file)
			       (read-line ls nil nil)))
	 (unwind-protect
	      (progn
		(with-open-file (ls lock-file :direction :output)
		  (format ls "~A~%" ,lock-string))
		. ,body)
	   (delete-file lock-file)))))

(defmacro with-lock-check (open-file-args &body body)
  `(let ((lock-file (merge-pathnames ".LOCK" ,(cadr open-file-args))))
     (if (probe-file lock-file)
	 (server-error "The file, ~A, is locked for ~A"
		       ,(cadr open-file-args)
		       (with-open-file (ls lock-file) (read-line ls nil nil)))
	 (with-open-file ,open-file-args . ,body))))



;;; records all file activity so that it can be processed in
;;; one chunk.  Any errors in the body should leave the state
;;; of the file system untouched.

;;; File transactions run in 3 phases
;;;
;;;   o in phase 1, boxer structure is dumped and info's are updated
;;;     with any new info appearing in the temp directory.
;;;     Phase 1 transactions, including recording, occur in the body
;;;     of the code.
;;;   o in phase 2, files are moved into the main directory,
;;;     old files are backed up and files are deleted.
;;;     Phase 2 transaction play through the *current-transactions*
;;;     list and are handled in Finalize-transactions
;;;   o If phase 1 and 2 complete, then the temp directory is cleared
;;;     if we are losing, then we try and undo any transactions which
;;;     have affected the main directory.  Phase 3 transaction run in
;;;     an unwind-protect as Undo-Transactions and Clear-Transactions
;;;

(defvar *transaction-sub-directory* "temp")

(defvar *transaction-log-file* "transactions.log")

(defvar *log-transactions?* t)

(defvar *inside-transaction-body* nil)

(defvar *current-transactions* nil)

(defvar *phase-2-transactions* nil)

;; the last step of finalize-transactions should set this to T
(defvar *transaction-complete* nil)

(defun record-bfs-transaction (trans-type &rest args)
  (if *inside-transaction-body*
      (setq *current-transactions*
	    (nconc *current-transactions* (list (cons trans-type args))))
      (error "No context to record a File system transaction")))

;;; possible transaction types that this might handle are:
;;;  NEW-FILE (current-filename destination-filename)
;;;  NEW-COPY (current-filename destination-filename)
;;;  DELETE-FILE (current-filename)
(defmacro with-single-bfs-local-transaction (&body body)
  `(let ((*inside-transaction-body* t)
	 (*current-transactions* nil)
	 (*transaction-complete* nil)
	 (*phase-2-transactions* nil))
     (flet ((finalize-transactions ()
	      (debugging-message "Performing Phase 2 transactions...")
	      (dolist (trans *current-transactions*)
		(ecase (car trans)
		  (new-file
		   (when (probe-file (caddr trans))
		     ;; check for an existing file and back it up if there is
		     (rename-file (caddr trans)
				  (backup-pathname (caddr trans))))
		   (rename-file (cadr trans) (caddr trans)))
		  (new-copy
		   ;; there should NOT be an existing file
		   ; (when (probe-file (caddr trans)) (error "..."))
		   (rename-file (cadr trans) (caddr trans)))
		  (delete-file
		   (delete-bid-box-internal-1 (cadr trans))))
		;; after each successful, phase 2 operation, mark it down
		(push trans *phase-2-transactions*))
	      ;; finally mark that we are done and we can also clear teh
	      ;; phase-2-transactions list
	      (setq *transaction-complete* t
		    *phase-2-transactions* nil))
	    (undo-transactions ()
	      ;; this tries to undo the effects of any phase 2 transactions
	      ;; which may have been performed prior to an error condition
	      (dolist (trans *phase-2-transactions*)
		(ecase (car trans)
		  (new-file
		   ;; if there is a backup available, use it, otherwise
		   ;; be satisfied with removing the file
		   (let ((backup (merge-pathnames
				  (format nil "~A~A"
					  (pathname-name (caddr trans))
					  *backup-file-suffix*)
				  (caddr trans))))
		     (if (probe-file backup)
			 (rename-file backup (caddr trans))
			 (delete-file (caddr trans)))))
		  (new-copy
		   ;; just remove the file
		   (delete-file (caddr trans)))
		  (delete-file
		   ;; bring the file back from the delete dir
		   (let ((del-file (make-pathname
				    :name (pathname-name (cadr trans))
				    :directory
				    (append (pathname-directory (cadr trans))
					    (list
					     *delete-subdirectory-name*)))))
		     (if (probe-file del-file)
			 (rename-file del-file (cadr trans))
			 (warn "Could not undo transaction: ~A" trans)))))))
	    (clear-transactions ()
	      ;; this removes all temp files
	      (dolist (trans *current-transactions*)
		(ecase (car trans)
		  ((new-file new-copy) (delete-file (cadr trans)))
		  (delete-file nil)))))
     (unwind-protect
	  (progn
	    ;; perhaps we should write a descriptor file into the temp dir now
	    (progn . ,body)
	    (finalize-transactions))
       (when (null *transaction-complete*) (undo-transactions))
       (clear-transactions *current-transactions*)))))


;;; it should be a big performance boost to carefully tune
;;; this piece here for different implementations
(defun %bfs-copy-stream (in out)
  (do* ((eof (list 'eof))
	(byte (read-byte in nil eof) (read-byte in nil eof)))
       ((eq byte eof))
    (write-byte byte out)))



;;;; Top Level Utilities

(defun get-box-info-internal (bid info directory)
  (with-lock-check (s (merge-pathnames (bid-filename bid) directory)
		      :direction :input :element-type '(unsigned-byte 8))
    ;; first grab the 1st 2 bytes and make sure we have a header
    (unless (header-start? s)
      (error "~A does not have a File System Header"
	     (merge-pathnames (bid-filename bid) directory)))
    ;; we should now be positioned at the beginning of the header
    (read-box-server-info s info)
    info))

(defun set-box-info-internal (bid new-info directory)
  (let ((dest-path (merge-pathnames (bid-filename bid) directory))
	(tmp-path (if (null *inside-transaction-body*)
		      (merge-pathnames (gensym) directory)
		      (make-pathname :name (bid-filename bid)
				     :directory
				     (append (pathname-directory (pathname
								  directory))
					     (list
					      *transaction-sub-directory*))))))
    (with-write-lock (dest-path (format nil "Setting info for ~A on ~A"
					(unless (null *box-server-user*)
					  (bsui-username *box-server-user*))
					(unless (null *box-server-user*)
					  (bsui-hostname *box-server-user*))))
      (with-open-file (s dest-path :direction :input
			 :element-type '(unsigned-byte 8))
	;; first grab the 1st 2 bytes and make sure we have a header
	(unless (header-start? s)
	  (error "~A does not have a File System Header"
		 (merge-pathnames (bid-filename bid) directory)))
	;; we should now be positioned at the beginning of the header
	(skip-box-server-info s)
	;; we should now be positioned at the beginning of the data
	;; now write the new header out into the temp file name
	(with-open-file (out tmp-path :direction :output)
	  (write-box-server-info out new-info))
	;; now write the data out (the file had better be there)
	(with-open-file (out tmp-path
			     :direction :output
			     :element-type '(unsigned-byte 8)
			     :if-exists :append :if-does-not-exist :error)
	  (%bfs-copy-stream s out)))
      ;; now carefully put everything where it is supposed to go
      ;; or else record the transaction and put it there later
      (cond ((null *inside-transaction-body*)
	     (rename-file dest-path (merge-pathnames (bid-backup-filename bid)
						     directory))
	     (rename-file tmp-path dest-path))
	    (t
	     (record-bfs-transaction 'new-file tmp-path dest-path))))))

(defun get-box-and-info-internal (bid info directory)
  (let ((file (merge-pathnames (bid-filename bid) directory)))
    (when (null (probe-file file)) (box::maybe-uncompress-file file))
    ;; if it STILL isn't here, then signal an error
    (when (null (probe-file file)) (server-error "Couldn't find ~A" file))
    (with-open-file (s file :direction :input
		       :element-type '(unsigned-byte 8))
      ;; We parse the header first
      (unless (header-start? s)
	;; make sure we have a header
	(error "~A does not have a File System Header"
	       (merge-pathnames (bid-filename bid) directory)))
      ;; we should now be positioned at the beginning of the header
      (read-box-server-info s info)
      ;; the info slots should now be filled
      ;; Now we read in the box, the stream should be at the start of
      ;; binary data with a bin-op-format-version word coming next
      ;; we do it in an environment where the forwarding table is bound
      (let ((*loading-via-box-server?* T))
	(let ((newbox (with-forwarding-table-bound (bid info)
			(boxer::load-binary-box-from-stream-internal s))))
	  (values newbox info))))))


(defun set-box-and-info-internal (bid box info directory)
  (let ((dest-path (merge-pathnames (bid-filename bid) directory))
	(tmp-path (if (null *inside-transaction-body*)
		      (merge-pathnames (gensym) directory)
		      (make-pathname :name (bid-filename bid)
				     :directory
				     (append (pathname-directory (pathname
								  directory))
					     (list
					      *transaction-sub-directory*)))))
	(backup-path (merge-pathnames (bid-backup-filename bid) directory)))
    (with-write-lock (dest-path (format nil "Writing box and info for ~A on ~A"
					(unless (null *box-server-user*)
					  (bsui-username *box-server-user*))
					(unless (null *box-server-user*)
					  (bsui-hostname *box-server-user*))))
      ;; check for existence of file first
      (cond ((not (null *inside-transaction-body*))
	     (set-box-and-info-internal-1 tmp-path box info)
	     (record-bfs-transaction 'new-file tmp-path dest-path))
	    ((probe-file dest-path)
	     (unwind-protect
		  (progn
		    (set-box-and-info-internal-1 tmp-path box info)
		    ;; backup
		    (rename-file dest-path backup-path)
		    ;; install
		    (rename-file tmp-path dest-path))
	       ;; make sure the temporary file is removed
	       (when (probe-file tmp-path) (delete-file tmp-path))
	       ;; make sure that there is the original, if there isn't
	       ;; bring the backup file back if we can
	       (when (and (null (probe-file dest-path))
			  (probe-file backup-path))
		 (rename-file backup-path dest-path))))
	    (t
	     (set-box-and-info-internal-1 dest-path box info))))))

(defun set-box-and-info-internal-1 (file box info)
  ;; first, write out the header
  (with-open-file (out file :direction :output)
    (write-box-server-info out info))
  ;; and now, dump out the box
  (with-open-file (out file
		       :direction :output :element-type '(unsigned-byte 8)
		       :if-exists :append :if-does-not-exist :error)
    (box::dump-top-level-box-to-stream box out)))


;;; This needs to do the following:
;;;   o copy all inferiors keeping track of the old and new BId's
;;;   o update the header of the top level box reflecting the (possibly)
;;;     new superior BId as well as a forwarding table for the inferiors
;;;   o copy the data part of the top level box
;;;   o Returns the BID of the copy
(defun copy-bid-box-internal (bid superior-bid
				  from-directory to-directory new-uid-prefix)
  (let* ((new-bid (make-bid new-uid-prefix
			    (increment-counter-file
			     (merge-pathnames "counter" to-directory))))
	 (temp-info (%make-server-box-info))
	 (old-pathname (merge-pathnames (bid-filename bid) from-directory))
	 (new-pathname (merge-pathnames (bid-filename new-bid) to-directory))
	 (tmp-pathname (if (null *inside-transaction-body*)
			   new-pathname
			   (make-pathname :name (pathname-name new-pathname)
					  :directory
					  (append
					   (pathname-directory new-pathname)
					   (list
					    *transaction-sub-directory*)))))
	 (new-forwarding-table nil)
	 (counter-file (merge-pathnames "counter" to-directory)))
    (dolist (inf (get-all-bid-inferiors bid))
      (let ((new-inf-bid (make-bid new-uid-prefix
				   (increment-counter-file counter-file))))
	(cond ((null *inside-transaction-body*)
	       (bfs-copy-file (merge-pathnames (bid-filename inf)
					       from-directory)
			      (merge-pathnames (bid-filename new-inf-bid)
					       to-directory)))
	      (t
	       (let* ((dest (merge-pathnames (bid-filename new-inf-bid)
					       to-directory))
		      (temp (make-pathname :name (pathname-name dest)
					   :directory
					   (append
					    (pathname-directory dest)
					    (list
					     *transaction-sub-directory*)))))
		 (bfs-copy-file (merge-pathnames (bid-filename inf)
						 from-directory)
				temp)
		 (record-bfs-transaction 'new-copy temp dest))))
	(setq new-forwarding-table (append new-forwarding-table
					   (list bid new-inf-bid)))))
    ;; write out new header info
    (with-open-file (s old-pathname :direction :input
		       :element-type '(unsigned-byte 8))
      (unless (header-start? s)
	;; make sure we have a header
	(error "~A does not have a File System Header" old-pathname))
      ;; we should now be positioned at the beginning of the header
      (get-box-info-internal bid temp-info from-directory)
      ;; we are now positioned at the beginning of the data in the old file
      ;; change the relevant fields in the info
      (setf (sbi-bid temp-info)              new-bid
	    (sbi-superior-box-bid temp-info) superior-bid
	    (sbi-forwarding-table temp-info) (combine-forwarding-tables
					      (sbi-forwarding-table temp-info)
					      new-forwarding-table))
      ;; now write the new header out
      (with-open-file (out tmp-pathname :direction :output)
	(write-box-server-info out temp-info))
      ;; now copy the data over
      (with-open-file (out tmp-pathname :direction :output
			   :element-type '(unsigned-byte 8)
			   :if-exists :append :if-does-not-exist :error)
	(%bfs-copy-stream s out)))
    (unless (null *inside-transaction-body*)
      (record-bfs-transaction 'new-copy tmp-pathname new-pathname))
    new-bid))

;; recurses through inferiors
(defun get-all-bid-inferiors (bid &optional include-top?)
  (let ((all-infs (when include-top? (list bid))))
    (dolist (inf (get-bid-inferiors bid))
      (setq all-infs (append all-infs (get-all-bid-inferiors inf t))))
    all-infs))

(defun bfs-copy-file (from to)
  (with-open-file (in from :direction :input :element-type '(unsigned-byte 8))
    (with-open-file(out to :direction :output :element-type '(unsigned-byte 8))
      (%bfs-copy-stream in out))))

;;; these 2 functions are identical to FIND-BID and MERGE-FORWARDING-TABLES
;;; in the client part of the code.  We use 2 versions to preserve the
;;; client/server abstraction
(defun bfs-local-find-bid (bid table)
  (do* ((remaining-pairs table (cddr remaining-pairs))
	(current-bid (car remaining-pairs) (car remaining-pairs)))
       ((null current-bid) nil)
    (when (= current-bid bid)
      (return (cadr remaining-pairs)))))

(defun combine-forwarding-tables (old new)
  (let ((newnew nil))
    ;; loop through the old looking for an entry in the new
    (do* ((remaining-pairs old (cddr remaining-pairs))
	  (orig-bid  (car remaining-pairs) (car remaining-pairs))
	  (trans-bid (cadr remaining-pairs) (cadr remaining-pairs))
	  (new-trans (bfs-local-find-bid trans-bid new)))
	 ((null orig-bid))
      (cond ((bfs-local-find-bid orig-bid new)
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
    (append newnew new)))

;; don't really delete the files, just move them to the
;; deleted files directory.  We rely on some other mechanism to
;; actually remove the files from the delete directory (an expunge primitive ?)
;; Also needs to plice the deleted BId out of the inferiors of the
;; superior file box.

(defvar *delete-subdirectory-name* ".Deleted")

;; moves the (possible) backup filename too
(defun delete-bid-box-internal-1 (file &optional dir)
  (let ((backup (backup-pathname file))
	(del-dir (append (if dir
			     (pathname-directory dir)
			     (pathname-directory file))
			 (list *delete-subdirectory-name*))))
    (when (probe-file backup)
      (rename-file backup
		   (make-pathname :name (pathname-name backup)
				  :directory del-dir)))
    (rename-file file
		 (make-pathname :name (pathname-name file)
				:directory del-dir))))

;; need to recurse through the inferiors as well
(defun delete-bid-box-internal (bid directory)
  (let ((file-to-delete (merge-pathnames (bid-filename bid) directory)))
    (unless (null (probe-file file-to-delete))
      ;; loop through the inferiors...
      (dolist (inf (get-bid-inferiors bid))
	(delete-bid-box-internal inf directory))
      (if (null *inside-transaction-body*)
	  ;; move the file right away
	  (delete-bid-box-internal-1 file-to-delete (pathname directory))
	  ;; or else record it for later handling
	  (record-bfs-transaction 'delete-file file-to-delete)))))



;;;; Ring out the old, ring in the new....
;;; Only useful for converting old Sun Files so...

#+sun
(progn

  (defvar *old-file-system-directory* "/usr/emstsun/boxer/user/bin/FILES/")

  (defun convert-old-file (bid &optional (new-directory (lcl::pwd))(level 0))
    (let* ((old-data-file-name (merge-pathnames
				(string-downcase (format nil "~16,'0X" bid))
				*old-file-system-directory*))
	   (old-head-file-name (merge-pathnames ".d" old-data-file-name))
	   (new-file-name (merge-pathnames (bid-filename bid) new-directory)))
      (cond ((and (probe-file old-data-file-name)
		  (probe-file old-head-file-name))
	     (terpri) (dotimes (i (* 3 level)) (write-char #\space))
	     (format t "Converting old ~X file to new ~D file" bid bid)
	     ;; first generate the new header
	     (with-open-file (new-s new-file-name :direction :output)
	       (write-header-preamble new-s)
	       ;; use the old info file
	       (with-open-file (old-s old-head-file-name :direction :input)
		 (do* ((eof (list 'eof))
		       (char (read-char old-s nil eof)
			     (read-char old-s nil eof)))
		      ((eq char eof))
		   (write-char char new-s)))
	       (write-header-finish new-s))
	     ;; now copy the old binary data
	     (with-open-file (new-s new-file-name :direction :output
				    :element-type '(unsigned-byte 8)
				    :if-exists :append
				    :if-does-not-exist :error)
	       (with-open-file (old-s old-data-file-name :direction :input
				      :element-type '(unsigned-byte 8))
		 (do* ((eof (list 'eof))
		       (byte (read-byte old-s nil eof)
			     (read-byte old-s nil eof)))
		      ((eq byte eof))
		   (write-byte byte new-s))))
	     t)
	    (t
	     (terpri) (dotimes (i (* 3 level)) (write-char #\space))
	     (format t "~%Data or Header file missing for ~16,'0X" bid)
	     nil))))

  ;; you have to first,
  ;; (1) make the new directory
  ;; (2) convert the top level box by hand and place it in the new directory

  ;; this uses internal functions in order to avoid having an active server

  (defun convert-user-files (top-bid &optional (new-dir (lcl::pwd))(level 0))
    (let ((info (%make-server-box-info)))
      ;; read the info
      (get-box-info-internal top-bid info new-dir)
      (let ((infs (sbi-inferiors info)))
	(dolist (bid infs)
	  (if (convert-old-file bid new-dir level)
	      (convert-user-files bid new-dir (1+ level))
	      (progn
		(format t "  ...Removing BID: ~D from inferiors" bid)
		(setf (sbi-inferiors info)
		      (delete bid (sbi-inferiors info) :test #'=)))))
	;; now write out the new info if the inferiors have changed
	(unless (equal infs (sbi-inferiors info))
	  (set-box-info-internal top-bid info new-dir)))))

) ; end of old sun specific conversion utilities




;;;; Debugging Help

(defun calculate-inferiors (box)
  (let ((infs nil))
    (box::map-over-all-inferior-boxes
     box #'(lambda (b)
	     (when (box::storage-chunk? b)
	       (let ((id (box::getprop b :server-box-id)))
		 (unless (null id) (push id infs))))))
    infs))

