;;; -*- Mode:lisp;Syntax:Common-Lisp; Package:BOXER; Base:10.-*-

#|


 $Header$

 $Log$

 Copyright 1991 - 1996 Regents of the University of California

 Enhancements and Modifications Copyright 1999 - 2003 Pyxisystems LLC

                                          +-Data--+
                 This file is part of the | BOXER | system
                                          +-------+


 This file contains utilies for searching boxer structure


Modification History (most recent at top)

 2/15/03 merged current LW and MCL files, no diffs, updated copyright


|#


#-(or lispworks mcl lispm) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+(or lispworks mcl)       (in-package :boxer)


(defvar *read-in-files-during-search* nil)

(defvar *case-sensitive-search* nil)

(defvar *search-inside-ports* nil)

;; this might get more complicated latter
(defstruct box-pattern
  (name nil)
  (type :unspecified))

(defun search-from (pattern bp stop-box)
  (let* ((start-row (bp-row bp))
	 (start-cha-no (bp-cha-no bp))
	 (start-box (superior-box start-row)))
    (catch 'pattern-found
      (loop
       (do ((row start-row (next-row row))
	    (cha-no start-cha-no 0))
	   ((null row) nil)
	 (multiple-value-bind (found-row found-cha-no)
	     (search-in-row row pattern cha-no)
	   (unless (null found-row)
	     (throw 'pattern-found (values found-row found-cha-no
					   (pattern-offset pattern))))))
       (when (or (null start-box) (eq start-box stop-box)
		 (null (setq start-row (superior-row start-box))))
	 (return nil))
       (setq start-cha-no (1+ (cha-cha-no start-row start-box))
	     start-box (superior-box start-row))))))

(defun rsearch-from (pattern bp stop-box)
  (let* ((start-row (bp-row bp))
	 (start-cha-no (bp-cha-no bp))
	 (start-box (superior-box start-row)))
    (catch 'pattern-found
      (loop
       (do ((row start-row (cond ((and (eq row (first-inferior-row start-box))
				       (not (eq row (slot-value start-box
								'closets))))
				  (slot-value start-box 'closets))
				 (t (previous-row row)))))
	   ((or (null row) (minusp& start-cha-no)) nil)
	 (multiple-value-bind (found-row found-cha-no)
	     (rsearch-in-row row pattern (if (eq row start-row)
					     (if (=& start-cha-no
						     (length-in-chas row))
						 (1-& start-cha-no)
						 start-cha-no)
					     (1-& (length-in-chas row))))
	   (unless (null found-row)
	     (throw 'pattern-found (values found-row found-cha-no)))))
       (when (or (null start-box) (eq start-box stop-box)
		 (null (setq start-row (superior-row start-box))))
	 (return nil))
       (setq start-cha-no (1- (cha-cha-no start-row start-box))
	     start-box (superior-box start-row))))))


(defmethod search-in-row ((self row) pattern &optional (start 0))
  (let* ((ca (slot-value self 'chas-array))
	 (pend (storage-vector-active-length pattern))
	 (end (storage-vector-active-length ca)))
    (do ((cha-no start (1+& cha-no)))
	((=& cha-no end) nil)
      (cond ((pattern-match? pattern self cha-no 0 pend ca)
	     (return (values self cha-no)))
	    ((box? (sv-nth cha-no ca))
	     ;; if it is a box and the search has failed, recurse
	     ;; downwards (depth first searching)
	     (multiple-value-bind (r c)
		 (search-in-box (sv-nth cha-no ca) pattern)
	       (unless (null r) (return (values r c)))))))))

(defmethod rsearch-in-row ((self row) pattern
			   &optional (start (1-& (length-in-chas self))))
  (let* ((ca (slot-value self 'chas-array))
	 (pend (storage-vector-active-length pattern)))
    (do ((cha-no start (1-& cha-no)))
	((minusp& cha-no) nil)
      (cond ((pattern-match? pattern self cha-no 0 pend ca)
	     (return (values self cha-no)))
	    ((box? (sv-nth cha-no ca))
	     ;; if it is a box and the search has failed, recurse
	     ;; downwards (depth first searching)
	     (multiple-value-bind (r c)
		 (rsearch-in-box (sv-nth cha-no ca) pattern)
	       (unless (null r) (return (values r c)))))))))

(defmethod search-in-box ((self box) pattern)
  (let ((closet (slot-value self 'closets)))
    (multiple-value-bind (crow ccha-no)
	(unless (null closet) (search-in-row closet pattern))
      (if (not (null crow))
	  (values crow ccha-no)
	  (do-box-rows ((row self))
	    (multiple-value-bind (trow cha-no)
		(search-in-row row pattern)
	      (unless (null trow) (return (values trow cha-no)))))))))

(defmethod rsearch-in-box ((self box) pattern)
  (do ((row (last-inferior-row self) (previous-row row)))
      ((null row)
       ;; if the regular rows have been searched, try the closet
       (let ((closet (slot-value self 'closets)))
	 (unless (null closet) (rsearch-in-row closet pattern))))
    (multiple-value-bind (trow cha-no)
	(rsearch-in-row row pattern)
      (unless (null trow) (return (values trow cha-no))))))

(defmethod search-in-box ((self port-box) pattern)
  (declare (ignore pattern))
  (when (not (null *search-inside-ports*))
    )
  )

(defun pattern-offset (pattern)
  (let ((row 0) (cha-no 0))
    (do-vector-contents (cha pattern)
      (cond ((eq cha :search))
	    ((eq cha :newline) (incf row) (setq cha-no 0))
	    (t (incf cha-no))))
    (if (zerop row) cha-no (cons row cha-no))))

(defun pattern-match? (pattern row start
			       &optional (pstart 0)
			       (pend (storage-vector-active-length pattern))
			       (chas-array (chas-array row)))
  (do ((pidx pstart (1+& pidx))
       (row-end (storage-vector-active-length chas-array))
       (cha-no start))
      ((=& pidx pend) t)
    (let ((pcha (sv-nth pidx pattern)))
      (cond ((eq pcha :search))
	    ((eq pcha :newline)
	     (if (=& cha-no row-end)
		 ;; if at the end of the row, continue onto the next row
		 (if (null (next-row row))
		     (return nil)
		     (pattern-match? pattern (next-row row) 0 (1+& pidx) pend))
		 ;; otherwise, lose
		 (return nil)))
	    ((=& cha-no row-end)
	     ;; we've reached the end of the row but there are still
	     ;; characters in the pattern so return nil
	     (return nil))
	     ;; looks like there is still both room in the row and the pattern
	    ((search-match? (sv-nth cha-no chas-array) pcha)
	     ;; as long as we match, continue
	     (incf& cha-no))
	    (t
	     ;; looks like there is no match so return nil
	     (return nil))))))


;;; this is a hook to (eventually) get more complicated
;;; Note that the args are asymetric, the row object arg comes before
;;; the pattern arg.  This matters with boxes inthe pattern
(defun search-match? (row-obj pattern-obj)
  (cond ((and (cha? row-obj) (cha? pattern-obj))
	 (if *case-sensitive-search*
	     (char= row-obj pattern-obj) (char-equal row-obj pattern-obj)))
	((or (cha? row-obj) (cha? pattern-obj)) nil)
	;; hooks here for box pattern searching....
	((not (null (box-pattern-name pattern-obj)))
	 (let ((nr (name-row row-obj)))
	   (and nr (search (search-string (box-pattern-name pattern-obj))
			   (text-string nr))
		(or (eq (box-pattern-type pattern-obj) :unspecified)
		    (eq (box-pattern-type pattern-obj) (type-of row-obj))))))
	(t
	 (or (eq (box-pattern-type pattern-obj) :unspecified)
	     (eq (box-pattern-type pattern-obj) (type-of row-obj))))))




;;;; The search mode
;;; we need to use a mode instead of an internal polling loop
;;; so that keyboard macros will work right
;;; also, the Mac version of an internal polling loop would
;;; present some porting difficulties (no peek-event)

(defclass search-mode
    (basic-mode)
  ((current-position :initform nil) ; NOT the same as the *point*
   (bp-stack :initform nil)
   (stop-box :initform nil)
   (pattern  :initform (make-storage-vector))
   (box-pattern :initform nil)
   (direction :initform 1))
  (:metaclass block-compile-class))

(block-compile-epilogue search-mode)

;;; If the key is not in search mode, then we need to exit
;;; search mode
(defmethod lookup-mode-key ((self search-mode) key-name)
  (let ((inkey (call-next-method)))
    (cond ((null inkey)
	   (save-search-mode self)
	   (reset-search-mode self)
	   (clear-all-pattern-messages)
	   (remove-mode self)
	   nil)
	  (t inkey))))

(defmethod reset-search-mode ((mode search-mode))
  (setf (slot-value mode 'current-position) nil
	(slot-value mode 'bp-stack) nil
	(slot-value mode 'stop-box) nil
	(slot-value mode 'box-pattern) nil)
  (clear-storage-vector (slot-value mode 'pattern)))

(defvar *previous-search* nil)

;; this is called aftger the end of a successful (or at least satisfying)
;; search, so we save the pattern string for subsequent searches and
;; also mark the original start point
(defmethod save-search-mode ((mode search-mode))
  ;; save the search for subsequent searches
  (setq *previous-search*
	(list (slot-value mode 'stop-box)
	      (copy-storage-vector (slot-value mode 'pattern))))
  ;; mark the original starting point
  (let ((starting-bp (car (last (slot-value mode 'bp-stack)))))
    (unless (null starting-bp)
      (let ((*point* starting-bp))
	(status-line-display 'boxer-editor-error "Mark set")
	(push (%record-current-place) *previous-place-stack*)))))

(defmethod restore-search-mode ((mode search-mode)
				&optional (old *previous-search*))
  (unless (null old)
    (setf (slot-value mode 'stop-box)
	  ;; prefer the higher
	  (if (superior? (point-box) (car old)) (car old) (point-box)))
    ;; should already be clear, but make sure
    (let ((pattern (slot-value mode 'pattern)))
      (clear-storage-vector pattern)
      (do-vector-contents (search-char (cadr old))
	(sv-append pattern search-char)))))

(defmethod empty-pattern? ((mode search-mode))
  (zerop& (storage-vector-active-length (slot-value mode 'pattern))))

;;; CONs new BP's because we may be pushing the old one onto the bp-stack
(defmethod new-current-position ((mode search-mode) bp)
  (let ((pos-bp (make-bp :fixed)))
    (setf (bp-row pos-bp) (bp-row bp)
	  (bp-cha-no pos-bp) (bp-cha-no bp)
	  ;; do we really need screen-box ?
	  (bp-screen-box pos-bp) (bp-screen-box bp))
    (setf (slot-value mode 'current-position) pos-bp)))

(defmethod set-current-position ((mode search-mode) bp)
  (let ((pos-bp (or (slot-value mode 'current-position)
		    (let ((new (make-bp :fixed)))
		      (setf (slot-value mode 'current-position) new) new))))
    (setf (bp-row pos-bp) (bp-row bp)
	  (bp-cha-no pos-bp) (bp-cha-no bp)
	  ;; do we really need screen-box ?
	  (bp-screen-box pos-bp) (bp-screen-box bp))
    pos-bp))

(defmethod new-current-position-values ((mode search-mode) row cha-no
					&optional screen-box)
  (let ((pos-bp (make-bp :fixed)))
    (setf (bp-row pos-bp) row (bp-cha-no pos-bp) cha-no
	  ;; do we really need screen-box ?
	  (bp-screen-box pos-bp) screen-box)
    (setf (slot-value mode 'current-position) pos-bp)))

(defmethod set-current-position-values ((mode search-mode) row cha-no
					&optional screen-box)
  (let ((pos-bp (or (slot-value mode 'current-position)
		    (let ((new (make-bp :fixed)))
		      (setf (slot-value mode 'current-position) new) new))))
    (setf (bp-row pos-bp) row (bp-cha-no pos-bp) cha-no
	  ;; do we really need screen-box ?
	  (bp-screen-box pos-bp) screen-box)
    pos-bp))

(defmethod push-bp ((mode search-mode) bp)
  (let ((mark-bp (make-bp :fixed)))
    (setf (bp-row mark-bp) (bp-row bp)
	  (bp-cha-no mark-bp) (bp-cha-no bp)
	  ;; do we really need screen-box ?
	  (bp-screen-box mark-bp) (bp-screen-box bp))
    (push mark-bp (slot-value mode 'bp-stack))))

(defmethod push-bp-values ((mode search-mode) row cha-no &optional screen-box)
  (let ((mark-bp (make-bp :fixed)))
    (setf (bp-row mark-bp) row
	  (bp-cha-no mark-bp) cha-no
	  ;; do we really need screen-box ?
	  (bp-screen-box mark-bp) screen-box)
    (push mark-bp (slot-value mode 'bp-stack))))

(defmethod pop-bp ((mode search-mode))
  (pop (slot-value mode 'bp-stack)))

(defmethod pop-bp-values ((mode search-mode))
  (let ((bp (pop (slot-value mode 'bp-stack))))
    (values (bp-row bp) (bp-cha-no bp) (bp-screen-box bp))))

(defmethod search-mode-search ((mode search-mode) &optional from)
  (if (minusp (slot-value mode 'direction))
      (rsearch-from (slot-value mode 'pattern)
		    (or from (slot-value mode 'current-position) *point*)
		    (slot-value mode 'stop-box))
      (search-from (slot-value mode 'pattern)
		   (or from (slot-value mode 'current-position) *point*)
		   (slot-value mode 'stop-box))))


(defvar *search-mode* (make-instance 'search-mode))

(defun search-mode () *search-mode*)

(defun search-mode-pattern () (slot-value *search-mode* 'pattern))




;;;; Pattern manipulators

(defvar *verbose-pattern-message* t)

(defvar *short-search-help*
  #-mcl
  "ESC=end, CTRL-.=abort, |=named box, CTRL-F=resume"
  #+mcl
  (format nil "ESC=end, ~C-G=abort, |=named box, ~C-F=resume"
          #\commandmark #\commandmark)
  )

(defun display-pattern-message (&optional prompt)
  (let ((sstring (or (search-string (slot-value (search-mode) 'pattern)) ""))
	(prefix (or prompt "Searching"))
	(suffix (if *verbose-pattern-message*
		    *short-search-help*
		    "")))
    (status-line-display 'search
			 (format nil "~A: ~A ~A" prefix sstring suffix))))


;;; let's reuse the same string
(defvar *search-string-display-buffer*
  (make-array 16 :adjustable t :fill-pointer 0
	      :element-type #+lucid 'string-char #-lucid 'character))

(defvar *search-string-box-name-display-buffer*
  (make-array 16 :adjustable t :fill-pointer 0
	      :element-type #+lucid 'string-char #-lucid 'character))

(defun search-string (pattern &optional(buffer *search-string-display-buffer*))
  (setf (fill-pointer buffer) 0)
  (dotimes (i (storage-vector-active-length pattern))
    (let ((element (sv-nth i pattern)))
      (cond ((characterp element)
	     (vector-push-extend element buffer))
	    ((eq element :search))
	    ((eq element :newline)
	     (vector-push-extend #\< buffer)
	     (vector-push-extend #\C buffer)
	     (vector-push-extend #\R buffer)
	     (vector-push-extend #\> buffer))
	    (t
	     ;; must be a box search object
	     (add-search-box-chars element buffer)))))
  buffer)

(defun add-search-box-chars (box-pattern buffer)
  (flet ((add-string (string)
	   (dotimes (i (length string))
	     (vector-push-extend (char string i) buffer))))
    (cond ((null (box-pattern-name box-pattern))
	   (case (box-pattern-type box-pattern)
	     (data-box (add-string "[Data Box]"))
	     (doit-box (add-string "[Doit Box]"))
	     (sprite-box (add-string "[Sprite Box]"))
	     (port-box (add-string "[Port]"))
	     (t (add-string "[]"))))
	  (t ; box has a name
	   (add-string
	    (format nil "[name: ~A]"
		    (search-string
		     (box-pattern-name box-pattern)
		     *search-string-box-name-display-buffer*)))))))

(defun clear-all-pattern-messages () (status-line-undisplay 'search))

(defvar *search-pattern* (make-storage-vector))

(defun clear-pattern (&optional (pattern (search-mode-pattern)))
  (clear-storage-vector pattern))

(defun add-pattern-char (char &optional (pattern (search-mode-pattern)))
  (sv-append (or (slot-value (search-mode) 'box-pattern) pattern) char))

(defun add-pattern-cr (&optional (pattern (search-mode-pattern)))
  (sv-append pattern ':newline))

(defun delete-pattern-char (&optional (pattern (search-mode-pattern)))
  (let ((name (slot-value (search-mode) 'box-pattern)))
    (cond ((and (not (null name))
		(not (zerop& (storage-vector-active-length name))))
	   (prog1
	       (sv-nth (1-& (storage-vector-active-length pattern)) name)
	     (sv-delete-last name)))
	  (t
	   (values
	    (if (zerop& (storage-vector-active-length pattern))
		:empty
		(prog1
		    (sv-nth (1-& (storage-vector-active-length pattern))
			    pattern)
		  (sv-delete-last pattern)))
	    (not (null name)))))))

(defun add-pattern-box (box &optional (pattern (search-mode-pattern)))
  (sv-append pattern box))
