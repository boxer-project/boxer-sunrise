;; -*- Mode:LISP; Syntax: Common-Lisp; Package:BOXER ;-*-

#|


 $Header: region.lisp,v 1.0 90/01/24 22:16:43 boxer Exp $

 $Log:	region.lisp,v $
;;;Revision 1.0  90/01/24  22:16:43  boxer
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


 This file defines Boxer editor REGIONS


Modification History (most recent at top)

 9/23/12 fixnum assumption lossage in update-region-row-blinker &
         {{left,right}-half,both-ends}-blinker-trim
12/01/10 #-opengl (flush-port-buffer) for region redisplay
 2/15/03 merged current LW and MCL files, no changes, copyright updated
 5/03/98 changed copy-interval to hack fonts
 5/03/98 started logging: source = boxer version 2.3

|#

(in-package :boxer)



(defun make-editor-region (start-bp &optional (stop-bp *point*))
  (if (and (bp? start-bp) (bp? stop-bp))
      (%make-interval start-bp stop-bp)
      (error "One or both of the args: ~S, ~S was not a Boxer pointer"
	     start-bp stop-bp)))

(defmacro do-region-rows ((row-var region) &body body)
  (declare (ignore row-var region body))
  (warn "DO-REGION-ROWS should only be used inside WITH-REGION-TOP-LEVEL-BPS")
  '(error
    "DO-REGION-ROWS should only be used inside WITH-REGION-TOP-LEVEL-BPS"))

(defmacro do-region-chas ((cha-var region) &body body)
  (declare (ignore cha-var region body))
  (warn "DO-REGION-CHAS should only be used inside WITH-REGION-TOP-LEVEL-BPS")
  '(error
    "DO-REGION-CHAS should only be used inside WITH-REGION-TOP-LEVEL-BPS"))

(defmacro with-region-top-level-bps ((editor-region
				      &key start-bp-name stop-bp-name)
				     &body body)
  "Creates and environment with REGION-START-BP and REGION-STOP-BP bound
   to BPs which are at the same level and ordered.  Then cleans up afterwards."
  (let ((real-start-bp-name (or start-bp-name (gensym)))
	(real-stop-bp-name (or stop-bp-name (gensym))))
    `(multiple-value-bind (,real-start-bp-name ,real-stop-bp-name)
	 (order-bps (interval-start-bp ,editor-region)
		    (interval-stop-bp ,editor-region))
       (macrolet ((do-region-rows ((row-var region) &body body)
                    (declare (ignore region)) ; region is here to look right
		     (let ((terminating-row (gensym))
			   (next-row-var (gensym))
			   (last-row-flag (gensym)))
		       `(let ((,terminating-row (bp-row ,',real-stop-bp-name))
			      (,next-row-var nil)
			      (,last-row-flag nil))
			  (do ((,row-var (bp-row ,',real-start-bp-name)
					 ,next-row-var))
			      ((or (null ,row-var)
				   (not (null ,last-row-flag))))
			    ;; we have to handle updating the next-row inside
			    ;; the body because the current row can be
			    ;; side-effected in the body in particular, the
			    ;; next-row slot is sometimes bashed
			    (setq ,next-row-var (next-row ,row-var))
			    (when (eq ,row-var ,terminating-row)
			      (setq ,last-row-flag t))
			    ,@body))))
                  (do-region-chas ((cha-var region) &body body)
                    (declare (ignore region))
                    (let ((starting-row (gensym))
                          (terminating-row (gensym))
                          (row-var (gensym))
                          (last-row-flag (gensym)))
                      `(let ((,starting-row (bp-row ,',real-start-bp-name))
                             (,terminating-row (bp-row ,',real-stop-bp-name))
                             (,last-row-flag nil))
                         (do ((,row-var ,starting-row (next-row ,row-var)))
                             ((or (null ,row-var)
                                  (not (null ,last-row-flag))))
                           (when (eq ,row-var ,terminating-row)
                             (setq ,last-row-flag t))
                           (do-row-chas ((,cha-var ,row-var
                                                   :start
                                                   (if (eq ,row-var
                                                           ,starting-row)
                                                       (bp-cha-no ,',real-start-bp-name)
                                                       0)
                                                   :stop
                                                   (when (eq ,row-var
                                                             ,terminating-row)
                                                     (bp-cha-no ,',real-stop-bp-name))))
                             ,@body))))))

	 (unwind-protect
	     (progn . ,body)
	   (deallocate-bp ,real-start-bp-name)
	   (deallocate-bp ,real-stop-bp-name))))))

;; this has to make copies of the rows that the start-bp and stop-bp
;; are pointing to (and any intervening rows as well), then set the
;; start-bp and stop-bp of the new copy to point into the newly created rows
;; the first and last rows are handled specially

(defun copy-interval (interval &optional (copy-box-arg nil c-b-a-supplied?))
  (with-editor-port-relinking
      (with-region-top-level-bps (interval :start-bp-name start-bp
					   :stop-bp-name  stop-bp)
	(let ((new-start-bp (make-bp (bp-type start-bp)))
	      (new-stop-bp  (make-bp (bp-type stop-bp))))
	  (flet ((first-row? (row) (eq row (bp-row start-bp)))
		 (last-row?  (row) (eq row (bp-row stop-bp)))
		 (copy-partial-row (row &key (start 0)
					(stop (length-in-chas row)))
		   (let* ((new-row (make-initialized-row))
			  (new-chas-array (chas-array new-row))
			  (idx 0))
		     (do-row-chas ((c row :start start :stop stop))
		       (let ((new-cha
			      (cond ((cha? c) c)
                                    (c-b-a-supplied? (copy-box c copy-box-arg))
				    (t (copy-box c)))))
			 (chas-array-insert-cha new-chas-array
						idx
						new-cha)
			 (unless (cha? new-cha)
			   (set-superior-row new-cha new-row)
			   (insert-self-action new-cha)))
		       (incf& idx))
                     ;; copy some/all FD's
                     (let ((copied-fds nil))
                       (dolist (fd (row-fds row))
                         (when (and (>=& (bfd-cha-no fd) start)
                                    (<&  (bfd-cha-no fd) stop))
                           (push (make-cfd (-& (bfd-cha-no fd) start)
                                           (bfd-font-no fd)
                                           (bfd-color   fd))
                                 copied-fds)))
                       (setf (chas-array-fds new-chas-array)
                             (nreverse copied-fds)))
		     new-row)))
	    (cond ((eq (bp-row start-bp) (bp-row stop-bp))
		   ;; there is only one (or part of one) row in the region
		   ;; so copy the row (or part of it)
		   (let ((new-region-row (copy-partial-row
					  (bp-row start-bp)
					  :start (bp-cha-no start-bp)
					  :stop (bp-cha-no stop-bp))))
		     ;; now bash the new bps to the proper values
		     ;; the new-start-bp
		     (setf (bp-row new-start-bp) new-region-row)
		     (setf (bp-cha-no new-start-bp) 0)
		     ;; and the new-stop-bp
		     (setf (bp-row new-stop-bp) new-region-row)
		     (setf (bp-cha-no new-stop-bp)
			   (length-in-chas new-region-row))))
		  (t
		   ;; looks like there is more than one row in this region
		   (let ((previous-row nil))
		     (do-region-rows (rr interval)
		       (cond ((first-row? rr)
			      (setq previous-row
				    (copy-partial-row rr
						      :start
						      (bp-cha-no
						       start-bp)))
			      ;; setup the new-start-bp
			      (setf (bp-row new-start-bp)
				    previous-row)
			      (setf (bp-cha-no new-start-bp) 0))
			     ((last-row? rr)
			      (let ((new-row (copy-partial-row
					      rr
					      :stop
					      (bp-cha-no stop-bp))))
				;; append it to the existing rows
				(set-previous-row new-row
						  previous-row)
				(set-next-row previous-row new-row)
				;; and then setup the new-stop-bp
				(setf (bp-row new-stop-bp) new-row)
				(setf (bp-cha-no new-stop-bp)
				      (length-in-chas new-row))))

			     (t
			      ;; otherwise append existing to rows
			      (let ((new-row (if c-b-a-supplied?
                                                 (copy-row rr nil nil copy-box-arg)
                                                 (copy-row rr))))
				(set-previous-row new-row
						  previous-row)
				(set-next-row previous-row new-row)
				(setq previous-row new-row)))))))))
	  ;; now that we have set up the BP's and hooked together the
	  ;; (intervening) rows, we can make the new copy of the region
	  (make-editor-region new-start-bp new-stop-bp)))))


(defun set-interval-bps (interval bp1 bp2)
  (setf (interval-start-bp interval) bp1)
  (setf (interval-stop-bp interval) bp2))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               INTERACTIONS BETWEEN REGIONS AND BOXES                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-region-check-superiors ((self box))
  (with-slots (region) self
    (let ((superior-box (superior-box self)))
      (cond ((interval? region) region)
	    ((box? superior-box) (get-region-check-superiors superior-box))
	    (t nil)))))

(defun set-box (interval new-box)
  (setf (interval-box interval) new-box))

(defmethod set-region ((self box) new-region)
  (setf (region self) new-region)
  (modified self))

;; Use this one from the outside since at some point in the future, we
;; may allow more than one region in a BOX or ONLY one region in ALL of BOXER
;; No matter what, this is guaranteed to get you whatever the most appropriate
;; region is if it exists

(defun get-current-region ()
  (let ((box (point-box)))
    (unless (null box)
      (get-region-check-superiors box))))

(defun get-local-region (&optional (bp *point*))
  (region (bp-box bp)))

(defun install-region (region &optional (bp *point*))
  (set-box region (bp-box bp))
  (set-region (bp-box bp) region)
  (when (eq region *region-being-defined*)
    (setq *region-being-defined* nil))
  (when (eq region *following-mouse-region*)
    (setq *following-mouse-region* nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                TOP  LEVEL  REGION  MANIPULATING  COMMANDS               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun flush-region (region)
  (when (not (null region))
    (make-interval-invisible region)
    (let ((i-box (interval-box region)))
      (unless (null i-box)
	(set-region i-box nil)))
    (dolist (blinker (interval-blinker-list region))
      (remove-region-row-blinker blinker))
    (setq *region-list* (fast-delq region *region-list*))
    (when (eq region *region-being-defined*)
      (setq *region-being-defined* nil))
    (when (eq region *following-mouse-region*)
      (setq *following-mouse-region* nil))
    (deallocate-region region)))

(defun deallocate-region (region)
  "take region out of the screen-row and screen-box"
  (let ((start (interval-start-bp region))
	(stop (interval-stop-bp region)))
    (unless (eq start *point*) (deallocate-bp start))
    (unless (eq stop  *point*) (deallocate-bp stop))))

;;; snip the interval delineated by the region out of the editor hierarchy...
(defun kill-region (region)
  ;; now bash away...
  (with-region-top-level-bps (region :start-bp-name region-start-bp
				     :stop-bp-name region-stop-bp)
    ;; first, if one of the region's BP's is the *point*, replace it
    (cond ((eq *point* (interval-start-bp region))
	   (setf (interval-start-bp region) (copy-bp *point*)))
	  ((eq *point* (interval-stop-bp region))
	   (setf (interval-stop-bp region) (copy-bp *point*))))
    ;; if the *point* is in the region being killed,
    ;; we need to get it out of there
    (let* ((first-row (bp-row region-start-bp))
	   (last-row (bp-row region-stop-bp))
	   (region-box (superior-box first-row)))
      (cond ((eq first-row last-row)
	     (let ((killed-row (delete-chas-between-cha-nos
				first-row
				(bp-cha-no region-start-bp)
				(bp-cha-no region-stop-bp))))
	       #-opengl (add-redisplay-clue first-row :delete)
	       (set-bp-row (interval-start-bp region) killed-row)
	       (set-bp-cha-no (interval-start-bp region) 0)
	       (set-bp-row (interval-stop-bp region) killed-row)
	       (set-bp-cha-no (interval-stop-bp region)
			      (length-in-chas killed-row))))
	    (t
	     (let ((previous-row nil))
	       (do-region-rows (rr region)
		 (cond ((eq rr first-row)
			(setq previous-row
			      (kill-chas-at-cha-no
			       rr (bp-cha-no
				   region-start-bp)))
			;; setup the start-bp
			(set-bp-row (interval-start-bp region) previous-row)
			(set-bp-cha-no (interval-start-bp region) 0))
		       ((eq rr last-row)
			(let ((region-last-row
			       (delete-chas-between-cha-nos
				last-row
				0
				(bp-cha-no region-stop-bp))))
			  ;; we can only remove from the last row
			  #-opengl (add-redisplay-clue rr :delete)
			  ;; setup the stop-bp
			  (set-bp-row (interval-stop-bp region)
				      region-last-row)
			  (set-bp-cha-no (interval-stop-bp region)
					 (length-in-chas region-last-row))
			  ;; adjoin the leftovers from the 1st and last row
			  ;; of where the region was in the editor
			  (insert-row-chas-at-cha-no first-row last-row
						     (length-in-chas first-row))
			  (delete-row region-box last-row)
			  ;; hook it up to any existing rows
			  (set-previous-row region-last-row previous-row)
			  (set-next-row previous-row region-last-row)
			  (set-next-row region-last-row nil)))
		       (t
			;; must be an in between row....
			;; so remove it from the editor...
			(delete-row region-box rr)
			;; and hook it up to the rows we already have
			(set-previous-row rr previous-row)
			(set-next-row previous-row rr)
			(setq previous-row rr)))))))
      ;; move the *point* outside the killed area
      (set-bp-row *point* (bp-row region-start-bp))
      (set-bp-cha-no *point* (bp-cha-no region-start-bp))
      (set-bp-screen-box *point* (dest-screen-box (bp-box region-start-bp)
						  (point-screen-box)))))
  region)

;; calculate a reasonable screen-box for the *point* to end up in
;; this can be a bit complicated if the *point* is inferior to
;; the top level BP's of the region
;; the algorthm is to start at the (point-screen-box) and
;; walk outward until the actual-obj of the screen-box is EQ
;; to the bp-box
(defun dest-screen-box (bp-box screen-box)
  (let ((superior-screen-row (screen-row screen-box)))
    (cond ((eq bp-box (box-or-port-target (screen-obj-actual-obj screen-box)))
	   screen-box)
	  ((not (screen-row? superior-screen-row))
	   (error "Can't find a screen-box to move the *point* to"))
	  (t (dest-screen-box bp-box (screen-box superior-screen-row))))))

(defun yank-region (bp region &optional (force-bp-type ':moving))
  (with-region-top-level-bps (region
			      :start-bp-name start-bp
			      :stop-bp-name stop-bp)
    (action-at-bp-internal
     (let* ((box (bp-box bp))
	    (row (bp-row bp))
	    (cha-no (min (length-in-chas row) (bp-cha-no bp)))
	    (remains (unless (> (bp-cha-no bp) (length-in-chas row))
		       (kill-chas-at-cha-no row cha-no)))
	    (first-new-row (bp-row start-bp))
	    (previous-added-row row)
	    (last-new-row (bp-row stop-bp)))
       ;;remember where we are
       (move-bp (interval-start-bp region) (bp-values bp))
       ;; add the rows in the region to the editor hierarchy
       (cond ((eq first-new-row last-new-row)
	      ;; the region is only on (part of) one row
	      ;; insert the (part of) one row from the region
	      (insert-row-chas-at-cha-no row first-new-row cha-no
					 (bp-cha-no start-bp)
					 (bp-cha-no stop-bp))
	      ;; then add the remainder of the original row back
	      (unless (null remains)
		(insert-row-chas-at-cha-no row remains (length-in-chas row)))
	      ;; move the interval-stop-bp as well as
	      ;; the bp from the arglist (usually
	      ;; the *point*) here
	      (set-bp-row (interval-stop-bp region) row)
	      (set-bp-cha-no (interval-stop-bp region)
			     (+& cha-no (length-in-chas last-new-row)))
	      (set-bp-row bp row)
	      (set-bp-cha-no bp (+& cha-no (length-in-chas last-new-row))))
	     (t
	      ;; looks like we have more than one row in the region so
	      ;; insert the part of the region's first row
	      (do-region-rows (rr region)
			      (cond ((eq rr first-new-row)
				     ;; handle the first row specially
				     (insert-row-chas-at-cha-no
				      row first-new-row cha-no
				      (bp-cha-no start-bp)))
				    ((eq rr last-new-row)
				     ;; handle the last row specially...
				     (insert-row-after-row box
							   rr
							   previous-added-row)
				     (unless (null remains)
				       (insert-row-chas-at-cha-no
					rr remains (length-in-chas rr)))
				     ;; move the interval-stop-bp as well as
				     ;; the bp from the arglist (usually
				     ;; the *point*) here
				     (set-bp-row (interval-stop-bp region) rr)
				     (set-bp-cha-no (interval-stop-bp region)
						    (length-in-chas rr))
				     (set-bp-row bp rr)
				     (set-bp-cha-no bp (length-in-chas rr)))
				    (t
				     (insert-row-after-row box rr
							   previous-added-row)
				     (setq previous-added-row rr))))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           Redisplay  of  REGIONS                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Regions are displayed as one or more blinkers.  With Each blinker
;;; corresponding to screen representation(s) for the rows which make
;;; up the region

(defun allocate-region-row-blinker (screen-row)
  (let ((new-blinker (make-region-row-blinker *boxer-pane* :visibility nil)))
    (setf (bw::region-row-blinker-uid new-blinker) screen-row)
    new-blinker))

(defun turn-on-interval (region)
  (unless (interval-visibility region)
    (make-interval-visible region))
  (setf (interval-visibility region) t))

(defun turn-off-interval (region)
  (when (interval-visibility region)
    (make-interval-invisible region))
  (setf (interval-visibility region) nil))

;;; Accessor Macros...
(defsubst region-row-blinker-wid (region)
  (bw::blinker-width region))

(defsubst region-row-blinker-hei (region)
  (bw::blinker-height region))

(defsubst region-row-blinker-x (region)
  (bw::blinker-x region))

(defsubst region-row-blinker-y (region)
  (bw::blinker-y region))

(defsubst region-row-blinker-visibility (region)
  (bw::blinker-visibility region))

(defsubst region-row-blinker-uid (region)
  (bw::region-row-blinker-uid region))

;;; setf's
;;; MCL AppGen lossage for macro form of defsetf...

#+mcl
(defun %set-region-row-blinker-wid (region new-wid)
  (setf (bw::blinker-width region) new-wid))
#+mcl(defsetf region-row-blinker-wid %set-region-row-blinker-wid)

#-mcl
(defsetf region-row-blinker-wid (region) (new-wid)
  `(setf (bw::blinker-width ,region) ,new-wid))

#+mcl
(defun %set-region-row-blinker-hei (region new-hei)
  (setf (bw::blinker-height region) new-hei))
#+mcl (defsetf region-row-blinker-hei %set-region-row-blinker-hei)

#-mcl
(defsetf region-row-blinker-hei (region) (new-hei)
  `(setf (bw::blinker-height ,region) ,new-hei))

#+mcl
(defun %set-region-row-blinker-x (region new-x)
  (setf (bw::blinker-x region) new-x))
#+mcl (defsetf region-row-blinker-x %set-region-row-blinker-x)

#-mcl
(defsetf region-row-blinker-x (region) (new-x)
  `(setf (bw::blinker-x ,region) ,new-x))

#+mcl
(defun %set-region-row-blinker-y (region new-y)
  (setf (bw::blinker-y region) new-y))
#+mcl (defsetf region-row-blinker-y %set-region-row-blinker-y)

#-mcl
(defsetf region-row-blinker-y (region) (new-y)
  `(setf (bw::blinker-y ,region) ,new-y))

#+mcl
(defun %set-region-row-blinker-visibility (region new-vis)
  (setf (bw::blinker-visibility region) new-vis))
#+mcl (defsetf region-row-blinker-visibility %set-region-row-blinker-visibility)

#-mcl
(defsetf region-row-blinker-visibility (region) (new-vis)
  `(setf (bw::blinker-visibility ,region) ,new-vis))

#+mcl
(defun %set-region-row-blinker-uid (region new-uid)
  (setf (bw::region-row-blinker-uid region) new-uid))
#+mcl (defsetf region-row-blinker-uid %set-region-row-blinker-uid)

#-mcl
(defsetf region-row-blinker-uid (region) (new-uid)
  `(setf (bw::region-row-blinker-uid ,region) ,new-uid))



;;; We provide two different messages for redisplay of regions.  One of them
;;; will just mark the screen rows corresponding to the region in
;;; the *CURRENT-SCREEN-BOX* while the other one will mark *ALL* the screen
;;; rows of the region.
(defun remove-region-row-blinker (row-blinker)
  (setf (region-row-blinker-visibility row-blinker) nil)
  (setf (bw::sheet-blinker-list *boxer-pane*)
	(fast-delq row-blinker (bw::sheet-blinker-list *boxer-pane*))))

;; Blinkers positions are with respect to the window WITH THE BORDERS INCLUDED
;(DEFMACRO FIXUP-COORDINATES-FOR-BLINKER (X Y BL)
;  `(LET ((SHEET (SEND ,BL :SHEET)))
;     (SETF ,X (+ ,X (SEND SHEET :LEFT-MARGIN-SIZE)))
;     (SETF ,Y (+ ,Y (SEND SHEET :TOP-MARGIN-SIZE)))))

;; used to make rows with 0 characters highlighted.
(defvar *minimum-row-blinker-wid* 10)

(defun update-region-row-blinker (region-row-blinker)
  (let* ((screen-row (region-row-blinker-uid region-row-blinker))
	 (wid (screen-obj-wid screen-row))
	 (hei (screen-obj-hei screen-row)))
    (multiple-value-bind (x y)
	(xy-position screen-row)
;      ;; Blinker positions are measured with the borders included
;      (FIXUP-COORDINATES-FOR-BLINKER X Y REGION-ROW-BLINKER)
      (when (or (not (= wid (region-row-blinker-wid region-row-blinker)))
		(not (= hei (region-row-blinker-hei region-row-blinker)))
		(not (= x   (region-row-blinker-x   region-row-blinker)))
		(not (= y   (region-row-blinker-y   region-row-blinker))))
	;; might be better to use timestamps (we might
	;; have to use timestamps in addition anyway)
	(with-open-blinker (region-row-blinker)
	  (setf (region-row-blinker-wid region-row-blinker)
		(if (zerop wid) *minimum-row-blinker-wid* wid))
	  (setf (region-row-blinker-hei region-row-blinker) hei)
	  (setf (region-row-blinker-x region-row-blinker) x)
	  (setf (region-row-blinker-y region-row-blinker) y))))))

(defun left-half-blinker-trim (blinker cha-no)
  (let* ((screen-row (region-row-blinker-uid blinker))
	 (row-wid (screen-obj-wid screen-row))
	 (row-hei (screen-obj-hei screen-row))
	 (amount-to-trim
	  (with-summation
            (do-screen-chas-with-font-info (cha (slot-value screen-row 'screen-chas)
				                :stop cha-no)
              (sum (if (screen-cha? cha)
                       (cha-wid cha)
                     (screen-obj-wid cha))))))
	 (desired-wid (- row-wid amount-to-trim)))
    (multiple-value-bind (x y)
	(xy-position screen-row)
      (when (or (not (= desired-wid        (region-row-blinker-wid blinker)))
		(not (= row-hei            (region-row-blinker-hei blinker)))
		(not (= (+ x amount-to-trim)
                        (region-row-blinker-x   blinker)))
		(not (= y                  (region-row-blinker-y   blinker))))
        (setf (region-row-blinker-wid blinker)
              (if (zerop desired-wid) *minimum-row-blinker-wid* desired-wid))
        (setf (region-row-blinker-hei blinker) row-hei)
        (setf (region-row-blinker-x   blinker) (+ x amount-to-trim))
        (setf (region-row-blinker-y   blinker) y)))))

(defun right-half-blinker-trim (blinker cha-no)
  (let* ((screen-row (region-row-blinker-uid blinker))
	 (row-wid (screen-obj-wid screen-row))
	 (row-hei (screen-obj-hei screen-row))
	 (amount-to-trim
	  (if (>=& cha-no (screen-chas-length screen-row))
	      0
            (with-summation
              (do-screen-chas-with-font-info (cha (slot-value screen-row
                                                              'screen-chas)
                                                  :start cha-no)
                (sum (if (screen-cha? cha) (cha-wid cha) (screen-obj-wid cha)))))))
	 (desired-wid (- row-wid amount-to-trim)))
    (multiple-value-bind (x y)
	(xy-position screen-row)
      (when (or (not (= desired-wid  (region-row-blinker-wid blinker)))
		(not (= row-hei      (region-row-blinker-hei blinker)))
		(not (= x            (region-row-blinker-x   blinker)))
		(not (= y            (region-row-blinker-y   blinker))))
        (setf (region-row-blinker-wid blinker)
              (if (zerop desired-wid) *minimum-row-blinker-wid* desired-wid))
        (setf (region-row-blinker-hei blinker) row-hei)
        (setf (region-row-blinker-x   blinker) x)
        (setf (region-row-blinker-y   blinker) y)))))

(defun both-ends-blinker-trim (blinker start-cha-no stop-cha-no)
  (let* ((screen-row (region-row-blinker-uid blinker))
	 (screen-chas (slot-value screen-row 'screen-chas))
	 (row-wid (screen-obj-wid screen-row))
	 (row-hei (screen-obj-hei screen-row))
	 (left-trim (with-summation
                      (do-screen-chas-with-font-info (cha screen-chas
                                                          :stop start-cha-no)
                        (sum (if (screen-cha? cha)
                                 (cha-wid cha)
                               (screen-obj-wid cha))))))
	 (right-trim
	  (if (>=& stop-cha-no (screen-chas-length screen-row))
	      0
            (with-summation
              (do-screen-chas-with-font-info (cha (slot-value screen-row
                                                              'screen-chas)
                                                  :start stop-cha-no)
                (sum (if (screen-cha? cha)
                         (cha-wid cha)
                       (screen-obj-wid cha)))))))
	 (desired-wid (- row-wid left-trim right-trim)))
    (multiple-value-bind (x y)
	(xy-position screen-row)
      (when (or (not (= desired-wid     (region-row-blinker-wid blinker)))
		(not (= row-hei         (region-row-blinker-hei blinker)))
		(not (= (+ x left-trim) (region-row-blinker-x   blinker)))
		(not (= y               (region-row-blinker-y   blinker))))
        (setf (region-row-blinker-wid blinker)
              (if (zerop desired-wid) *minimum-row-blinker-wid* desired-wid))
        (setf (region-row-blinker-hei blinker) row-hei)
        (setf (region-row-blinker-x   blinker) (+ x left-trim))
        (setf (region-row-blinker-y   blinker) y)))))

;; is the row conneted to the editor hierarchy ?
;; used for detemining whether we should bother trying to redisplay
;; a region...
(defun row-connected? (row &optional
			   (outermost-visible-box
			    (screen-obj-actual-obj
			     (outermost-screen-box))))
  (let ((superior-box (superior-box row)))
    (cond ((null superior-box) nil)
	  ((eq superior-box *initial-box*) t)
	  (t (let ((superior-row (superior-row superior-box)))
	       (cond ((null superior-row) nil)
		     (t (row-connected? superior-row outermost-visible-box))))))))

(defun update-row-blinker-list (row-blinkers screen-rows)
  "A blinker for every row and no extra blinkers. Returns a list of blinkers"
  (prog1
      (with-collection
	  (dolist (screen-row screen-rows)
	    (collect (let ((existing-region
			    (car (member screen-row row-blinkers
					 :test #'(lambda (uid reg)
						   (eq uid
						       (region-row-blinker-uid
							reg)))))))
		       (cond ((null existing-region)
			      (allocate-region-row-blinker screen-row))
			     (t
			      (setq row-blinkers (fast-delq existing-region
							    row-blinkers))
			      existing-region))))))
    (dolist (old-blinker row-blinkers)
      (remove-region-row-blinker old-blinker))))

(defun interval-update-repaint-all-rows (region &optional
						  (window *boxer-pane*))
  ;; we have to bind this because region redisplay can
  ;; legitimately be called OUTSIDE of normal redisplay
  (cond ((not (row-connected? (bp-row (interval-start-bp region))))
         ;; the region is no longer part of the visible editor hierarchy
         ;; so blank it....
         (dolist (blinker (interval-blinker-list region))
           (remove-region-row-blinker blinker))
         (setf (interval-blinker-list region) nil))
        (t
         (with-region-top-level-bps (region :start-bp-name region-start-bp
                                            :stop-bp-name region-stop-bp)
           ;; First we do "allocation" that is, make sure that there
           ;; is a blinker for every screen row and vice versa.  Note
           ;; that blinker list will be ordered from top to bottom
           (setf (interval-blinker-list region)
                 (update-row-blinker-list
                  (interval-blinker-list region)
                  (let ((displayed-rows nil))
                    (do-region-rows (row region)
                      (setq displayed-rows
                            (append displayed-rows (displayed-screen-objs row))))
                    displayed-rows)))
           (if (interval-visibility region)
               (make-interval-visible region)
             (make-interval-invisible region))
           (let ((starting-row (bp-row region-start-bp))
                 (starting-cha-no (bp-cha-no region-start-bp))
                 (stopping-row (bp-row region-stop-bp))
                 (stopping-cha-no (bp-cha-no region-stop-bp)))
             (dolist (blinker (interval-blinker-list region))
               (let* ((blinker-row (region-row-blinker-uid blinker))
                      (editor-row (screen-obj-actual-obj blinker-row)))
                 (cond ((and (eq starting-row editor-row)
                             (eq stopping-row editor-row))
                        ;; the row is both the first and last one in a
                        ;; region so we should trim both ends of it
                        (both-ends-blinker-trim blinker starting-cha-no
                                                stopping-cha-no))
                       ((eq starting-row editor-row)
                        ;; If the row is the first one in a region then it
                        ;; needs to be trimmed to correspond to where
                        ;; the BP is pointing
                        (left-half-blinker-trim blinker starting-cha-no))
                       ((eq stopping-row editor-row)
                        ;; If the row is the last one in the region, then
                        ;; it ALSO needs to be trimmed to correspond to
                        ;; where the BP is pointing
                        (right-half-blinker-trim blinker stopping-cha-no))
                       (t
                        ;; finally, take care of all the other rows
                        (update-region-row-blinker blinker)))))))))
  ;; @ this point all the blinkers are the correct size and inthe right place...
  (drawing-on-window (window)
    (dolist (blinker (interval-blinker-list region)) (draw-blinker blinker)))
  #-opengl
  (flush-port-buffer window))

(defun interval-update-repaint-current-rows (region &optional
						      (window *boxer-pane*))
  (cond ((not (row-connected? (bp-row (interval-start-bp region))))
         ;; No BP's mean that there is not any screen structure.
         ;; Probably a region got wiped
         (dolist (blinker (interval-blinker-list region))
           (remove-region-row-blinker blinker))
         (setf (interval-blinker-list region) nil))
        (t
         (with-region-top-level-bps (region :start-bp-name region-start-bp
                                            :stop-bp-name region-stop-bp)
           ;; First we do "allocation" that is, make sure that there is a
           ;; blinker for every screen row and vice versa.  Note that
           ;; blinker list will be ordered from top to bottom
           (setf (interval-blinker-list region)
                 (update-row-blinker-list
                  (interval-blinker-list region)
                  (with-collection
                    (do-region-rows (row region)
                      (collect (current-screen-row row))))))
           (if (interval-visibility region)
               (make-interval-visible region)
             (make-interval-invisible region))
           (let ((starting-row (bp-row region-start-bp))
                 (starting-cha-no (bp-cha-no region-start-bp))
                 (stopping-row (bp-row region-stop-bp))
                 (stopping-cha-no (bp-cha-no region-stop-bp)))
             (dolist (blinker (interval-blinker-list region))
               (let* ((blinker-row (region-row-blinker-uid blinker))
                      (editor-row (screen-obj-actual-obj blinker-row)))
                 (cond ((and (eq starting-row editor-row)
                             (eq stopping-row editor-row))
                        ;; the row is both the first and last one in a
                        ;; region so we should trim both ends of it
                        (both-ends-blinker-trim blinker starting-cha-no
                                                stopping-cha-no))
                       ((eq starting-row editor-row)
                        ;; If the row is the first one in a region then it
                        ;; needs to be trimmed to correspond to where
                        ;; the BP is pointing
                        (left-half-blinker-trim blinker starting-cha-no))
                       ((eq stopping-row editor-row)
                        ;; If the row is the last one in the region, then
                        ;; it ALSO needs to be trimmed to correspond to
                        ;; where the BP is pointing
                        (right-half-blinker-trim blinker stopping-cha-no))
                       (t
                        ;; finally, take care of all the other rows
                        (update-region-row-blinker blinker)))))))))
  ;; @ this point all the blinkers are the correct size and inthe right place...
  (drawing-on-window (window)
    (dolist (blinker (interval-blinker-list region)) (draw-blinker blinker)))
  #-opengl
  (flush-port-buffer window))


;; these need to have with-drawing-port's wrapped around them because
;; they get called OUTSIDE of the redisplay
(defun make-interval-visible (region)
  (dolist (row-blinker (interval-blinker-list region))
    (setf (region-row-blinker-visibility row-blinker) t)))

(defun make-interval-invisible (region)
  (dolist (row-blinker (interval-blinker-list region))
    (setf (region-row-blinker-visibility row-blinker) nil)))



;;; The fast track
#|

(defun update-tracking-blinkers (blinkers screen-box start-row stop-row
					  start-x stop-x context-x context-y)
  (let ((remaining-blinkers blinkers)
	(return-blinkers blinkers)
	(sr-x (+ context-x (screen-obj-x-offset screen-box)
                  (slot-value screen-box 'scroll-x-offset)))
	(sr-y (+ context-y (screen-obj-y-offset screen-box)
                  (slot-value screen-box 'scroll-y-offset))))
    (do-self-and-next-sv-contents (screen-row
				   (slot-value screen-box 'screen-rows)
				   start-row)
      (let ((blinker (or (car remaining-blinkers)
			 ;; if we have run out, add them onto the
			 ;; end of the return-blinkers list
			 (let ((new (allocate-region-row-blinker screen-row)))
			   (nconc return-blinkers (list new))
			   new))))
	;; next in line...
	;; do this now because ww may RETURN from the next expression
	(setq remaining-blinkers (cdr remaining-blinkers))
	(let ((bl-y (+ sr-y (screen-obj-y-offset screen-row)))
	      (bl-hei (screen-obj-hei screen-row)))
	  ;; now handle specific case
	  (cond ((and (eq screen-row start-row) (eq screen-row stop-row))
		 (update-blinker-values blinker screen-row
					(+ sr-x
					    (screen-obj-x-offset screen-row)
					    (min start-x stop-x))
					bl-y
					(abs (- stop-x start-x)) bl-hei)
		 ;; pop out of the loop now that we have gotten to the last row
		 (return))
		((eq screen-row start-row)
		 (update-blinker-values blinker screen-row
					(+ sr-x
					    (screen-obj-x-offset screen-row)
					    start-x)
					bl-y
					(- (screen-obj-wid screen-row)
					    start-x)
					bl-hei))
		((eq screen-row stop-row)
		 (update-blinker-values blinker screen-row
					(+ sr-x
					    (screen-obj-x-offset screen-row))
					bl-y stop-x bl-hei)
		 (return))
		(t
		 (update-blinker-values blinker screen-row
					(+ sr-x
					    (screen-obj-x-offset screen-row))
					bl-y
					(screen-obj-wid screen-row) bl-hei))))))
    ;; if there are any extra blinkers in the list, turn them
    ;; off, we can stop when we have found one that is already turned off
    (dolist (bl remaining-blinkers)
      (cond ((null (region-row-blinker-visibility bl)) (return))
	    (t (with-open-blinker (bl #+clx nil)
		 (setf (region-row-blinker-visibility bl) nil
		       (region-row-blinker-uid bl) nil)))))
    #+clx (bw::display-finish-output bw::*display*)
    return-blinkers))

(defun update-blinker-values (blinker uid x y wid hei)
  (cond ((and (eq uid (region-row-blinker-uid blinker))
	      (not (null (region-row-blinker-visibility blinker)))
	      (= y (region-row-blinker-y blinker))
	      (= hei (region-row-blinker-hei blinker))
	      (or (not (= x (region-row-blinker-x blinker)))
		  (not (= wid (region-row-blinker-wid blinker)))))
	 ;; check for and optimize the common case of moving
	 ;; back and forth along the same row
	 #+clx
	 ;; this stuff really belongs in boxwin-xxx
	 (progn
	   ;; we know that either the wid or x or both have changed
	   (unless (= (region-row-blinker-x blinker) x)
	     (%draw-rectangle (abs (- (bw::blinker-x blinker) x))
			      (bw::blinker-height blinker)
			      (min x (bw::blinker-x blinker))
			      (bw::blinker-y blinker)
			      alu-xor (bw::blinker-window blinker)))
	   (let ((new-right (+ x wid))
		 (old-right (+ (bw::blinker-x blinker)
				(bw::blinker-width blinker))))
	     (unless (= new-right old-right)
	       (%draw-rectangle (abs (- new-right old-right))
				(bw::blinker-height blinker)
				(min& new-right old-right)
				(bw::blinker-y blinker)
				alu-xor (bw::blinker-window blinker))))
	   (setf (region-row-blinker-x blinker) x
		 (region-row-blinker-wid blinker) wid)
	   )
	 #-clx
	 (with-open-blinker (blinker #+clx nil)
	   (setf (region-row-blinker-x blinker) x
		 (region-row-blinker-wid blinker) wid))
	 )
	 ((or (not (eq uid (region-row-blinker-uid blinker)))
	      (null (region-row-blinker-visibility blinker))
	      (not (=& x (region-row-blinker-x blinker)))
	      (not (=& y (region-row-blinker-y blinker)))
	      (not (=& wid (region-row-blinker-wid blinker)))
	      (not (=& hei (region-row-blinker-hei blinker))))
	  (with-open-blinker (blinker #+clx nil)
	    (setf (region-row-blinker-visibility blinker) t
		  (region-row-blinker-uid blinker) uid
		  (region-row-blinker-x blinker) x
		  (region-row-blinker-y blinker) y
		  (region-row-blinker-wid blinker) wid
		  (region-row-blinker-hei blinker) hei)))))
|#

;;;; mousy stuff

;;;; this function tells if the mouse is on top of the current region.


(defun mouse-on-region-being-defined-p ()
  (if (null boxer::*region-being-defined*)
      nil
      (let ((blinker-list (boxer::interval-blinker-list boxer::*region-being-defined*)))
	(multiple-value-bind (m-x m-y) (bw::mouse-window-coords)
	  (dolist (b-row blinker-list)
	    (if (coords-on-blinker-row m-x m-y b-row)
		(return t)))))))

(defun coords-on-blinker-row (m-x m-y b-row)
  (if (null b-row) nil)
  (let* ((x-low (region-row-blinker-x b-row))
	 (x-high (+ (region-row-blinker-x b-row)
		    (bw::region-row-blinker-width b-row)))

	 (y-low (region-row-blinker-y b-row))
  	 (y-high (+ (region-row-blinker-y b-row)
		    (bw::region-row-blinker-height b-row))))
    (and (and (< m-x x-high) (> m-x x-low))
	 (and (< m-y y-high) (> m-y y-low)))))



