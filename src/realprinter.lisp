;;;-*- Mode:Lisp; Syntax: Common-Lisp; Base: 10.;package: Boxer-*-
;;;
;;; $Header: realprinter.lisp,v 1.0 90/01/24 22:16:10 boxer Exp $
;;;
;;; $Log:	realprinter.lisp,v $
;;;Revision 1.0  90/01/24  22:16:10  boxer
;;;Initial revision
;;;

#|

        Copyright 1987 - 1998 Regents of the University of California

 Enhancements and Modifications Copyright 1998 - 2003 Pyxisystems LLC


                                      +-------+
             This file is part of the | Boxer | System
                                      +-Data--+


Modification History (most recent at top)
ToDO:make-editor-box-from-vc doesn't hack turtles correctly yet....
 8/20/02 added canonicalize-vc-name for use in make-editor-box-from-vc
 2/17/01 merged current LW and MCL files
10/05/99 added boxtop support in make-editor-box-from-vc
 9/07/99 changed make-editor-box-from-vc to print closet even if the box has
         been changed (null pointer to original editor box)
 4/16/99 names are capitized in make-editor-box-from-vc (instead of all upper)
10/16/98 scrolling info for port screen boxes is now reset during retarget-port
 8/24/98 record-port-printing now checks to see if the port has already been
         recorded to avoid multiple entries in the retargetting list which
         cause blowouts
 8/19/98 make the closet rendering part of make-editor-box-from-vc use
         #'make-editor-box-for-printing in order to capture the correct
         version of boxes in the closet
 8/19/98 started logging changes: source = boxer version 2.3beta

|#


#-(or lispworks mcl) (in-package 'boxer)
#+(or lispworks mcl) (in-package :boxer)

;;;; Port Printing...

(defvar *vc-target-printing-alist* nil)

(defvar *printing-ports-to-retarget* nil)

(defvar *outports* nil
  "ports whose target is outside of the context of the box being printed")

(defvar *creation-time-for-printing-vc* nil)


;;; THIS STILL NEEDS TO HACK NAMES

(defmacro with-port-retargetting (&body body)
  `(let ((*vc-target-printing-alist* nil)
	 (*printing-ports-to-retarget* nil)
	 (*outports* nil))
     (flet ((retarget-ports ()
	      (dolist (port *printing-ports-to-retarget*)
		(let* ((vc-target (getprop port 'retargetting-vc))
		       (new-target
			(cdr (fast-assq vc-target
					*vc-target-printing-alist*))))
		  ;; need to clear the entry here
		  (putprop port nil 'retargetting-vc)
		  (if (null new-target)
		      ;; Note that this is the case where we return a port
		      ;; to structure that was consed on the stack and so we
		      ;; don't have to worry about caching its link
		      (set-port-to-box port (make-editor-box-from-vc
					     vc-target))
		      ;; need to cache links here...
		      (retarget-port port new-target))))))
       (unwind-protect
	    (progn . ,body)
	 (retarget-ports)))))

(defun record-port-printing (port)
  (unless (fast-memq port *printing-ports-to-retarget*)
    (push port *printing-ports-to-retarget*)))

(defun record-target-printing (vc editor-box)
  (push (cons vc editor-box) *vc-target-printing-alist*))

(defun record-outlink-port (port)
  (push port *outports*))

(defun lookup-new-target (old-target)
  (cdr (fast-assq old-target *vc-target-printing-alist*)))


;; used to be in comdef.lisp until it started getting used
;; for lots of things, should probably be moved somewhere else, edvc.lisp ?

;;;; retarget-port: port new-box
;;; will unhook the port from its current box and hook it up to the new-box

(defun retarget-port (port newbox)
  (let ((old-target (ports port)))
    ;; cut away the connections to the old target
    (unless (null old-target)
      (remove-port old-target port)
      ;; this is the same as the top level part of delete-self-link-action
      ;; we can't use the rest of delete-self-link-action becasue we are
      ;; not really removing the target from the hierarchy
      ;; handle port link cache removal for the box itself...
      (let ((common-node (find-lowest-common-superior-box old-target port)))
	(unless (null common-node)
	  ;; remove inferior-links starting from the common node
	  (remove-link common-node (make-link 'inferior-link port old-target))
	  ;; remove 'port-branch-links starting from the port branch
	  ;; this should terminate when it reaches the common superior
	  (remove-link port (make-link 'port-branch-link port old-target))
	  ;; remove 'target-branch-links starting from the target
	  ;; branch.  This should also terminate at the common-node
	  (remove-link old-target
                       (make-link 'target-branch-link port old-target))))))
  (set-port-to-box port newbox)
  (insert-self-link-action newbox)
  (let ((dsl (display-style-list port)))
    (when (display-style-graphics-mode? dsl)
      ;; check to make sure the new target has graphics
      (when (null (graphics-sheet newbox))
	(setf (display-style-graphics-mode? dsl) nil)
	(dolist (sb (screen-objs port))
	  (toggle-type sb)
          (set-force-redisplay-infs? sb t)))))
  ;; this will uncrack any cracked ports...
  (inform-port-that-target-has-returned port)
  ;; if the port has been scrolled, we need to reset that info
  (dolist (sb (screen-objs port)) (set-scroll-to-actual-row sb nil))
  (modified port))

#|
;;; This is what we stick into rows for ports until we can retarget
;;; them.  The reason we need to do this is that inserting a port
;;; that doesn't have a legit target plays havoc with all the port caching
;;; code so we defer inserting ports untilthe very end when we have both
;;; a well defined hierarchy that we are inserting into and a legitimate
;;; port-target pair to insert.
;;;

;;; establish a default for these so we don't blow out
(defmethod insert-self-action ((self t) &optional superior)
  ;(declare (ignore self superior))
  nil)

(defmethod delete-self-action ((self t) &optional superior)
  ;(declare (ignore self superior))
  nil)
;; old stuff.  Using editor ports instead with the relevant info on the PLiST

;;; this ought to be just a vector but I'd like to be able to see
;;; what is what for now
(defstruct (port-retargetting-placeholder
	     (:conc-name prp-)
	     #+symbolics
	     (:print-function print-placeholder-internal))
  row
  target)

#+symbolics
(defun print-placeholder-internal (pl stream depth)
  (declare (ignore pl depth))
  (format stream ""))

|#

;;;; Formatting...

;;; Most of this stuff is just so I can say show-boxer-printing-options cause
;;; I KNOW I'm going to forget them all in a few weeks

(defvar *boxer-printing-options* nil)

(defmacro define-printing-option (name value &optional (doc-string nil))
  (let ((var-name (intern (symbol-format nil "*BOXER-PRINTING-OPTION-~A*" name))))
    `(progn (unless (fast-assq ',name *boxer-printing-options*)
	      (push (cons ',name ',var-name) *boxer-printing-options*))
	    (unless (null ,doc-string)
	      (setf (get ',name 'printing-documentation)
		    ,doc-string))
	      (defvar ,var-name ,value))))

(defun boxer-printing-option? (name)
  (fast-assq name *boxer-printing-options*))

(defun show-boxer-printing-options ()
  (format t "~%Currently Defined Boxer Printing Options are:")
  (dolist (option *boxer-printing-options*)
    (let ((doc (get (car option) 'printing-documentation)))
      (if (null doc)
	  (format t "~%~A is the variable ~A~%~
                    ~5twhich currently has a value of: ~A"
		  (car option) (cdr option) (symbol-value (cdr option)))
	  (format t "~%~A, ~A~%~
                     ~5t~A is bound to the variable ~A~%~
                     ~5twhich currently has a value of: ~A"
		  (car option) doc (car option)
		  (cdr option) (symbol-value (cdr option)))))))

;;; Standard Boxer Printing Options

(define-printing-option no-initial-whitespace t
  "Makes rows print out flush left when T.")

(define-printing-option format-preference ':left
  "Specifies how to combine formatting properties.
Allowed values are :LEFT :RIGHT and :MERGE.")

(define-printing-option default-formatting-property
    (make-whitespace 1)
  "What to Print out when you dont know what to print out")


(defun print-formatting-property (fi chas-array row
				     &optional last-p no-extra-space-p)
  (cond ((and (null fi) last-p))
	((null fi)
	 ;; the default formatting property is a space
	 (print-formatting-property
	   *boxer-printing-option-default-formatting-property*
	   chas-array
	   row))
	((eq fi ':start)
	 ;; this is formatting-info of the first chunk on the row
	 (when (null *boxer-printing-option-no-initial-whitespace*)
	   (print-formatting-property
	     *boxer-printing-option-default-formatting-property*
	     chas-array
	     row)))
	((null-fi? fi)
	 (unless (or last-p no-extra-space-p)
	   ;; Need to insure that items will be separated by spaces even if
	   ;; formatting-info (left +/or right) has 0 spaces
	   (fast-chas-array-append-cha chas-array #\Space)))
;	((not (null last-p))
	 ;; temporary hack.  this needs to be smart about when NOT to
	 ;; print the trailing fi.  An example of lossage is when a
	 ;; row terminating chunk is selected from in front of another chunk
	 ;; that has a label.  The terminating chunk now has a right-formatting
	 ;; property containing the label of the chunk that USED to follow it
	 ;; on the other hand, we would like to try and preserve comments
	 ;; this may end up neccesitating extra slots in chunk structs
;	 )
	(t
	 (do-fi-chas (cha fi)
	   (cond ((cha? cha)
		  (fast-chas-array-append-cha chas-array cha))
		 (t
		  ;; must be a box.
                  (let ((cb (copy-box cha)))
		    (fast-chas-array-append-cha chas-array cb)
		    (set-superior-row cb row))))))))

(defun merge-formatting-properties (f1 f2 chas-array row
				       &optional no-extra-space-p)
  (cond ((eql f1 f2)
	 ;; this is the easy common case where adjacent chunks
	 ;; have remained adjacent when it has become time to
	 ;; print them or else both chunks are newly created
	 ;; and neither of them has any associated formatting info
	 (print-formatting-property f1 chas-array row nil no-extra-space-p))
	((null f1)
	 (print-formatting-property f2 chas-array row nil no-extra-space-p))
	((eq f1 ':start)
	 ;; this is formatting-info of the first chunk on the row
	 (unless (and (or (numberp f2) (null f2))
		      *boxer-printing-option-no-initial-whitespace*)
	   (print-formatting-property f2 chas-array row)))
	((null f2)
	 (print-formatting-property f1 chas-array row nil no-extra-space-p))
	;; this clause is here for paranoia
	((or (not (formatting-info? f1)) (not (formatting-info? f2)))
	 (error "Both ~S and ~S should be formatting infos" f1 f2))
	;; at this point, we know that both f1 and f2 are legit
	;; formatting infos
	(t
	 (case *boxer-printing-option-format-preference*
	   (:right
            (let ((f2first (first-fi-cha f2)) (f1last (last-fi-cha f1)))
              ;; insure that there will be at least 1 space separating tokens
	      (when (and f2first (char= f2first #\space)
		         (not (and f1last (char= f1last #\space)))
		         (not no-extra-space-p))
	        (fast-chas-array-append-cha chas-array #\space)))
	    (print-formatting-property f1 chas-array row nil no-extra-space-p))
	   (:left
            (let ((f1last (last-fi-cha f1)) (f2first (first-fi-cha f2)))
	      (when (and f1last (char= f1last #\space)
		         (not (and f2first (char= f2first #\space)))
		         (not no-extra-space-p))
	        (fast-chas-array-append-cha chas-array #\Space)))
	    (print-formatting-property f2 chas-array row nil no-extra-space-p))
	   (:merge
	     ;; need to make some chunker mods to speed this up
	     ;; specifically, need to cache the fact that a fi is
	     ;; all spaces so we dont search the list every time
	     (cerror "How about printing the left format instead"
		     "Sorry, format info merging is not implemented yet")
             (let ((f1last (last-fi-cha f1)) (f2first (first-fi-cha f2)))
	       (when (and f1last (char= f1last #\space)
			  (not (and f2first (char= (first-fi-cha f2) #\space)))
			  (not no-extra-space-p))
	         (fast-chas-array-append-cha chas-array #\Space)))
	     (print-formatting-property f2 chas-array row
				       nil no-extra-space-p))))))



(defun print-thing-into-chas-array (item row &optional
					 (chas-array (chas-array row))
					 updating-editor-box)
    (cond ((numberp item)
	   (fast-string-into-chas-array (convert-number-to-string item)
					chas-array))
	  ((symbolp item)
	   (fast-string-into-chas-array (symbol-name item) chas-array))
	  ((virtual-copy? item)
	   (let ((new-box (make-editor-box-from-vc item)))
	     (fast-chas-array-append-cha chas-array new-box)
	     (set-superior-row new-box row)))
	  ((box? item)
	   (let ((new-box (make-editor-box-for-printing item
							updating-editor-box)))
	     (fast-chas-array-append-cha chas-array new-box)
	     (set-superior-row new-box row)))
	  ((virtual-port? item)
	   (let ((new-port (make-port-from-vp item)))
	     (fast-chas-array-append-cha chas-array new-port)
	     (set-superior-row new-port row)))
	  ((stringp item)
	   (fast-string-into-chas-array item chas-array))
	  ((eval-prop? item)
	   (fast-eval-prop-into-chas-array item row
					   chas-array updating-editor-box))
	  (t (error "Don't know how to insert ~S into ~S" item row))))

(defun fast-eval-prop-into-chas-array (prop row chas-array sbox)
  (let ((p (eval-prop-prop prop))
	(value (eval-prop-contents prop)))
    (case p
      (bu::@ (fast-chas-array-append-cha chas-array #\@)
	     (print-thing-into-chas-array value row chas-array sbox))
      (bu::eval-it (fast-chas-array-append-cha chas-array #\!)
		   (print-thing-into-chas-array value row chas-array sbox))
      (bu::previous-tell-environment
       (fast-chas-array-append-cha chas-array #\^)
       (print-thing-into-chas-array value row chas-array sbox))
      (bu::dots-list
       (print-thing-into-chas-array (car value) row chas-array sbox)
       (dolist (segment (cdr value))
	 (fast-chas-array-append-cha chas-array #\.)
	 (print-thing-into-chas-array segment row chas-array sbox))))))

(defun print-eval-props-into-chas-array (props chas-array)
  (dolist (prop props)
    (case prop
      (bu::@ (fast-chas-array-append-cha chas-array #\@))
      (bu::eval-it (fast-chas-array-append-cha chas-array #\!))
      (bu::previous-tell-environment
       (fast-chas-array-append-cha chas-array #\^)))))

(defun print-chunk-value-for-editor (chunk chas-array row
					   &optional
					   chunk-p updating-editor-box)
  (cond ((null chunk-p)
	 (print-thing-into-chas-array chunk row chas-array
				      updating-editor-box))
	((only-formatting-chunk? chunk)
	 (unless (null (chunk-pname chunk))
	   (print-formatting-property (chunk-pname chunk) chas-array row)))
	(t (let ((val (chunk-chunk chunk)))
	     (cond ((or (symbolp val)
			(numberp val))
		    (print-formatting-property (chunk-pname chunk)
					       chas-array
					       row))
		   (t
		    ;; we don't have to worry about eval-props in the
		    ;; symbol or number cases because they will be encoded
		    ;; into the chunk-pname for those chunk values
		    (let ((eval-props (getf (chunk-plist chunk) :eval-prop)))
		      (unless (null eval-props)
			(print-eval-props-into-chas-array eval-props
							  chas-array))
		      (print-thing-into-chas-array (chunk-chunk chunk)
						   row chas-array
						   updating-editor-box))))))))



;;;; numbers...

(defvar *print-rationals* nil)

(defvar *decimal-print-precision* 5)

(defvar *boxer-float-print-string* "~F")

(defun set-decimal-printing-precision (precision)
  (if (null precision)
      (setq *boxer-float-print-string* "~F")
      (setq *decimal-print-precision* precision
	    *boxer-float-print-string* (format nil "~~,~DF"
					       *decimal-print-precision*))))

(defun convert-number-to-string (number)
  (cond ((integerp number)
	 (format nil "~D" number))
	((floatp number)
	 (format nil *boxer-float-print-string*
		 #+lcl3.0 (case (lcl::extreme-float-p number)
			    (:minus-infinity most-negative-long-float)
			    (:plus-infinity most-positive-long-float)
			    (t number))
		 #-lcl3.0 number))
	((and (null *print-rationals*) (rationalp number))
	 (format nil *boxer-float-print-string* number))
	(t (format nil "~D" number))))

#|
 ;; this is for printing fractions > 1 as 1&2/3 but we need to
 ;; make the chunker understand this notation BEFORE we can
 ;; have the printer produce it....
	 (if (< (abs number) 1)
	     number
	   (multiple-value-bind (integer-part fractional-part)
              (truncate number)
	    (format nil "~D&~D" integer-part fractional-part)))
|#

(defun top-level-print-number (number)
  (make-box `((,(convert-number-to-string number)))))

;;; port retargetting is now handled inside of the top-level-eval-wrapper

(defun top-level-print-vc (vc)
  (let ((box (make-editor-box-from-vc vc)))
    (putprop box *outports* 'saved-inferior-ports)
    box))

(defun top-level-print-vp (vp) (make-port-from-vp vp t))

;;; functions for extracting graphics info from the VC's graphics slot
;;; see get-graphics-info in edvc.lisp for how the slot gets filled

(defun graphics-info-graphics-sheet (gi) (getf gi 'graphics-sheet))

(defun graphics-info-turtle (gi) (getf gi 'turtle))

(defun graphics-info-av (gi) (getf gi 'av-info))

;;; copying closets
;; In general, only a pointer to the closet in the original box is retained
;; this can be a problem when an item in the closet is symeval'd and then
;; CHANGEd because the variable lookup code only produces a copy of an
;; item on the cached-bind-alist.  Copy-Closet-Row-Checking-For-Changes
;; compares the items in the closet row against items in the
;; cached-binding-alist to see if they have been CHANGEd.  It is basically
;; a version of copy-row with a check inserted in the loop.
;;
;; At some point we may want to change this scheme to actually COPYing the
;; closet at virtual copy time.  This doesn't look like a good tradeoff
;; at the moment since many closets contain items which don't get modified
;; during the course of a particular EVAL.

(defun copy-closet-row-checking-for-changes (from-row binding-alist
						      &optional
						      new-previous-row
						      new-superior-box)
  (flet ((copy-chas-array (from-chas new-row)
	   (with-fast-chas-array-manipulation (from-chas fast-from-chas)
	     (flet ((make-length-n-chas-array (n)
		      (let ((array (make-chas-array
				    (max& *chas-array-default-size* n))))
			(setf (chas-array-active-length array) n)
			array)))
	       (let* ((length (chas-array-active-length from-chas))
		      (to-chas (make-length-n-chas-array length)))
		 (with-fast-chas-array-manipulation (to-chas fast-to-chas)
		   (dotimes& (index length)
		     (if (cha? (fast-chas-array-get-cha fast-from-chas index))
			 (setf (fast-chas-array-get-cha fast-to-chas index)
			       (fast-chas-array-get-cha fast-from-chas index))
			 (setf (fast-chas-array-get-cha fast-to-chas index)
			       ;; here is the check for modified items
			       ;; in the closet...
			       (let ((new-box
				      (let* ((box (fast-chas-array-get-cha
						   fast-from-chas index))
					     (nr (name-row box))
					     (name (unless (null nr)
						     (get-box-name nr)))
					     (alist-entry (fast-assq
							   name binding-alist))
					     (alist-value
					      (unless (null alist-entry)
						(eval::static-variable-value
						 alist-entry))))
					(if (or (null alist-value)
						(not (and (virtual-copy?
							   alist-value)
							  (vc-modified?
							   alist-value))))
					    (copy-box box)
					    (make-editor-box-from-vc
					     alist-value)))))
				 (setf (superior-row new-box) new-row)
				 new-box))))
		   to-chas))))))
    (let ((new-row (make-uninitialized-row)))
      (setf (actual-obj-tick new-row) -1)
      (setf (superior-box new-row) new-superior-box)
      (setf (previous-row new-row) new-previous-row)
      (setf (chas-array new-row)
	    (copy-chas-array (chas-array from-row) new-row))
      new-row)))

(defun make-editor-box-from-vc (vc)
  (let ((*creation-time-for-printing-vc* (vc-creation-time vc))
	(box (make-uninitialized-box (vc-type vc)))
	(original (vc-progenitor vc)))
    ;; fixup some slots that the init method would have fixed
    (let ((new-ds (if (not (box? original))
	            (make-display-style)
	            (copy-display-style (slot-value original
                                                    'display-style-list)))))
      (setf (slot-value box 'display-style-list) new-ds)
      ;; vc's never propogate transparency so make sure that
      ;; the result "looks" correct
      (setf (display-style-border-style new-ds)
	    (case (display-style-border-style new-ds)
              ((:thick :thick-dashed) :thick)
              (t nil))))
    (unless (null original)
      (let ((bt (getprop original :boxtop)))
        (unless (null bt) (putprop box bt :boxtop))))
    (setf (slot-value box 'tick) (tick))
    ;; look for and handle any box flags
    (when (box? original)
      (setf (slot-value box 'flags) (new-flags-for-copy original nil)))
    ;; if there was a graphics sheet in the original, copy it,
    (unless (or ; (null original)
	     (null (vc-graphics vc)))
      ;; first check for a graphics sheet
      (let ((graphics-sheet (graphics-info-graphics-sheet
			     (vc-graphics vc))))
	(unless (null graphics-sheet)
          ;; make sure we DON't deallocate the graphics sheet's
          ;; remember that graphics from VC's are marked for deallocation
          ;; at creation time
          (unqueue-non-lisp-structure-for-deallocation graphics-sheet)
	  (setf (graphics-info box) graphics-sheet)
	  (setf (graphics-sheet-superior-box graphics-sheet) box)
	  ;; if there IS graphics, make sure that the graphics view
	  ;; is the one to be presented....
	  (setf (display-style-graphics-mode? (display-style-list box)) t))
        ;; check for other visual props which may occur with a graphics sheet
        ;; like av-info, gif?, jpeg?
        (let ((av (graphics-info-av (vc-graphics vc))))
          (unless (null av)
            (unqueue-non-lisp-structure-for-deallocation av)
            (putprop box av 'av-info)))
        ;; turtle needs to be handled AFTER the rows and the closets are
        ;; added because we might need the values in the boxes to properly
        ;; initialize the graphics-object
        ;; the turtle isn't used anymore except perhaps to derive the
        ;; type of the graphics object
        ))
    ;; if the box is an unchanged copy, then look for a closet to copy
    ;; NoNoNo, preserve closets even for changed boxes....
    (let ((closet (vc-closets vc)))
      (unless (null closet) ; (null original) ;flushed on changed
	;; Note: closet can be an evrow (usually inside of build templates)
	(when (evrow? closet) (setq closet (make-row-from-evrow closet vc)))
	(add-closet-row box
			(let ((alist (vc-cached-binding-alist vc))
                              (*recursive-copy-box-function*
                               #'make-editor-box-for-printing))
			  (if (and (not (null original))
                                   (or (null alist)
				       (eq alist *no-names-in-vc-marker*)))
			      (copy-row closet)
			      (copy-closet-row-checking-for-changes
			       closet alist))))))
    ;; if this can be a port target, then record it
    (unless (null (vc-port-target? vc))
      (record-target-printing vc box))
    ;; if the box is a sprite, make the graphics-object here BEFORE the
    ;; rows are rendered.  We attach it to the sprite box in a minimal
    ;; way so that any subsprites in the inferior rows can be hooked up
    ;; to the supersprite correctly.  After the rows have been articulated,
    ;; we will link the turtle to the sprite box in a more careful way
    (when (eq (vc-type vc) 'sprite-box)
      (let* ((old-go (graphics-info-turtle (vc-graphics vc)))
             (go (if (null old-go)
                     (make-instance *default-graphics-object-class*)
                     (copy-graphics-object old-go))))
        (setf (graphics-info box) go)
        (setf (slot-value go 'sprite-box) box)))
    ;; now append the rows, make sure there is at least 1 row in the result
    (if (null (vc-rows vc))
	(append-row box (make-row '()))
	(dolist (vr (vc-rows vc)) (append-row box (make-row-from-evrow vr vc))))
    ;; now that the rows have been calculated, we link the turtle into the
    ;; sprite box, checking for the possibility that some of the turtle's
    ;; instance variables have box values
    (when (eq (vc-type vc) 'sprite-box)
      (link-graphics-object-to-box (graphics-info box) box))
    (unless (null (vc-name vc))
      (set-name box (make-name-row (list (canonicalize-vc-name (vc-name vc))))))
    (when (not (null (vc-exports vc))) (eval::set-box-transparency box T))
    ;; hooks
    (dolist (hook *print-vc-special-structures-hook*) (funcall hook vc box))
    box))

(defvar *canonicalize-vc-name-method* :capitalize)

(defun canonicalize-vc-name (string)
  (ecase *canonicalize-vc-name-method*
    (:capitalize (string-capitalize string))
    (:upcase     (string-upcase     string))
    (:downcase   (string-downcase   string))
    (:leave      (string            string))))

;; attaches a newly intialized graphics-object to a box
;; instance variable values are to be derived from the box and the
;; box-interface mechanism needs to supplant existing bindings in the box
(defun link-graphics-object-to-box (go box)
  ;; first, we need to set the values in the graphics-object from any
  ;; available values in the box
  (let ((alist (static-variables-alist box)))
    (dolist (slot-name (all-interface-slots go))
      (let ((existing (member slot-name alist
                              :test #'(lambda (slot binding)
                                        (string= slot (car binding))))))
        (unless (null existing)
          (set-slot-value-from-binding (slot-value go slot-name) (car existing))))))
  ;; Now we can attach the graphics-object to the box
  (setf (graphics-info box) go)
  (set-sprite-box go box)
  (update-save-under go)
  ;; make sure the window-shape-cache agrees with the (possibly) new shape
  (update-window-shape-allocation go)
  ;; force recompute of offsets and extents of the commands in the window shape
  (invalidate-window-shape-and-extent-caches go))

(defun set-slot-value-from-binding (slot binding)
  (let ((box (eval::static-variable-value binding)))
    (case (box-interface-slot-name slot)
      ((heading x-position y-position) (setf (box-interface-value slot)
                                             (check-and-get-number-arg box)))
      (shown? (setf (box-interface-value slot)
                    (check-and-get-hide-arg box slot)))
      (pen (setf (box-interface-value slot)
                 (check-and-get-pen-arg box)))
      (pen-width (setf (box-interface-value slot)
                       (check-and-get-pen-width-arg box)))
      (type-font (setf (box-interface-value slot)
                       (check-and-get-type-font-arg box)))
      (pen-color (let ((nc (graphics-sheet-background (graphics-sheet box))))
                   (when (color? nc) (setf (box-interface-value slot) nc))))
      (sprite-size (setf (box-interface-value slot)
                         (check-and-get-size-arg box)))
      (home-position (setf (box-interface-value slot)
                           (check-and-get-number-args box)))
      ;; does NOT hack recursive eval since we are trying to phase that out
      ;; only do something if we detect graphics in the box
      (shape (let* ((gs (graphics-sheet box))
                    (gl (and gs (graphics-sheet-graphics-list gs)))
                    (shape (box-interface-value slot)))
               (unless (null gl)
                 (with-graphics-vars-bound (box)
	           (clear-graphics-list shape)
	           (do-vector-contents (graphics-command gl)
	             (sv-append shape (allocate-window->boxer-command
				       graphics-command))))))))))

;; this is used by PROCESS-EDITOR-OBJECT-MUTATION-QUEUE
(defun make-editor-rows-from-evrows (evrows vc-rows-entry
					    creation-time editor-box)
  (let ((*creation-time-for-printing-vc* creation-time)
	(return-rows nil))
    ;; now process the rows
    (cond ((null evrows)
	   (push (make-row '()) return-rows))
	  ((numberp evrows)
	   (push (make-row (list (convert-number-to-string evrows)))
		 return-rows))
	  (t
	   ;; make sure that there is at least 1 empty row
	   (dolist (vr evrows (setq return-rows (nreverse return-rows)))
	     (push (make-row-from-evrow vr vc-rows-entry editor-box)
		   return-rows))))
    return-rows))

;;; now returns an extra value of T if the port needs to be retargetted
(defun make-port-from-vp (vp &optional ignore-name?)
  ;; now reset the target
  (let ((target (vp-target vp))
	(port (make-uninitialized-box 'port-box)))
    ;; fixup some slots that the init method would have fixed
    (setf (slot-value port 'display-style-list) (make-display-style))
    (setf (slot-value port 'tick) (tick))
    (unless (or ignore-name? (null (vp-name vp)))
      (set-name port (make-name-row (list (vp-name vp)))))
    (cond ((virtual-copy? target)
	   (let ((new-target (lookup-new-target target)))
	     (cond ((not (null new-target))
		    (set-port-to-box port new-target)
		    port)
		   (t
		    (putprop port target 'retargetting-vc)
		    (record-port-printing port)
		    (values port t)))))
	  ((box? target)
	   ;; ports to editor boxes can be made immediately
	   (set-port-to-box port target)
	   ;; record it so port caching will work later
	   (record-outlink-port port)
	   port)
          ((typep target 'foreign-data)
           (set-port-to-box port (make-editor-box-from-foreign-data target))
           port)
	  (t
	   (error "Virtual Port target, ~S, is not a Box or Virtual Copy"
		  target)))))

#|
;;; old stuff.  when vc's are targets, use real ports and record the VC
;; on the plist to be retargetted later

(defun make-port-from-vp (vp)
  ;; now reset the target
  (let ((target (vp-target vp)))
    (cond ((virtual-copy? target)
	   ;; this may have been an interior link so record it
	   ;; cause we may need to retarget it later
	   (let ((placeholder (make-port-retargetting-placeholder
				:target target)))
	     (record-port-printing placeholder)
	     placeholder))
	  ((box? target)
	   (let ((port (make-uninitialized-box 'port-box)))
	     ;; fixup some slots that the init method would have fixed
	     (setf (slot-value port 'display-style-list) (make-display-style))
	     (setf (slot-value port 'tick) (tick))
	     ;; ports to editor boxes can be made immediately
	     (set-port-to-box port target)
	     ;; record it so port caching will work later
	     (record-outlink-port port)
	     (unless (null (vp-name vp))
	       (set-name port (make-name-row (list (vp-name vp)))))
	     port))
	  (t
	   (error "Virtual Port target, ~S, is not a Box or Virtual Copy"
		  target)))))
|#

(defmethod make-editor-box-for-printing ((eb box) &optional new-superior)
  (if (numberp *creation-time-for-printing-vc*)
      (cond ((and new-superior (eq new-superior (superior-box eb))) eb)
            ((all-new-box? eb)
             (setf (all-new-box? eb) nil)
             eb)
	    ((null (slot-value eb 'virtual-copy-rows))
	     ;; looks like this box hasn't been touched at all
	     ;; during EVAL so we can just (editor) copy it
	     ;(copy-box eb)
             ;; WRONG !!!!!
             ;; even if THIS level of box is untouched it is possible
             ;; for a deep inferior of this box to have been modified
             ;; via ports so the recursive copy of the box has to be
             ;; able to handle the possibility of encountering versions
             ;; of virtual-copy-rows at ANY level
             (let ((*recursive-copy-box-function* #'make-editor-box-for-printing))
               (copy-box eb))
             )
	    (t
	     (let ((box (allocate-instance (class-of eb))))
	       (shared-initialize box t)
	       ;; fixup some slots that the init method would have fixed
	       (copy-special-box-properties eb box)
	       (setf (slot-value box 'tick) (tick))
	       ;; now fix up the innards
	       (cond ((port-box? box)
		      (record-outlink-port box)
		      (set-port-to-box box (ports eb)))
		     (t
		      ;; the deal with the rows
		      (let* ((vrows (virtual-copy-rows
				     eb *creation-time-for-printing-vc*))
			     (prev-row (if (null vrows)
					   (make-row '())
					   (make-row-from-evrow (car vrows)
								nil))))
			;; if the box is an unchanged copy,
			;; look for a closet to copy
			(let ((original-closet (and eb
						    (slot-value eb 'closets))))
			  (unless (null original-closet)
			    (let ((closet-in-box (row-row-no eb
							     original-closet)))
			      (if (null closet-in-box)
				  (add-closet-row box
						  (copy-row original-closet))
				  (setf (slot-value box 'closets)
					(copy-row original-closet))))))
			;; attach the 1st row
			(append-row box prev-row)
			;; now loop through the remaining rows
			(do* ((vrs (cdr vrows) (cdr vrs))
			      (vr (car vrs) (car vrs)))
			     ((null vrs) )
			  (let ((current-row (make-row-from-evrow vr nil)))
			    (insert-row-after-row box current-row prev-row)
			    (setq prev-row current-row))))))
	       box)))
      (error "Looks like we are trying print an editor box that ~
              is NOT part of a VC")))


(defun make-row-from-evrow (evrow sup-box &optional updating-editor-box)
  (let* ((new-row (make-initialized-row))
	 (chas-array (chas-array new-row))
	 (current-right-format ':start)
	 (evrow-format (evrow-row-format evrow)))
    ;; fix the timestamp
    (setf (slot-value new-row 'tick) (tick))
    (cond ((and (null evrow-format)
		(null (evrow-pointers evrow))))
	  ((null evrow-format)
	   (let ((last-chunk-was-box? nil))
	     (dolist (el (evrow-pointers evrow))
	       (let* ((chunk (get-pointer-value el sup-box))
		      (chunk-p (chunk-p chunk))
		      (value (if chunk-p (chunk-chunk chunk) chunk))
		      (current-left-format  (when chunk-p
					      (chunk-left-format chunk))))
	       ;;; first, print the stuff on the left
		 (merge-formatting-properties
		  current-right-format current-left-format
		  chas-array new-row
                  (let ((this-chunk-is-box? (or (virtual-copy? value)
                                                (virtual-port? value)
			                        (box? value))))
		    (prog1
                      (or
		       ;; don't need to insert spaces if
		       ;; the previous or current chunk is a box...
		       last-chunk-was-box? this-chunk-is-box?
		       ;; or this chunk has a leading
		       ;; special character
		       (when chunk-p
		         (fast-memq (car (getf (chunk-plist chunk) :eval-prop))
				    '(bu::@ bu::eval-it
				      bu::previous-tell-environment))))
                      (setq last-chunk-was-box? this-chunk-is-box?))))
	       ;;; now print the chunk itself
		 (print-chunk-value-for-editor
		  chunk chas-array new-row chunk-p updating-editor-box)
	       ;;; setup the right-format for the next entry in the row
		 (setq current-right-format
		       (when chunk-p (chunk-right-format chunk))))))
	   ;; finally, print the remaining right format
	   (print-formatting-property current-right-format
				      chas-array new-row t))
	  ((chunk-p evrow-format)
	   (when (formatting-info? (chunk-left-format evrow-format))
	     (print-formatting-property (chunk-left-format evrow-format)
					chas-array
					new-row))
	   (when (formatting-info? (chunk-pname evrow-format))
	     (print-formatting-property (chunk-pname evrow-format)
					chas-array
					new-row))
	   (when (formatting-info? (chunk-right-format evrow-format))
	     (print-formatting-property (chunk-right-format evrow-format)
					chas-array
					new-row)))
	  (t (error "The evrow, ~S, cannot be printed" evrow)))
    new-row))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           Old stuff below                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(defun convert-vc-box-to-editor-box (vc)
  (let ((box (make-uninitialized-box (vc-type vc)))
	(last-row (convert-evrow-to-editor-row (car (vc-rows vc)) vc)))
    (initialize-from-defaults box)
    (insert-row-at-row-no box last-row 0)
    (dolist (evrow (cdr (vc-rows vc)))
      (let ((new-row (convert-evrow-to-editor-row evrow vc)))
	(insert-row-after-row box new-row last-row)
	(setq last-row new-row)))
    (when (vc-name vc)
      (set-name box (make-name-row `(,(vc-name vc)))))
    box))


(defun convert-evrow-to-editor-row (evrow superior-box)
  (make-row-no-spaces
    (mapcon #'(lambda (list)
		(convert-to-editor-object
		  (get-pointer-value (car list) superior-box)
		  (null (cdr list))))
	    (evrow-pointers evrow))))

(defun convert-to-editor-object (object lastp)
  (cond ((chunk-p object)
	 (let* ((left (chunk-left-format object))
		(pname (chunk-pname object))
		(right (chunk-right-format object))
		(chars (fi-chas left)))
	   (nconc (subseq chars
			  (fi-start left)
			  (fi-stop left))
		  (cond ((box? pname) (list (copy-box pname)))
			((virtual-copy? object)
                         (list (convert-vc-box-to-editor-box pname)))
			(t (subseq chars
				   (fi-start pname)
				   (fi-stop pname))))
		  (if lastp
		      (subseq chars
			      (fi-start right)
			      (fi-stop right))))))
	(t
	 (let ((result (cond ((or (numberp object)
                                  (symbolp object)
                                  (characterp object))
			      object)
			     ((virtual-copy? object)
			      (convert-vc-box-to-editor-box object))
			     ((box? object) (copy-box object))
			     (t (error "~S is not a chunk" object)))))
	   (if lastp (list result) (list result #\space))))))

;;; *** We need to rewrite convert-to-editor-object
(defun make-row-no-spaces (list)
  (let* ((new-row (make-initialized-row))
	 (ca (chas-array new-row))
	 (idx 0))
    (dolist (item list)
      (cond ((numberp item) (fast-string-into-chas-array
			     (convert-number-to-string item) ca))
	    ((stringp item) (fast-string-into-chas-array item ca))
	    ((symbolp item) (fast-string-into-chas-array (symbol-name item) ca))
	    ((characterp item) (fast-chas-array-append-cha ca item))
	    ((box? item)
	     (fast-chas-array-append-cha ca item)
	     (set-superior-row item new-row))
	    (t (error "Don't know how to make a row out of ~S" item)))
      (incf idx))
    new-row))

|#
