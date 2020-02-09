;;;-*- Mode:Lisp; Syntax: Common-Lisp; Base: 10.;package: Boxer-*-


#|


 $Header: virtcopy.lisp,v 1.0 90/01/24 22:19:11 boxer Exp $

 $Log:	virtcopy.lisp,v $
;;;Revision 1.0  90/01/24  22:19:11  boxer
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


    This file contains the virtual copy mechanism for the Boxer Evaluator


Modification History (most recent at top)

 4/02/05 UPDATE-EVROW-FOR-NEW-VC now handles possible string values for
         evrow-items.  They can arise from user prompting prims
 7/11/02 smarter showmp
 5/19/01 port-to-item added simple-vector-p check before using fast-eval-port-box?
 1/15/01 merged with current mac version
 6/09/99 virtual-copy-virtual-copy sets progenitor slot to nil when
         copying modified VC's
 3/18/99 vc-pedigree-for-copy changed to use (now) when extending the pedigree
         this was done to capture the correct inferior multipointer slot (the
         bug case being change var { {x} <-[port] }
 6/5/98 added support for exporting inferiors in lookup-variable-in-virtual-copy-1
 6/2/98 access-pointer-element added check for box? as alternative to chunk
 6/2/98 Started logging changes: source = Boxer version 2.3alphaR2


|#


#-(or lispm mcl) (in-package 'boxer)
#+mcl            (in-package :boxer)



;;;; Predicates

#|see the file eval/ev-int.lisp for the real defs
(DEFUN EVAL-BOX? (THING)
  ;; this is to avoid multiple calls to typep
  (MEMBER (TYPE-OF THING) '(VIRTUAL-COPY VIRTUAL-PORT PORT-BOX DATA-BOX DOIT-BOX)))	;etc

(DEFUN EVAL-DOIT? (THING)
  (AND (VIRTUAL-COPY? THING) (EQ (VC-TYPE THING) ':DOIT-BOX)))

(DEFUN EVAL-DATA? (THING)
  (AND (VIRTUAL-COPY? THING) (EQ (VC-TYPE THING) ':DATA-BOX)))

(DEFSUBST EVAL-PORT? (THING)
  (VIRTUAL-PORT? THING))

(DEFSUBST EVAL-NAMED? (THING)
  (NOT (NULL (BOX-NAME THING))))

|#


(defun empty-evrow? (evrow)
  (or (null evrow) (null (evrow-pointers evrow))))




;;;; Constructors for EvRows

(defsubst make-evrow-from-entry (thing)
  (make-evrow :pointers (list (make-pointer thing))))

(defsubst make-evrow-from-entries (things)
  (make-evrow :pointers (mapcar #'make-pointer things)))

(defsubst make-evrow-from-pointer (pointer)
  (make-evrow :pointers (list pointer)))

(defsubst make-evrow-from-pointers (ptrs)
  (make-evrow :pointers ptrs))

(defun append-evrows (&rest evrows)
  (make-evrow :pointers (reduce #'nconc (mapcar #'evrow-pointers evrows))))

(defun make-empty-evrow (&optional (spaces 0))
  (if (zerop spaces)
      (make-evrow)
      (make-evrow :row-format (make-whitespace spaces))))

(defun make-evrow-from-port-with-pointer (item superior)
  (make-evrow :pointers (list (make-pointer (port-to-item item superior t)))))

(defun make-evrow-from-port-with-pointers (items superior)
  (make-evrow :pointers (mapcar #'(lambda (it)
				   (make-pointer
				     (port-to-item it superior t)))
			       items)))

;; this is smart enough to hack whitespace
(defun make-evrow-from-string (string)
  (let ((ss (make-test-stream string)))
    (with-local-formatting-info (ss)
      (make-evrow-from-pointers (chunk-top-level ss)))))

;;;; Data Accessor Utilities
;;; Use these macros for taking apart virtual copies.
;;; The evaluator should seldom see anything but virtual copies, anyway.
;;; Don't use these macros for getting eval objects out of virtual
;;; copies, like for @.  Use [something else] instead.

;;; This needs to properly handle :default and :single values returned
;;; from get-pointer-value

(defun access-evrow-element (vc element
				&optional
				(need-exact-match nil) (chunk-too? nil))
  (multiple-value-bind (item exact?)
       (get-pointer-value element vc)
     (let ((answer (if chunk-too? item (access-pointer-element item))))
       (if (or (not need-exact-match)
	       (symbolp answer) (numberp answer)
	       (and (chunk-p answer) (or (symbolp (chunk-chunk answer))
					 (numberp (chunk-chunk answer))))
	       (eq exact? t)
	       (and (or (eq exact? 'single) (eq exact? ':default))
		    ;; default can come about if something else makes
		    ;; multi pointers
		    (vc-rows-entry? vc)
		    (vc-rows-entry-single-is-exact? vc)))
	   answer
	   ;; looks like we need to generate an exact match
	   ;; which (maybe) keeps the formatting info
	   (let ((new-answer
		  (if (and chunk-too? (chunk-p item))
		      (let ((nc (copy-chunk item)))
			(setf (chunk-chunk nc)
			      (virtual-copy (access-pointer-element item)
					    :top-level? t))
			nc)
		      (virtual-copy (access-pointer-element item)
				    :top-level? t))))
	     (append-value-to-pointer element vc
				      (vcis-creation-time vc) new-answer)
	     (when (virtual-copy? new-answer)
	       (setf (vc-superior-for-scoping new-answer) vc))
	     new-answer)))))

#|
(defun access-evrow-element (vc element
				&optional
				(need-exact-match nil) (chunk-too? nil))
  (multiple-value-bind (item exact?)
       (get-pointer-value element vc)
     (let ((answer (if chunk-too? item (access-pointer-element item))))
       (if (and need-exact-match (or (not exact?)
				     (eq exact? ':default)
				     (and (eq exact? 'single)
					  ;;; aaacckkk!!!!
					  (not (=& (length vc) 9))
					  (not
					   (vc-rows-entry-single-is-exact?
					    vc)))))
	   (let ((new-answer (virtual-copy answer :top-level? t)))
	     (append-value-to-pointer element vc
				      (vcis-creation-time vc) new-answer)
	     new-answer)
	   answer))))
|#

(defun access-pointer-element (element)
  (if (or (numberp element) (symbolp element) (box? element))
      element
      (if (fast-chunk? element)
	  (chunk-chunk element)
	  element)))




;;;; Copying functions

;;; The file edvc.lisp contains the functions which make evaluator
;;; objects from editor objects

;;; copying various kinds of boxes
;; A copy of a copy needs to record the same inferiors and type and a new
;; creation time we make the simple optimization that copying an unchanged
;; box is the same thing as a copying that unchanged box's immediate ancestor.
;; We don't have to worry about the caches because they are supposed to be
;; flushed when the modified bit is

(defvar *vc-vc-target-alist* nil)
(defvar *vps-to-retarget* nil)

(defun mark-vc-target-copy (oldvc newvc)
  (push (cons oldvc newvc) *vc-vc-target-alist*))

(defun mark-vp-for-retargetting (vp)
  (push vp *vps-to-retarget*))

(defmacro with-virtual-port-retargetting (&body body)
  `(let ((*vc-vc-target-alist* nil)
	 (*vps-to-retarget* nil))
     (flet ((retarget-vps ()
	      (dolist (vp *vps-to-retarget*)
		(let* ((current-target (vp-target vp))
		       (new-target (cdr (fast-assq current-target
					      *vc-vc-target-alist*))))
		  (unless (null new-target)
		    (setf (vp-target vp) new-target))))))
       . ,body)))

(defun top-level-virtual-copy-virtual-copy (vc &optional top-level?)
  (with-virtual-port-retargetting
    (let ((newvc (virtual-copy-virtual-copy vc top-level?)))
      (retarget-vps)
      newvc)))

(defun top-level-virtual-copy-virtual-port (vp &optional top-level?)
  (let ((new-vp (copy-virtual-port vp)))
    (when top-level?
      (setf (vp-name new-vp) (vp-name vp)))
    new-vp))



;;; this is only called if it appears that we may have to check
;;; for and handle port links.  If so, it extends the relevant
;;; pointers.  Returns non-NIL only if it had to extend a multipointer
(defun update-evrow-for-new-vc (evrow old-sup-vc new-sup-vc links?)
  (let ((had-to-extend? nil))
    (dolist (ptr (evrow-pointers evrow))
      (setq had-to-extend?
	    (or
	      (let* ((raw-chunk (get-pointer-value ptr old-sup-vc))
		     (chunk-p (chunk-p raw-chunk))
		     (value (if chunk-p (chunk-chunk raw-chunk)
				raw-chunk)))
		(cond ((or (symbolp value) (numberp value)
                           ;; some prompting functions can make VC's with
                           ;; strings like CHOOSE-FILE
                           (stringp value))
		       nil)
		      ((and (virtual-copy? value)
			    (not links?)
			    (not (vc-inlinks? value))
			    (not (vc-port-target? value)))
		       nil)
		      ((and (virtual-port? value)
			    (not (virtual-copy? (vp-target value))))
		       nil)
		      ((box? value)
		       (unless (and (null links?)
				    (null (ports value)))
			 (let ((newvc (virtual-copy-editor-box value)))
			   (if (null chunk-p)
			       (extend-pointer ptr new-sup-vc (now) newvc)
			       (let ((new-chunk (copy-chunk raw-chunk)))
				 (setf (chunk-chunk new-chunk) newvc)
				 (extend-pointer ptr new-sup-vc (now)
						 new-chunk)))
			   t)))
		      ;; gots to virtual copy it, oh well
		      ((virtual-port? value)
		       (let ((newport (virtual-copy-virtual-port value)))
			 (unless (null links?)
			   (mark-vp-for-retargetting newport))
			 (if (null chunk-p)
			     (extend-pointer ptr new-sup-vc (now) newport)
			     (let ((new-chunk (copy-chunk raw-chunk)))
			       (setf (chunk-chunk new-chunk) newport)
			       (extend-pointer ptr new-sup-vc (now)
					       new-chunk))))
		       t)
		      ((virtual-copy? value)
		       (let ((newvc (virtual-copy-virtual-copy value)))
			 (unless (null links?)
			   (mark-vc-target-copy value newvc))
			 (if (null chunk-p)
			     (extend-pointer ptr new-sup-vc (now) newvc)
			     (let ((new-chunk (copy-chunk raw-chunk)))
			       (setf (chunk-chunk new-chunk) newvc)
			       (extend-pointer ptr new-sup-vc (now)
					       new-chunk))))
		       t)
		      (t (error "~S is an unexpected evrow item" value))))
	      had-to-extend?)))
    had-to-extend?))

;;; this should do the same job as update-evrow-for-new-vc except that
;;; instead of extending the multipointers, it conses up a new
;;; evrow with single pointers
(defun make-evrow-from-evrow (row old-sup-vc new-sup-vc)
  (declare (ignore row old-sup-vc new-sup-vc))
  (error "Not implemented yet"))

(defun virtual-copy-virtual-copy (vc &optional (top-level? t))
  (let* ((newped (copy-seq (vc-pedigree vc)))
	 (newvc (make-virtual-copy
		 :type (vc-type vc)
		 :name (when top-level? (vc-name vc))
		 :pedigree newped
		 :creation-time (let ((time (vc-creation-time vc)))
				  (if (=& time -2) -1 time))
		 :port-target? (vc-port-target? vc)
		 :progenitor (vc-progenitor vc)
		 :closets (vc-closets vc)
		 :graphics (vc-graphics vc)
		 :exports (when top-level? (vc-exports vc))))
	 (inlinks? (vc-inlinks? vc)))
    ;; adjust the pedigree and progenitor if we have to
    (when (vc-modified? vc)
      (push (cons vc (now)) (vc-pedigree newvc))
      ;; seems safe, progenitor slot is used mostly to help preserve formatting in
      ;; realprinter.lisp and further down to get port targets correct but ONLY is
      ;; the creation-time is -2, by this point, creation-time should be somthing else...
      ;;
      ;; put this back in, some other prims (like RUN) were using this to access
      ;; no longer valid cached data via the editor box
      (setf (vc-progenitor newvc) nil)
      )
    ;; now we figure out the inlinks? and the evrows
    (cond ((and (null inlinks?) (null (vc-modified? vc)))
	   ;; The simple fast case where there are NO
	   ;; inlinks to articulate
	   (setf (vc-rows newvc) (vc-rows vc)))
	  (t
	   ;; otherwise, we have to loop through the rows
	   (setf (vc-rows newvc) (vc-rows vc))
	   (dolist (vcr (vc-rows newvc))
	     (let ((row-had-to-be-fixed
		      (update-evrow-for-new-vc vcr vc newvc inlinks?)))
	     (when (null inlinks?)
	       (setq inlinks? row-had-to-be-fixed))))))
    (when (not (null inlinks?))
      (setf (vc-inlinks? newvc) inlinks?)
      (setf (vc-modified? newvc) t))
    ;; if the vc was a port target, then we need to
    ;; save the info since we MAY want to retarget ports
    (when (vc-port-target? newvc)
      (mark-vc-target-copy vc newvc))
    ;; hooks
    (dolist (hook *vcvc-special-structures-hook*) (funcall hook vc newvc))
    newvc))

(defun virtual-copy-virtual-port (vp &optional top-level?)
  (let ((newvp (copy-virtual-port vp)))
    (unless (null top-level?)
      (setf (vp-name newvp) (vp-name vp)))
    (when (virtual-copy? (vp-target newvp))
      (mark-vp-for-retargetting newvp))
    newvp))

(defsubst boxify-number (number)
  (make-virtual-copy :rows `(,(make-evrow-from-entry number))))

;;; Here is the generic function that others may use...

(defun virtual-copy (thing &key (top-level? nil) boxify-number?)
  (etypecase thing
    (virtual-copy (top-level-virtual-copy-virtual-copy thing top-level?))
    (virtual-port (top-level-virtual-copy-virtual-port thing top-level?))
    (box (top-level-virtual-copy-editor-box thing top-level?))
    (number (if boxify-number? (boxify-number thing)
		thing))
    (symbol thing)
    (foreign-data (virtual-copy-foreign-data thing))
    ;;these last 2 are here just in case
;    ((member 'bu::true 'bu::false) thing)
    ))



;;;; Virtual Copy Constructors

(defun make-empty-vc (&optional (type 'data-box))
  (make-virtual-copy :type type :rows `(,(make-empty-evrow))))



;;;; Pointer Manipulation
;;  Pointers come in 2 flavors, SINGLE and MULTIPLE. In the Beginning(i.e.
;;  upon conversion from an EDITOR Object, all pointers are SINGLE.  Mutating
;;  a pointer in an EVrow causes it to become a MULTIPLE pointer.  The same
;;  position in the row can refer to more than one thing as a function of WHO
;;  is asking.  The only legitimate way of getting at the value of a pointer
;;  is via GET-POINTER-VALUE.  BASH-SINGLE->MULTIPLE-POINTER is used to mutate
;;  a SINGLE pointer into a MULTIPOINTER with 1 item. Finally,
;;  APPEND-VALUE-TO-POINTER is used to add more Multi-Pointer-SLots to a
;;  Pointer.  Note that the TYPE field of the pointer actually bashed.  This
;;  is important because we need to preserve the EQness of the pointer
;;  for the Port Pathname Code to work.
;;
;;  We might want to remove the error checks in the final version if
;;  speed starts to become a problem

(defun get-pointer-value (pointer with-respect-to)
  (declare (values value exact?))
  (vc-check-pointer pointer)
  (cond ((eq (pointer-type pointer) 'single)
	 (record-single-ptr-ref)
	 (values (pointer-value-internal pointer) 'single))
	(t
	 (record-multi-ptr-ref
	  pointer
	  (disambiguate-multi-pointer (pointer-value-internal pointer)
				      with-respect-to)))))

(defun bash-single->multiple-pointer (sptr)
  (cond ((and (pointer? sptr) (eq (pointer-type sptr) 'single))
	 (rplaca  sptr 'multiple)
	 (rplacd sptr (list (%make-mps-from-original-item
			     (pointer-value-internal sptr)))))
	(t
	 (error "Trying to Bash an Object, ~S, which is not a SINGLE pointer ~
              into a Multi-Pointer. " sptr))))

(defun append-value-to-pointer (ptr who when item)
  (vc-check-pointer ptr)
  (trace-vc "Appending MP Value:" who when item)
  (when (eq (pointer-type ptr) 'single)
    (bash-single->multiple-pointer ptr))
  (setf (vc-modified? who) t)
  (push (%make-mps-from-mutation who when item) (pointer-value-internal ptr)))

;;; this is called when the multipointer gets too long and is known to
;;; be filled with bogus entries (at the end of an eval)
(defun scrunch-multipointer->single (ptr)
  (vc-check-pointer ptr)
  (cond ((single-pointer? ptr) ptr)
	(t (let ((first-mps (car (pointer-value-internal ptr))))
	     (setf (car ptr) 'single)
	     (setf (cdr ptr) (%mps-value first-mps))
	     ptr))))



;;;; Manipulating MultiPointers

;;;  DISAMBIGUATE-MULTI-POINTER is the heart of the virtual copy scheme
;;;  The algorithm is as follows:
;;
;;    Each MultiPointer slot (MPS) contain 3 things:
;;    1) a particular version of reality, 2) who made the change (note
;;    that "who" is either the superior box or a virtual copy of the
;;    superior box), and 3) when the change was made
;;
;;    We need to compare that against who (the QUERENT) is interested in
;;    the contents of the MultiPointer
;;
;;    We iterate through the list of MultiPointer slots looking for a MPS
;;    that was made by the QUERENT or an ancestor of the QUERENT THAT ALSO has
;;    a time-of-change that is OLDER than the QUERENT's time-of-creation.
;;    (Remember that the MPS's are ordered in time)
;;

;;; still need to think thru what happens when with-respect-to
;;; is NIL or an editor box

(defun disambiguate-multi-pointer (slots with-respect-to)
  (declare (values value exact?))
  (let ((pedigree (unless (or (null with-respect-to)
			      (box? with-respect-to))
		    (vc-pedigree with-respect-to))))
    (flet ((ancestor-modified? (mps) (fast-assq (%mps-who mps) pedigree))
	   (mps-older-than? (mps who-when) (<= (%mps-when mps)
					       (cdr who-when))))
      (trace-vc "Disambiguating MP:" slots)
      (dolist (slot slots (progn (trace-vc "Using Default Value:"
					   (%mps-value (car (last slots))))
				 (values (%mps-value (car (last slots)))
					 ':default)))
	;; If we come up empty, return the original value of the Multi-Pointer
	(when (eq with-respect-to (%mps-who slot))
	  ;; The QUERENT itself made the change so there is no
	  ;; need to compare times. We immediately RETURN the slot value
	  (trace-vc "Exact MP match:" (%mps-value slot) (%mps-who slot))
	  (return  (values (%mps-value slot) t)))
	;; Now we need to check the Ancestors of WITH-RESPECT-TO
	(let ((winning-ped (ancestor-modified? slot)))
	  (trace-vc "Checking Ancestors:" pedigree
		    (%mps-who slot) (%mps-when slot))
	  (when (and winning-ped (mps-older-than? slot winning-ped))
	    (trace-vc "Got Ancestor Match:" (%mps-value slot))
	    ;; An ancestor of WITH-RESPECT-TO is responsible for modifying
	    ;; the slot AND the time of modification is BEFORE WITH-RESPECT-TO
	    ;; was created so we win
	    (return (%mps-value slot))))))))

;;; useful for debugging

;; print out the ID's and MOD-times for each MP slot

(defun showmp (mp)
  (unless (single-pointer? mp)
    (dolist (mps (pointer-value-internal mp))
      (format t "~%~A(~A) by ~A at ~A"
	      (access-pointer-element (%mps-value mps))
              (%mps-value mps) (%mps-who mps) (%mps-when mps)))
    mp))



;;;; Ports

;; what about names ?
(defun port-to (thing)
  (cond ((and (eval::possible-eval-object? thing) (fast-eval-port-box? thing))
	 (make-virtual-port :target (vp-target thing)))
	;; will this ever happen?
	((box? thing)
	 ;; a hook to (possibly) fill the box from the server
	 (when (and (null (slot-value thing 'first-inferior-row))
		    (storage-chunk? thing))
	   (boxnet::fill-box-from-server thing))
	 (make-virtual-port :target (box-or-port-target thing)))
	((virtual-copy? thing)
	 (cond ((and (box? (vc-progenitor thing))
		     ;; progenitor's are held onto as long as possible, a
		     ;; creation-time of -2 means that the VC is masquerading
		     ;; as the editor box for the benefit of fast type
		     ;; checking in the evaluator
		     (= -2 (vc-creation-time thing)))
		(make-virtual-port :target (vc-progenitor thing)))
	       (t
		(setf (vc-port-target? thing) t)
		(make-virtual-port :target thing))))
        ((typep thing 'foreign-data) (port-to-foreign-data thing))
	(t
         ;; the default is to make a port to whatever the object is...
	 (make-virtual-port :target thing))))

;;; GET-PORT-TARGET and BOX-OR-PORT-TARGET are generic wrt
;;; editor/eval ports because the evaluator sees both.
(defun get-port-target (thing)
  (cond ((port-box? thing) (or (getprop thing 'retargetting-vc) (ports thing)))
	((fast-eval-port-box? thing)
	 (vp-target thing))
	(t (error "~S is not a Port" thing))))

(defun box-or-port-target (thing)
  (cond ((and (eval::possible-eval-object? thing) (fast-eval-port-box? thing))
	 (get-port-target thing))
	((port-box? thing)
	 (or (getprop thing 'retargetting-vc) (ports thing)))
	(t thing)))

(defun make-virtual-port-from-editor-port (port)
  (make-virtual-port :target (or (getprop port 'retargetting-vc) (ports port))
		     :editor-port-backpointer port))

#|
;; the old non-generic versions
(defun get-port-target (thing)
  (if (fast-port-box? thing)
      (vp-target thing)
      (error "~S is not an Evaluator Port" thing)))

(defun box-or-port-target (thing)
  (if (and (vectorp thing) (fast-port-box? thing))
      (get-port-target thing)
      thing))
|#
;;; If the inferior is already a virtual copy, then we return a port to it
;;; otherwise, we will have to virtual copy the inferior first in order to
;;; assure that multiple ports to the same inferior point to the same(EQ) thing

(defun port-to-item (ptr superior &optional keep-chunk)
  (let* ((item (access-evrow-element superior ptr t t))
	 (chunk-p (chunk-p item))
	 (value (if chunk-p (chunk-chunk item) item)))
    (flet ((new-chunk (new-value)
	     (if chunk-p
		 (let ((nc (copy-chunk item)))
		   (setf (chunk-chunk nc) new-value)
		   nc)
		 new-value)))
      (cond ((or (numberp value) (symbolp value))
	     item)
	    ((or (and (simple-vector-p value) (fast-eval-port-box? value))
		 (port-box? value))
	     (if keep-chunk
		 (new-chunk (virtual-copy value))
		 (virtual-copy value)))
	    (t
	     (if keep-chunk
		 (new-chunk (port-to value))
		 (port-to value)))))))
#|
(defun port-to-item (ptr superior &optional keep-chunk)
  (multiple-value-bind (item exact?)
      (get-pointer-value ptr superior)
    (let* ((chunk-p (fast-chunk? item))
	   (value (if chunk-p (chunk-chunk item) item)))
      (flet ((new-chunk (new-value)
	       (if chunk-p
		   (let ((nc (copy-chunk item)))
		     (setf (chunk-chunk nc) new-value)
		     nc)
		   new-value)))
	(cond ((or (numberp value) (symbolp value))
	       item)
	      ((or (fast-eval-port-box? value)
		   (port-box? value))
	       (if keep-chunk
		   (new-chunk (virtual-copy value))
		   (virtual-copy value)))
	      ((not (null exact?))
	       (if keep-chunk
		   (new-chunk (port-to value))
		   (port-to value)))
	      ((and (box? superior)
		    (box? value))
	       (if keep-chunk
		   (new-chunk (port-to value))
		   (port-to value)))
	      (t
	       ;; need to cons a unique target
	       (let* ((target (virtual-copy value))
		      (nc (new-chunk target)))
		 (extend-pointer ptr superior (now)
				 (if chunk-p nc target))
		 (if keep-chunk
		     (new-chunk (port-to target))
		     (port-to target)))))))))
|#



;;;; Evaluator Interface.

(defun LOOKUP-VARIABLE-IN-BOX-ONLY (box var &optional (exact-inferior-required? t))
  (if (box? box)
      (eval::lookup-static-variable-in-box-only box var)
      (lookup-variable-in-virtual-copy box var exact-inferior-required?)))

;;; symeval support

;; the basic idea is to loop through all of the inferiors of the box looking
;; for the name and then if we find it, cache it in case we are asked again.
;; If we dont find ANY names, then record that too so wee don't have to
;; loop through ALL of the inferiors again.
;;
;; If we DO find a name, then we have to VC it cause we may want to port to it
;; We can optimize this step out if we can KNOW at symeval time whether or
;; not we are actually going to have to port to it

(defvar *no-names-in-vc-marker* 'no-names-in-here-bub)

;;; The caller of this function can use the optional arg
;;; exact-inferior-required? if they know how the returned value will be
;;; flavorized.  Port-to flavored inputs required EXACT copies in order to
;;; maintain consistency between multiple ports to the same
;;; target.  On the other hand, if you know that you are just going to
;;; (virtual) copy the result, then any (equal) result will do and that can be
;;; potentially faster since it isn't obligated to CONS an exact inferior if
;;; none is available

;;; if the variable isn't present in the virtual copy, we should return nil.

(defun lookup-variable-in-virtual-copy (vc variable
					   &optional
					   (exact-inferior-required? t))
  (or (lookup-variable-in-virtual-copy-1 vc variable exact-inferior-required?)
      (when (eq (vc-name vc) variable) vc)
      (let ((sup (vc-superior-for-scoping vc)))
	(cond ((null sup) nil)
	      ((vc-rows-entry? sup)
	       (unless (null (vc-rows-entry-editor-box-backpointer sup))
		 (let ((superior-box
			(superior-box
			 (vc-rows-entry-editor-box-backpointer sup))))
		   (unless (null superior-box)
		     (let ((eval::*lexical-variables-root* superior-box))
		       (eval::static-variable-value
			(eval::lookup-static-variable superior-box
						      variable)))))))
	      ((virtual-copy? sup)
	       (lookup-variable-in-virtual-copy sup
						variable
						exact-inferior-required?))
	      (t
	       (cerror "Ignore the thing" "The thing, ~A, was not a virtual copy structure"
		       sup)
	       nil)))))


(defun lookup-variable-in-virtual-copy-1 (vc variable
					   &optional
					   (exact-inferior-required? t))
  (declare (ignore exact-inferior-required?)) ; no longer used
  (record-vc-var-lookup)
  (cond ((eq (vc-cached-binding-alist vc)
	     *no-names-in-vc-marker*)
	 (record-vc-var-lookup-cache-hit)
	 nil)
	(t
	 (when (null (vc-cached-binding-alist vc))
	   ;; looks like we have to fill the cache...
	   (record-vc-var-lookup-cache-fill)
	   ;; first, we loop through the box's inferiors
	   ;; punting on exports for the moment
	   (dolist (vr (vcis-rows vc))
	     (dolist (ptr (evrow-pointers vr))
	       (multiple-value-bind (item exact?)
		   (get-pointer-value ptr vc)
		 (let* ((chunk-p (chunk-p item))
			(val (if chunk-p
				 (chunk-chunk item)
				 item)))
		   (cond ((or (symbolp val) (numberp val)))
			 ((virtual-copy? val)
			  (let ((name (vc-name val)))
			    (cond ((null name))
				  (exact?
				   ;; box has a name so cache the name and the
				   ;; value unless the match is not exact.  We
				   ;; can only cache exact matches cause we may
				   ;; later want to Port-To
				   ;; the name that we find in the cache
				   (push (eval::make-static-variable name val)
					 (vc-cached-binding-alist vc)))
				  (t
				   ;; we dont have an exact
				   ;; match but we want one
				   (let* ((newval (virtual-copy val
								:top-level?
								t))
					  (svar (eval::make-static-variable
						 name newval)))
				     ;; might as well cache it
				     (push svar (vc-cached-binding-alist vc))
				     ;; put it into the row structure
				     (extend-pointer
				      ptr vc (vcis-creation-time vc)
				      (if chunk-p
					  (let ((nc (copy-chunk item)))
					    (setf (chunk-chunk nc) newval)
					    nc)
					  newval)
				      (evrow-original-row vr))
				     (setf (vcis-modified? vc) t)))))
                          ;; check exports
                          (unless (null (vc-exports val))
                            (cond ((eq (vc-exports val)
                                       eval::*exporting-all-variables-marker*)
                                   ;; fill cache here
                                   (setf (vc-cached-binding-alist vc)
                                         (append (vc-cached-binding-alist vc)
                                                 (vc-cached-binding-alist val))))
                                  ;; skeleton code for when we have finer grain
                                  ;; control over exports
                                  (t (dolist (binding (vc-exports val))
                                       (push binding (vc-cached-binding-alist vc))))))
                            )
			 ((virtual-port? val)
			  (let ((name (vp-name val)))
			    (cond ((null name))
				  (t
				   ;; box has a name so cache
				   ;; the name and the value
				   (let ((svar
					  (eval::make-static-variable name val)))
				     (push svar
					   (vc-cached-binding-alist
					    vc))))))
                          ;; transparent ports ?  Not Today !!
                          )
			 ((box? val)
			  (let ((name (editor-box-name-symbol val)))
			    (cond ((null name))
				  (t
				   ;; box has a name so cache the name and
				   ;; a virtual copy of the value.
				   (let* ((new-val (virtual-copy val
								 :top-level?
								 t))
					  (svar (eval::make-static-variable
						 name new-val)))
				     ;; instantiate the new value into
				     ;; the structure
				     (extend-pointer
				      ptr vc
				      (vcis-creation-time vc)
				      (if chunk-p
					  (let ((nc (copy-chunk item)))
					    (setf (chunk-chunk nc) new-val)
					    nc)
					  new-val)
				      (evrow-original-row vr))
				     (push svar
					   (vc-cached-binding-alist vc))
				     (setf (vcis-modified? vc) t)))))
                          ;; exports
                          (unless (null (exports val))
                            (cond ((eq (exports val)
                                       eval::*exporting-all-variables-marker*)
                                   ;; fill cache here
                                   (setf (vc-cached-binding-alist vc)
                                         (append (vc-cached-binding-alist vc)
                                                 (slot-value val 'static-variables-alist))))
                                  ;; skeleton code for when we have finer grain
                                  ;; control over exports
                                  (t (dolist (binding (exports val))
                                       (push binding (vc-cached-binding-alist vc))))))
                          ))))))
	   ;; check in the closet...
	   ;; Still need to think about whether it will be faster to
	   ;; go through the box's alist and then check to see if
	   ;; the result is in the closet or to iterate through the
	   ;; items in the closet
	   (let ((closet (vc-closets vc)))
	     (labels ((handle-row (row)
			(do-row-chas ((cha row))
			  (cond ((cha? cha))
				;; must be a box
				((not (null (exports cha)))
				 ;; a transparent box
				 (do-box-rows ((row cha))
				   (handle-row row)))
				;; must be a normal box
				(t
				 (let ((name (editor-box-name-symbol cha)))
				   (cond ((null name))
					 (t
					  ;; box has a name so cache the name and
					  ;; a virtual copy of the value.
					  (let* ((new-val (virtual-copy
							   cha :top-level?
							   t))
						 (svar
						  (eval::make-static-variable
						   name new-val)))
					    (push svar
						  (vc-cached-binding-alist vc))
					    (setf (vcis-modified? vc) t))))))))))
	       (unless (null closet)
		 (handle-row closet))))
	   ;; the cache is now as filled as it ever will be
	   ;;
	   ;; If there are no bindings pushed onto the cached binding list,
	   ;; then set the marker so we don't loop through the box again.
	   (when (null (vc-cached-binding-alist vc))
	     (setf (vc-cached-binding-alist vc)
		   *no-names-in-vc-marker*)
	     (return-from lookup-variable-in-virtual-copy-1 nil)))
	 ;; now check the cache
	 (when (consp (vc-cached-binding-alist vc))
	   (let ((val? (fast-assq variable (vc-cached-binding-alist vc))))
	     (cond ((null val?)
		    (record-vc-var-lookup-cache-miss)
		    nil)
		   (t
		    (record-vc-var-lookup-cache-hit)
		    (eval::static-variable-value val?))))))))





;;; This used to massage new items for mutators and constructors
;;; and for RUN although the arithmetic should be using these

;;; Doit Boxes should cached the parsed rows on their Plist and we
;;; should check for a cache Hit on the doit box here

;;; This returns a structured list of items.  The items no longer depend
;;; on their superior box anymore.  This means that they should be
;;; disambiguated and, if the superior was a virtual copy, any NON-exact
;;; matches should be virtual copied

;;; the $ 64K question is "do we need to bother caching the elements on VC's ?"
;;; let's not for now.  The only situation where a cache might win would be
;;; constant use of THE SAME COPY.  Right now, that loks like it might only
;;; happen (frequently and naturally) in some sort of object oriented type
;;; of programming where an instance gets created on the fly

(defun handle-pointer-for-unboxing (p superior
				      &optional
				      (unformatted? t) (make-ports? nil))
  (flet ((new-chunk (new-value raw-chunk)
           (let ((nc (copy-chunk raw-chunk)))
	     (setf (chunk-chunk nc) new-value)
	     nc)))
    (multiple-value-bind (raw-chunk exact?)
	(get-pointer-value p superior)
      (let* ((chunk-p (chunk-p raw-chunk))
	     (value (if chunk-p (chunk-chunk raw-chunk) raw-chunk)))
	(cond ((or (numberp value) (symbolp value)
		   (and (not (null exact?))
			(not (eq exact? 'single))
			(not (eq exact? ':default))))
	       (if unformatted? value raw-chunk))
	      ;; the item must be a box and is NOT an
	      ;; exact match so we have to copy it...
	      ((not (null make-ports?))
	       (port-to-item p superior (not unformatted?)))
	      (t (let ((copy (virtual-copy value :top-level? t)))
		   ;; set the creation time to be the same as the
		   ;; superior box (the default is NOW)
		   (setf (vc-creation-time copy) (vcis-creation-time superior))
		   (cond ((or (not (null unformatted?)) (not chunk-p))
			  copy)
			 (t (new-chunk copy raw-chunk))))))))))

(defun raw-unboxed-items (box)
  (if (numberp box)
      (list (list box))
      (let* ((sup (box-or-port-target box))
	     (inlinks? nil)
	     (rows (cond ((box? sup)
		          (multiple-value-bind (rows box-inlinks? new? rows-entry)
			      (virtual-copy-rows sup)
			    (declare (ignore new?))
			    (setq sup rows-entry)
			    (unless (port-box? box) (setq inlinks? box-inlinks?))
			    rows))
                         ((typep sup 'foreign-data) (virtual-copy-rows sup))
                         (t (vc-rows sup)))))
	(values (if (typep sup 'foreign-data)
                    rows
                    (mapcar #'(lambda (r)
			        (mapcar #'(lambda (p)
					    (handle-pointer-for-unboxing
					     p sup t (or (port-box? box)
						         (virtual-port? box))))
				        (evrow-pointers r)))
			    rows))
		(if (virtual-copy? box) (vc-inlinks? box) inlinks?)))))

(defun formatted-unboxed-items (box)
  (if (numberp box)
      (list (list box))
      (let* ((sup (box-or-port-target box))
	     (inlinks? nil)
	     (rows (cond ((box? sup)
		          (multiple-value-bind (rows box-inlinks? new? rows-entry)
			      (virtual-copy-rows sup)
			    (declare (ignore new?))
			    (setq sup rows-entry)
			    (unless (port-box? box) (setq inlinks? box-inlinks?))
			    rows))
                         ((typep sup 'foreign-data) (virtual-copy-rows sup))
                         (t (vc-rows sup)))))
	(values (if (typep sup 'foreign-data)
                    rows
                    (mapcar #'(lambda (r)
			    (mapcar #'(lambda (p)
					(handle-pointer-for-unboxing
					 p sup nil (or (port-box? box)
						       (virtual-port? box))))
				    (evrow-pointers r)))
			rows))
		(if (virtual-copy? box) (vc-inlinks? box) inlinks?)))))

;;; these next 2 are obsolete... should use unboxed-items instead

;; returns a list of lists of items. Also DISAMBIGUATES when neccesary
(defun structured-box-items (box)
  (cond ((and (virtual-copy? box) (vc-cached-elements box)))
	;; First, we check the Cache
	(t
	 (let ((elements (mapcar #'(lambda (row)
				     (mapcar #'(lambda (el)
						 (access-evrow-element box el))
					     (evrow-pointers row)))
				 (get-box-rows box))))
	   (when (virtual-copy? box)
	     (setf (vc-cached-elements box) elements))
	   elements))))

;; Returns a list of items
(defun flat-box-items (box)
  (cond ((numberp box) (list box))
        ((and (virtual-copy? box) (vc-cached-elements box))
	 ;; First, we check the Cache
	 (reduce #'append (vc-cached-elements box)))
	(t
	 (let ((elements (mapcar #'(lambda (row)
				     (mapcar #'(lambda (el)
						 (access-evrow-element box el))
					     (evrow-pointers row)))
				 (get-box-rows box))))
	   (when (virtual-copy? box)
	     (setf (vc-cached-elements box) elements))
	   (reduce #'append elements)))))

(defun get-rows-for-eval (box)
  (flet ((box-has-inputs-line? (rows)
	   ;; Is the first element of the first row an INPUTS marker ?
	   (member (caar rows) *symbols-for-input-line*)))
    (let ((raw-rows (raw-unboxed-items box)))
      (if (box-has-inputs-line? raw-rows)
	  (cdr raw-rows)
	  raw-rows))))



;;;; Things that count
;;   It might be useful in the future to cache some of these values.
;;   We need to do some more metering to decide what to cache

;;;  Counting by evaluator tokens

(defsubst evrow-length-in-elements (evrow)
  (length (evrow-pointers evrow)))

(defsubst number-of-rows (vc)
  (length (get-box-rows vc)))

(defun box-length-in-elements (box)
  (let ((sum 0))
    (dolist (r (get-box-rows box) sum)
      (setq sum (+ sum (evrow-length-in-elements r))))))

(defun max-length-in-elements (box)
  (let ((mlength 0))
    (dolist (r (get-box-rows box) mlength)
      (setq mlength (max mlength (evrow-length-in-elements r))))))

;;; Counting by characters
;;  The length of a row = (+ (+ (LENGTH LFP1) (LENGTH PNAME1))
;;                           (+ (LENGTH LFP2) (LENGTH PNAME2))
;;                           ...
;;                           (+ (LENGTH LFPn) (LENGTH PNAMEn) (LENGTH RFPn)))
;;
;;  The reason for emphasizing the LFP's is that most of the important objects
;;  of a CHUNK are to be found to the Left of the CHUNK.
;;

(defun evrow-length-in-chas (evrow superior)
  (let ((entries (evrow-pointers evrow)))
    (flet ((pname-length (pname)
	     ;; The Pname can be either an array or a box, Use ARRAYP
	     ;; cause it's bound to be faster than BOX?
	     (if (formatting-info? pname) (formatting-info-length pname) 1)))
      (cond ((not (null entries))
	     (let ((length 0))
	       (dolist (p entries length)
		 (let ((chunk (get-pointer-value p superior)))
		   (incf& length (+ (formatting-info-length
				     (chunk-left-format chunk))
				    (pname-length (chunk-pname chunk))))
		   (when (eq p (car (last entries)))
		     (incf& length (formatting-info-length
				    (chunk-right-format chunk))))))))
	    ((null (evrow-row-format evrow))
	     (error "Tried to find the Length of an EVROW without ENTRIES of FORMATTING."))
	    (t (formatting-info-length (evrow-row-format evrow)))))))

;; This is used by JOIN-RIGHT among other things to set up the
;; correct number of spaces
(defun box-max-width-in-chas (box)
  (let ((mwidth 0))
    (dolist (r (get-box-rows box) mwidth)
      (setq mwidth (max mwidth (evrow-length-in-chas r box))))))



;;;; Stringifying
;;   These routines are mainly for passing strings to various lisp utilities
;;   that we need to Interface with.  An example is the file system which
;;   calls FS:PARSE-PATHNAME (or to be more Common Lispy, #P) on a string.
;;   Boxes appear in the string as "[]" As in the counting routines, we
;;   accumalate left formatting properties and Pnames until we reach the end
;;   at which time we add on the right formatting property.

(defun evrow-text-string (row superior
			      &optional (return-string
					 (make-array 32
						     :element-type 'standard-char
						     :fill-pointer 0
						     :adjustable t))
			      (start-box-cha #\[) (stop-box-cha #\]))

  (let ((entries (evrow-pointers row)))
    (flet ((chunk-handler (chunk space? &optional last?)
             (when (and space? (formatting-info? (chunk-pname chunk)))
               ;; if we (maybe) need a space and the chunk is not a box...
               (let ((1st (or (first-fi-cha (chunk-left-format chunk)) ; null if empty
                              (first-fi-cha (chunk-pname chunk)))))
                 (unless (and 1st (member 1st '(#\space #\@ #\!) :test #'char=))
                   (vector-push-extend #\space return-string))))
             (do-fi-chas (cha (chunk-left-format chunk))
	       (vector-push-extend cha return-string))
	     (if (formatting-info? (chunk-pname chunk))
		 (do-fi-chas (cha (chunk-pname chunk)) (vector-push-extend
							cha return-string))
		 (progn (vector-push-extend start-box-cha return-string)
			(vector-push-extend stop-box-cha  return-string)))
	     (when last?
	       (do-fi-chas (cha (chunk-right-format chunk))
		 (vector-push-extend cha return-string))))
	   (value-handler (value space?)
	     (cond ((or (symbolp value) (stringp value) (numberp value))
		    (let ((val-string (format nil (if space? " ~A" "~A") value)))
		      (declare (simple-string val-string))
		      (dotimes (i (length val-string))
			(vector-push-extend (char val-string i) return-string))))
		   (t
		    ;(when space? (vector-push-extend #\space return-string))
                    ; no space insert if value is a box !
		    (vector-push-extend start-box-cha return-string)
		    (vector-push-extend stop-box-cha  return-string)))))
      (cond ((not (null entries))
	     (do* ((ptrs entries (cdr ptrs))
		   (p (car ptrs) (car ptrs))
                   (last-ptr-was-box? nil)
                   (already-space?
                    t
                    ;; is the last char some sort of whitespace ?
                    (member (char return-string (1-& (length return-string)))
                            '(#\space #\newline #\tab)
                            :test #'char=)))
		  ((null p))
	       (let ((chunk (get-pointer-value p superior)))
		 (cond ((chunk-p chunk)
                        (prog1 ; want the last-ptr-was-box? tested before setting it
                          (chunk-handler chunk (and (not already-space?)
                                                    (not last-ptr-was-box?))
                                         (null (cdr ptrs)))
                          (setq last-ptr-was-box?
                                (not (formatting-info? (chunk-pname chunk))))))
		       (t
                        (prog1
                          (value-handler chunk (and (not already-space?)
                                                    (not last-ptr-was-box?)))
                          (setq last-ptr-was-box?
                                (not (or (symbolp chunk)
                                         (stringp chunk) (numberp chunk))))))))))
	    ((null (evrow-row-format row)))
	    (t (let ((format-chunk (evrow-row-format row)))
		 (when (formatting-info? (chunk-left-format format-chunk))
		   (do-fi-chas (cha (chunk-left-format format-chunk))
		     (vector-push-extend cha return-string)))
		 (when (formatting-info? (chunk-pname format-chunk))
		   (do-fi-chas (cha (chunk-pname format-chunk))
		     (vector-push-extend cha return-string)))
		 (when (formatting-info? (chunk-right-format format-chunk))
		   (do-fi-chas (cha (chunk-right-format format-chunk))
		     (vector-push-extend cha return-string))))))))
  return-string)

(defun box-text-string (box)
  (cond ((numberp box) (convert-number-to-string box))
	(t
	 (let ((return-string (make-array 64 :element-type 'standard-char
					  :fill-pointer 0 :adjustable t)))
	   (multiple-value-bind (boxrows inlinks? new? vc-rows-entry)
	       (get-box-rows box)
	     (declare (ignore inlinks? new?))
	     (do* ((rows boxrows (cdr rows))
		   (r (car rows) (car rows)))
		  ((null rows) return-string)
	       (evrow-text-string r (or vc-rows-entry box) return-string)
	       (unless (null (cdr rows))
		 (vector-push-extend #\newline return-string))))))))

#| ;; too much CONSing !!!
(defun evrow-text-string (row superior)
  (let ((entries (evrow-pointers row)))
    (flet ((chunk-string (chunk &optional last?)
	     (if (null last?)
		 (concatenate 'string
			      (formatting-info-string (chunk-left-format
						       chunk))
			      (if (formatting-info? (chunk-pname chunk))
				  (formatting-info-string (chunk-pname chunk))
				  "[]"))
		 (concatenate 'string
			      (formatting-info-string (chunk-left-format
						       chunk))
			      (if (formatting-info? (chunk-pname chunk))
				  (formatting-info-string (chunk-pname chunk))
				  "[]")
			      (formatting-info-string
			       (chunk-right-format chunk)))))
	   (value-string (value space?)
	     (cond ((or (symbolp value) (stringp value) (numberp value))
		    (format nil (if space? " ~A" "~A") value))
		   (space? " []")
		   (t "[]"))))
      (cond ((not (null entries))
	     (let ((return-string (make-string 0)))
	       (dolist (p entries return-string)
		 (let ((chunk (get-pointer-value p superior)))
		   (setq return-string
			 (concatenate
			  'string
			  return-string
			  (cond ((chunk-p chunk)
				 (chunk-string chunk
					       (eq p (car (last entries)))))
				((and (not (zerop& (length return-string)))
				      (not (char= (char return-string
							(1-& (length
							      return-string)))
						  #\space)))
				 ;; add a space if there isn't already
				 ;; one between items
				 (value-string chunk t))
				(t (value-string chunk nil)))))))))
	    ((null (evrow-row-format row))
	     "")
	    (t (let ((format-chunk (evrow-row-format row))
		     (return-string (make-string 0)))
		 (when (formatting-info? (chunk-left-format format-chunk))
		   (setq return-string
			 (concatenate 'string
				      return-string
				      (formatting-info-string
				       (chunk-left-format format-chunk)))))
		 (when (formatting-info? (chunk-pname format-chunk))
		   (setq return-string
			 (concatenate 'string
				      return-string
				      (formatting-info-string
				       (chunk-pname format-chunk)))))
		 (when (formatting-info? (chunk-right-format format-chunk))
		   (setq return-string
			 (concatenate 'string
				      return-string
				      (formatting-info-string
				       (chunk-right-format format-chunk)))))
		 return-string))))))

(defun box-text-string (box)
  (cond ((numberp box) (format nil "~A" box))
	(t
	 (let ((return-string (make-string 0)))
	   (dolist (r (get-box-rows box) (string-right-trim '(#\newline)
							    return-string))
	     (setq return-string (concatenate 'string
					      return-string
					      (evrow-text-string r box)
					      (make-string 1
							   :initial-element
							   #\newline))))))))

|#



;;;; Data Manipulator Support for Manipulating Row Elements
;;   These functions manipulate items in EVROWs returning lists of items
;;   which are then run through another function which does the appropriate
;;   porting or virtual-copying
;;   to the items before they are wrapped up into a new EVROW.
;;   Where it matters, the functions are 0 based
;;   There is NO RANGE checking on the numbers

;;; Single Element Selectors
(defsubst get-nth-element-in-evrow (evrow n)
  (nth n  (evrow-pointers evrow)))

(defsubst get-first-element-in-evrow (evrow)
  (car (evrow-pointers evrow)))

(defsubst get-last-element-in-evrow (evrow)
  (car (last (evrow-pointers evrow))))

;;; All-But-One element Selectors

(defun get-butnth-element-in-evrow (evrow n)
  (let ((entries (evrow-pointers evrow)))
    (append (subseq entries 0 n)
	    (subseq entries (1+ n)))))

(defsubst get-butfirst-element-in-evrow (evrow)
  (cdr (evrow-pointers evrow)))

(defsubst get-butlast-element-in-evrow (evrow)
  (butlast (evrow-pointers evrow)))

;;; Range Manipulators
(defun get-elements-in-evrow (evrow start &optional end)
  (let ((ptrs (evrow-pointers evrow)))
      (if (null end)
	  (subseq ptrs start)
	  (subseq ptrs start (min& end (evrow-length-in-elements evrow))))))



;;; Row Mutator Support
;;  DELETE--these just expand into the relevant selectors

(defsubst delete-nth-element-in-evrow (evrow n)
  (get-butnth-element-in-evrow evrow n))

(defsubst delete-first-element-in-evrow (evrow)
  (get-butfirst-element-in-evrow evrow))

(defsubst delete-last-element-in-evrow (evrow)
  (get-butlast-element-in-evrow evrow))

(defun delete-elements-in-evrow (evrow start &optional end)
  (let ((ptrs (evrow-pointers evrow)))
    (if (null end)
	(subseq ptrs 0 start)
	(nconc (subseq ptrs 0 start)
	       (unless (> end (evrow-length-in-elements evrow))
		 (subseq ptrs end))))))

;;  INSERT

(defun insert-nth-element-into-evrow (evrow n new-element)
  (with-type-checking (evrow evrow new-element pointer)
    (let ((entries (evrow-pointers evrow)))
      (nconc (subseq entries 0 n)
	     (list new-element)
	     (subseq entries n)))))

(defun insert-first-element-into-evrow (evrow new-element)
  (with-type-checking (evrow evrow new-element pointer)
    (list* new-element (evrow-pointers evrow))))

(defun insert-last-element-into-evrow (evrow new-element)
  (with-type-checking (evrow evrow new-element pointer)
    (append (evrow-pointers evrow) (list new-element))))

;;  CHANGE--These are for new boxes, port-flavored versions are handled by the
;;  calling functions

(defun change-nth-element-in-evrow (evrow n new-element)
  (with-type-checking (evrow evrow new-element pointer)
    (let ((entries (evrow-pointers evrow)))
      (nconc (subseq entries 0 n)
	     (list* new-element (subseq entries (1+ n)))))))

(defun change-first-element-in-evrow (evrow new-element)
  (with-type-checking (evrow evrow new-element pointer)
    (list* new-element (cdr (evrow-pointers evrow)))))

(defun change-last-element-in-evrow (evrow new-element)
  (with-type-checking (evrow evrow new-element pointer)
    (append (butlast (evrow-pointers evrow)) (list new-element))))



;;; Row constructor support
;;  returns a list of items to be bundled up into a new row by the
;;  EVROW-CONSTRUCTOR.  note that All MP's must be disambiguated since the
;;  new row will NOT be in the same box as the original row and since multiple
;;  rows will be joined in some fashion, we won't be
;;  able to extend the pedigree due to the mutiple parents.
;;  if the row is free of MP's, then we can avoid consing up a new evrow
;;  We don't have to handle PORTing to inferiors here cause the
;;  EVROW-CONSTRUCTOR does that.
;;  This has to handle passing the PROGENITOR and OUTLINKS...

;; we can eliminate the consing here is are willing to make 2 passes
;; over the row
(defun assure-unique-evrow-items (evrow old-box)
  (let ((mps? nil))
    (flet ((item-op (item)
	     (cond ((multi-pointer? item)
		    (setq mps? t)
		    (make-pointer (get-pointer-value item old-box)))
		   (t item))))
      (let ((items (mapcar #'item-op (evrow-pointers evrow))))
	(if (null mps?)
	    evrow
	    (make-evrow :pointers items))))))



;;;; Data Manipulator Support for Manipulating Rows in Boxes
;;; The extra value is used by mutators to extend multipointers

(defun get-nth-row-in-box (n box)
  (declare (values row vc-rows-entry))
  (multiple-value-bind (rows inlinks? new? vc-rows-entry)
      (get-box-rows box)
    (declare (ignore inlinks? new?))
    (values (nth n rows) vc-rows-entry)))

(defun get-first-row-in-box (box)
  (declare (values row vc-rows-entry))
  (multiple-value-bind (rows inlinks? new? vc-rows-entry)
      (get-box-rows box)
    (declare (ignore inlinks? new?))
    (values (car rows) vc-rows-entry)))

;; AppGen Lossage for complex version of defsetf
#+mcl
(defun %set-first-row-in-box (box-or-port new-row)
  (let ((box (box-or-port-target box-or-port t)))
    (etypecase box
      (virtual-copy (setf (car (vc-rows box)) new-row))
      (box (change-virtual-copy-rows box (append (list new-row)
                                                 (cdr (vc-rows box)))))))
  new-row)

#+mcl
(defsetf get-first-row-in-box %set-first-row-in-box)

#-mcl
(defsetf get-first-row-in-box (box-or-port) (new-row)
  `(let ((box (box-or-port-target ,box-or-port t)))
     (etypecase box
       (virtual-copy (setf (car (vc-rows box)) ,new-row))
       (box (change-virtual-copy-rows box (append (list ,new-row)
						  (cdr (vc-rows box))))))))

#+mcl
(defun %set-nth-row-in-box (box-or-port n new-row)
  (let ((box (box-or-port-target box-or-port t)))
    (etypecase box
      (virtual-copy (setf (nth n (vc-rows box)) new-row))
      (box (change-virtual-copy-rows box (append (subseq (vc-rows box) 0 n)
					         (list new-row)
					         (subseq (vc-rows box)
						         (1+ n)))))))
  new-row)

#+mcl
(defsetf get-nth-row-in-box %set-nth-row-in-box)

#-mcl
(defsetf get-nth-row-in-box (box-or-port n) (new-row)
  `(let ((box (box-or-port-target ,box-or-port t)))
     (etypecase box
       (virtual-copy (setf (nth ,n (vc-rows box)) ,new-row))
       (box (change-virtual-copy-rows box (append (subseq (vc-rows box) 0 ,n)
					      (list ,new-row)
					      (subseq (vc-rows box)
						      (1+ ,n))))))))

(defun first-non-empty-row (box)
  (declare (values row vc-rows-entry row-no))
  (multiple-value-bind (box-rows inlinks? new? vc-rows-entry)
      (get-box-rows box)
    (declare (ignore inlinks? new?))
    (do* ((rows box-rows (cdr rows))
	  (row (car rows) (car rows)) (n 0 (1+& n)))
	 ((null rows))
      (unless (empty-evrow? row)
	(return (values row vc-rows-entry n))))))

(defun last-non-empty-row (box)
  (declare (values row vc-rows-entry row-no))
  (multiple-value-bind (box-rows inlinks? new? vc-rows-entry)
      (get-box-rows box)
    (declare (ignore inlinks? new?))
    (do* ((rows box-rows (butlast rows))
	  (row (car (last rows)) (car (last rows)))
	  (n (1- (length rows)) (1- n)))
	 ((null rows))
      (unless (empty-evrow? row)
	(return (values row vc-rows-entry n))))))



;;; Row Mutators
;;  DELETE, INSERT, CHANGE.
;;  These MODIFY

;;  These CREATE and return a new set of rows.  Note that in most cases, we
;;  are merely CONSing up a new top level list.  Many of the rows in the
;;  list will be the same

(defun delete-first-row-in-box (box)
  (cdr (fast-box-rows box)))

(defun delete-last-row-in-box (box)
  (butlast (fast-box-rows box)))

(defun delete-nth-row-in-box (n box)
  (let ((rows (fast-box-rows box)))
    (nconc (subseq rows 0 n) (subseq rows (1+ n)))))


(defun change-nth-row-in-box (n box new-row)
  (with-type-checking (new-row evrow)
    (let ((rows (fast-box-rows box)))
      (nconc (subseq rows 0 n) (list new-row) (subseq rows (1+ n))))))

(defun insert-nth-row-in-box (n box new-row)
  (with-type-checking (new-row evrow)
    (let ((rows (fast-box-rows box)))
      (nconc (subseq rows 0 n) (list new-row) (subseq rows n)))))


#|
;seems to be unused as of 4/28/92

;;;; How to Make new Evrows
;; EVROW-CONSTRUCTOR is the established way of making new EVROWS used by the
;; data manipulators. If we are making an EVROW for use in a constructor e.g.
;; JOIN-RIGHT then All MP's must be disambiguated since the new row will NOT
;; be in the a box which is ONLY a direct ancestor of the original row because
;; multiple rows will be joined in some fashion which means we won't be able
;; to just extend the pedigree due to the mutiple parents

;; the thing arg should either be a evrow entry or a list of entries
;; pointer should be disambiguted by this point
;; Note that we pay a cost for CONSing new Single pointers

(defun evrow-constructor (thing box)
  (flet ((port-item-operator (item)
	   (let ((val (get-pointer-value item box)))
	     (if (box? val)	   ; should we be porting to numbers here ?
		 (port-to-item item box)
		 item)))
	 (copy-item-operator (item)
	   ;; we may have to copy here
	   item)
	 (thing-discriminator (item-op)
	   (cond ((evrow? thing)
		  (make-evrow :pointers (mapcar item-op (evrow-pointers thing))
			      :row-format (evrow-row-format thing)))
		 ((listp thing)
		  (make-evrow-from-entries (mapcar item-op thing)))
		 ((pointer? thing)
		  (make-evrow-from-pointer  (funcall item-op thing)))
		 (t (error "~S was not an EVROW, LIST or POINTER. " thing)))))
    (etypecase box
      (virtual-copy (thing-discriminator #'copy-item-operator))
      (virtual-port (thing-discriminator #'port-item-operator)))))

|#

;;;; Box Operations
;; These perform data manipulations on Boxes.  These functions are responsible
;; for munging the proper rows and calling the appropriate row massaging
;; functions and then returning whatever is appropriate (that is, they are
;; supposed to do the right thing)

;;; Coordinate conversion
(defun get-row-and-col-number (n box)
  "Converts 1-based GET-NTH coordinates into 0-based GET-RC coordinates.
Values returned are row number and column number.  Returns NIL if there
aren't enough elements, 2nd value in the NIL case will be length in elements"
  (declare (values row-no index))
  (unless (<=& n 0)
    (let ((index (1- n)) (howfar 0))
      (do* ((rows (get-box-rows box) (cdr rows))
	    (row (car rows) (car rows))
	    (row-no 0 (1+ row-no)))
	   ((null rows) (values nil howfar))
	(let ((length (evrow-length-in-elements row)))
	  (incf& howfar length)
	  (if (< index length) (return (values row-no index))
	      (setq index (- index length))))))))

(defun good-rc-coords? (r c box)
  (let* ((rows (get-box-rows box)) (row (nth r rows)))
    (unless (null row) (<& -1 c (evrow-length-in-elements row)))))

;;; gets the whitespace out (you try scrubbing them out....)
(defun trim-empty-rows (list-of-rows)
  (or (remove-if #'empty-evrow? list-of-rows) (list (make-empty-evrow))))


;;; Random Support
(defun empty-box? (thing)
  (dolist (row (get-box-rows thing) t)
    (when (not (empty-evrow? row)) (return nil))))

(defun vc-pedigree-for-copy (thing)
  (cond ((virtual-port? thing)
	 ;; we will be making a brand new box to stuff all the sub-ports into
	 nil)
        ((vc-modified? thing) ; what about (vc-inlinks? thing))
	 (acons thing (now) ; changed from (vc-creation-time thing)
		(vc-pedigree thing)))
	(t (vc-pedigree thing))))

(defun get-progenitor-for-new-vc (thing)
  (when (virtual-copy? thing)
    (vc-progenitor thing)))

(defun make-vc (rows &optional (type 'data-box) name)
  (setq rows (mapcar #'(lambda (x) (cond ((evrow? x) x)
					 ((null   x) (make-empty-evrow))
					 ((listp  x) (make-evrow-from-entries x))
					 (t (make-evrow-from-entry x))))
		     rows))
  (make-virtual-copy :type type
		     :creation-time (now)
		     :rows (or rows (list (make-empty-evrow)))
		     :name name))

;;  Make a Virtual copy with a different row structure than the previous one
;;  but preserving everything else.  Selectors do this all the time.
(defun new-vc-rows (old-vc new-rows)
  (let ((newvc (%copy-vc old-vc)))
    (setf (vc-progenitor newvc) nil)
    (when (vc-modified? old-vc)
      (setf (vc-pedigree newvc)
	    (cons (cons old-vc (now))
		  (vc-pedigree old-vc))))
    (setf (vc-rows newvc) new-rows)
    newvc))

;; now that selectors can be passed editor boxes (as a result of
;; the eval::dont-copy flavor), we have to handle editor boxes
;; otherwise, this is pretty much the same as new-vc-rows

;; this needs to get info from the vc-rows-entry !!!!
(defun new-vc-rows-from-editor-box (ed-box new-rows)
  (let ((newvc (make-virtual-copy :type (class-name (class-of ed-box))
;				  :name (editor-box-name-symbol ed-box)
				  :pedigree (list `(,ed-box . ,(now))))))
    (setf (vc-rows newvc) new-rows)
    newvc))






;;;; Metering and Monitoring and Testing

;; Mainly for Debugging
(DEFUN SHOW-ROWS (THING &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  (LET* ((BOX (BOX-OR-PORT-TARGET THING))
	(ROWS (GET-BOX-ROWS BOX)))
    (FLET ((SHOW-ROW (ROW)
	     (DOLIST (ITEM (EVROW-pointers ROW))
	       (let ((item (get-pointer-value item box)))
		 (cond ((virtual-copy? item)
			(show-rows item stream))
		       (t
			(FORMAT STREAM " ~A " item)))))))
      (DOLIST (R ROWS)
	(TERPRI STREAM)
	(SHOW-ROW R)))))

#|

;;; Ye Olde Testing functions
(DEFVAR *ITEM-ACCESSORS* '(START BUTSTART %LAST %BUTLAST ITEM BUTITEM ROW-COL-OF-BOX))
(DEFVAR *ROW-ACCESSORS* '(FIRST-ROW LAST-ROW BUTFIRST-ROW BUTLAST-ROW NTH-ROW BUTNTH-ROW))
(DEFVAR *COL-ACCESSORS* '(FIRST-COLUMN BUTFIRST-COLUMN LAST-COLUMN BUTLAST-COLUMN
				       NTH-COLUMN BUTNTH-COLUMN))

(DEFUN TEST-FUNS (FUNS ON &OPTIONAL (NUMBER 1) (STREAM *STANDARD-OUTPUT*))
  (SETQ *CURRENT-EVALUATION-START-TIME* (TICK))
  (FORMAT STREAM "~%The object is : ")
  (SHOW-ROWS ON STREAM)
  (TERPRI STREAM)
  (DOLIST (F FUNS)
    (TERPRI STREAM)
    (FORMAT STREAM "~A ~A on ~A results in:" F
	                                     (CASE (LENGTH (ARGLIST F))
					       (1 "")
					       (2 (FORMAT NIL "with an arg of ~D" NUMBER))
					       (3 (FORMAT NIL "with args of ~D, ~D"
							  NUMBER NUMBER)))
					     ON)
    (SHOW-ROWS
      (COND ((= 2 (LENGTH (ARGLIST F)))
	     (FUNCALL F NUMBER ON))
	    ((= 3 (LENGTH (ARGLIST F)))
	     (FUNCALL F NUMBER NUMBER ON))
	    ((= 1 (LENGTH (ARGLIST F)))
	     (FUNCALL F ON))
	    (T (ERROR "don't know how to test ~A" F)))
      STREAM)))

(DEFUN TEST-MUTS (FUNS ON &OPTIONAL (N 1) (S *STANDARD-OUTPUT*))
  (SETQ *CURRENT-EVALUATION-START-TIME* (TICK))
  (FORMAT S "~%The object is: ")
  (SHOW-ROWS ON S)
  (TERPRI S)
  (DOLIST (F FUNS)
    (LET ((OBJ (VIRTUAL-COPY ON)))		;need to make a new one for each mutator
      (TERPRI S)
      (FORMAT S "~A ~A on ~A results in:" F
	      (CASE (LENGTH (ARGLIST F))
		(1 "")
		(2 (FORMAT NIL "with an arg of ~D" N))
		(3 (FORMAT NIL "with args of ~D, ~D"N N)))
	      OBJ)
      (SHOW-ROWS
	(COND ((= 2 (LENGTH (ARGLIST F)))
	       (FUNCALL F N OBJ))
	      ((= 3 (LENGTH (ARGLIST F)))
	       (FUNCALL F N N OBJ))
	      ((= 1 (LENGTH (ARGLIST F)))
	       (FUNCALL F OBJ))
	      (T (ERROR "don't know how to test ~A" F)))
	S)
      (FORMAT S "~% for copy flavored inputs and changes the original object to...~%")
      (COND ((= 2 (LENGTH (ARGLIST F)))
	     (FUNCALL F N (PORT-TO OBJ)))
	    ((= 3 (LENGTH (ARGLIST F)))
	     (FUNCALL F N N (PORT-TO OBJ)))
	    ((= 1 (LENGTH (ARGLIST F)))
	     (FUNCALL F (PORT-TO OBJ)))
	    (T (ERROR "don't know how to test ~A" F)))
      (SHOW-ROWS OBJ S)
      (FORMAT S "for port flavored objects~%"))))

(DEFSTRUCT MONITORING-STUFF
  (NUMBER 0)
  (ANALYZER NIL))

(DEFVAR *MONITORING-VARS* NIL)

(DEFMACRO DEFINE-MONITOR-RESET-FUNCTION (NAME) )

(DEFMACRO MONITOR-FUNCTION (FUNCTION)
  `(UNLESS (BOUNDP ',(INTERN (symbol-format nil "*~A-MONITOR-VAR*" FUNCTION)))
     (DEFVAR ,(INTERN (symbol-format nil "*~A-MONITOR-VAR*" FUNCTION)) 0)
     (PUSH ',(INTERN (symbol-format nil "*~A-MONITOR-VAR*" FUNCTION)) *MONITORING-VARS*)
     (DEFUN ,(INTERN (symbol-format nil "*RESET-~A-MONITOR-VAR" FUNCTION)) ()
       (SETQ ,(INTERN (symbol-format nil "*~A-MONITOR-VAR*" FUNCTION)) NIL))
     (ADVISE FUNCTION :AFTER NIL NIL
	     (SETQ ,(INTERN (symbol-format nil "*~A-MONITOR-VAR*" FUNCTION))
		   (1+ ,(INTERN (symbol-format nil "*~A-MONITOR-VAR*" FUNCTION)))))))

(MONITOR-FUNCTION VIRTUAL-COPY)

|#
