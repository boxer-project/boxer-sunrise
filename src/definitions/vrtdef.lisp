;;-*- Mode:Lisp; Syntax: Common-Lisp; Base: 10.;package: (Boxer :use (CL) :nicknames (box))-*-


#|


 $Header: vrtdef.lisp,v 1.0 90/01/24 22:19:38 boxer Exp $

 $Log:	vrtdef.lisp,v $
;;;Revision 1.0  90/01/24  22:19:38  boxer
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


 This file contains Definitions for the virtual copy mechanism

changes that need to be made to vanilla boxer:
1) simple-row-streams
2) :MODIFIED message needs to check on the value of *EVAL-IN-PROGRESS*


Modification History (most recent at top)

 9/24/11 fast-eval-sprite-box? warns of obsolescence during compilation
         fast-vc-has-sprite? added as new way to check for spriteness
12/10/09 process-editor-mutation-queue-within-eval no longer clears
         *editor-objects-to-be-modified*. Some important mods were being missed
 2/10/03 merged current LW and MCL files
 2/17/01 merged current LW and MCL files
10/05/99 added font-info slot to formatting-info structure
         *fonts-in-formatting-info* controls whether to use it
 6/10/99 added extra debugging info to record-{top-level/vc}-copy
10/28/98 Added #+lispworks changes for PC port
10/28/98 started logging changes: source = boxer version 2.3beta+


|#

(in-package :boxer)

;;; provide low level error checking that can be removed at compile time by
;;; setting this variable to NIL and recompiling

(defvar *virtual-copy-debugging-option-on?* t)

(defvar *vc-debugging?* nil)

(eval-when
    (:compile-toplevel :load-toplevel :execute)
  (defvar *vc-debug-block-size* 16
    "Should be larger than the largest number of variables in any TRACE-VC")

  (defvar *vc-debug-block* nil)
)

(defmacro init-vc-debug-block ()
  `(progn
     (setq *vc-debug-block* (make-array *vc-debug-block-size*))
     ,@(with-collection
    (dotimes (i *vc-debug-block-size*)
      (let ((name (intern (symbol-format nil "VC-DBG-~D" i))))
        (collect
         `(defun ,name ()
      (svref *vc-debug-block* ,i))))))))

(defun fill-vc-debug-block (&rest args)
  (do* ((i 0 (1+ i))
  (restargs args (cdr restargs))
  (arg (car restargs) (car restargs)))
       ((null restargs))
    (setf (svref *vc-debug-block* i) arg)))

(defun vc-debug (&optional single-step?)
  (setq *vc-debugging?*
  (if (null single-step?) t :single-step)))

(defun vc-nodebug () (setq *vc-debugging?* nil))

(defmacro vc-debugging (format-string &rest args)
  (when (not (null *virtual-copy-debugging-option-on?*))
    `(when (not (null *vc-debugging?*))
       (format *trace-output* ,format-string ,@args))))

(eval-when (:load-toplevel :execute)
  (unless (null *virtual-copy-debugging-option-on?*)
    (init-vc-debug-block))
  )

(defvar *vc-debug-break-char* #\^B)

;; this is like the Trace-Entering macro that the evaluator
;; uses except simpler and it looks at different variables
(defmacro trace-vc (place &rest args)
  (when *virtual-copy-debugging-option-on?*
    `(progn
       (when *vc-debugging?*
   (format t "~%~A " ',place)
   ,@(with-collection
         (dolist (arg args)
     (collect `(format t "~S ~S, " ',arg ,arg))))
   (when (eq *vc-debugging?* :single-step)
     (do () ((not (char-equal (read-char) *vc-debug-break-char*)))
       ;; fill the debug-block if it exists
       (unless (null *vc-debug-block*)
         (fill-vc-debug-block ,@args))
       (break)))))))


;; this is for use in a COND clause as in
;; (cond ((foo? x) (do stuff))
;;       (virtual-copy-error-clause (bar? x) "VC error, failed the bar? test")
;;       (t (do something else)))
(defmacro virtual-copy-error-clause (predicate-form reason . reason-args)
  (unless (null *virtual-copy-debugging-option-on?*)
    `(,predicate-form (error ,reason . ,reason-args))))

;;;; Some Object Definitions
;;  The lowest level of Objects is the CHUNK.  These are what the CHUNKER
;;  returns as components of a ROW.  CHUNK's are contained by POINTERS which
;;  can either be a SINGLEton pointer or a MULTIPLE pointer.  POINTERS are
;;  contained in EVROWS which are components of VIRTUAL-COPY's
;;
;;  CHUNK's and POINTER's are excellent candidates for speed bumming.
;;  we want to be able to make CHUNK-CHUNK as fast as possible and we want to
;;  be able to optimize storage for CHUNKs in the common case where they
;;  don't posess all of their possible attributes
;;
;;  The functions that handle POINTERs treat them as typed pointers. So if
;;   we can get access to the low level typing mechanism, then we can
;;   potentially win by saving all the list destructuring in getting the
;;   pointer value of SINGLE pointers and by bumming the Disambiguate routines
;;   for MULTIPLE pointers.
;;



;;; Maintaining Formatting Information

;; at CHUNK time, each chunk in a row will have a left-formatting and a
;; right-formatting property.  These will be a pointer to a vector
;; representation for the row along with offsets into the  vector.  We need
;; to CONs the vector rather than keeping pointers to a ROW's CHAS-ARRAY
;; because it is possible for a ROW to be modified in the middle of an
;; Evaluation which would invalidate any pointers into the CHAS-ARRAY
;;
;; we may be able to hold on to the vector representation in between
;; top level calls to eval.  Currently, they are consed each time.
;; the place to fix it is in the formatting-array method(s) in
;; simple-stream.lisp
;;
;; We also make an optimization for generic whitespace.  A number
;; in a position where a formatting-info is expected should be treated
;; as a formatting-info with that many spaces in it.  This avoids having
;; to CONS a formatting-info for the frequent simple cases of whitespace
;; All functions that deal with formatting-info's should be prepared to
;; accept numbers and handle them correctly.  Outside of this file, it
;; will usually be the case that they interface via do-fi-chas where it
;; is correctly handled
;;
;; 10/05/99 added font-info slot to formatting-info structure
;;

(defstruct (formatting-info (:conc-name fi-)
          (:predicate %formatting-info?)
          (:print-function print-formatting-info))
  (chas nil)
  (start -1)
  (stop -1)
  (font-info nil))

(defvar *fonts-in-formatting-info*
  "should font information be parsed and stored in formatting info structs")

(defsubst formatting-info? (thing)
  (or (numberp thing)
      (%formatting-info? thing)))

(defmacro do-fi-chas ((var fi) &body body)
  `(if (numberp ,fi)
       (let ((,var #\space))
   (dotimes& (i ,fi)
     . ,body))
       (do ((cha-no (fi-start ,fi) (1+& cha-no)))
     ((>=& cha-no (fi-stop ,fi)))
   (let ((,var (svref& (fi-chas ,fi) cha-no)))
     . ,body))))

(defun null-fi? (fi)
  (or (and (numberp fi) (zerop& fi))
      (and (not (numberp fi)) (= (fi-start fi) (fi-stop fi)))))

(defun first-fi-cha (fi)
  (cond ((null-fi? fi) nil)
  ((numberp fi) #\space)
  (t (svref& (fi-chas fi) (fi-start fi)))))

(defun last-fi-cha (fi)
  (cond ((null-fi? fi) nil)
  ((numberp fi) #\space)
  (t (svref& (fi-chas fi) (1-& (fi-stop fi))))))

;; need special handling to merge the old rfp with the new lfp since they
;; may share space
(defun coalesce-fi (orfp lfp &rest fis)
  (let* ((array (make-array 32 :adjustable t :fill-pointer 0))
   (return-fi (make-formatting-info :start 0))
         (trailing-rfp-space 0)
   (stop 0))
    (do-fi-chas (cha orfp)
      (vector-push-extend cha array) (incf& stop)
      (if (char= cha #\space)
          (incf& trailing-rfp-space)
          (setq trailing-rfp-space 0)))
    ;; we now know the number of trailing spaces in the old right format
    ;; we will try and ignore that many spaces before actually append any chas
    ;; more space from the new lfp
    (do-fi-chas (cha lfp)
      (cond ((and (>& trailing-rfp-space 0) (char= cha #\space))
             (decf& trailing-rfp-space))
            ((>& trailing-rfp-space 0)
             ;; if we see a real char, then zero out the space counter
             (setq trailing-rfp-space 0)
             (vector-push-extend cha array) (incf& stop))
            (t (vector-push-extend cha array) (incf& stop))))
    ;; now append the rest of the chars
    (dolist (fi fis)
      (unless (null fi)
  (do-fi-chas (cha fi) (vector-push-extend cha array) (incf& stop))))
    (setf (fi-stop return-fi) stop)
    ;; arrays have to be simple-vectors so we gots to copy into one
    ;; fortunately, this function should not get called a lot
    (let ((chas (make-array stop)))
      (dotimes (i stop)
  (setf (svref& chas i) (aref array i)))
      (setf (fi-chas return-fi) chas))
    return-fi))

(defun print-formatting-info (fi stream &rest ignore)
  (declare (ignore ignore))
  (format stream "#<Format Info ")
  (do-fi-chas (cha fi)
    (format stream "~A" cha))
  (format stream " >"))

;;; this binds info used by make-local-formatting-info
;;; this should be wrapped around the largest reasonable object
;;; that might be chunked.  For example, it is wrapped around
;;; chunk-row.
(defmacro with-local-formatting-info ((stream) &body body)
  `(let ((%%local-formatting-array%% (formatting-array ,stream))
   (%%local-formatting-offset%% (stream-position ,stream)))
     (declare (special %%local-formatting-array%%
           %%local-formatting-offset%%))
     . ,body))

(defun make-local-formatting-info (&optional (start-pos 0)
               (stop-pos start-pos))
  (declare (special %%local-formatting-array%%
        %%local-formatting-offset%%))
    (make-formatting-info :chas %%local-formatting-array%%
        :start (- start-pos %%local-formatting-offset%%)
        :stop (- stop-pos %%local-formatting-offset%%)))

(defun fi-length (fi)
  (cond ((numberp fi) fi)
  ((%formatting-info? fi)
   (-& (fi-stop fi) (fi-start fi)))
  (t (error "~A is not a valid Formatting-Info" fi))))

;;; CHUNK's
;;
;;  AT the moment, a CHUNK is a simple vector with slots for CHUNK (Lisp
;;  representation), PNAME (what the thing looks like), RIGHT-FORMAT
;;  (formatting info between the next object and this one (usually spaces),
;;  LEFT-FORMAT (formnatting info between this CHUNK and the previous CHUNK
;;  (can be EQ with previous RIGHT-FORMAT) and a PLIST like slot
;;  for extensable properties which at the moment are:
;;  LABEL (Lisp Symbol) and EVAL-PROP
;;
;;  For Example, the second CHUNK on a Boxer row that looks like this:
;;  [BOX]  Foo: BaR ; a comment
;;  would look like this:
;;  LEFT-FORMAT   -> "  "
;;  PNAME         -> "Foo: BaR"
;;  CHUNK         -> BU::BAR
;;  RIGHT-FORMAT  -> " ; a comment"
;;  PLIST         -> (:LABEL BU::FOO)
;;
;; We might want to Pre-cons a bunch of these (should put in some metering to
;; figure out how usage of chunks)  The problem is deallocation (that is, when
;; to do it (:modified maybe))
;;
;; The formatting properties should be pointers to the chas-arrays to reduce
;; CONSing but this need to be thought about some more in the case where we
;; bash the actual EDITOR-ROW via a port
;;

(defstruct (chunk :named (:type vector)
      (:conc-name chunk-)
      (:predicate nil)
      (:constructor %make-chunk (left-format
                pname
                chunk
                right-format
                &optional plist)))
  left-format
  pname
  chunk
  right-format
  plist)


(defmacro fast-chunk? (thing)
  `(eq (svref ,thing 0) 'chunk))

(defsubst chunk-p (thing) (and (simple-vector-p thing) (fast-chunk? thing)))

;;; this is here for bootstrapping reasons, the working one is
;;; in chunker.lisp (eventually)
(defun print-chunk (chunk stream depth)
  (declare (ignore depth))
  (format stream "#<CHUNK ")
  (format stream "~A" (chunk-chunk chunk))
  (format stream " >"))

;;; I dont think we'll need eval-props anymore (see the new eval for details)
(defun make-chunk (left-format pname chunk right-format
             &optional label eval-prop)
  (cond ((and (null label) (null eval-prop))
   (%make-chunk left-format pname chunk right-format))
  ((null eval-prop)
   (%make-chunk left-format pname chunk right-format `(:label ,label)))
  (T (%make-chunk left-format pname chunk right-format
      `(:label ,label :eval-prop ,eval-prop)))))


;;; Special evaluator proerties like unbox are bundled up into a struct with
;;; the first  element EQ to the eval-property
;;; see chunk-for-eval (in chunker.lisp) for more details
;;;
;;; This is hacking multiple eval and unbox by having them be nested

;;; this is old (look at process-chunks-for-eval in chunker.lisp)
;(defun make-eval-prop-thing (eval-props chunk)
; (let ((val chunk))
;    (dolist (ep eval-props val)
;      (setq val (make-array 3 :initial-contents
;			    `(boxer-eval::special-eval-token ,ep ,val))))))

(defun make-eval-prop (prop contents)
  (let ((eval-prop-array (make-array 3)))
    (setf (svref& eval-prop-array 0) 'boxer-eval::special-eval-token)
    (setf (svref& eval-prop-array 1) prop)
    (setf (svref& eval-prop-array 2) contents)
    eval-prop-array))

(defsubst eval-prop? (thing)
  (and (vectorp thing) (eq (svref& thing 0) 'boxer-eval::special-eval-token)))

(defsubst eval-prop-prop (ep) (svref& ep 1))

(defsubst eval-prop-contents (ep) (svref& ep 2))

;;;; Pointers.
;; There are 2 types of pointers. either SINGLE or MULTIPLE.  A single
;; pointer looks like: (SINGLE . <element>)
;; Multi-pointers are created when a slot in a ROW is modified.  The
;; identity of the modifier and the time of time of the modification are
;;  used to determine how to disambiguate the multi-pointer.  A multi-pointer
;;  looks like:
;; (MULTIPLE . <multi-pointer-slot1> <multi-pointer-slot2>...
;;  ... <multi-pointer-slotn>)
;; where each <multi-pointer-slot> looks like:
;; (<who-modified> <when-modified> <element>)
;; The ONLY LEGITIMATE WAY TO ACCESS A POINTER's VALUE is via POINTER-VALUE
;;
;; Change to 2-vectors later.  Lists are easier to debug
;;

(defun make-pointer (value &optional (type 'single))
  (cons type value))

(defmacro pointer-type (pointer) `(car ,pointer))

;; This is just to access the slot, the REAL work is done by GET-POINTER-VALUE
;; which checks for and disambiguates, the multipointer if it is one
(defmacro pointer-value-internal (pointer) `(cdr ,pointer))

;;; Pointer Predicates
(defun pointer? (thing) (and (listp thing)
           (fast-memq (car thing) '(single multiple))))
(deftype pointer () '(satisfies pointer?))


;;; just like check-type but can be compiled out for release
;;; this is faster because we can bypass the Common Lisp type mechanism
(defmacro vc-check-pointer (thing)
  (when (not (null *virtual-copy-debugging-option-on?*))
    `(unless (and (listp ,thing)
      (fast-memq (car ,thing) '(single multiple)))
       (error "~S is not a valid Pointer" ,thing))))

;; these are useful alternatives to having to write things like:
;; (AND (POINTER? <ptr>) (EQ (POINTER-TYPE <ptr> 'SINGLE)))

(defun single-pointer? (ptr)
  (and (consp ptr) (eq (pointer-type ptr) 'single)))

(defun multi-pointer?  (ptr)
  (and (consp ptr) (eq (pointer-type ptr) 'multiple)))


;;; Multipointers

(defstruct (multi-pointer-slot (:conc-name %mps-)
             (:copier nil)
             (:constructor
        %make-mps-from-original-item (value))
             (:constructor
        %make-mps-from-mutation (who when value))
             (:print-function multi-pointer-slot-printer)
             (:predicate multi-pointer-slot?))
  (who nil)
  (when -1)
  (value nil))

(defun multi-pointer-slot-printer (mps stream depth)
  (declare (ignore depth))
  (if (null *print-boxer-structure*)
      (format stream "#<MPS ~A>" (%mps-when mps))
      (format stream "#<MPS who:~A when:~A what:~A>"
        (%mps-who mps) (%mps-when mps) (%mps-value mps))))

;;;; Rows
;;  Rows are composed of Pointers (either SINGLE or MULTIPLE).
;;  They are basically a list. We use DEFSTRUCT for the abstraction because
;;  we may want to put some caches (maybe LENGTH-IN-CHARS or INFERIOR-PORTS)
;;  and flags (INFERIOR-NAMES?) in later to speed things up
;;  ROW-FORMAT is used when there are NO entries but the row contains ONLY
;;  formatting info such as a comment
(defstruct (evrow :copier
      (:predicate evrow?)
      (:conc-name evrow-)
      (:print-function print-evrow-internal)
      )
  (pointers   '())
  (row-format nil)
  (inferior-links? nil)
  (inferior-build? nil)
  (original-row nil))

(defmacro evrow-entries (evrow)
  (warn "Evrow-entries is obsolete, use evrow-pointers instead.")
  `(evrow-pointers ,evrow))

;;; The basic structure contains all the slots
;;; used explicitly by the virtual-copy algorthm
;;; it is used both by virtual-copy's and vc-rows-entry's

(defstruct (virtual-copy-internal-slots (:type vector)
          (:conc-name vcis-))
  (type 'data-box)
  ;; this has to be here so the fast evaluator type checking mechanism will
  ;; work.  This is only used by virtual-copy's.  vc-rows-entry's will
  ;; ignore this slot
  (pedigree nil)			;a list of important ancestors
  (creation-time -1)			;when the copy was made
  (modified? nil)			;a flag
  (inlinks? nil)			;are there any links that need copying
  (rows nil)				;a list of rows
  )

;;; Objects for the evaluator...
;;  This Virtual Copy Objects have lots of caches.  This is going to need
;;  some tuning for time/space tradeoffs.  There ought to be a better way of
;;  handling broken port links CACHED-ELEMENTS has to be per VC cause only
;;  at that level are we guaranteed uniqueness of inferiors.  We could cache
;;  them per row and use a (VC . ROW-ELEMENTS) kind of alist but this seems
;;  to be the right thing to do at the moment.  Note that it will optimize
;;  RUN and Matrix Arithmetic in the simple cases (i.e. NO box construction).
;;  Also note that in the EDITOR objects, these ARE cached by EDITOR-ROW but
;;  are flushed by any Mods to the EDITOR-ROW.
;;  We want Explicit Control over the representation so that we can use Fast
;;  type checking in the evaluator.  That is, in the Evaluator, TYPE checking
;;  will be a matter of NUMBERP, SYMBOLP or (SVREF <thing> 0) in order to
;;  avoid delving into the Common Lisp Type Lattice.
;;  Try Disassembling TYPEP to see why this is such a win
;;
;; Virtual-Copies are basically a combination of Virtual-Copy-Internal-Slots
;; and whatever properties of a box we deem important to preserve through
;; a trip through the evaluator
;;
(defstruct (virtual-copy (:type vector)
       (:include virtual-copy-internal-slots)
       (:conc-name vc-)
       (:copier %copy-vc))	;need to write our own version
  (name nil)
  (progenitor nil)			;A backpointer to the original Box
  (cached-binding-alist nil)		;Useful only in the case of TELL
  (cached-elements nil)		        ;speed up for matrix math
  (port-target? nil)			;might this be a port target ?
  (closets nil)                         ;an editor box closet
  (graphics nil)			;used for any graphics info
  (superior-for-scoping nil)
  (exports nil))

(defvar *virtual-copy-types* '(data-box doit-box sprite-box))

;;(defun virtual-copy? (thing)
;;  (and (vectorp thing) (fast-memq (svref thing 0) *virtual-copy-types*)))

;; in Lucid's lisp, the code for this function is only 8 bytes longer
;; once we have three vc types.
(defun virtual-copy? (thing)
  (and (simple-vector-p thing)
       (let ((type (vc-type thing)))
   (or (eq type 'data-box)
       (eq type 'doit-box)
       (eq type 'sprite-box)))))

(deftype virtual-copy () '(satisfies virtual-copy?))

(defstruct (virtual-port (:type vector)
       (:conc-name vp-)
       ;(:predicate virtual-port?)
       ;(:print-function print-virtual-port)
       )
  (type-slot 'port-box :read-only t)
  (name nil)
  (path nil) ;PATH will be valid until a change is made, when it gets flushed.
  (target nil)
  (editor-port-backpointer nil)) ;Always available if target is a doit.

(defun virtual-port? (thing)
  (and (simple-vector-p thing) (eq (svref thing 0) 'port-box)))

(deftype virtual-port () '(satisfies virtual-port?))


;; In practice, everytime we modify an editor box, we need
;; to keep track of all the information that we would need
;; to keep track of if we had modified a virtual-copy.  This
;; basically means that the vc-rows-entry needs to have slots
;; corresponding to each of the slots of a virtual-copy EXCEPT
;; those slots whose values can be inferred from the editor box
;; like the name.  Virtual-copy slots which are only used for
;; caches need not be included either.

;; the TIME slot is used to select the correct Vc-Rows-Entry
;; from above based on the CREATION-TIME of the top level
;; Virtual-Copy (or Vc-Rows-Entry)

;; The CACHED-BINDING-ALIST slot is used for the same thing that the
;; CACHED-BINDING-ALIST slot in virtual copies is used for.  The reason
;; why they appear separately is that we may eventually flush the slot
;; from one or both structures.
;; Need to do some metering on frequency of variable access.  Probably
;; it is MUCH more likely that there will be Multiple variable lookup
;; from vc-rows-entry's than from virtual-copy's

;; the SINGLE-IS-EXACT? slot is used to interpret the meaning of
;; dereferencing multipointers of type 'single
;; In general, it should only be T when the vc-rows-entry corresponds
;; to the initial chunking of the editor-box

(defstruct (vc-rows-entry (:type vector)
        (:include virtual-copy-internal-slots)
        (:constructor %make-vc-rows-entry))
  (time 0)
  (cached-binding-alist nil)
  (single-is-exact? nil)
  (editor-box-backpointer nil))

;;; localize the lossage here...

(defsubst vc-rows-entry? (thing)
  ;; aaacckkk !!! (checking for VCness vs VCRs )
  (and (simple-vector-p thing)
       (=& (length (the simple-vector thing))
       ;; In order to remove this run-time evaluation read-macro
       ;; we are hardcoding this value for now. It's not good. I
       ;; would like to revert this to a normal struct, but need to
       ;; look at all the other items in the code base that may
       ;; depend on it being a simple vector. For now we have a unit
       ;; test to catch if this value ever changes.
       ;;
       ;; We are removing the read-time evaluation macro such that
       ;; the defstruct vc-rows-entry no longer needs to be wrapped in
       ;; eval-when.
       ;;
       ;;   - sgithens 2022-01-12
       ;;  #.(length (%make-vc-rows-entry))
       10
     )))


;;; Mutations of editor structure are an enormous time sink
;;; What we do is to queue them around calls to eval and process
;;; the actual modification of the editor structure all at once.
;;; This can save us a lot of work in many cases iteratively updating
;;; a variable (need only perform the concete part of the update once)

(defvar *editor-objects-to-be-modified*)

(defvar *propagate-modified-messages?* nil)


(defmacro with-editor-mutation-queueing (&body body)
  `(let ((*editor-objects-to-be-modified* nil))
     (unwind-protect
   (progn . ,body)
       (process-editor-object-mutation-queue
  *editor-objects-to-be-modified*))))

(defun process-editor-mutation-queue-within-eval ()
  (let ((*propagate-modified-messages?* t))
    (process-editor-object-mutation-queue *editor-objects-to-be-modified*)
    ;; 12/10/2009 - commented out again, was causing an update bug
    ;; clear it so the same boxes don't keep getting updated
    ;; old comment for when the (setq * * nil) was commented out
    ;; not quite right butbe conservative for now.
    ;; the problem is that updated objects may still have their VC inferiors
    ;; referenced and CHANGEd which means that after the EVAL is completed,
    ;; we will have to update the object one more time.  THis is a bit
    ;; of a blunderbuss approach, we should be more fine grained about
    ;; only updating those objects which have changed since the last in-eval
    ;; update while still retaining the ability to update all the CHANGEd
    ;; objects at the end of an EVAL.  This may mean using 2 queues.
    ;(setq *editor-objects-to-be-modified* nil)
    ))



(defvar *non-lisp-data-structures-in-vcs* nil)

(defun queue-non-lisp-structure-for-deallocation (thing)
  (push thing *non-lisp-data-structures-in-vcs*))

(defun unqueue-non-lisp-structure-for-deallocation (thing)
  (setq *non-lisp-data-structures-in-vcs*
        (fast-delq thing *non-lisp-data-structures-in-vcs*)))

(defmacro with-non-lisp-structure-deallocation (&body body)
  `(let ((*non-lisp-data-structures-in-vcs* nil))
     (unwind-protect
         (progn . ,body)
       (dolist (thing *non-lisp-data-structures-in-vcs*)
         (deallocate-non-lisp-structure-in-vc thing)))))

(defun deallocate-non-lisp-structure-in-vc (thing)
  (let ((dealloc-fun (get (type-of thing) 'non-lisp-deallocation-function)))
    (if (null dealloc-fun)
        (warn "Don;'t know how to deallocate non lisp structures in ~A" thing)
        (funcall dealloc-fun thing))))

(defun deallocate-non-lisp-structures-in-graphics-sheet (gs)
  (let ((ba (graphics-sheet-bit-array gs)))
    (unless (null ba) (free-offscreen-bitmap ba))))

(setf (get 'graphics-sheet 'non-lisp-deallocation-function)
      #'deallocate-non-lisp-structures-in-graphics-sheet)


;;;; Printing

(defvar *evrow-print-length* 3)

(defun print-evrow-internal (evrow stream depth)
  (declare (ignore depth))
  (unless (null evrow)
    (let ((count 0))
      (format stream "#<EVROW")
      (dolist (entry (evrow-pointers evrow))
  (cond ((null entry) (return))
        ((and (null *print-boxer-structure* )
        (= count *evrow-print-length*))
         (format stream " ...")
         (return))
        (t (format stream "~A" entry))))
      (format stream ">"))))

(defun print-vc-internal (vc stream)
  (let ((1st-row (car (vc-rows vc))))
    (unless (null 1st-row)
      (print-evrow-internal 1st-row stream nil))))

(defun print-virtual-copy (vc stream level)
  (declare (ignore level))
  (format stream "#<vc-~a " (vc-type vc))
  (print-vc-internal vc stream)
  (format stream ">"))

(defvar *port-path-print-length* 4)

(defun print-virtual-port (vp stream level)
  (declare (ignore level))
  (let ((target (vp-target vp)))
    (format stream "#<VP-")
    (if (null target)
        ;; keep a limit on the path size printing
        (let ((path (vp-path vp)))
          (dotimes (limit *port-path-print-length*)
            (let ((box (nth limit path)))
        (cond ((null box) (return))
                    ((virtual-copy? box)
               (format stream "[~A]->" (print-vc-internal box stream)))
                    (t
               (format stream "[")
         (box-print-self-internal box stream)
         (format stream "]->"))))))
  (cond ((virtual-copy? target)
         (format stream "[~A]" (print-vc-internal target stream)))
        ((multi-pointer? target) (format stream "MP"))
        (t
         (format stream "[")
         (box-print-self-internal target stream)
         (format stream "]"))))
    (format stream  ">")))




;;;; variables

(DEFVAR *SYMBOLS-FOR-INPUT-LINE* '(BU::INPUT BU::INPUTS))

(DEFVAR *CURRENT-EVALUATION-START-TIME* 0
  "Global time that the current Eval was initiated.  Used by the VC
   mechanisms to flush caches.")

(DEFVAR *MUTATE-EDITOR-BOXES-DURING-EVAL?* NIL
  "Should Editor boxes be mutated in the middle of an evaluation ? ")

;;;; Accessor and Mutator Substs

;;; Note that some of the accessors have already been defined automatically by
;;; the DEFSTRUCTS like VC-ROWS, VP-TARGET

(defun box-name (box)
  (etypecase box
    (virtual-copy (vc-name box))
    (virtual-port (vp-name box))
    (box (editor-box-name-symbol box))))

(defun fast-box-rows (box-not-a-port)
  (if (virtual-copy? box-not-a-port) (vc-rows box-not-a-port)
      (error "~A is not a virtual copy" box-not-a-port)))

(defsetf fast-box-rows (box) (new-rows)
  `(if (virtual-copy? ,box) (setf (vc-rows ,box) ,new-rows)
       (change-vc-rows ,box ,new-rows)))

;;; this is generic.  returns the list of evrows
(defun get-box-rows (box-or-port &optional when)
  (let ((box (box-or-port-target box-or-port)))
    (cond ((virtual-copy? box) (fast-box-rows box))
    ((or (box? box) (typep box 'foreign-data))
     (if (null when)
         (virtual-copy-rows box)
         (virtual-copy-rows box when)))
    (t (boxer-eval::primitive-signal-error
        :vc-error
        box-or-port
        "is not a virtual copy or a box")))))

(defsetf get-box-rows (box-or-port) (new-rows)
  `(let ((box (box-or-port-target ,box-or-port)))
     (cond ((virtual-copy? box)
      (setf (fast-box-rows box) ,new-rows))
     ((box? box)
      (change-virtual-copy-rows box ,new-rows))
     (t (boxer-eval::primitive-signal-error
         box "is not a virtual copy or a box")))))

(defsetf box-name (box) (new-name)
  `(etypecase ,box
     (virtual-copy (setf (vc-name ,box) ,new-name))
     (virtual-port (setf (vp-name ,box) ,new-name))))

;;;; Port-Target Links

(defvar *valid-link-types*
  '(port-branch-link target-branch-link inferior-link))

(defvar *try-extra-hard-to-only-copy-what-we-have-to* t)

(defvar *links-to-be-copied* nil
  "This should be bound to the CONTAINED-LINKS slot of the box to be
   copied by the top level VC function")

(defvar *virtual-copy-target-alist* nil
  "This bound by the top level virtual copy function and is used to map
   port/target pairs to VC/VP pairs")

(defvar *virtual-copy-ports-left-to-process* nil
  "This is also bound by the top level virtual copy function and contains
   a list of port targets that can't be resolved immediately")

(defmacro with-top-level-vc-vars ((editor-box-to-be-copied) &body body)
  `(let ((*links-to-be-copied* (contained-links ,editor-box-to-be-copied))
   (*virtual-copy-target-alist* nil)
   (*virtual-copy-ports-left-to-process* nil))
     . ,body))

(defstruct (link (:conc-name link-)
     (:constructor make-link (type port target))
     (:copier nil))
  (type 'port-link)
  (port nil)
  (target nil))

;;;; Fast Type checking mechanisms
;; The evaluator assumes that all of the objects with which it has to deal
;; with will be either a symbol, a number, or a simple vector.  We will also
;; require that slot 0 in the array contains a symbol that specifies the type
;; to the evaluator.  This lets us do (svref <thing> 0) in order
;; to obtain an object's type.

(defmacro fast-eval-doit-box? (thing)
  `(eq (svref& ,thing 0) 'doit-box))

(defmacro fast-eval-data-box? (thing)
  `(if (vectorp ,thing) (eq (svref& ,thing 0) 'data-box) nil))

(defmacro fast-eval-sprite-box? (thing)
  (warn "Sprite Boxes are Obsolete !")
  `(eq (svref& ,thing 0) 'sprite-box))

;; this is the new way to check for "spriteness"
;; in virtual copies
(defun fast-vc-has-sprite? (thing) (member 'turtle (vc-graphics thing)))

(defmacro fast-eval-port-box? (thing)
  `(eq (svref& ,thing 0) 'port-box))


;;; Error checking

;;; These are COMPILE TIME CONSTANTs !!!
;;; used for error checking macros
(defconstant +link-checking-paranoia+ t)

(defconstant +compile-with-vc-metering+ t)

(defconstant +type-checking-on+ t)

(defsubst valid-link-type? (foo)
  (fast-memq foo *valid-link-types*))

(defmacro with-link-type-checking ((link) &body body)
  (if (null +link-checking-paranoia+)
      `(progn . ,body)
      `(if (valid-link-type? (link-type ,link))
     (progn . ,body)
     (error "The Link, ~S, was not a valid type of link" ,link))))

(defun expand-type-checking-pairs (list)
  (if (oddp (length list))
      (error
       "Odd number of elements in ~S which should contain PLACE-TYPE pairs" list)
      (let ((sexprs nil))
  (do* ((l list (cddr l))
        (place (car l) (car l))
        (type (cadr l) (cadr l)))
       ((null l) sexprs)
    (push `(check-type ,place ,type) sexprs)))))

(defmacro with-type-checking (place-type-pairs &body body)
  (if (null +type-checking-on+)
      `(,@body)
      `(progn ,@(expand-type-checking-pairs place-type-pairs)
        ,@body)))

;;; Metering Statistics

(defvar *vc-metering-on* t)
(defvar *current-vc-recording-structure* nil)
(defvar *old-vc-recording-structures* nil)


(defstruct (virtual-copy-statistic (:conc-name vcs-))
  (description "Generic Virtual Copy Recording")
  (link-discrimination-on? *try-extra-hard-to-only-copy-what-we-have-to*)
  ;; box level stats
  (top-level-copies 0)
  (vc-of-vcs 0)
  (portless-copies 0)
  ;; inferior stats
  (nodes-searched 0)
  (nodes-articulated 0)
  (acc-link-list-length 0)
  ;; pointer stats
  (rows-flushed-by-mpg 0)
  (single-ptr-refs 0)
  (multi-ptr-refs 0)
  (multi-ptr-acc-length 0)
  ;; other...
  (row-entry-vcs 0)
  ;; variable hacking
  (vc-var-lookups 0)
  (vc-var-lookup-cache-hits 0)
  (vc-var-lookup-cache-misses 0)
  (vc-var-lookup-cache-fills 0)
  ;;
  (vcre-var-lookups 0)
  (vcre-var-lookup-cache-hits 0)
  (vcre-var-lookup-cache-misses 0)
  (vcre-var-lookup-cache-fills 0))


(defmacro record-top-level-copy (ed-box)
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (vc-debugging "~%Virtual Copying Editor Box: ~A at ~D" ,ed-box *tick*)
       (incf (vcs-top-level-copies *current-vc-recording-structure*))
       (when (null (contained-links ,ed-box))
   (vc-debugging " (no contained links)")
   (incf (vcs-portless-copies *current-vc-recording-structure*))))))

(defmacro record-vc-copy (vc)
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (vc-debugging "~%Virtual Copying VC: ~A at ~D" ,vc *tick*)
       (incf (vcs-vc-of-vcs *current-vc-recording-structure*))
       (when (null (vc-inlinks? ,vc))
   (incf (vcs-portless-copies *current-vc-recording-structure*))))))

(defmacro record-inferior-node-search ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-nodes-searched *current-vc-recording-structure*)))))

(defmacro record-inferior-node-copy (box)
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (vc-debugging "~%Copying Inferior Node: ~A" ,box)
       (incf (vcs-nodes-articulated *current-vc-recording-structure*)))))

(defmacro record-search-list-length (length)
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-acc-link-list-length *current-vc-recording-structure*)
       ,length))))

(defmacro record-row-decache-from-mpg ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-rows-flushed-by-mpg *current-vc-recording-structure*)))))

(defmacro record-single-ptr-ref ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-single-ptr-refs *current-vc-recording-structure*)))))

;;; careful, this one has to always return the value
(defmacro record-multi-ptr-ref (mp val)
  (if (null +compile-with-vc-metering+)
      `,val
      `(unless (null *vc-metering-on*)
   (vc-debugging "~%DeReferencing MultiPointer: ~A ==> ~A" ,mp ,val)
   ;(showmp ,mp)
   (incf (vcs-multi-ptr-refs *current-vc-recording-structure*))
   (incf (vcs-multi-ptr-acc-length *current-vc-recording-structure*)
         (length (pointer-value-internal ,mp)))
   ,val)))

(defmacro record-row-entry-vc-creation ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-row-entry-vcs *current-vc-recording-structure*)))))

(defmacro record-vc-var-lookup ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-vc-var-lookups *current-vc-recording-structure*)))))

(defmacro record-vc-var-lookup-cache-hit ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-vc-var-lookup-cache-hits *current-vc-recording-structure*)))))

(defmacro record-vc-var-lookup-cache-miss ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-vc-var-lookup-cache-misses
        *current-vc-recording-structure*)))))

(defmacro record-vc-var-lookup-cache-fill ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-vc-var-lookup-cache-fills
        *current-vc-recording-structure*)))))

(defmacro record-vcre-var-lookup ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-vcre-var-lookups *current-vc-recording-structure*)))))

(defmacro record-vcre-var-lookup-cache-hit ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-vcre-var-lookup-cache-hits
        *current-vc-recording-structure*)))))

(defmacro record-vcre-var-lookup-cache-miss ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-vcre-var-lookup-cache-misses
        *current-vc-recording-structure*)))))

(defmacro record-vcre-var-lookup-cache-fill ()
  (unless (null +compile-with-vc-metering+)
    `(unless (null *vc-metering-on*)
       (incf (vcs-vcre-var-lookup-cache-fills
        *current-vc-recording-structure*)))))

;;;;; TIME

(defsubst now () (tick))

;;;; Virtual copy & printing hooks

(defvar *edvc-special-structures-hook* nil
  "A list of functions, each taking 2 args, the Editor box and the virtual copy.
   Any special structures in the editor box should be copied and put into the VC")

(defvar *vcvc-special-structures-hook* nil
  "A list of functions, each taking 2 args, the original VC and the new VC. Any
   special structures in the editor box should be copied and put into the VC")

(defvar *print-vc-special-structures-hook* nil
  "A list of functions, each taking 2 args, a VC and the new editor box.  Any
   special structures in the VC should be converted and placed in the editor box")
