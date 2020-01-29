;;; -*- Syntax: Common-Lisp; Base: 10; Package: EVAL -*-

;;; $Header: bind.lisp,v 1.0 90/01/24 22:06:35 boxer Exp $

;;; $Log:	bind.lisp,v $
;;;Revision 1.0  90/01/24  22:06:35  boxer
;;;Initial revision
;;;
#|

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-------+
             This file is part of the | Boxer | System
                                      +-Data--+




Modification History (most recent at top)

10/11/00 fixed bug in add-static-variable-pair.  The original fix didn't work
         because the mac version turned out to be relying on the original broken
         behavior
 4/10/00 boxer-symeval-handle-^ & boxer-symeval-previous-TELL-value now handle
         the possibility of ASK frames
 8/18/99 fixed up box-interface clause of add-static-variable-pair

|#

;;; NEED TO WORK ON BOXER-SYMEVAL.

(in-package :boxer-eval)

;;;;
;;;; This file handles variables and binding in Boxer programs.

;;; Static Variable Lookup and Caching
;;; Bugs:
;;;  Something should control the growth of the cache.  Maybe we can
;;;  make it a ring buffer.
;;; It would be nice if deleted variables could be made to go away
;;; automatically, since in this scheme they don't go away until you try
;;; to access one.

(defconstant *non-caching-variable-uid* -2)
(defconstant *deleted-variable-uid* -1)
(defvar *cached-variable-uid-counter* 0)

;;; We want the UID to wrap around to 0, but never become negative or a bignum.
(defun make-cached-variable-uid ()
  (incf (the fixnum *cached-variable-uid-counter*)))

;; Some sort of fixnum = might be necessary.
(defmacro cache-uid-= (A B)
  `(= (the fixnum ,A) (the fixnum ,B)))

(defstruct (static-variable
	     (:constructor
	       make-static-variable
	       (name value &optional (uid (make-cached-variable-uid))))
	     (:type list))
  name
  uid
  value)

(defun make-non-caching-static-variable (name value)
   (make-static-variable name value *non-caching-variable-uid*))

(defstruct (sv-cache-entry
	     (:type list)
	     (:constructor %make-sv-cache-entry (name uid real-object)))
  name
  uid
  real-object)

(defun make-sv-cache-entry (static-variable)
  (%make-sv-cache-entry
    (static-variable-name static-variable)
    (static-variable-uid static-variable)
    static-variable))


;;;
;;; Full-fledged static lookup.
;;; The static-variable-cache caches this information.
;;;

;;; LOOKUP-STATIC-VARIABLE-INTERNAL does a real lookup of
;;; static variables.  It searches the static-variables-alist of the box
;;; and either returns what it finds or passes the message off to its
;;; superior.

;;; However, during any given Boxer evaluation, the static
;;; variable root remains constant, unless someone does a TELL.
;;; Additionally, since the structure that the variables come from is
;;; indeed static (with the rare exception of added or deleted
;;; variables) we can cache each variable reference in an alist
;;; associated with each static variable root.
;;;
;;; So, in each box we keep a cache of static variable/value objects.
;;; We mark each variable in the cache and each variable in the real
;;; static structure with a unique ID, re-cache the variable when the
;;; unique ids are not the same.  Deleting a variable from a box causes
;;; its unique id to be set to the *deleted-variable-uid*.  Adding a
;;; variable to a box causes the next outermost variable with the same
;;; name to get a new unique id, thus breaking the link between all
;;; caches and the static variable itself.  Changing a variable name
;;; should act like deleting and adding.  Deleting a box should flush
;;; its cache and the cache of all the boxes in it, since it might be
;;; brought back in some other place.  Copying a box should not copy the
;;; cache.
;;;
;;; In variable lookup, whenever we discover that the uids don't match
;;; we have to recache or delete the variable.  If we find that the
;;; variable has been deleted we remove it from the cache.  We can
;;; easily tell that a variable was deleted:  the uid of the fetched
;;; object will be *deleted-variable-uid*, which is guaranteed not
;;; to match the cache.
;;; The recache case is also simple: add the result of
;;; LOOKUP-STATIC-VARIABLE-INTERNAL to the cache.
;;;
;;; Some variables may want to be exempt from caching.  Key functions
;;; for instance, are numerous and not time dependent.  If the uid
;;; of a variable is *non-caching-variable-uid*, then we never cache it.
;;;

;;; This function is smarter than you are.
(defmethod lookup-static-variable ((self boxer::box) variable)
  (let* ((box-cache (slot-value self 'static-variable-cache))
	 (cache-entry (fast-assq variable box-cache)))
    (cond ((not (null cache-entry))
	   (let ((real-object (sv-cache-entry-real-object cache-entry)))
	     ;; There is a cached object...
	     (cond
	       ;; ...and the cache-entry is valid.
	       ((cache-uid-= (sv-cache-entry-uid cache-entry)
			     (static-variable-uid real-object))
		real-object)
	       ;; ...but the cached object was subsequently deleted.
	       ((cache-uid-= (sv-cache-entry-uid cache-entry)
			     *deleted-variable-uid*)
		(setf (slot-value self 'static-variable-cache)
		      (fast-delq cache-entry box-cache))
		nil)
	       ;; ...but some other variable has been interposed in
	       ;; the meantime, so we need to cache that instead...
	       (t
		(let ((new-value (lookup-static-variable-internal
				   *lexical-variables-root* variable)))
		  (cond
		    ;; ...unless there is in fact no new real
		    ;; object (*it* was subsequently deleted)
		    ;; ...or if the new object doesn't want to be cached ever
		    ((or (null new-value)
			 (cache-uid-= (static-variable-uid new-value)
				      *non-caching-variable-uid*))
		     (setf (slot-value self 'static-variable-cache)
			   (fast-delq cache-entry box-cache)))
		    ;; ...but no, so we cache the new variable.
		    (t (setf (sv-cache-entry-real-object cache-entry)
			     new-value)
		       (setf (sv-cache-entry-uid cache-entry)
			     (static-variable-uid new-value))))
		  new-value)))))
	  ;; There is no cached object
	  (t (let ((new-value (lookup-static-variable-internal
				*lexical-variables-root* variable)))
	       (cond
		 ;;...if there is no real object, either, we can't cache it
		 ((null new-value))
		 ;;...or if the object doesn't want to be cached,
		 ;;   we won't cache it
		 ((cache-uid-= (sv-cache-entry-uid new-value)
			       *non-caching-variable-uid*))
		 ;;...otherwise we cache it
		 (t (push (make-sv-cache-entry new-value)
			  (slot-value self 'static-variable-cache))))
	       new-value)))))

;;; This is the basic function which looks up the value of a static variable
;;; without going through the cache.  It returns a static-variable object.
(defmethod lookup-static-variable-internal ((self boxer::box) variable)
  (cond ((boxer::editor-box-changed? self)
	 (boxer::lookup-variable-in-vc-rows-entry
	  (car (slot-value self 'boxer::virtual-copy-rows))
	  variable self))
	((fast-assq variable (slot-value self 'static-variables-alist)))
	(t (let ((sup (boxer::superior-box self)))
	     (if (null sup)
		 (if (boundp variable) (symbol-value variable) nil)
		 (lookup-static-variable-internal sup variable))))))

;;; This function does a one-level static variable lookup.
;;; It is for SPRITE instance variables.  Since we're looking in just
;;; one box, we don't go through the cache.
(defmethod lookup-static-variable-in-box-only ((self boxer::box) variable)
  (let ((vc-rows (slot-value self 'boxer::virtual-copy-rows))
	(tmp nil))
    (cond ((not (null (cdr vc-rows)))
	   ;; what his REALLY wants to check is if there is more than
	   ;; one set of virtual copy rows.  If there is, then the
	   ;; BOX has been CHANGEd
	   ;;
	   ;; Also, IWBNI we could tell whether we happen to be in the
	   ;; recursive case here and at least signal a warning that we are
	   ;; scoping for a variable up through a box that has been CHANGEd
	   (cond ((setq tmp
			(boxer::lookup-variable-in-vc-rows-entry (car vc-rows)
								 variable
								 self NIL))
		  (static-variable-value tmp))
		 (t nil)))
	  (t (let ((entry (fast-assq variable
				     (slot-value self
						 'static-variables-alist))))
	       (if (null entry) nil (static-variable-value entry)))))))


;;; This function sets up a whole new set of static variables, flushing
;;; the old ones.
;;; The file system and CHANGE must use this method.
;;; There is no other legitimate way to set the static variables alist.
(defmethod set-static-variables-alist ((self boxer::box) new-alist)
  (dolist (var (slot-value self 'static-variables-alist))
    (setf (static-variable-uid var) *deleted-variable-uid*))
  (setf (slot-value self 'static-variables-alist) new-alist))

;;; This function adds a new variable and value to a given box.
;;; If the variable exists in this box, then we change it the value
;;; and give appropriate warning message.  The cache entries remain
;;; valid, so we don't need to flush any caches, so we don't
;;; put in a new uid.
;;;
;;; Otherwise, if the variable doesn't already exist in this box, we make a
;;; static variable entry and push it on the static-variables-alist of the box.
;;; If the variable exists in a box outside this one, we change
;;; the uid for that variable, to cause it to be flushed from
;;; all caches.

(defvar *warn-about-primitive-shadowing* t)

(defmethod add-static-variable-pair ((self boxer::box) variable value)
  (let ((existing-entry (fast-assq variable
				   (slot-value self 'static-variables-alist))))
    (cond ((null existing-entry)
	   (let ((newly-shadowed-variable-entry
		   (lookup-static-variable-internal self variable)))
	     (when (not (null newly-shadowed-variable-entry))
	       ;; maybe warn about shadowing primitives
	       (when (and (not (null *warn-about-primitive-shadowing*))
			  (boundp variable)
			  (eq newly-shadowed-variable-entry
			      (symbol-value variable)))
		 (warn-about-primitive-shadowing variable))
	       (setf (static-variable-uid newly-shadowed-variable-entry)
		     (make-cached-variable-uid)))
	     (push (make-static-variable variable value)
		   (slot-value self 'static-variables-alist))
	     (propagate-exported-binding self variable value)))
	  ((eq value (static-variable-value existing-entry))
	   (when boxer::*boxer-system-hacker*
	     (cerror "Ignore the addition" "How come you're trying to add a new static variable entry (~S ~S)
when you have a perfectly good one already with the same value, just tell me, huh?
Don't take this message out; fix the code so that it doesn't try to add variables that
already exist.  I have enough trouble already with this hairy cache scheme."
		     variable value)))
	  ((boxer::box-interface? (static-variable-value existing-entry))
           (cond ((boxer::box-interface? value)
                  ;; not the best solution but rather benign
                  ;(warn "Trying to link a box-interface into a box-interface")
                  )
                 ((let ((intbox (boxer::box-interface-box
                                 (static-variable-value existing-entry))))
                    (and (boxer::box? intbox)
                         (not (eq intbox value))))
                  ;; looks like there is already a box associated with this
                  ;; make sure the behavior here matches the regular (box)
                  ;; name conflict clause below
                  (boxer::avoid-box-name-conflict-by-removing-name value))
                 (t
	          (boxer::link-box-into-interface
                   (static-variable-value existing-entry) value))))
	  (t (boxer::avoid-box-name-conflict-by-removing-name value)
	     ;; alternatively, we could use:
	     ;; (boxer::avoid-box-name-conflict-by-versioning
	     ;;    (static-variable-value existing-entry))
	     ;; (setf (static-variable-value existing-entry) value)
	     ;; (propagate-exported-binding self variable value)
	     ))))

(defvar *primitive-shadow-warning-on?* t)

(defun warn-about-primitive-shadowing (variable)
  (unless (null *primitive-shadow-warning-on?*)
    (boxer::boxer-editor-warning
     "Warning: The Primitive ~A will no longer be available in this box"
     variable)))

;;; Use this function to remove static variables.
;;; It removes the variable from the STATIC-VARIABLES-ALIST and invalidates the
;;; unique id.
(defmethod remove-static-variable ((self boxer::box) variable
				   &optional (box-interface-too? nil))
  ;; remove the binding from the superior box's alist if we have to...
  (remove-exported-binding self variable)
  ;; then remove it locally
  (setf (slot-value self 'static-variables-alist)
	(fast-del-if #'(lambda (entry)
			 (cond ((and (null box-interface-too?)
				     (eq (static-variable-name entry) variable)
				     (boxer::box-interface?
				      (static-variable-value entry)))
				;; should do some sort of consistency check
				(boxer::unlink-box-interface
				 (static-variable-value entry))
				nil)
			       ((eq (static-variable-name entry) variable)
				(setf (static-variable-uid entry)
				      *deleted-variable-uid*)
			       t)
			       (t nil)))
		(slot-value self 'static-variables-alist))))

;; this is used by avoid-box-name-conflict-by-removing-name rather than
;; remove-static-variable.  It differs in that the value is passed in as
;; well to be used for comparison purposes
(defmethod remove-static-variable-pair ((self boxer::box) variable value
                                        &optional (box-interface-too? nil))
  (setf (slot-value self 'static-variables-alist)
	(fast-del-if #'(lambda (entry)
			 (cond ((and (null box-interface-too?)
				     (eq (static-variable-name entry) variable)
				     (boxer::box-interface?
				      (static-variable-value entry))
                                     (eq (boxer::box-interface-box
                                          (static-variable-value entry))
                                         value))
				;; should do some sort of consistency check
				(boxer::unlink-box-interface
				 (static-variable-value entry))
				nil)
			       ((and (eq (static-variable-name entry) variable)
                                     (eq (static-variable-value entry) value))
				(setf (static-variable-uid entry)
				      *deleted-variable-uid*)
			       t)
			       (t nil)))
		     (slot-value self 'static-variables-alist)))
  ;; walk up the hierarchy if we are exporting
  (let ((exports (slot-value self 'boxer::exports))
        (sup (boxer::superior-box self)))
    (when (and (or (eq exports *exporting-all-variables-marker*)
                   (fast-memq variable exports))
               (boxer::box? sup))
      (remove-static-variable-pair sup variable value box-interface-too?))))

;;; For the EDITOR
;;; this needs to be done more gracefully than by
;;; having a function called eval::evaluator-delete-self-action.
;;; Well, what we need for now is not to call this function if we're only
;;; moving the box around inside another box.  This stuff is quite a chore.
;;; We need to have a move-box method that binds some variable so that
;;; and calls delete-self and insert-self but doesn't let them call
;;; this function.
(defmethod evaluator-delete-self-action ((self boxer::box)
					 &optional (superior-box
						    (boxer::superior-box
						     self)))
  (setf (slot-value self 'static-variable-cache) nil)
  (boxer::do-for-all-inferior-boxes-fast (box self)
    (setf (slot-value box 'static-variable-cache) nil))
  ;; handle any magic name deletion actions
  (let ((name-row (boxer::name-row self)))
    (when (not (null name-row))
      (let ((name (boxer::cached-name name-row)))
	(when (not (null name))
	  (let ((magic-name-delete-handler
		 (get name 'boxer::magic-name-delete)))
	    (when (not (null magic-name-delete-handler))
	      (funcall magic-name-delete-handler self superior-box)))
	  (remove-static-variable superior-box name))))))


#|    (remove-all-static-bindings-with-matching-value superior-box self) |#


;;;
;;; Dynamic Variable Binding
;;;

;;; Dynamic variables are stored in a pair of stacks, one for names and
;;; one for values.  Function boundaries are indicated by a special variable
;;; name, which is pushed at the beginning of each function frame.

;;; *dynamic-variables-bottom*, normally 0, is available for binding to mark
;;; dynamic context switches (as in TELL).  *dynamic-variables-top* is
;;; the stack pointer.
;;; The arrays are of type simple-vector.  they get copied if they grow
;;; too large.

;;; Variables
(defvar *initial-dynamic-variables-array-size* 50)

;;; called at startup of each boxer process.
(defun make-dynamic-variables-names-array ()
  (make-array *initial-dynamic-variables-array-size*))

(defun make-dynamic-variables-values-array ()
  (make-array *initial-dynamic-variables-array-size*))

;;; called only when trying to recover from bad lisp errors.
(defun reset-dynamic-variables ()
  (setq *dynamic-variables-bottom* 0)
  (setq *dynamic-variables-top* 0)
  (fill *dynamic-variables-names-array* nil)
  (fill *dynamic-variables-values-array* nil))

;;;
;;; Dynamic variable lookup interface:
;;;

;;; Dynamic variables are stored in a pair of vectors, with
;;; names in one vector and values in another.
;;; *dynamic-variables-top* is the index of the next place
;;; to store things.  *dynamic-variables-bottom* is the
;;; index of the place to stop looking.  TELL binds this
;;; variable to create an empty dynamic context.
;;;

;;;
;;; Below is the main variable lookup function.  BOXER-SYMEVAL calls this
;;; function.  It is an extreme inner-loop of Boxer evaluation.  If you
;;; can make it faster, please do.  Making it inline, however, seemed to
;;; make things slower.
(defun dynamic-variable-lookup (variable)
  ;; This gives something like a 20% speedup in MCL -- MT
  #+mcl (declare (optimize (speed 3) (safety 0)))
  ;; Array and bottom locals are for compilers that can't do the
  ;; side-effect analysis (most of them).
  (let ((array *dynamic-variables-names-array*)
	(bottom  *dynamic-variables-bottom*))
    (do ((index (1-& *dynamic-variables-top*) (1-& index)))
	((<& index bottom) nil)
      (when (eq variable (svref& array index))
	(return (svref& *dynamic-variables-values-array* index))))))

;; used by landscapes
(defun %dynamic-variable-set (variable newvalue)
  #+mcl (declare (optimize (speed 3) (safety 0)))
  ;; Array and bottom locals are for compilers that can't do the
  ;; side-effect analysis (most of them).
  (let ((array *dynamic-variables-names-array*)
	(bottom  *dynamic-variables-bottom*))
    (do ((index (1-& *dynamic-variables-top*) (1-& index)))
	((<& index bottom) :error)
      (when (eq variable (svref& array index))
	(return (setf (svref& *dynamic-variables-values-array* index)
                      newvalue))))))

;;; This function is for BOXER-SET-INTERNAL.  It returns the address
;;; of a dynamic variable so the value can be modified.
(defun dynamic-variable-lookup-index (name)
  (let ((array *dynamic-variables-names-array*)
	(bottom *dynamic-variables-bottom*))
    (do ((index (1-& *dynamic-variables-top*) (1-& index)))
	((<& index bottom) nil)
      (when (eq name (svref& array index))
	(return index)))))

;;; This function is for BOXER-SET-INTERNAL.
(defmacro dynamic-variable-index-value (index)
  `(svref& *dynamic-variables-values-array* ,index))

;;;
;;; Internals of dynamic binding.  No other file should call these.
;;;
(defmacro %dynamic-variable-push (name value)
  `(progn
    (setf (svref& *dynamic-variables-names-array* *dynamic-variables-top*)
	  ,name)
    (setf (svref& *dynamic-variables-values-array* *dynamic-variables-top*)
	  ,value)
    (incf& *dynamic-variables-top*)
    (when (=& *dynamic-variables-top*
	      (box::svlength *dynamic-variables-names-array*))
      (extend-dynamic-variables-arrays))))

;;; This function is called when the stack overflows.
;;; Maybe there should be some way to shrink the stack back down.<<<<----******
(defun extend-dynamic-variables-arrays ()
  (let ((old-length (box::svlength *dynamic-variables-names-array*)))
    (let ((new-names-array (make-array
			    (+& old-length
				*dynamic-variables-array-grow-length*)))
	  (new-values-array (make-array
			     (+& old-length
				 *dynamic-variables-array-grow-length*))))
      ;;; Don't use REPLACE here.  It's too stupid, even with declarations.
      (dotimes& (i old-length)
	(setf (svref& new-names-array i)
	      (svref& *dynamic-variables-names-array* i))
	(setf (svref& new-values-array i)
	      (svref& *dynamic-variables-values-array* i)))
      (setq *dynamic-variables-names-array* new-names-array)
      (setq *dynamic-variables-values-array* new-values-array))))

;;;
;;; Dynamic variable binding interface.
;;;
;;; The evaluator calls this function with a raw arglist (with flavors),
;;; a list of static-variable-objects, and the values for the arglist
;;; variables on the VPDL.

;;; Rather than pusing the dynamic stack pointer in the ufuncall
;;; frame, we push a marker on the dynamic variables stack to
;;; mark procedure boundaries.
;;; We should do tests and see which is better.
(defun dynamically-bind-variables-and-locals (arglist local-list)
  ;; We push the marker first, so unbind can always stop at a marker.
  (%dynamic-variable-push '*dynamic-binding-boundary* nil)
  ;; reversed order of local and input binding so inputs with the same name will
  ;; take precendence over locals with that name in dynamic variable lookup (EhL)
  (dolist (var local-list)
    (%dynamic-variable-push (static-variable-name var)
			    (let ((value (static-variable-value var)))
			      (if (doit-box? value)
				  value
				  (copy-thing value)))))
  (dolist (arg arglist)
    (%dynamic-variable-push arg (vpdl-pop))))

;;;
;;; The evaluator calls this function on function return or throw.
;;; It unbinds the variables for one function.
;;;

(defun dynamically-unbind-variables ()
  (do ((i (1-& *dynamic-variables-top*) (1-& i)))
      ((eq (svref& *dynamic-variables-names-array* i)
	   '*dynamic-binding-boundary*)
       (setq *dynamic-variables-top* i))
    ;; this test is wrong
    (when (<& i *dynamic-variables-bottom*)
      (error "Beginning of dynamic stack reached during pop."))
    ;; We reset the values array slots so the objects can be GC'd.
    ;; We need not erase the variable names, since they
    ;; won't be GC'd (GCTWA'd) anyway.
    (setf (svref& *dynamic-variables-values-array* i) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BOXER-SYMEVAL AND BOXER-TOPLEVEL-SYMEVAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic variables now exist in an array.  *dynamic-variables-bottom* is the
;;; array index to stop lookup up at, default 0.  *dynamic-variables-top* is
;;; where to start looking from.  *dynamic-variables-names-array* is the array
;;; of names, and *dynamic-variables-values-array* is the array of values.
;;; the local
;;; WRONG:
;;; Dynamic Boxer variables exist in an alist.  You get the value of a
;;; variable by calling the lookup function on it.

;;; If the variable is not found in the alist, then the static variables
;;; of the boxes in the lexical scope of the outermost box being executed
;;; are searched.  This searching happens by asking the DOIT'ed box to look
;;; up the variable in its static alist, and failing finding it there to
;;; ask the box it is inside of to do the same, all the way to the
;;; toplevel box.

;;; We cache references to lexical variables to keep execution deep inside box
;;; structure from taking a long time.

;;; BOXER-SYMEVAL is for use on symbols only.
(defun boxer-symeval (variable)
  (or (dynamic-variable-lookup variable)
      (cond ((eval-object? *lexical-variables-root*)
	     ;; If there is an arg flavor and it is not PORT-TO, we can
	     ;; give a third arg of NIL.  We need to check this out to
	     ;; make sure it's only PORT-TO that we need to check for,
	     ;; and to provide some sort of clean interface.
	     ;; If the variable is not present in the virtual copy,
	     ;; the we revert to the previous TELL context, since that's
	     ;; presumably where the box came from (i.e., "exists" in
	     ;; the stepper world).  There must be a previous TELL environment,
	     ;; since that's the only way we can get a VC as lexical root.
	     ;; (what about data port execution?)
	     (or (boxer::lookup-variable-in-virtual-copy *lexical-variables-root*
						         variable)
	         (boxer-symeval-previous-TELL-value variable)))
            ((typep *lexical-variables-root* 'boxer::foreign-data)
             (boxer::lookup-variable-in-foreign-data *lexical-variables-root*
                                                     variable))
            (t
	     (let ((slot
		    (lookup-static-variable *lexical-variables-root* variable)))
	       (or (and slot (static-variable-value slot))
		   *novalue*))))
      *novalue*))


;;; BOXER-SYMEVAL for use with ^ objects, symbols, or data boxes.
;;; If given a SYMBOL just do boxer-symeval.
;;; If given an ^ object, do symeval in the previous lexical environment,
;;; or just return the object if it's not a symbol.

;;; It would be nice if it didn't use the DBG- functions.
;;; Inefficiencies:
;;;       Shouldn't use TELL-SPECIAL-FRAME-... functions.
;;;       Should special-case the first ^.
;;;       Shouldn't use SPECIAL-TOKEN? unless it's made faster.
;;;        That part is gross anyway.

(defun boxer-symeval-special-token (special-token-object)
  (cond	((symbolp special-token-object) (boxer-symeval special-token-object))
	((previous-tell-environment-token? special-token-object)
	 (boxer-symeval-handle-^ special-token-object))
	((dots-list-token? special-token-object)
	 (boxer-symeval-dots-list (car (special-token-item
					special-token-object))
				  (special-token-item
				   special-token-object)))
	(t (signal-error :eval-bug
			 "Unknown special token object"
			 special-token-object
			 "in"
			 'boxer-symeval-special-token))))

;;; 4/10/99 now that we have ASK, we have to indirect, perhaps
;;; later we can bum these functions to to address the 1st pt above
;;; NOTE: undiscovered on the mac because type checking was left OUT
;;; because we habitually used safety = 1
(defun ask/tell-frame-dynamic-variables-bottom (frame)
  (if (ask-special-frame-p frame)
      (dbg-ask-special-frame-*dynamic-variables-bottom* frame)
    (dbg-tell-special-frame-*dynamic-variables-bottom* frame)))

(defun ask/tell-frame-lexical-variables-root (frame)
  (if (ask-special-frame-p frame)
      (dbg-ask-special-frame-*lexical-variables-root* frame)
    (dbg-tell-special-frame-*lexical-variables-root* frame)))

(defun ask/tell-frame-old-tell-special-frame (frame)
  (if (ask-special-frame-p frame)
      (dbg-ask-special-frame-*old-tell-special-frame* frame)
    (dbg-tell-special-frame-*old-tell-special-frame* frame)))

;; called only by boxer-symeval-special-token.
(defun boxer-symeval-handle-^ (special-token-object)
  (let ((old-frame *old-tell-special-frame*))
    (if (null old-frame)
	(signal-error :too-many-^
		      "can't find a box named" special-token-object)
	(let ((*dynamic-variables-bottom*
               (ask/tell-frame-dynamic-variables-bottom old-frame))
	      (*lexical-variables-root*
	       (ask/tell-frame-lexical-variables-root old-frame))
	      (*old-tell-special-frame*
	       (ask/tell-frame-old-tell-special-frame old-frame)))
	  (boxer-symeval-special-token
	   (special-token-item special-token-object))))))

;; called only by boxer-symeval when given a virtual copy which didn't
;; contain the variable.
(defun boxer-symeval-previous-TELL-value (variable)
  (let ((old-frame *old-tell-special-frame*))
    (if (null old-frame)
	*novalue*
	(let ((*dynamic-variables-bottom*
               (ask/tell-frame-dynamic-variables-bottom old-frame))
	      (*lexical-variables-root*
	       (ask/tell-frame-lexical-variables-root old-frame))
	      (*old-tell-special-frame*
	       (ask/tell-frame-old-tell-special-frame old-frame)))
	  (boxer-symeval variable)))))

(defun boxer-symeval-dots-list (symbol-for-error list)
  (let ((root (boxer-symeval (car list))))
    (cond ((eq root *novalue*) *novalue*)
	  ((numberp root) *novalue*) ;;;???
	  ((boxer::virtual-copy? root)
	   (boxer-symeval-dots-list-vc symbol-for-error root (cdr list)))
	  ((boxer::virtual-port? root)
           (let ((target (boxer::vp-target root)))
	     (cond ((boxer::virtual-copy? target)
	            (boxer-symeval-dots-list-vc symbol-for-error
					        target
					        (cdr list)))
                   ((typep target 'boxer::foreign-data)
                    (boxer-symeval-dots-list-fd symbol-for-error
                                                target
                                                (cdr list)))
                   (t
	            (boxer-symeval-dots-list-eb symbol-for-error
					        target
					        (cdr list))))))
	  ((boxer::port-box? root)
           (let ((target (boxer::ports root)))
	     (cond ((boxer::virtual-copy? target)
	            (boxer-symeval-dots-list-vc symbol-for-error
					        target
					        (cdr list)))
                   ((typep target 'boxer::foreign-data)
                    (boxer-symeval-dots-list-fd symbol-for-error
                                                target
                                                (cdr list)))
                   (t
	            (boxer-symeval-dots-list-eb symbol-for-error
					        target
					        (cdr list))))))
#|
	  ((and (possible-eval-object? root) (boxer-function? root))
	   (if (eq root (boxer-top-level-symeval 'bu::SELF))
	       (boxer-symeval-dots-list symbol-for-error (cdr list))
	       (signal-error :dots-variable-lookup
			     "in"
			     symbol-for-error (car list)
			     "is not a DATA box or PORT")))
|#
	  ((boxer::box? root)
	   (boxer-symeval-dots-list-eb symbol-for-error root (cdr list)))
          ((typep root 'boxer::foreign-data)
           (boxer-symeval-dots-list-fd symbol-for-error root (cdr list)))
	  (t (signal-error
	      :dots-variable-lookup
	      "in" symbol-for-error (car list) "is not a DATA box or PORT")))))

(defun boxer-symeval-dots-list-vc (symbol-for-error box vars)
  (do ((vars vars (cdr vars))
       (box box (and box (if (eval-object? box)
			     (boxer::lookup-variable-in-virtual-copy
			      box (car vars))
			     (lookup-static-variable-in-box-only
			      box (car vars))))))
      ((null vars) (or box *novalue*))
    (when (null box) (return *novalue*))
    (unless (null vars)
      (when (if (eval-object? box)
		(fast-eval-doit-box? box)
		(doit-box? box))
	(return (signal-error
		 :dots-variable-lookup
		 "in" symbol-for-error box "is not a DATA box or PORT"))))))

(defun boxer-symeval-dots-list-eb (symbol-for-error box vars)
  ;; a hook for (possible) loading the box from the server
  (when (and (null (slot-value box 'boxer::first-inferior-row))
	     (boxer::storage-chunk? box))
    (boxnet::fill-box-from-server box))
  (do ((vars vars (cdr vars))
       (box box (and box (lookup-static-variable-in-box-only box (car vars)))))
      ((null vars) (or box *novalue*))
    (unless (null vars)
      (when (doit-box? box)
	(return (signal-error
		 :dots-variable-lookup
		 "in" symbol-for-error box "not a DATA box or PORT"))))))




#|
;;;
;;; Experimental boxer-symeval-previous-value.  Don't cache the value.
;;; Really we need boxer-symeval-nth-previous-value.
;;;

(defun boxer-symeval-previous-value (variable)
  (let ((upper-root (member variable *dynamic-variables-root*
			    :test #'(lambda (a b) (eq a (car b))))))
    (if (null upper-root)
	(let ((second-slot (lookup-static-variable-previous-value-internal
			    *lexical-variables-root*
			    variable)))
	  (if (not (null second-slot))
	      (static-variable-value second-slot)
	      *novalue*))
	(let ((second-slot (fast-assq variable (cdr upper-root))))
	  (if (null second-slot)
	      (if (setq second-slot (lookup-static-variable-internal
				     *lexical-variables-root* variable))
		  (static-variable-value second-slot)
		  *novalue*)
	      (cdr second-slot))))))
|#

;;; This function is related to (and calls) lookup-static-variable-internal
;;; but is used for the ^ feature.  It looks up the second value in the box
;;; hierarchy.  It doesn't go through the cache.
(defmethod lookup-static-variable-previous-value-internal ((self boxer::box)
							   variable)
  (let ((slot (fast-assq variable (slot-value self 'static-variables-alist)))
	(sup (boxer::superior-box self)))
    (if (not (null slot))
	(if (null sup)
	    (if (boundp variable) (symbol-value variable) nil)
	    (lookup-static-variable-internal sup variable))
	(if (null sup)
	    nil
	    (lookup-static-variable-previous-value-internal sup variable)))))

;; Used for getting global values of things like infix functions
;; and other things the user can't bind.
(defun boxer-toplevel-symeval (variable)
  (if (boundp variable)
      (static-variable-value (symbol-value variable))
      *novalue*))

;; Always use boxer-toplevel-set or boxer-toplevel-set-nocache
;; to write into a global variable (i.e., primitive).
(defun boxer-toplevel-set (variable value)
  (cond ((boundp variable)
	 (let ((object (symbol-value variable)))
	   (setf (static-variable-value object) value)
	   (setf (static-variable-uid object)
		 (make-cached-variable-uid)))
	 value)
	(t (setf (symbol-value variable)
		 (make-static-variable variable value)))))

;; Use boxer-toplevel-set-nocache to write into things like keys and
;; mouse functions that you don't want to be cached.
(defun boxer-toplevel-set-nocache (variable value)
  (setf (symbol-value variable)
	(make-non-caching-static-variable variable value)))

;;; doesn't hack the potential of caches
;;; Use this ONLY on toplevel variables created with boxer-toplevel-set-nocache
(defun boxer-toplevel-nocache-unset (variable)
  (makunbound variable))

(defun boxer-set-internal (variable value)
  (let ((index (dynamic-variable-lookup-index variable)))
    (cond ((not (null index))
	   (setf (dynamic-variable-index-value index) value))
	  ((and *lexical-variables-root*
		(let ((slot
		       (lookup-static-variable *lexical-variables-root*
					       variable)))
		  (setf (static-variable-value slot) value))))
	  (t (error "Variable ~S not found -- boxer-set-internal" variable)))))

