;;-*- Mode:Lisp; Syntax: Common-Lisp; package: Boxer; -*-

#|

 $Header$

 $Log$

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


            This file contains BUILD and its supporting functions


Modification History (most recent at top)

 4/21/03 merged current LW and MCL files, no diffs, updated copyright

|#

(in-package :boxer)



#|

The theory for build is that we cache a function on the editor box
which is the build template.  A Fast implementation of BUILD is basically
centered around minimizing the amount of the template hierarchy that
needs to be walked in order to find imbedded EVAL's and UNBOX's.  We
walk the template once (per modification) and generate a function that will
produce the correct output.

;;; For Example:
BUILD [ foo [] bar
        [ @wowie literal
          more stuff]
        the last row ]
would generate the function:


There are several parts to this file.

 o stack manipulation macros.
   For now, we use a separate set.  At some point in the future,
   we may want to use the boxer stack primitives.  We avoid it
   for now so that lossage is localized and also until we have
   a chance to think through the ramifications of having non
   boxer objects pushed onto the boxer stack (especially if a
   boxer error is signalled). Note: multiprocessing = Future

 o A parser.  This basically the same as the old build implementation
   except that it parses into postfix notation and refers to subprimitives
   which understand the stack convention

 o Build Sub Primitives

 o A code generator which takes the output of the parser and makes
   a "compiled" build object

 o Code which understands how to run a "compiled" build object.

|#



;;;; Compiled Build Objects

(defstruct (compiled-build-object (:include compiled-boxer-object)
				  (:conc-name cbo-)
				  (:constructor %make-compiled-build-object))
  (forms nil)
  (eval-list nil))

(defsubst cbo-code (x) (compiled-boxer-object-code x))
(defsubst cbo-args (x) (compiled-boxer-object-args x))

;;;; Formal Arg Objects

(defvar *build-function-arg-counter* 0)

(defvar *build-function-args* nil)
(defvar *build-function-values* nil)

(defmacro with-build-function-arg-collection (&body body)
  `(let ((*build-function-arg-counter* 0)
	 (*build-function-args* nil)
	 (*build-function-values* nil))
     (values (progn . ,body)
	     (nreverse *build-function-args*)
	     (let ((flattened-forms nil))
	       (dolist (value-form *build-function-values*)
		 (if (null (cdr value-form))
		     (push (car value-form) flattened-forms)
		     (dolist (value (nreverse value-form))
		       (push value flattened-forms))))
	       flattened-forms))))


(defstruct (build-function-arg (:predicate build-function-arg?))
  (argpos 0))

(defstruct (build-fun+args (:predicate build-fun+args?)
			   (:print-function %print-build-fun+args)
			   (:constructor %make-build-fun+args))
  (function nil)
  (args nil))

(defun %print-build-fun+args (o s &optional depth)
  (declare (ignore depth))
  (format s "#< ~A ~A >"
	  (build-fun+args-function o) (build-fun+args-args o)))

(defun make-build-fun+args (fun &rest args)
  (%make-build-fun+args :function fun :args args))


(defun new-build-arg ()
  (let ((newarg (make-build-function-arg :argpos
					 *build-function-arg-counter*)))
    (incf *build-function-arg-counter*)
    newarg))

(defun get-build-arg (value-form)
  (let ((prev-ref (position value-form *build-function-values* :test #'equal)))
    (cond ((null prev-ref)
	   (let ((new (new-build-arg)))
	     (push new *build-function-args*)
	     (push value-form *build-function-values*)
	     new))
	  (t (nth prev-ref *build-function-args*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For BUILD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-build-function-object (template)
  (multiple-value-bind (parsed-template internal-arglist forms-to-be-evaled)
      (walk-box-for-build template)
    (let* ((codevec (allocate-storage-vector 32))
	   (pretty-arglist (mapcar #'(lambda (x)(declare (ignore x)) (gensym))
				   internal-arglist))
	   (object (%make-compiled-build-object
		    :code codevec
		    :args pretty-arglist
		    :forms forms-to-be-evaled)))
      (generate-code parsed-template codevec (length internal-arglist))
      (unless (null internal-arglist)
	(setf (cbo-eval-list object)
	      (list*
	       (boxer-eval::make-compiled-boxer-function
		:arglist pretty-arglist
		:precedence 0
		:infix-p nil
		:object object)
	       forms-to-be-evaled)))
      object)))

(defun execute-compiled-build-function (cbf)
  (cond ((null (cbo-args cbf))
	 (funcall-0-arg-compiled-boxer-function cbf))
	(t
	 (boxer-eval::recursive-eval-setup-append (cbo-eval-list cbf)))))





;;;; Build support

(defvar *interpolate-empty-rows-too?* t)

;;; The actual instructions and subprimitives that build uses

(define-instruction (interpolate-rows 33) ()
  (let ((box (cbos-pop)))
    (flet ((handle-rows (rows &optional port-to? (superior box))
	     (if *interpolate-empty-rows-too?*
		 (mapcan #'(lambda (row)
			     (let ((entries (evrow-pointers row)))
			       (if (null entries)
				   (list nil)
				   (list (mapcar
					  #'(lambda (x)
					      (if port-to?
						  (port-to-item
						   x (or superior (vp-target box)) t)
						  (get-pointer-value x box)))
					  entries)))))
			 rows)
		 (mapcan #'(lambda (row)
			     (let ((entries (evrow-pointers row)))
			       (if (null entries)
				   nil
				   (list (mapcar
					  #'(lambda (x)
					      (if port-to?
						  (port-to-item
						   x (or superior (vp-target box)) t)
						  (get-pointer-value x box)))
					  entries)))))
			 rows))))
      (cbos-push
       (cond ((numberp box)
	      (list (list box)))
	     ((virtual-copy? box)
	      (handle-rows (vc-rows box)))
	     ((virtual-port? box)
	      (multiple-value-bind (rows inlinks? new? vc-rows-entry)
		  (get-box-rows (get-port-target box))
		(declare (ignore inlinks? new?))
	      (handle-rows rows t vc-rows-entry)))
	     (t (error "Don't know how to Unbox a ~S" box)))))))

;; gather the row-items by popping the stack n times...
(defun collect-n-list-from-stack (n)
  (let ((list-items nil))
    (dotimes (i n list-items)
      (setq list-items (nconc list-items (list (cbos-pop)))))))

#|
(defun collect-n-list-from-stack (n)
  (let ((list-items nil))
    (dotimes (i n (nreverse list-items))
      (push (cbos-pop) list-items))))
|#


(define-instruction (interpolate-n-unbox-results 34) (n)
  (let ((row-items (collect-n-list-from-stack n))
	(extra-rows-items nil)
	(items nil))
    (flet ((handle-extra-rows (x-rows)
	       (do* ((idx 0 (1+ idx))
		     (rows x-rows (cdr rows))
		     (row (car rows) (car rows)))
		    ((null rows))
		  (cond ((>= idx (length extra-rows-items))
			 ;; this must be the first case of an
			 ;; extra row this deep
			 (setq extra-rows-items
			       (nconc extra-rows-items (list row))))
			(t
			 ;; there must already be some row items from
			 ;; a previous unbox
			 (setf (nth idx extra-rows-items)
			       (nconc (nth idx extra-rows-items) row)))))))
    (dolist (item row-items)
      (cond ((listp item)
	     (setq items (nconc items (car item)))
	     (unless (null (cdr item))
	       ;; are there extra rows ?
	       (handle-extra-rows (cdr item))))
	    (t ;; justa' vanilla item
	     (setq items (nconc items (list item))))))
    (cbos-push
     (cond ((null extra-rows-items)
	    ;; don't make empty rows -- just return nil.
	    (if (null items)
		nil
		(make-evrow-from-entries items)))
	   (t (cons (make-evrow-from-entries items)
		    (mapcar  #'make-evrow-from-entries extra-rows-items))))))))

(defun pname-from-value (value)
  (when (or (symbolp value) (numberp value))
    (let ((string (format nil "~A" value)))
      (make-formatting-info :chas string :start 0 :stop (length string)))))

(define-instruction (stack-chunk 35) ()
  (let ((lfp (cbos-pop)) (value (cbos-pop)) (rfp (cbos-pop)))
    (cbos-push (make-chunk lfp (pname-from-value value) value rfp))))

;; because coallescing of token may fail (e.g. when one token turns
;; out to be a box), a value MAY be a list of several other values
(define-instruction (evrow-from-n-entries 36) (n)
  (let ((ptrs nil))
    (dotimes (i n)
      (let ((value (cbos-pop)))
	(setq ptrs (nconc ptrs (if (consp value)
				   (mapcar #'make-pointer value)
				   (list (make-pointer value)))))))
    (cbos-push (make-evrow-from-pointers ptrs))))

(define-instruction (stack-virtual-copy 37) ()
  (cbos-push (virtual-copy (cbos-pop) :top-level? (cbos-pop))))

;;; same as make-vc except that any lists encountered are assumed to be
;;; lists of evrows generated by the unbox handler and are flattened out.
;;; Also, removes NIL "rows," but always makes at least one row.
;;; Remember that (listp nil) is true.
(define-instruction (vc-for-build 38) (n-rows type name closet-p)
  (let* ((rows (collect-n-list-from-stack n-rows))
	 (closet (when closet-p (prog1 (car rows) (setq rows (cdr rows)))))
	 (vc (if (every #'(lambda (x) (not (listp x))) rows)
		 (make-vc rows type name)
		 (let ((new-rows nil))
		   (dolist (row rows)
		     (unless (null row)
		       (if (consp row)
			   (setq new-rows (nconc (nreverse row) new-rows))
			   (push row new-rows))))
		   (make-vc (if (null new-rows)
				(list (make-empty-evrow))
				(nreverse new-rows))
			    type name)))))
    (unless (null closet)
      (setf (vc-closets vc) closet) (setf (vc-progenitor vc) :new))
    (cbos-push vc)))

;; same as vc-for-build except that it expects the name to be on
;; the stack instead of in the arglist
(define-instruction (named-vc-for-build 39) (n-rows type closet-p)
  (let* ((name (intern-in-bu-package
		(string-upcase (box-text-string (cbos-pop)))))
	 (rows (collect-n-list-from-stack n-rows))
	 (closet (when closet-p (prog1 (car rows) (setq rows (cdr rows)))))
	 (vc (if (every #'(lambda (x) (not (listp x))) rows)
		 (make-vc rows type name)
		 (let ((new-rows nil))
		   (dolist (row rows)
		     (unless (null row)
		       (if (consp row)
			   (setq new-rows (nconc (nreverse row) new-rows))
			   (push row new-rows))))
		   (make-vc (if (null new-rows)
				(list (make-empty-evrow))
				(nreverse new-rows))
			    type name)))))
    (unless (null closet)
      (setf (vc-closets vc) closet) (setf (vc-progenitor vc) :new))
    (cbos-push vc)))

;; named ports, name from stack, target as immediate arg
(define-instruction (named-port-for-build 40) (target)
  (cbos-push (make-virtual-port :name (intern-in-bu-package
		                       (string-upcase
                                        (box-text-string (cbos-pop))))
                                :target target)))




;;;; Parsing a template

;;; these return 2 values: the result of the walk and a flag indicating
;;; whether or not imbedded atsigns or excls were encountered
;;; A row-form can have two possible representations:  In the simple case
;;; when there are no unbox's, then a row form will be somthing like...
;;; (make-evrow-from-pointers (list <thing> <box-form> <thing> ...))

(defun canonicalize-prop-for-eval (prop)
  (ecase prop
    (bu::eval-it 'bu::run)
    (bu::@   'bu::@)))

(defvar *eval-props-that-build-handles* '(bu::eval-it bu::@))

(defun build-eval-prop? (eval-prop)
  (fast-memq eval-prop *eval-props-that-build-handles*))

(defun walk-evrow-for-build (evrow sup-box ct)
  (let ((row-args nil)
	(constant-p t)
	(unmatched-ev-prop nil)
	(unbox-in-row? nil))
    (labels ((unify-ev-props (existing-props new-props)
	       (append existing-props new-props))
	     (expand-ev-props-into-code (props value chunk)
	       (cond ((null props)
		      (error
		       "Why are you trying to expand ev-props when there arent any"))
		     ((eq (car props) 'bu::eval-it)
		      (setq constant-p nil)
		      (get-build-arg
		       (list (if (null (cdr props))
				 (coerce-object-for-evaluator value)
				 (handle-eval-props (cdr props) value)))))
		     ((eq (car props) 'bu::@)
		      (setq constant-p nil unbox-in-row? t)
		      `(,(get-build-arg
			  (list (if (null (cdr props))
				    (coerce-object-for-evaluator value)
				    (handle-eval-props (cdr props) value))))
			 interpolate-rows))
		     (t chunk)))

	     (pointer-handler (ptr)
		 (let* ((raw-chunk (get-pointer-value ptr sup-box))
			(chunk-p (chunk-p raw-chunk))
			(value (if chunk-p
				   (chunk-chunk raw-chunk)
				   raw-chunk))
			(ev-props (when chunk-p
				    (getf (chunk-plist raw-chunk)
					  ':eval-prop))))
		   (flet ((value-handler ()
			    ;; returns either literal values or code to
			    ;; produce a value
			    (cond ((or (numberp value) (symbolp value))
				   raw-chunk)
				  ((or (virtual-port? value) (port-box? value))
                                   (multiple-value-bind (form port-constant-p)
                                       (walk-build-port value raw-chunk)
                                     (when (and constant-p
						(null port-constant-p))
                                       (setq constant-p port-constant-p))
                                     (cond ((or (null chunk-p) port-constant-p)
                                            form)
                                           (t
                                            `(,(chunk-right-format raw-chunk)
                                              ,form
                                              ,(chunk-left-format raw-chunk)
                                              stack-chunk)))))
				  ((or (virtual-copy? value) (box? value))
				   (multiple-value-bind (bf box-constant-p)
				       (walk-build-box-internal value ct t)
				     (when (and constant-p
						(null box-constant-p))
				       (setq constant-p box-constant-p))
				     (cond ((null chunk-p)
					    bf)
					   (t
					    `(,(chunk-right-format
						raw-chunk)
					      ,bf
					      ,(chunk-left-format raw-chunk)
					      stack-chunk)))))
				  (t
				   (error "Don't know what to do with ~S"
					  value)))))

			 (cond ((and (null value)
				     (not (null ev-props)))
				;; this should really unify with
				;; what's already there
				(setq unmatched-ev-prop
				      (unify-ev-props unmatched-ev-prop
						      ev-props))
				':ev-props-only)
			       ((and (null ev-props) (null unmatched-ev-prop))
				(value-handler))
			       ;; still need to put in code to preserve the
			       ;; formatting info when merging results of run
			       ;; time expressions
			       ((null ev-props)
				(let ((code (expand-ev-props-into-code
					     unmatched-ev-prop
					     value raw-chunk)))
				  (setq unmatched-ev-prop nil)
				  code))
			       (t
				;; must be a chunk with ev-props in it
				(let ((code (expand-ev-props-into-code
					     (unify-ev-props
					      unmatched-ev-prop
					      ev-props) value raw-chunk)))
				  (setq unmatched-ev-prop nil)
				  code)))))))
	    (dolist (pointer (evrow-pointers evrow))
	      (let ((ptr-val (pointer-handler pointer)))
		(unless (eq ptr-val ':ev-props-only)
		  (push ptr-val row-args))))
	    (cond ((not (null constant-p))
		   (values (let ((l (length row-args)))
			     (append row-args
				     (list (make-build-fun+args
					    'evrow-from-n-entries l))))
			   t))
		  ((null unbox-in-row?)
		   (values (let ((l (length row-args)))
			     (append row-args
				     (list (make-build-fun+args
					    'evrow-from-n-entries l))))
			   nil))
		   ;; must be an unbox in the row
		  (t
		   (values (let ((l (length row-args)))
			     (append row-args
				     (list (make-build-fun+args
					    'interpolate-n-unbox-results l))))
			   nil))))))

;;; hack preservation of names here...
(defun walk-build-box-internal (box &optional creation-time top-level?)
  (declare (values box-form constant-p))
  (let ((row-forms nil)
	(constant-p t)
	(closet-p nil)
	(ct (or creation-time
		(and (virtual-copy? box) (vc-creation-time box)))))
    (let ((closet (if (virtual-copy? box)
		      (vc-closets box)
		      (slot-value box 'closets))))
      (unless (null closet)
	(setq closet-p t)
	(unless (evrow? closet) (setq closet (make-evrow-from-row closet)))
	(multiple-value-bind (closet-form closet-constant-p)
	    (walk-evrow-for-build closet box ct)
	  (setq constant-p closet-constant-p)
	  (push closet-form row-forms))))
    (dolist (row (get-box-rows box creation-time))
      (multiple-value-bind (row-form row-constant-p)
	  (walk-evrow-for-build row box ct)
	(push row-form row-forms)
	(when (and constant-p
		   (null row-constant-p))
	  (setq constant-p row-constant-p))))
    (let ((l (length row-forms))
	  (box-type (if (virtual-copy? box)
			(vc-type box)
			(class-name (class-of box))))
	  (box-name (if (virtual-copy? box)
			(vc-name box)
			(editor-box-name-symbol box))))
      (cond ((char= (char (symbol-name box-name) 0) #\@)
	     ;; should check to see if the rest of the symbol
	     ;; is valid (no more ev prop chars)
	     (values
	      (append row-forms
		      (list (get-build-arg
			     (list (intern-in-bu-package
				    (subseq (symbol-name box-name)
					    (1+ (position #\@ (symbol-name
							       box-name)
							  :test #'char=))))))
			    (make-build-fun+args 'named-vc-for-build
						 l box-type closet-p)))
	      nil))
	    ((null constant-p)
	     (values
	      (append row-forms
		      (list (make-build-fun+args 'vc-for-build l
						 box-type box-name closet-p)))
	      constant-p))
	    (t
	     (values (if (null top-level?)
			 `(nil ,box stack-virtual-copy)
			 `(T   ,box stack-virtual-copy))
		     t))))))

(defun walk-build-port (port raw-chunk)
  (let ((name (if (virtual-port? port)
                  (vp-name port)
                  (editor-box-name-symbol port))))
    (cond ((char= (char (symbol-name name) 0) #\@)
           (values
            (list (get-build-arg
                   (list
                    (intern-in-bu-package
                     (subseq (symbol-name name)
                             (1+ (position #\@ (symbol-name name)
                                           :test #'char=))))))
                  (make-build-fun+args 'named-port-for-build
                                       (if (virtual-port? port)
                                           (vp-target port)
                                           (ports port))))
            nil))
          (t (values raw-chunk t)))))

;;; this is the top level function to call
(defun walk-box-for-build (box)
  (declare (values template-function-body
		   template-function-arglist
		   forms-to-be-evaled))
  (with-build-function-arg-collection
      (walk-build-box-internal box nil t)))




;;;; The top level interface...

;;; Caching compiled-build-objects onto editor structure

;;; the build cache needs to be immediately flushed on modified
;;; rather than waiting for the eval to finish (see the modified
;;; function in editor.lisp)
(defun get-cached-build-function (box)
  (getprop box 'cached-build-function))

(defun put-cached-build-function (box fun)
  (setf (build-template? box) t)	; once a template always a template ?
  (putprop box fun 'cached-build-function))

(defun decache-build-function (box)
  (when (getprop box 'cached-build-function)
    (putprop box nil 'cached-build-function)))

(defun build (template)
  (setq template (box-or-port-target template))
  (let* ((edbox (cond ((data-box? template) template)
		      ((fast-eval-data-box? template)
		       (vc-progenitor template))))
	 (cached-function (unless (null edbox)
			    (get-cached-build-function edbox))))
    (cond ((not (null cached-function))
	   (execute-compiled-build-function cached-function))
	  (t
	   (let ((new-fun (make-build-function-object template)))
	     (unless (or (null edbox)
			 (not (editor-box-cacheable?
			       edbox
			       (or (and (virtual-copy? template)
					(vc-creation-time template))
				   (now)))))
	       (put-cached-build-function edbox new-fun))
	     (execute-compiled-build-function new-fun))))))


;;;; and into Boxer we go.....

(boxer-eval::defboxer-primitive bu::build (template)
  (if (numberp template) template (build template)))
