;;; -*- Package: EVAL; Mode: LISP; Base: 10; Syntax: Common-lisp -*-
;;;
;;; $Header: prims.lisp,v 1.0 90/01/24 22:15:44 boxer Exp $
;;;
;;; $Log:	prims.lisp,v $
;;;Revision 1.0  90/01/24  22:15:44  boxer
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



Modification History (most recent at the top)

 9/30/05 changed rounding function on REPEAT arg to round up for x.5 values
 7/17/04 Fixed copyright error message in ANY-OF and ALL-OF
         changed ALL-OF & EVERY to hack the empty box case
 2/11/03 merged current LW and MCL files
 9/05/02 UC free version for ANY/ALL-OF and new replacements, SOME & EVERY
 1/25/02 make sure that we have a ufun-frame in find-ufun-frame-with-name
         before asking for ufun frame slots
 2/17/01 merged current LW and MCL files
10/23/99 genericized the index arithmetic in bu::repeat because of reports
         of users running into the fixnum size limit
 4/21/99 implemented ANY-OF and ALL-OF
 4/21/99 Start logging changes: source = boxer version 2.3

|#

;;;

(in-package :boxer-eval)

;;; This file contains really fundamental primitives and support for those
;;; primitives.

(defvar *true*)
(defvar *false*)

(defun boxer-boolean (t-or-nil)
  (if t-or-nil *true* *false*))

(defun TRUE? (true-or-false)
  (and (not (numberp true-or-false))
       (cond ((eq true-or-false *true*) t)
	     ((eq true-or-false *false*) nil)
	     (t (equal '(bu::true) (boxer::flat-box-items true-or-false))))))

(defun FALSE? (true-or-false)
  (and (not (numberp true-or-false))
       (cond ((eq true-or-false *false*) t)
	     ((eq true-or-false *true*) nil)
	     (t (equal '(bu::false) (boxer::flat-box-items true-or-false))))))

(defboxer-primitive bu::true ()
  *true*)

(defboxer-primitive bu::false ()
  *false*)

(defboxer-primitive bu::else ()
  *true*)

(defboxer-primitive %novalue-internal ()
  boxer-eval::*novalue*)


(DEFBOXER-PRIMITIVE BU::PORT-TO ((BU::PORT-TO ARG))
  ARG)

(DEFBOXER-PRIMITIVE BU::DONT-PORT ((BU::DONT-PORT ARG))
  ARG)

(DEFBOXER-PRIMITIVE BU::DATAFY ((BU::DATAFY ARG))
  ARG)

(DEFBOXER-PRIMITIVE BU::SELF ()
  (BOXER::PORT-TO *LEXICAL-VARIABLES-ROOT*))

(defboxer-primitive bu::copy (box)
  (if (or (port-box? box) (and (vectorp box) (fast-eval-port-box? box)))
      (copy-thing (boxer::get-port-target box))
      box))

;;; Below are primitives that do hairy recursive invocations of the evaluator
;;; and aren't defined with DEFRECURSIVE-FUNCALL-PRIMITIVE.

; **********the below comment is out of date**********
;;; The primitives must be split into three parts.  The primitive itself
;;; contains code that should be run before the recursive invocation
;;; of the evaluator.  It should end by pushing a special frame
;;; saving at least *RUNNING-LIST* and *SPECIAL-FRAME-HANDLER*
;;; and setting those values to the list to be evaluated and
;;; the after-evaluation function, respectively.  The primitive
;;; must return *NOVALUE*.  The after-evaluation function should
;;; pop the special frame and then do the things the primitive
;;; does after the recursive eval, and then return NIL.  Primitives
;;; that want to repeat the *RUNNING-LIST* should, at this stage,
;;; not pop the frame and return T instead of NIL.
;;; The flag or function controlling the looping should be a global variable
;;; whose old value is saved in the special frame.
;;;
;;; Primitives which simply have no effects to be undone and don't need
;;; to repeat the list can instead set *sfun-continuation* to
;;; *macroexpand-sfun-continuation* and return a BOXER data
;;; structure to be evaluated.  RUN and IF use this method.

;;; IF
;; IF isn't done right yet.  What want is something like variable number of
;; args. What we've got is list-rest stuff.  We can try separating things out
;; a la Common Lisp -- WHEN, UNLESS, and IF.
(DEFBOXER-PRIMITIVE BU::IF ((dont-copy PREDICATE)
			    (LIST-REST CONSEQUENT-AND-ALTERNATIVE))
  (if (not (null (cddr consequent-and-alternative)))
      (signal-error :if-format "there are too many things on the line.")
      (cond ((TRUE? PREDICATE)
	     (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
	     (car consequent-and-alternative))
	    ((not (false? predicate))
	     (signal-error :NOT-TRUE-OR-FALSE predicate))
	    ((NOT (NULL (CDR CONSEQUENT-AND-ALTERNATIVE)))
	     (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
	     (cadr consequent-and-alternative))
	    (T *NOVALUE*))))

#|
;; The old version of IFS wasn't right because recursive-eval doesn't work
;;  right.  If you have a box with any false predicate then the rest of the
;; line after the IFS won't get executed.

;;; Right now I'm trying saving the rest of the IFS line and passing it through
;;; every time.  Unfortunately, that means some copying and some abstraction
;;; violations in accessing the "rest" of the stuff.

;;; It would be better to implement recursive-eval properly, or to flush it and
;;; do the (current) right thing:
;;; recursive funcall: funcalls a function of no args.
;;;                    may return a value.
;;;                    treated as a mini-doit-box which may return a value
;;; recursive eval:    evaluates an arbitrary, self-contained list.
;;;                    last expression may generate a value.
;;;                    treated as a mini-line which returns a value.
;;; macroexpand:       evaluates a list spliced into the token stream.
;;;                    may return a value.
;;; trigger funcall:   funcalls a function of no args.
;;;                    returns no value.  does not appear as an sub-expression
;;;                    treated as a mini-doit-box which is executed separately.
;;; trigger eval:      evaluates an arbitrary, self-contained list.
;;;                    generates no value. Does not appear as an sub-expression
;;;                    treated as a mini-line which is executed separately.
|#

;; dont-copy may be the wrong thing here.
(defrecursive-eval-primitive bu::ifs ((dont-copy box)
				      (list-rest rest-of-line-must-be-empty))
  :state-variables (*ifs-list*)
  :before
  (cond
    ((not (null rest-of-line-must-be-empty))
     (signal-error :ifs-bug
		   "IFS statements must appear on lines by themselves."))
    ((or (numberp box) (not (fast-eval-data-box? box)))
     (signal-error :ifs-format "expects a data box"))
    (t (let* ((code (interpreted-boxer-function-text
	             (if (boxer::virtual-copy? box)
	                 (cached-code-virtual-copy box)
	                 (cached-code-editor-box box))))
              (1stline (do ((line (car code) (car code)))
			   ((null code) nil)
			 (if (null line) (pop code) (return (pop code))))))
         (if (null 1stline)
             boxer-eval::*novalue*
             (progn
               (set-and-save-state-variables code)
	       (recursive-eval-invoke
                (list* 'boxer-eval::%ifs-internal-1 1stline)))))))
  :after (cond ((null *ifs-list*)
		(restore-state-variables) nil)
	       (t (let ((nextline (do ((line (car *ifs-list*)
					     (car *ifs-list*)))
				      ((null *ifs-list*) nil)
				    (if (null line) (pop *ifs-list*)
					(return (pop *ifs-list*))))))
		    (cond ((null nextline)
			   (restore-state-variables) nil)
			  (t
			   (cons 'boxer-eval::%ifs-internal-1 nextline)))))))


(defboxer-primitive boxer-eval::%ifs-internal-1 ((dont-copy predicate)
					 (list-rest consequent))
  (cond ((false? predicate) *novalue*)
	((true? predicate)
	 (setq *ifs-list* nil)
	 (setq *sfun-continuation* '*run-list-sfun-continuation*)
	 consequent)
	(t (signal-error :ifs-format predicate "neither true nor false"))))

(defboxer-primitive bu::when ((dont-copy predicate)
			      (list-rest consequent))
  (cond ((false? predicate) *novalue*)
	((true? predicate)
	 (setq *sfun-continuation* '*run-list-sfun-continuation*)
	 consequent)
	(t (signal-error :when-format predicate "neither true nor false"))))

(defboxer-primitive bu::unless ((dont-copy predicate)
			      (list-rest consequent))
  (cond ((false? predicate)
	 (setq *sfun-continuation* '*run-list-sfun-continuation*)
	 consequent)
	((true? predicate) *novalue*)
	(t (signal-error :unless-format predicate "neither true nor false"))))

(defboxer-primitive bu::and ((dont-copy a) (dont-copy b))
 (cond ((true? a)
	 (cond ((true? b) (boxer-boolean t))
	       ((false? b) (boxer-boolean nil))
	       (t (signal-error :NOT-TRUE-OR-FALSE b))))
	((false? a)
	 (cond ((or (true? b) (false? b)) (boxer-boolean nil))
	       (t (signal-error :NOT-TRUE-OR-FALSE b))))
	(t (signal-error :NOT-TRUE-OR-FALSE a))))

(defboxer-primitive bu::or ((dont-copy a) (dont-copy b))
  (cond ((true? a)
	 (cond ((or (true? b) (false? b))
		(boxer-boolean t))
	       (t (signal-error :NOT-TRUE-OR-FALSE b))))
	((false? a)
	 (cond ((true? b) (boxer-boolean t))
	       ((false? b) (boxer-boolean nil))
	       (t (signal-error :NOT-TRUE-OR-FALSE b))))
	(t (signal-error :NOT-TRUE-OR-FALSE a))))

(defboxer-primitive (bu::not 0) ((dont-copy true-or-false))
  (cond ((true? true-or-false) (boxer-boolean nil))
	((false? true-or-false) (boxer-boolean t))
	(t (signal-error :NOT-TRUE-OR-FALSE true-or-false))))

(defrecursive-eval-primitive bu::any-of ((dont-copy box)
                                         (list-rest rest-of-line-must-be-empty))
  :state-variables (*boolean-clauses*)
  :before
  (cond ((not (null boxer::*uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::any-of " is no longer available, use "
                                       'bu::some " instead"))
        (t
         (cond ((not (null rest-of-line-must-be-empty))
                (signal-error
                 :any-of-bug
                 "ANY-OF statements must appear on lines by themselves."))
               ((or (numberp box) (not (fast-eval-data-box? box)))
                (signal-error :any-of-bug "expects a data box"))
               (t (let* ((code (interpreted-boxer-function-text
	                        (if (boxer::virtual-copy? box)
	                            (cached-code-virtual-copy box)
	                          (cached-code-editor-box box))))
                         (1stline (do ((line (car code) (car code)))
			              ((null code) nil)
			            (if (null line)
                                        (pop code)
                                      (return (pop code))))))
                    (if (null 1stline) *false*
                      (progn
                        (set-and-save-state-variables code)
                        (recursive-eval-invoke
                         (list* 'boxer-eval::any-of 1stline)))))))))
  :after (cond ((null *boolean-clauses*)
                (restore-state-variables) nil)
               (t (let ((nextline (do ((line (car *boolean-clauses*)
					     (car *boolean-clauses*)))
				      ((null *boolean-clauses*) nil)
				    (if (null line) (pop *boolean-clauses*)
					(return (pop *boolean-clauses*))))))
		    (cond ((null nextline)
			   (restore-state-variables) nil)
			  (t
			   (cons 'boxer-eval::any-of nextline)))))))

;; give helper function same name in eval package is a crock to make
;; the error message come out right
(defboxer-primitive boxer-eval::any-of ((dont-copy clause) (list-rest ignore))
  ignore ; bound but not used blah blah...
  (cond ((true? clause)
         (setq *boolean-clauses* nil) *true*)
        ((false? clause) *false*)
        (t (signal-error :any-of clause "neither true nor false"))))

(defrecursive-eval-primitive bu::some ((dont-copy box)
                                       (list-rest rest-of-line-must-be-empty))
  :state-variables (*boolean-clauses*)
  :before
  (cond ((not (null rest-of-line-must-be-empty))
         (signal-error
          :some-of-bug
          "SOME statements must appear on lines by themselves."))
        ((or (numberp box) (not (fast-eval-data-box? box)))
         (signal-error :some-bug "expects a data box"))
        (t (let* ((code (interpreted-boxer-function-text
                         (if (boxer::virtual-copy? box)
                             (cached-code-virtual-copy box)
                           (cached-code-editor-box box))))
                  (1stline (do ((line (car code) (car code)))
                               ((null code) nil)
                             (if (null line)
                                 (pop code)
                               (return (pop code))))))
             (if (null 1stline) *false*
               (progn
                 (set-and-save-state-variables code)
                 (recursive-eval-invoke
                  (list* 'boxer-eval::some-of 1stline)))))))
  :after (cond ((null *boolean-clauses*)
                (restore-state-variables) nil)
               (t (let ((nextline (do ((line (car *boolean-clauses*)
					     (car *boolean-clauses*)))
				      ((null *boolean-clauses*) nil)
				    (if (null line) (pop *boolean-clauses*)
					(return (pop *boolean-clauses*))))))
		    (cond ((null nextline)
			   (restore-state-variables) nil)
			  (t
			   (cons 'boxer-eval::some-of nextline)))))))

;; give helper function same name in eval package is a crock to make
;; the error message come out right
(defboxer-primitive boxer-eval::some-of ((dont-copy clause) (list-rest ignore))
  ignore ; bound but not used blah blah...
  (cond ((true? clause)
         (setq *boolean-clauses* nil) *true*)
        ((false? clause) *false*)
        (t (signal-error :some-of clause "neither true nor false"))))

(defrecursive-eval-primitive bu::all-of ((dont-copy box)
                                         (list-rest rest-of-line-must-be-empty))
  :state-variables (*boolean-clauses*)
  :before
  (cond ((not (null boxer::*uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::all-of " is no longer available, use "
                                       'bu::every " instead"))
        (t
         (cond ((not (null rest-of-line-must-be-empty))
                (signal-error
                 :all-of-bug
                 "ALL-OF statements must appear on lines by themselves."))
               ((or (numberp box) (not (fast-eval-data-box? box)))
                (signal-error :all-of-bug "expects a data box"))
               (t (let* ((code (interpreted-boxer-function-text
	                        (if (boxer::virtual-copy? box)
	                            (cached-code-virtual-copy box)
	                          (cached-code-editor-box box))))
                         (1stline (do ((line (car code) (car code)))
			              ((null code) nil)
			            (if (null line)
                                        (pop code)
                                      (return (pop code))))))
                    (if (null 1stline) *true*
                      (progn
                        (set-and-save-state-variables code)
                        (recursive-eval-invoke
                         (list* 'boxer-eval::all-of 1stline)))))))))
  :after (cond ((null *boolean-clauses*)
                (restore-state-variables) nil)
               (t (let ((nextline (do ((line (car *boolean-clauses*)
					     (car *boolean-clauses*)))
				      ((null *boolean-clauses*) nil)
				    (if (null line) (pop *boolean-clauses*)
					(return (pop *boolean-clauses*))))))
		    (cond ((null nextline)
			   (restore-state-variables) nil)
			  (t
			   (cons 'boxer-eval::all-of nextline)))))))

(defboxer-primitive boxer-eval::all-of ((dont-copy clause) (list-rest ignore))
  ignore
  (cond ((true? clause) *true*)
        ((false? clause) (setq *boolean-clauses* nil) *false*)
        (t (signal-error :any-of clause "neither true nor false"))))

(defrecursive-eval-primitive bu::every ((dont-copy box)
                                         (list-rest rest-of-line-must-be-empty))
  :state-variables (*boolean-clauses*)
  :before
  (cond ((not (null rest-of-line-must-be-empty))
         (signal-error :every-bug
		       "EVERY statements must appear on lines by themselves."))
        ((or (numberp box) (not (fast-eval-data-box? box)))
         (signal-error :every-bug "expects a data box"))
        (t (let* ((code (interpreted-boxer-function-text
	                 (if (boxer::virtual-copy? box)
	                   (cached-code-virtual-copy box)
	                   (cached-code-editor-box box))))
                  (1stline (do ((line (car code) (car code)))
			       ((null code) nil)
			     (if (null line) (pop code) (return (pop code))))))
             (if (null 1stline) *true*
                 (progn
                   (set-and-save-state-variables code)
                   (recursive-eval-invoke (list* 'boxer-eval::every-1 1stline)))))))
  :after (cond ((null *boolean-clauses*)
                (restore-state-variables) nil)
               (t (let ((nextline (do ((line (car *boolean-clauses*)
					     (car *boolean-clauses*)))
				      ((null *boolean-clauses*) nil)
				    (if (null line) (pop *boolean-clauses*)
					(return (pop *boolean-clauses*))))))
		    (cond ((null nextline)
			   (restore-state-variables) nil)
			  (t
			   (cons 'boxer-eval::every-1 nextline)))))))

;; Can't use the same name because of conflict with lisp:every
(defboxer-primitive boxer-eval::every-1 ((dont-copy clause) (list-rest ignore))
  ignore
  (cond ((true? clause) *true*)
        ((false? clause) (setq *boolean-clauses* nil) *false*)
        (t (signal-error :every clause "neither true nor false"))))


;;; RUN
(DEFBOXER-PRIMITIVE BU::RUN (DATA-BOX)
  (setq *sfun-continuation* '*ufuncall-sfun-result-sfun-continuation*)
  (convert-data-to-function data-box))

#|
(defrecursive-eval-primitive bu::tell ((bu::port-to who) (list-rest what))
  :stack-frame-allocation (10 5 10 10)
  :state-variables (*dynamic-variables-root* *lexical-variables-root*)
  :before (progn
	    (set-and-save-state-variables nil (boxer::box-or-port-target who))
	    (recursive-eval-invoke what))
  :after (progn (restore-state-variables) nil))
|#

;; IF the WHO has a running process, we need to use that process' bindings

(defrecursive-funcall-primitive bu::tell ((bu::port-to who) (list-rest what))
  :stack-frame-allocation (10 5 10 10)
  ;; for now, to make ! work with TELL, we add these *OLD- variables.
  :state-variables (*old-dynamic-variables-bottom* *old-lexical-variables-root*
		    *old-tell-special-frame*
 		    *dynamic-variables-bottom* *lexical-variables-root*)
  :before (progn
	    (set-and-save-state-variables
	     *dynamic-variables-bottom* *lexical-variables-root*
	     *PDL*			;; *PDL* will be this new stack frame.
	     *dynamic-variables-top* (boxer::box-or-port-target who))
	    (recursive-funcall-invoke
	      (if (or (not (null (cdr what)))
		      (symbolp (car what))
		      (numberp (car what))
		      (not (fast-eval-doit-box? (car what))))
		  (make-interpreted-procedure-from-list (list what))
		  (cached-code-virtual-copy (car what)))))

  :after
    (progn (restore-state-variables) nil))

;;; ASK is a synonym for TELL; the code is exactly the same.
;;; defboxer-funcall-primitive-alias is too hard to write.
(defrecursive-funcall-primitive bu::ask ((bu::port-to who) (list-rest what))
  :stack-frame-allocation (10 5 10 10)
  ;; for now, to make ! work with TELL, we add these *OLD- variables.
  :state-variables (*old-dynamic-variables-bottom* *old-lexical-variables-root*
		    *old-tell-special-frame*
 		    *dynamic-variables-bottom* *lexical-variables-root*)
  :before (progn
	    (set-and-save-state-variables
	     *dynamic-variables-bottom* *lexical-variables-root*
	     *PDL*			;; *PDL* will be this new stack frame.
	     *dynamic-variables-top* (boxer::box-or-port-target who))
	    (recursive-funcall-invoke
	      (if (or (not (null (cdr what)))
		      (symbolp (car what))
		      (numberp (car what))
		      (not (fast-eval-doit-box? (car what))))
		  (make-interpreted-procedure-from-list (list what))
		  (cached-code-virtual-copy (car what)))))

  :after
    (progn (restore-state-variables) nil))


;;;
;;; REPEAT
;;;
(defrecursive-eval-primitive bu::repeat ((NUMBERIZE TIMES) (LIST-REST WHAT))
  :stack-frame-allocation (10 5 10 10)
  :state-variables (*repeat-list* *repeat-count*)
  :before (progn
	    (cond ((complexp times)
		   (signal-error :IMPROPER-ARGUMENT
				 "cannot accept complex numbers"))
		  ((minusp times)
		   (signal-error :IMPROPER-ARGUMENT
				 "expected a positive number"))
		  ((zerop (SETQ times (floor (+ times .5)))) boxer-eval::*novalue*)
		  (t (set-and-save-state-variables what (1- times))
		     (recursive-eval-invoke what))))
  :after  (cond ((zerop *repeat-count*)
		 (restore-state-variables)
		 nil)
		(t (decf *repeat-count*)
		   *repeat-list*)))

(defrecursive-eval-primitive bu::loop ((list-rest what))
  :state-variables (*repeat-list*)
  :before (progn (set-and-save-state-variables what)
		 (recursive-eval-invoke what))
  :after *repeat-list*)


;;; FOR-EACH-ITEM does ((CHANGE VARIABLE ITEM n BOX) (@WHAT)) for each item
;;; in the box.  As a result, the variable will contain the boxed item.
;;; An @ or UNBOX will be necessary for dereference.
;;; Words and numbers may appear in the box, but @ or UNBOX will barf on them,
;;; so you will need to do IF UNBOXABLE? VARIABLE ...
;;; See FOR-EACH-BOX below.
(defrecursive-eval-primitive bu::for-each-item ((bu::port-to variable)
						box (list-rest what))
  :state-variables (*for-each-variable-object*
		    *for-each-counter*
		    *for-each-length*
		    *for-each-box*
		    *for-each-list*)
  :before (let ((variable-object (boxer::get-port-target variable))
		(length (if (numberp box) 1
			    (boxer::box-length-in-elements box))))
	    (cond ((zerop& length) *novalue*)
		  (t (boxer::change variable-object (boxer::item 1 box))
		     (set-and-save-state-variables
		      variable-object
		      1
		      length
		      box
		      what)
		     (recursive-eval-invoke what))))
  :after (cond ((=& *for-each-counter* *for-each-length*)
		(restore-state-variables)
		nil)
	       (t (incf *for-each-counter*)
		  (boxer::change *for-each-variable-object*
				 (boxer::item *for-each-counter* box))
		  *for-each-list*)))

#|
;; THIS IS BROKEN --- DO NOT USE IT.
;; it's conceptually broken, too:
;; %unboxable? return nil on empty data boxes, so it skips empty boxes.
;; for another thing, there's the data/doit thing:
;; %unboxable? returns nil on doit boxes, so it skips
;; doit boxes.  maybe that's the right thing, but
;; maybe it's better to force people to think about it
;; by using the the FOR-EACH-ITEM version.

;;; FOR-EACH-BOX puts the box contents of each box item into the
;;; variable directly.  Numbers and words are just skipped.
;;; FOR-EACH-BOX either is a bad idea or is a good idea.  It would be
;;; nice not to have to box and unbox the object.  We could do it with
;;; a %ITEM-OBJECT routine which would return a copy of the object
;;; itself, but that's hard in the current VC interface.

(defrecursive-eval-primitive bu::for-each-box ((bu::port-to variable)
					       box (list-rest what))
  :state-variables (*for-each-variable-object*
		    *for-each-counter*
		    *for-each-length*
		    *for-each-box*
		    *for-each-list*)
  :before (let ((variable-object (boxer::get-port-target variable))
		(length (if (numberp box) 1
			    (boxer::box-length-in-elements box))))
	    (do ((start-item-number 1 (1+& start-item-number)))
		((=& start-item-number length) *novalue*)
	      (let ((object (boxer::item start-item-number box)))
		(when (boxer::%unboxable? object)
		  (boxer::change variable-object (boxer::%unbox object))
		  (set-and-save-state-variables
		   variable-object
		   start-item-number
		   length
		   box
		   what)
		  (return (recursive-eval-invoke what))))))
  :after (do ((next-item-number *for-each-counter*
				(1+& next-item-number)))
	     ((=& next-item-number *for-each-length*)
	      (restore-state-variables)
	      nil)
	   (let ((object (boxer::item next-item-number box)))
	     (when (boxer::%unboxable? object)
	       (setq *for-each-counter* next-item-number)
	       (boxer::change *for-each-variable-object*
			      (boxer::%unbox object))))
	   (return *for-each-list*)))
|#

(defrecursive-eval-primitive bu::for-each-row ((bu::port-to variable) box (list-rest what))
  :state-variables (*for-each-variable-object*
		    *for-each-counter*
		    *for-each-length*
		    *for-each-box*
		    *for-each-list*)
  :before (let ((variable-object (boxer::get-port-target variable)))
	    (boxer::change variable-object (boxer::nth-row 1 box))
	    (set-and-save-state-variables
	     variable-object
	     1
	     (if (numberp box) 1 (boxer::number-of-rows box))
	     box
	     what)
	    (recursive-eval-invoke what))
  :after (cond ((=& *for-each-counter* *for-each-length*)
		(restore-state-variables)
		nil)
	       (t (incf *for-each-counter*)
		  (boxer::change *for-each-variable-object*
				 (boxer::nth-row *for-each-counter* box))
		  *for-each-list*)))




;;;
;;; @ (unbox)
;;;

;;; UNBOX (@) is an implied EVAL followed by an UNBOX, with the result
;;; stuck into the token stream.
;;; 1. For symbols and self-evaluating objects we can TRY to skip the
;;;    full EVAL phase:
;;;   If the object to unbox is a symbol, we get the value immediately and
;;;    continue unboxing. If the object is a number, then the result is the
;;;    number and we dispatch to NUMBER, skipping the rest of the @ process
;;;    because numbers are self-evaluating and not unboxable.
;;;   Otherwise the object is a self-evaluating box.  To do the recursive eval,
;;;    we make a procedure out of its contents and run that contents as if the
;;;    box were invisible.  (Later if we have a variable for the box being
;;;    funcalled, we must make sure that variable doesn't get bound.)
;;; 2. Then we take that result and put it into the token stream.
;;;   If the result is a DATA box we dispatch to DATA-BOX.  If the result is a
;;;    number of tokens, we must put them into a procedure and put that
;;;    procedure into the token stream.
;;; Notes:
;;; When we get a self-evaluating box, we strip away the box and put its
;;; contents into a procedure.  We can do some hackery here to make the simple
;;; unbox case simple -- if there is only one box inside the box and it's a
;;; self-evaluating object then we just return that object.
;;; MULTIPLE @ isn't done.
;;; The interaction between &REST and @ and DATAFY and @ isn't done.
;;; Locals inside the data box aren't deal with at all.

(defboxer-primitive bu::@ (thing)
  (cond ((numberp thing) thing)
	((fast-eval-data-box? thing)
	 (setq *sfun-continuation* '*run-list-sfun-continuation*)
	 (nconc (boxer::flat-box-items-copy-for-atsign thing)
		*executing-pointer*))
	((fast-eval-port-box? thing)
	 (setq *sfun-continuation* '*run-list-sfun-continuation*)
	 (nconc (boxer::flat-box-unport-for-atsign thing) *executing-pointer*))
	 (t (error "@ of a ~S object is not yet supported." (TYPE-OF THING)))))

;; this doesn't really copy -- in fact, @ isn't supposed to copy, is it?
(defun boxer::flat-box-items-copy-for-atsign (vc)
  (mapcan #'(lambda (evrow)
	      (boxer::process-pointer-values-for-eval
	       (mapcar #'(lambda (element)
			   (boxer::get-pointer-value element vc))
		       (boxer::evrow-pointers evrow))))
	  (boxer::get-box-rows vc)))


;; @ of a port.
;; this is probably not quite right yet.
;; it would be nice to get rid of the case dispatch.
(defun boxer::flat-box-unport-for-atsign (vp)
  (let ((list (boxer::flat-box-items-copy-for-atsign
	       (boxer::vp-target vp))))
    (do ((tail list (cdr tail))
	 (item (car list) (car tail)))
	((null tail) list)
      (cond ((or (symbolp item) (numberp item) (boxer::virtual-port? item)
		 (special-token? item))
	     item)
	    ((boxer::port-box? item)
	     (setf (car tail)
		   (boxer::make-virtual-port-from-editor-port item)))
	    ((boxer::virtual-copy? item)
	     (setf (car tail)
		   (boxer::port-to item)))
	    ((boxer::box? item)
	     (setf (car tail)
		   (boxer::port-to item)))
	    (t (primitive-signal-error
		:atsign-bug
		"Unrecognized object" item "in @"))))))
;;; STOP
;;; STOP takes an optional box name to stop (i.e., like a throw tag)
;;; The stopped procedure returns the last value available, even if
;;; it was on a previous line.  Is that what we want?
;;; check lexical-funcall frames to make sure they work with stop.

(defboxer-primitive bu::stop ((list-rest possible-procedure-name))
  (when (not (null *current-function*))
    (primitive-signal-error :DOESN\'T-OUTPUT))
  (cond ((not (null possible-procedure-name))
	 (if (null (cdr possible-procedure-name))
	     (unwind-to-tag (car possible-procedure-name))
	     (primitive-signal-error
	      :excess-args-error "needs at most one DOIT-BOX name as input")))
	(t (unwind-to-frame (find-next-ufun-frame *pdl*))))
  (setq *executing-pointer* nil)
  (setq *executing-line* '(nil))
  *returned-value*)

;;; STOP-LOOP
;;; Probably needs some more reality checking
(defboxer-primitive bu::stop-loop ()
  (unwind-to-loop-frame)
  *returned-value*)

(defun unwind-to-top-level ()
  (do ()
      ((null *pdl*))
    (funcall (get-stack-frame-unwinder (stack-frame-type *pdl*)))))

;; Unwind up to and including the ufun frame with the procedure named by tag,
;; followed by any associated special frames.
;; That will put you back in the context of the procedure you want to stop.
;; Unwind-to-tag's caller must set the body to nil after that.
(defun unwind-to-tag (tag)
  (let ((frame (find-ufun-frame-with-name tag)))
    (when (null frame) (primitive-signal-error :no-such-procedure-to-stop tag))
    ;; up to:
    (unwind-to-frame frame))
  ;; including:
  (funcall (get-stack-frame-unwinder (stack-frame-type *pdl*)))
  ;; followed by:
  (do ()
      ((eq (stack-frame-type *pdl*) 'ufun-frame))
    (if (not (null *pdl*))
        (funcall (get-stack-frame-unwinder (stack-frame-type *pdl*)))
        (return))))

(defun unwind-to-loop-frame ()
  (let ((frame (find-loop-frame *pdl*)))
    (when (null frame) (primitive-signal-error :not-in-a-looping-procedure))
    ;; up to:
    (unwind-to-frame frame))
  ;; including:
  (funcall (get-stack-frame-unwinder (stack-frame-type *pdl*)))
  ;; followed by:
  (do ()
      ((eq (stack-frame-type *pdl*) 'ufun-frame))
    (if (not (null *pdl*))
        (funcall (get-stack-frame-unwinder (stack-frame-type *pdl*)))
        (return))))

(defun unwind-to-frame (frame)
  (do ()
      ((eq frame *pdl*))
    (when (not (null *pdl*))
      (funcall (get-stack-frame-unwinder (stack-frame-type *pdl*))))))

(defun find-next-ufun-frame (start-frame)
  (do ((frame start-frame (stack-frame-next-frame frame)))
      ((null frame) nil)
    (when (ufun-frame-p frame) (return frame))))

(defun find-ufun-frame-with-name (tag)
  (if (eq tag (crock-boxer-fun-box-name *executing-function*))
      (find-next-ufun-frame *pdl*)
      (do ((frame *pdl* (find-next-ufun-frame (stack-frame-next-frame frame))))
	  ((null frame) nil)
	(let ((fun (when (ufun-frame-p frame)
                     ;; make sure that we have a ufun-frame
                     (dbg-ufun-frame-*executing-function* frame))))
	  (when (and fun (eq tag (crock-boxer-fun-box-name fun)))
	   (return frame))))))

(defun find-loop-frame (start-frame)
  (do ((frame start-frame (stack-frame-next-frame frame)))
      ((null frame) nil)
    (when (or (repeat-special-frame-p frame)
              (loop-special-frame-p   frame))
      (return frame))))

;;; Errors

(defrecursive-eval-primitive bu::handle-errors ((dont-copy handlers)
                                                (list-rest body))
  :state-variables (*error-handler-list* *error-signalled?*)
  :before (progn
            (set-and-save-state-variables (parse-error-handlers handlers) nil)
            (recursive-eval-invoke body))
  :after (cond ((null *error-signalled?*)
                (restore-state-variables)
                nil)
               (t ;; *error-signalled?* should have a list of handler and
                  ;; the error box
                  (prog1 *error-signalled?*
                    (setq *error-signalled?* nil)))))

(defun parse-error-handlers (handlers-box)
  (let ((ftext (interpreted-boxer-function-text
                (if (boxer::virtual-copy? handlers-box)
                    (cached-code-virtual-copy handlers-box)
                    (cached-code-editor-box handlers-box))))
        (parsed-handlers nil))
    (dolist (line ftext (nreverse parsed-handlers))
      (cond ((null (cdr line))
             (signal-error
              :handler-errors-format
              "Handler lines should be pairs of error-type(s) and handlers"
              handlers-box))
            (t (push (list*
                      (cond ((symbolp (car line))
                             (intern (symbol-name (car line))
                                     (find-package "KEYWORD")))
                            ((or (boxer::box? (car line))
                                 (boxer::virtual-copy? (car line)))
                             (mapcar #'(lambda (x)
                                         (intern (symbol-name x)
                                                 (find-package "KEYWORD")))
                                     (boxer::flat-box-items (car line))))
                            (t (signal-error
                                :handler-errors-format
                                "error types should be a name or a box of names")))
                      (cdr line))
                     parsed-handlers))))))

(defun find-error-handler (error-type start-frame)
  (do ((handler-frame (find-error-handler-frame start-frame)
                      (find-error-handler-frame
                       (stack-frame-next-frame handler-frame)))
       (handlers *error-handler-list*
                 ;; Note: this grabs the value from the PREVIOUS frame (do NOT do*)
                 (dbg-handle-errors-special-frame-*error-handler-list*
                  handler-frame)))
      ((null handler-frame) nil)
    (let ((handler (get-error-handler error-type handlers)))
      (unless (null handler) (return (values handler-frame handler))))))

(defun find-error-handler-frame (start-frame)
  (do ((frame start-frame (stack-frame-next-frame frame)))
      ((null frame) nil)
    (when (handle-errors-special-frame-p frame) (return frame))))

(defun get-error-handler (error-type handlers)
  (dolist (handler handlers)
    (let ((handler-type (car handler)))
      (when (or (eq handler-type :ignore-errors)
                (eq error-type handler-type)
                (and (listp handler-type) (fast-memq error-type handler-type)))
        (return (cadr handler))))))

;;;; Signalling Errors
;; should we DATAFY the error-type ?
(defboxer-primitive bu::error ((bu::datafy error-type) (list-rest error-args))
  ;; this is like signal error except we setup the error reporting args
  ;; in a different way to avoid seeing things like "Error, Primitive: Error"
  ;; search for a possible handler
  (let ((canonicalized-error
         (intern (symbol-name (car (boxer::flat-box-items error-type)))
                 (find-package "KEYWORD"))))
    (multiple-value-bind (ehf handler)
        (find-error-handler canonicalized-error *pdl*)
      ;; search the stack for a error-handler frames with a relevant handler
      ;; and then unwind to that frame
      (cond ((not (null ehf))
             (unwind-to-frame ehf)
             (setq *error-signalled?*
                   (list handler (make-error-box-for-handler canonicalized-error
                                                             error-args))))
            (t
             ;; otherwise do the old behavior of throwing to the top level
             ;; of the evaluator
             (setq *exception-signalled* 'error-exception
	           *error-type* (intern (boxer::box-text-string error-type)
                                        (find-package "KEYWORD"))
	           *error-sfun* nil ; should try and get the real fun name here
	           *exception-args* (copy-seq error-args))
             ;; now do the throw like primitive-signal-error does
             (throw 'boxer-primitive-error-signal nil))))))

;; this should get smarter, make a VC with cached-names when appropriate...
(defun make-error-box-for-handler (error-type error-args)
  (boxer::virtual-copy (boxer::make-box (List (list* error-type error-args))
                                        'boxer::data-box)))


;;; For internal debugging
(defun print-pdl ()
  (do ((frame *pdl* (stack-frame-next-frame frame)))
      ((null frame))
    (cond ((eval-args-frame-p frame)
	   (print-eval-args-frame frame))
	  ((ufun-frame-p frame)
	   (print-ufun-frame frame))
	  ((doit-port-funcall-frame-p frame)
	   (print-doit-port-funcall-frame frame))
	  ((tell-special-frame-p frame)
	   (print-tell-special-frame frame))
	  ((repeat-special-frame-p frame)
	   (print-repeat-special-frame frame))
	  (t (format t "~%Unprintable frame: ~S"
		     (car frame))))))

(defun print-state ()
  (dolist (var *eval-state-vars*)
    (when (not (evsi-local-p var))
      (format t "~%~S: ~S" (evsi-variable-name var)
	      (symbol-value (evsi-variable-name var))))))


