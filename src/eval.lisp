;;; -*- Syntax: Common-Lisp ; Base: 10; Package: EVAL -*-

#|


 $Header: eval.lisp,v 1.0 90/01/24 22:11:22 boxer Exp $

 $Log:	eval.lisp,v $
;;;Revision 1.0  90/01/24  22:11:22  boxer
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



Modification History (most recent at top)

10/31/11 fixed typo
 9/28/11 Handled conversion of sprite boxes to data boxes with sprite props
 2/11/03 merged current LW and MCL files

|#
;;;;
;;;;  The Explicit Control Evaluator
;;;;

#-(or mcl lispworks LISPM) (in-package 'eval :use '(lisp sm))
#+(or lispworks mcl) (in-package :eval)

(defmacro evaluator-body (iline-name process-state)
  `(with-evaluator-state-variables-and-unwind-protect #|()|#
     (init-timing)
     (trace-entering eval)
     (setq *poll-count* *initial-poll-count*)
     (tagbody
      STANDARD-ENTRY
	(trace-entering standard-entry)
	(when (null ,process-state)
	  ;; if we are being called from :TOPLEVEL, initialize some state
	  (setq *returned-value* *novalue*)
	  (setq *process-state* :RUN)
	  ;; this used to be under the STANDARD-ENTRY tag moved here
	  ;;  to since it should run if there is existing process-state
	  ;; *EXECUTING-FUNCTION* will be NIL here, because we're
	  ;; only called from toplevel.
	  (setq *executing-pointer* ,iline-name)
	  ;; *EXECUTING-LINE* is a ufun body cdr except at top level,
	  ;; where  it is a list and used mostly by error messages.
	  (setq *executing-line* *executing-line*)
	  (go EVAL-LOOP))
      CONTINUE-PROCESS
	;; if there IS a process, grab the state from it and then
	;; re-enter the evaluator
	(if (eq *current-process* ,process-state)
	    (warn
	     "Trying to Switch to the same process in CONTINUE-PROCESS: ~A"
	     *current-process*)
	    (progn
	      (unless (null ,process-state)
		(setq *current-process* ,process-state))
	      (retrieve-evaluator-state)
	      ;; if there is a *pc* go there
	      (case *pc*
		(:sfuncall-continuation-dispatch
		 (go SFUNCALL-CONTINUATION-DISPATCH))
		;; default *pc* means fall through to EVAL-LOOP
		)))
      EVAL-LOOP
	(trace-entering eval-loop *executing-pointer*)
	(setq *pc* :eval-loop)
	(let ((poll-result (poll)))
	  (cond ((null poll-result))
		((eq poll-result :pause)
		 ;; we should come here ONLY if we are in the
		 ;; evaluator running a backgrounded process
		 (trace-entering Process-Pause *current-process*)
		 (store-evaluator-state)
		 ;; queue the process to be restarted later....
		 (cond ((null *boxer-processes*)
			(setq *boxer-processes* (list *current-process*)))
		       ((fast-memq *current-process* *boxer-processes*))
		       (t (nconc (last *boxer-processes*)
				 (list *current-process*))))
		 		 ;; Make a new process.
		 ;; leave a top level process in charge
		 (or (eq (process-variable *current-process* *process-state*)
			 :toplevel)
		     (setq *current-process* (get-toplevel-process))
		     (init-process-state-vector))
		 ;; (re)set the evaluator from the new process
		 (retrieve-evaluator-state)
		 (setq *returned-value* *novalue*)
		 (go EXIT-EVAL))
		((simple-vector-p poll-result)
		 ;; looks like another process so switch to it
		 (store-evaluator-state)
		 (setq *current-process* poll-result)
		 (retrieve-evaluator-state)
		 (trace-entering Process-Switch *current-process* poll-result
				 *executing-pointer*)
		 (case *pc*
		 (:sfuncall-continuation-dispatch
		  (go SFUNCALL-CONTINUATION-DISPATCH))))
		(t
		 (handle-interrupt-char)
		 (when (not (null *exception-signalled*))
		   (go handle-exception)))))
      EVAL-LOOP-SKIP-POLL-AND-ERRORS
	(trace-entering eval-loop-skip-poll-and-errors)
	;;...
      TOKEN-DISPATCH-IGNORE-DEFINITIONS-LOOP
	;; *EXECUTING-POINTER* might be NIL -- we will
	;; check for sure after LIST-REST handling.
	(setq *thing-pointer* (car *executing-pointer*))
	(when (eq *thing-pointer* '*ignoring-definition-object*)
	  (when-stepping (step-advance-token) (step-redisplay))
	  (setq *executing-pointer* (cdr *executing-pointer*))
	  (go token-dispatch-ignore-definitions-loop))
	;; First we check to see if the input flavor takes precedence over
	;; is one which the token-type handlers can't process.  @ takes
	;; precedence over &rest, though.
	(special-arg-handling-cases
	 (list-rest
	  (vpdl-push *executing-pointer*)
	  (when-stepping (step-handle-list-rest-arg) (step-redisplay))
	  (setq *executing-pointer* nil)
	  (go got-value))
	 (bu::box-rest
	  (vpdl-push (boxer::make-vc (list *executing-pointer*)))
	  (when-stepping (step-handle-list-rest-arg) (step-redisplay))
	  (setq *executing-pointer* nil)
	  (go got-value))
	 #|  ;; TOKEN is probably never necessary.  It's like DATAFY-INTERNAL
             ;; without the box.  If you find yourself wanting to use this to
             ;; get at variable names, you should be using ports instead.
           (token
	     (vpdl-push (car *executing-pointer*))
  	     (when-stepping (step-handle-token-arg) (step-redisplay))
	     (setq *executing-pointer* (cdr *executing-pointer*))
	     (go got-value))|#)
	(when (null *executing-pointer*) (go eval-done-with-line))
	;;...
      RAW-TOKEN-DISPATCH
	(trace-entering raw-token-dispatch *thing-pointer*)
	(cond ((symbolp *thing-pointer*) (go symbol))
	      ((numberp *thing-pointer*) (go number))
	      ((fast-eval-data-box? *thing-pointer*)
               ;; data boxes might have sprites...
               (cond ((box::fast-vc-has-sprite? *thing-pointer*)
                      (setq *thing-pointer* (boxer::port-to *thing-pointer*))
                      (go port))
                     (t (go data-box))))
	      ((fast-eval-doit-box? *thing-pointer*) (go doit-box))
	      ((fast-eval-port-box? *thing-pointer*) (go port))
; no more sprite boxes...
;	      ((fast-eval-sprite-box? *thing-pointer*)
;	       (setq *thing-pointer* (boxer::port-to *thing-pointer*))
;	       (go port))
	      ((special-token? *thing-pointer*) (go special-token))
	      ;; this is new, and might be taken out.
	      ((boxer-function? *thing-pointer*)
	       (go raw-boxer-function))
              ((typep *thing-pointer* 'boxer::foreign-data)
               (go FOREIGN-DATA))
	      (t (signal-error
		  :eval-error
		  "unusual object encountered in eval:" *thing-pointer*)
		 (go handle-exception)))

      NUMBER
	(trace-entering number *thing-pointer*)
	;; We enter NUMBER only when encountering a number on the input line,
	;; not as a returned value.
	(when-stepping (setq *thing-pointer*
			     (boxer::data-boxify *thing-pointer*))
		       (go data-box))
	(special-arg-handling-cases
	 ;; Do you really want to do CHANGE 7 3???
	 (bu::port-to
	  (setq *thing-pointer* (boxer::port-to
				 (boxer::data-boxify *thing-pointer*))))
	 (otherwise))
	(go self-evaluating-object)

      EDITOR-DATA-BOX
	(trace-entering editor-data-box *thing-pointer*)
	(when-stepping
	 (special-arg-handling-cases
	  (bu::port-to (setq *thing-pointer*
			     (boxer::port-to-internal *thing-pointer*)))
	  (bu::datafy (go datafy-object))
	  (dont-copy (when-stepping
		      (setq *thing-pointer*
			    (boxer::copy-box *thing-pointer* nil))))
	  (otherwise (setq *thing-pointer*
			   (boxer::copy-box *thing-pointer* nil))))
	 (go self-evaluating-object))
	(go data-box)

      DATA-BOX
	(trace-entering data-box *thing-pointer*)
	(special-arg-handling-cases
	 (bu::port-to (setq *thing-pointer* (boxer::port-to *thing-pointer*)))
	 (bu::datafy (go datafy-object))
	 (dont-copy (when-stepping (setq *thing-pointer*
					 (copy-thing *thing-pointer*))))
	 (otherwise (setq *thing-pointer* (copy-thing *thing-pointer*))))
	(go self-evaluating-object)

      FOREIGN-DATA  ;; is mostly like DATA-BOX's
        (trace-entering foreign-data *thing-pointer*)
        (special-arg-handling-cases
	 (bu::port-to (setq *thing-pointer* (boxer::port-to *thing-pointer*)))
	 (bu::datafy (go datafy-object))
	 (dont-copy (when-stepping (setq *thing-pointer*
					 (copy-thing *thing-pointer*))))
	 (otherwise (setq *thing-pointer* (copy-thing *thing-pointer*))))
        (go self-evaluating-object)

      DOIT-BOX
	(trace-entering doit-box *thing-pointer*)
	(special-arg-handling-cases
	 (bu::datafy (go datafy-object))
	 (otherwise (setq *function*
			  (cached-code-virtual-copy *thing-pointer*))
		    (go eval-args-setup)))

      EDITOR-DOIT-BOX
	(trace-entering editor-doit-box *thing-pointer*)
	(special-arg-handling-cases
	 (bu::datafy (go datafy-object))
	 (otherwise (setq *function* (cached-code-editor-box *thing-pointer*))
		    (go eval-args-setup)))

;;; EDITOR-PORTS are encountered only inside variables, and have
;;; only editor boxes as targets.
      EDITOR-PORT
	(trace-entering editor-port *thing-pointer*)
	(special-arg-handling-cases
	 (bu::datafy (go datafy-object)))
	(let ((target (boxer::get-port-target *thing-pointer*)))
	  (cond ((not (doit-box? target))
		 (setq *thing-pointer*
		       (boxer::make-virtual-port-from-editor-port
			*thing-pointer*))
		 (go self-evaluating-object))
		(t
		 (setq *function* (cached-code-editor-port *thing-pointer*))
		 (go eval-args-setup))))

;;; PORTS are seen either in variables on directly, and can have
;;; editor/eval data boxes or editor doit boxes.
      PORT
	(trace-entering port *thing-pointer*)
	(special-arg-handling-cases
	 (bu::datafy (go datafy-object)))
	(let ((target (boxer::vp-target *thing-pointer*)))
	  (cond ((or (and (possible-eval-object? target)
			  (fast-eval-data-box? target))
		     (not (doit-box? target)))
		 (go self-evaluating-object))
		(t
		 (setq *function* (cached-code-virtual-port *thing-pointer*))
		 (go eval-args-setup))))

      SPECIAL-TOKEN
	(trace-entering special-token *thing-pointer* *executing-pointer*)
	;; @->BU::@.
	;; BU::EVAL-IT, BU::PREVIOUS-TELL-ENVIRONMENT objects left alone
	(cond ((unbox-token? *thing-pointer*)
	       (setq *executing-pointer*
		     (list*
		      'BU::@
		      (special-token-item *thing-pointer*)
		      (cdr *executing-pointer*)))
	       (setq *thing-pointer* 'BU::@)
	       (go symbol))
	      ((previous-tell-environment-token? *thing-pointer*)
	       (go previous-tell-environment-token))
	      ((dots-list-token? *thing-pointer*)
	       (go dots-list-token))
	      ((squid-token? *thing-pointer*)
	       (go self-quoting-internal-datum))
	      ((eval-it-token? *thing-pointer*)
	       (go eval-it-token))
	      (t (signal-error :eval-bug
			       "Unknown SPECIAL-TOKEN object: "
			       *thing-pointer*)
		 (go handle-exception)))

      PREVIOUS-TELL-ENVIRONMENT-TOKEN
	(trace-entering previous-tell-environment-token *thing-pointer*
			*executing-pointer*)
	;; Inside a previous-tell-environment token may be a symbol, a box,
        ;; a number, or another token.
	;; Handle this just like symbol.
	(go symbol)

      EVAL-IT-TOKEN
	(trace-entering eval-it-token *thing-pointer* *executing-pointer*)
	;; These are errors now.
	(signal-error
	 :!-out-of-place
	 "The ! character is used only inside BUILD.  Maybe you want ^?")
	(go handle-exception)

      DOTS-LIST-TOKEN
	(trace-entering dots-list-token *thing-pointer* *executing-pointer*)
	(go symbol)

      SYMBOL
	(trace-entering symbol *thing-pointer* *executing-pointer*)
	;; Symbol dispatches only to NUMBER, DATA-BOX, and PORT.
	;; The token represents a one of the following:
	;;  a !var or !<box/num> pair
	;;  a special flavor-overriding token
	;;  an eval doit, port, or data box
	;;  a number
	;;  a regular or infix primitive
	;;  an editor data, doit, or port box
	;; We check the special word case first.
	;; We don't cdr down *EXECUTING-POINTER* and in general and
	;; don't worry about arg handling because we will
	;; dispatch to a handler that will do that for us.
	;; However, we deal with cases that require boxification:
	;; If the input flavor is DATAFY, we deal with it here.
	;; If the value is a number and the arg flavor is port-to,
	;; we stuff a boxified number into the variable and then go on.
	;; ----------------------------------------------------------------
	;; If we encounter a DONT-PORT token and the current input flavor is
	;; PORT-TO, then we cons a little and remove the flavor.  I think I
	;; like this a lot, but on Tuesdays I think it's a hack.
	(when (eq *thing-pointer* 'bu::dont-port)
	  (special-arg-handling-cases
	   (bu::port-to (setq *executing-pointer* (cdr *executing-pointer*))
			(when-stepping (step-advance-token) (step-redisplay))
			(setq *arglist*
			      (cons (cdar *arglist*) (cdr *arglist*)))
			(go eval-loop-skip-poll-and-errors))))

	;; When there's an @, we get a special token, and the special
	;; token gets expanded by the SPECIAL-TOKEN handler into
	;; an @ followed by the item.  Then it comes directly here
	;; because we know @ is a symbol, and we want to call the @
	;; primitive.  Now, since the @ is internal, we don't want
	;; to datafy it;  what we want is to datafy the result of the @
	;; primitive.  So if we're gathering a DATAFY-flavored arg
	;; here we don't do the datafy.  Ugh.
	;; If it turns out it is possible for the user to put
	;; a BU::@ into a Boxer line somehow (like with quoting
	;; or something) then we can just move the BU::@ primitive
	;; to the EVAL package.
	(special-arg-handling-cases
	 (bu::datafy
	  (unless (eq *thing-pointer* 'BU::@)
	    (go datafy-object))))

	;; Otherwise we're going to evaluate the variable, do some
	;; hacks to support bare numbers and bare editor objects
	;; living in variables, and then do our own version of TOKEN-DISPATCH,
	;; leaving out the impossible cases.
	(let ((variable-name *thing-pointer*))
	  (setq *thing-pointer*
		(if (symbolp variable-name)
		    (boxer-symeval variable-name)
		    (boxer-symeval-special-token variable-name)))
	  ;; handle special exceptions (other than unbound variable).
	  (when *exception-signalled* (go handle-exception))
	  ;; Now we do a mini-TOKEN-DISPATCH.  We don't go back to
	  ;; token dispatch because we can encounter editor objects
	  ;; here and only here, and we have to deal with them.
	  ;; We also don't want symbols as values of symbols [unless we
	  ;; make TRUE and FALSE be special things later].
	  (cond ((numberp *thing-pointer*)
		 ;; If the value is a raw number and we want a port to it,
		 ;; we quick-like change the number into a data box and stuff
		 ;; it back into the variable. It's OK to do this because there
		 ;; can be no prior ports to the object since this is the first
		 ;; time we're making a port to it. Future port-makers will get
		 ;; a data-box they can get a handle on.
		 (special-arg-handling-cases
		  (bu::port-to
		   ;; THIS BETTER NOT BE A ![] -- fix this!!!
		   (boxer-set-internal variable-name
				       (setq *thing-pointer*
					     (boxer::data-boxify
					      *thing-pointer*)))
		   (when-stepping
		    (step-update-number-to-box *thing-pointer*)
		    (step-redisplay))
		   (go data-box)))
		 (go number))
		((eq *thing-pointer* *novalue*)
		 (signal-error :unbound-variable variable-name)
		 (go handle-exception))
		((eval-object? *thing-pointer*)
		 ;; Everything in this clause is something that has the
		 ;; type in slot 0 and is not an editor object.
		 (cond
		   ((boxer-function? *thing-pointer*)
                    ;; changed from compiled-boxer-function? to boxer-function?
                    ;; to handle landscape slot access which use interpreted-boxer-functions
                    ;;
		    ;; If this is an infix function then it's out of place,
		    ;; unless it's a unary use of -.
		    ;; If you already have a compiled function it's faster
		    ;; to check with boxer-function-infix-p, but for a random
		    ;; object it's faster to use infix-function-symbol-p.
		    (when (boxer-function-infix-p *thing-pointer*)
		      ;; Ideally these should be distinguished at read time,
		      ;; but that's harder in Boxer when spaces count.
		      (cond ((eq variable-name 'bu::-)
			     (setq *thing-pointer*
				   (boxer-toplevel-symeval
				    '%negative-internal)))
			    (t (signal-error :infix-out-of-place variable-name)
			       (go handle-exception))))
		    ;; Otherwise, set about gathering the args
		    (setq *function* *thing-pointer*)
		    (go eval-args-setup))
		   ;; Eval objects
		   ((fast-eval-doit-box? *thing-pointer*) (go doit-box))
		   ((fast-eval-data-box? *thing-pointer*)
                    ;; data boxes might have sprites...
                    (cond ((box::fast-vc-has-sprite? *thing-pointer*)
                           (setq *thing-pointer* (boxer::port-to *thing-pointer*))
                           (go port))
                          (t (go data-box))))
		   ((fast-eval-port-box? *thing-pointer*) (go port))
;		   ((fast-eval-sprite-box? *thing-pointer*)
;		    (setq *thing-pointer* (boxer::port-to *thing-pointer*))
;		    (go port))
		   ;; ************ Black Box Interface ************
		   ((boxer::vv-box-interface? *thing-pointer*)
		    (special-arg-handling-cases
		     (bu::port-to
		      (setq *thing-pointer*
			  (or (boxer::box-interface-box *thing-pointer*)
			      (boxer::connect-and-imbed-interface-slot
			       *thing-pointer*)))
		      (go editor-data-box)))
		    (setq *thing-pointer*
			  (boxer::box-interface-value *thing-pointer*))
		    (go number))
		   ((or (boxer::iv-box-interface? *thing-pointer*)
			(boxer::sv-box-interface? *thing-pointer*))
		    (setq *thing-pointer*
			  (or (boxer::box-interface-box *thing-pointer*)
			      (boxer::connect-and-imbed-interface-slot
			       *thing-pointer*)))
		    (go editor-data-box))
		   ;; ************ Black Box Interface ************
		   (t (signal-error
		       :eval-error
		       "unusual object encountered in eval: variable"
		       variable-name "value"
		       *thing-pointer* variable-name)
		      (go handle-exception))))
		;; Editor objects -- they might be POSSIBLE-EVAL-OBJECT? T in
		;; some implementations, but EVAL-OBJECT? is supposed to catch
		;; those cases.
                ;; NOTE: 10/01/2011 sprite check has to come before data-box because sprites
                ;;       are now data boxes with extra properties
		((sprite-box? *thing-pointer*)
		 (setq *thing-pointer* (boxer::port-to *thing-pointer*))
		 (go port))
		((doit-box? *thing-pointer*) (go editor-doit-box))
		((data-box? *thing-pointer*) (go editor-data-box))
		((port-box? *thing-pointer*) (go editor-port))
		(t (signal-error
		    :eval-error
		    "unusual object encountered in eval: variable"
		    variable-name "value" *thing-pointer* variable-name)
		   (go handle-exception))))

      RAW-BOXER-FUNCTION
	;; a compiled boxer function is actually a primitive object,
	;; usually put in the token stream by some weird macro or
	;; feature or other.
	(trace-entering raw-boxer-function *executing-pointer*)
	(setq *function* *thing-pointer*)
	;;... falls through ...

      EVAL-ARGS-SETUP
	;; We expect the function in *function*
	(trace-entering eval-args-setup *executing-pointer*)
	(when-stepping (step-start-function-sequence *function*)
		       (step-redisplay))
	(setq *executing-pointer* (cdr *executing-pointer*))
	;; If there are no args, avoid all contact.
	;; Skip making the eval args frame and go directly to UFUNCALL-NO-ARGS
	;; or SFUN-SKIP-ARGS.
	;; This check does an extra BOXER-FUNCTION-ARGLIST call on
	;; functions with arguments, but it's just an AREF.
	(when (null (boxer-function-arglist *function*))
	  (cond ((interpreted-boxer-function? *function*)
		 (go ufuncall-no-args))
		(t;; Next 2 Lines added by EhL
		 (setq *executing-sfun* *function*)
		 (go sfuncall-skip-args))))
	(when-stepping
	 (pdl-push-frame stepper-eval-args-frame *stepper-item-no*)
	 (step-advance-token)
	 (step-redisplay))
	;; Push the EVAL-ARGS frame.
	(trace-entering push-eval-args-frame *arglist* *current-function*)
	(pdl-push-frame eval-args-frame *arglist* *current-function*)
	;; End of the EVAL-ARGS frame.
	(setq *current-function* *function*)
	;;The args and flavors for *current-function* function, valid only
	;; when *current-function* is non-nil.
	(setq *arglist* (boxer-function-arglist *current-function*))
	(go check-if-all-args)

      DATAFY-OBJECT
	(trace-entering DATAFY-OBJECT)
	(setq *thing-pointer* (boxer::data-boxify *thing-pointer*))
;;; ... fall through ...
      SELF-EVALUATING-OBJECT
	(trace-entering self-evaluating-object *thing-pointer*)
	(when-stepping (step-self-evaluating-object *thing-pointer*)
		       (step-redisplay))
;;; ... fall through ...
      SELF-QUOTING-INTERNAL-DATUM
	(setq *executing-pointer* (cdr *executing-pointer*))
	(vpdl-push *thing-pointer*)
;;; ... fall through ...
      GOT-VALUE
	(trace-entering got-value)
	(unless (null *executing-pointer*)
	  (let ((next-token (car *executing-pointer*)))
	    (when (and (symbolp next-token)
		       (infix-function-symbol-p next-token)
                       ;; datafy should take precedence over infix
                       (not (and *current-function*
                                 (not (symbolp (car *arglist*)))
                                 (eq (caar *arglist*) 'bu::datafy))))
	      ;; If the next thing is an infix function we have to test
	      ;; precedences.  We take advantage of the fact that
	      ;; infix functions are forever to keep from having to
	      ;; to do an extra boxer-symeval on every token.
	      (let ((infix-function (boxer-toplevel-symeval next-token)))
		;; then if we're not looking for anyone else's args
		;; or if we are looking for someone else's args but the
		;; precedence of this infix function is greater than the
		;; function we're gathering args for...
		(when (or (null *current-function*)
			  (>& (boxer-function-precedence infix-function)
			      (boxer-function-precedence *current-function*)))
		  (when-stepping
		   (step-handle-infix-function infix-function next-token)
		   (step-redisplay)
		   (pdl-push-frame stepper-eval-args-frame *stepper-item-no*))

		  ;; then we should do this infix function now, so push an
		  ;; eval args frame for it.
		  (pdl-push-frame eval-args-frame *arglist* *current-function*)
		  (setq *current-function* infix-function)
		  ;;get number of args (should always be 2 for infix functions)
		  (setq *arglist*
			(cdr (boxer-function-arglist infix-function)))
		  ;; CROCK:
		  ;; We COMPLETELY ignore the flavor of the first input
		  ;; to infix functions.  If we wanted a port to the
		  ;; first arg we've already lost.  If there are ever
		  ;; infix primitives with xxx-flavored inputs we will
		  ;; have to figure out some way to handle this problem
		  ;; in the future, probably.  For now, we can just
		  ;; vpdl-pop the arg and arg-case treat it here.
		  (setq *executing-pointer* (cdr *executing-pointer*))
		  (when-stepping
		   (step-advance-token)
		   (step-advance-token)
		   (step-redisplay))
		  ;; We know we don't have all the args, so don't do
		  ;; check-if-all-args.
		  (go eval-finished-unit))))))
	(cond
	  ;; If there's a currently a function needing args, say we got another
	  ;; value for it and go see if that's all.
	  ((not (null *current-function*))
	   (setq *arglist* (cdr *arglist*))
	   (go check-if-all-args))
	  (t
	   ;; Otherwise we'll just keep track of this value for use at
	   ;; the end of the procedure.
	   (setq *returned-value* (vpdl-pop))
	   (go eval-finished-unit)))

      EVAL-FINISHED-UNIT
	(trace-entering eval-finished-unit)
	;; When we come here we have just evaluated a single expression.
	(go eval-loop)

      CHECK-IF-ALL-ARGS
	(trace-entering check-if-all-args *arglist*)
	(cond ((null *arglist*)
	       (go funcall))
	      ;; for now we always take the default number of args
	      (t (go eval-finished-unit)))

      FUNCALL
	(if (compiled-boxer-function? *current-function*)
	    (go sfuncall)
	    (go ufuncall))

      UFUNCALL
	(trace-entering ufuncall *current-function*)
	;; pop the eval-args frame.  save the function away first.
	(setq *function* *current-function*)
	(pdl-pop-frame eval-args-frame *arglist* *current-function*)
	(when-stepping
	 (let ((it *stepper-item-no*))
	   (pdl-pop-frame stepper-eval-args-frame *stepper-item-no*)
	   (step-finish-eval-args it)))
					;...
      UFUNCALL-NO-ARGS
	(setq *returned-value* *novalue*)
	;; tail recurse if we can, but not from top level.
	#|
	 (when (and (not (null *executing-function*))
		    (tail-recurse-p))
	   (go tail-recurse))
|#
	(go ufuncall-really)

      UFUNCALL-REALLY
	(trace-entering ufuncall-really)
	;; push the UFUN-FRAME
	;; We don't need to push *arglist* because it's not examined unless
	;; current-function is non-nil, and it's set to nil here.

	;; If the function is from a port, we alter the save away the
	;; *-variable-roots and set them accordingly before proceeding
	;; with the function call.
	(when (interpreted-boxer-function-lexical-call-p *function*)
	  (pdl-push-frame doit-port-funcall-frame *lexical-variables-root*
			  *dynamic-variables-bottom*)
	  (setq *lexical-variables-root*
		(interpreted-boxer-function-backpointer *function*))
	  (setq *dynamic-variables-bottom* *dynamic-variables-top*))
	(when-stepping
	 (let ((item-no *stepper-item-no*))
	   (pdl-push-frame stepper-ufun-frame
			   *stepper-item-no* *stepper-row-no*)
	   (setq *stepper-item-no* 0
		 *stepper-row-no* 0)
	   (stepper-enter-box item-no)))
	(pdl-push-frame ufun-frame
			*executing-function*
			*executing-line* *executing-pointer*
			*run-list-sfun-epilog-handler*
			*ufuncall-sfun-epilog-handler*
			*current-function*)
	(setq *run-list-sfun-epilog-handler* nil)
	(setq *ufuncall-sfun-epilog-handler* nil)
	(setq *executing-function* *function*)
	(setq *executing-line* (interpreted-boxer-function-text *function*))
	(setq *current-function* nil)
        (dynamically-bind-variables-and-locals
         (interpreted-boxer-function-reversed-arg-names *function*)
         (interpreted-boxer-function-locals *function*))
	(when-stepping
	 (unless (null (interpreted-boxer-function-arglist *function*))
	   (step-replace-input-line-with-values
	    (interpreted-boxer-function-arglist *function*)))
	 (step-redisplay)
	 (step-advance-line)
	 (step-redisplay))
	(when (null *executing-line*)
	  (go ufuncall-return))
	(setq *executing-pointer* (car *executing-line*))
	(go eval-loop)

      TAIL-RECURSE
	;; sorry
	(go ufuncall-really)

      SFUNCALL
	(trace-entering sfuncall *current-function*)
	(setq *executing-sfun* *current-function*)
	(pdl-pop-frame eval-args-frame *arglist* *current-function*)
	(when-stepping
	 (let ((it *stepper-item-no*))
	   (pdl-pop-frame stepper-eval-args-frame *stepper-item-no*)
	   (step-finish-eval-args it)))

      SFUNCALL-SKIP-ARGS
	(trace-entering SFUNCALL-SKIP-ARGS)
	(when-stepping
	 ;; step replace input line with values goes in here somewhere
	 (stepper-enter-box *stepper-item-no*))
	(setq *sfun-continuation* '*std-sfun-continuation*)
	(setq *returned-value*
	      (catch 'boxer-primitive-error-signal
		(if (typep (compiled-boxer-function-object
				*executing-sfun*)
			   'box::compiled-boxer-object)
		    (boxer::funcall-compiled-boxer-function
		     (compiled-boxer-function-object *executing-sfun*))
		    (funcall
		     (compiled-boxer-function-object *executing-sfun*)))))
	;; preserve the executing_sfun as function in case we need it if
	;; this sfun didn't return a value and one was wanted
	(setq *function* *executing-sfun*)
	;; set executing-sfun to nil again for error messages and for pause
	;; on so it can tell whether to be invisible
	(setq *executing-sfun* nil)
	;; Handle trigger stuff (merge it into sfun continuation)
	(when (not (null *trigger-list-to-run*))
	  (setq *returned-value*
		(handle-trigger-list-in-eval *returned-value*
					     *executing-pointer*)))
	;; go to the sfun's continuation
      SFUNCALL-CONTINUATION-DISPATCH
	(trace-entering SFUNCALL-CONTINUATION-DISPATCH
			*sfun-continuation* *returned-value*)
	(case *sfun-continuation*
	  (*std-sfun-continuation* (go sfuncall-return)) ;Used by most sfuns.
	  (*macroexpand-sfun-continuation*
	   (when-stepping
	    (when (numberp *returned-value*)
	      (setq *returned-value* (boxer::data-boxify *returned-value*)))
	    (step-function-return-sequence *returned-value*)
	    (step-redisplay))
	   (setq *thing-pointer* *returned-value*)
	   (setq *returned-value* *novalue*)
	   ;; *executing-line* might be nil here--*thing-pointer* is unrelated.
	   (go raw-token-dispatch))
	  (*ufuncall-sfun-result-sfun-continuation*
	   ;; used by DEFRECURSIVE-FUNCALL-PRIMITIVEs
	   (setq *function* *returned-value*)       ;ufun must take no args.
	   ;; added by EhL to handle empty data boxes in RUN
	   ;; since we use *returned-value* pass the procedure, we need to
	   ;; bash it back to *novalue* after we are through
	   ;; procedures that get run will set *returned-value* themselves,
	   ;; this is for the case of the empty procedure
	   (setq *returned-value* *novalue*)
	   (go ufuncall-really))
	  (*run-list-sfun-continuation*
	   ;; Used by REPEAT and recursive EVAL
	   ;; invocations and by ATSIGN to insert things
	   ;; into the token stream.  Bug in recursive
	   ;; eval use -- frames don't get popped.
	   (when-stepping
	    (when (numberp *returned-value*)
	      (setq *returned-value* (boxer::data-boxify *returned-value*)))
	    (step-handle-run-list-continuation *returned-value*)
	    (step-redisplay))
	   (setq *executing-pointer* *returned-value*)
	   (setq *returned-value* eval::*novalue*)
	   (go eval-loop))
	  (*access-evaluator-state-sfun-continuation*
	   (setq *pc* :sfuncall-continuation-dispatch)
	   (store-evaluator-state)
	   (when (funcall *returned-value*)
	     (retrieve-evaluator-state))
	   (go sfuncall-continuation-dispatch))
	  (*eval-loop-sfun-continuation* (go eval-loop)) ;Used by CONTINUE???
	  (t (signal-error :eval-error
			   "Bad continuation from sfuncall"
			   *sfun-continuation*)
	     (go handle-exception)))
	;; can't fall through

      SFUNCALL-RETURN
	(trace-entering sfuncall-return *returned-value*)
	;; If an error was signalled, we know that a value has not been
	;; returned [at best, it was caught and we are about to run a
	;;           catch resumption list:]
	;; ^^^that was a logo comment.  one day when we have an error system...
	;; Go recover from it.
	;; Otherwise, if the sfun did output then we copy the value.
	(cond (*exception-signalled* (go handle-exception))
	      ((eq *returned-value* *novalue*)
	       (when-stepping
		(step-function-return-sequence *returned-value*)
		(step-redisplay))
	       (go funcall-return-no-value))
	      (t
	       (special-arg-handling-cases
		(bu::datafy
		    (setq *returned-value*
			  (boxer::data-boxify *returned-value*)))
		   (bu::port-to
		    (cond ((numberp *returned-value*)
			   (evaluator-helpful-message
			    "Made a PORT to a copy of the result of a primitive.")
			   (setq *returned-value*
				 (boxer::port-to
				  (boxer::data-boxify *returned-value*))))
			  ((fast-eval-port-box? *returned-value*))
                          ((typep *returned-value* 'boxer::foreign-data)
                           (setq *returned-value* (boxer::port-to *returned-value*)))
			  (t
			   (evaluator-helpful-message
			    "Made a PORT to a copy of the result of a primitive.")
			   (setq *returned-value*
				 (boxer::port-to
				  (copy-thing *returned-value*))))))
		   (otherwise))
		  (when-stepping
		   (when (numberp *returned-value*)
		     (setq *returned-value*
			   (boxer::data-boxify *returned-value*)))
		   (step-function-return-sequence *returned-value*)
		   (step-redisplay)
		   (step-advance-token))

                  ;; yuck again
;                  (when (typep *returned-value* 'boxer::foreign-data)
;                    (setq *returned-value* (copy-thing *returned-value*)))

		  (vpdl-push *returned-value*)
		  (go got-value)))

      EVAL-DONE-WITH-LINE
	(trace-entering eval-done-with-line)
	;; When we get here, if there is a current function, we haven't gotten
	;; the default number of args for it yet, because it would have been
	;; caught by check-if-all-args
	(unless (null *current-function*)
	  (signal-error :insufficient-args-error *current-function*)
	  (go handle-exception))
	;; if there was no current function and we were at end of line,
	;; just do the next line (fall through)
      EVAL-DO-NEXT-LINE
	(trace-entering eval-do-next-line *executing-function*)
;;; THIS IS AN OLD COMMENT -- NEEDS REVAMPING.  SEE UFUNCALL-SFUN-CONTINUATION.
;;; The code is old and broken too.
	;; At the end of each line we check to see if a special frame handler
	;; is in force.  If one is, then we call the handler function to
	;; let it do any cleanup it needs, if it returns nil, then we're done.
	;; If it returns something else, then we set executing-pointer to that
	;; and go to eval lopp.
	;; Thus, a special frame is in effect for one line.
	;; This is the case where special frame code didn''t return a
	;; value -- the other one is handled by GOT-VALUE.
	(when (not (null *run-list-sfun-epilog-handler*))
	  (let ((result (funcall *run-list-sfun-epilog-handler*)))
	    (cond ((null result)
		   ;; The special evaluation is done.
		   ;; Since we didn't get a value, check now to see if one was
		   ;; wanted and error if so.
		   (when *current-function*
		     ;; used to use *running-function* here to report who
		     ;; wanted the value.
		     (signal-error :didnt-output "???" *current-function*)
		     (go handle-exception))
		   (go eval-finished-unit))
		  (t
		   ;; A non-null result from a special frame handler means to
		   ;; eval that result (presumably the same thing again).
		   (setq *executing-pointer* result)
		   (go eval-loop)))))
	;; if we were at top level
	(when (null *executing-function*)
	  (setq *executing-line* nil)
	  (go done-with-eval))
	;; if we're inside a ufun, get the next line
	(setq *executing-line* (cdr *executing-line*))
	;; if this wasn't the last line of this ufun, go for more
	(when-stepping (step-advance-line))
	(unless (null *executing-line*)
	  (setq *executing-pointer* (car *executing-line*))
	  (go eval-loop))
	;; otherwise this was the last line of the ufun, return from it.
	;; fall through to ufuncall-return

      UFUNCALL-RETURN
	(trace-entering ufuncall-return *returned-value*)
	;;THIS COMMENT IS NOW INACCURATE.
	;; get the ufun frame off the stack--this is never called when evalling
	;; the toplevel line or when a ufun did output.  When a ufun outputs,
	;; the frame is popped by the output sfun.
	;; remember the function for error messages about failure to output
	(setq *function* *executing-function*)
	(trace-entering unbinding-variables)
	(dynamically-unbind-variables)
	(trace-entering popping-ufun-frame *pdl*)
	(pdl-pop-frame ufun-frame
		       *executing-function* *executing-line*
		       *executing-pointer*
		       *run-list-sfun-epilog-handler*
		       *ufuncall-sfun-epilog-handler*
		       *current-function*)
	;; If the function was from a port to a doit box, then we restore
	;; the *-variables-roots before proceeding.
	(when (interpreted-boxer-function-lexical-call-p *function*)
	  (pdl-pop-frame doit-port-funcall-frame *lexical-variables-root*
			 *dynamic-variables-bottom*))

	(when (not (null *ufuncall-sfun-epilog-handler*))
	  (funcall *ufuncall-sfun-epilog-handler*)
	  (setq *ufuncall-sfun-epilog-handler* nil))
	(trace-entering arg-handling-result)
	(cond ((not (eq *returned-value* *novalue*))
	       (special-arg-handling-cases
		(bu::port-to
		 (cond
		   ((numberp *returned-value*)
		    (evaluator-helpful-message
		     "Made a PORT to a copy of the result of a DOIT box.")
		    (setq *returned-value*
			  (boxer::port-to
			   (boxer::data-boxify *returned-value*))))
                   ((typep *returned-value* 'boxer::foreign-data)
                    (setq *returned-value* (boxer::port-to *returned-value*)))
		   ((not (fast-eval-port-box? *returned-value*))
		    (evaluator-helpful-message
		     "Made a PORT to a copy of the result of a DOIT box.")
		    (setq *returned-value*
			  (boxer::port-to (copy-thing *returned-value*))))))
		(bu::datafy
		 (setq *returned-value* (boxer::data-boxify *returned-value*)))
		(otherwise))
	       (when-stepping
		(when (numberp *returned-value*)
		  (setq *returned-value*
			(boxer::data-boxify *returned-value*)))
		(step-function-return-sequence *returned-value*)
		(step-redisplay)
		(pdl-pop-frame stepper-ufun-frame
			       *stepper-item-no* *stepper-row-no*)
		(step-restore-cursor-position)
		(step-redisplay)
		(step-advance-token)
		(step-redisplay))

               ;; yuck
               (when (typep *returned-value* 'boxer::foreign-data)
                 (setq *returned-value* (copy-thing *returned-value*)))

	       (vpdl-push *returned-value*)
	       (go got-value))
	      (t
	       (when-stepping
		(step-function-return-sequence *returned-value*)
		(step-redisplay)
		(pdl-pop-frame stepper-ufun-frame
			       *stepper-item-no* *stepper-row-no*)
		(step-restore-cursor-position)
		(step-redisplay)
		(step-advance-token)
		(step-redisplay))
	       (go funcall-return-no-value)))

      FUNCALL-RETURN-NO-VALUE
	(trace-entering funcall-return-no-value)
	;; come here when returning from an sfuncall or ufuncall from which
	;; no value was returned -- checks if one was wanted
	(when (null *current-function*)
	  (go eval-finished-unit))
	(signal-error :didnt-output *function* *current-function*)
	(go handle-exception)

      HANDLE-EXCEPTION
	(trace-entering handle-exception)
	(let ((old-exception-value *exception-signalled*))
	  ;; clear exception-signalled so that things that run during the error
	  ;; handler can win
	  (setq *exception-signalled* nil)
	  (case old-exception-value
	    (error-exception
	     (when (eq (handle-error) :background-error)
	       (go DONE-WITH-EVAL)))
	    ((pause-int-exception pause-sfun-exception pause-error-exception)
	     (pause old-exception-value))
	    ;; here for interrupts of various sorts
	    (t (signal-error :eval-error "Bad value for exception signalled:"
		      old-exception-value)
	       (go handle-exception))))
	;; if there was another exception signalled during the handling above,
	;; handle it.
	(when (not (null *exception-signalled*)) (go handle-exception))
	;; done handling the exception
	(go eval-loop)
    DONE-WITH-EVAL
	(trace-entering done-with-eval)
	;; DeQueue the *current-process* and make sure there aren't
	;; any more processes to run.  If there are, keep going
	(process-finish *current-process*)
	(let ((todo (next-restartable-process nil)))
	  (when (not (null todo))
	    (cond ((eq todo *current-process*)
		   (warn "~%Trying to switch to same process: ~A" todo))
		  (t
		   (setq *current-process* todo)
		   (go CONTINUE-PROCESS)))))
	;; Need to make sure that a top level process is left
	;; as *current-process* before we exit the evaluator
	;; either by finding the old one associated with the top
	;; level doit-key or else making a new one
	(or (eq (process-variable *current-process* *process-state*) :toplevel)
	    (setq *current-process* (get-toplevel-process))
	    (init-process-state-vector))
      EXIT-EVAL
	(trace-entering exit-eval)
	(values))
     ;; End of TAGBODY
     (setq *process-state* :TOPLEVEL)
     *returned-value*))
