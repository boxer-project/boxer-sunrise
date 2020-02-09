;;-*- Mode:Lisp; Syntax: Common-Lisp; package: Boxer; -*-

#|

 $Header$

 $Log$



          Copyright 1990 - 1996 Regents of the University of California

     Enhancements and Modifications Copyright 1999 - 2003 Pyxisystems LLC


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


            This file contains BUILD and its supporting functions


Modification History (most recent at top)

 2/15/03 merged current LW and MCL files, no diffs, copyright updated


|#

#-(or lispworks mcl lispm)(in-package 'boxer :nicknames '(box))
#+(or lispworks mcl)      (in-package :boxer)




(defstruct (compiled-boxer-object
	     (:print-function print-compiled-boxer-object))
  (code nil)
  (args nil))

(defun print-compiled-boxer-object (obj stream &optional depth)
  (declare (ignore depth))
  (format stream "#<Compiled Boxer Object ~A ~D>"
	  (compiled-boxer-object-args obj)
	  (storage-vector-active-length (compiled-boxer-object-code obj))))

;;;; stack Support Macros
;;; (cbos = compiled-build-object-stack)

(defvar *build-stack* nil)

(defvar *debug-stack-machine* nil)

#| ;; These use a separate stack so that it is easier to debug

(defmacro cbos-pop ()
  `(if (null *build-stack*)
       (error "The build stack is empty")
       (let ((value (pop *build-stack*)))
	 (when *debug-stack-machine*
	   (format *trace-output* "~%  Popping ~A" value))
	 value)))

(defmacro cbos-push (object)
  `(let ((value ,object))
     (when *debug-stack-machine*
       (format *trace-output* "~%  Pushing ~A" value))
     (push value *build-stack*)))

(defmacro cbos-reset () `(setq *build-stack* nil))

(defmacro cbos-push-stack-ref (index)
  `(let ((value (nth ,index *build-stack*)))
     (when *debug-stack-machine*
       (format *trace-output* "~%  Pushing stack Ref to ~A" value))
     (push value *build-stack*)))

;; transfer args from boxer to build stack
(defun push-args-onto-stack (number-of-args)
  (let ((tmp nil))
    (dotimes (i number-of-args)
      (push (eval::vpdl-pop) tmp))
    (dotimes (i number-of-args)
      (format t "~%pushing formal arg ~A" (car tmp))
      (cbos-push (pop tmp)))))

|#

;; these hack the evaluator's *vpdl* stack

(defmacro cbos-pop ()
  `(let ((value (eval::vpdl-pop)))
     (when *debug-stack-machine*
       (format *trace-output* "~%  Popping ~A" value))
     value))

(defmacro cbos-push (object)
  `(let ((value ,object))
     (when *debug-stack-machine*
       (format *trace-output* "~%  Pushing ~A" value))
     (eval::vpdl-push value)))

(defmacro cbos-reset () `(eval::reset-vpdl))

(defmacro cbos-push-stack-ref (index)
  `(let ((value (svref& eval::*vpdl* (-& eval::*vpdl-index* ,index))))
     (when *debug-stack-machine*
       (format *trace-output* "~%  Pushing stack Ref to ~A" value))
     (eval::vpdl-push value)))

(defun push-args-onto-stack (number-of-args)
  (when *debug-stack-machine*
    (dotimes (i number-of-args)
      (format *trace-output* "~%  Found formal arg ~A at ~D"
	      (svref& eval::*vpdl* (-& eval::*vpdl-index* i 1))
	      (-& eval::*vpdl-index* i 1)))))






;;;; Code generation

;;; code-vectors are the storage arrays define in storage.lisp

(defvar *instruction-token-array* (make-array 255 :initial-element nil))

(defvar *token-instruction-alist* nil)

(defvar *instruction-arg-vector* (make-array 255 :initial-element nil))

;;; there are 2 flavors of instructions distinguished by
;;; whether they require further arguments in the instruction
;;; stream or not.  All instructions
;;; will pop all of its arguments off the stack
;;; and leave the result back on the stack.
;;;

(defmacro define-instruction ((name byte) args &body body)
  `(progn
     (when (and (not (null (aref *instruction-token-array* ,byte)))
		(not (eq (aref *instruction-token-array* ,byte) ',name)))
       (cerror "Go ahead and redefine"
	       "Defining Opcode ~D to be ~A was previously defined to be ~A"
	       ,byte ',name (aref *instruction-token-array* ,byte)))
     (setf (aref *instruction-token-array* ,byte) ',name)
     (setf (aref *instruction-arg-vector* ,byte) ,(length args))
     (let ((entry (assoc ',name *token-instruction-alist*)))
       (cond ((null entry)
	      (setq *token-instruction-alist*
		    (acons ',name ,byte *token-instruction-alist*)))
	     (t
	      (unless (= ,byte (cdr entry))
		(warn "Changing token instruction for ~A from ~D to ~D"
		      ',name (cdr entry) ,byte))
	      (setf (cdr entry) ,byte))))
     (defun ,name ,args . ,body)))

(defun token->instruction (token)
  (cdr (assoc token *token-instruction-alist*)))

(defsubst instruction->token (instruction)
  (svref& *instruction-token-array* instruction))

(defsubst instruction-args (instruction)
  (svref& *instruction-arg-vector* instruction))

(defun disassemble-compiled-boxer-object (build-object)
  (let ((number-of-args 0))
    (do-vector-contents (item (cbo-code build-object))
      (cond ((plusp number-of-args)
	     (decf number-of-args)
	     (format t " ~A" item))
	    ((plusp (instruction-args item))
	     (setf number-of-args (instruction-args item))
	     (format t "~%~A" (instruction->token item)))
	    (t
	     (format t "~%~A" (instruction->token item))))))
  build-object)

(defun generate-code (parse code-vector number-of-args
			    &optional (stack-depth 0))
  (do* ((remaining-tokens parse (cdr remaining-tokens))
	(token (car remaining-tokens) (car remaining-tokens)))
       ((null (cdr remaining-tokens))
	(issue-instruction token code-vector))
    (cond ((consp token)
	   (generate-code token code-vector number-of-args stack-depth)
	   (incf stack-depth))
	  ((build-function-arg? token)
	   (issue-stack-ref token stack-depth code-vector number-of-args)
	   (incf stack-depth))
	  ;; otherwise, we push
	  ;; we may eventually want to be more type specific here
	  ((typep token 'fixnum)
	   (incf stack-depth)
	   (issue-fixnum-push token code-vector))
	  (t
	   (incf stack-depth)
	   (issue-pointer-push token code-vector)))))


(defun issue-instruction (token code-vector)
  (if (build-fun+args? token)
      (let ((instruction (token->instruction (build-fun+args-function token))))
	(cond ((null instruction)
	       (error "No instruction defined for the token ~A" token))
	      (t
	       (sv-append code-vector instruction)
	       (dolist (arg (build-fun+args-args token))
		 (sv-append code-vector arg)))))
      (let ((instruction (token->instruction token)))
	(if (null instruction)
	    (error "No instruction defined for the token ~A" token)
	    (sv-append code-vector instruction)))))

(defun issue-stack-ref (arg current-depth code-vector number-of-args)
  (let ((arg-offset (build-function-arg-argpos arg)))
    (sv-append code-vector (token->instruction 'push-offset-stack-ref))
    (sv-append code-vector (+ current-depth (- number-of-args arg-offset)))))

(defun issue-fixnum-push (num code-vector)
  (sv-append code-vector (token->instruction 'push-next))
  (sv-append code-vector num))

(defun issue-pointer-push (token code-vector)
  (sv-append code-vector (token->instruction 'push-next))
  (sv-append code-vector token))



;;; this is used by the evaluator whenever it encounters
(defun funcall-compiled-boxer-function (object)
  (let ((number-of-args (length (compiled-boxer-object-args object))))
    ;; put the args in the (other) stack
    (push-args-onto-stack number-of-args)
    (execute-compiled-boxer-function-internal
     (compiled-boxer-object-code object))
    (prog1
	;; first, pop the returned value
	(cbos-pop)
      ;; then pop the formal args
      (dotimes (i number-of-args) (cbos-pop)))))

;;; the 0 args case is handled specially (to avoid recursive eval)
(defun funcall-0-arg-compiled-boxer-function (object)
  ;; this willleave the returned value on the top of the stack
  (execute-compiled-boxer-function-internal
   (compiled-boxer-object-code object))
  ;; pop the answer....
  (cbos-pop))

;; this loops through the instructions in the code-vector
;; It assumes that any args have already been pushed onto the
;; (alternative) stack.

(defun execute-compiled-boxer-function-internal (code)
  (check-sv-arg code)
  (let ((instr-ptr 0)
	(end (storage-vector-active-length code))
	(codevector (%sv-contents code)))
    (flet ((get-instr ()
	       (prog1 (svref& codevector instr-ptr) (incf& instr-ptr))))
      (do ((current-instruction (get-instr) (get-instr)))
	  ((>& instr-ptr end))
	(let ((argcount (instruction-args current-instruction))
	      (instruction-function (instruction->token current-instruction)))
	  (cond ((zerop& argcount)
		 (when *debug-stack-machine*
		   (format *trace-output* "~%Executing ~A"
			   instruction-function))
		 (funcall instruction-function))
		(t
		 (let ((args (make-list argcount)))
		   #+lucid(declare (lcl::dynamic-extent args))
		   (dotimes (i argcount) (setf (nth i args) (get-instr)))
		   (when *debug-stack-machine*
		     (format *trace-output* "~%Executing ~A on ~A"
			     instruction-function args))
		   (apply instruction-function args)))))))))




;;; Common Instructions

(define-instruction (push-next 0) (obj) (cbos-push obj))

(define-instruction (push-offset-stack-ref 1) (offset)
  (cbos-push-stack-ref offset))

(define-instruction (push-symbol-value 2) (symbol)
  (cbos-push (eval::boxer-symeval symbol)))




#|

(define-instruction (test-beep 254) ()
  (dotimes (i (cbos-pop)) (bw::beep))
  (cbos-push eval::*novalue*))

|#
