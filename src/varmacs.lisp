;;;-*-LISP-*-
#|


 $Header: varmacs.lisp,v 1.0 90/01/24 22:18:58 boxer Exp $

 $Log:	varmacs.lisp,v $
;;;Revision 1.0  90/01/24  22:18:58  boxer
;;;Initial revision
;;;


        Copyright 1987 - 1996 Regents of the University of California

     Enhancements and Modifications Copyright 1999 - 2006 Pyxisystems LLC

                                      +-------+
             This file is part of the | Boxer | System
                                      +-Data--+



Modification History (most recent at the top)

 2/11/02 merged current LW and MCL files

|#

#-(or lispworks mcl) (in-package 'eval)
#+(or lispworks mcl) (in-package :eval)




(defvar *number-of-eval-state-vars* 0)
(defvar *eval-state-vars* nil)

;;; Evaluator variables.
;;; The definitions themselves are in the VARS file.
;;; :LOCAL-P makes the variable local to the evaluator.
;;; :LOCAL-INITIAL-VALUE is the variable's initial value in the LET.
;;; :GLOBAL-P makes the variable global.
;;; :GLOBAL-INITIAL-VALUE sets its value once (a la DEFVAR).

(defstruct (eval-state-vars-info
	    (:constructor make-eval-state-vars-info
			  (variable-name
			     local-p
			     local-initial-value
			     global-p
			     global-initial-value))
	    (:conc-name evsi-))
  variable-name
  local-p
  local-initial-value
  global-p
  global-initial-value)

(defvar *original-dvbsv*)

(defun dummy-make-eval-state-vars-info (variable-name
					local-p
					local-initial-value
					global-p
					global-initial-value)
  (let ((new (make-eval-state-vars-info variable-name
			     local-p
			     local-initial-value
			     global-p
			     global-initial-value)))
    (if (eq variable-name '*dynamic-variables-bottom*)
	(setq *original-dvbsv* new)
	new)))

;;; Eval state vars live in a sorted list, so re-ordering the
;;; definitions won't matter.  Changing their names, however
;;; does matter.  All of them must be defined before any macros
;;; depending on their ordering information (i.e., process macros)
;;; can be compiled.  At this point, this means definitions are in
;;; one file.

(defvar *eval-state-vars-definitions-closed* nil)

(defun begin-eval-var-definitions ()
  (setq *eval-state-vars-definitions-closed* nil)
  (setq *eval-state-vars* nil))

;; all functions which use macros that depend on the ordering of eval state vars should
;; call this macro with the name of the function.
(defun end-eval-var-definitions ()
  (setq *eval-state-vars* (sort *eval-state-vars* #'string-lessp :key #'evsi-variable-name))
  (setq *eval-state-vars-definitions-closed* t)
  nil)

(defun declare-eval-state-variable-index-macro ()
  (when (null *eval-state-vars-definitions-closed*)
    (warn "Attempt to make use of eval-var ordering before all variables defined"))
  nil)

(defun record-eval-var (variable-name local-p local-initial-value
				      global-p global-initial-value)
  (let ((existing-entry (find variable-name *eval-state-vars*
			      :key #'evsi-variable-name)))
    (cond ((not (null existing-entry))
	   (setf (evsi-local-p existing-entry) local-p)
	   (setf (evsi-local-initial-value existing-entry) local-initial-value)
	   (setf (evsi-global-p existing-entry) global-p)
	   (setf (evsi-global-initial-value existing-entry)
		 global-initial-value))
	  (t
	   (cond (*eval-state-vars-definitions-closed*
		  (warn "Attempt to define a new eval-var when definitions were closed."))
		 (t (incf *number-of-eval-state-vars*)
		    (push (dummy-make-eval-state-vars-info
			   variable-name local-p local-initial-value
			   global-p global-initial-value)
			  *eval-state-vars*))))))
  variable-name)

(defun eval-var-defined? (variable-name)
  (find variable-name *eval-state-vars* :key #'evsi-variable-name))    

(defmacro define-eval-var (name &key 
				(local nil local-specified-p)
				(global nil global-specified-p))
  (when (and local-specified-p global-specified-p)
    (error "~%DEFINE-EVAL-VAR ~S: You may not specify both LOCAL and GLOBAL values"
	   name))
  `(progn
     ,(if global-specified-p
	  ;; we don't put the global value into the defvar
	  ;; because the initializer might not be defined at this point.
	  `(defvar ,name))
     (record-eval-var ',name ',local-specified-p ',local ',global-specified-p ',global)))

;; this is yukky.  maybe we should do it as a macro,
;; but then we'd have to recompile it every time we
;; add an eval var, and it would have to be in a different
;; file.
(defun init-global-eval-vars ()
  (init-process-state-vector)
  (dolist (var *eval-state-vars*)
    (setf (symbol-value (evsi-variable-name var))
          ;; this is supposed to replace (eval (evsi-global-initial-value var))
          ;; for the limited domain of evsi-global-initial-values
	  (let ((global (evsi-global-initial-value var)))
            (cond ((null global) nil)
                  ((numberp global) global)
                  ((symbolp global) (symbol-value global))
                  ((listp global) 
                   (let ((fun (car global)))
                     (cond ((eq fun 'quote) (cadr global))
                           (t (apply fun (cdr global))))))
                  (t (error "Don't know how to initialize ~S from ~S"
                            (evsi-variable-name var) global)))))))
  

;;;
;;; Macros for setting up sprite triggers.
;;;

;; we should move these to the eval package, but
;; for now they will stay in the boxer package
;; so we don't have to recompile the prims files.

(defvar boxer::*sprite-update-functions* nil)

(defsubst boxer::sprite-update-function? (symbol)
  (fast-memq symbol boxer::*sprite-update-functions*))

(defmacro boxer::add-sprite-update-function (slot-name name-descriptor)
  `(progn
     (when (and (symbolp ',name-descriptor)
		(not (member ',name-descriptor
			     boxer::*sprite-update-functions*)))
       (setf (get ',slot-name 'boxer::update-function-name)
	     ',name-descriptor))))
