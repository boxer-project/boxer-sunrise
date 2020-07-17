;;; -*- Package: BOXER-EVAL; Mode: LISP; Base: 10; Syntax: Common-lisp -*-
;;;
;;; $Header: funs.lisp,v 1.0 90/01/24 22:12:04 boxer Exp $
;;;
;;; $Log:	funs.lisp,v $
;;;Revision 1.0  90/01/24  22:12:04  boxer
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

 2/11/03 merged current LW and MCL files
 6/04/02 better COMPILED-BOXER-FUNCTION-NAME for LWW and generic
 9/18/01 updated COMPILED-BOXER-FUNCTION-NAME for lispworks
 2/13/01 merged current LW and MCL files
12/16/99 added placeholder stub for compiled-boxer-function-name
 5/04/99 make sure we always use VC mechanism in
         convert-editor-box-to-interpreted-procedure-internal
 5/04/99 starting logging changes: source = Boxer version 2.3 pre-release

|#

;;;

(in-package :boxer-eval)

;;;; FUNS
;;;; Functions and Macros for making and using Boxer functions.
;;;;


;;; The BOXER-FUNCTION struct is defined to give us the selectors
;;; BOXER-FUNCTION-ARGLIST and BOXER-FUNCTION-PRECEDENCE.  We
;;; don't want a constructor or a predicate for the BOXER-FUNCTION
;;; type per se, since it is just a "mix-in," and this defstruct
;;; would do arbitrarily hairy things.

;;;(defstruct (boxer-function
;;;	    :named (:type vector)
;;;	    (:predicate nil)
;;;	    (:constructor nil))
;;;   arglist
;;;   precedence
;;;   (special-info nil))

(defmacro boxer-function-arglist (x)
  `(svref ,x 1))
(defmacro boxer-function-precedence (x)
  `(svref ,x 2))
(defmacro boxer-function-infix-p (x)
  `(svref ,x 3))

(defstruct (interpreted-boxer-function
        :named (:type vector)
        (:predicate nil))
  arglist
  precedence
  infix-p
  text
  locals				;doesn't include INPUT locals
  lexical-call-p
  backpointer
  reversed-arg-names)

(defstruct (compiled-boxer-function
        :named (:type vector)
        (:predicate nil))
  arglist
  precedence
  infix-p
  object)

;;; The defstruct-generated predicates are too slow by a factor
;;; of ten!  Sorry Charlie, but until they fix it we use these
;;; somewhat implementation-dependent ones.

;;; Use BOXER-FUNCTION? to check the base object if you need to.
;;; Use the other functions below when you just want to know which
;;; kind you have when you know you have one or the other.
(defmacro interpreted-boxer-function? (object)
  `(eq 'interpreted-boxer-function (svref ,object 0)))

(defmacro compiled-boxer-function? (object)
  `(eq 'compiled-boxer-function (svref ,object 0)))

(defun boxer-function? (object)
  (and (possible-eval-object? object)
       (or (interpreted-boxer-function? object)
       (compiled-boxer-function? object))))

;; Use this on a symbol to see if it represents an infix function.
(boxer::defsubst infix-function-symbol-p (symbol)
  (fast-memq symbol *infix-symbol-list*))

;; Use this on a boxer function object to see if it's infix.
;; sgithens TODO (defsubst infix-function-p (function)
;;   (boxer-function-infix-p function))

;;; Use this function for the debugger.
;; ****************LISTP is wrong here

;; really only a placeholder
#+lispworks
(defun compiled-boxer-function-name (function)
  (let ((function (compiled-boxer-function-object function)))
    (if #+win32 (system::compiled-code-p function)
        #+mac   (system::simple-compiled-function-p function)
        (system::function-dspec function)
      ;; try the common lisp thing
      (multiple-value-bind (exp env name)
          (function-lambda-expression function)
        (declare (ignore exp env))
        name))))

#-(or lispm lucid excl mcl lispworks)
(defun compiled-boxer-function-name (function)
 ;  (warn
 ;   "~%The function, Compiled-Boxer-Function-Name, needs to be defined for ~A"
 ;   (lisp-implementation-type))
 (multiple-value-bind (exp env name)
     (function-lambda-expression function)
   (declare (ignore exp env))
   name))


;;; BOXER-FUNCTION-NARGS should be called sparingly, since it calls
;;; length.  The evaulator doesn't use it.
(defun boxer-function-nargs (object)
  (length (the list (boxer-function-arglist object))))

;;; This default precedence works for all prefix functions except NOT,
;;; which needs to be treated "like" a multi-arg function.
(defun calculate-default-precedence (arglist)
  (cond ((null arglist) *default-precedence*)
    ((null (cdr arglist)) *default-single-arg-precedence*)
    (t *default-multi-arg-precedence*)))

;; This should be used sparingly, like only in defboxer-primitive, but
;; making it a LABELS makes the code too ugly.
(defun numberize-flavor? (arg)
  (and (consp arg)
       (member 'numberize arg)))

;;; Takes a flavorized arglist and returns one with the flavors as the
;;; evaluator wants to see them.
;;;
;;; NUMBERIZE gets replaced with DONT-COPY, because the NUMBERIZE part
;;; is handled in DEFBOXER-PRIMITIVE.
;;; Evaluator-handled flavors of the first arg of infix functions
;;; will get ignored, since the value is produced before the evaluator
;;; gets to the flavor.
(defun canonicalize-arglist-flavors (arglist)
  (mapcar #'(lambda (arg-entry)
          (cond ((numberize-flavor? arg-entry)
             (cons 'dont-copy (delete 'numberize arg-entry)))
            (t arg-entry)))
      arglist))

;;; The main function for defining primitives.
(defmacro defboxer-primitive (name-descriptor arglist &body body)
  (let* ((name (if (symbolp name-descriptor) name-descriptor
                   (car name-descriptor)))
         (precedence (if (symbolp name-descriptor)
                         (calculate-default-precedence arglist)
                         (cadr name-descriptor)))
         (infixp (and (consp name-descriptor)
                      (not (null (member 'infix name-descriptor))))))
    `(progn
       (proclaim '(special ,name))
       ,(when infixp
      `(unless (member ',name *infix-symbol-list*)
         (push ',name *infix-symbol-list*)))
       (defun ,name nil
     (let ,(mapcar #'(lambda (u)
               `(,(if (symbolp u) u (cadr u))
                 (vpdl-pop-no-test)))
        (reverse arglist))
       ,(when (some #'numberize-flavor? arglist)
          `(progn . ,(mapcan
              #'(lambda (var)
                  (if (numberize-flavor? var)
                  (list `(setq ,(cadr var)
                           (boxer::numberize-or-error
                        ,(cadr var))))
                  nil))
              (reverse arglist))))
       (progn . ,body)))
       (boxer-toplevel-set ',name
               (make-compiled-boxer-function
                :arglist ',(canonicalize-arglist-flavors arglist)
                :precedence ,precedence
                :infix-p ',infixp
                :object #',name)))))

#|				      (list `(progn (setq bad-arg ,(cadr var))
                            (setq ,(cadr var)
                              (boxer::numberize-or-nil
                               ,(cadr var)))))
|#

;;;;
;;;; Functions for defining primities which do recursive evaluation
;;;;

;;; defrecursive-eval-primitive is somewhat of a misnomer.
;;; the :after stuff only gets run at the end of the line,
;;; so it's only useful for primitives that take an &rest arg of stuff to eval.

;;; NAME is the name of the boxer primitive.
;;; ARGLIST is the arglist of the boxer primitive.
;;; :STACK-FRAME-ALLOCATION (INITIAL-COUNT GROW-SIZE)
;;; :STATE-VARIABLES is the list of variables where the state of the function
;;; 		     lives during the recursive evaluations.
;;; :BEFORE is the form which gets run before the recursive eval/funcall.
;;;         This part of the function should look at the args, and
;;;         if it decides to do a recursive eval/funcall it should:
;;;          call SET-AND-SAVE-STATE-VARIABLES (with the args in the same
;;;          order as above).  Call either RECURSIVE-EVAL-INVOKE or
;;;          RECURSIVE-FUNCALL-INVOKE as the LAST SUBFORM.  If it doesn't,
;;;          the recursive eval/funcall won't happen.
;;; :AFTER is the form which gets run after the recursive eval/funcall.
;;;        It has can examine, modify, or restore the state variables.
;;;        It should either call RESTORE-STATE-VARIABLES and return NIL, or if
;;;        desired, a new list to run (for RECURSIVE-EVAL only).
;;; :UNWIND-PROTECT-FORM is the form which gets run when the stack is unwinding
;;;                      because of an error or throw.  It may be omitted if
;;; nothing other than (RESTORE-STATE-VARIABLES) need be done.
;;;
(defmacro defrecursive-eval-primitive
    (name arglist
      &key state-variables stack-frame-allocation
      before after
      unwind-protect-form)
  (unless (eq 'LIST-REST (caar (last arglist)))
    (error "Bugs in the implementation of DEFRECURSIVE-EVAL-PRIMITIVE will prevent this from working: ~S ~S" name arglist))
  (let ((FRAME-NAME (intern
             (concatenate 'string (symbol-name name) "-SPECIAL-FRAME")
             'boxer-eval))
    (USUAL-STATE-VARIABLES '(*RUN-LIST-SFUN-EPILOG-HANDLER*
                 *EXECUTING-POINTER*)))
    `(progn
       (DEFINE-STACK-FRAME (,frame-name .,(or stack-frame-allocation
                          '(10 5 10 10)))
       ,(format nil "Making more room for ~A" name)
     ,unwind-protect-form
     ,@usual-state-variables
     . ,state-variables)
       (DEFBOXER-PRIMITIVE ,name ,arglist
     (macrolet ((SET-AND-SAVE-STATE-VARIABLES
            (&rest values)
              `(progn (pdl-push-frame
                   ,',frame-name
                   ;; +++ this previously contained an illegal
                   ;; backquote construct, I'm hoping this has
                   ;; the right effect
                   . ,',(append usual-state-variables
                        state-variables))
                  .,(mapcar #'(lambda (var val)
                        `(setq ,var ,val))
                    ',state-variables values)))
            (RESTORE-STATE-VARIABLES
            ()
              `(pdl-pop-frame ,',frame-name
                                      ;; +++ see above
                                      . ,',(append usual-state-variables
                           state-variables)))
            (RECURSIVE-EVAL-INVOKE
            (list)
              `(progn
             (setq *sfun-continuation*
                   '*run-list-sfun-continuation*)
             (setq *run-list-sfun-epilog-handler*
                   #'(lambda () (progn ,',after)))
             ,list)))
       ,before)))))



#|
(setq *run-list-sfun-epilog-handler*
      #'(lambda ()
      (cond ((not (null *stop-flag*))
         (funcall ,',(get-stack-frame-unwinder
                  frame-name))
         nil)
        (t ,',after))))
|#


;;; NAME is the name of the boxer primitive.
;;; ARGLIST is the arglist of the boxer primitive.
;;; :STACK-FRAME-ALLOCATION (INITIAL-COUNT GROW-SIZE)
;;; :STATE-VARIABLES is the list of variables where the state of the function
;;; 		     lives during the recursive evaluations.
;;; :BEFORE is the form which gets run before the recursive funcall.
;;;         This part of the function should look at the args, and
;;;         if it decides to do a recursive funcall it should:
;;;          call SET-AND-SAVE-STATE-VARIABLES (with the args
;;;             in the same order as above).
;;;          call RECURSIVE-FUNCALL-INVOKE as the LAST SUBFORM.
;;;          If it doesn't, the recursive funcall won't happen.
;;; :AFTER is the form which gets run after the recursive funcall.
;;;        It has can examine, modify, or restore the state variables.
;;;        It should either call RESTORE-STATE-VARIABLES and return NIL, or if
;;;        desired, a new list to run (for RECURSIVE-EVAL only).????????
;;; :UNWIND-PROTECT-FORM is the form which gets run when the stack is unwinding
;;;                      because of an error or throw.  It may be omitted if
;;;                      nothing other than (RESTORE-STATE-VARIABLES) need
;;;                      be done.

(defmacro defrecursive-funcall-primitive
  (name arglist
    &key state-variables stack-frame-allocation
    before after
    unwind-protect-form)
  (let ((*package* (find-package "BOXER-EVAL")))
    (let ((FRAME-NAME (intern
               (concatenate 'string (symbol-name name)
                    "-SPECIAL-FRAME")
               'boxer-eval))
      (USUAL-STATE-VARIABLES '(*UFUNCALL-SFUN-EPILOG-HANDLER*)))
      `(progn
     ,.(mapcar #'(lambda (var) (when (not (eval-var-defined? var))
                     `(define-eval-var ,var :global nil)))
           state-variables)
     (DEFINE-STACK-FRAME (,frame-name .,(or stack-frame-allocation
                        '(10 5 10 10)))
         ,(format nil "Making more room for ~A" name)
       ,unwind-protect-form
       ,@usual-state-variables
       . ,state-variables)
     (DEFBOXER-PRIMITIVE ,name ,arglist
       (macrolet ((SET-AND-SAVE-STATE-VARIABLES
               (&rest values)
               `(progn (pdl-push-frame ,',frame-name
                           ,',@usual-state-variables
                           . ,',state-variables)
                   .,(mapcar #'(lambda (var val)
                         `(setq ,var ,val))
                     ',state-variables values)))
              (RESTORE-STATE-VARIABLES
               ()
               `(pdl-pop-frame ,',frame-name
                       ,',@usual-state-variables
                       . ,',state-variables))
              (RECURSIVE-FUNCALL-INVOKE
               (function)
               `(progn
              (setq *sfun-continuation*
                '*ufuncall-sfun-result-sfun-continuation*)
              (setq *ufuncall-sfun-epilog-handler*
                #'(lambda () ,',after))
              ,function)))
             ,before))))))


;;; Use RECURSIVE-EVAL-SETUP-LIST when possible!
;;; Use RECURSIVE-EVAL-SETUP-APPEND when you need to copy the list.
;;; Use RECURSIVE-EVAL-SETUP-NCONC when can't use
;;;  RECURSIVE-EVAL-SETUP-LIST* because you need a function,
;;; but can destroy the list.
;;; All of these forms have to be the last form in the primitive.

;;; This form does append.
(defun recursive-eval-setup-append (list)
  (setq *sfun-continuation* '*run-list-sfun-continuation*)
  (append list *executing-pointer*))

;;; And this form modifies its argument.
(defun recursive-eval-setup-nconc (modifiable-list)
  (setq *sfun-continuation* '*run-list-sfun-continuation*)
  (nconc modifiable-list *executing-pointer*))

;;; And this form is macro which takes multiple args.
(defmacro recursive-eval-setup-list (&rest args)
  `(progn (setq *sfun-continuation* '*run-list-sfun-continuation*)
      (list* ,@args *executing-pointer*)))

(defmacro recursive-eval-setup-list* (&rest args)
  `(progn (setq *sfun-continuation* '*run-list-sfun-continuation*)
      (list* ,@args *executing-pointer*)))

;;; And use this when all else fails and you decided to do it yourself.
(defun recursive-eval-setup-manual (list)
  (setq *sfun-continuation* '*run-list-sfun-continuation*)
  list)

(defun make-boxer-primitive-internal (arglist code)
  (let ((name (gensym)))
    (proclaim `(special ,name))
    (let ((lisp-function-object
        (compile-lambda-if-possible
          name
          `(lambda ()
         (let ,(mapcar
             #'(lambda (u) `(,u (vpdl-pop-no-test)))
             (reverse arglist))
           . ,code)))))
      (boxer-toplevel-set
    name
    (make-compiled-boxer-function
      :arglist arglist
      :precedence 0
      :infix-p nil
      :object lisp-function-object)))
    name))



;;;; Functions which define keys.
;;;;
;; set in MAKE-INPUT-DEVICES (has to be declared here because the key
;; definition functions may need to use it to determine the appropriate
;; shifted key names)
(defvar boxer::*current-input-device-platform* nil
  "The input platform associated with the existing input device names")

;;; Use exclusively this function for defining keys, and define the key
;;; function itself using DEFBOXER-COMMAND.  Using this function insures
;;; that the -KEY value won't be cached in lexical environments.  (Just
;;; think of the mess if every key you ever typed were cached in a
;;; lexical environment!)
(defmacro defboxer-key (key-spec function)
  (let* ((shift-bits (if (listp key-spec) (cadr key-spec) 0))
     (key-name
      (if (zerop shift-bits)
          key-spec
          (boxer::intern-in-bu-package
           (symbol-format nil "~A-~A" (boxer::get-shift-names shift-bits)
                  (car key-spec))))))
    `(progn
      (proclaim '(special ,key-name))
      (boxer::record-command-key ',key-name ',function)
      ;; used by top level mode
      (boxer::record-vanilla-key ',key-name ',function)
      (boxer-toplevel-set-nocache ',key-name
       (boxer-eval::encapsulate-key-function ',function)))))

(defun defboxer-key-internal (key-spec function)
  (let* ((shift-bits (if (listp key-spec) (cadr key-spec) 0))
     (key-name
      (if (zerop shift-bits)
          key-spec
          (boxer::intern-in-bu-package
           (symbol-format nil "~A-~A"
                  (boxer::get-shift-names shift-bits)
                  (car key-spec))))))
    (proclaim `(special ,key-name))
    (boxer::record-command-key-internal key-name function)
;+++ this is unused  (boxer::record-command-key-internal key-name function)
;  #+mcl
;  (when (symbolp function)
;    (pushnew key-name (get function :on-keys)))       ; this is new +++
    ;; used by top level mode
    (boxer::record-vanilla-key key-name function)
    (boxer-toplevel-set-nocache key-name
                (boxer-eval::encapsulate-key-function function))))

(eval-when (compile load eval)
(defun encapsulate-key-function (fun)
  (make-compiled-boxer-function
    :arglist nil
    :precedence *default-precedence*
    :object fun))
)
;;;;
;;;; Functions for making internal Boxer function objects out of boxes.
;;;;

;; still to come -- virtual ports to doit and data
(defmethod cached-code-editor-box ((box boxer::box))
  (or (slot-value box 'cached-code)
      (let ((new-fun (convert-editor-box-to-interpreted-procedure-internal box)))
    (setf (slot-value box 'cached-code) new-fun)
    new-fun)))

(defmethod cached-code-editor-port ((port boxer::port-box))
  (let ((box-to-parse (boxer::get-port-target port)))
    (or ;; bypass the cache for now (slot-value box-to-parse 'cached-code)
    (let ((new-fun (convert-editor-box-to-interpreted-procedure-internal
            box-to-parse)))
      (setf (interpreted-boxer-function-lexical-call-p new-fun) t)
      (setf (slot-value port 'cached-code) new-fun)
      new-fun))))

(defun cached-code-virtual-copy (vc)
  (let ((editor-box (boxer::vc-progenitor vc)))
    (if (null editor-box)
    (convert-virtual-copy-to-interpreted-procedure-internal vc)
    (or (slot-value editor-box 'cached-code)
        (let ((new-fun
           (convert-editor-box-to-interpreted-procedure-internal
            editor-box)))
          (when (boxer::editor-box-cacheable?
             editor-box
             (or (boxer::vc-creation-time vc) (boxer::now)))
        (setf (slot-value editor-box 'cached-code) new-fun))
          new-fun)))))

;;; +++ I changed this to expect vp's to point to real data-boxes, rather than vector representations
;;; of data-boxes, since that's what it's getting called with.  I added a check to make sure this is
;;; really always the case, which should be removed eventually.  -- mt

;;; VP's can point to both kinds of boxes -- ehl

(defun cached-code-virtual-port (vp)
  ;; VPs are made either as top level copies from editor ports or by
  ;; PORT-TO from editor or eval boxes.  The backpointer points to
  ;; the original editor port, if it exists.
  ;; If there is a backpointer then the target is either an editor doit
  ;; or editor data, and we just call cached-code-editor-port.
  ;; If there's no backpointer, the target is either an eval data or
  ;; editor data box and we can't cache the code (because we won't know
  ;; when it becomes invalid.)
  ;; We don't have ports to eval doit boxes.
  (if (null (boxer::vp-editor-port-backpointer vp))
      (let ((new-fun (if (fast-eval-data-box?
              (boxer::vp-target vp))
             (convert-virtual-copy-to-interpreted-procedure-internal
              (boxer::vp-target vp))
             (convert-editor-box-to-interpreted-procedure-internal
              (boxer::vp-target vp)))))
    (setf (interpreted-boxer-function-lexical-call-p new-fun) t)
    new-fun)
      (cached-code-editor-port (boxer::vp-editor-port-backpointer vp))))

(defmethod cached-code ((box boxer::box))
  box
  (error "cached-code: Call either cached-code-editor-box, cached-code-editor-port, cached-code-virtual-copy or cached-code-virtual-port"))

(defvar *flavored-input-markers* '(bu::datafy bu::port-to bu::dont-port
                   bu::box-rest))

(defun flavored-input-marker? (symbol)
  (fast-memq symbol *flavored-input-markers*))

;;; This is the function that does all the hairy calculation for making boxer
;;; functions from doit boxes, data boxes, or ports.  If there is more than
;;; one set of virtual copy rows for the editor box, then this box has been
;;; CHANGEd but not updated (since we haven't yet returned to top level).
;;; If that's the case we call some even hairier function for getting the
;;; eval objects.  This uncertainty doesn't affect the cacheability of the
;;; resulting code, since if there are subsequent modifications the code will
;;; be flushed, and if there are references still to the older box the
;;; EDITOR-BOX-CACHEABLE? mechanism will detect that via time stamps.
;;; new version which allows boxes in input line (and removes them from
;;; interpreted-boxer-function-locals)

(defun convert-editor-box-to-interpreted-procedure-internal (box)
  (let* ((list (boxer::get-current-editor-box-items box))
     (argsp (fast-memq (caar list) boxer::*symbols-for-input-line*))
     (boxes-in-arglist nil)
     (arglist nil)
     (reversed-arg-names nil))
    (catch 'bad
      (unless (null argsp)
        (flet ((process-input-object (chunk symbol)
             (cond ((eq symbol '*ignoring-definition-object*)
                        ;; a named box....
                (let* ((input-box (boxer::chunk-pname
                                           (boxer::get-pointer-value chunk
                                                                     box)))
                               (input-box-name-row-objs
                                (boxer::eval-objs(boxer::name-row input-box))))
                          (push input-box boxes-in-arglist)
                          (values
                           (if (null (cdr input-box-name-row-objs))
                             (car input-box-name-row-objs)
                             (boxer::get-box-name
                              (boxer::name-row input-box)))
                           (when (port-box? input-box) t))))
                       ((symbolp symbol) symbol)
                       (t
                        ;; something else, probably an unnamed box
                        (let ((thing (box::get-pointer-value chunk box)))
                          (setq argsp nil)
                          (setq list `((bu::error "Bad Input Object" ,(if (box::chunk-p thing)
                                                                        (box::chunk-chunk thing)
                                                                        thing))))
                          (throw 'bad nil))))))
      (setq arglist
            (with-collection
          (do ((chunks-sublist (cdr (boxer::chunks
                         (boxer::first-inferior-row box)))
                       (cdr chunks-sublist))
               (symbols-sublist (cdr (car list))
                    (cdr symbols-sublist)))
              ((null symbols-sublist))
            (multiple-value-bind (input-symbol port-flavored?)
                             (process-input-object (car chunks-sublist)
                                           (car symbols-sublist))
              (cond ((flavored-input-marker? input-symbol)
                 (unless (null (cdr symbols-sublist))
                   (let ((arg-name (process-input-object
                        (cadr chunks-sublist)
                        (cadr symbols-sublist))))
                 (push arg-name reversed-arg-names)
                 (collect (list input-symbol arg-name))
                 (setq symbols-sublist (cdr symbols-sublist))
                 (setq chunks-sublist (cdr chunks-sublist)))))
                ((not (null port-flavored?))
                 (push input-symbol reversed-arg-names)
                 (collect (list 'bu::port-to input-symbol)))
                (t (push input-symbol reversed-arg-names)
                   (collect input-symbol))))))))))
    (make-interpreted-boxer-function
     :arglist arglist
     :reversed-arg-names reversed-arg-names
     :backpointer box
     :locals (remove-if #'(lambda (local)
                (fast-memq (static-variable-value local)
                       boxes-in-arglist))
            (slot-value box 'static-variables-alist))
     :precedence (calculate-default-precedence arglist)
     :lexical-call-p nil
     :text (if argsp (cdr list) list))))

;;; This function does hairy calculation too, but we can't cache the result.
(defun convert-virtual-copy-to-interpreted-procedure-internal (box)
  (let* ((list (boxer::raw-unboxed-items box))
     (argsp (member (caar list) boxer::*symbols-for-input-line*))
     (arglist nil)
     (reversed-arg-names nil))
    (when argsp
      (with-collection
      (do* ((sublist (cdr (car list)) (cdr sublist))
        (symbol (car sublist) (car sublist)))
           ((null sublist))
        (cond ((flavored-input-marker? symbol)
           (unless (null (cdr sublist))
             (push (cadr sublist) reversed-arg-names)
             (collect (list symbol (cadr sublist)))
             (setq sublist (cdr sublist))))
          (t (push symbol reversed-arg-names)
             (collect symbol))))))
    (make-interpreted-boxer-function
     :arglist arglist
     :reversed-arg-names reversed-arg-names
     :backpointer box
     :locals nil			; needs to be fixed at some point
     :precedence (calculate-default-precedence arglist)
     :lexical-call-p nil
     :text (if argsp (cdr list) list))))

#|
(defun convert-data-to-function (box-or-evbox)
  (cond ((fast-eval-data-box? box-or-evbox)
     (cached-code-virtual-copy box-or-evbox))
    ((data-box? box-or-evbox)
     (cached-code-editor-box box-or-evbox))
    (t (error "Unknown object: ~S" box-or-evbox))))
|#

;;; temporary hack to get RUN to work with ports
(defun convert-data-to-function (box-or-evbox)
  (cond ((numberp box-or-evbox)
     (make-interpreted-boxer-function
      :arglist nil
      :reversed-arg-names nil
      :backpointer nil
      :locals nil
      :precedence (calculate-default-precedence nil)
      :lexical-call-p nil
      :text (list (list box-or-evbox))))
    ((fast-eval-data-box? box-or-evbox)
     (cached-code-virtual-copy box-or-evbox))
    ((data-box? box-or-evbox)
     (cached-code-editor-box box-or-evbox))
    ((fast-eval-port-box? box-or-evbox)
     (cached-code-virtual-port box-or-evbox))
    ((port-box? box-or-evbox)
     (cached-code-editor-port box-or-evbox))
    (t (error "Unknown object: ~S" box-or-evbox))))

(defun make-interpreted-procedure-from-list (lines)
  (make-interpreted-boxer-function :arglist nil
                   :text lines))

#|
;; Perhaps this should be like convert-data-to-function ?
(defun convert-doit-to-function (box)
  ;; For now we assume that NO changes have
  ;; been perpetrated on the virtual copy of the
  ;; doit box.  Later, when VC settles down,
  ;; we will check to see that no changes have occurred,
  ;; and if they have, then we have to call
  ;; something like convert-editor-box-to-interpreted-procedure-internal
  ;; except that it would be convert-vc-box... and it wouldn't
  ;; cache the code in the vc or anything.
  (cached-code
    (if (fast-eval-doit-box? box)
    (boxer::vc-progenitor box)
    box)))

;; either kind of port to either kind of box
;; actually, vc port to doit box isn't handled
;; or even needed (probly)
;; For now we just take the code object from the
;; doit or vc-doit, copy the structure itself,
;; and set the lexical-call-p slot to t.
;; It would be nice to cache this somehow.
;; Maybe the right thing to do is to have a *function-lexical-p*
;; which is bound in the evaluator at the same time as *function*.
;; Ug.
(defun convert-port-to-function (box)
  (let ((code (copy-interpreted-boxer-function
           (cached-code (if (fast-eval-port-box? box)
                (boxer::vp-target box)
                (boxer::get-port-target box))))))
    (setf (interpreted-boxer-function-lexical-call-p code) t)
    code))

(defun convert-data-to-function (box)
  (cond ((fast-eval-data-box? box)
     (let ((progenitor (boxer::vc-progenitor box)))
       (if (not (null progenitor))
           (cached-code progenitor)
           (convert-virtual-copy-to-interpreted-procedure-internal box))))
    (t (cached-code box))))

|#

