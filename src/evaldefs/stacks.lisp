;;; -*- Syntax: Common-Lisp; Base: 10; Package: EVAL -*-
;;;
;;; $Header: stacks.lisp,v 1.0 90/01/24 22:17:41 boxer Exp $
;;;
;;; $Log:	stacks.lisp,v $
;;;Revision 1.0  90/01/24  22:17:41  boxer
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

 2/11/03 merged current LW and MCL files
 2/17/01 merged current LW and MCL files
 5/12/99 stack growth notices are now controlled by *warn-about-stack-growth*
         which is NIL by default
 5/12/98 Start logging changes: source = boxer version 2.3pre-release

|#

(in-package :boxer-eval)

;;; This file deals with
;;;    the VPDL,
;;;    the PDL,
;;;    the stack frame declarations which the evaluator uses directly,
;;;           (primitives may declare their own pdl frames)
;;;    and the dynamic variable binding/unbinding mechanism.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                    PDL                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *pdl-n-obligatory-slots* (length '(NAME NEXT PREVIOUS)))

;;;; Special Stack Frame Resource Vector
;;;
;;; Stack frame caches need to allocated on a per process basis
;;; since there are (potentially) lots of special types of stack
;;; frames (at least one per recursive eval primitive), we don't
;;; want to define each stack frame cache resource as an evaluator
;;; variable (define-eval-var).
;;;
;;; Instead, for the less common stack frames, we will keep them
;;; in a single per process vector.  Trading off one level of indirection
;;; in stack frame allocation/deallocation for a substantially smaller
;;; process state
;;;
;;; At some point we will probably want to meter what the real usage
;;; is and move some particular frames resources directly into the
;;; process state.  At the moment, ufun-frames, eval-args-frame and
;;; doit-port-funcall-frames are directly in the process state.  All
;;; other frames, expecially those associated with primitives are
;;; accessed indirectly via the *special-eval-frames* vector
;;;
;;defined in vars.lisp
;;(defvar *special-eval-frames*)

;;; compile time variables used to generate offsets for special stack frame
;;; caches in the special stack frame vector

(eval-when (compile load eval)
           (defvar *stack-frame-name-offset-alist* nil)
           (defvar *max-special-stack-frame-offset* 0)
           )

(defun reset-stack-frame-allocation-offsets ()
  (setq *stack-frame-name-offset-alist* nil
        *max-special-stack-frame-offset* 0))

(eval-when (compile load eval)
           (defun special-stack-frame-offset (frame-name)
             (let ((entry (fast-assq frame-name *stack-frame-name-offset-alist*)))
               (cond ((null entry)
                      (push (cons frame-name *max-special-stack-frame-offset*)
                            *stack-frame-name-offset-alist*)
                      (prog1 *max-special-stack-frame-offset*
                             (incf *max-special-stack-frame-offset*)))
                 (t (cdr entry)))))
           )

(defmacro special-frame-source (frame-name)
  `(svref& *special-eval-frames* ,(special-stack-frame-offset frame-name)))

(defvar *special-stack-frame-initializers*)

;;; this called inside of setup-eval
(defun initialize-special-stack-frame-initializers ()
  (let ((inits (make-array *max-special-stack-frame-offset*)))
    (dolist (pair *stack-frame-name-offset-alist*)
      (setf (svref& inits (cdr pair))
            (symbol-function
             (intern-in-eval-package
              ;; this should be the same as the "pool-allocate"
              ;; function name defined below
              (symbol-format nil "ALLOCATE-~A-CACHE" (car pair))))))
    (setq *special-stack-frame-initializers* inits)))

;;; this is called whenever new processes are made and also by
;;; init-global-eval-vars.  It can only be called AFTER
;;; initialize-special-stack-frame-initializers has been called
(defun allocate-special-eval-frame-caches ()
  (let ((special-frames (make-array *max-special-stack-frame-offset*)))
    (dotimes (i *max-special-stack-frame-offset*)
      (let ((fun (svref& *special-stack-frame-initializers* i)))
        (if (null fun)
          (error "No Initializer for special stack frame slot ~D" i)
          (setf (svref& special-frames i) (funcall fun)))))
    special-frames))

;;;; Stack Frame caches consist of a pointer and a contents vector
(defun make-stack-frame-cache (size)
  (let ((contents-vector (make-array size)))
    (boxer::with-stack-list (contents 0 contents-vector)
                            (let ((vector (make-array 2 :initial-contents contents)))
                              vector))))

(defmacro stack-frame-cache-pointer  (sf) `(the fixnum (svref& ,sf 0)))
(defmacro stack-frame-cache-contents (sf) `(the simple-vector (svref& ,sf 1)))
(defmacro stack-frame-cache-size (sf)
  `(the fixnum (length (the simple-vector (stack-frame-cache-contents ,sf)))))

(defvar *warn-about-stack-growth* nil)

;;; The real macro.
(defmacro define-stack-frame ((name initial-quantity
                                    increment-quantity
                                    maximum-preservable-quantity
                                    warning-quantity
                                    &optional (special-frame t) (pool-size 2))
                              growth-notice
                              unwind-protect-form
                              &body slot-names)
  (let ((list-size (+ *pdl-n-obligatory-slots* (length slot-names)))
        (pool   (intern-in-eval-package (symbol-format nil "*~A-POOL*" name)))
        (pool-allocate (intern-in-eval-package
                        (symbol-format nil "ALLOCATE-~A-CACHE" name)))
        (pool-deallocate (intern-in-eval-package
                          (symbol-format nil "DEALLOCATE-~A-CACHE" name)))
        (allocate (intern-in-eval-package (symbol-format nil "ALLOCATE-~A-INTERNAL"
                                                         name)))
        (deallocate (intern-in-eval-package
                     (symbol-format nil "DEALLOCATE-~A-INTERNAL" name)))
        (create (intern-in-eval-package (symbol-format nil "CREATE-NEW-~A-INTERNAL"
                                                       name)))
        (pdl-push-frame (intern-in-eval-package
                         (symbol-format nil "PDL-PUSH-~S-INTERNAL" name)))
        (pdl-pop-frame (intern-in-eval-package
                        (symbol-format nil "PDL-POP-~S-INTERNAL" name)))
        (reset-name (intern-in-eval-package
                     (symbol-format nil "RESET-~S-ALLOCATION" name)))
        (defstruct-conc-string  (symbol-format nil "DBG-~S-" name))
        (unwind-protect-name (intern-in-eval-package
                              (symbol-format nil "UNWIND-~A-SPECIAL-FORM" name))))
    (flet ((source ()
                   (if (null special-frame)
                     (intern-in-eval-package (symbol-format NIL "*~AS*" name))
                     `(special-frame-source ,name)))
           (set-source (new)
                       (if (null special-frame)
                         `(setq ,(intern-in-eval-package (symbol-format NIL "*~AS*" name))
                                 ,new)
                         `(setf (special-frame-source ,name)
                                ,new))))
          `(progn
            (defvar ,pool)
            (setq ,pool
                   (let ((pool nil))
                     (dotimes (i ,pool-size pool)
                       (push (make-n-stack-frames ',initial-quantity ',name
                                                  ',list-size)
                             pool))))
            ;; we could make these smarter and have them sort the caches by size
            (defun ,pool-allocate (&optional cons-new?)
              (or (when (null cons-new?) (pop ,pool))
                  (make-n-stack-frames ',initial-quantity ',name ',list-size)))
            (defun ,pool-deallocate (stack-frame-cache)
              (push stack-frame-cache ,pool))
            ;; this has to appear here so the frame offsets will get
            ;; properly incremented during load time which is used by
            ;; Allocate-Special-Eval-Frame-Caches
            ,(unless (null special-frame)
               `(special-stack-frame-offset ',name))
            (defun ,allocate ()
              (let ((size (stack-frame-cache-size ,(source)))
                    (next-free (stack-frame-cache-pointer ,(source))))
                (when (=& size next-free)
                  (when (and *warn-about-stack-growth*
                             (>=& size ,warning-quantity))
                    (evaluator-notice ,growth-notice))
                  (poll t)
                  (add-n-stack-frames ,(source) ,increment-quantity ',name
                                       ,list-size))
                (prog1 (svref (stack-frame-cache-contents ,(source)) next-free)
                       (incf& (stack-frame-cache-pointer ,(source))))))
            (defun ,deallocate (frame)
              (declare (ignore frame))
              (decf& (stack-frame-cache-pointer ,(source))))
            (defun ,reset-name ()
              (setf (stack-frame-cache-pointer ,(source)) 0)
              ;; this is a really simple memory-deallocation algorithm.
              ;; it's not particularly suitable for use with
              ;; the current frame alloc/dealloc mechanism, whereby
              ;; frames are kept in arrays, since the array gets
              ;; thrown away too.  if we keep this simple
              ;; deallocation algorithm then maybe we should
              ;; change the allocation algorithm to use conses
              ;; or something.  but it's probably better to redo
              ;; the whole thing, with emphasis on a smarter
              ;; way to do allocation/deallocation.
              (when (>& (stack-frame-cache-size ,(source))
                        ,maximum-preservable-quantity)
                (,pool-deallocate ,(source))
                ,(set-source `(,pool-allocate T))))
            (add-stack-frame-resetter ',name #',reset-name)
            ;;; who uses ,create ?
            (defun ,create ()
              (make-stack-frame ',name ,list-size))
            (defmacro ,pdl-push-frame (&rest names)
              (unless (equal names ',slot-names)
                (error
                 "PDL-PUSH-FRAME: Variables ~S do not match defined slots: ~S"
                 names ',slot-names))
              (let ((frame (gensym)))
                `(let ((,frame (,',allocate)))
                   ;; Put in pointer to previous stack frame
                   (unless (null *pdl*)
                     (setf (stack-frame-previous-frame *pdl*) ,frame))
                   ;; put in pointer to current top of stack
                   (setf (stack-frame-next-frame ,frame) *pdl*)
                   (setq *pdl* ,frame)
                   (setq ,frame (stack-frame-contents-location ,frame))
                   ;; put the variables in the stack frame
                   . ,(with-collection
                        (dolist (var names)
                          (collect `(setq ,frame (cdr ,frame)))
                          (collect `(setf (car ,frame) ,var)))))))
            (defmacro ,pdl-pop-frame (&rest names)
              (unless (equal names ',slot-names)
                (error "PDL-POP-FRAME: Variables ~S do not match defined slots: ~S"
                       names ',slot-names))
              (let ((frame (gensym)))
                `(let ((,frame *pdl*))
                   (when (not (eq (first ,frame) ',',name))
                     (frame-pop-error ',',name (first ,frame)))
                   ;; Return the frame to the stack frame cache it came from.
                   (,',deallocate ,frame)
                   ;; "pop" the frame off the stack.
                   (setq *pdl* (stack-frame-next-frame ,frame))
                   ;; Clear pointer to previous stack frame in new top frame,
                   ;; since it now has to previous.
                   (unless (null *pdl*)
                     (setf (stack-frame-previous-frame *pdl*) nil))
                   (setq ,frame (stack-frame-contents-location ,frame))
                   ,.(with-collection
                       (dolist (var names)
                         (collect `(setq ,frame (cdr ,frame) ,var (car ,frame)))
                         (collect `(setf (car ,frame) nil)))))))
            (defstruct (,name
                        :named
                        (:type list)
                        ;; can't use :conc-name because of package lossage
                        (:conc-name nil)
                        ;; predicate!!!
                        (:constructor nil)
                        (:copier nil))
              . ,(mapcar #'(lambda (u)
                                   (intern-in-eval-package
                                    (concatenate 'string
                                                 defstruct-conc-string
                                                 (symbol-name u))))
                         (append '(next-frame previous-frame)
                                 slot-names)))
            (defun ,unwind-protect-name ()
              (macrolet ((restore-state-variables ()
                                                  `(pdl-pop-frame ,',name
                                                                   . ,',slot-names)))
                        ,(or unwind-protect-form '(restore-state-variables))))
            (add-stack-frame-unwinder ',name #',unwind-protect-name)
            (define-frame-printer ,name . ,slot-names)))))

;;; Some macros for stack frame access
(defmacro stack-frame-type (frame)
  `(car ,frame))

;;; Next Frame means the next one to pop after this one.
(defmacro stack-frame-next-frame (frame)
  `(cadr ,frame))

;;; The Previous Frame is the frame pushed after this one.
(defmacro stack-frame-previous-frame (frame)
  `(caddr ,frame))

;;; Dereference this pointer with CDR to get a pointer to the stack frame contents.
(defmacro stack-frame-contents-location (frame)
  `(cddr ,frame))

(defmacro pdl-push-frame (type &rest names)
  `(,(intern-in-eval-package
      (concatenate 'string "PDL-PUSH-" (string type) "-INTERNAL"))
    . ,names))

(defmacro pdl-pop-frame (type &rest names)
  `(,(intern-in-eval-package
      (concatenate 'string "PDL-POP-" (string type) "-INTERNAL"))
    . ,names))

;;;
;;; Making and Using Stack Frames
;;;

;;; An alist of frame names and functions to reset the frames.
(defvar *stack-frame-resetters* nil)

(defun add-stack-frame-resetter (frame-type reset-function)
  (setq *stack-frame-resetters*
        (delete frame-type *stack-frame-resetters*
                :test #'(lambda (a b) (eq a (car b)))))
  (push (cons frame-type reset-function) *stack-frame-resetters*))

;;; an alist of unwinding functions for each kind of stack frame,
;;; used for throwing past a frame.
(defvar *stack-frame-unwinders* nil)

(defun add-stack-frame-unwinder (frame-type unwind-function)
  (setq *stack-frame-unwinders*
        (delete frame-type *stack-frame-unwinders*
                :test #'(lambda (a b) (eq a (car b)))))
  (push (cons frame-type unwind-function) *stack-frame-unwinders*))

(defun get-stack-frame-unwinder (frame-type)
  (cdr (fast-assq frame-type *stack-frame-unwinders*)))

(defun make-n-stack-frames (n name size)
  (let* ((stack (make-stack-frame-cache n))
         (contents-vector (stack-frame-cache-contents stack)))
    (dotimes (i n)
      (setf (svref contents-vector i) (make-stack-frame name size)))
    stack))

(defun add-n-stack-frames (stack inc name size)
  (let* ((old-contents (stack-frame-cache-contents stack))
         (old-size (stack-frame-cache-size stack))
         (new-size (+& old-size inc))
         (new-vector (make-array new-size)))
    ;; move the old contents to the new vector
    (dotimes (i old-size)
      (setf (svref new-vector i) (svref old-contents i)))
    ;; make new frames for the additional slots
    (do ((i old-size (1+& i)))
      ((=& i new-size))
      (setf (svref new-vector i) (make-stack-frame name size)))
    ;; put the new contents vector into the cache
    ;; we could reclaim the old contents-vector here....
    (setf (stack-frame-cache-contents stack) new-vector)
    stack))

(defun make-stack-frame (name size)
  (let ((list (make-list size)))
    (setf (car list) name)
    list))


;;; The function for resetting the stacks after a Lisp error which
;;; prevented proper unwinding of the stacks.  Only the first
;;; form *should* be necessary, since the PDL handlers should
;;; undo all the VPDL and dynamic variables stuff, but since
;;; we're trying to recover from an unanticipated Lisp error here, it's
;;; possible that one of the PDL handlers will blow out.  To ensure
;;; that things get close to consistent, we reset the other stacks
;;; by hand.  If they're already reset, it won't matter.
(defun reset-stacks ()
  (unwind-protect
   (dolist (pair *stack-frame-resetters*)
     (funcall (cdr pair)))
   (setq *pdl* nil)
   (reset-vpdl)
   (reset-dynamic-variables)))

(defun frame-pop-error (type-expected type-found)
  (error "Expected to pop a ~S frame, but got ~S" type-expected type-found))


;;;
;;; An auxillary macro to keep things from getting too hairy.  Used for
;;; debugging the evaluator.
;;;
(defmacro define-frame-printer (frame-type &rest names)
  `(defun ,(intern-in-eval-package (concatenate 'string "PRINT-"
                                                (string frame-type)))
     (frame)
     (format t "~%~A:" ',frame-type)
     (format
      t
      ,(string-trim
        "()"
        (format
         nil
         "~A"
         (map 'list
              #'(lambda (name)
                        (concatenate 'string
                                     "~%"
                                     (nstring-capitalize
                                      (string-trim "*"
                                                   (substitute #\space #\-
                                                               (string name))))
                                     ": ~A"))
              names)))
      . ,(map 'list #'(lambda (name)
                              `(,(intern-in-eval-package
                                  (concatenate 'string "DBG-" (string frame-type) "-"
                                               (string name)))
                                frame))
              names))
     (format t "~%")
     nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VPDL.  The VPDL is where values from functions and self-evaluating
;;; objects are kept prior to being passed to functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simple-vector style VPDL
(DEFVAR *default-VPDL-SIZE* 200)
(defvar *default-vpdl-grow-length* 100)

(defun make-vpdl ()
  (make-array *default-vpdl-size*))

;; called on bad lisp errors
(defun reset-vpdl ()
  (setq *vpdl-index* 0)
  (fill *vpdl* nil))

(defmacro vpdl-push (object)
  `(progn
    (when (=& *vpdl-index* *vpdl-size*) (grow-vpdl))
    (setf (svref& *vpdl* *vpdl-index*) ,object)
    (incf& *vpdl-index*)))

(defmacro vpdl-pop ()
  `(progn
    (when (zerop& *vpdl-index*) (vpdl-underflow))
    (decf& *vpdl-index*)
    (prog1 (svref& *vpdl* *vpdl-index*)
           (setf (svref& *vpdl* *vpdl-index*) nil))))

(defun vpdl-underflow ()
  (error "Attempt to pop the vpdl when it was empty"))

(defmacro VPDL-POP-NO-TEST ()
  `(prog1 (svref& *vpdl* (decf& *vpdl-index*))
          (setf (svref& *vpdl* *vpdl-index*) nil)))

(defun grow-vpdl ()
  (let ((new-vpdl (make-array (+& *vpdl-size* *vpdl-grow-length*))))
    (dotimes (i *vpdl-size*)
      (setf (svref& new-vpdl i)
            (svref& *vpdl* i)))
    (incf *vpdl-size* *vpdl-grow-length*)
    (setq *vpdl* new-vpdl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Evaluator PDL frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-stack-frame (eval-args-frame 50 50 50 100 nil)
  "Making more room for procedure inputs"
  (progn (do ((list (boxer-function-arglist *current-function*) (cdr list))
              (test-list *arglist*))
           ((eq list test-list))
           (vpdl-pop))
         (restore-state-variables))
  *arglist* *current-function*)

(define-stack-frame (ufun-frame 100 100 200 400 nil)
  "Making more procedure copies"
  (progn (dynamically-unbind-variables)
         (restore-state-variables))
  *executing-function* *executing-line* *executing-pointer*
  *run-list-sfun-epilog-handler* *ufuncall-sfun-epilog-handler*
  *current-function*)

(define-stack-frame (doit-port-funcall-frame 10 10 10 10 nil)
  "Making more procedure port copies"
  nil
  *lexical-variables-root* *dynamic-variables-bottom*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Stepper Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; these have to be here in order to be defined when the stepper-eval
;;; file is compiled....

;;; (they used to be in stepper.lisp)

(defvar *stepper-item-no*)

(defvar *stepper-row-no*)

(define-stack-frame (stepper-ufun-frame 20 10 10 20)
  "Making more procedure copies"
  nil
  *stepper-item-no* *stepper-row-no*)

(define-stack-frame (stepper-eval-args-frame 10 10 10 10)
  "Making more room for procedure inputs"
  nil
  *stepper-item-no*)

