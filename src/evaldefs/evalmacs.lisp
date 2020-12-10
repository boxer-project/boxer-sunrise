;;; -*- Syntax: Common-lisp; Base: 10; Package: EVAL -*-
;;;;
;;;;        Boxer
;;;;        Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;        Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;        used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;        Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;        https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                             +-Data--+
;;;;                    This file is part of the | BOXER | system
;;;;                                             +-------+
;;;;
;;;;
;;;;      The Explicit Control Evaluator
;;;;
;;;;    Modification History (most recent at top)
;;;;
;;;;    10/26/08 poll has to run periodic eval actions when polling as well (poll count = 0)
;;;;    10/21/03 modified poll macro to handle periodic evaluator actions which
;;;;             take place more often than checks for interrupt chars
;;;;     2/11/03 merged current LW and MCL files
;;;;     8/09/00 changed lwwin interrupt char in trace-entering to #\c
;;;;     2/14/99 started Harlequin Lispworks changes, shadow DEBUG
;;;;             eval-object? and possible-eval-object?, quick-time
;;;;


(in-package :boxer-eval)

(defun intern-in-eval-package (thing)
  (intern (string thing) 'boxer-eval))

(eval-when
  (:compile-toplevel :load-toplevel :execute)
    (defun create-local-eval-state-vars-bvl ()
      (mapcan #'(lambda (entry)
            (if (evsi-local-p entry)
          (list (list (evsi-variable-name entry) (evsi-local-initial-value entry)))
              nil))
            *eval-state-vars*))
)

;;; The evaluator invokes this macro right at the beginning
;;; to set up the state variables and handle unexpected Lisp errors.
(defmacro with-evaluator-state-variables-and-unwind-protect (&body body)
  (let ((eval-completion-gensym (gensym)))
    `(let ((,eval-completion-gensym nil))
       (catch 'boxer-error
   (unwind-protect
        (compiler-let ()
    (let ,(create-local-eval-state-vars-bvl)
      (compiler-let ()
        (prog1 (progn . ,body) (setq ,eval-completion-gensym t)))))
     (unless ,eval-completion-gensym
       (when boxer::*boxer-system-hacker* (format t "~%Unwinding stack..."))
       (unwind-to-top-level)
       (reset-stacks)
       (when boxer::*boxer-system-hacker* (format t "Done~%"))))))))

;;;
;;; Evaluator Debugging
;;;
(defvar *debugging* nil)
(defvar *start-time*)
(defvar *timing-overhead*)
(defvar *last-timing-figure*)
(defvar *compile-with-debugging* t)

;; sgithens March 7, 2020
;; (defun debug ()
  ;; (setq *debugging* t))

(defun nodebug ()
  (setq *debugging* nil))

(defmacro when-debugging (&body body)
  `(when *debugging*
     ., body))

(defmacro quick-time ()
 `(get-internal-real-time))

(defmacro init-timing ()
  (when *compile-with-debugging*
    `(when-debugging
      (setq *timing-overhead* 0)
      (setq *start-time* (quick-time))
      (trace-entering Testing)
      (trace-entering Testing)
      (setq *timing-overhead* *last-timing-figure*))))

(defvar *pc-for-debugging* nil
  "Examine this variable to see where the evaluator croaked.")

(defmacro trace-entering (place &rest args)
  (when *compile-with-debugging*
    `(progn
       ,(unless (eq place 'handle-exception) `(setq *pc-for-debugging* ',place))
       (when *debugging*
   (let ((time (quick-time)))
     (setq *last-timing-figure* (- time *start-time* *timing-overhead*))
     (format t "~Dms~%" *last-timing-figure*)
     (format t "~A " ',place)
     ,@(boxer::with-collection
        (dolist (arg args)
    (boxer::collect `(format t "~S ~S, " ',arg ,arg))))
     (do () ((not (char-equal (read-char)
            #\c))); TODO sgithens #\control-c)))
       (break))
     (setq *start-time* (quick-time)))))))

;;;
;;; Fast Type checking mechanisms
;;;
;;; The evaluator assumes that all of the objects with which it has to deal with will be
;;; either a symbol, a number, or a simple vector.  We will also require that the first
;;; slot in the array contains a symbol that specifies the type to the evaluator.  This
;;; lets us do (svref <thing> 0) in order to obtain an object's type.
;;;
;;; see macros for fast-editor-*-box? fast-eval-*-box.
;;;
;;; Call this macro if you're not sure whether the object is an editor object
;;; or an eval object.  If this macro returns t, then it is safe to look in
;;; slot 0 to find the evaluator-type of the object.
;;;
;;; DO NOT depend on this function to distinguish editor objects from evaluator
;;; objects, since it might return T on an editor object, or any object,
;;; in some implementations.  This function should be renamed
;;; to SAFE-TO-CALL-FAST-EVAL-PREDICATES?

;;; POSSIBLE-EVAL-OBJECT? tells whether it is legal to look in slot 0.
(defmacro possible-eval-object? (thing)
  #+(or lucid lispworks)
  `(simple-vector-p ,thing)
  #+(or excl lispm)
  T
  #+mcl
  `(vectorp ,thing)  ; +++ I guess
  #-(or lucid lispm excl mcl lispworks)
  (warn "Check if your CLOS or PCL implementation uses vectors to make its objects~%~
         If (vectorp (make-instance <whatever>) is NIL, then~
         change the definition of ~S to simply return T" 'possible-eval-object)
  #-(or lucid lispm excl mcl lispworks)
  `(vectorp ,thing))

;; This function distinguishes eval objects from editor objects,
;; but doesn't guarantee to distinguish eval objects from other vectors.
(defmacro eval-object? (thing)
  `(simple-vector-p ,thing))

(defmacro special-token? (thing)
  `(eq (svref& ,thing 0) 'special-eval-token))

(defmacro special-token-type (thing)
  `(svref ,thing 1))

(defmacro special-token-item (thing)
  `(svref& ,thing 2))

(defmacro unbox-token? (thing)
  `(eq (special-token-type ,thing) 'bu::@))

(defmacro eval-it-token? (thing)
  `(eq (special-token-type ,thing) 'bu::eval-it))

(defmacro previous-tell-environment-token? (thing)
  `(eq (special-token-type ,thing) 'bu::previous-tell-environment))

(defmacro dots-list-token? (thing)
  `(eq (special-token-type ,thing) 'bu::dots-list))

(defmacro squid-token? (thing)
  `(eq (special-token-type ,thing) 'self-quoting-internal-datum))

;;;
;;; Compiler interface
;;;
(defvar *old-compilation-speed* 0)

(defmacro compile-lambda-if-possible (name lambda-form)
  `(cond ((null *compile-boxer-generated-lambda?*) ,lambda-form)
   ((eq *compile-boxer-generated-lambda?* :fast-compile)
    (proclaim '(optimize (compilation-speed 3)))
    (unwind-protect (symbol-function (compile ,name ,lambda-form))
      (proclaim `(optimize (compilation-speed ,*old-compilation-speed*)))))
   (t
    (symbol-function (compile ,name ,lambda-form)))))

;;;
;;; Polling/Interrupts
;;;
(defmacro poll (&optional always?)
  (cond (always?
   `(if (poll-internal)
        (setq *poll-count* 1)))
  (t
   `(or *last-interrupt-char*
              (let ((pc (decf& *poll-count*)))
          (cond ((zerop& pc)
           (setq *poll-count* *initial-poll-count*)
                       (unless (null *periodic-eval-action*) (funcall *periodic-eval-action*))
           (poll-internal))
                      ((member pc *periodic-eval-times* :test #'=)
                       (unless (null *periodic-eval-action*) (funcall *periodic-eval-action*))
                       nil)))))))

;;;
;;; Arg Flavors
;;;
;;; Here's the theory:
;;; Never put numbers in boxes or take them out.  The primitives will do it
;;; themselves via functions.

;for number of selectq items?
(defmacro special-arg-handling-cases (&body selectq-body)
  `(cond ((or (null *current-function*) (symbolp (car *arglist*)))
    ., (cdr (assoc 'otherwise selectq-body)))
   (t (case (caar *arglist*)
        ., selectq-body))))

;;;
;;; Stepper -- we have a separate evaluator for the stepper.
;;;
(defvar *compiling-stepper* nil)

(defmacro when-stepping (&body body)
  (if *compiling-stepper*
      `(when *stepping* . ,body)
    nil))

(defmacro unless-stepping (&body body)
  (if (not *compiling-stepper*)
      `(progn . ,body)
    nil))

;;;
;;; tail recursion
;;;
(boxer::defsubst tail-recurse-p ()
  nil)



;;;
;;; Virtual Copy Interface
;;;
(defmacro boxer-eval::copy-thing (object)
  `(boxer::virtual-copy ,object))

