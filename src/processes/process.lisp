;;;-*-LISP-*-

#|

 $Header: process.lisp,v 1.0 90/01/24 22:15:57 boxer Exp $

 $Log:	process.lisp,v $
;;;Revision 1.0  90/01/24  22:15:57  boxer
;;;Initial revision
;;;

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


|#

(in-package :boxer-eval)

;;; This file needs to be loaded after all the eval vars are defined,
;;; including those defined in recursive primitives.

;;; Hmm, that means we can't use process-variable inside a recursive-prim.

;;; This file and process-prims need to be recompiled whenever there are
;;; changes in the state variables, local or global, including changes in
;;; recursive primitives.

;;; The evaluator itself needs to be recompiled only if we change the
;;; :LOCAL state variables.

;;; The variable info is sorted just before final use.


(defmacro process-variable (process variable-name)
  (declare-eval-state-variable-index-macro)
  (let ((index (position variable-name *eval-state-vars*
       :key #'evsi-variable-name)))
    (when (null index)
      (error "~S is not a known evaluator process variable." variable-name))
    `(svref& ,process ,index)))

(defmacro %make-process-state-vector ()
  (declare-eval-state-variable-index-macro)
  `(vector .
    ,(mapcar #'(lambda (var)
     (if (evsi-local-p var)
         (evsi-local-initial-value var)
         (evsi-global-initial-value var)))
       *eval-state-vars*)))

;;; Call this to make a process vector for a new process.
(defun init-process-state-vector ()
  (setq *current-process* (%make-process-state-vector))
  (setf (process-variable *current-process* *current-process*)
  *current-process*))

(defmacro %reset-process (proc)
  (let ((forms nil))
    (dolist (var *eval-state-vars* `(progn . ,(nreverse forms)))
      (push `(setf (process-variable ,proc ,(evsi-variable-name var))
       ,(if (evsi-local-p var)
      (evsi-local-initial-value var)
      (evsi-global-initial-value var)))
      forms))))

(defun reset-process (proc)
  (%reset-process proc))

(defmacro %print-process (proc)
  (let ((forms nil))
    (dolist (var *eval-state-vars* `(progn . ,(nreverse forms)))
      (push `(format t "~%~S: ~S" ',(evsi-variable-name var)
         (process-variable ,proc ,(evsi-variable-name var)))
      forms))))

(defun print-process (proc)
  (let ((*print-circle* t))
    (%print-process proc)))



;;; This macro is invoked inside the evaluator.  It
;;; bundles up the local values and calls a function which
;;; will add to them the global values.
;;; evaluator states live in the *current-process* state variable.

(defmacro store-evaluator-state ()
  (let ((i 0))
    `(progn
       (when *debugging*
   (format t "~%Storing Process State for ~A" *current-process*))
       (setf . ,(mapcan #'(lambda (var)
          (if (evsi-local-p var)
        (list `(svref& *current-process*
                 ,(prog1 i (incf i)))
              (evsi-variable-name var))
        (progn (incf i) nil)))
      *eval-state-vars*))
       (%store-evaluator-state-internal))))

(defmacro %store-evaluator-state-internal-1 ()
  (let ((i 0))
    `(setf . ,(mapcan #'(lambda (var)
        (if (not (evsi-local-p var))
            (list `(svref& *current-process*
               ,(prog1 i (incf i)))
            (evsi-variable-name var))
            (progn (incf i) nil)))
          *eval-state-vars*))))


(defun %store-evaluator-state-internal ()
  (%store-evaluator-state-internal-1))


(defmacro retrieve-evaluator-state ()
  (let ((i 0))
    `(progn
       (when *debugging*
   (format t "~%Retrieving Process State from ~A" *current-process*))
       (setq . ,(mapcan #'(lambda (var)
          (if (evsi-local-p var)
        (list (evsi-variable-name var)
              `(svref& *current-process*
                 ,(prog1 i (incf i))))
        (progn (incf i) nil)))
      *eval-state-vars*))
       (%retrieve-evaluator-state-internal))))



(defmacro %retrieve-evaluator-state-internal-1 ()
  (let ((i 0))
    `(setq . ,(mapcan #'(lambda (var)
        (if (not (evsi-local-p var))
            (list (evsi-variable-name var)
            `(svref& *current-process*
               ,(prog1 i (incf i))))
            (progn (incf i) nil)))
          *eval-state-vars*))))

(defun %retrieve-evaluator-state-internal ()
  (%retrieve-evaluator-state-internal-1))





;;; some crocks
;; this function needs to do all sorts of stuff it's not doing.

(defun kill-boxer-processes ()
  (setq *boxer-processes* NIL))

(defun process-run-function (rest-of-line box)
  (let ((proc (or (box::getprop box :process)
      (let ((newproc (%make-process-state-vector)))
        (setf (process-variable newproc *current-function*)
        newproc)
        newproc))))
    (let ((current-line (process-variable proc *executing-pointer*)))
      (cond ((null current-line)
       )
      ;; eventually
      ;; ((eq (car rest-of-line) 'bu::stop-process)...)
      (t
       ;; not really the right thing,
       ;; we should walk up ufun frames and append rest-of-line
       ;; to the top level *executing-pointer*
       (primitive-signal-error :process "The box, " box ", is busy"))))))


;;


;;; Scheduler stuff

;;; Each process keep track of the position of the cursor when the user
;;; pressed DOIT.  When the process returns a value, Boxer moves the cursor
;;; to the best approximation of THERE that exists and prints the returned
;;; value.

#|    (setf (process-variable *current-process* *process-doit-cursor-position*)
    vector) |#


(defun init-process-doit-cursor-position ()
  (let ((vector (boxer::make-process-doit-cursor-position)))
    (boxer::fill-doit-cursor-position-vector vector)
    vector))


;;;
;;; need to check the following 3 possible conditions:
;;;
;;; (1) IF there is no unhandled input
;;;       THEN IF there is another process waiting to be run,
;;;              THEN do a process switch
;;;            OTHERWISE, do nothing (continue
;;;                       with the current process) and return NIL
;;; (2) IF there is unhandled input
;;;       THEN IF there is a process in :iowait
;;;              THEN handle the input with the process's bindings and
;;;                   then pause the process to be restarted at the
;;;                   bottom of the command loop
;;;            IF there is a :toplevel process
;;;              THEN do a process switch if we can
;;;            OTHERWISE pause the current process and handle the input
;;;                      the paused process should be restarted at the
;;;                      bottom of the command loop
;;; (3) IF there is an unhandled interrupt
;;;       THEN IF there is a :toplevel process
;;;              THEN switch to that process and return T, setting
;;;               the *last-interrupt-char*
;;;            OTHERWISE do a process switch if we can
;;;

;;;
;;; This is (should be) 2 tiered, polling for input
;;; a lot less frequently than process switching since input
;;; polling is SO expensive under X
;;;

(defvar *doit-key-process* nil)

(defun poll-internal-and-schedule ()
  (or *last-interrupt-char*
      (multiple-value-bind (abort? any-input?)
    (bw::keyboard-interrupt? boxer::*boxer-pane*)
  (cond ((null any-input?)
         (next-restartable-process))
        ((null abort?)
         ;; there IS pending input
         ;; switch to the keyboard owning process, or else :pause
         ;; the current one so we can return to toplevel
         (or (keyboard-owning-process)
       (if (not (null *doit-key-process*))
           (next-restartable-process)
           :pause)))
        (t
         (setq *last-interrupt-char* abort?)
         t)))))

(defun queue-process (proc)
  (if (null *boxer-processes*)
      (setq *boxer-processes* (list proc))
      (nconc (last *boxer-processes*) (list proc))))

;; we could slam the evaluator state into the process before
;; finishing it, instead, we'll just reset it explicitly here
(defun process-finish (proc)
  (reset-process proc)
  ;; shouldn't be here but check anyway
  (when (fast-memq proc *boxer-processes*)
    (setq *boxer-processes* (fast-delq proc *boxer-processes*))
    (warn "Found process, ~A, in the process queue, removing..." proc))
  (when (eq proc *doit-key-process*)
    ;(queue-process proc)
    (setq *doit-key-process* nil))
;  (process-display *boxer-processes*)
  proc)

;; only setting it when it is already bound
(defun set-keyboard-process (process)
  (unless (null *doit-key-process*) (setq *doit-key-process* process)))

;; this is usefule for debugging when a box's process gets
;; hopelessly mangled

(defun remove-process (box)
  (box::removeprop box :process))

(defmacro with-recursive-edit-key-bindings (&body body)
  (let* ((newdoitfun (encapsulate-key-function 'box::com-recursive-doit))
   (lk (make-non-caching-static-variable 'bu::line-key newdoitfun))
   (r2 (make-non-caching-static-variable 'bu::R2-key newdoitfun))
   (r3 (make-non-caching-static-variable 'bu::R3-key newdoitfun)))
    `(let ((bu::line-key ',lk) (bu::r2-key ',r2) (bu::r3-key ',r3))
       (declare (special bu::line-key bu::r2-key bu::r3-key))
       . ,body)))

